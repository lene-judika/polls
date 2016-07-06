{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GADTs, DeriveGeneric, OverloadedStrings, TemplateHaskell, TupleSections, FlexibleInstances #-}

module Main(main) where
  import GHC.Generics

  import Lib

  import Data.Maybe

  import Data.Time.Clock
  import Data.Time.Format

  import qualified Data.ByteString.Lazy as L

  import Data.Aeson hiding (Error, Result)
  import Data.Aeson.Types(typeMismatch)

  import Data.Set(Set)
  import qualified Data.Set as Set

  import Data.Map(Map)
  import qualified Data.Map as Map

  import Control.Exception

  import Control.Applicative
  import Control.Monad
  import Control.Monad.Trans(liftIO)
  import Control.Monad.Reader
  import Control.Concurrent.MVar

  import Control.Arrow(first, second)

  import Text.Read

  import Control.Lens.Operators
  import Control.Lens hiding (view)

  import Network.HTTP
  import Network.Stream
  import Network.URI

  import Graphics.UI.Gtk(AttrOp( (:=) ))
  import qualified Graphics.UI.Gtk as Gtk

  -- simple helper
  liftEvent :: IO () -> Gtk.EventM a Bool
  liftEvent = (>> return False) . liftIO

  liftReader :: Monad m => Reader r a -> ReaderT r m a
  liftReader r = do
    a <- ask
    return $ runReader r a


  -- A Bunch of Messages
  type MsgBox msg = [Maybe msg]

  emptyMsgBox :: MsgBox msg
  emptyMsgBox = []


  type GuiEnv msg = (MVar (MsgBox msg))

  type Gui msg = ReaderT (GuiEnv msg) IO

  drain :: Gui msg (MsgBox msg)
  drain = do
    m <- ask
    liftIO $ swapMVar m emptyMsgBox

  --provide an extra value and a action and execute the action on the value when called
  mkCallback :: (b -> IO (Maybe msg)) -> b -> Reader (GuiEnv msg) (IO ())
  mkCallback f v = do
    m <- ask
    return (cb m)
    where
      cb m = do
        v' <- f v
        mbox <- takeMVar m
        putMVar m (v' : mbox)

  mkSimpleCallback :: Maybe msg -> Reader (GuiEnv msg) (IO ())
  mkSimpleCallback = mkCallback return


  -- Widget --
  class MkWidget w wc | w -> wc where
    mkWidget :: w msg -> Gui msg wc

  data Widget msg where
    Widget :: (Gtk.WidgetClass wc, MkWidget w wc) => w msg -> Widget msg

  instance MkWidget Widget Gtk.Widget where
    mkWidget (Widget w) = Gtk.toWidget <$> mkWidget w


  -- Box --
  data Orientation = Horizontal | Vertical

  data Box msg = Box Orientation [Widget msg]

  mkBox :: Orientation -> [Widget msg] -> Widget msg
  mkBox o ws = Widget $ Box o ws

  instance MkWidget Box Gtk.Box where
    mkWidget (Box o ws) = do
      let
        newBox = case o of
          Horizontal -> \b i -> Gtk.toBox <$> Gtk.hBoxNew b i
          Vertical   -> \b i -> Gtk.toBox <$> Gtk.vBoxNew b i
      box <- liftIO $ newBox False 10
      ws' <- mapM mkWidget ws
      liftIO $ mapM_ (\w -> Gtk.boxPackStart box w Gtk.PackGrow 0) ws' --TODO make packing an attribute to Box
      return box



  -- Button --
  data Button msg = Button String (Maybe msg)

  mkButton :: String -> Maybe msg -> Widget msg
  mkButton s m = Widget $ Button s m

  instance MkWidget Button Gtk.Button where
    mkWidget (Button label msg) = do
      b <- liftIO Gtk.buttonNew
      liftIO $ Gtk.buttonSetLabel b label
      case msg of
        Just m  -> do
          liftIO $ Gtk.widgetSetSensitive b True
          cb <- liftReader $ mkSimpleCallback (Just m)
          _ <- liftIO $ Gtk.on b Gtk.buttonActivated cb
          return ()
        Nothing -> do
          liftIO $ Gtk.widgetSetSensitive b False
          return ()
      return b

  -- Label --
  data Label msg = Label String

  mkLabel :: String -> Widget msg
  mkLabel = Widget . Label

  instance MkWidget Label Gtk.Label where
    mkWidget (Label s) = liftIO $ Gtk.labelNew (Just s)

  -- Frame --
  data Frame msg = Frame (Maybe String) (Widget msg)

  mkFrame :: Maybe String -> Widget msg -> Widget msg
  mkFrame s w = Widget $ Frame s w

  instance MkWidget Frame Gtk.Frame where
    mkWidget (Frame s w) = do
      f <- liftIO Gtk.frameNew
      case s of
        Just label ->
          liftIO $ Gtk.frameSetLabel f label
        _ ->
          return ()
      w' <- mkWidget w
      liftIO $ Gtk.containerAdd f w'

      return f

  -- Text --
  data TextEvent = OnActivate | OnFocusLost
    deriving (Eq, Ord)

  data Text msg = Text String (Maybe (Set TextEvent, String -> msg))

  mkText :: String -> Maybe ([TextEvent], String -> msg) -> Widget msg
  mkText s c = Widget $ Text s (first Set.fromList <$> c)

  activateTextCallback :: Gtk.Entry -> TextEvent -> IO ()  -> IO ()
  activateTextCallback t e c = void $ case e of
    OnActivate  -> Gtk.on t Gtk.entryActivated              c
    OnFocusLost -> Gtk.on t Gtk.focusOutEvent . liftEvent $ c

  instance MkWidget Text Gtk.Entry where
    mkWidget (Text s e) = do
      t <- liftIO Gtk.entryNew
      liftIO $ Gtk.entrySetText t s
      case e of
        Just (es, f) -> do
          cb <- liftReader $ mkCallback (fmap (Just . f) . Gtk.entryGetText) t
          liftIO $ mapM_ (flip (activateTextCallback t) cb) es
          liftIO $ Gtk.widgetSetSensitive t True
        Nothing -> liftIO $ Gtk.widgetSetSensitive t False
      return t


  -- TextList --
  data TextList msg = TextList [String] (Int -> msg)

  mkTextList :: [String] -> (Int -> msg) -> Widget msg
  mkTextList ts c = Widget $ TextList ts c

  instance MkWidget TextList Gtk.TreeView where
    mkWidget (TextList ts f) = do
      ls <- liftIO $ Gtk.listStoreNew ts
      tv <- liftIO $ Gtk.treeViewNewWithModel (Gtk.toTreeModel ls)
      col <- liftIO Gtk.treeViewColumnNew
      cr <- liftIO Gtk.cellRendererTextNew

      liftIO $ Gtk.treeViewColumnPackStart col cr True
      _ <- liftIO $ Gtk.treeViewAppendColumn tv col

      liftIO $ Gtk.cellLayoutSetAttributes col cr ls (\ind -> [Gtk.cellText := ind])
      liftIO $ Gtk.treeViewSetHeadersVisible tv False

      cb <- liftReader $ mkCallback (fmap (Just . f . head . fst) . Gtk.treeViewGetCursor) tv
      _ <- liftIO $ Gtk.on tv Gtk.cursorChanged cb

      return tv


  -- ScrolledWindow --
  data ScrolledWindow msg = ScrolledWindow (Widget msg)

  mkScrolledWindow :: Widget msg -> Widget msg
  mkScrolledWindow w = Widget $ ScrolledWindow w

  instance MkWidget ScrolledWindow Gtk.ScrolledWindow where
    mkWidget (ScrolledWindow w) = do
      sw <- liftIO $ Gtk.scrolledWindowNew Nothing Nothing

      w' <- mkWidget w

      liftIO $ Gtk.scrolledWindowSetPolicy sw Gtk.PolicyAutomatic Gtk.PolicyAutomatic


      liftIO $ Gtk.containerAdd sw w'

      return sw


  -- used to store information about the gui while iterating
  type GuiState = Gtk.Window

  initGuiState :: Gui msg GuiState
  initGuiState = do
    window <- liftIO Gtk.windowNew
    cb <- liftReader $ mkSimpleCallback Nothing
    _ <- liftIO $ Gtk.onDestroy window cb
    return window

  runGui :: GuiState -> Widget msg -> Gui msg (MsgBox msg)
  runGui window ws = do
    liftIO $ Gtk.containerForeach window (Gtk.containerRemove window)

    w <- mkWidget ws

    liftIO $ Gtk.containerAdd window w
    liftIO $ Gtk.widgetShowAll window

    stepGui

  stepGui :: Gui msg (MsgBox msg)
  stepGui = do
    _ <- liftIO Gtk.mainIteration
    m <- drain

    case m of
      [] -> stepGui
      _  -> return m

  runStarterApp :: model -> (model -> msg -> model) -> (model -> Widget msg) -> IO ()
  runStarterApp i u v = do
    _ <- Gtk.initGUI
    m <- newMVar emptyMsgBox

    runReaderT (initGuiState >>= \g -> runStarterApp' g i u v) m

  runStarterApp' :: GuiState -> model -> (model -> msg -> model) -> (model -> Widget msg) -> Gui msg ()
  runStarterApp' g i u v = do
    a <- runGui g (v i)

    let i' = foldM (fmap . u) i a

    case i' of
      Just n -> runStarterApp' g n u v
      Nothing  -> return ()

  runApp :: (model, Cmd msg) -> (model -> msg -> (model, Cmd msg)) -> (model -> Widget msg) -> IO ()
  runApp i u v = do
    _ <- Gtk.initGUI
    m <- newMVar emptyMsgBox

    runReaderT (initGuiState >>= \g -> runApp' g i u v) m

  runApp' :: GuiState -> (model, Cmd msg) -> (model -> msg -> (model, Cmd msg)) -> (model -> Widget msg) -> Gui msg ()
  runApp' g (i, c) u v = do
    i' <- liftIO $ execCmd u i c -- TODO split cmds here

    a <- runGui g (v i')

    let (i'', cs) = applyMsgs u i' a

    case i'' of
      Just n -> runApp' g (n, cs) u v
      Nothing  -> return ()

  applyMsgs :: (model -> msg -> (model, Cmd msg)) -> model -> MsgBox msg -> (Maybe model, Cmd msg)
  applyMsgs _ i []             = (Just i,  noCmd)
  applyMsgs _ _ (Nothing : _)  = (Nothing, noCmd)
  applyMsgs u i (Just m  : ms) = second (cs ++) $ applyMsgs u next ms
    where
      (next, cs) = u i m

  type Cmd msg = [IO msg]

  noCmd :: Cmd msg
  noCmd = mempty

  execCmd :: (model -> msg -> (model, Cmd msg)) -> model -> Cmd msg -> IO model --TODO fold it
  execCmd _ m []     = return m
  -- FIXME this is highly recursive, a recursive cmd will break the app
  execCmd u m (c:cs) = do
    msg <- c
    let (m',cs') = u m msg
    execCmd u m' (cs ++ cs')

  type Appointment = UTCTime

  data Poll = Poll
    { _pollPid  :: Maybe Int
    , _pollName :: String
    , _pollVotes :: Map (Maybe Appointment) Int
  }
  makeFields ''Poll

  emptyPoll :: Poll
  emptyPoll = Poll Nothing "" Map.empty

  class ToPoll p where
    toPoll :: p -> Either String Poll

  data JsonPollGetResp = JsonPollGetResp
    { _JsonPollGetRespPid :: Int
    , _JsonPollGetRespName :: String
    , _JsonPollGetRespAppointments :: [String]
    , _JsonPollGetRespVotes :: Map String Int
    } deriving (Generic, Show)

  instance ToPoll JsonPollGetResp where
    toPoll (JsonPollGetResp p n as vs) = fmap (Poll (Just p) n) (mkVotes as vs)

  instance FromJSON JsonPollGetResp where
      parseJSON (Object v) = JsonPollGetResp <$>
        v .: "PID" <*>
        v .: "name" <*>
        v .: "appointments" <*>
        v .: "votes"
      -- A non-Object value is of the wrong type, so fail.
      parseJSON invalid = typeMismatch "Poll" invalid

  parseISO8601 :: String -> Maybe UTCTime
  parseISO8601 t = parseTimeM False defaultTimeLocale "%Y-%m-%d" t <|>
                   parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M" t


  mkAppointment :: String -> Either String (Maybe Appointment)
  mkAppointment "" = return Nothing
  mkAppointment s  = Just <$> maybe (Left $ "ParseError: invalid ISO8601-Date: " ++ s) Right (parseISO8601 s)

  mkVotes :: [String] -> Map String Int -> Either String (Map (Maybe Appointment) Int)
  mkVotes as vs = do
    as' <- catMaybes <$> mapM mkAppointment as
    vs' <- Map.fromList <$> mapM (\(s,i) -> (,i) <$> mkAppointment s ) (Map.assocs vs)
    return (mkVotes' as' vs')

  -- make sure the appointments are valid
  mkVotes' :: [Appointment] -> Map (Maybe Appointment) Int -> Map (Maybe Appointment) Int
  mkVotes' as = Map.filterWithKey $ \a _ -> fromMaybe True $ (`elem` as) <$> a


  data ApiError = ApiError
    { _apiErrorStatuscode :: Int
    , _apiErrorReason :: String
    } deriving (Show) --TODO better show instance
  deriveFromJSONLensLike ''ApiError





  catchError :: (L.ByteString -> Message) -> Response L.ByteString -> Message
  catchError f r = case rspCode r of
    (2,_,_) ->
      f (rspBody r)
    _ ->
      handleError (rspBody r)

  handleError :: L.ByteString -> Message
  -- TODO Left contains a non 2xx message, but not json, give better error messages here
  handleError msg = either Error (Error . (show :: ApiError -> String)) $ eitherDecode msg

  readPoll :: L.ByteString -> Message
  readPoll msg = either Error RecvPollRead $ do
    p <- eitherDecode msg :: Either String JsonPollGetResp
    toPoll p


  send :: RequestMethod -> (L.ByteString -> Message) -> (String -> Message) -> String -> IO Message
  send r f e s = case parseURI s of
      Nothing ->
        return (e $ "ParseError: Not a valid URI: " ++ s)
      Just uri -> do
        result <- try $ simpleHTTP (mkRequest r uri) :: IO (Either IOException (Result (Response L.ByteString)))
        case result of
          Right (Right response) -> do
            L.putStrLn $ rspBody response -- XXX this is only for DEBUG
            return $ catchError f response
          _ ->
            return (e (show result)) --TODO better error messages


  data Message = SendPollRead Int | RecvPollRead Poll | Error String | NoOp | Disconnect | Connect | SetTempHost String | SetCursor Int | ReadPoll | SetRead String | ToOverview | CreatePoll

  data Model = Connected ModelConnected | Disconnected ModelDisconnected

  data ModelConnected = ModelConnected
    { _modelConnectedHostname :: String
    , _modelConnectedPolls :: Map Int Poll
    , _modelConnectedPollIdx :: Maybe Int
    , _modelConnectedMode :: Mode
    }

  data Mode = Overview | Read (Maybe Int) | Change ChangeMode Poll

  data ChangeMode = Update | Create

  data ModelDisconnected = ModelDisconnected
    { _modelDisconnectedHostname :: String
    }
  makeFields ''ModelConnected
  makeFields ''ModelDisconnected
  makePrisms ''Mode
  makeFields ''ChangeMode
  makePrisms ''Model


  initModel :: (Model, Cmd Message)
  initModel = (Disconnected $ ModelDisconnected "http://127.0.0.1:8080", noCmd)

  update :: Model -> Message -> (Model, Cmd Message)
  update (Disconnected m) Connect       = (Connected $ ModelConnected (m^.hostname) Map.empty Nothing Overview, noCmd)
  update (Connected _) Disconnect       = initModel

  update (Connected m) ToOverview       = (Connected $ m & mode .~ Overview, noCmd)
  --update (Connected m) CreatePoll       = (Connected $ m & mode .~ Change Create emptyPoll, noCmd)
  update (Connected m) (SetRead s)      = (Connected $ m & mode._Read %~ (readMaybe s <|>), noCmd)
  update (Connected m) ReadPoll         = (Connected $ m & mode .~ Read Nothing, noCmd)
  update (Connected m) (SendPollRead n) = (Connected m, [send GET readPoll Error (m^.hostname ++ "/polls/" ++ show n), return ToOverview])
  update (Connected m) (RecvPollRead p) = (Connected $ m & polls %~ Map.insert (fromJust $ p^.pid) p, noCmd) -- XXX fromJust seems pretty Hacky
  update m (Error s)                    = (m, [putStrLn s >> return NoOp])
  update m _                            = (m, noCmd)

  view :: Model -> Widget Message
  view (Disconnected m) = mkBox Vertical
      [ mkLabel "Enter Hostname:"
      , mkText (m^.hostname) (Just ([OnFocusLost], SetTempHost))
      , mkButton "Connect" (Just Connect)
      ]
  view (Connected m) = mkBox Vertical
        [ mkFrame (Just "Host") $ mkBox Horizontal [ mkLabel (m^.hostname), mkButton "Disconnect" (Just Disconnect)]
        , case m^.mode of
            Overview ->
              mkFrame (Just "Polls") $ mkBox Horizontal
              [ mkScrolledWindow $ mkTextList (m^..polls.traverse.name) SetCursor --TODO highlight cursor
              , mkBox Vertical
                [ mkButton "Create" (Just CreatePoll)
                , mkButton "Read"   (Just ReadPoll)
                --, mkButton "Update" (Just UpdatePoll)
                --, mkButton "Delete" (Just DeletePoll)
                ]
              ]
            Read n ->
              mkFrame (Just "Read Poll") $ mkBox Vertical
              [ mkText (maybe "" show n) (Just ([OnFocusLost], SetRead))
              , mkBox Horizontal
                [ mkButton "Ok" (fmap SendPollRead n)
                , mkButton "Back" (Just ToOverview)
                ]
              ]
            Change c p ->
             viewPoll c p
        ]

  viewPoll :: ChangeMode -> Poll -> Widget Message
  viewPoll c p = undefined --mkFrame (Just (show c ++ "Poll")) $ mkBox Horizontal

  main :: IO ()
  main = runApp initModel update view

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GADTs #-}

module Widget
  ( Widget
  , Orientation(..)
  , mkBox
  , mkFrame
  , mkText
  , TextEvent(..)
  , mkButton
  , mkTextList
  , mkScrolledWindow
  , mkLabel
  , GuiState
  , Gui
  , MsgBox
  , runGui
  , buildGui
  ) where

  import Data.Coerce

  import Data.Set(Set)
  import qualified Data.Set as Set

  import Control.Monad.Reader
  import Control.Monad.State
  import Control.Concurrent.MVar

  import Control.Lens

  import Graphics.UI.Gtk(AttrOp( (:=) ))
  import qualified Graphics.UI.Gtk as Gtk


  type MsgBox msg = [Maybe msg]

  emptyMsgBox :: MsgBox msg
  emptyMsgBox = []

  type GuiEnv msg = MVar (MsgBox msg)

  type Gui msg = StateT GuiState (ReaderT (GuiEnv msg) IO)

  runGui :: Gui msg b -> IO b
  runGui g = do
    m <- newMVar emptyMsgBox
    flip runReaderT m $ do
      s <- initGuiState
      evalStateT g s

  class Functor w => MkWidget w wc | w -> wc where
    mkWidget :: w msg -> Gui msg wc



  -- simple helper
  liftEvent :: IO () -> Gtk.EventM a Bool
  liftEvent = (>> return False) . liftIO


  liftReader :: MonadReader r m => Reader r b -> m b
  liftReader r = do
    a <- ask
    return $ runReader r a

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
  data Widget msg where
    Widget :: (Gtk.WidgetClass wc, MkWidget w wc) => w msg -> Widget msg

  instance Functor Widget where
    fmap f (Widget w) = Widget (fmap f w)

  instance MkWidget Widget Gtk.Widget where
    mkWidget (Widget w) = Gtk.toWidget <$> mkWidget w

  -- Box --
  data Orientation = Horizontal | Vertical

  data Box msg = Box Orientation [Widget msg]

  mkBox :: Orientation -> [Widget msg] -> Widget msg
  mkBox o ws = Widget $ Box o ws

  instance Functor Box where
    fmap f (Box o ws) = Box o (fmap (fmap f) ws)

  instance MkWidget Box Gtk.Box where
    mkWidget (Box o ws) = do
      let
        newBox = case o of
          Horizontal -> \b i -> Gtk.toBox <$> Gtk.hBoxNew b i
          Vertical   -> \b i -> Gtk.toBox <$> Gtk.vBoxNew b i
      box <- liftIO $ newBox False 10
      ws' <- mapM mkWidget ws
      liftIO $ mapM_ (\w -> Gtk.boxPackStart box w Gtk.PackNatural 0) ws' --TODO make packing an attribute to Box
      return box



  -- Button --
  data Button msg = Button String (Maybe msg)

  mkButton :: String -> Maybe msg -> Widget msg
  mkButton s m = Widget $ Button s m

  instance Functor Button where
    fmap f (Button label msg) = Button label (fmap f msg)

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

  instance Functor Label where
    fmap _ = coerce

  instance MkWidget Label Gtk.Label where
    mkWidget (Label s) = liftIO $ Gtk.labelNew (Just s)

  -- Frame --
  data Frame msg = Frame (Maybe String) (Widget msg)

  mkFrame :: Maybe String -> Widget msg -> Widget msg
  mkFrame s w = Widget $ Frame s w

  instance Functor Frame where
    fmap f (Frame s w) = Frame s (fmap f w)

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
  mkText s c = Widget $ Text s (c & _Just._1 %~ Set.fromList)

  activateTextCallback :: Gtk.Entry -> TextEvent -> IO ()  -> IO ()
  activateTextCallback t e c = void $ case e of
    OnActivate  -> Gtk.on t Gtk.entryActivated              c
    OnFocusLost -> Gtk.on t Gtk.focusOutEvent . liftEvent $ c

  instance Functor Text where
    fmap f (Text s c) = Text s ((fmap . fmap . fmap $ f) c)

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
  data TextList a msg = TextList [a] (a -> String) (Maybe (a -> msg))

  mkTextList :: [a] -> (a -> String) -> Maybe (a -> msg) -> Widget msg
  mkTextList ts s c = Widget $ TextList ts s c

  instance Functor (TextList a) where
    fmap f (TextList ts s c) = TextList ts s ((fmap . fmap $ f) c)

  instance MkWidget (TextList a) Gtk.TreeView where
    mkWidget (TextList ts s f) = do
      ls <- liftIO $ Gtk.listStoreNew ts
      tv <- liftIO $ Gtk.treeViewNewWithModel (Gtk.toTreeModel ls)
      col <- liftIO Gtk.treeViewColumnNew
      cr <- liftIO Gtk.cellRendererTextNew

      liftIO $ Gtk.treeViewColumnPackStart col cr True
      _ <- liftIO $ Gtk.treeViewAppendColumn tv col

      liftIO $ Gtk.cellLayoutSetAttributes col cr ls (\ind -> [Gtk.cellText := s ind])
      liftIO $ Gtk.treeViewSetHeadersVisible tv False

      case f of
        Just f' -> do
          cb <- liftReader $ mkCallback (fmap (Just . f' . (ts !!) . head . fst) . Gtk.treeViewGetCursor) tv
          _ <- liftIO $ Gtk.on tv Gtk.cursorChanged cb
          return ()
        Nothing ->
          return ()

      return tv


  -- ScrolledWindow --
  data ScrolledWindow msg = ScrolledWindow (Widget msg)

  mkScrolledWindow :: Widget msg -> Widget msg
  mkScrolledWindow w = Widget $ ScrolledWindow w

  instance Functor ScrolledWindow where
    fmap f (ScrolledWindow w) = ScrolledWindow (fmap f w)

  instance MkWidget ScrolledWindow Gtk.ScrolledWindow where
    mkWidget (ScrolledWindow w) = do
      sw <- liftIO $ Gtk.scrolledWindowNew Nothing Nothing

      w' <- mkWidget w

      liftIO $ Gtk.scrolledWindowSetPolicy sw Gtk.PolicyAutomatic Gtk.PolicyAutomatic

      liftIO $ Gtk.containerAdd sw w'

      return sw


  -- used to store information about the gui while iterating
  type GuiState = Gtk.Window

  initGuiState :: ReaderT (GuiEnv msg) IO GuiState
  initGuiState = do
    _ <- liftIO Gtk.initGUI
    window <- liftIO Gtk.windowNew
    cb <- liftReader $ mkSimpleCallback Nothing
    _ <- liftIO $ Gtk.onDestroy window cb
    return window

  buildGui :: Widget msg -> Gui msg (MsgBox msg)
  buildGui ws = do
    window <- get
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

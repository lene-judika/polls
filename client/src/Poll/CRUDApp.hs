{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeFamilies #-}

module Poll.CRUDApp (Model, Msg, initModel, update, view) where
  import Poll
  import Poll.Read

  import Component
  import Command
  import Widget
  import qualified CRUDApp as CRUD

  import GHC.Generics

  import Data.Maybe
  import qualified Data.Map as Map

  import qualified Data.ByteString.Lazy as L

  import Control.Exception

  import Control.Applicative

  import Control.Monad
  import Control.Monad.IO.Class
  import Control.Monad.Trans.Except

  import Control.Lens hiding (view)

  import Text.Read

  import Network.HTTP hiding (password)
  import Network.Stream
  import Network.URI

  type Msg = CRUD.Msg PollDisplayMsg PollSelectorMsg PollChangerMsg Poll PID

  data PollSelector = PollSelector

  instance HasFormCallbacks PollSelector where
    type Unwrapped PollSelector = PollSelectorMsg
    type Wrapped   PollSelector = Msg
    type Return    PollSelector = PID
    wrapMsg   _ = CRUD.SelectorMsg
    abortMsg  _ = CRUD.Back
    returnMsg _ = CRUD.Read


  data PollChanger = PollChanger

  instance HasFormCallbacks PollChanger where
    type Unwrapped PollChanger = PollChangerMsg
    type Wrapped   PollChanger = Msg
    type Return    PollChanger = Poll
    wrapMsg   _ = CRUD.ChangerMsg
    abortMsg  _ = CRUD.Back
    returnMsg _ = CRUD.Send

  data PollDisplayModel = PollDisplayModel Poll

  data PollDisplayMsg

  instance Component PollDisplayModel where
    type Recv PollDisplayModel = PollDisplayMsg
    type Send PollDisplayModel = PollDisplayMsg
    type Init PollDisplayModel = Poll
    initModel p = (PollDisplayModel p, mempty)
    update p msg = (p, mempty)
    view (PollDisplayModel p) = mkBox Vertical
      [ mkLabel ("pid: "   ++ show (p^.pid))
      , mkLabel ("name: "  ++ show (p^.name))
      , mkLabel ("votes: " ++ show (p^.votes))
      ]

  data PollSelectorModel = PollSelectorModel (Maybe PID)

  data PollSelectorMsg = SelectorSetPID String

  instance Component PollSelectorModel where
    type Recv PollSelectorModel = PollSelectorMsg
    type Send PollSelectorModel = Msg
    type Init PollSelectorModel = Maybe PID
    initModel p = (PollSelectorModel p, mempty)
    update (PollSelectorModel p) (SelectorSetPID s) = (PollSelectorModel $ readMaybe s <|> p, mempty)
    view (PollSelectorModel p) = mkBox Vertical
      [ mkLabel "Enter PID"
      , mkText (maybe "" show p) (Just ([OnFocusLost], wrapMsg PollSelector . SelectorSetPID))
      , mkBox Horizontal
        [ mkButton "Ok" (fmap (returnMsg PollSelector) p)
        , mkButton "Back" (Just (abortMsg PollSelector))
        ]
      ]

  data PollChangerModel = PollChangerModel Poll

  data PollChangerMsg = ChangerSetPID String | ChangerSetName String | ChangerSetPassword String

  instance Component PollChangerModel where
    type Recv PollChangerModel = PollChangerMsg
    type Send PollChangerModel = Msg
    type Init PollChangerModel = Poll
    initModel p = (PollChangerModel p, mempty)

    update (PollChangerModel p) (ChangerSetPID s)      = (PollChangerModel $ p & pid  %~ flip fromMaybe (readMaybe s),            mempty)
    update (PollChangerModel p) (ChangerSetName s)     = (PollChangerModel $ p & name .~ s,                                       mempty)
    update (PollChangerModel p) (ChangerSetPassword s) = (PollChangerModel $ p & password .~ if s == "" then Nothing else Just s, mempty)

    view (PollChangerModel p) = mkBox Vertical
      [ mkText (show (p^.pid))              (Just ([OnFocusLost], wrapMsg PollChanger . ChangerSetPID))
      , mkText (p^.name)                    (Just ([OnFocusLost], wrapMsg PollChanger . ChangerSetName))
      , mkText (fromMaybe "" (p^.password)) (Just ([OnFocusLost], wrapMsg PollChanger . ChangerSetPassword))
      , mkBox Horizontal
        [ mkButton "Ok" (Just (returnMsg PollChanger p))
        , mkButton "Back" (Just (abortMsg PollChanger))
        ]
      ]

  data Model = Model
    { _crud :: CRUD.Model PollDisplayModel PollSelectorModel PollChangerModel Poll PID
    , _hostname :: String
    }
  makeLenses ''Model

  type Info = CRUD.Info Poll PID
  type Actions = CRUD.Actions Poll PID

  instance Component Model where
    type Recv Model = Msg
    type Send Model = Msg
    type Init Model = String
    initModel h = let (m, c) = CRUD.initModel (info h) in (Model m h, c)
    update (Model m h) msg = let (m', c) = CRUD.update m msg in (Model m' h, c)
    view (Model m h) = mkFrame (Just h) $ CRUD.view m

  info :: String -> Info
  info h = CRUD.Info (^.pid) (Poll 0 "" Map.empty Nothing) Nothing (actions h) (^.name)

  actions :: String -> Actions
  actions h = CRUD.Actions (const mempty) (readCmd h) (const mempty) (const mempty)

  readCmd :: String -> PID  -> Cmd (Either String Poll)
  readCmd h i = cmd . runExceptT $ send (h ++ "/polls/" ++ show i) GET [] "" readPoll

  createCmd :: String -> Poll -> Cmd (Either String Poll)
  createCmd h p = cmd . runExceptT $ send (h ++ "/polls") POST [] (createPollBdy p) createPoll

  handleError :: L.ByteString -> String
  handleError _ = "ERROR, TODO: FIXME" --TODO

  liftExceptT = ExceptT . return

  send :: String -> RequestMethod -> [Header] -> String -> (String -> Either String a) ->  ExceptT String IO a
  send h r hs bdy f = do
    uri <- ExceptT . return . maybe (Left $ "ParseError: Not a valid URI: " ++ h) Right . parseURI $ h
    let req = Request uri r hs bdy
    liftIO $ print req
    response <- join . fmap (withExceptT show . liftExceptT) . withExceptT (show :: IOException -> String) . ExceptT . try . simpleHTTP $ req -- TODO replace show, for better error messages
    --liftIO $ print response
    case rspCode response of
      (2,_,_) ->
        liftExceptT . f . rspBody $ response
      _ ->
        throwE . handleError . rspBody $ response

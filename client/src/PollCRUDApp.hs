{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeFamilies, DeriveGeneric, OverloadedStrings #-}

module PollCRUDApp (Model, Msg, initModel, update, view) where
  import Poll

  import Component
  import Command
  import Widget
  import qualified CRUDApp as CRUD
  import qualified VoteCRUDApp as VoteCRUD

  import GHC.Generics

  import Data.Maybe
  import qualified Data.Map as Map

  import Data.Semigroup

  import qualified Data.ByteString.Lazy as L
  import qualified Data.ByteString.Lazy.Char8 as C

  import Data.Aeson

  import Control.Applicative

  import Control.Lens hiding (view, (.=))

  import Text.Read



  data PollDisplayModel = PollDisplayModel Poll VoteCRUD.Model

  data PollDisplayMsg = VoteCRUDMsg VoteCRUD.Msg

  pollDisplay :: String -> Component m Poll PollDisplayModel PollDisplayMsg (CRUD.DisplayEvent PID)
  pollDisplay h = Component (displayInit h) displayUpdate displayView

  displayInit :: String -> Poll ->  PollDisplayModel
  displayInit h p = PollDisplayModel p ((voteComponent^.initModel) (h ++ "/polls/" ++ show (p^.pid)))
    --let (m',c) = initModel (h ++ "/polls/" ++ show (p^.pid)) in (PollDisplayModel p m', fmap VoteCRUDMsg c)
  update (PollDisplayModel p m) (VoteCRUDMsg msg) = let (m', c) = update m msg in (PollDisplayModel p m', fmap VoteCRUDMsg c)
  view (PollDisplayModel p m) = mkBox Horizontal
    [ mkBox Vertical
      [ mkLabel ("pid: "      ++ show (p^.pid))
      , mkLabel ("name: "     ++ show (p^.name))
      , mkLabel ("password: " ++ (show . fromMaybe "<unknown>" $ (p^.password)))
      , mkScrolledWindow $ mkTextList (Map.assocs (p^.votes)) (\(a,v) -> maybe "<deleted>" show a ++ ": " ++ show v) Nothing
      ]
      , VoteCRUDMsg <$> view m
    ]

  data PollSelectorModel = PollSelectorModel (Maybe PID)

  data PollSelectorMsg = SetPID String

  instance Component PollSelectorModel where
    type Recv PollSelectorModel = PollSelectorMsg
    type Send PollSelectorModel = Msg
    type Init PollSelectorModel = Maybe PID
    initModel p = (PollSelectorModel p, mempty)
    update (PollSelectorModel p) (SetPID s) = (PollSelectorModel $ readMaybe s <|> p, mempty)
    view m@(PollSelectorModel p) = mkBox Vertical
      [ mkLabel "Enter PID"
      , mkText (maybe "" show p) (Just ([OnFocusLost], wrapMsg m . SetPID))
      , mkBox Horizontal
        [ mkButton "Ok" (fmap (returnMsg m) p)
        , mkButton "Back" (Just (abortMsg m))
        ]
      ]



  data PollChangerMsg = SetName String | SetPassword String | Add | Remove | SetCursor (Maybe Appointment) | SetTemp String

  data PollChangerModel = PollChangerModel
    { _action :: CRUD.ChangeAction
    , _cursor :: Maybe (Maybe Appointment)
    , _new    :: String
    , _poll   :: Poll
    }
  makeLenses ''PollChangerModel

  instance Component PollChangerModel where
    type Recv PollChangerModel = PollChangerMsg
    type Send PollChangerModel = Msg
    type Init PollChangerModel = (Poll, CRUD.ChangeAction)
    initModel (p, m) = (PollChangerModel m Nothing "" p, mempty)

    update m (SetName s)     = (m & poll.name .~ s,                                       mempty)
    update m (SetPassword s) = (m & poll.password .~ if s == "" then Nothing else Just s, mempty)
    update m (SetCursor a)   = (m & cursor ?~ a, mempty)
    update m Add             = (maybe m (\k -> m & poll.votes %~ Map.insert (Just k) 0 ) (parseISO8601 (m^.new)), mempty)
    update m Remove          = (maybe m (\k -> m & poll.votes %~ Map.delete k) (m^.cursor), mempty)
    update m (SetTemp s)     = (m & new .~ s, mempty)

    view m = mkBox Vertical $
      (if (m^.action) == CRUD.Update then (mkLabel ("pid: " ++ show (m^.poll.pid)) :) else id)
      [ mkBox Horizontal [ mkLabel "Name: ",     mkText (m^.poll.name)                    (Just ([OnFocusLost], wrapMsg m . SetName))     ]
      , mkBox Horizontal [ mkLabel "Password: ", mkText (fromMaybe "" (m^.poll.password)) (Just ([OnFocusLost], wrapMsg m . SetPassword)) ]
      , mkFrame (Just "Appointments") $ mkBox Vertical
        [ mkScrolledWindow $ mkTextList (Map.keys (m^.poll.votes)) (maybe "<deleted>" showISO8601) (Just (wrapMsg m . SetCursor))
        , mkBox Horizontal [mkButton "Add"    (Just (wrapMsg m Add)), mkButton "Remove" (Just (wrapMsg m Remove)) ]
        , mkText (m^.new) (Just ([OnFocusLost], wrapMsg m . SetTemp))
        ]
      , mkBox Horizontal
        [ mkButton "Ok" (Just (returnMsg m (m^.poll)))
        , mkButton "Back" (Just (abortMsg m))
        ]
      ]


  --type Msg = CRUD.Msg PollDisplayMsg PollSelectorMsg PollChangerMsg Poll PID

  data Model = Model
    { _crud :: CRUD.Model PollDisplayModel PollSelectorModel PollChangerModel String Poll PID
    , _hostname :: String
    }
  makeLenses ''Model

  type Info = CRUD.Info String Poll PID
  type Actions = CRUD.Actions Poll PID

  instance Component Model where
    type Recv Model = Msg
    type Send Model = Msg
    type Init Model = String
    initModel h = let (m, c) = CRUD.initModel (info h) in (Model m h, c)
    update (Model m h) msg = let (m', c) = CRUD.update m msg in (Model m' h, c)
    view (Model m h) = mkFrame (Just h) $ CRUD.view m

  info :: String -> Info
  info h = CRUD.Info (^.pid) newPoll Nothing (actions h) (^.name) h

  actions :: String -> Actions
  actions h = CRUD.Actions (createCmd h) (readCmd h) (updateCmd h) (deleteCmd h)

  readCmd :: String -> PID  -> Cmd (Either String Poll)
  readCmd h i = cmd . runExceptT $ send (h ++ "/polls/" ++ show i) GET "" readPoll

  copyPassword :: Poll -> Poll -> Poll
  copyPassword p p' = case p^.password of
    Just pw -> p' & password ?~ pw
    Nothing -> p'

  createPollBody :: Poll -> String
  createPollBody = C.unpack . encode . (fromPoll :: Poll -> JsonPoll)

  data JsonPoll = JsonPoll String [String] String
    deriving (Generic)

  instance FromPoll JsonPoll where
    fromPoll (Poll _ name vs pw) = JsonPoll name (map showISO8601 . catMaybes . Map.keys $ vs) (fromMaybe "" pw)

  instance ToJSON JsonPoll where
    toEncoding (JsonPoll name as pw) =
      pairs
        (  "name"         .= name
        <> "appointments" .= as
        <> "password"     .= pw
        )


  passwordOnlyBody :: Poll -> String
  passwordOnlyBody = C.unpack . encode . (fromPoll :: Poll -> JsonPollPassword)

  data JsonPollPassword = JsonPollPassword String
    deriving (Generic)

  instance FromPoll JsonPollPassword where
    fromPoll (Poll _ _ _ pw) = JsonPollPassword (fromMaybe "" pw)

  instance ToJSON JsonPollPassword where
    toEncoding (JsonPollPassword pw) =
      pairs ("password" .= pw)
  createCmd :: String -> Poll -> Cmd (Either String Poll)
  createCmd h p = cmd . runExceptT . fmap (copyPassword p) $ send (h ++ "/polls") POST (createPollBody p) readPoll

  updateCmd :: String -> Poll -> Cmd (Either String Poll)
  updateCmd h p = cmd . runExceptT . fmap (copyPassword p) $ send (h ++ "/polls/" ++ show (p^.pid)) PUT (createPollBody p) readPoll

  deleteCmd :: String -> Poll -> Cmd (Either String ())
  deleteCmd h p = cmd . runExceptT $ send (h ++ "/polls/" ++ show (p^.pid)) DELETE (passwordOnlyBody p) (const (Right ()))

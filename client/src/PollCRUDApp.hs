{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeFamilies, DeriveGeneric, OverloadedStrings #-}

module PollCRUDApp
  ( pollCRUD
  , Model
  , Msg
  ) where
  import Poll
  import Appointment
  import Component
  import Command
  import Widget
  import Network
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
  import Control.Monad.Trans.Except

  import Control.Lens hiding (view, (.=))

  import Text.Read
  import Network.HTTP hiding (password)

  data DisplayModel = DisplayModel Poll VoteCRUD.Model

  data DisplayMsg = VoteCRUDMsg VoteCRUD.Msg | VoteEvent VoteCRUD.Event

  display :: String -> Component Cmd Poll DisplayModel DisplayMsg (CRUD.DisplayEvent PID)
  display h = Component (displayInit h) displayUpdate displayView

  displayInit :: String -> Poll ->  DisplayModel
  displayInit h p = DisplayModel p ((VoteCRUD.voteCRUD^.initModel) (h ++ "/polls/" ++ show (p^.pid)))

  displayUpdate :: DisplayModel -> DisplayMsg -> Action Cmd DisplayModel DisplayMsg (CRUD.DisplayEvent PID)
  displayUpdate (DisplayModel p m) (VoteCRUDMsg msg) = component m msg (DisplayModel p) VoteCRUDMsg VoteEvent (VoteCRUD.voteCRUD^.update)
  displayUpdate (DisplayModel p m) (VoteEvent _) = ActionEvent (CRUD.DEChanged (p^.pid))

  displayView (DisplayModel p m) = mkBox Horizontal
    [ mkBox Vertical
      [ mkLabel ("pid: "      ++ show (p^.pid))
      , mkLabel ("name: "     ++ show (p^.name))
      , mkLabel ("password: " ++ (show . fromMaybe "<unknown>" $ (p^.password)))
      , mkTextList (Map.assocs (p^.votes)) (\(a,v) -> maybe "<deleted>" show a ++ ": " ++ show v) Nothing
      ]
      , VoteCRUDMsg <$> (VoteCRUD.voteCRUD^.view) m
    ]

  data SelectorModel = SelectorModel (Maybe PID)

  data SelectorMsg = SetPID String | SendPID | CancelSelector

  selector :: Component Cmd (Maybe PID) SelectorModel SelectorMsg (CRUD.SelectorEvent PID)
  selector = Component SelectorModel selectorUpdate selectorView

  selectorUpdate :: SelectorModel -> SelectorMsg -> Action Cmd SelectorModel SelectorMsg (CRUD.SelectorEvent PID)
  selectorUpdate (SelectorModel p) (SetPID s) = ActionModel (SelectorModel $ readMaybe s <|> p)
  selectorUpdate (SelectorModel p) SendPID = maybe ActionIgnore (ActionEvent . CRUD.SESelect) p
  selectorUpdate (SelectorModel p) CancelSelector = ActionEvent CRUD.SEClose

  selectorView m@(SelectorModel p) = mkBox Vertical
    [ mkLabel "Enter PID"
    , mkText (maybe "" show p) (Just ([OnFocusLost], SetPID))
    , mkBox Horizontal
      [ mkButton "Ok"   (Just SendPID)
      , mkButton "Back" (Just CancelSelector)
      ]
    ]

  data ChangerMsg = SetName String | SetPassword String | AddAppointment | RemoveAppointment | SetCursor (Maybe Appointment) | SetAppointment String | SendPoll | CancelChanger

  data ChangerModel = ChangerModel
    { _cursor :: Maybe (Maybe Appointment)
    , _new    :: String
    , _poll   :: Poll
    , _action :: CRUD.ChangeAction
    }
  makeLenses ''ChangerModel

  changer :: Component Cmd (Poll, CRUD.ChangeAction) ChangerModel ChangerMsg (CRUD.ChangerEvent Poll)
  changer = Component (uncurry $ ChangerModel Nothing "") changerUpdate changerView

  changerUpdate m (SetName s)        = ActionModel $ m & poll.name     .~ s
  changerUpdate m (SetPassword s)    = ActionModel $ m & poll.password .~ if s == "" then Nothing else Just s
  changerUpdate m (SetCursor a)      = ActionModel $ m & cursor        ?~ a
  changerUpdate m (SetAppointment s) = ActionModel $ m & new           .~ s
  changerUpdate m AddAppointment     = maybe ActionIgnore (\k -> ActionModel $ m & poll.votes %~ Map.insert (Just k) 0 ) (readAppointmentMaybe (m^.new))
  changerUpdate m RemoveAppointment  = maybe ActionIgnore (\k -> ActionModel $ m & poll.votes %~ Map.delete k) (m^.cursor)
  changerUpdate m SendPoll           = case m^.action of
    CRUD.ActionCreate -> ActionEvent (CRUD.CECreate (m^.poll))
    CRUD.ActionUpdate -> ActionEvent (CRUD.CEUpdate (m^.poll))
  changerUpdate _ CancelChanger      = ActionEvent CRUD.CEClose

  changerView m = mkBox Vertical $
    (if (m^.action) == CRUD.ActionUpdate then (mkLabel ("pid: " ++ show (m^.poll.pid)) :) else id)
    [ mkBox Horizontal [ mkLabel "Name: ",     mkText (m^.poll.name)                    (Just ([OnFocusLost], SetName))     ]
    , mkBox Horizontal [ mkLabel "Password: ", mkText (fromMaybe "" (m^.poll.password)) (Just ([OnFocusLost], SetPassword)) ]
    , mkFrame (Just "Appointments") $ mkBox Vertical
      [ mkScrolledWindow $ mkTextList (Map.keys (m^.poll.votes)) (maybe "<deleted>" show) (Just SetCursor)
      , mkBox Horizontal [mkButton "Add"    (Just AddAppointment), mkButton "Remove" (Just RemoveAppointment) ]
      , mkText (m^.new) (Just ([OnFocusLost], SetAppointment))
      ]
    , mkBox Horizontal
      [ mkButton "Ok" (Just SendPoll)
      , mkButton "Back" (Just CancelChanger)
      ]
    ]


  data Msg = CRUDEvent | CRUDMsg (CRUD.Msg DisplayMsg SelectorMsg ChangerMsg Poll PID)

  data Model = Model String (CRUD.Model Cmd DisplayModel DisplayMsg SelectorModel SelectorMsg ChangerModel ChangerMsg Poll PID)

  type Info = CRUD.Info Cmd DisplayModel DisplayMsg SelectorModel SelectorMsg ChangerModel ChangerMsg Poll PID
  type Actions = CRUD.Actions Cmd Poll PID

  data Event

  pollCRUD :: Component Cmd String Model Msg Event
  pollCRUD = Component (\h -> Model h ((CRUD.crudComponent^.initModel) (info h))) pollUpdate pollView

  pollUpdate (Model h m) (CRUDMsg msg) = component m msg (Model h) CRUDMsg (const CRUDEvent) (CRUD.crudComponent^.update)
  pollView (Model h m) = mkFrame (Just h) $ CRUDMsg <$> (CRUD.crudComponent^.view) m

  info :: String -> Info
  info h = CRUD.Info (^.pid) newPoll Nothing (actions h) (^.name) (display h) selector changer

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
    fromPoll (Poll _ name vs pw) = JsonPoll name (map show . catMaybes . Map.keys $ vs) (fromMaybe "" pw)

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

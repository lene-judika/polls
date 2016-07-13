{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeFamilies, DeriveGeneric, OverloadedStrings #-}

module VoteCRUDApp
  ( voteCRUD
  , Model
  , Event
  , Msg
  ) where
  import Vote
  import Appointment
  import Network

  import Component
  import Command
  import Widget
  import qualified CRUDApp as CRUD

  import GHC.Generics

  import Data.Tuple
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
  import Network.HTTP

  data DisplayModel = DisplayModel Vote

  data DisplayMsg

  display :: Component m Vote DisplayModel DisplayMsg (CRUD.DisplayEvent VID)
  display = Component DisplayModel (\_ _ -> ActionIgnore) displayView

  displayView (DisplayModel v) = mkBox Vertical
    [ mkLabel ("VID: "         ++ show (v^.vid))
    , mkLabel ("Appointment: " ++ maybe "<deleted>" show (v^.appointment))
    ]

  data SelectorModel = SelectorModel { cursor :: Maybe VID }

  data SelectorMsg = SetVID String | SendVID | CancelSelector

  selector :: Component m (Maybe VID) SelectorModel SelectorMsg (CRUD.SelectorEvent VID)
  selector = Component SelectorModel selectorUpdate selectorView

  selectorUpdate m (SetVID s)     = ActionModel . SelectorModel $ readMaybe s <|> cursor m
  selectorUpdate m SendVID        = maybe ActionIgnore (ActionEvent . CRUD.SESelect) (cursor m)
  selectorUpdate _ CancelSelector = ActionEvent CRUD.SEClose

  selectorView m@(SelectorModel p) = mkBox Vertical
    [ mkLabel "Enter VID"
    , mkText (maybe "" show p) (Just ([OnFocusLost], SetVID))
    , mkBox Horizontal
      [ mkButton "Ok"   (Just SendVID)
      , mkButton "Back" (Just CancelSelector)
      ]
    ]



  data ChangerModel = ChangerModel
    { _action :: CRUD.ChangeAction
    , _vote   :: Vote
    }
  makeLenses ''ChangerModel

  data ChangerMsg = SetAppointment String | SendAppointment | CancelChanger

  changer :: Component m (Vote, CRUD.ChangeAction) ChangerModel ChangerMsg (CRUD.ChangerEvent Vote)
  changer = Component (uncurry ChangerModel . swap) changerUpdate changerView

  changerUpdate :: ChangerModel -> ChangerMsg -> Action m ChangerModel ChangerMsg (CRUD.ChangerEvent Vote)
  changerUpdate m (SetAppointment a) = ActionModel $ m & vote.appointment %~ (readAppointmentMaybe a <|>)
  changerUpdate m SendAppointment    = case (validate (m^.vote), m^.action) of
    (Just v, CRUD.ActionCreate) ->               ActionEvent $ CRUD.CECreate v
    (Just v, CRUD.ActionUpdate) ->               ActionEvent $ CRUD.CEUpdate v
    _ -> ActionIgnore
    where validate v = if isJust (v^.appointment) then Just v else Nothing
  changerUpdate m CancelChanger      = ActionEvent CRUD.CEClose

  changerView m = mkBox Vertical $
    (if (m^.action) == CRUD.ActionUpdate then (mkLabel ("VID: " ++ show (m^.vote.vid)) :) else id)
    [ mkBox Horizontal [ mkLabel "Appointment: ", mkText (maybe "<deleted>" show (m^.vote.appointment)) (Just ([OnFocusLost], SetAppointment)) ]
    , mkBox Horizontal
      [ mkButton "Ok"   (Just SendAppointment)
      , mkButton "Back" (Just CancelChanger)
      ]
    ]


  data Msg = UpdateEvent | CRUDMsg (CRUD.Msg DisplayMsg SelectorMsg ChangerMsg Vote VID)

  data Model = Model String (CRUD.Model Cmd DisplayModel DisplayMsg SelectorModel SelectorMsg ChangerModel ChangerMsg Vote VID)

  data Event = VoteUpdate

  type Info = CRUD.Info Cmd DisplayModel DisplayMsg SelectorModel SelectorMsg ChangerModel ChangerMsg Vote VID
  type Actions = CRUD.Actions Cmd Vote VID

  voteCRUD :: Component Cmd String Model Msg Event
  voteCRUD = Component (\s -> Model s . (CRUD.crudComponent^.initModel) . info $ s) voteUpdate voteView

  voteUpdate :: Model -> Msg -> Action Cmd Model Msg Event
  voteUpdate _ UpdateEvent = ActionEvent VoteUpdate
  voteUpdate (Model h m) (CRUDMsg msg) = component m msg (Model h) CRUDMsg (const UpdateEvent) (CRUD.crudComponent^.update)


  voteView :: Model -> Widget Msg
  voteView (Model h m) = mkFrame (Just h) $ CRUDMsg <$> (CRUD.crudComponent^.view) m

  info :: String -> Info
  info h = CRUD.Info (^.vid) newVote Nothing (actions h) (maybe "<deleted>" show . (^.appointment)) display selector changer

  actions :: String -> Actions
  actions h = CRUD.Actions (createCmd h) (readCmd h) (updateCmd h) (deleteCmd h)

  readCmd :: String -> VID  -> Cmd (Either String Vote)
  readCmd h i = cmd . runExceptT $ send (h ++ "/votes/" ++ show i) GET "" readVote

  voteBody :: Vote -> String
  voteBody = C.unpack . encode . fromVote

  data JsonVote = JsonVote String
    deriving (Generic)

  fromVote :: Vote -> JsonVote
  fromVote (Vote _ a) = JsonVote . show . fromJust $ a

  instance ToJSON JsonVote where
    toEncoding (JsonVote a) = pairs ("appointment" .= a)

  createCmd :: String -> Vote -> Cmd (Either String Vote)
  createCmd h v = cmd . runExceptT $ send (h ++ "/votes") POST (voteBody v) readVote

  updateCmd :: String -> Vote -> Cmd (Either String Vote)
  updateCmd h v = cmd . runExceptT $ send (h ++ "/votes/" ++ show (v^.vid)) PUT (voteBody v) readVote

  deleteCmd :: String -> Vote -> Cmd (Either String ())
  deleteCmd h v = cmd . runExceptT $ send (h ++ "/votes/" ++ show (v^.vid)) DELETE (voteBody v) (const (Right ()))

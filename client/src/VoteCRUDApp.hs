{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeFamilies, DeriveGeneric, OverloadedStrings #-}

module VoteCRUDApp (Model, Msg, initModel, update, view) where
  import Vote

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

  import Control.Lens hiding (view, (.=))

  import Text.Read

  data DisplayModel = DisplayModel Vote

  data DisplayMsg

  display :: Component m Vote DisplayModel DisplayMsg (CRUD.DisplayEvent VID)
  display = Component DisplayModel ActionIgnore displayView

  displayView (DisplayModel v) = mkBox Vertical
    [ mkLabel ("VID: "         ++ show (v^.vid))
    , mkLabel ("Appointment: " ++ show (v^.appointment))
    ]

  data SelectorModel = SelectorModel { cursor :: Maybe VID }

  data SelectorMsg = SetVID String | SendVID | CancelSelector

  selector :: Component m (Maybe VID) SelectorModel SelectorMsg (CRUD.SelectorEvent VID)
  selector = Component SelectorModel selectorUpdate selectorView

  selectorUpdate m (SetVID s)     = ActionModel . SelectorModel $ readMaybe s <|> cursor m
  selectorUpdate m SendVID        = maybe ActionIgnore (ActionEvent CRUD.SESelect) (cursor m)
  selectorUpdate _ CancelSelector = ActionEvent CRUD.SEClose

  selectorView m@(SelectorModel p) = mkBox Vertical
    [ mkLabel "Enter VID"
    , mkText (maybe "" show p) (Just ([OnFocusLost], SetVID))
    , mkBox Horizontal
      [ mkButton "Ok"   SendVID
      , mkButton "Back" CancelSelector
      ]
    ]



  data ChangerModel = ChangerModel
    { _action :: CRUD.ChangeAction
    , _vote   :: Vote
    }
  makeLenses ''ChangerModel

  data ChangerMsg = SetAppointment String | SendAppointment | CancelChanger

  changer :: Component m (Vote, CRUD.ChangeAction) ChangerModel ChangerMsg CRUD.ChangerEvent
  changer = Component (uncurry ChangerModel . swap) changerUpdate changerView

  changerUpdate m (SetAppointment a) = ActionModel $ m & vote.appointment %~ flip fromMaybe (readAppointment a)
  changerUpdate m SendAppointment    = case m^.action of
    CRUD.ActionCreate ->               ActionEvent $ CRUD.CECreate (m^.vote)
    CRUD.ActionUpdate ->               ActionEvent $ CRUD.CEUpdate (m^.vote)
  changerUpdate m CancelChanger      = ActionEvent CRUD.CEClose

  changerView m = mkBox Vertical $
    (if (m^.action) == CRUD.ActionUpdate then (mkLabel ("VID: " ++ show (m^.vote.vid)) :) else id)
    [ mkBox Horizontal [ mkLabel "Appointment: ", mkText (show (m^.vote.appointment)) (Just ([OnFocusLost], wrapMsg m . SetAppointment))     ]
    , mkBox Horizontal
      [ mkButton "Ok" (Just (returnMsg m (m^.vote)))
      , mkButton "Back" (Just (abortMsg m))
      ]
    ]


  newtype Msg = Msg (CRUD.Msg DisplayMsg SelectorMsg ChangerMsg Vote VID)

  newtype Model = Model (CRUD.Model DisplayModel DisplayMsg SelectorModel SelectorMsg ChangerModel ChangerMsg Vote VID)

  data Event = Event

  -- type Info = CRUD.Info Vote VID
  -- type Actions = CRUD.Actions Vote VID

  voteCRUD :: Component m String Model Msg Event
  voteCRUD = Component (Model . info)

  voteUpdate m msg = component m msg Model Msg ...
    --let (m', c) = CRUD.update m msg in (Model m' h, c)
  voteView (Model m h) = mkFrame (Just h) $ (CRUD.crudComponent^.view) m

  info :: String -> Info
  info h = CRUD.Info (^.vid) newVote Nothing (actions h) (show . (^.appointment))

  actions :: String -> Actions
  actions h = CRUD.Actions (createCmd h) (readCmd h) (updateCmd h) (deleteCmd h)

  readCmd :: String -> VID  -> Cmd (Either String Vote)
  readCmd h i = cmd . runExceptT $ send (h ++ "/votes/" ++ show i) GET "" readVote

  voteBody :: Vote -> String
  voteBody = C.unpack . encode . fromVote

  data JsonVote = JsonVote String
    deriving (Generic)

  fromVote :: Vote -> JsonVote
  fromVote (Vote _ a) = JsonVote . showISO8601 $ a

  instance ToJSON JsonVote where
    toEncoding (JsonVote a) = pairs ("appointment" .= a)

  createCmd :: String -> Vote -> Cmd (Either String Vote)
  createCmd h v = cmd . runExceptT $ send (h ++ "/votes") POST (voteBody v) readVote

  updateCmd :: String -> Vote -> Cmd (Either String Vote)
  updateCmd h v = cmd . runExceptT $ send (h ++ "/votes/" ++ show (v^.vid)) PUT (voteBody v) readVote

  deleteCmd :: String -> Vote -> Cmd (Either String ())
  deleteCmd h v = cmd . runExceptT $ send (h ++ "/votes/" ++ show (v^.vid)) DELETE (voteBody v) (const (Right ()))

  handleError :: String -> String
  handleError = id

  liftExceptT = ExceptT . return

  send :: String -> RequestMethod -> String -> (String -> Either String a) ->  ExceptT String IO a
  send h r body f = do
    uri <- ExceptT . return . maybe (Left $ "ParseError: Not a valid URI: " ++ h) Right . parseURI $ h
    let req = setRequestBody (mkRequest r uri) ("application/json", body)
    liftIO . print $ req
    liftIO . putStrLn . rqBody $ req
    response <- join . fmap (withExceptT show . liftExceptT) . withExceptT (show :: IOException -> String) . ExceptT . try . simpleHTTP $ req -- TODO replace show, for better error messages
    liftIO . print $ response
    liftIO . putStrLn . rspBody $ response
    case rspCode response of
      (2,_,_) ->
        liftExceptT . f . rspBody $ response
      _ ->
        throwE . handleError . rspBody $ response

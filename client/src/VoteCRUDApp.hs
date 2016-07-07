{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeFamilies, DeriveGeneric, OverloadedStrings #-}

module VoteCRUDApp (Model, Msg, initModel, update, view) where
  import Vote
  import Poll

  import Component
  import Command
  import Widget
  import qualified CRUDApp as CRUD

  import GHC.Generics

  import Data.Maybe
  import qualified Data.Map as Map

  import Data.Semigroup

  import qualified Data.ByteString.Lazy as L
  import qualified Data.ByteString.Lazy.Char8 as C

  import Data.Aeson

  import Control.Exception

  import Control.Applicative

  import Control.Monad
  import Control.Monad.IO.Class
  import Control.Monad.Trans.Except

  import Control.Lens hiding (view, (.=))

  import Text.Read

  import Network.HTTP hiding (password)
  import Network.Stream
  import Network.URI

  type Msg = CRUD.Msg VoteDisplayMsg VoteSelectorMsg VoteChangerMsg Vote VID

  data VoteDisplayModel = VoteDisplayModel Vote

  data VoteDisplayMsg

  instance Component VoteDisplayModel where
    type Recv VoteDisplayModel = VoteDisplayMsg
    type Send VoteDisplayModel = VoteDisplayMsg
    type Init VoteDisplayModel = (Vote, ())
    initModel (v,()) = (VoteDisplayModel v, mempty)
    update v msg = (v, mempty)
    view (VoteDisplayModel v) = mkBox Vertical
      [ mkLabel ("vid: "         ++ show (v^.vid))
      , mkLabel ("appointment: " ++ show (v^.appointment))
      ]

  data VoteSelectorModel = VoteSelectorModel (Maybe VID)

  data VoteSelectorMsg = SetVID String

  instance HasFormCallbacks VoteSelectorModel where
    type Unwrapped VoteSelectorModel = Recv VoteSelectorModel
    type Wrapped VoteSelectorModel   = Send VoteSelectorModel
    type Return  VoteSelectorModel   = VID
    wrapMsg   _ = CRUD.SelectorMsg
    abortMsg  _ = CRUD.Back
    returnMsg _ = CRUD.Read

  instance Component VoteSelectorModel where
    type Recv VoteSelectorModel = VoteSelectorMsg
    type Send VoteSelectorModel = Msg
    type Init VoteSelectorModel = Maybe VID
    initModel p = (VoteSelectorModel p, mempty)
    update (VoteSelectorModel p) (SetVID s) = (VoteSelectorModel $ readMaybe s <|> p, mempty)
    view m@(VoteSelectorModel p) = mkBox Vertical
      [ mkLabel "Enter VID"
      , mkText (maybe "" show p) (Just ([OnFocusLost], wrapMsg m . SetVID))
      , mkBox Horizontal
        [ mkButton "Ok" (fmap (returnMsg m) p)
        , mkButton "Back" (Just (abortMsg m))
        ]
      ]


  data VoteChangerMsg = SetAppointment String

  data VoteChangerModel = VoteChangerModel
    { _action :: CRUD.ChangeAction
    , _vote   :: Vote
    }
  makeLenses ''VoteChangerModel

  instance HasFormCallbacks VoteChangerModel where
    type Unwrapped VoteChangerModel = Recv VoteChangerModel
    type Wrapped VoteChangerModel   = Send VoteChangerModel
    type Return  VoteChangerModel   = Vote
    wrapMsg   _ = CRUD.ChangerMsg
    abortMsg  _ = CRUD.Back
    returnMsg _ = CRUD.Send

  instance Component VoteChangerModel where
    type Recv VoteChangerModel = VoteChangerMsg
    type Send VoteChangerModel = Msg
    type Init VoteChangerModel = (Vote, CRUD.ChangeAction)
    initModel (v, m) = (VoteChangerModel m v, mempty)

    update m (SetAppointment s) = (m & vote.appointment %~ flip fromMaybe (parseISO8601 s), mempty)

    view m = mkBox Vertical $
      (if (m^.action) == CRUD.Update then (mkLabel ("vid: " ++ show (m^.vote.vid)) :) else id)
      [ mkBox Horizontal [ mkLabel "Appointment: ", mkText (showISO8601 (m^.vote.appointment)) (Just ([OnFocusLost], wrapMsg m . SetAppointment))     ]
      , mkBox Horizontal
        [ mkButton "Ok" (Just (returnMsg m (m^.vote)))
        , mkButton "Back" (Just (abortMsg m))
        ]
      ]

  data Model = Model
    { _crud :: CRUD.Model VoteDisplayModel VoteSelectorModel VoteChangerModel () Vote VID
    , _hostname :: String
    }
  makeLenses ''Model

  type Info = CRUD.Info () Vote VID
  type Actions = CRUD.Actions Vote VID

  instance Component Model where
    type Recv Model = Msg
    type Send Model = Msg
    type Init Model = String
    initModel h = let (m, c) = CRUD.initModel (info h) in (Model m h, c)
    update (Model m h) msg = let (m', c) = CRUD.update m msg in (Model m' h, c)
    view (Model m h) = mkFrame (Just h) $ CRUD.view m

  info :: String -> Info
  info h = CRUD.Info (^.vid) newVote Nothing (actions h) (\a -> showISO8601 (a^.appointment)) ()

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

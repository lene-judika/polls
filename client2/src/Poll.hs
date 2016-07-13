{-# LANGUAGE TemplateHaskell, DeriveGeneric, TupleSections, OverloadedStrings #-}

module Poll
  ( Poll(Poll)
  , pid
  , name
  , votes
  , password
  , newPoll
  , FromPoll(..)
  , PID
  , mkVotes
  , readPoll
  ) where

  import Appointment
  import Network

  import GHC.Generics

  import Data.String
  import Data.Maybe

  import Data.Map(Map)
  import qualified Data.Map as Map

  import qualified Data.ByteString.Lazy as L

  import Data.Aeson hiding (Error, Result)
  import Data.Aeson.Types(typeMismatch)

  import Control.Applicative

  import Control.Lens

  type PID = Int

  data Poll = Poll
    { _pid      :: PID
    , _name     :: String
    , _votes    :: Map (Maybe Appointment) Int
    , _password :: Maybe String
  }
  makeLenses ''Poll

  class FromPoll p where
    fromPoll :: Poll -> p

  newPoll :: Poll
  newPoll = Poll 0 "" (Map.singleton (readAppointmentMaybe "2016-10-10") 0) Nothing

  mkVotes :: [String] -> Map String Int -> Either String (Map (Maybe Appointment) Int)
  mkVotes as vs = do
    as' <- mapM readAppointmentEither as
    let vs' = Map.mapKeys readAppointmentMaybe vs
    return (mkVotes' as' vs')

  -- filter map keys, only keep the ones from the List
  mkVotes' :: [Appointment] -> Map (Maybe Appointment) Int -> Map (Maybe Appointment) Int
  mkVotes' as vs = Map.union vs (Map.fromList (zip (map Just as) (repeat 0)))


  readPoll :: String -> Either String Poll
  readPoll msg = do
    p <- eitherDecode $ fromString msg :: Either String JsonPollResp
    toPoll p


  data JsonPollResp = JsonPollResp
    { _JsonPollRespPid :: Int
    , _JsonPollRespName :: String
    , _JsonPollRespAppointments :: [String]
    , _JsonPollRespVotes :: Map String Int
    } deriving (Generic, Show)

  toPoll :: JsonPollResp -> Either String Poll
  toPoll (JsonPollResp p n as vs) = do
    votes <- mkVotes as vs
    return $ Poll p n votes Nothing

  instance FromJSON JsonPollResp where
    parseJSON (Object v) = JsonPollResp <$>
      v .: "PID" <*>
      v .: "name" <*>
      v .: "appointments" <*>
      v .: "votes"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON invalid = typeMismatch "Poll" invalid

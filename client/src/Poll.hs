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
  , Appointment
  , parseISO8601
  , showISO8601
  , mkVotes
  , readPoll
  ) where

  import GHC.Generics

  import Data.String
  import Data.Maybe

  import Data.Map(Map)
  import qualified Data.Map as Map

  import qualified Data.ByteString.Lazy as L

  import Data.Aeson hiding (Error, Result)
  import Data.Aeson.Types(typeMismatch)

  import Data.Time.Clock
  import Data.Time.Format

  import Control.Applicative

  import Control.Lens

  type PID = Int

  type Appointment = UTCTime

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
  newPoll = Poll 0 "" (Map.singleton (parseISO8601 "2016-10-10") 0) Nothing

  parseISO8601 :: String -> Maybe Appointment
  parseISO8601 t = parseTimeM False defaultTimeLocale "%Y-%m-%d" t <|>
                   parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M" t

  showISO8601 :: Appointment -> String
  showISO8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M"

  mkAppointment :: String -> Either String (Maybe Appointment)
  mkAppointment "" = return Nothing
  mkAppointment s  = Just <$> maybe (Left $ "ParseError: invalid ISO8601-Date: " ++ s) Right (parseISO8601 s)

  mkVotes :: [String] -> Map String Int -> Either String (Map (Maybe Appointment) Int)
  mkVotes as vs = do
    as' <- catMaybes <$> mapM mkAppointment as
    vs' <- Map.fromList <$> mapM (\(s,i) -> (,i) <$> mkAppointment s ) (Map.assocs vs)
    return (mkVotes' as' vs')

  -- filter map keys, only keep the ones from the List
  mkVotes' :: [Appointment] -> Map (Maybe Appointment) Int -> Map (Maybe Appointment) Int
  mkVotes' as m = Map.union m (Map.fromList (zip (map Just as) (repeat 0)))


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

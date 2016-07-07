{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Poll
  ( Poll(Poll)
  , pid
  , name
  , votes
  , password
  , ToPoll(toPoll)
  , PID
  , Appointment
  , parseISO8601
  , mkVotes
  ) where


  import Data.Maybe

  import Data.Map(Map)
  import qualified Data.Map as Map

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


  class ToPoll p where
    toPoll :: p -> Either String Poll



  parseISO8601 :: String -> Maybe Appointment
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

  -- filter map keys, only keep the ones from the List
  mkVotes' :: [Appointment] -> Map (Maybe Appointment) Int -> Map (Maybe Appointment) Int
  mkVotes' as m = Map.union m (Map.fromList (zip (map Just as) (repeat 0)))

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Poll.Read
  (readPoll
  ) where

  import Poll

  import GHC.Generics

  import Data.Map(Map)

  import Data.String

  import qualified Data.ByteString.Lazy as L

  import Data.Aeson hiding (Error, Result)
  import Data.Aeson.Types(typeMismatch)

  readPoll :: String -> Either String Poll
  readPoll msg = do
    p <- eitherDecode $ fromString msg :: Either String JsonPollGetResp
    toPoll p


  data JsonPollGetResp = JsonPollGetResp
    { _JsonPollGetRespPid :: Int
    , _JsonPollGetRespName :: String
    , _JsonPollGetRespAppointments :: [String]
    , _JsonPollGetRespVotes :: Map String Int
    } deriving (Generic, Show)

  instance ToPoll JsonPollGetResp where
    toPoll (JsonPollGetResp p n as vs) = do
      votes <- mkVotes as vs
      return $ Poll p n votes Nothing

  instance FromJSON JsonPollGetResp where
      parseJSON (Object v) = JsonPollGetResp <$>
        v .: "PID" <*>
        v .: "name" <*>
        v .: "appointments" <*>
        v .: "votes"
      -- A non-Object value is of the wrong type, so fail.
      parseJSON invalid = typeMismatch "Poll" invalid

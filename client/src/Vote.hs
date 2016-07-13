{-# LANGUAGE TemplateHaskell, DeriveGeneric, OverloadedStrings #-}

module Vote
  ( Vote(Vote)
  , vid
  , appointment
  , newVote
  , readVote
  , VID
  ) where

  import Poll
  import Appointment

  import GHC.Generics

  import Data.String

  import Data.Aeson hiding (Error, Result)
  import Data.Aeson.Types(typeMismatch)

  import Control.Lens


  type VID = Int

  data Vote = Vote
    { _vid :: VID
    , _appointment :: Maybe Appointment
    }
  makeLenses ''Vote

  newVote :: Vote
  newVote = Vote 0 (Just $ mkAppointment 2016 1 1)

  readVote :: String -> Either String Vote
  readVote msg = do
    v <- eitherDecode $ fromString msg :: Either String JsonVoteResp
    toVote v

  data JsonVoteResp = JsonVoteResp
    { _JsonVoteRespVid :: Int
    , _JsonPollRespAppointment :: String
    } deriving (Generic, Show)

  toVote :: JsonVoteResp -> Either String Vote
  toVote (JsonVoteResp v a) = Vote v <$> if a == "" then Right Nothing else Just <$> readAppointmentEither a

  instance FromJSON JsonVoteResp where
    parseJSON (Object v) = JsonVoteResp <$>
      v .: "VID" <*>
      v .: "appointment"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON invalid = typeMismatch "Poll" invalid

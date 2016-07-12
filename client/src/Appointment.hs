module Appointment (Appointment(), mkAppointment, readAppointmentMaybe, readAppointmentEither, showAppointment) where
  import Data.Time.Calendar
  import Data.Time.Clock
  import Data.Time.Format

  import Control.Applicative

  newtype Appointment = Appointment { unappointment :: UTCTime }
    deriving (Eq, Ord)


  mkAppointment :: Integer -> Int -> Int -> Appointment
  mkAppointment y m d = Appointment $ UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

  readAppointmentEither :: String -> Either String Appointment
  readAppointmentEither s = maybe (Left $ "ParseError: invalid ISO8601-Date: " ++ s) Right (readAppointmentMaybe s)

  readAppointmentMaybe:: String -> Maybe Appointment
  readAppointmentMaybe s = Appointment <$>
    ( parseTimeM False defaultTimeLocale "%Y-%m-%d" s <|>
    parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M" s
    )

  showAppointment :: Appointment -> String
  showAppointment = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" . unappointment

  instance Show Appointment where
    show = showAppointment

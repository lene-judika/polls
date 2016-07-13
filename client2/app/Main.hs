module Main (main) where
  import CommandLine
  import Network

  import Control.Monad

  import System.Environment
  import Control.Monad.Trans.Class
  import Control.Monad.IO.Class

  import Text.Read hiding (lift)

  data Method = Get | Post | Put | Delete
    deriving (Eq, Read, Show)

  data Target = Poll | Vote
    deriving(Eq, Read, Show)

  main = do
    args <- getArgs
    res <- runCommandLine cmd args
    either (putStrLn . ("Error: " ++)) return res

  convert :: Method -> RequestMethod
  convert Get    = GET
  convert Post   = POST
  convert Put    = PUT
  convert Delete = DELETE

  cmd :: CommandLine ()
  cmd = do
    rawHost <- getArg "Host"

    rawMethod <- getArg "Request Method"
    method <- liftMaybe ("Invalid Method: " ++ rawMethod) (readMaybe rawMethod)

    rawTarget <- getArg "Target"
    target <- liftMaybe ("Invalid Target: " ++ rawTarget) (readMaybe rawTarget)

    when (target == Vote && method == Get) $ do
      liftIO $ putStrLn "GET on Votes is not supported"
      return ()
    unless (target == Vote && method == Get) $ do
      pidPath <- fmap ("/appointments" ++) $ case (method, target) of
        (m, t) | m /= Post || t == Vote -> do
          pid <- getArg "PID"
          return ("/" ++ pid)
        _ -> return ""

      let pidHost = rawHost ++ pidPath

      vidPath <- case (method, target) of
        (m, t) | m /= Post && t == Vote -> do
          vid <- getArg "VID"
          return ("/votes/" ++ vid)
        _ -> return ""

      let vidHost = pidHost ++ vidPath

      host <- liftMaybe ("Invalid host: " ++ vidHost) (parseURI vidHost)

      token <- case method of
        m | m == Put || m == Delete -> do
          token <- getArg "Token"
          return (Just token)
        _ -> return Nothing

      json <- getArg "Data"

      liftIO $ print vidHost
      resp <- lift $ send host (convert method) token json
      liftIO $ putStrLn resp

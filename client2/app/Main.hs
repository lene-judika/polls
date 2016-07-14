module Main (main) where
  import CommandLine
  import Network

  import Control.Monad

  import System.Environment
  import Control.Monad.Trans.Class
  import Control.Monad.IO.Class
  import Control.Monad.Except

  import Text.Read hiding (lift)

  data Method = Get | Post | Put | Delete
    deriving (Eq, Read, Show)

  data Target = Poll | Vote
    deriving(Eq, Read, Show)

  main = do
    args <- getArgs
    res <- runCommandLine args cmd
    either (putStrLn . ("Error: " ++)) return res

  convert :: Method -> RequestMethod
  convert Get    = GET
  convert Post   = POST
  convert Put    = PUT
  convert Delete = DELETE

  cmd :: CommandLine ()
  cmd = do
    rawHost <- getArg "Host"
    method <- readArg "Request-Method"
    target <- readArg "Target"

    if target == Vote && method == Get then
      throwError "Get on Votes is not supported"
    else do
      pidPath <- fmap ("/appointments" ++) $ if method /= Post || target == Vote then do
        pid <- getArg "PID"
        return ("/" ++ pid)
      else
        return ""

      vidPath <- if method /= Post && target == Vote then do
        vid <- getArg "VID"
        return ("/votes/" ++ vid)
      else
        return ""

      let rawHost' = rawHost ++ pidPath ++ vidPath

      host <- unwrap (parseURI rawHost') ("Invalid host: " ++ show rawHost')

      token <- if method == Put || method == Delete then
        Just <$> getArg "Token"
      else
        return Nothing

      json <- if method == Put || method == Post then
        getArg "Data"
      else
        return ""

      resp <- lift $ send host (convert method) token json
      liftIO $ putStrLn resp

module Network (send) where

  import Control.Exception
  import Control.Monad
  import Control.Monad.IO.Class
  import Control.Monad.Trans.Except

  import Network.HTTP hiding (password)
  import Network.Stream
  import Network.URI

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

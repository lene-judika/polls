module Network (send) where

  import Control.Exception
  import Control.Monad
  import Control.Monad.IO.Class
  import Control.Monad.Trans.Except

  import Network.HTTP
  import Network.Stream
  import Network.URI

  handleError :: String -> String
  handleError = id

  liftExceptT = ExceptT . return

  send :: String -> RequestMethod -> Maybe String -> String -> (String -> Either String a) ->  ExceptT String IO a
  send h r token body f = do
    uri <- ExceptT . return . maybe (Left $ "ParseError: Not a valid URI: " ++ h) Right . parseURI $ h
    let req = maybe id (\t -> insertHeader HdrAuthorization ("Token token=" ++ t)) token $ setRequestBody (mkRequest r uri) ("application/json", body)
    liftIO . putStrLn . ("DEBUG: request: " ++) . show $ req
    liftIO . putStrLn . ("DEBUG: " ++) . rqBody $ req
    response <- join . fmap (withExceptT show . liftExceptT) . withExceptT (show :: IOException -> String) . ExceptT . try . simpleHTTP $ req -- TODO replace show, for better error messages
    liftIO . putStrLn . ("DEBUG: response: " ++) . show $ response
    liftIO . putStrLn . ("DEBUG: " ++) . rspBody $ response
    case rspCode response of
      (2,_,_) ->
        liftExceptT . f . rspBody $ response
      _ ->
        throwE . handleError . rspBody $ response

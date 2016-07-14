module Network
  ( send
  , parseURI
  , parseRelativeReference
  , URI
  , relativeTo
  , RequestMethod(..)
  ) where

  import Control.Exception
  import Control.Monad
  import Control.Monad.IO.Class
  import Control.Monad.Except

  import Network.HTTP
  import Network.Stream
  import Network.URI

  liftExceptT = ExceptT . return

  a =. b = (a, b)

  hdrAccept = mkHeader HdrAccept "application/json"

  headers t = maybe id ((:) . mkHeader HdrAuthorization) t [hdrAccept]

  send :: URI -> RequestMethod -> Maybe String -> String -> ExceptT String IO String
  send uri r token body = do
    let req = insertHeaders (headers token) $ setRequestBody (mkRequest r uri) ("application/json", body)
    -- liftIO . putStrLn . ("request: " ++) . show $ req
    -- liftIO . putStrLn . rqBody $ req
    response <- join . fmap (withExceptT show . liftExceptT) . withExceptT (show :: IOException -> String) . ExceptT . try . simpleHTTP $ req -- TODO replace show, for better error messages
    -- liftIO . putStrLn . ("response: " ++) . show $ response
    -- liftIO . putStrLn . rspBody $ response
    case rspCode response of
      (2,_,_) ->
        return . rspBody $ response
      _ ->
        throwError . rspBody $ response

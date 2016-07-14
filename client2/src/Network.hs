module Network
  ( send
  , parseURI
  , parseRelativeReference
  , URI
  , relativeTo
  , RequestMethod(..)
  , request
  , Request_String
  , liftExceptT
  ) where

  import Data.Functor.Identity

  import Control.Exception
  import Control.Monad
  import Control.Monad.IO.Class
  import Control.Monad.Except

  import Network.HTTP
  import Network.Stream
  import Network.URI

  hdrAccept :: Header
  hdrAccept = mkHeader HdrAccept "application/json"

  headers :: Maybe String -> [Header]
  headers t = maybe id ((:) . mkHeader HdrAuthorization) t [hdrAccept]

  liftExceptT :: Either e a -> ExceptT e IO a
  liftExceptT = ExceptT . return

  request :: URI -> RequestMethod -> Maybe String -> String -> Request_String
  request uri r token body = insertHeaders (headers token) $ setRequestBody (mkRequest r uri) ("application/json", body)

  send :: Request_String -> ExceptT String IO String
  send req = do
    response <- join . fmap (withExceptT show . liftExceptT) . withExceptT (show :: IOException -> String) . ExceptT . try . simpleHTTP $ req -- TODO replace show, for better error messages
    case rspCode response of
      (2,_,_) ->
        return . rspBody $ response
      _ ->
        throwError . rspBody $ response

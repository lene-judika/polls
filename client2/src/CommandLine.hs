{-# LANGUAGE FlexibleContexts #-}

module CommandLine
  ( CommandLine
  , getArg
  , readArg
  , runCommandLine
  , unwrap
  ) where

  import Control.Monad.Except
  import Control.Monad.State

  import Safe

  import Text.Read (readMaybe)

  type CommandLine = StateT [String] (ExceptT String IO)

  runCommandLine :: [String] -> CommandLine a -> IO (Either String a)
  runCommandLine args cmd = runExceptT $ evalStateT cmd args

  unwrap :: MonadError String m => Maybe a -> String -> m a
  unwrap m s = maybe (throwError s) return m

  getArg :: String -> CommandLine String
  getArg name = do
    args <- get
    (arg, args') <- unwrap ((,) <$> headMay args <*> tailMay args) ("Missing argument: " ++ show name)
    put args'
    return arg

  readArg :: Read a => String -> CommandLine a
  readArg name = do
    arg <- getArg name
    unwrap (readMaybe arg) ("Invalid " ++ name ++ ": " ++ show arg) 

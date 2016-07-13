{-# LANGUAGE FlexibleContexts #-}

module CommandLine
  ( CommandLine
  , getArg
  , runCommandLine
  , liftMaybe
  ) where

  import Safe

  import Control.Monad.Except
  import Control.Monad.State

  type CommandLine = StateT [String] (ExceptT String IO)

  runCommandLine :: CommandLine a -> [String] -> IO (Either String a)
  runCommandLine cmd = runExceptT . evalStateT cmd

  liftMaybe :: MonadError String m => String -> Maybe a -> m a
  liftMaybe s = maybe (throwError s) return

  getArg :: String -> CommandLine String
  getArg s = do
    args <- get
    (arg, args') <- liftMaybe ("Argument \"" ++ s ++ "\" missing") ((,) <$> headMay args <*> tailMay args)
    put args'
    return arg

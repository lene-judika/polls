{-# LANGUAGE FlexibleContexts, DeriveFunctor #-}

module CommandLine
  ( CommandLine
  , getArg
  , readArg
  , runCommandLine
  , unwrap
  , abort
  ) where

  import Control.Monad.Except
  import Control.Monad.State

  import Control.Monad.Free

  import Text.Read (readMaybe)

  data CommandLineF a =
      GetArg String (String -> a)
    | Abort String
    deriving(Functor)

  type CommandLine = Free CommandLineF

  runCommandLine :: [String] -> CommandLine a -> Either String a
  runCommandLine _      (Pure a)               = Right a
  runCommandLine []     (Free (GetArg name _)) = Left ("Missing argument: " ++ show name)
  runCommandLine (a:as) (Free (GetArg name f)) = runCommandLine as $ f a
  runCommandLine _      (Free (Abort reason))  = Left reason

  getArg :: String -> CommandLine String
  getArg name = liftF $ GetArg name id

  abort :: String -> CommandLine a
  abort s = liftF $ Abort s

  unwrap :: Maybe a -> String -> CommandLine a
  unwrap m s = maybe (abort s) return m

  readArg :: Read a => String -> CommandLine a
  readArg name = do
    arg <- getArg name
    unwrap (readMaybe arg) ("Invalid " ++ name ++ ": " ++ show arg)

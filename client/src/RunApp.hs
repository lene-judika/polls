{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module RunApp (runStarterApp, runApp) where

  import Widget
  import Command
  import Component


  import Control.Monad
  import Control.Monad.Trans

  import Control.Arrow

  runStarterApp :: model -> (model -> msg -> model) -> (model -> Widget msg) -> IO ()
  runStarterApp i u v = runGui (runStarterApp' i u v)

  runStarterApp' :: model -> (model -> msg -> model) -> (model -> Widget msg) -> Gui msg ()
  runStarterApp' i u v = do
    a <- buildGui (v i)

    let i' = foldM (fmap . u) i a

    case i' of
      Just n -> runStarterApp' n u v
      Nothing  -> return ()

  runApp :: (Component model, Recv model ~ Send model) => (model, Cmd (Send model)) -> (model -> Recv model -> (model, Cmd (Send model))) -> (model -> Widget (Send model)) -> IO ()
  runApp i u v = runGui (runApp' i u v)

  runApp' :: (model, Cmd msg) -> (model -> msg -> (model, Cmd msg)) -> (model -> Widget msg) -> Gui msg ()
  runApp' (i, c) u v = do
    i' <- liftIO $ execCmd u i c -- TODO split cmds here -- XXX forgot what this TODO means -- TODO write better TODO notes

    a <- buildGui (v i')

    let (i'', cs) = applyMsgs u i' a

    case i'' of
      Just n -> runApp' (n, cs) u v
      Nothing  -> return ()

  applyMsgs :: (model -> msg -> (model, Cmd msg)) -> model -> MsgBox msg -> (Maybe model, Cmd msg)
  applyMsgs _ i []             = (Just i,  mempty)
  applyMsgs _ _ (Nothing : _)  = (Nothing, mempty)
  applyMsgs u i (Just m  : ms) = second (mappend cs) $ applyMsgs u next ms
    where
      (next, cs) = u i m

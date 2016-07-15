{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module RunApp (runStarterApp, runApp) where

  import Widget
  import Command
  import Component


  import Control.Monad
  import Control.Monad.Trans

  import Control.Arrow

  import Control.Lens hiding (view)

  runStarterApp :: model -> (model -> msg -> model) -> (model -> Widget msg) -> IO ()
  runStarterApp i u v = runGui (runStarterApp' i u v)

  runStarterApp' :: model -> (model -> msg -> model) -> (model -> Widget msg) -> Gui msg ()
  runStarterApp' i u v = do
    a <- buildGui (v i)

    let i' = foldM (fmap . u) i a

    case i' of
      Just n -> runStarterApp' n u v
      Nothing  -> return ()

  runApp :: Program Cmd model msg -> IO ()
  runApp = runGui . runApp'

  runApp' :: Program Cmd model msg -> Gui msg ()
  runApp' p = do
    --i' <- liftIO $ execCmd u i c -- TODO split cmds here -- XXX forgot what this TODO means -- TODO write better TODO notes

    a <- buildGui ((p^.view) (p^.initModel))

    let (m', cs) = applyMsgs (p^.update) (p^.initModel) a


    case m' of
      Just n -> do
        n' <- liftIO $ execCmd (p^.update) n cs
        runApp' (p & initModel .~ n')
      Nothing  -> return ()

  applyMsgs :: (model -> msg -> (model, Cmd msg)) -> model -> MsgBox msg -> (Maybe model, Cmd msg)
  applyMsgs _ i []             = (Just i,  mempty)
  applyMsgs _ _ (Nothing : _)  = (Nothing, mempty)
  applyMsgs u i (Just m  : ms) = second (mappend cs) $ applyMsgs u next ms
    where
      (next, cs) = u i m

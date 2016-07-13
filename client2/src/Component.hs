{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Component
  ( Program(..)
  , Component(..)
  , Action(..)
  , program
  , component
  , components
  , HasInitModel(..)
  , HasUpdate(..)
  , HasView(..)
  ) where

  import Widget

  import Data.Map(Map)
  import qualified Data.Map as Map

  import Control.Lens hiding (view)

  -- | Represents the actions an update function may perform.
  data Action m model msg event =
      ActionIgnore
    | ActionCmd (m msg)
    | ActionMsg msg
    | ActionEvent event
    | ActionModel model
    | ActionModelAndCmd model (m msg)
    | ActionModelAndMsg model msg
    | ActionModelAndEvent model event

  data Program m model msg = Program
    { _programInitModel :: model
    , _programUpdate :: model -> msg -> (model, m msg)
    , _programView :: model -> Widget msg
    }
  makeFields ''Program

  data Component m init model msg event = Component
    { _componentInitModel :: init -> model
    , _componentUpdate :: model -> msg -> Action m model msg event
    , _componentView :: model -> Widget msg
    }
  makeFields ''Component

  program :: (Functor m, Monoid (m msg)) => Component m init model msg event -> init  -> Program m model msg
  program c i = Program ((c^.initModel) i) (programU (c^.update)) (c^.view)

  -- | Forward messages to a child component.
  component :: Functor m => model -> msg -> (model -> model') -> (msg -> msg') -> (event -> msg') -> (model -> msg -> Action m model msg event) -> Action m model' msg' event'
  component = component' False

  component' :: Functor m => Bool -> model -> msg -> (model -> model') -> (msg -> msg') -> (event -> msg') -> (model -> msg -> Action m model msg event) -> Action m model' msg' event'
  component' updated model msg wrap tag notify update =
    case update model msg of
      ActionIgnore ->
        if updated then
          ActionModel (wrap model)
        else
          ActionIgnore

      ActionCmd cmd ->
        if updated then
          ActionModelAndCmd (wrap model) (fmap tag cmd)
        else
          ActionCmd (fmap tag cmd)

      ActionMsg msg' ->
        component' updated model msg' wrap tag notify update

      ActionEvent ev' ->
        if updated then
          ActionModelAndMsg (wrap model) (notify ev')
        else
          ActionMsg (notify ev')

      ActionModel model' ->
        ActionModel (wrap model')

      ActionModelAndCmd model' cmd ->
        ActionModelAndCmd (wrap model') (fmap tag cmd)

      ActionModelAndMsg model' msg' ->
        component' True model' msg' wrap tag notify update

      ActionModelAndEvent model' ev' ->
        ActionModelAndMsg (wrap model') (notify ev')

  -- | Forward messages to a list of child components.
  components :: (Functor m, Ord ident) => Map ident model -> ident -> msg -> (Map ident model -> model') -> (msg -> msg') -> (event -> msg') -> (model -> msg -> Action m model msg event) -> Action m model' msg' event'
  components models ident msg wrap tag notify update =
    case Map.lookup ident models of
      Nothing ->
        ActionIgnore
      Just model ->
        let wrap' = wrap . (\model -> Map.insert ident model models)
        in component model msg wrap' tag notify update


  programU :: (Functor m, Monoid (m msg)) => (model -> msg -> Action m model msg event) -> model -> msg -> (model, m msg)
  programU update model msg = programU' model msg update

  programU' :: (Functor m, Monoid (m msg)) => model -> msg -> (model -> msg -> Action m model msg event) -> (model, m msg)
  programU' model msg update =
    case update model msg of
      ActionIgnore ->
        (model, mempty)

      ActionCmd cmd ->
        (model, cmd)

      ActionMsg msg' ->
        programU' model msg' update

      ActionEvent ev' ->
        -- no one to notify, ignore instead
        (model, mempty)

      ActionModel model' ->
        (model', mempty)

      ActionModelAndCmd model' cmd ->
        (model', cmd)

      ActionModelAndMsg model' msg' ->
        programU' model' msg' update

      ActionModelAndEvent model' ev' ->
        -- no one to notify, ignore instead
        (model', mempty)

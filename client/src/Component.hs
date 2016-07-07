{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes #-}

module Component
  ( Component(..)
  , HasFormCallbacks(..)
  ) where
  import Command
  import Widget

  class Component model where
    type Recv model
    type Send model
    type Init model
    initModel :: Init model -> (model, Cmd (Send model))
    update    :: model -> Recv model -> (model, Cmd (Send model))
    view      :: model -> Widget (Send model)



  class HasFormCallbacks p where
    type Unwrapped p
    type Wrapped   p
    type Return    p
    wrapMsg   :: p -> Unwrapped p -> Wrapped p
    abortMsg  :: p -> Wrapped p
    returnMsg :: p -> Return p -> Wrapped p
  --
  -- class (HasFormCallbacks f msg msg' a) => Form model msg msg' f a where

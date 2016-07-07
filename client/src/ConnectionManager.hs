{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies #-}

module ConnectionManager (Model, Msg) where
  import Widget
  import Command
  import Component
  import qualified PollCRUDApp as CRUDPoll

  import Control.Arrow

  import Control.Lens hiding (view)

  data Model = Connected
      { _defaultHost :: String
      , _model       :: CRUDPoll.Model
      }
    | Disconnected
      { _defaultHost :: String
      , _hostname     :: String
      }
  makeLenses ''Model

  data Msg = Connect | Disconnect | SetHost String | CRUDPollMsg CRUDPoll.Msg

  instance Component Model where
    type Recv Model = Msg
    type Send Model = Msg
    type Init Model = String
    initModel s = (Disconnected s s, mempty)

    update m Connect = let (m', c) = CRUDPoll.initModel (m^.hostname) in (Connected (m^.defaultHost) m', fmap CRUDPollMsg c)
    update m Disconnect = initModel (m^.defaultHost)
    update m (SetHost h) = (m & hostname .~ h, mempty)
    update m (CRUDPollMsg msg) = maybe (m, mempty) ((\m' -> m & model .~ m') *** fmap CRUDPollMsg) (update <$> (m^?model) <*> pure msg)

    view (Disconnected _ h) = mkBox Vertical
      [ mkLabel "Enter Hostname:"
      , mkText h (Just ([OnFocusLost], SetHost))
      , mkButton "Connect" (Just Connect)
      ]
    view (Connected _ a) = mkBox Vertical
      [ mkButton "Disconnect" (Just Disconnect)
      , CRUDPollMsg <$> CRUDPoll.view a
      ]

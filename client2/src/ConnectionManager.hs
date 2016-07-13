{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies #-}

module ConnectionManager (connManager) where
  import Widget
  import Command
  import Component
  import qualified PollCRUDApp as PollCRUD

  import Control.Arrow

  import Control.Lens hiding (view)

  data Model = Connected
      { _defaultHost :: String
      , _model       :: PollCRUD.Model
      }
    | Disconnected
      { _defaultHost :: String
      , _hostname    :: String
      }
  makeLenses ''Model

  data Msg = Connect | Disconnect | SetHost String | PollCRUDMsg PollCRUD.Msg | Event


  connManager :: Component Cmd String Model Msg ()
  connManager = Component (\h -> Disconnected h h) cmUpdate cmView

  cmUpdate :: Model -> Msg -> Action Cmd Model Msg ()
  cmUpdate m Connect = ActionModel $ Connected (m^.defaultHost) ((PollCRUD.pollCRUD^.initModel) (m^.hostname))
  cmUpdate m Disconnect = ActionModel $ Disconnected (m^.defaultHost) (m^.defaultHost)
  cmUpdate m (SetHost h) = ActionModel $ m & hostname .~ h
  cmUpdate m (PollCRUDMsg msg) = maybe ActionIgnore (\m' -> component m' msg (($ m) . (model .~)) PollCRUDMsg (const Event) (PollCRUD.pollCRUD^.update)) (m^?model)
  cmUpdate _ Event = ActionEvent ()

  cmView (Disconnected _ h) = mkBox Vertical
    [ mkLabel "Enter Hostname:"
    , mkText h (Just ([OnFocusLost], SetHost))
    , mkButton "Connect" (Just Connect)
    ]
  cmView (Connected _ a) = mkBox Vertical
    [ mkButton "Disconnect" (Just Disconnect)
    , PollCRUDMsg <$> (PollCRUD.pollCRUD^.view) a
    ]

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module CRUDApp
  ( crudComponent
  , ChangeAction(..)
  , DisplayEvent(..)
  , SelectorEvent(..)
  , ChangerEvent(..)
  , Model
  , Msg
  , Info(..)
  , Actions(..)
  ) where

  import Widget
  import Command
  import Component

  import Data.Map(Map)
  import qualified Data.Map as Map


  import Control.Arrow
  import Control.Lens hiding (view)



  data ChangeAction = ActionUpdate | ActionCreate
    deriving (Eq)

  data Mode sm cm = ModeOverview | ModeRead sm | ModeChange ChangeAction cm | ModeError String
  makePrisms ''Mode

  data Actions m a i = Actions
    { _createCmd :: a -> m (Either String a)
    , _readCmd   :: i -> m (Either String a)
    , _updateCmd :: a -> m (Either String a)
    , _deleteCmd :: a -> m (Either String ())
    }
  makeLenses ''Actions

  data Msg dmsg smsg cmsg a i =
      SetCursor a
    | Insert a
    | Remove i
    | ClickCreate
    | ClickRead
    | ClickUpdate
    | ClickDelete
    | CreateCmd a
    | ReadCmd i
    | UpdateCmd a
    | DeleteCmd a
    | Back
    | Ignore
    | DisplayMsg dmsg
    | SelectorMsg smsg
    | ChangerMsg cmsg
    | Error String

  data DisplayEvent i = DEChanged i | DEClose
  data SelectorEvent i = SESelect i | SEClose
  data ChangerEvent a = CECreate a | CEUpdate a | CEClose

  data Info m dm dmsg sm smsg cm cmsg a i = Info
    { _infoIdentifier :: a -> i
    , _infoNew        :: a
    , _infoDef        :: Maybe i
    , _infoActions    :: Actions m a i
    , _infoName       :: a -> String
    , _infoDisplay    :: Component m a                 dm dmsg (DisplayEvent i)
    , _infoSelector   :: Component m (Maybe i)         sm smsg (SelectorEvent i)
    , _infoChanger    :: Component m (a, ChangeAction) cm cmsg (ChangerEvent a)
    }
  makeFields ''Info

  data Model m dm dmsg sm smsg cm cmsg a i = Model
    { _modelInfo      :: Info m dm dmsg sm smsg cm cmsg a i
    , _modelValues    :: Map i a
    , _modelDisplayed :: Maybe (i, dm)
    , _modelMode      :: Mode sm cm
    }
  makeFields ''Model

  data Event a i -- = Create a | Read i | Update a | Delete i

  displayEvent :: DisplayEvent i -> Msg dmsg smsg cmsg a i
  displayEvent (DEChanged i) = ReadCmd i
  displayEvent DEClose = Back

  selectorEvent :: SelectorEvent i -> Msg dmsg smsg cmsg a i
  selectorEvent (SESelect i) = ReadCmd i
  selectorEvent SEClose = Back

  changerEvent :: ChangerEvent a -> Msg dmsg smsg cmsg a i
  changerEvent (CECreate a) = CreateCmd a
  changerEvent (CEUpdate a) = UpdateCmd a
  changerEvent CEClose = Back


  selected :: Ord i => Model m dm dmsg sm smsg cm cmsg a i -> Maybe a
  selected m = do
    i <- m^?displayed._Just._1
    m^?values.ix i

  crudComponent :: (Applicative m, Monoid (m (Msg dmsg smsg cmsg a i)), Ord i) => Component m (Info m dm dmsg sm smsg cm cmsg a i) (Model m dm dmsg sm smsg cm cmsg a i) (Msg dmsg smsg cmsg a i) (Event a i)
  crudComponent = Component crudInitModel crudUpdate crudView

  crudInitModel :: Info m dm dmsg sm smsg cm cmsg a i -> Model m dm dmsg sm smsg cm cmsg a i
  crudInitModel info = Model info Map.empty Nothing ModeOverview

  crudUpdate :: (Applicative m, Monoid (m (Msg dmsg smsg cmsg a i)), Ord i) =>
    Model m dm dmsg sm smsg cm cmsg a i -> Msg dmsg smsg cmsg a i -> Action m (Model m dm dmsg sm smsg cm cmsg a i) (Msg dmsg smsg cmsg a i) (Event a i)
  crudUpdate m Ignore            = ActionIgnore
  crudUpdate m (Error s)         = ActionModel $ m & mode      .~ ModeError s
  crudUpdate m Back              = ActionModel $ m & mode      .~ ModeOverview & displayed .~ Nothing
  crudUpdate m (Insert a)        = ActionModel $ m & values    %~ Map.insert ((m^.info.identifier) a) a
  crudUpdate m (Remove i)        = ActionModel $ m & values    %~ Map.delete i
  crudUpdate m (SetCursor a)     = ActionModel $ m & displayed ?~ ((m^.info.identifier) a, (m^.info.display.initModel) a)
  crudUpdate m ClickCreate       = ActionModel $ m & mode      .~ ModeChange ActionCreate ((m^.info.changer.initModel) (m^.info.new, ActionCreate))
  crudUpdate m ClickRead         = ActionModel $ m & mode      .~ ModeRead ((m^.info.selector.initModel) (m^.info.def))
  crudUpdate m ClickUpdate       = maybe ActionIgnore (\a -> ActionModel $ m & mode .~ ModeChange ActionUpdate ((m^.info.changer.initModel) (a, ActionUpdate))) (selected m)
  crudUpdate m ClickDelete       = maybe ActionIgnore (ActionMsg . DeleteCmd) (selected m)
  crudUpdate m (CreateCmd a)     = ActionCmd $ fmap (either Error Insert) (m^.info.actions.createCmd $ a)
  crudUpdate m (ReadCmd i)       = ActionCmd $ fmap (either Error Insert) (m^.info.actions.readCmd $ i)
  crudUpdate m (UpdateCmd a)     = ActionCmd $ fmap (either Error Insert) (m^.info.actions.updateCmd $ a)
  crudUpdate m (DeleteCmd a)     = ActionCmd $ fmap (either Error (const $ Remove . (m^.info.identifier) $ a)) (m^.info.actions.deleteCmd $ a)
  crudUpdate m (DisplayMsg msg)  = maybe ActionIgnore (\dm -> component dm msg (\dm' -> m & displayed._Just._2  .~ dm') DisplayMsg  displayEvent  (m^.info.display.update))  (m^?displayed._Just._2)
  crudUpdate m (SelectorMsg msg) = maybe ActionIgnore (\sm -> component sm msg (\sm' -> m & mode._ModeRead      .~ sm') SelectorMsg selectorEvent (m^.info.selector.update)) (m^?mode._ModeRead)
  crudUpdate m (ChangerMsg msg)  = maybe ActionIgnore (\cm -> component cm msg (\cm' -> m & mode._ModeChange._2 .~ cm') ChangerMsg  changerEvent  (m^.info.changer.update))  (m^?mode._ModeChange._2)

  crudView :: Model m dm dmsg sm smsg cm cmsg a i -> Widget (Msg dmsg smsg cmsg a i)
  crudView (Model i vs c ModeOverview) = mkBox Horizontal $
    [ mkTextList (Map.elems vs) (i^.name) (Just SetCursor) --TODO highlight cursor
    , mkBox Vertical
      [ mkButton "Create" (Just ClickCreate)
      , mkButton "Read"   (Just ClickRead)
      , mkButton "Update" (Just ClickUpdate)
      , mkButton "Delete" (Just ClickDelete)
      ]
    ] ++ case c of
      Nothing ->
        []
      Just (_, dm) ->
        [DisplayMsg <$> (i^.display.view) dm]

  crudView (Model i _ _ (ModeError s))     = mkBox Vertical
    [ mkLabel ("Error: " ++ s)
    , mkButton "Back" (Just Back)
    ]
  crudView (Model i _ _ (ModeRead sm))     = SelectorMsg <$> (i^.selector.view) sm
  crudView (Model i _ _ (ModeChange c cm)) = ChangerMsg  <$> (i^.changer.view) cm

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module CRUDApp
  ( Model
  , Msg(..)
  , initModel
  , update
  , view
  , Info(Info)
  , Actions(Actions)
  , ChangeAction(..)
  ) where

  import Widget
  import Command
  import Component

  import Data.Map(Map)
  import qualified Data.Map as Map


  import Control.Arrow
  import Control.Lens hiding (view)



  data ChangeAction = Update | Create
    deriving (Eq)

  data Mode sm cm = ModeOverview | ModeRead sm | ModeChange ChangeAction cm
  makePrisms ''Mode

  data Actions a i = Actions
    { _createCmd :: a -> Cmd (Either String a)
    , _readCmd   :: i -> Cmd (Either String a)
    , _updateCmd :: a -> Cmd (Either String a)
    , _deleteCmd :: a -> Cmd (Either String ())
    }
  makeLenses ''Actions

  data Msg dmsg smsg cmsg a i =
      SetCursor a
    | Recv a
    | Remove i
    | ClickCreate
    | ClickRead
    | ClickUpdate
    | ClickDelete
    | Send a
    | Read i
    | Delete a
    | Back
    | Ignore
    | DisplayMsg dmsg
    | SelectorMsg smsg
    | ChangerMsg cmsg
    | Error String

  data Info r a i = Info
    { _infoIdentifier :: a -> i
    , _infoNew        :: a
    , _infoDef        :: Maybe i
    , _infoActions    :: Actions a i
    , _infoName       :: a -> String
    , _infoDisplayInit    :: r
    }
  makeFields ''Info

  data Model dm sm cm r a i = Model
    { _modelInfo    :: Info r a i
    , _modelValues  :: Map i a
    , _modelDisplay :: Maybe (i, dm)
    , _modelMode    :: Mode sm cm
    }
  makeFields ''Model

  selected :: Ord i => Model dm sm cm r a i -> Maybe a
  selected m = do
    i <- m^?display._Just._1
    m^?values.ix i

  selectCmd :: Model dm sm cm r a i -> a -> Cmd (Either String a)
  selectCmd m = case m^?mode._ModeChange._1 of
    Just Create -> m^.info.actions.createCmd
    Just Update -> m^.info.actions.updateCmd

  instance
    ( Component dm
    , Component sm
    , Component cm
    , Init dm ~ (a,r)
    , Init cm ~ (a, ChangeAction)
    , Init sm ~ (Maybe i)
    , Send dm ~ Recv dm
    , Send sm ~ Msg (Recv dm) (Recv sm) (Recv cm) a i
    , Send cm ~ Msg (Recv dm) (Recv sm) (Recv cm) a i
    , Ord i
    ) => Component (Model dm sm cm r a i) where
    type Recv   (Model dm sm cm r a i) = Msg (Recv dm) (Recv sm) (Recv cm) a i
    type Send   (Model dm sm cm r a i) = Msg (Recv dm) (Recv sm) (Recv cm) a i
    type Init   (Model dm sm cm r a i) = Info r a i
    initModel i = (Model i Map.empty Nothing ModeOverview, mempty)

    update m Ignore            = (m, mempty)
    update m Back              = ((m & mode .~ ModeOverview) & display .~ Nothing, mempty)
    update m (SetCursor a)     = let (m', c) = initModel (a, m^.info.displayInit) in (m & display ?~ ((m^.info.identifier) a, m'), fmap DisplayMsg c)
    update m (Recv a)          = (m & values %~ Map.insert (a^.to (m^.info.identifier)) a, mempty)
    update m (Remove i)        = (m & values %~ Map.delete i, mempty)
    update m ClickCreate       = let (m', c) = initModel (m^.info.new, Create) in (m & mode .~ ModeChange Create m', c)
    update m ClickRead         = let (m', c) = initModel (m^.info.def) in (m & mode .~ ModeRead m', c)
    update m ClickUpdate       = maybe (m, mempty) (\a -> let (m',c) = initModel (a, Update) in (m & mode .~ ModeChange Update m', c)) (selected m)
    update m ClickDelete       = maybe (m, mempty) (\i -> (m, cmd . return . Delete $ i)) (selected m)
    update m (Send a)          = (m, fmap (either Error Recv) (selectCmd m a)               `mappend` cmd (return  Back))
    update m (Read i)          = (m, fmap (either Error Recv) (m^.info.actions.readCmd $ i) `mappend` cmd (return  Back))
    update m (Delete a)        = (m, fmap (either Error (const $ Remove ((m^.info.identifier) a))) (m^.info.actions.deleteCmd $ a))
    update m (SelectorMsg msg) = maybe (m, mempty) (first (\m' -> m & mode._ModeRead .~ m'))                 (update <$> (m^?mode._ModeRead)      <*> pure msg)
    update m (ChangerMsg msg)  = maybe (m, mempty) (first (\m' -> m & mode._ModeChange._2 .~ m'))            (update <$> (m^?mode._ModeChange._2) <*> pure msg)
    update m (DisplayMsg msg)  = maybe (m, mempty) ((\m' -> m & display._Just._2 .~ m') *** fmap DisplayMsg) (update <$> (m^?display._Just._2)    <*> pure msg)
    update _ (Error s)         = error s --TODO better error handling

    view (Model i vs c ModeOverview) = mkBox Horizontal $
      [ mkScrolledWindow $ mkTextList (Map.elems vs) (i^.name) (Just SetCursor) --TODO highlight cursor
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
          [DisplayMsg <$> view dm]

    view (Model _ _ _ (ModeRead sm))     = view sm
    view (Model _ _ _ (ModeChange c cm)) = view cm

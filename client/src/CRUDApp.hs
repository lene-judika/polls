{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies #-}

module CRUDApp
  ( Model
  , Msg(..)
  , initModel
  , update
  , view
  , Info(Info)
  , Actions(Actions)
  ) where

  import Widget
  import Command
  import Component

  import Data.Map(Map)
  import qualified Data.Map as Map


  import Control.Arrow
  import Control.Lens hiding (view)

  data ChangeAction = Update | Create

  data Mode sm cm = ModeOverview | ModeRead sm | ModeChange ChangeAction cm
  makePrisms ''Mode

  data Actions a i = Actions
    { _createCmd :: a -> Cmd (Either String a)
    , _readCmd   :: i -> Cmd (Either String a)
    , _updateCmd :: a -> Cmd (Either String a)
    , _deleteCmd :: i -> Cmd (Maybe String)
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
    | Delete i
    | Back
    | Ignore
    | DisplayMsg dmsg
    | SelectorMsg smsg
    | ChangerMsg cmsg
    | Error String

  data Info a i = Info
    { _infoIdentifier :: a -> i
    , _infoNew        :: a
    , _infoDef        :: Maybe i
    , _infoActions    :: Actions a i
    , _infoName       :: a -> String
    }
  makeFields ''Info

  data Model dm sm cm a i = Model
    { _modelInfo    :: Info a i
    , _modelValues  :: Map i a
    , _modelDisplay :: Maybe (i, dm)
    , _modelMode    :: Mode sm cm
    }
  makeFields ''Model


  selected :: Ord i => Model dm sm cm a i -> Maybe a
  selected m = do
    i <- m^?display._Just._1
    m^?values.ix i


  instance
    ( Component dm
    , Component sm
    , Component cm
    , Init dm ~ a
    , Init cm ~ a
    , Init sm ~ (Maybe i)
    , Send dm ~ Recv dm
    , Send sm ~ Msg (Recv dm) (Recv sm) (Recv cm) a i
    , Send cm ~ Msg (Recv dm) (Recv sm) (Recv cm) a i
    , Ord i
    ) => Component (Model dm sm cm a i) where
    type Recv (Model dm sm cm a i) = Msg (Recv dm) (Recv sm) (Recv cm) a i
    type Send (Model dm sm cm a i) = Msg (Recv dm) (Recv sm) (Recv cm) a i
    type Init (Model dm sm cm a i) = Info a i
    initModel i = (Model i Map.empty Nothing ModeOverview, mempty)

    update m Ignore            = (m, mempty)
    update m Back              = (m & mode .~ ModeOverview, mempty)
    update m (SetCursor a)     = let (m', c) = initModel a in (m & display ?~ ((m^.info.identifier) a, m'), fmap DisplayMsg c)
    update m (Recv a)          = (m & values %~ Map.insert (a^.to (m^.info.identifier)) a, mempty)
    update m (Remove i)        = (m & values %~ Map.delete i, mempty)
    update m ClickCreate       = let (m', c) = initModel (m^.info.new) in (m & mode .~ ModeChange Create m', c)
    update m ClickRead         = let (m', c) = initModel (m^.info.def) in (m & mode .~ ModeRead m', c)
    update m ClickUpdate       = maybe (m, mempty) (\a -> let (m',c) = initModel a in (m & mode .~ ModeChange Update m', c)) (selected m)
    update m ClickDelete       = maybe (m, mempty) (\i -> (m, cmd . return . Delete $ i))    (m^?display._Just._1)
    update m (Send a)          = (m, fmap (either Error Recv)      (m^.info.actions.createCmd $ a) `mappend` cmd (return  Back)) -- TODO change mode (create -> Create/Update)
    update m (Read i)          = (m, fmap (either Error Recv)      (m^.info.actions.readCmd $ i)   `mappend` cmd (return  Back))
    update m (Delete i)        = (m, fmap (maybe (Remove i) Error) (m^.info.actions.deleteCmd $ i) `mappend` cmd (return  Back))
    update m (SelectorMsg msg) = maybe (m, mempty) (first (\m' -> m & mode._ModeRead .~ m'))                 (update <$> (m^?mode._ModeRead)      <*> pure msg)
    update m (ChangerMsg msg)  = maybe (m, mempty) (first (\m' -> m & mode._ModeChange._2 .~ m'))            (update <$> (m^?mode._ModeChange._2) <*> pure msg)
    update m (DisplayMsg msg)  = maybe (m, mempty) ((\m' -> m & display._Just._2 .~ m') *** fmap DisplayMsg) (update <$> (m^?display._Just._2)    <*> pure msg)
    update _ (Error s)         = error s --TODO better error handling

    view (Model i vs c ModeOverview) = mkBox Horizontal $
      [ mkScrolledWindow $ mkTextList (Map.elems vs) (i^.name) (Just SetCursor) --TODO highlight cursor
      , mkBox Vertical
        [ mkButton "Create" (Just ClickCreate)
        , mkButton "Read"   (Just ClickRead)
        --, mkButton "Update" (Just UpdatePoll)
        --, mkButton "Delete" (Just DeletePoll)
        ]
      ] ++ case c of
        Nothing ->
          []
        Just (_, dm) ->
          [DisplayMsg <$> view dm]

    view (Model _ _ _ (ModeRead sm)) = view sm
    view (Model _ _ _ (ModeChange c cm)) = view cm

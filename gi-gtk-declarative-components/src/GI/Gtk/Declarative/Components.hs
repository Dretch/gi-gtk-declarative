{-# LANGUAGE DeriveFunctor                 #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE KindSignatures                #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE LambdaCase                    #-}
{-# LANGUAGE NamedFieldPuns                #-}
{-# LANGUAGE RankNTypes                    #-}
{-# LANGUAGE RecordWildCards               #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE StandaloneDeriving            #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}

module GI.Gtk.Declarative.Components
  ( Component(..)
  , UpdateM
  , updateIO
  , updateIO_
  , updateParent
  , component
  , AppAction(..)
  , run
  , runWith
  )
where

import           Control.Concurrent
import           Control.Exception             (SomeException, catch, finally)
import qualified Control.Concurrent.Async      as Async
import           Control.Monad                 (ap, liftM, unless, void)
import           Control.Monad.State.Class     (MonadState(get, put))
import           Data.Foldable                 (for_)
import           Data.Hashable                 (Hashable(..))
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.IORef                    (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.TMap as TMap
import           Data.Typeable
import qualified GI.Gtk                        as Gtk
import qualified GI.Gdk                        as Gdk
import qualified GI.GLib.Constants             as GLib
import           System.Exit
import           System.IO

import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Context
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch()
import           GI.Gtk.Declarative.State

-- | A declarative component. Types in this class must have a single type
-- parameter which describes the type of the events emitted by the component.
class Typeable c => Component (c :: * -> *) where

  -- | The internal state of the component. This is initialized when the
  -- component is first shown, and then varies over the lifetime of the
  -- component as `patchComponent` and `update` modify it.
  data ComponentState c
  
  -- | Internal actions that are emitted by child widgets and make this
  -- component do stuff.
  data ComponentAction c

  -- | Called when the component is first shown. This creates the initial
  -- component state from the declarative component, and optionally
  -- provides an initial action to start the component doing stuff.
  createComponent :: c event -> (ComponentState c, Maybe (ComponentAction c))

  -- | Called when a new declarative component is applied to the existing
  -- component state. This method should copy any required state from the
  -- new declarative component. This method will never be called for top
  -- level components - so there is a default implementation that returns
  -- an error: components intended to be used a non-top-level components
  -- should implement this method.
  patchComponent :: ComponentState c -> c event -> ComponentState c
  patchComponent _ _ =
    error "patchComponent not implemented: it should be implemented for all non-top-level components."

  -- | Called when an action is emitted by a child event. The `UpdateM`
  -- monad allows updating the component state, running asynchronous IO
  -- actions, and sending events to the parent on this component.
  update :: c event -> ComponentAction c -> UpdateM c event ()

  -- | Provides the tree of widgets that display this component. Called
  -- whenever the state changes (and possibly other times too).
  view :: c event -> ComponentState c -> Widget (ComponentAction c)

-- | An "update action" that happens when a component of type `c` receives an
-- action from a child widget. The `event` type parameter describes the type of
-- events that the parent widget can receive. This implements `MonadState` to
-- allow getting/setting the component state.
data UpdateM c event a where
  UpdatePure :: a -> UpdateM c event a
  UpdateBind :: UpdateM c event a -> (a -> UpdateM c event b) -> UpdateM c event b
  UpdateStateGet :: UpdateM c event (ComponentState c)
  UpdateStatePut :: ComponentState c -> UpdateM c event ()
  UpdateIO :: IO (Maybe (ComponentAction c)) -> UpdateM c event ()
  UpdateParent :: event -> UpdateM c event ()

instance Functor (UpdateM c event) where
  fmap = liftM

instance Applicative (UpdateM c event) where
  pure = UpdatePure
  (<*>) = ap

instance Monad (UpdateM c event) where
  (>>=) = UpdateBind

instance MonadState (ComponentState c) (UpdateM c event) where
  get = UpdateStateGet
  put = UpdateStatePut

-- | An update action that asynchronously runs some IO - and then maybe
-- emits another action.
updateIO :: IO (Maybe (ComponentAction c)) -> UpdateM c event ()
updateIO = UpdateIO

-- | An update action that asynchronously runs some IO (and never emits
-- another action).
updateIO_ :: IO () -> UpdateM c event ()
updateIO_ cmd = UpdateIO (Nothing <$ cmd)

-- | An update action that (synchronously) sends an action to the parent
-- component.
updateParent :: event -> UpdateM c event ()
updateParent = UpdateParent

-- | The result of running an `UpdateM`
data UpdateResult a
  = UpdateResultValue
      { updateValue :: a
      , updatedState :: Bool
      }
  | UpdateResultExited

updateResult :: UpdateResult ()
updateResult = UpdateResultValue
 { updateValue = ()
 , updatedState = False
 }

newtype ComponentId = ComponentId Int
  deriving (Show, Enum, Eq, Hashable)

rootComponentId :: ComponentId
rootComponentId = ComponentId 0

data StoredComponent =
  forall c parent. (Component c, Component parent) => StoredComponent
    { parentComponentId :: !ComponentId
    , nonRootComponent :: !(c (ComponentAction parent))
    , storedState :: !(ComponentState c)
    , storedView :: !(Widget (ComponentAction c)) -- ^ Must only be updated inside `patch`, otherwise it gets out-of-sync with the state tree
    }

data PatchContext = PatchContext
  { currentComponentId :: ComponentId
  , components :: HashMap ComponentId StoredComponent
  , nextComponents :: IORef (HashMap ComponentId StoredComponent)
  , nextComponentId :: IORef ComponentId
  , events :: Chan DynamicAction
  }

data EventSourceContext = EventSourceContext
  { esComponents :: HashMap ComponentId StoredComponent
  , esEvents :: Chan DynamicAction
  }

-- | A dynamically typed action that is destined for a particular component.
data DynamicAction = forall comp. Component comp => DynamicAction
   ComponentId (ComponentAction comp)

data ComponentWidget comp event where
  ComponentWidget
   :: (Component comp, Component parentComp)
   => comp (ComponentAction parentComp)
   -> ComponentWidget comp (ComponentAction parentComp)

instance EventSource (ComponentWidget comp) where
  subscribe ctx (ComponentWidget _comp) state _cb = do
    let (cid, state') = unwrapState state
        (chan, _state, compView) = getEventSourceContext @comp ctx cid
        cb' = writeChan chan . DynamicAction cid
    subscribe ctx compView state' cb'

instance Patchable (ComponentWidget comp) where

  create ctx (ComponentWidget c) = do
    let (state, action) = createComponent c
        view' = view c state
    (cid, ctx') <- storeNewComponent ctx c state view' action
    ss <- create ctx' view'
    pure (wrapState cid ss)
  
  patch ctx ss (ComponentWidget _c1) (ComponentWidget c2) =
    let (cid, ss') = unwrapState ss
        (oldState, oldView) = getStoredComponent ctx cid
        newState = patchComponent oldState c2
        newView = view c2 newState
        ctx' = setCurrentComponentId ctx cid
    in case patch ctx' ss' oldView newView of
      Modify cmd -> Modify $ do
        setStoredComponent ctx' cid c2 newState newView
        wrapState cid <$> cmd
      Replace cmd -> Replace $ do
        setStoredComponent ctx' cid c2 newState newView
        wrapState cid <$> cmd
      Keep -> Modify $ do
        setStoredComponent ctx' cid c2 newState newView
        pure ss
  
  destroy ctx ss (ComponentWidget (_ :: c e)) = do
    let (cid, ss') = unwrapState ss
        (_state, view') = getStoredComponent @c ctx cid
        ctx' = setCurrentComponentId ctx cid
    destroy ctx' ss' view'
    removeComponent ctx' cid

-- | The underlying widget custom state, wrapped up with the id of this component
-- (which we can use to obtain the component state, etc)
data WrappedCustomState =
  forall state. Typeable state => WrappedCustomState ComponentId state

wrapState :: ComponentId -> SomeState -> SomeState
wrapState cid (SomeState tree) =
  SomeState (modifyCustomState (WrappedCustomState cid) tree)

unwrapState :: SomeState -> (ComponentId, SomeState)
unwrapState (SomeState (tree :: StateTree t w c e cs)) =
  case eqT @WrappedCustomState @cs of
    Just Refl ->
      case stateTreeCustomState (stateTreeNode tree) of
        WrappedCustomState cid cs' ->
          (cid, SomeState (modifyCustomState (const cs') tree))
    Nothing ->
      error "Custom state not of expected type: this is a bug."

-- todo: investigate replacing this with a StateTreeComponent constructor (and Context with explicit ComponentContext) ?
modifyCustomState
 :: forall t w e c cs1 cs2
  . (cs1 -> cs2)
 -> StateTree t w c e cs1
 -> StateTree t w c e cs2
modifyCustomState f = \case
  StateTreeWidget node -> StateTreeWidget (modifyNode node)
  StateTreeBin node bin' -> StateTreeBin (modifyNode node) bin'
  StateTreeContainer node kids -> StateTreeContainer (modifyNode node) kids
  where
    modifyNode :: StateTreeNode w e cs1 -> StateTreeNode w e cs2
    modifyNode StateTreeNode{..} =
      StateTreeNode {stateTreeCustomState = f stateTreeCustomState, ..}

component
  :: (Component comp, Component parentComp)
  => comp (ComponentAction parentComp)
  -> Widget (ComponentAction parentComp)
component = Widget . ComponentWidget

-- | A top-level component sends this event to its parent when it wants to
-- affect the top-level GTK environment itself.
data AppAction = Exit -- ^ Destroy all components, quit GTK, and cause `run` to finish.

data LoopState c = LoopState
  { rootView :: Widget (ComponentAction c)
  , rootSomeState :: SomeState
  , rootSubscription :: Subscription
  }

-- | Show the component and process events from it until it emits `AppAction.Exit`
run
 :: Component c => (forall event. (AppAction -> event) -> c event)
 -> IO ()
run = runWith (pure ())

-- | Show the component and process events from it until it emits `AppAction.Exit`
runWith
 :: IO () -- ^ Additional initialisation action to run after GTK `init` but before GTK `main`.
 -> Component c => (forall event. (AppAction -> event) -> c event)
 -> IO ()
runWith postInitGtk rootCtor = do
  assertRuntimeSupportsBoundThreads
  
  let rootComponent = rootCtor id
      (initState, initAction) = createComponent rootComponent
  rootComponentState <- newIORef initState
  events <- newChan
  for_ initAction $
    writeChan events . DynamicAction rootComponentId
  components <- newIORef mempty
  nextComponentId <- newIORef $ succ rootComponentId

  void $ Gtk.init Nothing
  postInitGtk
  main <- Async.async Gtk.main
  runLoop rootComponent rootComponentState events components nextComponentId
    `finally` (Gtk.mainQuit >> Async.wait main)

runLoop
 :: forall c. Component c
 => c AppAction
 -> IORef (ComponentState c)
 -> Chan DynamicAction
 -> IORef (HashMap ComponentId StoredComponent)
 -> IORef ComponentId
 -> IO ()
runLoop rootComponent rootComponentState events components nextComponentId = do

  rootView <- view rootComponent <$> readIORef rootComponentState
  pCtx <- patchContext events rootComponentId components nextComponentId
  rootSomeState <- runUI $ create pCtx rootView
  runUI (Gtk.widgetShowAll =<< someStateWidget rootSomeState)
  rootSubscription <- rootSubscribe rootView rootSomeState

  loop LoopState{..}
  where

    loop :: LoopState c -> IO ()
    loop ls = do
      event <- readChan events
      processAction event >>= \case
        UpdateResultExited                      -> pure ()
        UpdateResultValue{updatedState = True}  -> rerender ls >>= loop
        UpdateResultValue{updatedState = False} -> loop ls
    
    rerender :: LoopState c -> IO (LoopState c)
    rerender ls = do
      newRootView <- view rootComponent <$> readIORef rootComponentState
      pCtx <- patchContext events rootComponentId components nextComponentId
      case patch pCtx (rootSomeState ls) (rootView ls) newRootView of
        Modify modify -> runUI $ do
          cancel (rootSubscription ls)
          newRootSomeState <- modify
          newRootSubscription <- rootSubscribe newRootView newRootSomeState
          pure LoopState
            { rootView = newRootView
            , rootSomeState = newRootSomeState
            , rootSubscription = newRootSubscription
            }
        Replace createNew -> runUI $ do
          cancel (rootSubscription ls)
          dCtx <- patchContext events rootComponentId components nextComponentId
          destroy dCtx (rootSomeState ls) (rootView ls)
          newRootSomeState <- createNew
          runUI (Gtk.widgetShowAll =<< someStateWidget newRootSomeState)
          newRootSubscription <- rootSubscribe newRootView newRootSomeState
          pure LoopState
            { rootView = newRootView
            , rootSomeState = newRootSomeState
            , rootSubscription = newRootSubscription
            }
        Keep ->
          pure ls

    rootSubscribe
     :: Widget (ComponentAction c)
     -> SomeState
     -> IO Subscription
    rootSubscribe rootView rootSomeState = do
      esCtx <- eventSourceContext events components
      let cb = writeChan events . DynamicAction rootComponentId
      subscribe esCtx rootView rootSomeState cb

    processAction :: DynamicAction -> IO (UpdateResult ())
    processAction (DynamicAction cid (action :: ComponentAction d)) = do
      if cid == rootComponentId then
        case eqT @d @c of
          Nothing ->
            error "Unexpected root action type. This is a bug."
          Just Refl ->
            runRootUpdateM (update rootComponent action)
      else do
        components' <- readIORef components
        case HashMap.lookup cid components' of
          Nothing -> do
            -- the component must have been destroyed already (not a bug - due to async IO)
            pure updateResult
          Just (StoredComponent pid (comp :: d' _e) _state view') -> do
            case eqT @d @d' of
              Nothing ->
                error "Unexpected non-root action type. This is a bug."
              Just Refl ->
                runNonRootUpdateM pid cid comp view' (update comp action)

    runRootUpdateM :: UpdateM c AppAction a -> IO (UpdateResult a)
    runRootUpdateM = \case
      UpdatePure a ->
        pure updateResult{updateValue = a}
      UpdateBind cmd f -> do
        runUpdateBind cmd f runRootUpdateM
      UpdateStateGet -> do
        val <- readIORef rootComponentState
        pure updateResult{ updateValue = val }
      UpdateStatePut s -> do
        writeIORef rootComponentState s
        pure updateResult{ updatedState = True }
      UpdateIO cmd -> do
        runUpdateIO cmd rootComponentId
      UpdateParent Exit -> do
        pure UpdateResultExited

    runNonRootUpdateM
     :: forall comp parentComp a. (Component comp, Component parentComp)
     => ComponentId
     -> ComponentId
     -> comp (ComponentAction parentComp)
     -> Widget (ComponentAction comp)
     -> UpdateM comp (ComponentAction parentComp) a
     -> IO (UpdateResult a)
    runNonRootUpdateM pid cid comp' view' = \case
      UpdatePure a ->
        pure updateResult{updateValue = a}
      UpdateBind cmd f -> do
        runUpdateBind cmd f (runNonRootUpdateM pid cid comp' view')
      UpdateStateGet -> do
        (state, _) <- getStoredComponent' cid <$> readIORef components
        pure updateResult{ updateValue = state }
      UpdateStatePut s -> do
        -- the view must not change until re-render (when `patch` is
        -- called), to avoid it getting out-of-sync with the state tree
        setStoredComponent' components cid comp' s view'
        pure updateResult{ updatedState = True }
      UpdateIO cmd -> do
        runUpdateIO cmd cid
      UpdateParent action' -> do
        processAction $ DynamicAction pid action'

    runUpdateBind
     :: UpdateM comp e a
     -> (a -> UpdateM comp e b)
     -> (forall x. UpdateM comp e x -> IO (UpdateResult x))
     -> IO (UpdateResult b)
    runUpdateBind cmd f runner =
        runner cmd >>= \case
          UpdateResultExited ->
            pure UpdateResultExited
          UpdateResultValue{updateValue, updatedState} -> do
              runner (f updateValue) >>= \case
                UpdateResultExited ->
                  pure UpdateResultExited
                UpdateResultValue{updateValue = updateValue', updatedState = updatedState'} ->
                  pure $ UpdateResultValue updateValue' (updatedState || updatedState')

    runUpdateIO
     :: Component comp
     => IO (Maybe (ComponentAction comp))
     -> ComponentId
     -> IO (UpdateResult ())
    runUpdateIO cmd cid = do
        void . Async.async $ do
          cmd >>= \case
            Nothing -> pure ()
            Just action' -> writeChan events (DynamicAction cid action')
        pure updateResult

patchContext
 :: Chan DynamicAction
 -> ComponentId
 -> IORef (HashMap ComponentId StoredComponent)
 -> IORef ComponentId
 -> IO Context
patchContext events currentComponentId nextComponents nextComponentId = do
  components <- readIORef nextComponents
  pure . Context . TMap.one $ PatchContext{..}

storeNewComponent
 :: (Component comp, Component parentComp)
 => Context
 -> comp (ComponentAction parentComp)
 -> ComponentState comp
 -> Widget (ComponentAction comp)
 -> Maybe (ComponentAction comp)
 -> IO (ComponentId, Context)
storeNewComponent ctx comp state view' action = do
  let ctxData = getPatchContext ctx
  cid <- atomicModifyIORef (nextComponentId ctxData) (\i -> (succ i, i))
  modifyIORef (nextComponents ctxData) $ \components ->
    let pid = currentComponentId ctxData
    in HashMap.insert cid (StoredComponent pid comp state view') components
  for_ action $
    writeChan (events ctxData) . DynamicAction cid
  pure (cid, Context . TMap.one $ ctxData{currentComponentId = cid})

getStoredComponent
 :: forall comp. Component comp
 => Context
 -> ComponentId
 -> (ComponentState comp, Widget (ComponentAction comp))
getStoredComponent ctx cid =
  getStoredComponent' cid (components $ getPatchContext ctx)

getStoredComponent'
 :: forall comp. Component comp
 => ComponentId
 -> HashMap ComponentId StoredComponent
 -> (ComponentState comp, Widget (ComponentAction comp))
getStoredComponent' cid map' =
  case HashMap.lookup cid map' of
    Just (StoredComponent _pid _comp (state :: state) view') ->
      case eqT @state @(ComponentState comp) of
        Just Refl -> (state, view')
        Nothing   -> error "Bad non-root component state type: this is a bug"
    Nothing ->
      error "Missing component state: this is a bug"

setStoredComponent
 :: (Component comp, Component parentComp)
 => Context
 -> ComponentId
 -> comp (ComponentAction parentComp)
 -> ComponentState comp
 -> Widget (ComponentAction comp)
 -> IO ()
setStoredComponent ctx cid comp state view' = do
  let ctxData = getPatchContext ctx
  setStoredComponent' (nextComponents ctxData) cid comp state view'

setStoredComponent'
 :: (Component comp, Component parentComp)
 => IORef (HashMap ComponentId StoredComponent)
 -> ComponentId
 -> comp (ComponentAction parentComp)
 -> ComponentState comp
 -> Widget (ComponentAction comp)
 -> IO ()
setStoredComponent' components cid comp state view' = do
  modifyIORef components $ HashMap.adjust setState cid
  where
    setState StoredComponent{..} =
      StoredComponent
        { storedState = state
        , storedView = view'
        , nonRootComponent = comp
        , parentComponentId
        }
  
setCurrentComponentId :: Context -> ComponentId -> Context
setCurrentComponentId (Context tmap) cid =
  Context $ TMap.adjust (\pc -> pc{ currentComponentId = cid}) tmap

removeComponent :: Context -> ComponentId -> IO ()
removeComponent ctx cid = do
  let ctxData = getPatchContext ctx
  modifyIORef (nextComponents ctxData) (HashMap.delete cid)

getPatchContext :: Context -> PatchContext
getPatchContext (Context ctx) =
  case TMap.lookup ctx of
    Just c   -> c
    Nothing  -> error "Missing patch context: this is a bug."

eventSourceContext
 :: Chan DynamicAction
 -> IORef (HashMap ComponentId StoredComponent)
 -> IO Context
eventSourceContext esEvents esComponentsVar = do
  esComponents <- readIORef esComponentsVar
  pure . Context . TMap.one $ EventSourceContext{..}

getEventSourceContext
 :: Component comp
 => Context
 -> ComponentId
 -> (Chan DynamicAction, ComponentState comp, Widget (ComponentAction comp))
getEventSourceContext (Context ctx) cid =
  let (state', view') = getStoredComponent' cid esComponents
  in (esEvents, state', view')
  where
    EventSourceContext{..} =
      case TMap.lookup ctx of
        Just c  -> c
        Nothing -> error "Missing event source context: this is a bug"

-- | Assert that the program was linked using the @-threaded@ flag, to
-- enable the threaded runtime required by this module.
assertRuntimeSupportsBoundThreads :: IO ()
assertRuntimeSupportsBoundThreads = unless rtsSupportsBoundThreads $ do
  hPutStrLn
    stderr
    "GI.Gtk.Declarative.App.Simple requires the program to \
                     \be linked using the threaded runtime of GHC (-threaded \
                     \flag)."
  exitFailure

runUI :: IO a -> IO a
runUI ma = do
  r <- newEmptyMVar
  runUI_ (ma >>= putMVar r)
  takeMVar r

runUI_ :: IO () -> IO ()
runUI_ ma = do
  tId <- myThreadId

  void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    -- Any exception in the gtk ui thread will be rethrown in the calling thread.
    -- This ensure that this exception won't terminate the application without any control.
    ma `catch` throwTo @SomeException tId
    return False

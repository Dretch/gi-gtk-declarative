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

module GI.Gtk.Declarative.Component
  ( Component(..)
  , ComponentContext
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
import           Control.Monad                 (unless, void)
import           Data.Foldable                 (for_)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.IORef                    (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)
import           Data.Typeable
import qualified GI.Gtk                        as Gtk
import qualified GI.Gdk                        as Gdk
import qualified GI.GLib.Constants             as GLib
import           System.Exit
import           System.IO

import           GI.Gtk.Declarative.Component.Internal
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.State
import           GI.Gtk.Declarative.Widget

rootComponentId :: ComponentId
rootComponentId = ComponentId 0

data ComponentWidget comp event where
  ComponentWidget
   :: (Component comp, Component parentComp)
   => comp (ComponentAction parentComp)
   -> ComponentWidget comp (ComponentAction parentComp)

instance EventSource (ComponentWidget comp) where
  subscribe ctx (ComponentWidget _comp) state _cb = do
    let (cid, state') = unwrapState state
        (_state, compView) = getStoredComponent @comp ctx cid
        chan = events ctx
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
        ctx' = ctx{ currentComponentId = cid }
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
        ctx' = ctx{ currentComponentId = cid }
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

-- todo: investigate replacing this with a StateTreeComponent constructor?
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
data AppAction x =
  Exit x -- ^ Destroy all components, quit GTK, and cause `run` to finish and produce `x`.

data LoopState c = LoopState
  { rootView :: Widget (ComponentAction c)
  , rootSomeState :: SomeState
  , rootSubscription :: Subscription
  }

-- | Show the component and process its internal events until it
-- sends `AppAction.Exit x` to its parent. The IO action returns `x`
run :: Component c => c (AppAction x) -> IO x
run = runWith (pure ())

-- | Show the component and process its internal events until it
-- sends `AppAction.Exit x` to its parent. The IO action returns `x`
runWith
 :: Component c
 => IO () -- ^ Additional initialisation action to run after GTK `init` but before GTK `main`.
 -> c (AppAction x)
 -> IO x
runWith postInitGtk rootComponent = do
  assertRuntimeSupportsBoundThreads
  
  let (initState, initAction) = createComponent rootComponent
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
 :: forall x c. Component c
 => c (AppAction x)
 -> IORef (ComponentState c)
 -> Chan DynamicAction
 -> IORef (HashMap ComponentId StoredComponent)
 -> IORef ComponentId
 -> IO x
runLoop rootComponent rootComponentState events components nextComponentId = do

  rootView <- view rootComponent <$> readIORef rootComponentState
  ctx <- componentContext events rootComponentId components nextComponentId
  rootSomeState <- runUI $ create ctx rootView
  runUI (Gtk.widgetShowAll =<< someStateWidget rootSomeState)
  rootSubscription <- rootSubscribe rootView rootSomeState

  loop LoopState{..}
  where

    loop :: LoopState c -> IO x
    loop ls = do
      event <- readChan events
      processAction event >>= \case
        UpdateResultExited x                    -> pure x
        UpdateResultValue{updatedState = True}  -> rerender ls >>= loop
        UpdateResultValue{updatedState = False} -> loop ls
    
    rerender :: LoopState c -> IO (LoopState c)
    rerender ls = do
      newRootView <- view rootComponent <$> readIORef rootComponentState
      ctx <- componentContext events rootComponentId components nextComponentId
      case patch ctx (rootSomeState ls) (rootView ls) newRootView of
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
          dCtx <- componentContext events rootComponentId components nextComponentId
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
      ctx <- componentContext events rootComponentId components nextComponentId
      let cb = writeChan events . DynamicAction rootComponentId
      subscribe ctx rootView rootSomeState cb

    processAction :: DynamicAction -> IO (UpdateResult x ())
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

    runRootUpdateM :: UpdateM c (AppAction x) a -> IO (UpdateResult x a)
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
      UpdateParent (Exit x) -> do
        pure $ UpdateResultExited x

    runNonRootUpdateM
     :: forall comp parentComp a. (Component comp, Component parentComp)
     => ComponentId
     -> ComponentId
     -> comp (ComponentAction parentComp)
     -> Widget (ComponentAction comp)
     -> UpdateM comp (ComponentAction parentComp) a
     -> IO (UpdateResult x a)
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
     -> (forall d. UpdateM comp e d -> IO (UpdateResult x d))
     -> IO (UpdateResult x b)
    runUpdateBind cmd f runner =
        runner cmd >>= \case
          UpdateResultExited x ->
            pure $ UpdateResultExited x
          UpdateResultValue{updateValue, updatedState} -> do
              runner (f updateValue) >>= \case
                UpdateResultExited x ->
                  pure $ UpdateResultExited x
                UpdateResultValue{updateValue = updateValue', updatedState = updatedState'} ->
                  pure $ UpdateResultValue updateValue' (updatedState || updatedState')

    runUpdateIO
     :: Component comp
     => IO (Maybe (ComponentAction comp))
     -> ComponentId
     -> IO (UpdateResult x ())
    runUpdateIO cmd cid = do
        void . Async.async $ do
          cmd >>= \case
            Nothing -> pure ()
            Just action' -> writeChan events (DynamicAction cid action')
        pure updateResult

componentContext
 :: Chan DynamicAction
 -> ComponentId
 -> IORef (HashMap ComponentId StoredComponent)
 -> IORef ComponentId
 -> IO ComponentContext
componentContext events currentComponentId nextComponents nextComponentId = do
  components <- readIORef nextComponents
  pure ComponentContext{..}

storeNewComponent
 :: (Component comp, Component parentComp)
 => ComponentContext
 -> comp (ComponentAction parentComp)
 -> ComponentState comp
 -> Widget (ComponentAction comp)
 -> Maybe (ComponentAction comp)
 -> IO (ComponentId, ComponentContext)
storeNewComponent ctx comp state view' action = do
  cid <- atomicModifyIORef (nextComponentId ctx) (\i -> (succ i, i))
  modifyIORef (nextComponents ctx) $ \components ->
    let pid = currentComponentId ctx
    in HashMap.insert cid (StoredComponent pid comp state view') components
  for_ action $
    writeChan (events ctx) . DynamicAction cid
  pure (cid, ctx{currentComponentId = cid})

getStoredComponent
 :: forall comp. Component comp
 => ComponentContext
 -> ComponentId
 -> (ComponentState comp, Widget (ComponentAction comp))
getStoredComponent ctx cid =
  getStoredComponent' cid (components ctx)

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
 => ComponentContext
 -> ComponentId
 -> comp (ComponentAction parentComp)
 -> ComponentState comp
 -> Widget (ComponentAction comp)
 -> IO ()
setStoredComponent ctx cid comp state view' = do
  setStoredComponent' (nextComponents ctx) cid comp state view'

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

removeComponent :: ComponentContext -> ComponentId -> IO ()
removeComponent ctx cid = do
  modifyIORef (nextComponents ctx) (HashMap.delete cid)

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

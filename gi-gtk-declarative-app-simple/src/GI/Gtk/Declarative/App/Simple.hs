{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

-- | A simple application architecture style inspired by PureScript's Pux
-- framework.
module GI.Gtk.Declarative.App.Simple
  ( App(..)
  , AppView
  , Transition(..)
  , run
  , runLoop
  )
where

import           Control.Concurrent
import qualified Control.Concurrent.Async      as Async
import           Control.Exception              ( SomeException,
                                                  Exception,
                                                  throwIO,
                                                  catch )
import           Control.Monad
import           Data.Typeable
import qualified GI.Gdk                        as Gdk
import qualified GI.GLib.Constants             as GLib
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State
import           Pipes
import           Pipes.Concurrent
import           System.Exit
import           System.IO

-- | Describes an state reducer application.
data App window state event =
  App
    { update       :: state -> event -> Transition state event
    -- ^ The update function of an application reduces the current state and
    -- a new event to a 'Transition', which decides if and how to transition
    -- to the next state.
    , view         :: state -> AppView window event
    -- ^ The view renders a state value as a window, parameterized by the
    -- 'App's event type.
    , inputs       :: [Producer event IO ()]
    -- ^ Inputs are pipes 'Producer's that feed events into the application.
    , initialState :: state
    -- ^ The initial state value of the state reduction loop.
    }

-- | The top-level widget for the 'view' function of an 'App',
-- requiring a GTK+ 'Window'.
type AppView window event = Bin window event

-- | The result of applying the 'update' function, deciding if and how to
-- transition to the next state.
data Transition state event =
  -- Transition to the given state, and with an IO action that may return a
  -- new event.
  Transition state (IO (Maybe event))
  -- | Exit the application.
  | Exit

-- | An exception thrown by the 'run' function when gtk's main loop exits
-- before event/state handling which should never happen but can be caused
-- by user code calling 'Gtk.mainQuit'
data GtkMainExitedException =
  GtkMainExitedException String deriving (Typeable, Show)

instance Exception GtkMainExitedException

-- | Initialize GTK and run the application in it. This is a
-- convenience function that is highly recommended. If you need more
-- flexibility, e.g. to set up GTK+ yourself, use 'runLoop' instead.
run
  :: Gtk.IsBin window
  => App window state event      -- ^ Application to run
  -> IO state
run app = do
  assertRuntimeSupportsBoundThreads
  void $ Gtk.init Nothing

  -- If any exception happen in `runLoop`, it will be re-throw here
  -- and the application will be killed.
  -- NOTE: the use of Aync.async for `Gtk.main` is *MANDATORY*. The documentation for `
  -- 'Async.concurrently' says that the first exception 'Async.cancel'
  -- the second thread only at the condition that it is not a foreign
  -- call, which is the case for 'Gtk.main'.
  -- By using a second `Async.async` call, the wrapping thread can be killed.
  fst <$> Async.concurrently (runLoop app <* Gtk.mainQuit) (Async.async Gtk.main)

-- | Run an 'App'. This IO action will loop, so run it in a separate thread
-- using 'async' if you're calling it before the GTK main loop.
-- Note: the following example take care of exception raised in
-- 'runLoop'. The wrapping of 'Gtk.main' with 'Async.async' is
-- mandatory.
--
-- @
--     void $ Gtk.init Nothing
--     concurrently (runLoop app <* Gtk.mainQuit) (Async.async Gtk.main)
-- @
runLoop :: Gtk.IsBin window => App window state event -> IO state
runLoop App {..} = do
  let firstMarkup = view initialState

  events                     <- newChan
  (firstState, subscription) <- do
    firstState <- runUI (create firstMarkup)
    runUI (Gtk.widgetShowAll =<< someStateWidget firstState)
    sub <- subscribe firstMarkup firstState (publishEvent events)
    return (firstState, sub)
  snd <$> Async.concurrently (void $ runEffect
    (mergeProducers inputs >-> publishInputEvents events))
    (loop firstState firstMarkup events subscription initialState
      -- Catch exception of linked thread and reraise them without the
      -- async wrapping.
      `catch` (\(Async.ExceptionInLinkedThread _ e) -> throwIO e)
    )

 where
  loop oldState oldMarkup events oldSubscription oldModel = do
    event <- readChan events
    case update oldModel event of
      Transition newModel action -> do
        let newMarkup = view newModel

        (newState, sub) <- case patch oldState oldMarkup newMarkup of
          Modify ma -> runUI $ do
            cancel oldSubscription
            newState <- ma
            sub      <- subscribe newMarkup newState (publishEvent events)
            return (newState, sub)
          Replace createNew -> runUI $ do
            destroy oldState oldMarkup
            cancel oldSubscription
            newState <- createNew
            Gtk.widgetShowAll =<< someStateWidget newState
            sub <- subscribe newMarkup newState (publishEvent events)
            return (newState, sub)
          Keep -> return (oldState, oldSubscription)

        -- If the action returned by the update function produced an event, then
        -- we write that to the channel.
        -- This is done in a thread to avoid blocking the event loop.
        a <- Async.async $
          -- TODO: Use prioritized queue for events returned by 'update', to take
          -- precendence over those from 'inputs'.
          action >>= maybe (return ()) (writeChan events)

        -- If any exception happen in the action, it will be reraised here and
        -- catched in the thread. See the ExceptionInLinkedThread
        -- catch.
        Async.link a

        loop newState newMarkup events sub newModel
      Exit -> return oldModel

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


publishEvent :: Chan event -> event -> IO ()
publishEvent mvar = void . writeChan mvar

mergeProducers :: [Producer a IO ()] -> Producer a IO ()
mergeProducers producers = do
  (output, input) <- liftIO $ spawn unbounded
  _               <- liftIO $ Async.mapConcurrently_ (fork output) producers
  fromInput input
 where
  fork :: Output a -> Producer a IO () -> IO ()
  fork output producer = do
    runEffect $ producer >-> toOutput output
    performGC

publishInputEvents :: Chan event -> Consumer event IO ()
publishInputEvents events = forever (await >>= liftIO . writeChan events)

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

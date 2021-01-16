{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE KindSignatures                #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE TypeFamilies                  #-}

module GI.Gtk.Declarative.Component.Internal
  ( Component(..)
  , UpdateM(..)
  , updateIO
  , updateIO_
  , updateParent
  , UpdateResult(..)
  , updateResult
  , ComponentId(..)
  , StoredComponent(..)
  , ComponentContext(..)
  , DynamicAction(..)
  )
where

import           Control.Concurrent
import           Control.Monad                 (ap, liftM)
import           Control.Monad.State.Class     (MonadState(get, put))
import           Data.Hashable                 (Hashable(..))
import           Data.HashMap.Strict           (HashMap)
import           Data.IORef                    (IORef)
import           Data.Typeable

import           GI.Gtk.Declarative.Widget

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
data UpdateResult x a
  = UpdateResultValue
      { updateValue :: a
      , updatedState :: Bool
      }
  | UpdateResultExited x

updateResult :: UpdateResult x ()
updateResult = UpdateResultValue
  { updateValue = ()
  , updatedState = False
  }

newtype ComponentId = ComponentId Int
  deriving (Show, Enum, Eq, Hashable)

data StoredComponent =
  forall c parent. (Component c, Component parent) => StoredComponent
    { parentComponentId :: !ComponentId
    , nonRootComponent :: !(c (ComponentAction parent))
    , storedState :: !(ComponentState c)
    , storedView :: !(Widget (ComponentAction c)) -- ^ Must only be updated inside `patch`, otherwise it gets out-of-sync with the state tree
    }

-- | Keeps track of the current component, and also the global component state,
-- when patching/subscribing-to components.
data ComponentContext = ComponentContext
  { currentComponentId :: ComponentId
  , components :: HashMap ComponentId StoredComponent
  , nextComponents :: IORef (HashMap ComponentId StoredComponent)
  , nextComponentId :: IORef ComponentId
  , events :: Chan DynamicAction
  }

-- | A dynamically typed action that is destined for a particular component.
data DynamicAction = forall comp. Component comp =>
  DynamicAction ComponentId (ComponentAction comp)

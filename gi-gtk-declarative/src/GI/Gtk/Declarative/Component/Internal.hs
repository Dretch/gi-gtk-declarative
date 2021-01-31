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

-- | A declarative component that can be used as a widget via the `component`
-- function. Types in this class are parameterized by the type of the events
-- that the component emits externally (i.e. the type that its parent receives).
class Typeable c => Component (c :: * -> *) where

  -- | The internal state of the component. This is initialized when the
  -- component is first shown, and then varies over the lifetime of the
  -- component as `patchComponent` and `update` modify it.
  data ComponentState c
  
  -- | Internal actions that are emitted by child widgets from the `view`
  -- method and are fed into the `update` method.
  data ComponentAction c

  -- | Called when the component is first shown. This creates the initial
  -- component state from the declarative component, and optionally
  -- provides an initial action.
  createComponent :: c event -> (ComponentState c, Maybe (ComponentAction c))

  -- | Called when a new declarative component is applied to the existing
  -- component state. This method should copy any required state from the
  -- new declarative component.
  --
  -- This method will never be called for root components (since the root
  -- declarative component never changes) so there is a default implementation
  -- that returns an error: components intended to be used as non-root components
  -- should implement this method.
  patchComponent :: ComponentState c -> c event -> ComponentState c
  patchComponent _ _ =
    error "patchComponent not implemented: it should be implemented for all non-top-level components."

  -- | Called when an event is emitted by a child widget. The `UpdateM`
  -- monad allows updating the component state, running asynchronous IO
  -- actions, and sending events to the parent component.
  update :: c event -> ComponentAction c -> UpdateM c event ()

  -- | Provides the tree of widgets that display this component. Called
  -- whenever the state changes (and possibly other times too).
  view :: c event -> ComponentState c -> Widget (ComponentAction c)

-- | An "update action" that is provided by `update` when a component of type
-- `c` receives an action from a child widget. The `event` type parameter
-- describes the type of events that the parent widget can receive.
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

-- | Sequences update actions. A sequence of actions will continue to be evaluated
-- until the root component emits an `Exit` event.
instance Monad (UpdateM c event) where
  (>>=) = UpdateBind

-- | Get/Set the internal state of the component. Setting the state triggers a re-render.
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
data DynamicAction = forall c. Component c =>
  DynamicAction ComponentId (ComponentAction c)

[![CI](https://github.com/Dretch/gi-gtk-declarative/workflows/CI/badge.svg)](https://github.com/Dretch/gi-gtk-declarative/actions)

# GI GTK Declarative + components + custom attributes

This is a half-finished fork of the wonderful [gi-gtk-declarative](https://github.com/owickstrom/gi-gtk-declarative) that adds _components_ and _custom attributes_.

## Components

The original gi-gtk-declarative comes with an Elm-style architecture where there is a single event handler for all events in the application.

This fork adds something more akin to React's components. Each component has it's own internal state and its own event handler. Inside this event handler a component can update its state, do asynchronous IO, and send events (synchronously) to the parent component. Components can communicate with their children by creating new declarative widgets (rather like updating props in React).

Components are supposed to enable abstraction and code-reuse.

### Complete Example

``` haskell
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Components where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( when )
import           Control.Monad.State.Class      ( get, modify, put )
import           Data.Text                      ( pack )

import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , Label(..)
                                                , Orientation(..)
                                                , Window(..)
                                                , WindowPosition(..) )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Widget      ()
import           GI.Gtk.Declarative.Component

-- The declarative version of this component. `event` is the type of events that
-- get emitted externally - i.e. sent to the parent component.
data IncButton event = IncButton
  { incEvent :: Int -> event -- ^ constructs an event to let the parent know that
                             --   the count has been incremented.
  }

-- A declarative component must implement the `Component` class.
instance Component IncButton where

  -- The internal state of the component.
  data ComponentState IncButton = IncButtonState Int

  -- The internal events that are fired by this components internal widget tree.
  data ComponentAction IncButton = Inc | Reset

  -- Creates the initial component state, and fires an initial event.
  createComponent IncButton{} =
    (IncButtonState 0, Just Inc)

  -- Called when the declarative component is updated so that the internal state
  -- can be updated.
  patchComponent state IncButton{} =
    state

  -- Handles internal events.
  update IncButton{..} = \case
    Reset -> do
      -- Set internal state.
      put $ IncButtonState 0
      notifyParent
    Inc -> do
      -- Modify internal state.
      modify $ \(IncButtonState i) -> IncButtonState (i + 1)
      notifyParent
      -- Run some IO that sleeps and then emits another action,
      -- causing an infinite loop of events (until the component
      -- is removed from the widget tree).
      updateIO $ Just Inc <$ threadDelay 1000000
    where
      notifyParent = do
        -- Get the internal state.
        IncButtonState i <- get
        -- Do some IO (without firing an event when it finishes):
        updateIO_ $ putStrLn ("about to tell parent that i = " <> show i)
        -- Send a message to the parent component:
        updateParent $ incEvent i

  -- Creates the declarative widget tree for this component.
  view IncButton{} (IncButtonState i) =
    widget Button
      [ #label := pack ("Reset (i = " <> show i <> ") to 0")
      , on #clicked Reset
      ]

-- The root component.
data App event = App
  { exitEvent :: event -- ^ This event lets the runtime know to quit the app.
  }

instance Component App where

  data ComponentState App = AppState

  data ComponentAction App = ReceiveInc Int | CloseWindow

  createComponent App{} =
    (AppState, Nothing)

  update App{..} = \case
    CloseWindow ->
      updateParent exitEvent
    ReceiveInc i -> do
      updateIO_ $ putStrLn ("a child told us that i = " <> show i)
      when (i == 10) $ do
        updateIO_ $ putStrLn "i == 10, that's enough!"
        updateParent exitEvent

  view App{} AppState =
    bin
      Window
      [ #title := "Components"
      , on #deleteEvent (const (True, CloseWindow))
      , #heightRequest := 100
      , #windowPosition := WindowPositionCenter
      ] $
      container Box
        [ #orientation := OrientationVertical
        , #margin := 4
        ]
        [ BoxChild
            defaultBoxChildProperties
            (widget Label [ #label := "The app will finish when i = 10" ])
        , BoxChild
            defaultBoxChildProperties
            -- Components can be used anywhere a regular widget can be used
            -- they just need to be turned into a `Widget e` via the `component`
            -- method. We pass `ReceiveInc` so that `IncButton` knows how to
            -- construct an event that `App` understands.
            (component (IncButton ReceiveInc))
        ]

-- This will start the GTK event loop, render the component widget tree, and keep
-- running component `update` methods when events occur, until the `Exit` event
-- is emitted by `App`.
main :: IO ()
main = run App{ exitEvent = Exit () }
```

### Bigger Example

This is an application built with components: https://github.com/Dretch/foundationdb-explorer/

## Custom Attributes

Normal attributes allow declaring values for GTK properties. However, often the GTK API requires you to call a method on a widget rather than write to an attribute.

Custom attributes maintain arbitrary internal state and patching behaviour, so they can call any GTK methods you like. This is rather like `CustomWidget` in gi-gtk-declarative, but in a more composable manner: you can use any number of custom attributes on any widget.

### Examples

- [Extra top-level windows, with a life-cycle tied to the widget the attribute is attached to.](https://github.com/Dretch/gi-gtk-declarative/blob/657a65f3e3fb86d98613466b95195ea4db88c938/gi-gtk-declarative/src/GI/Gtk/Declarative/Attributes/Custom/Window.hs#L38)
- [Setting a window icon.](https://github.com/Dretch/gi-gtk-declarative/blob/657a65f3e3fb86d98613466b95195ea4db88c938/gi-gtk-declarative/src/GI/Gtk/Declarative/Attributes/Custom/Window.hs#L89)
- [Presenting (focussing) a window when a value changes](https://github.com/Dretch/gi-gtk-declarative/blob/657a65f3e3fb86d98613466b95195ea4db88c938/gi-gtk-declarative/src/GI/Gtk/Declarative/Attributes/Custom/Window.hs#L64)

## Status

Unfinished, badly designed, undocumented, full of bugs. Really just for information at this point.

## FAQ

**Q. Why build this as a fork instead of a module (like gi-gtk-declarative-app-simple)?**

A 1. Custom attributes are, I think, impossible to implement as a module, because it needs changes throughout the core of the codebase.

A 2. It seems _almost_ possible to provide the component system as a module, but there is one blocker that so far I cannot solve. The `Functor` constraint on widgets means that events can't be `Typeable`, which is required in order to implement the dynamic typing involved in sending events to their parent components.

**Q. How does a running component decide to exit the application?**

A. The root component sends an `Exit x` event to its' parent (via `updateParent`), and this causes the runtime to stop running the GTK event loop and return the `x` value to the program that started the component in the first place.

**Q. Where can I find API Docs?**

A. [API Docs](https://dretch.github.io/gi-gtk-declarative/docs/haddock/)

**Q. What happens if the view created by the top-level event is not a Window widget?**

A. Wierd stuff happens, unfortunately. In future the type system might enforce that root components always creates a window.

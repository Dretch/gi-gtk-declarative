[![CI](https://github.com/Dretch/gi-gtk-declarative/workflows/CI/badge.svg)](https://github.com/Dretch/gi-gtk-declarative/actions)

# GI GTK Declarative + components + custom attributes

This is a half-finished fork of the wonderful [gi-gtk-declarative](https://github.com/owickstrom/gi-gtk-declarative) that adds _components_ and _custom attributes_.

## Components

The original gi-gtk-declarative comes with an Elm-style architecture where there is a single event handler for all events in the application.

This fork adds something more akin to React's components. Each component has it's own internal state and its own event handler. Inside this event handler a component can update its state, do asynchronous IO, and send events (synchronously) to the parent component. Components can communicate with their children by creating new declarative widgets (rather like updating props in React).

Components are supposed to enable abstraction and code-reuse.

### Quick Example

#### Defining a component

``` haskell

-- The declarative version of the component. The argument is a kind-of
-- callback: it converts an event into the parent components event
-- type so that we can send a message to the parent
data IncButton event = IncButton (Int -> event)

instance Component IncButton where

  -- The internal state of the component
  data ComponentState IncButton = IncButtonState Int

  -- The internal events that are fired by this components internal widget tree.
  data ComponentAction IncButton = Inc | Reset

  -- creates the initial component state, and fires an initial event
  createComponent (IncButton _) = (IncButtonState 0, Just Inc)

  -- called when the declarative component is updated so that the internal state can be updated
  patchComponent state (IncButton _) = state

  -- handles internal events
  update (IncButton cb) = \case
    Reset -> do
      put $ IncButtonState 0 -- set internal state
      notifyParent
    Inc -> do
      modify $ \(IncButtonState i) -> IncButtonState (i + 1)
      notifyParent
      updateIO $ Just Inc <$ threadDelay 1000000 -- do some IO, and fire an event when it finishes
    where
      notifyParent = do
        IncButtonState i <- get
        updateIO_ $ putStrLn ("about to tell parent that i = " <> show i)
        updateParent $ cb i -- send a message to the parent component

  -- create the declarative widget tree for this component
  view (IncButton _cb) (IncButtonState i) =
    widget Button
      [ #label := pack ("Reset (i = " <> show i <> ") to 0")
      , on #clicked Reset
      ]
```

#### Using a component

Components can be used anywhere a regular widget can be used.

``` haskell

-- define an event for when the child component invokes its callback
data ComponentAction SomeParentComponent =
  ParentComponentCallback Int

-- turn the declarative component into a Widget (ComponentAction SomeParentComponent)
component (IncButton ParentComponentCallback)
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

A. I tried, but could not find a way to pass the component "context" around when creating/patching components.

**Q. How does a running component decide to exit the application?**

A. The root component sends an `Exit x` event to its' parent (via `updateParent`), and this causes the runtime to stop running the GTK event loop and return the `x` value to the program that started the component in the first place.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Windows where

import           Control.Concurrent            (threadDelay)
import           Control.Monad                 (void)
import           Data.Text                     (pack)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as Vector
import           Pipes.Prelude                 (repeatM)

import           GI.Gtk                        (Box (..), Button (..),
                                                Label (..), Orientation (..),
                                                Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Attributes.Custom.Window (presentWindow, window)
import           GI.Gtk.Declarative.App.Simple

data WindowState = WindowState
  { windowStateCount :: Int
  , windowStatePresented :: Int
  }

type State = Vector (Maybe WindowState)

data Event
  = IncrAll
  | AddWindow
  | PresentWindow Int
  | CloseWindow Int
  | RemoveWindow
  | Closed

view' :: State -> AppView Window Event
view' ws =
  bin
      Window
      [ #title := "Windows"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 200
      , #heightRequest := 300
      ]
    $  container
         Box
         [#orientation := OrientationVertical, #spacing := 4, #margin := 4]
    $  [addButton, removeButton ws]
    <> Vector.imap windowLabel ws

addButton :: BoxChild Event
addButton = BoxChild defaultBoxChildProperties
  $ widget Button [#label := "Add Window", on #clicked AddWindow]

removeButton :: State -> BoxChild Event
removeButton ns = BoxChild defaultBoxChildProperties $ widget
  Button
  [ #label := "Remove Window"
  , #sensitive := not (Vector.null ns)
  , on #clicked RemoveWindow
  ]

windowLabel :: Int -> Maybe WindowState -> BoxChild Event
windowLabel i ws =
  BoxChild defaultBoxChildProperties
    $ windowChild i ws

mkWindow :: Int -> WindowState -> Bin Window Event
mkWindow i WindowState {..} = bin
  Window
  [ #title := pack ("Window " <> show i)
  , on #deleteEvent (const (True, CloseWindow i))
  , presentWindow windowStatePresented
  ] $
  container Box [#orientation := OrientationVertical, #spacing := 4, #margin := 4]
   [ widget
       Label
       [#label := pack ("Open for " <> show windowStateCount <> " seconds")]
   , widget
       Button
       [#label := "Close Window", on #clicked (CloseWindow i)]
   ]

windowChild :: Int -> Maybe WindowState -> Widget Event
windowChild i = \case
  Nothing -> widget Label [#label := pack ("Window " <> show i <> " Closed")]
  Just ws  -> widget
    Button
    [ #label := pack ("Present window " <> show i)
    , on #clicked $ PresentWindow i
    , window $ mkWindow i ws
    ]

update' :: State -> Event -> Transition State Event
update' ws = \case
  IncrAll ->
    mapWindows ws (\_i w -> w { windowStateCount = windowStateCount w + 1 })
  AddWindow ->
    Transition (ws `Vector.snoc` Just (WindowState 1 1)) (pure Nothing)
  PresentWindow i -> mapWindows
    ws
    (\i' w -> if i' == i
      then w { windowStatePresented = windowStatePresented w + 1 }
      else w
    )
  CloseWindow i -> Transition
    (Vector.imap (\i' w -> if i' == i then Nothing else w) ws)
    (pure Nothing)
  RemoveWindow -> Transition (Vector.init ws) (pure Nothing)
  Closed       -> Exit

mapWindows :: State -> (Int -> WindowState -> WindowState) -> Transition State e
mapWindows windows f =
  Transition (Vector.imap (\i w -> fmap (f i) w) windows) (pure Nothing)

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = [incrPeriodically]
  , initialState = []
  }
  where incrPeriodically = repeatM $ IncrAll <$ threadDelay (1000 * 1000)
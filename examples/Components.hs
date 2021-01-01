{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Components where

import           Control.Monad                  (when)
import           Control.Monad.State.Class      (MonadState(get, put))
import           Data.Text                      (pack)

import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , Label(..)
                                                , Orientation(..)
                                                , Window(..)
                                                , WindowPosition(..) )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Widget      ()
import           GI.Gtk.Declarative.Components

data IncButton event = IncButton (Int -> event)

instance Component IncButton where

  data ComponentState IncButton = IncButtonState Int

  data ComponentAction IncButton = IncCounter

  createComponent _decl = IncButtonState 0

  patchComponent state _decl = state

  update (IncButton cb) = \case
    IncCounter -> do
      IncButtonState i <- get
      let i' = i + 1
      put $ IncButtonState i'
      updateIO_ $ putStrLn ("about to tell parent that i = " <> show i')
      updateParent $ cb i'

  view (IncButtonState i) =
    widget Button
      [ #label := pack ("Increment i (current value = " <> show i <> ")")
      , on #clicked IncCounter
      ]

data ExampleApp event = ExampleApp (AppAction -> event)

instance Component ExampleApp where

  data ComponentState ExampleApp = ExampleAppState

  data ComponentAction ExampleApp = ReceiveInc Int | CloseWindow

  createComponent _decl = ExampleAppState

  update (ExampleApp cb) = \case
    CloseWindow ->
      updateParent (cb Exit)
    ReceiveInc i -> do
      updateIO_ $ putStrLn ("a child told us that i = " <> show i)
      when (i == 10) $ do
        updateIO_ $ putStrLn "i == 10, that's enough!"
        updateParent (cb Exit)

  view _state =
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
            (component (IncButton ReceiveInc))
        ]

main :: IO ()
main = run ExampleApp

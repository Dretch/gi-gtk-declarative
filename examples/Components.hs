{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Components where

import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (when)
import           Control.Monad.State.Class      (MonadState(get, put), modify)
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

  data ComponentAction IncButton = Inc | Reset

  createComponent _decl = (IncButtonState 0, Just Inc)

  patchComponent state _decl = state

  update (IncButton cb) = \case
    Reset -> do
      put $ IncButtonState 0
      notifyParent
    Inc -> do
      modify $ \(IncButtonState i) -> IncButtonState (i + 1)
      notifyParent
      updateIO $ Just Inc <$ threadDelay 1000000
    where
      notifyParent = do
        IncButtonState i <- get
        updateIO_ $ putStrLn ("about to tell parent that i = " <> show i)
        updateParent $ cb i

  view (IncButton _cb) (IncButtonState i) =
    widget Button
      [ #label := pack ("Reset (i = " <> show i <> ") to 0")
      , on #clicked Reset
      ]

data ExampleApp event = ExampleApp (AppAction -> event)

instance Component ExampleApp where

  data ComponentState ExampleApp = ExampleAppState

  data ComponentAction ExampleApp = ReceiveInc Int | CloseWindow

  createComponent _decl = (ExampleAppState, Nothing)

  update (ExampleApp cb) = \case
    CloseWindow ->
      updateParent (cb Exit)
    ReceiveInc i -> do
      updateIO_ $ putStrLn ("a child told us that i = " <> show i)
      when (i == 10) $ do
        updateIO_ $ putStrLn "i == 10, that's enough!"
        updateParent (cb Exit)

  view _decl _state =
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

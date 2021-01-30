{-# LANGUAGE DeriveFunctor     #-}
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

data IncButton event = IncButton { incEvent :: Int -> event }

instance Component IncButton where

  data ComponentState IncButton = IncButtonState Int

  data ComponentAction IncButton = Inc | Reset

  createComponent IncButton{} =
    (IncButtonState 0, Just Inc)

  patchComponent state IncButton{} =
    state

  update IncButton{..} = \case
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
        updateParent $ incEvent i

  view IncButton{} (IncButtonState i) =
    widget Button
      [ #label := pack ("Reset (i = " <> show i <> ") to 0")
      , on #clicked Reset
      ]

data App event = App { exitEvent :: event }

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
            (component (IncButton ReceiveInc))
        ]

main :: IO ()
main = run App{ exitEvent = Exit () }

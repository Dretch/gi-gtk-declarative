{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
module GI.Gtk.Declarative.ComponentTest where

import           Control.Concurrent           (threadDelay)
import           Control.Monad.State.Class    (modify)
import qualified GI.Gtk                       as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Component
import           System.Timeout
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "run" $ do
    it "finishes app on Exit with passed value" $ do
      runApp app{ initialEvent = Just (ExitWith 42) } >>= (`shouldBe` Just 42)
    it "finishes app on Exit when IO still going" $ do
      runApp app{ initialEvent = Just StartSleepThenExit } >>= (`shouldBe` Just 0)
    -- Propagating exception from the view/patch/update is important to crash
    -- the application instead of keeping it in a weird state.
    it "propagates exceptions from createComponent function" $
      runApp app{ createError = True } `shouldThrow` errorCall "oh no"
    it "propagates exceptions from patchComponent function" $
      runApp app{ initialEvent = Just IncState, patchError = True } `shouldThrow` errorCall "oh no"
    it "propagates exceptions from view function" $
      runApp app{ viewError = True } `shouldThrow` errorCall "oh no"
    it "propagates exceptions from update function" $
      runApp app{ initialEvent = Just ThrowError } `shouldThrow` errorCall "oh no"
  where
    app = App
      { createError = False
      , patchError = False
      , viewError = False
      , initialEvent = Nothing
      , exitEvent = Exit
      }
    runApp = timeout 1000000 . run

data App event = App
  { createError :: Bool
  , patchError :: Bool
  , viewError :: Bool
  , initialEvent :: Maybe (ComponentAction App)
  , exitEvent :: Int -> event
  }

instance Component App where

    data ComponentState App = AppState Int

    data ComponentAction App
      = IncState
      | ExitWith Int
      | StartSleepThenExit
      | SleepForever
      | ThrowError

    createComponent App{..} =
      if createError then
        error "oh no"
      else
        (AppState 0, initialEvent)
    
    update App{..} = \case
      IncState ->
        modify $ \(AppState i) -> AppState (i + 1)
      ExitWith i ->
        updateParent (exitEvent i)
      StartSleepThenExit -> do
        updateIO $ Just SleepForever <$ threadDelay 1000000
        updateParent (exitEvent 0)
      SleepForever ->
        updateIO $ Just SleepForever <$ threadDelay 1000000
      ThrowError ->
        error "oh no"

    view App{..} (AppState i) =
      if viewError then
        error "oh no"
      else
        bin Gtk.Window [] (component InnerComponent{ innerN = i, innerPatchError = patchError } )

data InnerComponent event = InnerComponent
  { innerPatchError :: Bool
  , innerN :: Int
  }

instance Component InnerComponent where

  data ComponentState InnerComponent = InnerComponentState

  data ComponentAction InnerComponent

  createComponent _decl =
    (InnerComponentState, Nothing)
  
  patchComponent state InnerComponent{..} =
    if innerPatchError then
      error "oh no"
    else
      state
  
  update _decl = \case

  view _decl _state =
    widget Gtk.Label []

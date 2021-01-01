{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Implementation of 'Gtk.Grid' as a declarative container.
module GI.Gtk.Declarative.Container.Grid
  ( GridChild(..)
  , GridChildProperties(..)
  , defaultGridChildProperties
  )
where

import           Data.Default.Class             ( Default(def) )
import           Data.Int                       ( Int32 )
import           Data.Vector                    ( Vector )
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Widget

-- | Describes a child widget to be added with to a 'Grid'.
data GridChild event =
  GridChild
    { properties :: GridChildProperties
    , child      :: Widget event
    }

-- | Values used when /packing/ child widgets into grids.
data GridChildProperties =
  GridChildProperties
    { height     :: Int32
    , width      :: Int32
    , leftAttach :: Int32
    , topAttach  :: Int32
    }
  deriving (Eq, Show)

-- | Defaults for 'GridChildProperties'. Use these and override
-- specific fields.
defaultGridChildProperties :: GridChildProperties
defaultGridChildProperties =
  GridChildProperties { height = 1, width = 1, leftAttach = 0, topAttach = 0 }

instance Default GridChildProperties where
  def = defaultGridChildProperties

instance Patchable GridChild where
  create ctx = create ctx . child
  patch ctx s b1 b2 | properties b1 == properties b2 = patch ctx s (child b1) (child b2)
                        | otherwise                      = Replace (create ctx b2)
  destroy ctx s b = destroy ctx s (child b)

instance EventSource GridChild where
  subscribe ctx GridChild {..} = subscribe ctx child

instance ToChildren Gtk.Grid Vector GridChild

instance IsContainer Gtk.Grid GridChild where
  appendChild grid GridChild { properties } widget' = do
    let GridChildProperties { width, height, leftAttach, topAttach } =
          properties
    Gtk.gridAttach grid widget' leftAttach topAttach width height
  replaceChild grid _ destroyOld gridChild' new = do
    destroyOld
    appendChild grid gridChild' new

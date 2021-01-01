-- | A clunky hack to support gi-gtk-declarative-components
module GI.Gtk.Declarative.Context
  ( Context(..)
  , empty
  )
where

import Data.TMap (TMap)
import qualified Data.TMap as TMap

data Context = Context { map :: TMap }

empty :: Context
empty = Context TMap.empty
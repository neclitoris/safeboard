{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}
module Quotes.Slices (mkSlice) where

import Data.Vector.Lift
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

mkSlice :: forall a . (Storable a, Lift a) => Int -> Int -> V.Vector a -> TExpQ (V.Vector a)
mkSlice i n vec = let nVec = V.slice i n vec in [|| nVec ||]

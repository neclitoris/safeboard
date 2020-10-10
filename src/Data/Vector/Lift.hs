{-# LANGUAGE TemplateHaskell #-}
module Data.Vector.Lift where

import Data.Vector.Storable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

instance (Storable a, Lift a) => Lift (Vector a) where
    lift = appE (varE 'fromList) . lift . toList

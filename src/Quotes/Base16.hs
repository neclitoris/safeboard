{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}
module Quotes.Base16 (base16) where

import Data.Vector.Lift
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Parsers.Base16

base16 :: forall a . (Storable a, Lift a) => String -> TExpQ (V.Vector a)
base16 s =
  case parseBase16 s of
    Left err -> fail (show err)
    Right vec -> [|| vec ||]

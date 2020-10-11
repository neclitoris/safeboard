{-# LANGUAGE TemplateHaskell #-}
module Quotes.DataURL (dataURL) where

import Data.Vector.Lift
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Parsers.DataURL

dataURL :: (Storable a, Lift a) => String -> TExpQ (V.Vector a)
dataURL s =
  case parseDataURL s of
    Left err -> fail (show err)
    Right vec -> [|| vec ||]

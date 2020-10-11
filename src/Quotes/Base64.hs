{-# LANGUAGE TemplateHaskell #-}
module Quotes.Base64 (base64) where

import Data.Vector.Lift
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Parsers.Base64

base64 :: (Storable a, Lift a) => String -> TExpQ (V.Vector a)
base64 s =
  case parseBase64 s of
    Left err -> fail (show err)
    Right vec -> [|| vec ||]

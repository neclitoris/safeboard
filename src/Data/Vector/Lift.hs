{-# LANGUAGE TemplateHaskell #-}
module Data.Vector.Lift where

import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Storable
import GHC.ForeignPtr
import GHC.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

instance (Storable a) => Lift (V.Vector a) where
  lift v = [|
    unsafePerformIO $ do
      ptr <- newForeignPtr_ (Ptr $(bytes))
      return $ V.unsafeFromForeignPtr ptr offset length
    |]
    where
      bytes = litE $ StringPrimL $ V.toList $ (V.unsafeCast v :: V.Vector Word8)
      offset = 0 :: Int
      length = V.length v

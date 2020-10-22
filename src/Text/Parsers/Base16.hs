{-# LANGUAGE ScopedTypeVariables #-}
module Text.Parsers.Base16 ( parseBase16 ) where

import Control.Monad
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Storable
import Numeric (readHex)
import Text.Parsec
import Text.Printf

base16Byte :: Parsec String () Word8
base16Byte = spaces *> (toEnum . fst . head . readHex <$> replicateM 2 hexDigit)

base16 :: forall a . Storable a => Parsec String () (V.Vector a)
base16 = V.unsafeCast . V.concat <$> many (try batch) <* spaces <* eof
  where batch = V.replicateM len base16Byte <?> printf "%s bytes" (show len)
        len = sizeOf (undefined :: a)


parseBase16 :: Storable a => String -> Either ParseError (V.Vector a)
parseBase16 = parse base16 ""

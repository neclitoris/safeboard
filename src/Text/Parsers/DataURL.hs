{-# LANGUAGE ScopedTypeVariables #-}
module Text.Parsers.DataURL ( parseDataURL ) where

import Control.Monad
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Storable
import Numeric
import Text.Parsec
import Text.Printf

dataURLByte :: Parsec String () Word8
dataURLByte = fromIntegral . fromEnum <$> unreserved <|> encoded
  where
    unreserved = spaces *> noneOf ":/?#[]@!$&\"\"()*+';=%"
    fromHex    = fromIntegral . fst . head . readHex
    encoded    = spaces *> char '%' *> (fromHex <$> replicateM 2 hexDigit)

dataURL :: forall a . Storable a => Parsec String () (V.Vector a)
dataURL = V.unsafeCast . V.concat <$> many (try batch) <* spaces <* eof
  where batch = V.replicateM len dataURLByte <?> printf "%s bytes" (show len)
        len = sizeOf (undefined :: a)


parseDataURL :: Storable a => String -> Either ParseError (V.Vector a)
parseDataURL = parse dataURL ""

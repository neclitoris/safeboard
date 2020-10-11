{-# LANGUAGE ScopedTypeVariables #-}
module Text.Parsers.Base64 ( parseBase64 ) where

import Control.Monad (when)
import Data.Bits
import Data.Char
import Data.Default
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Storable
import Text.Parsec
import Text.Printf (printf)

import Prelude hiding (concat, (++))

data Base64C = Value Word8 | Padding deriving (Eq)

-- User state contains data which doesn't belong to a parsed byte (yet).
data Filled = Filled Int Word16

instance Default Filled where
  def = Filled 0 0

base64Char :: Parsec String Filled Base64C
base64Char =
  spaces
    *>  (Padding <$ char '=' <|> fromChar <$> valid)
    <?> "valid base64 character"
    where
      chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
      valid = oneOf chars
      fromChar = Value . toEnum . fromJust . (`elemIndex` chars)

-- Parse a single byte from base64 string. Abuses parsec user state
-- to store data needed for next byte. This allows parsing bytes one
-- by one.
base64Byte :: Parsec String Filled Word8
base64Byte = try $ do
  Filled have val <- getState
  if have >= 8
    then do
      putState $ Filled (have - 8) (val `shiftL` 8)
      return $ fromIntegral (val `shiftR` 8)
    else do
      tok <- base64Char
      when (tok == Padding) $ unexpected "'=' character"
      let (Value x) = tok
          addtl     = fromIntegral x `shiftL` (10 - have)
      putState $ Filled (have + 6) (val .|. addtl)
      base64Byte

-- Used to enforce valid padding at the end of base64 string. Padding is not
-- necessary to parse base64, but I decided to enforce it.
base64Padding :: Parsec String Filled ()
base64Padding = do
  Filled have val <- getState
  when (val /= 0) $ unexpected "last bits before padding must be zero"
  case have of
    0 -> pure ()
    2 -> () <$ string "="
    4 -> () <$ string "=="
    _ -> unexpected "bug in base64 parser"

base64 :: forall a . Storable a => Parsec String Filled (V.Vector a)
base64 =
  V.unsafeCast . V.concat <$> many batch <* base64Padding <* spaces <* eof
    where batch = V.replicateM len base64Byte <?> printf "%s bytes" (show len)
          len = sizeOf (undefined :: a)


parseBase64 :: Storable a => String -> Either ParseError (V.Vector a)
parseBase64 = runParser base64 def ""

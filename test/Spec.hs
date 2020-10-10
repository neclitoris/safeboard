{-# LANGUAGE TemplateHaskell #-}

import Data.Either
import qualified Data.Vector.Storable as V
import Data.Word
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Printf

import Quotes.Base16
import Quotes.Base64
import Quotes.DataURL
import Quotes.Slices

import Language.Haskell.TH.TestUtils
import Test.Tasty
import Test.Tasty.HUnit

example :: V.Vector Word8
example = V.fromList $ map (toEnum . fromEnum) "example text"

mple :: V.Vector Word8
mple = V.slice 3 4 example

tests :: TestTree
tests = testGroup "tests" [quotes, slices]

quotes :: TestTree
quotes = testGroup "quotes"
  [ testGroup "base64"
    [

    testCase "valid expression" $(do
      res <- tryTestQ unmockedState $
        unTypeQ (base64 " ZXhhbX BsZQ== " :: TExpQ (V.Vector Word8))
      runIO $
        isRight res @? printf "Unexpected error: %s" (show $ fmap ppr res)
      [| return () |]
    )

    , testCase "wrong padding" $(do
      res <- tryTestQ unmockedState $
        unTypeQ (base64 " ZXhhbX BsZQ= " :: TExpQ (V.Vector Word8))
      runIO $
        isLeft res @? printf "Expected error, got %s" (show $ fmap ppr res)
      [| return () |]
    )

    , testCase "wrong target size" $(do
      res <- tryTestQ unmockedState $
        unTypeQ (base64 " ZXhhbX BsZQ== " :: TExpQ (V.Vector Word32))
      runIO $
        isLeft res @? printf "Expected error, got %s" (show $ fmap ppr res)
      [| return () |]
    )

    , testCase "result" $
      ($$(base64 " ZXhhbX BsZSB0 ZXh0 ") :: V.Vector Word8) @?= example

    ]
  , testGroup "base16"
    [

    testCase "valid expression" $(do
      res <- tryTestQ unmockedState $
        unTypeQ (base16 "6578616D706C65" :: TExpQ (V.Vector Word8))
      runIO $
        isRight res @? printf "Unexpected error: %s" (show $ fmap ppr res)
      [| return () |]
    )

    , testCase "invalid expression" $(do
      res <- tryTestQ unmockedState $
        unTypeQ (base16 "6578616D706C6" :: TExpQ (V.Vector Word8))
      runIO $
        isLeft res @? printf "Expected error, got %s" (show $ fmap ppr res)
      [| return () |]
    )

    , testCase "wrong target size" $(do
      res <- tryTestQ unmockedState $
        unTypeQ (base16 "6578616D706C6" :: TExpQ (V.Vector Word32))
      runIO $
        isLeft res @? printf "Expected error, got %s" (show $ fmap ppr res)
      [| return () |]
    )

    , testCase "result" $
      ($$(base16 "657861 6D706C 652074 657874") :: V.Vector Word8) @?= example

    ]
    , testGroup "dataURL"
    [

    testCase "valid expression" $(do
      res <- tryTestQ unmockedState $
        unTypeQ (dataURL "example%20text" :: TExpQ (V.Vector Word8))
      runIO $
        isRight res @? printf "Unexpected error: %s" (show $ fmap ppr res)
      [| return () |]
    )

    , testCase "invalid expression" $(do
      res <- tryTestQ unmockedState $
        unTypeQ (dataURL "example%2text" :: TExpQ (V.Vector Word8))
      runIO $
        isLeft res @? printf "Expected error, got %s" (show $ fmap ppr res)
      [| return () |]
    )

    , testCase "wrong target size" $(do
      res <- tryTestQ unmockedState $
        unTypeQ (dataURL "example" :: TExpQ (V.Vector Word32))
      runIO $
        isLeft res @? printf "Expected error, got %s" (show $ fmap ppr res)
      [| return () |]
    )

    , testCase "result" $
      ($$(dataURL "example%20text") :: V.Vector Word8) @?= example

    ]
  ]

slices :: TestTree
slices = testGroup "slices"
  [
  testCase "example" $
    $$(mkSlice 3 4 ($$(base64 " ZXhhbX BsZQ== ") :: V.Vector Word8)) @?= mple
  ]

main :: IO ()
main = defaultMain tests

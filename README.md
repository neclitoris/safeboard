# Safeboard

This is a library for [Safeboard](https://safeboard.kaspersky.ru/) entrance test.

# Usage

```
GHCi> let vec = $$(base64 "ZXhhbX BsZQ==") :: Vector Word8
GHCi> vec
[101,120,97,109,112,108,101]
```

Byte size of the expression is checked automagically (compile time):
```
GHCi> $$(base64 "ZXhhbX BsZQ==") :: Vector Word32

<interactive>:4:4: error:
    • (line 1, column 13):
unexpected '=' character
    • In the Template Haskell splice $$(base64 "ZXhhbX BsZQ==")
      In the expression: $$(base64 "ZXhhbX BsZQ==") :: Vector Word32
      In an equation for ‘it’:
          it = $$(base64 "ZXhhbX BsZQ==") :: Vector Word32
```

Generated code:
```
GHCi> expr <- runQ $ unTypeQ (base64 "ZXhhbX BsZQ==" :: TExpQ (Vector Word8))
GHCi> putStrLn $ pprint expr
GHC.IO.Unsafe.unsafePerformIO GHC.Base.$ (do {ptr_0 <- GHC.ForeignPtr.newForeignPtr_ (GHC.Ptr.Ptr "<binary data>");
                                              GHC.Base.return GHC.Base.$ Data.Vector.Storable.unsafeFromForeignPtr ptr_0 0 7})
```

This looks scary, but the resulting vector simply points to data in the compiled binary, so there is no runtime overhead.

Slices:
```
GHCi> $$(mkSlice 3 4 vec)
[109, 112, 108, 101]
```

Generates another vector in the binary just for kicks. `Data.Vector.Storable.slice` doesn't do that and is also O(1).

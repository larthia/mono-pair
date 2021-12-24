{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Pair.Mono as M

import GHC.Exts
    ( unsafeCoerce#, Int(I#), (+#), sizeofArray#, unpackClosure# )
import Foreign ( Storable(sizeOf) )

import Data.Int ( Int8 )
import Data.Word ( Word64 )

import qualified Data.Aeson as A ( decode, encode )
import qualified Data.ByteString as B

import Codec.Serialise
    ( deserialiseOrFail, serialise, DeserialiseFailure )

unsafeSizeof :: a -> Int
unsafeSizeof a =
  case unpackClosure# a of
    (# x, ptrs, nptrs #) ->
      sizeOf (undefined::Int) + -- one word for the header
        I# (sizeofArray# (unsafeCoerce# ptrs)
             +# sizeofArray# nptrs)

main :: IO ()
main = do
    putStrLn $ " Pair Int         -> SIZE: " <> show (unsafeSizeof $ M.pair (1::Int8) (2 :: Int8))
    putStrLn $ "(Int,Int)         -> SIZE: " <> show (unsafeSizeof (1 :: Int8, 2 :: Int8))
    putStrLn $ " Pair Bool        -> SIZE: " <> show (unsafeSizeof $ M.pair True False)
    putStrLn $ "(Bool,Bool)       -> SIZE: " <> show (unsafeSizeof (True,False))
    putStrLn $ " Pair Word64      -> SIZE: " <> show (unsafeSizeof $ M.pair (1::Word64) (2 :: Word64))
    putStrLn $ "(Word64, Word64)  -> SIZE: " <> show (unsafeSizeof (1::Word64, 2 :: Word64))
    putStrLn $ "(Word64, Word64)  -> JSON: " <> show (A.encode (1 :: Int, 2 :: Int))
    putStrLn $ "(Pair Word64)     -> JSON: " <> show (A.encode (M.pair (1 :: Int) (2 :: Int)))

    let p1 = A.decode "[1, 2]" :: Maybe (Int, Int)
    putStrLn $ "JSON: " <> show p1

    let p2 = A.decode "[1, 2, 3]" :: Maybe (Int, Int)
    putStrLn $ "JSON: " <> show p2

    let p3 = A.decode "[1, 2]" :: Maybe (M.Pair Int)
    putStrLn $ "JSON: " <> show p3

    let p4 = A.decode "[1, 2, 3]" :: Maybe (M.Pair Int)
    putStrLn $ "JSON: " <> show p4

    let m = M.pair "a" "b" :: M.Pair String
    putStrLn $ "Monoid: " <> show (m <> m)

    let m2 = M.pair 1 2 :: M.Pair Int
    putStrLn $ "CBOR: " <> show (deserialiseOrFail (serialise m2) :: Either DeserialiseFailure (M.Pair Int))
    putStrLn $ "CBOR: " <> show (deserialiseOrFail (serialise ((3,4) :: (Int, Int))) :: Either DeserialiseFailure (M.Pair Int))
    putStrLn $ "CBOR: " <> show (deserialiseOrFail (serialise ((3,2.3) :: (Int, Float))) :: Either DeserialiseFailure (M.Pair Int))
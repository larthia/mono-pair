{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Data.Pair.Mono (
    PairClass(..)
  , toLazy
  , toStrict
  , swap
  , zip
  , unzip
  , curry
  , uncurry
) where

import Prelude hiding (fst, snd, zip, unzip, curry, uncurry)
import qualified Prelude as P (Maybe(..))

import GHC.Stack.Types ( HasCallStack )
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Monoid ()
import Data.Semigroup ( Semigroup(stimes) )
import Data.Kind ( Type )
import Codec.Serialise.Class ( Serialise(decode, encode) )
import qualified Data.Store as S
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
import Data.Binary (Binary (..))
import Data.Functor ( (<&>) )
import Foreign.Storable (Storable, sizeOf)


class PairClass a where
  data Pair a :: Type
  fst  :: Pair a -> a
  snd  :: Pair a -> a
  pair :: a -> a -> Pair a

-- | Convert a mono pair to a lazy version.
toLazy :: (PairClass a) => Pair a -> (a, a)
toLazy p = (fst p, snd p)
{-# INLINE toLazy #-}

-- | Convert a mono pair to a strict version.
toStrict :: (PairClass a) => (a,a) -> Pair a
toStrict (f,s) = pair f s
{-# INLINE toStrict #-}

swap :: (PairClass a) => Pair a -> Pair a
swap p = pair (snd p) (fst p)
{-# INLINE swap #-}

-- | Zip for strict mono pairs.
zip :: (PairClass a) => [a] -> [a] -> [Pair a]
zip = L.zipWith pair
{-# INLINE zip #-}

-- | Unzip for strict mono pairs into a (lazy) pair of lists.
unzip :: (PairClass a) => [Pair a] -> ([a], [a])
unzip x = ( map fst x
          , map snd x
          )
{-# INLINE unzip #-}

-- | Curry a function on strict mono pairs.
curry :: (PairClass a) => (Pair a -> c) -> a -> a -> c
curry f x y = f (pair x y)
{-# INLINE curry #-}

-- | Convert a curried function to a function on strict mono pairs.
uncurry :: (PairClass a) => (a -> a -> c) -> Pair a -> c
uncurry f p = f (fst p) (snd p)
{-# INLINE uncurry #-}


instance (Eq a, PairClass a) => Eq (Pair a) where
    x == y = fst x == fst y && snd x == snd y

instance (Ord a, PairClass a) => Ord (Pair a) where
    x `compare` y = fst x `compare` fst y
                      <> snd x `compare` snd y

instance (Semigroup a, PairClass a) => Semigroup (Pair a) where
        a <> b = pair (fst a <> fst b) (snd a <> snd b)
        stimes n a = pair (stimes n (fst a)) (stimes n (snd a))

instance (Monoid a, PairClass a) => Monoid (Pair a) where
        mempty = pair mempty mempty

instance (Show a, PairClass a) => Show (Pair a) where
    show p = "(" <> show (fst p) <> "," <> show (snd p) <> ")"

instance (Read a, PairClass a) => Read (Pair a) where
  readsPrec n =  readsPrec n <&> fmap (\((x,y), s) -> (pair x y, s))


instance (PairClass a, A.ToJSON a) => A.ToJSON (Pair a) where
    toJSON p = A.toJSON [ fst p , snd p ]

instance (PairClass a, A.FromJSON a) => A.FromJSON (Pair a) where
    parseJSON v@(A.Array _) = do
      [fst, snd] <- A.parseJSON v
      return $ pair (fst :: a) (snd :: a)
    parseJSON _ = fail "Expected an Array"

instance (PairClass a, Show a, A.ToJSON a) => A.ToJSONKey (Pair a) where
     toJSONKey = A.toJSONKeyText (T.pack . show . fst)

instance (S.Store a, Storable a, PairClass a) => S.Store (Pair a) where
    size = S.ConstSize $ 2 * sizeOf (undefined :: a)
    poke p = S.poke (fst p) >> S.poke (snd p)
    peek = pair <$> S.peek <*> S.peek


instance (Serialise a, PairClass a) => Serialise (Pair a) where
    encode p = encodeListLen 2
                <> encode (fst p)
                <> encode (snd p)
    decode = do decodeListLenOf 2
                !x <- decode
                !y <- decode
                return $ pair x y

instance (Binary a, PairClass a) => Binary (Pair a) where
  put = put . toLazy
  get = toStrict <$> get


instance PairClass Int where
  data Pair Int =
    MonoPairInt
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  fst (MonoPairInt a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairInt _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairInt a b
  {-# INLINEABLE pair #-}


instance PairClass Integer where
  data Pair Integer =
    MonoPairInteger
      !Integer
      !Integer
  fst (MonoPairInteger a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairInteger _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairInteger a b
  {-# INLINEABLE pair #-}


instance PairClass Bool where
  data Pair Bool =
    MonoPairBool
      !Bool
      !Bool
  fst (MonoPairBool a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairBool _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairBool a b
  {-# INLINEABLE pair #-}

instance PairClass Char where
  data Pair Char =
    MonoPairChar
      {-# UNPACK #-} !Char
      {-# UNPACK #-} !Char
  fst (MonoPairChar a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairChar _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairChar a b
  {-# INLINEABLE pair #-}

instance PairClass Float where
  data Pair Float =
    MonoPairFloat
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
  fst (MonoPairFloat a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairFloat _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairFloat a b
  {-# INLINEABLE pair #-}

instance PairClass Double where
  data Pair Double =
    MonoPairDouble
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
  fst (MonoPairDouble a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairDouble _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairDouble a b
  {-# INLINEABLE pair #-}

instance PairClass String where
  data Pair String =
    MonoPairString
      !String
      !String
  fst (MonoPairString a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairString _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairString a b
  {-# INLINEABLE pair #-}


instance PairClass T.Text where
  data Pair T.Text =
    MonoPairText
      {-# UNPACK #-} !T.Text
      {-# UNPACK #-} !T.Text
  fst (MonoPairText a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairText _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairText a b
  {-# INLINEABLE pair #-}


instance PairClass Int8 where
  data Pair Int8 =
    MonoPairInt8
      {-# UNPACK #-} !Int8
      {-# UNPACK #-} !Int8
  fst (MonoPairInt8 a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairInt8 _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairInt8 a b
  {-# INLINEABLE pair #-}

instance PairClass Int16 where
  data Pair Int16 =
    MonoPairInt16
      {-# UNPACK #-} !Int16
      {-# UNPACK #-} !Int16
  fst (MonoPairInt16 a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairInt16 _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairInt16 a b
  {-# INLINEABLE pair #-}

instance PairClass Int32 where
  data Pair Int32 =
    MonoPairInt32
      {-# UNPACK #-} !Int32
      {-# UNPACK #-} !Int32
  fst (MonoPairInt32 a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairInt32 _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairInt32 a b
  {-# INLINEABLE pair #-}

instance PairClass Int64 where
  data Pair Int64 =
    MonoPairInt64
      {-# UNPACK #-} !Int64
      {-# UNPACK #-} !Int64
  fst (MonoPairInt64 a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairInt64 _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairInt64 a b
  {-# INLINEABLE pair #-}

instance PairClass Word8 where
  data Pair Word8 =
    MonoPairWord8
      {-# UNPACK #-} !Word8
      {-# UNPACK #-} !Word8
  fst (MonoPairWord8 a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairWord8 _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairWord8 a b
  {-# INLINEABLE pair #-}

instance PairClass Word16 where
  data Pair Word16 =
    MonoPairWord16
      {-# UNPACK #-} !Word16
      {-# UNPACK #-} !Word16
  fst (MonoPairWord16 a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairWord16 _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairWord16 a b
  {-# INLINEABLE pair #-}

instance PairClass Word32 where
  data Pair Word32 =
    MonoPairWord32
      {-# UNPACK #-} !Word32
      {-# UNPACK #-} !Word32
  fst (MonoPairWord32 a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairWord32 _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairWord32 a b
  {-# INLINEABLE pair #-}

instance PairClass Word64 where
  data Pair Word64 =
    MonoPairWord64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  fst (MonoPairWord64 a _) = a
  {-# INLINEABLE fst #-}
  snd (MonoPairWord64 _ b) = b
  {-# INLINEABLE snd #-}
  pair a b = MonoPairWord64 a b
  {-# INLINEABLE pair #-}

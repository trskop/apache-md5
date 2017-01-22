{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

#ifdef __GLASGOW_HASKELL__
#define LANGUAGE_DERIVE_DATA_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
#define LANGUAGE_DERIVE_GENERIC
{-# LANGUAGE DeriveGeneric #-}
#endif

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:      Data.Digest.ApacheMD5.Internal
-- Copyright:   (c) 2009, 2010, 2012-2015, 2017 Peter Trško
-- License:     BSD3
-- Maintainer:  Peter Trško <peter.trsko@gmail.com>
-- Stability:   Provisional
-- Portability: GHC specific language extensions; linking against OpenSSL.
--
-- Internal and unsafe functions used for implementing Apache MD5
-- hash algorithm.
--
-- Try to avoid using this module directly when possible, but there
-- are situations when it might come handy.
module Data.Digest.ApacheMD5.Internal
    (
    -- * Types
      Password
    , Salt(Salt)

    -- * ApacheMD5 Hash
    , apacheMD5

    -- * Base64-like encoding
    , alpha64
    , isAlpha64
    , encode64

    -- * OpenSSL Bindings
    , md5BS
    , md5DigestLength
    )
  where

import Prelude
    ( Integral(div, mod, rem, toInteger)
    , Num((+), fromInteger)
    , Read
    , Show
    , fromIntegral
    )

import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Bits (Bits((.|.), (.&.), shiftL, shiftR))
import Data.Bool (Bool, (||), (&&), otherwise)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($))
import Data.Int (Int)
import Data.List (concatMap, foldl1, iterate, map, replicate, take)
import Data.Ord (Ord((<=), (>), (>=)))
import Data.Word (Word8, Word16, Word32)
import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CULong(..))
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)

#ifdef WITH_deepseq
import Control.DeepSeq (NFData)
#endif

#ifdef LANGUAGE_DERIVE_DATA_TYPEABLE
import Data.Data (Data)
import Data.Typeable (Typeable)
#endif

#ifdef LANGUAGE_DERIVE_GENERIC
import GHC.Generics (Generic)
#endif

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
    ( append
    , concat
    , cons
    , empty
    , head
    , index
    , length
    , null
    , pack
    , take
    )
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString.Internal as BS (create)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)


-- | Type alias for more readable type signatures.
type Password = ByteString

-- | Apache MD5 hash salt. When constructing @.htpasswd@ file it is necessary
-- for the salt to be consisting of octets from 'alpha64' \"set\". This newtype
-- along with 'Data.Digest.ApacheMD5.mkSalt' smart constructor are here to
-- ensure such invariant.
newtype Salt = Salt ByteString
  deriving
    ( Eq, Ord, Read, Show
#ifdef LANGUAGE_DERIVE_DATA_TYPEABLE
    , Data, Typeable
#endif
#ifdef LANGUAGE_DERIVE_GENERIC
    , Generic
#endif
    )

#ifdef WITH_deepseq
instance NFData Salt
#endif

-- | Raw Apache MD5 implementation that is parametrized by MD5 implementation
-- and doesn't encode result in to Base64.
--
-- This module provides 'encode64' for producing Apache `htpasswd` compatible
-- Base64 encoding.
apacheMD5
    :: (ByteString -> ByteString)
    -- ^ MD5 hash function.
    -> Password
    -> Salt
    -> ByteString
    -- ^ Apache MD5 Hash
apacheMD5 md5 !password (Salt !salt) =
    g . f . md5 $ password <> salt <> password
  where
    (<>) = BS.append

    f :: ByteString -> ByteString
    f !digest = md5 $ password <> C8.pack "$apr1$" <> salt
        <> BS.concat (replicate (passwordLength `div` md5DigestLength) digest)
        <> BS.take (passwordLength `rem` md5DigestLength) digest
        <> f' pwHead passwordLength
      where
        !passwordLength = BS.length password
        pwHead = if BS.null password then 0 else BS.head password
            -- Consistent with htpasswd implementation.

        f' :: Word8 -> Int -> ByteString
        f' !pwhead !i
          | i == 0    = BS.empty
          | otherwise = (if i .&. 1 == 1 then 0 else pwhead)
            `BS.cons` f' pwhead (i `shiftR` 1)

    g :: ByteString -> ByteString
    g = g' 0
      where
        -- Iterate this function 1000 times, starting with 0 and ending with
        -- 999.
        g' :: Word16 -> ByteString -> ByteString
        g' !i !digest
          | i >= 1000 = digest
          | otherwise = g' (i + 1) . md5
            $  (if i .&. 1 == 1  then password else digest)
            <> (if i `mod` 3 > 0 then salt     else BS.empty)
            <> (if i `mod` 7 > 0 then password else BS.empty)
            <> (if i .&. 1 == 1  then digest   else password)

-- | Alphabet used by 'encode64'.
alpha64 :: ByteString
alpha64 = C8.pack
    "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

-- | Check if specified 8 bit word is a valid member of 'alpha64'.
isAlpha64 :: Word8 -> Bool
isAlpha64 = ((>= dot) <&&> (<= _9))
    <||> ((>= _A) <&&> (<= _Z))
    <||> ((>= _a) <&&> (<= _z))
  where
    (<&&>) = liftA2 (&&)
    (<||>) = liftA2 (||)

    dot = 46 -- '.'
    _9 = 57  -- '9'
    _A = 65  -- 'A'
    _Z = 90  -- 'Z'
    _a = 97  -- 'a'
    _z = 122 -- 'z'

-- | Encode raw MD5 hash in to Base64-like encoding used by Apache `htpasswd`.
encode64 :: ByteString -> ByteString
encode64 str = BS.pack $ concatMap (encode64' str)
  -- Index --.   ,-- Shift bits left this much
  --         V   V
    [ (4, [( 0, 16), ( 6, 8), (12, 0)])
    , (4, [( 1, 16), ( 7, 8), (13, 0)])
    , (4, [( 2, 16), ( 8, 8), (14, 0)])
    , (4, [( 3, 16), ( 9, 8), (15, 0)])
    , (4, [( 4, 16), (10, 8), ( 5, 0)])
    , (2, [(11,  0)                  ])
  --   ^  `-----------. ,------------'
  --   |               V
  --   |    Do bitwise OR on results
  --   |
  --   `-- How many characters from alpa64
  --       will be used to encode the row.
    ]
  where
    encode64' :: ByteString -> (Int, [(Int, Int)]) -> [Word8]
    encode64' !s (!n, xs) =
        to64 n . foldl1 (.|.) . (`map` xs) $ \ (!i, !t) ->
            conv (s `BS.index` i) `shiftL` t

    conv :: (Integral i, Integral n) => i -> n
    conv = fromInteger . toInteger

    to64 :: Int -> Word32 -> [Word8]
    to64 !n !c = take n . map ((alpha64 `BS.index`) . conv . (.&. 0x3f))
        $ iterate (`shiftR` 6) c

-- Inspired by nano-md5 <http://hackage.haskell.org/package/nano-md5> package
-- by Don Stewart
--
-- From MD5(3SSL) man page:
--
-- unsigned char *MD5(const unsigned char *d, unsigned long n,
--     unsigned char *md);
foreign import ccall "openssl/md5.h MD5"
    c_md5 :: Ptr CChar -> CULong -> Ptr Word8 -> IO (Ptr Word8)

-- | Length of MD5 hash in octets.
md5DigestLength :: Int
md5DigestLength = 16

-- | Thin Haskell wrapper around OpenSSL's MD5 hash function.
md5BS :: ByteString -> ByteString
md5BS bs = unsafePerformIO . BS.unsafeUseAsCStringLen bs $ \ (ptr, len) ->
    BS.create md5DigestLength $ void . c_md5 ptr (fromIntegral len)

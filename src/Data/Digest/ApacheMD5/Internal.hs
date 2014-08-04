{-# LANGUAGE CPP #-}

#ifdef __GLASGOW_HASKELL__
#define LANGUAGE_DERIVE_DATA_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
#define LANGUAGE_DERIVE_GENERIC
{-# LANGUAGE DeriveGeneric #-}
#endif

{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module:      Data.Digest.ApacheMD5.Internal
-- Copyright:   (c) 2009, 2010, 2012 - 2014 Peter Trško
-- License:     BSD3
-- Maintainer:  Peter Trško <peter.trsko@gmail.com>
-- Stability:   Provisional
-- Portability: non-portable (CPP, DeriveDataTypeable, DeriveGeneric,
--              ForeignFunctionInterface)
--
module Data.Digest.ApacheMD5.Internal
    (
    -- * Salt Newtype Wrapper
      Salt(Salt)

    -- * OpenSSL Bindings
    , md5BS
    , md5DigestLength
    )
  where

import Control.Monad (void)
import Data.Word (Word8)
import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CULong(..))
import System.IO.Unsafe (unsafePerformIO)

#ifdef LANGUAGE_DERIVE_DATA_TYPEABLE
import Data.Data (Data)
import Data.Typeable (Typeable)
#endif

#ifdef LANGUAGE_DERIVE_GENERIC
import GHC.Generics (Generic)
#endif

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS (create)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)

#ifdef WITH_deepseq
import Control.DeepSeq (NFData)
#endif


-- | Apache MD5 hash salt. When constructing @.htpasswd@ file it is necessary
-- for the salt to be consisting of octets from 'alpha64' \"set\". This newtype
-- along with 'mkSalt' smart constructor are here to ensure such invariant.
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

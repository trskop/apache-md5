{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Digest.ApacheMD5.Internal
    ( md5BS
    , md5DigestLength
    )
  where

import Control.Monad (void)
import Data.Word (Word8)
import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CULong(..))
import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS (create)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)


-- Inspired by nano-md5 <http://hackage.haskell.org/package/nano-md5> package
-- by Don Stewart
--
-- From MD5(3SSL) man page:
--
-- unsigned char *MD5(const unsigned char *d, unsigned long n,
--     unsigned char *md);
foreign import ccall "openssl/md5.h MD5"
    c_md5 :: Ptr CChar -> CULong -> Ptr Word8 -> IO (Ptr Word8)

md5DigestLength :: Int
md5DigestLength = 16

md5BS :: ByteString -> ByteString
md5BS bs = unsafePerformIO . BS.unsafeUseAsCStringLen bs $ \ (ptr, len) ->
    BS.create md5DigestLength $ void . c_md5 ptr (fromIntegral len)

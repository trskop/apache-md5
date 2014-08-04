{-# LANGUAGE BangPatterns #-}
-- |
-- Module:      Data.Digest.ApacheMD5
-- Copyright:   (c) 2009, 2010, 2012 - 2014 Peter Trško
-- License:     BSD3
-- Maintainer:  Peter Trško <peter.trsko@gmail.com>
-- Stability:   Provisional
-- Portability: non-portable (BangPatterns)
--
-- ApacheMD5 is one of the hash algorithms used by Apache HTTP server for basic
-- authentication. It is Apache specific, but e.g. nginx supports this
-- algorithm since version 1.0.3
-- <http://wiki.nginx.org/HttpAuthBasicModule#auth_basic_user_file>.
--
-- This is a naive implementation that doesn't aim for high speed, but to be
-- reasonably fast it uses @MD5()@ function from OpenSSL library so during
-- compilation you'll nead to have it installed including header files.
-- Many Linux distributions have separate dev packages for this.
module Data.Digest.ApacheMD5
    (
    -- * Htpasswd
    --
    -- | Apache comes with utility named @htpasswd@ that allows to create,
    -- delete and update flat files normally named @.htpasswd@ that store pairs
    -- of usernames and passwords.  While both this utility and Apache support
    -- more algorithms most of them rely on UNIX @crypt()@ function.  ApacheMD5
    -- is not one of them and therefore it is suitable for cross-platform
    -- usage.  See also @htpasswd@ documentation on
    -- <http://httpd.apache.org/docs/current/programs/htpasswd.html>.

    -- ** Example: Creating htpasswd-like entry
    --
    -- | Output of this function is not identical to what @htpasswd@ does.  To
    -- create @htpasswd@-like entry do:
    --
    -- > import Data.ByteString (ByteString)
    -- > import qualified Data.ByteString.Char8 as C8 (concat, pack, singleton)
    -- > import Data.Digest.ApacheMD5 (Salt, apacheMD5, unSalt)
    -- >
    -- > htpasswdEntry :: ByteString -> ByteString -> Salt -> ByteString
    -- > htpasswdEntry username password salt = C8.concat
    -- >     [ username
    -- >     , C8.pack ":$apr1$"
    -- >     , unSalt salt
    -- >     , C8.singleton '$'
    -- >     , apacheMD5 password salt
    -- >     ]

    -- * API Documentation
      apacheMD5
    , apacheMD5'
    , Salt
    , mkSalt
    , unSalt
    , alpha64
    , isAlpha64
    , encode64
    , md5DigestLength
    )
  where

import Control.Applicative (liftA2)
import Data.Bits (Bits((.|.), (.&.), shiftL, shiftR))
import Data.Word (Word8, Word16, Word32)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
    ( all
    , append
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

import Data.Digest.ApacheMD5.Internal (Salt(Salt), md5BS, md5DigestLength)


type Password = ByteString

-- | Smart constructor for 'Salt'. It tests all octets to be members of
-- 'alpha64' by using 'isAlpha64' predicate.
mkSalt :: ByteString -> Maybe Salt
mkSalt str
  | BS.all isAlpha64 str = Just $ Salt str
  | otherwise            = Nothing

-- | Unpack 'Salt' in to 'ByteString'.
unSalt :: Salt -> ByteString
unSalt (Salt str) = str

-- | Taking password and salt this function produces resulting ApacheMD5 hash
-- which is already base 64 encoded.
apacheMD5
    :: Password
    -> Salt
    -> ByteString
    -- ^ Apache MD5 Hash
apacheMD5 = (encode64 .) . apacheMD5' md5BS

-- | Raw Apache MD5 implementation that is parametrized by MD5 implementation
-- and doesn't encode result in to base 64.
apacheMD5'
    :: (ByteString -> ByteString)
    -- ^ MD5 hash function.
    -> Password
    -> Salt
    -> ByteString
    -- ^ Apache MD5 Hash
apacheMD5' md5 !password (Salt !salt) =
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
          | i < 1000 = g' (i + 1) . md5
            $  (if i .&. 1 == 1  then password else digest)
            <> (if i `mod` 3 > 0 then salt     else BS.empty)
            <> (if i `mod` 7 > 0 then password else BS.empty)
            <> (if i .&. 1 == 1  then digest   else password)
          | otherwise = digest

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

    conv :: (Integral i, Num n, Integral n, Bits n) => i -> n
    conv = fromInteger . toInteger

    to64 :: Int -> Word32 -> [Word8]
    to64 !n !c = take n . map ((alpha64 `BS.index`) . conv . (.&. 0x3f))
        $ iterate (`shiftR` 6) c

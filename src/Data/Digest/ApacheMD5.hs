{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Digest.ApacheMD5
-- Copyright:   (c) 2009, 2010, 2012-2015 Peter Trško
-- License:     BSD3
-- Maintainer:  Peter Trško <peter.trsko@gmail.com>
-- Stability:   Provisional
-- Portability: NoImplicitPrelude; depends on non-portable internal module
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

    -- * Example: Creating htpasswd-like entry
    --
    -- | Output of 'apacheMD5' function is not identical to what @htpasswd@
    -- does.  To create @htpasswd@-like entry one needs to do:
    --
    -- @
    -- import Data.ByteString (ByteString)
    -- import qualified Data.ByteString.Char8 as C8 (concat, pack, singleton)
    -- import Data.Digest.ApacheMD5 ('Salt', 'apacheMD5', 'unSalt')
    --
    -- htpasswdEntry :: ByteString -> ByteString -> 'Salt' -> ByteString
    -- htpasswdEntry username password salt = C8.concat
    --     [ username
    --     , C8.pack \":$apr1$\"
    --     , 'unSalt' salt
    --     , C8.singleton \'$\'
    --     , 'apacheMD5' password salt
    --     ]
    -- @

    -- * API Documentation
      apacheMD5
    , Password

    -- ** Salt
    --
    -- | 'Salt' is a byte sequence that contains one or more characters from
    -- 'Internal.alpha64', i.e. the same characters used to encode hash in to
    -- printable form. Reason for using 'Internal.alpha64' is compatibility
    -- with Apache implementation. Some implementations (including Apache
    -- implementation) are capable to verify passwords hashed with salt that
    -- contains characters outside of this set. There are some hard
    -- restrictions, e.g. that \'$\' character can not be used, since it is
    -- reserved as a field separator. Overall it is safer to stick with
    -- 'Integral.alpha64'.
    , Salt
    , mkSalt
    , unSalt
    )
  where

import Data.Bool ((&&), not, otherwise)
import Data.Function ((.), ($))
import Data.Maybe (Maybe(Nothing, Just))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (all, null)

import Data.Digest.ApacheMD5.Internal (Password, Salt(Salt))
import qualified Data.Digest.ApacheMD5.Internal as Internal
    ( apacheMD5
    , encode64
    , isAlpha64
    , md5BS
    )

-- | Smart constructor for 'Salt'. It tests that provided 'ByteString' is not
-- empty and that all its octets are members of alphabet used for base 64
-- encoding of final hash ('Internal.alpha64') and it uses 'Internal.isAlpha64'
-- predicate to do so.
mkSalt :: ByteString -> Maybe Salt
mkSalt str
  | isValidSalt = Just $ Salt str
  | otherwise   = Nothing
  where
    isValidSalt = not (BS.null str) && BS.all Internal.isAlpha64 str

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
apacheMD5 = (Internal.encode64 .) . Internal.apacheMD5 Internal.md5BS

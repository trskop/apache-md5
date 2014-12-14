Apache MD5
==========


Description
-----------

Haskell implementation of Apache specific MD5 digest algorithm that uses
OpenSSL MD5.


Documentation
-------------

Stable releases with API documentation are available on [Hackage][].


Installation
------------

Requires OpenSSL library with header files. On Debian and Ubuntu Linux it's
provided by [`libssl-dev`][libssl-dev] package that can be installed using
`apt-get`:

    $ apt-get install libssl-dev

For more see `apt-get(8)` manual page or e.g. [Ubuntu Documentation: AptGet
Howto][apt-get-howto].

After that just use [`cabal-install`][cabal-install] as you would normally do.
For details see [HaskellWiki: How to install a Cabal package][].


Example
-------

Create htpasswd like entry and print it to stdout:

```Haskell
module Main (main)
    where

import Control.Applicative ((<$>))
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Control.Monad.Random (evalRandIO, getRandomRs)
    -- MonadRandom package http://hackage.haskell.org/package/MonadRandom/
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (index, pack)
import qualified Data.ByteString.Char8 as C8 (concat, pack, putStrLn, singleton)
    -- bytestring package http://hackage.haskell.org/package/bytestring
import Data.Digest.ApacheMD5 (Salt, apacheMD5, mkSalt, unSalt)
import Data.Digest.ApacheMD5.Internal (alpha64)


htpasswdEntry :: String -> String -> Salt -> ByteString
htpasswdEntry username password salt = C8.concat
    [ C8.pack username
    , C8.pack ":$apr1$"
    , unSalt salt
    , C8.singleton '$'
    , apacheMD5 (C8.pack password) salt
    ]

genSalt :: IO Salt
genSalt = do
    Just s <- evalRandIO $ mkSalt . BS.pack . map (BS.index alpha64) . take 8
        <$> getRandomRs (0, 63)
        -- We know that Salt is correctly generated, since we use alpha64 to do
        -- it. That is the reason why we can pattern match on Just.
        --
        -- Other option would be to use Salt value constructor from
        -- Data.Digest.ApacheMD5.Internal module.
    return s

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [userName, password] ->
            genSalt >>= C8.putStrLn . htpasswdEntry userName password
        _ -> do
            hPutStrLn stderr $ "Usage: " ++ progName ++ " USER_NAME PASSWORD"
            exitFailure
```

Compiling and running above example:

    $ ghc -Wall example.hs
    [1 of 1] Compiling Main             ( example.hs, example.o )
    Linking example ...
    $ ./example
    Usage: example USER_NAME PASSWORD
    $ ./example foo 123456
    foo:$apr1$yM9AMlr2$EHssuHrqSAe8HPrAdN7HC/


Unit Tests
----------

Requires `htpasswd` command line utility installed. On Debian and Ubuntu Linux
it is provided by [`apache2-utils`][apache2-utils] package that can be
installed using `apt-get`:

    $ apt-get install apache2-utils

For more see `apt-get(8)` manual page or e.g. [Ubuntu Documentation: AptGet
Howto][apt-get-howto].

To run tests use command similar to this:

    $ cabal configure --enable-tests && cabal build && cabal test


Benchmarks
----------

This package provides [Criterion][] benchmarks, to run them you can use
something like:

    $ cabal configure --enable-benchmarks && cabal build && cabal bench

To generate HTML output one needs to specify output file. Then the last
command in above chain would look like:

    $ cabal bench --benchmark-option=--output=benchmarks.html

Where `benchmarks.html` is the name of [Criterion][] generated HTML file.


Contributions
-------------

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail (see `.cabal` file for
that).


[apache2-utils]:
    http://packages.debian.org/stable/apache2-utils
[apt-get-howto]:
    https://help.ubuntu.com/community/AptGet/Howto
[cabal-install]:
    http://haskell.org/haskellwiki/Cabal-Install
[Criterion]:
    http://hackage.haskell.org/package/criterion
[Hackage]:
    http://hackage.haskell.org/package/apache-md5
[HaskellWiki: How to install a Cabal package]:
    http://haskell.org/haskellwiki/Cabal/How_to_install_a_Cabal_package
[libssl-dev]:
    http://packages.debian.org/lenny/libssl-dev

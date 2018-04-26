# Apache MD5

[![Hackage](http://img.shields.io/hackage/v/apache-md5.svg)][Hackage: apache-md5]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/apache-md5.svg)](http://packdeps.haskellers.com/reverse/apache-md5)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/apache-md5.svg)](https://travis-ci.org/trskop/apache-md5)


## Description

Haskell implementation of Apache specific MD5 digest algorithm that uses
OpenSSL MD5.


## Documentation

Stable releases with API documentation are available on
[Hackage][Hackage: apache-md5].


## Installation

Requires OpenSSL library with header files. On Debian and Ubuntu Linux it's
provided by [`libssl-dev`][libssl-dev] package that can be installed using
`apt-get`:

    $ apt-get install libssl-dev

For more see `apt-get(8)` manual page or e.g. [Ubuntu Documentation: AptGet
Howto][apt-get-howto].

After that just use [`cabal-install`][cabal-install] as you would normally do.
For details see [HaskellWiki: How to install a Cabal package][].


### Building Options

* `pedantic` (disabled by default)

    Pass additional warning flags to GHC.

* `deepseq` (disabled by default)

    Define instance of `NFData` for `Salt` newtype. This dependency is enforced
    for benchmark.

* `examples` (disabled by default)

    Build example(s).


## Example

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


## Unit Tests

Requires `htpasswd` command line utility installed. It is used as a reference
implementation of Apache MD5 algorithm by unit tests. On Debian and Ubuntu
Linux it is provided by [`apache2-utils`][apache2-utils] package that can be
installed using `apt-get`:

    $ apt-get install apache2-utils

For more see `apt-get(8)` manual page or e.g. [Ubuntu Documentation: AptGet
Howto][apt-get-howto].

There is also a possibility to compile Apache `htpasswd` your self, see
[test/apache-htpasswd/README.md][] for more information on how to do that.

To run tests use command similar to this:

    $ cabal configure --enable-tests && cabal build && cabal test


## Benchmarks

This package provides [Criterion][] benchmarks, to run them you can use
something like:

    $ cabal configure --enable-benchmarks && cabal build && cabal bench

To generate HTML output one needs to specify output file. Then the last
command in above chain would look like:

    $ cabal bench --benchmark-option=--output=benchmarks.html

Where `benchmarks.html` is the name of [Criterion][] generated HTML file.


License
-------

The BSD 3-Clause License, see [LICENSE][] file for details.


## Contributions

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact the author.



[apache2-utils]:
    http://packages.debian.org/stable/apache2-utils
    "apache2-utils Debian package"
[apt-get-howto]:
    https://help.ubuntu.com/community/AptGet/Howto
    "apt-get Howto"
[cabal-install]:
    http://haskell.org/haskellwiki/Cabal-Install
    "HaskellWiki: Cabal-install"
[Criterion]:
    http://hackage.haskell.org/package/criterion
    "criterion package on Hackage"
[Hackage: apache-md5]:
    http://hackage.haskell.org/package/apache-md5
    "apache-md5 package on Hackage"
[Haskell.org]:
    http://www.haskell.org
    "The Haskell Programming Language"
[HaskellWiki: How to install a Cabal package]:
    http://haskell.org/haskellwiki/Cabal/How_to_install_a_Cabal_package
    "HaskellWiki: How to install a Cabal package"
[libssl-dev]:
    http://packages.debian.org/lenny/libssl-dev
    "libssl-dev Debian package"
[LICENSE]:
  https://github.com/trskop/apache-md5/blob/master/LICENSE
  "License of apache-md5 package."
[test/apache-htpasswd/README.md]:
  https://github.com/trskop/apache-md5/blob/master/test/apache-htpasswd/README.md
  "Compile your own Apache htpasswd binary to use as a reference implementation."
[tl;dr Legal: BSD3]:
    https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
    "BSD 3-Clause License (Revised)"

Apache MD5
==========


Description
-----------

Haskell implementation of Apache specific MD5 digest algorithm that uses
OpenSSL MD5.


Installation
------------

Requires OpenSSL library with header files. On Debian and Ubuntu Linux it's
provided by [`libssl-dev`][libssl-dev] package.

After that just use [`cabal-install`][cabal-install] as you would normally do.
For details see [HaskellWiki: How to install a Cabal package][].


Unit Tests
----------

Requires `htpasswd` command line utility installed. On Debian and Ubuntu Linux
it is provided by [`apache2-utils`][apache2-utils] package.

To run tests use command similar to this:

    cabal configure --enable-tests && cabal build && cabal test


Contributions
-------------

Contributions, pull requests and bug reports are welcome!


[libssl-dev]:
    http://packages.debian.org/lenny/libssl-dev
[cabal-install]:
    http://haskell.org/haskellwiki/Cabal-Install
[HaskellWiki: How to install a Cabal package]:
    http://haskell.org/haskellwiki/Cabal/How_to_install_a_Cabal_package
[apache2-utils]:
    http://packages.debian.org/stable/apache2-utils

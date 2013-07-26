Apache MD5
==========


Description
-----------

Haskell implementation of Apache specific MD5 digest algorithm that uses
OpenSSL MD5.


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


Contributions
-------------

Contributions, pull requests and bug reports are welcome!


[apt-get-howto]:
    https://help.ubuntu.com/community/AptGet/Howto
[libssl-dev]:
    http://packages.debian.org/lenny/libssl-dev
[cabal-install]:
    http://haskell.org/haskellwiki/Cabal-Install
[HaskellWiki: How to install a Cabal package]:
    http://haskell.org/haskellwiki/Cabal/How_to_install_a_Cabal_package
[apache2-utils]:
    http://packages.debian.org/stable/apache2-utils

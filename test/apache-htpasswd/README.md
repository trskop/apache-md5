htpasswd
========

This utility is included solely for the purpose of testing apache-md5
implementation, since it contains reference implementation of Apache MD5
algorithm.


Compilation
-----------

On Ubuntu/Debian install following packages:

* `libapr1-dev`
* `libaprutil1-dev`

Using this command:

```
apt-get install libapr1-dev libaprutil1-dev
```

Now compile htpasswd utility using following GCC call:

```
gcc $(pkg-config --cflags apr-1 apr-util-1) passwd_common.c htpasswd.c -o htpasswd -lcrypt $(pkg-config --libs apr-1 apr-util-1)
```

Same command can be found in `build.sh` script.


Origin of the Source Code
-------------------------

Following files:

* `htpasswd.c`
* `passwd_common.c`
* `passwd_common.h`

Were taken from Apache HTTP Server 2.4.18 source distribution, see:

<https://httpd.apache.org/download.cgi#apache24>

This source code files are unchanged. See their contents or included `LICENSE`
file for license information.

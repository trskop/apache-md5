#!/bin/bash

(
    cd "$(dirname $0)"

    gcc $(pkg-config --cflags apr-1 apr-util-1) \
        passwd_common.c htpasswd.c -o htpasswd \
        -lcrypt $(pkg-config --libs apr-1 apr-util-1)
)

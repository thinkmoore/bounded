#!/bin/bash

set -e
set -o xtrace

RACKET="/Users/sdmoore/Documents/racket"
EXTENSION="dylib" # Change to .so for linux/unix

INSTALL=$(racket -e "(display (path->string (build-path \"compiled\" \"native\" (system-library-subpath))))")
NAME="bounded-impersonator-util_rkt.${EXTENSION}"

mkdir -p "private/${INSTALL}"
raco ctool --cc ++ccf "-I${RACKET}/racket/src/racket/src" ++ccf -g src/bounded-impersonator-util.c
raco ctool --ld "${NAME}" bounded-impersonator-util.o
rm bounded-impersonator-util.o
mv "${NAME}" "private/${INSTALL}"
chmod +x "private/${INSTALL}/${NAME}"

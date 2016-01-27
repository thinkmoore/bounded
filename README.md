bounded
=======

Experimental Racket package implementing bounded polymorphic contracts.

The implementation requires removing impersonators from existing values. This is implemented as a primitive function `remove-impersonator` in a C Racket extension. The extension uses private headers from Racket so requires additional includes beyond those provided by `raco ctool`.

`build.sh` will build the extension so that it can be loaded by the package. To use it, change the `RACKET` and `EXTENSION` variables in the script to the path to your local racket repository and the dynamic library extension for your OS, respectively.

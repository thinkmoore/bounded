bounded
=======

*Experimental* Racket package implementing bounded polymorphic contracts.

The implementation requires removing impersonators from existing values. This is implemented as a primitive function `remove-impersonator` in a C Racket extension. The extension uses private headers from Racket so requires additional includes beyond those provided by `raco ctool`. The package may fail to install if your Racket installation does not include the Racket source tree.

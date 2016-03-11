# README #

Several example files for implementing coroutines in Clozure Common Lisp (CCL). Because low-level implementation-specific details are used, this code only works in CCL.

Each file is independent of the others. For most advanced functionality, just load the last file described herein.

```ccl-coroutines-basic.lisp```
Provides basic coroutine functionality. Examples are at the end.

```ccl-coroutines-sg.lisp```
Improves ccl-coroutines-basic by being slightly less verbose.

```ccl-coroutines-mv.lisp```
Improves ccl-coroutines-sg by enabling multiple return values.
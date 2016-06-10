# README #

Several example files for implementing coroutines in Clozure Common Lisp (CCL). Because low-level implementation-specific details are used, this code only works in CCL.

Background: In Macintosh Common Lisp (MCL), coroutines were easy to implement with stack-groups. Since MCL did its own cooperative multitasking, it managed its own stacks and stack-groups were first-class objects. In CCL, however, native threads are used and there are no Lisp-accessible stack-groups. The code herein implements coroutine behavior on top of native threads. It's a (mostly) complete coroutine API and doesn't require changing existing code to use coroutines. However, these pseudo-coroutines are probably not as fast (in terms of latency) as true stack-groups coroutines would be.

There's more information in the code itself.

Each file herein is independent of the others. For the most advanced functionality, just load the last file described herein.

```ccl-coroutines-basic.lisp```
Provides basic coroutine functionality. Examples are at the end.

```ccl-coroutines-sg.lisp```
Improves ccl-coroutines-basic by being slightly less verbose.

```ccl-coroutines-mv.lisp```
Improves ccl-coroutines-sg by enabling multiple return values.
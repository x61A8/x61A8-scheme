# x61A8-scheme
This is a scheme interpreter built on top of the common lisp reader.
It is an approximate implementation of R3RS.
Tail call optimization (TCO) and call-with-current-continuation are both fully implemented.
All number, string, and vector functions specified are also implemented.

## Installation and Usage
First clone the repository or otherwise download x61A8-scheme.lisp.
Then in the REPL of your Common Lisp implementation of choice type:
```Common Lisp
(load "./PATH/TO/x61A8-scheme.lisp")
(in-package :x6s)
(scheme)
```
You will be presented with the prompt '=x>' and can begin entering scheme expressions.

If you purposefully or accidentally break out of the scheme interpreter you can re-enter without resetting the environment by entering:

    (start-scheme-rspl)
    
While you are in the Common Lisp repl you can examine the global scheme environment by entering:

    *global-env*

## Divergences from R3RS
x61A8-scheme uses the Common Lisp reader instead of implementing a custom reader.
This allows full use of CL's reader macros, but also imposes some restrictions on the implemented scheme.
They are as follows:
* #t is replaced by t
* #f is replaced by nil
* The empty list is synonymous with nil

Functions also follow Common Lisp semantics, such as (cdr '()) being nil instead of an error.

## Areas for Further Development
### Implement a custom reader
A custom reader would allow eliminating the differences described in the above section.
It would also allow implementation of quasiquote.
With quasiquote creating a useful user macro system would be possible.
This would allow moving the implementation to R5RS.

### Improve on how built-in macros are currently implemented
To allow the user to redefine symbols such as `lambda` without breaking macros like `let` macros are implemented using macro-intrinsics directly.
However, this leads to macro definition code being difficult to understand.
The strategy to fix this would depend on how the user macro system is implemented.

### Implement the remaining functions in the spec
Control functions like map and apply, as well as I/O like reading files are not currently supported.
There are also various features that are not marked essential such as delay/force that are not implemented.

## Related Reading
* Chapter 22 of Paradigms of AI Programming (PAIP) presents a scheme interpreter which was the base of this project.
* Chapter 4 of Structure and Interpretation of Computer Programs (SICP) presents how to write a scheme interpreter in scheme.
* [R3RS](http://people.csail.mit.edu/jaffer/r3rs_toc.html) is the specification followed for this project.

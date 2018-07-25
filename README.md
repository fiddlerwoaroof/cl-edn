Installation
============

- Clone this repository somewhere ASDF can find the system: e.g. ```git clone https://github.com/fiddlerwoaroof/cl-edn.git ~/quicklisp/local-projects/cl-edn/```
- At a REPL, ```(ql:quickload :cl-edn/fset)```

Usage
=====

Conceptual Model:
-----------------

```
                         +-------------+
                         | SYNTHESIZER |
                         +------+------+
                                |
                                V
+----------+      +----------------------------+
| READ-EDN +----->| (PARSE string synthesizer) |
+----------+      +----------------------------+
```

Systems for Components:
-----------------------

- CL-EDN:
    - READ-EDN
    - PARSE
- CL-EDN/FSET:
    - 'EDN:FSET: a synthesizer that uses FSET datastructures but
      preserves the case of keywords and symbols.
- CL-EDN/FSET-LOSSY:
    - 'EDN:FSET-LOSSY: a synthesizer that uses FSET datastructures but
      uppercases keywords and symbols.  This is probably preferable for
      most cases where the data is only going to be used by Common Lisp,
      because CL symbols are uppercase by default

Notes:
------

This library divides the task of parsing EDN into two stages.  In the
first stage, implemented by `(EDN:READ-EDN string)` an EDN file is is
parsed into an AST where primitives are converted into lisp values and
compound forms are converted into lists of the form 
`(type-specifier . data)`.  This AST can be passed to 
`(EDN:SYNTHESIZE implementation ast)` to produce datastructures of a
specific kind.  The system `cl-edn/fset` provides a synthesizer that
produces appropriate fset datastructures. The system
`cl-edn/fset-lossy` produces fset datastructures, but forces symbols
and keywords to have uppercase names, for easier interoperation with
CL's default readtable.  As a convenience, there is also 
`(EDN:PARSE string &optional (implementation 'fset))` that combines
the two steps into a single call.  These implementations can passed to
`PARSE` and `SYNTHESIZE` either as the symbols `EDN:FSET` and
`EDN:FSET-LOSSY` or by instantiating the classes named by those symbols.

EXTENSION
=========

`EDN:SYNTHESIZE` is a generic function that takes an implementation as
the first argument. The main system, `CL-EDN`, provides two
implementations of this function: one that specializes the first
argument on `SYMBOL`, that just makes an instance of the class named
by `IMPLEMENTATION` and calls `EDN:SYNTHESIZE` with the instance as
its first argument; the other implementation inspects the second
argument and, if it is a list, it delegates to
`(EDN:SYNTHESIZE-COMPOUND IMPLEMENTATION DISCRIMINATOR ARGS)`, the
head of the list as `DISCRIMINATOR` and the tail as `ARGS`. The
default implementation of this generic also provides methods for
strings, symbols and keywords that produce the relevant lisp types, as
well as an implementation for tagged literals that implements `#inst`
and `#uuid` and produces a form `(:TAGGED TAG-SYMBOL DATA)` where
`DATA` is synthesized according to the rules governing
`IMPLEMENTATION` (e.g. the `FSET` implementation makes `DATA` to be an
instance of the datastructures provided by the `FSET` library). Tags
can also be added to the default implementation by providing an
EQL-specialized method of `(EDN:SYNTHESIZE-TAG IMPLEMENTATION TAG ARG)`
for the symbol you want to define a behavior for.  When this method is
called, the tag-symbol will be uppercased and converted into a common
lisp keyword and the `ARG` will have been synthesized according to the
rules provided by `IMPLEMENTATION`.  To override this behavior, an
implementation can override `EDN:SYNTHESIZE-COMPOUND`, but such
implementations should either call `CALL-NEXT-METHOD` or implement
`#inst` and `#uuid` processing themselves.

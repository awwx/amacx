## v.NEXT

### Amacx command line

* Load bug fixed: `amacx foo.arc` or `(load "foo.arc")` is now
  relative to the current directory instead of the Amacx installation
  location.

* Option to boot with a topvar implementation: "ref" (generic
  containers) or "ail" (top level variables implemented in Ail, such
  as in Racket a plain symbol references a variable in the namespace).

* Default topvar implementation to "ail" (faster).

* Amacx `--validate-ail` option to validate the output of the
  compiler.

* Amacx `--time` option to show the time it takes to do the various
  steps specified on the command line.


### Compiler

* ssyntax merged into the compiler instead of being a compiler
  extension; it turned out to be too hard to cross compile when a
  compiler instance had to simultaneously support both the source and
  target runtimes.

* Can now pass options to the compiler: currently `validate-ail` is
  available.


### Bug fixes

* Fix bug in blockstr for empty input.


### arc.arc

* Roughly 55% of arc.arc is implemented.


## v0.0.1, 1019-01-04

Initial release.

# Modified version of Arc

This directory contains a modified version of
[Arc](http://arclanguage.org/) version 3.2.  Arc was released under
the Artistic License:

> This software is copyright (c) Paul Graham and Robert Morris.
> Permission to use it is granted under the Perl Foundations's
> Artistic License 2.0.

The Artistic License 2.0 in turn allows distribution of modified
versions:

> (4) You may Distribute your Modified Version as Source (either
> gratis or for a Distributor Fee, and with or without a Compiled form
> of the Modified Version) provided that you clearly document how it
> differs from the Standard Version, including, but not limited to,
> documenting any non-standard features, executables, or modules, and
> provided that you do at least ONE of the following:

We’re using the first option:

> (a) make the Modified Version available to the Copyright Holder of
> the Standard Version, under the Original License, so that the
> Copyright Holder may include your modifications in the Standard
> Version.

Modifications and additions copyright 2018, 2019 Andrew Wilcox, dual
licensed under the Original License (the Artistic License 2.0) for
incorporating code with Arc, and the MIT license for any modifications
or additions that might be able to be used separately.


## Differences from the Standard Version

* Only parts of Arc are included.

* “arc.arc” is split into smaller source code files so that they can
  be loaded individually.

* I swapped the arguments to `sref`: it's now `(sref g key val)`.

* To enable being copied into different containers, macros often
  insert their dependencies (for example, `` `(,let ...)`` is used
  instead of `` `(let ...) ``).

* `join` allows a non-list as a final argument: `(join '(a b) 'c)`

* Base forms such as `fn` and `quote` expand into their Ail
  representations.

* ssyntax, optional arguments, and argument destructuring are
  implemented as macros.

* To simplify runtime implementation, many of Arc’s builtins that take
  optional arguments have an implementation in Arc which handles the
  optional arguments and then calls a simpler builtin provided by the
  runtime.

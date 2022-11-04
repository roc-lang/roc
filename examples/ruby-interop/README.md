# Ruby Interop

This is an example of calling Roc code from [Ruby](https://www.ruby-lang.org).

## Installation

To run this example, you will need these to be installed already (in addition to Roc):

- [`ruby`](https://www.ruby-lang.org/en/downloads) version 2.7.6 or later
- [`clang`](https://clang.llvm.org/) version 11.0.0 or later
- [`make`](https://www.gnu.org/software/make/) version 4.0 or later

Make sure you have the right versions of Ruby and Clang especially! This example won't work with earlier versions.

## Building the Roc library

First, `cd` into this directory and run this in your terminal:

```sh
roc build --lib
```

This compiles your Roc code into a binary library in the current directory. The library's filename will be `libhello` plus an OS-specific extension (e.g. `libhello.dylib` on macOS).

## Generating the Makefile

Next, run this: (remember that you need Ruby 2.7.6 or higher - otherwise later steps will fail!)

```sh
ruby extconf.rb
```

This generates a `Makefile`. (There are only two Roc-specific lines in `extconf.rb`; they are both commented.) You only need to do this step once; now that you have the `Makefile`, you can use it along with `roc build` to rebuild from now.

## Building the Ruby Library from the Roc Library

Finally, run this:

```sh
make
```

This uses the `Makefile` generated earlier to take the compiled Roc library and combine it with `demo.c` to generate a Ruby library.

## Try it out!

You can now try this out in Ruby's REPL (`irb`), like so:

```sh
$ irb
irb(main):001:0> require_relative 'demo'
Ruby just required Roc. Let's get READY TO ROC.
=> true
irb(main):002:0> RocStuff::hello 'Hello, World'
=> "Hello, World, OH YEAH!!! 🤘🤘"
```

## Rebuilding after Changes

To rebuild after changing either the `demo.c` file or any `.roc` files, run:

```sh
roc build --lib && make -B
```

The `-B` flag is necessary when you only change .roc files, because otherwise `make` thinks there's no work to do and doesn't bother rebuilding.

## About this example

This was created by following a [tutorial on Ruby C extensions](https://silverhammermba.github.io/emberb/c/) and [some documentation](https://github.com/ruby/ruby/blob/master/doc/extension.rdoc#label-Prepare+extconf.rb) (along with [more nicely formatted, but potentially out-of-date docs](https://docs.ruby-lang.org/en/2.4.0/extension_rdoc.html)).

The [mkmf](https://ruby-doc.org/stdlib-2.5.1/libdoc/mkmf/rdoc/MakeMakefile.html) ("make makefile") method in `extconf.rb` generates a Makefile, which can then be run (using `make` with no arguments) to generate a compiled library that Ruby knows how to import via `require` or `require_relative`.

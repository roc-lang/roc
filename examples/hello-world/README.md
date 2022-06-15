# Hello, World!

To run, `cd` into this directory and run this in your terminal:

```bash
roc run
```

This will run `main.roc` because, unless you explicitly give it a filename, `roc run`
defaults to running a file named `main.roc`. Other `roc` commands (like `roc build`, `roc test`, and so on) also default to `main.roc` unless you explicitly give them a filename.

# About this example

This uses a very simple platform which does nothing more than printing the string you give it.

The line `main = "Hello, World!\n"` sets this string to be `"Hello, World!"` with a newline at the end, and the lines `packages { pf: "c-platform" }` and `provides [main] to pf` specify that the `c-platform/` directory contains this app's platform.

This platform is called `c-platform` because its low-level code is written in C. There's also a `rust-platform`, `zig-platform`, and so on; if you like, you can try switching `pf: "c-platform"` to `pf: "zig-platform"` or `pf: "rust-platform"` to try one of those platforms instead. They all do the same thing, so the application won't look any different, but if you want to start building your own platforms, this Hello World example gives you some very simple platforms to use as starting points too.

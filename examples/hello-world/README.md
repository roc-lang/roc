# Hello, World!

To run, `cd` into this directory and run this in your terminal:

```bash
roc run
```

This will run `main.roc` because, unless you explicitly give it a filename, `roc run`
defaults to running a file named `main.roc`. Other `roc` commands (like `roc build`, `roc test`, and so on) also default to `main.roc` unless you explicitly give them a filename.

## About this example

This uses a very simple platform which does nothing more than printing the string you give it.

The line `main = "Hello, World!\n"` sets this string to be `"Hello, World!"` with a newline at the end, and the lines `packages { pf: "platform/main.roc" }` and `provides [main] to pf` specify that the `platform/` directory contains this app's platform.

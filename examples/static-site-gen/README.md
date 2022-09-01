# Static site generator

This is an example of how you might build a static site generator using Roc.
It searches for files in the `input` directory, transforms the contents to HTML
using a Roc function, and writes the result into the corresponding file path in
the `output` directory.

To run, `cd` into this directory and run this in your terminal:

If `roc` is on your PATH:
```bash
roc run static-site.roc input/ output/
```

If not, and you're building Roc from source:
```
cargo run -- static-site.roc -- input/ output/
```

Eventually this example could be expanded to support Markdown parsing, database connections at build time, etc.

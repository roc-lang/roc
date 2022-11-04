# Static site generator

This is an example of how you might build a static site generator using Roc.
It searches for Markdown (`.md`) files in the `input` directory, inserts them
into a HTML template defined in Roc, and writes the result into the
corresponding file path in the `output` directory.

To run, `cd` into this directory and run this in your terminal:

If `roc` is on your PATH:
```bash
roc run static-site.roc input/ output/
```

If not, and you're building Roc from source:
```
cargo run -- static-site.roc -- input/ output/
```

The example in the `input` directory is a copy of the 2004 website
by John Gruber, introducing the Markdown format.
https://daringfireball.net/projects/markdown/

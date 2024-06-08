# Platform switching

To run, `cd` into one of the examples in this directory and run this in your terminal:

```bash
roc build.roc
```

This will build the platform into a standalone library. 

Then you can run the roc app using e.g. 

```bash
roc rocLovesZig.roc
```

## About this example

This uses a very simple platform which does nothing more than printing the string you give it. 

The line `main = "Which platform am I running on now?\n"` sets this string to be `"Which platform am I running on now?"` with a newline at the end, and the lines `packages { pf: "platform/main.roc" }` and `provides [main] to pf` specify that the `platform/` directory contains this app's platform, and importantly the host's prebuilt-binaries.

This host is called `host` because its lower-level code is written in a systems programming language like C, Zig, Rust, Swift, Go. You can look through all the examples and you will see the `platform/main.roc` is identical. This shows how you can the exact same roc application and platform even if you swap out the implementation of the host. 

If you want to start building your own platforms, these are some very simple example platforms to use as starting points. If you are looking for more advanced examples, consider asking in the roc zulip or checkout the [official examples repository](https://www.roc-lang.org/examples).

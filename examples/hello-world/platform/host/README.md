# Rebuilding the host from source

Here are the current steps to rebuild this host. 

> This is a C host, which is the simplest to build.
> For an example of a Rust host, see the quicksort example.

First, `cd` into `host/src/` and rebuild `host.o` like so:

```
$ clang -c host.c -o host.o
```

Next, move `host.o` into the appropriate `platform/` subdirectory
based on your architecture and operating system. For example,
on macOS, you'd move `host.o` into the `platform/host/x86_64-unknown-darwin10/` directory.

Congratulations! You now have an updated host.

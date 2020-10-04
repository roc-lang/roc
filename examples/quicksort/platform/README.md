# Rebuilding the host from source

Here are the current steps to rebuild this host. These
steps can likely be moved into a `build.rs` script after
turning `host.rs` into a `cargo` project, but that hasn't
been attempted yet.

## Compile the Rust and C sources

Currently this host has both a `host.rs` and a `host.c`.
This is only because we haven't figured out a way to convince
Rust to emit a `.o` file that doesn't define a `main` entrypoint,
but which is capable of being linked into one later.

As a workaround, we have `host.rs` expose a function called
`rust_main` instead of `main`, and all `host.c` does is provide
an actual `main` which imports and then calls `rust_main` from
the compiled `host.rs`. It's not the most elegant workaround,
but [asking on `users.rust-lang.org`](https://users.rust-lang.org/t/error-when-compiling-linking-with-o-files/49635/4)
didn't turn up any nicer approaches. Maybe they're out there though!

To make this workaround happen, we need to compile both `host.rs`
and `host.c`. First, `cd` into `platform/host/src/` and then run:

```
$ clang -c host.c -o c_host.o
$ rustc host.rs -o rust_host.o
```

Now we should have `c_host.o` and `rust_host.o` in the curent directory.

## Link together the `.o` files

Next, combine `c_host.o` and `rust_host.o` into `host.o` using `ld -r` like so:

```
$ ld -r c_host.o rust_host.o -o host.o
```

Move `host.o` into the appropriate `platform/` subdirectory
based on your architecture and operating system. For example,
on macOS, you'd move `host.o` into the `platform/host/x86_64-unknown-darwin10/` directory.

## All done!

Congratulations! You now have an updated host.

It's now fine to delete `c_host.o` and `rust_host.o`,
since they were only needed to produce `host.o`.

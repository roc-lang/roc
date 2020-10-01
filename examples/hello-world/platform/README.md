## Recompiling the platform from source

This example platform for "Hello, World!" includes the same host implemented
in two different languages—C and Rust—to demonstrate how it's done in both.
Ordinarily you'd only implement your host once, in your preferred language.


### Recompiling the C implementation

```shell
$ clang -c host.c -o host.o 
```

Then `mv` the compiled `host.o` into the apropriate subdirectory under
`host/` based on the operating system you've compiled it for.

### Recompiling the Rust implementation

```shell
$ rustc
```

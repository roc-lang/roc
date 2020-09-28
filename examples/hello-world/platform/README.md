## Recompiling the platform from source

This example platform for "Hello, World!" includes the same host implemented
in two different languages—C and Rust—to demonstrate how it's done in both.
Ordinarily you'd only implement your host once, in your preferred language.


### Recompiling the C implementation

```shell
$ clang -c host.c -o host.o 
```

### Recompiling the Rust implementation

```shell
$ rustc
```

ld -L . -arch "x86_64" -lc -lroc_app.o -e "_main" host.o -o app

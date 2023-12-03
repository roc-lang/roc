
# Running the benchmarks

Install cargo criterion:

```sh
cargo install cargo-criterion
```

To prevent stack overflow on the `CFold` benchmark:

```sh
ulimit -s unlimited
```

In the `cli` folder execute:

```sh
cargo criterion
```

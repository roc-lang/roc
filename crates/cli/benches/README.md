
# Running the benchmarks

Install cargo criterion:

```
cargo install cargo-criterion
```

To prevent stack overflow on the `CFold` benchmark:

```
ulimit -s unlimited
```

In the `cli` folder execute:

```
cargo criterion
```
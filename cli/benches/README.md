
# Running the benchmarks

Install cargo criterion:

```bash
cargo install cargo-criterion
```

To prevent stack overflow on the `CFold` benchmark:

```bash
ulimit -s unlimited
```

In the `cli` folder execute:

```bash
cargo criterion
```

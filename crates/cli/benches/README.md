
# Running the benchmarks

If you're not using nix, install cargo criterion:
```sh
cargo install cargo-criterion
```

To prevent stack overflow on the `CFold` benchmark:

```sh
ulimit -s unlimited
```

In the `crates/cli` folder execute:

```sh
cargo criterion
```

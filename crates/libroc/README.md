# libroc

To generate `roc.h` from the Rust sources in this crate, first `cargo install cbindgen` and then run:

```
cbindgen --config cbindgen.toml --crate libroc --output roc.h
```

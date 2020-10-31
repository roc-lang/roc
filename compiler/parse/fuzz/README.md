To setup fuzzing you will need to install cargo-fuzz and run with rust nightly:

```
$ cargo install cargo-fuzz
$ cargo +nightly fuzz run -O -a fuzz_parse
```

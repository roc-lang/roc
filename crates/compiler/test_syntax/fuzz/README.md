# fuzz

To setup fuzzing you will need to install cargo-fuzz and run with rust nightly:

```sh
$ cargo install cargo-fuzz
$ cargo +nightly fuzz run -j<cores> <target> -- -dict=../dict.txt
```

The different targets can be found by running `cargo fuzz list`.

When a bug is found, it will be reported with commands to run it again and look for a minimized version.
If you are going to file a bug, please minimize the input before filing the bug.

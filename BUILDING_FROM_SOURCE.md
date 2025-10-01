# Building the new Roc compiler from source

Note: If you're looking to build the old compiler from source, go [here](./crates/BUILDING_FROM_SOURCE.md).

If you run into any problems getting Roc built from source, please ask for help in the `#beginners` channel on [Roc Zulip](https://roc.zulipchat.com) (the fastest way), or create an issue in this repo!

## Recommended way

[Download zig 0.15.1](https://ziglang.org/download/) and add it to your PATH.
[Search "Setting up PATH"](https://ziglang.org/learn/getting-started/) for more details.

Do a test run with
```
zig build roc
```

## Using Nix

If you're familiar with nix and like using it, you can build the compiler like this:
```
nix develop ./src
buildcmd
```

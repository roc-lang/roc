# Building the new Roc compiler from source

If you run into any problems getting Roc built from source, please ask for help in the `#beginners` channel on [Roc Zulip](https://roc.zulipchat.com) (the fastest way), or create an issue in this repo!

## Recommended way

[Download zig 0.16.0](https://ziglang.org/download/) and add it to your PATH.
[Search "Setting up PATH"](https://ziglang.org/learn/getting-started/) for more details.

Do a test run with
```
zig build roc
./zig-out/bin/roc version
```

## Using Nix

If you're familiar with nix and like using it, you can build the compiler like this:
```
nix develop ./src
buildcmd
./zig-out/bin/roc version
```

## Windows Notes

Due to a [Zig bug](https://github.com/ziglang/zig/issues/17652) related to extracting dependencies from tarball files containing symlinks (which is not allowed by default on Windows), you might encounter permission denial issues. The workaround is to enable the `Developer Mode` option on Windows, which could be found under `Settings > System > Advanced`. If that does not work, please review the aforementioned bug for any additional clues.

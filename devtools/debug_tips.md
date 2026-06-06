# Debug Tips

## General

- When using github search to find similar errors/issues use `org:roc-lang`, for example: `org:roc-lang valgrind unrecognised instruction`. This will search in basic-cli, basic-webserver, ... as well. Just using `roc` instead of `org:roc-lang` may yield useful results as well.
- Use a debug build of the compiler. We have many asserts enabled in the debug compiler that can alert you to something going wrong. When building from source, build the debug compiler with `zig build roc`, the binary is at `./zig-out/bin/roc`. 
- For Roc code; minimize the code that produces the issue.
- If you plan to look at the data used and produced inside the compiler, try to reproduce your issue with a very simple platform like our [minimal Rust platform](https://github.com/roc-lang/roc/tree/main/examples/platform-switching/rust-platform) instead of for example basic-cli.


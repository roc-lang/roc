# Debug Tips

## General

- When using github search to find similar errors/issues use `org:roc-lang`, for example: `org:roc-lang valgrind unrecognised instruction`. This will search in basic-cli, basic-webserver, ... as well. Just using `roc` instead of `org:roc-lang` may yield useful results as well.
- Use a debug build of the compiler. We have many asserts enabled in the debug compiler that can alert you to something going wrong. When building from source, build the debug compiler with `cargo build --bin roc`, the binary is at `roc/target/debug/roc`. When using roc through a nix flake like in [basic-cli](https://github.com/roc-lang/basic-cli), use `rocPkgs.cli-debug` instead of `rocPkgs.cli`.
- At the bottom of [.cargo/config.toml](https://github.com/roc-lang/roc/blob/main/.cargo/config.toml) we have useful debug flags that activate certain debug prints and extra checks.
- For Roc code; minimize the code that produces the issue.
- If you plan to look at the data used and produced inside the compiler, try to reproduce your issue with a very simple platform like our [minimal Rust platform](https://github.com/roc-lang/roc/tree/main/examples/platform-switching/rust-platform) instead of for example basic-cli.

## Segmentation Faults

- In general we recommend using linux to investigate, it has better tools for this. 
- If your segfault also happens when using `--linker=legacy`, use it to improve valgrind output. For example: `roc build myApp.roc --linker=legacy` followed by `valgrind ./myApp`.
- Use gdb to step through the code, [this gdb script](https://roc.zulipchat.com/#narrow/stream/395097-compiler-development/topic/gdb.20script/near/424422545) can be helpful.
- Use objdump to look at the assembly of the code, for example `objdump -d -M intel ./examples/Arithmetic/main`. Replace `-M intel` with the appropriate flag for your CPU.
- Inspect the generated LLVM IR (`roc build myApp.roc --emit-llvm-ir`) between Roc code that encounters the segfault and code that doesn't.
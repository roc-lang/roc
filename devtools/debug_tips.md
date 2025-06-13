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

### Assembly debuggers

Stepping through the executed assembly is super useful to find out what is going wrong.
Use a debugger (see below) and find the last executed instruction, look that instruction up and check its requirements. An instruction can for example require 16 bit alignment and passing it 8 byte aligned data can cause a segfault.
If you have a commit that works and one that doesn't, step through both executables at the same time to check where they differ.
It can also be useful to keep the llvm IR .ll files open on the side (`roc build myApp.roc --emit-llvm-ir`) to understand how that assembly was generated.
I like using both [IDA free](https://hex-rays.com/ida-free/) and gdb.
IDA free is easier to use and has nicer visualizations compared to gdb, but it does sometimes have difficulty with binaries created by surgical linking.
I've also [not been able to view output (stdout) of a program in IDA free](https://stackoverflow.com/questions/78888834/how-to-view-stdout-in-ida-debugger).

objdump can also be used to look at the full assembly of the executable, for example `objdump -d -M intel ./examples/Arithmetic/main`. Replace `-M intel` with the appropriate flag for your CPU.
Note that the addresses shown in objdump may use a different offset compared to those in IDA or gdb.

#### IDA free

1. [Download here](https://hex-rays.com/ida-free/)
2. Build your roc app with the legacy linker if it does not error only with the surgical linker: `roc build myApp.roc --linker=legacy`
3. Open the produced executable with IDA free, don't change any of the suggested settings.
4. If IDA ever asks for the path for roc_app, just click cancel.
5. You probably want to go to the function you saw in valgrind like `List_walkTryHelp_...` [here](https://github.com/roc-lang/examples/pull/192#issuecomment-2269571439). You can use Ctrl+F in the Function s window in IDA free.
6. Right click and choose `Add Breakpoint` at the first instruction of the function you clicked on the previous step.
7. Run the debugger by pressing F9
8. Use step into (F7) and step over (F8) to see what's going on. Keep an eye on the `General Registers` and `Stack view` windows while you're stepping.


#### gdb

1. Set up [this handy gdb layout](https://github.com/cyrus-and/gdb-dashboard).
2. Start with `gdb ./your-roc-app-executable`, or if your executable takes command line arguments; `gdb --args ./your-roc-app-executable arg1 arg2`
3. Get the complete function name of the function you want to analyze: `info functions yourInterestingFunction`
4. Use that complete function name to set a breakpoint `break fullFunctionName` or set a breakpoint at a specific address `break*0x00000000012345`
5. Execute `run` to start debugging.
6. Step to the next assembly instruction with `si`, or use `ni` if you don't want to step into calls.

gdb scripting is very useful, [for example](https://roc.zulipchat.com/#narrow/stream/395097-compiler-development/topic/gdb.20script/near/424422545).
ChatGPT and Claude are good at writing those scripts as well.

## Code Coverage

When investigating a bug, it can be nice to instantly see if a line of rust code was executed during for example `roc build yourFile.roc`. We can use [`cargo-llvm-cov`](https://github.com/taiki-e/cargo-llvm-cov) for this, on linux, it comes pre-installed with our flake.nix. On macos you'll need to install it with `cargo +stable install cargo-llvm-cov --locked`.

To generate the code coverage file:

```shell
$ cd roc
$ source <(cargo llvm-cov show-env --export-prefix)
$ cargo llvm-cov clean --workspace
$ cargo build --bin roc
# Replace with the command you want to generate coverage for:
$ cargo llvm-cov run build yourFile.roc
# To view in editor
$ cargo llvm-cov report --lcov  --output-path lcov.info
# To view in browser. This html report also allows you to see how many times each line of code was run. 
$ cargo llvm-cov report --html
```
Viewing lcov.info will depend on your editor. For vscode, you can use the [coverage gutters](https://marketplace.visualstudio.com/items?itemName=ryanluker.vscode-coverage-gutters) extension. After installing, click `Watch` in the bottom bar and go to a file for which you want to see the coverage, for example `crates/compiler/build/src/link.rs`. `Watch` in the bottom bar will now be replaced with `x% Coverage`.

## Trace all Function Calls

[uftrace](https://github.com/namhyung/uftrace) allows you to trace all functions that were called in the compiler in order. For example, you can use it with `./target/debug/roc build yourFile.roc`. The output looks like this:
```
roc::main() {
  roc_tracing::setup_tracing();
  roc_cli::build_app();
  roc_packaging::cache::roc_cache_dir();
  roc_cli::build() {
    roc_cli::opt_level_from_flags();
    roc_linker::supported();
    roc_target::Target::architecture();
    roc_build::program::standard_load_config() {
...
```
It can be valuable if you want to compare two compiler versions/commits and see how their function calls differ. It also gives you a nice overview compared to stepping with the debugger.

### Getting started with uftrace

1. [Install uftrace](https://github.com/namhyung/uftrace?tab=readme-ov-file#how-to-build-and-install-uftrace)
1. In the roc repo in rust-toolchain.toml, switch to the commented out nightly channel
1. `export RUSTFLAGS="-Awarnings -Z instrument-mcount -C passes=ee-instrument<post-inline>"`
1. set the [threads variable](https://github.com/roc-lang/roc/blob/690e690bb7c9333825157f54fc471a8a15b92aae/crates/compiler/load_internal/src/file.rs#L1555) to `Threads::Single`.
1. `cargo build --bin roc`
1. Example usage: `uftrace record --filter 'roc_*' ./target/debug/roc build yourFile.roc`
1. Show the trace and drop all functions that do not start with `roc`: `uftrace replay -f none --notrace '^[^r]|^r[^o]|^ro[^c]' -D 5`. `-D 5` sets the function call depth, feel free to modify it to best suit your purpose.

Depending on which functions you are interested in, you may also want to set `let threads = Threads::Single;` in the function `load` in `crates/compiler/load_internal/src/file.rs`. That avoids function calls from being obscured between multiple threads.

If you want to compare the outputs of `uftrace replay -f none ...` between two versions/commits of the compiler, you can do so easily with smart_diff_utrace.html in this devtools folder. This tool ignores differences in `{`,`}` and `;`.

uftrace also allows you to log function arguments but I have not played with that yet. Our arguments can contain a lot of data, so that may not be so practical.

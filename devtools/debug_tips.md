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
4. You probably want to go to the function you saw in valgrind like `List_walkTryHelp_...` [here](https://github.com/roc-lang/examples/pull/192#issuecomment-2269571439). You can use Ctrl+F in the Function s window in IDA free.
5. Right click and choose `Add Breakpoint` at the first instruction of the function you clicked on the previous step.
6. Run the debugger by pressing F9
7. Use step into (F7) and step over (F8) to see what's going on. Keep an eye on the `General Registers` and `Stack view` windows while you're stepping.


#### gdb

1. Set up [this handy gdb layout](https://github.com/cyrus-and/gdb-dashboard).
2. Start with `gdb ./your-roc-app-executable`, or if your executable takes command line arguments; `gdb --args ./your-roc-app-executable arg1 arg2`
3. Get the complete function name of the function you want to analyze: `info functions yourInterestingFunction`
4. Use that complete function name to set a breakpoint `break fullFunctionName` or set a breakpoint at a specific address `break*0x00000000012345`
5. Execute `run` to start debugging.
6. Step to the next assembly instruction with `si`, or use `ni` if you don't want to step into calls.

gdb scripting is very useful, [for example](https://roc.zulipchat.com/#narrow/stream/395097-compiler-development/topic/gdb.20script/near/424422545).
ChatGPT and Claude are good at writing those scripts as well.

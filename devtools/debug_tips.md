# Debug Tips

## General

- When using github search to find similar errors/issues use `org:roc-lang`, for example: `org:roc-lang valgrind unrecognised instruction`. This will search in basic-cli, basic-webserver, ... as well. Just using `roc` instead of `org:roc-lang` may yield useful results as well.
- Use a debug build of the compiler. We have many asserts enabled in the debug compiler that can alert you to something going wrong. When building from source, build the debug compiler with `cargo build --bin roc`, the binary is at `roc/target/debug/roc`. When using roc through a nix flake like in [basic-cli](https://github.com/roc-lang/basic-cli), use `rocPkgs.cli-debug` instead of `rocPkgs.cli`.
- At the bottom of [.cargo/config.toml](https://github.com/roc-lang/roc/blob/main/.cargo/config.toml) we have useful debug flags that activate certain debug prints and extra checks.
- For Roc code; minimize the code that produces the issue.
- If you plan to look at the data used and produced inside the compiler, try to reproduce your issue with a very simple platform like our [minimal Rust platform](https://github.com/roc-lang/roc/tree/main/examples/platform-switching/rust-platform) instead of for example basic-cli.

## Debugging the compiler

If you want to debug the rust compiler instead of a roc program, you need to comment out [this line](https://github.com/roc-lang/roc/blob/bba2103882a8124f3887da6adaaa44cf2b728c3e/Cargo.toml#L240) to be able to see variable contents with lldb or gdb.

## zig build signal 11

Example:

```
❯ zig build test
test
└─ run echo
   └─ run test failure
error: while executing test 'test.test_0', the following command terminated with signal 11 (expected exited with code 0):
/home/username/gitrepos/roc2/roc/.zig-cache/o/c001d6b7d92a5fb63e64be83c43d0d57/test --seed=0x5772e400 --cache-dir=/home/username/gitrepos/roc2/roc/.zig-cache --listen=- 
Build Summary: 4/7 steps succeeded; 1 failed; 361/361 tests passed
test transitive failure
├─ run test failure
└─ run echo transitive failure
   └─ run test (+1 more reused dependencies)
error: the following build command failed with exit code 1:
/home/username/gitrepos/roc2/roc/.zig-cache/o/6e344dd24cd22c7b75f5c380fe9169ed/build /home/username/Downloads/zig-x86_64-linux-0.14.1/zig /home/username/Downloads/zig-x86_64-linux-0.14.1/lib /home/username/gitrepos/roc2/roc /home/username/gitrepos/roc2/roc/.zig-cache /home/username/.cache/zig --seed 0x5772e400 -Z47c7adeb63ed8ed2 test
```

The key is to remove the listen flag from the command that zig provided, so:
```
❯ /home/username/gitrepos/roc2/roc/.zig-cache/o/c001d6b7d92a5fb63e64be83c43d0d57/test --seed=0x5772e400 --cache-dir=/home/username/gitrepos/roc2/roc/.zig-cache
Segmentation fault (core dumped)
```
Then use gdb to get a backtrace:
```
❯ gdb --ex run --ex bt --args /home/username/gitrepos/roc2/roc/.zig-cache/o/c001d6b7d92a5fb63e64be83c43d0d57/test --seed=0x8532821a --cache-dir=/home/username/gitrepos/roc2/roc/.zig-cache
GNU gdb (Ubuntu 15.0.50.20240403-0ubuntu1) 15.0.50.20240403-git
Copyright (C) 2024 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
Type "show copying" and "show warranty" for details.
This GDB was configured as "x86_64-linux-gnu".
Type "show configuration" for configuration details.
For bug reporting instructions, please see:
<https://www.gnu.org/software/gdb/bugs/>.
Find the GDB manual and other documentation resources online at:
    <http://www.gnu.org/software/gdb/documentation/>.

For help, type "help".
Type "apropos word" to search for commands related to "word"...
Reading symbols from /home/username/gitrepos/roc2/roc/.zig-cache/o/c001d6b7d92a5fb63e64be83c43d0d57/test...
Starting program: /home/username/gitrepos/roc2/roc/.zig-cache/o/c001d6b7d92a5fb63e64be83c43d0d57/test --seed=0x8532821a --cache-dir=/home/username/gitrepos/roc2/roc/.zig-cache

This GDB supports auto-downloading debuginfo from the following URLs:
  <https://debuginfod.ubuntu.com>
Enable debuginfod for this session? (y or [n]) y
Debuginfod has been enabled.
To make this setting permanent, add 'set debuginfod enabled on' to .gdbinit.
Downloading separate debug info for system-supplied DSO at 0x7ffff7ffd000
[New LWP 73692]                                                                                                                                                                        

Thread 1 "test" received signal SIGSEGV, Segmentation fault.
testing.refAllDeclsRecursive__anon_13463 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
1158                    .@"struct", .@"enum", .@"union", .@"opaque" => refAllDeclsRecursive(@field(T, decl.name)),
#0  testing.refAllDeclsRecursive__anon_13463 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
#1  0x0000000001206d71 in testing.refAllDeclsRecursive__anon_28897 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
#2  0x00000000011505f1 in testing.refAllDeclsRecursive__anon_13463 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
#3  0x0000000001206d71 in testing.refAllDeclsRecursive__anon_28897 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
#4  0x00000000011505f1 in testing.refAllDeclsRecursive__anon_13463 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
#5  0x0000000001206d71 in testing.refAllDeclsRecursive__anon_28897 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
#6  0x00000000011505f1 in testing.refAllDeclsRecursive__anon_13463 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
#7  0x0000000001206d71 in testing.refAllDeclsRecursive__anon_28897 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
#8  0x00000000011505f1 in testing.refAllDeclsRecursive__anon_13463 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
#9  0x0000000001206d71 in testing.refAllDeclsRecursive__anon_28897 () at /home/username/Downloads/zig-x86_64-linux-0.14.1/lib/std/testing.zig:1158
...
```
Here we have a case of infinite recursion.

## Segmentation Faults

- In general we recommend using linux to investigate, it has better tools for this. 
- (old rust compiler) If your segfault also happens when using `--linker=legacy`, use it to improve valgrind output. For example: `roc build myApp.roc --linker=legacy` followed by `valgrind ./myApp`.
- A very quick way to get a backtrace at the point of segfault is:
```
$ gdb --batch --ex run --ex bt --args roc check myApp.roc
```

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
8. Use step into (F7), step over (F8) and run until next breakpoint (F9) to see what's going on. Keep an eye on the `General Registers` and `Stack view` windows while you're stepping.


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

## Tracing

If there's too much to step through with a debugger, tracing may be the tool for the job.

### Trace executed lines in file

If you want to trace all executed lines in a single file, use the lldb_line_tracer.py script:
```
./devtools/lldb_line_tracer.py <file_to_trace.rs> <binary> -- <binary args> 
```
For example:
```
❯ ./devtools/lldb_line_tracer.py crates/compiler/build/src/program.rs target/debug/roc -- check hello.roc
INFO: Will launch binary with arguments: ['check', 'hello.roc']
[...]
(lldb) process launch -- check hello.roc
=> function: check_file
*  1339     let compilation_start = Instant::now();
*  1343     let target = Target::LinuxX64;
*  1349         function_kind: FunctionKind::from_env(),
*  1351         render: RenderTarget::ColorTerminal,
*  1354         exec_mode: ExecutionMode::Check,
*  1347     let load_config = LoadConfig {
*  1358         roc_file_path,
*  1359         opt_main_path,
*  1356     let mut loaded = roc_load::load_and_typecheck(
*  1364     let buf = &mut String::with_capacity(1024);
*  1366     let mut it = loaded.timings.iter().peekable();
*  1367     while let Some((module_id, module_timing)) = it.next() {
*  1368         let module_name = loaded.interns.module_name(*module_id);
*  1370         buf.push_str("    ");
*  1372         if module_name.is_empty() {
*  1376             buf.push_str(module_name);
*  1379         buf.push('\n');
*  1381         report_timing(buf, "Read .roc file from disk", module_timing.read_roc_file);
=> function: report_timing
*   606         duration.as_secs_f64() * 1000.0,
*   603     writeln!(
*   610 }
[...]
```
The script puts an lldb breakpoint at every line in the provided file, note that some lines contain nothing executable, so they will not be printed.

If needed you can probably easily adapt the script to trace e.g. all *.rs files in a given folder.

### Trace Function Calls

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

#### Getting started with uftrace

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

# Bytebox

<div align="center">
<a href=https://webassembly.org/><img src="https://avatars.githubusercontent.com/u/11578470?s=200&v=4" alt="Markdown Logo" width="150"/></a>

Bytebox is a WebAssembly VM.
</div>

# Getting started

### Requirements
Bytebox currently builds with [Zig 0.15.x](https://ziglang.org/download) to avoid churn on zig master.

To run the tests:
* `wasm-tools` is required to run the wasm testsuite. You can install it via the rust toolchain `cargo install wasm-tools` or directly from the [release page](https://github.com/bytecodealliance/wasm-tools/releases).
* `python3` is required to run the wasi testsuite. You may need to run `python3 -m pip install -r test/wasi/wasi-testsuite/test-runner/requirements.txt` to ensure the wasi test runner has all the necessary dependencies installed.

## Run Tests

```sh
git clone --recurse-submodules https://github.com/rdunnington/bytebox.git
cd bytebox
zig build test-unit # run builtin zig unit tests
zig build test-wasm # run official wasm spec testsuite
zig build test-wasi # run official wasi spec testsuite
zig build test-mem64 # run memory64 compat test
zig build test # run all of the above in parallel (output will not be pretty!)
```

## Usage

You can use the standalone runtime to load and execute WebAssembly programs:
```sh
zig build run -- <file> [function] [function args]...
```

Or embed Bytebox in your own programs:

```zig
// build.zig
const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const exe = b.addExecutable("my_program", "src/main.zig");
    exe.addPackage(std.build.Pkg{
        .name = "bytebox",
        .source = .{ .path = "bytebox/src/core.zig" }, // submodule in the root dir
    });
    exe.setTarget(b.standardTargetOptions(.{}));
    exe.setBuildMode(b.standardReleaseOptions());
    exe.install();
    const run = exe.run();
    const step = b.step("run", "runs my_program");
    step.dependOn(&run.step);
}

// main.zig
const std = @import("std");
const bytebox = @import("bytebox");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator: std.mem.Allocator = gpa.allocator();

    const wasm_data: []u8 = try std.fs.cwd().readFileAlloc(allocator, "example.wasm", 1024 * 128);
    defer allocator.free(wasm_data);

    const module_def = try bytebox.createModuleDefinition(allocator, .{});
    defer module_def.destroy();
    try module_def.decode(wasm_data);

    const module_instance = try bytebox.createModuleInstance(.Stack, module_def, allocator);
    defer module_instance.destroy();
    try module_instance.instantiate(.{});
}
```

Inter-language FFI is also supported. See `src/bytebox.h` for an overview in C. To use bytebox as a static library, link with the built library in `zig-out/lib/`. Note that Zig assumes a default stack size of 8MB, so you'll need to ensure the same in your program.

# Status

This project is still in the alpha stage.

| Legend | Meaning |
| --- | --- |
|✅|Implemented|
|❌|TODO|
|💀|Not planned/Removed from spec|

## [WebAssembly](https://webassembly.github.io/spec/core/index.html) support:

| Status | Feature |
| --- | --- |
|✅|WebAssembly 1.0|
|✅|Sign extension instructions|
|✅|Non-trapping float-to-int conversion|
|✅|Multiple values|
|✅|Reference types|
|✅|Table instructions|
|✅|Multiple tables|
|✅|Bulk memory and table instructions|
|✅|Vector instructions|

## [WASI Preview 1](https://github.com/WebAssembly/WASI/tree/main) support:

| Status | Feature |
| --- | --- |
|✅|args_get|
|✅|args_sizes_get|
|✅|environ_get|
|✅|environ_sizes_get|
|✅|clock_res_get|
|✅|clock_time_get|
|✅|fd_advise|
|✅|fd_allocate|
|✅|fd_close|
|✅|fd_datasync|
|✅|fd_fdstat_get|
|✅|fd_fdstat_set_flags|
|💀|fd_fdstat_set_rights|
|✅|fd_filestat_get|
|✅|fd_filestat_set_size|
|✅|fd_filestat_set_times|
|✅|fd_pread|
|✅|fd_prestat_get|
|✅|fd_prestat_dir_name|
|✅|fd_pwrite|
|✅|fd_read|
|✅|fd_readdir|
|✅|fd_renumber|
|✅|fd_seek|
|❌|fd_sync|
|✅|fd_tell|
|✅|fd_write|
|✅|path_create_directory|
|✅|path_filestat_get|
|✅|path_filestat_set_times|
|❌|path_link|
|✅|path_open|
|❌|path_readlink|
|✅|path_remove_directory|
|❌|path_rename|
|✅|path_symlink|
|✅|path_unlink_file|
|❌|poll_oneoff|
|✅|proc_exit|
|💀|proc_raise|
|❌|sched_yield|
|✅|random_get|
|❌|sock_accept|
|❌|sock_recv|
|❌|sock_send|
|❌|sock_shutdown|

### Roadmap
These tasks must be completed to enter alpha:
* API ergonomics pass
* Documentation
* General TODO/code cleanup
* Crash hardening/fuzzing

To enter beta:
* No breaking API changes after this point
* Performance competitive with other well-known interpreters (e.g. [micro-wasm-runtime](https://github.com/bytecodealliance/wasm-micro-runtime), [wasm3](https://github.com/wasm3/wasm3))

To have a 1.0 release:
* Tested with a wide variety of wasm programs
* Successfully used in other beta-quality projects

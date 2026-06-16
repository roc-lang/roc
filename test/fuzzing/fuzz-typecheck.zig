//! Fuzzing for Roc type checking with generated well-typed source.
//!
//! To build just the repro executable:
//!   zig build build-repro-typecheck
//!
//! To run one input:
//!   ./zig-out/bin/repro-typecheck --verbose <seed_file>
//!
//! To run with AFL++:
//!   zig build -Dfuzz
//!   mkdir -p /tmp/roc-typecheck-corpus
//!   printf '\0' > /tmp/roc-typecheck-corpus/seed
//!   ./zig-out/AFLplusplus/bin/afl-fuzz -i /tmp/roc-typecheck-corpus -o /tmp/roc-typecheck-out zig-out/bin/fuzz-typecheck

const std = @import("std");
const compile = @import("compile");
const roc_target = @import("roc_target");
const FuzzHarness = @import("FuzzHarness.zig");
const FuzzReader = @import("FuzzReader.zig");
const TypedCodeGenerator = @import("TypedCodeGenerator.zig");

const BuildEnv = compile.BuildEnv;

pub export fn zig_fuzz_init() void {}

pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    const input = buf[0..@intCast(len)];
    var reader = FuzzReader.init(input);

    var generator = TypedCodeGenerator.init(gpa, &reader);
    defer generator.deinit();
    generator.generateModule() catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during typecheck fuzz source generation"),
    };

    const generated = generator.getOutput();
    const root_file_name = generator.getRootFileName();
    const generated_files = [_]FuzzHarness.GeneratedFile{
        .{ .name = root_file_name, .source = generated },
    };
    if (debug) {
        std.debug.print("Input length: {d}, bytes consumed: {d}\n", .{ input.len, reader.position });
        FuzzHarness.printGeneratedFiles(&generated_files);
    }

    const fuzz_io = std.Io.Threaded.global_single_threaded.io();
    const abs_paths = FuzzHarness.writeGeneratedFiles(gpa, fuzz_io, "/tmp/roc-fuzz-typecheck", &generated_files) catch @panic("failed to write generated typecheck fuzz source");
    defer FuzzHarness.freeGeneratedPaths(gpa, abs_paths);

    const cwd = std.Io.Dir.cwd().realPathFileAlloc(fuzz_io, ".", gpa) catch @panic("failed to resolve cwd");
    defer gpa.free(cwd);

    var build_env = BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, fuzz_io) catch @panic("OOM during BuildEnv init");
    defer build_env.deinit();
    build_env.setPostCheckPublicationMode(.none);

    build_env.build(abs_paths[0]) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during generated source typecheck"),
        else => {},
    };

    FuzzHarness.assertNoBlockingReports(&build_env, debug, &generated_files, "generated typecheck fuzz source produced blocking report") catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM while draining typecheck fuzz reports"),
    };
}

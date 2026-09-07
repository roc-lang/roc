//! Regression test for issue #11132.

const std = @import("std");
const builtin = @import("builtin");
const lir = @import("lir");
const llvm_codegen = @import("llvm_codegen");
const harness = @import("lower_to_lir_harness.zig");
const Allocator = std.mem.Allocator;

/// Cumulative requested bytes, not peak memory: scratch freed after every
/// procedure still contributes to this deterministic measurement.
const TotalBytesAllocator = struct {
    child: Allocator,
    total_bytes: usize = 0,

    fn allocator(self: *TotalBytesAllocator) Allocator {
        return .{ .ptr = self, .vtable = &.{ .alloc = alloc, .resize = resize, .remap = remap, .free = free } };
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *TotalBytesAllocator = @ptrCast(@alignCast(ctx));
        const result = self.child.rawAlloc(len, alignment, ret_addr);
        if (result != null) self.total_bytes += len;
        return result;
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *TotalBytesAllocator = @ptrCast(@alignCast(ctx));
        if (!self.child.rawResize(memory, alignment, new_len, ret_addr)) return false;
        if (new_len > memory.len) self.total_bytes += new_len - memory.len;
        return true;
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *TotalBytesAllocator = @ptrCast(@alignCast(ctx));
        const result = self.child.rawRemap(memory, alignment, new_len, ret_addr);
        if (result != null and new_len > memory.len) self.total_bytes += new_len - memory.len;
        return result;
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const self: *TotalBytesAllocator = @ptrCast(@alignCast(ctx));
        self.child.rawFree(memory, alignment, ret_addr);
    }
};

const CodeGenMeasurement = struct {
    total_bytes: usize = 0,
    procs: usize = 0,
    locals: usize = 0,
};

// The lowered-program inspector is a plain function pointer.
var measurement: CodeGenMeasurement = .{};

fn measureLlvmIrGeneration(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    var counting = TotalBytesAllocator{ .child = std.testing.allocator };
    {
        var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(
            counting.allocator(),
            &lowered.lir_result.store,
            lowered.lir_result.boxy_erased_arg_desc_offsets.items,
            lowered.lir_result.boxy_erased_arg_desc_params.items,
            lowered.lir_result.boxy_worker_procs.items,
            builtin.target,
        );
        defer codegen.deinit();
        codegen.layout_store = &lowered.lir_result.layouts;
        var generated = codegen.generateEntrypointModule("issue_11132", &.{}) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.TestUnexpectedResult,
        };
        generated.deinit();
    }
    measurement = .{
        .total_bytes = counting.total_bytes,
        .procs = lowered.lir_result.store.getProcSpecs().len,
        .locals = lowered.lir_result.store.localCount(),
    };
}

/// Binary call tree consuming a runtime argument, with string locals in each
/// procedure, so the backend must emit every procedure.
fn procTreeSource(allocator: Allocator, proc_count: usize, locals_per_proc: usize) Allocator.Error![]u8 {
    var source: std.ArrayList(u8) = .empty;
    errdefer source.deinit(allocator);
    try source.appendSlice(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\
    );
    for (0..proc_count) |index| {
        const left = 2 * index + 1;
        const right = 2 * index + 2;
        try source.print(allocator, "f{d} : Str -> Str\nf{d} = |s| {{\n", .{ index, index });
        try source.appendSlice(allocator, "    a1 = Str.concat(s, \"1\")\n");
        for (2..locals_per_proc + 1) |local| {
            try source.print(allocator, "    a{d} = Str.concat(a{d}, \"{d}\")\n", .{ local, local - 1, local });
        }
        if (right < proc_count) {
            try source.print(allocator, "    Str.concat(f{d}(a{d}), f{d}(a{d}))\n", .{ left, locals_per_proc, right, locals_per_proc });
        } else if (left < proc_count) {
            try source.print(allocator, "    f{d}(a{d})\n", .{ left, locals_per_proc });
        } else {
            try source.print(allocator, "    a{d}\n", .{locals_per_proc});
        }
        try source.appendSlice(allocator, "}\n\n");
    }
    try source.appendSlice(allocator,
        \\main! = |args| {
        \\    Echo.line!(f0(args.len().to_str()))
        \\    Ok({})
        \\}
        \\
    );
    return try source.toOwnedSlice(allocator);
}

const platform_source =
    \\platform ""
    \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    \\    exposes [Echo]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "roc_echo_line": Echo.line! }
    \\
    \\import Echo
    \\
    \\main_for_host! : List(Str) => I8
    \\main_for_host! = |args|
    \\    match main!(args) {
    \\        Ok({}) => 0
    \\        Err(Exit(code)) => code
    \\        Err(other) => {
    \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
    \\            1
    \\        }
    \\    }
;

fn llvmIrGenerationMeasurement(proc_count: usize, locals_per_proc: usize) harness.LowerToLirHarnessError!CodeGenMeasurement {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const app_source = try procTreeSource(gpa, proc_count, locals_per_proc);
    defer gpa.free(app_source);
    try tmp_dir.dir.createDirPath(std.testing.io, ".roc_echo_platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = "main.roc", .data = app_source });
    try tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = ".roc_echo_platform/main.roc", .data = platform_source });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = ".roc_echo_platform/Echo.roc",
        .data =
        \\Echo := [].{
        \\    line! : Str => {}
        \\}
        ,
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", gpa);
    defer gpa.free(app_path);
    measurement = .{};
    try harness.runAppPathLoweredInspection(app_path, .{}, measureLlvmIrGeneration);
    return measurement;
}

test "issue 11132: LLVM IR generation work is linear in program size" {
    // Cumulative allocations detect per-proc allocation of program-sized
    // tables without noisy wall-clock thresholds. This must be accompanied
    // by inventory-bounded clearing: reusing allocations alone is insufficient.
    const locals_per_proc = 48;
    const sizes = [_]usize{ 32, 64, 128 };
    var measurements: [sizes.len]CodeGenMeasurement = undefined;
    for (sizes, &measurements) |proc_count, *out| {
        out.* = try llvmIrGenerationMeasurement(proc_count, locals_per_proc);
    }
    const first_growth_linear = measurements[1].total_bytes <= measurements[0].total_bytes * 5 / 2;
    const second_growth_linear = measurements[2].total_bytes <= measurements[1].total_bytes * 5 / 2;
    if (!first_growth_linear or !second_growth_linear) {
        std.debug.print(
            "LLVM IR generation allocation grew nonlinearly: " ++
                "{d}/{d}/{d} app procedures ({d}/{d}/{d} LIR procedures, " ++
                "{d}/{d}/{d} LIR locals) allocated {d}/{d}/{d} bytes\n",
            .{
                sizes[0],                    sizes[1],                    sizes[2],
                measurements[0].procs,       measurements[1].procs,       measurements[2].procs,
                measurements[0].locals,      measurements[1].locals,      measurements[2].locals,
                measurements[0].total_bytes, measurements[1].total_bytes, measurements[2].total_bytes,
            },
        );
    }
    try std.testing.expect(first_growth_linear);
    try std.testing.expect(second_growth_linear);
}

const std = @import("std");
const testing = std.testing;
const expectEqual = testing.expectEqual;

const core = @import("core.zig");
const Limits = core.Limits;
const MemoryInstance = core.MemoryInstance;

const metering = @import("metering.zig");

test "StackVM.Integration" {
    const wasm_filepath = "zig-out/bin/mandelbrot.wasm";

    var allocator = std.testing.allocator;

    var cwd = std.fs.cwd();
    const wasm_data: []u8 = try cwd.readFileAlloc(allocator, wasm_filepath, 1024 * 1024 * 128);
    defer allocator.free(wasm_data);

    const module_def_opts = core.ModuleDefinitionOpts{
        .debug_name = std.fs.path.basename(wasm_filepath),
    };
    var module_def = try core.createModuleDefinition(allocator, module_def_opts);
    defer module_def.destroy();

    try module_def.decode(wasm_data);

    var module_inst = try core.createModuleInstance(.Stack, module_def, allocator);
    defer module_inst.destroy();
}

test "StackVM.Metering" {
    if (!metering.enabled) {
        return;
    }
    const wasm_filepath = "zig-out/bin/fibonacci.wasm";

    var allocator = std.testing.allocator;

    var cwd = std.fs.cwd();
    const wasm_data: []u8 = try cwd.readFileAlloc(allocator, wasm_filepath, 1024 * 1024 * 128);
    defer allocator.free(wasm_data);

    const module_def_opts = core.ModuleDefinitionOpts{
        .debug_name = std.fs.path.basename(wasm_filepath),
    };
    var module_def = try core.createModuleDefinition(allocator, module_def_opts);
    defer module_def.destroy();

    try module_def.decode(wasm_data);

    var module_inst = try core.createModuleInstance(.Stack, module_def, allocator);
    defer module_inst.destroy();

    try module_inst.instantiate(.{});

    var returns = [1]core.Val{.{ .I64 = 5555 }};
    var params = [1]core.Val{.{ .I32 = 10 }};

    const handle = try module_inst.getFunctionHandle("run");
    const res = module_inst.invoke(handle, &params, &returns, .{
        .meter = 2,
    });
    try std.testing.expectError(metering.MeteringTrapError.TrapMeterExceeded, res);
    try std.testing.expectEqual(5555, returns[0].I32);

    const res2 = module_inst.resumeInvoke(&returns, .{ .meter = 5 });
    try std.testing.expectError(metering.MeteringTrapError.TrapMeterExceeded, res2);
    try std.testing.expectEqual(5555, returns[0].I32);

    try module_inst.resumeInvoke(&returns, .{ .meter = 10000 });
    try std.testing.expectEqual(89, returns[0].I32);
}

test "MemoryInstance.init" {
    {
        const limits = Limits{
            .min = 0,
            .max = null,
            .limit_type = 0, // i32 index type
        };
        var memory = try MemoryInstance.init(limits, null);
        defer memory.deinit();
        try expectEqual(memory.limits.min, 0);
        try expectEqual(memory.limits.max, Limits.k_max_pages_i32);
        try expectEqual(memory.size(), 0);
        try expectEqual(memory.mem.Internal.items.len, 0);
    }

    {
        const limits = Limits{
            .min = 0,
            .max = null,
            .limit_type = 4, // i64 index type
        };
        var memory = try MemoryInstance.init(limits, null);
        defer memory.deinit();
        try expectEqual(memory.limits.min, 0);
        try expectEqual(memory.limits.max, Limits.k_max_pages_i64);
        try expectEqual(memory.size(), 0);
        try expectEqual(memory.mem.Internal.items.len, 0);
    }

    {
        const limits = Limits{
            .min = 25,
            .max = 25,
            .limit_type = 1,
        };
        var memory = try MemoryInstance.init(limits, null);
        defer memory.deinit();
        try expectEqual(memory.limits.min, 0);
        try expectEqual(memory.limits.max, limits.max);
        try expectEqual(memory.mem.Internal.items.len, 0);
    }
}

test "MemoryInstance.Internal.grow" {
    {
        const limits = Limits{
            .min = 0,
            .max = null,
            .limit_type = 0,
        };
        var memory = try MemoryInstance.init(limits, null);
        defer memory.deinit();
        try expectEqual(memory.grow(0), true);
        try expectEqual(memory.grow(1), true);
        try expectEqual(memory.size(), 1);
        try expectEqual(memory.grow(1), true);
        try expectEqual(memory.size(), 2);
        try expectEqual(memory.grow(Limits.k_max_pages_i32 - memory.size()), true);
        try expectEqual(memory.size(), Limits.k_max_pages_i32);
    }

    {
        const limits = Limits{
            .min = 0,
            .max = 25,
            .limit_type = 1,
        };
        var memory = try MemoryInstance.init(limits, null);
        defer memory.deinit();
        try expectEqual(memory.grow(25), true);
        try expectEqual(memory.size(), 25);
        try expectEqual(memory.grow(1), false);
        try expectEqual(memory.size(), 25);
    }
}

test "MemoryInstance.Internal.growAbsolute" {
    {
        const limits = Limits{
            .min = 0,
            .max = null,
            .limit_type = 0,
        };
        var memory = try MemoryInstance.init(limits, null);
        defer memory.deinit();
        try expectEqual(memory.growAbsolute(0), true);
        try expectEqual(memory.size(), 0);
        try expectEqual(memory.growAbsolute(1), true);
        try expectEqual(memory.size(), 1);
        try expectEqual(memory.growAbsolute(5), true);
        try expectEqual(memory.size(), 5);
        try expectEqual(memory.growAbsolute(Limits.k_max_pages_i32), true);
        try expectEqual(memory.size(), Limits.k_max_pages_i32);
    }

    {
        const limits = Limits{
            .min = 0,
            .max = 25,
            .limit_type = 1,
        };
        var memory = try MemoryInstance.init(limits, null);
        defer memory.deinit();
        try expectEqual(memory.growAbsolute(25), true);
        try expectEqual(memory.size(), 25);
        try expectEqual(memory.growAbsolute(26), false);
        try expectEqual(memory.size(), 25);
    }
}

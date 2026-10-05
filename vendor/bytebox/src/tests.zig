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

    const wasm_data: []u8 = try std.Io.Dir.cwd().readFileAlloc(std.Options.debug_io, wasm_filepath, allocator, .limited(1024 * 1024 * 128));
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

    const wasm_data: []u8 = try std.Io.Dir.cwd().readFileAlloc(std.Options.debug_io, wasm_filepath, allocator, .limited(1024 * 1024 * 128));
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

fn appendTestU32(bytes: *std.ArrayList(u8), value: u32) !void {
    var remaining = value;
    while (true) {
        const low: u8 = @intCast(remaining & 0x7f);
        remaining >>= 7;
        try bytes.append(testing.allocator, low | @as(u8, if (remaining == 0) 0 else 0x80));
        if (remaining == 0) return;
    }
}

fn appendTestSection(bytes: *std.ArrayList(u8), id: u8, payload: []const u8) !void {
    try bytes.append(testing.allocator, id);
    try appendTestU32(bytes, @intCast(payload.len));
    try bytes.appendSlice(testing.allocator, payload);
}

test "StackVM.If continuations exceed 65535 instructions" {
    const allocator = testing.allocator;
    for ([_]bool{ false, true }) |has_else| {
        for ([_]bool{ false, true }) |branch_to_end| {
            var body: std.ArrayList(u8) = .empty;
            defer body.deinit(allocator);
            // One i32 local, then if (parameter) { local = 11; ... }.
            try body.appendSlice(allocator, &.{ 1, 1, 0x7f, 0x20, 0, 0x04, 0x40, 0x41, 11, 0x21, 1 });
            if (branch_to_end) try body.appendSlice(allocator, &.{ 0x0c, 0 });
            for (0..40000) |_| try body.appendSlice(allocator, &.{ 0x41, 0, 0x1a });
            if (has_else) {
                try body.appendSlice(allocator, &.{ 0x05, 0x41, 22, 0x21, 1 });
                if (branch_to_end) try body.appendSlice(allocator, &.{ 0x0c, 0 });
                for (0..40000) |_| try body.appendSlice(allocator, &.{ 0x41, 0, 0x1a });
            }
            try body.appendSlice(allocator, &.{ 0x0b, 0x20, 1, 0x0b });
            var code: std.ArrayList(u8) = .empty;
            defer code.deinit(allocator);
            try code.append(allocator, 1);
            try appendTestU32(&code, @intCast(body.items.len));
            try code.appendSlice(allocator, body.items);
            var wasm: std.ArrayList(u8) = .empty;
            defer wasm.deinit(allocator);
            try wasm.appendSlice(allocator, "\x00asm\x01\x00\x00\x00");
            try appendTestSection(&wasm, 1, &.{ 1, 0x60, 1, 0x7f, 1, 0x7f });
            try appendTestSection(&wasm, 3, &.{ 1, 0 });
            try appendTestSection(&wasm, 7, &.{ 1, 3, 'r', 'u', 'n', 0, 0 });
            try appendTestSection(&wasm, 10, code.items);
            const module = try core.createModuleDefinition(allocator, .{});
            defer module.destroy();
            try module.decode(wasm.items);
            const instance = try core.createModuleInstance(.Stack, module, allocator);
            defer instance.destroy();
            try instance.instantiate(.{});
            const handle = try instance.getFunctionHandle("run");
            for ([_]i32{ 0, 1 }) |condition| {
                var params = [_]core.Val{.{ .I32 = condition }};
                var returns = [_]core.Val{.{ .I32 = -1 }};
                try instance.invoke(handle, &params, &returns, .{});
                try expectEqual(@as(i32, if (condition != 0) 11 else if (has_else) 22 else 0), returns[0].I32);
            }
        }
    }
}

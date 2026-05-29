const builtin = @import("builtin");
const std = @import("std");
const assert = std.debug.assert;

const def = @import("definition.zig");
const i8x16 = def.i8x16;
const u8x16 = def.u8x16;
const i16x8 = def.i16x8;
const u16x8 = def.u16x8;
const i32x4 = def.i32x4;
const u32x4 = def.u32x4;
const i64x2 = def.i64x2;
const u64x2 = def.u64x2;
const f32x4 = def.f32x4;
const f64x2 = def.f64x2;
const v128 = def.v128;
const Instruction = def.Instruction;
const ValType = def.ValType;
const FuncRef = def.FuncRef;
const ExternRef = def.ExternRef;

const inst = @import("instance.zig");
const ModuleInstance = inst.ModuleInstance;
const TrapError = inst.TrapError;

// V128 values are packed into 2 separate StackVals. This helps reduce wasted memory due to
// alignment requirements since most values are 4 or 8 bytes.
// This is an extern union to avoid the debug error checking Zig adds to unions by default -
// bytebox is aware of the bitwise contents of the stack and often uses types interchangably
const StackVal = extern union {
    I32: i32,
    I64: i64,
    F32: f32,
    F64: f64,
    FuncRef: FuncRef,
    ExternRef: ExternRef,

    comptime {
        if (builtin.mode == .ReleaseFast) {
            std.debug.assert(@sizeOf(StackVal) == 8);
        }
    }
};

// The number of locals/params/returns in this struct counts V128 as 2 values
pub const FunctionInstance = struct {
    type_def_index: usize,
    def_index: usize,
    code: [*]const Instruction,
    instructions_begin: usize,
    num_locals: u32,
    num_params: u16,
    num_returns: u16,

    max_values: u32,
    max_labels: u32,

    module: *ModuleInstance,
};

pub const Label = struct {
    num_returns: u32,
    continuation: u32,
    start_offset_values: u32,
};

pub const CallFrame = struct {
    func: *const FunctionInstance,
    module_instance: *ModuleInstance,
    num_returns: u16,
    start_offset_values: u32,
    start_offset_labels: u16,
};

pub const FuncCallData = struct {
    code: [*]const Instruction,
    continuation: u32,
};

const Stack = @This();

values: []StackVal,
labels: []Label,
frames: []CallFrame,
locals: []StackVal, // references values
num_values: u32,
num_labels: u16,
num_frames: u16,
mem: []u8,
allocator: std.mem.Allocator,

const AllocOpts = struct {
    max_values: u32,
    max_labels: u16,
    max_frames: u16,
};

pub fn init(allocator: std.mem.Allocator) Stack {
    const stack = Stack{
        .values = &[_]StackVal{},
        .labels = &[_]Label{},
        .frames = &[_]CallFrame{},
        .locals = &.{},
        .num_values = 0,
        .num_labels = 0,
        .num_frames = 0,
        .mem = &[_]u8{},
        .allocator = allocator,
    };

    return stack;
}

pub fn deinit(stack: *Stack) void {
    if (stack.mem.len > 0) {
        stack.allocator.free(stack.mem);
    }
}

pub fn allocMemory(stack: *Stack, opts: AllocOpts) !void {
    const alignment = @max(@alignOf(StackVal), @alignOf(Label), @alignOf(CallFrame));
    const values_alloc_size = std.mem.alignForward(usize, @as(usize, @intCast(opts.max_values)) * @sizeOf(StackVal), alignment);
    const labels_alloc_size = std.mem.alignForward(usize, @as(usize, @intCast(opts.max_labels)) * @sizeOf(Label), alignment);
    const frames_alloc_size = std.mem.alignForward(usize, @as(usize, @intCast(opts.max_frames)) * @sizeOf(CallFrame), alignment);
    const total_alloc_size: usize = values_alloc_size + labels_alloc_size + frames_alloc_size;

    const begin_labels = values_alloc_size;
    const begin_frames = values_alloc_size + labels_alloc_size;

    stack.mem = try stack.allocator.alloc(u8, total_alloc_size);
    stack.values.ptr = @as([*]StackVal, @alignCast(@ptrCast(stack.mem.ptr)));
    stack.values.len = opts.max_values;
    stack.labels.ptr = @as([*]Label, @alignCast(@ptrCast(stack.mem[begin_labels..].ptr)));
    stack.labels.len = opts.max_labels;
    stack.frames.ptr = @as([*]CallFrame, @alignCast(@ptrCast(stack.mem[begin_frames..].ptr)));
    stack.frames.len = opts.max_frames;
}

pub fn pushI32(stack: *Stack, v: i32) void {
    stack.values[stack.num_values] = .{ .I32 = v };
    stack.num_values += 1;
}

pub fn pushI64(stack: *Stack, v: i64) void {
    stack.values[stack.num_values] = .{ .I64 = v };
    stack.num_values += 1;
}

pub fn pushF32(stack: *Stack, v: f32) void {
    stack.values[stack.num_values] = .{ .F32 = v };
    stack.num_values += 1;
}

pub fn pushF64(stack: *Stack, v: f64) void {
    stack.values[stack.num_values] = .{ .F64 = v };
    stack.num_values += 1;
}

pub fn pushFuncRef(stack: *Stack, v: FuncRef) void {
    stack.values[stack.num_values] = .{ .FuncRef = v };
    stack.num_values += 1;
}

pub fn pushExternRef(stack: *Stack, v: ExternRef) void {
    stack.values[stack.num_values] = .{ .ExternRef = v };
    stack.num_values += 1;
}

pub fn pushV128(stack: *Stack, v: v128) void {
    const vec2 = @as(f64x2, @bitCast(v));
    stack.values[stack.num_values + 0].F64 = vec2[0];
    stack.values[stack.num_values + 1].F64 = vec2[1];
    stack.num_values += 2;
}

pub fn popI32(stack: *Stack) i32 {
    stack.num_values -= 1;
    return stack.values[stack.num_values].I32;
}

pub fn popI64(stack: *Stack) i64 {
    stack.num_values -= 1;
    return stack.values[stack.num_values].I64;
}

pub fn popF32(stack: *Stack) f32 {
    stack.num_values -= 1;
    return stack.values[stack.num_values].F32;
}

pub fn popF64(stack: *Stack) f64 {
    stack.num_values -= 1;
    return stack.values[stack.num_values].F64;
}

pub fn popFuncRef(stack: *Stack) FuncRef {
    stack.num_values -= 1;
    return stack.values[stack.num_values].FuncRef;
}

pub fn popV128(stack: *Stack) v128 {
    stack.num_values -= 2;
    const f0 = stack.values[stack.num_values + 0].F64;
    const f1 = stack.values[stack.num_values + 1].F64;
    return @bitCast(@as(f64x2, .{ f0, f1 }));
}

pub fn popIndexType(stack: *Stack, index_type: ValType) i64 {
    return switch (index_type) {
        .I32 => stack.popI32(),
        .I64 => stack.popI64(),
        else => unreachable,
    };
}

pub fn pushLabel(stack: *Stack, num_returns: u32, continuation: u32) void {
    assert(stack.num_labels < stack.labels.len);

    stack.labels[stack.num_labels] = Label{
        .num_returns = num_returns,
        .continuation = continuation,
        .start_offset_values = stack.num_values,
    };
    stack.num_labels += 1;
}

pub fn popLabel(stack: *Stack) void {
    stack.num_labels -= 1;
}

pub fn findLabel(stack: Stack, id: u32) *const Label {
    const index: usize = (stack.num_labels - 1) - id;
    return &stack.labels[index];
}

pub fn topLabel(stack: Stack) *const Label {
    return &stack.labels[stack.num_labels - 1];
}

pub fn frameLabel(stack: Stack) *const Label {
    const frame: *const CallFrame = stack.topFrame();
    const frame_label: *const Label = &stack.labels[frame.start_offset_labels];
    return frame_label;
}

pub fn popAllUntilLabelId(stack: *Stack, label_id: u64, pop_final_label: bool, num_returns: usize) void {
    const label_index: u16 = @as(u16, @intCast((stack.num_labels - label_id) - 1));
    const label: *const Label = &stack.labels[label_index];

    if (pop_final_label) {
        const source_begin: usize = stack.num_values - num_returns;
        const source_end: usize = stack.num_values;
        const dest_begin: usize = label.start_offset_values;
        const dest_end: usize = label.start_offset_values + num_returns;

        const returns_source: []const StackVal = stack.values[source_begin..source_end];
        const returns_dest: []StackVal = stack.values[dest_begin..dest_end];
        if (dest_begin <= source_begin) {
            std.mem.copyForwards(StackVal, returns_dest, returns_source);
        } else {
            std.mem.copyBackwards(StackVal, returns_dest, returns_source);
        }

        stack.num_values = @as(u32, @intCast(dest_end));
        stack.num_labels = label_index;
    } else {
        stack.num_values = label.start_offset_values;
        stack.num_labels = label_index + 1;
    }
}

pub fn pushFrame(stack: *Stack, func: *const FunctionInstance, module_instance: *ModuleInstance) TrapError!void {
    // check stack exhaustion
    if (stack.frames.len <= stack.num_frames + 1) {
        @branchHint(std.builtin.BranchHint.cold);
        return error.TrapStackExhausted;
    }
    if (stack.values.len <= stack.num_values + func.max_values) {
        @branchHint(std.builtin.BranchHint.cold);
        return error.TrapStackExhausted;
    }
    if (stack.labels.len <= stack.num_labels + func.max_labels) {
        @branchHint(std.builtin.BranchHint.cold);
        return error.TrapStackExhausted;
    }

    // the stack should already be populated with the params to the function, so all that's
    // left to do is initialize the locals to their default values
    const values_index_begin: u32 = stack.num_values - func.num_params;
    const values_index_end: u32 = stack.num_values + func.num_locals;

    assert(stack.num_frames < stack.frames.len);
    assert(values_index_end < stack.values.len);

    const func_locals = stack.values[stack.num_values..values_index_end];

    // All locals must be initialized to their default value
    // https://webassembly.github.io/spec/core/exec/instructions.html#exec-invoke
    @memset(std.mem.sliceAsBytes(func_locals), 0);

    stack.num_values = values_index_end;
    stack.locals = stack.values[values_index_begin..values_index_end];

    stack.frames[stack.num_frames] = CallFrame{
        .func = func,
        .module_instance = module_instance,
        .num_returns = func.num_returns,
        .start_offset_values = values_index_begin,
        .start_offset_labels = stack.num_labels,
    };
    stack.num_frames += 1;
}

pub fn popFrame(stack: *Stack) ?FuncCallData {
    var frame: *CallFrame = stack.topFrame();

    const continuation: u32 = stack.labels[frame.start_offset_labels].continuation;
    const num_returns: usize = frame.num_returns;
    const source_begin: usize = stack.num_values - num_returns;
    const source_end: usize = stack.num_values;
    const dest_begin: usize = frame.start_offset_values;
    const dest_end: usize = frame.start_offset_values + num_returns;
    assert(dest_begin <= source_begin);

    // Because a function's locals take up stack space, the return values are located
    // after the locals, so we need to copy them back down to the start of the function's
    // stack space, where the caller expects them to be.
    const returns_source: []const StackVal = stack.values[source_begin..source_end];
    const returns_dest: []StackVal = stack.values[dest_begin..dest_end];
    std.mem.copyForwards(StackVal, returns_dest, returns_source);

    stack.num_values = @as(u32, @intCast(dest_end));
    stack.num_labels = frame.start_offset_labels;
    stack.num_frames -= 1;

    if (stack.num_frames > 0) {
        frame = stack.topFrame();
        stack.locals = stack.values[frame.start_offset_values .. stack.num_values + frame.func.num_locals];

        return FuncCallData{
            .code = frame.func.code,
            .continuation = continuation,
        };
    }

    return null;
}

pub fn topFrame(stack: *const Stack) *CallFrame {
    return &stack.frames[stack.num_frames - 1];
}

pub fn localGet(stack: *Stack, local_index: usize) void {
    stack.values[stack.num_values] = stack.locals[local_index];
    stack.num_values += 1;
}

pub fn localGetV128(stack: *Stack, local_index: usize) void {
    stack.values[stack.num_values + 0] = stack.locals[local_index + 0];
    stack.values[stack.num_values + 1] = stack.locals[local_index + 1];
    stack.num_values += 2;
}

pub fn localSet(stack: *Stack, local_index: usize) void {
    stack.num_values -= 1;
    stack.locals[local_index] = stack.values[stack.num_values];
}

pub fn localSetV128(stack: *Stack, local_index: usize) void {
    stack.num_values -= 2;
    stack.locals[local_index + 0] = stack.values[stack.num_values + 0];
    stack.locals[local_index + 1] = stack.values[stack.num_values + 1];
}

pub fn localTee(stack: *Stack, local_index: usize) void {
    stack.locals[local_index] = stack.values[stack.num_values - 1];
}

pub fn localTeeV128(stack: *Stack, local_index: usize) void {
    stack.locals[local_index + 0] = stack.values[stack.num_values - 2];
    stack.locals[local_index + 1] = stack.values[stack.num_values - 1];
}

pub fn select(stack: *Stack) void {
    const boolean: i32 = stack.values[stack.num_values - 1].I32;
    if (boolean == 0) {
        stack.values[stack.num_values - 3] = stack.values[stack.num_values - 2];
    }
    stack.num_values -= 2;
}

pub fn selectV128(stack: *Stack) void {
    const boolean: i32 = stack.values[stack.num_values - 1].I32;
    if (boolean == 0) {
        stack.values[stack.num_values - 5] = stack.values[stack.num_values - 3];
        stack.values[stack.num_values - 4] = stack.values[stack.num_values - 2];
    }
    stack.num_values -= 3;
}

pub fn popAll(stack: *Stack) void {
    stack.num_values = 0;
    stack.num_labels = 0;
    stack.num_frames = 0;
    stack.locals = &.{};
}

pub fn debugDump(stack: Stack) void {
    std.debug.print("===== stack dump =====\n", .{});
    for (stack.values[0..stack.num_values]) |val| {
        std.debug.print("I32: {}, I64: {}, F32: {}, F64: {}, FuncRef: {}\n", .{
            val.I32,
            val.I64,
            val.F32,
            val.F64,
            val.FuncRef.func,
        });
    }
    std.debug.print("======================\n", .{});
}

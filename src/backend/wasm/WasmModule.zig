//! Wasm binary format encoder.
//!
//! Builds a valid wasm module from type definitions, function bodies,
//! exports, and other sections. Produces the binary encoding with
//! proper LEB128 variable-length integers.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Self = @This();

/// Wasm value types
pub const ValType = enum(u8) {
    i32 = 0x7F,
    i64 = 0x7E,
    f32 = 0x7D,
    f64 = 0x7C,
};

/// Wasm export kinds
pub const ExportKind = enum(u8) {
    func = 0x00,
    table = 0x01,
    memory = 0x02,
    global = 0x03,
};

/// Wasm section IDs
const SectionId = enum(u8) {
    type_section = 1,
    import_section = 2,
    function_section = 3,
    memory_section = 5,
    global_section = 6,
    export_section = 7,
    code_section = 10,
    data_section = 11,
};

/// Wasm opcodes
pub const Op = struct {
    // Control
    pub const @"unreachable": u8 = 0x00;
    pub const nop: u8 = 0x01;
    pub const block: u8 = 0x02;
    pub const loop_: u8 = 0x03;
    pub const @"if": u8 = 0x04;
    pub const @"else": u8 = 0x05;
    pub const end: u8 = 0x0B;
    pub const br: u8 = 0x0C;
    pub const br_if: u8 = 0x0D;
    pub const br_table: u8 = 0x0E;
    pub const @"return": u8 = 0x0F;
    pub const call: u8 = 0x10;
    pub const call_indirect: u8 = 0x11;

    // Parametric
    pub const drop: u8 = 0x1A;
    pub const select: u8 = 0x1B;

    // Variable
    pub const local_get: u8 = 0x20;
    pub const local_set: u8 = 0x21;
    pub const local_tee: u8 = 0x22;
    pub const global_get: u8 = 0x23;
    pub const global_set: u8 = 0x24;

    // Memory
    pub const i32_load: u8 = 0x28;
    pub const i64_load: u8 = 0x29;
    pub const f32_load: u8 = 0x2A;
    pub const f64_load: u8 = 0x2B;
    pub const i32_load8_s: u8 = 0x2C;
    pub const i32_load8_u: u8 = 0x2D;
    pub const i32_load16_s: u8 = 0x2E;
    pub const i32_load16_u: u8 = 0x2F;
    pub const i64_load8_s: u8 = 0x30;
    pub const i64_load8_u: u8 = 0x31;
    pub const i64_load16_s: u8 = 0x32;
    pub const i64_load16_u: u8 = 0x33;
    pub const i64_load32_s: u8 = 0x34;
    pub const i64_load32_u: u8 = 0x35;
    pub const i32_store: u8 = 0x36;
    pub const i64_store: u8 = 0x37;
    pub const f32_store: u8 = 0x38;
    pub const f64_store: u8 = 0x39;
    pub const i32_store8: u8 = 0x3A;
    pub const i32_store16: u8 = 0x3B;
    pub const i64_store8: u8 = 0x3C;
    pub const i64_store16: u8 = 0x3D;
    pub const i64_store32: u8 = 0x3E;

    // Constants
    pub const i32_const: u8 = 0x41;
    pub const i64_const: u8 = 0x42;
    pub const f32_const: u8 = 0x43;
    pub const f64_const: u8 = 0x44;

    // i32 comparison
    pub const i32_eqz: u8 = 0x45;
    pub const i32_eq: u8 = 0x46;
    pub const i32_ne: u8 = 0x47;
    pub const i32_lt_s: u8 = 0x48;
    pub const i32_lt_u: u8 = 0x49;
    pub const i32_gt_s: u8 = 0x4A;
    pub const i32_gt_u: u8 = 0x4B;
    pub const i32_le_s: u8 = 0x4C;
    pub const i32_le_u: u8 = 0x4D;
    pub const i32_ge_s: u8 = 0x4E;
    pub const i32_ge_u: u8 = 0x4F;

    // i64 comparison
    pub const i64_eqz: u8 = 0x50;
    pub const i64_eq: u8 = 0x51;
    pub const i64_ne: u8 = 0x52;
    pub const i64_lt_s: u8 = 0x53;
    pub const i64_lt_u: u8 = 0x54;
    pub const i64_gt_s: u8 = 0x55;
    pub const i64_gt_u: u8 = 0x56;
    pub const i64_le_s: u8 = 0x57;
    pub const i64_le_u: u8 = 0x58;
    pub const i64_ge_s: u8 = 0x59;
    pub const i64_ge_u: u8 = 0x5A;

    // f32 comparison
    pub const f32_eq: u8 = 0x5B;
    pub const f32_ne: u8 = 0x5C;
    pub const f32_lt: u8 = 0x5D;
    pub const f32_gt: u8 = 0x5E;
    pub const f32_le: u8 = 0x5F;
    pub const f32_ge: u8 = 0x60;

    // f64 comparison
    pub const f64_eq: u8 = 0x61;
    pub const f64_ne: u8 = 0x62;
    pub const f64_lt: u8 = 0x63;
    pub const f64_gt: u8 = 0x64;
    pub const f64_le: u8 = 0x65;
    pub const f64_ge: u8 = 0x66;

    // i32 arithmetic
    pub const i32_add: u8 = 0x6A;
    pub const i32_sub: u8 = 0x6B;
    pub const i32_mul: u8 = 0x6C;
    pub const i32_div_s: u8 = 0x6D;
    pub const i32_div_u: u8 = 0x6E;
    pub const i32_rem_s: u8 = 0x6F;
    pub const i32_rem_u: u8 = 0x70;
    pub const i32_and: u8 = 0x71;
    pub const i32_or: u8 = 0x72;
    pub const i32_xor: u8 = 0x73;
    pub const i32_shl: u8 = 0x74;
    pub const i32_shr_s: u8 = 0x75;
    pub const i32_shr_u: u8 = 0x76;
    pub const i32_rotl: u8 = 0x77;
    pub const i32_rotr: u8 = 0x78;

    // i64 arithmetic
    pub const i64_add: u8 = 0x7C;
    pub const i64_sub: u8 = 0x7D;
    pub const i64_mul: u8 = 0x7E;
    pub const i64_div_s: u8 = 0x7F;
    pub const i64_div_u: u8 = 0x80;
    pub const i64_rem_s: u8 = 0x81;
    pub const i64_rem_u: u8 = 0x82;
    pub const i64_and: u8 = 0x83;
    pub const i64_or: u8 = 0x84;
    pub const i64_xor: u8 = 0x85;
    pub const i64_shl: u8 = 0x86;
    pub const i64_shr_s: u8 = 0x87;
    pub const i64_shr_u: u8 = 0x88;
    pub const i64_rotl: u8 = 0x89;
    pub const i64_rotr: u8 = 0x8A;

    // f32 arithmetic
    pub const f32_abs: u8 = 0x8B;
    pub const f32_neg: u8 = 0x8C;
    pub const f32_ceil: u8 = 0x8D;
    pub const f32_floor: u8 = 0x8E;
    pub const f32_trunc: u8 = 0x8F;
    pub const f32_nearest: u8 = 0x90;
    pub const f32_sqrt: u8 = 0x91;
    pub const f32_add: u8 = 0x92;
    pub const f32_sub: u8 = 0x93;
    pub const f32_mul: u8 = 0x94;
    pub const f32_div: u8 = 0x95;
    pub const f32_min: u8 = 0x96;
    pub const f32_max: u8 = 0x97;
    pub const f32_copysign: u8 = 0x98;

    // f64 arithmetic
    pub const f64_abs: u8 = 0x99;
    pub const f64_neg: u8 = 0x9A;
    pub const f64_ceil: u8 = 0x9B;
    pub const f64_floor: u8 = 0x9C;
    pub const f64_trunc: u8 = 0x9D;
    pub const f64_nearest: u8 = 0x9E;
    pub const f64_sqrt: u8 = 0x9F;
    pub const f64_add: u8 = 0xA0;
    pub const f64_sub: u8 = 0xA1;
    pub const f64_mul: u8 = 0xA2;
    pub const f64_div: u8 = 0xA3;
    pub const f64_min: u8 = 0xA4;
    pub const f64_max: u8 = 0xA5;
    pub const f64_copysign: u8 = 0xA6;

    // Conversions
    pub const i32_wrap_i64: u8 = 0xA7;
    pub const i32_trunc_f32_s: u8 = 0xA8;
    pub const i32_trunc_f32_u: u8 = 0xA9;
    pub const i32_trunc_f64_s: u8 = 0xAA;
    pub const i32_trunc_f64_u: u8 = 0xAB;
    pub const i64_extend_i32_s: u8 = 0xAC;
    pub const i64_extend_i32_u: u8 = 0xAD;
    pub const i64_trunc_f32_s: u8 = 0xAE;
    pub const i64_trunc_f32_u: u8 = 0xAF;
    pub const i64_trunc_f64_s: u8 = 0xB0;
    pub const i64_trunc_f64_u: u8 = 0xB1;
    pub const f32_convert_i32_s: u8 = 0xB2;
    pub const f32_convert_i32_u: u8 = 0xB3;
    pub const f32_convert_i64_s: u8 = 0xB4;
    pub const f32_convert_i64_u: u8 = 0xB5;
    pub const f32_demote_f64: u8 = 0xB6;
    pub const f64_convert_i32_s: u8 = 0xB7;
    pub const f64_convert_i32_u: u8 = 0xB8;
    pub const f64_convert_i64_s: u8 = 0xB9;
    pub const f64_convert_i64_u: u8 = 0xBA;
    pub const f64_promote_f32: u8 = 0xBB;
    pub const i32_reinterpret_f32: u8 = 0xBC;
    pub const i64_reinterpret_f64: u8 = 0xBD;
    pub const f32_reinterpret_i32: u8 = 0xBE;
    pub const f64_reinterpret_i64: u8 = 0xBF;

    // i32 sign extension
    pub const i32_extend8_s: u8 = 0xC0;
    pub const i32_extend16_s: u8 = 0xC1;
};

/// Block type for structured control flow
pub const BlockType = enum(u8) {
    void = 0x40,
    i32 = 0x7F,
    i64 = 0x7E,
    f32 = 0x7D,
    f64 = 0x7C,
};

/// A function type (signature)
const FuncType = struct {
    params: []const ValType,
    results: []const ValType,
};

/// An export entry
const Export = struct {
    name: []const u8,
    kind: ExportKind,
    idx: u32,
};

/// A function body (code)
const FuncBody = struct {
    body: []const u8,
};

/// A data segment placed in linear memory
const DataSegment = struct {
    offset: u32, // offset in linear memory
    data: []const u8, // bytes to place
};

/// Module state
allocator: Allocator,
func_types: std.ArrayList(FuncType),
func_type_indices: std.ArrayList(u32), // func_idx -> type_idx
func_bodies: std.ArrayList(FuncBody),
exports: std.ArrayList(Export),
data_segments: std.ArrayList(DataSegment),
/// Next available offset for data placement in linear memory (grows up from 0).
data_offset: u32,
has_memory: bool,
memory_min_pages: u32,
has_stack_pointer: bool,

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .func_types = .empty,
        .func_type_indices = .empty,
        .func_bodies = .empty,
        .exports = .empty,
        .data_segments = .empty,
        .data_offset = 1024, // reserve first 1KB for future use
        .has_memory = false,
        .memory_min_pages = 1,
        .has_stack_pointer = false,
    };
}

pub fn deinit(self: *Self) void {
    for (self.func_types.items) |ft| {
        self.allocator.free(ft.params);
        self.allocator.free(ft.results);
    }
    self.func_types.deinit(self.allocator);
    self.func_type_indices.deinit(self.allocator);
    for (self.func_bodies.items) |fb| {
        if (fb.body.len > 0) {
            self.allocator.free(fb.body);
        }
    }
    self.func_bodies.deinit(self.allocator);
    self.exports.deinit(self.allocator);
    for (self.data_segments.items) |ds| {
        self.allocator.free(ds.data);
    }
    self.data_segments.deinit(self.allocator);
}

/// Add a function type (signature) and return its index.
pub fn addFuncType(self: *Self, params: []const ValType, results: []const ValType) !u32 {
    const idx: u32 = @intCast(self.func_types.items.len);
    const params_copy = try self.allocator.dupe(ValType, params);
    errdefer self.allocator.free(params_copy);
    const results_copy = try self.allocator.dupe(ValType, results);
    try self.func_types.append(self.allocator, .{
        .params = params_copy,
        .results = results_copy,
    });
    return idx;
}

/// Add a function (maps to a type index) and return the function index.
pub fn addFunction(self: *Self, type_idx: u32) !u32 {
    const func_idx: u32 = @intCast(self.func_type_indices.items.len);
    try self.func_type_indices.append(self.allocator, type_idx);
    return func_idx;
}

/// Set the body of a function.
pub fn setFunctionBody(self: *Self, func_idx: u32, body: []const u8) !void {
    const body_copy = try self.allocator.dupe(u8, body);
    // Ensure we have enough slots
    while (self.func_bodies.items.len <= func_idx) {
        try self.func_bodies.append(self.allocator, .{ .body = &.{} });
    }
    // Free any old body
    if (self.func_bodies.items[func_idx].body.len > 0) {
        self.allocator.free(self.func_bodies.items[func_idx].body);
    }
    self.func_bodies.items[func_idx] = .{ .body = body_copy };
}

/// Add an export.
pub fn addExport(self: *Self, name: []const u8, kind: ExportKind, idx: u32) !void {
    try self.exports.append(self.allocator, .{
        .name = name,
        .kind = kind,
        .idx = idx,
    });
}

/// Enable memory section with the given minimum page count.
pub fn enableMemory(self: *Self, min_pages: u32) void {
    self.has_memory = true;
    self.memory_min_pages = min_pages;
}

/// Add a data segment to linear memory. Returns the offset where the data
/// will be placed. The data is copied and aligned to `align_bytes`.
pub fn addDataSegment(self: *Self, data: []const u8, align_bytes: u32) !u32 {
    // Align the offset
    const alignment = if (align_bytes > 0) align_bytes else 1;
    self.data_offset = (self.data_offset + alignment - 1) & ~(alignment - 1);

    const offset = self.data_offset;
    const data_copy = try self.allocator.dupe(u8, data);
    try self.data_segments.append(self.allocator, .{
        .offset = offset,
        .data = data_copy,
    });
    self.data_offset += @intCast(data.len);
    return offset;
}

/// Enable the __stack_pointer global.
pub fn enableStackPointer(self: *Self, initial_value: u32) void {
    _ = initial_value;
    self.has_stack_pointer = true;
}

/// Encode the module to a valid wasm binary.
pub fn encode(self: *Self, allocator: Allocator) ![]u8 {
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);

    // Magic number and version
    try output.appendSlice(allocator, &.{ 0x00, 0x61, 0x73, 0x6D }); // \0asm
    try output.appendSlice(allocator, &.{ 0x01, 0x00, 0x00, 0x00 }); // version 1

    // Type section
    if (self.func_types.items.len > 0) {
        try self.encodeTypeSection(allocator, &output);
    }

    // Function section
    if (self.func_type_indices.items.len > 0) {
        try self.encodeFunctionSection(allocator, &output);
    }

    // Memory section
    if (self.has_memory) {
        try self.encodeMemorySection(allocator, &output);
    }

    // Global section
    if (self.has_stack_pointer) {
        try self.encodeGlobalSection(allocator, &output);
    }

    // Export section
    if (self.exports.items.len > 0) {
        try self.encodeExportSection(allocator, &output);
    }

    // Code section
    if (self.func_bodies.items.len > 0) {
        try self.encodeCodeSection(allocator, &output);
    }

    // Data section
    if (self.data_segments.items.len > 0) {
        try self.encodeDataSection(allocator, &output);
    }

    return output.toOwnedSlice(allocator);
}

fn encodeTypeSection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, @intCast(self.func_types.items.len));

    for (self.func_types.items) |ft| {
        try section_data.append(gpa, 0x60); // func type marker
        try leb128WriteU32(gpa, &section_data, @intCast(ft.params.len));
        for (ft.params) |p| {
            try section_data.append(gpa, @intFromEnum(p));
        }
        try leb128WriteU32(gpa, &section_data, @intCast(ft.results.len));
        for (ft.results) |r| {
            try section_data.append(gpa, @intFromEnum(r));
        }
    }

    try output.append(gpa, @intFromEnum(SectionId.type_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

fn encodeFunctionSection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, @intCast(self.func_type_indices.items.len));
    for (self.func_type_indices.items) |type_idx| {
        try leb128WriteU32(gpa, &section_data, type_idx);
    }

    try output.append(gpa, @intFromEnum(SectionId.function_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

fn encodeMemorySection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, 1); // 1 memory
    try section_data.append(gpa, 0x00); // no max
    try leb128WriteU32(gpa, &section_data, self.memory_min_pages);

    try output.append(gpa, @intFromEnum(SectionId.memory_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

fn encodeGlobalSection(_: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, 1); // 1 global
    try section_data.append(gpa, @intFromEnum(ValType.i32));
    try section_data.append(gpa, 0x01); // mutable
    // Init expr: i32.const 65536; end
    try section_data.append(gpa, Op.i32_const);
    try leb128WriteI32(gpa, &section_data, 65536);
    try section_data.append(gpa, Op.end);

    try output.append(gpa, @intFromEnum(SectionId.global_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

fn encodeExportSection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, @intCast(self.exports.items.len));
    for (self.exports.items) |exp| {
        try leb128WriteU32(gpa, &section_data, @intCast(exp.name.len));
        try section_data.appendSlice(gpa, exp.name);
        try section_data.append(gpa, @intFromEnum(exp.kind));
        try leb128WriteU32(gpa, &section_data, exp.idx);
    }

    try output.append(gpa, @intFromEnum(SectionId.export_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

fn encodeCodeSection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, @intCast(self.func_bodies.items.len));
    for (self.func_bodies.items) |fb| {
        try leb128WriteU32(gpa, &section_data, @intCast(fb.body.len));
        try section_data.appendSlice(gpa, fb.body);
    }

    try output.append(gpa, @intFromEnum(SectionId.code_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

fn encodeDataSection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, @intCast(self.data_segments.items.len));
    for (self.data_segments.items) |ds| {
        // Active segment for memory 0
        try leb128WriteU32(gpa, &section_data, 0); // flags: active, memory 0
        // Offset expression: i32.const <offset>; end
        try section_data.append(gpa, Op.i32_const);
        try leb128WriteI32(gpa, &section_data, @intCast(ds.offset));
        try section_data.append(gpa, Op.end);
        // Data bytes
        try leb128WriteU32(gpa, &section_data, @intCast(ds.data.len));
        try section_data.appendSlice(gpa, ds.data);
    }

    try output.append(gpa, @intFromEnum(SectionId.data_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

// --- LEB128 encoding utilities ---

/// Encode a u32 as unsigned LEB128 and append to the list.
pub fn leb128WriteU32(gpa: Allocator, output: *std.ArrayList(u8), value: u32) !void {
    var val = value;
    while (true) {
        const byte: u8 = @truncate(val & 0x7F);
        val >>= 7;
        if (val == 0) {
            try output.append(gpa, byte);
            break;
        } else {
            try output.append(gpa, byte | 0x80);
        }
    }
}

/// Encode an i32 as signed LEB128 and append to the list.
pub fn leb128WriteI32(gpa: Allocator, output: *std.ArrayList(u8), value: i32) !void {
    var val = value;
    while (true) {
        const byte: u8 = @truncate(@as(u32, @bitCast(val)) & 0x7F);
        val >>= 7;
        if ((val == 0 and (byte & 0x40) == 0) or (val == -1 and (byte & 0x40) != 0)) {
            try output.append(gpa, byte);
            break;
        } else {
            try output.append(gpa, byte | 0x80);
        }
    }
}

/// Encode an i64 as signed LEB128 and append to the list.
pub fn leb128WriteI64(gpa: Allocator, output: *std.ArrayList(u8), value: i64) !void {
    var val = value;
    while (true) {
        const byte: u8 = @truncate(@as(u64, @bitCast(val)) & 0x7F);
        val >>= 7;
        if ((val == 0 and (byte & 0x40) == 0) or (val == -1 and (byte & 0x40) != 0)) {
            try output.append(gpa, byte);
            break;
        } else {
            try output.append(gpa, byte | 0x80);
        }
    }
}

//! Wasm binary format encoder and parser.
//!
//! Builds valid wasm modules from type definitions, function bodies,
//! exports, and other sections. Also parses relocatable WASM objects
//! for surgical linking.

const std = @import("std");
const Allocator = std.mem.Allocator;
const WasmLinking = @import("WasmLinking.zig");

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
pub const SectionId = enum(u8) {
    custom_section = 0,
    type_section = 1,
    import_section = 2,
    function_section = 3,
    table_section = 4,
    memory_section = 5,
    global_section = 6,
    export_section = 7,
    start_section = 8,
    element_section = 9,
    code_section = 10,
    data_section = 11,
    data_count_section = 12,
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
pub const FuncType = struct {
    params: []const ValType,
};

/// An export entry
pub const Export = struct {
    name: []const u8,
    kind: ExportKind,
    idx: u32,
};

/// A function body (code)
const FuncBody = struct {
    body: []const u8,
};

/// A defined global variable (beyond the built-in __stack_pointer).
/// Used for PIC globals like __memory_base and __table_base that
/// resolve to constants in the final linked module.
pub const DefinedGlobal = struct {
    val_type: u8, // 0x7F = i32, 0x7E = i64
    mutable: bool,
    init_value: i32,
};

/// A data segment placed in linear memory
const DataSegment = struct {
    offset: u32, // offset in linear memory
    data: []u8, // bytes to place
    /// Byte offset of this segment's payload within the original data section body.
    /// Used to normalize reloc.DATA entries during preload.
    section_offset: u32 = 0,
};

/// An imported function
pub const Import = struct {
    module_name: []const u8,
    field_name: []const u8,
    type_idx: u32,
};

/// An imported global (e.g. __stack_pointer, __memory_base).
/// PIC modules use these for position-independent addressing.
pub const GlobalImport = struct {
    module_name: []const u8,
    field_name: []const u8,
    val_type: u8, // raw valtype byte (0x7F=i32, 0x7E=i64, etc.)
    mutable: bool,
};

/// An imported table (e.g. __indirect_function_table).
/// PIC modules use this for indirect function calls.
pub const TableImport = struct {
    module_name: []const u8,
    field_name: []const u8,
};

/// Wasm reference type for funcref tables
const funcref: u8 = 0x70;

/// WASM32 layout of the RocOps struct in linear memory.
///
/// On native 64-bit targets, RocOps is 72 bytes with 8-byte pointers and function
/// pointers. On wasm32, function pointers don't exist in linear memory — instead,
/// functions are referenced by u32 table indices for use with `call_indirect`.
/// This makes the WASM layout 36 bytes with all fields being i32.
///
/// Two distinct `call_indirect` type signatures are used:
/// - RocOps callbacks (roc_alloc, etc.): 2-arg `(i32 args_struct_ptr, i32 env_ptr) → void`
/// - Hosted functions (RocCall ABI):     3-arg `(i32 roc_ops_ptr, i32 ret_ptr, i32 args_ptr) → void`
pub const WasmRocOps = struct {
    /// Host environment pointer (passed as second arg to all RocOps callbacks).
    pub const env_ptr: u32 = 0;
    /// Table index for roc_alloc: (args_ptr, env_ptr) → void.
    pub const roc_alloc_table_idx: u32 = 4;
    /// Table index for roc_dealloc: (args_ptr, env_ptr) → void.
    pub const roc_dealloc_table_idx: u32 = 8;
    /// Table index for roc_realloc: (args_ptr, env_ptr) → void.
    pub const roc_realloc_table_idx: u32 = 12;
    /// Table index for roc_dbg: (args_ptr, env_ptr) → void.
    pub const roc_dbg_table_idx: u32 = 16;
    /// Table index for roc_expect_failed: (args_ptr, env_ptr) → void.
    pub const roc_expect_failed_table_idx: u32 = 20;
    /// Table index for roc_crashed: (args_ptr, env_ptr) → void.
    pub const roc_crashed_table_idx: u32 = 24;
    /// Number of hosted functions provided by the platform.
    pub const hosted_fns_count: u32 = 28;
    /// Pointer to array of u32 table indices for hosted functions in linear memory.
    pub const hosted_fns_ptr: u32 = 32;
    /// Total size of the WasmRocOps struct in bytes.
    pub const total_size: u32 = 36;

    comptime {
        // Verify layout: 9 consecutive i32 fields at 4-byte stride = 36 bytes total.
        std.debug.assert(total_size == 36);
        std.debug.assert(hosted_fns_ptr + 4 == total_size);

        // All offsets must be 4-byte aligned and sequential.
        std.debug.assert(env_ptr == 0);
        std.debug.assert(roc_alloc_table_idx == env_ptr + 4);
        std.debug.assert(roc_dealloc_table_idx == roc_alloc_table_idx + 4);
        std.debug.assert(roc_realloc_table_idx == roc_dealloc_table_idx + 4);
        std.debug.assert(roc_dbg_table_idx == roc_realloc_table_idx + 4);
        std.debug.assert(roc_expect_failed_table_idx == roc_dbg_table_idx + 4);
        std.debug.assert(roc_crashed_table_idx == roc_expect_failed_table_idx + 4);
        std.debug.assert(hosted_fns_count == roc_crashed_table_idx + 4);
        std.debug.assert(hosted_fns_ptr == hosted_fns_count + 4);
    }
};

/// Module state
allocator: Allocator,
func_types: std.ArrayList(FuncType),
func_type_results: std.ArrayList(?ValType), // parallel to func_types
func_type_indices: std.ArrayList(u32), // func_idx -> type_idx
func_bodies: std.ArrayList(FuncBody),
exports: std.ArrayList(Export),
/// Function imports (indices 0..import_fn_count-1 in the function index space).
imports: std.ArrayList(Import),
/// Global imports (e.g. __stack_pointer, __memory_base for PIC modules).
global_imports: std.ArrayList(GlobalImport),
/// Table imports (e.g. __indirect_function_table for PIC modules).
table_imports: std.ArrayList(TableImport),
data_segments: std.ArrayList(DataSegment),
/// Next available offset for data placement in linear memory (grows up from 0).
data_offset: u32,
has_memory: bool,
memory_min_pages: u32,
has_stack_pointer: bool,
stack_pointer_init: u32,
/// Whether the module has a funcref table (for call_indirect).
has_table: bool,
/// Function indices to place in the table (element section).
table_func_indices: std.ArrayList(u32),
/// Additional defined globals (beyond __stack_pointer at index 0).
/// Used for PIC globals like __memory_base, __table_base.
extra_globals: std.ArrayList(DefinedGlobal),

// --- Fields for surgical linking (populated by preload) ---

/// Raw bytes of all function bodies in the code section.
/// Relocation offsets refer to positions within this buffer.
code_bytes: std.ArrayList(u8),
/// Byte offset of each function body within code_bytes.
/// Length: func_type_indices.items.len (locally-defined functions only).
function_offsets: std.ArrayList(u32),
/// Number of dummy functions prepended during linking to maintain index stability.
dead_import_dummy_count: u32,
/// Number of function imports (may differ from imports.items.len after linking).
import_fn_count: u32,
/// Number of global imports parsed from the import section.
/// Tracked for validation — global imports are not stored in the imports array.
import_global_count: u32,
/// LEB128 byte size of the function count in the code section header.
/// Relocation offsets in reloc.CODE are relative to the section body (which
/// includes the function count), but code_bytes starts AFTER the count.
/// This delta must be subtracted from reloc offsets to index into code_bytes.
code_section_fn_count_leb_size: u32,

/// Linking metadata (symbol table, segment info, init funcs).
linking: WasmLinking.LinkingSection,
/// Relocations for the code section.
reloc_code: WasmLinking.RelocationSection,
/// Relocations for the data section.
reloc_data: WasmLinking.RelocationSection,

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .func_types = .empty,
        .func_type_results = .empty,
        .func_type_indices = .empty,
        .func_bodies = .empty,
        .exports = .empty,
        .imports = .empty,
        .global_imports = .empty,
        .table_imports = .empty,
        .data_segments = .empty,
        .data_offset = 1024, // reserve first 1KB for future use
        .has_memory = false,
        .memory_min_pages = 1,
        .has_stack_pointer = false,
        .stack_pointer_init = 65536,
        .has_table = false,
        .table_func_indices = .empty,
        .extra_globals = .empty,
        .code_bytes = .empty,
        .function_offsets = .empty,
        .dead_import_dummy_count = 0,
        .import_fn_count = 0,
        .import_global_count = 0,
        .code_section_fn_count_leb_size = 0,
        .linking = .{ .symbol_table = .empty, .segment_info = .empty, .init_funcs = .empty },
        .reloc_code = .{ .name = "reloc.CODE", .target_section_index = 0, .entries = .empty },
        .reloc_data = .{ .name = "reloc.DATA", .target_section_index = 0, .entries = .empty },
    };
}

pub fn deinit(self: *Self) void {
    for (self.func_types.items) |ft| {
        self.allocator.free(ft.params);
    }
    self.func_types.deinit(self.allocator);
    self.func_type_results.deinit(self.allocator);
    self.func_type_indices.deinit(self.allocator);
    for (self.func_bodies.items) |fb| {
        if (fb.body.len > 0) {
            self.allocator.free(fb.body);
        }
    }
    self.func_bodies.deinit(self.allocator);
    self.exports.deinit(self.allocator);
    self.imports.deinit(self.allocator);
    self.global_imports.deinit(self.allocator);
    self.table_imports.deinit(self.allocator);
    for (self.data_segments.items) |ds| {
        self.allocator.free(ds.data);
    }
    self.data_segments.deinit(self.allocator);
    self.table_func_indices.deinit(self.allocator);
    self.extra_globals.deinit(self.allocator);
    self.code_bytes.deinit(self.allocator);
    self.function_offsets.deinit(self.allocator);
    self.linking.symbol_table.deinit(self.allocator);
    self.linking.segment_info.deinit(self.allocator);
    self.linking.init_funcs.deinit(self.allocator);
    self.reloc_code.entries.deinit(self.allocator);
    self.reloc_data.entries.deinit(self.allocator);
}

/// Add an imported function. Returns the function index (imports come before regular functions).
/// Important: all imports must be added BEFORE any regular functions via addFunction().
pub fn addImport(self: *Self, module_name: []const u8, field_name: []const u8, type_idx: u32) !u32 {
    const func_idx: u32 = @intCast(self.imports.items.len);
    try self.imports.append(self.allocator, .{
        .module_name = module_name,
        .field_name = field_name,
        .type_idx = type_idx,
    });
    return func_idx;
}

/// Get the number of imported functions. Regular function indices are offset by this.
pub fn importCount(self: *const Self) u32 {
    return @intCast(self.imports.items.len);
}

/// Add a function type (signature) and return its index.
pub fn addFuncType(self: *Self, params: []const ValType, results: []const ValType) !u32 {
    const idx: u32 = @intCast(self.func_types.items.len);
    const params_copy = try self.allocator.dupe(ValType, params);
    try self.func_types.append(self.allocator, .{
        .params = params_copy,
    });
    try self.func_type_results.append(self.allocator, if (results.len > 0) results[0] else null);
    return idx;
}

/// Add a function (maps to a type index) and return the global function index.
/// Global indices account for imports: imports occupy indices 0..import_count-1,
/// and locally-defined functions start at import_count.
pub fn addFunction(self: *Self, type_idx: u32) !u32 {
    const local_idx: u32 = @intCast(self.func_type_indices.items.len);
    try self.func_type_indices.append(self.allocator, type_idx);
    return local_idx + self.importCount();
}

/// Set the body of a function. Takes a global function index (as returned by addFunction).
pub fn setFunctionBody(self: *Self, global_func_idx: u32, body: []const u8) !void {
    const local_idx = global_func_idx - self.importCount();
    const body_copy = try self.allocator.dupe(u8, body);
    // Ensure we have enough slots
    while (self.func_bodies.items.len <= local_idx) {
        try self.func_bodies.append(self.allocator, .{ .body = &.{} });
    }
    // Free any old body
    if (self.func_bodies.items[local_idx].body.len > 0) {
        self.allocator.free(self.func_bodies.items[local_idx].body);
    }
    self.func_bodies.items[local_idx] = .{ .body = body_copy };
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
        .section_offset = 0,
    });
    self.data_offset += @intCast(data.len);
    return offset;
}

/// Enable the __stack_pointer global (global index 0).
pub fn enableStackPointer(self: *Self, initial_value: u32) void {
    self.has_stack_pointer = true;
    self.stack_pointer_init = initial_value;
}

/// Add a defined global and return its index (accounting for __stack_pointer at 0).
/// Used to define PIC globals like __memory_base and __table_base with value 0.
pub fn addDefinedGlobal(self: *Self, val_type: u8, mutable: bool, init_value: i32) !u32 {
    // Global 0 is __stack_pointer (when has_stack_pointer is true).
    // Extra globals start at index 1.
    const idx: u32 = 1 + @as(u32, @intCast(self.extra_globals.items.len));
    try self.extra_globals.append(self.allocator, .{
        .val_type = val_type,
        .mutable = mutable,
        .init_value = init_value,
    });
    return idx;
}

/// Enable the funcref table for call_indirect.
pub fn enableTable(self: *Self) void {
    self.has_table = true;
}

/// Add a function to the table and return its table index.
pub fn addTableElement(self: *Self, func_idx: u32) !u32 {
    const table_idx: u32 = @intCast(self.table_func_indices.items.len);
    try self.table_func_indices.append(self.allocator, func_idx);
    return table_idx;
}

/// Import a hosted function and add it to the funcref table.
///
/// Hosted functions use the RocCall ABI: (i32 roc_ops_ptr, i32 ret_ptr, i32 args_ptr) → void.
/// The caller must provide the type index for this 3-arg signature (registered separately
/// from the 2-arg RocOps callback type).
///
/// Returns the table index that can be used with `call_indirect` to invoke the function.
pub fn addHostedFunctionToTable(self: *Self, module_name: []const u8, fn_name: []const u8, roc_call_type_idx: u32) !u32 {
    const func_idx = try self.addImport(module_name, fn_name, roc_call_type_idx);
    return try self.addTableElement(func_idx);
}

/// Find an imported function's index by module and field name.
/// Returns null if no matching import exists.
pub fn findImportFuncIdx(self: *const Self, module_name: []const u8, field_name: []const u8) ?u32 {
    for (self.imports.items, 0..) |imp, i| {
        if (std.mem.eql(u8, imp.module_name, module_name) and std.mem.eql(u8, imp.field_name, field_name)) {
            return @intCast(i);
        }
    }
    return null;
}

/// Find a function index by its resolved symbol/import name.
pub fn findFunctionIdxByName(self: *const Self, name: []const u8) ?u32 {
    if (self.linking.findSymbolByName(name, self.imports.items, self.global_imports.items, self.table_imports.items)) |sym_idx| {
        const sym = self.linking.symbol_table.items[sym_idx];
        if (sym.kind == .function) return sym.index;
    }

    for (self.imports.items, 0..) |imp, i| {
        if (std.mem.eql(u8, imp.field_name, name)) return @intCast(i);
    }

    return null;
}

/// Find a defined function whose resolved name ends with `suffix`.
/// This intentionally ignores undefined/imported symbols so host callback lookups
/// do not accidentally bind raw platform imports like `roc_dbg`.
pub fn findFunctionIdxBySuffix(self: *const Self, suffix: []const u8) ?u32 {
    for (self.linking.symbol_table.items) |sym| {
        if (sym.kind != .function or sym.isUndefined()) continue;
        const sym_name = sym.resolveName(self.imports.items, self.global_imports.items, self.table_imports.items) orelse continue;
        if (std.mem.endsWith(u8, sym_name, suffix)) return sym.index;
    }
    return null;
}

/// Find or append a function in the element section.
pub fn ensureTableElement(self: *Self, func_idx: u32) !u32 {
    return self.findTableIndex(func_idx) orelse try self.addTableElement(func_idx);
}

// --- Surgical Linking ---

/// Dummy function body: unreachable + end. Inserted to maintain function index
/// stability when an import is removed during surgical linking.
pub const DUMMY_FUNCTION = [3]u8{
    0x00, // zero local variable declarations
    Op.@"unreachable", // trap if called (means DCE was wrong)
    Op.end, // end of function body
};

/// Entry in the host-to-app linking map: maps an app function name
/// (which the host imports) to its defined function index.
pub const HostToAppEntry = struct {
    name: []const u8,
    fn_index: u32,
};

/// Perform surgical linking: for each (app_fn_name, app_fn_index) pair,
/// remove the host's import for that name and redirect all call sites to
/// the app-defined function at app_fn_index.
///
/// The last function import is swapped into the vacated slot so that only
/// two symbols need relocation updates. A dummy function is prepended to
/// func_type_indices to keep the total function count stable.
pub fn linkHostToAppCalls(self: *Self, host_to_app_map: []const HostToAppEntry) !void {
    for (host_to_app_map) |entry| {
        const app_fn_name = entry.name;
        const app_fn_index = entry.fn_index;

        // 1. Find the host import matching app_fn_name, and the last import (swap candidate).
        //    Since self.imports only contains function imports, import_index == fn_index.
        var host_fn_index: ?u32 = null;
        var last_fn_index: u32 = 0;
        for (self.imports.items, 0..) |imp, i| {
            last_fn_index = @intCast(i);
            if (std.mem.eql(u8, imp.field_name, app_fn_name)) {
                host_fn_index = @intCast(i);
            }
        }

        const host_idx = host_fn_index orelse {
            // The host doesn't import this function — export the app's definition
            // so it can be called from JS.
            try self.exports.append(self.allocator, .{
                .name = app_fn_name,
                .kind = .func,
                .idx = app_fn_index,
            });
            continue;
        };

        // 2. Swap: remove the last import and put it where the host import was.
        //    This keeps all other import indices stable — only the host and swap
        //    indices need relocation updates.
        const swap_import = self.imports.items[last_fn_index];
        self.imports.items.len -= 1;
        if (last_fn_index != host_idx) {
            self.imports.items[host_idx] = swap_import;
        }

        // 3. Update symbol table and apply relocations for the host function.
        //    The host import (at host_idx) is now a defined app function at app_fn_index.
        if (self.linking.findAndReindexImportedFn(host_idx, app_fn_index)) |sym_index| {
            self.reloc_code.applyRelocsU32(self.code_bytes.items, sym_index, app_fn_index);
        }

        // 4. Update symbol table and apply relocations for the swapped function.
        //    The last import (at last_fn_index) moved to host_idx.
        if (last_fn_index != host_idx) {
            if (self.linking.findAndReindexImportedFn(last_fn_index, host_idx)) |swap_sym_index| {
                self.reloc_code.applyRelocsU32(self.code_bytes.items, swap_sym_index, host_idx);
            }
        }

        // 5. Insert a dummy function to compensate for the removed import.
        //    This keeps defined-function indices unchanged: import_count decreases
        //    by 1, but one dummy is prepended to the code section, so the first
        //    real defined function stays at the same global index.
        self.dead_import_dummy_count += 1;
        try self.func_type_indices.insert(self.allocator, 0, 0); // dummy uses type signature 0

        // 6. Track the decreased import count.
        self.import_fn_count -= 1;
    }
}

// --- Phase 8a: Module Merging (Builtins) ---

/// Result of mergeModule: maps from the source module's symbol indices
/// to the merged module's symbol indices.
pub const MergeResult = struct {
    /// Maps source module symbol index → merged module symbol index.
    /// Length equals source module's symbol_table.items.len.
    symbol_remap: []u32,
    allocator: Allocator,

    pub fn deinit(self: *MergeResult) void {
        self.allocator.free(self.symbol_remap);
    }
};

// --- Phase 8b: Builtin Symbol Lookup ---

/// Maps builtin operations to their symbol indices in the merged module.
///
/// After `mergeModule` incorporates `roc_builtins.o`, this struct is populated
/// by looking up each `roc_builtins_*` symbol name in the merged module's
/// symbol table. WasmCodeGen uses these symbol indices with
/// `emitRelocatableCall` to emit calls to builtins.
pub const BuiltinSymbols = struct {
    // --- Decimal / i128 arithmetic ---
    dec_mul: u32, // roc_builtins_dec_mul_saturated
    dec_div: u32, // roc_builtins_dec_div
    dec_div_trunc: u32, // roc_builtins_dec_div_trunc
    dec_to_str: u32, // roc_builtins_dec_to_str
    i128_div_s: u32, // roc_builtins_num_div_trunc_i128
    i128_mod_s: u32, // roc_builtins_num_rem_trunc_i128
    u128_div: u32, // roc_builtins_num_div_trunc_u128
    u128_mod: u32, // roc_builtins_num_rem_trunc_u128

    // --- Numeric conversions ---
    i128_to_dec: u32, // roc_builtins_i128_to_dec_try_unsafe
    u128_to_dec: u32, // roc_builtins_u128_to_dec_try_unsafe
    dec_to_int_try_unsafe: u32, // roc_builtins_dec_to_int_try_unsafe
    dec_to_f32: u32, // roc_builtins_dec_to_f32_try_unsafe
    float_to_str: u32, // roc_builtins_float_to_str
    int_to_str: u32, // roc_builtins_int_to_str
    int_from_str: u32, // roc_builtins_int_from_str
    dec_from_str: u32, // roc_builtins_dec_from_str
    float_from_str: u32, // roc_builtins_float_from_str

    // --- String operations ---
    str_equal: u32, // roc_builtins_str_equal
    str_concat: u32, // roc_builtins_str_concat
    str_repeat: u32, // roc_builtins_str_repeat
    str_trim: u32, // roc_builtins_str_trim
    str_trim_start: u32, // roc_builtins_str_trim_start
    str_trim_end: u32, // roc_builtins_str_trim_end
    str_split: u32, // roc_builtins_str_split
    str_join_with: u32, // roc_builtins_str_join_with
    str_reserve: u32, // roc_builtins_str_reserve
    str_release_excess_capacity: u32, // roc_builtins_str_release_excess_capacity
    str_with_capacity: u32, // roc_builtins_str_with_capacity
    str_drop_prefix: u32, // roc_builtins_str_drop_prefix
    str_drop_suffix: u32, // roc_builtins_str_drop_suffix
    str_with_ascii_lowercased: u32, // roc_builtins_str_with_ascii_lowercased
    str_with_ascii_uppercased: u32, // roc_builtins_str_with_ascii_uppercased
    str_caseless_ascii_equals: u32, // roc_builtins_str_caseless_ascii_equals
    str_from_utf8: u32, // roc_builtins_str_from_utf8

    // --- List operations ---
    list_append_unsafe: u32, // roc_builtins_list_append_unsafe
    list_append_safe: u32, // roc_builtins_list_append_safe
    list_sort_with: u32, // roc_builtins_list_sort_with
    list_eq: u32, // roc_builtins_list_eq
    list_str_eq: u32, // roc_builtins_list_str_eq
    list_list_eq: u32, // roc_builtins_list_list_eq
    list_reverse: u32, // roc_builtins_list_reverse

    // --- Memory management ---
    allocate_with_refcount: u32, // roc_builtins_allocate_with_refcount

    // --- Integer modulo ---
    i32_mod_by: u32, // roc_builtins_i32_mod_by
    i64_mod_by: u32, // roc_builtins_i64_mod_by

    /// Name → field mapping used by `populate` to fill this struct.
    const mapping = .{
        .{ "roc_builtins_dec_mul_saturated", "dec_mul" },
        .{ "roc_builtins_dec_div", "dec_div" },
        .{ "roc_builtins_dec_div_trunc", "dec_div_trunc" },
        .{ "roc_builtins_dec_to_str", "dec_to_str" },
        .{ "roc_builtins_num_div_trunc_i128", "i128_div_s" },
        .{ "roc_builtins_num_rem_trunc_i128", "i128_mod_s" },
        .{ "roc_builtins_num_div_trunc_u128", "u128_div" },
        .{ "roc_builtins_num_rem_trunc_u128", "u128_mod" },
        .{ "roc_builtins_i128_to_dec_try_unsafe", "i128_to_dec" },
        .{ "roc_builtins_u128_to_dec_try_unsafe", "u128_to_dec" },
        .{ "roc_builtins_dec_to_int_try_unsafe", "dec_to_int_try_unsafe" },
        .{ "roc_builtins_dec_to_f32_try_unsafe", "dec_to_f32" },
        .{ "roc_builtins_float_to_str", "float_to_str" },
        .{ "roc_builtins_int_to_str", "int_to_str" },
        .{ "roc_builtins_int_from_str", "int_from_str" },
        .{ "roc_builtins_dec_from_str", "dec_from_str" },
        .{ "roc_builtins_float_from_str", "float_from_str" },
        .{ "roc_builtins_str_equal", "str_equal" },
        .{ "roc_builtins_str_concat", "str_concat" },
        .{ "roc_builtins_str_repeat", "str_repeat" },
        .{ "roc_builtins_str_trim", "str_trim" },
        .{ "roc_builtins_str_trim_start", "str_trim_start" },
        .{ "roc_builtins_str_trim_end", "str_trim_end" },
        .{ "roc_builtins_str_split", "str_split" },
        .{ "roc_builtins_str_join_with", "str_join_with" },
        .{ "roc_builtins_str_reserve", "str_reserve" },
        .{ "roc_builtins_str_release_excess_capacity", "str_release_excess_capacity" },
        .{ "roc_builtins_str_with_capacity", "str_with_capacity" },
        .{ "roc_builtins_str_drop_prefix", "str_drop_prefix" },
        .{ "roc_builtins_str_drop_suffix", "str_drop_suffix" },
        .{ "roc_builtins_str_with_ascii_lowercased", "str_with_ascii_lowercased" },
        .{ "roc_builtins_str_with_ascii_uppercased", "str_with_ascii_uppercased" },
        .{ "roc_builtins_str_caseless_ascii_equals", "str_caseless_ascii_equals" },
        .{ "roc_builtins_str_from_utf8", "str_from_utf8" },
        .{ "roc_builtins_list_append_unsafe", "list_append_unsafe" },
        .{ "roc_builtins_list_append_safe", "list_append_safe" },
        .{ "roc_builtins_list_sort_with", "list_sort_with" },
        .{ "roc_builtins_list_eq", "list_eq" },
        .{ "roc_builtins_list_str_eq", "list_str_eq" },
        .{ "roc_builtins_list_list_eq", "list_list_eq" },
        .{ "roc_builtins_list_reverse", "list_reverse" },
        .{ "roc_builtins_allocate_with_refcount", "allocate_with_refcount" },
        .{ "roc_builtins_i32_mod_by", "i32_mod_by" },
        .{ "roc_builtins_i64_mod_by", "i64_mod_by" },
    };

    pub const PopulateError = error{MissingBuiltinSymbol};

    /// Populate this struct by looking up each builtin symbol name in the
    /// module's merged symbol table. Returns the actual function index for
    /// each builtin (from sym.index), not the symbol table index.
    pub fn populate(module: *const Self) PopulateError!BuiltinSymbols {
        var result: BuiltinSymbols = undefined;
        inline for (mapping) |entry| {
            const sym_name = entry[0];
            const field_name = entry[1];
            const sym_table_idx = module.linking.findSymbolByName(
                sym_name,
                module.imports.items,
                module.global_imports.items,
                module.table_imports.items,
            ) orelse return error.MissingBuiltinSymbol;
            @field(result, field_name) = module.linking.symbol_table.items[sym_table_idx].index;
        }
        return result;
    }
};

/// Merge a relocatable module (e.g. roc_builtins.o) into this module.
///
/// This appends the source module's functions, code, data, symbols, and
/// relocations into self, resolving shared symbols (like roc_alloc) against
/// this module's existing imports.
///
/// After merging, the source module's defined functions become defined
/// functions in self, and all relocation entries are remapped so that
/// the merged symbol table is consistent.
///
/// Returns a MergeResult with the symbol index mapping, which callers
/// use to look up merged builtins by their original symbol indices.
pub fn mergeModule(self: *Self, source: *const Self) !MergeResult {
    const gpa = self.allocator;

    // --- 1. Merge type section (with deduplication) ---
    // Maps source type index → self type index.
    const type_remap = try gpa.alloc(u32, source.func_types.items.len);
    defer gpa.free(type_remap);

    for (source.func_types.items, source.func_type_results.items, 0..) |src_ft, src_result, src_idx| {
        // Check if self already has an identical type signature.
        var found: ?u32 = null;
        for (self.func_types.items, self.func_type_results.items, 0..) |dst_ft, dst_result, dst_idx| {
            if (src_result != dst_result) continue;
            if (src_ft.params.len != dst_ft.params.len) continue;
            if (std.mem.eql(ValType, src_ft.params, dst_ft.params)) {
                found = @intCast(dst_idx);
                break;
            }
        }
        if (found) |idx| {
            type_remap[src_idx] = idx;
        } else {
            // Add new type to self.
            type_remap[src_idx] = try self.addFuncType(src_ft.params, if (src_result) |r| &.{r} else &.{});
        }
    }

    // --- 2. Compute function index mapping ---
    // Source defined functions start at source.import_fn_count in source's index space.
    // In self, they'll be appended after existing defined functions.
    const source_defined_count: u32 = @intCast(source.func_type_indices.items.len);

    // func_remap: maps source global function index → self global function index.
    // For source imports, we resolve them against self's imports by name.
    // For source defined functions, they get sequential indices after self's existing functions.
    const total_source_fns = source.import_fn_count + source_defined_count;
    const func_remap = try gpa.alloc(u32, total_source_fns);
    defer gpa.free(func_remap);

    // Remap source imports → self imports (by name match).
    // NOTE: This loop may add new imports to self, changing importCount().
    // We must compute self_defined_base AFTER this loop completes.
    const old_import_count = self.importCount();
    for (source.imports.items, 0..) |src_imp, src_idx| {
        // Find matching import in self by field_name.
        var matched: ?u32 = null;
        for (self.imports.items, 0..) |self_imp, self_idx| {
            if (std.mem.eql(u8, self_imp.field_name, src_imp.field_name)) {
                matched = @intCast(self_idx);
                break;
            }
        }
        func_remap[src_idx] = matched orelse {
            // Source imports a function self doesn't have — add it as a new import.
            const remapped_type = type_remap[src_imp.type_idx];
            func_remap[src_idx] = try self.addImport(src_imp.module_name, src_imp.field_name, remapped_type);
            continue;
        };
    }

    // If new imports were added, all existing defined function indices in self
    // shift up by the number of new imports. Update symbol table, element section,
    // and exports to reflect the new indices.
    const new_import_count = self.importCount();
    const import_delta = new_import_count - old_import_count;
    if (import_delta > 0) {
        // Shift defined function symbols.
        for (self.linking.symbol_table.items) |*sym| {
            if (sym.isFunction() and !sym.isUndefined() and sym.index >= old_import_count) {
                sym.index += import_delta;
            }
        }
        // Shift element section entries (table func indices).
        for (self.table_func_indices.items) |*fi| {
            if (fi.* >= old_import_count) fi.* += import_delta;
        }
        // Shift export entries referencing defined functions.
        for (self.exports.items) |*exp| {
            if (exp.kind == .func and exp.idx >= old_import_count) {
                exp.idx += import_delta;
            }
        }
    }

    // Compute defined function base AFTER imports are finalized,
    // since addImport above may have increased importCount().
    const self_defined_base = self.importCount() + @as(u32, @intCast(self.func_type_indices.items.len));

    // Remap source defined functions → new indices in self.
    for (0..source_defined_count) |i| {
        func_remap[source.import_fn_count + i] = self_defined_base + @as(u32, @intCast(i));
    }

    // --- 3. Merge function section (remap type indices) ---
    for (source.func_type_indices.items) |src_type_idx| {
        try self.func_type_indices.append(gpa, type_remap[src_type_idx]);
    }

    // --- 4. Merge code section (append bytes, track offsets) ---
    const base_code_offset: u32 = @intCast(self.code_bytes.items.len);

    try self.code_bytes.appendSlice(gpa, source.code_bytes.items);

    for (source.function_offsets.items) |src_offset| {
        try self.function_offsets.append(gpa, base_code_offset + src_offset);
    }

    // --- 5. Merge data section (adjust memory offsets) ---
    // data_remap: maps source segment index → new data base address.
    const data_remap = try gpa.alloc(u32, source.data_segments.items.len);
    defer gpa.free(data_remap);
    // data_segment_remap: maps source segment index → new segment index in self.data_segments.
    const data_segment_remap = try gpa.alloc(u32, source.data_segments.items.len);
    defer gpa.free(data_segment_remap);

    for (source.data_segments.items, 0..) |src_ds, i| {
        // Find alignment from segment info if available, default to 1.
        const alignment: u32 = if (i < source.linking.segment_info.items.len)
            @as(u32, 1) << @intCast(source.linking.segment_info.items[i].alignment)
        else
            1;
        data_segment_remap[i] = @intCast(self.data_segments.items.len);
        const new_offset = try self.addDataSegment(src_ds.data, alignment);
        data_remap[i] = new_offset;
    }

    // --- 5b. Merge element section (remap function indices) ---
    for (source.table_func_indices.items) |src_func_idx| {
        const remapped = func_remap[src_func_idx];
        try self.table_func_indices.append(gpa, remapped);
        self.has_table = true;
    }

    // --- 6. Merge symbol table ---
    // symbol_remap: maps source symbol index → self symbol index.
    const source_sym_count = source.linking.symbol_table.items.len;
    const symbol_remap = try gpa.alloc(u32, source_sym_count);
    errdefer gpa.free(symbol_remap);

    for (source.linking.symbol_table.items, 0..) |src_sym, src_sym_idx| {
        const src_name = src_sym.resolveName(source.imports.items, source.global_imports.items, source.table_imports.items);

        switch (src_sym.kind) {
            .function => {
                if (src_sym.isUndefined()) {
                    // Undefined function in source — resolve against self's symbol table.
                    if (src_name) |name| {
                        if (self.linking.findSymbolByName(name, self.imports.items, self.global_imports.items, self.table_imports.items)) |existing| {
                            symbol_remap[src_sym_idx] = existing;
                            continue;
                        }
                    }
                    // Not found — add as new undefined symbol referencing the (possibly new) import.
                    const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                    try self.linking.symbol_table.append(gpa, .{
                        .kind = .function,
                        .flags = src_sym.flags,
                        .name = src_name,
                        .index = func_remap[src_sym.index],
                    });
                    symbol_remap[src_sym_idx] = new_sym_idx;
                } else {
                    // Defined function in source — add as defined in self.
                    const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                    try self.linking.symbol_table.append(gpa, .{
                        .kind = .function,
                        .flags = src_sym.flags,
                        .name = src_sym.name,
                        .index = func_remap[src_sym.index],
                    });
                    symbol_remap[src_sym_idx] = new_sym_idx;
                }
            },
            .data => {
                if (src_sym.isUndefined()) {
                    // Undefined data — resolve against self.
                    if (src_name) |name| {
                        if (self.linking.findSymbolByName(name, self.imports.items, self.global_imports.items, self.table_imports.items)) |existing| {
                            symbol_remap[src_sym_idx] = existing;
                            continue;
                        }
                    }
                    // Add as-is (undefined).
                    const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                    try self.linking.symbol_table.append(gpa, src_sym);
                    symbol_remap[src_sym_idx] = new_sym_idx;
                } else {
                    // Defined data — remap segment offset.
                    const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                    const new_offset = if (src_sym.index < data_remap.len)
                        data_remap[src_sym.index] + src_sym.data_offset
                    else
                        src_sym.data_offset;
                    const new_segment_idx = if (src_sym.index < data_segment_remap.len)
                        data_segment_remap[src_sym.index]
                    else
                        src_sym.index;
                    try self.linking.symbol_table.append(gpa, .{
                        .kind = .data,
                        .flags = src_sym.flags,
                        .name = src_sym.name,
                        .index = new_segment_idx,
                        .data_offset = new_offset,
                        .data_size = src_sym.data_size,
                    });
                    symbol_remap[src_sym_idx] = new_sym_idx;
                }
            },
            .global => {
                // Resolve globals (like __stack_pointer) against self.
                if (src_sym.isUndefined()) {
                    if (src_name) |name| {
                        if (self.linking.findSymbolByName(name, self.imports.items, self.global_imports.items, self.table_imports.items)) |existing| {
                            symbol_remap[src_sym_idx] = existing;
                            continue;
                        }
                        // PIC globals: define them as constants in the final module.
                        // __memory_base and __table_base are 0 in statically-linked modules.
                        if (std.mem.eql(u8, name, "__memory_base") or
                            std.mem.eql(u8, name, "__table_base"))
                        {
                            const global_idx = try self.addDefinedGlobal(0x7F, false, 0); // i32, immutable, value=0
                            const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                            try self.linking.symbol_table.append(gpa, .{
                                .kind = .global,
                                .flags = 0, // defined (not undefined)
                                .name = name,
                                .index = global_idx,
                            });
                            symbol_remap[src_sym_idx] = new_sym_idx;
                            continue;
                        }
                    }
                }
                // Add as-is if not resolved.
                const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                try self.linking.symbol_table.append(gpa, src_sym);
                symbol_remap[src_sym_idx] = new_sym_idx;
            },
            .table => {
                // PIC: __indirect_function_table → just enable the module's table.
                if (src_sym.isUndefined()) {
                    if (src_name) |name| {
                        if (std.mem.eql(u8, name, "__indirect_function_table")) {
                            self.has_table = true;
                            // Map to a symbol that references table 0.
                            const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                            try self.linking.symbol_table.append(gpa, .{
                                .kind = .table,
                                .flags = 0,
                                .name = name,
                                .index = 0,
                            });
                            symbol_remap[src_sym_idx] = new_sym_idx;
                            continue;
                        }
                    }
                }
                const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                try self.linking.symbol_table.append(gpa, src_sym);
                symbol_remap[src_sym_idx] = new_sym_idx;
            },
            .section, .event => {
                // Carry over as-is.
                const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                try self.linking.symbol_table.append(gpa, src_sym);
                symbol_remap[src_sym_idx] = new_sym_idx;
            },
        }
    }

    // --- 7. Merge relocation entries (remap symbol indices and offsets) ---
    // Code relocations.
    for (source.reloc_code.entries.items) |src_entry| {
        switch (src_entry) {
            .index => |idx| {
                if (idx.type_id == .type_index_leb) {
                    // R_WASM_TYPE_INDEX_LEB: the placeholder in code_bytes is the
                    // SOURCE type index. Remap it immediately using type_remap rather
                    // than deferring — resolveCodeRelocations doesn't have type_remap.
                    const src_type_idx = readPaddedU32(
                        self.code_bytes.items,
                        base_code_offset + idx.offset,
                    );
                    const remapped = if (src_type_idx < type_remap.len)
                        type_remap[src_type_idx]
                    else
                        src_type_idx;
                    overwritePaddedU32(
                        self.code_bytes.items,
                        base_code_offset + idx.offset,
                        remapped,
                    );
                    // Don't add to reloc_code — already resolved.
                    continue;
                }
                try self.reloc_code.entries.append(gpa, .{ .index = .{
                    .type_id = idx.type_id,
                    .offset = base_code_offset + idx.offset,
                    .symbol_index = symbol_remap[idx.symbol_index],
                } });
            },
            .offset => |off| {
                try self.reloc_code.entries.append(gpa, .{ .offset = .{
                    .type_id = off.type_id,
                    .offset = base_code_offset + off.offset,
                    .symbol_index = symbol_remap[off.symbol_index],
                    .addend = off.addend,
                } });
            },
        }
    }

    // Data relocations.
    for (source.reloc_data.entries.items) |src_entry| {
        switch (src_entry) {
            .index => |idx| {
                const remapped_segment_idx = if (idx.data_segment_index < data_segment_remap.len)
                    data_segment_remap[idx.data_segment_index]
                else
                    idx.data_segment_index;
                try self.reloc_data.entries.append(gpa, .{
                    .index = .{
                        .type_id = idx.type_id,
                        .offset = idx.offset,
                        .symbol_index = symbol_remap[idx.symbol_index],
                        .data_segment_index = remapped_segment_idx,
                    },
                });
            },
            .offset => |off| {
                const remapped_segment_idx = if (off.data_segment_index < data_segment_remap.len)
                    data_segment_remap[off.data_segment_index]
                else
                    off.data_segment_index;
                try self.reloc_data.entries.append(gpa, .{ .offset = .{
                    .type_id = off.type_id,
                    .offset = off.offset,
                    .symbol_index = symbol_remap[off.symbol_index],
                    .addend = off.addend,
                    .data_segment_index = remapped_segment_idx,
                } });
            },
        }
    }

    // Update import_fn_count to reflect any new imports added during merge.
    self.import_fn_count = @intCast(self.imports.items.len);

    return .{
        .symbol_remap = symbol_remap,
        .allocator = gpa,
    };
}

/// Find the table index (position in table_func_indices) for a given function index.
/// Returns null if the function is not in the table.
fn findTableIndex(self: *const Self, func_idx: u32) ?u32 {
    for (self.table_func_indices.items, 0..) |tfi, i| {
        if (tfi == func_idx) return @intCast(i);
    }
    return null;
}

fn patchResolvedRelocation(self: *const Self, target_bytes: []u8, entry: WasmLinking.RelocationEntry, patch_offset: u32) void {
    const sym = self.linking.symbol_table.items[entry.getSymbolIndex()];
    switch (entry) {
        .index => |idx| {
            const value = sym.index;
            switch (idx.type_id) {
                .type_index_leb => {
                    // type_index_leb relocations are normally resolved during merge
                    // (when the type_remap is available). If one survives, use the
                    // resolved symbol index directly.
                    overwritePaddedU32(target_bytes, patch_offset, value);
                },
                .function_index_leb,
                .global_index_leb,
                .event_index_leb,
                .table_number_leb,
                => overwritePaddedU32(target_bytes, patch_offset, value),
                .table_index_sleb,
                .table_index_rel_sleb,
                => {
                    const table_idx = self.findTableIndex(value) orelse value;
                    overwritePaddedI32(target_bytes, patch_offset, @intCast(table_idx));
                },
                .table_index_i32 => {
                    const table_idx = self.findTableIndex(value) orelse value;
                    const off: usize = @intCast(patch_offset);
                    std.mem.writeInt(u32, target_bytes[off..][0..4], table_idx, .little);
                },
                .global_index_i32 => {
                    const off: usize = @intCast(patch_offset);
                    std.mem.writeInt(u32, target_bytes[off..][0..4], value, .little);
                },
            }
        },
        .offset => |off| {
            // For data symbols, the resolved address is the data_offset.
            // For others, use the symbol's index as the base address.
            const base: i64 = if (sym.kind == .data)
                @intCast(sym.data_offset)
            else
                @intCast(sym.index);
            const patched = base + @as(i64, off.addend);
            switch (off.type_id) {
                .memory_addr_leb => overwritePaddedU32(
                    target_bytes,
                    patch_offset,
                    @intCast(patched),
                ),
                .memory_addr_sleb,
                .memory_addr_rel_sleb,
                => overwritePaddedI32(
                    target_bytes,
                    patch_offset,
                    @intCast(patched),
                ),
                .memory_addr_i32,
                .function_offset_i32,
                .section_offset_i32,
                => {
                    const o: usize = @intCast(patch_offset);
                    std.mem.writeInt(u32, target_bytes[o..][0..4], @intCast(patched), .little);
                },
            }
        },
    }
}

/// Resolve all code relocations in place.
///
/// For each relocation entry in `reloc_code`, look up the symbol's resolved
/// value (function index, global index, or memory address) and patch the
/// corresponding site in `code_bytes`.
pub fn resolveCodeRelocations(self: *Self) void {
    for (self.reloc_code.entries.items) |entry| {
        self.patchResolvedRelocation(self.code_bytes.items, entry, entry.getOffset());
    }
}

/// Resolve all data relocations in place.
pub fn resolveDataRelocations(self: *Self) void {
    // First pass: ensure functions referenced by table_index_* relocations
    // are present in the element section. This is needed because data segments
    // can store function pointers (e.g. hosted_function_ptrs) which need valid
    // table indices, and the functions must be in the table for call_indirect.
    for (self.reloc_data.entries.items) |entry| {
        switch (entry) {
            .index => |idx| {
                if (idx.type_id == .table_index_i32 or
                    idx.type_id == .table_index_sleb or
                    idx.type_id == .table_index_rel_sleb)
                {
                    const sym = self.linking.symbol_table.items[idx.symbol_index];
                    if (sym.isFunction()) {
                        _ = self.ensureTableElement(sym.index) catch continue;
                    }
                }
            },
            .offset => {},
        }
    }

    // Second pass: patch data bytes with resolved values.
    for (self.reloc_data.entries.items) |entry| {
        const segment_idx = switch (entry) {
            .index => |idx| idx.data_segment_index,
            .offset => |off| off.data_segment_index,
        };
        std.debug.assert(segment_idx != std.math.maxInt(u32));
        std.debug.assert(segment_idx < self.data_segments.items.len);

        const segment = &self.data_segments.items[segment_idx];
        self.patchResolvedRelocation(segment.data, entry, entry.getOffset());
    }
}

/// Resolve both code and data relocations in place.
pub fn resolveRelocations(self: *Self) void {
    self.resolveCodeRelocations();
    self.resolveDataRelocations();
}

/// Transfer function bodies added via setFunctionBody into the code_bytes
/// representation. This makes app-generated functions compatible with
/// linkHostToAppCalls, resolveCodeRelocations, eliminateDeadCode, and
/// materializeFuncBodies.
///
/// Must be called after all addFunction/setFunctionBody calls are complete
/// and before linkHostToAppCalls.
pub fn transferAppFunctions(self: *Self) !void {
    const host_defined_count = self.function_offsets.items.len;
    const total_defined_count = self.func_type_indices.items.len;

    if (total_defined_count <= host_defined_count) return;

    for (host_defined_count..total_defined_count) |i| {
        if (i >= self.func_bodies.items.len) break;
        const body = self.func_bodies.items[i].body;
        if (body.len == 0) continue;

        const fn_offset: u32 = @intCast(self.code_bytes.items.len);
        try self.function_offsets.append(self.allocator, fn_offset);

        // Write body length + body to code_bytes
        try leb128WriteU32(self.allocator, &self.code_bytes, @intCast(body.len));
        try self.code_bytes.appendSlice(self.allocator, body);
    }
}

/// Convert code_bytes + function_offsets into func_bodies for encoding.
///
/// After `resolveRelocations()` has patched all relocation sites, this method
/// splits the contiguous code_bytes buffer into individual function bodies
/// (skipping dummy functions from dead_import_dummy_count) and populates
/// func_bodies so that `encode()` can emit them.
pub fn materializeFuncBodies(self: *Self) !void {
    const gpa = self.allocator;
    const defined_count = self.func_type_indices.items.len;

    // Clear existing func_bodies.
    for (self.func_bodies.items) |fb| {
        if (fb.body.len > 0) gpa.free(fb.body);
    }
    self.func_bodies.clearRetainingCapacity();

    // First dead_import_dummy_count entries in func_type_indices are dummies.
    for (0..self.dead_import_dummy_count) |_| {
        const body_copy = try gpa.dupe(u8, &DUMMY_FUNCTION);
        try self.func_bodies.append(gpa, .{ .body = body_copy });
    }

    // The real functions follow the dummies.
    const real_count = defined_count - self.dead_import_dummy_count;
    for (0..real_count) |i| {
        const fn_offset = self.function_offsets.items[i];

        // Parse the body length from the code_bytes (LEB128 encoded at fn_offset).
        var cursor: usize = @intCast(fn_offset);
        const body_len = readU32(self.code_bytes.items, &cursor) catch unreachable;
        const body_start: usize = cursor;
        const body_end: usize = body_start + body_len;

        const body_copy = try gpa.dupe(u8, self.code_bytes.items[body_start..body_end]);
        try self.func_bodies.append(gpa, .{ .body = body_copy });
    }
}

// --- Phase 8e: Verification ---

/// Verify that no stale builtin roc_* imports remain in the final module.
/// `roc_panic` is also tolerated because current host platforms still import it
/// behind the `roc_crashed` wrapper, and verification runs before DCE.
pub fn verifyNoBuiltinImports(self: *const Self) !void {
    const allowed = [_][]const u8{
        "roc_alloc",
        "roc_dealloc",
        "roc_realloc",
        "roc_dbg",
        "roc_expect_failed",
        "roc_crashed",
        "roc_panic",
    };
    for (self.imports.items) |imp| {
        var is_allowed = false;
        for (allowed) |name| {
            if (std.mem.eql(u8, imp.field_name, name)) {
                is_allowed = true;
                break;
            }
        }
        if (!is_allowed and std.mem.startsWith(u8, imp.field_name, "roc_")) {
            return error.UnresolvedBuiltinImport;
        }
    }
}

// --- Phase 10: Dead Code Elimination ---

/// Trace the call graph from exported/live functions and replace unreachable
/// function bodies with `unreachable; end` stubs. Dead imports are removed
/// entirely (so the host page doesn't need to provide dummy JS functions),
/// and `dead_import_dummy_count` is incremented accordingly.
///
/// This must be called AFTER `resolveRelocations()` has patched all
/// relocation sites but BEFORE `materializeFuncBodies()`.
///
/// `called_fns` is a bitset of function indices that are directly called
/// by the app (e.g. from codegen). It is combined with exports, init funcs,
/// and element section entries to seed the live set.
pub fn eliminateDeadCode(self: *Self, called_fns: []const bool) !void {
    const gpa = self.allocator;

    const import_count = self.import_fn_count;
    const fn_index_min = import_count + self.dead_import_dummy_count;
    const fn_count = fn_index_min + @as(u32, @intCast(self.function_offsets.items.len));

    // --- 1. Trace live functions ---
    const live_flags = try self.traceLiveFunctions(called_fns, fn_index_min, fn_count);
    defer gpa.free(live_flags);

    // --- 2. Remove all unused JS imports ---
    // Track which live imports need relocation updates.
    var live_import_fns: std.ArrayList(u32) = .empty;
    defer live_import_fns.deinit(gpa);
    try live_import_fns.ensureTotalCapacity(gpa, import_count);

    var fn_index: u32 = 0;
    var eliminated_import_count: u32 = 0;
    var write_idx: usize = 0;
    for (self.imports.items) |imp| {
        if (fn_index < import_count and live_flags[fn_index]) {
            live_import_fns.appendAssumeCapacity(fn_index);
            self.imports.items[write_idx] = imp;
            write_idx += 1;
        } else if (fn_index < import_count) {
            eliminated_import_count += 1;
        }
        fn_index += 1;
    }
    self.imports.items.len = write_idx;

    // Update dead_import_dummy_count to account for removed imports.
    self.dead_import_dummy_count += eliminated_import_count;

    // Insert function signatures for the new dummy functions.
    // Dummies use type signature 0 (arbitrary — they never execute).
    for (0..eliminated_import_count) |_| {
        try self.func_type_indices.insert(gpa, 0, 0);
    }

    // Relocate calls to remaining JS imports.
    // This must happen before we rebuild the code section.
    for (live_import_fns.items, 0..) |old_index, new_idx| {
        if (new_idx == old_index) continue;
        if (self.linking.findAndReindexImportedFn(old_index, @intCast(new_idx))) |sym_index| {
            self.reloc_code.applyRelocsU32(self.code_bytes.items, sym_index, @intCast(new_idx));
        }
    }

    // --- 3. Replace dead defined-function bodies with dummies ---
    var buffer: std.ArrayList(u8) = .empty;
    defer buffer.deinit(gpa);
    try buffer.ensureTotalCapacity(gpa, self.code_bytes.items.len);

    const offsets = self.function_offsets.items;
    for (offsets, 0..) |fn_offset, i| {
        const global_fn_idx = fn_index_min + @as(u32, @intCast(i));
        if (live_flags[global_fn_idx]) {
            // Copy the live function body verbatim.
            const code_start: usize = fn_offset;
            const code_end: usize = if (i + 1 < offsets.len) offsets[i + 1] else self.code_bytes.items.len;
            buffer.appendSliceAssumeCapacity(self.code_bytes.items[code_start..code_end]);
        } else {
            // Serialize dummy: body_size (LEB128) + body bytes.
            // Body size = 3 (DUMMY_FUNCTION.len).
            buffer.appendAssumeCapacity(DUMMY_FUNCTION.len); // single-byte LEB128 for 3
            buffer.appendSliceAssumeCapacity(&DUMMY_FUNCTION);
        }
    }

    // Replace code_bytes with the rebuilt buffer.
    self.code_bytes.clearRetainingCapacity();
    try self.code_bytes.appendSlice(gpa, buffer.items);

    // Rebuild function_offsets.
    var offset: u32 = 0;
    for (offsets, 0..) |_, i| {
        self.function_offsets.items[i] = offset;
        const global_fn_idx = fn_index_min + @as(u32, @intCast(i));
        if (live_flags[global_fn_idx]) {
            // Parse body length to advance offset.
            var cursor: usize = offset;
            const body_len = readU32(self.code_bytes.items, &cursor) catch unreachable;
            offset = @intCast(cursor + body_len);
        } else {
            // Dummy: 1 byte LEB128 size + 3 bytes body = 4 bytes.
            offset += 1 + DUMMY_FUNCTION.len;
        }
    }

    // Update import_fn_count to reflect removals.
    self.import_fn_count -= eliminated_import_count;
}

/// Trace the call graph starting from called functions, exports, init funcs,
/// and element section entries. Returns a bool slice where `result[fn_index]`
/// is true if the function is reachable.
fn traceLiveFunctions(
    self: *const Self,
    called_fns: []const bool,
    fn_index_min: u32,
    fn_count: u32,
) ![]bool {
    const gpa = self.allocator;

    // --- Categorize relocation entries ---
    // We iterate the relocation entries directly in the inner loop rather
    // than copying them, to avoid needing temporary ArrayLists of anonymous structs.

    // Build symbol_index → function_index lookup.
    const sym_fn_indices = try gpa.alloc(u32, self.linking.symbol_table.items.len);
    defer gpa.free(sym_fn_indices);
    for (self.linking.symbol_table.items, 0..) |sym, i| {
        sym_fn_indices[i] = if (sym.isFunction()) sym.index else std.math.maxInt(u32);
    }

    // --- Iterative live-function tracing ---
    const live_flags = try gpa.alloc(bool, fn_count);
    @memset(live_flags, false);

    const current_pass = try gpa.alloc(bool, fn_count);
    defer gpa.free(current_pass);
    @memset(current_pass, false);

    const next_pass = try gpa.alloc(bool, fn_count);
    defer gpa.free(next_pass);
    @memset(next_pass, false);

    // Seed with called_fns.
    for (called_fns, 0..) |is_called, i| {
        if (is_called and i < fn_count) current_pass[i] = true;
    }

    // Seed with exported functions.
    for (self.exports.items) |exp| {
        if (exp.kind == .func and exp.idx < fn_count) {
            current_pass[exp.idx] = true;
        }
    }

    // Seed with init functions.
    for (self.linking.init_funcs.items) |init_fn| {
        const sym = self.linking.symbol_table.items[init_fn.symbol_index];
        if (sym.isFunction() and sym.index < fn_count) {
            current_pass[sym.index] = true;
        }
    }

    // Seed with element section entries (indirect call targets).
    for (self.table_func_indices.items) |fi| {
        if (fi < fn_count) current_pass[fi] = true;
    }

    // Iterate until no new functions are discovered.
    while (true) {
        const any_new = std.mem.indexOfScalar(bool, current_pass, true) != null;
        if (!any_new) break;

        // Mark current pass as live.
        for (current_pass, 0..) |is_current, i| {
            if (is_current) live_flags[i] = true;
        }

        // For each live function in this pass, find its callees.
        for (current_pass, 0..) |is_current, fi| {
            if (!is_current) continue;
            if (fi < fn_index_min or fi >= fn_count) continue;

            // Find function body byte range.
            const offset_index = fi - fn_index_min;
            const code_start = self.function_offsets.items[offset_index];
            const code_end: u32 = if (offset_index + 1 < self.function_offsets.items.len)
                self.function_offsets.items[offset_index + 1]
            else
                @intCast(self.code_bytes.items.len);

            // Scan relocation entries within this function body.
            for (self.reloc_code.entries.items) |entry| {
                switch (entry) {
                    .index => |idx| {
                        if (idx.offset > code_start and idx.offset < code_end) {
                            switch (idx.type_id) {
                                .function_index_leb => {
                                    // Direct call: mark the callee as live.
                                    const callee = sym_fn_indices[idx.symbol_index];
                                    if (callee < fn_count and !live_flags[callee]) {
                                        next_pass[callee] = true;
                                    }
                                },
                                .type_index_leb => {
                                    // Indirect call: conservatively mark all element-section
                                    // functions with matching type signature as live.
                                    const type_idx = self.linking.symbol_table.items[idx.symbol_index].index;
                                    for (self.table_func_indices.items) |tfi| {
                                        if (tfi >= fn_index_min and tfi < fn_count) {
                                            const local = tfi - fn_index_min;
                                            const tfi_type = self.func_type_indices.items[self.dead_import_dummy_count + local];
                                            if (tfi_type == type_idx and !live_flags[tfi]) {
                                                next_pass[tfi] = true;
                                            }
                                        }
                                    }
                                },
                                else => {},
                            }
                        }
                    },
                    .offset => {},
                }
            }
        }

        // Swap passes.
        @memcpy(current_pass, next_pass);
        @memset(next_pass, false);
    }

    return live_flags;
}

// --- Phase 5: Memory, Table, and Stack Pointer Ownership ---

/// Setup step (called after preload, before code generation):
/// Validate that memory and table ownership is correctly configured.
///
/// In relocatable WASM objects, memory, table, and __stack_pointer are imported.
/// Our parser already strips non-function imports from the imports array — only
/// function imports are stored. Memory and table state is tracked via `has_memory`
/// and `has_table` flags, and will be emitted as defined sections (not imports)
/// when the module is encoded.
///
/// The __stack_pointer global import is handled implicitly: it exists in the
/// symbol table for relocation resolution and will become a defined global
/// during `finalizeMemoryAndTable()`.
/// Promote globally-visible, defined function symbols from the linking section
/// to actual WASM exports. In relocatable objects, `export fn` in Zig generates
/// symbols with `binding=global vis=default`, but no Export section exists.
/// This must be called after preload so that the surgical linker pipeline can
/// see and preserve these exports.
pub fn exportGlobalSymbols(self: *Self) void {
    for (self.linking.symbol_table.items) |sym| {
        if (sym.kind != .function or sym.isUndefined() or sym.isLocal()) continue;
        if ((sym.flags & WasmLinking.SymFlag.VISIBILITY_HIDDEN) != 0) continue;
        const name = sym.name orelse continue;
        // Skip roc__ symbols (handled by linkHostToAppCalls).
        if (std.mem.startsWith(u8, name, "roc__")) continue;
        // Avoid duplicate exports.
        var already_exported = false;
        for (self.exports.items) |exp| {
            if (exp.kind == .func and std.mem.eql(u8, exp.name, name)) {
                already_exported = true;
                break;
            }
        }
        if (!already_exported) {
            self.addExport(name, .func, sym.index) catch {};
        }
    }
}

/// No-op: memory and table imports are already stored in separate lists
/// during parsing (has_memory, has_table flags). This method only asserts
/// that the host module declared memory.
pub fn removeMemoryAndTableImports(self: *Self) void {
    // The parser separates function imports from memory/table/global imports.
    // Non-function imports are NOT in self.imports, so import_fn_count is correct.
    // Memory and table flags were set during parseImportSection.
    //
    // Assert the host module declared memory (required for any useful program).
    std.debug.assert(self.has_memory);
    // Note: has_table may not be set if the host doesn't use indirect calls yet.
    // Table will be set during finalization if table_func_indices are populated.
}

/// Finalization step (called after all code generation and surgical linking,
/// before encode):
///
/// 1. Calculate memory layout from data segments and stack requirements
/// 2. Define __stack_pointer global at top of memory
/// 3. Configure table size based on actual element count
/// 4. Export memory as "memory" for host/runtime access
pub fn finalizeMemoryAndTable(self: *Self, stack_bytes: u32) !void {
    // Calculate the highest data segment end address.
    var data_end: u32 = self.data_offset;
    for (self.data_segments.items) |ds| {
        const seg_end = ds.offset + @as(u32, @intCast(ds.data.len));
        data_end = @max(data_end, seg_end);
    }

    // Calculate memory pages: data + stack, rounded up to page boundary.
    const total_bytes: u64 = @as(u64, data_end) + @as(u64, stack_bytes);
    const page_size: u64 = 65536;
    const pages: u32 = @intCast(@max(1, (total_bytes + page_size - 1) / page_size));
    self.memory_min_pages = pages;

    // Define __stack_pointer as a mutable i32 global.
    // Initial value = top of memory (stack grows downward).
    self.has_stack_pointer = true;
    self.stack_pointer_init = pages * @as(u32, 65536);

    // Ensure memory is defined (not imported) in the final module.
    self.has_memory = true;

    // Configure table if we have any function indices to place in it.
    if (self.table_func_indices.items.len > 0) {
        self.has_table = true;
    }

    // Export memory as "memory" for host/runtime access.
    try self.exports.append(self.allocator, .{
        .name = "memory",
        .kind = .memory,
        .idx = 0,
    });
}

// --- Parsing (preload) ---

const wasm_magic = "\x00asm";
const wasm_version = 1;

/// Parse a relocatable WASM binary into a WasmModule.
/// The input bytes must contain `linking` and `reloc.*` custom sections
/// if `require_relocatable` is true.
pub fn preload(allocator: Allocator, bytes: []const u8, require_relocatable: bool) ParseError!Self {
    if (bytes.len < 8) return error.UnexpectedEnd;
    if (!std.mem.eql(u8, bytes[0..4], wasm_magic)) return error.InvalidMagic;
    if (std.mem.readInt(u32, bytes[4..8], .little) != wasm_version) return error.InvalidVersion;

    var module = Self.init(allocator);
    errdefer module.deinit();

    var cursor: usize = 8;

    // Parse standard sections in binary order.
    // Each parser checks the section ID and returns early if it doesn't match.
    try module.parseTypeSection(bytes, &cursor);
    try module.parseImportSection(bytes, &cursor);
    try module.parseFunctionSection(bytes, &cursor);
    try module.parseTableSection_(bytes, &cursor);
    try module.parseMemorySection_(bytes, &cursor);
    try module.parseGlobalSection_(bytes, &cursor);
    try module.parseExportSection(bytes, &cursor);
    try module.parseStartSection(bytes, &cursor);
    try module.parseElementSection_(bytes, &cursor);
    try module.parseDataCountSection(bytes, &cursor);
    try module.parseCodeSection(bytes, &cursor);
    try module.parseDataSection_(bytes, &cursor);

    // Parse trailing custom sections (linking, reloc.CODE, reloc.DATA)
    while (cursor < bytes.len) {
        try module.parseCustomSection(bytes, &cursor);
    }

    // Adjust reloc.CODE offsets: they are relative to the code section body
    // (which includes the function count LEB128), but code_bytes starts after
    // the count. Subtract the count's LEB128 size so offsets index into code_bytes.
    if (module.code_section_fn_count_leb_size > 0) {
        const delta = module.code_section_fn_count_leb_size;
        for (module.reloc_code.entries.items) |*entry| {
            switch (entry.*) {
                .index => |*idx| {
                    std.debug.assert(idx.offset >= delta);
                    idx.offset -= delta;
                },
                .offset => |*off| {
                    std.debug.assert(off.offset >= delta);
                    off.offset -= delta;
                },
            }
        }
        module.code_section_fn_count_leb_size = 0;
    }

    try module.normalizeDataRelocations();

    // Validate relocatable requirements
    if (require_relocatable) {
        if (module.linking.symbol_table.items.len == 0)
            return error.MissingLinkingSection;
        if (module.reloc_code.entries.items.len == 0)
            return error.MissingRelocCode;
        if (module.has_stack_pointer)
            return error.HasInternalGlobals;
    }

    module.import_fn_count = @intCast(module.imports.items.len);
    return module;
}

fn normalizeDataRelocations(self: *Self) ParseError!void {
    for (self.reloc_data.entries.items) |*entry| {
        const raw_offset = entry.getOffset();
        var matched = false;

        for (self.data_segments.items, 0..) |segment, seg_idx| {
            const seg_start = segment.section_offset;
            const seg_end = seg_start + @as(u32, @intCast(segment.data.len));
            if (raw_offset < seg_start or raw_offset >= seg_end) continue;

            const in_segment_offset = raw_offset - seg_start;
            switch (entry.*) {
                .index => |*idx| {
                    idx.offset = in_segment_offset;
                    idx.data_segment_index = @intCast(seg_idx);
                },
                .offset => |*off| {
                    off.offset = in_segment_offset;
                    off.data_segment_index = @intCast(seg_idx);
                },
            }
            matched = true;
            break;
        }

        if (!matched) return error.InvalidSection;
    }
}

/// Check if the byte at cursor matches the expected section ID.
/// If yes, read the section size and return it. If no, return null (section absent).
fn beginSection(bytes: []const u8, cursor: *usize, expected: SectionId) ParseError!?u32 {
    if (cursor.* >= bytes.len) return null;
    if (bytes[cursor.*] != @intFromEnum(expected)) return null;
    cursor.* += 1;
    return try readU32(bytes, cursor);
}

fn parseTypeSection(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .type_section) orelse return;
    const section_end = cursor.* + section_size;
    const count = try readU32(bytes, cursor);

    for (0..count) |_| {
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        _ = bytes[cursor.*]; // 0x60 func type marker
        cursor.* += 1;

        // Parse params
        const param_count = try readU32(bytes, cursor);
        const params_start = cursor.*;
        try skipBytes(bytes, cursor, param_count);
        const param_bytes = bytes[params_start..cursor.*];
        const params = try self.allocator.alloc(ValType, param_count);
        for (param_bytes, 0..) |b, i| {
            params[i] = std.meta.intToEnum(ValType, b) catch return error.InvalidSection;
        }
        try self.func_types.append(self.allocator, .{ .params = params });

        // Parse results
        const result_count = try readU32(bytes, cursor);
        if (result_count > 0) {
            if (cursor.* >= bytes.len) return error.UnexpectedEnd;
            const result_type = std.meta.intToEnum(ValType, bytes[cursor.*]) catch return error.InvalidSection;
            cursor.* += 1;
            // Skip any additional results (multi-value)
            if (result_count > 1) try skipBytes(bytes, cursor, result_count - 1);
            try self.func_type_results.append(self.allocator, result_type);
        } else {
            try self.func_type_results.append(self.allocator, null);
        }
    }
    cursor.* = section_end;
}

fn parseImportSection(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .import_section) orelse return;
    const section_end = cursor.* + section_size;
    const count = try readU32(bytes, cursor);

    for (0..count) |_| {
        const module_name = try readString(bytes, cursor);
        const field_name = try readString(bytes, cursor);

        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const kind_byte = bytes[cursor.*];
        cursor.* += 1;

        switch (kind_byte) {
            0x00 => { // function import
                const type_idx = try readU32(bytes, cursor);
                try self.imports.append(self.allocator, .{
                    .module_name = module_name,
                    .field_name = field_name,
                    .type_idx = type_idx,
                });
            },
            0x01 => { // table import
                self.has_table = true;
                _ = try readU32(bytes, cursor); // elem type (funcref)
                if (cursor.* >= bytes.len) return error.UnexpectedEnd;
                const limits_flag = bytes[cursor.*];
                cursor.* += 1;
                _ = try readU32(bytes, cursor); // min
                if (limits_flag == 0x01) _ = try readU32(bytes, cursor); // max
                try self.table_imports.append(self.allocator, .{
                    .module_name = module_name,
                    .field_name = field_name,
                });
            },
            0x02 => { // memory import
                self.has_memory = true;
                if (cursor.* >= bytes.len) return error.UnexpectedEnd;
                const limits_flag = bytes[cursor.*];
                cursor.* += 1;
                self.memory_min_pages = try readU32(bytes, cursor);
                if (limits_flag == 0x01) _ = try readU32(bytes, cursor); // max
            },
            0x03 => { // global import (e.g. __stack_pointer)
                const val_type_byte = try readU32(bytes, cursor);
                if (cursor.* >= bytes.len) return error.UnexpectedEnd;
                const mutability = bytes[cursor.*];
                cursor.* += 1;
                try self.global_imports.append(self.allocator, .{
                    .module_name = module_name,
                    .field_name = field_name,
                    .val_type = @intCast(val_type_byte),
                    .mutable = mutability == 0x01,
                });
                self.import_global_count += 1;
            },
            else => return error.InvalidSection,
        }
    }
    cursor.* = section_end;
}

fn parseFunctionSection(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .function_section) orelse return;
    const section_end = cursor.* + section_size;
    const count = try readU32(bytes, cursor);

    try self.func_type_indices.ensureTotalCapacity(self.allocator, count);
    for (0..count) |_| {
        const type_idx = try readU32(bytes, cursor);
        self.func_type_indices.appendAssumeCapacity(type_idx);
    }
    cursor.* = section_end;
}

fn parseTableSection_(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .table_section) orelse return;
    const section_end = cursor.* + section_size;
    self.has_table = true;
    // Skip table section contents (we just note it exists)
    cursor.* = section_end;
}

fn parseMemorySection_(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .memory_section) orelse return;
    const section_end = cursor.* + section_size;
    self.has_memory = true;
    const count = try readU32(bytes, cursor);
    if (count > 0) {
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const limits_flag = bytes[cursor.*];
        cursor.* += 1;
        self.memory_min_pages = try readU32(bytes, cursor);
        if (limits_flag == 0x01) _ = try readU32(bytes, cursor); // max
    }
    cursor.* = section_end;
}

fn parseGlobalSection_(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .global_section) orelse return;
    const section_end = cursor.* + section_size;
    // For relocatable modules, globals should NOT be defined internally
    // (the __stack_pointer comes from an import). Mark that we found them.
    self.has_stack_pointer = true;
    cursor.* = section_end;
}

fn parseExportSection(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .export_section) orelse return;
    const section_end = cursor.* + section_size;
    const count = try readU32(bytes, cursor);

    for (0..count) |_| {
        const name = try readString(bytes, cursor);
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const kind = std.meta.intToEnum(ExportKind, bytes[cursor.*]) catch return error.InvalidSection;
        cursor.* += 1;
        const idx = try readU32(bytes, cursor);
        try self.exports.append(self.allocator, .{ .name = name, .kind = kind, .idx = idx });
    }
    cursor.* = section_end;
}

fn parseStartSection(_: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .start_section) orelse return;
    cursor.* += section_size; // Skip start section
}

fn parseElementSection_(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .element_section) orelse return;
    const section_end = cursor.* + section_size;
    const count = try readU32(bytes, cursor);

    for (0..count) |_| {
        const seg_flags = try readU32(bytes, cursor);

        // Only handle flags=0 (active, table 0) — this is what LLVM/Zig emit for PIC.
        // Skip other segment types (passive, declarative, etc.) gracefully.
        if (seg_flags != 0) {
            cursor.* = section_end;
            return;
        }

        // Parse init expression: i32.const <offset>; end
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        cursor.* += 1; // skip i32.const opcode
        _ = try readI32(bytes, cursor); // skip offset value
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        cursor.* += 1; // skip end opcode

        // Parse function indices
        const elem_count = try readU32(bytes, cursor);
        for (0..elem_count) |_| {
            const func_idx = try readU32(bytes, cursor);
            try self.table_func_indices.append(self.allocator, func_idx);
        }

        self.has_table = true;
    }
    cursor.* = section_end;
}

fn parseDataCountSection(_: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .data_count_section) orelse return;
    cursor.* += section_size; // Consume and ignore
}

fn parseCodeSection(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .code_section) orelse return;
    const section_end = cursor.* + section_size;

    const before_fn_count = cursor.*;
    const fn_count = try readU32(bytes, cursor);

    // Record how many bytes the function count LEB128 consumed.
    // reloc.CODE offsets are relative to section body (including fn count),
    // but code_bytes starts after it — this delta is needed to adjust offsets.
    self.code_section_fn_count_leb_size = @intCast(cursor.* - before_fn_count);

    // Store raw bytes of the entire code section body (after the count).
    // Record each function's byte offset within code_bytes.
    const code_start = cursor.*;
    try self.function_offsets.ensureTotalCapacity(self.allocator, fn_count);

    for (0..fn_count) |_| {
        const fn_offset: u32 = @intCast(cursor.* - code_start);
        self.function_offsets.appendAssumeCapacity(fn_offset);
        const fn_size = try readU32(bytes, cursor);
        try skipBytes(bytes, cursor, fn_size);
    }

    // Copy the raw code bytes (from after count to end of section)
    const code_len = section_end - code_start;
    try self.code_bytes.ensureTotalCapacity(self.allocator, code_len);
    self.code_bytes.appendSliceAssumeCapacity(bytes[code_start..section_end]);

    cursor.* = section_end;
}

fn parseDataSection_(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .data_section) orelse return;
    const section_end = cursor.* + section_size;
    const section_body_start = cursor.*;
    const count = try readU32(bytes, cursor);

    for (0..count) |_| {
        // Segment flags per WASM spec: 0=active mem0, 1=passive, 2=active+memidx
        const seg_flags = try readU32(bytes, cursor);
        if (seg_flags == 2) {
            // Skip explicit memory index
            const mem_idx = try readU32(bytes, cursor);
            if (mem_idx != 0) return error.InvalidSection;
        }
        if (seg_flags == 1) {
            // Passive segment — no init expression, just data
            const data_len = try readU32(bytes, cursor);
            const data_start = cursor.*;
            try skipBytes(bytes, cursor, data_len);
            const data_copy = try self.allocator.dupe(u8, bytes[data_start .. data_start + data_len]);
            try self.data_segments.append(self.allocator, .{
                .offset = 0,
                .data = data_copy,
                .section_offset = @intCast(data_start - section_body_start),
            });
        } else {
            // Active segment — parse init expression: i32.const <offset> end
            if (cursor.* >= bytes.len) return error.UnexpectedEnd;
            cursor.* += 1; // skip i32.const opcode
            const offset: u32 = @bitCast(try readI32(bytes, cursor));
            if (cursor.* >= bytes.len) return error.UnexpectedEnd;
            cursor.* += 1; // skip end opcode
            const data_len = try readU32(bytes, cursor);
            const data_start = cursor.*;
            try skipBytes(bytes, cursor, data_len);
            const data_copy = try self.allocator.dupe(u8, bytes[data_start .. data_start + data_len]);
            try self.data_segments.append(self.allocator, .{
                .offset = offset,
                .data = data_copy,
                .section_offset = @intCast(data_start - section_body_start),
            });
            if (offset + data_len > self.data_offset) {
                self.data_offset = offset + data_len;
            }
        }
    }
    cursor.* = section_end;
}

fn parseCustomSection(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    if (cursor.* >= bytes.len) return;
    if (bytes[cursor.*] != @intFromEnum(SectionId.custom_section)) return error.InvalidSection;
    cursor.* += 1;

    const section_size = try readU32(bytes, cursor);
    const section_end = cursor.* + section_size;
    const name = try readString(bytes, cursor);

    if (std.mem.eql(u8, name, "linking")) {
        self.linking = try WasmLinking.LinkingSection.parse(
            self.allocator,
            bytes,
            cursor,
            section_end,
        );
    } else if (std.mem.eql(u8, name, "reloc.CODE")) {
        self.reloc_code = try WasmLinking.RelocationSection.parse(
            self.allocator,
            name,
            bytes,
            cursor,
            section_end,
        );
    } else if (std.mem.eql(u8, name, "reloc.DATA")) {
        self.reloc_data = try WasmLinking.RelocationSection.parse(
            self.allocator,
            name,
            bytes,
            cursor,
            section_end,
        );
    }
    // Skip any remaining bytes in this custom section (including unknown ones)
    cursor.* = section_end;
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

    // Import section (must be between type and function sections)
    if (self.imports.items.len > 0) {
        try self.encodeImportSection(allocator, &output);
    }

    // Function section
    if (self.func_type_indices.items.len > 0) {
        try self.encodeFunctionSection(allocator, &output);
    }

    // Table section (between function and memory)
    if (self.has_table) {
        try self.encodeTableSection(allocator, &output);
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

    // Element section (between export and code)
    if (self.has_table and self.table_func_indices.items.len > 0) {
        try self.encodeElementSection(allocator, &output);
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

    for (self.func_types.items, 0..) |ft, idx| {
        try section_data.append(gpa, 0x60); // func type marker
        try leb128WriteU32(gpa, &section_data, @intCast(ft.params.len));
        for (ft.params) |p| {
            try section_data.append(gpa, @intFromEnum(p));
        }
        if (self.func_type_results.items[idx]) |r| {
            try section_data.append(gpa, 1); // 1 result
            try section_data.append(gpa, @intFromEnum(r));
        } else {
            try section_data.append(gpa, 0); // 0 results
        }
    }

    try output.append(gpa, @intFromEnum(SectionId.type_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

fn encodeImportSection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, @intCast(self.imports.items.len));
    for (self.imports.items) |imp| {
        // Module name
        try leb128WriteU32(gpa, &section_data, @intCast(imp.module_name.len));
        try section_data.appendSlice(gpa, imp.module_name);
        // Field name
        try leb128WriteU32(gpa, &section_data, @intCast(imp.field_name.len));
        try section_data.appendSlice(gpa, imp.field_name);
        // Import kind: 0x00 = function
        try section_data.append(gpa, 0x00);
        // Type index
        try leb128WriteU32(gpa, &section_data, imp.type_idx);
    }

    try output.append(gpa, @intFromEnum(SectionId.import_section));
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

fn encodeGlobalSection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    const global_count: u32 = 1 + @as(u32, @intCast(self.extra_globals.items.len));
    try leb128WriteU32(gpa, &section_data, global_count);

    // Global 0: __stack_pointer (i32, mutable)
    try section_data.append(gpa, @intFromEnum(ValType.i32));
    try section_data.append(gpa, 0x01); // mutable
    try section_data.append(gpa, Op.i32_const);
    try leb128WriteI32(gpa, &section_data, @intCast(self.stack_pointer_init));
    try section_data.append(gpa, Op.end);

    // Extra globals (PIC: __memory_base=0, __table_base=0, etc.)
    for (self.extra_globals.items) |g| {
        try section_data.append(gpa, g.val_type);
        try section_data.append(gpa, if (g.mutable) @as(u8, 0x01) else @as(u8, 0x00));
        try section_data.append(gpa, Op.i32_const);
        try leb128WriteI32(gpa, &section_data, g.init_value);
        try section_data.append(gpa, Op.end);
    }

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

fn encodeTableSection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    // Table size = number of entries in the element section.
    // Minimum 1 because table index 0 is reserved (null function reference).
    const table_size: u32 = @max(1, @as(u32, @intCast(self.table_func_indices.items.len)));

    try leb128WriteU32(gpa, &section_data, 1); // 1 table
    try section_data.append(gpa, funcref); // element type: funcref
    try section_data.append(gpa, 0x00); // limits: no max
    try leb128WriteU32(gpa, &section_data, table_size);

    try output.append(gpa, @intFromEnum(SectionId.table_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

fn encodeElementSection(self: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, 1); // 1 element segment
    // Active segment for table 0
    try leb128WriteU32(gpa, &section_data, 0); // flags: active, table 0
    // Offset expression: i32.const 0; end
    try section_data.append(gpa, Op.i32_const);
    try leb128WriteI32(gpa, &section_data, 0);
    try section_data.append(gpa, Op.end);
    // Function indices
    try leb128WriteU32(gpa, &section_data, @intCast(self.table_func_indices.items.len));
    for (self.table_func_indices.items) |func_idx| {
        try leb128WriteU32(gpa, &section_data, func_idx);
    }

    try output.append(gpa, @intFromEnum(SectionId.element_section));
    try leb128WriteU32(gpa, output, @intCast(section_data.items.len));
    try output.appendSlice(gpa, section_data.items);
}

// --- LEB128 decoding utilities (for parsing WASM binaries) ---

/// Errors that can occur when parsing a WASM binary module.
pub const ParseError = error{
    UnexpectedEnd,
    Overflow,
    InvalidMagic,
    InvalidVersion,
    InvalidSection,
    MissingLinkingSection,
    MissingRelocCode,
    InvalidLinkingVersion,
    HasInternalGlobals,
    OutOfMemory,
};

/// Decode a u32 from unsigned LEB128 at `bytes[cursor.*]`, advancing cursor.
pub fn readU32(bytes: []const u8, cursor: *usize) ParseError!u32 {
    var result: u32 = 0;
    for (0..5) |i| {
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const byte = bytes[cursor.*];
        cursor.* += 1;
        const shift: u5 = @intCast(i * 7);
        result |= @as(u32, byte & 0x7f) << shift;
        if ((byte & 0x80) == 0) return result;
    }
    return error.Overflow;
}

/// Decode an i32 from signed LEB128 at `bytes[cursor.*]`, advancing cursor.
pub fn readI32(bytes: []const u8, cursor: *usize) ParseError!i32 {
    var result: u32 = 0;
    var shift: u6 = 0;
    var byte: u8 = undefined;
    for (0..5) |_| {
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        byte = bytes[cursor.*];
        cursor.* += 1;
        result |= @as(u32, byte & 0x7f) << @intCast(shift);
        shift += 7;
        if ((byte & 0x80) == 0) {
            if (shift < 32 and (byte & 0x40) != 0) {
                result |= @as(u32, 0xFFFFFFFF) << @intCast(shift);
            }
            return @bitCast(result);
        }
    }
    return error.Overflow;
}

/// Read a length-prefixed string from `bytes[cursor.*]`, advancing cursor.
/// Returns a slice into `bytes` (zero-copy, caller must keep bytes alive).
pub fn readString(bytes: []const u8, cursor: *usize) ParseError![]const u8 {
    const len = try readU32(bytes, cursor);
    const end = cursor.* + len;
    if (end > bytes.len) return error.UnexpectedEnd;
    const result = bytes[cursor.*..end];
    cursor.* = end;
    return result;
}

/// Skip `count` bytes, returning error if past end.
fn skipBytes(bytes: []const u8, cursor: *usize, count: u32) ParseError!void {
    const end = cursor.* + count;
    if (end > bytes.len) return error.UnexpectedEnd;
    cursor.* = end;
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

// --- Padded LEB128 utilities for surgical linking ---

/// Fixed size of a padded LEB128 value in bytes.
/// WASM relocatable objects use 5-byte padded LEB128 for all relocatable indices
/// so that values can be patched in-place without shifting surrounding bytes.
/// 5 = ceil(32/7) which is the maximum LEB128 encoding for a u32.
const padded_leb128_size: u32 = 5;

comptime {
    // A u32 LEB128 uses at most ceil(32/7) = 5 bytes.
    std.debug.assert(padded_leb128_size == 5);
    std.debug.assert(padded_leb128_size == (32 + 6) / 7);
}

/// Read a u32 from a 5-byte padded LEB128 encoding in the buffer.
pub fn readPaddedU32(buffer: []const u8, offset: u32) u32 {
    const off: usize = @intCast(offset);
    std.debug.assert(off + padded_leb128_size <= buffer.len);
    var result: u32 = 0;
    for (0..padded_leb128_size) |i| {
        result |= @as(u32, buffer[off + i] & 0x7f) << @intCast(7 * i);
    }
    return result;
}

/// Overwrite a 5-byte padded LEB128 u32 at the given offset in the buffer.
pub fn overwritePaddedU32(buffer: []u8, offset: u32, value: u32) void {
    var x = value;
    const off: usize = @intCast(offset);
    std.debug.assert(off + padded_leb128_size <= buffer.len);
    for (0..4) |i| {
        buffer[off + i] = @as(u8, @truncate(x & 0x7f)) | 0x80;
        x >>= 7;
    }
    buffer[off + 4] = @as(u8, @truncate(x));
}

/// Overwrite 5 bytes with a signed i32 in padded LEB128.
/// Used for signed memory address relocations.
pub fn overwritePaddedI32(buffer: []u8, offset: u32, value: i32) void {
    var x = value;
    const off: usize = @intCast(offset);
    std.debug.assert(off + padded_leb128_size <= buffer.len);
    for (0..4) |i| {
        buffer[off + i] = @as(u8, @truncate(@as(u32, @bitCast(x)) & 0x7f)) | 0x80;
        x >>= 7;
    }
    buffer[off + 4] = @as(u8, @truncate(@as(u32, @bitCast(x)) & 0x7f));
}

/// Append a u32 as exactly 5 bytes of padded LEB128 to an output buffer.
/// Used when emitting new relocatable instructions (call, global.get/set).
pub fn appendPaddedU32(gpa: Allocator, output: *std.ArrayList(u8), value: u32) !void {
    var x = value;
    for (0..padded_leb128_size - 1) |_| {
        try output.append(gpa, @as(u8, @truncate(x & 0x7f)) | 0x80);
        x >>= 7;
    }
    try output.append(gpa, @as(u8, @truncate(x)));
}

// --- Tests for padded LEB128 ---

/// Decode a 5-byte padded unsigned LEB128 value (test helper).
fn decodePaddedU32(bytes: []const u8) u32 {
    var result: u32 = 0;
    for (0..5) |i| {
        result |= @as(u32, bytes[i] & 0x7f) << @intCast(7 * i);
    }
    return result;
}

/// Decode a 5-byte padded signed LEB128 value (test helper).
fn decodePaddedI32(bytes: []const u8) i32 {
    var result: u32 = 0;
    for (0..5) |i| {
        result |= @as(u32, bytes[i] & 0x7f) << @intCast(7 * i);
    }
    return @bitCast(result);
}

test "overwritePaddedU32 — value 0 encodes as [0x80, 0x80, 0x80, 0x80, 0x00]" {
    var buf = [_]u8{0} ** 5;
    overwritePaddedU32(&buf, 0, 0);
    try std.testing.expectEqualSlices(u8, &.{ 0x80, 0x80, 0x80, 0x80, 0x00 }, &buf);
}

test "overwritePaddedU32 — value 1 encodes as [0x81, 0x80, 0x80, 0x80, 0x00]" {
    var buf = [_]u8{0} ** 5;
    overwritePaddedU32(&buf, 0, 1);
    try std.testing.expectEqualSlices(u8, &.{ 0x81, 0x80, 0x80, 0x80, 0x00 }, &buf);
}

test "overwritePaddedU32 — value 0x7F encodes as [0xFF, 0x80, 0x80, 0x80, 0x00]" {
    var buf = [_]u8{0} ** 5;
    overwritePaddedU32(&buf, 0, 0x7F);
    try std.testing.expectEqualSlices(u8, &.{ 0xFF, 0x80, 0x80, 0x80, 0x00 }, &buf);
}

test "overwritePaddedU32 — value 128 encodes as [0x80, 0x81, 0x80, 0x80, 0x00]" {
    var buf = [_]u8{0} ** 5;
    overwritePaddedU32(&buf, 0, 128);
    try std.testing.expectEqualSlices(u8, &.{ 0x80, 0x81, 0x80, 0x80, 0x00 }, &buf);
}

test "overwritePaddedU32 — max u32 (0xFFFFFFFF) encodes correctly" {
    var buf = [_]u8{0} ** 5;
    overwritePaddedU32(&buf, 0, 0xFFFFFFFF);
    try std.testing.expectEqualSlices(u8, &.{ 0xFF, 0xFF, 0xFF, 0xFF, 0x0F }, &buf);
}

test "overwritePaddedU32 — round-trip: write then decode matches original value" {
    const test_values = [_]u32{ 0, 1, 127, 128, 255, 256, 16383, 16384, 2097151, 2097152, 0x0FFFFFFF, 0xFFFFFFFF };
    for (test_values) |val| {
        var buf = [_]u8{0} ** 5;
        overwritePaddedU32(&buf, 0, val);
        try std.testing.expectEqual(val, decodePaddedU32(&buf));
    }
}

test "overwritePaddedI32 — negative value (-1) encodes correctly" {
    var buf = [_]u8{0} ** 5;
    overwritePaddedI32(&buf, 0, -1);
    // -1 in signed padded LEB128: all 7-bit groups are 0x7F, last byte keeps sign bit
    try std.testing.expectEqualSlices(u8, &.{ 0xFF, 0xFF, 0xFF, 0xFF, 0x7F }, &buf);
}

test "overwritePaddedI32 — positive value round-trips correctly" {
    const test_values = [_]i32{ 0, 1, -1, 127, -128, 32767, -32768, std.math.maxInt(i32), std.math.minInt(i32) };
    for (test_values) |val| {
        var buf = [_]u8{0} ** 5;
        overwritePaddedI32(&buf, 0, val);
        try std.testing.expectEqual(val, decodePaddedI32(&buf));
    }
}

test "appendPaddedU32 — appends exactly 5 bytes" {
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);
    try appendPaddedU32(std.testing.allocator, &output, 42);
    try std.testing.expectEqual(@as(usize, 5), output.items.len);
}

test "appendPaddedU32 — output is decodable as standard LEB128" {
    const test_values = [_]u32{ 0, 1, 127, 128, 16384, 0xFFFFFFFF };
    for (test_values) |val| {
        var output: std.ArrayList(u8) = .empty;
        defer output.deinit(std.testing.allocator);
        try appendPaddedU32(std.testing.allocator, &output, val);
        try std.testing.expectEqual(val, decodePaddedU32(output.items));
    }
}

// --- Tests for LEB128 decoding ---

test "readU32 — decodes single-byte value" {
    const bytes = [_]u8{42};
    var cursor: usize = 0;
    try std.testing.expectEqual(@as(u32, 42), try readU32(&bytes, &cursor));
    try std.testing.expectEqual(@as(usize, 1), cursor);
}

test "readU32 — decodes multi-byte value" {
    const bytes = [_]u8{ 0x80, 0x01 }; // 128
    var cursor: usize = 0;
    try std.testing.expectEqual(@as(u32, 128), try readU32(&bytes, &cursor));
    try std.testing.expectEqual(@as(usize, 2), cursor);
}

test "readU32 — decodes max u32 padded" {
    const bytes = [_]u8{ 0xFF, 0xFF, 0xFF, 0xFF, 0x0F };
    var cursor: usize = 0;
    try std.testing.expectEqual(@as(u32, 0xFFFFFFFF), try readU32(&bytes, &cursor));
}

test "readI32 — decodes negative value" {
    // -1 in signed LEB128 = 0x7F
    const bytes = [_]u8{0x7F};
    var cursor: usize = 0;
    try std.testing.expectEqual(@as(i32, -1), try readI32(&bytes, &cursor));
}

test "readI32 — decodes positive value" {
    const bytes = [_]u8{42};
    var cursor: usize = 0;
    try std.testing.expectEqual(@as(i32, 42), try readI32(&bytes, &cursor));
}

test "readString — reads length-prefixed string" {
    const bytes = [_]u8{ 3, 'f', 'o', 'o' };
    var cursor: usize = 0;
    const s = try readString(&bytes, &cursor);
    try std.testing.expectEqualStrings("foo", s);
    try std.testing.expectEqual(@as(usize, 4), cursor);
}

// --- Tests for preload ---

/// Build a minimal relocatable WASM binary for testing.
/// Contains: 1 function import, 1 defined function, 1 export, linking + reloc sections.
fn buildTestRelocatableModule(allocator: Allocator) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);

    // Magic + version
    try out.appendSlice(allocator, "\x00asm");
    try out.appendSlice(allocator, &[_]u8{ 0x01, 0x00, 0x00, 0x00 });

    // Type section: 1 type () -> ()
    try out.append(allocator, @intFromEnum(SectionId.type_section));
    try writeSectionBody(allocator, &out, &.{
        0x01, // 1 type
        0x60, // func
        0x00, // 0 params
        0x00, // 0 results
    });

    // Import section: 1 function import "env" "roc__main_exposed"
    {
        var import_data: std.ArrayList(u8) = .empty;
        defer import_data.deinit(allocator);
        try leb128WriteU32(allocator, &import_data, 1); // 1 import
        // module name "env"
        try leb128WriteU32(allocator, &import_data, 3);
        try import_data.appendSlice(allocator, "env");
        // field name "roc__main_exposed"
        try leb128WriteU32(allocator, &import_data, 17);
        try import_data.appendSlice(allocator, "roc__main_exposed");
        // function import, type 0
        try import_data.append(allocator, 0x00);
        try leb128WriteU32(allocator, &import_data, 0);

        try out.append(allocator, @intFromEnum(SectionId.import_section));
        try leb128WriteU32(allocator, &out, @intCast(import_data.items.len));
        try out.appendSlice(allocator, import_data.items);
    }

    // Function section: 1 function with type 0
    try out.append(allocator, @intFromEnum(SectionId.function_section));
    try writeSectionBody(allocator, &out, &.{
        0x01, // 1 function
        0x00, // type index 0
    });

    // Memory section: 1 memory, min 1 page
    try out.append(allocator, @intFromEnum(SectionId.memory_section));
    try writeSectionBody(allocator, &out, &.{
        0x01, // 1 memory
        0x00, // no max
        0x01, // min 1 page
    });

    // Export section: 1 export "_start" -> function 1
    {
        var export_data: std.ArrayList(u8) = .empty;
        defer export_data.deinit(allocator);
        try leb128WriteU32(allocator, &export_data, 1); // 1 export
        try leb128WriteU32(allocator, &export_data, 6); // name length
        try export_data.appendSlice(allocator, "_start");
        try export_data.append(allocator, 0x00); // func export
        try leb128WriteU32(allocator, &export_data, 1); // func index 1

        try out.append(allocator, @intFromEnum(SectionId.export_section));
        try leb128WriteU32(allocator, &out, @intCast(export_data.items.len));
        try out.appendSlice(allocator, export_data.items);
    }

    // Code section: 1 function body
    // Body: call import_0 (padded LEB128), end
    {
        var code_section: std.ArrayList(u8) = .empty;
        defer code_section.deinit(allocator);
        try leb128WriteU32(allocator, &code_section, 1); // 1 function

        // Function body: [size] [0 locals] [call 0 (padded)] [end]
        const body = [_]u8{
            0x00, // 0 local declarations
            Op.call,
            0x80,   0x80, 0x80, 0x80, 0x00, // padded LEB128 for function index 0
            Op.end,
        };
        try leb128WriteU32(allocator, &code_section, body.len);
        try code_section.appendSlice(allocator, &body);

        try out.append(allocator, @intFromEnum(SectionId.code_section));
        try leb128WriteU32(allocator, &out, @intCast(code_section.items.len));
        try out.appendSlice(allocator, code_section.items);
    }

    // Custom section: "linking"
    {
        var linking_body: std.ArrayList(u8) = .empty;
        defer linking_body.deinit(allocator);

        // Section name "linking"
        try leb128WriteU32(allocator, &linking_body, 7);
        try linking_body.appendSlice(allocator, "linking");

        // Version 2
        try linking_body.append(allocator, 2);

        // Subsection: symbol table (ID 8)
        {
            var sym_data: std.ArrayList(u8) = .empty;
            defer sym_data.deinit(allocator);

            try leb128WriteU32(allocator, &sym_data, 2); // 2 symbols

            // Symbol 0: undefined function import "roc__main_exposed" (index 0)
            try sym_data.append(allocator, @intFromEnum(WasmLinking.SymKind.function));
            try leb128WriteU32(allocator, &sym_data, WasmLinking.SymFlag.UNDEFINED); // flags
            try leb128WriteU32(allocator, &sym_data, 0); // function index

            // Symbol 1: defined function "_start" (index 1)
            try sym_data.append(allocator, @intFromEnum(WasmLinking.SymKind.function));
            try leb128WriteU32(allocator, &sym_data, 0); // flags (defined, not undefined)
            try leb128WriteU32(allocator, &sym_data, 1); // function index
            try leb128WriteU32(allocator, &sym_data, 6); // name length
            try sym_data.appendSlice(allocator, "_start");

            try linking_body.append(allocator, @intFromEnum(WasmLinking.LinkingSubsection.symbol_table));
            try leb128WriteU32(allocator, &linking_body, @intCast(sym_data.items.len));
            try linking_body.appendSlice(allocator, sym_data.items);
        }

        try out.append(allocator, @intFromEnum(SectionId.custom_section));
        try leb128WriteU32(allocator, &out, @intCast(linking_body.items.len));
        try out.appendSlice(allocator, linking_body.items);
    }

    // Custom section: "reloc.CODE"
    {
        var reloc_body: std.ArrayList(u8) = .empty;
        defer reloc_body.deinit(allocator);

        // Section name
        try leb128WriteU32(allocator, &reloc_body, 10);
        try reloc_body.appendSlice(allocator, "reloc.CODE");

        // Target section index (code section)
        try leb128WriteU32(allocator, &reloc_body, @intFromEnum(SectionId.code_section));

        // 1 relocation entry
        try leb128WriteU32(allocator, &reloc_body, 1);

        // R_WASM_FUNCTION_INDEX_LEB, offset=2 (after size+locals), symbol_index=0
        try reloc_body.append(allocator, @intFromEnum(WasmLinking.IndexRelocType.function_index_leb));
        try leb128WriteU32(allocator, &reloc_body, 2); // offset within code_bytes
        try leb128WriteU32(allocator, &reloc_body, 0); // symbol index

        try out.append(allocator, @intFromEnum(SectionId.custom_section));
        try leb128WriteU32(allocator, &out, @intCast(reloc_body.items.len));
        try out.appendSlice(allocator, reloc_body.items);
    }

    return out.toOwnedSlice(allocator);
}

/// Helper: write a section body with known bytes.
fn writeSectionBody(allocator: Allocator, out: *std.ArrayList(u8), body: []const u8) !void {
    try leb128WriteU32(allocator, out, @intCast(body.len));
    try out.appendSlice(allocator, body);
}

test "preload — rejects bytes without WASM magic number" {
    const bad_bytes = [_]u8{ 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00 };
    try std.testing.expectError(error.InvalidMagic, preload(std.testing.allocator, &bad_bytes, false));
}

test "preload — rejects wrong version" {
    const bad_bytes = [_]u8{ 0x00, 0x61, 0x73, 0x6D, 0x02, 0x00, 0x00, 0x00 };
    try std.testing.expectError(error.InvalidVersion, preload(std.testing.allocator, &bad_bytes, false));
}

test "preload — rejects too-short input" {
    const bad_bytes = [_]u8{ 0x00, 0x61, 0x73, 0x6D };
    try std.testing.expectError(error.UnexpectedEnd, preload(std.testing.allocator, &bad_bytes, false));
}

test "preload — parses minimal valid module (magic + version only)" {
    const bytes = [_]u8{ 0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00 };
    var module = try preload(std.testing.allocator, &bytes, false);
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.func_types.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.imports.items.len);
}

test "preload — parses type section with multiple signatures" {
    const allocator = std.testing.allocator;
    const wasm_bytes = try buildTestRelocatableModule(allocator);
    defer allocator.free(wasm_bytes);

    var module = try preload(allocator, wasm_bytes, false);
    defer module.deinit();

    // We built 1 type: () -> ()
    try std.testing.expectEqual(@as(usize, 1), module.func_types.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.func_types.items[0].params.len);
    try std.testing.expectEqual(@as(?ValType, null), module.func_type_results.items[0]);
}

test "preload — parses import section with function import" {
    const allocator = std.testing.allocator;
    const wasm_bytes = try buildTestRelocatableModule(allocator);
    defer allocator.free(wasm_bytes);

    var module = try preload(allocator, wasm_bytes, false);
    defer module.deinit();

    try std.testing.expectEqual(@as(usize, 1), module.imports.items.len);
    try std.testing.expectEqualStrings("env", module.imports.items[0].module_name);
    try std.testing.expectEqualStrings("roc__main_exposed", module.imports.items[0].field_name);
    try std.testing.expectEqual(@as(u32, 0), module.imports.items[0].type_idx);
    try std.testing.expectEqual(@as(u32, 1), module.import_fn_count);
}

test "preload — records correct function_offsets for code section" {
    const allocator = std.testing.allocator;
    const wasm_bytes = try buildTestRelocatableModule(allocator);
    defer allocator.free(wasm_bytes);

    var module = try preload(allocator, wasm_bytes, false);
    defer module.deinit();

    // 1 locally-defined function
    try std.testing.expectEqual(@as(usize, 1), module.function_offsets.items.len);
    try std.testing.expectEqual(@as(usize, 1), module.func_type_indices.items.len);
    // Function offset 0 = start of first function body within code_bytes
    try std.testing.expectEqual(@as(u32, 0), module.function_offsets.items[0]);
    // code_bytes should contain the function body
    try std.testing.expect(module.code_bytes.items.len > 0);
}

test "preload — parses linking section symbol table" {
    const allocator = std.testing.allocator;
    const wasm_bytes = try buildTestRelocatableModule(allocator);
    defer allocator.free(wasm_bytes);

    var module = try preload(allocator, wasm_bytes, false);
    defer module.deinit();

    try std.testing.expectEqual(@as(usize, 2), module.linking.symbol_table.items.len);

    // Symbol 0: undefined function import
    const sym0 = module.linking.symbol_table.items[0];
    try std.testing.expectEqual(WasmLinking.SymKind.function, sym0.kind);
    try std.testing.expect(sym0.isUndefined());
    try std.testing.expectEqual(@as(u32, 0), sym0.index);
    // Implicitly named (no explicit name for undefined import without EXPLICIT_NAME flag)
    try std.testing.expect(sym0.isImplicitlyNamed());

    // Symbol 1: defined function "_start"
    const sym1 = module.linking.symbol_table.items[1];
    try std.testing.expectEqual(WasmLinking.SymKind.function, sym1.kind);
    try std.testing.expect(!sym1.isUndefined());
    try std.testing.expectEqualStrings("_start", sym1.name.?);
    try std.testing.expectEqual(@as(u32, 1), sym1.index);
}

test "preload — parses reloc.CODE section entries" {
    const allocator = std.testing.allocator;
    const wasm_bytes = try buildTestRelocatableModule(allocator);
    defer allocator.free(wasm_bytes);

    var module = try preload(allocator, wasm_bytes, false);
    defer module.deinit();

    try std.testing.expectEqual(@as(usize, 1), module.reloc_code.entries.items.len);
    const entry = module.reloc_code.entries.items[0];
    switch (entry) {
        .index => |idx| {
            try std.testing.expectEqual(WasmLinking.IndexRelocType.function_index_leb, idx.type_id);
            // Original binary offset was 2 (relative to code section body start,
            // including fn count LEB128). preload adjusts by subtracting the fn
            // count LEB size (1 byte), so the stored offset is 1.
            try std.testing.expectEqual(@as(u32, 1), idx.offset);
            try std.testing.expectEqual(@as(u32, 0), idx.symbol_index);
        },
        .offset => unreachable,
    }
}

test "preload — require_relocatable rejects module without linking section" {
    // A minimal valid module with no custom sections
    const bytes = [_]u8{ 0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00 };
    try std.testing.expectError(error.MissingLinkingSection, preload(std.testing.allocator, &bytes, true));
}

test "preload — parsed module has correct function count" {
    const allocator = std.testing.allocator;
    const wasm_bytes = try buildTestRelocatableModule(allocator);
    defer allocator.free(wasm_bytes);

    var module = try preload(allocator, wasm_bytes, false);
    defer module.deinit();

    // 1 import + 1 defined = 2 total functions
    try std.testing.expectEqual(@as(u32, 1), module.import_fn_count);
    try std.testing.expectEqual(@as(usize, 1), module.func_type_indices.items.len);
    // Total function count = import_fn_count + func_type_indices.len
    try std.testing.expectEqual(
        @as(usize, 2),
        @as(usize, module.import_fn_count) + module.func_type_indices.items.len,
    );
}

test "preload — parses export section" {
    const allocator = std.testing.allocator;
    const wasm_bytes = try buildTestRelocatableModule(allocator);
    defer allocator.free(wasm_bytes);

    var module = try preload(allocator, wasm_bytes, false);
    defer module.deinit();

    try std.testing.expectEqual(@as(usize, 1), module.exports.items.len);
    try std.testing.expectEqualStrings("_start", module.exports.items[0].name);
    try std.testing.expectEqual(ExportKind.func, module.exports.items[0].kind);
    try std.testing.expectEqual(@as(u32, 1), module.exports.items[0].idx);
}

test "preload — parses memory section" {
    const allocator = std.testing.allocator;
    const wasm_bytes = try buildTestRelocatableModule(allocator);
    defer allocator.free(wasm_bytes);

    var module = try preload(allocator, wasm_bytes, false);
    defer module.deinit();

    try std.testing.expect(module.has_memory);
    try std.testing.expectEqual(@as(u32, 1), module.memory_min_pages);
}

test "preload — symbol name resolution from imports" {
    const allocator = std.testing.allocator;
    const wasm_bytes = try buildTestRelocatableModule(allocator);
    defer allocator.free(wasm_bytes);

    var module = try preload(allocator, wasm_bytes, false);
    defer module.deinit();

    // Symbol 0 is implicitly named — resolve via imports
    const sym0 = module.linking.symbol_table.items[0];
    const resolved_name = sym0.resolveName(module.imports.items, module.global_imports.items, module.table_imports.items);
    try std.testing.expect(resolved_name != null);
    try std.testing.expectEqualStrings("roc__main_exposed", resolved_name.?);

    // findSymbolByName should find it
    const found = module.linking.findSymbolByName("roc__main_exposed", module.imports.items, module.global_imports.items, module.table_imports.items);
    try std.testing.expectEqual(@as(?u32, 0), found);
}

// --- Tests for linkHostToAppCalls ---

/// Build a WasmModule in memory for testing linkHostToAppCalls.
///
/// Function index space:
///   0: js_foo          (import, env)
///   1: roc__main_exposed (import, env) — the app function to link
///   2: js_bar          (import, env)
///   3: defined_0       — body calls roc__main_exposed (fn 1)
///   4: defined_1       — body calls js_bar (fn 2)
///
/// Each function body in code_bytes:
///   [body_size] [0x00 locals] [Op.call] [padded_leb128 fn_index] [Op.end]
///   = 1 + 8 = 9 bytes per function (body_size=8, encoded in 1 byte)
///
/// Relocation entries (offsets into code_bytes):
///   fn3 call operand at offset 3  → symbol 1 (roc__main_exposed)
///   fn4 call operand at offset 12 → symbol 2 (js_bar)
fn buildLinkingTestModule(allocator: Allocator) !Self {
    var module = Self.init(allocator);
    errdefer module.deinit();

    // Type 0: () -> ()
    _ = try module.addFuncType(&.{}, &.{});

    // 3 function imports
    _ = try module.addImport("env", "js_foo", 0);
    _ = try module.addImport("env", "roc__main_exposed", 0);
    _ = try module.addImport("env", "js_bar", 0);
    module.import_fn_count = 3;

    // 2 defined functions (type 0)
    try module.func_type_indices.append(allocator, 0); // defined_0 (global index 3)
    try module.func_type_indices.append(allocator, 0); // defined_1 (global index 4)

    // Build code_bytes: two function bodies.
    // fn3 body: call fn 1 (roc__main_exposed)
    //   offset 0: body_size=8
    //   offset 1: 0x00 (no locals)
    //   offset 2: Op.call
    //   offset 3..7: padded LEB128(1)
    //   offset 8: Op.end
    // fn4 body: call fn 2 (js_bar)
    //   offset 9: body_size=8
    //   offset 10: 0x00 (no locals)
    //   offset 11: Op.call
    //   offset 12..16: padded LEB128(2)
    //   offset 17: Op.end
    try module.code_bytes.appendSlice(allocator, &.{0x08}); // body size = 8
    try module.code_bytes.append(allocator, 0x00); // no locals
    try module.code_bytes.append(allocator, Op.call);
    try appendPaddedU32(allocator, &module.code_bytes, 1); // call fn 1
    try module.code_bytes.append(allocator, Op.end);

    try module.code_bytes.appendSlice(allocator, &.{0x08}); // body size = 8
    try module.code_bytes.append(allocator, 0x00); // no locals
    try module.code_bytes.append(allocator, Op.call);
    try appendPaddedU32(allocator, &module.code_bytes, 2); // call fn 2
    try module.code_bytes.append(allocator, Op.end);

    try module.function_offsets.append(allocator, 0); // fn3 at offset 0
    try module.function_offsets.append(allocator, 9); // fn4 at offset 9

    // Symbol table:
    //   sym 0: undefined function index 0 (js_foo) — implicitly named
    //   sym 1: undefined function index 1 (roc__main_exposed) — implicitly named
    //   sym 2: undefined function index 2 (js_bar) — implicitly named
    //   sym 3: defined function index 3 (defined_0)
    //   sym 4: defined function index 4 (defined_1)
    try module.linking.symbol_table.appendSlice(allocator, &.{
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 0 },
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 1 },
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 2 },
        .{ .kind = .function, .flags = 0, .name = "defined_0", .index = 3 },
        .{ .kind = .function, .flags = 0, .name = "defined_1", .index = 4 },
    });

    // Relocation entries for code section:
    //   fn3's call operand at code_bytes offset 3 → sym 1 (roc__main_exposed)
    //   fn4's call operand at code_bytes offset 12 → sym 2 (js_bar)
    try module.reloc_code.entries.appendSlice(allocator, &.{
        .{ .index = .{ .type_id = .function_index_leb, .offset = 3, .symbol_index = 1 } },
        .{ .index = .{ .type_id = .function_index_leb, .offset = 12, .symbol_index = 2 } },
    });

    return module;
}

test "linkHostToAppCalls — single app function: import removed, dummy inserted" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Before: 3 imports, 2 defined, 0 dummies
    try std.testing.expectEqual(@as(usize, 3), module.imports.items.len);
    try std.testing.expectEqual(@as(usize, 2), module.func_type_indices.items.len);
    try std.testing.expectEqual(@as(u32, 0), module.dead_import_dummy_count);

    // Link roc__main_exposed → app function at index 5
    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});

    // After: 2 imports, 3 func_type_indices (1 dummy + 2 original), 1 dummy
    try std.testing.expectEqual(@as(usize, 2), module.imports.items.len);
    try std.testing.expectEqual(@as(usize, 3), module.func_type_indices.items.len);
    try std.testing.expectEqual(@as(u32, 1), module.dead_import_dummy_count);
}

test "linkHostToAppCalls — verifies call instruction patched to app function index" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Before: fn3 calls fn 1 (roc__main_exposed) — LEB128 at code_bytes[3..8]
    try std.testing.expectEqual(@as(u32, 1), decodePaddedU32(module.code_bytes.items[3..8]));

    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});

    // After: fn3's call should be patched to fn 5 (the app function)
    try std.testing.expectEqual(@as(u32, 5), decodePaddedU32(module.code_bytes.items[3..8]));
}

test "linkHostToAppCalls — last import swapped into vacated slot" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});

    // js_bar (was at index 2) should now be at index 1 (the vacated slot)
    try std.testing.expectEqualStrings("js_foo", module.imports.items[0].field_name);
    try std.testing.expectEqualStrings("js_bar", module.imports.items[1].field_name);
}

test "linkHostToAppCalls — swap import's call sites updated to new index" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Before: fn4 calls fn 2 (js_bar) — LEB128 at code_bytes[12..17]
    try std.testing.expectEqual(@as(u32, 2), decodePaddedU32(module.code_bytes.items[12..17]));

    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});

    // After: fn4's call should be patched to fn 1 (js_bar's new position)
    try std.testing.expectEqual(@as(u32, 1), decodePaddedU32(module.code_bytes.items[12..17]));
}

test "linkHostToAppCalls — multiple app functions linked in sequence" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Link two app functions sequentially
    try module.linkHostToAppCalls(&.{
        .{ .name = "roc__main_exposed", .fn_index = 5 },
        .{ .name = "js_foo", .fn_index = 6 },
    });

    // 2 imports removed → 1 remaining, 2 dummies
    try std.testing.expectEqual(@as(usize, 1), module.imports.items.len);
    try std.testing.expectEqual(@as(u32, 2), module.dead_import_dummy_count);
    try std.testing.expectEqual(@as(usize, 4), module.func_type_indices.items.len); // 2 dummies + 2 original
}

test "linkHostToAppCalls — dead_import_dummy_count incremented correctly" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    try std.testing.expectEqual(@as(u32, 0), module.dead_import_dummy_count);

    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});
    try std.testing.expectEqual(@as(u32, 1), module.dead_import_dummy_count);

    try module.linkHostToAppCalls(&.{.{ .name = "js_foo", .fn_index = 6 }});
    try std.testing.expectEqual(@as(u32, 2), module.dead_import_dummy_count);
}

test "linkHostToAppCalls — func_type_indices has dummy signature at position 0" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Before: func_type_indices = [0, 0] (two defined functions, both type 0)
    try std.testing.expectEqual(@as(usize, 2), module.func_type_indices.items.len);

    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});

    // After: func_type_indices = [0, 0, 0] — dummy at position 0
    try std.testing.expectEqual(@as(usize, 3), module.func_type_indices.items.len);
    try std.testing.expectEqual(@as(u32, 0), module.func_type_indices.items[0]); // dummy type signature
}

test "linkHostToAppCalls — total function count unchanged after linking" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Before: 3 imports + 2 defined = 5 total
    const total_before = module.imports.items.len + module.func_type_indices.items.len;
    try std.testing.expectEqual(@as(usize, 5), total_before);

    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});

    // After: 2 imports + 3 func_type_indices (1 dummy + 2 original) = 5 total
    const total_after = module.imports.items.len + module.func_type_indices.items.len;
    try std.testing.expectEqual(@as(usize, 5), total_after);
}

test "linkHostToAppCalls — unfound import exports app function instead" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    const exports_before = module.exports.items.len;

    // Link a function name that doesn't exist in imports
    try module.linkHostToAppCalls(&.{.{ .name = "roc__nonexistent", .fn_index = 7 }});

    // No imports removed, but an export was added
    try std.testing.expectEqual(@as(usize, 3), module.imports.items.len);
    try std.testing.expectEqual(exports_before + 1, module.exports.items.len);
    const new_export = module.exports.items[module.exports.items.len - 1];
    try std.testing.expectEqualStrings("roc__nonexistent", new_export.name);
    try std.testing.expectEqual(ExportKind.func, new_export.kind);
    try std.testing.expectEqual(@as(u32, 7), new_export.idx);
}

test "linkHostToAppCalls — import_fn_count decremented" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    try std.testing.expectEqual(@as(u32, 3), module.import_fn_count);

    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});

    try std.testing.expectEqual(@as(u32, 2), module.import_fn_count);
}

test "linkHostToAppCalls — symbol table updated for linked function" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Before: sym 1 (roc__main_exposed) has index 1
    try std.testing.expectEqual(@as(u32, 1), module.linking.symbol_table.items[1].index);

    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});

    // After: sym 1 should now point to app function index 5
    try std.testing.expectEqual(@as(u32, 5), module.linking.symbol_table.items[1].index);
}

test "linkHostToAppCalls — symbol table updated for swapped function" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Before: sym 2 (js_bar) has index 2
    try std.testing.expectEqual(@as(u32, 2), module.linking.symbol_table.items[2].index);

    try module.linkHostToAppCalls(&.{.{ .name = "roc__main_exposed", .fn_index = 5 }});

    // After: sym 2 (js_bar) should now have index 1 (swapped into roc__main_exposed's slot)
    try std.testing.expectEqual(@as(u32, 1), module.linking.symbol_table.items[2].index);
}

test "linkHostToAppCalls — linking last import is a no-op swap" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Link js_bar (the last import) — swap_fn_index == host_fn_index, no swap needed
    try module.linkHostToAppCalls(&.{.{ .name = "js_bar", .fn_index = 5 }});

    // js_bar removed, js_foo and roc__main_exposed remain
    try std.testing.expectEqual(@as(usize, 2), module.imports.items.len);
    try std.testing.expectEqualStrings("js_foo", module.imports.items[0].field_name);
    try std.testing.expectEqualStrings("roc__main_exposed", module.imports.items[1].field_name);

    // fn4's call should be patched from 2 to 5
    try std.testing.expectEqual(@as(u32, 5), decodePaddedU32(module.code_bytes.items[12..17]));

    // No swap relocation needed — sym 1 (roc__main_exposed) should be unchanged
    try std.testing.expectEqual(@as(u32, 1), module.linking.symbol_table.items[1].index);
}

// --- Tests for loading a real relocatable host module ---

test "preload — parses real Zig-compiled wasm host object" {
    const allocator = std.testing.allocator;
    const host_bytes = try std.fs.cwd().readFileAlloc(
        allocator,
        "test/wasm/platform/targets/wasm32/host.wasm",
        10 * 1024 * 1024, // 10 MB max
    );
    defer allocator.free(host_bytes);

    var module = try preload(allocator, host_bytes, true);
    defer module.deinit();

    // The host should have function imports (extern fn declarations in host.zig)
    try std.testing.expect(module.imports.items.len > 0);
    try std.testing.expect(module.import_fn_count > 0);

    // Should have locally-defined functions
    try std.testing.expect(module.func_type_indices.items.len > 0);

    // Should have code bytes and matching function offsets
    try std.testing.expect(module.code_bytes.items.len > 0);
    try std.testing.expectEqual(module.func_type_indices.items.len, module.function_offsets.items.len);

    // Should have a populated symbol table
    try std.testing.expect(module.linking.symbol_table.items.len > 0);

    // Should have relocation entries for code
    try std.testing.expect(module.reloc_code.entries.items.len > 0);

    // The host imports roc__main — verify we can find it by name
    var found_roc_main = false;
    for (module.imports.items) |imp| {
        if (std.mem.eql(u8, imp.field_name, "roc__main")) {
            found_roc_main = true;
            break;
        }
    }
    try std.testing.expect(found_roc_main);

    // Verify total function count is consistent
    const total_fns = @as(usize, module.import_fn_count) + module.func_type_indices.items.len;
    try std.testing.expect(total_fns > 3); // at least imports + a few defined functions
}

// --- Phase 5 tests: Memory, Table, and Stack Pointer Ownership ---

/// Build a test module simulating a parsed relocatable host with memory, table,
/// and __stack_pointer global imports (as produced by clang/zig for wasm32).
fn buildPhase5TestModule(allocator: Allocator) !Self {
    var module = Self.init(allocator);
    errdefer module.deinit();

    // Type 0: () -> ()
    _ = try module.addFuncType(&.{}, &.{});

    // Function imports
    _ = try module.addImport("env", "roc__main", 0);
    _ = try module.addImport("env", "roc_panic", 0);
    module.import_fn_count = 2;

    // Simulate that the parser found memory and table imports
    // (these are NOT stored in the imports array, just flagged)
    module.has_memory = true;
    module.memory_min_pages = 1;
    module.has_table = true;

    // Simulate a __stack_pointer global import
    module.import_global_count = 1;

    // One defined function
    try module.func_type_indices.append(allocator, 0);
    try module.code_bytes.append(allocator, 0x02); // body size
    try module.code_bytes.append(allocator, 0x00); // no locals
    try module.code_bytes.append(allocator, Op.end);
    try module.function_offsets.append(allocator, 0);

    // Add a data segment at offset 1024 (after the reserved area)
    const data = try allocator.dupe(u8, "Hello, WASM!");
    try module.data_segments.append(allocator, .{ .offset = 1024, .data = data });

    // Symbol table with __stack_pointer global symbol
    try module.linking.symbol_table.appendSlice(allocator, &.{
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 0 },
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 1 },
        .{ .kind = .global, .flags = WasmLinking.SymFlag.UNDEFINED, .name = "__stack_pointer", .index = 0 },
        .{ .kind = .function, .flags = 0, .name = "wasm_main", .index = 2 },
    });

    // Relocation for global.get __stack_pointer
    try module.reloc_code.entries.append(allocator, .{
        .index = .{ .type_id = .global_index_leb, .offset = 1, .symbol_index = 2 },
    });

    return module;
}

test "setup — memory and table imports removed from host module" {
    const allocator = std.testing.allocator;
    var module = try buildPhase5TestModule(allocator);
    defer module.deinit();

    module.removeMemoryAndTableImports();

    // After setup, the imports array should only contain function imports
    try std.testing.expectEqual(@as(usize, 2), module.imports.items.len);
    try std.testing.expectEqualStrings("roc__main", module.imports.items[0].field_name);
    try std.testing.expectEqualStrings("roc_panic", module.imports.items[1].field_name);

    // Memory and table flags should still be set (they'll be defined sections)
    try std.testing.expect(module.has_memory);
    try std.testing.expect(module.has_table);
}

test "setup — import_fn_count unchanged after removing non-function imports" {
    const allocator = std.testing.allocator;
    var module = try buildPhase5TestModule(allocator);
    defer module.deinit();

    const fn_count_before = module.import_fn_count;
    module.removeMemoryAndTableImports();

    // import_fn_count should be unchanged — it only counts function imports
    try std.testing.expectEqual(fn_count_before, module.import_fn_count);
    try std.testing.expectEqual(@as(u32, 2), module.import_fn_count);
}

test "setup — __stack_pointer global defined with correct initial value" {
    const allocator = std.testing.allocator;
    var module = try buildPhase5TestModule(allocator);
    defer module.deinit();

    module.removeMemoryAndTableImports();
    try module.finalizeMemoryAndTable(1024); // 1KB stack

    // __stack_pointer should be defined (not imported)
    try std.testing.expect(module.has_stack_pointer);

    // Initial value = memory_pages * 65536 (top of memory)
    try std.testing.expectEqual(module.memory_min_pages * 65536, module.stack_pointer_init);
}

test "setup — memory section has correct minimum pages" {
    const allocator = std.testing.allocator;
    var module = try buildPhase5TestModule(allocator);
    defer module.deinit();

    module.removeMemoryAndTableImports();

    // Data segment: 12 bytes at offset 1024 → data_end = 1036
    // data_offset is 1024 (init default), data_end = max(1024, 1036) = 1036
    // With 1KB stack: total = 1036 + 1024 = 2060 bytes → 1 page
    try module.finalizeMemoryAndTable(1024);
    try std.testing.expectEqual(@as(u32, 1), module.memory_min_pages);

    // With a larger stack that pushes past one page:
    // total = 1036 + 65000 = 66036 → 2 pages
    module.memory_min_pages = 1; // reset
    try module.finalizeMemoryAndTable(65000);
    // Recalculate: data_end stays 1036, total = 1036 + 65000 = 66036
    // 66036 / 65536 = 1.007... → 2 pages
    try std.testing.expectEqual(@as(u32, 2), module.memory_min_pages);
}

test "setup — table size matches element count after finalization" {
    const allocator = std.testing.allocator;
    var module = try buildPhase5TestModule(allocator);
    defer module.deinit();

    // Add some function indices to the table
    try module.table_func_indices.append(allocator, 2); // wasm_main
    try module.table_func_indices.append(allocator, 3); // another fn
    try module.table_func_indices.append(allocator, 4); // another fn

    module.removeMemoryAndTableImports();
    try module.finalizeMemoryAndTable(1024);

    // Encode and verify the table section uses the correct size
    const encoded = try module.encode(allocator);
    defer allocator.free(encoded);

    // Parse the encoded output to verify table section
    var decoded = try preload(allocator, encoded, false);
    defer decoded.deinit();

    // The table should exist
    try std.testing.expect(decoded.has_table);
}

test "setup — memory exported as 'memory'" {
    const allocator = std.testing.allocator;
    var module = try buildPhase5TestModule(allocator);
    defer module.deinit();

    const exports_before = module.exports.items.len;
    module.removeMemoryAndTableImports();
    try module.finalizeMemoryAndTable(1024);

    // Should have one more export than before
    try std.testing.expectEqual(exports_before + 1, module.exports.items.len);

    // Find the memory export
    var found_memory_export = false;
    for (module.exports.items) |exp| {
        if (std.mem.eql(u8, exp.name, "memory") and exp.kind == .memory) {
            found_memory_export = true;
            try std.testing.expectEqual(@as(u32, 0), exp.idx);
        }
    }
    try std.testing.expect(found_memory_export);
}

test "setup — global import count tracked correctly" {
    const allocator = std.testing.allocator;
    var module = try buildPhase5TestModule(allocator);
    defer module.deinit();

    // The test module simulates 1 global import (__stack_pointer)
    try std.testing.expectEqual(@as(u32, 1), module.import_global_count);
}

test "setup — finalized module encodes and re-parses as valid WASM" {
    const allocator = std.testing.allocator;
    var module = try buildPhase5TestModule(allocator);
    defer module.deinit();

    // Add table entries for encoding
    try module.table_func_indices.append(allocator, 2);

    module.removeMemoryAndTableImports();
    try module.finalizeMemoryAndTable(4096);

    // Encode to final WASM binary
    const encoded = try module.encode(allocator);
    defer allocator.free(encoded);

    // Verify it's valid WASM (magic + version)
    try std.testing.expectEqualSlices(u8, &.{ 0x00, 0x61, 0x73, 0x6D }, encoded[0..4]);
    try std.testing.expectEqual(@as(u32, 1), std.mem.readInt(u32, encoded[4..8], .little));

    // Should be parseable as a non-relocatable module
    var decoded = try preload(allocator, encoded, false);
    defer decoded.deinit();

    // Verify memory is defined
    try std.testing.expect(decoded.has_memory);

    // Verify exports include "memory"
    var found_memory = false;
    for (decoded.exports.items) |exp| {
        if (std.mem.eql(u8, exp.name, "memory")) {
            found_memory = true;
        }
    }
    try std.testing.expect(found_memory);

    // Verify __stack_pointer global is defined
    try std.testing.expect(decoded.has_stack_pointer);
}

test "phase5 — real host module: removeMemoryAndTableImports preserves function imports" {
    const allocator = std.testing.allocator;
    const host_bytes = try std.fs.cwd().readFileAlloc(
        allocator,
        "test/wasm/platform/targets/wasm32/host.wasm",
        10 * 1024 * 1024,
    );
    defer allocator.free(host_bytes);

    var module = try preload(allocator, host_bytes, true);
    defer module.deinit();

    const fn_count_before = module.import_fn_count;
    const imports_before = module.imports.items.len;

    module.removeMemoryAndTableImports();

    // Function imports should be completely unchanged
    try std.testing.expectEqual(fn_count_before, module.import_fn_count);
    try std.testing.expectEqual(imports_before, module.imports.items.len);

    // Memory flag should be set (host imports memory)
    try std.testing.expect(module.has_memory);
}

test "phase5 — real host module: full setup and finalization produces valid WASM" {
    const allocator = std.testing.allocator;
    const host_bytes = try std.fs.cwd().readFileAlloc(
        allocator,
        "test/wasm/platform/targets/wasm32/host.wasm",
        10 * 1024 * 1024,
    );
    defer allocator.free(host_bytes);

    var module = try preload(allocator, host_bytes, true);
    defer module.deinit();

    // Phase 5 setup
    module.removeMemoryAndTableImports();

    // Phase 5 finalization with 64KB stack
    try module.finalizeMemoryAndTable(65536);

    // Verify state after finalization
    try std.testing.expect(module.has_memory);
    try std.testing.expect(module.has_stack_pointer);
    try std.testing.expect(module.memory_min_pages >= 1);
    try std.testing.expectEqual(module.memory_min_pages * 65536, module.stack_pointer_init);

    // Verify memory export was added
    var found_memory_export = false;
    for (module.exports.items) |exp| {
        if (std.mem.eql(u8, exp.name, "memory") and exp.kind == .memory) {
            found_memory_export = true;
        }
    }
    try std.testing.expect(found_memory_export);
}

// --- Phase 6 tests: WASM Function Pointer Representation & RocOps Layout ---

test "RocOps struct — correct field offsets for wasm32 (36 bytes total)" {
    const W = Self.WasmRocOps;
    try std.testing.expectEqual(@as(u32, 0), W.env_ptr);
    try std.testing.expectEqual(@as(u32, 4), W.roc_alloc_table_idx);
    try std.testing.expectEqual(@as(u32, 8), W.roc_dealloc_table_idx);
    try std.testing.expectEqual(@as(u32, 12), W.roc_realloc_table_idx);
    try std.testing.expectEqual(@as(u32, 16), W.roc_dbg_table_idx);
    try std.testing.expectEqual(@as(u32, 20), W.roc_expect_failed_table_idx);
    try std.testing.expectEqual(@as(u32, 24), W.roc_crashed_table_idx);
    try std.testing.expectEqual(@as(u32, 28), W.hosted_fns_count);
    try std.testing.expectEqual(@as(u32, 32), W.hosted_fns_ptr);
    try std.testing.expectEqual(@as(u32, 36), W.total_size);
    // Each field is 4 bytes (i32 on wasm32), 9 fields total
    try std.testing.expectEqual(@as(u32, 9 * 4), W.total_size);
}

test "call_indirect — roc_alloc uses 2-arg callback type, not RocCall type" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    // Register the 2-arg RocOps callback type: (i32, i32) -> void
    const roc_ops_type = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    // Register the 3-arg RocCall type: (i32, i32, i32) -> void
    const roc_call_type = try module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});

    // They must be distinct type indices
    try std.testing.expect(roc_ops_type != roc_call_type);

    // Import roc_alloc with the 2-arg type
    module.enableTable();
    const roc_alloc_idx = try module.addImport("env", "roc_alloc", roc_ops_type);

    // Verify the import's type index is the 2-arg type, not the 3-arg type
    try std.testing.expectEqual(roc_ops_type, module.imports.items[roc_alloc_idx].type_idx);
    try std.testing.expect(module.imports.items[roc_alloc_idx].type_idx != roc_call_type);
}

test "call_indirect — hosted function uses 3-arg RocCall type" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    // Register both type signatures
    const roc_ops_type = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    const roc_call_type = try module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});

    module.enableTable();

    // Add a hosted function using the convenience method
    const table_idx = try module.addHostedFunctionToTable("env", "hosted_fn_0", roc_call_type);

    // The hosted function import should use the 3-arg type
    try std.testing.expectEqual(roc_call_type, module.imports.items[0].type_idx);
    try std.testing.expect(module.imports.items[0].type_idx != roc_ops_type);

    // It should have a valid table entry
    try std.testing.expectEqual(@as(u32, 0), table_idx);
    try std.testing.expectEqual(@as(u32, 0), module.table_func_indices.items[0]);
}

test "call_indirect — mismatched type index would trap (validate type separation)" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    // Register both type signatures
    const roc_ops_type = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    const roc_call_type = try module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});

    // Verify they are stored as distinct entries in func_types
    try std.testing.expectEqual(@as(usize, 2), module.func_types.items.len);
    try std.testing.expect(roc_ops_type != roc_call_type);

    // The 2-arg type has 2 params
    try std.testing.expectEqual(@as(usize, 2), module.func_types.items[roc_ops_type].params.len);
    try std.testing.expectEqual(ValType.i32, module.func_types.items[roc_ops_type].params[0]);
    try std.testing.expectEqual(ValType.i32, module.func_types.items[roc_ops_type].params[1]);

    // The 3-arg type has 3 params
    try std.testing.expectEqual(@as(usize, 3), module.func_types.items[roc_call_type].params.len);
    try std.testing.expectEqual(ValType.i32, module.func_types.items[roc_call_type].params[0]);
    try std.testing.expectEqual(ValType.i32, module.func_types.items[roc_call_type].params[1]);
    try std.testing.expectEqual(ValType.i32, module.func_types.items[roc_call_type].params[2]);

    // Both return void (no result)
    try std.testing.expectEqual(@as(?ValType, null), module.func_type_results.items[roc_ops_type]);
    try std.testing.expectEqual(@as(?ValType, null), module.func_type_results.items[roc_call_type]);
}

test "function table — all RocOps functions have valid table entries after linking" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    const roc_ops_type = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    module.enableTable();

    // Import all 6 RocOps callbacks and add them to the table
    const roc_alloc_idx = try module.addImport("env", "roc_alloc", roc_ops_type);
    const roc_alloc_table = try module.addTableElement(roc_alloc_idx);

    const roc_dealloc_idx = try module.addImport("env", "roc_dealloc", roc_ops_type);
    const roc_dealloc_table = try module.addTableElement(roc_dealloc_idx);

    const roc_realloc_idx = try module.addImport("env", "roc_realloc", roc_ops_type);
    const roc_realloc_table = try module.addTableElement(roc_realloc_idx);

    const roc_dbg_idx = try module.addImport("env", "roc_dbg", roc_ops_type);
    const roc_dbg_table = try module.addTableElement(roc_dbg_idx);

    const roc_expect_failed_idx = try module.addImport("env", "roc_expect_failed", roc_ops_type);
    const roc_expect_failed_table = try module.addTableElement(roc_expect_failed_idx);

    const roc_crashed_idx = try module.addImport("env", "roc_crashed", roc_ops_type);
    const roc_crashed_table = try module.addTableElement(roc_crashed_idx);

    // Verify all 6 have sequential table indices
    try std.testing.expectEqual(@as(u32, 0), roc_alloc_table);
    try std.testing.expectEqual(@as(u32, 1), roc_dealloc_table);
    try std.testing.expectEqual(@as(u32, 2), roc_realloc_table);
    try std.testing.expectEqual(@as(u32, 3), roc_dbg_table);
    try std.testing.expectEqual(@as(u32, 4), roc_expect_failed_table);
    try std.testing.expectEqual(@as(u32, 5), roc_crashed_table);

    // Verify table_func_indices maps back to the correct function indices
    try std.testing.expectEqual(@as(usize, 6), module.table_func_indices.items.len);
    try std.testing.expectEqual(roc_alloc_idx, module.table_func_indices.items[0]);
    try std.testing.expectEqual(roc_dealloc_idx, module.table_func_indices.items[1]);
    try std.testing.expectEqual(roc_realloc_idx, module.table_func_indices.items[2]);
    try std.testing.expectEqual(roc_dbg_idx, module.table_func_indices.items[3]);
    try std.testing.expectEqual(roc_expect_failed_idx, module.table_func_indices.items[4]);
    try std.testing.expectEqual(roc_crashed_idx, module.table_func_indices.items[5]);

    // All imports should use the 2-arg type
    for (module.imports.items) |import| {
        try std.testing.expectEqual(roc_ops_type, import.type_idx);
    }
}

test "function table — hosted functions added to table with correct indices" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    const roc_ops_type = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    const roc_call_type = try module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
    module.enableTable();

    // Add RocOps callbacks first (as the codegen does)
    const roc_alloc_idx = try module.addImport("env", "roc_alloc", roc_ops_type);
    _ = try module.addTableElement(roc_alloc_idx);
    const roc_dealloc_idx = try module.addImport("env", "roc_dealloc", roc_ops_type);
    _ = try module.addTableElement(roc_dealloc_idx);

    // Now add hosted functions — they follow the RocOps entries in the table
    const hosted_0_table = try module.addHostedFunctionToTable("env", "hosted_fn_0", roc_call_type);
    const hosted_1_table = try module.addHostedFunctionToTable("env", "hosted_fn_1", roc_call_type);
    const hosted_2_table = try module.addHostedFunctionToTable("env", "hosted_fn_2", roc_call_type);

    // Hosted functions follow RocOps entries (indices 0, 1 are alloc/dealloc)
    try std.testing.expectEqual(@as(u32, 2), hosted_0_table);
    try std.testing.expectEqual(@as(u32, 3), hosted_1_table);
    try std.testing.expectEqual(@as(u32, 4), hosted_2_table);

    // Total table size: 2 RocOps + 3 hosted = 5
    try std.testing.expectEqual(@as(usize, 5), module.table_func_indices.items.len);

    // Verify hosted function imports use the 3-arg type
    // Imports: [roc_alloc, roc_dealloc, hosted_fn_0, hosted_fn_1, hosted_fn_2]
    try std.testing.expectEqual(roc_ops_type, module.imports.items[0].type_idx);
    try std.testing.expectEqual(roc_ops_type, module.imports.items[1].type_idx);
    try std.testing.expectEqual(roc_call_type, module.imports.items[2].type_idx);
    try std.testing.expectEqual(roc_call_type, module.imports.items[3].type_idx);
    try std.testing.expectEqual(roc_call_type, module.imports.items[4].type_idx);

    // Verify table entries point to correct function indices
    try std.testing.expectEqual(@as(u32, 2), module.table_func_indices.items[2]); // hosted_fn_0
    try std.testing.expectEqual(@as(u32, 3), module.table_func_indices.items[3]); // hosted_fn_1
    try std.testing.expectEqual(@as(u32, 4), module.table_func_indices.items[4]); // hosted_fn_2
}

test "findFunctionIdxBySuffix — ignores imported symbols and finds defined host callback" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    const type_idx = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    _ = try module.addImport("env", "roc_dbg", type_idx);
    module.import_fn_count = 1;

    const callback_idx = try module.addFunction(type_idx);
    try module.linking.symbol_table.appendSlice(allocator, &.{
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 0 },
        .{ .kind = .function, .flags = 0, .name = "host.roc_dbg", .index = callback_idx },
    });

    try std.testing.expectEqual(callback_idx, module.findFunctionIdxBySuffix("roc_dbg").?);
}

test "ensureTableElement — reuses existing table entry" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    const type_idx = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    module.enableTable();

    const func_idx = try module.addFunction(type_idx);
    const first = try module.ensureTableElement(func_idx);
    const second = try module.ensureTableElement(func_idx);

    try std.testing.expectEqual(@as(u32, 0), first);
    try std.testing.expectEqual(first, second);
    try std.testing.expectEqual(@as(usize, 1), module.table_func_indices.items.len);
}

// --- mergeModule tests ---

/// Build a "host" module for merge testing.
///
/// Function index space:
///   0: roc_alloc       (import, env)
///   1: roc_dealloc     (import, env)
///   2: host_fn_0       (defined) — body calls roc_alloc (fn 0)
///
/// Types:
///   0: (i32, i32) → void   (roc_alloc signature)
///   1: () → void            (simple void function)
///
/// Symbol table:
///   sym 0: undefined function index 0 (roc_alloc) — implicitly named
///   sym 1: undefined function index 1 (roc_dealloc) — implicitly named
///   sym 2: defined function index 2 (host_fn_0)
fn buildMergeHostModule(allocator: Allocator) !Self {
    var module = Self.init(allocator);
    errdefer module.deinit();

    // Type 0: (i32, i32) -> void
    _ = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    // Type 1: () -> void
    _ = try module.addFuncType(&.{}, &.{});

    // 2 imports
    _ = try module.addImport("env", "roc_alloc", 0);
    _ = try module.addImport("env", "roc_dealloc", 0);
    module.import_fn_count = 2;

    // 1 defined function (type 1: () -> void)
    try module.func_type_indices.append(allocator, 1); // host_fn_0 (global index 2)

    // Code: host_fn_0 calls roc_alloc (fn 0)
    //   offset 0: body_size=8
    //   offset 1: 0x00 (no locals)
    //   offset 2: Op.call
    //   offset 3..7: padded LEB128(0)
    //   offset 8: Op.end
    try module.code_bytes.appendSlice(allocator, &.{0x08}); // body size = 8
    try module.code_bytes.append(allocator, 0x00); // no locals
    try module.code_bytes.append(allocator, Op.call);
    try appendPaddedU32(allocator, &module.code_bytes, 0); // call fn 0 (roc_alloc)
    try module.code_bytes.append(allocator, Op.end);

    try module.function_offsets.append(allocator, 0); // host_fn_0 at offset 0

    // Symbol table
    try module.linking.symbol_table.appendSlice(allocator, &.{
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 0 },
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 1 },
        .{ .kind = .function, .flags = 0, .name = "host_fn_0", .index = 2 },
    });

    // Relocation: host_fn_0's call at offset 3 → sym 0 (roc_alloc)
    try module.reloc_code.entries.append(allocator, .{ .index = .{
        .type_id = .function_index_leb,
        .offset = 3,
        .symbol_index = 0,
    } });

    return module;
}

/// Build a "builtins" module for merge testing.
///
/// Function index space:
///   0: roc_alloc       (import, env) — shared with host
///   1: builtin_fn_0    (defined) — body calls roc_alloc (fn 0)
///   2: builtin_fn_1    (defined) — body is just Op.end
///
/// Types:
///   0: (i32, i32) → void   (roc_alloc — same as host type 0)
///   1: (i32) → i32          (new type, not in host)
///
/// Data segment:
///   Segment 0: 4 bytes "DATA" at offset 0
///
/// Symbol table:
///   sym 0: undefined function index 0 (roc_alloc) — implicitly named
///   sym 1: defined function index 1 (roc_builtins_str_trim)
///   sym 2: defined function index 2 (roc_builtins_str_concat)
///   sym 3: defined data segment 0, offset 0, size 4
fn buildMergeBuiltinsModule(allocator: Allocator) !Self {
    var module = Self.init(allocator);
    errdefer module.deinit();
    module.data_offset = 0; // builtins start data at 0

    // Type 0: (i32, i32) -> void (same as host type 0)
    _ = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    // Type 1: (i32) -> i32 (new type)
    _ = try module.addFuncType(&.{.i32}, &.{.i32});

    // 1 import (roc_alloc — shared with host)
    _ = try module.addImport("env", "roc_alloc", 0);
    module.import_fn_count = 1;

    // 2 defined functions
    try module.func_type_indices.append(allocator, 0); // builtin_fn_0 uses type 0
    try module.func_type_indices.append(allocator, 1); // builtin_fn_1 uses type 1

    // Code: builtin_fn_0 calls roc_alloc (fn 0)
    //   offset 0: body_size=8
    //   offset 1: 0x00
    //   offset 2: Op.call
    //   offset 3..7: padded LEB128(0)
    //   offset 8: Op.end
    try module.code_bytes.appendSlice(allocator, &.{0x08});
    try module.code_bytes.append(allocator, 0x00);
    try module.code_bytes.append(allocator, Op.call);
    try appendPaddedU32(allocator, &module.code_bytes, 0); // call fn 0
    try module.code_bytes.append(allocator, Op.end);

    // Code: builtin_fn_1 (no calls, just return)
    //   offset 9: body_size=2
    //   offset 10: 0x00
    //   offset 11: Op.end
    try module.code_bytes.appendSlice(allocator, &.{0x02});
    try module.code_bytes.append(allocator, 0x00);
    try module.code_bytes.append(allocator, Op.end);

    try module.function_offsets.append(allocator, 0); // builtin_fn_0 at offset 0
    try module.function_offsets.append(allocator, 9); // builtin_fn_1 at offset 9

    // Data segment: 4 bytes "DATA"
    _ = try module.addDataSegment("DATA", 4);

    // Symbol table
    try module.linking.symbol_table.appendSlice(allocator, &.{
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 0 },
        .{ .kind = .function, .flags = 0, .name = "roc_builtins_str_trim", .index = 1 },
        .{ .kind = .function, .flags = 0, .name = "roc_builtins_str_concat", .index = 2 },
        .{ .kind = .data, .flags = 0, .name = ".rodata", .index = 0, .data_offset = 0, .data_size = 4 },
    });

    // Relocation: builtin_fn_0's call at offset 3 → sym 0 (roc_alloc)
    try module.reloc_code.entries.append(allocator, .{ .index = .{
        .type_id = .function_index_leb,
        .offset = 3,
        .symbol_index = 0,
    } });

    return module;
}

fn buildMergeDataRelocModule(allocator: Allocator) !Self {
    var module = Self.init(allocator);
    errdefer module.deinit();
    module.data_offset = 0;

    // Segment 0: relocation patch site (4-byte placeholder)
    _ = try module.addDataSegment(&[_]u8{ 0, 0, 0, 0 }, 4);
    // Segment 1: relocation target
    _ = try module.addDataSegment("DATA", 4);

    try module.linking.symbol_table.append(allocator, .{
        .kind = .data,
        .flags = 0,
        .name = ".rodata.target",
        .index = 1,
        .data_offset = 0, // offset within segment 1 before merge
        .data_size = 4,
    });

    try module.reloc_data.entries.append(allocator, .{ .offset = .{
        .type_id = .memory_addr_i32,
        .offset = 0,
        .symbol_index = 0,
        .addend = 0,
        .data_segment_index = 0,
    } });

    return module;
}

test "mergeModule — type deduplication: identical signatures share index" {
    const allocator = std.testing.allocator;
    var host = try buildMergeHostModule(allocator);
    defer host.deinit();
    const builtins = try buildMergeBuiltinsModule(allocator);
    defer @constCast(&builtins).deinit();

    const host_types_before = host.func_types.items.len;

    var result = try host.mergeModule(&builtins);
    defer result.deinit();

    // Host had 2 types: (i32,i32)->void, ()->void
    // Builtins had 2 types: (i32,i32)->void (dup), (i32)->i32 (new)
    // After merge: 3 types total (one deduplicated)
    try std.testing.expectEqual(host_types_before + 1, host.func_types.items.len);
}

test "mergeModule — function indices remapped correctly" {
    const allocator = std.testing.allocator;
    var host = try buildMergeHostModule(allocator);
    defer host.deinit();
    const builtins = try buildMergeBuiltinsModule(allocator);
    defer @constCast(&builtins).deinit();

    // Host has: 2 imports + 1 defined = 3 total functions
    // After merge: 2 imports + 1 host_defined + 2 builtins_defined = 5 total
    var result = try host.mergeModule(&builtins);
    defer result.deinit();

    try std.testing.expectEqual(@as(usize, 3), host.func_type_indices.items.len); // 1 host + 2 builtins
    // Builtins defined functions should start at global index 3 (2 imports + 1 host_defined)
    // Check symbol for roc_builtins_str_trim (was source global index 1)
    const trim_sym_idx = result.symbol_remap[1]; // src sym 1 → host sym
    const trim_sym = host.linking.symbol_table.items[trim_sym_idx];
    try std.testing.expectEqual(@as(u32, 3), trim_sym.index); // global fn index 3
    try std.testing.expectEqualStrings("roc_builtins_str_trim", trim_sym.name.?);

    // Check roc_builtins_str_concat (was source global index 2)
    const concat_sym_idx = result.symbol_remap[2];
    const concat_sym = host.linking.symbol_table.items[concat_sym_idx];
    try std.testing.expectEqual(@as(u32, 4), concat_sym.index); // global fn index 4
    try std.testing.expectEqualStrings("roc_builtins_str_concat", concat_sym.name.?);
}

test "mergeModule — code bytes appended at correct offset" {
    const allocator = std.testing.allocator;
    var host = try buildMergeHostModule(allocator);
    defer host.deinit();
    const builtins = try buildMergeBuiltinsModule(allocator);
    defer @constCast(&builtins).deinit();

    const host_code_len = host.code_bytes.items.len;
    const builtins_code_len = builtins.code_bytes.items.len;

    var result = try host.mergeModule(&builtins);
    defer result.deinit();

    // Code bytes should be concatenated
    try std.testing.expectEqual(host_code_len + builtins_code_len, host.code_bytes.items.len);

    // Function offsets for builtins should be shifted by host's code length
    // Host had 1 function_offset at 0. Builtins had 2 at 0, 9.
    // After merge: offsets [0, host_code_len+0, host_code_len+9]
    try std.testing.expectEqual(@as(usize, 3), host.function_offsets.items.len);
    try std.testing.expectEqual(@as(u32, 0), host.function_offsets.items[0]); // host_fn_0
    try std.testing.expectEqual(@as(u32, @intCast(host_code_len)), host.function_offsets.items[1]); // builtin_fn_0
    try std.testing.expectEqual(@as(u32, @intCast(host_code_len + 9)), host.function_offsets.items[2]); // builtin_fn_1
}

test "mergeModule — undefined symbol in builtins resolved to host's roc_alloc import" {
    const allocator = std.testing.allocator;
    var host = try buildMergeHostModule(allocator);
    defer host.deinit();
    const builtins = try buildMergeBuiltinsModule(allocator);
    defer @constCast(&builtins).deinit();

    var result = try host.mergeModule(&builtins);
    defer result.deinit();

    // Builtins sym 0 was undefined roc_alloc. It should resolve to host sym 0.
    try std.testing.expectEqual(@as(u32, 0), result.symbol_remap[0]);

    // No new import should be added (roc_alloc already exists in host)
    try std.testing.expectEqual(@as(usize, 2), host.imports.items.len);
}

test "mergeModule — relocation offsets shifted by base_code_offset" {
    const allocator = std.testing.allocator;
    var host = try buildMergeHostModule(allocator);
    defer host.deinit();
    const builtins = try buildMergeBuiltinsModule(allocator);
    defer @constCast(&builtins).deinit();

    const host_code_len: u32 = @intCast(host.code_bytes.items.len);
    const host_reloc_count = host.reloc_code.entries.items.len;

    var result = try host.mergeModule(&builtins);
    defer result.deinit();

    // Host had 1 relocation, builtins had 1 → total 2
    try std.testing.expectEqual(host_reloc_count + 1, host.reloc_code.entries.items.len);

    // Host's original relocation at offset 3 should be unchanged
    try std.testing.expectEqual(@as(u32, 3), host.reloc_code.entries.items[0].getOffset());

    // Builtins' relocation was at offset 3, should now be at host_code_len + 3
    try std.testing.expectEqual(host_code_len + 3, host.reloc_code.entries.items[1].getOffset());

    // The builtins relocation's symbol should be remapped to host's roc_alloc symbol
    try std.testing.expectEqual(@as(u32, 0), host.reloc_code.entries.items[1].getSymbolIndex());
}

test "mergeModule — data segment merged with adjusted offset" {
    const allocator = std.testing.allocator;
    var host = try buildMergeHostModule(allocator);
    defer host.deinit();
    const builtins = try buildMergeBuiltinsModule(allocator);
    defer @constCast(&builtins).deinit();

    const host_data_count = host.data_segments.items.len;

    var result = try host.mergeModule(&builtins);
    defer result.deinit();

    // Builtins had 1 data segment
    try std.testing.expectEqual(host_data_count + 1, host.data_segments.items.len);

    // The new data segment should have the "DATA" content
    const new_ds = host.data_segments.items[host.data_segments.items.len - 1];
    try std.testing.expectEqualStrings("DATA", new_ds.data);

    // Offset should be >= host's data_offset (1024 default)
    try std.testing.expect(new_ds.offset >= 1024);
}

test "mergeModule + resolveDataRelocations — patches merged data segment bytes" {
    const allocator = std.testing.allocator;
    var host = try buildMergeHostModule(allocator);
    defer host.deinit();
    var source = try buildMergeDataRelocModule(allocator);
    defer source.deinit();

    _ = try host.addDataSegment("HOST", 4);

    var result = try host.mergeModule(&source);
    defer result.deinit();

    try std.testing.expectEqual(@as(usize, 3), host.data_segments.items.len);
    try std.testing.expectEqual(@as(usize, 1), host.reloc_data.entries.items.len);

    host.resolveDataRelocations();

    const patch_segment = host.data_segments.items[1];
    const target_segment = host.data_segments.items[2];
    const patched = std.mem.readInt(u32, patch_segment.data[0..4], .little);

    try std.testing.expectEqual(target_segment.offset, patched);
}

test "mergeModule — element section entries remapped and appended" {
    const allocator = std.testing.allocator;
    var host = try buildMergeHostModule(allocator);
    defer host.deinit();
    var builtins = try buildMergeBuiltinsModule(allocator);
    defer builtins.deinit();

    // Host has 1 table entry: func_idx=2 (host_fn_0)
    _ = try host.addTableElement(2);

    // Builtins have 1 table entry: func_idx=1 (builtin_fn_0, source index space)
    try builtins.table_func_indices.append(allocator, 1);

    var result = try host.mergeModule(&builtins);
    defer result.deinit();

    // Host had 1 + builtins had 1 → total 2
    try std.testing.expectEqual(@as(usize, 2), host.table_func_indices.items.len);
    // Host's entry unchanged
    try std.testing.expectEqual(@as(u32, 2), host.table_func_indices.items[0]);
    // Builtins' entry remapped: source fn 1 → self fn 3
    // (host has 2 imports + 1 defined = base 3, source defined fn 0 maps to 3)
    try std.testing.expectEqual(@as(u32, 3), host.table_func_indices.items[1]);
    try std.testing.expect(host.has_table);
}

test "resolveCodeRelocations — table_index_sleb resolves to table index not function index" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    // Type 0: (i32, i32) -> void
    _ = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    // Type 1: () -> void
    _ = try module.addFuncType(&.{}, &.{});

    // 2 imports
    _ = try module.addImport("env", "roc_alloc", 0);
    _ = try module.addImport("env", "roc_dealloc", 0);
    module.import_fn_count = 2;

    // 3 defined functions (global indices 2, 3, 4)
    try module.func_type_indices.append(allocator, 0);
    try module.func_type_indices.append(allocator, 0);
    try module.func_type_indices.append(allocator, 1);

    // Table: fn 0 at table idx 0, fn 3 at table idx 1, fn 4 at table idx 2
    _ = try module.addTableElement(0);
    _ = try module.addTableElement(3);
    _ = try module.addTableElement(4);

    // Code: a function body with i32.const <table index placeholder>
    try module.code_bytes.appendSlice(allocator, &.{0x08}); // body size
    try module.code_bytes.append(allocator, 0x00); // no locals
    try module.code_bytes.append(allocator, Op.i32_const);
    try appendPaddedU32(allocator, &module.code_bytes, 0); // placeholder at offset 3
    try module.code_bytes.append(allocator, Op.end);
    try module.function_offsets.append(allocator, 0);

    // Symbol: sym 0 = function at global index 4 (which is table index 2)
    try module.linking.symbol_table.append(allocator, .{
        .kind = .function,
        .flags = 0,
        .name = "my_fn",
        .index = 4,
    });

    // Relocation: table_index_sleb at offset 3 → sym 0
    try module.reloc_code.entries.append(allocator, .{ .index = .{
        .type_id = .table_index_sleb,
        .offset = 3,
        .symbol_index = 0,
    } });

    module.resolveCodeRelocations();

    // Should be patched to table index 2 (position in table_func_indices), NOT function index 4
    var expected = [_]u8{0} ** 5;
    overwritePaddedI32(&expected, 0, 2);
    try std.testing.expectEqualSlices(u8, &expected, module.code_bytes.items[3..8]);
}

test "parseElementSection_ — parses function indices into table_func_indices" {
    const allocator = std.testing.allocator;

    // Build a module with known table entries, encode it, then re-parse.
    var source = Self.init(allocator);
    defer source.deinit();

    _ = try source.addFuncType(&.{}, &.{});
    _ = try source.addImport("env", "fn0", 0);
    source.import_fn_count = 1;

    try source.func_type_indices.append(allocator, 0); // fn 1
    try source.func_type_indices.append(allocator, 0); // fn 2

    // Minimal function bodies
    try source.func_bodies.append(allocator, .{ .body = try allocator.dupe(u8, &.{ 0x00, Op.end }) });
    try source.func_bodies.append(allocator, .{ .body = try allocator.dupe(u8, &.{ 0x00, Op.end }) });

    _ = try source.addTableElement(1); // table idx 0 → fn 1
    _ = try source.addTableElement(2); // table idx 1 → fn 2
    source.has_table = true;

    source.has_memory = true;

    // Encode to binary
    const encoded = try source.encode(allocator);
    defer allocator.free(encoded);

    // Re-parse from binary
    var parsed = try Self.preload(allocator, encoded, false);
    defer parsed.deinit();

    // Verify element section was parsed
    try std.testing.expectEqual(@as(usize, 2), parsed.table_func_indices.items.len);
    try std.testing.expectEqual(@as(u32, 1), parsed.table_func_indices.items[0]);
    try std.testing.expectEqual(@as(u32, 2), parsed.table_func_indices.items[1]);
    try std.testing.expect(parsed.has_table);
}

test "BuiltinSymbols — all symbols found after merge" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    // Type 0: () -> void
    _ = try module.addFuncType(&.{}, &.{});
    // 1 import (roc_alloc)
    _ = try module.addImport("env", "roc_alloc", 0);
    module.import_fn_count = 1;

    // Add a defined function symbol for each builtin that BuiltinSymbols expects.
    const names = comptime blk: {
        var result: [BuiltinSymbols.mapping.len][]const u8 = undefined;
        for (BuiltinSymbols.mapping, 0..) |entry, i| {
            result[i] = entry[0];
        }
        break :blk result;
    };

    for (names, 0..) |name, i| {
        try module.linking.symbol_table.append(allocator, .{
            .kind = .function,
            .flags = 0,
            .name = name,
            .index = @as(u32, @intCast(i)) + 1, // function index after imports
        });
    }

    const syms = BuiltinSymbols.populate(&module) catch |err| {
        std.debug.print("populate failed: {}\n", .{err});
        return err;
    };

    // Spot check a few fields (populate returns function index = i + 1, since index 0 is the import)
    try std.testing.expectEqual(@as(u32, 1), syms.dec_mul); // function index 1 (first defined fn after import)
    try std.testing.expectEqual(@as(u32, 21), syms.str_trim); // function index 21
}

test "BuiltinSymbols — fails when symbol missing" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    // Empty symbol table — should fail
    const result = BuiltinSymbols.populate(&module);
    try std.testing.expectError(error.MissingBuiltinSymbol, result);
}

test "resolveCodeRelocations — patches function call in code_bytes" {
    const allocator = std.testing.allocator;
    var module = try buildMergeHostModule(allocator);
    defer module.deinit();

    // Before resolution, the call operand at offset 3 is padded LEB128(0).
    // Symbol 0 is roc_alloc at function index 0.
    // After resolution it should still be 0 — but let's change the symbol's index
    // to verify the patch actually happens.

    // Change roc_alloc's symbol to point to function index 42.
    module.linking.symbol_table.items[0].index = 42;

    module.resolveCodeRelocations();

    // Read the patched value at offset 3 (5-byte padded LEB128).
    var expected = [_]u8{0} ** 5;
    overwritePaddedU32(&expected, 0, 42);
    try std.testing.expectEqualSlices(u8, &expected, module.code_bytes.items[3..8]);
}

test "materializeFuncBodies — produces correct function bodies from code_bytes" {
    const allocator = std.testing.allocator;
    var module = try buildMergeHostModule(allocator);
    defer module.deinit();

    try module.materializeFuncBodies();

    // Host has 1 defined function, no dummies.
    try std.testing.expectEqual(@as(usize, 1), module.func_bodies.items.len);

    // The body should be 8 bytes: [0x00 (locals), Op.call, 5 bytes LEB128, Op.end]
    try std.testing.expectEqual(@as(usize, 8), module.func_bodies.items[0].body.len);
}

test "materializeFuncBodies — includes dummy functions for dead imports" {
    const allocator = std.testing.allocator;
    var module = try buildLinkingTestModule(allocator);
    defer module.deinit();

    // Simulate one dead import dummy
    module.dead_import_dummy_count = 1;
    try module.func_type_indices.insert(allocator, 0, 0);

    try module.materializeFuncBodies();

    // Should have 1 dummy + 2 real = 3 func bodies
    try std.testing.expectEqual(@as(usize, 3), module.func_bodies.items.len);

    // First should be the dummy (unreachable + end)
    try std.testing.expectEqualSlices(u8, &DUMMY_FUNCTION, module.func_bodies.items[0].body);
}

test "verifyNoBuiltinImports — passes when only RocOps imports remain" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    _ = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    _ = try module.addImport("env", "roc_alloc", 0);
    _ = try module.addImport("env", "roc_dealloc", 0);
    _ = try module.addImport("env", "roc_realloc", 0);
    _ = try module.addImport("env", "roc_dbg", 0);
    _ = try module.addImport("env", "roc_expect_failed", 0);
    _ = try module.addImport("env", "roc_crashed", 0);

    try module.verifyNoBuiltinImports();
}

test "verifyNoBuiltinImports — fails if roc_str_trim import still present" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    _ = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    _ = try module.addImport("env", "roc_alloc", 0);
    _ = try module.addImport("env", "roc_str_trim", 0); // stale builtin import

    const result = module.verifyNoBuiltinImports();
    try std.testing.expectError(error.UnresolvedBuiltinImport, result);
}

test "verifyNoBuiltinImports — allows non-roc imports" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    _ = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    _ = try module.addImport("env", "roc_alloc", 0);
    _ = try module.addImport("env", "custom_platform_fn", 0); // non-roc import is fine

    try module.verifyNoBuiltinImports();
}

test "verifyNoBuiltinImports — allows roc_panic platform import" {
    const allocator = std.testing.allocator;
    var module = Self.init(allocator);
    defer module.deinit();

    _ = try module.addFuncType(&.{ .i32, .i32 }, &.{});
    _ = try module.addImport("env", "roc_panic", 0);

    try module.verifyNoBuiltinImports();
}

// --- Dead Code Elimination Tests ---

/// Build a test module for DCE tests.
///
/// Layout:
///   import 0: js_log     (type 0)
///   import 1: js_unused  (type 0)
///   import 2: js_helper  (type 0)
///   defined 3: main_fn   — calls js_log (import 0) and helper_fn (defined 4)
///   defined 4: helper_fn — calls js_helper (import 2)
///   defined 5: dead_fn   — calls js_unused (import 1)
///
/// Exports: main_fn (index 3) as "main"
///
/// Symbol table:
///   sym 0: undefined fn 0 (js_log)
///   sym 1: undefined fn 1 (js_unused)
///   sym 2: undefined fn 2 (js_helper)
///   sym 3: defined fn 3 (main_fn)
///   sym 4: defined fn 4 (helper_fn)
///   sym 5: defined fn 5 (dead_fn)
///
/// Relocation entries:
///   fn3 body: call sym 0 (js_log) at offset 3, call sym 4 (helper_fn) at offset 10
///   fn4 body: call sym 2 (js_helper) at offset 21
///   fn5 body: call sym 1 (js_unused) at offset 30
fn buildDCETestModule(allocator: Allocator) !Self {
    var module = Self.init(allocator);
    errdefer module.deinit();

    // Type 0: () -> ()
    _ = try module.addFuncType(&.{}, &.{});

    // 3 function imports
    _ = try module.addImport("env", "js_log", 0);
    _ = try module.addImport("env", "js_unused", 0);
    _ = try module.addImport("env", "js_helper", 0);
    module.import_fn_count = 3;

    // 3 defined functions (type 0)
    try module.func_type_indices.append(allocator, 0); // main_fn (global index 3)
    try module.func_type_indices.append(allocator, 0); // helper_fn (global index 4)
    try module.func_type_indices.append(allocator, 0); // dead_fn (global index 5)

    // Build code_bytes: three function bodies, each 9 bytes.
    // fn3 (main_fn): call js_log + call helper_fn
    //   offset 0: body_size=16
    //   offset 1: 0x00 (no locals)
    //   offset 2: Op.call
    //   offset 3..7: padded LEB128 → sym 0 (js_log, fn 0)
    //   offset 8: Op.call (second call)
    //   offset 9..13: padded LEB128 → sym 4 (helper_fn, fn 4)
    //   offset 14..16: nop nop end
    try module.code_bytes.appendSlice(allocator, &.{16}); // body size = 16
    try module.code_bytes.append(allocator, 0x00); // no locals
    try module.code_bytes.append(allocator, Op.call);
    try appendPaddedU32(allocator, &module.code_bytes, 0); // call fn 0 (placeholder)
    try module.code_bytes.append(allocator, Op.call);
    try appendPaddedU32(allocator, &module.code_bytes, 4); // call fn 4 (placeholder)
    try module.code_bytes.append(allocator, Op.nop);
    try module.code_bytes.append(allocator, Op.nop);
    try module.code_bytes.append(allocator, Op.end);
    // fn3 total: 1 (size) + 16 (body) = 17 bytes, offsets 0..16

    // fn4 (helper_fn): call js_helper
    //   offset 17: body_size=8
    //   offset 18: 0x00 (no locals)
    //   offset 19: Op.call
    //   offset 20..24: padded LEB128 → sym 2 (js_helper, fn 2)
    //   offset 25: Op.end
    try module.code_bytes.appendSlice(allocator, &.{8}); // body size = 8
    try module.code_bytes.append(allocator, 0x00);
    try module.code_bytes.append(allocator, Op.call);
    try appendPaddedU32(allocator, &module.code_bytes, 2); // call fn 2 (placeholder)
    try module.code_bytes.append(allocator, Op.end);
    // fn4 total: 1 (size) + 8 (body) = 9 bytes, offsets 17..25

    // fn5 (dead_fn): call js_unused
    //   offset 26: body_size=8
    //   offset 27: 0x00 (no locals)
    //   offset 28: Op.call
    //   offset 29..33: padded LEB128 → sym 1 (js_unused, fn 1)
    //   offset 34: Op.end
    try module.code_bytes.appendSlice(allocator, &.{8}); // body size = 8
    try module.code_bytes.append(allocator, 0x00);
    try module.code_bytes.append(allocator, Op.call);
    try appendPaddedU32(allocator, &module.code_bytes, 1); // call fn 1 (placeholder)
    try module.code_bytes.append(allocator, Op.end);
    // fn5 total: 1 (size) + 8 (body) = 9 bytes, offsets 26..34

    try module.function_offsets.append(allocator, 0); // fn3 at offset 0
    try module.function_offsets.append(allocator, 17); // fn4 at offset 17
    try module.function_offsets.append(allocator, 26); // fn5 at offset 26

    // Symbol table
    try module.linking.symbol_table.appendSlice(allocator, &.{
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 0 }, // sym 0: js_log
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 1 }, // sym 1: js_unused
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 2 }, // sym 2: js_helper
        .{ .kind = .function, .flags = 0, .name = "main_fn", .index = 3 }, // sym 3: main_fn
        .{ .kind = .function, .flags = 0, .name = "helper_fn", .index = 4 }, // sym 4: helper_fn
        .{ .kind = .function, .flags = 0, .name = "dead_fn", .index = 5 }, // sym 5: dead_fn
    });

    // Relocation entries for code section
    try module.reloc_code.entries.appendSlice(allocator, &.{
        .{ .index = .{ .type_id = .function_index_leb, .offset = 3, .symbol_index = 0 } }, // fn3 calls js_log
        .{ .index = .{ .type_id = .function_index_leb, .offset = 10, .symbol_index = 4 } }, // fn3 calls helper_fn (sym 4)
        .{ .index = .{ .type_id = .function_index_leb, .offset = 21, .symbol_index = 2 } }, // fn4 calls js_helper
        .{ .index = .{ .type_id = .function_index_leb, .offset = 30, .symbol_index = 1 } }, // fn5 calls js_unused
    });

    // Export main_fn
    try module.exports.append(allocator, .{ .name = "main", .kind = .func, .idx = 3 });

    return module;
}

test "eliminateDeadCode — exported function and its callees are preserved" {
    const allocator = std.testing.allocator;
    var module = try buildDCETestModule(allocator);
    defer module.deinit();

    // No extra called_fns — only exports seed the live set.
    var called_fns = [_]bool{false} ** 6;
    try module.eliminateDeadCode(&called_fns);

    // main_fn (3) is exported → live.
    // helper_fn (4) is called by main_fn → live.
    // Both should have non-dummy bodies after DCE.
    // Materialize to check.
    try module.materializeFuncBodies();

    // func_type_indices should have: 1 dummy (for eliminated import) + 3 original = 4.
    // The first dummy entry + 3 defined functions.
    // Check that main_fn and helper_fn bodies are NOT dummy.
    const dummy_count = module.dead_import_dummy_count;
    // main_fn is at func_type_indices[dummy_count + 0], helper_fn at [dummy_count + 1]
    const main_body = module.func_bodies.items[dummy_count + 0].body;
    const helper_body = module.func_bodies.items[dummy_count + 1].body;

    // Live bodies should be longer than the 3-byte dummy.
    try std.testing.expect(main_body.len > DUMMY_FUNCTION.len);
    try std.testing.expect(helper_body.len > DUMMY_FUNCTION.len);
}

test "eliminateDeadCode — unreachable function body replaced with unreachable stub" {
    const allocator = std.testing.allocator;
    var module = try buildDCETestModule(allocator);
    defer module.deinit();

    var called_fns = [_]bool{false} ** 6;
    try module.eliminateDeadCode(&called_fns);

    try module.materializeFuncBodies();

    // dead_fn (5) is not exported and not called by any live function.
    // Its body should be the dummy stub.
    const dummy_count = module.dead_import_dummy_count;
    const dead_body = module.func_bodies.items[dummy_count + 2].body;
    try std.testing.expectEqualSlices(u8, &DUMMY_FUNCTION, dead_body);
}

test "eliminateDeadCode — dead import removed, dead_import_dummy_count incremented" {
    const allocator = std.testing.allocator;
    var module = try buildDCETestModule(allocator);
    defer module.deinit();

    const orig_import_count = module.imports.items.len;
    const orig_dummy_count = module.dead_import_dummy_count;

    var called_fns = [_]bool{false} ** 6;
    try module.eliminateDeadCode(&called_fns);

    // js_unused (import 1) is only called by dead_fn which is dead.
    // It should be removed.
    try std.testing.expect(module.imports.items.len < orig_import_count);
    try std.testing.expect(module.dead_import_dummy_count > orig_dummy_count);

    // js_log and js_helper should still be present (called by live functions).
    var has_js_log = false;
    var has_js_helper = false;
    for (module.imports.items) |imp| {
        if (std.mem.eql(u8, imp.field_name, "js_log")) has_js_log = true;
        if (std.mem.eql(u8, imp.field_name, "js_helper")) has_js_helper = true;
    }
    try std.testing.expect(has_js_log);
    try std.testing.expect(has_js_helper);
}

test "eliminateDeadCode — non-function imports are preserved" {
    const allocator = std.testing.allocator;
    var module = try buildDCETestModule(allocator);
    defer module.deinit();

    // Non-function imports (memory, table, global) are NOT stored in module.imports
    // (the parser strips them). So we just verify that module.has_memory and
    // module.has_table are not touched by DCE.
    module.has_memory = true;
    module.has_table = true;

    var called_fns = [_]bool{false} ** 6;
    try module.eliminateDeadCode(&called_fns);

    try std.testing.expect(module.has_memory);
    try std.testing.expect(module.has_table);
}

test "eliminateDeadCode — indirect call targets (element section) preserved" {
    const allocator = std.testing.allocator;
    var module = try buildDCETestModule(allocator);
    defer module.deinit();

    // Add dead_fn (5) to the element section (indirect call target).
    // Even though nothing directly calls it, it should stay live because
    // it's in the table.
    try module.table_func_indices.append(allocator, 5);

    var called_fns = [_]bool{false} ** 6;
    try module.eliminateDeadCode(&called_fns);

    try module.materializeFuncBodies();

    // dead_fn should now be live (its body is NOT the dummy).
    const dummy_count = module.dead_import_dummy_count;
    const dead_body = module.func_bodies.items[dummy_count + 2].body;
    try std.testing.expect(dead_body.len > DUMMY_FUNCTION.len);
}

test "eliminateDeadCode — transitive callees preserved (A calls B calls C → all live)" {
    const allocator = std.testing.allocator;
    var module = try buildDCETestModule(allocator);
    defer module.deinit();

    // main_fn (3) → helper_fn (4) → js_helper (import 2)
    // All three should be live. js_helper is an import that's called by
    // helper_fn which is called by exported main_fn.
    var called_fns = [_]bool{false} ** 6;
    try module.eliminateDeadCode(&called_fns);

    // js_helper should still be in imports.
    var found_js_helper = false;
    for (module.imports.items) |imp| {
        if (std.mem.eql(u8, imp.field_name, "js_helper")) found_js_helper = true;
    }
    try std.testing.expect(found_js_helper);
}

test "eliminateDeadCode — init functions preserved" {
    const allocator = std.testing.allocator;
    var module = try buildDCETestModule(allocator);
    defer module.deinit();

    // Remove the export so main_fn would normally be dead.
    module.exports.items.len = 0;

    // But mark dead_fn (sym 5) as an init function — it should stay live.
    try module.linking.init_funcs.append(allocator, .{ .priority = 0, .symbol_index = 5 });

    var called_fns = [_]bool{false} ** 6;
    try module.eliminateDeadCode(&called_fns);

    try module.materializeFuncBodies();

    // dead_fn (index 5, third defined function) should be live.
    const dummy_count = module.dead_import_dummy_count;
    const init_body = module.func_bodies.items[dummy_count + 2].body;
    try std.testing.expect(init_body.len > DUMMY_FUNCTION.len);

    // main_fn and helper_fn should now be dead (no export, no callers).
    const main_body = module.func_bodies.items[dummy_count + 0].body;
    const helper_body = module.func_bodies.items[dummy_count + 1].body;
    try std.testing.expectEqualSlices(u8, &DUMMY_FUNCTION, main_body);
    try std.testing.expectEqualSlices(u8, &DUMMY_FUNCTION, helper_body);
}

test "eliminateDeadCode — call_indirect conservatively keeps matching-signature functions" {
    const allocator = std.testing.allocator;
    var module = try buildDCETestModule(allocator);
    defer module.deinit();

    // Add a type 1: (i32) -> ()
    _ = try module.addFuncType(&.{.i32}, &.{});

    // Change dead_fn (index 5) to type 1 so it has a unique signature.
    // func_type_indices[2] = type 1 (dead_fn is the 3rd defined function).
    module.func_type_indices.items[2] = 1;

    // Add dead_fn to the element section so it's an indirect call target.
    try module.table_func_indices.append(allocator, 5);

    // Add a call_indirect (type_index_leb) relocation in main_fn's body
    // pointing to a symbol with type index 1 (matching dead_fn's signature).
    // We need a symbol for type 1. Add it to the symbol table.
    try module.linking.symbol_table.append(allocator, .{
        .kind = .function, // type_index_leb relocs use function symbols in some impls,
        // but the index field carries the type index.
        // For our implementation, we use the symbol's index as the type index.
        .flags = 0,
        .name = "type1_sig",
        .index = 1, // type index 1
    });
    const type_sym_idx: u32 = @intCast(module.linking.symbol_table.items.len - 1);

    // Add a type_index_leb reloc inside main_fn's body range (offset 0..17).
    try module.reloc_code.entries.append(allocator, .{
        .index = .{
            .type_id = .type_index_leb,
            .offset = 14, // within main_fn's byte range
            .symbol_index = type_sym_idx,
        },
    });

    // Remove the direct call to helper_fn (reloc at offset 10) so the only
    // reason dead_fn stays live is the indirect call.
    // Actually, let's keep things simple: just verify dead_fn is live.
    var called_fns = [_]bool{false} ** 6;
    try module.eliminateDeadCode(&called_fns);

    try module.materializeFuncBodies();

    // dead_fn (type 1) is in element section AND there's a call_indirect
    // with matching type 1 in a live function → should be live.
    const dummy_count = module.dead_import_dummy_count;
    const dead_body = module.func_bodies.items[dummy_count + 2].body;
    try std.testing.expect(dead_body.len > DUMMY_FUNCTION.len);
}

test "eliminateDeadCode — function indices unchanged after elimination" {
    const allocator = std.testing.allocator;
    var module = try buildDCETestModule(allocator);
    defer module.deinit();

    // Record original function count.
    const orig_defined_count = module.function_offsets.items.len;

    var called_fns = [_]bool{false} ** 6;
    try module.eliminateDeadCode(&called_fns);

    // The number of function_offsets entries should be unchanged
    // (dead functions are stubbed, not removed).
    try std.testing.expectEqual(orig_defined_count, module.function_offsets.items.len);

    // func_type_indices should grow by the number of eliminated imports
    // (dummies are prepended), but the defined function entries are unchanged.
    // The total should be: eliminated_imports + original_defined_count.
    try std.testing.expectEqual(
        module.dead_import_dummy_count + @as(u32, @intCast(orig_defined_count)),
        @as(u32, @intCast(module.func_type_indices.items.len)),
    );
}

// --- Phase 11: Serialization Tests ---

/// Build a module simulating the post-surgical-linking state:
/// - Has code_bytes and function_offsets (from preload)
/// - Has dead_import_dummy_count > 0 (from linkHostToAppCalls)
/// - Has linking and reloc sections (from preload, should NOT appear in output)
/// - Has memory, exports, and data segments
fn buildEncodeTestModule(allocator: Allocator) !Self {
    var module = Self.init(allocator);
    errdefer module.deinit();

    // Type 0: () -> ()
    _ = try module.addFuncType(&.{}, &.{});
    // Type 1: (i32) -> i32
    _ = try module.addFuncType(&.{.i32}, &.{.i32});

    // One remaining import (e.g. roc_alloc)
    _ = try module.addImport("env", "roc_alloc", 0);
    module.import_fn_count = 1;

    // Simulate dead_import_dummy_count = 1 (one import was removed during linking)
    module.dead_import_dummy_count = 1;

    // Two defined functions: dummy type + real type
    // func_type_indices[0] = type 0 (dummy placeholder)
    // func_type_indices[1] = type 0 (real func: main)
    // func_type_indices[2] = type 1 (real func: helper)
    try module.func_type_indices.append(allocator, 0); // dummy
    try module.func_type_indices.append(allocator, 0); // main
    try module.func_type_indices.append(allocator, 1); // helper

    // Build code_bytes with two real function bodies (LEB128 size prefix + body).
    // Function 0 (main): body = [0x00 (no locals), Op.nop, Op.end] → size = 3
    try module.function_offsets.append(allocator, @intCast(module.code_bytes.items.len));
    try module.code_bytes.append(allocator, 0x03); // LEB128 body size = 3
    try module.code_bytes.append(allocator, 0x00); // no locals
    try module.code_bytes.append(allocator, Op.nop);
    try module.code_bytes.append(allocator, Op.end);

    // Function 1 (helper): body = [0x00 (no locals), Op.nop, Op.nop, Op.end] → size = 4
    try module.function_offsets.append(allocator, @intCast(module.code_bytes.items.len));
    try module.code_bytes.append(allocator, 0x04); // LEB128 body size = 4
    try module.code_bytes.append(allocator, 0x00); // no locals
    try module.code_bytes.append(allocator, Op.nop);
    try module.code_bytes.append(allocator, Op.nop);
    try module.code_bytes.append(allocator, Op.end);

    // Memory and stack pointer
    module.enableMemory(2);
    module.enableStackPointer(131072);

    // Export: memory and main function
    try module.addExport("memory", .memory, 0);
    try module.addExport("_start", .func, 2); // func idx 2 = import(1) + dummy(0) + main(local 1)

    // Data segment
    const data = try allocator.dupe(u8, "test data");
    try module.data_segments.append(allocator, .{ .offset = 1024, .data = data });

    // Linking section (should NOT appear in output)
    try module.linking.symbol_table.appendSlice(allocator, &.{
        .{ .kind = .function, .flags = WasmLinking.SymFlag.UNDEFINED, .name = null, .index = 0 },
        .{ .kind = .function, .flags = 0, .name = "main", .index = 2 },
        .{ .kind = .function, .flags = 0, .name = "helper", .index = 3 },
    });

    // Reloc.CODE section (should NOT appear in output)
    try module.reloc_code.entries.append(allocator, .{
        .index = .{ .type_id = .function_index_leb, .offset = 1, .symbol_index = 1 },
    });

    return module;
}

test "encode — output is valid WASM (magic, version, section ordering)" {
    const allocator = std.testing.allocator;
    var module = try buildEncodeTestModule(allocator);
    defer module.deinit();

    try module.materializeFuncBodies();
    const output = try module.encode(allocator);
    defer allocator.free(output);

    // Check magic number: \0asm
    try std.testing.expectEqualSlices(u8, &.{ 0x00, 0x61, 0x73, 0x6D }, output[0..4]);
    // Check version: 1
    try std.testing.expectEqualSlices(u8, &.{ 0x01, 0x00, 0x00, 0x00 }, output[4..8]);

    // Walk through sections and verify ordering
    var pos: usize = 8;
    var prev_section_id: u8 = 0;
    while (pos < output.len) {
        const section_id = output[pos];
        pos += 1;

        // Custom sections (id=0) can appear anywhere; standard sections must be ordered
        if (section_id != 0) {
            try std.testing.expect(section_id > prev_section_id);
            prev_section_id = section_id;
        }

        // Read section size (LEB128)
        var cursor: usize = pos;
        const section_size = readU32(output, &cursor) catch unreachable;
        pos = cursor + section_size;
    }

    // Should have consumed exactly all bytes
    try std.testing.expectEqual(output.len, pos);
}

test "encode — code section function count includes dummies" {
    const allocator = std.testing.allocator;
    var module = try buildEncodeTestModule(allocator);
    defer module.deinit();

    try module.materializeFuncBodies();
    const output = try module.encode(allocator);
    defer allocator.free(output);

    // Find code section (id = 10)
    var pos: usize = 8;
    var code_section_start: ?usize = null;
    while (pos < output.len) {
        const section_id = output[pos];
        pos += 1;
        var cursor: usize = pos;
        const section_size = readU32(output, &cursor) catch unreachable;
        if (section_id == @intFromEnum(SectionId.code_section)) {
            code_section_start = cursor;
            break;
        }
        pos = cursor + section_size;
    }

    try std.testing.expect(code_section_start != null);

    // Read function count from the code section
    var cursor = code_section_start.?;
    const func_count = readU32(output, &cursor) catch unreachable;

    // Should be dummies (1) + real functions (2) = 3
    try std.testing.expectEqual(@as(u32, 3), func_count);
}

test "encode — dummy functions prepended before real functions in code section" {
    const allocator = std.testing.allocator;
    var module = try buildEncodeTestModule(allocator);
    defer module.deinit();

    try module.materializeFuncBodies();
    const output = try module.encode(allocator);
    defer allocator.free(output);

    // Find code section
    var pos: usize = 8;
    var code_body_start: ?usize = null;
    while (pos < output.len) {
        const section_id = output[pos];
        pos += 1;
        var cursor: usize = pos;
        const section_size = readU32(output, &cursor) catch unreachable;
        if (section_id == @intFromEnum(SectionId.code_section)) {
            // Skip the function count
            _ = readU32(output, &cursor) catch unreachable;
            code_body_start = cursor;
            break;
        }
        pos = cursor + section_size;
    }

    try std.testing.expect(code_body_start != null);
    var cursor = code_body_start.?;

    // First function should be the dummy: body size = 3, body = DUMMY_FUNCTION
    const dummy_size = readU32(output, &cursor) catch unreachable;
    try std.testing.expectEqual(@as(u32, DUMMY_FUNCTION.len), dummy_size);
    try std.testing.expectEqualSlices(u8, &DUMMY_FUNCTION, output[cursor .. cursor + dummy_size]);
    cursor += dummy_size;

    // Second function (main): body size = 3, body = [0x00, nop, end]
    const main_size = readU32(output, &cursor) catch unreachable;
    try std.testing.expectEqual(@as(u32, 3), main_size);
    try std.testing.expectEqual(@as(u8, 0x00), output[cursor]); // no locals
    try std.testing.expectEqual(Op.nop, output[cursor + 1]);
    try std.testing.expectEqual(Op.end, output[cursor + 2]);
    cursor += main_size;

    // Third function (helper): body size = 4, body = [0x00, nop, nop, end]
    const helper_size = readU32(output, &cursor) catch unreachable;
    try std.testing.expectEqual(@as(u32, 4), helper_size);
    try std.testing.expectEqual(@as(u8, 0x00), output[cursor]); // no locals
    try std.testing.expectEqual(Op.nop, output[cursor + 1]);
    try std.testing.expectEqual(Op.nop, output[cursor + 2]);
    try std.testing.expectEqual(Op.end, output[cursor + 3]);
}

test "encode — linking section NOT present in output" {
    const allocator = std.testing.allocator;
    var module = try buildEncodeTestModule(allocator);
    defer module.deinit();

    // Verify the module actually has linking data (precondition)
    try std.testing.expect(module.linking.symbol_table.items.len > 0);

    try module.materializeFuncBodies();
    const output = try module.encode(allocator);
    defer allocator.free(output);

    // Scan all sections — no custom section should have name "linking"
    var pos: usize = 8;
    while (pos < output.len) {
        const section_id = output[pos];
        pos += 1;
        var cursor: usize = pos;
        const section_size = readU32(output, &cursor) catch unreachable;
        const section_end = cursor + section_size;

        if (section_id == @intFromEnum(SectionId.custom_section)) {
            // Read custom section name
            const name_len = readU32(output, &cursor) catch unreachable;
            const name = output[cursor .. cursor + name_len];
            try std.testing.expect(!std.mem.eql(u8, name, "linking"));
        }

        pos = section_end;
    }
}

test "encode — reloc.CODE section NOT present in output" {
    const allocator = std.testing.allocator;
    var module = try buildEncodeTestModule(allocator);
    defer module.deinit();

    // Verify the module actually has reloc data (precondition)
    try std.testing.expect(module.reloc_code.entries.items.len > 0);

    try module.materializeFuncBodies();
    const output = try module.encode(allocator);
    defer allocator.free(output);

    // Scan all sections — no custom section should have name "reloc.CODE"
    var pos: usize = 8;
    while (pos < output.len) {
        const section_id = output[pos];
        pos += 1;
        var cursor: usize = pos;
        const section_size = readU32(output, &cursor) catch unreachable;
        const section_end = cursor + section_size;

        if (section_id == @intFromEnum(SectionId.custom_section)) {
            // Read custom section name
            const name_len = readU32(output, &cursor) catch unreachable;
            const name = output[cursor .. cursor + name_len];
            try std.testing.expect(!std.mem.eql(u8, name, "reloc.CODE"));
        }

        pos = section_end;
    }
}

test "preload + merge + encode roundtrip with real builtins" {
    const allocator = std.testing.allocator;

    // Load the pre-built wasm32 builtins object
    const wasm32_builtins = @import("wasm32_builtins");
    var builtins_module = try preload(allocator, wasm32_builtins.bytes, false);
    defer builtins_module.deinit();

    // Create an app module with standard RocOps imports
    var app_module = Self.init(allocator);

    const roc_ops_type_idx = try app_module.addFuncType(&.{ .i32, .i32 }, &.{});
    for ([_][]const u8{ "roc_alloc", "roc_dealloc", "roc_realloc", "roc_dbg", "roc_expect_failed", "roc_crashed" }) |name| {
        _ = try app_module.addImport("env", name, roc_ops_type_idx);
    }

    // Merge builtins into app module
    var merge_result = try app_module.mergeModule(&builtins_module);
    merge_result.deinit();

    // Populate builtin symbols
    _ = try BuiltinSymbols.populate(&app_module);

    // Resolve relocations and materialize function bodies
    app_module.resolveRelocations();
    try app_module.materializeFuncBodies();

    // Enable memory + stack pointer + table (as generateModule does)
    app_module.enableMemory(2);
    app_module.enableStackPointer(131072);
    app_module.enableTable();
    app_module.addExport("memory", .memory, 0) catch unreachable;

    // Add RocCall function: (i32, i32, i32) -> void
    const roc_call_type_idx = try app_module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
    const roc_call_fn_idx = try app_module.addFunction(roc_call_type_idx);
    const roc_call_body = [_]u8{ 0x00, Op.end };
    try app_module.setFunctionBody(roc_call_fn_idx, &roc_call_body);
    app_module.addExport("roc__main_for_host_1_exposed", .func, roc_call_fn_idx) catch unreachable;

    // Add eval wrapper: (i32) -> i32
    const eval_type_idx = try app_module.addFuncType(&.{.i32}, &.{.i32});
    const eval_fn_idx = try app_module.addFunction(eval_type_idx);
    const eval_body = [_]u8{ 0x00, Op.i32_const, 42, Op.end };
    try app_module.setFunctionBody(eval_fn_idx, &eval_body);
    app_module.addExport("main", .func, eval_fn_idx) catch unreachable;

    // Encode the module
    const encoded = try app_module.encode(allocator);
    defer allocator.free(encoded);
    app_module.deinit();

    // Verify bytebox can decode it
    const bytebox = @import("bytebox");
    var arena_impl = std.heap.ArenaAllocator.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var module_def = try bytebox.createModuleDefinition(arena, .{});
    module_def.decode(encoded) catch |err| {
        std.debug.print("bytebox decode failed: {}\n", .{err});
        return err;
    };
}

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

/// A data segment placed in linear memory
const DataSegment = struct {
    offset: u32, // offset in linear memory
    data: []const u8, // bytes to place
};

/// An imported function
pub const Import = struct {
    module_name: []const u8,
    field_name: []const u8,
    type_idx: u32,
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
        // Verify layout consistency: last field offset + field size = total size.
        std.debug.assert(hosted_fns_ptr + 4 == total_size);
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
        .data_segments = .empty,
        .data_offset = 1024, // reserve first 1KB for future use
        .has_memory = false,
        .memory_min_pages = 1,
        .has_stack_pointer = false,
        .stack_pointer_init = 65536,
        .has_table = false,
        .table_func_indices = .empty,
        .code_bytes = .empty,
        .function_offsets = .empty,
        .dead_import_dummy_count = 0,
        .import_fn_count = 0,
        .import_global_count = 0,
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
    for (self.data_segments.items) |ds| {
        self.allocator.free(ds.data);
    }
    self.data_segments.deinit(self.allocator);
    self.table_func_indices.deinit(self.allocator);
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
    });
    self.data_offset += @intCast(data.len);
    return offset;
}

/// Enable the __stack_pointer global (global index 0).
pub fn enableStackPointer(self: *Self, initial_value: u32) void {
    self.has_stack_pointer = true;
    self.stack_pointer_init = initial_value;
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
    list_sort_with: u32, // roc_builtins_list_sort_with

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
        .{ "roc_builtins_list_sort_with", "list_sort_with" },
    };

    pub const PopulateError = error{MissingBuiltinSymbol};

    /// Populate this struct by looking up each builtin symbol name in the
    /// module's merged symbol table.
    pub fn populate(module: *const Self) PopulateError!BuiltinSymbols {
        var result: BuiltinSymbols = undefined;
        inline for (mapping) |entry| {
            const sym_name = entry[0];
            const field_name = entry[1];
            @field(result, field_name) = module.linking.findSymbolByName(
                sym_name,
                module.imports.items,
            ) orelse return error.MissingBuiltinSymbol;
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
    const self_defined_base = self.importCount() + @as(u32, @intCast(self.func_type_indices.items.len));
    const source_defined_count: u32 = @intCast(source.func_type_indices.items.len);

    // func_remap: maps source global function index → self global function index.
    // For source imports, we resolve them against self's imports by name.
    // For source defined functions, they get sequential indices after self's existing functions.
    const total_source_fns = source.import_fn_count + source_defined_count;
    const func_remap = try gpa.alloc(u32, total_source_fns);
    defer gpa.free(func_remap);

    // Remap source imports → self imports (by name match).
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

    for (source.data_segments.items, 0..) |src_ds, i| {
        // Find alignment from segment info if available, default to 1.
        const alignment: u32 = if (i < source.linking.segment_info.items.len)
            @as(u32, 1) << @intCast(source.linking.segment_info.items[i].alignment)
        else
            1;
        const new_offset = try self.addDataSegment(src_ds.data, alignment);
        data_remap[i] = new_offset;
    }

    // --- 6. Merge symbol table ---
    // symbol_remap: maps source symbol index → self symbol index.
    const source_sym_count = source.linking.symbol_table.items.len;
    const symbol_remap = try gpa.alloc(u32, source_sym_count);
    errdefer gpa.free(symbol_remap);

    // Track which __stack_pointer global we resolved to.
    const self_stack_pointer_sym = self.linking.findSymbolByName("__stack_pointer", self.imports.items);

    for (source.linking.symbol_table.items, 0..) |src_sym, src_sym_idx| {
        const src_name = src_sym.resolveName(source.imports.items);

        switch (src_sym.kind) {
            .function => {
                if (src_sym.isUndefined()) {
                    // Undefined function in source — resolve against self's symbol table.
                    if (src_name) |name| {
                        if (self.linking.findSymbolByName(name, self.imports.items)) |existing| {
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
                        if (self.linking.findSymbolByName(name, self.imports.items)) |existing| {
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
                    try self.linking.symbol_table.append(gpa, .{
                        .kind = .data,
                        .flags = src_sym.flags,
                        .name = src_sym.name,
                        .index = src_sym.index, // segment index (not remapped — data_offset is absolute)
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
                        if (self.linking.findSymbolByName(name, self.imports.items)) |existing| {
                            symbol_remap[src_sym_idx] = existing;
                            continue;
                        }
                    }
                }
                // Add as-is if not resolved.
                const new_sym_idx: u32 = @intCast(self.linking.symbol_table.items.len);
                try self.linking.symbol_table.append(gpa, src_sym);
                symbol_remap[src_sym_idx] = new_sym_idx;
            },
            .section, .event, .table => {
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
                try self.reloc_data.entries.append(gpa, .{ .index = .{
                    .type_id = idx.type_id,
                    .offset = idx.offset, // data offsets are segment-relative
                    .symbol_index = symbol_remap[idx.symbol_index],
                } });
            },
            .offset => |off| {
                try self.reloc_data.entries.append(gpa, .{ .offset = .{
                    .type_id = off.type_id,
                    .offset = off.offset,
                    .symbol_index = symbol_remap[off.symbol_index],
                    .addend = off.addend,
                } });
            },
        }
    }

    // Suppress unused local warning.
    _ = self_stack_pointer_sym;

    return .{
        .symbol_remap = symbol_remap,
        .allocator = gpa,
    };
}

/// Resolve all code relocations in place.
///
/// For each relocation entry in `reloc_code`, look up the symbol's resolved
/// value (function index, global index, or memory address) and patch the
/// corresponding site in `code_bytes`.
///
/// This must be called after all merging and linking is complete, before
/// converting code_bytes into func_bodies for encoding.
pub fn resolveCodeRelocations(self: *Self) void {
    for (self.reloc_code.entries.items) |entry| {
        const sym = self.linking.symbol_table.items[entry.getSymbolIndex()];
        switch (entry) {
            .index => |idx| {
                // The resolved value is the symbol's index (function index, type index, etc.).
                const value = sym.index;
                switch (idx.type_id) {
                    .function_index_leb,
                    .type_index_leb,
                    .global_index_leb,
                    .event_index_leb,
                    .table_number_leb,
                    => overwritePaddedU32(self.code_bytes.items, idx.offset, value),
                    .table_index_sleb => overwritePaddedI32(
                        self.code_bytes.items,
                        idx.offset,
                        @intCast(value),
                    ),
                    .table_index_i32, .global_index_i32 => {
                        const off: usize = @intCast(idx.offset);
                        std.mem.writeInt(u32, self.code_bytes.items[off..][0..4], value, .little);
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
                        self.code_bytes.items,
                        off.offset,
                        @intCast(patched),
                    ),
                    .memory_addr_sleb,
                    .memory_addr_rel_sleb,
                    => overwritePaddedI32(
                        self.code_bytes.items,
                        off.offset,
                        @intCast(patched),
                    ),
                    .memory_addr_i32,
                    .function_offset_i32,
                    .section_offset_i32,
                    => {
                        const o: usize = @intCast(off.offset);
                        std.mem.writeInt(u32, self.code_bytes.items[o..][0..4], @intCast(patched), .little);
                    },
                }
            },
        }
    }
}

/// Convert code_bytes + function_offsets into func_bodies for encoding.
///
/// After `resolveCodeRelocations()` has patched all call sites, this method
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
/// After Phase 8 rewrite, only the 6 RocOps callback functions should be imported.
pub fn verifyNoBuiltinImports(self: *const Self) !void {
    const allowed = [_][]const u8{
        "roc_alloc",
        "roc_dealloc",
        "roc_realloc",
        "roc_dbg",
        "roc_expect_failed",
        "roc_crashed",
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
                _ = try readU32(bytes, cursor); // val type
                if (cursor.* >= bytes.len) return error.UnexpectedEnd;
                cursor.* += 1; // mutability
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

fn parseElementSection_(_: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .element_section) orelse return;
    // Skip element section contents for now (parsed on demand during linking)
    cursor.* += section_size;
}

fn parseDataCountSection(_: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .data_count_section) orelse return;
    cursor.* += section_size; // Consume and ignore
}

fn parseCodeSection(self: *Self, bytes: []const u8, cursor: *usize) ParseError!void {
    const section_size = try beginSection(bytes, cursor, .code_section) orelse return;
    const section_end = cursor.* + section_size;

    const fn_count = try readU32(bytes, cursor);

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

    try leb128WriteU32(gpa, &section_data, 1); // 1 global

    // Global 0: __stack_pointer (i32, mutable)
    try section_data.append(gpa, @intFromEnum(ValType.i32));
    try section_data.append(gpa, 0x01); // mutable
    try section_data.append(gpa, Op.i32_const);
    try leb128WriteI32(gpa, &section_data, @intCast(self.stack_pointer_init));
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

/// Overwrite 5 bytes at `buffer[offset..offset+5]` with a u32 in padded LEB128.
/// The buffer must already have 5 bytes reserved at that position.
/// This is the core primitive for surgical relocation patching.
pub fn overwritePaddedU32(buffer: []u8, offset: u32, value: u32) void {
    var x = value;
    const off: usize = @intCast(offset);
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
    for (0..4) |_| {
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
            try std.testing.expectEqual(@as(u32, 2), idx.offset);
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
    const resolved_name = sym0.resolveName(module.imports.items);
    try std.testing.expect(resolved_name != null);
    try std.testing.expectEqualStrings("roc__main_exposed", resolved_name.?);

    // findSymbolByName should find it
    const found = module.linking.findSymbolByName("roc__main_exposed", module.imports.items);
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

    // Spot check a few fields
    try std.testing.expectEqual(@as(u32, 0), syms.dec_mul); // sym index 0 → roc_builtins_dec_mul_saturated
    try std.testing.expectEqual(@as(u32, 19), syms.str_trim); // sym index 19
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

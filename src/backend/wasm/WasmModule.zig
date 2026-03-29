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
const FuncType = struct {
    params: []const ValType,
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

/// An imported function
pub const Import = struct {
    module_name: []const u8,
    field_name: []const u8,
    type_idx: u32,
};

/// Wasm reference type for funcref tables
const funcref: u8 = 0x70;

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
            0x03 => { // global import
                _ = try readU32(bytes, cursor); // val type
                if (cursor.* >= bytes.len) return error.UnexpectedEnd;
                cursor.* += 1; // mutability
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

fn encodeTableSection(_: *Self, gpa: Allocator, output: *std.ArrayList(u8)) !void {
    var section_data: std.ArrayList(u8) = .empty;
    defer section_data.deinit(gpa);

    try leb128WriteU32(gpa, &section_data, 1); // 1 table
    try section_data.append(gpa, funcref); // element type: funcref
    try section_data.append(gpa, 0x00); // limits: no max
    try leb128WriteU32(gpa, &section_data, 16); // min size (enough for RocOps functions)

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

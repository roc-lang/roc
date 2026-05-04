//! LIR -> WebAssembly code generator.
//!
//! Walks LIR expressions and emits wasm instructions. Each `generateExpr`
//! call leaves the result on the wasm value stack (for primitives) or writes
//! to linear memory (for composites).

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const layout = @import("layout");

const lir = @import("lir");
const LIR = lir.LIR;
const LirExprStore = lir.LirExprStore;
const LirExpr = LIR.LirExpr;
const LirExprId = LIR.LirExprId;
const LirPattern = LIR.LirPattern;
const Symbol = LIR.Symbol;
const WasmModule = @import("WasmModule.zig");
const WasmLayout = @import("WasmLayout.zig");
const Storage = @import("Storage.zig");
const Op = WasmModule.Op;
const ValType = WasmModule.ValType;
const BlockType = WasmModule.BlockType;

const LirProcSpec = LIR.LirProcSpec;
const CFStmtId = LIR.CFStmtId;
const RcOpKind = enum { incref, decref, free };

const LayoutStore = layout.Store;

const Self = @This();

allocator: Allocator,
store: *const LirExprStore,
layout_store: *const LayoutStore,
module: WasmModule,
body: std.ArrayList(u8), // instruction bytes for current function
storage: Storage,
/// Accumulated stack frame size for the current function (for stack memory values).
stack_frame_size: u32 = 0,
/// Whether the current function uses stack memory (needs prologue/epilogue).
uses_stack_memory: bool = false,
/// Local index of the frame pointer ($fp) - only valid when uses_stack_memory is true.
fp_local: u32 = 0,
/// Map from proc symbol key → compiled wasm function index (for LirProcSpec compilation).
registered_procs: std.AutoHashMap(u64, u32),
/// Type index for the RocOps function signature: (i32, i32) -> void.
roc_ops_type_idx: u32 = 0,
/// Table indices for RocOps functions (used with call_indirect).
roc_alloc_table_idx: u32 = 0,
roc_dealloc_table_idx: u32 = 0,
roc_realloc_table_idx: u32 = 0,
roc_dbg_table_idx: u32 = 0,
roc_expect_failed_table_idx: u32 = 0,
roc_crashed_table_idx: u32 = 0,
/// Local index holding the roc_ops_ptr (pointer to RocOps struct in linear memory).
/// In main(), this is a local storing the constant 0 (struct at memory offset 0).
/// In compiled functions, this is parameter 0.
roc_ops_local: u32 = 0,
/// CFStmt block nesting depth (for br targets in proc compilation).
cf_depth: u32 = 0,
/// Expression-level structured control depth (for break_expr branch depths).
expr_control_depth: u32 = 0,
/// Whether we're currently generating code inside a proc body.
in_proc: bool = false,
/// Map from JoinPointId → loop depth (for jump → br targeting).
join_point_depths: std.AutoHashMap(u32, u32),
/// Map from JoinPointId → param local indices.
join_point_param_locals: std.AutoHashMap(u32, []u32),
/// Stack of expression-level loop exit label depths for lowering break_expr.
loop_break_target_depths: std.ArrayList(u32),
/// Wasm function index for imported roc_dec_mul host function.
dec_mul_import: ?u32 = null,
/// Wasm function index for imported roc_dec_to_str host function.
dec_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_str_eq host function.
str_eq_import: ?u32 = null,
/// Wasm function index for imported roc_list_eq host function.
list_eq_import: ?u32 = null,
/// Wasm function index for imported roc_i128_div_s host function.
i128_div_s_import: ?u32 = null,
/// Wasm function index for imported roc_i128_mod_s host function.
i128_mod_s_import: ?u32 = null,
/// Wasm function index for imported roc_i32_mod_by host function.
i32_mod_by_import: ?u32 = null,
/// Wasm function index for imported roc_i64_mod_by host function.
i64_mod_by_import: ?u32 = null,
/// Wasm function index for imported roc_u128_div host function.
u128_div_import: ?u32 = null,
/// Wasm function index for imported roc_u128_mod host function.
u128_mod_import: ?u32 = null,
/// Wasm function index for imported roc_dec_div host function.
dec_div_import: ?u32 = null,
/// Wasm function index for imported roc_dec_div_trunc host function.
dec_div_trunc_import: ?u32 = null,
/// Wasm function index for imported roc_i128_to_str host function.
i128_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_u128_to_str host function.
u128_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_float_to_str host function.
float_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_u128_to_dec host function.
u128_to_dec_import: ?u32 = null,
/// Wasm function index for imported roc_i128_to_dec host function.
i128_to_dec_import: ?u32 = null,
/// Wasm function index for imported roc_dec_to_i128 host function.
dec_to_i128_import: ?u32 = null,
/// Wasm function index for imported roc_dec_to_u128 host function.
dec_to_u128_import: ?u32 = null,
/// Wasm function index for imported roc_dec_to_f32 host function.
dec_to_f32_import: ?u32 = null,
/// Wasm function index for imported roc_list_str_eq host function.
list_str_eq_import: ?u32 = null,
/// Wasm function index for imported roc_list_list_eq host function.
list_list_eq_import: ?u32 = null,
str_repeat_import: ?u32 = null,
str_concat_import: ?u32 = null,
str_trim_import: ?u32 = null,
str_trim_start_import: ?u32 = null,
str_trim_end_import: ?u32 = null,
str_split_import: ?u32 = null,
str_join_with_import: ?u32 = null,
str_reserve_import: ?u32 = null,
str_release_excess_capacity_import: ?u32 = null,
str_with_capacity_import: ?u32 = null,
str_drop_prefix_import: ?u32 = null,
str_drop_suffix_import: ?u32 = null,
str_with_ascii_lowercased_import: ?u32 = null,
str_with_ascii_uppercased_import: ?u32 = null,
str_caseless_ascii_equals_import: ?u32 = null,
str_from_utf8_import: ?u32 = null,
int_from_str_import: ?u32 = null,
dec_from_str_import: ?u32 = null,
float_from_str_import: ?u32 = null,
list_append_unsafe_import: ?u32 = null,
list_sort_with_import: ?u32 = null,
list_reverse_import: ?u32 = null,
/// Configurable wasm stack size in bytes (default 1MB).
wasm_stack_bytes: u32 = 1024 * 1024,
/// Configurable wasm memory pages (0 = auto-compute from stack size).
wasm_memory_pages: u32 = 0,
pub fn init(allocator: Allocator, store: *const LirExprStore, layout_store: *const LayoutStore) Self {
    return .{
        .allocator = allocator,
        .store = store,
        .layout_store = layout_store,
        .module = WasmModule.init(allocator),
        .body = .empty,
        .storage = Storage.init(allocator),
        .stack_frame_size = 0,
        .uses_stack_memory = false,
        .fp_local = 0,
        .registered_procs = std.AutoHashMap(u64, u32).init(allocator),
        .join_point_depths = std.AutoHashMap(u32, u32).init(allocator),
        .join_point_param_locals = std.AutoHashMap(u32, []u32).init(allocator),
        .loop_break_target_depths = .empty,
    };
}

pub fn deinit(self: *Self) void {
    self.module.deinit();
    self.body.deinit(self.allocator);
    self.storage.deinit();
    self.registered_procs.deinit();
    self.join_point_depths.deinit();
    // Free allocated param local arrays
    var jp_it = self.join_point_param_locals.iterator();
    while (jp_it.next()) |entry| {
        self.allocator.free(entry.value_ptr.*);
    }
    self.join_point_param_locals.deinit();
    self.loop_break_target_depths.deinit(self.allocator);
}

/// Register host function imports. Must be called before any addFunction calls
/// because wasm imports must come before locally-defined functions.
fn registerHostImports(self: *Self) !void {
    // roc_dec_mul: (i32 lhs_ptr, i32 rhs_ptr, i32 result_ptr) -> void
    // Takes pointers to 16-byte Dec values in linear memory,
    // stores the result at result_ptr.
    const dec_mul_type = try self.module.addFuncType(
        &.{ .i32, .i32, .i32 },
        &.{},
    );
    self.dec_mul_import = try self.module.addImport("env", "roc_dec_mul", dec_mul_type);

    // roc_dec_to_str: (i32 dec_ptr, i32 buf_ptr) -> i32 str_len
    // Reads 16-byte Dec value from dec_ptr, formats it as a string,
    // writes the string bytes to buf_ptr, returns the length.
    const dec_to_str_type = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{.i32},
    );
    self.dec_to_str_import = try self.module.addImport("env", "roc_dec_to_str", dec_to_str_type);

    // roc_str_eq: (i32 str_a_ptr, i32 str_b_ptr) -> i32 (0 or 1)
    // Compares two 12-byte RocStr structs for content equality.
    const str_eq_type = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{.i32},
    );
    self.str_eq_import = try self.module.addImport("env", "roc_str_eq", str_eq_type);

    // roc_list_eq: (i32 list_a_ptr, i32 list_b_ptr, i32 elem_size) -> i32
    // Compares two 12-byte RocList structs for content equality (byte-wise comparison of elements).
    const list_eq_type = try self.module.addFuncType(
        &.{ .i32, .i32, .i32 },
        &.{.i32},
    );
    self.list_eq_import = try self.module.addImport("env", "roc_list_eq", list_eq_type);

    // RocOps function imports: all have signature (i32 args_ptr, i32 env_ptr) -> void
    // These are called via call_indirect through the funcref table.
    const roc_ops_type = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{},
    );
    self.roc_ops_type_idx = roc_ops_type;

    // Enable table and add each RocOps function as a table element
    self.module.enableTable();

    const roc_alloc_idx = try self.module.addImport("env", "roc_alloc", roc_ops_type);
    self.roc_alloc_table_idx = try self.module.addTableElement(roc_alloc_idx);

    const roc_dealloc_idx = try self.module.addImport("env", "roc_dealloc", roc_ops_type);
    self.roc_dealloc_table_idx = try self.module.addTableElement(roc_dealloc_idx);

    const roc_realloc_idx = try self.module.addImport("env", "roc_realloc", roc_ops_type);
    self.roc_realloc_table_idx = try self.module.addTableElement(roc_realloc_idx);

    const roc_dbg_idx = try self.module.addImport("env", "roc_dbg", roc_ops_type);
    self.roc_dbg_table_idx = try self.module.addTableElement(roc_dbg_idx);

    const roc_expect_failed_idx = try self.module.addImport("env", "roc_expect_failed", roc_ops_type);
    self.roc_expect_failed_table_idx = try self.module.addTableElement(roc_expect_failed_idx);

    const roc_crashed_idx = try self.module.addImport("env", "roc_crashed", roc_ops_type);
    self.roc_crashed_table_idx = try self.module.addTableElement(roc_crashed_idx);

    // i128/u128 division and modulo host functions
    // All take (lhs_ptr, rhs_ptr, result_ptr) -> void
    const i128_binop_type = try self.module.addFuncType(
        &.{ .i32, .i32, .i32 },
        &.{},
    );
    self.i128_div_s_import = try self.module.addImport("env", "roc_i128_div_s", i128_binop_type);
    self.i128_mod_s_import = try self.module.addImport("env", "roc_i128_mod_s", i128_binop_type);
    self.u128_div_import = try self.module.addImport("env", "roc_u128_div", i128_binop_type);
    self.u128_mod_import = try self.module.addImport("env", "roc_u128_mod", i128_binop_type);
    self.dec_div_import = try self.module.addImport("env", "roc_dec_div", i128_binop_type);
    self.dec_div_trunc_import = try self.module.addImport("env", "roc_dec_div_trunc", i128_binop_type);

    const i32_mod_by_type = try self.module.addFuncType(&.{ .i32, .i32 }, &.{.i32});
    self.i32_mod_by_import = try self.module.addImport("env", "roc_i32_mod_by", i32_mod_by_type);

    const i64_mod_by_type = try self.module.addFuncType(&.{ .i64, .i64 }, &.{.i64});
    self.i64_mod_by_import = try self.module.addImport("env", "roc_i64_mod_by", i64_mod_by_type);

    // i128/u128 to string: (val_ptr, buf_ptr) -> i32 str_len
    const i128_to_str_type = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{.i32},
    );
    self.i128_to_str_import = try self.module.addImport("env", "roc_i128_to_str", i128_to_str_type);
    self.u128_to_str_import = try self.module.addImport("env", "roc_u128_to_str", i128_to_str_type);

    const float_to_str_type = try self.module.addFuncType(
        &.{ .i64, .i32, .i32 },
        &.{.i32},
    );
    self.float_to_str_import = try self.module.addImport("env", "roc_float_to_str", float_to_str_type);

    // 128-bit ↔ Dec conversions: (val_ptr, result_ptr) -> i32 (success flag)
    const i128_dec_conv_type = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{.i32},
    );
    self.u128_to_dec_import = try self.module.addImport("env", "roc_u128_to_dec", i128_dec_conv_type);
    self.i128_to_dec_import = try self.module.addImport("env", "roc_i128_to_dec", i128_dec_conv_type);
    self.dec_to_i128_import = try self.module.addImport("env", "roc_dec_to_i128", i128_dec_conv_type);
    self.dec_to_u128_import = try self.module.addImport("env", "roc_dec_to_u128", i128_dec_conv_type);

    // Dec to f32: (val_ptr) -> f32
    const dec_to_f32_type = try self.module.addFuncType(
        &.{.i32},
        &.{.f32},
    );
    self.dec_to_f32_import = try self.module.addImport("env", "roc_dec_to_f32", dec_to_f32_type);

    // List of strings equality: (list_a_ptr, list_b_ptr) -> i32
    const list_str_eq_type = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{.i32},
    );
    self.list_str_eq_import = try self.module.addImport("env", "roc_list_str_eq", list_str_eq_type);

    // List of lists equality: (list_a_ptr, list_b_ptr, inner_elem_size) -> i32
    const list_list_eq_type = try self.module.addFuncType(
        &.{ .i32, .i32, .i32 },
        &.{.i32},
    );
    self.list_list_eq_import = try self.module.addImport("env", "roc_list_list_eq", list_list_eq_type);

    // String ops: (str_ptr, result_ptr) -> void
    const str_unary_type = try self.module.addFuncType(&.{ .i32, .i32 }, &.{});
    self.str_trim_import = try self.module.addImport("env", "roc_str_trim", str_unary_type);
    self.str_trim_start_import = try self.module.addImport("env", "roc_str_trim_start", str_unary_type);
    self.str_trim_end_import = try self.module.addImport("env", "roc_str_trim_end", str_unary_type);
    self.str_with_ascii_lowercased_import = try self.module.addImport("env", "roc_str_with_ascii_lowercased", str_unary_type);
    self.str_with_ascii_uppercased_import = try self.module.addImport("env", "roc_str_with_ascii_uppercased", str_unary_type);
    self.str_release_excess_capacity_import = try self.module.addImport("env", "roc_str_release_excess_capacity", str_unary_type);
    self.str_with_capacity_import = try self.module.addImport("env", "roc_str_with_capacity", str_unary_type);

    const str_from_utf8_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32 }, &.{});
    self.str_from_utf8_import = try self.module.addImport("env", "roc_str_from_utf8", str_from_utf8_type);

    const int_from_str_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.int_from_str_import = try self.module.addImport("env", "roc_int_from_str", int_from_str_type);

    const dec_from_str_type = try self.module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
    self.dec_from_str_import = try self.module.addImport("env", "roc_dec_from_str", dec_from_str_type);

    const float_from_str_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32 }, &.{});
    self.float_from_str_import = try self.module.addImport("env", "roc_float_from_str", float_from_str_type);

    const list_append_unsafe_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.list_append_unsafe_import = try self.module.addImport("env", "roc_list_append_unsafe", list_append_unsafe_type);

    const list_sort_with_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.list_sort_with_import = try self.module.addImport("env", "roc_list_sort_with", list_sort_with_type);

    const list_reverse_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32 }, &.{});
    self.list_reverse_import = try self.module.addImport("env", "roc_list_reverse", list_reverse_type);

    // String ops: (arg1, arg2, result_ptr) -> void
    const str_binary_type = try self.module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
    self.str_drop_prefix_import = try self.module.addImport("env", "roc_str_drop_prefix", str_binary_type);
    self.str_drop_suffix_import = try self.module.addImport("env", "roc_str_drop_suffix", str_binary_type);
    self.str_split_import = try self.module.addImport("env", "roc_str_split", str_binary_type);
    self.str_join_with_import = try self.module.addImport("env", "roc_str_join_with", str_binary_type);
    self.str_concat_import = try self.module.addImport("env", "roc_str_concat", str_binary_type);
    self.str_repeat_import = try self.module.addImport("env", "roc_str_repeat", str_binary_type);
    self.str_reserve_import = try self.module.addImport("env", "roc_str_reserve", str_binary_type);

    // Caseless equals: (str_a, str_b) -> i32
    self.str_caseless_ascii_equals_import = try self.module.addImport("env", "roc_str_caseless_ascii_equals", str_eq_type);
}

/// Result of generating a wasm module
pub const GenerateResult = struct {
    wasm_bytes: []u8,
    result_layout: layout.Idx,
    has_imports: bool = false,
};

/// Generate a complete wasm module for a single expression.
/// The expression becomes the body of an exported "main" function.
pub fn generateModule(self: *Self, expr_id: LirExprId, result_layout: layout.Idx) Allocator.Error!GenerateResult {
    // Register host function imports (must be done before addFunction calls)
    self.registerHostImports() catch return error.OutOfMemory;

    // Compile any procedures (recursive functions) before the main expression
    const proc_specs = self.store.getProcSpecs();
    if (proc_specs.len > 0) {
        self.compileAllProcSpecs(proc_specs) catch return error.OutOfMemory;
    }

    // Determine return type from the expression's actual wasm type.
    // We use exprValType because nominal layout indices can collide
    // with well-known sentinel values (e.g., Bool's nominal layout
    // index may equal the i64 sentinel).
    const result_vt = self.exprValType(expr_id);

    // Add function type: (i32 env_ptr) -> (result_type)
    const type_idx = self.module.addFuncType(&.{.i32}, &.{result_vt}) catch return error.OutOfMemory;

    // Add function
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;

    // Generate the expression body into self.body
    self.body.clearRetainingCapacity();
    self.storage.reset();
    self.stack_frame_size = 0;
    self.uses_stack_memory = false;

    // Local 0 = env_ptr parameter
    const env_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    // Local 1 = roc_ops_local (will hold constant 0, the RocOps struct address)
    self.roc_ops_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    // Pre-allocate frame pointer local so it doesn't collide with user locals
    self.fp_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    try self.generateExpr(expr_id);

    // Always enable memory + stack pointer (RocOps struct + allocations need linear memory)
    const stack_pages = (self.wasm_stack_bytes + 65535) / 65536; // round up to page boundary
    const memory_pages = if (self.wasm_memory_pages > 0) self.wasm_memory_pages else stack_pages;
    self.module.enableMemory(memory_pages);
    self.module.enableStackPointer(memory_pages * 65536); // stack starts at top of memory
    self.uses_stack_memory = true;
    self.module.addExport("memory", .memory, 0) catch return error.OutOfMemory;

    // Build function body: locals declaration + prologue + instructions + epilogue + end
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    // Encode locals declaration (skip 1 for the env_ptr parameter)
    try self.encodeLocalsDecl(&func_body, 1);

    // Prologue: allocate stack frame
    // global.get $__stack_pointer (global 0)
    func_body.append(self.allocator, Op.global_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
    // i32.const frame_size
    func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &func_body, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
    // i32.sub
    func_body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    // local.tee $fp
    func_body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &func_body, self.fp_local) catch return error.OutOfMemory;
    // global.set $__stack_pointer
    func_body.append(self.allocator, Op.global_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;

    // Build RocOps struct at memory offset 0 (36 bytes on wasm32)
    // Set roc_ops_local = 0 (constant address of the struct)
    func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
    func_body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &func_body, self.roc_ops_local) catch return error.OutOfMemory;

    // Write env pointer (offset 0)
    try self.emitI32StoreToBody(&func_body, 0, env_ptr_local, null);
    // Write roc_alloc table index (offset 4)
    try self.emitI32StoreConstToBody(&func_body, 4, self.roc_alloc_table_idx);
    // Write roc_dealloc table index (offset 8)
    try self.emitI32StoreConstToBody(&func_body, 8, self.roc_dealloc_table_idx);
    // Write roc_realloc table index (offset 12)
    try self.emitI32StoreConstToBody(&func_body, 12, self.roc_realloc_table_idx);
    // Write roc_dbg table index (offset 16)
    try self.emitI32StoreConstToBody(&func_body, 16, self.roc_dbg_table_idx);
    // Write roc_expect_failed table index (offset 20)
    try self.emitI32StoreConstToBody(&func_body, 20, self.roc_expect_failed_table_idx);
    // Write roc_crashed table index (offset 24)
    try self.emitI32StoreConstToBody(&func_body, 24, self.roc_crashed_table_idx);
    // Write hosted_fns.count = 0 (offset 28)
    try self.emitI32StoreConstToBody(&func_body, 28, 0);
    // Write hosted_fns.fns = 0 (offset 32)
    try self.emitI32StoreConstToBody(&func_body, 32, 0);

    // Main body instructions
    func_body.appendSlice(self.allocator, self.body.items) catch return error.OutOfMemory;

    // Epilogue: restore stack pointer
    // local.get $fp
    func_body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &func_body, self.fp_local) catch return error.OutOfMemory;
    // i32.const frame_size
    func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &func_body, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
    // i32.add
    func_body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    // global.set $__stack_pointer
    func_body.append(self.allocator, Op.global_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;

    // End opcode
    func_body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.module.setFunctionBody(func_idx, func_body.items) catch return error.OutOfMemory;

    // Export the function as "main"
    self.module.addExport("main", .func, func_idx) catch return error.OutOfMemory;

    // Encode the module
    const wasm_bytes = self.module.encode(self.allocator) catch return error.OutOfMemory;

    return .{
        .wasm_bytes = wasm_bytes,
        .result_layout = result_layout,
        .has_imports = self.module.importCount() > 0,
    };
}

/// Encode the locals declaration vector for a function body.
/// Groups consecutive locals of the same type: (count, type)*
/// `skip_count` is the number of leading locals to skip (e.g., function parameters).
fn encodeLocalsDecl(self: *Self, func_body: *std.ArrayList(u8), skip_count: u32) Allocator.Error!void {
    const all_types = self.storage.local_types.items;
    if (all_types.len <= skip_count) {
        WasmModule.leb128WriteU32(self.allocator, func_body, 0) catch return error.OutOfMemory;
        return;
    }

    const types = all_types[skip_count..];

    // Build groups of consecutive locals with the same type
    var groups: std.ArrayList(struct { count: u32, val_type: ValType }) = .empty;
    defer groups.deinit(self.allocator);

    var i: usize = 0;
    while (i < types.len) {
        const vt = types[i];
        var count: u32 = 1;
        while (i + count < types.len and types[i + count] == vt) {
            count += 1;
        }
        groups.append(self.allocator, .{ .count = count, .val_type = vt }) catch return error.OutOfMemory;
        i += count;
    }

    WasmModule.leb128WriteU32(self.allocator, func_body, @intCast(groups.items.len)) catch return error.OutOfMemory;
    for (groups.items) |g| {
        WasmModule.leb128WriteU32(self.allocator, func_body, g.count) catch return error.OutOfMemory;
        func_body.append(self.allocator, @intFromEnum(g.val_type)) catch return error.OutOfMemory;
    }
}

/// Generate wasm instructions for a LirExpr, leaving the result on the value stack.
fn generateExpr(self: *Self, expr_id: LirExprId) Allocator.Error!void {
    const expr: LirExpr = self.store.getExpr(expr_id);
    switch (expr) {
        .i64_literal => |val| {
            switch (self.resolveValType(val.layout_idx)) {
                .i32 => {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @truncate(val.value)) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, val.value) catch return error.OutOfMemory;
                },
                .f32 => {
                    self.body.append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                    const bytes: [4]u8 = @bitCast(@as(f32, @floatFromInt(val.value)));
                    self.body.appendSlice(self.allocator, &bytes) catch return error.OutOfMemory;
                },
                .f64 => {
                    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                    const bytes: [8]u8 = @bitCast(@as(f64, @floatFromInt(val.value)));
                    self.body.appendSlice(self.allocator, &bytes) catch return error.OutOfMemory;
                },
            }
        },
        .f64_literal => |val| {
            self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            const bytes: [8]u8 = @bitCast(val);
            self.body.appendSlice(self.allocator, &bytes) catch return error.OutOfMemory;
        },
        .f32_literal => |val| {
            self.body.append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
            const bytes: [4]u8 = @bitCast(val);
            self.body.appendSlice(self.allocator, &bytes) catch return error.OutOfMemory;
        },
        .bool_literal => |val| {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, if (val) 1 else 0) catch return error.OutOfMemory;
        },
        .dec_literal => |val| {
            // Dec is i128 stored in 16 bytes of linear memory
            const base_offset = try self.allocStackMemory(16, 8);
            const base_local = self.fp_local;

            const unsigned: u128 = @bitCast(val);
            const low: i64 = @bitCast(@as(u64, @truncate(unsigned)));
            const high: i64 = @bitCast(@as(u64, @truncate(unsigned >> 64)));

            // Store low 8 bytes
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, low) catch return error.OutOfMemory;
            try self.emitI64Store(base_offset);

            // Store high 8 bytes
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, high) catch return error.OutOfMemory;
            try self.emitI64Store(base_offset + 8);

            // Push pointer to the 16-byte value
            try self.emitFpOffset(base_offset);
        },
        .i128_literal => |val| {
            const unsigned: u128 = @bitCast(val.value);
            const low: i64 = @bitCast(@as(u64, @truncate(unsigned)));
            const high: i64 = @bitCast(@as(u64, @truncate(unsigned >> 64)));

            // MirToLir lowers integer literals exceeding maxInt(i64) to i128_literal
            // while preserving the original scalar layout (e.g., U64 max stays U64).
            // Emit a scalar constant directly when the layout is primitive.
            switch (WasmLayout.wasmReprWithStore(val.layout_idx, self.getLayoutStore())) {
                .primitive => |vt| switch (vt) {
                    .i32 => {
                        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, &self.body, @truncate(low)) catch return error.OutOfMemory;
                    },
                    .i64 => {
                        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI64(self.allocator, &self.body, low) catch return error.OutOfMemory;
                    },
                    .f32 => {
                        self.body.append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                        const bytes: [4]u8 = @bitCast(@as(f32, @floatFromInt(val.value)));
                        self.body.appendSlice(self.allocator, &bytes) catch return error.OutOfMemory;
                    },
                    .f64 => {
                        self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                        const bytes: [8]u8 = @bitCast(@as(f64, @floatFromInt(val.value)));
                        self.body.appendSlice(self.allocator, &bytes) catch return error.OutOfMemory;
                    },
                },
                .stack_memory => {
                    // i128/u128/dec — store 16 bytes in linear memory and push pointer.
                    const base_offset = try self.allocStackMemory(16, 8);
                    const base_local = self.fp_local;

                    // Store low 8 bytes
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, low) catch return error.OutOfMemory;
                    try self.emitI64Store(base_offset);

                    // Store high 8 bytes
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, high) catch return error.OutOfMemory;
                    try self.emitI64Store(base_offset + 8);

                    // Push pointer to the 16-byte value
                    try self.emitFpOffset(base_offset);
                },
            }
        },
        .block => |b| {
            // Process statements (let bindings)
            const stmts = self.store.getStmts(b.stmts);
            for (stmts) |stmt_union| {
                switch (stmt_union) {
                    .cell_init => |cell| {
                        try self.bindCellValue(cell.cell, cell.layout_idx, cell.expr);
                        continue;
                    },
                    .cell_store => |cell| {
                        try self.bindCellValue(cell.cell, cell.layout_idx, cell.expr);
                        continue;
                    },
                    .cell_drop => continue,
                    .decl, .mutate => {},
                }

                const stmt = stmt_union.binding();
                const pattern = self.store.getPattern(stmt.pattern);
                switch (pattern) {
                    .bind => |bind| {
                        {
                            // Check for type representation mismatch: composite expr bound
                            // to scalar local (e.g., dec_literal bound to U64 local).
                            // Conversion between these representations isn't supported yet.
                            const expr_is_composite = self.isCompositeExpr(stmt.expr);
                            const target_is_composite = self.isCompositeLayout(bind.layout_idx);
                            if (expr_is_composite and !target_is_composite) {
                                // Composite expr bound to scalar local:
                                const target_size = self.layoutByteSize(bind.layout_idx);
                                const expr_size = self.exprByteSize(stmt.expr);
                                if (target_size == expr_size or target_size < expr_size) {
                                    // Same size: representation difference (not truncation)
                                    // Smaller target: truncation (e.g., i128_literal → u64 — take lower bytes)
                                    try self.generateExpr(stmt.expr);
                                    const vt2 = self.resolveValType(bind.layout_idx);
                                    // Load the scalar from the pointer (lower bytes on little-endian)
                                    try self.emitLoadOpSized(vt2, target_size, 0);
                                    const local_idx = self.getOrAllocTypedLocal(bind.symbol, vt2) catch return error.OutOfMemory;
                                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                                    continue;
                                }
                                unreachable;
                            } else if (!expr_is_composite and target_is_composite) {
                                // Scalar expr bound to composite local
                                const target_size = self.layoutByteSize(bind.layout_idx);
                                const expr_size = self.exprByteSize(stmt.expr);

                                // Allocate stack memory for the composite target
                                const alignment: u32 = if (target_size >= 8) 8 else if (target_size >= 4) 4 else if (target_size >= 2) 2 else 1;
                                const stack_offset = try self.allocStackMemory(target_size, alignment);

                                if (target_size > expr_size) {
                                    // Widening: e.g., i64_literal → i128/u128 (8 → 16)
                                    // Zero-init the target memory first
                                    const base_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                                    try self.emitFpOffset(stack_offset);
                                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
                                    try self.emitZeroInit(base_local, target_size);

                                    // Generate the scalar and store at offset 0 (lower bytes)
                                    try self.generateExpr(stmt.expr);
                                    const scalar_vt = self.exprValType(stmt.expr);
                                    try self.emitStoreToMem(base_local, 0, scalar_vt);

                                    // For signed types, sign-extend the upper bytes
                                    const expr_data = self.store.getExpr(stmt.expr);
                                    if (expr_data == .i64_literal and expr_data.i64_literal.value < 0 and
                                        (bind.layout_idx == .i128 or bind.layout_idx == .dec))
                                    {
                                        // Store -1 (all ones) in upper 8 bytes for sign extension
                                        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                                        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
                                        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                                        WasmModule.leb128WriteI64(self.allocator, &self.body, -1) catch return error.OutOfMemory;
                                        try self.emitStoreOp(.i64, 8);
                                    }

                                    // Bind pointer to the symbol's local
                                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
                                    const local_idx = self.getOrAllocTypedLocal(bind.symbol, .i32) catch return error.OutOfMemory;
                                    try self.emitLocalSet(local_idx);
                                    continue;
                                }

                                std.debug.assert(target_size == expr_size);

                                // Same size: store scalar into stack memory, bind pointer
                                try self.generateExpr(stmt.expr);
                                const scalar_vt = self.exprValType(stmt.expr);
                                const tmp_local = self.storage.allocAnonymousLocal(scalar_vt) catch return error.OutOfMemory;
                                try self.emitLocalSet(tmp_local);

                                // Store scalar at offset 0 of the allocated memory
                                try self.emitLocalGet(self.fp_local);
                                try self.emitLocalGet(tmp_local);
                                try self.emitStoreOp(scalar_vt, stack_offset);

                                // Bind pointer (fp + stack_offset) to the symbol's local
                                try self.emitLocalGet(self.fp_local);
                                if (stack_offset > 0) {
                                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(stack_offset)) catch return error.OutOfMemory;
                                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                                }
                                const local_idx = self.getOrAllocTypedLocal(bind.symbol, .i32) catch return error.OutOfMemory;
                                try self.emitLocalSet(local_idx);
                                continue;
                            }
                            // Determine the target wasm type using layout store
                            const vt = self.resolveValType(bind.layout_idx);
                            // Generate the expression value
                            try self.generateExpr(stmt.expr);

                            // After a function call returns a composite value (record, list,
                            // string), the result pointer references the callee's now-freed
                            // stack frame. Copy to the caller's frame so subsequent calls
                            // don't overwrite the data.
                            if (target_is_composite and self.exprNeedsCompositeCallStabilization(stmt.expr)) {
                                const ret_size = self.layoutByteSize(bind.layout_idx);
                                if (ret_size > 0) {
                                    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                                    try self.emitLocalSet(src_local);
                                    const dst_offset = try self.allocStackMemory(ret_size, 4);
                                    const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                                    try self.emitFpOffset(dst_offset);
                                    try self.emitLocalSet(dst_local);
                                    try self.emitMemCopy(dst_local, 0, src_local, ret_size);
                                    try self.emitLocalGet(dst_local);
                                }
                            }

                            // Convert if the expression produced a different wasm type
                            const expr_vt = self.exprValType(stmt.expr);
                            try self.emitConversion(expr_vt, vt);
                            // Allocate a local (or reuse existing one for mutable rebinding)
                            const local_idx = self.getOrAllocTypedLocal(bind.symbol, vt) catch return error.OutOfMemory;
                            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                        }
                    },
                    .wildcard => {
                        // Evaluate expression for side effects, drop result
                        try self.generateExpr(stmt.expr);
                        self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
                    },
                    .struct_ => |s| {
                        // Struct destructuring: generate expr → pointer, then bind each field
                        try self.generateExpr(stmt.expr);
                        const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, ptr) catch return error.OutOfMemory;
                        try self.bindStructPattern(ptr, s);
                    },
                    .as_pattern => |as_pat| {
                        // As-pattern: bind the whole value AND match inner pattern
                        try self.generateExpr(stmt.expr);
                        const vt = self.resolveValType(as_pat.layout_idx);
                        const local_idx = self.storage.allocLocal(as_pat.symbol, vt) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                        // Now bind inner pattern with the same value on the stack
                        const inner = self.store.getPattern(as_pat.inner);
                        switch (inner) {
                            .bind => |inner_bind| {
                                const inner_vt = self.resolveValType(inner_bind.layout_idx);
                                const inner_local = self.storage.allocLocal(inner_bind.symbol, inner_vt) catch return error.OutOfMemory;
                                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, &self.body, inner_local) catch return error.OutOfMemory;
                            },
                            .wildcard => {
                                self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
                            },
                            else => {
                                self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
                            },
                        }
                    },
                    .tag => |tag_pat| {
                        // Tag destructuring in let-binding
                        try self.generateExpr(stmt.expr);
                        const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, ptr) catch return error.OutOfMemory;
                        try self.bindTagPattern(ptr, tag_pat);
                    },
                    .list => |list_pat| {
                        // List destructuring in let-binding
                        try self.generateExpr(stmt.expr);
                        const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, ptr) catch return error.OutOfMemory;
                        try self.bindListPattern(ptr, list_pat);
                    },
                    // Comparison patterns don't appear in let bindings
                    .int_literal, .float_literal, .str_literal => unreachable,
                }
            }
            // Generate the final expression (the block's result)
            try self.generateExpr(b.final_expr);
        },
        .lookup => |l| {
            const key: u64 = @bitCast(l.symbol);
            if (self.storage.locals.get(key)) |local_info| {
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, local_info.idx) catch return error.OutOfMemory;
                // Convert if the local's actual type differs from the expression's layout type.
                // This can happen when a function parameter is i64 (Roc I64) but the body
                // expression's layout resolves to i32 (e.g., used as a list count).
                const expected_vt = self.resolveValType(l.layout_idx);
                if (local_info.val_type != expected_vt) {
                    try self.emitConversion(local_info.val_type, expected_vt);
                }
            } else if (self.store.getSymbolDef(l.symbol)) |def_id| {
                // Symbol not in locals — resolve via getSymbolDef and generate the expression.
                try self.generateExpr(def_id);
            } else {
                unreachable;
            }
        },
        .cell_load => |l| {
            const key: u64 = @bitCast(l.cell);
            if (self.storage.locals.get(key)) |local_info| {
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, local_info.idx) catch return error.OutOfMemory;
                const expected_vt = self.resolveValType(l.layout_idx);
                if (local_info.val_type != expected_vt) {
                    try self.emitConversion(local_info.val_type, expected_vt);
                }
            } else if (self.store.getSymbolDef(l.cell)) |def_id| {
                try self.generateExpr(def_id);
            } else {
                unreachable;
            }
        },
        .if_then_else => |ite| {
            const branches = self.store.getIfBranches(ite.branches);
            const bt = valTypeToBlockType(self.resolveValType(ite.result_layout));
            try self.generateIfChain(branches, ite.final_else, bt);
        },
        .match_expr => |w| {
            try self.generateMatch(w);
        },
        .nominal => |nom| {
            // Nominal is transparent at runtime — just generate the backing expression.
            // The nominal's runtime representation is always identical to its backing.
            try self.generateExpr(nom.backing_expr);
        },
        .struct_ => |s| {
            try self.generateStruct(s);
        },
        .struct_access => |sa| {
            try self.generateStructAccess(sa);
        },
        .empty_list => {
            // Empty list: 12 bytes of zeros (ptr=0, len=0, cap=0)
            const base_offset = try self.allocStackMemory(12, 4);
            const base_local = self.fp_local;
            // Zero out the 12 bytes
            for (0..3) |i| {
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                try self.emitStoreOp(.i32, base_offset + @as(u32, @intCast(i)) * 4);
            }
            // Push pointer
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
            if (base_offset > 0) {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(base_offset)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            }
        },
        .runtime_error => {
            self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },
        .crash => |crash| {
            const msg_bytes = self.store.getString(crash.msg);
            const data_offset = self.module.addDataSegment(msg_bytes, 1) catch return error.OutOfMemory;

            // Build 8-byte RocCrashed struct on stack: {utf8_bytes: u32, len: u32}
            const crashed_slot = try self.allocStackMemory(8, 4);

            // Write utf8_bytes pointer
            try self.emitFpOffset(crashed_slot);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(data_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

            // Write len
            try self.emitFpOffset(crashed_slot);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(msg_bytes.len)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;

            // Push call_indirect args: (crashed_args_ptr, env_ptr)
            try self.emitFpOffset(crashed_slot);
            try self.emitLocalGet(self.roc_ops_local);
            self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

            // Load roc_crashed table index from roc_ops_ptr offset 24
            try self.emitLocalGet(self.roc_ops_local);
            self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 24) catch return error.OutOfMemory;

            // call_indirect
            self.body.append(self.allocator, Op.call_indirect) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, self.roc_ops_type_idx) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },
        .early_return => |er| {
            try self.generateExpr(er.expr);
            self.body.append(self.allocator, Op.@"return") catch return error.OutOfMemory;
        },
        .proc_call => |c| {
            try self.generateCall(c);
        },
        .zero_arg_tag => |z| {
            try self.generateZeroArgTag(z);
        },
        .tag => |t| {
            try self.generateTag(t);
        },
        .str_literal => |str_idx| {
            try self.generateStrLiteral(str_idx);
        },
        .dbg => |d| {
            // Debug: evaluate expression and return its value (print is a no-op in wasm)
            try self.generateExpr(d.expr);
        },
        .expect => |e| {
            // Expect: evaluate condition (drop result), then evaluate body
            try self.generateExpr(e.cond);
            self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
            try self.generateExpr(e.body);
        },
        .low_level => |ll| {
            try self.generateLowLevel(ll);
        },
        .incref => |rc_op| {
            try self.generateRcExpr(.incref, rc_op.value, rc_op.layout_idx, rc_op.count);
        },
        .decref => |rc_op| {
            try self.generateRcExpr(.decref, rc_op.value, rc_op.layout_idx, 1);
        },
        .free => |rc_op| {
            try self.generateRcExpr(.free, rc_op.value, rc_op.layout_idx, 1);
        },
        .discriminant_switch => |ds| {
            // debug removed
            try self.generateDiscriminantSwitch(ds);
        },
        .while_loop => |wl| {
            try self.generateWhileLoop(wl);
        },
        .for_loop => |fl| {
            try self.generateForLoopExpr(fl);
        },
        .list => |l| {
            try self.generateList(l);
        },
        .str_concat => |span| {
            try self.generateStrConcat(span);
        },
        .int_to_str => |its| {
            try self.generateIntToStr(its);
        },
        .float_to_str => |fts| {
            try self.generateFloatToStr(fts);
        },
        .dec_to_str => |dec_expr| {
            try self.generateDecToStr(dec_expr);
        },
        .str_escape_and_quote => |quote_expr| {
            try self.generateStrEscapeAndQuote(quote_expr);
        },
        .tag_payload_access => |tpa| {
            // Payload is always at offset 0 in the tag union memory.
            // Generate the tag union value (pushes i32 pointer to union).
            try self.generateExpr(tpa.value);

            if (self.isCompositeLayout(tpa.payload_layout)) {
                // Composite payload: the union pointer IS the payload pointer (offset 0)
            } else {
                // Scalar payload: load from offset 0
                try self.emitLoadOpForLayout(tpa.payload_layout, 0);
            }
        },
        .hosted_call => {
            // TODO: Implement hosted_call expression lowering for wasm.
            @panic("TODO: wasm hosted_call expression path is not implemented");
        },
        .break_expr => {
            const target_depth = self.currentLoopBreakDepth();
            std.debug.assert(self.expr_control_depth >= target_depth);

            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(
                self.allocator,
                &self.body,
                self.expr_control_depth - target_depth,
            ) catch return error.OutOfMemory;
        },
    }
}

/// Generate RC op expression while preserving the underlying value.
fn generateRcExpr(
    self: *Self,
    comptime kind: RcOpKind,
    value_expr: LirExprId,
    layout_idx: layout.Idx,
    inc_count: u16,
) Allocator.Error!void {
    try self.generateExpr(value_expr);
    const vt = self.exprValType(value_expr);
    const value_local = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
    try self.emitLocalSet(value_local);
    try self.emitRcForValueLocal(kind, value_local, vt, layout_idx, inc_count);
    try self.emitLocalGet(value_local);
}

fn emitRcForValueLocal(
    self: *Self,
    comptime kind: RcOpKind,
    value_local: u32,
    value_vt: ValType,
    layout_idx: layout.Idx,
    inc_count: u16,
) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);
    if (!ls.layoutContainsRefcounted(l)) return;
    if (value_vt != .i32) return;

    switch (l.tag) {
        .scalar => {
            if (l.data.scalar.tag == .str) {
                try self.emitStrRc(kind, value_local, inc_count);
            }
        },
        .list, .list_of_zst => {
            try self.emitListRc(kind, value_local, layout_idx, inc_count);
        },
        .box, .box_of_zst => {
            try self.emitBoxRc(kind, value_local, layout_idx, inc_count);
        },
        .struct_, .tag_union => {
            try self.emitRcAtPtr(kind, value_local, layout_idx, inc_count);
        },
        .closure => {
            // RC the captures payload, which may contain refcounted values
            try self.emitRcAtPtr(kind, value_local, l.data.closure.captures_layout_idx, inc_count);
        },
        .zst => {},
    }
}

/// Recursively emit RC ops for a value addressed by `value_ptr_local`.
fn emitRcAtPtr(
    self: *Self,
    comptime kind: RcOpKind,
    value_ptr_local: u32,
    layout_idx: layout.Idx,
    inc_count: u16,
) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);
    if (!ls.layoutContainsRefcounted(l)) return;

    switch (l.tag) {
        .scalar => {
            if (l.data.scalar.tag == .str) {
                try self.emitStrRc(kind, value_ptr_local, inc_count);
            }
        },
        .list, .list_of_zst => {
            try self.emitListRc(kind, value_ptr_local, layout_idx, inc_count);
        },
        .box, .box_of_zst => {
            const box_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr);
            try self.emitBoxRc(kind, box_ptr, layout_idx, inc_count);
        },
        .struct_ => {
            const struct_idx = l.data.struct_.idx;
            const struct_data = ls.getStructData(struct_idx);
            var field_i: u32 = 0;
            while (field_i < struct_data.fields.count) : (field_i += 1) {
                const field_layout_idx = ls.getStructFieldLayout(struct_idx, @intCast(field_i));
                const field_layout = ls.getLayout(field_layout_idx);
                if (!ls.layoutContainsRefcounted(field_layout)) continue;

                const field_size = ls.getStructFieldSize(struct_idx, @intCast(field_i));
                if (field_size == 0) continue;

                const field_offset = ls.getStructFieldOffset(struct_idx, @intCast(field_i));
                const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalGet(value_ptr_local);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                try self.emitLocalSet(field_ptr);
                try self.emitRcAtPtr(kind, field_ptr, field_layout_idx, inc_count);
            }
        },
        .tag_union => {
            const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
            const variants = ls.getTagUnionVariants(tu_data);
            if (variants.len == 0) return;

            const disc_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            if (tu_data.discriminant_size == 0) {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            } else {
                try self.emitLocalGet(value_ptr_local);
                try self.emitLoadBySize(tu_data.discriminant_size, tu_data.discriminant_offset);
            }
            try self.emitLocalSet(disc_local);

            for (0..variants.len) |variant_i| {
                const payload_layout_idx = variants.get(variant_i).payload_layout;
                const payload_layout = ls.getLayout(payload_layout_idx);
                if (!ls.layoutContainsRefcounted(payload_layout)) continue;
                if (ls.layoutSizeAlign(payload_layout).size == 0) continue;

                self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
                self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

                try self.emitLocalGet(disc_local);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(variant_i)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

                try self.emitRcAtPtr(kind, value_ptr_local, payload_layout_idx, inc_count);

                self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            }
        },
        .closure => {
            // RC the captures payload, which may contain refcounted values
            try self.emitRcAtPtr(kind, value_ptr_local, l.data.closure.captures_layout_idx, inc_count);
        },
        .zst => {},
    }
}

fn emitDecodeListAllocPtr(self: *Self, list_ptr_local: u32, out_alloc_ptr: u32, out_is_slice: u32) Allocator.Error!void {
    const cap_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr_local);
    try self.emitLoadOp(.i32, 8);
    try self.emitLocalSet(cap_local);

    try self.emitLocalGet(cap_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_lt_s) catch return error.OutOfMemory;
    try self.emitLocalSet(out_is_slice);

    try self.emitLocalGet(out_is_slice);
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(ValType.i32)) catch return error.OutOfMemory;
    try self.emitLocalGet(cap_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_shl) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr_local);
    try self.emitLoadOp(.i32, 0);
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    try self.emitLocalSet(out_alloc_ptr);
}

fn emitDecodeStrAllocPtr(self: *Self, str_ptr_local: u32, out_alloc_ptr: u32, out_is_small: u32) Allocator.Error!void {
    const cap_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(str_ptr_local);
    try self.emitLoadOp(.i32, 8);
    try self.emitLocalSet(cap_local);

    try self.emitLocalGet(cap_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_lt_s) catch return error.OutOfMemory;
    try self.emitLocalSet(out_is_small);

    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(out_alloc_ptr);

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitLocalGet(out_is_small);
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    const is_slice = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(str_ptr_local);
    try self.emitLoadOp(.i32, 4);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_lt_s) catch return error.OutOfMemory;
    try self.emitLocalSet(is_slice);

    try self.emitLocalGet(is_slice);
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(ValType.i32)) catch return error.OutOfMemory;
    try self.emitLocalGet(cap_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_shl) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.emitLocalGet(str_ptr_local);
    try self.emitLoadOp(.i32, 0);
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    try self.emitLocalSet(out_alloc_ptr);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitPtrWithOffset(self: *Self, ptr_local: u32, offset: i32) Allocator.Error!void {
    try self.emitLocalGet(ptr_local);
    if (offset != 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    }
}

fn emitLoadI32AtPtrOffset(self: *Self, ptr_local: u32, offset: i32, out_local: u32) Allocator.Error!void {
    try self.emitPtrWithOffset(ptr_local, offset);
    try self.emitLoadOp(.i32, 0);
    try self.emitLocalSet(out_local);
}

fn emitPrepareListSliceMetadata(self: *Self, list_ptr_local: u32, elements_refcounted: bool, out_encoded_cap: u32) Allocator.Error!void {
    const source_alloc_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const source_is_slice = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitDecodeListAllocPtr(list_ptr_local, source_alloc_ptr, source_is_slice);

    if (elements_refcounted) {
        const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
        self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        try self.emitLocalGet(source_alloc_ptr);
        self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

        try self.emitLocalGet(source_is_slice);
        self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

        try self.emitLoadI32AtPtrOffset(source_alloc_ptr, -4, rc_val);
        try self.emitLocalGet(rc_val);
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

        try self.emitPtrWithOffset(source_alloc_ptr, -8);
        try self.emitLocalGet(list_ptr_local);
        try self.emitLoadOp(.i32, 4);
        try self.emitStoreOp(.i32, 0);

        self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    try self.emitLocalGet(source_alloc_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_shr_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @as(i32, @bitCast(@as(u32, 0x80000000)))) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
    try self.emitLocalSet(out_encoded_cap);
}

fn emitCallRocDealloc(self: *Self, ptr_local: u32, alignment: u32) Allocator.Error!void {
    const dealloc_slot = try self.allocStackMemory(8, 4);

    try self.emitFpOffset(dealloc_slot);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(alignment)) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, 0);

    try self.emitFpOffset(dealloc_slot);
    try self.emitLocalGet(ptr_local);
    try self.emitStoreOp(.i32, 4);

    try self.emitFpOffset(dealloc_slot);
    try self.emitLocalGet(self.roc_ops_local);
    try self.emitLoadOp(.i32, 0); // env ptr

    try self.emitLocalGet(self.roc_ops_local);
    try self.emitLoadOp(.i32, 8); // roc_dealloc table idx

    self.body.append(self.allocator, Op.call_indirect) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, self.roc_ops_type_idx) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
}

fn emitFreeRcPtr(self: *Self, rc_ptr_local: u32, element_alignment: u32, elements_refcounted: bool) Allocator.Error!void {
    const ptr_width: u32 = 4;
    const required_space: u32 = if (elements_refcounted) 2 * ptr_width else ptr_width;
    const extra_bytes: u32 = if (element_alignment > required_space) element_alignment else required_space;
    const alloc_alignment: u32 = if (element_alignment > ptr_width) element_alignment else ptr_width;
    const alloc_adjust: i32 = @intCast(extra_bytes - ptr_width);

    const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(rc_ptr_local);
    if (alloc_adjust > 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, alloc_adjust) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    }
    try self.emitLocalSet(alloc_ptr_local);
    try self.emitCallRocDealloc(alloc_ptr_local, alloc_alignment);
}

fn emitDataPtrIncref(self: *Self, data_ptr_local: u32, amount: u16) Allocator.Error!void {
    if (amount == 0) return;

    const masked_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, -4) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(masked_ptr);

    try self.emitLocalGet(masked_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, -4) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rc_ptr);

    try self.emitLoadI32AtPtrOffset(rc_ptr, 0, rc_val);
    try self.emitLocalGet(rc_val);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitLocalGet(rc_ptr);
    try self.emitLocalGet(rc_val);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(amount)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, 0);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitDataPtrDecref(self: *Self, data_ptr_local: u32, alignment: u32, elements_refcounted: bool) Allocator.Error!void {
    const masked_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, -4) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(masked_ptr);

    try self.emitLocalGet(masked_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, -4) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rc_ptr);

    try self.emitLoadI32AtPtrOffset(rc_ptr, 0, rc_val);
    try self.emitLocalGet(rc_val);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitLocalGet(rc_val);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitFreeRcPtr(rc_ptr, alignment, elements_refcounted);
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.emitLocalGet(rc_ptr);
    try self.emitLocalGet(rc_val);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, 0);
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitDataPtrFree(self: *Self, data_ptr_local: u32, alignment: u32, elements_refcounted: bool) Allocator.Error!void {
    const masked_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, -4) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(masked_ptr);

    try self.emitLocalGet(masked_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, -4) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rc_ptr);
    try self.emitFreeRcPtr(rc_ptr, alignment, elements_refcounted);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitListElementDecrefsIfUnique(
    self: *Self,
    list_ptr_local: u32,
    alloc_ptr_local: u32,
    is_slice_local: u32,
    elem_layout_idx: layout.Idx,
) Allocator.Error!void {
    const elem_size = self.layoutByteSize(elem_layout_idx);
    if (elem_size == 0) return;

    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const idx_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const elem_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(alloc_ptr_local);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitLoadI32AtPtrOffset(alloc_ptr_local, -4, rc_val);
    try self.emitLocalGet(rc_val);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitLocalGet(is_slice_local);
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitLoadI32AtPtrOffset(alloc_ptr_local, -8, count_local);
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr_local);
    try self.emitLoadOp(.i32, 4);
    try self.emitLocalSet(count_local);
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(idx_local);

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(idx_local);
    try self.emitLocalGet(count_local);
    self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    try self.emitLocalGet(alloc_ptr_local);
    try self.emitLocalGet(idx_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(elem_ptr_local);

    try self.emitRcAtPtr(.decref, elem_ptr_local, elem_layout_idx, 1);

    try self.emitLocalGet(idx_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(idx_local);

    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitListRc(
    self: *Self,
    comptime kind: RcOpKind,
    list_ptr_local: u32,
    list_layout_idx: layout.Idx,
    inc_count: u16,
) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const list_layout = ls.getLayout(list_layout_idx);

    var elem_alignment: u32 = 1;
    var elements_refcounted = false;
    var elem_layout_idx: ?layout.Idx = null;
    if (list_layout.tag == .list) {
        const info = ls.getListInfo(list_layout);
        elem_alignment = info.elem_alignment;
        elements_refcounted = info.contains_refcounted;
        if (info.contains_refcounted) {
            elem_layout_idx = info.elem_layout_idx;
        }
    }

    const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const is_slice_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitDecodeListAllocPtr(list_ptr_local, alloc_ptr_local, is_slice_local);

    switch (kind) {
        .incref => {
            try self.emitDataPtrIncref(alloc_ptr_local, inc_count);
        },
        .decref => {
            if (elements_refcounted and elem_layout_idx != null) {
                try self.emitListElementDecrefsIfUnique(list_ptr_local, alloc_ptr_local, is_slice_local, elem_layout_idx.?);
            }
            try self.emitDataPtrDecref(alloc_ptr_local, elem_alignment, elements_refcounted);
        },
        .free => {
            try self.emitDataPtrFree(alloc_ptr_local, elem_alignment, elements_refcounted);
        },
    }
}

fn emitStrRc(self: *Self, comptime kind: RcOpKind, str_ptr_local: u32, inc_count: u16) Allocator.Error!void {
    const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const is_small_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitDecodeStrAllocPtr(str_ptr_local, alloc_ptr_local, is_small_local);

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitLocalGet(is_small_local);
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    switch (kind) {
        .incref => try self.emitDataPtrIncref(alloc_ptr_local, inc_count),
        .decref => try self.emitDataPtrDecref(alloc_ptr_local, 1, false),
        .free => try self.emitDataPtrFree(alloc_ptr_local, 1, false),
    }

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitBoxRc(self: *Self, comptime kind: RcOpKind, box_ptr_local: u32, box_layout_idx: layout.Idx, inc_count: u16) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const box_layout = ls.getLayout(box_layout_idx);

    var elem_alignment: u32 = @intCast(ls.layoutSizeAlign(box_layout).alignment.toByteUnits());
    var elements_refcounted = false;
    if (box_layout.tag == .box) {
        const info = ls.getBoxInfo(box_layout);
        elem_alignment = info.elem_alignment;
        elements_refcounted = info.contains_refcounted;
    }

    switch (kind) {
        .incref => try self.emitDataPtrIncref(box_ptr_local, inc_count),
        .decref => try self.emitDataPtrDecref(box_ptr_local, elem_alignment, elements_refcounted),
        .free => try self.emitDataPtrFree(box_ptr_local, elem_alignment, elements_refcounted),
    }
}

/// Generate a cascading if/else chain from LirIfBranch array + final_else.
fn generateIfChain(self: *Self, branches: []const LIR.LirIfBranch, final_else: LirExprId, bt: BlockType) Allocator.Error!void {
    if (branches.len == 0) {
        // No branches — just generate the else expression
        try self.generateExpr(final_else);
        return;
    }

    // Generate first branch condition
    try self.generateExpr(branches[0].cond);
    // if (block_type)
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
    self.pushExprControlFrame();
    defer self.popExprControlFrame();
    // then body
    try self.generateExpr(branches[0].body);
    // else
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    // Remaining branches become nested if/else, or just the final_else
    if (branches.len > 1) {
        try self.generateIfChain(branches[1..], final_else, bt);
    } else {
        try self.generateExpr(final_else);
    }
    // end
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Generate a match expression (pattern matching).
fn generateMatch(self: *Self, w: anytype) Allocator.Error!void {
    const branches = self.store.getMatchBranches(w.branches);
    const bt = valTypeToBlockType(self.resolveValType(w.result_layout));

    if (branches.len == 0) {
        // No branches — unreachable
        self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        return;
    }

    // Generate the value being matched once, store in a temp local
    const value_vt = self.resolveValType(w.value_layout);

    try self.generateExpr(w.value);
    const temp_local = self.storage.allocAnonymousLocal(value_vt) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, temp_local) catch return error.OutOfMemory;

    // Generate cascading if/else for each branch
    try self.generateMatchBranches(branches, temp_local, value_vt, bt);
}

fn generateMatchBranches(self: *Self, branches: []const LIR.LirMatchBranch, value_local: u32, value_vt: ValType, bt: BlockType) Allocator.Error!void {
    if (branches.len == 0) {
        // Fallthrough — unreachable
        self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        return;
    }

    const branch = branches[0];
    const pattern = self.store.getPattern(branch.pattern);
    const remaining = branches[1..];

    switch (pattern) {
        .wildcard => {
            // Wildcard matches anything — just generate the body
            try self.generateExpr(branch.body);
        },
        .bind => |bind| {
            // Bind the value to the symbol and generate the body
            const local_idx = self.storage.allocLocal(bind.symbol, value_vt) catch return error.OutOfMemory;
            // Copy value from temp to the bound local
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
            try self.generateExpr(branch.body);
        },
        .int_literal => |int_pat| {
            // Compare value to the integer literal
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;

            // Push the pattern value
            switch (value_vt) {
                .i32 => {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @truncate(@as(i64, @truncate(int_pat.value)))) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, @truncate(int_pat.value)) catch return error.OutOfMemory;
                },
                .f32, .f64 => unreachable,
            }

            // Compare
            const eq_op: u8 = switch (value_vt) {
                .i32 => Op.i32_eq,
                .i64 => Op.i64_eq,
                .f32, .f64 => unreachable,
            };
            self.body.append(self.allocator, eq_op) catch return error.OutOfMemory;

            // if match
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
            self.pushExprControlFrame();
            defer self.popExprControlFrame();
            try self.generateExpr(branch.body);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateMatchBranches(remaining, value_local, value_vt, bt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .tag => |tag_pat| {
            // Match on tag discriminant
            const arg_patterns = self.store.getPatternSpan(tag_pat.args);

            // For tag unions that fit in a single i32 (discriminant only, no payload),
            // value_local holds the discriminant directly. For larger tag unions,
            // value_local holds a pointer to the tag union in memory.
            const ls = self.getLayoutStore();
            const is_pointer = switch (WasmLayout.wasmReprWithStore(tag_pat.union_layout, ls)) {
                .stack_memory => true,
                .primitive => false,
            };
            if (is_pointer) {
                // Load discriminant from memory at discriminant_offset
                const l = ls.getLayout(tag_pat.union_layout);
                std.debug.assert(l.tag == .tag_union);
                const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
                const disc_offset = tu_data.discriminant_offset;
                const disc_size: u32 = tu_data.discriminant_size;
                if (disc_size == 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                } else {
                    // Load discriminant: value_local[disc_offset]
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                    try self.emitLoadOpSized(.i32, disc_size, disc_offset);
                }
            } else {
                // Value is the discriminant itself
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
            }

            // Push discriminant to compare against
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(tag_pat.discriminant)) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
            self.pushExprControlFrame();
            defer self.popExprControlFrame();

            // Bind any sub-pattern arguments (load payload from memory)
            if (is_pointer and arg_patterns.len > 0) {
                var payload_offset: u32 = 0;
                for (arg_patterns) |arg_pat_id| {
                    const arg_pat = self.store.getPattern(arg_pat_id);
                    switch (arg_pat) {
                        .bind => |bind| {
                            const bind_vt = self.resolveValType(bind.layout_idx);
                            const bind_byte_size = self.layoutStorageByteSize(bind.layout_idx);
                            const local_idx = self.storage.allocLocal(bind.symbol, bind_vt) catch return error.OutOfMemory;

                            const wasm_repr = WasmLayout.wasmReprWithStore(bind.layout_idx, ls);

                            switch (wasm_repr) {
                                .stack_memory => {
                                    // Composite types (Str, Dec, List, records, etc.) are
                                    // stored inline in the tag union memory. The "value" is
                                    // a pointer to the start of the data within the tag union.
                                    // Compute: value_local + payload_offset
                                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                                    if (payload_offset > 0) {
                                        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(payload_offset)) catch return error.OutOfMemory;
                                        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                                    }
                                },
                                .primitive => {
                                    // Primitive types: load the value from memory
                                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                                    try self.emitLoadOpForLayout(bind.layout_idx, payload_offset);
                                },
                            }

                            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                            payload_offset += bind_byte_size;
                        },
                        .wildcard => |wc| {
                            // Skip this payload field — use wildcard's layout for size
                            payload_offset += self.layoutByteSize(wc.layout_idx);
                        },
                        .struct_ => |inner_struct| {
                            // Struct destructuring of tag payload field
                            const field_byte_size = self.layoutByteSize(inner_struct.struct_layout);
                            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                            if (payload_offset > 0) {
                                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(payload_offset)) catch return error.OutOfMemory;
                                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                            }
                            const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, &self.body, field_ptr) catch return error.OutOfMemory;
                            try self.bindStructPattern(field_ptr, inner_struct);
                            payload_offset += field_byte_size;
                        },
                        .tag => |inner_tag| {
                            if (builtin.mode == .Debug and !self.tagPatternIsIrrefutable(inner_tag)) {
                                std.debug.panic(
                                    "WasmCodeGen invariant violated: nested tag payload patterns must be irrefutable single-tag unions",
                                    .{},
                                );
                            }

                            const field_byte_size = self.layoutByteSize(inner_tag.union_layout);
                            if (self.store.getPatternSpan(inner_tag.args).len != 0) {
                                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                                if (payload_offset > 0) {
                                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(payload_offset)) catch return error.OutOfMemory;
                                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                                }
                                const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, &self.body, field_ptr) catch return error.OutOfMemory;
                                try self.bindTagPattern(field_ptr, inner_tag);
                            }
                            payload_offset += field_byte_size;
                        },
                        else => unreachable,
                    }
                }
            } else {
                // Simple enum (no payload) — just allocate locals for any binds
                for (arg_patterns) |arg_pat_id| {
                    const arg_pat = self.store.getPattern(arg_pat_id);
                    switch (arg_pat) {
                        .bind => |bind| {
                            const bind_vt = self.resolveValType(bind.layout_idx);
                            _ = self.storage.allocLocal(bind.symbol, bind_vt) catch return error.OutOfMemory;
                        },
                        .wildcard => {},
                        else => unreachable,
                    }
                }
            }

            try self.generateExpr(branch.body);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateMatchBranches(remaining, value_local, value_vt, bt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .struct_ => |struct_pat| {
            // Struct destructuring: bind each field to a local
            const ls = self.getLayoutStore();
            const l = ls.getLayout(struct_pat.struct_layout);
            std.debug.assert(l.tag == .struct_);
            const field_patterns = self.store.getPatternSpan(struct_pat.fields);

            for (field_patterns, 0..) |field_pat_id, i| {
                const field_pat = self.store.getPattern(field_pat_id);
                switch (field_pat) {
                    .bind => |bind| {
                        const bind_vt = self.resolveValType(bind.layout_idx);
                        const bind_byte_size = self.layoutStorageByteSize(bind.layout_idx);
                        const local_idx = self.storage.allocLocal(bind.symbol, bind_vt) catch return error.OutOfMemory;
                        const field_offset = ls.getStructFieldOffset(l.data.struct_.idx, @intCast(i));
                        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                        if (self.isCompositeLayout(bind.layout_idx)) {
                            if (field_offset > 0) {
                                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                            }
                        } else {
                            try self.emitLoadOpSized(bind_vt, bind_byte_size, field_offset);
                        }
                        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                    },
                    .wildcard => {},
                    .struct_ => |inner_struct| {
                        const field_offset = ls.getStructFieldOffset(l.data.struct_.idx, @intCast(i));
                        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                        if (field_offset > 0) {
                            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                        }
                        const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, field_ptr) catch return error.OutOfMemory;
                        try self.bindStructPattern(field_ptr, inner_struct);
                    },
                    else => unreachable,
                }
            }
            try self.generateExpr(branch.body);
        },
        .as_pattern => |as_pat| {
            const bind_vt = self.resolveValType(as_pat.layout_idx);
            const local_idx = self.storage.allocLocal(as_pat.symbol, bind_vt) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;

            const inner_pat = self.store.getPattern(as_pat.inner);
            switch (inner_pat) {
                .wildcard => {
                    try self.generateExpr(branch.body);
                },
                .bind => |bind| {
                    const inner_vt = self.resolveValType(bind.layout_idx);
                    const inner_local = self.storage.allocLocal(bind.symbol, inner_vt) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, inner_local) catch return error.OutOfMemory;
                    try self.generateExpr(branch.body);
                },
                .struct_ => |inner_struct| {
                    try self.bindStructPattern(value_local, inner_struct);
                    try self.generateExpr(branch.body);
                },
                else => unreachable,
            }
        },
        .float_literal => |float_pat| {
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;

            switch (value_vt) {
                .f64 => {
                    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                    const bytes: [8]u8 = @bitCast(float_pat.value);
                    self.body.appendSlice(self.allocator, &bytes) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f64_eq) catch return error.OutOfMemory;
                },
                .f32 => {
                    self.body.append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                    const bytes: [4]u8 = @bitCast(@as(f32, @floatCast(float_pat.value)));
                    self.body.appendSlice(self.allocator, &bytes) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f32_eq) catch return error.OutOfMemory;
                },
                .i32, .i64 => unreachable,
            }

            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
            self.pushExprControlFrame();
            defer self.popExprControlFrame();
            try self.generateExpr(branch.body);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateMatchBranches(remaining, value_local, value_vt, bt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .str_literal => |str_idx| {
            // String literal comparison in match branch
            const import_idx = self.str_eq_import orelse unreachable;

            // Generate the pattern string as a RocStr
            try self.generateStrLiteral(str_idx);
            const pat_str = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(pat_str);

            // Compare value with pattern using roc_str_eq
            try self.emitLocalGet(value_local);
            try self.emitLocalGet(pat_str);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;

            // if match
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
            self.pushExprControlFrame();
            defer self.popExprControlFrame();
            try self.generateExpr(branch.body);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateMatchBranches(remaining, value_local, value_vt, bt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .list => |list_pat| {
            // List destructuring in match branch
            // Check if length matches prefix count (exact match when no rest pattern)
            const prefix_patterns = self.store.getPatternSpan(list_pat.prefix);
            const prefix_count: u32 = @intCast(prefix_patterns.len);

            // Load list length from RocList (offset 4)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);

            // Compare length
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(prefix_count)) catch return error.OutOfMemory;

            if (list_pat.rest.isNone()) {
                // Exact match: length == prefix_count
                self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
            } else {
                // Has rest: length >= prefix_count
                self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            }

            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
            self.pushExprControlFrame();
            defer self.popExprControlFrame();

            // Bind prefix elements
            if (prefix_count > 0) {
                const elem_size = self.layoutByteSize(list_pat.elem_layout);
                const elem_vt = self.resolveValType(list_pat.elem_layout);
                const is_composite = self.isCompositeLayout(list_pat.elem_layout);

                // Load elements pointer from RocList (offset 0)
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                try self.emitLoadOp(.i32, 0);
                const elems_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(elems_ptr);

                for (prefix_patterns, 0..) |pat_id, idx| {
                    const pat = self.store.getPattern(pat_id);
                    const elem_offset: u32 = @intCast(idx * elem_size);
                    switch (pat) {
                        .bind => |bind| {
                            if (is_composite and elem_size > 0) {
                                // Composite: pointer = elems_ptr + offset
                                try self.emitLocalGet(elems_ptr);
                                if (elem_offset > 0) {
                                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_offset)) catch return error.OutOfMemory;
                                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                                }
                            } else {
                                // Scalar: load from elems_ptr + offset
                                try self.emitLocalGet(elems_ptr);
                                try self.emitLoadOpSized(elem_vt, elem_size, elem_offset);
                            }
                            const local_idx = self.storage.allocLocal(bind.symbol, if (is_composite) .i32 else elem_vt) catch return error.OutOfMemory;
                            try self.emitLocalSet(local_idx);
                        },
                        .wildcard => {},
                        .struct_ => |inner_struct| {
                            try self.emitLocalGet(elems_ptr);
                            if (elem_offset > 0) {
                                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_offset)) catch return error.OutOfMemory;
                                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                            }
                            const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                            try self.emitLocalSet(field_ptr);
                            try self.bindStructPattern(field_ptr, inner_struct);
                        },
                        else => unreachable,
                    }
                }
            }

            try self.generateExpr(branch.body);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateMatchBranches(remaining, value_local, value_vt, bt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
    }
}

fn bindCellValue(self: *Self, cell: Symbol, layout_idx: layout.Idx, expr_id: LirExprId) Allocator.Error!void {
    const target_is_composite = self.isCompositeLayout(layout_idx);

    if (target_is_composite) {
        try self.generateExpr(expr_id);

        if (self.exprNeedsCompositeCallStabilization(expr_id)) {
            const ret_size = self.layoutByteSize(layout_idx);
            if (ret_size > 0) {
                const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(src_local);
                const dst_offset = try self.allocStackMemory(ret_size, 4);
                const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitFpOffset(dst_offset);
                try self.emitLocalSet(dst_local);
                try self.emitMemCopy(dst_local, 0, src_local, ret_size);
                try self.emitLocalGet(dst_local);
            }
        }

        const local_idx = self.getOrAllocTypedLocal(cell, .i32) catch return error.OutOfMemory;
        try self.emitLocalSet(local_idx);
        return;
    }

    const vt = self.resolveValType(layout_idx);
    try self.generateExpr(expr_id);
    const expr_vt = self.exprValType(expr_id);
    try self.emitConversion(expr_vt, vt);
    const local_idx = self.getOrAllocTypedLocal(cell, vt) catch return error.OutOfMemory;
    try self.emitLocalSet(local_idx);
}

/// Check whether a layout represents an unsigned integer type.
fn isUnsignedLayout(layout_idx: layout.Idx) bool {
    return switch (layout_idx) {
        .u8, .u16, .u32, .u64, .u128 => true,
        else => false,
    };
}

fn getOrAllocTypedLocal(self: *Self, symbol: Symbol, val_type: ValType) Allocator.Error!u32 {
    if (self.storage.getLocalInfo(symbol)) |info| {
        if (info.val_type == val_type) {
            return info.idx;
        }
    }

    return self.storage.allocLocal(symbol, val_type);
}

/// Convert a ValType to the corresponding BlockType for structured control flow.
fn valTypeToBlockType(vt: ValType) BlockType {
    return switch (vt) {
        .i32 => .i32,
        .i64 => .i64,
        .f32 => .f32,
        .f64 => .f64,
    };
}

fn pushExprControlFrame(self: *Self) void {
    self.expr_control_depth += 1;
}

fn popExprControlFrame(self: *Self) void {
    std.debug.assert(self.expr_control_depth > 0);
    self.expr_control_depth -= 1;
}

fn currentLoopBreakDepth(self: *Self) u32 {
    return self.loop_break_target_depths.getLastOrNull() orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("WASM/codegen invariant violated: break_expr emitted outside an enclosing loop", .{});
        }
        unreachable;
    };
}

/// Get the concrete layout of a value-producing expression.
/// Non-value expressions panic in debug.
fn exprLayoutIdx(self: *Self, expr_id: LirExprId) layout.Idx {
    const expr = self.store.getExpr(expr_id);
    return switch (expr) {
        .block => |b| b.result_layout,
        .lookup => |l| l.layout_idx,
        .if_then_else => |ite| ite.result_layout,
        .match_expr => |w| w.result_layout,
        .nominal => |nom| self.exprLayoutIdx(nom.backing_expr),
        .proc_call => |c| c.ret_layout,
        .struct_ => |s| s.struct_layout,
        .struct_access => |sa| sa.field_layout,
        .zero_arg_tag => |z| z.union_layout,
        .tag => |t| t.union_layout,
        .low_level => |ll| ll.ret_layout,
        .dbg => |d| d.result_layout,
        .expect => |e| e.result_layout,
        .incref, .decref, .free => layout.Idx.zst,
        .i64_literal => |i| i.layout_idx,
        .f64_literal => layout.Idx.f64,
        .f32_literal => layout.Idx.f32,
        .bool_literal => layout.Idx.bool,
        .i128_literal => |i| i.layout_idx,
        .dec_literal => layout.Idx.dec,
        .str_literal => layout.Idx.str,
        .str_concat => layout.Idx.str,
        .int_to_str => layout.Idx.str,
        .float_to_str => layout.Idx.str,
        .dec_to_str => layout.Idx.str,
        .str_escape_and_quote => layout.Idx.str,
        .tag_payload_access => |tpa| tpa.payload_layout,
        .hosted_call => |hc| hc.ret_layout,
        .discriminant_switch => |ds| ds.result_layout,
        .early_return => |er| er.ret_layout,
        .cell_load => |l| l.layout_idx,
        .for_loop, .while_loop => layout.Idx.zst,
        .break_expr, .crash, .runtime_error => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "LIR/wasm invariant violated: exprLayoutIdx called on non-value expression {s}",
                    .{@tagName(expr)},
                );
            }
            unreachable;
        },
        .empty_list => |l| l.list_layout,
        .list => |l| l.list_layout,
    };
}

/// Infer the wasm ValType that an expression will push onto the stack.
fn exprValType(self: *Self, expr_id: LirExprId) ValType {
    const expr = self.store.getExpr(expr_id);
    return switch (expr) {
        .i64_literal => |i| self.resolveValType(i.layout_idx),
        .f64_literal => .f64,
        .f32_literal => .f32,
        .bool_literal => .i32,
        .i128_literal => |i| self.resolveValType(i.layout_idx),
        .dec_literal => .i32, // pointer to stack memory
        .block => |b| self.exprValType(b.final_expr),
        .lookup => |l| self.resolveValType(l.layout_idx),
        .cell_load => |l| self.resolveValType(l.layout_idx),
        .if_then_else => |ite| self.resolveValType(ite.result_layout),
        .match_expr => |w| self.resolveValType(w.result_layout),
        .nominal => |nom| self.exprValType(nom.backing_expr),
        .empty_list => .i32, // pointer to 12-byte RocList
        .proc_call => |c| self.resolveValType(c.ret_layout),
        .struct_ => .i32, // pointer to stack memory
        .struct_access => |sa| self.resolveValType(sa.field_layout),
        .zero_arg_tag => .i32, // discriminant or pointer
        .tag => .i32, // pointer to stack memory
        .low_level => |ll| self.resolveValType(ll.ret_layout),
        .dbg => |d| self.resolveValType(d.result_layout),
        .expect => |e| self.resolveValType(e.result_layout),
        .incref => |inc| self.exprValType(inc.value),
        .decref => |dec| self.exprValType(dec.value),
        .free => |f| self.exprValType(f.value),
        .discriminant_switch => |ds| blk: {
            // Result type is determined by the branch expressions
            const branches = self.store.getExprSpan(ds.branches);
            break :blk if (branches.len > 0) self.exprValType(branches[0]) else .i32;
        },
        .early_return => |er| self.resolveValType(er.ret_layout),
        .str_literal => .i32,
        .list => .i32, // pointer to 12-byte RocList
        .str_concat => .i32, // pointer to 12-byte RocStr
        .int_to_str => .i32, // pointer to 12-byte RocStr
        .float_to_str => .i32, // pointer to 12-byte RocStr
        .dec_to_str => .i32, // pointer to 12-byte RocStr
        .str_escape_and_quote => .i32, // pointer to 12-byte RocStr
        .tag_payload_access => |tpa| self.resolveValType(tpa.payload_layout),
        .hosted_call => |hc| self.resolveValType(hc.ret_layout),
        .for_loop, .while_loop => .i32, // returns unit (empty record)
        .crash, .runtime_error, .break_expr => {
            if (builtin.mode == .Debug) std.debug.panic("LIR/wasm invariant violated: exprValType called on non-value expression {s}", .{@tagName(expr)});
            unreachable;
        },
    };
}

/// Get the byte size of the value an expression produces.
fn exprByteSize(self: *Self, expr_id: LirExprId) u32 {
    return self.layoutByteSize(self.exprLayoutIdx(expr_id));
}

/// Check if an expression produces a composite value (stored in stack memory).
fn isCompositeExpr(self: *const Self, expr_id: LirExprId) bool {
    const expr = self.store.getExpr(expr_id);
    return switch (expr) {
        .dec_literal => true, // 16 bytes in stack memory
        .i128_literal => |i| self.isCompositeLayout(i.layout_idx),
        .str_literal => true, // 12-byte RocStr in stack memory
        .list => true, // 12-byte RocList in stack memory
        .empty_list => true, // 12-byte RocList in stack memory
        .str_concat => true, // produces 12-byte RocStr
        .int_to_str => true, // produces 12-byte RocStr
        .float_to_str => true, // produces 12-byte RocStr
        .dec_to_str => true, // produces 12-byte RocStr
        .str_escape_and_quote => true, // produces 12-byte RocStr
        .struct_ => |s| self.isCompositeLayout(s.struct_layout),
        .tag => |t| self.isCompositeLayout(t.union_layout),
        .zero_arg_tag => |z| self.isCompositeLayout(z.union_layout),
        .nominal => |nom| self.isCompositeExpr(nom.backing_expr),
        .block => |b| self.isCompositeExpr(b.final_expr),
        .if_then_else => |ite| self.isCompositeLayout(ite.result_layout),
        .match_expr => |w| self.isCompositeLayout(w.result_layout),
        .lookup => |l| self.isCompositeLayout(l.layout_idx),
        .cell_load => |l| self.isCompositeLayout(l.layout_idx),
        .proc_call => |c| self.isCompositeLayout(c.ret_layout),
        .struct_access => |sa| self.isCompositeLayout(sa.field_layout),
        .low_level => |ll| self.isCompositeLayout(ll.ret_layout),
        .dbg => |d| self.isCompositeLayout(d.result_layout),
        .expect => |e| self.isCompositeLayout(e.result_layout),
        .tag_payload_access => |tpa| self.isCompositeLayout(tpa.payload_layout),
        .incref => |inc| self.isCompositeExpr(inc.value),
        .decref => |dec| self.isCompositeExpr(dec.value),
        .free => |f| self.isCompositeExpr(f.value),
        .discriminant_switch => |ds| self.isCompositeLayout(ds.result_layout),
        .hosted_call => |hc| self.isCompositeLayout(hc.ret_layout),
        .early_return => |er| self.isCompositeLayout(er.ret_layout),
        .i64_literal, .f64_literal, .f32_literal, .bool_literal => false, // scalars
        .for_loop, .while_loop, .break_expr, .crash, .runtime_error => false, // unit/noreturn
    };
}

/// True when a composite expression result may point into a callee-owned stack frame.
/// These values must be copied before binding if they need to outlive subsequent calls.
fn exprNeedsCompositeCallStabilization(self: *const Self, expr_id: LirExprId) bool {
    const expr = self.store.getExpr(expr_id);
    return switch (expr) {
        .proc_call => true,
        .nominal => |nom| self.exprNeedsCompositeCallStabilization(nom.backing_expr),
        .block => |b| self.exprNeedsCompositeCallStabilization(b.final_expr),
        .incref => |inc| self.exprNeedsCompositeCallStabilization(inc.value),
        .decref => |dec| self.exprNeedsCompositeCallStabilization(dec.value),
        .free => |f| self.exprNeedsCompositeCallStabilization(f.value),
        .dbg => |d| self.exprNeedsCompositeCallStabilization(d.expr) or self.exprNeedsCompositeCallStabilization(d.formatted),
        .expect => |e| self.exprNeedsCompositeCallStabilization(e.body),
        .if_then_else => |ite| blk: {
            const branches = self.store.getIfBranches(ite.branches);
            for (branches) |branch| {
                if (self.exprNeedsCompositeCallStabilization(branch.body)) break :blk true;
            }
            break :blk self.exprNeedsCompositeCallStabilization(ite.final_else);
        },
        .match_expr => |w| blk: {
            const branches = self.store.getMatchBranches(w.branches);
            for (branches) |branch| {
                if (self.exprNeedsCompositeCallStabilization(branch.body)) break :blk true;
            }
            break :blk false;
        },
        .discriminant_switch => |sw| blk: {
            const branches = self.store.getExprSpan(sw.branches);
            for (branches) |branch| {
                if (self.exprNeedsCompositeCallStabilization(branch)) break :blk true;
            }
            break :blk false;
        },
        else => false,
    };
}

/// Check if a layout represents a composite type stored in stack memory.
fn isCompositeLayout(self: *const Self, layout_idx: layout.Idx) bool {
    const repr = WasmLayout.wasmReprWithStore(layout_idx, self.getLayoutStore());
    return switch (repr) {
        .stack_memory => |s| s > 0,
        .primitive => false,
    };
}

/// Generate structural equality comparison for two composite values (records, tuples, tag unions).
/// Uses layout-aware comparison for fields containing heap types (strings, lists).
/// Leaves an i32 (bool) on the stack: 1 for equal, 0 for not equal.
fn generateStructuralEq(self: *Self, lhs: LirExprId, rhs: LirExprId, negate: bool) Allocator.Error!void {
    // Check for string/list type via layout
    {
        const lay_idx = self.exprLayoutIdx(lhs);
        if (lay_idx == .str) {
            try self.generateStrEq(lhs, rhs, negate);
            return;
        }
        const ls = self.getLayoutStore();
        const l = ls.getLayout(lay_idx);
        if (l.tag == .list or l.tag == .list_of_zst) {
            try self.generateListEq(lhs, rhs, lay_idx, negate);
            return;
        }
    }

    // Also check via expression type for list/string expressions where layout may not be available
    const lhs_expr = self.store.getExpr(lhs);
    switch (lhs_expr) {
        .list => |list_payload| {
            try self.generateListEqWithElemLayout(lhs, rhs, list_payload.elem_layout, negate);
            return;
        },
        .empty_list => |empty_list_payload| {
            try self.generateListEqWithElemLayout(lhs, rhs, empty_list_payload.elem_layout, negate);
            return;
        },
        .str_literal, .str_concat, .int_to_str, .float_to_str, .dec_to_str, .str_escape_and_quote => {
            // String equality should have been handled by layout check above, but catch it here too
            try self.generateStrEq(lhs, rhs, negate);
            return;
        },
        else => {},
    }

    // Generate both operand expressions — each pushes an i32 pointer
    try self.generateExpr(lhs);
    const lhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_local);

    try self.generateExpr(rhs);
    const rhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(rhs_local);

    // Try layout-aware comparison for records/tuples/tag unions containing heap types
    {
        const lay_idx = self.exprLayoutIdx(lhs);
        const ls = self.getLayoutStore();
        const l = ls.getLayout(lay_idx);
        switch (l.tag) {
            .struct_ => {
                try self.compareCompositeByLayout(lhs_local, rhs_local, lay_idx);
                if (negate) {
                    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
                }
                return;
            },
            .tag_union => {
                const tu_info = ls.getTagUnionInfo(l);
                if (tu_info.contains_refcounted) {
                    try self.compareTagUnionByLayout(lhs_local, rhs_local, lay_idx);
                    if (negate) {
                        self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
                    }
                    return;
                }
                // No heap types — fall through to bytewise comparison
            },
            else => {},
        }
    }

    // Fallback: bytewise comparison for types without heap-allocated fields
    const byte_size = self.exprByteSize(lhs);
    try self.emitBytewiseEq(lhs_local, rhs_local, byte_size);

    if (negate) {
        self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    }
}

/// Compare a composite type (record or tuple) field-by-field using layout information.
/// Pushes an i32 (1=equal, 0=not equal) onto the WASM stack.
fn compareCompositeByLayout(self: *Self, lhs_local: u32, rhs_local: u32, layout_idx: layout.Idx) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);

    switch (l.tag) {
        .struct_ => {
            const struct_idx = l.data.struct_.idx;
            const struct_data = ls.getStructData(struct_idx);
            const field_count = struct_data.fields.count;
            if (field_count == 0) {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
                return;
            }

            var first = true;
            var field_i: u32 = 0;
            while (field_i < field_count) : (field_i += 1) {
                const field_offset = ls.getStructFieldOffset(struct_idx, @intCast(field_i));
                const field_size = ls.getStructFieldSize(struct_idx, @intCast(field_i));
                const field_layout_idx = ls.getStructFieldLayout(struct_idx, @intCast(field_i));

                if (field_size == 0) continue;

                try self.compareFieldByLayout(lhs_local, rhs_local, field_offset, field_size, field_layout_idx);

                if (!first) {
                    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                }
                first = false;
            }

            if (first) {
                // All fields were zero-size
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            }
        },
        else => {
            // For non-composite types, fall back to bytewise comparison
            const byte_size = ls.layoutSizeAlign(l).size;
            try self.emitBytewiseEq(lhs_local, rhs_local, byte_size);
        },
    }
}

/// Compare a tag union by layout: compare discriminants first, then dispatch
/// per-variant payload comparison. Pushes i32 result onto the WASM stack.
fn compareTagUnionByLayout(self: *Self, lhs_local: u32, rhs_local: u32, layout_idx: layout.Idx) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);
    std.debug.assert(l.tag == .tag_union);

    const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
    const disc_offset = tu_data.discriminant_offset;
    const disc_size = tu_data.discriminant_size;

    // Allocate a local to hold the result
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // Load LHS discriminant
    if (disc_size == 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    } else {
        try self.emitLocalGet(lhs_local);
        try self.emitLoadBySize(disc_size, disc_offset);
    }
    const lhs_disc = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_disc);

    // Load RHS discriminant
    if (disc_size == 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    } else {
        try self.emitLocalGet(rhs_local);
        try self.emitLoadBySize(disc_size, disc_offset);
    }
    const rhs_disc = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(rhs_disc);

    // Compare discriminants
    try self.emitLocalGet(lhs_disc);
    try self.emitLocalGet(rhs_disc);
    self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
    const disc_eq_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(disc_eq_local);

    // If discriminants differ, result is 0
    // We use: result = disc_eq AND payload_eq
    // Start by assuming payload_eq = 1 (for the case where discriminants differ, we short-circuit)

    // Default result: discriminants equal ? (will be AND'd with payload comparison) : 0
    try self.emitLocalGet(disc_eq_local);
    try self.emitLocalSet(result_local);

    // Only compare payloads if discriminants are equal
    // Only compare payloads if discriminants are equal
    // block { if disc_ne: br 0; ... payload comparison ... } end
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, 0x40) catch return error.OutOfMemory; // void block type
    try self.emitLocalGet(disc_eq_local);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory; // disc_ne
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // break out of block

    // Payload comparison: compare based on variant
    // For simplicity, compare the payload bytes up to discriminant_offset
    // using layout-aware comparison for the variant's payload layout
    const variants = ls.getTagUnionVariants(tu_data);
    if (variants.len > 0) {
        const payload_size = disc_offset; // Payload occupies bytes [0..disc_offset)
        if (payload_size > 0) {
            // Compare payloads based on variant layout
            // For each variant, check if discriminant matches and compare payload
            // Use a local to accumulate payload equality
            const payload_eq_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

            // Default: payload equal (1) - will be overwritten
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            try self.emitLocalSet(payload_eq_local);

            for (0..variants.len) |variant_i| {
                const variant_payload_layout = variants.get(variant_i).payload_layout;
                const variant_payload_size = self.layoutByteSize(variant_payload_layout);

                if (variant_payload_size == 0) continue;

                // Check: if (disc == variant_i) { compare payload with this variant's layout }
                self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
                self.body.append(self.allocator, 0x40) catch return error.OutOfMemory; // void block

                // Skip if disc != variant_i
                try self.emitLocalGet(lhs_disc);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(variant_i)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

                // This variant matches - compare its payload
                const variant_layout_tag = ls.getLayout(variant_payload_layout).tag;
                if (variant_payload_layout == .str or variant_layout_tag == .struct_ or
                    variant_layout_tag == .tag_union or
                    variant_layout_tag == .list)
                {
                    // Layout-aware comparison
                    try self.compareFieldByLayout(lhs_local, rhs_local, 0, variant_payload_size, variant_payload_layout);
                } else {
                    // Bytewise comparison for scalar payloads
                    try self.emitBytewiseEq(lhs_local, rhs_local, variant_payload_size);
                }
                try self.emitLocalSet(payload_eq_local);

                self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end block
            }

            // result = disc_eq AND payload_eq
            try self.emitLocalGet(disc_eq_local);
            try self.emitLocalGet(payload_eq_local);
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            try self.emitLocalSet(result_local);
        }
    }

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end outer block

    // Push result
    try self.emitLocalGet(result_local);
}

/// Compare a single field by layout type. Pushes i32 (1=equal, 0=not equal) onto the WASM stack.
/// lhs_local/rhs_local are i32 pointers to the parent struct, field_offset is the byte offset.
fn compareFieldByLayout(
    self: *Self,
    lhs_local: u32,
    rhs_local: u32,
    field_offset: u32,
    field_size: u32,
    field_layout_idx: layout.Idx,
) Allocator.Error!void {
    if (field_layout_idx == .str) {
        // String: call roc_str_eq(lhs_ptr + offset, rhs_ptr + offset)
        const import_idx = self.str_eq_import orelse unreachable;
        try self.emitLocalGet(lhs_local);
        if (field_offset > 0) {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        }
        try self.emitLocalGet(rhs_local);
        if (field_offset > 0) {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        }
        self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
        return;
    }

    const ls = self.getLayoutStore();

    const field_layout = ls.getLayout(field_layout_idx);
    switch (field_layout.tag) {
        .list => {
            // List: call roc_list_eq or roc_list_str_eq
            const elem_layout = field_layout.data.list;
            if (elem_layout == .str) {
                const import_idx = self.list_str_eq_import orelse unreachable;
                try self.emitLocalGet(lhs_local);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                try self.emitLocalGet(rhs_local);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            } else if (ls.getLayout(elem_layout).tag == .list) {
                // List of lists - use specialized host function with inner element size
                const inner_elem_layout = ls.getLayout(elem_layout).data.list;
                const inner_elem_size = self.layoutByteSize(inner_elem_layout);
                const import_idx = self.list_list_eq_import orelse unreachable;
                try self.emitLocalGet(lhs_local);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                try self.emitLocalGet(rhs_local);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(inner_elem_size)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            } else if (ls.layoutContainsRefcounted(ls.getLayout(elem_layout))) {
                // Composite elements (records/tuples/tag-unions with refcounted fields):
                // inline element-by-element structural comparison loop.
                const elem_size = self.layoutByteSize(elem_layout);
                const lhs_list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                const rhs_list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

                try self.emitLocalGet(lhs_local);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                try self.emitLocalSet(lhs_list_local);

                try self.emitLocalGet(rhs_local);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                try self.emitLocalSet(rhs_list_local);

                try self.emitListEqLoop(lhs_list_local, rhs_list_local, elem_layout, elem_size);
            } else {
                // Simple scalar elements: bytewise comparison via host function
                const import_idx = self.list_eq_import orelse unreachable;
                const elem_size = self.layoutByteSize(elem_layout);
                try self.emitLocalGet(lhs_local);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                try self.emitLocalGet(rhs_local);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            }
        },
        .struct_, .tag_union => {
            // Nested composite: create offset locals and recurse
            const lhs_field_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const rhs_field_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

            try self.emitLocalGet(lhs_local);
            if (field_offset > 0) {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            }
            try self.emitLocalSet(lhs_field_local);

            try self.emitLocalGet(rhs_local);
            if (field_offset > 0) {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            }
            try self.emitLocalSet(rhs_field_local);

            if (field_layout.tag == .tag_union) {
                const tu_info = ls.getTagUnionInfo(field_layout);
                if (tu_info.contains_refcounted) {
                    try self.compareTagUnionByLayout(lhs_field_local, rhs_field_local, field_layout_idx);
                } else {
                    try self.emitBytewiseEq(lhs_field_local, rhs_field_local, field_size);
                }
            } else {
                try self.compareCompositeByLayout(lhs_field_local, rhs_field_local, field_layout_idx);
            }
        },
        else => {
            // Scalar/other: bytewise comparison at offset
            try self.emitBytewiseEqAtOffset(lhs_local, rhs_local, field_offset, field_size);
        },
    }
}

/// Emit an inline element-by-element comparison loop for two lists whose elements
/// need structural (non-bytewise) equality — e.g. lists of records/tuples/tag-unions
/// containing refcounted fields.
/// lhs_local/rhs_local are i32 pointers to 12-byte RocList structs.
/// Pushes i32 (1=equal, 0=not equal) onto the WASM stack.
fn emitListEqLoop(
    self: *Self,
    lhs_local: u32,
    rhs_local: u32,
    elem_layout_idx: layout.Idx,
    elem_size: u32,
) Allocator.Error!void {
    // Allocate scratch locals
    const lhs_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rhs_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const lhs_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rhs_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const idx_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const lhs_elem = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rhs_elem = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // Load lhs length (offset 4 in RocList)
    try self.emitLocalGet(lhs_local);
    try self.emitLoadOp(.i32, 4);
    try self.emitLocalSet(lhs_len);

    // Load rhs length (offset 4 in RocList)
    try self.emitLocalGet(rhs_local);
    try self.emitLoadOp(.i32, 4);
    try self.emitLocalSet(rhs_len);

    // result = (lhs_len == rhs_len)
    try self.emitLocalGet(lhs_len);
    try self.emitLocalGet(rhs_len);
    self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
    try self.emitLocalSet(result_local);

    // Load data pointers (offset 0 in RocList)
    try self.emitLocalGet(lhs_local);
    try self.emitLoadOp(.i32, 0);
    try self.emitLocalSet(lhs_data);

    try self.emitLocalGet(rhs_local);
    try self.emitLoadOp(.i32, 0);
    try self.emitLocalSet(rhs_data);

    // idx = 0
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(idx_local);

    // block { loop {
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // if result == 0, break (lengths didn't match or previous elem failed)
    try self.emitLocalGet(result_local);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    // if idx >= lhs_len, break (all elements compared)
    try self.emitLocalGet(idx_local);
    try self.emitLocalGet(lhs_len);
    self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    // lhs_elem = lhs_data + idx * elem_size
    try self.emitLocalGet(lhs_data);
    try self.emitLocalGet(idx_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_elem);

    // rhs_elem = rhs_data + idx * elem_size
    try self.emitLocalGet(rhs_data);
    try self.emitLocalGet(idx_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rhs_elem);

    // Compare elements structurally: pushes i32 result onto stack
    try self.compareFieldByLayout(lhs_elem, rhs_elem, 0, elem_size, elem_layout_idx);

    // result = result AND elem_eq
    try self.emitLocalGet(result_local);
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(result_local);

    // idx++
    try self.emitLocalGet(idx_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(idx_local);

    // br 0 (continue loop)
    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // end loop, end block
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Push final result
    try self.emitLocalGet(result_local);
}

/// Emit bytewise equality comparison of two memory regions.
/// lhs_local and rhs_local are i32 pointers, compared for byte_size bytes starting at offset 0.
/// Pushes an i32 (1=equal, 0=not equal) onto the WASM stack.
fn emitBytewiseEq(self: *Self, lhs_local: u32, rhs_local: u32, byte_size: u32) Allocator.Error!void {
    if (byte_size == 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
        return;
    }

    var offset: u32 = 0;
    var first = true;

    while (offset + 4 <= byte_size) : (offset += 4) {
        try self.emitLocalGet(lhs_local);
        self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    if (offset + 2 <= byte_size) {
        try self.emitLocalGet(lhs_local);
        self.body.append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.body.append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
        offset += 2;
    }

    if (offset < byte_size) {
        try self.emitLocalGet(lhs_local);
        self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    if (first) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    }
}

/// Emit bytewise equality comparison at a specific offset within parent structs.
/// Pushes an i32 (1=equal, 0=not equal) onto the WASM stack.
fn emitBytewiseEqAtOffset(self: *Self, lhs_local: u32, rhs_local: u32, base_offset: u32, byte_size: u32) Allocator.Error!void {
    if (byte_size == 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
        return;
    }

    var offset: u32 = 0;
    var first = true;

    while (offset + 4 <= byte_size) : (offset += 4) {
        try self.emitLocalGet(lhs_local);
        self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_offset + offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_offset + offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    if (offset + 2 <= byte_size) {
        try self.emitLocalGet(lhs_local);
        self.body.append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_offset + offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.body.append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_offset + offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
        offset += 2;
    }

    if (offset < byte_size) {
        try self.emitLocalGet(lhs_local);
        self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_offset + offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_offset + offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    if (first) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    }
}

/// Emit a load instruction appropriate for the discriminant size.
/// Loads an unsigned integer of disc_size bytes at the given offset from the address on the stack.
fn emitLoadBySize(self: *Self, disc_size: u8, offset: u16) Allocator.Error!void {
    switch (disc_size) {
        0 => {
            self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        },
        1 => {
            self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
        },
        2 => {
            self.body.append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
        },
        4 => {
            self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
        },
        8 => {
            self.body.append(self.allocator, Op.i64_load) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 3) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
            // Wrap to i32 since callers use i32 locals for discriminant values
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        else => unreachable,
    }
}

/// Copy a composite return value to a caller-owned buffer.
/// Takes an i32 pointer from the wasm stack, copies `size` bytes to a new
/// buffer in the current function's stack frame, and returns the local
/// holding the buffer pointer. This prevents stack use-after-free when
/// a subsequent function call reuses the callee's stack frame.
fn stabilizeCompositeResult(self: *Self, size: u32) Allocator.Error!u32 {
    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(src_local);

    const buf_offset = try self.allocStackMemory(size, 8);
    const buf_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(buf_offset);
    try self.emitLocalSet(buf_local);

    try self.emitMemCopy(buf_local, 0, src_local, size);

    return buf_local;
}

/// Generate composite (i128/Dec) numeric operations via LowLevel ops.
/// Both operands are i32 pointers to 16-byte values in linear memory.
fn generateCompositeNumericOp(self: *Self, op: anytype, args: []const LirExprId, ret_layout: layout.Idx, operand_layout: layout.Idx) Allocator.Error!void {
    // For comparison ops like num_is_eq, check for structural equality first
    if (op == .num_is_eq) {
        try self.generateStructuralEq(args[0], args[1], false);
        return;
    }

    // Generate operand pointers and stabilize them
    try self.generateExpr(args[0]);
    const lhs_local = try self.stabilizeCompositeResult(16);

    if (args.len > 1) {
        try self.generateExpr(args[1]);
        const rhs_local = try self.stabilizeCompositeResult(16);

        switch (op) {
            .num_plus => try self.emitI128Add(lhs_local, rhs_local),
            .num_minus => try self.emitI128Sub(lhs_local, rhs_local),
            .num_times => {
                if (operand_layout == .dec) {
                    const import_idx = self.dec_mul_import orelse unreachable;
                    const result_offset = try self.allocStackMemory(16, 8);
                    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitFpOffset(result_offset);
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
                    return;
                }
                try self.emitI128Mul(lhs_local, rhs_local);
            },
            .num_div_by => {
                if (operand_layout == .dec) {
                    const import_idx = self.dec_div_import orelse unreachable;
                    try self.emitI128HostBinOp(lhs_local, rhs_local, import_idx);
                } else {
                    const is_signed = operand_layout == .i128;
                    const import_idx = if (is_signed) self.i128_div_s_import else self.u128_div_import;
                    try self.emitI128HostBinOp(lhs_local, rhs_local, import_idx orelse unreachable);
                }
            },
            .num_div_trunc_by => {
                if (operand_layout == .dec) {
                    const import_idx = self.dec_div_trunc_import orelse unreachable;
                    try self.emitI128HostBinOp(lhs_local, rhs_local, import_idx);
                } else {
                    const is_signed = operand_layout == .i128;
                    const import_idx = if (is_signed) self.i128_div_s_import else self.u128_div_import;
                    try self.emitI128HostBinOp(lhs_local, rhs_local, import_idx orelse unreachable);
                }
            },
            .num_rem_by => {
                const is_signed = operand_layout == .i128 or operand_layout == .dec;
                const import_idx = if (is_signed) self.i128_mod_s_import else self.u128_mod_import;
                try self.emitI128HostBinOp(lhs_local, rhs_local, import_idx orelse unreachable);
            },
            .num_mod_by => {
                const is_signed = operand_layout == .i128 or operand_layout == .dec;
                const import_idx = if (is_signed) self.i128_mod_s_import else self.u128_mod_import;
                try self.emitI128HostBinOp(lhs_local, rhs_local, import_idx orelse unreachable);
            },
            .num_is_gt => {
                const is_signed = operand_layout == .i128 or operand_layout == .dec;
                try self.emitI128CompareWithSignedness(lhs_local, rhs_local, .gt, is_signed);
            },
            .num_is_gte => {
                const is_signed = operand_layout == .i128 or operand_layout == .dec;
                try self.emitI128CompareWithSignedness(lhs_local, rhs_local, .gte, is_signed);
            },
            .num_is_lt => {
                const is_signed = operand_layout == .i128 or operand_layout == .dec;
                try self.emitI128CompareWithSignedness(lhs_local, rhs_local, .lt, is_signed);
            },
            .num_is_lte => {
                const is_signed = operand_layout == .i128 or operand_layout == .dec;
                try self.emitI128CompareWithSignedness(lhs_local, rhs_local, .lte, is_signed);
            },
            .num_abs_diff => {
                const is_signed = operand_layout == .i128 or operand_layout == .dec;
                try self.emitI128CompareWithSignedness(lhs_local, rhs_local, .gte, is_signed);
                self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;
                try self.emitI128Sub(lhs_local, rhs_local);
                self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
                try self.emitI128Sub(rhs_local, lhs_local);
                self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            },
            else => unreachable,
        }
    } else {
        // Unary composite op (num_neg handled before calling this function)
        switch (op) {
            .num_negate => try self.generateCompositeI128Negate(args[0], ret_layout),
            .num_abs => {
                if (operand_layout == .u128) {
                    try self.generateExpr(args[0]);
                } else {
                    try self.generateCompositeI128Abs(args[0]);
                }
            },
            else => unreachable,
        }
    }
}

/// Emit an i128 binary operation via host function call.
/// The host function takes (lhs_ptr, rhs_ptr, result_ptr) and returns void.
/// Pushes an i32 pointer to the 16-byte result on the wasm stack.
fn emitI128HostBinOp(self: *Self, lhs_local: u32, rhs_local: u32, import_idx: u32) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_local);

    // Call host function: (lhs_ptr, rhs_ptr, result_ptr) -> void
    try self.emitLocalGet(lhs_local);
    try self.emitLocalGet(rhs_local);
    try self.emitLocalGet(result_local);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;

    // Push result pointer
    try self.emitLocalGet(result_local);
}

/// Emit i128 addition: result = lhs + rhs
/// Pushes an i32 pointer to the 16-byte result on the wasm stack.
/// Pre-loads all operand words into locals to avoid aliasing issues
/// when result memory overlaps with lhs/rhs (e.g., in loops).
fn emitI128Add(self: *Self, lhs_local: u32, rhs_local: u32) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Pre-load all operand words into locals (prevents aliasing with result memory)
    const a_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_low) catch return error.OutOfMemory;

    const a_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_high) catch return error.OutOfMemory;

    const b_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_low) catch return error.OutOfMemory;

    const b_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_high) catch return error.OutOfMemory;

    // result_low = a_low + b_low
    const result_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_low) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_low) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_low) catch return error.OutOfMemory;

    // Store result_low
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // carry = (result_low < a_low) ? 1 : 0
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_low) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_low) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_lt_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;

    // result_high = a_high + b_high + carry
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;

    // Store result_high
    const result_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_high) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Emit i128 subtraction: result = lhs - rhs
/// Pushes an i32 pointer to the 16-byte result on the wasm stack.
/// Pre-loads all operand words into locals to avoid aliasing issues
/// when result memory overlaps with lhs/rhs (e.g., in loops).
fn emitI128Sub(self: *Self, lhs_local: u32, rhs_local: u32) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Pre-load all operand words into locals (prevents aliasing with result memory)
    const a_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_low) catch return error.OutOfMemory;

    const a_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_high) catch return error.OutOfMemory;

    const b_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_low) catch return error.OutOfMemory;

    const b_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_high) catch return error.OutOfMemory;

    // result_low = a_low - b_low
    const result_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_low) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_low) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_low) catch return error.OutOfMemory;

    // Store result_low
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // borrow = (a_low < b_low) ? 1 : 0
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_low) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_low) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_lt_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    const borrow_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, borrow_local) catch return error.OutOfMemory;

    // result_high = (a_high - b_high) - borrow
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, borrow_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;

    // Store result_high
    const result_high_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_high_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_high_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Emit i128 × i128 → i128 truncating multiply.
/// Takes two i32 pointers to 16-byte i128 values in linear memory.
/// Pushes an i32 pointer to the 16-byte result.
///
/// Algorithm:
///   a = (a_hi, a_lo), b = (b_hi, b_lo)  (each hi/lo is i64)
///   result_lo = a_lo * b_lo  (truncating i64.mul)
///   result_hi = high64(a_lo * b_lo) + (a_lo * b_hi) + (a_hi * b_lo)
///   (a_hi * b_hi contributes only to bits 128+, discarded)
///
/// For high64(a_lo * b_lo), uses 32-bit schoolbook method (same as emitI64MulToI128).
fn emitI128Mul(self: *Self, lhs_local: u32, rhs_local: u32) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Pre-load all operand words into locals
    const a_lo = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_lo) catch return error.OutOfMemory;

    const a_hi = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_hi) catch return error.OutOfMemory;

    const b_lo = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_lo) catch return error.OutOfMemory;

    const b_hi = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_hi) catch return error.OutOfMemory;

    // --- result_lo = a_lo * b_lo (truncating i64.mul) ---
    const result_lo_val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_lo) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_lo) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_lo_val) catch return error.OutOfMemory;

    // Store result_lo
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_lo_val) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // --- Compute high64(a_lo * b_lo) using 32-bit schoolbook method ---
    // Split a_lo into 32-bit halves: al0 = a_lo & 0xFFFFFFFF, al1 = a_lo >> 32
    const al0 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const al1 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_lo) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0xFFFFFFFF) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, al0) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_lo) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, al1) catch return error.OutOfMemory;

    // Split b_lo similarly
    const bl0 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const bl1 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_lo) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0xFFFFFFFF) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, bl0) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_lo) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, bl1) catch return error.OutOfMemory;

    // t = al0 * bl0
    const t = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, al0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, bl0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, t) catch return error.OutOfMemory;

    // cross = (t >> 32) + al1*bl0
    const cross = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, t) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, al1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, bl0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;

    // cross = cross + al0*bl1 (may overflow u64 — need to track carry)
    // Save old cross for overflow detection
    const old_cross = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, old_cross) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, al0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, bl1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;

    // carry = (cross < old_cross) ? 1 : 0  (unsigned overflow detection)
    const carry = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, old_cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_lt_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, carry) catch return error.OutOfMemory;

    // high64_of_lo_mul = al1*bl1 + (cross >> 32) + (carry << 32)
    const hi_of_lo_mul = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, al1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, bl1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    // Add carry << 32 (carry is 0 or 1, so carry << 32 is 0 or 0x100000000)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, carry) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, hi_of_lo_mul) catch return error.OutOfMemory;

    // --- result_hi = high64(a_lo * b_lo) + (a_lo * b_hi) + (a_hi * b_lo) ---
    // Start with hi_of_lo_mul
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, hi_of_lo_mul) catch return error.OutOfMemory;

    // + a_lo * b_hi (truncating, only lower 64 bits matter)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_lo) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_hi) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;

    // + a_hi * b_lo (truncating, only lower 64 bits matter)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_hi) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_lo) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;

    // Store result_hi
    const result_hi_val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_hi_val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_hi_val) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

const I128CmpOp = enum { lt, lte, gt, gte };

fn emitI128CompareWithSignedness(self: *Self, lhs_local: u32, rhs_local: u32, cmp_op: I128CmpOp, is_signed: bool) Allocator.Error!void {
    // Signed i128 comparison strategy:
    // Compare high words (signed). If different, that determines the result.
    // If equal, compare low words (unsigned).
    //
    // Using wasm if/else:
    //   a_high = load lhs+8; b_high = load rhs+8
    //   if (a_high == b_high)
    //     result = a_low <cmp_unsigned> b_low
    //   else
    //     result = a_high <cmp_signed> b_high

    // Load high words
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const a_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_high) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const b_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_high) catch return error.OutOfMemory;

    // if (a_high == b_high)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;
    // if (result is i32)
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;

    // Then: compare low words unsigned
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    const low_cmp: u8 = switch (cmp_op) {
        .lt => Op.i64_lt_u,
        .lte => Op.i64_le_u,
        .gt => Op.i64_gt_u,
        .gte => Op.i64_ge_u,
    };
    self.body.append(self.allocator, low_cmp) catch return error.OutOfMemory;

    // Else: compare high words signed
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_high) catch return error.OutOfMemory;
    const high_cmp: u8 = switch (cmp_op) {
        .lt => if (is_signed) Op.i64_lt_s else Op.i64_lt_u,
        .lte => if (is_signed) Op.i64_le_s else Op.i64_le_u,
        .gt => if (is_signed) Op.i64_gt_s else Op.i64_gt_u,
        .gte => if (is_signed) Op.i64_ge_s else Op.i64_ge_u,
    };
    self.body.append(self.allocator, high_cmp) catch return error.OutOfMemory;

    // End if
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Emit i128 bitwise operation (AND, OR, XOR) on both halves.
/// Result is a pointer to 16-byte stack memory.
/// Generate i128/Dec negation: result = -value (two's complement)
fn generateCompositeI128Negate(self: *Self, expr: LirExprId, _: layout.Idx) Allocator.Error!void {
    try self.generateExpr(expr);
    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;

    try self.emitCompositeI128NegateFromLocal(src_local);
}

fn emitCompositeI128NegateFromLocal(self: *Self, src_local: u32) Allocator.Error!void {
    // Allocate result
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Two's complement: -x = ~x + 1
    // low = ~a_low + 1
    // carry = (low == 0) ? 1 : 0  (overflow when ~a_low was 0xFFFF... i.e. a_low was 0)
    // high = ~a_high + carry

    // Compute ~a_low + 1
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, -1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.body.append(self.allocator, Op.i64_xor) catch return error.OutOfMemory;
    // Stack: ~a_low
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    // Stack: result_low = ~a_low + 1

    const result_low_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_low_local) catch return error.OutOfMemory;

    // Store result_low
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_low_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // carry = (result_low == 0) ? 1 : 0
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_low_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;

    // high = ~a_high + carry
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, -1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.body.append(self.allocator, Op.i64_xor) catch return error.OutOfMemory;
    // Stack: [carry, ~a_high]
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    // Stack: [result_high]

    // Store result_high
    const result_high_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_high_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_high_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

fn generateCompositeI128Abs(self: *Self, expr: LirExprId) Allocator.Error!void {
    try self.generateExpr(expr);
    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_lt_s) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;
    try self.emitCompositeI128NegateFromLocal(src_local);
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Multiply two i64 values producing a 128-bit result stored in stack memory.
/// Takes two wasm locals holding i64 operands.
/// Returns via stack: pushes an i32 pointer to the 16-byte result.
///
/// Algorithm (schoolbook multiply with 32-bit limbs):
///   a = (a1 << 32) + a0,  b = (b1 << 32) + b0
///   low  = a * b (truncating i64.mul)
///   high = a1*b1 + ((a0*b0 >> 32) + a0*b1 + a1*b0) >> 32
///         (but we must track 64-bit carry properly)
fn emitI64MulToI128(self: *Self, a_local: u32, b_local: u32) Allocator.Error!void {
    // Allocate result memory
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Split a into 32-bit halves: a0 = a & 0xFFFFFFFF, a1 = a >> 32
    const a0 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const a1 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    // a0 = a & 0xFFFFFFFF
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0xFFFFFFFF) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a0) catch return error.OutOfMemory;
    // a1 = a >>> 32 (unsigned shift)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a1) catch return error.OutOfMemory;

    // Split b similarly
    const b0 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const b1 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0xFFFFFFFF) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b1) catch return error.OutOfMemory;

    // Compute low = a * b (truncating multiply gives lower 64 bits)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // Compute high word using schoolbook method:
    // t = a0*b0
    // cross = (t >> 32) + a0*b1 + a1*b0  (can overflow, but we only need lower 64 bits + carry)
    // high = a1*b1 + (cross >> 32)

    // t = a0 * b0
    const t = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, t) catch return error.OutOfMemory;

    // cross1 = (t >> 32) + a1*b0
    const cross = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, t) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;

    // cross2 = cross1 + a0*b1 (can carry past 64 bits — must track carry)
    // Save old cross for overflow detection
    const old_cross = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, old_cross) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;

    // carry = (cross < old_cross) ? 1 : 0  (unsigned overflow detection)
    const carry = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, old_cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_lt_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, carry) catch return error.OutOfMemory;

    // high = a1*b1 + (cross >> 32) + (carry << 32)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, b1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, cross) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    // Add carry << 32
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, carry) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Emit i128 signed division: result = a / b (truncating).
/// Takes two i32 pointers to 16-byte i128 values.
/// For Dec→int conversions, we only need division by a constant (10^18).
/// This implementation handles the general case for positive divisors.
fn emitI128DivByConst(self: *Self, numerator_local: u32, divisor_val: i64) Allocator.Error!void {
    // For Dec→int: we divide by 10^18 (positive constant).
    // Strategy: use signed division.
    // For simplicity, handle only the case where the numerator fits in i64
    // after division (which is always true for Dec→i64 and smaller).
    //
    // result = (i128 as i64-pair) / divisor
    // Since divisor fits in i64 and result fits in i64, we can compute:
    //   result = ((high * 2^64) + low) / divisor
    //
    // For signed division when high == 0 or high == -1 (sign extension),
    // the value fits in i64 and we can do i64.div_s directly.
    //
    // General approach: extract the full i128, then truncate to i64 and divide.
    // This works because the result of Dec→int always fits in i64.

    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Load the low i64 from the numerator
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, numerator_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);

    // Divide by divisor
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, divisor_val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;

    // Store as i128 (sign-extend to high word)
    const quotient = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, quotient) catch return error.OutOfMemory;

    // Store low word
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, quotient) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // Store high word (sign extension)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, quotient) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 63) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Convert an i64 value on the wasm stack to a 16-byte i128 in stack memory.
/// The caller must ensure the value is i64 (extend i32 first if needed).
/// If `signed` is true, sign-extends the high word; otherwise zero-extends.
/// Pushes an i32 pointer to the 16-byte result.
fn emitIntToI128(self: *Self, signed: bool) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Save the i64 value from the stack
    const val_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val_local) catch return error.OutOfMemory;

    // Store low word
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // Store high word
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    if (signed) {
        // Sign extend: high = value >> 63 (arithmetic shift)
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, val_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, &self.body, 63) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
    } else {
        // Zero extend: high = 0
        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    }
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Convert f64 value (in val_local) to i128, storing result at result_local pointer.
/// If signed=true, uses signed truncation for the high word; unsigned otherwise.
/// Pushes the result pointer onto the stack.
fn emitF64ToI128(self: *Self, val_local: u32, result_local: u32, signed: bool) Allocator.Error!void {
    // high = trunc(val / 2^64)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_div) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_trunc) catch return error.OutOfMemory;
    const high_f = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high_f) catch return error.OutOfMemory;
    if (signed) {
        self.body.append(self.allocator, Op.i64_trunc_f64_s) catch return error.OutOfMemory;
    } else {
        self.body.append(self.allocator, Op.i64_trunc_f64_u) catch return error.OutOfMemory;
    }
    const high_i = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high_i) catch return error.OutOfMemory;
    // low = (val - high_f * 2^64) as u64
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high_f) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_sub) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_trunc_f64_u) catch return error.OutOfMemory;
    const low_i = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, low_i) catch return error.OutOfMemory;
    // Store low and high words
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, low_i) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high_i) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);
    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Emit float-to-int try_unsafe conversion.
/// Returns a record {val: IntType, is_int: Bool, in_range: Bool} stored in stack memory.
/// The f64 value should already be on the wasm stack.
/// val_size is the byte size of the target integer (1, 2, 4, or 8).
/// min_f and max_f are the f64 bounds for the target integer type.
fn emitFloatToIntTryUnsafe(self: *Self, val_size: u32, is_i64: bool, min_f: f64, max_f: f64) Allocator.Error!void {
    // Total record size: val_size + 2 (is_int + in_range bools), aligned to val_size
    const total_size = if (is_i64) @as(u32, 16) else @as(u32, 8); // align to 8 or 4
    const alignment: u32 = if (is_i64) 8 else 4;
    const result_offset = try self.allocStackMemory(total_size, alignment);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Save the f64 value
    const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;

    // Compute is_int: !isNaN(val) && !isInf(val) && trunc(val) == val
    // !isNaN: val == val
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_eq) catch return error.OutOfMemory;
    // !isInf: abs(val) != inf
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(std.math.inf(f64)))) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_ne) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    // trunc(val) == val
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_trunc) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_eq) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    const is_int = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, is_int) catch return error.OutOfMemory;

    // Compute in_range: val >= min_f && val <= max_f
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(min_f))) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_ge) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(max_f))) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_le) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    const in_range = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, in_range) catch return error.OutOfMemory;

    // Store value (only if is_int && in_range — but for try_unsafe we always store)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    if (is_i64) {
        self.body.append(self.allocator, Op.i64_trunc_f64_s) catch return error.OutOfMemory;
        try self.emitStoreOp(.i64, 0);
    } else {
        self.body.append(self.allocator, Op.i32_trunc_f64_s) catch return error.OutOfMemory;
        try self.emitStoreOp(.i32, 0);
    }

    // Store is_int at offset val_size
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, is_int) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, val_size);

    // Store in_range at offset val_size + 1
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, in_range) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, val_size + 1);

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Emit f64→i128/u128 try_unsafe conversion.
/// Returns {val: i128, is_int: bool, in_range: bool} — 18 bytes, padded to 24 with align 8.
/// f64 value is on the wasm stack. signed=true for i128, false for u128.
fn emitFloatToI128TryUnsafe(self: *Self, signed: bool) Allocator.Error!void {
    // Result struct: 16 bytes val + 1 byte is_int + 1 byte in_range = 18, padded to 24
    const result_offset = try self.allocStackMemory(24, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Save the f64 value
    const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;

    // Compute is_int: !isNaN(val) && !isInf(val) && trunc(val) == val
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_eq) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(std.math.inf(f64)))) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_ne) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_trunc) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_eq) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    const is_int = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, is_int) catch return error.OutOfMemory;

    // Compute in_range using f64 bounds
    if (signed) {
        // i128 range: roughly -1.7e38 to 1.7e38
        const min_f: f64 = -170141183460469231731687303715884105728.0;
        const max_f: f64 = 170141183460469231731687303715884105727.0;
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
        self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(min_f))) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.f64_ge) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
        self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(max_f))) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.f64_le) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    } else {
        // u128 range: 0 to ~3.4e38
        const max_f: f64 = 340282366920938463463374607431768211455.0;
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
        self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 0.0)))) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.f64_ge) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
        self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(max_f))) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.f64_le) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    }
    const in_range = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, in_range) catch return error.OutOfMemory;

    // Convert f64 to i128 and store as val (16 bytes at offset 0)
    try self.emitF64ToI128(val, result_local, signed);

    // Store is_int at offset 16
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, is_int) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 16);

    // Store in_range at offset 17
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, in_range) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 17);

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Emit an integer try conversion that returns a Result(TargetInt, {}) tag union.
/// Layout: payload at offset 0 (payload_size bytes), discriminant at disc_offset (1 byte).
/// Ok = discriminant 1, Err = discriminant 0.
/// Expects the source value as i32 or i64 on the wasm stack.
/// `cond_ops` should emit a comparison that leaves 1 (in range) or 0 (out of range) on stack.
fn emitIntTryResult(
    self: *Self,
    src_vt: ValType,
    payload_size: u32,
    disc_offset: u32,
) Allocator.Error!struct { result_local: u32, val_local: u32 } {
    // Save the source value
    const val_local = switch (src_vt) {
        .i32 => self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .i64 => self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory,
        .f32, .f64 => unreachable,
    };
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val_local) catch return error.OutOfMemory;

    // Allocate result
    const total_size = disc_offset + 1;
    const alignment: u32 = if (payload_size >= 8) 8 else if (payload_size >= 4) 4 else if (payload_size >= 2) 2 else 1;
    const padded = (total_size + alignment - 1) & ~(alignment - 1);
    const result_offset = try self.allocStackMemory(padded, alignment);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Zero out discriminant (Err by default)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, disc_offset);

    return .{ .result_local = result_local, .val_local = val_local };
}

/// Finish an integer try conversion: store the value and set discriminant to Ok (1).
/// Should be called inside an `if` block that checked the range condition.
fn emitIntTryOk(
    self: *Self,
    result_local: u32,
    val_local: u32,
    src_vt: ValType,
    payload_size: u32,
    disc_offset: u32,
) Allocator.Error!void {
    // Store value
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, val_local) catch return error.OutOfMemory;
    if (src_vt == .i64 and payload_size <= 4) {
        self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        try self.emitStoreOpSized(.i32, payload_size, 0);
    } else if (src_vt == .i32 and payload_size < 4) {
        try self.emitStoreOpSized(.i32, payload_size, 0);
    } else if (payload_size == 8) {
        try self.emitStoreOp(.i64, 0);
    } else {
        try self.emitStoreOp(.i32, 0);
    }

    // Set discriminant to 1 (Ok)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, disc_offset);
}

/// Emit an i128/u128 → smaller integer try conversion.
/// Source is an i32 pointer to 16 bytes in memory.
/// target_bytes: byte size of target type (1, 2, 4, 8)
/// signed_source: true if source is i128, false if u128
/// signed_target: true if target is signed (i8-i64), false if unsigned (u8-u64)
fn emitI128TryNarrow(
    self: *Self,
    target_bytes: u32,
    signed_source: bool,
    signed_target: bool,
) Allocator.Error!void {
    // Save source pointer
    const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr) catch return error.OutOfMemory;

    // Load low i64 from [src_ptr + 0]
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;

    // Load high i64 from [src_ptr + 8]
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;

    // Determine result layout
    // Payload is the target type in wasm representation
    const payload_wasm_size: u32 = if (target_bytes <= 4) 4 else 8;
    const disc_offset: u32 = payload_wasm_size;
    const total_size = disc_offset + 1;
    const alignment: u32 = payload_wasm_size;
    const padded = (total_size + alignment - 1) & ~(alignment - 1);

    // Allocate result
    const result_offset = try self.allocStackMemory(padded, alignment);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Zero discriminant (Err by default)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, disc_offset);

    // Build the range check condition
    // For unsigned target: high must be 0, AND low must be <= max_unsigned
    // For signed target from unsigned source: high must be 0, AND low must be <= max_signed (as unsigned)
    // For signed target from signed source: high must be sign-extension of low's upper bits
    if (!signed_target) {
        // Unsigned target: high == 0
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_eqz) catch return error.OutOfMemory;

        if (target_bytes < 8) {
            // AND low <= max_unsigned_target
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            const max_val: i64 = (@as(i64, 1) << @intCast(target_bytes * 8)) - 1;
            WasmModule.leb128WriteI64(self.allocator, &self.body, max_val) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        // For target_bytes == 8: just high == 0 is sufficient
    } else if (!signed_source) {
        // Signed target from unsigned source: high == 0 AND low <= max_signed_target
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_eqz) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        const max_signed: i64 = (@as(i64, 1) << @intCast(target_bytes * 8 - 1)) - 1;
        WasmModule.leb128WriteI64(self.allocator, &self.body, max_signed) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    } else {
        // Signed target from signed source (i128 → i8/i16/i32/i64)
        // Value fits if sign-extending the low N bits back to i128 gives the same value.
        // Simplified check:
        if (target_bytes < 8) {
            // For targets smaller than i64:
            // The value must fit in the signed range of target_bytes.
            // high must be 0 (positive) or -1 (negative),
            // AND it must match the sign of low's relevant bits,
            // AND low must be in range.

            // Check 1: high == 0 AND low in [0, max_signed]
            // OR high == -1 AND low sign-extended from target_bytes == low
            // Simplified: sign-extend low from target_bytes to i64,
            // check it equals low, AND high is sign extension of that.

            // Sign-extend low from target_bytes:
            const bit_count = target_bytes * 8;
            const sign_ext_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;
            // Shift left by (64 - bit_count), then arithmetic shift right by (64 - bit_count)
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, @intCast(64 - bit_count)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, @intCast(64 - bit_count)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, sign_ext_low) catch return error.OutOfMemory;

            // Check: sign_ext_low == low
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, sign_ext_low) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;

            // AND high == (sign_ext_low >> 63)  (sign extension of the sign-extended low)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, sign_ext_low) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 63) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        } else {
            // i128 → i64: high must equal (low >> 63) (sign extension)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 63) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;
        }
    }

    // If condition is true, store Ok result
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // Store payload (truncated value)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;
    if (target_bytes <= 4) {
        self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        try self.emitStoreOpSized(.i32, target_bytes, 0);
    } else {
        try self.emitStoreOp(.i64, 0);
    }

    // Set discriminant = 1 (Ok)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, disc_offset);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Emit i128 → u128 try conversion (or signed widening → u128 try).
/// Source is an i32 pointer to 16 bytes. Check high word sign bit is 0.
/// Result is a Result(U128, {}) — 16-byte payload at offset 0, disc at offset 16.
fn emitI128TryToU128(self: *Self, _: bool) Allocator.Error!void {
    const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr) catch return error.OutOfMemory;

    // Load high word
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;

    // Load low word
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;

    // Allocate result: 16 bytes payload + 1 byte disc, aligned to 8
    const result_offset = try self.allocStackMemory(24, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Zero discriminant at offset 16
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 16);

    // Check: high >= 0 (sign bit not set)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // Store payload (copy both words)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Set disc = 1 (Ok)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 16);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Emit u128 → i128 try conversion.
/// Source is an i32 pointer to 16 bytes. Check value < 2^127 (high bit not set).
fn emitI128TryToI128(self: *Self) Allocator.Error!void {
    const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr) catch return error.OutOfMemory;

    // Load high word
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;

    // Load low word
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;

    // Allocate result: 16 bytes payload + 1 byte disc, aligned to 8
    const result_offset = try self.allocStackMemory(24, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

    // Zero discriminant
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 16);

    // Check: high >= 0 (MSB not set, value < 2^127)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // Store payload
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, high) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Set disc = 1 (Ok)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 16);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
}

/// Resolve a layout.Idx to its wasm ValType, using the layout store for dynamic indices.
/// This is the safe way to map layout indices that might be dynamically allocated
/// (not one of the well-known sentinel values like .bool, .i32, etc.).
/// If a lambda's body returns an unwrapped_capture closure, get the capture's layout.
fn resolveValType(self: *const Self, layout_idx: layout.Idx) ValType {
    return WasmLayout.resultValTypeWithStore(layout_idx, self.getLayoutStore());
}

/// Allocate space on the stack frame, returning the offset from the frame pointer.
/// Ensures the frame pointer local exists and memory is enabled.
fn allocStackMemory(self: *Self, size: u32, alignment: u32) Allocator.Error!u32 {
    if (!self.uses_stack_memory) {
        self.uses_stack_memory = true;
        // Reserve local 0 for frame pointer (or next available local)
        self.fp_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    }
    // Align up
    const aligned_offset = (self.stack_frame_size + alignment - 1) & ~(alignment - 1);
    self.stack_frame_size = aligned_offset + size;
    return aligned_offset;
}

/// Emit bump allocation: allocates `size` bytes with given alignment
/// from the heap (global 1). Leaves the allocated pointer on the wasm stack.
/// This is a simple bump allocator that never frees — suitable for tests.
/// Emit heap allocation via roc_alloc (call_indirect through RocOps).
/// `size_local` holds the size to allocate; `alignment` is the byte alignment.
/// Leaves the allocated pointer on the wasm stack.
fn emitHeapAlloc(self: *Self, size_local: u32, alignment: u32) Allocator.Error!void {
    // Allocate 12-byte RocAlloc struct on stack frame: {alignment: u32, length: u32, answer: u32}
    const alloc_slot = try self.allocStackMemory(12, 4);

    // Write alignment field
    try self.emitFpOffset(alloc_slot);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(alignment)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // Write length field
    try self.emitFpOffset(alloc_slot);
    try self.emitLocalGet(size_local);
    self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;

    // Push call_indirect args: (alloc_args_ptr, env_ptr)
    try self.emitFpOffset(alloc_slot); // args_ptr
    try self.emitLocalGet(self.roc_ops_local); // load roc_ops_ptr
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory; // load env from offset 0
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // Load roc_alloc table index from roc_ops_ptr offset 4
    try self.emitLocalGet(self.roc_ops_local);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;

    // call_indirect with RocOps function type, table 0
    self.body.append(self.allocator, Op.call_indirect) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, self.roc_ops_type_idx) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // table index 0

    // Read answer from struct (offset +8) → result pointer on stack
    try self.emitFpOffset(alloc_slot);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 8) catch return error.OutOfMemory;
}

/// Emit heap allocation via roc_alloc with a constant size.
/// Leaves the allocated pointer on the wasm stack.
fn emitHeapAllocConst(self: *Self, size: u32, alignment: u32) Allocator.Error!void {
    // Store size in a temp local, then delegate to emitHeapAlloc
    const size_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, size_local) catch return error.OutOfMemory;
    try self.emitHeapAlloc(size_local, alignment);
}

/// Emit i32.store to a func_body buffer: stores a local's value at memory offset 0 + field_offset.
/// Used during main() prologue to build the RocOps struct.
fn emitI32StoreToBody(self: *Self, func_body: *std.ArrayList(u8), field_offset: u32, local_idx: u32, _: ?void) Allocator.Error!void {
    // i32.const 0  (base address of RocOps struct)
    func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, func_body, 0) catch return error.OutOfMemory;
    // local.get $local_idx
    func_body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, func_body, local_idx) catch return error.OutOfMemory;
    // i32.store offset=field_offset
    func_body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, func_body, 2) catch return error.OutOfMemory; // alignment log2(4)
    WasmModule.leb128WriteU32(self.allocator, func_body, field_offset) catch return error.OutOfMemory;
}

/// Emit i32.store to a func_body buffer: stores a constant value at memory offset 0 + field_offset.
/// Used during main() prologue to build the RocOps struct.
fn emitI32StoreConstToBody(self: *Self, func_body: *std.ArrayList(u8), field_offset: u32, value: u32) Allocator.Error!void {
    // i32.const 0  (base address of RocOps struct)
    func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, func_body, 0) catch return error.OutOfMemory;
    // i32.const value
    func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, func_body, @intCast(value)) catch return error.OutOfMemory;
    // i32.store offset=field_offset
    func_body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, func_body, 2) catch return error.OutOfMemory; // alignment log2(4)
    WasmModule.leb128WriteU32(self.allocator, func_body, field_offset) catch return error.OutOfMemory;
}

/// Emit: local.get $fp; i32.const offset; i32.add
/// Leaves (fp + offset) on the stack as an i32 pointer.
fn emitFpOffset(self: *Self, offset: u32) Allocator.Error!void {
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, self.fp_local) catch return error.OutOfMemory;
    if (offset > 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(offset)) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    }
}

/// Emit: i64.store with alignment 3 (8 bytes) and the given offset.
fn emitI64Store(self: *Self, offset: u32) Allocator.Error!void {
    self.body.append(self.allocator, Op.i64_store) catch return error.OutOfMemory;
    // alignment (log2 of bytes): 3 = 8-byte aligned
    WasmModule.leb128WriteU32(self.allocator, &self.body, 3) catch return error.OutOfMemory;
    // offset
    WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
}

/// Copy a stack_memory value from a (potentially dangling) source pointer to
/// Emit a wasm conversion instruction if source and target types differ.
fn emitConversion(self: *Self, source: ValType, target: ValType) Allocator.Error!void {
    if (source == target) return;

    // Wasm conversion opcodes:
    // i32.wrap_i64      = 0xA7
    // i64.extend_i32_s  = 0xAC
    // i64.extend_i32_u  = 0xAD
    // f32.convert_i32_s = 0xB2
    // f32.convert_i64_s = 0xB4
    // f64.convert_i32_s = 0xB7
    // f64.convert_i64_s = 0xB9
    // f32.demote_f64    = 0xB6
    // f64.promote_f32   = 0xBB
    const op: ?u8 = switch (source) {
        .i64 => switch (target) {
            .i32 => @as(u8, 0xA7), // i32.wrap_i64
            .f32 => @as(u8, 0xB4), // f32.convert_i64_s
            .f64 => @as(u8, 0xB9), // f64.convert_i64_s
            else => null,
        },
        .i32 => switch (target) {
            .i64 => @as(u8, 0xAC), // i64.extend_i32_s
            .f32 => @as(u8, 0xB2), // f32.convert_i32_s
            .f64 => @as(u8, 0xB7), // f64.convert_i32_s
            else => null,
        },
        .f64 => switch (target) {
            .f32 => @as(u8, 0xB6), // f32.demote_f64
            else => null,
        },
        .f32 => switch (target) {
            .f64 => @as(u8, 0xBB), // f64.promote_f32
            else => null,
        },
    };
    if (op) |opcode| {
        self.body.append(self.allocator, opcode) catch return error.OutOfMemory;
    }
}

/// Compile all LirProcs as separate wasm functions.
/// Must be called before generateExpr so that call sites can find compiled proc_specs.
pub fn compileAllProcSpecs(self: *Self, proc_specs: []const LirProcSpec) Allocator.Error!void {
    // Two-pass compilation to support mutual recursion.
    // Pass 1: Register ALL proc_specs (create function types, get func_idx).
    // This ensures that when compiling any proc body, all sibling proc_specs
    // are already known and can be called without triggering recursive compilation.
    for (proc_specs) |proc| {
        try self.registerProcSpec(proc);
    }
    // Pass 2: Compile proc bodies.
    for (proc_specs) |proc| {
        try self.compileProcSpecBody(proc);
    }
}

/// Compile a single LirProcSpec as a wasm function.
/// Does NOT compile the body — that's done by compileProcSpecBody.
fn registerProcSpec(self: *Self, proc: LirProcSpec) Allocator.Error!void {
    const key: u64 = @bitCast(proc.name);

    // Build parameter types: roc_ops_ptr first, then arg_layouts
    const arg_layouts = self.store.getLayoutIdxSpan(proc.arg_layouts);
    var param_types: std.ArrayList(ValType) = .empty;
    defer param_types.deinit(self.allocator);

    param_types.append(self.allocator, .i32) catch return error.OutOfMemory;
    for (arg_layouts) |arg_layout| {
        const vt = self.resolveValType(arg_layout);
        param_types.append(self.allocator, vt) catch return error.OutOfMemory;
    }

    const ret_vt = self.resolveValType(proc.ret_layout);
    const type_idx = self.module.addFuncType(param_types.items, &.{ret_vt}) catch return error.OutOfMemory;
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;

    self.registered_procs.put(key, func_idx) catch return error.OutOfMemory;
}

/// Compile a proc body. The proc must already be registered via registerProcSpec.
fn compileProcSpecBody(self: *Self, proc: LirProcSpec) Allocator.Error!void {
    const key: u64 = @bitCast(proc.name);

    // Get the pre-registered func_idx (must exist — registerProcSpec runs in pass 1)
    const func_idx = self.registered_procs.get(key) orelse unreachable;

    const arg_layouts = self.store.getLayoutIdxSpan(proc.arg_layouts);
    const ret_vt = self.resolveValType(proc.ret_layout);

    // Save current codegen state
    const saved = self.saveState() catch return error.OutOfMemory;

    // Initialize fresh state with ALL registered proc_specs (for mutual recursion)
    self.body = .empty;
    self.storage.locals = std.AutoHashMap(u64, Storage.LocalInfo).init(self.allocator);
    self.storage.next_local_idx = 0;
    self.storage.local_types = .empty;
    // Note: registered_procs is NOT cleared — all pre-registered proc_specs remain
    // visible. This is critical for mutual recursion: when compiling is_even's
    // body, calls to is_odd must find its func_idx without re-compilation.
    self.stack_frame_size = 0;
    self.uses_stack_memory = false;
    self.fp_local = 0;
    self.cf_depth = 0;
    self.in_proc = true;

    // Local 0 = roc_ops_ptr parameter
    self.roc_ops_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // Bind parameters to locals (starting at local 1)
    const params = self.store.getPatternSpan(proc.args);
    for (params, 0..) |param_id, i| {
        const pat = self.store.getPattern(param_id);
        switch (pat) {
            .bind => |bind| {
                const vt = if (i < arg_layouts.len) self.resolveValType(arg_layouts[i]) else .i32;
                _ = self.storage.allocLocal(bind.symbol, vt) catch return error.OutOfMemory;
            },
            .wildcard => {
                const vt = if (i < arg_layouts.len) self.resolveValType(arg_layouts[i]) else .i32;
                _ = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
            },
            .struct_ => |s| {
                const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.bindStructPattern(ptr, s);
            },
            else => unreachable,
        }
    }

    // Pre-allocate frame pointer local (after params, so it doesn't conflict)
    self.fp_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // Emit proc body block (ret targets this block)
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(ret_vt)) catch return error.OutOfMemory;
    self.cf_depth = 1; // inside the ret block

    // Generate CFStmt body
    self.generateCFStmt(proc.body) catch |err| {
        self.restoreState(saved);
        return err;
    };

    // End of ret block
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Build function body
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    // Pre-allocate result_tmp BEFORE encoding locals (so it's included in the declaration)
    const result_tmp = if (self.uses_stack_memory)
        self.storage.allocAnonymousLocal(ret_vt) catch return error.OutOfMemory
    else
        0;

    // Locals declaration (beyond parameters: 1 roc_ops_ptr + params)
    try self.encodeLocalsDecl(&func_body, @intCast(1 + params.len));

    // Prologue (if stack memory used)
    if (self.uses_stack_memory) {
        // global.get $__stack_pointer
        func_body.append(self.allocator, Op.global_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
        // i32.const frame_size
        func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &func_body, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        // i32.sub
        func_body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
        // local.tee $fp
        func_body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, self.fp_local) catch return error.OutOfMemory;
        // global.set $__stack_pointer
        func_body.append(self.allocator, Op.global_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
    }

    // Body instructions
    func_body.appendSlice(self.allocator, self.body.items) catch return error.OutOfMemory;

    if (self.uses_stack_memory) {
        // Epilogue: restore stack pointer
        func_body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, result_tmp) catch return error.OutOfMemory;
        // local.get $fp
        func_body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, self.fp_local) catch return error.OutOfMemory;
        // i32.const frame_size
        func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &func_body, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        // i32.add
        func_body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        // global.set $__stack_pointer
        func_body.append(self.allocator, Op.global_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
        // Push result back
        func_body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, result_tmp) catch return error.OutOfMemory;
    }

    // End opcode
    func_body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.module.setFunctionBody(func_idx, func_body.items) catch return error.OutOfMemory;

    // Propagate stack memory usage to outer scope
    const proc_used_stack_memory = self.uses_stack_memory;

    // Restore state
    self.restoreState(saved);

    // If the proc used stack memory, the outer scope needs to know
    if (proc_used_stack_memory) self.uses_stack_memory = true;
}

/// Saved codegen state for restoring after compiling a nested function.
const SavedState = struct {
    body_items: []u8,
    body_capacity: usize,
    locals: std.AutoHashMap(u64, Storage.LocalInfo),
    next_local_idx: u32,
    local_types_items: []ValType,
    local_types_capacity: usize,
    // Note: registered_procs is NOT saved/restored — once a proc is registered,
    // its entry persists globally since the wasm function code is already emitted.
    stack_frame_size: u32,
    uses_stack_memory: bool,
    fp_local: u32,
    roc_ops_local: u32,
    cf_depth: u32,
    in_proc: bool,
};

/// Capture current codegen state for later restoration.
fn saveState(self: *Self) Allocator.Error!SavedState {
    return .{
        .body_items = self.body.items,
        .body_capacity = self.body.capacity,
        .locals = self.storage.locals,
        .next_local_idx = self.storage.next_local_idx,
        .local_types_items = self.storage.local_types.items,
        .local_types_capacity = self.storage.local_types.capacity,
        .stack_frame_size = self.stack_frame_size,
        .uses_stack_memory = self.uses_stack_memory,
        .fp_local = self.fp_local,
        .roc_ops_local = self.roc_ops_local,
        .cf_depth = self.cf_depth,
        .in_proc = self.in_proc,
    };
}

/// Restore codegen state after compiling a nested function.
fn restoreState(self: *Self, saved: SavedState) void {
    self.body.deinit(self.allocator);
    self.body.items = saved.body_items;
    self.body.capacity = saved.body_capacity;
    self.storage.locals.deinit();
    self.storage.locals = saved.locals;
    self.storage.next_local_idx = saved.next_local_idx;
    self.storage.local_types.deinit(self.allocator);
    self.storage.local_types.items = saved.local_types_items;
    self.storage.local_types.capacity = saved.local_types_capacity;
    // Note: registered_procs is NOT restored — entries added during nested
    // compilation must persist since the wasm function code is already emitted.
    self.stack_frame_size = saved.stack_frame_size;
    self.uses_stack_memory = saved.uses_stack_memory;
    self.fp_local = saved.fp_local;
    self.roc_ops_local = saved.roc_ops_local;
    self.cf_depth = saved.cf_depth;
    self.in_proc = saved.in_proc;
}

/// Generate code for a control flow statement (used in LirProcSpec bodies).
fn generateCFStmt(self: *Self, stmt_id: CFStmtId) Allocator.Error!void {
    if (stmt_id.isNone()) return;
    const stmt = self.store.getCFStmt(stmt_id);

    switch (stmt) {
        .let_stmt => |let_s| {
            // Generate value expression
            try self.generateExpr(let_s.value);
            // Bind to pattern
            const pat = self.store.getPattern(let_s.pattern);
            try self.bindCFLetPattern(pat, let_s.value);
            // Continue with next statement
            try self.generateCFStmt(let_s.next);
        },
        .ret => |r| {
            // Generate return value
            try self.generateExpr(r.value);
            // Break out to the proc ret block
            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, self.cf_depth - 1) catch return error.OutOfMemory;
        },
        .expr_stmt => |es| {
            // Generate value for side effects, then drop
            try self.generateExpr(es.value);
            self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
            try self.generateCFStmt(es.next);
        },
        .switch_stmt => |sw| {
            // Generate condition value, save to local
            try self.generateExpr(sw.cond);
            const cond_vt = self.resolveValType(sw.cond_layout);
            const cond_local = self.storage.allocAnonymousLocal(cond_vt) catch return error.OutOfMemory;
            try self.emitLocalSet(cond_local);

            const branches = self.store.getCFSwitchBranches(sw.branches);
            const ret_vt = self.resolveValType(sw.ret_layout);

            // Cascading if/else for each branch
            for (branches) |branch| {
                // Compare cond to branch value
                try self.emitLocalGet(cond_local);
                if (cond_vt == .i64) {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, @bitCast(branch.value)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;
                } else {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(@as(i64, @bitCast(branch.value)))) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
                }

                // if (result_type)
                self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.body.append(self.allocator, @intFromEnum(ret_vt)) catch return error.OutOfMemory;
                self.cf_depth += 1;

                try self.generateCFStmt(branch.body);

                self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            }

            // Default branch
            try self.generateCFStmt(sw.default_branch);

            // Close all if/else blocks
            for (0..branches.len) |_| {
                self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.cf_depth -= 1;
            }
        },
        .join => |j| {
            const jp_key = @intFromEnum(j.id);

            // Store join point parameter info for later rebinding
            const jp_params = self.store.getPatternSpan(j.params);
            const jp_layouts = self.store.getLayoutIdxSpan(j.param_layouts);
            var param_locals = self.allocator.alloc(u32, jp_params.len) catch return error.OutOfMemory;

            // Allocate locals for join point parameters
            for (jp_params, 0..) |param_id, i| {
                const pat = self.store.getPattern(param_id);
                switch (pat) {
                    .bind => |bind| {
                        const vt = if (i < jp_layouts.len) self.resolveValType(jp_layouts[i]) else .i32;
                        const local_idx = self.storage.allocLocal(bind.symbol, vt) catch return error.OutOfMemory;
                        param_locals[i] = local_idx;
                    },
                    .wildcard => {
                        if (std.debug.runtime_safety) {
                            std.debug.panic(
                                "WASM/codegen invariant violated: wildcard join params are not allowed in canonical tail-recursive form",
                                .{},
                            );
                        }
                        unreachable;
                    },
                    .struct_ => |s| {
                        const local_idx = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        param_locals[i] = local_idx;
                        try self.bindStructPattern(local_idx, s);
                    },
                    else => unreachable,
                }
            }
            self.join_point_param_locals.put(jp_key, param_locals) catch return error.OutOfMemory;

            // Generate remainder (includes initial jump that sets params)
            try self.generateCFStmt(j.remainder);

            // Emit loop for the join point body
            self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
            self.body.append(self.allocator, 0x40) catch return error.OutOfMemory; // void block type
            self.cf_depth += 1;

            // Record the loop depth for jump targeting
            // br 0 inside this loop will re-enter the loop
            self.join_point_depths.put(jp_key, self.cf_depth) catch return error.OutOfMemory;

            try self.generateCFStmt(j.body);

            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;
        },
        .jump => |jmp| {
            const jp_key = @intFromEnum(jmp.target);
            const args = self.store.getExprSpan(jmp.args);

            // Get param locals for this join point
            const param_locals = self.join_point_param_locals.get(jp_key) orelse unreachable;
            if (args.len != param_locals.len) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "WASM/codegen invariant violated: jump arg arity ({d}) does not match join param arity ({d})",
                        .{ args.len, param_locals.len },
                    );
                }
                unreachable;
            }

            // Evaluate all arguments first (to temp locals), to avoid
            // overwriting params that are referenced by later args
            var temp_locals = self.allocator.alloc(u32, args.len) catch return error.OutOfMemory;
            defer self.allocator.free(temp_locals);

            for (args, 0..) |arg_id, i| {
                try self.generateExpr(arg_id);
                const vt = self.exprValType(arg_id);
                const tmp = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
                try self.emitLocalSet(tmp);
                temp_locals[i] = tmp;
            }

            // Copy temp locals to param locals
            for (0..args.len) |i| {
                try self.emitLocalGet(temp_locals[i]);
                try self.emitLocalSet(param_locals[i]);
            }

            // If the loop has been entered, branch back to it
            if (self.join_point_depths.get(jp_key)) |loop_depth| {
                // br to the loop (br 0 from directly inside the loop)
                const br_target = self.cf_depth - loop_depth;
                self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, br_target) catch return error.OutOfMemory;
            }
            // If loop not entered yet (initial jump in remainder), just fall through
        },
        .match_stmt => |ms| {
            // Evaluate value, store to local
            try self.generateExpr(ms.value);
            const value_vt = self.resolveValType(ms.value_layout);
            const value_local = self.storage.allocAnonymousLocal(value_vt) catch return error.OutOfMemory;
            try self.emitLocalSet(value_local);

            const branches = self.store.getCFMatchBranches(ms.branches);

            // Generate cascading pattern match using if/else blocks
            // Each branch body is a CFStmt that handles its own return/jump
            try self.generateCFMatchBranches(branches, value_local, value_vt);
        },
    }
}

/// Generate cascading match branches for match_stmt.
/// Each branch body is a CF statement (handles its own ret/jump).
fn generateCFMatchBranches(self: *Self, branches: []const LIR.CFMatchBranch, value_local: u32, value_vt: ValType) Allocator.Error!void {
    if (branches.len == 0) {
        self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        return;
    }

    const branch = branches[0];
    const pattern = self.store.getPattern(branch.pattern);
    const remaining = branches[1..];

    switch (pattern) {
        .wildcard => {
            try self.generateCFStmtWithGuard(branch, remaining, value_local, value_vt);
        },
        .bind => |bind| {
            const local_idx = self.storage.allocLocal(bind.symbol, value_vt) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
            try self.generateCFStmtWithGuard(branch, remaining, value_local, value_vt);
        },
        .int_literal => |int_pat| {
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;

            switch (value_vt) {
                .i32 => {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @truncate(@as(i64, @truncate(int_pat.value)))) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, @truncate(int_pat.value)) catch return error.OutOfMemory;
                },
                .f32, .f64 => unreachable,
            }

            const eq_op: u8 = switch (value_vt) {
                .i32 => Op.i32_eq,
                .i64 => Op.i64_eq,
                .f32, .f64 => unreachable,
            };
            self.body.append(self.allocator, eq_op) catch return error.OutOfMemory;

            // if match: void block type since branch bodies handle their own control flow
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, 0x40) catch return error.OutOfMemory; // void
            self.cf_depth += 1;
            try self.generateCFStmtWithGuard(branch, remaining, value_local, value_vt);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateCFMatchBranches(remaining, value_local, value_vt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;
        },
        .tag => |tag_pat| {
            // For composite tag unions (value_local is a pointer to memory),
            // load the discriminant from memory before comparing.
            // For scalar tag unions, value_local holds the discriminant directly.
            const ls = self.getLayoutStore();
            const is_pointer = switch (WasmLayout.wasmReprWithStore(tag_pat.union_layout, ls)) {
                .stack_memory => true,
                .primitive => false,
            };

            if (is_pointer) {
                // Load discriminant from memory at discriminant_offset
                const l = ls.getLayout(tag_pat.union_layout);
                std.debug.assert(l.tag == .tag_union);
                const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
                const disc_offset = tu_data.discriminant_offset;
                const disc_size: u32 = tu_data.discriminant_size;
                if (disc_size == 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                } else {
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                    try self.emitLoadOpSized(.i32, disc_size, disc_offset);
                }
            } else {
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
            }

            // Compare against the discriminant constant
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(tag_pat.discriminant)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, 0x40) catch return error.OutOfMemory; // void
            self.cf_depth += 1;
            try self.generateCFStmtWithGuard(branch, remaining, value_local, value_vt);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateCFMatchBranches(remaining, value_local, value_vt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;
        },
        .struct_, .list, .as_pattern => {
            // These pattern types should not appear in CFStmt match_stmt.
            // CFStmt match_stmt is used for tail-recursive matches, which only
            // match on discriminants, integer literals, wildcards, and binds.
            unreachable;
        },
        .float_literal, .str_literal => {
            unreachable;
        },
    }
}

/// Generate a CF statement body, handling optional guard expressions.
/// If the branch has a guard, wraps the body in an if/else so that
/// a failing guard falls through to the remaining branches.
fn generateCFStmtWithGuard(
    self: *Self,
    branch: LIR.CFMatchBranch,
    remaining: []const LIR.CFMatchBranch,
    value_local: u32,
    value_vt: ValType,
) Allocator.Error!void {
    if (!branch.guard.isNone()) {
        // Evaluate the guard expression (pushes i32 0 or 1)
        try self.generateExpr(branch.guard);
        self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.body.append(self.allocator, 0x40) catch return error.OutOfMemory; // void
        self.cf_depth += 1;
        try self.generateCFStmt(branch.body);
        self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
        try self.generateCFMatchBranches(remaining, value_local, value_vt);
        self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        self.cf_depth -= 1;
    } else {
        try self.generateCFStmt(branch.body);
    }
}

/// Bind a CFStmt let-pattern to the value just generated.
/// The value is on the wasm stack. We need to store it to a local.
fn bindCFLetPattern(self: *Self, pat: LirPattern, value_expr: LirExprId) Allocator.Error!void {
    switch (pat) {
        .bind => |bind| {
            const expr_is_composite = self.isCompositeExpr(value_expr);
            const target_is_composite = self.isCompositeLayout(bind.layout_idx);

            if (expr_is_composite and !target_is_composite) {
                // Composite → scalar: load scalar from pointer
                const vt = self.resolveValType(bind.layout_idx);
                const byte_size = self.layoutByteSize(bind.layout_idx);
                try self.emitLoadOpSized(vt, byte_size, 0);
                const local_idx = self.getOrAllocTypedLocal(bind.symbol, vt) catch return error.OutOfMemory;
                try self.emitLocalSet(local_idx);
            } else if (!expr_is_composite and target_is_composite) {
                // Scalar → composite: store scalar into stack memory
                const scalar_vt = self.exprValType(value_expr);
                const tmp_local = self.storage.allocAnonymousLocal(scalar_vt) catch return error.OutOfMemory;
                try self.emitLocalSet(tmp_local);
                const byte_size = self.layoutByteSize(bind.layout_idx);
                const alignment: u32 = if (byte_size >= 8) 8 else if (byte_size >= 4) 4 else if (byte_size >= 2) 2 else 1;
                const stack_offset = try self.allocStackMemory(byte_size, alignment);
                try self.emitLocalGet(self.fp_local);
                try self.emitLocalGet(tmp_local);
                try self.emitStoreOp(scalar_vt, stack_offset);
                try self.emitLocalGet(self.fp_local);
                if (stack_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(stack_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                const local_idx = self.getOrAllocTypedLocal(bind.symbol, .i32) catch return error.OutOfMemory;
                try self.emitLocalSet(local_idx);
            } else {
                // Composite values returned from calls point into the callee's stack
                // frame. Copy them into the caller's frame before binding.
                if (target_is_composite and self.exprNeedsCompositeCallStabilization(value_expr)) {
                    const ret_size = self.layoutByteSize(bind.layout_idx);
                    if (ret_size > 0) {
                        const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        try self.emitLocalSet(src_local);
                        const dst_offset = try self.allocStackMemory(ret_size, 4);
                        const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        try self.emitFpOffset(dst_offset);
                        try self.emitLocalSet(dst_local);
                        try self.emitMemCopy(dst_local, 0, src_local, ret_size);
                        try self.emitLocalGet(dst_local);
                    }
                }

                const vt = self.resolveValType(bind.layout_idx);
                const expr_vt = self.exprValType(value_expr);
                try self.emitConversion(expr_vt, vt);
                const local_idx = self.getOrAllocTypedLocal(bind.symbol, vt) catch return error.OutOfMemory;
                try self.emitLocalSet(local_idx);
            }
        },
        .wildcard => {
            self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
        },
        .struct_ => |s| {
            const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(ptr);
            try self.bindStructPattern(ptr, s);
        },
        .tag => |tag_pat| {
            const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(ptr);
            try self.bindTagPattern(ptr, tag_pat);
        },
        .as_pattern => |as_pat| {
            // Bind the outer symbol, then recurse on the inner pattern
            const vt = self.resolveValType(as_pat.layout_idx);
            const local_idx = self.storage.allocLocal(as_pat.symbol, vt) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
            const inner_pat = self.store.getPattern(as_pat.inner);
            try self.bindCFLetPattern(inner_pat, value_expr);
        },
        .list => |list_pat| {
            const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(ptr);
            try self.bindListPattern(ptr, list_pat);
        },
        else => unreachable,
    }
}

/// Generate code for a function call.
/// In the new pipeline, MIR→LIR generates all closure dispatch as generic LIR
/// constructs (discriminant_switch, tag_payload_access, direct calls). The backend
/// just handles explicit direct-call symbols plus the residual runtime
/// function-value expression path. No closure-specific dispatch.
fn generateCall(self: *Self, c: anytype) Allocator.Error!void {
    const proc = self.store.getProcSpec(c.proc);
    const proc_key: u64 = @bitCast(proc.name);
    const func_idx = self.registered_procs.get(proc_key) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic("generateCall: unresolved proc call target {d}", .{@intFromEnum(c.proc)});
        }
        unreachable;
    };

    try self.emitLocalGet(self.roc_ops_local);
    try self.generateCallArgs(c.args);
    try self.emitCall(func_idx);
}

/// Emit a call instruction.
fn emitCall(self: *Self, func_idx: u32) Allocator.Error!void {
    try self.body.append(self.allocator, Op.call);
    try WasmModule.leb128WriteU32(self.allocator, &self.body, func_idx);
}

/// Generate call arguments (helper to avoid duplication).
fn generateCallArgs(self: *Self, args: LIR.LirExprSpan) Allocator.Error!void {
    const arg_exprs = self.store.getExprSpan(args);
    for (arg_exprs, 0..) |arg_id, i| {
        try self.generateExpr(arg_id);
        // When there are multiple args and this one returns a composite value
        // (pointer to stack memory), stabilize it by copying into the caller's
        // stack frame. Otherwise a subsequent arg's call can deallocate the
        // callee's stack frame and reuse the memory, clobbering this result.
        if (arg_exprs.len > 1 and i < arg_exprs.len - 1 and self.isCompositeExpr(arg_id)) {
            const layout_idx = self.exprLayoutIdx(arg_id);
            const repr = WasmLayout.wasmReprWithStore(layout_idx, self.getLayoutStore());
            switch (repr) {
                .stack_memory => |size| if (size > 0) {
                    const stabilized = try self.stabilizeCompositeResult(size);
                    try self.emitLocalGet(stabilized);
                },
                .primitive => {},
            }
        }
    }
}

// ---- Lambda set / Closure value generation ----
// These functions handle runtime dispatch for lambda sets with multiple members.
// Used when closures have enum_dispatch or union_repr representations.

// ---- Composite type generation (records, tuples, tags) ----

/// Get the layout store (required for wasm codegen).
fn getLayoutStore(self: *const Self) *const LayoutStore {
    return self.layout_store;
}

/// Get the byte size of a layout index using the layout store.
fn layoutByteSize(self: *const Self, layout_idx: layout.Idx) u32 {
    const ls = self.getLayoutStore();
    return switch (WasmLayout.wasmReprWithStore(layout_idx, ls)) {
        .primitive => |vt| switch (vt) {
            .i32, .f32 => 4,
            .i64, .f64 => 8,
        },
        .stack_memory => |size| size,
    };
}

fn layoutStorageByteSize(self: *const Self, layout_idx: layout.Idx) u32 {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);
    return switch (l.tag) {
        .zst => 0,
        .scalar => switch (l.data.scalar.tag) {
            .str => 12,
            .int => switch (l.data.scalar.data.int) {
                .u8, .i8 => 1,
                .u16, .i16 => 2,
                .u32, .i32 => 4,
                .u64, .i64 => 8,
                .u128, .i128 => 16,
            },
            .frac => switch (l.data.scalar.data.frac) {
                .f32 => 4,
                .f64 => 8,
                .dec => 16,
            },
        },
        .list, .list_of_zst => 12,
        .box, .box_of_zst => 4,
        else => self.layoutByteSize(layout_idx),
    };
}

fn layoutByteAlign(self: *const Self, layout_idx: layout.Idx) u32 {
    const ls = self.getLayoutStore();
    return switch (WasmLayout.wasmReprWithStore(layout_idx, ls)) {
        .primitive => |vt| switch (vt) {
            .i32, .f32 => 4,
            .i64, .f64 => 8,
        },
        .stack_memory => {
            const l = ls.getLayout(layout_idx);
            return switch (l.tag) {
                .list, .list_of_zst, .box, .box_of_zst => 4,
                .scalar => if (l.data.scalar.tag == .str) 4 else @intCast(ls.layoutSizeAlign(l).alignment.toByteUnits()),
                else => @intCast(ls.layoutSizeAlign(l).alignment.toByteUnits()),
            };
        },
    };
}

fn layoutStorageByteAlign(self: *const Self, layout_idx: layout.Idx) u32 {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);
    return switch (l.tag) {
        .zst => 1,
        .scalar => switch (l.data.scalar.tag) {
            .str => 4,
            .int => switch (l.data.scalar.data.int) {
                .u8, .i8 => 1,
                .u16, .i16 => 2,
                .u32, .i32 => 4,
                .u64, .i64 => 8,
                .u128, .i128 => 16,
            },
            .frac => switch (l.data.scalar.data.frac) {
                .f32 => 4,
                .f64 => 8,
                .dec => 16,
            },
        },
        .list, .list_of_zst, .box, .box_of_zst => 4,
        else => self.layoutByteAlign(layout_idx),
    };
}

/// Emit a store instruction for the given value type at an address already on the stack.
/// The memory operand format is: alignment (log2) + offset.
fn emitStoreOp(self: *Self, vt: ValType, mem_offset: u32) Allocator.Error!void {
    const op: u8 = switch (vt) {
        .i32 => Op.i32_store,
        .i64 => Op.i64_store,
        .f32 => Op.f32_store,
        .f64 => Op.f64_store,
    };
    self.body.append(self.allocator, op) catch return error.OutOfMemory;
    // Alignment (log2): i32=2, i64=3, f32=2, f64=3
    const align_log2: u32 = switch (vt) {
        .i32, .f32 => 2,
        .i64, .f64 => 3,
    };
    WasmModule.leb128WriteU32(self.allocator, &self.body, align_log2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, mem_offset) catch return error.OutOfMemory;
}

/// Emit a size-aware store instruction for a field with a known byte size.
/// For sub-32-bit fields (1 or 2 bytes), uses i32.store8/i32.store16.
fn emitStoreOpSized(self: *Self, vt: ValType, byte_size: u32, mem_offset: u32) Allocator.Error!void {
    if (vt == .i32 and byte_size < 4) {
        // Sub-word store for i32 values in small fields
        const op: u8 = if (byte_size == 1) Op.i32_store8 else Op.i32_store16;
        self.body.append(self.allocator, op) catch return error.OutOfMemory;
        const align_log2: u32 = if (byte_size == 1) 0 else 1;
        WasmModule.leb128WriteU32(self.allocator, &self.body, align_log2) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, mem_offset) catch return error.OutOfMemory;
    } else {
        try self.emitStoreOp(vt, mem_offset);
    }
}

/// Emit a load instruction for the given value type at an address already on the stack.
fn emitLoadOp(self: *Self, vt: ValType, mem_offset: u32) Allocator.Error!void {
    const op: u8 = switch (vt) {
        .i32 => Op.i32_load,
        .i64 => Op.i64_load,
        .f32 => Op.f32_load,
        .f64 => Op.f64_load,
    };
    self.body.append(self.allocator, op) catch return error.OutOfMemory;
    const align_log2: u32 = switch (vt) {
        .i32, .f32 => 2,
        .i64, .f64 => 3,
    };
    WasmModule.leb128WriteU32(self.allocator, &self.body, align_log2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, mem_offset) catch return error.OutOfMemory;
}

/// Emit a size-aware load instruction for a field with a known byte size.
/// For sub-32-bit fields (1 or 2 bytes), uses i32.load8_u/i32.load16_u.
fn emitLoadOpSized(self: *Self, vt: ValType, byte_size: u32, mem_offset: u32) Allocator.Error!void {
    if (byte_size == 0) {
        self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
        switch (vt) {
            .i32 => {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            },
            .i64 => {
                self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            },
            .f32 => {
                self.body.append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                try self.body.appendSlice(self.allocator, std.mem.asBytes(&@as(f32, 0)));
            },
            .f64 => {
                self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                try self.body.appendSlice(self.allocator, std.mem.asBytes(&@as(f64, 0)));
            },
        }
        return;
    }

    if (vt == .i32 and byte_size < 4) {
        // Sub-word load for i32 values in small fields
        const op: u8 = if (byte_size == 1) Op.i32_load8_u else Op.i32_load16_u;
        self.body.append(self.allocator, op) catch return error.OutOfMemory;
        const align_log2: u32 = if (byte_size == 1) 0 else 1;
        WasmModule.leb128WriteU32(self.allocator, &self.body, align_log2) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, mem_offset) catch return error.OutOfMemory;
    } else {
        try self.emitLoadOp(vt, mem_offset);
    }
}

/// Like emitLoadOpSized but uses sign-extending loads for signed sub-word types.
fn emitLoadOpForLayout(self: *Self, lay: layout.Idx, mem_offset: u32) Allocator.Error!void {
    const vt = self.resolveValType(lay);
    const byte_size = self.layoutStorageByteSize(lay);
    if (vt == .i32 and byte_size < 4) {
        const is_signed = (lay == .i8 or lay == .i16);
        const op: u8 = if (byte_size == 1)
            (if (is_signed) Op.i32_load8_s else Op.i32_load8_u)
        else
            (if (is_signed) Op.i32_load16_s else Op.i32_load16_u);
        self.body.append(self.allocator, op) catch return error.OutOfMemory;
        const align_log2: u32 = if (byte_size == 1) 0 else 1;
        WasmModule.leb128WriteU32(self.allocator, &self.body, align_log2) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, mem_offset) catch return error.OutOfMemory;
    } else {
        try self.emitLoadOp(vt, mem_offset);
    }
}

/// Store a value from the wasm stack into memory at [base_local + field_offset].
/// The value to store must already be on the wasm value stack.
/// Stack effect: pops 1 value.
fn emitStoreToMem(self: *Self, base_local: u32, field_offset: u32, vt: ValType) Allocator.Error!void {
    // We need the address under the value on the stack.
    // Strategy: store value in a temp local, push address, get value back, then store.
    const temp = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
    // local.set temp (pop value)
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
    // local.get base
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    // local.get temp (push value)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
    // store with field_offset as immediate
    try self.emitStoreOp(vt, field_offset);
}

/// Store a value into memory with size-aware opcodes for sub-32-bit fields.
fn emitStoreToMemSized(self: *Self, base_local: u32, field_offset: u32, vt: ValType, byte_size: u32) Allocator.Error!void {
    if (byte_size == 0) {
        const temp = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
        return;
    }

    const temp = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
    try self.emitStoreOpSized(vt, byte_size, field_offset);
}

/// Copy `byte_count` bytes from src_local pointer to (dst_local + dst_offset).
/// Uses i32.load/i32.store in chunks, with i32.load8_u/i32.store8 for remainder.
fn emitMemCopy(self: *Self, dst_local: u32, dst_offset: u32, src_local: u32, byte_count: u32) Allocator.Error!void {
    var offset: u32 = 0;

    // Copy i32-sized chunks
    while (offset + 4 <= byte_count) : (offset += 4) {
        // dst_local
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, dst_local) catch return error.OutOfMemory;
        // load from src
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
        // store to dst
        self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, &self.body, dst_offset + offset) catch return error.OutOfMemory;
    }

    // Copy i16 chunk
    if (offset + 2 <= byte_count) {
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, dst_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_store16) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, dst_offset + offset) catch return error.OutOfMemory;
        offset += 2;
    }

    // Copy remaining byte
    if (offset < byte_count) {
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, dst_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, dst_offset + offset) catch return error.OutOfMemory;
    }
}

/// Zero-initialize a region of memory.
/// Emits i32.store 0 for each 4-byte chunk, plus i32.store8 0 for remaining bytes.
fn emitZeroInit(self: *Self, base_local: u32, byte_count: u32) Allocator.Error!void {
    var offset: u32 = 0;
    while (offset + 4 <= byte_count) : (offset += 4) {
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
    }
    while (offset < byte_count) : (offset += 1) {
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
    }
}

/// Generate a struct construction expression (unified record/tuple/empty_record).
/// Allocates stack memory, stores each field in layout order, returns pointer.
fn generateStruct(self: *Self, r: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(r.struct_layout);
    // Empty structs (ZST) have scalar layout, not struct_ — push dummy pointer
    if (l.tag != .struct_) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        return;
    }

    const size = ls.layoutSize(l);
    if (size == 0) {
        // Zero-sized struct — push dummy pointer
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        return;
    }

    const align_val: u32 = @intCast(l.data.struct_.alignment.toByteUnits());

    const frame_offset = try self.allocStackMemory(size, align_val);

    // Allocate a local to hold the base pointer
    const base_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(frame_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;

    // Generate all field expressions FIRST and save values to locals.
    // This must happen before zero-init because field expressions may read from
    // memory that aliases the output record (e.g., in loops where $acc is rebound
    // to the same stack offset each iteration).
    const fields = self.store.getExprSpan(r.fields);

    const field_val_locals = self.allocator.alloc(u32, fields.len) catch return error.OutOfMemory;
    defer self.allocator.free(field_val_locals);
    const field_val_types = self.allocator.alloc(ValType, fields.len) catch return error.OutOfMemory;
    defer self.allocator.free(field_val_types);

    for (fields, 0..) |field_expr_id, i| {
        const field_byte_size = ls.getStructFieldSize(l.data.struct_.idx, @intCast(i));
        const field_layout_idx = ls.getStructFieldLayout(l.data.struct_.idx, @intCast(i));
        const is_composite = self.isCompositeLayout(field_layout_idx);
        const field_vt = WasmLayout.resultValTypeWithStore(field_layout_idx, ls);

        // Generate the field expression
        try self.generateExpr(field_expr_id);

        // Composite field expressions can return pointers into callee-owned stack
        // frames. Stabilize by copying bytes into this frame before saving pointer.
        if (is_composite and field_byte_size > 0) {
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(src_local);

            const stable_align: u32 = if (field_byte_size >= 8) 8 else if (field_byte_size >= 4) 4 else if (field_byte_size >= 2) 2 else 1;
            const stable_offset = try self.allocStackMemory(field_byte_size, stable_align);
            const stable_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(stable_offset);
            try self.emitLocalSet(stable_local);
            try self.emitMemCopy(stable_local, 0, src_local, field_byte_size);
            try self.emitLocalGet(stable_local);
        }

        // Convert type if needed (for primitives)
        if (!is_composite) {
            const expr_vt = self.exprValType(field_expr_id);
            try self.emitConversion(expr_vt, field_vt);
        }

        // Save value to a local (i32 pointer for composite, value type for primitive)
        const save_vt: ValType = if (is_composite) .i32 else field_vt;
        const local_idx = self.storage.allocAnonymousLocal(save_vt) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;

        field_val_locals[i] = local_idx;
        field_val_types[i] = save_vt;
    }

    // Zero-initialize record memory for consistent padding in structural equality
    try self.emitZeroInit(base_local, size);

    // Store each field from pre-computed locals
    for (fields, 0..) |_, i| {
        const field_offset = ls.getStructFieldOffset(l.data.struct_.idx, @intCast(i));
        const field_layout_idx = ls.getStructFieldLayout(l.data.struct_.idx, @intCast(i));
        const field_byte_size = ls.getStructFieldSize(l.data.struct_.idx, @intCast(i));
        const is_composite = self.isCompositeLayout(field_layout_idx);

        if (is_composite and field_byte_size > 0) {
            // The local holds an i32 pointer to source data. Copy to record slot.
            try self.emitMemCopy(base_local, field_offset, field_val_locals[i], field_byte_size);
        } else {
            // Primitive — push value from local, then store to record
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, field_val_locals[i]) catch return error.OutOfMemory;
            try self.emitStoreToMemSized(base_local, field_offset, field_val_types[i], field_byte_size);
        }
    }

    // Push the base pointer as the result
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
}

/// Bind a struct destructuring pattern: load each field from a struct pointer
/// and bind sub-patterns (recursively for nested destructuring).
/// Fields are in layout order (sorted by alignment).
fn bindStructPattern(self: *Self, ptr_local: u32, s: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const struct_layout = ls.getLayout(s.struct_layout);
    std.debug.assert(struct_layout.tag == .struct_);

    const field_patterns = self.store.getPatternSpan(s.fields);
    for (field_patterns, 0..) |pat_id, i| {
        const pat = self.store.getPattern(pat_id);
        const field_idx: u16 = @intCast(i);
        const field_offset = ls.getStructFieldOffset(struct_layout.data.struct_.idx, field_idx);
        const field_byte_size = ls.getStructFieldSize(struct_layout.data.struct_.idx, field_idx);
        const field_layout_idx = ls.getStructFieldLayout(struct_layout.data.struct_.idx, field_idx);

        switch (pat) {
            .bind => |bind| {
                const is_composite = self.isCompositeLayout(field_layout_idx);
                if (is_composite and field_byte_size > 0) {
                    // Composite field: compute pointer = ptr + offset
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                    if (field_offset > 0) {
                        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                    }
                    const local_idx = self.storage.allocLocal(bind.symbol, .i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                } else {
                    // Scalar field: load from memory
                    const field_vt = WasmLayout.resultValTypeWithStore(field_layout_idx, ls);
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                    try self.emitLoadOpSized(field_vt, field_byte_size, field_offset);
                    const local_idx = self.storage.allocLocal(bind.symbol, field_vt) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                }
            },
            .wildcard => {},
            .struct_ => |inner_struct| {
                // Nested struct destructuring: compute pointer to field
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, field_ptr) catch return error.OutOfMemory;
                try self.bindStructPattern(field_ptr, inner_struct);
            },
            .tag => |inner_tag| {
                if (builtin.mode == .Debug and !self.tagPatternIsIrrefutable(inner_tag)) {
                    std.debug.panic(
                        "WasmCodeGen invariant violated: nested struct field tag patterns must be irrefutable single-tag unions",
                        .{},
                    );
                }
                if (self.store.getPatternSpan(inner_tag.args).len == 0) continue;

                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, field_ptr) catch return error.OutOfMemory;
                try self.bindTagPattern(field_ptr, inner_tag);
            },
            else => unreachable,
        }
    }
}

fn tagPatternIsIrrefutable(self: *Self, tag: anytype) bool {
    const ls = self.getLayoutStore();
    const union_layout = ls.getLayout(tag.union_layout);
    return switch (union_layout.tag) {
        .tag_union => blk: {
            const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
            break :blk ls.getTagUnionVariants(tu_data).len == 1;
        },
        .box => blk: {
            const inner_layout = ls.getLayout(union_layout.data.box);
            if (inner_layout.tag != .tag_union) break :blk false;
            const tu_data = ls.getTagUnionData(inner_layout.data.tag_union.idx);
            break :blk ls.getTagUnionVariants(tu_data).len == 1;
        },
        .scalar, .zst => true,
        else => false,
    };
}

/// Bind a tag union destructuring pattern: extract payload fields from a tag pointer.
/// `ptr_local` is an i32 local holding a pointer to the tag union in memory.
fn bindTagPattern(self: *Self, ptr_local: u32, tag: anytype) Allocator.Error!void {
    const arg_patterns = self.store.getPatternSpan(tag.args);
    if (arg_patterns.len == 0) return;

    const ls = self.getLayoutStore();
    const l = ls.getLayout(tag.union_layout);

    if (l.tag != .tag_union) {
        // Simple enum (discriminant only, no payload) — nothing to extract
        return;
    }

    // Extract payload fields at increasing offsets from the tag pointer
    var payload_offset: u32 = 0;
    for (arg_patterns) |arg_pat_id| {
        const arg_pat = self.store.getPattern(arg_pat_id);
        switch (arg_pat) {
            .bind => |bind| {
                const bind_vt = self.resolveValType(bind.layout_idx);
                const bind_byte_size = self.layoutStorageByteSize(bind.layout_idx);
                const is_composite = self.isCompositeLayout(bind.layout_idx);
                if (is_composite and bind_byte_size > 0) {
                    // Composite field: compute pointer = ptr + offset
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                    if (payload_offset > 0) {
                        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(payload_offset)) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                    }
                    const local_idx = self.storage.allocLocal(bind.symbol, .i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                } else {
                    // Scalar field: load from memory
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                    try self.emitLoadOpForLayout(bind.layout_idx, payload_offset);
                    const local_idx = self.storage.allocLocal(bind.symbol, bind_vt) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                }
                payload_offset += bind_byte_size;
            },
            .wildcard => |wc| {
                payload_offset += self.layoutStorageByteSize(wc.layout_idx);
            },
            .struct_ => |inner_struct| {
                const field_byte_size = self.layoutStorageByteSize(inner_struct.struct_layout);
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                if (payload_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(payload_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, field_ptr) catch return error.OutOfMemory;
                try self.bindStructPattern(field_ptr, inner_struct);
                payload_offset += field_byte_size;
            },
            .tag => |inner_tag| {
                if (builtin.mode == .Debug and !self.tagPatternIsIrrefutable(inner_tag)) {
                    std.debug.panic(
                        "WasmCodeGen invariant violated: nested tag payload bindings must be irrefutable single-tag unions",
                        .{},
                    );
                }
                const field_byte_size = self.layoutStorageByteSize(inner_tag.union_layout);
                if (self.store.getPatternSpan(inner_tag.args).len != 0) {
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                    if (payload_offset > 0) {
                        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(payload_offset)) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                    }
                    const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, field_ptr) catch return error.OutOfMemory;
                    try self.bindTagPattern(field_ptr, inner_tag);
                }
                payload_offset += field_byte_size;
            },
            else => unreachable,
        }
    }
}

/// Bind a list destructuring pattern: extract prefix elements from a list pointer.
/// `ptr_local` is an i32 local holding a pointer to the RocList struct in memory.
fn bindListPattern(self: *Self, ptr_local: u32, list_pat: anytype) Allocator.Error!void {
    const prefix_patterns = self.store.getPatternSpan(list_pat.prefix);
    if (prefix_patterns.len == 0 and list_pat.rest.isNone()) return;

    const elem_size = self.layoutByteSize(list_pat.elem_layout);
    const elem_vt = self.resolveValType(list_pat.elem_layout);
    const is_composite = self.isCompositeLayout(list_pat.elem_layout);

    // Load elements pointer from RocList (offset 0)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i32, 0);
    const elems_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(elems_ptr);

    // Bind each prefix element
    for (prefix_patterns, 0..) |pat_id, idx| {
        const pat = self.store.getPattern(pat_id);
        const elem_offset: u32 = @intCast(idx * elem_size);
        switch (pat) {
            .bind => |bind| {
                if (is_composite and elem_size > 0) {
                    try self.emitLocalGet(elems_ptr);
                    if (elem_offset > 0) {
                        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_offset)) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                    }
                } else {
                    try self.emitLocalGet(elems_ptr);
                    try self.emitLoadOpForLayout(list_pat.elem_layout, elem_offset);
                }
                const local_idx = self.storage.allocLocal(bind.symbol, if (is_composite) .i32 else elem_vt) catch return error.OutOfMemory;
                try self.emitLocalSet(local_idx);
            },
            .wildcard => {},
            .struct_ => |inner_struct| {
                try self.emitLocalGet(elems_ptr);
                if (elem_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                const field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(field_ptr);
                try self.bindStructPattern(field_ptr, inner_struct);
            },
            else => unreachable,
        }
    }
}

/// Generate a struct field access expression.
/// Loads a field value from a struct pointer by sorted field index.
fn generateStructAccess(self: *Self, sa: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();

    // Generate the struct expression → pushes i32 pointer
    try self.generateExpr(sa.struct_expr);
    const struct_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(struct_ptr);

    // Get the field offset
    const struct_layout = ls.getLayout(sa.struct_layout);
    std.debug.assert(struct_layout.tag == .struct_);

    const field_offset = ls.getStructFieldOffset(struct_layout.data.struct_.idx, sa.field_idx);
    const field_byte_size = ls.getStructFieldSize(struct_layout.data.struct_.idx, sa.field_idx);
    const field_layout = ls.getLayout(sa.field_layout);

    // Check if the field is a composite type
    if (self.isCompositeLayout(sa.field_layout) and field_byte_size > 0) {
        const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(struct_ptr);
        if (field_offset > 0) {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        }
        try self.emitLocalSet(src_local);

        const stable_align: u32 = @intCast(field_layout.alignment(ls.targetUsize()).toByteUnits());
        const stable_offset = try self.allocStackMemory(field_byte_size, @max(stable_align, 1));
        const stable_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(stable_offset);
        try self.emitLocalSet(stable_local);
        try self.emitMemCopy(stable_local, 0, src_local, field_byte_size);
        try self.emitLocalGet(stable_local);
    } else {
        const field_vt = WasmLayout.resultValTypeWithStore(sa.field_layout, ls);
        // Load the field: [struct_ptr + field_offset] (size-aware for sub-32-bit fields)
        try self.emitLocalGet(struct_ptr);
        try self.emitLoadOpSized(field_vt, field_byte_size, field_offset);
    }
}

/// Generate a zero-arg tag expression (enum with no payload).
fn generateZeroArgTag(self: *Self, z: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(z.union_layout);

    if (l.tag == .tag_union) {
        const tu_size = ls.layoutSize(l);
        if (tu_size <= 4) {
            // Small tag union — fits in an i32 discriminant
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(z.discriminant)) catch return error.OutOfMemory;
            return;
        }
        // Larger tag union — allocate memory, store discriminant
        const align_val: u32 = @intCast(l.data.tag_union.alignment.toByteUnits());
        const frame_offset = try self.allocStackMemory(tu_size, align_val);

        const base_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(frame_offset);
        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;

        // Store discriminant (size-aware)
        const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
        const disc_offset = tu_data.discriminant_offset;
        const disc_size: u32 = tu_data.discriminant_size;
        // Push discriminant value
        if (disc_size != 0) {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(z.discriminant)) catch return error.OutOfMemory;
            try self.emitStoreToMemSized(base_local, disc_offset, .i32, disc_size);
        }

        // Push base pointer
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    } else {
        // Possibly a simple bool/enum tag
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(z.discriminant)) catch return error.OutOfMemory;
    }
}

/// Generate a tag expression (with payload).
fn generateTag(self: *Self, t: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(t.union_layout);

    std.debug.assert(l.tag == .tag_union);

    const tu_size = ls.layoutSize(l);
    const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
    const disc_offset = tu_data.discriminant_offset;
    if (tu_size <= 4 and disc_offset == 0) {
        // Small tag union — discriminant only, no payload (enum).
        // Still generate args for side effects (e.g., early_return from ? operator).
        // Args must be zero-sized since the tag has no payload room, but they may
        // contain control flow like early returns that need to execute.
        const small_args = self.store.getExprSpan(t.args);
        for (small_args) |arg_expr_id| {
            try self.generateExpr(arg_expr_id);
            self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
        }
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(t.discriminant)) catch return error.OutOfMemory;
        return;
    }

    const align_val: u32 = @intCast(l.data.tag_union.alignment.toByteUnits());
    const frame_offset = try self.allocStackMemory(tu_size, align_val);

    const base_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(frame_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;

    // Store payload args at offset 0 FIRST (payload may overlap discriminant
    // if the expression generates a wider type than the payload slot, e.g. i64
    // for a u32 tag payload — the i64 store would clobber the discriminant)
    const args = self.store.getExprSpan(t.args);
    var payload_offset: u32 = 0;
    for (args) |arg_expr_id| {
        const arg_byte_size = self.exprByteSize(arg_expr_id);
        try self.generateExpr(arg_expr_id);
        if (self.isCompositeExpr(arg_expr_id)) {
            // Composite types (Str, List, records, etc.) produce a pointer on
            // the stack. Copy the full data from the source to the tag union.
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
            try self.emitMemCopy(base_local, payload_offset, src_local, arg_byte_size);
        } else {
            const arg_vt = self.exprValType(arg_expr_id);
            try self.emitStoreToMemSized(base_local, payload_offset, arg_vt, arg_byte_size);
        }
        payload_offset += arg_byte_size;
    }

    // Store discriminant AFTER payload (so it can't be overwritten)
    const disc_size: u32 = tu_data.discriminant_size;
    if (disc_size != 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(t.discriminant)) catch return error.OutOfMemory;
        try self.emitStoreToMemSized(base_local, disc_offset, .i32, disc_size);
    }

    // Push base pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
}

/// Generate a discriminant switch expression.
/// Evaluates the tag union value, loads its discriminant, and generates
/// cascading if/else branches indexed by discriminant value.
fn generateDiscriminantSwitch(self: *Self, ds: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const branches = self.store.getExprSpan(ds.branches);

    if (branches.len == 0) {
        self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        return;
    }

    // For a single branch, just generate it directly
    if (branches.len == 1) {
        try self.generateExpr(branches[0]);
        return;
    }

    // Generate the value expression
    try self.generateExpr(ds.value);

    // Determine how to read the discriminant
    const union_layout = ls.getLayout(ds.union_layout);

    const disc_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    if (union_layout.tag == .tag_union) {
        // Tag union in memory — load discriminant from memory offset
        const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
        const disc_offset = tu_data.discriminant_offset;
        const disc_size: u32 = tu_data.discriminant_size;
        const tu_size = ls.layoutSize(union_layout);

        if (tu_size <= 4) {
            // Small tag union — the value IS the discriminant (already on stack as i32)
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, disc_local) catch return error.OutOfMemory;
        } else {
            // Value is a pointer — load discriminant from memory
            const ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
            if (disc_size == 0) {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            } else {
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                try self.emitLoadOpSized(.i32, disc_size, disc_offset);
            }
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, disc_local) catch return error.OutOfMemory;
        }
    } else {
        // Scalar/ZST — the value itself is the discriminant
        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, disc_local) catch return error.OutOfMemory;
    }

    // Determine result block type from the first branch
    const first_branch_result_layout = self.exprLayoutIdx(branches[0]);
    const bt: BlockType = valTypeToBlockType(self.resolveValType(first_branch_result_layout));

    // Generate cascading if/else: if (disc == 0) { branch0 } else if (disc == 1) { branch1 } ...
    try self.generateDiscSwitchBranches(branches, disc_local, bt, 0);
}

fn generateDiscSwitchBranches(self: *Self, branches: []const LirExprId, disc_local: u32, bt: BlockType, disc_value: u32) Allocator.Error!void {
    if (branches.len == 1) {
        // Last branch — generate unconditionally
        try self.generateExpr(branches[0]);
        return;
    }

    // Compare discriminant to disc_value
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, disc_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(disc_value)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;

    // if (disc == disc_value)
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
    self.pushExprControlFrame();
    defer self.popExprControlFrame();
    try self.generateExpr(branches[0]);
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.generateDiscSwitchBranches(branches[1..], disc_local, bt, disc_value + 1);
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Generate a while loop expression.
/// Wasm structure: block { loop { <cond> i32.eqz br_if 1 <body> drop br 0 } } i32.const 0
fn generateWhileLoop(self: *Self, wl: anytype) Allocator.Error!void {
    // block (void) — exit target for br_if
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.pushExprControlFrame();
    const break_target_depth = self.expr_control_depth;

    // loop (void) — back-edge target for br
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.pushExprControlFrame();
    self.loop_break_target_depths.append(self.allocator, break_target_depth) catch return error.OutOfMemory;
    defer {
        _ = self.loop_break_target_depths.pop();
        self.popExprControlFrame();
        self.popExprControlFrame();
    }

    // Generate condition
    try self.generateExpr(wl.cond);

    // If condition is false (0), break out of the block
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory; // break out of block (depth 1)

    // Generate body (result is discarded)
    try self.generateExpr(wl.body);
    self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;

    // Branch back to loop start
    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // continue loop (depth 0)

    // end loop
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    // end block
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // While loops return unit — push dummy i32 0
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
}

/// Generate a for loop expression.
/// Iterates over list elements, binding each to a pattern and executing the body.
fn generateForLoopExpr(self: *Self, fl: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();

    // Generate the list expression → i32 pointer to RocList struct
    try self.generateExpr(fl.list_expr);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, list_ptr) catch return error.OutOfMemory;

    // Load elements pointer (offset 0 in RocList)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, list_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i32, 0);
    const elems_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, elems_ptr) catch return error.OutOfMemory;

    // Load list length (offset 4 in RocList)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, list_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i32, 4);
    const list_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, list_len) catch return error.OutOfMemory;

    // Loop index (initialized to 0)
    const idx_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;

    // Get element size
    const elem_size: u32 = self.layoutStorageByteSize(fl.elem_layout);
    const elem_vt = WasmLayout.resultValTypeWithStore(fl.elem_layout, ls);

    // block { loop {
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.pushExprControlFrame();
    const break_target_depth = self.expr_control_depth;
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.pushExprControlFrame();
    self.loop_break_target_depths.append(self.allocator, break_target_depth) catch return error.OutOfMemory;
    defer {
        _ = self.loop_break_target_depths.pop();
        self.popExprControlFrame();
        self.popExprControlFrame();
    }

    // Check: if idx >= len, break
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, list_len) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory; // break out of block

    // Bind element to pattern
    const elem_pattern = self.store.getPattern(fl.elem_pattern);
    switch (elem_pattern) {
        .bind => |bind| {
            const bind_vt = if (self.isCompositeLayout(fl.elem_layout)) ValType.i32 else elem_vt;
            const local_idx = self.storage.allocLocal(bind.symbol, bind_vt) catch return error.OutOfMemory;

            if (elem_size == 0) {
                // ZST elements — push dummy value
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            } else if (self.isCompositeLayout(fl.elem_layout)) {
                // Composite element — compute pointer: elems_ptr + idx * elem_size
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, elems_ptr) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            } else {
                // Primitive element — load from elems_ptr + idx * elem_size
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, elems_ptr) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                try self.emitLoadOpForLayout(fl.elem_layout, 0);
            }

            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
        },
        .wildcard => {
            // No binding needed
        },
        .struct_ => |s| {
            // Compute element pointer: elems_ptr + idx * elem_size
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, elems_ptr) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            const elem_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, elem_ptr) catch return error.OutOfMemory;
            try self.bindStructPattern(elem_ptr, s);
        },
        else => unreachable,
    }

    // Generate body (result is discarded)
    try self.generateExpr(fl.body);
    self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;

    // Increment index
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;

    // Branch back to loop start
    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // end loop, end block
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // For loops return unit — push dummy i32 0
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
}

/// Generate a list construction expression.
/// Allocates element data on the stack frame and constructs a RocList struct.
fn generateList(self: *Self, l: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const elems = self.store.getExprSpan(l.elems);

    if (elems.len == 0) {
        // Empty list — same as empty_list
        const base_offset = try self.allocStackMemory(12, 4);
        try self.emitZeroInit(self.fp_local, base_offset);
        // Actually we need to zero-init at the right location
        const base_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(base_offset);
        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
        try self.emitZeroInit(base_local, 12);
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
        return;
    }

    // Get element layout and size
    const elem_size: u32 = self.layoutStorageByteSize(l.elem_layout);
    const elem_align: u32 = self.layoutStorageByteAlign(l.elem_layout);

    // Allocate space for all elements on the heap so list literals remain valid
    // when returned from functions (callee stack frames are reclaimed on return).
    const total_data_size = elem_size * @as(u32, @intCast(elems.len));
    const data_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    if (total_data_size > 0) {
        try self.emitHeapAllocConst(total_data_size, elem_align);
        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, data_base) catch return error.OutOfMemory;

        // Zero-initialize element data for consistent padding
        try self.emitZeroInit(data_base, total_data_size);
    } else {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, data_base) catch return error.OutOfMemory;
    }

    // Store each element
    const elem_vt = WasmLayout.resultValTypeWithStore(l.elem_layout, ls);
    for (elems, 0..) |elem_expr_id, i| {
        try self.generateExpr(elem_expr_id);

        const offset = @as(u32, @intCast(i)) * elem_size;
        if (self.isCompositeLayout(l.elem_layout) and elem_size > 0) {
            // Composite element — copy from source pointer
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
            try self.emitMemCopy(data_base, offset, src_local, elem_size);
        } else {
            // Primitive element — store directly
            const expr_vt = self.exprValType(elem_expr_id);
            try self.emitConversion(expr_vt, elem_vt);
            try self.emitStoreToMemSized(data_base, offset, elem_vt, elem_size);
        }
    }

    // Construct the 12-byte RocList struct on the stack frame
    const list_offset = try self.allocStackMemory(12, 4);
    const list_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(list_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, list_base) catch return error.OutOfMemory;

    // Store elements pointer (offset 0)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, data_base) catch return error.OutOfMemory;
    try self.emitStoreToMem(list_base, 0, .i32);

    // Store length (offset 4)
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elems.len)) catch return error.OutOfMemory;
    try self.emitStoreToMem(list_base, 4, .i32);

    // Store capacity (offset 8) — same as length for stack-allocated lists
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elems.len)) catch return error.OutOfMemory;
    try self.emitStoreToMem(list_base, 8, .i32);

    // Push pointer to the RocList struct
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, list_base) catch return error.OutOfMemory;
}

/// Generate a low-level operation.
fn generateLowLevel(self: *Self, ll: anytype) Allocator.Error!void {
    const args = self.store.getExprSpan(ll.args);

    switch (ll.op) {
        // Numeric operations (arithmetic, comparisons, shifts)
        .num_plus,
        .num_minus,
        .num_times,
        .num_div_by,
        .num_div_trunc_by,
        .num_rem_by,
        .num_negate,
        .num_abs,
        .num_is_eq,
        .num_is_gt,
        .num_is_gte,
        .num_is_lt,
        .num_is_lte,
        => {
            return self.generateNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        .bool_not => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        },

        // Safe integer widenings (no-op or single instruction)
        .u8_to_i16, .u8_to_i32, .u8_to_u16, .u8_to_u32 => {
            // u8 is already i32 in wasm, and widening to larger types is a no-op
            try self.generateExpr(args[0]);
            // If arg produces i64 (e.g. from i64_literal), wrap to i32
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
        },
        .i8_to_i16, .i8_to_i32 => {
            // i8 is i32 in wasm, sign-extend from 8 bits
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        },
        .u16_to_i32, .u16_to_u32 => {
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
        },
        .i16_to_i32 => {
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
        },

        // i32/u32 → i64/u64
        .u8_to_i64,
        .u8_to_u64,
        .u16_to_i64,
        .u16_to_u64,
        .u32_to_i64,
        .u32_to_u64,
        => {
            try self.generateExpr(args[0]);
            const arg_vt = self.exprValType(args[0]);
            if (arg_vt == .i64) {
                // Already i64 — no extension needed
                return;
            }
            self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
        },
        .i8_to_i64, .i16_to_i64, .i32_to_i64 => {
            try self.generateExpr(args[0]);
            const arg_vt = self.exprValType(args[0]);
            if (arg_vt == .i64) {
                // Already i64 — no extension needed
                return;
            }
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },

        // Narrowing/wrapping conversions
        .i64_to_i32_wrap, .u64_to_u32_wrap, .u64_to_i32_wrap, .i64_to_u32_wrap => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .i32_to_i8_wrap,
        .u32_to_u8_wrap,
        .i32_to_u8_wrap,
        .i64_to_u8_wrap,
        .u64_to_u8_wrap,
        .i64_to_i8_wrap,
        .u64_to_i8_wrap,
        .u16_to_i8_wrap,
        .u16_to_u8_wrap,
        .i16_to_i8_wrap,
        .i16_to_u8_wrap,
        .u32_to_i8_wrap,
        => {
            try self.generateExpr(args[0]);
            // May need to wrap i64 to i32 first
            const arg_vt = self.exprValType(args[0]);
            if (arg_vt == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            // Mask to 8 bits
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i32_to_i16_wrap,
        .u32_to_u16_wrap,
        .i32_to_u16_wrap,
        .i64_to_u16_wrap,
        .u64_to_u16_wrap,
        .i64_to_i16_wrap,
        .u64_to_i16_wrap,
        .u32_to_i16_wrap,
        => {
            try self.generateExpr(args[0]);
            const arg_vt = self.exprValType(args[0]);
            if (arg_vt == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            // Mask to 16 bits
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFFFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i32_to_u32_wrap,
        .u32_to_i32_wrap,
        .u8_to_i8_wrap,
        .i8_to_u8_wrap,
        .u16_to_i16_wrap,
        .i16_to_u16_wrap,
        => {
            // Same representation in wasm (both i32), no-op
            try self.generateExpr(args[0]);
        },
        .i64_to_u64_wrap, .u64_to_i64_wrap => {
            // Same representation in wasm (both i64), no-op
            try self.generateExpr(args[0]);
        },

        // Signed sub-i32 to unsigned wider wrapping (needs sign extension)
        .i8_to_u32_wrap => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        },
        .i16_to_u32_wrap => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
        },
        .i8_to_u16_wrap => {
            try self.generateExpr(args[0]);
            // Sign-extend from 8 bits then mask to 16 bits
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFFFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i8_to_u64_wrap => {
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i16_to_u64_wrap => {
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i32_to_u64_wrap => {
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i32_to_u128_wrap => {
            // Signed i32→u128 wrap: sign-extend to i64, then to i128
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                // Already i64
            } else {
                self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            }
            try self.emitIntToI128(true);
        },
        .i32_to_u64_try => {
            // Signed i32 → unsigned u64: check >= 0, then sign-extend to i64
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 8, 8);
            // Check: val >= 0
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            // Ok path: sign-extend i32 to i64, store payload
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 0);
            // Set discriminant = 1 (Ok)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            try self.emitStoreOpSized(.i32, 1, 8);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .i32_to_u128_try => {
            // Signed i32 → unsigned u128: check >= 0, then sign-extend to i128
            try self.generateExpr(args[0]);
            // Sign-extend to i64 first
            if (self.exprValType(args[0]) != .i64) {
                self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            }
            try self.emitIntToI128(true);
            try self.emitI128TryToU128(true);
        },

        // Float conversions
        .f32_to_f64 => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
        },
        .f64_to_f32_wrap => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },

        // Int to float
        .i32_to_f32, .i8_to_f32, .i16_to_f32 => {
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.f32_convert_i32_s) catch return error.OutOfMemory;
        },
        .u32_to_f32, .u8_to_f32, .u16_to_f32 => {
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.f32_convert_i32_u) catch return error.OutOfMemory;
        },
        .i32_to_f64, .i8_to_f64, .i16_to_f64 => {
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.f64_convert_i32_s) catch return error.OutOfMemory;
        },
        .u32_to_f64, .u8_to_f64, .u16_to_f64 => {
            try self.generateExpr(args[0]);
            if (self.exprValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.f64_convert_i32_u) catch return error.OutOfMemory;
        },
        .i64_to_f32 => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f32_convert_i64_s) catch return error.OutOfMemory;
        },
        .u64_to_f32 => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f32_convert_i64_u) catch return error.OutOfMemory;
        },
        .i64_to_f64 => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
        },
        .u64_to_f64 => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
        },

        // Float to int (truncating)
        .f32_to_i32_trunc, .f32_to_i8_trunc, .f32_to_i16_trunc => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f32_s) catch return error.OutOfMemory;
        },
        .f32_to_u32_trunc, .f32_to_u8_trunc, .f32_to_u16_trunc => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f32_u) catch return error.OutOfMemory;
        },
        .f64_to_i32_trunc, .f64_to_i8_trunc, .f64_to_i16_trunc => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f64_s) catch return error.OutOfMemory;
        },
        .f64_to_u32_trunc, .f64_to_u8_trunc, .f64_to_u16_trunc => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f64_u) catch return error.OutOfMemory;
        },
        .f32_to_i64_trunc => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f32_s) catch return error.OutOfMemory;
        },
        .f32_to_u64_trunc => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f32_u) catch return error.OutOfMemory;
        },
        .f64_to_i64_trunc => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f64_s) catch return error.OutOfMemory;
        },
        .f64_to_u64_trunc => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f64_u) catch return error.OutOfMemory;
        },

        // Float math functions (direct wasm opcodes)
        .num_sqrt => {
            try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_sqrt,
                .f64 => Op.f64_sqrt,
                .i32, .i64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_floor => {
            try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_floor,
                .f64 => Op.f64_floor,
                .i32, .i64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_ceiling => {
            try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_ceil,
                .f64 => Op.f64_ceil,
                .i32, .i64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_round => {
            try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_nearest,
                .f64 => Op.f64_nearest,
                .i32, .i64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },

        // Modulo (integer only — float mod not yet supported)
        .num_mod_by => {
            return self.generateNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        // List operations
        .list_len => {
            // Load length from RocList struct (offset 4)
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i32, 4);
            // list_len returns U64 in Roc, but we store it as i32 on wasm32
            // If ret_layout expects i64, extend
            const ret_vt = self.resolveValType(ll.ret_layout);
            if (ret_vt == .i64) {
                self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            }
        },
        .list_get_unsafe => {
            // args[0] = list, args[1] = index
            // Returns bare element without bounds checking.
            const ls = self.getLayoutStore();

            // Get element layout from the list type
            const list_layout_idx = self.exprLayoutIdx(args[0]);
            const list_layout = ls.getLayout(list_layout_idx);
            const elem_layout_idx = switch (list_layout.tag) {
                .list => list_layout.data.list,
                .list_of_zst => ll.ret_layout,
                else => unreachable,
            };
            const elem_size: u32 = self.layoutStorageByteSize(elem_layout_idx);
            const elem_is_composite = self.isCompositeLayout(elem_layout_idx);

            // Generate list expression and save pointer
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            // Generate index as i32
            const index_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const index_expr = self.store.getExpr(args[1]);
            switch (index_expr) {
                .dec_literal => |v| {
                    // Dec literals are scaled by 10^18. Convert back to integer.
                    const one_point_zero: i128 = 1_000_000_000_000_000_000;
                    const actual: i32 = if (v == 0) 0 else @intCast(@divExact(v, one_point_zero));
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, actual) catch return error.OutOfMemory;
                },
                .i64_literal => |v| {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(v.value)) catch return error.OutOfMemory;
                },
                else => {
                    try self.generateExpr(args[1]);
                    if (self.exprValType(args[1]) == .i64) {
                        self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                    }
                },
            }
            try self.emitLocalSet(index_local);

            if (builtin.mode == .Debug and ls.getLayout(ll.ret_layout).tag == .tag_union) {
                std.debug.panic(
                    "WasmCodeGen invariant violated: list_get_unsafe must not return a tag_union layout",
                    .{},
                );
            }

            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalGet(index_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

            if (!elem_is_composite) {
                try self.emitLoadOpForLayout(elem_layout_idx, 0);
            }
        },

        // String operations
        // Bitwise operations
        .num_pow, .num_log => unreachable, // Resolved by MIR/LIR lowering

        .num_abs_diff => {
            // abs_diff(a, b) -> |a - b|
            return self.generateNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        .num_shift_left_by, .num_shift_right_by, .num_shift_right_zf_by => {
            // Shift operations: shift(value, amount)
            return self.generateNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        .list_sort_with => {
            try self.generateLLListSortWith(ll, args, ll.ret_layout);
        },

        .list_drop_at => {
            // TODO: implement list_drop_at for wasm.
            @panic("TODO: wasm list_drop_at is not implemented");
        },

        // List element access operations (no heap allocation needed)
        .list_first => {
            // list_first(list) -> elem  (loads first element)
            try self.generateExpr(args[0]);
            // Load elements_ptr from RocList (offset 0)
            try self.emitLoadOp(.i32, 0);
            // Load first element from elements_ptr
            if (self.isCompositeLayout(ll.ret_layout)) {
                // Composite — pointer is the result
            } else {
                const ret_vt = self.resolveValType(ll.ret_layout);
                const ret_byte_size = self.layoutStorageByteSize(ll.ret_layout);
                try self.emitLoadOpSized(ret_vt, ret_byte_size, 0);
            }
        },
        .list_last => {
            // list_last(list) -> elem  (loads last element)
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            // Load elements_ptr (offset 0)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);

            // Load length (offset 4)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);

            // Compute address: elements_ptr + (len-1) * elem_size
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            const ret_byte_size = self.layoutStorageByteSize(ll.ret_layout);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(ret_byte_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

            // Load last element
            if (self.isCompositeLayout(ll.ret_layout)) {
                // Composite — pointer is the result
            } else {
                const ret_vt = self.resolveValType(ll.ret_layout);
                try self.emitLoadOpSized(ret_vt, ret_byte_size, 0);
            }
        },
        .list_drop_first => {
            // list_drop_first(list, count) -> list
            // Returns a RocList with adjusted elements_ptr and length
            // No allocation needed — returns a view
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            try self.generateExpr(args[1]);
            if (self.exprValType(args[1]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;

            // Allocate result RocList (12 bytes)
            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

            // Get element size from ret_layout (which is the list layout, not elem)
            const elem_size = self.getListElemSize(ll.ret_layout);

            // new_ptr = old_ptr + count * elem_size
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 0);

            // new_len = old_len - count
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Encode seamless-slice cap from the source allocation pointer.
            const encoded_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitPrepareListSliceMetadata(list_local, self.listContainsRefcounted(ll.ret_layout), encoded_cap);

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, encoded_cap) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 8);

            // Push result pointer
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
        },
        .list_drop_last => {
            // list_drop_last(list, count) -> list
            // Returns a RocList with adjusted length (pointer stays same)
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            try self.generateExpr(args[1]);
            if (self.exprValType(args[1]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;

            // Allocate result RocList (12 bytes)
            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

            // Same elements_ptr
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            try self.emitStoreOp(.i32, 0);

            // new_len = old_len - count
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Same capacity
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 8);
            try self.emitStoreOp(.i32, 8);

            // Push result pointer
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
        },
        .list_take_first => {
            // list_take_first(list, count) -> list
            // Same as list but with length = min(count, len)
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            try self.generateExpr(args[1]);
            if (self.exprValType(args[1]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;

            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

            // Same elements_ptr
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            try self.emitStoreOp(.i32, 0);

            // new_len = min(count, old_len) using select
            // Stack: count, old_len, count <= old_len
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.body.append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.select) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Same capacity
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 8);
            try self.emitStoreOp(.i32, 8);

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
        },
        .list_take_last => {
            // list_take_last(list, count) -> list
            // elements_ptr += (len - min(count, len)) * elem_size
            // length = min(count, len)
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            try self.generateExpr(args[1]);
            if (self.exprValType(args[1]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;

            // Load length
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;

            // actual_count = min(count, len)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.select) catch return error.OutOfMemory;
            const actual_count = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, actual_count) catch return error.OutOfMemory;

            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

            const elem_size = self.getListElemSize(ll.ret_layout);

            // new_ptr = old_ptr + (len - actual_count) * elem_size
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, actual_count) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 0);

            // new_len = actual_count
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, actual_count) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Encode seamless-slice cap from the source allocation pointer.
            const encoded_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitPrepareListSliceMetadata(list_local, self.listContainsRefcounted(ll.ret_layout), encoded_cap);

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, encoded_cap) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 8);

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
        },

        .list_contains => blk: {
            // list_contains(list, needle) -> Bool
            // Linear scan through list elements using layout-aware equality.
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            const ls = self.getLayoutStore();
            const list_layout_idx = self.exprLayoutIdx(args[0]);
            const list_layout = ls.getLayout(list_layout_idx);
            const list_info = switch (list_layout.tag) {
                .list => ls.getListInfo(list_layout),
                .list_of_zst => {
                    // contains for ZST elements is true iff the list is non-empty
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
                    try self.emitLoadOp(.i32, 4);
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
                    break :blk;
                },
                else => unreachable,
            };
            const elem_layout_idx = list_info.elem_layout_idx;
            const elem_byte_size = list_info.elem_size;
            const elem_is_composite = self.isCompositeLayout(elem_layout_idx);

            const needle_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            if (elem_is_composite) {
                try self.generateExpr(args[1]);

                if (self.exprNeedsCompositeCallStabilization(args[1])) {
                    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitLocalSet(src_local);
                    const dst_offset = try self.allocStackMemory(elem_byte_size, 4);
                    const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitFpOffset(dst_offset);
                    try self.emitLocalSet(dst_local);
                    try self.emitMemCopy(dst_local, 0, src_local, elem_byte_size);
                    try self.emitLocalGet(dst_local);
                }

                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, needle_ptr_local) catch return error.OutOfMemory;
            } else {
                try self.generateExpr(args[1]);
                const needle_vt = self.exprValType(args[1]);
                const needle_tmp = self.storage.allocAnonymousLocal(needle_vt) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, needle_tmp) catch return error.OutOfMemory;

                const alignment: u32 = if (elem_byte_size >= 8) 8 else if (elem_byte_size >= 4) 4 else if (elem_byte_size >= 2) 2 else 1;
                const needle_offset = try self.allocStackMemory(elem_byte_size, alignment);
                try self.emitFpOffset(needle_offset);
                const needle_addr_tmp = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(needle_addr_tmp);
                try self.emitLocalGet(needle_addr_tmp);
                try self.emitLocalGet(needle_tmp);
                try self.emitStoreOpSized(needle_vt, elem_byte_size, 0);
                try self.emitLocalGet(needle_addr_tmp);
                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, needle_ptr_local) catch return error.OutOfMemory;
            }

            // Load list ptr and len
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            const ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;

            // result = 0 (not found)
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

            // idx = 0
            const idx_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;

            // block { loop {
            self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

            // if idx >= len: br 1 (exit block)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

            // Load element at ptr + idx * elem_size
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_byte_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            const elem_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, elem_ptr_local) catch return error.OutOfMemory;

            // Compare with needle using layout-aware equality
            try self.compareFieldByLayout(elem_ptr_local, needle_ptr_local, 0, elem_byte_size, elem_layout_idx);

            // if equal: set result = 1, br 1 (exit)
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

            // idx += 1
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, idx_local) catch return error.OutOfMemory;

            // br 0 (continue loop)
            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

            // } } end loop, end block
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

            // Push result
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
        },

        .list_append_unsafe => {
            // list_append(list, elem) -> new list with elem appended
            try self.generateLLListAppend(args, ll.ret_layout);
        },
        .list_prepend => {
            // list_prepend(list, elem) -> new list with elem prepended
            try self.generateLLListPrepend(args, ll.ret_layout);
        },
        .list_concat => {
            // list_concat(list_a, list_b) -> concatenated list
            try self.generateLLListConcat(args, ll.ret_layout);
        },
        .list_reverse => {
            // list_reverse(list) -> reversed list
            try self.generateLLListReverse(args, ll.ret_layout);
        },
        // list_with_capacity(capacity) -> empty list with given capacity
        .list_with_capacity => {
            try self.generateLLListWithCapacity(args, ll.ret_layout);
        },
        // list_set(list, index, value) -> list with value at index
        .list_set => {
            try self.generateLLListSet(args, ll.ret_layout);
        },
        // list_reserve(list, capacity) -> list with at least that capacity
        .list_reserve => {
            try self.generateLLListReserve(args, ll.ret_layout);
        },
        // list_release_excess_capacity(list) -> list with capacity = length
        .list_release_excess_capacity => {
            try self.generateLLListReleaseExcessCapacity(args, ll.ret_layout);
        },
        // list_split_first(list) -> { first: elem, rest: list }
        .list_split_first => {
            try self.generateLLListSplitFirst(args, ll.ret_layout);
        },
        // list_split_last(list) -> { rest: list, last: elem }
        .list_split_last => {
            try self.generateLLListSplitLast(args, ll.ret_layout);
        },
        // list_sublist(list, {len: U64, start: U64}) -> list
        .list_sublist => {
            // Shared layout uses canonical alphabetical field indices for records.
            // For { start : U64, len : U64 }, that means index 0 = len and index 1 = start.
            const ls = self.getLayoutStore();
            const record_layout_idx = self.exprLayoutIdx(args[1]);
            const record_layout = ls.getLayout(record_layout_idx);
            const record_idx = record_layout.data.struct_.idx;
            const len_field_off = ls.getStructFieldOffsetByOriginalIndex(record_idx, 0);
            const start_field_off = ls.getStructFieldOffsetByOriginalIndex(record_idx, 1);
            if (builtin.mode == .Debug) {
                const sd = ls.getStructData(record_idx);
                const sorted_fields = ls.struct_fields.sliceRange(sd.getFields());
                if (sorted_fields.len != 2) {
                    std.debug.panic(
                        "LIR/wasm invariant violated: list_sublist record expected 2 fields, got {d}",
                        .{sorted_fields.len},
                    );
                }
                const record_size = ls.getStructData(record_idx).size;
                if (ls.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .u64 or
                    ls.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .u64 or
                    ls.getStructFieldSizeByOriginalIndex(record_idx, 0) != 8 or
                    ls.getStructFieldSizeByOriginalIndex(record_idx, 1) != 8 or
                    record_size != 16)
                {
                    std.debug.panic(
                        "LIR/wasm invariant violated: list_sublist record expected canonical fields len/start as two U64s in 16 bytes, got layouts [{}, {}] size {d}",
                        .{
                            ls.getStructFieldLayoutByOriginalIndex(record_idx, 0),
                            ls.getStructFieldLayoutByOriginalIndex(record_idx, 1),
                            record_size,
                        },
                    );
                }
            }

            // Generate list arg (pointer to {data_ptr, len, capacity})
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            // Generate record arg (pointer to the config record)
            try self.generateExpr(args[1]);
            const rec_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, rec_local) catch return error.OutOfMemory;

            // Load "len" field by original semantic index, wrap to i32
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, rec_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, len_field_off);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            const sub_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, sub_len) catch return error.OutOfMemory;

            // Load "start" field by original semantic index, wrap to i32
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, rec_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, start_field_off);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            const start_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, start_local) catch return error.OutOfMemory;

            // Load old_len from list
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, old_len) catch return error.OutOfMemory;

            // actual_start = min(start, old_len)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, start_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, old_len) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, start_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, old_len) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.select) catch return error.OutOfMemory;
            const actual_start = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, actual_start) catch return error.OutOfMemory;

            // remaining = old_len - actual_start
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, old_len) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, actual_start) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            const remaining = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, remaining) catch return error.OutOfMemory;

            // actual_len = min(sub_len, remaining)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, sub_len) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, remaining) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, sub_len) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, remaining) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.select) catch return error.OutOfMemory;
            const actual_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, actual_len) catch return error.OutOfMemory;

            // Allocate result RocList (12 bytes)
            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

            const elem_size = self.getListElemSize(ll.ret_layout);

            // new_ptr = old_ptr + actual_start * elem_size
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, actual_start) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 0);

            // new_len = actual_len
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, actual_len) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Encode seamless-slice cap from the source allocation pointer.
            const encoded_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitPrepareListSliceMetadata(list_local, self.listContainsRefcounted(ll.ret_layout), encoded_cap);

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, encoded_cap) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 8);

            // Push result pointer
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
        },

        .str_count_utf8_bytes => {
            // Returns the length of the string in UTF-8 bytes
            try self.generateExpr(args[0]);
            // For SSO (byte 11 high bit set): length = byte 11 & 0x7F
            // For heap: length at offset 4
            // We use the simplified approach: load byte 11, check SSO bit
            const str_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, str_local) catch return error.OutOfMemory;

            // Load byte 11 (SSO tag byte)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, str_local) catch return error.OutOfMemory;
            try self.emitLoadOpSized(.i32, 1, 11);
            const tag_byte = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, tag_byte) catch return error.OutOfMemory;

            // Check if SSO: high bit set
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0x80) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

            // if SSO: len = tag_byte & 0x7F; else: len = load i32 from offset 4
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(ValType.i32)) catch return error.OutOfMemory;
            // SSO path
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, tag_byte) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0x7F) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            // Heap path
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, str_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

            // Result is length as i32. If ret_layout expects i64, extend.
            const ret_vt = self.resolveValType(ll.ret_layout);
            if (ret_vt == .i64) {
                self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            }
        },

        .str_is_eq => {
            // String equality via host function (handles both SSO and heap strings)
            const import_idx = self.str_eq_import orelse unreachable;
            try self.generateExpr(args[0]);
            const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a);
            try self.generateExpr(args[1]);
            const b = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(b);
            // Push both pointers and call host function
            try self.emitLocalGet(a);
            try self.emitLocalGet(b);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
        },
        .str_concat => {
            // LowLevel str_concat: concatenate 2 strings
            try self.generateExpr(args[0]);
            const a_str = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a_str);
            try self.generateExpr(args[1]);
            const b_str = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(b_str);

            // Extract ptr+len from each
            const a_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const a_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitExtractStrPtrLen(a_str, a_ptr, a_len);
            const b_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const b_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitExtractStrPtrLen(b_str, b_ptr, b_len);

            // total = a_len + b_len
            const total = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(a_len);
            try self.emitLocalGet(b_len);
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(total);

            // Allocate buffer
            try self.emitHeapAlloc(total, 1);
            const buf = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(buf);

            // Copy a bytes at offset 0
            const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalSet(zero);
            try self.emitMemCopyLoop(buf, zero, a_ptr, a_len);

            // Copy b bytes at offset a_len
            try self.emitMemCopyLoop(buf, a_len, b_ptr, b_len);

            // Build heap RocStr
            try self.buildHeapRocStr(buf, total);
        },
        .str_contains => {
            // Check if string a contains substring b
            try self.generateLLStrSearch(args, .contains);
        },
        .str_starts_with => {
            try self.generateLLStrSearch(args, .starts_with);
        },
        .str_ends_with => {
            try self.generateLLStrSearch(args, .ends_with);
        },
        .str_to_utf8 => {
            try self.generateStrToUtf8(args[0]);
        },
        .str_from_utf8_lossy => {
            try self.generateStrFromUtf8Lossy(args[0]);
        },
        .str_trim,
        .str_trim_start,
        .str_trim_end,
        .str_with_ascii_lowercased,
        .str_with_ascii_uppercased,
        .str_release_excess_capacity,
        => {
            const import_idx = switch (ll.op) {
                .str_trim => self.str_trim_import orelse unreachable,
                .str_trim_start => self.str_trim_start_import orelse unreachable,
                .str_trim_end => self.str_trim_end_import orelse unreachable,
                .str_with_ascii_lowercased => self.str_with_ascii_lowercased_import orelse unreachable,
                .str_with_ascii_uppercased => self.str_with_ascii_uppercased_import orelse unreachable,
                .str_release_excess_capacity => self.str_release_excess_capacity_import orelse unreachable,
                else => unreachable,
            };
            try self.generateExpr(args[0]);
            const input = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(input);
            const result_offset = try self.allocStackMemory(12, 4);
            try self.emitLocalGet(input);
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
        },
        .str_drop_prefix, .str_drop_suffix => {
            const import_idx = switch (ll.op) {
                .str_drop_prefix => self.str_drop_prefix_import orelse unreachable,
                .str_drop_suffix => self.str_drop_suffix_import orelse unreachable,
                else => unreachable,
            };
            try self.generateExpr(args[0]);
            const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a);
            try self.generateExpr(args[1]);
            const b = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(b);
            const result_offset = try self.allocStackMemory(12, 4);
            try self.emitLocalGet(a);
            try self.emitLocalGet(b);
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
        },
        .str_split_on, .str_join_with => {
            const import_idx = switch (ll.op) {
                .str_split_on => self.str_split_import orelse unreachable,
                .str_join_with => self.str_join_with_import orelse unreachable,
                else => unreachable,
            };
            try self.generateExpr(args[0]);
            const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a);
            try self.generateExpr(args[1]);
            const b = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(b);
            const result_offset = try self.allocStackMemory(12, 4);
            try self.emitLocalGet(a);
            try self.emitLocalGet(b);
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
        },
        .str_repeat, .str_reserve => {
            const import_idx = switch (ll.op) {
                .str_repeat => self.str_repeat_import orelse unreachable,
                .str_reserve => self.str_reserve_import orelse unreachable,
                else => unreachable,
            };
            try self.generateExpr(args[0]);
            const str_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(str_local);
            try self.generateExpr(args[1]);
            const int_vt = self.exprValType(args[1]);
            if (int_vt == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const int_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(int_local);
            const result_offset = try self.allocStackMemory(12, 4);
            try self.emitLocalGet(str_local);
            try self.emitLocalGet(int_local);
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
        },
        .str_with_capacity => {
            const import_idx = self.str_with_capacity_import orelse unreachable;
            try self.generateExpr(args[0]);
            const int_vt = self.exprValType(args[0]);
            if (int_vt == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const int_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(int_local);
            const result_offset = try self.allocStackMemory(12, 4);
            try self.emitLocalGet(int_local);
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
        },
        .str_caseless_ascii_equals => {
            const import_idx = self.str_caseless_ascii_equals_import orelse unreachable;
            try self.generateExpr(args[0]);
            const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a);
            try self.generateExpr(args[1]);
            const b = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(b);
            try self.emitLocalGet(a);
            try self.emitLocalGet(b);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
        },
        .str_from_utf8 => {
            const ls = self.getLayoutStore();
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            if (ret_layout_val.tag != .tag_union) unreachable;
            const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
            const import_idx = self.str_from_utf8_import orelse unreachable;
            try self.generateExpr(args[0]);
            const input = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(input);
            const result_offset = try self.allocStackMemory(tu_data.size, 4);
            try self.emitLocalGet(input);
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(tu_data.size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(tu_data.discriminant_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
        },
        .num_from_str => {
            const ls = self.getLayoutStore();
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            if (ret_layout_val.tag != .tag_union) unreachable;
            const tu_data = ls.getTagUnionData(ret_layout_val.data.tag_union.idx);
            const disc_offset: u32 = tu_data.discriminant_offset;
            const result_offset = try self.allocStackMemory(tu_data.size, 4);

            var ok_payload_idx: ?layout.Idx = null;
            const variants = ls.getTagUnionVariants(tu_data);
            for (0..variants.len) |i| {
                const payload = variants.get(@intCast(i)).payload_layout;
                const candidate_payload = blk: {
                    const payload_layout = ls.getLayout(payload);
                    if (payload_layout.tag != .struct_) break :blk payload;

                    const struct_data = ls.getStructData(payload_layout.data.struct_.idx);
                    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
                    if (fields.len != 1) break :blk payload;

                    break :blk fields.get(0).layout;
                };

                if (candidate_payload == .dec or candidate_payload == .f32 or candidate_payload == .f64) {
                    ok_payload_idx = candidate_payload;
                    break;
                }
                const payload_layout = ls.getLayout(candidate_payload);
                if (payload_layout.tag == .scalar) {
                    ok_payload_idx = candidate_payload;
                    break;
                }
            }
            const ok_payload = ok_payload_idx orelse unreachable;

            try self.generateExpr(args[0]);
            const input = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(input);

            if (ok_payload == .dec) {
                const import_idx = self.dec_from_str_import orelse unreachable;
                try self.emitLocalGet(input);
                try self.emitFpOffset(result_offset);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(disc_offset)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            } else if (ok_payload == .f32 or ok_payload == .f64) {
                const import_idx = self.float_from_str_import orelse unreachable;
                const float_width: i32 = if (ok_payload == .f32) 4 else 8;
                try self.emitLocalGet(input);
                try self.emitFpOffset(result_offset);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, float_width) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(disc_offset)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            } else {
                const import_idx = self.int_from_str_import orelse unreachable;
                const int_width: i32 = switch (ok_payload) {
                    .u8, .i8 => 1,
                    .u16, .i16 => 2,
                    .u32, .i32 => 4,
                    .u64, .i64 => 8,
                    .u128, .i128 => 16,
                    else => unreachable,
                };
                const is_signed: i32 = switch (ok_payload) {
                    .i8, .i16, .i32, .i64, .i128 => 1,
                    .u8, .u16, .u32, .u64, .u128 => 0,
                    else => unreachable,
                };
                try self.emitLocalGet(input);
                try self.emitFpOffset(result_offset);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, int_width) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, is_signed) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(disc_offset)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            }

            try self.emitFpOffset(result_offset);
        },

        .str_inspect,
        .u8_to_str,
        .i8_to_str,
        .u16_to_str,
        .i16_to_str,
        .u32_to_str,
        .i32_to_str,
        .u64_to_str,
        .i64_to_str,
        .u128_to_str,
        .i128_to_str,
        .dec_to_str,
        .f32_to_str,
        .f64_to_str,
        .num_to_str,
        .num_from_numeral,
        => unreachable, // Resolved before backend codegen

        // Box operations
        .box_box => {
            // box_box(value) -> Box value (pointer to heap-allocated copy)
            const value_expr = args[0];
            const value_size = self.exprByteSize(value_expr);
            const value_vt = self.exprValType(value_expr);

            // Determine alignment (same logic as allocStackMemory)
            const alignment: u32 = if (value_size >= 8) 8 else if (value_size >= 4) 4 else if (value_size >= 2) 2 else 1;

            // Allocate heap memory for the boxed value
            try self.emitHeapAllocConst(value_size, alignment);
            const box_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(box_ptr);

            // Generate the value expression
            try self.generateExpr(value_expr);

            // Store value to box - depends on whether it's scalar or composite
            if (value_vt == .i32 and value_size > 4) {
                // Composite type (value is a pointer to data) - need to copy bytes
                const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(src_ptr);

                // Copy value_size bytes from src_ptr to box_ptr using a byte-by-byte loop
                // For small sizes, unroll; for larger, use a loop
                if (value_size <= 16) {
                    // Unroll for small values
                    var offset: u32 = 0;
                    while (offset + 4 <= value_size) : (offset += 4) {
                        try self.emitLocalGet(box_ptr);
                        try self.emitLocalGet(src_ptr);
                        self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory; // align
                        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
                    }
                    while (offset < value_size) : (offset += 1) {
                        try self.emitLocalGet(box_ptr);
                        try self.emitLocalGet(src_ptr);
                        self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
                    }
                } else {
                    // Use a loop for larger values
                    const i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    try self.emitLocalSet(i);

                    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
                    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

                    // box_ptr[i] = src_ptr[i]
                    try self.emitLocalGet(box_ptr);
                    try self.emitLocalGet(i);
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                    try self.emitLocalGet(src_ptr);
                    try self.emitLocalGet(i);
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

                    // i++
                    try self.emitLocalGet(i);
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                    try self.emitLocalSet(i);

                    // continue if i < size
                    try self.emitLocalGet(i);
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(value_size)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_lt_u) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

                    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
                }
            } else {
                // Scalar type - store directly
                try self.emitStoreToMemSized(box_ptr, 0, value_vt, value_size);
            }

            // Return box pointer
            try self.emitLocalGet(box_ptr);
        },
        .box_unbox => {
            // box_unbox(box_ptr) -> value
            // Box is a transparent pointer - dereference it
            const box_expr = args[0];
            try self.generateExpr(box_expr);

            // Determine result type
            const result_vt = self.resolveValType(ll.ret_layout);
            const result_size = self.layoutByteSize(ll.ret_layout);

            if (result_vt == .i32 and result_size > 4) {
                // Composite type - box pointer IS the result (transparent pointer)
                // Just leave it on the stack
            } else {
                // Scalar type - load from the box pointer
                const box_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(box_ptr);
                try self.emitLocalGet(box_ptr);
                try self.emitLoadOpSized(result_vt, result_size, 0);
            }
        },

        // Compare — returns Ordering enum (EQ=0, GT=1, LT=2)
        .compare => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            // Determine arg type from first arg's layout
            const arg_layout = self.exprLayoutIdx(args[0]);
            const arg_vt = self.exprValType(args[0]);

            // Determine if unsigned from layout
            const is_unsigned = switch (arg_layout) {
                .u8, .u16, .u32, .u64, .u128 => true,
                else => false,
            };

            switch (arg_vt) {
                .i32 => {
                    // gt_flag = (a > b) ? 1 : 0
                    const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    const b = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    // gt_flag
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, if (is_unsigned) Op.i32_gt_u else Op.i32_gt_s) catch return error.OutOfMemory;
                    // lt_flag * 2
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, if (is_unsigned) Op.i32_lt_u else Op.i32_lt_s) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                    // result = gt_flag + lt_flag * 2
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                },
                .i64 => {
                    const a = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    const b = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, if (is_unsigned) Op.i64_gt_u else Op.i64_gt_s) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, if (is_unsigned) Op.i64_lt_u else Op.i64_lt_s) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                },
                .f32 => {
                    const a = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
                    const b = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f32_gt) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f32_lt) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                },
                .f64 => {
                    const a = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
                    const b = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f64_gt) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, a) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, b) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f64_lt) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                },
            }
        },

        // Crash
        .crash => {
            self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },

        // Integer try conversions — return Result(TargetInt, {}) tag union
        // Layout: payload at offset 0, discriminant (1 byte) after payload. Ok=1, Err=0.
        // Narrowing i32 → smaller signed
        .i32_to_i8_try, .i16_to_i8_try, .u16_to_i8_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            // Check: val >= -128 && val <= 127
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, -128) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 127) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u8_to_i8_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            // u8 → i8: check val <= 127
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 127) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        // Narrowing to u8
        .i32_to_u8_try, .i16_to_u8_try, .u16_to_u8_try, .i8_to_u8_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 255) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        // Narrowing to i16
        .i32_to_i16_try, .u32_to_i16_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 2, 2);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, -32768) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 32767) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 2, 2);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u16_to_i16_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 2, 2);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 32767) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 2, 2);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        // Narrowing to u16
        .i32_to_u16_try, .u32_to_u16_try, .i16_to_u16_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 2, 2);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 65535) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 2, 2);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        // i32 <-> u32 try
        .i32_to_u32_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 4, 4);
            // i32 → u32: check val >= 0
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 4, 4);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u32_to_i32_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 4, 4);
            // u32 → i32: check high bit is 0 (val <= 0x7FFFFFFF)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 4, 4);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u32_to_i8_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 127) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u32_to_u8_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 255) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        // i8/i16 → unsigned wider types (always succeed since value fits — but need sign check)
        .i8_to_u16_try,
        .i8_to_u32_try,
        .i8_to_u64_try,
        .i16_to_u32_try,
        .i16_to_u64_try,
        => {
            try self.generateExpr(args[0]);
            // These are widening but signed→unsigned, so check val >= 0
            const target_is_i64 = (ll.op == .i8_to_u64_try or ll.op == .i16_to_u64_try);
            const payload_size: u32 = if (target_is_i64) 8 else if (ll.op == .i8_to_u16_try) 2 else 4;
            const disc_offset: u32 = payload_size;
            if (target_is_i64) {
                // Extend to i64 first
                self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
                const r = try self.emitIntTryResult(.i64, payload_size, disc_offset);
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
                try self.emitIntTryOk(r.result_local, r.val_local, .i64, payload_size, disc_offset);
                self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
            } else {
                const r = try self.emitIntTryResult(.i32, payload_size, disc_offset);
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
                try self.emitIntTryOk(r.result_local, r.val_local, .i32, payload_size, disc_offset);
                self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
            }
        },
        // i64 → narrowing try conversions
        .i64_to_i8_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 1, 1);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, -128) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 127) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 1, 1);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_i16_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 2, 2);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, -32768) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 32767) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 2, 2);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_i32_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 4, 4);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, -2147483648) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 2147483647) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 4, 4);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_u8_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 1, 1);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 255) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 1, 1);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_u16_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 2, 2);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 65535) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 2, 2);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_u32_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 4, 4);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 4294967295) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 4, 4);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_u64_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 8, 8);
            // i64 → u64: check val >= 0
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 8, 8);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        // u64 → narrowing try conversions
        .u64_to_i8_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 1, 1);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 127) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 1, 1);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_i16_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 2, 2);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 32767) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 2, 2);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_i32_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 4, 4);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 2147483647) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 4, 4);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_i64_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 8, 8);
            // u64 → i64: check high bit is 0
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 8, 8);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_u8_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 1, 1);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 255) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 1, 1);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_u16_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 2, 2);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 65535) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 2, 2);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_u32_try => {
            try self.generateExpr(args[0]);
            const r = try self.emitIntTryResult(.i64, 4, 4);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.val_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 4294967295) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 4, 4);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, r.result_local) catch return error.OutOfMemory;
        },
        // 128-bit try conversions: narrowing from i128/u128 to smaller types
        .i128_to_i8_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(1, true, true);
        },
        .i128_to_i16_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(2, true, true);
        },
        .i128_to_i32_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(4, true, true);
        },
        .i128_to_i64_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(8, true, true);
        },
        .i128_to_u8_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(1, true, false);
        },
        .i128_to_u16_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(2, true, false);
        },
        .i128_to_u32_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(4, true, false);
        },
        .i128_to_u64_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(8, true, false);
        },
        .i128_to_u128_try => {
            // i128 → u128: check >= 0 (high word sign bit)
            try self.generateExpr(args[0]);
            try self.emitI128TryToU128(true);
        },
        .u128_to_i8_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(1, false, true);
        },
        .u128_to_i16_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(2, false, true);
        },
        .u128_to_i32_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(4, false, true);
        },
        .u128_to_i64_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(8, false, true);
        },
        .u128_to_i128_try => {
            // u128 → i128: check high bit not set (value < 2^127)
            try self.generateExpr(args[0]);
            try self.emitI128TryToI128();
        },
        .u128_to_u8_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(1, false, false);
        },
        .u128_to_u16_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(2, false, false);
        },
        .u128_to_u32_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(4, false, false);
        },
        .u128_to_u64_try => {
            try self.generateExpr(args[0]);
            try self.emitI128TryNarrow(8, false, false);
        },
        // Widening signed→unsigned try: check >= 0
        .i8_to_u128_try, .i16_to_u128_try, .i64_to_u128_try => {
            try self.generateExpr(args[0]);
            // Source is a small signed int (i32 or i64 on wasm stack)
            // Convert to i128, then check >= 0
            const src_vt = self.exprValType(args[0]);
            if (src_vt == .i32) {
                self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            }
            // Now we have i64 on stack. Convert to i128 first.
            try self.emitIntToI128(true);
            // Now we have an i32 pointer to i128. Check if >= 0.
            try self.emitI128TryToU128(true);
        },

        // Integer widening to i128/u128 (zero/sign-extend to 128 bits in stack memory)
        .u8_to_i128,
        .u8_to_u128,
        .u16_to_i128,
        .u16_to_u128,
        .u32_to_i128,
        .u32_to_u128,
        => {
            // Unsigned i32→i128: zero-extend i32 to i64, then to i128
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            try self.emitIntToI128(false);
        },
        .u64_to_i128,
        .u64_to_u128,
        => {
            // Unsigned i64→i128: value is already i64
            try self.generateExpr(args[0]);
            try self.emitIntToI128(false);
        },
        .i8_to_i128,
        .i16_to_i128,
        .i32_to_i128,
        => {
            // Signed i32→i128: sign-extend i32 to i64, then to i128
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            try self.emitIntToI128(true);
        },
        .i64_to_i128,
        => {
            // Signed i64→i128: value is already i64
            try self.generateExpr(args[0]);
            try self.emitIntToI128(true);
        },
        .i8_to_u128_wrap,
        .i16_to_u128_wrap,
        => {
            // Signed i32→u128 wrap: sign-extend to i64, then i128
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            try self.emitIntToI128(true);
        },
        .i64_to_u128_wrap,
        => {
            // Signed i64→u128 wrap: already i64, sign-extend to i128
            try self.generateExpr(args[0]);
            try self.emitIntToI128(true);
        },
        // i128/u128 truncation to smaller types (load low word, mask)
        .i128_to_i8_wrap,
        .i128_to_u8_wrap,
        .u128_to_i8_wrap,
        .u128_to_u8_wrap,
        => {
            try self.generateExpr(args[0]);
            // Load low i64, wrap to i32, mask to 8 bits
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i128_to_i16_wrap,
        .i128_to_u16_wrap,
        .u128_to_i16_wrap,
        .u128_to_u16_wrap,
        => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFFFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i128_to_i32_wrap,
        .i128_to_u32_wrap,
        .u128_to_i32_wrap,
        .u128_to_u32_wrap,
        => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .i128_to_i64_wrap,
        .i128_to_u64_wrap,
        .u128_to_i64_wrap,
        .u128_to_u64_wrap,
        => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
        },
        .u128_to_i128_wrap,
        .i128_to_u128_wrap,
        => {
            // Same representation — just pass through (pointer stays the same)
            try self.generateExpr(args[0]);
        },
        // i128/u128 → float conversions
        .i128_to_f64 => {
            // Approximate: convert low u64 to f64 + high i64 * 2^64
            try self.generateExpr(args[0]);
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            // high_f64 = (f64)high * 2^64
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 8);
            self.body.append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory; // 2^64
            self.body.append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
            // low_f64 = (f64)(u64)low
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            // result = high_f64 + low_f64
            self.body.append(self.allocator, Op.f64_add) catch return error.OutOfMemory;
        },
        .u128_to_f64 => {
            // Same as i128 but high word is unsigned
            try self.generateExpr(args[0]);
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 8);
            self.body.append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_add) catch return error.OutOfMemory;
        },
        .i128_to_f32 => {
            // Convert via f64 then demote
            try self.generateExpr(args[0]);
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 8);
            self.body.append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_add) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },
        .u128_to_f32 => {
            try self.generateExpr(args[0]);
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 8);
            self.body.append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            self.body.appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_add) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },
        // float → i128/u128 truncating conversions
        .f64_to_i128_trunc => {
            try self.generateExpr(args[0]);
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const result_offset = try self.allocStackMemory(16, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            try self.emitF64ToI128(val, result_local, true);
        },
        .f64_to_u128_trunc => {
            try self.generateExpr(args[0]);
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const result_offset = try self.allocStackMemory(16, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            try self.emitF64ToI128(val, result_local, false);
        },
        .f32_to_i128_trunc => {
            // Promote f32 to f64, then use f64_to_i128 logic
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const result_offset = try self.allocStackMemory(16, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            try self.emitF64ToI128(val, result_local, true);
        },
        .f32_to_u128_trunc => {
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const result_offset = try self.allocStackMemory(16, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            try self.emitF64ToI128(val, result_local, false);
        },
        // 128-bit → Dec conversions: multiply by 10^18, check overflow
        .u128_to_dec_try_unsafe, .i128_to_dec_try_unsafe => {
            const is_signed = ll.op == .i128_to_dec_try_unsafe;
            const import_idx = if (is_signed) self.i128_to_dec_import else self.u128_to_dec_import;

            // Generate the 128-bit value (pointer to 16 bytes in stack memory)
            try self.generateExpr(args[0]);
            const val_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(val_ptr);

            // Allocate result: { dec: i128 (16 bytes), success: bool (1 byte) }
            // Align to 8 for the i128
            const result_offset = try self.allocStackMemory(17, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitLocalSet(result_local);

            // Call host function: (val_ptr, result_ptr) -> i32 (success)
            try self.emitLocalGet(val_ptr);
            try self.emitLocalGet(result_local);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx orelse unreachable) catch return error.OutOfMemory;

            // Store success flag at offset 16
            const success_flag = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(success_flag);
            try self.emitLocalGet(result_local);
            try self.emitLocalGet(success_flag);
            self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, &self.body, 16) catch return error.OutOfMemory; // offset

            // Push result pointer
            try self.emitLocalGet(result_local);
        },

        // Decimal conversions: int → Dec (multiply by 10^18)
        .u8_to_dec, .u16_to_dec, .u32_to_dec => {
            // Unsigned small int → Dec: zero-extend to i64, multiply by 10^18
            try self.generateExpr(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, dec_factor) catch return error.OutOfMemory;
            try self.emitI64MulToI128(val, dec_factor);
        },
        .u64_to_dec => {
            // u64 → Dec: already i64
            try self.generateExpr(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, dec_factor) catch return error.OutOfMemory;
            try self.emitI64MulToI128(val, dec_factor);
        },
        .i8_to_dec, .i16_to_dec, .i32_to_dec => {
            // Signed small int → Dec: sign-extend to i64, multiply by 10^18
            try self.generateExpr(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, dec_factor) catch return error.OutOfMemory;
            try self.emitI64MulToI128(val, dec_factor);
        },
        .i64_to_dec => {
            // i64 → Dec: already i64
            try self.generateExpr(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, dec_factor) catch return error.OutOfMemory;
            try self.emitI64MulToI128(val, dec_factor);
        },

        // Dec → integer truncating conversions (divide by 10^18, truncate)
        .dec_to_i64_trunc => {
            // Dec → i64: load low i64, divide by 10^18
            try self.generateExpr(args[0]);
            // The Dec value is a pointer to 16-byte i128
            // For values that fit in i64, low word / 10^18 gives the result
            // (with sign from high word already encoded in the i128 representation)
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            // Load full i128 as two i64 parts, reconstruct the signed value,
            // then divide. For most Dec values (< 2^63), the low word suffices.
            // We use the simpler approach: load low word, signed divide.
            // This works for Dec values representing integers that fit in i64.
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
        },
        .dec_to_i32_trunc => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .dec_to_i16_trunc, .dec_to_i8_trunc => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            // Mask to target size
            const mask: i32 = if (ll.op == .dec_to_i8_trunc) 0xFF else 0xFFFF;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, mask) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .dec_to_u64_trunc => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
        },
        .dec_to_u32_trunc => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .dec_to_u16_trunc, .dec_to_u8_trunc => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            const mask: i32 = if (ll.op == .dec_to_u8_trunc) 0xFF else 0xFFFF;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, mask) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .dec_to_i128_trunc, .dec_to_u128_trunc => {
            // Dec → i128/u128: divide i128 by 10^18
            try self.generateExpr(args[0]);
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitI128DivByConst(src, 1_000_000_000_000_000_000);
        },
        .dec_to_f64 => {
            // Dec → f64: load i128 as i64 (low word), convert to f64, divide by 10^18.0
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            // 10^18 as f64 bytes (IEEE 754 double for 1e18)
            const dec_f64_bytes = @as([8]u8, @bitCast(@as(f64, 1_000_000_000_000_000_000.0)));
            self.body.appendSlice(self.allocator, &dec_f64_bytes) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_div) catch return error.OutOfMemory;
        },
        .dec_to_f32_wrap => {
            // Dec → f32: same approach as f64, then demote
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            const dec_f64_bytes = @as([8]u8, @bitCast(@as(f64, 1_000_000_000_000_000_000.0)));
            self.body.appendSlice(self.allocator, &dec_f64_bytes) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_div) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },
        // Dec try_unsafe conversions — return {val, is_int, in_range} record
        // Dec is i128 (fixed-point × 10^18). Check if remainder is 0 (is_int),
        // and if integer part fits in target range (in_range).
        .dec_to_i8_try_unsafe,
        .dec_to_i16_try_unsafe,
        .dec_to_i32_try_unsafe,
        .dec_to_i64_try_unsafe,
        .dec_to_u8_try_unsafe,
        .dec_to_u16_try_unsafe,
        .dec_to_u32_try_unsafe,
        .dec_to_u64_try_unsafe,
        => {
            try self.generateExpr(args[0]);
            // Dec value is a pointer to 16-byte i128
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;

            // Load low i64 word
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            const dec_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, dec_low) catch return error.OutOfMemory;

            // is_int = (dec_low % 10^18) == 0
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, dec_low) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_rem_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_eqz) catch return error.OutOfMemory;
            const is_int = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, is_int) catch return error.OutOfMemory;

            // int_val = dec_low / 10^18
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, dec_low) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
            const int_val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, int_val) catch return error.OutOfMemory;

            // Determine target range and value size
            const TryInfo = struct { val_size: u32, is_i64: bool, min_i: i64, max_i: i64 };
            const info: TryInfo = switch (ll.op) {
                .dec_to_i8_try_unsafe => .{ .val_size = 1, .is_i64 = false, .min_i = -128, .max_i = 127 },
                .dec_to_u8_try_unsafe => .{ .val_size = 1, .is_i64 = false, .min_i = 0, .max_i = 255 },
                .dec_to_i16_try_unsafe => .{ .val_size = 2, .is_i64 = false, .min_i = -32768, .max_i = 32767 },
                .dec_to_u16_try_unsafe => .{ .val_size = 2, .is_i64 = false, .min_i = 0, .max_i = 65535 },
                .dec_to_i32_try_unsafe => .{ .val_size = 4, .is_i64 = false, .min_i = -2147483648, .max_i = 2147483647 },
                .dec_to_u32_try_unsafe => .{ .val_size = 4, .is_i64 = false, .min_i = 0, .max_i = 4294967295 },
                .dec_to_i64_try_unsafe => .{ .val_size = 8, .is_i64 = true, .min_i = std.math.minInt(i64), .max_i = std.math.maxInt(i64) },
                .dec_to_u64_try_unsafe => .{ .val_size = 8, .is_i64 = true, .min_i = 0, .max_i = std.math.maxInt(i64) },
                else => unreachable,
            };

            // in_range = int_val >= min && int_val <= max
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, int_val) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, info.min_i) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, int_val) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, info.max_i) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            const in_range = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, in_range) catch return error.OutOfMemory;

            // Allocate result record
            const total_size: u32 = if (info.is_i64) 16 else 8;
            const alignment: u32 = if (info.is_i64) 8 else 4;
            const result_offset = try self.allocStackMemory(total_size, alignment);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

            // Store value
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, int_val) catch return error.OutOfMemory;
            if (info.is_i64) {
                try self.emitStoreOp(.i64, 0);
            } else {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                try self.emitStoreOpSized(.i32, info.val_size, 0);
            }

            // Store is_int
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, is_int) catch return error.OutOfMemory;
            try self.emitStoreOpSized(.i32, 1, info.val_size);

            // Store in_range
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, in_range) catch return error.OutOfMemory;
            try self.emitStoreOpSized(.i32, 1, info.val_size + 1);

            // Push result pointer
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
        },
        // Dec → i128/u128: divide by 10^18
        .dec_to_i128_try_unsafe, .dec_to_u128_try_unsafe => {
            const is_signed = ll.op == .dec_to_i128_try_unsafe;
            const import_idx = if (is_signed) self.dec_to_i128_import else self.dec_to_u128_import;

            // Generate the Dec value (pointer to 16 bytes in stack memory)
            try self.generateExpr(args[0]);
            const val_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(val_ptr);

            // Allocate result: { value: i128/u128 (16 bytes), success: bool (1 byte) }
            const result_offset = try self.allocStackMemory(17, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitLocalSet(result_local);

            // Call host function: (val_ptr, result_ptr) -> i32 (success)
            try self.emitLocalGet(val_ptr);
            try self.emitLocalGet(result_local);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx orelse unreachable) catch return error.OutOfMemory;

            // Store success flag at offset 16
            const success_flag = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(success_flag);
            try self.emitLocalGet(result_local);
            try self.emitLocalGet(success_flag);
            self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, &self.body, 16) catch return error.OutOfMemory; // offset

            // Push result pointer
            try self.emitLocalGet(result_local);
        },
        // Dec → f32: convert Dec to floating point
        .dec_to_f32_try_unsafe => {
            const import_idx = self.dec_to_f32_import orelse unreachable;

            // Generate the Dec value (pointer to 16 bytes in stack memory)
            try self.generateExpr(args[0]);
            const val_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(val_ptr);

            // Allocate result: { value: f32 (4 bytes), is_int: bool (1 byte), in_range: bool (1 byte) }
            // Total 6 bytes, align to 4
            const result_offset = try self.allocStackMemory(8, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitLocalSet(result_local);

            // Call host function: (val_ptr) -> f32
            try self.emitLocalGet(val_ptr);
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            const f32_val = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
            try self.emitLocalSet(f32_val);

            // Store f32 value at offset 0
            try self.emitLocalGet(result_local);
            try self.emitLocalGet(f32_val);
            self.body.append(self.allocator, Op.f32_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // offset

            // Store is_int = 1 at offset 4 (Dec values converted to f32 are always considered valid)
            try self.emitLocalGet(result_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory; // offset

            // Store in_range = 1 at offset 5 (Dec always in f32 range for practical values)
            try self.emitLocalGet(result_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, &self.body, 5) catch return error.OutOfMemory; // offset

            // Push result pointer
            try self.emitLocalGet(result_local);
        },

        // Float try_unsafe conversions — return {val, is_int, in_range} record
        .f32_to_i8_try_unsafe, .f64_to_i8_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_i8_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(1, false, -128.0, 127.0);
        },
        .f32_to_u8_try_unsafe, .f64_to_u8_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_u8_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(1, false, 0.0, 255.0);
        },
        .f32_to_i16_try_unsafe, .f64_to_i16_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_i16_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(2, false, -32768.0, 32767.0);
        },
        .f32_to_u16_try_unsafe, .f64_to_u16_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_u16_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(2, false, 0.0, 65535.0);
        },
        .f32_to_i32_try_unsafe, .f64_to_i32_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_i32_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(4, false, -2147483648.0, 2147483647.0);
        },
        .f32_to_u32_try_unsafe, .f64_to_u32_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_u32_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(4, false, 0.0, 4294967295.0);
        },
        .f32_to_i64_try_unsafe, .f64_to_i64_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_i64_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(8, true, @as(f64, @floatFromInt(@as(i64, std.math.minInt(i64)))), @as(f64, @floatFromInt(@as(i64, std.math.maxInt(i64)))));
        },
        .f32_to_u64_try_unsafe, .f64_to_u64_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_u64_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(8, true, 0.0, @as(f64, @floatFromInt(@as(u64, std.math.maxInt(u64)))));
        },
        // 128-bit float try_unsafe: return {val: i128, is_int: bool, in_range: bool}
        .f32_to_i128_try_unsafe, .f64_to_i128_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_i128_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToI128TryUnsafe(true);
        },
        .f32_to_u128_try_unsafe, .f64_to_u128_try_unsafe => {
            try self.generateExpr(args[0]);
            if (ll.op == .f32_to_u128_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToI128TryUnsafe(false);
        },
        .f64_to_f32_try_unsafe => {
            // Returns {val: F32, success: Bool} — 8 bytes
            try self.generateExpr(args[0]);
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;

            const result_offset = try self.allocStackMemory(8, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;

            // Convert f64 to f32
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
            const f32_val = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, f32_val) catch return error.OutOfMemory;

            // Store f32 at offset 0
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, f32_val) catch return error.OutOfMemory;
            try self.emitStoreOp(.f32, 0);

            // success = !isInf(f32_val) && (!isNaN(val) || isNaN(f32_val))

            // not_inf = abs(f32_val) != inf
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, f32_val) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f32_abs) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
            self.body.appendSlice(self.allocator, &@as([4]u8, @bitCast(std.math.inf(f32)))) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f32_ne) catch return error.OutOfMemory;

            // is_not_nan = (val == val)  (NaN != NaN)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_eq) catch return error.OutOfMemory;

            // is_nan_f32 = (f32_val != f32_val)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, f32_val) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, f32_val) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f32_ne) catch return error.OutOfMemory;

            // is_not_nan OR is_nan_f32
            self.body.append(self.allocator, Op.i32_or) catch return error.OutOfMemory;

            // not_inf AND (is_not_nan OR is_nan_f32)
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

            // Store success at offset 4
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            // swap: need [addr, val] for store
            const success = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, success) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, success) catch return error.OutOfMemory;
            try self.emitStoreOpSized(.i32, 1, 4);

            // Push result pointer
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
        },
    }
}

/// Generate numeric low-level operations (num_add, num_sub, etc.)
/// Handles both scalar and composite (i128/Dec) types.
fn generateNumericLowLevel(self: *Self, op: anytype, args: []const LirExprId, ret_layout: layout.Idx) Allocator.Error!void {
    // For comparison ops, the operand type determines composite-ness, not ret_layout (which is bool)
    const use_operand_layout = switch (op) {
        .num_is_eq, .num_is_gt, .num_is_gte, .num_is_lt, .num_is_lte, .num_abs_diff => true,
        else => false,
    };

    // Check for composite types (i128/Dec)
    const check_layout = if (use_operand_layout) self.exprLayoutIdx(args[0]) else ret_layout;
    if (self.isCompositeExpr(args[0]) or self.isCompositeLayout(check_layout)) {
        return self.generateCompositeNumericOp(op, args, ret_layout, check_layout);
    }

    // For neg, also check composite via ret_layout
    if (op == .num_negate and self.isCompositeLayout(ret_layout)) {
        return self.generateCompositeI128Negate(args[0], ret_layout);
    }

    const vt = if (use_operand_layout) self.exprValType(args[0]) else self.resolveValType(ret_layout);

    switch (op) {
        .num_plus => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_add,
                .i64 => Op.i64_add,
                .f32 => Op.f32_add,
                .f64 => Op.f64_add,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_minus => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_sub,
                .i64 => Op.i64_sub,
                .f32 => Op.f32_sub,
                .f64 => Op.f64_sub,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_times => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_mul,
                .i64 => Op.i64_mul,
                .f32 => Op.f32_mul,
                .f64 => Op.f64_mul,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_div_by => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const is_unsigned = isUnsignedLayout(ret_layout);
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_div_u else Op.i32_div_s,
                .i64 => if (is_unsigned) Op.i64_div_u else Op.i64_div_s,
                .f32 => Op.f32_div,
                .f64 => Op.f64_div,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_div_trunc_by => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const is_unsigned = isUnsignedLayout(ret_layout);
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_div_u else Op.i32_div_s,
                .i64 => if (is_unsigned) Op.i64_div_u else Op.i64_div_s,
                .f32 => Op.f32_div,
                .f64 => Op.f64_div,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_rem_by => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const is_unsigned = isUnsignedLayout(ret_layout);
            switch (vt) {
                .i32 => self.body.append(self.allocator, if (is_unsigned) Op.i32_rem_u else Op.i32_rem_s) catch return error.OutOfMemory,
                .i64 => self.body.append(self.allocator, if (is_unsigned) Op.i64_rem_u else Op.i64_rem_s) catch return error.OutOfMemory,
                .f32, .f64 => try self.emitFloatMod(vt),
            }
        },
        .num_negate => {
            switch (vt) {
                .i32 => {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                },
                .f32 => {
                    try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.f32_neg) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.f64_neg) catch return error.OutOfMemory;
                },
            }
        },
        .num_is_eq => {
            // Check for structural equality (strings, lists, records, etc.)
            const lay_idx = self.exprLayoutIdx(args[0]);
            if (lay_idx == .str or self.isCompositeLayout(lay_idx)) {
                try self.generateStructuralEq(args[0], args[1], false);
                return;
            }
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_eq,
                .i64 => Op.i64_eq,
                .f32 => Op.f32_eq,
                .f64 => Op.f64_eq,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_is_gt => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const is_unsigned = isUnsignedLayout(self.exprLayoutIdx(args[0]));
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_gt_u else Op.i32_gt_s,
                .i64 => if (is_unsigned) Op.i64_gt_u else Op.i64_gt_s,
                .f32 => Op.f32_gt,
                .f64 => Op.f64_gt,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_is_gte => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const is_unsigned = isUnsignedLayout(self.exprLayoutIdx(args[0]));
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_ge_u else Op.i32_ge_s,
                .i64 => if (is_unsigned) Op.i64_ge_u else Op.i64_ge_s,
                .f32 => Op.f32_ge,
                .f64 => Op.f64_ge,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_is_lt => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const is_unsigned = isUnsignedLayout(self.exprLayoutIdx(args[0]));
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_lt_u else Op.i32_lt_s,
                .i64 => if (is_unsigned) Op.i64_lt_u else Op.i64_lt_s,
                .f32 => Op.f32_lt,
                .f64 => Op.f64_lt,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_is_lte => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const is_unsigned = isUnsignedLayout(self.exprLayoutIdx(args[0]));
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_le_u else Op.i32_le_s,
                .i64 => if (is_unsigned) Op.i64_le_u else Op.i64_le_s,
                .f32 => Op.f32_le,
                .f64 => Op.f64_le,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_abs => {
            switch (vt) {
                .f32 => {
                    try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.f32_abs) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
                },
                .i32 => {
                    // abs(x) = select(x, -x, x >= 0)
                    try self.generateExpr(args[0]);
                    const temp = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
                    // Stack: [x]. Compute -x.
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                    // Stack: [x, -x]. Compute condition: x >= 0.
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
                    // select(x, -x, x >= 0) — returns x if true, -x if false
                    self.body.append(self.allocator, Op.select) catch return error.OutOfMemory;
                },
                .i64 => {
                    try self.generateExpr(args[0]);
                    const temp = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, temp) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.select) catch return error.OutOfMemory;
                },
            }
        },
        .num_mod_by => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            switch (vt) {
                .i32 => {
                    const import_idx = self.i32_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .i64 => {
                    const import_idx = self.i64_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .f32, .f64 => try self.emitFloatMod(vt),
            }
        },
        .num_abs_diff => {
            const is_unsigned = isUnsignedLayout(self.exprLayoutIdx(args[0]));
            switch (vt) {
                .i32 => {
                    try self.generateExpr(args[0]);
                    const lhs = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    try self.generateExpr(args[1]);
                    const rhs = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs) catch return error.OutOfMemory;

                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, if (is_unsigned) Op.i32_ge_u else Op.i32_ge_s) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
                },
                .i64 => {
                    try self.generateExpr(args[0]);
                    const lhs = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    try self.generateExpr(args[1]);
                    const rhs = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs) catch return error.OutOfMemory;

                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, if (is_unsigned) Op.i64_ge_u else Op.i64_ge_s) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.i64)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
                },
                .f32 => {
                    try self.generateExpr(args[0]);
                    try self.generateExpr(args[1]);
                    self.body.append(self.allocator, Op.f32_sub) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f32_abs) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.generateExpr(args[0]);
                    try self.generateExpr(args[1]);
                    self.body.append(self.allocator, Op.f64_sub) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
                },
            }
        },
        .num_shift_left_by => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_shl,
                .i64 => Op.i64_shl,
                .f32, .f64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_shift_right_by => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_shr_s,
                .i64 => Op.i64_shr_s,
                .f32, .f64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_shift_right_zf_by => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_shr_u,
                .i64 => Op.i64_shr_u,
                .f32, .f64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        else => unreachable,
    }
}

/// Generate string equality comparison using roc_str_eq host function.
/// Both lhs and rhs should produce i32 pointers to 12-byte RocStr values.
fn generateStrEq(self: *Self, lhs: LirExprId, rhs: LirExprId, negate: bool) Allocator.Error!void {
    const import_idx = self.str_eq_import orelse unreachable;

    // Generate both string expressions, store to locals
    try self.generateExpr(lhs);
    const lhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_local);

    try self.generateExpr(rhs);
    const rhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(rhs_local);

    // Call roc_str_eq(lhs_ptr, rhs_ptr) -> i32
    try self.emitLocalGet(lhs_local);
    try self.emitLocalGet(rhs_local);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;

    // If negate, flip the result
    if (negate) {
        self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    }
}

/// Generate list equality comparison using roc_list_eq host function.
/// Both lhs and rhs should produce i32 pointers to 12-byte RocList values.
fn generateListEq(self: *Self, lhs: LirExprId, rhs: LirExprId, list_layout_idx: layout.Idx, negate: bool) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const list_layout = ls.getLayout(list_layout_idx);
    std.debug.assert(list_layout.tag == .list);
    const elem_layout = list_layout.data.list;
    try self.generateListEqWithElemLayout(lhs, rhs, elem_layout, negate);
}

/// Generate list equality with a known element layout.
/// Supports all element types including strings and nested lists.
fn generateListEqWithElemLayout(self: *Self, lhs: LirExprId, rhs: LirExprId, elem_layout: layout.Idx, negate: bool) Allocator.Error!void {
    // Generate both list expressions, store to locals
    try self.generateExpr(lhs);
    const lhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_local);

    try self.generateExpr(rhs);
    const rhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(rhs_local);

    // Determine which comparison to use based on element type
    if (elem_layout == .str) {
        // List of strings - use specialized host function
        const import_idx = self.list_str_eq_import orelse unreachable;
        try self.emitLocalGet(lhs_local);
        try self.emitLocalGet(rhs_local);
        self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
    } else {
        const ls = self.getLayoutStore();
        const elem_l = ls.getLayout(elem_layout);
        if (elem_l.tag == .list) {
            // List of lists - use specialized host function with inner element size
            const inner_elem_layout = elem_l.data.list;
            const inner_elem_size = self.layoutByteSize(inner_elem_layout);
            const import_idx = self.list_list_eq_import orelse unreachable;
            try self.emitLocalGet(lhs_local);
            try self.emitLocalGet(rhs_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(inner_elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
        } else if (ls.layoutContainsRefcounted(elem_l)) {
            // Composite elements with refcounted fields: inline structural loop
            const elem_size = self.layoutByteSize(elem_layout);
            try self.emitListEqLoop(lhs_local, rhs_local, elem_layout, elem_size);
        } else {
            // Simple scalar elements - byte-wise comparison
            const import_idx = self.list_eq_import orelse unreachable;
            const elem_size = self.layoutByteSize(elem_layout);
            try self.emitLocalGet(lhs_local);
            try self.emitLocalGet(rhs_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
        }
    }

    // If negate, flip the result
    if (negate) {
        self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    }
}

/// Generate a RocStr for a string literal.
/// On wasm32, RocStr is 12 bytes: { ptr/bytes[0..3], len/bytes[4..7], cap/bytes[8..11] }.
/// Small strings (≤11 bytes) use SSO: bytes inline, byte 11 = len | 0x80.
/// Large strings (>11 bytes) use a data segment in linear memory.
fn generateStrLiteral(self: *Self, str_idx: anytype) Allocator.Error!void {
    const str_bytes = self.store.getString(str_idx);
    const len = str_bytes.len;

    // Allocate 12 bytes on stack frame for the RocStr struct
    const base_offset = try self.allocStackMemory(12, 4);
    const base_local = self.fp_local;

    if (len <= 11) {
        // Small string optimization (SSO)
        // Store string bytes inline in the 12-byte struct
        // First, zero out the 12 bytes (3 × i32.store)
        for (0..3) |i| {
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, base_offset + @as(u32, @intCast(i)) * 4);
        }

        // Store string bytes one at a time
        for (str_bytes, 0..) |byte, i| {
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(byte)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
            // alignment = 0 (byte-aligned)
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            // offset
            WasmModule.leb128WriteU32(self.allocator, &self.body, base_offset + @as(u32, @intCast(i))) catch return error.OutOfMemory;
        }

        // Store SSO marker: byte 11 = len | 0x80
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @as(i32, @intCast(len)) | @as(i32, 0x80)) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_offset + 11) catch return error.OutOfMemory; // offset
    } else {
        // Large string — place data in a data segment with static RC header.
        // Runtime RC ops read/write at data_ptr - 4, so static literals must reserve
        // those 4 bytes and initialize them to 0 (immortal static marker).
        var segment_data = std.ArrayList(u8).empty;
        defer segment_data.deinit(self.allocator);
        try segment_data.appendNTimes(self.allocator, 0, 4);
        try segment_data.appendSlice(self.allocator, str_bytes);
        const segment_offset = self.module.addDataSegment(segment_data.items, 4) catch return error.OutOfMemory;
        const data_offset = segment_offset + 4;

        // Store ptr (offset 0)
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(data_offset)) catch return error.OutOfMemory;
        try self.emitStoreOp(.i32, base_offset);

        // Store len (offset 4)
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(len)) catch return error.OutOfMemory;
        try self.emitStoreOp(.i32, base_offset + 4);

        // Store capacity (offset 8) — same as len for constants
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(len)) catch return error.OutOfMemory;
        try self.emitStoreOp(.i32, base_offset + 8);
    }

    // Push pointer to the RocStr on the stack
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    if (base_offset > 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(base_offset)) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    }
}

/// Generate code for str_concat: concatenate multiple RocStr values into one.
/// Each sub-expression produces a RocStr pointer (12 bytes: ptr/bytes, len/bytes, cap/bytes).
fn generateStrConcat(self: *Self, span: anytype) Allocator.Error!void {
    const expr_ids = self.store.getExprSpan(span);
    const import_idx = self.str_concat_import orelse unreachable;

    if (expr_ids.len == 0) {
        try self.generateEmptyStr();
        return;
    }
    if (expr_ids.len == 1) {
        try self.generateExpr(expr_ids[0]);
        return;
    }

    try self.generateExpr(expr_ids[0]);
    const current = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(current);

    for (expr_ids[1..]) |expr_id| {
        try self.generateExpr(expr_id);
        const rhs = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(rhs);
        const result_offset = try self.allocStackMemory(12, 4);
        try self.emitLocalGet(current);
        try self.emitLocalGet(rhs);
        try self.emitFpOffset(result_offset);
        self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
        try self.emitFpOffset(result_offset);
        try self.emitLocalSet(current);
    }

    try self.emitLocalGet(current);
}

/// Generate an empty SSO RocStr (12 bytes, all zeros except byte 11 = 0x80).
fn generateEmptyStr(self: *Self) Allocator.Error!void {
    const base_offset = try self.allocStackMemory(12, 4);
    const base_local = self.fp_local;

    // Zero out 12 bytes (3 x i32.store of 0)
    for (0..3) |i| {
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        try self.emitStoreOp(.i32, base_offset + @as(u32, @intCast(i)) * 4);
    }

    // Set byte 11 = 0x80 (SSO marker, length 0)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0x80) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_offset + 11) catch return error.OutOfMemory; // offset

    // Push pointer to the result
    try self.emitFpOffset(base_offset);
}

/// Extract the byte pointer and length from a RocStr.
/// Handles both SSO (small string optimization) and heap-allocated strings.
/// Emits: if SSO { ptr=str_local, len=byte11&0x7F } else { ptr=*(str+0), len=*(str+4) }
fn emitExtractStrPtrLen(self: *Self, str_local: u32, ptr_local: u32, len_local: u32) Allocator.Error!void {
    // Load byte 11 to check SSO bit
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, str_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
    WasmModule.leb128WriteU32(self.allocator, &self.body, 11) catch return error.OutOfMemory; // offset = 11

    const sso_marker = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, sso_marker) catch return error.OutOfMemory;

    // Check if SSO: bit 7 set
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0x80) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

    // if (is_sso)
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // SSO path: len = sso_marker & 0x7F, ptr = str_local (bytes inline)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, sso_marker) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0x7F) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, str_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;

    // else — heap path
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;

    // len = *(str_local + 4)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, str_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory; // align = 2
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory; // offset = 4
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;

    // ptr = *(str_local + 0)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, str_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory; // align = 2
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // offset = 0
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;

    // end if
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Emit a byte-by-byte copy loop: memcpy(dst_base + dst_offset, src_ptr, len).
/// Uses a wasm block+loop construct with a counter local.
fn emitMemCopyLoop(self: *Self, dst_base_local: u32, dst_offset_local: u32, src_ptr_local: u32, len_local: u32) Allocator.Error!void {
    const loop_i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // loop_i = 0
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, loop_i) catch return error.OutOfMemory;

    // block (void)
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // loop (void)
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // if loop_i >= len, break
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, loop_i) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory; // break out of block

    // dst address = dst_base + dst_offset + loop_i
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, dst_base_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, dst_offset_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, loop_i) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

    // src value = *(src_ptr + loop_i)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_ptr_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, loop_i) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align = 0
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // offset = 0

    // store byte
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align = 0
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // offset = 0

    // loop_i++
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, loop_i) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, loop_i) catch return error.OutOfMemory;

    // br back to loop
    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // continue loop

    // end loop
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    // end block
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Build a heap-format RocStr on the stack frame from ptr and len locals.
/// Leaves a pointer to the 12-byte RocStr on the wasm value stack.
fn buildHeapRocStr(self: *Self, ptr_local: u32, len_local: u32) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(12, 4);
    const base_local = self.fp_local;

    // Store ptr (offset 0)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, result_offset);

    // Store len (offset 4)
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, result_offset + 4);

    // Store cap (offset 8) = len
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, result_offset + 8);

    // Push pointer to result
    try self.emitFpOffset(result_offset);
}

/// Generate str_to_utf8: convert RocStr to RocList(U8).
/// SSO strings have their bytes copied to heap memory.
/// Non-SSO strings share the same layout, so the 12 bytes are copied directly.
fn generateStrToUtf8(self: *Self, str_arg: LirExprId) Allocator.Error!void {
    // Generate the string expression (produces i32 pointer to 12-byte RocStr)
    try self.generateExpr(str_arg);
    const str_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(str_ptr);

    // Allocate result memory (12 bytes for RocList(U8))
    const result_offset = try self.allocStackMemory(12, 4);
    const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_ptr);

    // Read byte 11 to check SSO flag
    try self.emitLocalGet(str_ptr);
    self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
    WasmModule.leb128WriteU32(self.allocator, &self.body, 11) catch return error.OutOfMemory; // offset
    const last_byte = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(last_byte);

    // Check SSO flag: last_byte & 0x80
    try self.emitLocalGet(last_byte);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0x80) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

    // if (is_sso)
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    {
        // SSO case: extract len = last_byte & 0x7F
        try self.emitLocalGet(last_byte);
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0x7F) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        const sso_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(sso_len);

        // Allocate sso_len bytes on heap via roc_alloc
        try self.emitHeapAlloc(sso_len, 1);
        const heap_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(heap_ptr);

        // Copy SSO bytes from str_ptr to heap: memcpy(heap_ptr+0, str_ptr, sso_len)
        const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        try self.emitLocalSet(zero);
        try self.emitMemCopyLoop(heap_ptr, zero, str_ptr, sso_len);

        // Write RocList {heap_ptr, sso_len, sso_len} to result_ptr
        // ptr (offset 0)
        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(heap_ptr);
        try self.emitStoreOp(.i32, 0);
        // len (offset 4)
        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(sso_len);
        try self.emitStoreOp(.i32, 4);
        // cap (offset 8)
        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(sso_len);
        try self.emitStoreOp(.i32, 8);
    }
    // else (non-SSO)
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    {
        // Non-SSO: RocStr {ptr, len, cap} has same layout as RocList(U8)
        // Copy 12 bytes from str_ptr to result_ptr
        try self.emitMemCopy(result_ptr, 0, str_ptr, 12);
    }
    // end if
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Leave result pointer on stack
    try self.emitLocalGet(result_ptr);
}

/// Generate str_from_utf8_lossy: convert RocList(U8) to RocStr.
/// Short lists (len <= 11) produce SSO strings.
/// Longer lists share the same layout, so the 12 bytes are copied directly.
fn generateStrFromUtf8Lossy(self: *Self, list_arg: LirExprId) Allocator.Error!void {
    // Generate the list expression (produces i32 pointer to 12-byte RocList)
    try self.generateExpr(list_arg);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    // Allocate result memory (12 bytes for RocStr)
    const result_offset = try self.allocStackMemory(12, 4);
    const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_ptr);

    // Read len from list struct (offset 4)
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory; // align = 4-byte
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory; // offset
    const len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(len);

    // Check if len <= 11 (fits in SSO)
    try self.emitLocalGet(len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 12) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_lt_u) catch return error.OutOfMemory;

    // if (len < 12) — SSO
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    {
        // Zero-initialize the 12-byte result (so unused SSO bytes are 0)
        try self.emitZeroInit(result_ptr, 12);

        // Read data_ptr from list struct (offset 0)
        try self.emitLocalGet(list_ptr);
        self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // offset
        const data_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(data_ptr);

        // Copy len bytes from data_ptr to result_ptr: memcpy(result_ptr+0, data_ptr, len)
        const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        try self.emitLocalSet(zero);
        try self.emitMemCopyLoop(result_ptr, zero, data_ptr, len);

        // Set byte 11 = len | 0x80 (SSO marker)
        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(len);
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0x80) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, &self.body, 11) catch return error.OutOfMemory; // offset
    }
    // else (non-SSO)
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    {
        // Non-SSO: RocList(U8) {ptr, len, cap} has same layout as RocStr
        // Copy 12 bytes from list_ptr to result_ptr
        try self.emitMemCopy(result_ptr, 0, list_ptr, 12);
    }
    // end if
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Leave result pointer on stack
    try self.emitLocalGet(result_ptr);
}

/// Generate int_to_str: convert an integer value to its decimal string representation.
/// Supports all integer types including i128/u128.
fn generateIntToStr(self: *Self, its: anytype) Allocator.Error!void {
    const precision = its.int_precision;

    // i128/u128 use host function (no native 128-bit division in wasm)
    if (precision == .i128 or precision == .u128) {
        return self.generateI128ToStr(its.value, precision == .i128);
    }

    const is_signed = switch (precision) {
        .i8, .i16, .i32, .i64 => true,
        else => false,
    };
    const is_64bit = switch (precision) {
        .i64, .u64 => true,
        else => false,
    };
    const val_type: ValType = if (is_64bit) .i64 else .i32;

    // Generate value expression
    try self.generateExpr(its.value);
    const value_local = self.storage.allocAnonymousLocal(val_type) catch return error.OutOfMemory;
    try self.emitLocalSet(value_local);

    // For signed: check negative, negate if needed
    const is_neg_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    if (is_signed) {
        // is_neg = value < 0
        try self.emitLocalGet(value_local);
        if (is_64bit) {
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_lt_s) catch return error.OutOfMemory;
        } else {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_lt_s) catch return error.OutOfMemory;
        }
        try self.emitLocalSet(is_neg_local);

        // if negative: value = 0 - value
        try self.emitLocalGet(is_neg_local);
        self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        if (is_64bit) {
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalGet(value_local);
            self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
        } else {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalGet(value_local);
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
        }
        try self.emitLocalSet(value_local);
        self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    } else {
        // Not signed: is_neg = 0
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        try self.emitLocalSet(is_neg_local);
    }

    // Allocate 21-byte buffer on heap for digits (max: "-9223372036854775808" = 20 chars)
    try self.emitHeapAllocConst(21, 1);
    const buf_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(buf_local);

    // pos = 20 (write position, rightmost)
    const pos_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 20) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);

    // Do-while digit extraction loop
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // digit = value % 10
    try self.emitLocalGet(value_local);
    if (is_64bit) {
        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, &self.body, 10) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_rem_u) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
    } else {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 10) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_rem_u) catch return error.OutOfMemory;
    }
    const digit_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(digit_local);

    // value = value / 10
    try self.emitLocalGet(value_local);
    if (is_64bit) {
        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, &self.body, 10) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_div_u) catch return error.OutOfMemory;
    } else {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 10) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_div_u) catch return error.OutOfMemory;
    }
    try self.emitLocalSet(value_local);

    // buffer[pos] = digit + '0'
    try self.emitLocalGet(buf_local);
    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalGet(digit_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '0') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // offset

    // pos--
    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);

    // if value > 0: continue loop
    try self.emitLocalGet(value_local);
    if (is_64bit) {
        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_gt_u) catch return error.OutOfMemory;
    } else {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_gt_u) catch return error.OutOfMemory;
    }
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // end loop
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // If negative, prepend '-'
    if (is_signed) {
        try self.emitLocalGet(is_neg_local);
        self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        // buffer[pos] = '-'
        try self.emitLocalGet(buf_local);
        try self.emitLocalGet(pos_local);
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, '-') catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

        // pos--
        try self.emitLocalGet(pos_local);
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
        try self.emitLocalSet(pos_local);

        self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    // String starts at buf + pos + 1, length = 20 - pos
    const str_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const str_len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // str_ptr = buf + pos + 1
    try self.emitLocalGet(buf_local);
    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(str_ptr_local);

    // str_len = 20 - pos
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 20) catch return error.OutOfMemory;
    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(str_len_local);

    // Build heap RocStr
    try self.buildHeapRocStr(str_ptr_local, str_len_local);
}

/// Generate float_to_str: convert a float to its string representation.
/// Uses the same host-side formatter as the native backends to keep output stable.
fn generateFloatToStr(self: *Self, fts: anytype) Allocator.Error!void {
    const import_idx = self.float_to_str_import orelse unreachable;
    const is_f32 = fts.float_precision == .f32;

    std.debug.assert(fts.float_precision != .dec);

    try self.generateExpr(fts.value);
    if (is_f32) {
        self.body.append(self.allocator, Op.i32_reinterpret_f32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    } else {
        self.body.append(self.allocator, Op.i64_reinterpret_f64) catch return error.OutOfMemory;
    }

    const val_bits = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(val_bits);

    try self.emitHeapAllocConst(48, 1);
    const buf_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(buf_ptr);

    try self.emitLocalGet(val_bits);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intFromBool(is_f32)) catch return error.OutOfMemory;
    try self.emitLocalGet(buf_ptr);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;

    const str_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(str_len);
    try self.buildHeapRocStr(buf_ptr, str_len);
}

/// Generate i128/u128 to string: convert a 128-bit integer to its decimal string representation.
/// Uses a host function import since wasm has no native 128-bit division.
fn generateI128ToStr(self: *Self, value_expr: anytype, is_signed: bool) Allocator.Error!void {
    const import_idx = if (is_signed)
        self.i128_to_str_import orelse unreachable
    else
        self.u128_to_str_import orelse unreachable;

    // Generate the 128-bit value expression → pointer to 16-byte value in stack memory
    try self.generateExpr(value_expr);
    const val_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(val_ptr);

    // Allocate a 48-byte buffer on the heap for the formatted string
    // (max i128 string length is 40 bytes: 39 digits + sign)
    try self.emitHeapAllocConst(48, 1);
    const buf_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(buf_ptr);

    // Call roc_i128_to_str(val_ptr, buf_ptr) -> str_len
    try self.emitLocalGet(val_ptr);
    try self.emitLocalGet(buf_ptr);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;

    // Result (str_len) is on the stack
    const str_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(str_len);

    // Build a heap RocStr from buf_ptr and str_len
    try self.buildHeapRocStr(buf_ptr, str_len);
}

/// Generate dec_to_str: convert a RocDec (i128 scaled by 10^18) to string.
/// Uses a host function import to perform the formatting.
fn generateDecToStr(self: *Self, dec_expr: anytype) Allocator.Error!void {
    const import_idx = self.dec_to_str_import orelse unreachable;

    // Generate the Dec expression → pointer to 16-byte Dec value in stack memory
    try self.generateExpr(dec_expr);
    const dec_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(dec_ptr);

    // Allocate a 48-byte buffer on the heap for the formatted string
    // (max Dec string length is 41 bytes: 39 digits + sign + decimal point)
    try self.emitHeapAllocConst(48, 1);
    const buf_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(buf_ptr);

    // Call roc_dec_to_str(dec_ptr, buf_ptr) -> str_len
    try self.emitLocalGet(dec_ptr);
    try self.emitLocalGet(buf_ptr);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;

    // Result (str_len) is on the stack
    const str_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(str_len);

    // Build a heap RocStr from buf_ptr and str_len
    try self.buildHeapRocStr(buf_ptr, str_len);
}

/// Generate str_escape_and_quote: surround string with quotes and escape special chars.
/// For the common case of strings with no special chars, this just prepends and appends '"'.
fn generateStrEscapeAndQuote(self: *Self, quote_expr: anytype) Allocator.Error!void {
    // Generate the inner string expression
    try self.generateExpr(quote_expr);
    const inner_str = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(inner_str);

    // Extract ptr and len from inner string
    const inner_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const inner_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitExtractStrPtrLen(inner_str, inner_ptr, inner_len);

    // Allocate buffer: len + 2 (for surrounding quotes) + some slack for escapes
    // For simplicity, allocate 2 * len + 2 (worst case: every char needs escaping)
    const buf_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(inner_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(buf_size);

    try self.emitHeapAlloc(buf_size, 1);
    const buf_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(buf_local);

    const out_pos = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // Write opening '"'
    try self.emitLocalGet(buf_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '"') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    try self.emitLocalSet(out_pos);

    // Copy inner bytes with escaping: " -> \", \ -> \\, \n -> \\n, \r -> \\r, \t -> \\t
    // Loop over each byte, check if it needs escaping, write accordingly.
    const src_idx = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(src_idx);

    // block { loop {
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.void)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.void)) catch return error.OutOfMemory;

    // if (src_idx >= inner_len) br 1 (exit block)
    try self.emitLocalGet(src_idx);
    try self.emitLocalGet(inner_len);
    self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    // Load current byte: byte = mem[inner_ptr + src_idx]
    const cur_byte = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(inner_ptr);
    try self.emitLocalGet(src_idx);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(cur_byte);

    // Check if byte needs escaping: " (34), \ (92), \n (10), \r (13), \t (9)
    // if (byte == '"' || byte == '\\')
    try self.emitLocalGet(cur_byte);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '"') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
    try self.emitLocalGet(cur_byte);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '\\') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_or) catch return error.OutOfMemory;

    // if (needs_escape) { write '\' + byte } else { write byte }
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.void)) catch return error.OutOfMemory;

    // Then: write '\\' at buf[out_pos]
    try self.emitLocalGet(buf_local);
    try self.emitLocalGet(out_pos);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '\\') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    // out_pos++
    try self.emitLocalGet(out_pos);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(out_pos);
    // Then write the original byte
    try self.emitLocalGet(buf_local);
    try self.emitLocalGet(out_pos);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalGet(cur_byte);
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    // out_pos++
    try self.emitLocalGet(out_pos);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(out_pos);

    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;

    // Else: write byte directly
    try self.emitLocalGet(buf_local);
    try self.emitLocalGet(out_pos);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalGet(cur_byte);
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    // out_pos++
    try self.emitLocalGet(out_pos);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(out_pos);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end if

    // src_idx++
    try self.emitLocalGet(src_idx);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(src_idx);

    // br 0 (continue loop)
    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end loop
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end block

    // Write closing '"'
    try self.emitLocalGet(buf_local);
    try self.emitLocalGet(out_pos);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '"') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // out_pos++
    try self.emitLocalGet(out_pos);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(out_pos);

    // Build RocStr
    try self.buildHeapRocStr(buf_local, out_pos);
}

/// Helper: emit local.get instruction
fn emitLocalGet(self: *Self, local: u32) Allocator.Error!void {
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, local) catch return error.OutOfMemory;
}

/// Helper: emit local.set instruction
fn emitLocalSet(self: *Self, local: u32) Allocator.Error!void {
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, local) catch return error.OutOfMemory;
}

/// Emit float modulo: a % b = a - trunc(a / b) * b
/// Expects a and b already on the wasm value stack.
fn emitFloatMod(self: *Self, vt: ValType) Allocator.Error!void {
    const div_op: u8 = if (vt == .f32) Op.f32_div else Op.f64_div;
    const trunc_op: u8 = if (vt == .f32) Op.f32_trunc else Op.f64_trunc;
    const mul_op: u8 = if (vt == .f32) Op.f32_mul else Op.f64_mul;
    const sub_op: u8 = if (vt == .f32) Op.f32_sub else Op.f64_sub;

    const a = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
    const b = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
    try self.emitLocalSet(b);
    try self.emitLocalSet(a);
    // a
    try self.emitLocalGet(a);
    // trunc(a / b)
    try self.emitLocalGet(a);
    try self.emitLocalGet(b);
    self.body.append(self.allocator, div_op) catch return error.OutOfMemory;
    self.body.append(self.allocator, trunc_op) catch return error.OutOfMemory;
    // * b
    try self.emitLocalGet(b);
    self.body.append(self.allocator, mul_op) catch return error.OutOfMemory;
    // a - ...
    self.body.append(self.allocator, sub_op) catch return error.OutOfMemory;
}

/// Get the element size for a list layout.
fn getListElemSize(self: *const Self, list_layout: layout.Idx) u32 {
    const ls = self.getLayoutStore();
    const info = ls.getListInfo(ls.getLayout(list_layout));
    return self.layoutStorageByteSize(info.elem_layout_idx);
}

/// Get the element alignment for a list layout.
fn getListElemAlign(self: *const Self, list_layout: layout.Idx) u32 {
    const ls = self.getLayoutStore();
    const info = ls.getListInfo(ls.getLayout(list_layout));
    return self.layoutStorageByteAlign(info.elem_layout_idx);
}

fn listContainsRefcounted(self: *const Self, list_layout: layout.Idx) bool {
    const ls = self.getLayoutStore();
    return ls.getListInfo(ls.getLayout(list_layout)).contains_refcounted;
}

const StrSearchMode = enum { contains, starts_with, ends_with };

/// Generate LowLevel str_contains / str_starts_with / str_ends_with.
/// Compares bytes using a nested loop (naive O(n*m) search).
fn generateLLStrSearch(self: *Self, args: anytype, mode: StrSearchMode) Allocator.Error!void {
    // Generate both string args
    try self.generateExpr(args[0]);
    const a_str = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(a_str);
    try self.generateExpr(args[1]);
    const b_str = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(b_str);

    // Extract ptr+len from each
    const a_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const a_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitExtractStrPtrLen(a_str, a_ptr, a_len);
    const b_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const b_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitExtractStrPtrLen(b_str, b_ptr, b_len);

    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    switch (mode) {
        .starts_with => {
            // starts_with: compare first b_len bytes of a with b
            // If a_len < b_len, return false
            // Otherwise, compare b_len bytes byte-by-byte
            try self.emitStrPrefixCompare(a_ptr, a_len, b_ptr, b_len, result_local);
        },
        .ends_with => {
            // ends_with: compare last b_len bytes of a with b
            // offset = a_len - b_len
            // If a_len < b_len, return false
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalSet(result_local);

            // if a_len >= b_len
            try self.emitLocalGet(a_len);
            try self.emitLocalGet(b_len);
            self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

            // Compare b_len bytes starting at a_ptr + (a_len - b_len) vs b_ptr
            const offset_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(a_len);
            try self.emitLocalGet(b_len);
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            try self.emitLocalSet(offset_local);

            const a_end_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(a_ptr);
            try self.emitLocalGet(offset_local);
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(a_end_ptr);

            try self.emitBytewiseCompare(a_end_ptr, b_ptr, b_len, result_local);

            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .contains => {
            // contains: search for b as a substring of a
            // Naive O(n*m): for each position i in [0..a_len-b_len], compare b_len bytes
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalSet(result_local);

            // if b_len == 0, result = true (empty string is always contained)
            try self.emitLocalGet(b_len);
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            try self.emitLocalSet(result_local);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;

            // if a_len >= b_len, search
            try self.emitLocalGet(a_len);
            try self.emitLocalGet(b_len);
            self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

            // search_end = a_len - b_len + 1
            const search_end = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(a_len);
            try self.emitLocalGet(b_len);
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(search_end);

            const search_i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalSet(search_i);

            // block { loop {
            self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

            // if search_i >= search_end: break
            try self.emitLocalGet(search_i);
            try self.emitLocalGet(search_end);
            self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

            // Compare b_len bytes at a_ptr+search_i vs b_ptr
            const cand_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(a_ptr);
            try self.emitLocalGet(search_i);
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(cand_ptr);

            const match_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitBytewiseCompare(cand_ptr, b_ptr, b_len, match_local);

            // if match: result = 1, break
            try self.emitLocalGet(match_local);
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            try self.emitLocalSet(result_local);
            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 3) catch return error.OutOfMemory; // break out of block (past loop + block)
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

            // search_i++
            try self.emitLocalGet(search_i);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(search_i);

            // br loop
            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end loop
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end block

            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end if a_len >= b_len
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end if b_len == 0
        },
    }

    // Push result
    try self.emitLocalGet(result_local);
}

/// Compare a_ptr[0..b_len] with b_ptr[0..b_len], assuming a is long enough.
/// Sets result_local to 1 if equal, 0 otherwise.
fn emitStrPrefixCompare(self: *Self, a_ptr: u32, a_len: u32, b_ptr: u32, b_len: u32, result_local: u32) Allocator.Error!void {
    // Default result = 0
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(result_local);

    // if a_len >= b_len
    try self.emitLocalGet(a_len);
    try self.emitLocalGet(b_len);
    self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitBytewiseCompare(a_ptr, b_ptr, b_len, result_local);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Compare len bytes at ptr_a vs ptr_b, store result (1=equal, 0=not) in result_local.
fn emitBytewiseCompare(self: *Self, ptr_a: u32, ptr_b: u32, len: u32, result_local: u32) Allocator.Error!void {
    // Assume equal
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    try self.emitLocalSet(result_local);

    const cmp_i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(cmp_i);

    // block { loop {
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // if cmp_i >= len: break (all bytes matched)
    try self.emitLocalGet(cmp_i);
    try self.emitLocalGet(len);
    self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    // Load byte from a[cmp_i]
    try self.emitLocalGet(ptr_a);
    try self.emitLocalGet(cmp_i);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // Load byte from b[cmp_i]
    try self.emitLocalGet(ptr_b);
    try self.emitLocalGet(cmp_i);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // If not equal: result = 0, break
    self.body.append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(result_local);
    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 3) catch return error.OutOfMemory; // break out of block (skip if + loop + block)
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // cmp_i++
    try self.emitLocalGet(cmp_i);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(cmp_i);

    // continue loop
    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end loop
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end block
}

/// Generate LowLevel list_append: create new list with one element appended.
fn generateLLListAppend(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    const elem_align = self.getListElemAlign(ret_layout);
    const elem_layout_idx = switch (self.getLayoutStore().getLayout(ret_layout).tag) {
        .list => self.getLayoutStore().getLayout(ret_layout).data.list,
        .list_of_zst => layout.Idx.zst,
        else => unreachable,
    };
    const import_idx = self.list_append_unsafe_import orelse unreachable;

    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    const elem_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    if (elem_size == 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        try self.emitLocalSet(elem_ptr);
    } else {
        const target_is_composite = self.isCompositeLayout(elem_layout_idx);
        if (target_is_composite) {
            try self.generateExpr(args[1]);

            if (self.exprNeedsCompositeCallStabilization(args[1])) {
                const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(src_local);
                const dst_offset = try self.allocStackMemory(elem_size, elem_align);
                try self.emitFpOffset(dst_offset);
                try self.emitLocalSet(elem_ptr);
                try self.emitMemCopy(elem_ptr, 0, src_local, elem_size);
            } else {
                try self.emitLocalSet(elem_ptr);
            }
        } else {
            const elem_vt = self.resolveValType(elem_layout_idx);
            try self.generateExpr(args[1]);
            try self.emitConversion(self.exprValType(args[1]), elem_vt);
            const elem_val = self.storage.allocAnonymousLocal(elem_vt) catch return error.OutOfMemory;
            try self.emitLocalSet(elem_val);

            const elem_offset = try self.allocStackMemory(elem_size, elem_align);
            try self.emitFpOffset(elem_offset);
            try self.emitLocalSet(elem_ptr);

            try self.emitLocalGet(elem_ptr);
            try self.emitLocalGet(elem_val);
            try self.emitStoreOpSized(elem_vt, elem_size, 0);
        }
    }

    const result_offset = try self.allocStackMemory(12, 4);
    try self.emitLocalGet(list_ptr);
    try self.emitLocalGet(elem_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_align)) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
}

/// Generate LowLevel list_prepend: create new list with one element prepended.
fn generateLLListPrepend(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    const elem_align = self.getListElemAlign(ret_layout);

    // Generate list and element
    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);
    try self.generateExpr(args[1]);
    const prepend_elem_vt: ValType = if (elem_size <= 4) .i32 else if (elem_size <= 8) .i64 else .i32;
    const elem_val = self.storage.allocAnonymousLocal(prepend_elem_vt) catch return error.OutOfMemory;
    try self.emitLocalSet(elem_val);

    // Load list length
    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    const old_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const new_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(new_len);

    // Allocate new buffer
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(new_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAlloc(total_size, elem_align);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // Store new element at position 0
    if (elem_size <= 8) {
        try self.emitLocalGet(new_data);
        try self.emitLocalGet(elem_val);
        if (elem_size <= 4) {
            self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
        } else {
            self.body.append(self.allocator, Op.i64_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 3) catch return error.OutOfMemory;
        }
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    } else {
        const zero2 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        try self.emitLocalSet(zero2);
        const es = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
        try self.emitLocalSet(es);
        try self.emitMemCopyLoop(new_data, zero2, elem_val, es);
    }

    // Copy old elements at offset elem_size
    const old_byte_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(old_byte_len);

    const dst_off = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    try self.emitLocalSet(dst_off);

    try self.emitMemCopyLoop(new_data, dst_off, old_data, old_byte_len);

    try self.buildRocList(new_data, new_len);
}

/// Generate LowLevel list_concat: concatenate two lists.
fn generateLLListConcat(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    const elem_align = self.getListElemAlign(ret_layout);

    // Generate both lists
    try self.generateExpr(args[0]);
    const a_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(a_ptr);
    try self.generateExpr(args[1]);
    const b_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(b_ptr);

    // Load data+len from each
    const a_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const a_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(a_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(a_data);
    try self.emitLocalGet(a_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;
    try self.emitLocalSet(a_len);

    const b_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const b_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(b_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(b_data);
    try self.emitLocalGet(b_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;
    try self.emitLocalSet(b_len);

    // new_len = a_len + b_len
    const new_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(a_len);
    try self.emitLocalGet(b_len);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(new_len);

    // Allocate new buffer
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(new_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAlloc(total_size, elem_align);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // Copy a's bytes at offset 0
    const a_bytes = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(a_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(a_bytes);

    const zero3 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero3);

    try self.emitMemCopyLoop(new_data, zero3, a_data, a_bytes);

    // Copy b's bytes at offset a_bytes
    const b_bytes = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(b_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(b_bytes);

    try self.emitMemCopyLoop(new_data, a_bytes, b_data, b_bytes);

    try self.buildRocList(new_data, new_len);
}

/// Generate LowLevel list_reverse: create new list with elements in reverse order.
fn generateLLListReverse(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    if (elem_size == 0) {
        try self.generateExpr(args[0]);
        return;
    }

    const import_idx = self.list_reverse_import orelse unreachable;
    const elem_align = self.getListElemAlign(ret_layout);

    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    const result_offset = try self.allocStackMemory(12, 4);
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_align)) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
}

/// Build a RocList struct on the stack frame from data ptr and length locals.
fn buildRocList(self: *Self, data_local: u32, len_local: u32) Allocator.Error!void {
    const list_offset = try self.allocStackMemory(12, 4);
    const list_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(list_offset);
    try self.emitLocalSet(list_base);

    // ptr (offset 0)
    try self.emitLocalGet(data_local);
    try self.emitStoreToMem(list_base, 0, .i32);

    // len (offset 4)
    try self.emitLocalGet(len_local);
    try self.emitStoreToMem(list_base, 4, .i32);

    // cap (offset 8) = len
    try self.emitLocalGet(len_local);
    try self.emitStoreToMem(list_base, 8, .i32);

    // Push pointer
    try self.emitLocalGet(list_base);
}

/// Build a RocList struct with separate capacity value.
fn buildRocListWithCap(self: *Self, data_local: u32, len_local: u32, cap_local: u32) Allocator.Error!void {
    const list_offset = try self.allocStackMemory(12, 4);
    const list_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(list_offset);
    try self.emitLocalSet(list_base);

    // ptr (offset 0)
    try self.emitLocalGet(data_local);
    try self.emitStoreToMem(list_base, 0, .i32);

    // len (offset 4)
    try self.emitLocalGet(len_local);
    try self.emitStoreToMem(list_base, 4, .i32);

    // cap (offset 8)
    try self.emitLocalGet(cap_local);
    try self.emitStoreToMem(list_base, 8, .i32);

    // Push pointer
    try self.emitLocalGet(list_base);
}

fn generateLLListSortWith(self: *Self, ll: anytype, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    if (elem_size == 0) {
        try self.generateExpr(args[0]);
        return;
    }

    const elem_align = self.getListElemAlign(ret_layout);
    if (ll.callable_proc.isNone()) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "WasmCodeGen invariant violated: list_sort_with is missing callable_proc metadata",
                .{},
            );
        }
        unreachable;
    }
    const cmp_proc = self.store.getProcSpec(ll.callable_proc);
    const cmp_proc_key: u64 = @bitCast(cmp_proc.name);
    const cmp_func_idx = self.registered_procs.get(cmp_proc_key) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "WasmCodeGen invariant violated: list_sort_with comparator proc {d} was not compiled before codegen",
                .{@intFromEnum(ll.callable_proc)},
            );
        }
        unreachable;
    };

    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    const old_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    try self.emitLoadOp(.i32, 0);
    try self.emitLocalSet(old_data);

    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    try self.emitLoadOp(.i32, 4);
    try self.emitLocalSet(old_len);

    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_lt_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    {
        try self.emitLocalGet(list_ptr);
        try self.emitLocalSet(result_local);
    }
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    {
        const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(old_len);
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
        try self.emitLocalSet(total_size);

        try self.emitHeapAlloc(total_size, elem_align);
        const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(new_data);

        const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        try self.emitLocalSet(zero);
        try self.emitMemCopyLoop(new_data, zero, old_data, total_size);

        const temp_offset = try self.allocStackMemory(elem_size, elem_align);
        const temp_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(temp_offset);
        try self.emitLocalSet(temp_ptr);

        const i_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
        try self.emitLocalSet(i_local);

        self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
        self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
        self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        {
            try self.emitLocalGet(i_local);
            try self.emitLocalGet(old_len);
            self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
            try WasmModule.leb128WriteU32(self.allocator, &self.body, 1);

            const elem_i_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(new_data);
            try self.emitLocalGet(i_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(elem_i_ptr);

            try self.emitMemCopy(temp_ptr, 0, elem_i_ptr, elem_size);

            const j_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(i_local);
            try self.emitLocalSet(j_local);

            self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            {
                try self.emitLocalGet(j_local);
                self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                try WasmModule.leb128WriteU32(self.allocator, &self.body, 1);

                const prev_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalGet(new_data);
                try self.emitLocalGet(j_local);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                try self.emitLocalSet(prev_ptr);

                try self.emitLocalGet(self.roc_ops_local);
                try self.emitLocalGet(temp_ptr);
                try self.emitLocalGet(prev_ptr);
                try self.emitCall(cmp_func_idx);
                const cmp_result = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(cmp_result);

                try self.emitLocalGet(cmp_result);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                try WasmModule.leb128WriteU32(self.allocator, &self.body, 1);

                const dst_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalGet(new_data);
                try self.emitLocalGet(j_local);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                try self.emitLocalSet(dst_ptr);

                try self.emitMemCopy(dst_ptr, 0, prev_ptr, elem_size);

                try self.emitLocalGet(j_local);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                try self.emitLocalSet(j_local);

                self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
                try WasmModule.leb128WriteU32(self.allocator, &self.body, 0);
            }
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

            const insert_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(new_data);
            try self.emitLocalGet(j_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(insert_ptr);

            try self.emitMemCopy(insert_ptr, 0, temp_ptr, elem_size);

            try self.emitLocalGet(i_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(i_local);

            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            try WasmModule.leb128WriteU32(self.allocator, &self.body, 0);
        }
        self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

        try self.buildRocList(new_data, old_len);
        try self.emitLocalSet(result_local);
    }
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    try self.emitLocalGet(result_local);
}

/// Generate list_with_capacity: create empty list with given capacity
fn generateLLListWithCapacity(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    const elem_align = self.getListElemAlign(ret_layout);

    // Generate capacity arg (may be i64 from MonoIR layout; convert to i32 for wasm32)
    try self.generateExpr(args[0]);
    try self.emitConversion(self.exprValType(args[0]), .i32);
    const cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(cap);

    // Allocate cap * elem_size bytes on heap
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(cap);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAlloc(total_size, elem_align);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // len = 0
    const len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(len);

    try self.buildRocListWithCap(new_data, len, cap);
}

/// Generate list_set: set element at index, creating a new list
fn generateLLListSet(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    const elem_align = self.getListElemAlign(ret_layout);

    // Generate list arg
    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    // Generate index arg (may be i64 from MonoIR; convert to i32 for wasm32)
    try self.generateExpr(args[1]);
    try self.emitConversion(self.exprValType(args[1]), .i32);
    const index = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(index);

    // Generate element arg
    try self.generateExpr(args[2]);
    const set_elem_vt: ValType = if (elem_size <= 4) .i32 else if (elem_size <= 8) .i64 else .i32;
    const elem_val = self.storage.allocAnonymousLocal(set_elem_vt) catch return error.OutOfMemory;
    try self.emitLocalSet(elem_val);

    // Load list fields
    const old_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    // Allocate new data buffer (same size as old)
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAlloc(total_size, elem_align);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // Copy all old data
    const zero2 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero2);
    try self.emitMemCopyLoop(new_data, zero2, old_data, total_size);

    // Overwrite element at index
    const dst = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(new_data);
    try self.emitLocalGet(index);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(dst);

    const bytes_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    try self.emitLocalSet(bytes_local);
    const zero3 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero3);
    try self.emitMemCopyLoop(dst, zero3, elem_val, bytes_local);

    try self.buildRocList(new_data, old_len);
}

/// Generate list_reserve: ensure list has at least given capacity
fn generateLLListReserve(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    const elem_align = self.getListElemAlign(ret_layout);

    // Generate list arg
    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    // Generate additional capacity arg (may be i64 from MonoIR; convert to i32 for wasm32)
    try self.generateExpr(args[1]);
    try self.emitConversion(self.exprValType(args[1]), .i32);
    const additional = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(additional);

    // Load list fields
    const old_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    // new_cap = old_len + additional
    const new_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    try self.emitLocalGet(additional);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(new_cap);

    // Allocate new_cap * elem_size bytes
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(new_cap);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAlloc(total_size, elem_align);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // Copy old data (old_len * elem_size bytes)
    const copy_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(copy_size);

    const zero4 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero4);
    try self.emitMemCopyLoop(new_data, zero4, old_data, copy_size);

    try self.buildRocListWithCap(new_data, old_len, new_cap);
}

/// Generate list_release_excess_capacity: shrink list to exact length
fn generateLLListReleaseExcessCapacity(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    const elem_align = self.getListElemAlign(ret_layout);

    // Generate list arg
    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    // Load list fields
    const old_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    // Allocate new_len * elem_size bytes
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAlloc(total_size, elem_align);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // Copy old data
    const zero5 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero5);
    try self.emitMemCopyLoop(new_data, zero5, old_data, total_size);

    try self.buildRocList(new_data, old_len);
}

/// Generate list_split_first: split list into first element and rest
fn generateLLListSplitFirst(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    // ret_layout is a record { first: elem, rest: list }
    // We need to extract the element type and size
    const elem_size = self.getListElemSize(ret_layout);
    const elem_align = self.getListElemAlign(ret_layout);
    // Generate list arg
    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    // Load list fields
    const old_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    // Allocate result struct: { first: elem (elem_size bytes), rest: list (12 bytes) }
    // Alignment should be max(elem_align, 4)
    const result_align: u32 = if (elem_align > 4) elem_align else 4;
    const first_offset: u32 = 0;
    const rest_offset: u32 = if (elem_size % 4 == 0) elem_size else ((elem_size / 4) + 1) * 4;
    const result_size: u32 = rest_offset + 12;

    const result_offset = try self.allocStackMemory(result_size, result_align);
    const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_ptr);

    // Copy first element to result
    const bytes_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    try self.emitLocalSet(bytes_local);

    const first_dst = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(result_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(first_offset)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(first_dst);

    const zero6 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero6);
    try self.emitMemCopyLoop(first_dst, zero6, old_data, bytes_local);

    // Build rest list (pointing to old_data + elem_size, len = old_len - 1)
    const rest_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_data);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rest_data);

    const rest_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(rest_len);

    const encoded_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitPrepareListSliceMetadata(list_ptr, self.listContainsRefcounted(self.exprLayoutIdx(args[0])), encoded_cap);

    // Store rest list in result struct
    const rest_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(result_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(rest_offset)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rest_base);

    // ptr
    try self.emitLocalGet(rest_data);
    try self.emitStoreToMem(rest_base, 0, .i32);
    // len
    try self.emitLocalGet(rest_len);
    try self.emitStoreToMem(rest_base, 4, .i32);
    // cap = encoded seamless-slice allocation pointer
    try self.emitLocalGet(encoded_cap);
    try self.emitStoreToMem(rest_base, 8, .i32);

    // Push result pointer
    try self.emitLocalGet(result_ptr);
}

/// Generate list_split_last: split list into rest and last element
fn generateLLListSplitLast(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const elem_size = self.getListElemSize(ret_layout);
    const elem_align = self.getListElemAlign(ret_layout);

    // Generate list arg
    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    // Load list fields
    const old_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    // Allocate result struct: { rest: list (12 bytes), last: elem (elem_size bytes) }
    const rest_offset: u32 = 0;
    // Align last element properly
    const aligned_last_offset: u32 = if (elem_align <= 4) 12 else ((12 + elem_align - 1) / elem_align) * elem_align;
    const result_size: u32 = aligned_last_offset + elem_size;
    const result_align: u32 = if (elem_align > 4) elem_align else 4;

    const result_offset = try self.allocStackMemory(result_size, result_align);
    const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_ptr);

    // rest_len = old_len - 1
    const rest_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(rest_len);

    // Store rest list in result struct (shares data ptr, but with reduced length)
    const rest_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(result_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(rest_offset)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rest_base);

    // ptr (same as old_data)
    try self.emitLocalGet(old_data);
    try self.emitStoreToMem(rest_base, 0, .i32);
    // len
    try self.emitLocalGet(rest_len);
    try self.emitStoreToMem(rest_base, 4, .i32);
    // Preserve source list cap metadata (including seamless-slice encoding).
    const source_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    try self.emitLoadOp(.i32, 8);
    try self.emitLocalSet(source_cap);
    try self.emitLocalGet(source_cap);
    try self.emitStoreToMem(rest_base, 8, .i32);

    // Copy last element to result
    // last_src = old_data + rest_len * elem_size
    const last_src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_data);
    try self.emitLocalGet(rest_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(last_src);

    const bytes_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    try self.emitLocalSet(bytes_local);

    const last_dst = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(result_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(aligned_last_offset)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(last_dst);

    const zero7 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero7);
    try self.emitMemCopyLoop(last_dst, zero7, last_src, bytes_local);

    // Push result pointer
    try self.emitLocalGet(result_ptr);
}

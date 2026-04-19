//! Statement-only LIR -> WebAssembly code generator.
//!
//! Walks explicit `CFStmt` procedure bodies and emits wasm instructions.
//! All value-producing work is expressed through explicit local assignments;
//! there is no runtime expression-tree interpretation in the active code path.
//!
//! Ownership boundary:
//! - explicit RC lowering happens through `generateRcStmt`
//! - builtin/runtime helper implementations may perform primitive-internal RC
//! - ordinary wasm lowering is forbidden from inventing ownership policy

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const layout = @import("layout");

const lir = @import("lir");
const LIR = lir.LIR;
const LirStore = lir.LirStore;
const ownership_boundary = lir.OwnershipBoundary;
const RcHelperKey = layout.RcHelperKey;
const RcHelperPlan = layout.RcHelperPlan;
const RcListPlan = layout.ListPlan;
const ProcLocalId = LIR.LocalId;
const ProcLocalSpan = LIR.LocalSpan;
const RefOp = LIR.RefOp;
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
const wasm_roc_ops_env_offset: u32 = 0;
const wasm_roc_ops_dbg_offset: u32 = 16;
const wasm_roc_ops_expect_failed_offset: u32 = 20;
const wasm_roc_ops_crashed_offset: u32 = 24;

const Self = @This();

fn builtinInternalLayoutContainsRefcounted(ls: *const LayoutStore, comptime site: []const u8, layout_idx: layout.Idx) bool {
    ownership_boundary.builtinRuntimeInternal(site);
    return ls.layoutContainsRefcounted(ls.getLayout(layout_idx));
}

fn explicitRcLayoutContainsRefcounted(ls: *const LayoutStore, comptime site: []const u8, layout_idx: layout.Idx) bool {
    ownership_boundary.explicitLirRcExecution(site);
    return ls.layoutContainsRefcounted(ls.getLayout(layout_idx));
}

const BuiltinListAbi = struct {
    elem_layout_idx: ?layout.Idx,
    elem_layout: layout.Layout,
    elem_size: u32,
    elem_align: u32,
    elements_refcounted: bool,
};

fn builtinInternalListAbi(self: *const Self, comptime site: []const u8, list_layout_idx: layout.Idx) BuiltinListAbi {
    ownership_boundary.builtinRuntimeInternal(site);
    const abi = self.getLayoutStore().builtinListAbi(list_layout_idx);
    return .{
        .elem_layout_idx = abi.elem_layout_idx,
        .elem_layout = abi.elem_layout,
        .elem_size = abi.elem_size,
        .elem_align = abi.elem_alignment,
        .elements_refcounted = abi.contains_refcounted,
    };
}

allocator: Allocator,
store: *const LirStore,
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
/// Map from proc spec id → compiled wasm function index.
registered_procs: std.AutoHashMap(u32, u32),
/// Map from RC helper key → compiled wasm function index.
rc_helper_funcs: std.AutoHashMap(u64, u32),
/// Map from proc spec id → wasm table index (for proc_ref literals).
proc_table_indices: std.AutoHashMap(u32, u32),
/// Cache of function type signatures → wasm type index.
func_type_cache: std.StringHashMap(u32),
/// Scratch buffer for function type cache keys.
func_type_key_scratch: std.ArrayList(u8),
/// Type index for the RocOps function signature: (i32, i32) -> void.
roc_ops_type_idx: u32 = 0,
/// Table indices for RocOps functions (used with call_indirect).
roc_alloc_table_idx: u32 = 0,
roc_dealloc_table_idx: u32 = 0,
roc_realloc_table_idx: u32 = 0,
roc_dbg_table_idx: u32 = 0,
roc_expect_failed_table_idx: u32 = 0,
roc_crashed_table_idx: u32 = 0,
proc_arg_counts_offset: u32 = 0,
/// Local index holding the roc_ops_ptr (pointer to RocOps struct in linear memory).
/// In main(), this is a local storing the constant 0 (struct at memory offset 0).
/// In compiled functions, this is parameter 0.
roc_ops_local: u32 = 0,
/// Local index used to hold the current proc's return value until epilogue time.
proc_return_local: u32 = 0,
/// CFStmt block nesting depth (for br targets in proc compilation).
cf_depth: u32 = 0,
/// Structured control depth used for loop-break branch depths.
structured_control_depth: u32 = 0,
/// Whether we're currently generating code inside a proc body.
in_proc: bool = false,
/// Current proc being compiled (debugging/tracing).
current_proc_id: ?LIR.LirProcSpecId = null,
/// Map from JoinPointId → loop depth (for jump → br targeting).
join_point_depths: std.AutoHashMap(u32, u32),
/// Map from JoinPointId → param local indices.
join_point_param_locals: std.AutoHashMap(u32, []u32),
/// Map from JoinPointId → state local selecting remainder or join body.
join_point_state_locals: std.AutoHashMap(u32, u32),
/// Debug-only guard for recursive statement generation.
active_stmt_generations: std.AutoHashMap(u32, void),
/// Debug-only count of how many times a statement has been generated.
stmt_generation_counts: std.AutoHashMap(u32, u32),
/// Stack of loop-continue label depths for lowering explicit LIR loop_continue.
loop_continue_target_depths: std.ArrayList(u32),
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
/// Wasm function index for imported roc_i8_mod_by host function.
i8_mod_by_import: ?u32 = null,
/// Wasm function index for imported roc_u8_mod_by host function.
u8_mod_by_import: ?u32 = null,
/// Wasm function index for imported roc_i16_mod_by host function.
i16_mod_by_import: ?u32 = null,
/// Wasm function index for imported roc_u16_mod_by host function.
u16_mod_by_import: ?u32 = null,
/// Wasm function index for imported roc_u32_mod_by host function.
u32_mod_by_import: ?u32 = null,
/// Wasm function index for imported roc_u64_mod_by host function.
u64_mod_by_import: ?u32 = null,
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
/// Wasm function index for imported roc_int_to_str host function.
int_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_float_to_str host function.
float_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_str_escape_and_quote host function.
str_escape_and_quote_import: ?u32 = null,
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
list_concat_import: ?u32 = null,
list_drop_at_import: ?u32 = null,
list_reverse_import: ?u32 = null,
/// Configurable wasm stack size in bytes (default 1MB).
wasm_stack_bytes: u32 = 1024 * 1024,
/// Configurable wasm memory pages (0 = auto-compute from stack size).
wasm_memory_pages: u32 = 0,
pub fn init(allocator: Allocator, store: *const LirStore, layout_store: *const LayoutStore) Self {
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
        .registered_procs = std.AutoHashMap(u32, u32).init(allocator),
        .rc_helper_funcs = std.AutoHashMap(u64, u32).init(allocator),
        .proc_table_indices = std.AutoHashMap(u32, u32).init(allocator),
        .func_type_cache = std.StringHashMap(u32).init(allocator),
        .func_type_key_scratch = .empty,
        .join_point_depths = std.AutoHashMap(u32, u32).init(allocator),
        .join_point_param_locals = std.AutoHashMap(u32, []u32).init(allocator),
        .join_point_state_locals = std.AutoHashMap(u32, u32).init(allocator),
        .active_stmt_generations = std.AutoHashMap(u32, void).init(allocator),
        .stmt_generation_counts = std.AutoHashMap(u32, u32).init(allocator),
        .loop_continue_target_depths = .empty,
    };
}

pub fn deinit(self: *Self) void {
    self.module.deinit();
    self.body.deinit(self.allocator);
    self.storage.deinit();
    self.registered_procs.deinit();
    self.rc_helper_funcs.deinit();
    self.proc_table_indices.deinit();
    var func_type_keys = self.func_type_cache.keyIterator();
    while (func_type_keys.next()) |key| {
        self.allocator.free(key.*);
    }
    self.func_type_cache.deinit();
    self.func_type_key_scratch.deinit(self.allocator);
    self.join_point_depths.deinit();
    // Free allocated param local arrays
    var jp_it = self.join_point_param_locals.iterator();
    while (jp_it.next()) |entry| {
        self.allocator.free(entry.value_ptr.*);
    }
    self.join_point_param_locals.deinit();
    self.join_point_state_locals.deinit();
    self.active_stmt_generations.deinit();
    self.stmt_generation_counts.deinit();
    self.loop_continue_target_depths.deinit(self.allocator);
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
    self.i8_mod_by_import = try self.module.addImport("env", "roc_i8_mod_by", i32_mod_by_type);
    self.u8_mod_by_import = try self.module.addImport("env", "roc_u8_mod_by", i32_mod_by_type);
    self.i16_mod_by_import = try self.module.addImport("env", "roc_i16_mod_by", i32_mod_by_type);
    self.u16_mod_by_import = try self.module.addImport("env", "roc_u16_mod_by", i32_mod_by_type);
    self.u32_mod_by_import = try self.module.addImport("env", "roc_u32_mod_by", i32_mod_by_type);

    const i64_mod_by_type = try self.module.addFuncType(&.{ .i64, .i64 }, &.{.i64});
    self.i64_mod_by_import = try self.module.addImport("env", "roc_i64_mod_by", i64_mod_by_type);
    self.u64_mod_by_import = try self.module.addImport("env", "roc_u64_mod_by", i64_mod_by_type);

    // i128/u128 to string: (val_ptr, buf_ptr) -> i32 str_len
    const i128_to_str_type = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{.i32},
    );
    self.i128_to_str_import = try self.module.addImport("env", "roc_i128_to_str", i128_to_str_type);
    self.u128_to_str_import = try self.module.addImport("env", "roc_u128_to_str", i128_to_str_type);

    const int_to_str_type = try self.module.addFuncType(
        &.{ .i64, .i64, .i32, .i32, .i32 },
        &.{.i32},
    );
    self.int_to_str_import = try self.module.addImport("env", "roc_int_to_str", int_to_str_type);

    const float_to_str_type = try self.module.addFuncType(
        &.{ .i64, .i32, .i32 },
        &.{.i32},
    );
    self.float_to_str_import = try self.module.addImport("env", "roc_float_to_str", float_to_str_type);

    const str_escape_and_quote_type = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{},
    );
    self.str_escape_and_quote_import = try self.module.addImport("env", "roc_str_escape_and_quote", str_escape_and_quote_type);

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

    const str_from_utf8_type = try self.module.addFuncType(
        &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 },
        &.{},
    );
    self.str_from_utf8_import = try self.module.addImport("env", "roc_str_from_utf8", str_from_utf8_type);

    const int_from_str_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.int_from_str_import = try self.module.addImport("env", "roc_int_from_str", int_from_str_type);

    const dec_from_str_type = try self.module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
    self.dec_from_str_import = try self.module.addImport("env", "roc_dec_from_str", dec_from_str_type);

    const float_from_str_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32 }, &.{});
    self.float_from_str_import = try self.module.addImport("env", "roc_float_from_str", float_from_str_type);

    const list_append_unsafe_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.list_append_unsafe_import = try self.module.addImport("env", "roc_list_append_unsafe", list_append_unsafe_type);

    const list_concat_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.list_concat_import = try self.module.addImport("env", "roc_list_concat", list_concat_type);

    const list_drop_at_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.list_drop_at_import = try self.module.addImport("env", "roc_list_drop_at", list_drop_at_type);

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

/// Generate a complete wasm module for a zero-argument root proc.
/// The exported `main` function initializes RocOps and tail-calls the root proc.
pub fn generateModule(self: *Self, root_proc_id: LIR.LirProcSpecId, result_layout: layout.Idx) Allocator.Error!GenerateResult {
    // Register host function imports (must be done before addFunction calls)
    self.registerHostImports() catch return error.OutOfMemory;

    // Compile all procedures before the synthetic main wrapper.
    const proc_specs = self.store.getProcSpecs();
    if (proc_specs.len > 0) {
        self.compileAllProcSpecs(proc_specs) catch return error.OutOfMemory;
    }

    const root_proc = self.store.getProcSpec(root_proc_id);
    if (!root_proc.args.isEmpty()) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "WASM/codegen invariant violated: synthetic main expects a zero-arg root proc, got {d} args",
                .{self.store.getLocalSpan(root_proc.args).len},
            );
        }
        unreachable;
    }

    const root_key: u32 = @intFromEnum(root_proc_id);
    const root_func_idx = self.registered_procs.get(root_key) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("WASM/codegen invariant violated: missing compiled root proc {d}", .{@intFromEnum(root_proc_id)});
        }
        unreachable;
    };

    const result_vt = self.resolveValType(result_layout);

    // Add function type: (i32 env_ptr) -> (result_type)
    const type_idx = self.module.addFuncType(&.{.i32}, &.{result_vt}) catch return error.OutOfMemory;

    // Add function
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;

    // Generate the proc body into self.body
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

    try self.emitLocalGet(self.roc_ops_local);
    try self.emitCall(root_func_idx);

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

    self.module.addExport("main", .func, func_idx) catch return error.OutOfMemory;

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

/// Generate wasm instructions for an already-bound local value.
fn emitProcLocal(self: *Self, value: ProcLocalId) Allocator.Error!void {
    const local_info = self.storage.getLocalInfo(value) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "WASM/codegen invariant violated: missing local binding for LIR local {d}",
                .{@intFromEnum(value)},
            );
        }
        unreachable;
    };

    try self.emitLocalGet(local_info.idx);

    const expected_vt = self.resolveValType(self.procLocalLayoutIdx(value));
    if (local_info.val_type != expected_vt) {
        try self.emitConversion(local_info.val_type, expected_vt);
    }
    try self.emitCanonicalizeScalarForLayout(self.procLocalLayoutIdx(value));
}

fn emitRawRcForValueLocal(
    self: *Self,
    comptime kind: RcOpKind,
    value_local: u32,
    value_vt: ValType,
    layout_idx: layout.Idx,
    inc_count: u16,
) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);
    if (!explicitRcLayoutContainsRefcounted(ls, "wasm.emitRcForValueLocal.layout_rc", layout_idx)) return;
    if (value_vt != .i32) return;

    if (self.isCompositeLayout(layout_idx)) {
        try self.emitRawRcHelperCallForValuePtr(kind, value_local, layout_idx, inc_count);
        return;
    }

    const size_align = ls.layoutSizeAlign(l);
    if (size_align.size == 0) return;

    const slot = try self.allocStackMemory(@intCast(size_align.size), @intCast(size_align.alignment.toByteUnits()));
    const ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(slot);
    try self.emitLocalSet(ptr_local);

    try self.emitLocalGet(ptr_local);
    try self.emitLocalGet(value_local);
    try self.emitStoreOpSized(.i32, @intCast(size_align.size), 0);

    try self.emitRawRcHelperCallForValuePtr(kind, ptr_local, layout_idx, inc_count);
}

fn emitExplicitRcForValueLocal(
    self: *Self,
    comptime kind: RcOpKind,
    value_local: u32,
    value_vt: ValType,
    layout_idx: layout.Idx,
    inc_count: u16,
) Allocator.Error!void {
    ownership_boundary.explicitLirRcExecution("wasm.emitExplicitRcForValueLocal");
    try self.emitRawRcForValueLocal(kind, value_local, value_vt, layout_idx, inc_count);
}

fn emitRawDirectRcPlan(
    self: *Self,
    helper_key: RcHelperKey,
    helper_plan: RcHelperPlan,
    value_ptr_local: u32,
    count_local: ?u32,
) Allocator.Error!bool {
    switch (helper_plan) {
        .noop => return true,
        .str_incref => {
            if (count_local) |count| {
                const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                const is_small_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitDecodeStrAllocPtr(value_ptr_local, alloc_ptr_local, is_small_local);

                self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
                self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
                try self.emitLocalGet(is_small_local);
                self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                try self.emitDataPtrIncrefByLocal(alloc_ptr_local, count);
                self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            } else {
                try self.emitBuiltinInternalStrRc(.incref, value_ptr_local, 1);
            }
            return true;
        },
        .str_decref => {
            try self.emitBuiltinInternalStrRc(.decref, value_ptr_local, 1);
            return true;
        },
        .str_free => {
            try self.emitBuiltinInternalStrRc(.free, value_ptr_local, 1);
            return true;
        },
        .list_incref => |list_plan| {
            if (list_plan.child != null) return false;
            const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const is_slice_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitDecodeListAllocPtr(value_ptr_local, alloc_ptr_local, is_slice_local);
            if (count_local) |count| {
                try self.emitDataPtrIncrefByLocal(alloc_ptr_local, count);
            } else {
                try self.emitDataPtrIncref(alloc_ptr_local, 1);
            }
            return true;
        },
        .list_decref => |list_plan| {
            if (list_plan.child != null) return false;
            try self.emitBuiltinInternalListRc(.decref, value_ptr_local, helper_key.layout_idx, list_plan, 1);
            return true;
        },
        .list_free => |list_plan| {
            if (list_plan.child != null) return false;
            try self.emitBuiltinInternalListRc(.free, value_ptr_local, helper_key.layout_idx, list_plan, 1);
            return true;
        },
        .box_incref => {
            const box_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr_local);
            if (count_local) |count| {
                try self.emitDataPtrIncrefByLocal(box_ptr_local, count);
            } else {
                try self.emitDataPtrIncref(box_ptr_local, 1);
            }
            return true;
        },
        .box_decref => |box_plan| {
            if (box_plan.child != null) return false;
            const box_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr_local);
            if (box_plan.child) |child_key| {
                try self.emitBuiltinInternalBoxChildDropIfUnique(box_ptr_local, child_key);
            }
            try self.emitDataPtrDecref(box_ptr_local, box_plan.elem_alignment, box_plan.child != null);
            return true;
        },
        .box_free => |box_plan| {
            if (box_plan.child != null) return false;
            const box_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr_local);
            if (box_plan.child) |child_key| {
                try self.emitBuiltinInternalBoxChildDropIfUnique(box_ptr_local, child_key);
            }
            try self.emitDataPtrFree(box_ptr_local, box_plan.elem_alignment, box_plan.child != null);
            return true;
        },
        .struct_, .tag_union, .closure => return false,
    }
}

fn emitRawRcHelperCallForValuePtr(
    self: *Self,
    comptime kind: RcOpKind,
    value_ptr_local: u32,
    layout_idx: layout.Idx,
    inc_count: u16,
) Allocator.Error!void {
    const normalized_value_ptr = try self.normalizeCompositeValuePtr(value_ptr_local, layout_idx);
    const helper_key = RcHelperKey{ .op = switch (kind) {
        .incref => .incref,
        .decref => .decref,
        .free => .free,
    }, .layout_idx = layout_idx };
    const helper_plan = self.getLayoutStore().rcHelperPlan(helper_key);
    if (helper_plan == .noop) return;
    if (try self.emitRawDirectRcPlan(helper_key, helper_plan, normalized_value_ptr, null)) return;

    const helper_func_idx = try self.compileBuiltinInternalRcHelper(helper_key);
    try self.emitLocalGet(normalized_value_ptr);
    switch (kind) {
        .incref => {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(inc_count)) catch return error.OutOfMemory;
            try self.emitLocalGet(self.roc_ops_local);
        },
        .decref, .free => {
            try self.emitLocalGet(self.roc_ops_local);
        },
    }
    try self.emitCall(helper_func_idx);
}

fn normalizeCompositeValuePtr(self: *Self, value_ptr_local: u32, layout_idx: layout.Idx) Allocator.Error!u32 {
    if (!self.isCompositeLayout(layout_idx)) return value_ptr_local;

    const normalized = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(value_ptr_local);
    try self.emitLocalSet(normalized);
    try self.emitLocalGet(normalized);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    const normalized_size = self.layoutByteSize(self.runtimeRepresentationLayoutIdx(layout_idx));
    const normalized_align = self.layoutStorageByteAlign(layout_idx);
    const stable_offset = try self.allocStackMemory(normalized_size, normalized_align);
    const stable_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(stable_offset);
    try self.emitLocalSet(stable_ptr);
    try self.emitZeroInit(stable_ptr, normalized_size);
    try self.emitLocalGet(stable_ptr);
    try self.emitLocalSet(normalized);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    return normalized;
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

fn emitBuiltinInternalFreeRcPtr(self: *Self, rc_ptr_local: u32, element_alignment: u32, elements_refcounted: bool) Allocator.Error!void {
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

fn emitDataPtrIncrefByLocal(self: *Self, data_ptr_local: u32, amount_local: u32) Allocator.Error!void {
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
    try self.emitLocalGet(amount_local);
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
    try self.emitBuiltinInternalFreeRcPtr(rc_ptr, alignment, elements_refcounted);
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
    try self.emitBuiltinInternalFreeRcPtr(rc_ptr, alignment, elements_refcounted);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitBuiltinInternalListElementDecrefsIfUnique(
    self: *Self,
    list_ptr_local: u32,
    alloc_ptr_local: u32,
    is_slice_local: u32,
    elem_width: usize,
    child_key: RcHelperKey,
) Allocator.Error!void {
    const elem_size: u32 = @intCast(elem_width);
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

    try self.emitRawRcHelperCallByKey(child_key, elem_ptr_local, null);

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

fn emitBuiltinInternalListRc(
    self: *Self,
    comptime kind: RcOpKind,
    list_ptr_local: u32,
    list_layout_idx: layout.Idx,
    list_plan: ?layout.RcListPlan,
    inc_count: u16,
) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.emitBuiltinInternalListRc.builtin_list_abi", list_layout_idx);

    const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const is_slice_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitDecodeListAllocPtr(list_ptr_local, alloc_ptr_local, is_slice_local);

    switch (kind) {
        .incref => {
            try self.emitDataPtrIncref(alloc_ptr_local, inc_count);
        },
        .decref => {
            if (list_plan) |plan| {
                if (plan.child) |child_key| {
                    try self.emitBuiltinInternalListElementDecrefsIfUnique(list_ptr_local, alloc_ptr_local, is_slice_local, plan.elem_width, child_key);
                }
            }
            try self.emitDataPtrDecref(alloc_ptr_local, list_abi.elem_align, list_abi.elements_refcounted);
        },
        .free => {
            if (list_plan) |plan| {
                if (plan.child) |child_key| {
                    try self.emitBuiltinInternalListElementDecrefsIfUnique(list_ptr_local, alloc_ptr_local, is_slice_local, plan.elem_width, child_key);
                }
            }
            try self.emitDataPtrFree(alloc_ptr_local, list_abi.elem_align, list_abi.elements_refcounted);
        },
    }
}

fn emitBuiltinInternalListIncrefByLocal(
    self: *Self,
    list_ptr_local: u32,
    list_layout_idx: layout.Idx,
    list_plan: layout.RcListPlan,
    count_local: u32,
) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.emitBuiltinInternalListIncrefByLocal.builtin_list_abi", list_layout_idx);
    const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const is_slice_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitDecodeListAllocPtr(list_ptr_local, alloc_ptr_local, is_slice_local);

    if (list_abi.elements_refcounted and list_plan.child != null) {
        self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
        self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        try self.emitLocalGet(is_slice_local);
        self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

        try self.emitLocalGet(alloc_ptr_local);
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 8) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;

        try self.emitLocalGet(list_ptr_local);
        try self.emitLoadOp(.i32, 4);
        try self.emitStoreOp(.i32, 0);

        self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    try self.emitDataPtrIncrefByLocal(alloc_ptr_local, count_local);
}

fn emitBuiltinInternalStrRc(self: *Self, comptime kind: RcOpKind, str_ptr_local: u32, inc_count: u16) Allocator.Error!void {
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

fn emitRawRcHelperCallByKey(
    self: *Self,
    helper_key: RcHelperKey,
    value_ptr_local: u32,
    count_local: ?u32,
) Allocator.Error!void {
    const helper_plan = self.getLayoutStore().rcHelperPlan(helper_key);
    if (helper_plan == .noop) return;
    if (try self.emitRawDirectRcPlan(helper_key, helper_plan, value_ptr_local, count_local)) return;

    const helper_func_idx = try self.compileBuiltinInternalRcHelper(helper_key);
    try self.emitLocalGet(value_ptr_local);
    switch (helper_key.op) {
        .incref => {
            try self.emitLocalGet(count_local.?);
            try self.emitLocalGet(self.roc_ops_local);
        },
        .decref, .free => {
            try self.emitLocalGet(self.roc_ops_local);
        },
    }
    try self.emitCall(helper_func_idx);
}

fn emitBuiltinInternalBoxChildDropIfUnique(
    self: *Self,
    box_ptr_local: u32,
    child_key: RcHelperKey,
) Allocator.Error!void {
    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(box_ptr_local);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitLoadI32AtPtrOffset(box_ptr_local, -4, rc_val);
    try self.emitLocalGet(rc_val);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitRawRcHelperCallByKey(child_key, box_ptr_local, null);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn generateBuiltinInternalRcHelperBody(
    self: *Self,
    helper_key: RcHelperKey,
    value_ptr_local: u32,
    count_local: ?u32,
) Allocator.Error!void {
    switch (self.getLayoutStore().rcHelperPlan(helper_key)) {
        .noop => {},
        .str_incref => {
            const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const is_small_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitDecodeStrAllocPtr(value_ptr_local, alloc_ptr_local, is_small_local);

            self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitLocalGet(is_small_local);
            self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitDataPtrIncrefByLocal(alloc_ptr_local, count_local.?);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .str_decref => try self.emitBuiltinInternalStrRc(.decref, value_ptr_local, 1),
        .str_free => try self.emitBuiltinInternalStrRc(.free, value_ptr_local, 1),
        .list_incref => |list_plan| try self.emitBuiltinInternalListIncrefByLocal(value_ptr_local, helper_key.layout_idx, list_plan, count_local.?),
        .list_decref => |list_plan| try self.emitBuiltinInternalListRc(.decref, value_ptr_local, helper_key.layout_idx, list_plan, 1),
        .list_free => |list_plan| try self.emitBuiltinInternalListRc(.free, value_ptr_local, helper_key.layout_idx, list_plan, 1),
        .box_incref => {
            const box_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr_local);
            try self.emitDataPtrIncrefByLocal(box_ptr_local, count_local.?);
        },
        .box_decref => |box_plan| {
            const box_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr_local);
            if (box_plan.child) |child_key| {
                try self.emitBuiltinInternalBoxChildDropIfUnique(box_ptr_local, child_key);
            }
            try self.emitDataPtrDecref(box_ptr_local, box_plan.elem_alignment, box_plan.child != null);
        },
        .box_free => |box_plan| {
            const box_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr_local);
            if (box_plan.child) |child_key| {
                try self.emitBuiltinInternalBoxChildDropIfUnique(box_ptr_local, child_key);
            }
            try self.emitDataPtrFree(box_ptr_local, box_plan.elem_alignment, box_plan.child != null);
        },
        .struct_ => |struct_plan| {
            const field_count = self.getLayoutStore().rcHelperStructFieldCount(struct_plan);
            var i: u32 = 0;
            while (i < field_count) : (i += 1) {
                const field_plan = self.getLayoutStore().rcHelperStructFieldPlan(struct_plan, i) orelse continue;
                const field_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalGet(value_ptr_local);
                if (field_plan.offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_plan.offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                try self.emitLocalSet(field_ptr_local);
                try self.emitRawRcHelperCallByKey(field_plan.child, field_ptr_local, count_local);
            }
        },
        .tag_union => |tag_plan| {
            const variant_count = self.getLayoutStore().rcHelperTagUnionVariantCount(tag_plan);
            if (variant_count == 0) return;

            if (variant_count == 1) {
                if (self.getLayoutStore().rcHelperTagUnionVariantPlan(tag_plan, 0)) |child_key| {
                    try self.emitRawRcHelperCallByKey(child_key, value_ptr_local, count_local);
                }
                return;
            }

            const disc_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const ls = self.getLayoutStore();
            const tu_layout = WasmLayout.tagUnionLayoutWithStore(tag_plan.tag_union_idx, ls);
            const disc_size = tu_layout.discriminant_size;
            if (disc_size == 0) {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            } else {
                try self.emitLocalGet(value_ptr_local);
                try self.emitLoadBySize(disc_size, @intCast(tu_layout.discriminant_offset));
            }
            try self.emitLocalSet(disc_local);

            var variant_i: u32 = 0;
            while (variant_i < variant_count) : (variant_i += 1) {
                const child_key = self.getLayoutStore().rcHelperTagUnionVariantPlan(tag_plan, variant_i) orelse continue;
                self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
                self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

                try self.emitLocalGet(disc_local);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(variant_i)) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

                try self.emitRawRcHelperCallByKey(child_key, value_ptr_local, count_local);

                self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            }
        },
        .closure => |child_key| {
            const captures_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(captures_ptr_local);
            try self.emitRawRcHelperCallByKey(child_key, captures_ptr_local, count_local);
        },
    }
}

fn compileBuiltinInternalRcHelper(self: *Self, helper_key: RcHelperKey) Allocator.Error!u32 {
    const cache_key = helper_key.encode();
    if (self.rc_helper_funcs.get(cache_key)) |func_idx| {
        return func_idx;
    }

    const helper_plan = self.getLayoutStore().rcHelperPlan(helper_key);
    if (helper_plan == .noop) {
        if (builtin.mode == .Debug) {
            std.debug.panic("WASM/codegen invariant violated: attempted to compile noop RC helper for layout {d}", .{@intFromEnum(helper_key.layout_idx)});
        }
        unreachable;
    }

    const param_types: []const ValType = switch (helper_key.op) {
        .incref => &.{ .i32, .i32, .i32 },
        .decref, .free => &.{ .i32, .i32 },
    };
    const type_idx = try self.internFuncType(param_types, &.{});
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;
    try self.rc_helper_funcs.put(cache_key, func_idx);

    const saved = try self.saveState();

    self.body = .empty;
    self.storage.locals = std.AutoHashMap(u64, Storage.LocalInfo).init(self.allocator);
    self.storage.next_local_idx = 0;
    self.storage.local_types = .empty;
    self.stack_frame_size = 0;
    self.uses_stack_memory = false;
    self.fp_local = 0;
    self.proc_return_local = 0;
    self.cf_depth = 0;
    self.in_proc = false;
    self.current_proc_id = null;

    const value_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const count_local = switch (helper_key.op) {
        .incref => self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .decref, .free => null,
    };
    self.roc_ops_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.fp_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitLocalGet(value_ptr_local);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.generateBuiltinInternalRcHelperBody(helper_key, value_ptr_local, count_local);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);
    try self.encodeLocalsDecl(&func_body, @intCast(param_types.len));

    if (self.uses_stack_memory) {
        func_body.append(self.allocator, Op.global_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
        func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &func_body, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        func_body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
        func_body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, self.fp_local) catch return error.OutOfMemory;
        func_body.append(self.allocator, Op.global_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
    }

    func_body.appendSlice(self.allocator, self.body.items) catch return error.OutOfMemory;

    if (self.uses_stack_memory) {
        func_body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, self.fp_local) catch return error.OutOfMemory;
        func_body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &func_body, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        func_body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        func_body.append(self.allocator, Op.global_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
    }

    func_body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.module.setFunctionBody(func_idx, func_body.items) catch return error.OutOfMemory;
    self.restoreState(saved);
    return func_idx;
}

/// Generate a cascading if/else chain from IfBranch array + final_else.
fn emitIfChain(self: *Self, branches: []const LIR.IfBranch, final_else: ProcLocalId, bt: BlockType) Allocator.Error!void {
    if (branches.len == 0) {
        // No branches remain; emit the final else value directly.
        try self.emitProcLocal(final_else);
        return;
    }

    // Generate first branch condition
    try self.emitProcLocal(branches[0].cond);
    // if (block_type)
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
    self.pushStructuredControlFrame();
    defer self.popStructuredControlFrame();
    // then body
    try self.emitProcLocal(branches[0].body);
    // else
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    // Remaining branches become nested if/else, or just the final_else
    if (branches.len > 1) {
        try self.emitIfChain(branches[1..], final_else, bt);
    } else {
        try self.emitProcLocal(final_else);
    }
    // end
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Helper predicates for statement-oriented control lowering.
fn isUnsignedLayout(layout_idx: layout.Idx) bool {
    return switch (layout_idx) {
        .u8, .u16, .u32, .u64, .u128 => true,
        else => false,
    };
}

fn getOrAllocTypedLocal(self: *Self, local_id: ProcLocalId, val_type: ValType) Allocator.Error!u32 {
    if (self.storage.getLocalInfo(local_id)) |info| {
        if (info.val_type == val_type) {
            return info.idx;
        }
    }

    return self.storage.allocLocal(local_id, val_type);
}

fn recordProcLocal(locals: *std.AutoHashMap(u64, void), local: ProcLocalId) Allocator.Error!void {
    _ = try locals.getOrPut(@intFromEnum(local));
}

fn recordRefOpLocals(locals: *std.AutoHashMap(u64, void), op: RefOp) Allocator.Error!void {
    switch (op) {
        .local => |local| try recordProcLocal(locals, local),
        .discriminant => |disc| try recordProcLocal(locals, disc.source),
        .field => |field| try recordProcLocal(locals, field.source),
        .tag_payload => |payload| try recordProcLocal(locals, payload.source),
        .tag_payload_struct => |payload| try recordProcLocal(locals, payload.source),
        .list_reinterpret => |list_bridge| try recordProcLocal(locals, list_bridge.backing_ref),
        .nominal => |nominal| try recordProcLocal(locals, nominal.backing_ref),
    }
}

fn collectProcLocals(
    self: *Self,
    stmt_id: CFStmtId,
    locals: *std.AutoHashMap(u64, void),
    visited: *std.AutoHashMap(u32, void),
) Allocator.Error!void {
    const gop = try visited.getOrPut(@intFromEnum(stmt_id));
    if (gop.found_existing) return;

    switch (self.store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| {
            try recordProcLocal(locals, assign.target);
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .assign_ref => |assign| {
            try recordProcLocal(locals, assign.target);
            try recordRefOpLocals(locals, assign.op);
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .assign_literal => |assign| {
            try recordProcLocal(locals, assign.target);
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .assign_call => |assign| {
            try recordProcLocal(locals, assign.target);
            for (self.store.getLocalSpan(assign.args)) |arg| try recordProcLocal(locals, arg);
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .assign_call_indirect => |assign| {
            try recordProcLocal(locals, assign.target);
            try recordProcLocal(locals, assign.closure);
            for (self.store.getLocalSpan(assign.args)) |arg| try recordProcLocal(locals, arg);
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .assign_low_level => |assign| {
            try recordProcLocal(locals, assign.target);
            for (self.store.getLocalSpan(assign.args)) |arg| try recordProcLocal(locals, arg);
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .assign_list => |assign| {
            try recordProcLocal(locals, assign.target);
            for (self.store.getLocalSpan(assign.elems)) |elem| try recordProcLocal(locals, elem);
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .assign_struct => |assign| {
            try recordProcLocal(locals, assign.target);
            for (self.store.getLocalSpan(assign.fields)) |field| try recordProcLocal(locals, field);
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .assign_tag => |assign| {
            try recordProcLocal(locals, assign.target);
            if (assign.payload) |payload| {
                try recordProcLocal(locals, payload);
            }
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .set_local => |assign| {
            try recordProcLocal(locals, assign.target);
            try recordProcLocal(locals, assign.value);
            try self.collectProcLocals(assign.next, locals, visited);
        },
        .debug => |debug_stmt| {
            try recordProcLocal(locals, debug_stmt.message);
            try self.collectProcLocals(debug_stmt.next, locals, visited);
        },
        .expect => |expect_stmt| {
            try recordProcLocal(locals, expect_stmt.condition);
            try self.collectProcLocals(expect_stmt.next, locals, visited);
        },
        .runtime_error => {},
        .switch_stmt => |switch_stmt| {
            try recordProcLocal(locals, switch_stmt.cond);
            for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try self.collectProcLocals(branch.body, locals, visited);
            }
            try self.collectProcLocals(switch_stmt.default_branch, locals, visited);
        },
        .for_list => |for_stmt| {
            try recordProcLocal(locals, for_stmt.elem);
            try recordProcLocal(locals, for_stmt.iterable);
            try self.collectProcLocals(for_stmt.body, locals, visited);
            try self.collectProcLocals(for_stmt.next, locals, visited);
        },
        .borrow_scope => |scope| {
            try self.collectProcLocals(scope.body, locals, visited);
            try self.collectProcLocals(scope.remainder, locals, visited);
        },
        .scope_exit => {},
        .join => |join_stmt| {
            for (self.store.getLocalSpan(join_stmt.params)) |param| try recordProcLocal(locals, param);
            try self.collectProcLocals(join_stmt.body, locals, visited);
            try self.collectProcLocals(join_stmt.remainder, locals, visited);
        },
        .jump => |jump_stmt| {
            for (self.store.getLocalSpan(jump_stmt.args)) |arg| try recordProcLocal(locals, arg);
        },
        .ret => |ret_stmt| try recordProcLocal(locals, ret_stmt.value),
        .incref => |inc| {
            try recordProcLocal(locals, inc.value);
            try self.collectProcLocals(inc.next, locals, visited);
        },
        .decref => |dec| {
            try recordProcLocal(locals, dec.value);
            try self.collectProcLocals(dec.next, locals, visited);
        },
        .free => |free_stmt| {
            try recordProcLocal(locals, free_stmt.value);
            try self.collectProcLocals(free_stmt.next, locals, visited);
        },
        .crash => {},
        .loop_continue => {},
    }
}

fn prebindProcLocals(self: *Self, proc: LirProcSpec) Allocator.Error!void {
    var locals = std.AutoHashMap(u64, void).init(self.allocator);
    defer locals.deinit();
    var visited = std.AutoHashMap(u32, void).init(self.allocator);
    defer visited.deinit();

    for (self.store.getLocalSpan(proc.args)) |arg| try recordProcLocal(&locals, arg);
    try self.collectProcLocals(requireProcBody(proc), &locals, &visited);

    var it = locals.iterator();
    while (it.next()) |entry| {
        const local_id: ProcLocalId = @enumFromInt(@as(u32, @intCast(entry.key_ptr.*)));
        if (self.storage.getLocal(local_id) != null) continue;
        const vt = self.procLocalValType(local_id);
        _ = try self.storage.allocLocal(local_id, vt);
    }
}

/// Convert a ValType to the corresponding BlockType for structured control flow.
fn pushStructuredControlFrame(self: *Self) void {
    self.structured_control_depth += 1;
}

fn popStructuredControlFrame(self: *Self) void {
    std.debug.assert(self.structured_control_depth > 0);
    self.structured_control_depth -= 1;
}

fn procLocalLayoutIdx(self: *Self, value: ProcLocalId) layout.Idx {
    return self.store.getLocal(value).layout_idx;
}

/// Infer the wasm ValType that an explicit local value will push onto the stack.
fn procLocalValType(self: *Self, value: ProcLocalId) ValType {
    return self.resolveValType(self.procLocalLayoutIdx(value));
}

fn emitCanonicalizeScalarForLayout(self: *Self, layout_idx: layout.Idx) Allocator.Error!void {
    if (self.resolveValType(layout_idx) != .i32) return;

    switch (layout_idx) {
        .i8 => self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory,
        .i16 => self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory,
        .u8 => {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .u16 => {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFFFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        else => {},
    }
}

/// Get the byte size of the value a local produces.
fn procLocalByteSize(self: *Self, value: ProcLocalId) u32 {
    return self.layoutByteSize(self.procLocalLayoutIdx(value));
}

/// Check if a local produces a composite value (stored in stack memory).
fn isCompositeLocal(self: *const Self, value: ProcLocalId) bool {
    return self.isCompositeLayout(self.store.getLocal(value).layout_idx);
}

/// Check if a layout represents a composite type stored in stack memory.
fn isCompositeLayout(self: *const Self, layout_idx: layout.Idx) bool {
    const repr = WasmLayout.wasmReprWithStore(layout_idx, self.getLayoutStore());
    return switch (repr) {
        .stack_memory => |s| s > 0,
        .primitive => false,
    };
}

fn unwrapSingleFieldPayloadLayout(self: *const Self, layout_idx: layout.Idx) ?layout.Idx {
    const ls = self.getLayoutStore();
    const layout_val = ls.getLayout(layout_idx);
    if (layout_val.tag != .struct_) return null;

    const struct_data = ls.getStructData(layout_val.data.struct_.idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    if (fields.len != 1) return null;

    const field = fields.get(0);
    if (field.index != 0) return null;
    return field.layout;
}

fn shiftBitWidth(layout_idx: layout.Idx) u32 {
    return switch (layout_idx) {
        .i8, .u8 => 8,
        .i16, .u16 => 16,
        .i32, .u32 => 32,
        .i64, .u64 => 64,
        else => 32,
    };
}

fn shiftNeedsZeroFillMask(layout_idx: layout.Idx) bool {
    return switch (layout_idx) {
        .i8, .i16 => true,
        else => false,
    };
}

/// Generate structural equality comparison for two composite values (records, tuples, tag unions).
/// Uses layout-aware comparison for fields containing heap types (strings, lists).
/// Leaves an i32 (bool) on the stack: 1 for equal, 0 for not equal.
fn emitStructuralEq(self: *Self, lhs: ProcLocalId, rhs: ProcLocalId, negate: bool) Allocator.Error!void {
    // Check for string/list type via layout
    {
        const lay_idx = self.procLocalLayoutIdx(lhs);
        if (lay_idx == .str) {
            try self.emitStrEq(lhs, rhs, negate);
            return;
        }
        const ls = self.getLayoutStore();
        const l = ls.getLayout(lay_idx);
        if (l.tag == .list or l.tag == .list_of_zst) {
            try self.emitListEq(lhs, rhs, lay_idx, negate);
            return;
        }
    }

    // Generate both operand expressions — each pushes an i32 pointer
    try self.emitProcLocal(lhs);
    const lhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_local);

    try self.emitProcLocal(rhs);
    const rhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(rhs_local);

    // Try layout-aware comparison for records/tuples/tag unions containing heap types
    {
        const lay_idx = self.procLocalLayoutIdx(lhs);
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
    const byte_size = self.procLocalByteSize(lhs);
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
                const field_offset = self.structFieldOffsetBySortedIndexWasm(struct_idx, @intCast(field_i));
                const field_size = self.structFieldSizeBySortedIndexWasm(struct_idx, @intCast(field_i));
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
            // Non-composite layouts compare directly as raw bytes.
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

    const tu_layout = WasmLayout.tagUnionLayoutWithStore(l.data.tag_union.idx, ls);
    const disc_offset = tu_layout.discriminant_offset;
    const disc_size = tu_layout.discriminant_size;

    // Allocate a local to hold the result
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // Load LHS discriminant
    if (disc_size == 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    } else {
        try self.emitLocalGet(lhs_local);
        try self.emitLoadBySize(disc_size, @intCast(disc_offset));
    }
    const lhs_disc = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_disc);

    // Load RHS discriminant
    if (disc_size == 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    } else {
        try self.emitLocalGet(rhs_local);
        try self.emitLoadBySize(disc_size, @intCast(disc_offset));
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
    const variants = ls.getTagUnionVariants(ls.getTagUnionData(l.data.tag_union.idx));
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
            } else if (builtinInternalLayoutContainsRefcounted(ls, "wasm.compareFieldByLayout.builtin_elem_rc", elem_layout)) {
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
fn emitCompositeNumericOp(self: *Self, op: anytype, args: []const ProcLocalId, ret_layout: layout.Idx, operand_layout: layout.Idx) Allocator.Error!void {
    // For comparison ops like num_is_eq, check for structural equality first
    if (op == .num_is_eq) {
        try self.emitStructuralEq(args[0], args[1], false);
        return;
    }

    // Generate operand pointers and stabilize them
    try self.emitProcLocal(args[0]);
    const lhs_local = try self.stabilizeCompositeResult(16);

    if (args.len > 1) {
        try self.emitProcLocal(args[1]);
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
            .num_is_gt => try self.emitI128Compare(lhs_local, rhs_local, .gt),
            .num_is_gte => try self.emitI128Compare(lhs_local, rhs_local, .gte),
            .num_is_lt => try self.emitI128Compare(lhs_local, rhs_local, .lt),
            .num_is_lte => try self.emitI128Compare(lhs_local, rhs_local, .lte),
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
            .num_negate => try self.emitCompositeI128Negate(args[0], ret_layout),
            .num_abs => {
                if (operand_layout == .u128) {
                    try self.emitProcLocal(args[0]);
                } else {
                    try self.emitCompositeI128Abs(args[0]);
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
/// Emit i128/u128 shift operation. LHS is composite (16 bytes), RHS is U8 (i32 on wasm stack).
/// Uses wasm structured if/else to handle shift amounts >= 64.
/// Pushes an i32 pointer to the 16-byte result on the wasm stack.
fn emitI128Shift(self: *Self, op: anytype, args: []const ProcLocalId) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_local);

    // Load LHS low and high words into locals
    try self.emitProcLocal(args[0]);
    const lhs_local = try self.stabilizeCompositeResult(16);

    const a_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalGet(lhs_local);
    try self.emitLoadOp(.i64, 0);
    try self.emitLocalSet(a_low);

    const a_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalGet(lhs_local);
    try self.emitLoadOp(.i64, 8);
    try self.emitLocalSet(a_high);

    // Load shift amount (U8 -> i32 on wasm stack) and extend to i64
    try self.emitProcLocal(args[1]);
    const shift_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    try self.emitLocalSet(shift_local);

    // Locals for result
    const r_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const r_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;

    // Branch: if shift >= 64
    try self.emitLocalGet(shift_local);
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_ge_u) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.void)) catch return error.OutOfMemory;

    // === shift >= 64 path ===
    switch (op) {
        .num_shift_left_by => {
            // r_low = 0, r_high = a_low << (shift - 64)
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(shift_local);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
        },
        .num_shift_right_by => {
            // r_high = a_high >> 63 (sign extend), r_low = a_high >> (shift - 64)  [arithmetic]
            try self.emitLocalGet(a_high);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 63) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
        },
        .num_shift_right_zf_by => {
            // r_high = 0, r_low = a_high >> (shift - 64)  [logical]
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
        },
        else => unreachable,
    }

    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;

    // === shift < 64 path ===
    // inv = 64 - shift
    const inv_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 64) catch return error.OutOfMemory;
    try self.emitLocalGet(shift_local);
    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(inv_local);

    switch (op) {
        .num_shift_left_by => {
            // r_low = a_low << shift
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(shift_local);
            self.body.append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
            // r_high = (a_high << shift) | (a_low >> inv)
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.body.append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(inv_local);
            self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_or) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
        },
        .num_shift_right_by => {
            // r_high = a_high >> shift  [arithmetic]
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
            // r_low = (a_low >> shift) | (a_high << inv)
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(shift_local);
            self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(inv_local);
            self.body.append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_or) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
        },
        .num_shift_right_zf_by => {
            // r_high = a_high >> shift  [logical]
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
            // r_low = (a_low >> shift) | (a_high << inv)
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(shift_local);
            self.body.append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(inv_local);
            self.body.append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_or) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
        },
        else => unreachable,
    }

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Store results
    try self.emitLocalGet(result_local);
    try self.emitLocalGet(r_low);
    try self.emitStoreOp(.i64, 0);
    try self.emitLocalGet(result_local);
    try self.emitLocalGet(r_high);
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    try self.emitLocalGet(result_local);
}

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

/// Emit signed i128 comparison. Pushes i32 (0 or 1) result.
fn emitI128Compare(self: *Self, lhs_local: u32, rhs_local: u32, cmp_op: I128CmpOp) Allocator.Error!void {
    return self.emitI128CompareWithSignedness(lhs_local, rhs_local, cmp_op, true);
}

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
fn emitCompositeI128Negate(self: *Self, expr: ProcLocalId, _: layout.Idx) Allocator.Error!void {
    try self.emitProcLocal(expr);
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

fn emitCompositeI128Abs(self: *Self, expr: ProcLocalId) Allocator.Error!void {
    try self.emitProcLocal(expr);
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

fn emitI64MulToI128Signed(self: *Self, a_local: u32, b_local: u32) Allocator.Error!void {
    const is_neg = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_lt_s) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, is_neg) catch return error.OutOfMemory;

    const abs_val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, is_neg) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.i64)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, a_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, abs_val) catch return error.OutOfMemory;

    try self.emitI64MulToI128(abs_val, b_local);
    const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_ptr) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, is_neg) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;
    try self.emitCompositeI128NegateFromLocal(result_ptr);
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result_ptr) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Emit i128 signed division: result = a / b (truncating).
/// Takes two i32 pointers to 16-byte i128 values.
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

fn internFuncType(self: *Self, params: []const ValType, results: []const ValType) Allocator.Error!u32 {
    self.func_type_key_scratch.clearRetainingCapacity();
    try self.func_type_key_scratch.append(self.allocator, @intCast(params.len));
    for (params) |param| {
        try self.func_type_key_scratch.append(self.allocator, @intFromEnum(param));
    }
    try self.func_type_key_scratch.append(self.allocator, @intCast(results.len));
    for (results) |result| {
        try self.func_type_key_scratch.append(self.allocator, @intFromEnum(result));
    }

    if (self.func_type_cache.get(self.func_type_key_scratch.items)) |existing| {
        return existing;
    }

    const type_idx = try self.module.addFuncType(params, results);
    const key = try self.allocator.dupe(u8, self.func_type_key_scratch.items);
    try self.func_type_cache.put(key, type_idx);
    return type_idx;
}

/// Compile all LIR procs as separate wasm functions.
pub fn compileAllProcSpecs(self: *Self, proc_specs: []const LirProcSpec) Allocator.Error!void {
    // Two-pass compilation to support mutual recursion.
    // Pass 1: Register ALL proc_specs (create function types, get func_idx).
    // This ensures that when compiling any proc body, all sibling proc_specs
    // are already known and can be called without triggering recursive compilation.
    for (proc_specs, 0..) |proc, i| {
        try self.registerProcSpec(@enumFromInt(@as(u32, @intCast(i))), proc);
    }
    try self.buildProcArgCountsTable(proc_specs);
    // Pass 2: Compile proc bodies.
    for (proc_specs, 0..) |proc, i| {
        try self.compileProcSpecBody(@enumFromInt(@as(u32, @intCast(i))), proc);
    }
}

fn buildProcArgCountsTable(self: *Self, proc_specs: []const LirProcSpec) Allocator.Error!void {
    const table_len: usize = self.module.table_func_indices.items.len;
    if (table_len == 0) return;

    const counts = try self.allocator.alloc(u32, table_len);
    defer self.allocator.free(counts);
    @memset(counts, 0);

    for (proc_specs, 0..) |proc, i| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(i)));
        const key: u32 = @intFromEnum(proc_id);
        const table_idx = self.proc_table_indices.get(key) orelse continue;
        counts[table_idx] = @intCast(self.store.getLocalSpan(proc.args).len);
    }

    const byte_len = table_len * 4;
    const bytes = try self.allocator.alloc(u8, byte_len);
    defer self.allocator.free(bytes);
    for (counts, 0..) |count, idx| {
        const offset = idx * 4;
        std.mem.writeInt(u32, bytes[offset..][0..4], count, .little);
    }

    self.proc_arg_counts_offset = try self.module.addDataSegment(bytes, 4);
}

/// Compile a single LirProcSpec as a wasm function.
/// Does NOT compile the body — that's done by compileProcSpecBody.
fn registerProcSpec(self: *Self, proc_id: LIR.LirProcSpecId, proc: LirProcSpec) Allocator.Error!void {
    if (proc.hosted != null) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "WASM/codegen invariant violated: hosted procs are not yet supported in statement-only wasm codegen ({d})",
                .{proc.name.raw()},
            );
        }
        unreachable;
    }

    const key: u32 = @intFromEnum(proc_id);

    // Build parameter types: roc_ops_ptr first, then explicit proc args.
    const args = self.store.getLocalSpan(proc.args);
    var param_types: std.ArrayList(ValType) = .empty;
    defer param_types.deinit(self.allocator);

    param_types.append(self.allocator, .i32) catch return error.OutOfMemory;
    for (args) |arg| {
        const vt = self.resolveValType(self.store.getLocal(arg).layout_idx);
        param_types.append(self.allocator, vt) catch return error.OutOfMemory;
    }

    const ret_vt = self.resolveValType(proc.ret_layout);
    const type_idx = try self.internFuncType(param_types.items, &.{ret_vt});
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;
    const table_idx = self.module.addTableElement(func_idx) catch return error.OutOfMemory;

    self.registered_procs.put(key, func_idx) catch return error.OutOfMemory;
    self.proc_table_indices.put(key, table_idx) catch return error.OutOfMemory;
}

/// Compile a proc body. The proc must already be registered via registerProcSpec.
fn compileProcSpecBody(self: *Self, proc_id: LIR.LirProcSpecId, proc: LirProcSpec) Allocator.Error!void {
    const key: u32 = @intFromEnum(proc_id);

    // Get the pre-registered func_idx (must exist — registerProcSpec runs in pass 1)
    const func_idx = self.registered_procs.get(key) orelse unreachable;

    const args = self.store.getLocalSpan(proc.args);
    const ret_vt = self.resolveValType(proc.ret_layout);

    // Save current codegen state
    const saved = self.saveState() catch return error.OutOfMemory;

    // Initialize fresh state with ALL registered proc_specs (for mutual recursion)
    self.body = .empty;
    self.storage.locals = std.AutoHashMap(u64, Storage.LocalInfo).init(self.allocator);
    self.storage.next_local_idx = 0;
    self.storage.local_types = .empty;
    self.current_proc_id = proc_id;
    // Note: registered_procs is NOT cleared — all pre-registered proc_specs remain
    // visible. This is critical for mutual recursion: when compiling is_even's
    // body, calls to is_odd must find its func_idx without re-compilation.
    self.stack_frame_size = 0;
    self.uses_stack_memory = false;
    self.fp_local = 0;
    self.proc_return_local = 0;
    self.cf_depth = 0;
    self.in_proc = true;

    // Local 0 = roc_ops_ptr parameter
    self.roc_ops_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // Bind parameters to locals (starting at local 1 after roc_ops_ptr).
    for (args) |arg| {
        const local = self.store.getLocal(arg);
        const vt = self.resolveValType(local.layout_idx);
        _ = self.storage.allocLocal(arg, vt) catch return error.OutOfMemory;
    }

    try self.prebindProcLocals(proc);

    // Pre-allocate frame pointer local (after params, so it doesn't conflict)
    self.fp_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.proc_return_local = self.storage.allocAnonymousLocal(ret_vt) catch return error.OutOfMemory;

    // Emit proc body block (ret branches to this block after storing the return local)
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.cf_depth = 1; // inside the ret block

    // Generate CFStmt body
    self.generateCFStmt(requireProcBody(proc)) catch |err| {
        self.restoreState(saved);
        return err;
    };

    // End of ret block
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Build function body
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    // Locals declaration (beyond parameters: 1 roc_ops_ptr + params)
    try self.encodeLocalsDecl(&func_body, @intCast(1 + args.len));

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
    }

    // Push the stored proc return value as the function result.
    func_body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &func_body, self.proc_return_local) catch return error.OutOfMemory;

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

fn requireProcBody(proc: LirProcSpec) LIR.CFStmtId {
    return proc.body orelse std.debug.panic(
        "WASM/codegen invariant violated: non-hosted proc {d} missing statement body",
        .{proc.name.raw()},
    );
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
    proc_return_local: u32,
    cf_depth: u32,
    in_proc: bool,
    current_proc_id: ?LIR.LirProcSpecId,
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
        .proc_return_local = self.proc_return_local,
        .cf_depth = self.cf_depth,
        .in_proc = self.in_proc,
        .current_proc_id = self.current_proc_id,
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
    self.proc_return_local = saved.proc_return_local;
    self.cf_depth = saved.cf_depth;
    self.in_proc = saved.in_proc;
    self.current_proc_id = saved.current_proc_id;
}

/// Generate code for a control flow statement (used in LirProcSpec bodies).
fn generateCFStmt(self: *Self, stmt_id: CFStmtId) Allocator.Error!void {
    const stmt_key = @intFromEnum(stmt_id);
    if (builtin.mode == .Debug) {
        const gop = try self.stmt_generation_counts.getOrPut(stmt_key);
        if (gop.found_existing) {
            gop.value_ptr.* += 1;
            if (gop.value_ptr.* > 32) {
                const stmt = self.store.getCFStmt(stmt_id);
                std.debug.panic(
                    "WASM/codegen excessive generateCFStmt duplication on stmt {d} kind {s} count {d}",
                    .{ stmt_key, @tagName(stmt), gop.value_ptr.* },
                );
            }
        } else {
            gop.value_ptr.* = 1;
        }
        if (self.active_stmt_generations.contains(stmt_key)) {
            std.debug.panic(
                "WASM/codegen recursive generateCFStmt re-entry on stmt {d}",
                .{stmt_key},
            );
        }
        try self.active_stmt_generations.put(stmt_key, {});
        defer _ = self.active_stmt_generations.remove(stmt_key);
    }

    const stmt = self.store.getCFStmt(stmt_id);
    switch (stmt) {
        .assign_symbol => |assign| {
            std.debug.panic(
                "WasmCodeGen invariant violated: assign_symbol for symbol {d} is not implemented in statement-only wasm codegen yet",
                .{assign.symbol.raw()},
            );
        },
        .assign_ref => |assign| {
            try self.generateRefOp(assign.op, self.procLocalLayoutIdx(assign.target));
            try self.bindAssignedLocal(assign.target);
            try self.generateCFStmt(assign.next);
        },
        .assign_literal => |assign| {
            try self.generateLiteral(assign.value);
            try self.bindAssignedLocal(assign.target);
            try self.generateCFStmt(assign.next);
        },
        .assign_call => |assign| {
            try self.generateCall(.{
                .proc = assign.proc,
                .args = assign.args,
                .ret_layout = self.procLocalLayoutIdx(assign.target),
            });
            try self.bindAssignedLocal(assign.target);
            try self.generateCFStmt(assign.next);
        },
        .assign_call_indirect => |assign| {
            try self.generateCallIndirect(.{
                .closure = assign.closure,
                .args = assign.args,
                .ret_layout = self.procLocalLayoutIdx(assign.target),
                .capture_layout = assign.capture_layout,
            });
            try self.bindAssignedLocal(assign.target);
            try self.generateCFStmt(assign.next);
        },
        .assign_low_level => |assign| {
            try self.generateLowLevel(.{
                .op = assign.op,
                .args = assign.args,
                .ret_layout = self.procLocalLayoutIdx(assign.target),
            });
            try self.bindAssignedLocal(assign.target);
            try self.generateCFStmt(assign.next);
        },
        .assign_list => |assign| {
            try self.generateList(.{
                .elems = assign.elems,
                .elem_layout = self.listElemLayout(self.procLocalLayoutIdx(assign.target)),
            });
            try self.bindAssignedLocal(assign.target);
            try self.generateCFStmt(assign.next);
        },
        .assign_struct => |assign| {
            try self.generateStruct(.{
                .fields = assign.fields,
                .struct_layout = self.procLocalLayoutIdx(assign.target),
            });
            try self.bindAssignedLocal(assign.target);
            try self.generateCFStmt(assign.next);
        },
        .assign_tag => |assign| {
            try self.generateTag(.{
                .union_layout = self.procLocalLayoutIdx(assign.target),
                .discriminant = assign.discriminant,
                .payload = assign.payload,
            });
            try self.bindAssignedLocal(assign.target);
            try self.generateCFStmt(assign.next);
        },
        .set_local => |assign| {
            try self.emitProcLocal(assign.value);
            try self.emitLocalSet(try self.getOrAllocTypedLocal(assign.target, self.procLocalValType(assign.target)));
            try self.generateCFStmt(assign.next);
        },
        .debug => |debug_stmt| {
            try self.emitRocDbg(debug_stmt.message);
            try self.generateCFStmt(debug_stmt.next);
        },
        .expect => |expect_stmt| {
            const condition_vt = self.procLocalValType(expect_stmt.condition);
            try self.emitProcLocal(expect_stmt.condition);
            switch (condition_vt) {
                .i32 => {},
                .i64 => {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i64_ne) catch return error.OutOfMemory;
                },
                .f32, .f64 => std.debug.panic(
                    "WasmCodeGen invariant violated: expect condition local {d} had non-integer value type {s}",
                    .{ @intFromEnum(expect_stmt.condition), @tagName(condition_vt) },
                ),
            }
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, 0x40) catch return error.OutOfMemory;
            self.cf_depth += 1;

            try self.emitRocStaticStringCall(wasm_roc_ops_expect_failed_offset, "expect failed");

            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;

            try self.generateCFStmt(expect_stmt.next);
        },
        .ret => |r| {
            try self.emitProcLocal(r.value);
            try self.emitLocalSet(self.proc_return_local);
            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, self.cf_depth - 1) catch return error.OutOfMemory;
        },
        .switch_stmt => |sw| {
            const cond_vt = self.procLocalValType(sw.cond);
            const cond_local = self.storage.allocAnonymousLocal(cond_vt) catch return error.OutOfMemory;
            try self.emitProcLocal(sw.cond);
            try self.emitLocalSet(cond_local);
            if (cond_vt == .i32) {
                const cond_layout_idx = self.procLocalLayoutIdx(sw.cond);
                const cond_size = self.layoutStorageByteSize(cond_layout_idx);
                if (cond_size > 0 and cond_size < 4) {
                    const mask: i32 = (@as(i32, 1) << @intCast(cond_size * 8)) - 1;
                    try self.emitLocalGet(cond_local);
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, mask) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                    try self.emitLocalSet(cond_local);
                }
            }

            const branches = self.store.getCFSwitchBranches(sw.branches);

            for (branches) |branch| {
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

                self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.body.append(self.allocator, 0x40) catch return error.OutOfMemory;
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
        .for_list => |for_stmt| {
            try self.emitProcLocal(for_stmt.iterable);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 4);
            try self.emitLocalSet(len_local);

            const data_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(data_local);

            const index_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalSet(index_local);

            self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
            self.body.append(self.allocator, 0x40) catch return error.OutOfMemory;
            self.cf_depth += 1;

            self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
            self.body.append(self.allocator, 0x40) catch return error.OutOfMemory;
            self.cf_depth += 1;

            const saved_loop_depth = self.loop_continue_target_depths.items.len;
            try self.loop_continue_target_depths.append(self.allocator, self.cf_depth);

            try self.emitLocalGet(index_local);
            try self.emitLocalGet(len_local);
            self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

            const elem_layout = for_stmt.iterable_elem_layout;
            const elem_size = self.layoutStorageByteSize(elem_layout);
            if (elem_size == 0) {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                try self.bindAssignedLocal(for_stmt.elem);
            } else {
                try self.emitLocalGet(data_local);
                try self.emitLocalGet(index_local);
                if (elem_size != 1) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                }
                self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

                if (self.isCompositeLayout(elem_layout)) {
                    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitLocalSet(src_local);

                    const elem_align = @max(self.layoutStorageByteAlign(elem_layout), 1);
                    const dst_offset = try self.allocStackMemory(elem_size, elem_align);
                    const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitFpOffset(dst_offset);
                    try self.emitLocalSet(dst_local);

                    try self.emitMemCopy(dst_local, 0, src_local, elem_size);
                    try self.emitLocalGet(dst_local);
                } else {
                    try self.emitLoadOpForLayout(elem_layout, 0);
                }
                try self.bindAssignedLocal(for_stmt.elem);
            }

            try self.emitLocalGet(index_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(index_local);

            try self.generateCFStmt(for_stmt.body);
            self.loop_continue_target_depths.shrinkRetainingCapacity(saved_loop_depth);

            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;

            try self.generateCFStmt(for_stmt.next);
        },
        .join => |j| {
            const jp_key = @intFromEnum(j.id);

            const jp_params = self.store.getLocalSpan(j.params);
            var param_locals = self.allocator.alloc(u32, jp_params.len) catch return error.OutOfMemory;

            for (jp_params, 0..) |param, i| {
                const vt = self.procLocalValType(param);
                const local_idx = self.getOrAllocTypedLocal(param, vt) catch return error.OutOfMemory;
                param_locals[i] = local_idx;
            }
            self.join_point_param_locals.put(jp_key, param_locals) catch return error.OutOfMemory;
            const state_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.join_point_state_locals.put(jp_key, state_local) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalSet(state_local);

            self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
            self.body.append(self.allocator, 0x40) catch return error.OutOfMemory;
            self.cf_depth += 1;

            self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
            self.body.append(self.allocator, 0x40) catch return error.OutOfMemory; // void block type
            self.cf_depth += 1;

            self.join_point_depths.put(jp_key, self.cf_depth) catch return error.OutOfMemory;

            try self.emitLocalGet(state_local);
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, 0x40) catch return error.OutOfMemory;
            self.cf_depth += 1;

            try self.generateCFStmt(j.body);

            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;

            try self.generateCFStmt(j.remainder);

            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;

            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;
        },
        .jump => |jmp| {
            const jp_key = @intFromEnum(jmp.target);
            const args = self.store.getLocalSpan(jmp.args);

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

            var temp_locals = self.allocator.alloc(u32, args.len) catch return error.OutOfMemory;
            defer self.allocator.free(temp_locals);

            for (args, 0..) |arg, i| {
                try self.emitProcLocal(arg);
                const vt = self.procLocalValType(arg);
                const tmp = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
                try self.emitLocalSet(tmp);
                temp_locals[i] = tmp;
            }

            for (0..args.len) |i| {
                try self.emitLocalGet(temp_locals[i]);
                try self.emitLocalSet(param_locals[i]);
            }

            const state_local = self.join_point_state_locals.get(jp_key) orelse unreachable;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            try self.emitLocalSet(state_local);

            const loop_depth = self.join_point_depths.get(jp_key) orelse std.debug.panic(
                "WASM/codegen invariant violated: jump target {d} has no active join-point depth",
                .{jp_key},
            );
            const br_target = self.cf_depth - loop_depth;
            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, br_target) catch return error.OutOfMemory;
        },
        .borrow_scope => |scope| {
            try self.generateCFStmt(scope.body);
            try self.generateCFStmt(scope.remainder);
        },
        .scope_exit => {},
        .incref => |inc| {
            try self.generateRcStmt(.incref, inc.value, inc.count);
            try self.generateCFStmt(inc.next);
        },
        .decref => |dec| {
            try self.generateRcStmt(.decref, dec.value, 1);
            try self.generateCFStmt(dec.next);
        },
        .free => |free_stmt| {
            try self.generateRcStmt(.free, free_stmt.value, 1);
            try self.generateCFStmt(free_stmt.next);
        },
        .runtime_error => {
            var msg_buf: [64]u8 = undefined;
            const proc_id = self.current_proc_id orelse {
                if (comptime builtin.mode == .Debug) {
                    std.debug.panic("runtime_error emitted without current proc", .{});
                }
                unreachable;
            };
            const msg = std.fmt.bufPrint(
                &msg_buf,
                "runtime_error {d} proc {d}",
                .{ @intFromEnum(stmt_id), @intFromEnum(proc_id) },
            ) catch "runtime_error";
            try self.emitRocStaticStringCall(wasm_roc_ops_crashed_offset, msg);
            self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },
        .crash => |crash| {
            const msg_bytes = self.store.getString(crash.msg);
            const data_offset = self.module.addDataSegment(msg_bytes, 1) catch return error.OutOfMemory;

            const crashed_slot = try self.allocStackMemory(8, 4);
            try self.emitFpOffset(crashed_slot);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(data_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

            try self.emitFpOffset(crashed_slot);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(msg_bytes.len)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;

            try self.emitFpOffset(crashed_slot);
            try self.emitLocalGet(self.roc_ops_local);
            self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

            try self.emitLocalGet(self.roc_ops_local);
            self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 24) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.call_indirect) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, self.roc_ops_type_idx) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },
        .loop_continue => {
            if (builtin.mode == .Debug and self.loop_continue_target_depths.items.len == 0) {
                std.debug.panic(
                    "WasmCodeGen invariant violated: loop_continue encountered outside for_list",
                    .{},
                );
            }
            const loop_depth = self.loop_continue_target_depths.items[self.loop_continue_target_depths.items.len - 1];
            const br_target = self.cf_depth - loop_depth;
            self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, br_target) catch return error.OutOfMemory;
        },
    }
}

fn generateLiteral(self: *Self, value: LIR.LiteralValue) Allocator.Error!void {
    switch (value) {
        .i64_literal => |lit| {
            switch (self.resolveValType(lit.layout_idx)) {
                .i32 => {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @truncate(lit.value)) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, lit.value) catch return error.OutOfMemory;
                },
                .f32, .f64 => unreachable,
            }
        },
        .i128_literal => |lit| try self.generateI128Literal(lit.value),
        .f64_literal => |lit| {
            self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            try self.body.writer(self.allocator).writeInt(u64, @bitCast(lit), .little);
        },
        .f32_literal => |lit| {
            self.body.append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
            try self.body.writer(self.allocator).writeInt(u32, @bitCast(lit), .little);
        },
        .dec_literal => |lit| try self.generateI128Literal(lit),
        .str_literal => |str_idx| try self.generateStrLiteral(str_idx),
        .bool_literal => |lit| {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, if (lit) 1 else 0) catch return error.OutOfMemory;
        },
        .null_ptr => {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        },
        .proc_ref => |proc_id| {
            const key: u32 = @intFromEnum(proc_id);
            const table_idx = self.proc_table_indices.get(key) orelse {
                std.debug.panic(
                    "WasmCodeGen invariant violated: proc_ref target {d} missing table index",
                    .{@intFromEnum(proc_id)},
                );
            };
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(table_idx)) catch return error.OutOfMemory;
        },
    }
}

fn emitRocOpsCall(self: *Self, args_slot: u32, table_offset: u32) Allocator.Error!void {
    try self.emitFpOffset(args_slot);

    try self.emitLocalGet(self.roc_ops_local);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, wasm_roc_ops_env_offset) catch return error.OutOfMemory;

    try self.emitLocalGet(self.roc_ops_local);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, table_offset) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.call_indirect) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, self.roc_ops_type_idx) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
}

fn emitRocStaticStringCall(self: *Self, table_offset: u32, msg: []const u8) Allocator.Error!void {
    const data_offset = self.module.addDataSegment(msg, 1) catch return error.OutOfMemory;
    const args_slot = try self.allocStackMemory(8, 4);

    try self.emitFpOffset(args_slot);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(data_offset)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitFpOffset(args_slot);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(msg.len)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;

    try self.emitRocOpsCall(args_slot, table_offset);
}

fn emitRocDbg(self: *Self, message: ProcLocalId) Allocator.Error!void {
    if (self.procLocalLayoutIdx(message) != .str) {
        std.debug.panic(
            "WasmCodeGen invariant violated: debug local {d} did not have Str layout",
            .{@intFromEnum(message)},
        );
    }

    try self.emitProcLocal(message);
    const str_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(str_local);

    const ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitExtractStrPtrLen(str_local, ptr_local, len_local);

    const args_slot = try self.allocStackMemory(8, 4);
    try self.emitFpOffset(args_slot);
    try self.emitLocalGet(ptr_local);
    self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitFpOffset(args_slot);
    try self.emitLocalGet(len_local);
    self.body.append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;

    try self.emitRocOpsCall(args_slot, wasm_roc_ops_dbg_offset);
}

fn generateI128Literal(self: *Self, value: i128) Allocator.Error!void {
    const base_offset = try self.allocStackMemory(16, 8);

    const unsigned: u128 = @bitCast(value);
    const low: i64 = @bitCast(@as(u64, @truncate(unsigned)));
    const high: i64 = @bitCast(@as(u64, @truncate(unsigned >> 64)));

    try self.emitFpOffset(base_offset);
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    try self.emitFpOffset(base_offset);
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, high) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    try self.emitFpOffset(base_offset);
}

fn bindAssignedLocal(self: *Self, target: ProcLocalId) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const runtime_layout = self.runtimeRepresentationLayoutIdx(self.procLocalLayoutIdx(target));
    const repr = WasmLayout.wasmReprWithStore(runtime_layout, ls);
    switch (repr) {
        .stack_memory => |size| {
            if (size > 0) {
                const stable_local = try self.stabilizeCompositeResult(size);
                try self.emitLocalGet(stable_local);
            }
        },
        .primitive => {},
    }
    const vt = self.procLocalValType(target);
    const local_idx = self.getOrAllocTypedLocal(target, vt) catch return error.OutOfMemory;
    try self.emitLocalSet(local_idx);
}

fn generateRefOp(self: *Self, op: RefOp, target_layout: layout.Idx) Allocator.Error!void {
    switch (op) {
        .local => |local| try self.emitProcLocal(local),
        .discriminant => |disc| {
            const ls = self.getLayoutStore();
            const source_layout_idx = self.procLocalLayoutIdx(disc.source);
            const source_layout = ls.getLayout(source_layout_idx);
            const target_vt = self.resolveValType(target_layout);
            const source_vt: ValType = switch (source_layout.tag) {
                .tag_union => blk: {
                    const tu_layout = WasmLayout.tagUnionLayoutWithStore(source_layout.data.tag_union.idx, ls);
                    if (tu_layout.size <= 4 and tu_layout.discriminant_offset == 0) {
                        if (tu_layout.discriminant_size == 0) {
                            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                        } else {
                            try self.emitProcLocal(disc.source);
                            if (tu_layout.discriminant_size < 4) {
                                const mask: i32 = (@as(i32, 1) << @intCast(tu_layout.discriminant_size * 8)) - 1;
                                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                WasmModule.leb128WriteI32(self.allocator, &self.body, mask) catch return error.OutOfMemory;
                                self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                            }
                        }
                        break :blk self.procLocalValType(disc.source);
                    } else {
                        try self.emitProcLocal(disc.source);
                        try self.emitLoadBySize(tu_layout.discriminant_size, @intCast(tu_layout.discriminant_offset));
                        break :blk .i32;
                    }
                },
                .box => blk: {
                    const inner_layout = ls.getLayout(source_layout.data.box);
                    if (inner_layout.tag != .tag_union) {
                        std.debug.panic(
                            "WasmCodeGen invariant violated: discriminant access on boxed non-tag-union layout {s}",
                            .{@tagName(inner_layout.tag)},
                        );
                    }
                    const tu_layout = WasmLayout.tagUnionLayoutWithStore(inner_layout.data.tag_union.idx, ls);
                    try self.emitProcLocal(disc.source);
                    try self.emitLoadBySize(tu_layout.discriminant_size, @intCast(tu_layout.discriminant_offset));
                    break :blk .i32;
                },
                else => blk: {
                    try self.emitProcLocal(disc.source);
                    break :blk self.procLocalValType(disc.source);
                },
            };

            switch (source_vt) {
                .i32 => switch (target_vt) {
                    .i32 => {},
                    .i64 => self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory,
                    else => std.debug.panic(
                        "WasmCodeGen invariant violated: discriminant target layout lowered to non-integer value type {s}",
                        .{@tagName(target_vt)},
                    ),
                },
                .i64 => switch (target_vt) {
                    .i32 => self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory,
                    .i64 => {},
                    else => std.debug.panic(
                        "WasmCodeGen invariant violated: discriminant target layout lowered to non-integer value type {s}",
                        .{@tagName(target_vt)},
                    ),
                },
                else => std.debug.panic(
                    "WasmCodeGen invariant violated: discriminant source layout lowered to non-integer value type {s}",
                    .{@tagName(source_vt)},
                ),
            }
        },
        .field => |field| try self.generateStructAccess(.{
            .struct_expr = field.source,
            .struct_layout = self.procLocalLayoutIdx(field.source),
            .field_idx = field.field_idx,
            .field_layout = target_layout,
        }),
        .tag_payload => |payload| {
            try self.emitProcLocal(payload.source);
            const ls = self.getLayoutStore();
            const source_layout = self.procLocalLayoutIdx(payload.source);
            const union_layout = ls.getLayout(source_layout);
            const payload_layout_idx = switch (union_layout.tag) {
                .tag_union => blk: {
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(union_layout.data.tag_union.idx));
                    break :blk variants.get(payload.tag_discriminant).payload_layout;
                },
                .box => blk: {
                    const inner = ls.getLayout(union_layout.data.box);
                    if (inner.tag != .tag_union) break :blk .zst;
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(inner.data.tag_union.idx));
                    break :blk variants.get(payload.tag_discriminant).payload_layout;
                },
                else => .zst,
            };
            const payload_layout = ls.getLayout(payload_layout_idx);
            if (payload_layout.tag == .struct_) {
                const field_offset = self.structFieldOffsetByOriginalIndexWasm(payload_layout.data.struct_.idx, payload.payload_idx);
                if (field_offset > 0) {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
            } else if (builtin.mode == .Debug and payload.payload_idx != 0) {
                std.debug.panic(
                    "LIR/wasm invariant violated: scalar tag payload access requested payload_idx {d} from non-struct payload",
                    .{payload.payload_idx},
                );
            }
            if (!self.isCompositeLayout(target_layout)) {
                try self.emitLoadOpForLayout(target_layout, 0);
            }
        },
        .tag_payload_struct => |payload| {
            try self.emitProcLocal(payload.source);
            const ls = self.getLayoutStore();
            const source_layout = self.procLocalLayoutIdx(payload.source);
            const union_layout = ls.getLayout(source_layout);
            const payload_layout_idx = switch (union_layout.tag) {
                .tag_union => blk: {
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(union_layout.data.tag_union.idx));
                    break :blk variants.get(payload.tag_discriminant).payload_layout;
                },
                .box => blk: {
                    const inner = ls.getLayout(union_layout.data.box);
                    if (inner.tag != .tag_union) break :blk .zst;
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(inner.data.tag_union.idx));
                    break :blk variants.get(payload.tag_discriminant).payload_layout;
                },
                else => .zst,
            };
            if (!self.isCompositeLayout(target_layout)) {
                try self.emitLoadOpForLayout(target_layout, 0);
            } else if (builtin.mode == .Debug) {
                const payload_layout = ls.getLayout(payload_layout_idx);
                if (payload_layout.tag != .struct_) {
                    std.debug.panic(
                        "LIR/wasm invariant violated: tag_payload_struct access expected struct payload layout, found {s}",
                        .{@tagName(payload_layout.tag)},
                    );
                }
            }
        },
        .list_reinterpret => |list_bridge| try self.emitProcLocal(list_bridge.backing_ref),
        .nominal => |nom| try self.emitProcLocal(nom.backing_ref),
    }
}

fn generateRcStmt(
    self: *Self,
    comptime kind: RcOpKind,
    value: ProcLocalId,
    inc_count: u16,
) Allocator.Error!void {
    try self.emitProcLocal(value);
    const value_local = self.storage.allocAnonymousLocal(self.procLocalValType(value)) catch return error.OutOfMemory;
    try self.emitLocalSet(value_local);
    try self.emitExplicitRcForValueLocal(kind, value_local, self.procLocalValType(value), self.procLocalLayoutIdx(value), inc_count);
}

fn listElemLayout(self: *Self, list_layout_idx: layout.Idx) layout.Idx {
    const ls = self.getLayoutStore();
    const list_layout = ls.getLayout(list_layout_idx);
    return switch (list_layout.tag) {
        .list => self.runtimeRepresentationLayoutIdx(list_layout.data.list),
        .list_of_zst => list_layout_idx,
        else => unreachable,
    };
}

fn runtimeRepresentationLayoutIdx(self: *const Self, layout_idx: layout.Idx) layout.Idx {
    const ls = self.getLayoutStore();
    const layout_val = ls.getLayout(layout_idx);
    return switch (layout_val.tag) {
        .closure => self.runtimeRepresentationLayoutIdx(layout_val.data.closure.captures_layout_idx),
        else => layout_idx,
    };
}

/// Generate code for a function call.
/// In the current pipeline, lowering generates all closure dispatch as generic LIR
/// constructs (discriminant_switch, tag_payload_access, direct calls). The backend
/// just handles explicit direct-call symbols plus the residual runtime
/// function-value expression path. No closure-specific dispatch.
fn generateCall(self: *Self, c: anytype) Allocator.Error!void {
    const proc_key: u32 = @intFromEnum(c.proc);
    const func_idx = self.registered_procs.get(proc_key) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic("generateCall: unresolved proc call target {d}", .{@intFromEnum(c.proc)});
        }
        unreachable;
    };

    try self.emitLocalGet(self.roc_ops_local);
    try self.emitCallArgs(c.args);
    try self.emitCall(func_idx);

    if (self.isCompositeLayout(c.ret_layout)) {
        const result_size = self.layoutByteSize(self.runtimeRepresentationLayoutIdx(c.ret_layout));
        const stable_local = try self.stabilizeCompositeResult(result_size);
        try self.emitLocalGet(stable_local);
    }
}

fn generateCallIndirect(self: *Self, c: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const closure_layout = self.procLocalLayoutIdx(c.closure);
    const closure_layout_val = ls.getLayout(closure_layout);

    var struct_layout_idx = closure_layout;
    switch (closure_layout_val.tag) {
        .box => {
            struct_layout_idx = closure_layout_val.data.box;
            const inner_layout = ls.getLayout(struct_layout_idx);
            if (inner_layout.tag != .struct_) {
                std.debug.panic(
                    "WasmCodeGen invariant violated: indirect call closure layout {d} boxed non-struct layout {d}",
                    .{ @intFromEnum(closure_layout), @intFromEnum(struct_layout_idx) },
                );
            }
        },
        .struct_ => {},
        else => std.debug.panic(
            "WasmCodeGen invariant violated: indirect call closure layout {d} is not struct or boxed struct",
            .{@intFromEnum(closure_layout)},
        ),
    }

    try self.emitProcLocal(c.closure);
    const struct_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(struct_ptr);

    const struct_layout = ls.getLayout(struct_layout_idx);
    const struct_idx = struct_layout.data.struct_.idx;

    const fn_offset = self.structFieldOffsetByOriginalIndexWasm(struct_idx, 0);
    const fn_size = self.structFieldSizeByOriginalIndexWasm(struct_idx, 0);
    const fn_layout = ls.getStructFieldLayoutByOriginalIndex(struct_idx, 0);
    if (fn_layout != .opaque_ptr) {
        std.debug.panic(
            "WasmCodeGen invariant violated: indirect call closure field 0 layout {d} is not opaque_ptr",
            .{@intFromEnum(fn_layout)},
        );
    }

    const fn_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(struct_ptr);
    try self.emitLoadOpSized(.i32, fn_size, fn_offset);
    try self.emitLocalSet(fn_ptr);

    const capture_layout = c.capture_layout;
    const has_capture = capture_layout != null;
    var capture_local: u32 = 0;
    var capture_vt: ValType = .i32;
    if (has_capture) {
        const capture_layout_idx = capture_layout.?;
        if (capture_layout_idx == .zst) {
            capture_vt = self.resolveValType(capture_layout_idx);
            capture_local = self.storage.allocAnonymousLocal(capture_vt) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitLocalSet(capture_local);
        } else {
            const capture_offset = self.structFieldOffsetByOriginalIndexWasm(struct_idx, 1);
            const capture_ptr_size = self.structFieldSizeByOriginalIndexWasm(struct_idx, 1);
            const capture_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(struct_ptr);
            try self.emitLoadOpSized(.i32, capture_ptr_size, capture_offset);
            try self.emitLocalSet(capture_ptr_local);

            const capture_size = self.layoutStorageByteSize(capture_layout_idx);
            capture_vt = self.resolveValType(capture_layout_idx);
            capture_local = self.storage.allocAnonymousLocal(capture_vt) catch return error.OutOfMemory;
            if (self.isCompositeLayout(capture_layout_idx) and capture_size > 0) {
                try self.emitLocalGet(capture_ptr_local);
            } else {
                try self.emitLocalGet(capture_ptr_local);
                try self.emitLoadOpSized(capture_vt, capture_size, 0);
                try self.emitCanonicalizeScalarForLayout(capture_layout_idx);
            }
            try self.emitLocalSet(capture_local);
        }
    }

    const arg_refs = self.store.getLocalSpan(c.args);
    var param_types: std.ArrayList(ValType) = .empty;
    defer param_types.deinit(self.allocator);
    param_types.append(self.allocator, .i32) catch return error.OutOfMemory;
    for (arg_refs) |arg| {
        param_types.append(self.allocator, self.procLocalValType(arg)) catch return error.OutOfMemory;
    }
    if (has_capture) {
        param_types.append(self.allocator, capture_vt) catch return error.OutOfMemory;
    }
    const ret_vt = self.resolveValType(c.ret_layout);
    const type_idx = try self.internFuncType(param_types.items, &.{ret_vt});
    try self.emitLocalGet(self.roc_ops_local);
    try self.emitCallArgs(c.args);
    if (has_capture) {
        try self.emitLocalGet(capture_local);
    }
    try self.emitLocalGet(fn_ptr);
    self.body.append(self.allocator, Op.call_indirect) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, type_idx) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    if (self.isCompositeLayout(c.ret_layout)) {
        const result_size = self.layoutByteSize(self.runtimeRepresentationLayoutIdx(c.ret_layout));
        const stable_local = try self.stabilizeCompositeResult(result_size);
        try self.emitLocalGet(stable_local);
    }
}

/// Emit a call instruction.
fn emitCall(self: *Self, func_idx: u32) Allocator.Error!void {
    try self.body.append(self.allocator, Op.call);
    try WasmModule.leb128WriteU32(self.allocator, &self.body, func_idx);
}

/// Generate call arguments (helper to avoid duplication).
fn emitCallArgs(self: *Self, args: ProcLocalSpan) Allocator.Error!void {
    const arg_refs = self.store.getLocalSpan(args);
    for (arg_refs) |arg| {
        const layout_idx = self.procLocalLayoutIdx(arg);
        if (!self.isCompositeLayout(layout_idx)) {
            try self.emitProcLocal(arg);
            continue;
        }

        try self.emitProcLocal(arg);
        const arg_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(arg_ptr);
        try self.emitLocalGet(arg_ptr);
        self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        const stable_offset = try self.allocStackMemory(
            self.layoutByteSize(self.runtimeRepresentationLayoutIdx(layout_idx)),
            self.layoutStorageByteAlign(layout_idx),
        );
        const stable_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(stable_offset);
        try self.emitLocalSet(stable_ptr);
        try self.emitZeroInit(stable_ptr, self.layoutByteSize(self.runtimeRepresentationLayoutIdx(layout_idx)));
        try self.emitLocalGet(stable_ptr);
        try self.emitLocalSet(arg_ptr);
        self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        try self.emitLocalGet(arg_ptr);
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
            .opaque_ptr => 4,
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
        .tag_union => WasmLayout.tagUnionLayoutWithStore(l.data.tag_union.idx, ls).size,
        .struct_ => WasmLayout.structSizeWithStore(l.data.struct_.idx, ls),
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
            .opaque_ptr => 4,
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
        .tag_union => WasmLayout.tagUnionLayoutWithStore(l.data.tag_union.idx, ls).alignment,
        .struct_ => WasmLayout.structAlignWithStore(l.data.struct_.idx, ls),
        else => self.layoutByteAlign(layout_idx),
    };
}

fn alignUp(value: u32, alignment: u32) u32 {
    const mask = alignment - 1;
    return (value + mask) & ~mask;
}

fn structFieldOffsetByOriginalIndexWasm(self: *const Self, struct_idx: layout.StructIdx, original_idx: u16) u32 {
    const ls = self.getLayoutStore();
    const struct_data = ls.getStructData(struct_idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    var offset: u32 = 0;
    for (0..fields.len) |i| {
        const field = fields.get(i);
        const field_align = self.layoutStorageByteAlign(field.layout);
        offset = alignUp(offset, field_align);
        if (field.index == original_idx) return offset;
        offset += self.layoutStorageByteSize(field.layout);
    }
    unreachable;
}

fn structFieldSizeByOriginalIndexWasm(self: *const Self, struct_idx: layout.StructIdx, original_idx: u16) u32 {
    const ls = self.getLayoutStore();
    const struct_data = ls.getStructData(struct_idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    for (0..fields.len) |i| {
        const field = fields.get(i);
        if (field.index == original_idx) return self.layoutStorageByteSize(field.layout);
    }
    unreachable;
}

fn structFieldOffsetBySortedIndexWasm(self: *const Self, struct_idx: layout.StructIdx, sorted_index: u32) u32 {
    const ls = self.getLayoutStore();
    const struct_data = ls.getStructData(struct_idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    var offset: u32 = 0;
    for (0..fields.len) |i| {
        const field = fields.get(i);
        const field_align = self.layoutStorageByteAlign(field.layout);
        offset = alignUp(offset, field_align);
        if (i == sorted_index) return offset;
        offset += self.layoutStorageByteSize(field.layout);
    }
    unreachable;
}

fn structFieldSizeBySortedIndexWasm(self: *const Self, struct_idx: layout.StructIdx, sorted_index: u32) u32 {
    const ls = self.getLayoutStore();
    const struct_data = ls.getStructData(struct_idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    if (sorted_index >= fields.len) unreachable;
    return self.layoutStorageByteSize(fields.get(sorted_index).layout);
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
/// Allocates stack memory, stores each field by its original semantic index,
/// and returns a pointer to the result.
fn generateStruct(self: *Self, r: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(self.runtimeRepresentationLayoutIdx(r.struct_layout));
    // Empty structs (ZST) have scalar layout, not struct_ — push dummy pointer
    if (l.tag != .struct_) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        return;
    }

    const size = WasmLayout.structSizeWithStore(l.data.struct_.idx, ls);
    if (size == 0) {
        // Zero-sized struct — push dummy pointer
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        return;
    }

    const align_val: u32 = WasmLayout.structAlignWithStore(l.data.struct_.idx, ls);

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
    const fields = self.store.getLocalSpan(r.fields);

    const field_val_locals = self.allocator.alloc(u32, fields.len) catch return error.OutOfMemory;
    defer self.allocator.free(field_val_locals);
    const field_val_types = self.allocator.alloc(ValType, fields.len) catch return error.OutOfMemory;
    defer self.allocator.free(field_val_types);

    for (fields, 0..) |field_expr_id, i| {
        const field_byte_size = self.structFieldSizeByOriginalIndexWasm(l.data.struct_.idx, @intCast(i));
        const field_layout_idx = ls.getStructFieldLayoutByOriginalIndex(l.data.struct_.idx, @intCast(i));
        const is_composite = self.isCompositeLayout(field_layout_idx);
        const field_vt = WasmLayout.resultValTypeWithStore(field_layout_idx, ls);

        // Generate the field expression
        try self.emitProcLocal(field_expr_id);

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
            const expr_vt = self.procLocalValType(field_expr_id);
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
        const field_offset = self.structFieldOffsetByOriginalIndexWasm(l.data.struct_.idx, @intCast(i));
        const field_layout_idx = ls.getStructFieldLayoutByOriginalIndex(l.data.struct_.idx, @intCast(i));
        const field_byte_size = self.structFieldSizeByOriginalIndexWasm(l.data.struct_.idx, @intCast(i));
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

/// Generate a field access from a struct/tuple pointer value.
fn generateStructAccess(self: *Self, sa: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();

    // Generate the struct expression → pushes i32 pointer
    try self.emitProcLocal(sa.struct_expr);
    const struct_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(struct_ptr);

    // Get the field offset
    const struct_layout = ls.getLayout(sa.struct_layout);
    std.debug.assert(struct_layout.tag == .struct_);

    const field_offset = self.structFieldOffsetByOriginalIndexWasm(struct_layout.data.struct_.idx, sa.field_idx);
    const field_byte_size = self.structFieldSizeByOriginalIndexWasm(struct_layout.data.struct_.idx, sa.field_idx);
    const field_layout = ls.getLayout(ls.getStructFieldLayoutByOriginalIndex(struct_layout.data.struct_.idx, sa.field_idx));

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

/// Generate a tag expression with an optional payload.
fn generateTag(self: *Self, t: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(t.union_layout);

    std.debug.assert(l.tag == .tag_union);

    const tu_layout = WasmLayout.tagUnionLayoutWithStore(l.data.tag_union.idx, ls);
    const tu_size = tu_layout.size;
    const disc_offset = tu_layout.discriminant_offset;
    if (tu_size <= 4 and disc_offset == 0) {
        // Small tag union — discriminant only, no payload (enum).
        // Still evaluate payload for side effects (e.g., early_return from ? operator).
        // Payload must be zero-sized since the tag has no payload room.
        if (t.payload) |payload_local| {
            try self.emitProcLocal(payload_local);
            self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
        }
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(t.discriminant)) catch return error.OutOfMemory;
        return;
    }

    const align_val: u32 = tu_layout.alignment;
    const frame_offset = try self.allocStackMemory(tu_size, align_val);

    const base_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(frame_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;

    // Store payload FIRST (payload may overlap discriminant if it is wider than
    // the payload slot, e.g. i64 for a u32 tag payload — the i64 store would
    // clobber the discriminant).
    if (t.payload) |payload_local| {
        const payload_byte_size = self.procLocalByteSize(payload_local);
        try self.emitProcLocal(payload_local);
        if (self.isCompositeLocal(payload_local)) {
            // Composite types (Str, List, records, etc.) produce a pointer on
            // the stack. Copy the full data from the source to the tag union.
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
            try self.emitMemCopy(base_local, 0, src_local, payload_byte_size);
        } else {
            const payload_vt = self.procLocalValType(payload_local);
            try self.emitStoreToMemSized(base_local, 0, payload_vt, payload_byte_size);
        }
    }

    // Store discriminant AFTER payload (so it can't be overwritten)
    const disc_size: u32 = tu_layout.discriminant_size;
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
fn generateList(self: *Self, l: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const elems = self.store.getLocalSpan(l.elems);

    if (elems.len == 0) {
        // Empty list — same as empty_list
        const base_offset = try self.allocStackMemory(12, 4);
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
        try self.emitProcLocal(elem_expr_id);

        const offset = @as(u32, @intCast(i)) * elem_size;
        if (self.isCompositeLayout(l.elem_layout) and elem_size > 0) {
            // Composite element — copy from source pointer
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
            try self.emitMemCopy(data_base, offset, src_local, elem_size);
        } else {
            // Primitive element — store directly
            const expr_vt = self.procLocalValType(elem_expr_id);
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
    const args = self.store.getLocalSpan(ll.args);

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
            return self.emitNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        .bool_not => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        },

        // Safe integer widenings (no-op or single instruction)
        .u8_to_i16, .u8_to_i32, .u8_to_u16, .u8_to_u32 => {
            // u8 is already i32 in wasm, and widening to larger types is a no-op
            try self.emitProcLocal(args[0]);
            // If arg produces i64 (e.g. from i64_literal), wrap to i32
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
        },
        .i8_to_i16, .i8_to_i32 => {
            // i8 is i32 in wasm, sign-extend from 8 bits
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        },
        .u16_to_i32, .u16_to_u32 => {
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
        },
        .i16_to_i32 => {
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
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
            try self.emitProcLocal(args[0]);
            const arg_vt = self.procLocalValType(args[0]);
            if (arg_vt == .i64) {
                // Already i64 — no extension needed
                return;
            }
            self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
        },
        .i8_to_i64, .i16_to_i64, .i32_to_i64 => {
            try self.emitProcLocal(args[0]);
            const arg_vt = self.procLocalValType(args[0]);
            if (arg_vt == .i64) {
                // Already i64 — no extension needed
                return;
            }
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },

        // Narrowing/wrapping conversions
        .i64_to_i32_wrap, .u64_to_u32_wrap, .u64_to_i32_wrap, .i64_to_u32_wrap => {
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
            // May need to wrap i64 to i32 first
            const arg_vt = self.procLocalValType(args[0]);
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
            try self.emitProcLocal(args[0]);
            const arg_vt = self.procLocalValType(args[0]);
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
            try self.emitProcLocal(args[0]);
        },
        .i64_to_u64_wrap, .u64_to_i64_wrap => {
            // Same representation in wasm (both i64), no-op
            try self.emitProcLocal(args[0]);
        },

        // Signed sub-i32 to unsigned wider wrapping (needs sign extension)
        .i8_to_u32_wrap => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        },
        .i16_to_u32_wrap => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
        },
        .i8_to_u16_wrap => {
            try self.emitProcLocal(args[0]);
            // Sign-extend from 8 bits then mask to 16 bits
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFFFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i8_to_u64_wrap => {
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i16_to_u64_wrap => {
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i32_to_u64_wrap => {
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i32_to_u128_wrap => {
            // Signed i32→u128 wrap: sign-extend to i64, then to i128
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                // Already i64
            } else {
                self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            }
            try self.emitIntToI128(true);
        },
        .i32_to_u64_try => {
            // Signed i32 → unsigned u64: check >= 0, then sign-extend to i64
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
            // Sign-extend to i64 first
            if (self.procLocalValType(args[0]) != .i64) {
                self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            }
            try self.emitIntToI128(true);
            try self.emitI128TryToU128(true);
        },

        // Float conversions
        .f32_to_f64 => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
        },
        .f64_to_f32_wrap => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },

        // Int to float
        .i32_to_f32, .i8_to_f32, .i16_to_f32 => {
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.f32_convert_i32_s) catch return error.OutOfMemory;
        },
        .u32_to_f32, .u8_to_f32, .u16_to_f32 => {
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.f32_convert_i32_u) catch return error.OutOfMemory;
        },
        .i32_to_f64, .i8_to_f64, .i16_to_f64 => {
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.f64_convert_i32_s) catch return error.OutOfMemory;
        },
        .u32_to_f64, .u8_to_f64, .u16_to_f64 => {
            try self.emitProcLocal(args[0]);
            if (self.procLocalValType(args[0]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.f64_convert_i32_u) catch return error.OutOfMemory;
        },
        .i64_to_f32 => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.f32_convert_i64_s) catch return error.OutOfMemory;
        },
        .u64_to_f32 => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.f32_convert_i64_u) catch return error.OutOfMemory;
        },
        .i64_to_f64 => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
        },
        .u64_to_f64 => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
        },

        // Float to int (truncating)
        .f32_to_i32_trunc, .f32_to_i8_trunc, .f32_to_i16_trunc => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f32_s) catch return error.OutOfMemory;
        },
        .f32_to_u32_trunc, .f32_to_u8_trunc, .f32_to_u16_trunc => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f32_u) catch return error.OutOfMemory;
        },
        .f64_to_i32_trunc, .f64_to_i8_trunc, .f64_to_i16_trunc => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f64_s) catch return error.OutOfMemory;
        },
        .f64_to_u32_trunc, .f64_to_u8_trunc, .f64_to_u16_trunc => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f64_u) catch return error.OutOfMemory;
        },
        .f32_to_i64_trunc => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f32_s) catch return error.OutOfMemory;
        },
        .f32_to_u64_trunc => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f32_u) catch return error.OutOfMemory;
        },
        .f64_to_i64_trunc => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f64_s) catch return error.OutOfMemory;
        },
        .f64_to_u64_trunc => {
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f64_u) catch return error.OutOfMemory;
        },

        // Float math functions (direct wasm opcodes)
        .num_sqrt => {
            try self.emitProcLocal(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_sqrt,
                .f64 => Op.f64_sqrt,
                .i32, .i64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_floor => {
            try self.emitProcLocal(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_floor,
                .f64 => Op.f64_floor,
                .i32, .i64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_ceiling => {
            try self.emitProcLocal(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_ceil,
                .f64 => Op.f64_ceil,
                .i32, .i64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_round => {
            try self.emitProcLocal(args[0]);
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
            return self.emitNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        // List operations
        .list_len => {
            // Load length from RocList struct (offset 4)
            try self.emitProcLocal(args[0]);
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
            const list_layout_idx = self.procLocalLayoutIdx(args[0]);
            const elem_layout_idx = self.listElemLayout(list_layout_idx);
            const elem_size: u32 = self.layoutStorageByteSize(elem_layout_idx);
            const elem_is_composite = self.isCompositeLayout(elem_layout_idx);

            // Generate list expression and save pointer
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            // Generate index as i32
            const index_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitProcLocal(args[1]);
            if (self.procLocalValType(args[1]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            try self.emitLocalSet(index_local);

            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalGet(index_local);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

            if (elem_is_composite) {
                const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(src_local);

                const elem_align: u32 = @intCast(@max(ls.layoutSizeAlign(ls.getLayout(elem_layout_idx)).alignment.toByteUnits(), 1));
                const dst_offset = try self.allocStackMemory(elem_size, elem_align);
                const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitFpOffset(dst_offset);
                try self.emitLocalSet(dst_local);

                try self.emitMemCopy(dst_local, 0, src_local, elem_size);
                try self.emitLocalGet(dst_local);
            } else {
                try self.emitLoadOpForLayout(elem_layout_idx, 0);
            }
        },

        // String operations
        // Bitwise operations
        .num_pow, .num_log => unreachable, // Resolved by earlier lowering

        .num_abs_diff => {
            // abs_diff(a, b) -> |a - b|
            return self.emitNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        .num_shift_left_by, .num_shift_right_by, .num_shift_right_zf_by => {
            // Shift operations: shift(value, amount)
            return self.emitNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        .list_drop_at => {
            try self.generateLLListDropAt(args, ll.ret_layout);
        },

        // List element access operations (no heap allocation needed)
        .list_first => {
            const ls = self.getLayoutStore();
            const list_layout_idx = self.procLocalLayoutIdx(args[0]);
            const elem_layout_idx = self.listElemLayout(list_layout_idx);
            const elem_size: u32 = self.layoutStorageByteSize(elem_layout_idx);
            const elem_is_composite = self.isCompositeLayout(elem_layout_idx);

            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);

            if (elem_is_composite) {
                const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(src_local);

                const elem_align: u32 = @intCast(@max(ls.layoutSizeAlign(ls.getLayout(elem_layout_idx)).alignment.toByteUnits(), 1));
                const dst_offset = try self.allocStackMemory(elem_size, elem_align);
                const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitFpOffset(dst_offset);
                try self.emitLocalSet(dst_local);

                try self.emitMemCopy(dst_local, 0, src_local, elem_size);
                try self.emitLocalGet(dst_local);
            } else {
                try self.emitLoadOpForLayout(elem_layout_idx, 0);
            }
        },
        .list_last => {
            const ls = self.getLayoutStore();
            const list_layout_idx = self.procLocalLayoutIdx(args[0]);
            const elem_layout_idx = self.listElemLayout(list_layout_idx);
            const elem_size: u32 = self.layoutStorageByteSize(elem_layout_idx);
            const elem_is_composite = self.isCompositeLayout(elem_layout_idx);

            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 4);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

            if (elem_is_composite) {
                const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(src_local);

                const elem_align: u32 = @intCast(@max(ls.layoutSizeAlign(ls.getLayout(elem_layout_idx)).alignment.toByteUnits(), 1));
                const dst_offset = try self.allocStackMemory(elem_size, elem_align);
                const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitFpOffset(dst_offset);
                try self.emitLocalSet(dst_local);

                try self.emitMemCopy(dst_local, 0, src_local, elem_size);
                try self.emitLocalGet(dst_local);
            } else {
                try self.emitLoadOpForLayout(elem_layout_idx, 0);
            }
        },
        .list_drop_first => {
            // list_drop_first(list, count) -> list
            // Returns a RocList with adjusted elements_ptr and length
            // No allocation needed — returns a view
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            try self.emitProcLocal(args[1]);
            if (self.procLocalValType(args[1]) == .i64) {
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
            const list_abi = self.builtinInternalListAbi("wasm.list_drop_first.builtin_list_abi", ll.ret_layout);
            const elem_size = list_abi.elem_size;

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
            try self.emitPrepareListSliceMetadata(list_local, list_abi.elements_refcounted, encoded_cap);

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
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            try self.emitProcLocal(args[1]);
            if (self.procLocalValType(args[1]) == .i64) {
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
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            try self.emitProcLocal(args[1]);
            if (self.procLocalValType(args[1]) == .i64) {
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
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            try self.emitProcLocal(args[1]);
            if (self.procLocalValType(args[1]) == .i64) {
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

            const list_abi = self.builtinInternalListAbi("wasm.list_take_last.builtin_list_abi", ll.ret_layout);
            const elem_size = list_abi.elem_size;

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
            try self.emitPrepareListSliceMetadata(list_local, list_abi.elements_refcounted, encoded_cap);

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, encoded_cap) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 8);

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
            const record_layout_idx = self.procLocalLayoutIdx(args[1]);
            const record_layout = ls.getLayout(record_layout_idx);
            const record_idx = record_layout.data.struct_.idx;
            const len_field_off = self.structFieldOffsetByOriginalIndexWasm(record_idx, 0);
            const start_field_off = self.structFieldOffsetByOriginalIndexWasm(record_idx, 1);
            if (builtin.mode == .Debug) {
                const sd = ls.getStructData(record_idx);
                const sorted_fields = ls.struct_fields.sliceRange(sd.getFields());
                if (sorted_fields.len != 2) {
                    std.debug.panic(
                        "LIR/wasm invariant violated: list_sublist record expected 2 fields, got {d}",
                        .{sorted_fields.len},
                    );
                }
                const record_size = self.layoutStorageByteSize(record_layout_idx);
                if (ls.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .u64 or
                    ls.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .u64 or
                    self.structFieldSizeByOriginalIndexWasm(record_idx, 0) != 8 or
                    self.structFieldSizeByOriginalIndexWasm(record_idx, 1) != 8 or
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
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            // Generate record arg (pointer to the config record)
            try self.emitProcLocal(args[1]);
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

            const list_abi = self.builtinInternalListAbi("wasm.list_sublist.builtin_list_abi", ll.ret_layout);
            const elem_size = list_abi.elem_size;

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
            try self.emitPrepareListSliceMetadata(list_local, list_abi.elements_refcounted, encoded_cap);

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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
            const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a);
            try self.emitProcLocal(args[1]);
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
            try self.emitProcLocal(args[0]);
            const a_str = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a_str);
            try self.emitProcLocal(args[1]);
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
            try self.emitStrToUtf8(args[0]);
        },
        .str_from_utf8_lossy => {
            try self.emitStrFromUtf8Lossy(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
            const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a);
            try self.emitProcLocal(args[1]);
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
            try self.emitProcLocal(args[0]);
            const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a);
            try self.emitProcLocal(args[1]);
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
            try self.emitProcLocal(args[0]);
            const str_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(str_local);
            try self.emitProcLocal(args[1]);
            const int_vt = self.procLocalValType(args[1]);
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
            try self.emitProcLocal(args[0]);
            const int_vt = self.procLocalValType(args[0]);
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
            try self.emitProcLocal(args[0]);
            const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a);
            try self.emitProcLocal(args[1]);
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
            const tu_layout = WasmLayout.tagUnionLayoutWithStore(ret_layout_val.data.tag_union.idx, ls);
            const variants = ls.getTagUnionVariants(tu_data);
            var ok_disc: ?u16 = null;
            var err_disc: ?u16 = null;
            var err_record_idx: ?layout.StructIdx = null;
            for (0..variants.len) |i| {
                const v_payload = variants.get(@intCast(i)).payload_layout;
                const candidate = self.unwrapSingleFieldPayloadLayout(v_payload) orelse v_payload;
                if (candidate == .str) {
                    ok_disc = @intCast(i);
                } else {
                    err_disc = @intCast(i);
                    const err_layout = ls.getLayout(candidate);
                    err_record_idx = switch (err_layout.tag) {
                        .struct_ => err_layout.data.struct_.idx,
                        .tag_union => inner: {
                            const inner_tu = ls.getTagUnionData(err_layout.data.tag_union.idx);
                            const inner_v = ls.getTagUnionVariants(inner_tu);
                            if (inner_v.len == 0) break :inner null;
                            const inner_payload = inner_v.get(0).payload_layout;
                            const unwrapped = self.unwrapSingleFieldPayloadLayout(inner_payload) orelse inner_payload;
                            const inner_layout = ls.getLayout(unwrapped);
                            if (inner_layout.tag == .struct_) break :inner inner_layout.data.struct_.idx;
                            break :inner null;
                        },
                        else => null,
                    };
                }
            }
            const resolved_ok = ok_disc orelse std.debug.panic(
                "WasmCodeGen invariant violated: str_from_utf8 had no Ok(Str) variant",
                .{},
            );
            const resolved_err = err_disc orelse std.debug.panic(
                "WasmCodeGen invariant violated: str_from_utf8 had no Err variant",
                .{},
            );
            const rec_idx = err_record_idx orelse std.debug.panic(
                "WasmCodeGen invariant violated: str_from_utf8 could not resolve error record layout",
                .{},
            );
            const struct_data = ls.getStructData(rec_idx);
            const fields = ls.struct_fields.sliceRange(struct_data.getFields());
            var index_off: ?u32 = null;
            var index_size: ?u32 = null;
            var problem_off: ?u32 = null;
            var problem_size: ?u32 = null;
            for (0..fields.len) |i| {
                const field = fields.get(i);
                const field_layout = ls.getLayout(field.layout);
                const field_size = self.layoutStorageByteSize(field.layout);
                const field_offset = self.structFieldOffsetByOriginalIndexWasm(rec_idx, field.index);
                const is_index = switch (field_layout.tag) {
                    .scalar => field_layout.data.scalar.tag == .int and switch (field_layout.data.scalar.data.int) {
                        .u64, .i64 => true,
                        else => false,
                    },
                    else => false,
                };
                if (is_index) {
                    index_off = field_offset;
                    index_size = field_size;
                    continue;
                }
                if (problem_off == null) {
                    problem_off = field_offset;
                    problem_size = field_size;
                }
            }
            const resolved_index_off = index_off orelse std.debug.panic(
                "WasmCodeGen invariant violated: str_from_utf8 could not resolve index offset",
                .{},
            );
            const resolved_index_size = index_size orelse std.debug.panic(
                "WasmCodeGen invariant violated: str_from_utf8 could not resolve index size",
                .{},
            );
            const resolved_problem_off = problem_off orelse std.debug.panic(
                "WasmCodeGen invariant violated: str_from_utf8 could not resolve problem offset",
                .{},
            );
            const resolved_problem_size = problem_size orelse std.debug.panic(
                "WasmCodeGen invariant violated: str_from_utf8 could not resolve problem size",
                .{},
            );
            const index_offset = resolved_index_off;
            const problem_offset = resolved_problem_off;
            const import_idx = self.str_from_utf8_import orelse unreachable;
            try self.emitProcLocal(args[0]);
            const input = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(input);
            const result_offset = try self.allocStackMemory(tu_layout.size, 4);
            try self.emitLocalGet(input);
            try self.emitFpOffset(result_offset);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(tu_layout.size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(tu_layout.discriminant_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(tu_layout.discriminant_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(resolved_ok)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(resolved_err)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(index_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(resolved_index_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(problem_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(resolved_problem_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
        },
        .u8_from_str,
        .i8_from_str,
        .u16_from_str,
        .i16_from_str,
        .u32_from_str,
        .i32_from_str,
        .u64_from_str,
        .i64_from_str,
        .u128_from_str,
        .i128_from_str,
        .dec_from_str,
        .f32_from_str,
        .f64_from_str,
        => {
            const ls = self.getLayoutStore();
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            if (ret_layout_val.tag != .tag_union) unreachable;
            const tu_layout = WasmLayout.tagUnionLayoutWithStore(ret_layout_val.data.tag_union.idx, ls);
            const disc_offset: u32 = tu_layout.discriminant_offset;
            const result_offset = try self.allocStackMemory(tu_layout.size, 4);
            const parse_spec = ll.op.numericParseSpec() orelse unreachable;

            try self.emitProcLocal(args[0]);
            const input = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(input);

            switch (parse_spec) {
                .dec => {
                    const import_idx = self.dec_from_str_import orelse unreachable;
                    try self.emitLocalGet(input);
                    try self.emitFpOffset(result_offset);
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(disc_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .float => |float| {
                    const import_idx = self.float_from_str_import orelse unreachable;
                    try self.emitLocalGet(input);
                    try self.emitFpOffset(result_offset);
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, float.width_bytes) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(disc_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .int => |int| {
                    const import_idx = self.int_from_str_import orelse unreachable;
                    try self.emitLocalGet(input);
                    try self.emitFpOffset(result_offset);
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, int.width_bytes) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, if (int.signed) 1 else 0) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(disc_offset)) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
            }

            try self.emitFpOffset(result_offset);
        },

        .str_inspect => {
            try self.emitStrEscapeAndQuote(args[0]);
        },
        .u8_to_str => try self.emitIntToStr(args[0], 1, false),
        .i8_to_str => try self.emitIntToStr(args[0], 1, true),
        .u16_to_str => try self.emitIntToStr(args[0], 2, false),
        .i16_to_str => try self.emitIntToStr(args[0], 2, true),
        .u32_to_str => try self.emitIntToStr(args[0], 4, false),
        .i32_to_str => try self.emitIntToStr(args[0], 4, true),
        .u64_to_str => try self.emitIntToStr(args[0], 8, false),
        .i64_to_str => try self.emitIntToStr(args[0], 8, true),
        .u128_to_str => try self.emitIntToStr(args[0], 16, false),
        .i128_to_str => try self.emitIntToStr(args[0], 16, true),
        .dec_to_str => try self.emitDecToStr(args[0]),
        .f32_to_str => try self.emitFloatToStr(args[0], true),
        .f64_to_str => try self.emitFloatToStr(args[0], false),
        .num_to_str => switch (self.procLocalLayoutIdx(args[0])) {
            .u8 => try self.emitIntToStr(args[0], 1, false),
            .i8 => try self.emitIntToStr(args[0], 1, true),
            .u16 => try self.emitIntToStr(args[0], 2, false),
            .i16 => try self.emitIntToStr(args[0], 2, true),
            .u32 => try self.emitIntToStr(args[0], 4, false),
            .i32 => try self.emitIntToStr(args[0], 4, true),
            .u64 => try self.emitIntToStr(args[0], 8, false),
            .i64 => try self.emitIntToStr(args[0], 8, true),
            .u128 => try self.emitIntToStr(args[0], 16, false),
            .i128 => try self.emitIntToStr(args[0], 16, true),
            .dec => try self.emitDecToStr(args[0]),
            .f32 => try self.emitFloatToStr(args[0], true),
            .f64 => try self.emitFloatToStr(args[0], false),
            else => std.debug.panic(
                "WasmCodeGen invariant violated: num_to_str received non-numeric layout {s}",
                .{@tagName(self.procLocalLayoutIdx(args[0]))},
            ),
        },
        .num_from_numeral => unreachable, // Resolved before backend codegen

        // Box operations
        .box_box => {
            // box_box(value) -> Box value (pointer to heap-allocated copy)
            const value_expr = args[0];
            const ls = self.getLayoutStore();
            const ret_layout = ls.getLayout(ll.ret_layout);

            if (ret_layout.tag == .box_of_zst) {
                _ = try self.emitProcLocal(value_expr);
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            } else {
                const box_abi = ls.builtinBoxAbi(ll.ret_layout);
                const value_size = box_abi.elem_size;
                const value_vt = self.procLocalValType(value_expr);
                if (value_size == 0) {
                    _ = try self.emitProcLocal(value_expr);
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                } else {
                    const alignment: u32 = box_abi.elem_alignment;

                    try self.emitHeapAllocConst(value_size, alignment);
                    const box_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitLocalSet(box_ptr);

                    try self.emitProcLocal(value_expr);

                    if (value_vt == .i32 and value_size > 4) {
                        const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        try self.emitLocalSet(src_ptr);

                        if (value_size <= 16) {
                            var offset: u32 = 0;
                            while (offset + 4 <= value_size) : (offset += 4) {
                                try self.emitLocalGet(box_ptr);
                                try self.emitLocalGet(src_ptr);
                                self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
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
                            const i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                            try self.emitLocalSet(i);

                            self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
                            self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

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

                            try self.emitLocalGet(i);
                            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
                            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                            try self.emitLocalSet(i);

                            try self.emitLocalGet(i);
                            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(value_size)) catch return error.OutOfMemory;
                            self.body.append(self.allocator, Op.i32_lt_u) catch return error.OutOfMemory;
                            self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

                            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
                        }
                    } else {
                        const value_local = self.storage.allocAnonymousLocal(value_vt) catch return error.OutOfMemory;
                        try self.emitLocalSet(value_local);
                        try self.emitLocalGet(value_local);
                        try self.emitStoreToMemSized(box_ptr, 0, value_vt, value_size);
                    }

                    try self.emitLocalGet(box_ptr);
                }
            }
        },
        .box_unbox => {
            // box_unbox(box_ptr) -> value
            // Box is a transparent pointer - dereference it
            const box_expr = args[0];
            const ls = self.getLayoutStore();
            const box_layout = ls.getLayout(self.procLocalLayoutIdx(box_expr));

            if (box_layout.tag == .box_of_zst) {
                _ = try self.emitProcLocal(box_expr);
                const result_vt = self.resolveValType(ll.ret_layout);
                switch (result_vt) {
                    .i32 => {
                        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    },
                    .i64 => {
                        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
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
            } else {
                const box_abi = ls.builtinBoxAbi(self.procLocalLayoutIdx(box_expr));
                if (box_abi.elem_size == 0) {
                    _ = try self.emitProcLocal(box_expr);
                    const result_vt = self.resolveValType(ll.ret_layout);
                    switch (result_vt) {
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
                } else {
                    try self.emitProcLocal(box_expr);

                    const result_vt = self.resolveValType(ll.ret_layout);
                    const result_size = self.layoutByteSize(ll.ret_layout);
                    if (result_vt == .i32 and result_size > 4) {
                        const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        try self.emitLocalSet(src_local);

                        const result_align: u32 = @intCast(@max(self.getLayoutStore().layoutSizeAlign(self.getLayoutStore().getLayout(ll.ret_layout)).alignment.toByteUnits(), 1));
                        const dst_offset = try self.allocStackMemory(result_size, result_align);
                        const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        try self.emitFpOffset(dst_offset);
                        try self.emitLocalSet(dst_local);

                        try self.emitMemCopy(dst_local, 0, src_local, result_size);
                        try self.emitLocalGet(dst_local);
                    } else {
                        const box_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        try self.emitLocalSet(box_ptr);
                        if (result_size == 0) {
                            switch (result_vt) {
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
                        } else {
                            const temp_offset = try self.allocStackMemory(@max(result_size, 4), 4);
                            const temp_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                            try self.emitFpOffset(temp_offset);
                            try self.emitLocalSet(temp_ptr);

                            try self.emitMemCopy(temp_ptr, 0, box_ptr, result_size);
                            try self.emitLocalGet(temp_ptr);
                            try self.emitLoadOpSized(result_vt, result_size, 0);
                        }
                    }
                }
            }
        },

        // Compare — returns Ordering enum (EQ=0, GT=1, LT=2)
        .compare => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            // Determine arg type from first arg's layout
            const arg_layout = self.procLocalLayoutIdx(args[0]);
            const arg_vt = self.procLocalValType(args[0]);

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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(1, true, true);
        },
        .i128_to_i16_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(2, true, true);
        },
        .i128_to_i32_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(4, true, true);
        },
        .i128_to_i64_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(8, true, true);
        },
        .i128_to_u8_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(1, true, false);
        },
        .i128_to_u16_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(2, true, false);
        },
        .i128_to_u32_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(4, true, false);
        },
        .i128_to_u64_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(8, true, false);
        },
        .i128_to_u128_try => {
            // i128 → u128: check >= 0 (high word sign bit)
            try self.emitProcLocal(args[0]);
            try self.emitI128TryToU128(true);
        },
        .u128_to_i8_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(1, false, true);
        },
        .u128_to_i16_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(2, false, true);
        },
        .u128_to_i32_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(4, false, true);
        },
        .u128_to_i64_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(8, false, true);
        },
        .u128_to_i128_try => {
            // u128 → i128: check high bit not set (value < 2^127)
            try self.emitProcLocal(args[0]);
            try self.emitI128TryToI128();
        },
        .u128_to_u8_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(1, false, false);
        },
        .u128_to_u16_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(2, false, false);
        },
        .u128_to_u32_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(4, false, false);
        },
        .u128_to_u64_try => {
            try self.emitProcLocal(args[0]);
            try self.emitI128TryNarrow(8, false, false);
        },
        // Widening signed→unsigned try: check >= 0
        .i8_to_u128_try, .i16_to_u128_try, .i64_to_u128_try => {
            try self.emitProcLocal(args[0]);
            // Source is a small signed int (i32 or i64 on wasm stack)
            // Convert to i128, then check >= 0
            const src_vt = self.procLocalValType(args[0]);
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
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            try self.emitIntToI128(false);
        },
        .u64_to_i128,
        .u64_to_u128,
        => {
            // Unsigned i64→i128: value is already i64
            try self.emitProcLocal(args[0]);
            try self.emitIntToI128(false);
        },
        .i8_to_i128,
        .i16_to_i128,
        .i32_to_i128,
        => {
            // Signed i32→i128: sign-extend i32 to i64, then to i128
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            try self.emitIntToI128(true);
        },
        .i64_to_i128,
        => {
            // Signed i64→i128: value is already i64
            try self.emitProcLocal(args[0]);
            try self.emitIntToI128(true);
        },
        .i8_to_u128_wrap,
        .i16_to_u128_wrap,
        => {
            // Signed i32→u128 wrap: sign-extend to i64, then i128
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            try self.emitIntToI128(true);
        },
        .i64_to_u128_wrap,
        => {
            // Signed i64→u128 wrap: already i64, sign-extend to i128
            try self.emitProcLocal(args[0]);
            try self.emitIntToI128(true);
        },
        // i128/u128 truncation to smaller types (load low word, mask)
        .i128_to_i8_wrap,
        .i128_to_u8_wrap,
        .u128_to_i8_wrap,
        .u128_to_u8_wrap,
        => {
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .i128_to_i64_wrap,
        .i128_to_u64_wrap,
        .u128_to_i64_wrap,
        .u128_to_u64_wrap,
        => {
            try self.emitProcLocal(args[0]);
            try self.emitLoadOp(.i64, 0);
        },
        .u128_to_i128_wrap,
        .i128_to_u128_wrap,
        => {
            // Same representation — just pass through (pointer stays the same)
            try self.emitProcLocal(args[0]);
        },
        // i128/u128 → float conversions
        .i128_to_f64 => {
            // Approximate: convert low u64 to f64 + high i64 * 2^64
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, dec_factor) catch return error.OutOfMemory;
            try self.emitI64MulToI128Signed(val, dec_factor);
        },
        .i64_to_dec => {
            // i64 → Dec: already i64
            try self.emitProcLocal(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, dec_factor) catch return error.OutOfMemory;
            try self.emitI64MulToI128Signed(val, dec_factor);
        },

        // Dec → integer truncating conversions (divide i128 by 10^18, truncate)
        // Uses roc_i128_div_s host function for correct 128-bit division.
        .dec_to_i64_trunc,
        .dec_to_i32_trunc,
        .dec_to_i16_trunc,
        .dec_to_i8_trunc,
        .dec_to_u64_trunc,
        .dec_to_u32_trunc,
        .dec_to_u16_trunc,
        .dec_to_u8_trunc,
        => {
            // Get pointer to Dec value (i128)
            try self.emitProcLocal(args[0]);
            const dec_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(dec_local);

            // Store 10^18 as i128 constant in stack memory
            const divisor_offset = try self.allocStackMemory(16, 8);
            const divisor_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(divisor_offset);
            try self.emitLocalSet(divisor_local);
            // low word = 10^18
            try self.emitLocalGet(divisor_local);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 0);
            // high word = 0
            try self.emitLocalGet(divisor_local);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 8);

            // Call roc_i128_div_s(dec_ptr, divisor_ptr, result_ptr)
            try self.emitI128HostBinOp(dec_local, divisor_local, self.i128_div_s_import orelse unreachable);
            // Result is an i32 pointer to the 16-byte quotient; load low i64
            try self.emitLoadOp(.i64, 0);

            // Truncate to target size
            switch (ll.op) {
                .dec_to_i64_trunc, .dec_to_u64_trunc => {},
                .dec_to_i32_trunc, .dec_to_u32_trunc => {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                },
                .dec_to_i16_trunc, .dec_to_i8_trunc, .dec_to_u16_trunc, .dec_to_u8_trunc => {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                    const mask: i32 = switch (ll.op) {
                        .dec_to_i8_trunc, .dec_to_u8_trunc => 0xFF,
                        .dec_to_i16_trunc, .dec_to_u16_trunc => 0xFFFF,
                        else => unreachable,
                    };
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, mask) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                },
                else => unreachable,
            }
        },
        .dec_to_i128_trunc, .dec_to_u128_trunc => {
            // Dec → i128/u128: divide i128 by 10^18 using host function
            try self.emitProcLocal(args[0]);
            const dec_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(dec_local);

            // Store 10^18 as i128 constant in stack memory
            const divisor_offset = try self.allocStackMemory(16, 8);
            const divisor_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(divisor_offset);
            try self.emitLocalSet(divisor_local);
            try self.emitLocalGet(divisor_local);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 0);
            try self.emitLocalGet(divisor_local);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 8);

            try self.emitI128HostBinOp(dec_local, divisor_local, self.i128_div_s_import orelse unreachable);
        },
        .dec_to_f64 => {
            // Dec → f64: load i128 as i64 (low word), convert to f64, divide by 10^18.0
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
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
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i8_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(1, false, -128.0, 127.0);
        },
        .f32_to_u8_try_unsafe, .f64_to_u8_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u8_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(1, false, 0.0, 255.0);
        },
        .f32_to_i16_try_unsafe, .f64_to_i16_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i16_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(2, false, -32768.0, 32767.0);
        },
        .f32_to_u16_try_unsafe, .f64_to_u16_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u16_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(2, false, 0.0, 65535.0);
        },
        .f32_to_i32_try_unsafe, .f64_to_i32_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i32_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(4, false, -2147483648.0, 2147483647.0);
        },
        .f32_to_u32_try_unsafe, .f64_to_u32_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u32_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(4, false, 0.0, 4294967295.0);
        },
        .f32_to_i64_try_unsafe, .f64_to_i64_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i64_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(8, true, @as(f64, @floatFromInt(@as(i64, std.math.minInt(i64)))), @as(f64, @floatFromInt(@as(i64, std.math.maxInt(i64)))));
        },
        .f32_to_u64_try_unsafe, .f64_to_u64_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u64_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(8, true, 0.0, @as(f64, @floatFromInt(@as(u64, std.math.maxInt(u64)))));
        },
        // 128-bit float try_unsafe: return {val: i128, is_int: bool, in_range: bool}
        .f32_to_i128_try_unsafe, .f64_to_i128_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i128_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToI128TryUnsafe(true);
        },
        .f32_to_u128_try_unsafe, .f64_to_u128_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u128_try_unsafe) self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToI128TryUnsafe(false);
        },
        .f64_to_f32_try_unsafe => {
            // Returns {val: F32, success: Bool} — 8 bytes
            try self.emitProcLocal(args[0]);
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
fn emitNumericLowLevel(self: *Self, op: anytype, args: []const ProcLocalId, ret_layout: layout.Idx) Allocator.Error!void {
    // For comparison ops, the operand type determines composite-ness, not ret_layout (which is bool)
    const use_operand_layout = switch (op) {
        .num_is_eq, .num_is_gt, .num_is_gte, .num_is_lt, .num_is_lte, .num_abs_diff => true,
        else => false,
    };

    // Check for composite types (i128/Dec)
    const check_layout = if (use_operand_layout) self.procLocalLayoutIdx(args[0]) else ret_layout;
    const is_shift = op == .num_shift_left_by or op == .num_shift_right_by or op == .num_shift_right_zf_by;
    if (!is_shift and (self.isCompositeLocal(args[0]) or self.isCompositeLayout(check_layout))) {
        return self.emitCompositeNumericOp(op, args, ret_layout, check_layout);
    }
    // I128/U128 shifts: LHS is composite but RHS is U8 — needs dedicated handling.
    if (is_shift and (self.isCompositeLocal(args[0]) or self.isCompositeLayout(check_layout))) {
        return self.emitI128Shift(op, args);
    }

    // For neg, also check composite via ret_layout
    if (op == .num_negate and self.isCompositeLayout(ret_layout)) {
        return self.emitCompositeI128Negate(args[0], ret_layout);
    }

    const vt = if (use_operand_layout) self.procLocalValType(args[0]) else self.resolveValType(ret_layout);
    const layout_idx = self.procLocalLayoutIdx(args[0]);

    switch (op) {
        .num_plus => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_add,
                .i64 => Op.i64_add,
                .f32 => Op.f32_add,
                .f64 => Op.f64_add,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_minus => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_sub,
                .i64 => Op.i64_sub,
                .f32 => Op.f32_sub,
                .f64 => Op.f64_sub,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_times => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_mul,
                .i64 => Op.i64_mul,
                .f32 => Op.f32_mul,
                .f64 => Op.f64_mul,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_div_by => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
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
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
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
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
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
                    try self.emitProcLocal(args[0]);
                    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    try self.emitProcLocal(args[0]);
                    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                },
                .f32 => {
                    try self.emitProcLocal(args[0]);
                    self.body.append(self.allocator, Op.f32_neg) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.emitProcLocal(args[0]);
                    self.body.append(self.allocator, Op.f64_neg) catch return error.OutOfMemory;
                },
            }
        },
        .num_is_eq => {
            // Check for structural equality (strings, lists, records, etc.)
            const lay_idx = self.procLocalLayoutIdx(args[0]);
            if (lay_idx == .str or self.isCompositeLayout(lay_idx)) {
                try self.emitStructuralEq(args[0], args[1], false);
                return;
            }
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_eq,
                .i64 => Op.i64_eq,
                .f32 => Op.f32_eq,
                .f64 => Op.f64_eq,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_is_gt => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const is_unsigned = isUnsignedLayout(self.procLocalLayoutIdx(args[0]));
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_gt_u else Op.i32_gt_s,
                .i64 => if (is_unsigned) Op.i64_gt_u else Op.i64_gt_s,
                .f32 => Op.f32_gt,
                .f64 => Op.f64_gt,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_is_gte => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const is_unsigned = isUnsignedLayout(self.procLocalLayoutIdx(args[0]));
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_ge_u else Op.i32_ge_s,
                .i64 => if (is_unsigned) Op.i64_ge_u else Op.i64_ge_s,
                .f32 => Op.f32_ge,
                .f64 => Op.f64_ge,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_is_lt => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const is_unsigned = isUnsignedLayout(self.procLocalLayoutIdx(args[0]));
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_lt_u else Op.i32_lt_s,
                .i64 => if (is_unsigned) Op.i64_lt_u else Op.i64_lt_s,
                .f32 => Op.f32_lt,
                .f64 => Op.f64_lt,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_is_lte => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const is_unsigned = isUnsignedLayout(self.procLocalLayoutIdx(args[0]));
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
                    try self.emitProcLocal(args[0]);
                    self.body.append(self.allocator, Op.f32_abs) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.emitProcLocal(args[0]);
                    self.body.append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
                },
                .i32 => {
                    // abs(x) = select(x, -x, x >= 0)
                    try self.emitProcLocal(args[0]);
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
                    try self.emitProcLocal(args[0]);
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
            const mod_layout_idx = self.procLocalLayoutIdx(args[0]);
            try self.emitProcLocal(args[0]);
            try self.emitCanonicalizeScalarForLayout(mod_layout_idx);
            try self.emitProcLocal(args[1]);
            try self.emitCanonicalizeScalarForLayout(mod_layout_idx);

            switch (mod_layout_idx) {
                .i8 => {
                    const import_idx = self.i8_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .u8 => {
                    const import_idx = self.u8_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .i16 => {
                    const import_idx = self.i16_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .u16 => {
                    const import_idx = self.u16_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .i32 => {
                    const import_idx = self.i32_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .u32 => {
                    const import_idx = self.u32_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .i64 => {
                    const import_idx = self.i64_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                .u64 => {
                    const import_idx = self.u64_mod_by_import orelse unreachable;
                    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                },
                else => switch (vt) {
                    .f32, .f64 => try self.emitFloatMod(vt),
                    else => unreachable,
                },
            }
        },
        .num_abs_diff => {
            const is_unsigned = isUnsignedLayout(self.procLocalLayoutIdx(args[0]));
            switch (vt) {
                .i32 => {
                    try self.emitProcLocal(args[0]);
                    const lhs = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    try self.emitProcLocal(args[1]);
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
                    try self.emitProcLocal(args[0]);
                    const lhs = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs) catch return error.OutOfMemory;
                    try self.emitProcLocal(args[1]);
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
                    try self.emitProcLocal(args[0]);
                    try self.emitProcLocal(args[1]);
                    self.body.append(self.allocator, Op.f32_sub) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f32_abs) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.emitProcLocal(args[0]);
                    try self.emitProcLocal(args[1]);
                    self.body.append(self.allocator, Op.f64_sub) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
                },
            }
        },
        .num_shift_left_by => {
            const shift_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitProcLocal(args[1]);
            try self.emitLocalSet(shift_local);

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, shift_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(shiftBitWidth(layout_idx))) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(if (vt == .i64) WasmModule.BlockType.i64 else WasmModule.BlockType.i32)) catch return error.OutOfMemory;

            if (vt == .i64) {
                self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            } else {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            }

            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.emitProcLocal(args[0]);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, shift_local) catch return error.OutOfMemory;
            if (vt == .i64) self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_shl,
                .i64 => Op.i64_shl,
                .f32, .f64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .num_shift_right_by => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            if (vt == .i64) self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_shr_s,
                .i64 => Op.i64_shr_s,
                .f32, .f64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_shift_right_zf_by => {
            const shift_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitProcLocal(args[1]);
            try self.emitLocalSet(shift_local);

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, shift_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(shiftBitWidth(layout_idx))) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(if (vt == .i64) WasmModule.BlockType.i64 else WasmModule.BlockType.i32)) catch return error.OutOfMemory;

            if (vt == .i64) {
                self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            } else {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
            }

            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.emitProcLocal(args[0]);
            if (shiftNeedsZeroFillMask(layout_idx) and vt == .i32) {
                self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                const mask: i32 = if (layout_idx == .i8) 0xFF else 0xFFFF;
                WasmModule.leb128WriteI32(self.allocator, &self.body, mask) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            }
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, shift_local) catch return error.OutOfMemory;
            if (vt == .i64) self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_shr_u,
                .i64 => Op.i64_shr_u,
                .f32, .f64 => unreachable,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        else => unreachable,
    }
}

/// Generate string equality comparison using roc_str_eq host function.
/// Both lhs and rhs should produce i32 pointers to 12-byte RocStr values.
fn emitStrEq(self: *Self, lhs: ProcLocalId, rhs: ProcLocalId, negate: bool) Allocator.Error!void {
    const import_idx = self.str_eq_import orelse unreachable;

    // Generate both string expressions, store to locals
    try self.emitProcLocal(lhs);
    const lhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_local);

    try self.emitProcLocal(rhs);
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
fn emitListEq(self: *Self, lhs: ProcLocalId, rhs: ProcLocalId, list_layout_idx: layout.Idx, negate: bool) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const list_layout = ls.getLayout(list_layout_idx);
    std.debug.assert(list_layout.tag == .list);
    const elem_layout = self.listElemLayout(list_layout_idx);
    try self.emitListEqWithElemLayout(lhs, rhs, elem_layout, negate);
}

/// Generate list equality with a known element layout.
/// Supports all element types including strings and nested lists.
fn emitListEqWithElemLayout(self: *Self, lhs: ProcLocalId, rhs: ProcLocalId, elem_layout: layout.Idx, negate: bool) Allocator.Error!void {
    // Generate both list expressions, store to locals
    try self.emitProcLocal(lhs);
    const lhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_local);

    try self.emitProcLocal(rhs);
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
        } else if (builtinInternalLayoutContainsRefcounted(ls, "wasm.emitListEqWithElemLayout.builtin_elem_rc", elem_layout)) {
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

fn emitNormalizedIntParts(
    self: *Self,
    value: ProcLocalId,
    int_width_bytes: u8,
    is_signed: bool,
) Allocator.Error!struct { low: u32, high: u32 } {
    if (int_width_bytes == 16) {
        try self.emitProcLocal(value);
        const value_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(value_ptr);

        try self.emitLocalGet(value_ptr);
        try self.emitLoadOp(.i64, 0);
        const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
        try self.emitLocalSet(low);

        try self.emitLocalGet(value_ptr);
        try self.emitLoadOp(.i64, 8);
        const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
        try self.emitLocalSet(high);

        return .{ .low = low, .high = high };
    }

    const value_vt = self.procLocalValType(value);
    if (value_vt == .i64) {
        try self.emitProcLocal(value);
        const raw = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
        try self.emitLocalSet(raw);

        const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
        if (int_width_bytes < 8) {
            const shift_amount: i64 = switch (int_width_bytes) {
                1 => 56,
                2 => 48,
                4 => 32,
                else => unreachable,
            };
            try self.emitLocalGet(raw);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, shift_amount) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, shift_amount) catch return error.OutOfMemory;
            self.body.append(self.allocator, if (is_signed) Op.i64_shr_s else Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalSet(low);
        } else {
            try self.emitLocalGet(raw);
            try self.emitLocalSet(low);
        }

        const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
        if (is_signed) {
            try self.emitLocalGet(low);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 63) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
        } else {
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        }
        try self.emitLocalSet(high);

        return .{ .low = low, .high = high };
    }

    try self.emitProcLocal(value);
    const raw = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(raw);

    const normalized = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(raw);
    switch (int_width_bytes) {
        1 => if (is_signed) {
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        } else {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        2 => if (is_signed) {
            self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
        } else {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFFFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        4 => {},
        else => unreachable,
    }
    try self.emitLocalSet(normalized);

    const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalGet(normalized);
    self.body.append(self.allocator, if (is_signed) Op.i64_extend_i32_s else Op.i64_extend_i32_u) catch return error.OutOfMemory;
    try self.emitLocalSet(low);

    const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    if (is_signed) {
        try self.emitLocalGet(low);
        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, &self.body, 63) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
    } else {
        self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    }
    try self.emitLocalSet(high);

    return .{ .low = low, .high = high };
}

fn emitIntToStr(self: *Self, value: ProcLocalId, int_width_bytes: u8, is_signed: bool) Allocator.Error!void {
    const import_idx = self.int_to_str_import orelse unreachable;
    const parts = try self.emitNormalizedIntParts(value, int_width_bytes, is_signed);
    const buf_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitHeapAllocConst(48, 1);
    try self.emitLocalSet(buf_ptr);

    try self.emitLocalGet(parts.low);
    try self.emitLocalGet(parts.high);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, int_width_bytes) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, if (is_signed) 1 else 0) catch return error.OutOfMemory;
    try self.emitLocalGet(buf_ptr);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(len_local);

    try self.buildHeapRocStr(buf_ptr, len_local);
}

fn emitDecToStr(self: *Self, value: ProcLocalId) Allocator.Error!void {
    const import_idx = self.dec_to_str_import orelse unreachable;

    try self.emitProcLocal(value);
    const dec_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(dec_ptr);

    const buf_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitHeapAllocConst(48, 1);
    try self.emitLocalSet(buf_ptr);

    try self.emitLocalGet(dec_ptr);
    try self.emitLocalGet(buf_ptr);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(len_local);

    try self.buildHeapRocStr(buf_ptr, len_local);
}

fn emitFloatToStr(self: *Self, value: ProcLocalId, is_f32: bool) Allocator.Error!void {
    const import_idx = self.float_to_str_import orelse unreachable;

    if (is_f32) {
        try self.emitProcLocal(value);
        const raw_f32 = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
        try self.emitLocalSet(raw_f32);
        try self.emitLocalGet(raw_f32);
        self.body.append(self.allocator, Op.i32_reinterpret_f32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    } else {
        try self.emitProcLocal(value);
        const raw_f64 = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
        try self.emitLocalSet(raw_f64);
        try self.emitLocalGet(raw_f64);
        self.body.append(self.allocator, Op.i64_reinterpret_f64) catch return error.OutOfMemory;
    }

    const bits_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(bits_local);

    const buf_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitHeapAllocConst(400, 1);
    try self.emitLocalSet(buf_ptr);

    try self.emitLocalGet(bits_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, if (is_f32) 1 else 0) catch return error.OutOfMemory;
    try self.emitLocalGet(buf_ptr);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(len_local);

    try self.buildHeapRocStr(buf_ptr, len_local);
}

fn emitStrEscapeAndQuote(self: *Self, value: ProcLocalId) Allocator.Error!void {
    const import_idx = self.str_escape_and_quote_import orelse unreachable;

    try self.emitProcLocal(value);
    const str_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(str_ptr);

    const result_offset = try self.allocStackMemory(12, 4);
    try self.emitLocalGet(str_ptr);
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
}

/// Generate str_to_utf8: convert RocStr to RocList(U8).
/// SSO strings have their bytes copied to heap memory.
/// Non-SSO strings share the same layout, so the 12 bytes are copied directly.
fn emitStrToUtf8(self: *Self, str_arg: ProcLocalId) Allocator.Error!void {
    // Generate the string expression (produces i32 pointer to 12-byte RocStr)
    try self.emitProcLocal(str_arg);
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
fn emitStrFromUtf8Lossy(self: *Self, list_arg: ProcLocalId) Allocator.Error!void {
    // Generate the list expression (produces i32 pointer to 12-byte RocList)
    try self.emitProcLocal(list_arg);
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

const StrSearchMode = enum { contains, starts_with, ends_with };

/// Generate LowLevel str_contains / str_starts_with / str_ends_with.
/// Compares bytes using a nested loop (naive O(n*m) search).
fn generateLLStrSearch(self: *Self, args: anytype, mode: StrSearchMode) Allocator.Error!void {
    // Generate both string args
    try self.emitProcLocal(args[0]);
    const a_str = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(a_str);
    try self.emitProcLocal(args[1]);
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
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory; // break out of block (skip if + loop + block)
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
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListAppend.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;
    const elem_layout_idx = list_abi.elem_layout_idx orelse ret_layout;
    const import_idx = self.list_append_unsafe_import orelse unreachable;

    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, list_ptr) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    const empty_list_offset = try self.allocStackMemory(12, 4);
    const empty_list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(empty_list_offset);
    try self.emitLocalSet(empty_list_ptr);
    try self.emitZeroInit(empty_list_ptr, 12);
    try self.emitLocalGet(empty_list_ptr);
    try self.emitLocalSet(list_ptr);
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    const elem_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    if (elem_size == 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        try self.emitLocalSet(elem_ptr);
    } else {
        const target_is_composite = self.isCompositeLayout(elem_layout_idx);
        if (target_is_composite) {
            try self.emitProcLocal(args[1]);
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(src_local);
            const dst_offset = try self.allocStackMemory(elem_size, elem_align);
            try self.emitFpOffset(dst_offset);
            try self.emitLocalSet(elem_ptr);
            try self.emitMemCopy(elem_ptr, 0, src_local, elem_size);
        } else {
            const elem_vt = self.resolveValType(elem_layout_idx);
            try self.emitProcLocal(args[1]);
            try self.emitConversion(self.procLocalValType(args[1]), elem_vt);
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
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListPrepend.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    // Generate list and element
    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);
    try self.emitProcLocal(args[1]);
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
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListConcat.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;
    if (elem_size == 0) {
        try self.emitProcLocal(args[0]);
        const a_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(a_ptr);
        try self.emitProcLocal(args[1]);
        const b_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(b_ptr);

        const a_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(a_ptr);
        try self.emitLoadOp(.i32, 4);
        try self.emitLocalSet(a_len);

        const b_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(b_ptr);
        try self.emitLoadOp(.i32, 4);
        try self.emitLocalSet(b_len);

        const new_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(a_len);
        try self.emitLocalGet(b_len);
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        try self.emitLocalSet(new_len);

        const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        try self.emitLocalSet(zero);

        try self.buildRocListWithCap(zero, new_len, new_len);
        return;
    }

    const import_idx = self.list_concat_import orelse unreachable;

    try self.emitProcLocal(args[0]);
    const a_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(a_ptr);
    try self.emitProcLocal(args[1]);
    const b_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(b_ptr);

    const result_offset = try self.allocStackMemory(12, 4);
    try self.emitLocalGet(a_ptr);
    try self.emitLocalGet(b_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_align)) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
}

/// Generate LowLevel list_drop_at: remove element at index, returning new list.
fn generateLLListDropAt(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const import_idx = self.list_drop_at_import orelse unreachable;
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListDropAt.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    try self.emitProcLocal(args[1]);
    try self.emitConversion(self.procLocalValType(args[1]), .i32);
    const index_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(index_local);

    const result_offset = try self.allocStackMemory(12, 4);
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_align)) catch return error.OutOfMemory;
    try self.emitLocalGet(index_local);
    try self.emitFpOffset(result_offset);
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
}

/// Generate LowLevel list_reverse: create new list with elements in reverse order.
fn generateLLListReverse(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListReverse.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    if (elem_size == 0) {
        try self.emitProcLocal(args[0]);
        return;
    }

    const import_idx = self.list_reverse_import orelse unreachable;
    const elem_align = list_abi.elem_align;

    try self.emitProcLocal(args[0]);
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

/// Generate list_with_capacity: create empty list with given capacity
fn generateLLListWithCapacity(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListWithCapacity.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    // Generate capacity arg (may be i64 from MonoIR layout; convert to i32 for wasm32)
    try self.emitProcLocal(args[0]);
    try self.emitConversion(self.procLocalValType(args[0]), .i32);
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
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListSet.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    // Generate list arg
    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    // Generate index arg (may be i64 from MonoIR; convert to i32 for wasm32)
    try self.emitProcLocal(args[1]);
    try self.emitConversion(self.procLocalValType(args[1]), .i32);
    const index = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(index);

    // Generate element arg
    try self.emitProcLocal(args[2]);
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
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListReserve.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    // Generate list arg
    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    // Generate additional capacity arg (may be i64 from MonoIR; convert to i32 for wasm32)
    try self.emitProcLocal(args[1]);
    try self.emitConversion(self.procLocalValType(args[1]), .i32);
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
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListReleaseExcessCapacity.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    // Generate list arg
    try self.emitProcLocal(args[0]);
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

const ListElementPairLayout = struct {
    result_size: u32,
    result_align: u32,
    elem_offset: u32,
    list_offset: u32,
    list_layout: layout.Idx,
};

fn resolveListElementPairLayout(self: *Self, ret_layout: layout.Idx) ListElementPairLayout {
    const ls = self.getLayoutStore();
    const ret_layout_val = ls.getLayout(ret_layout);
    if (ret_layout_val.tag != .struct_) unreachable;

    const record_idx = ret_layout_val.data.struct_.idx;
    const record_data = ls.getStructData(record_idx);
    const field0_layout = ls.getStructFieldLayout(record_idx, 0);
    const field1_layout = ls.getStructFieldLayout(record_idx, 1);
    const field0_val = ls.getLayout(field0_layout);
    const field0_is_list = field0_val.tag == .list or field0_val.tag == .list_of_zst;

    return .{
        .result_size = record_data.size,
        .result_align = @intCast(ret_layout_val.alignment(ls.targetUsize()).toByteUnits()),
        .elem_offset = if (field0_is_list)
            ls.getStructFieldOffset(record_idx, 1)
        else
            ls.getStructFieldOffset(record_idx, 0),
        .list_offset = if (field0_is_list)
            ls.getStructFieldOffset(record_idx, 0)
        else
            ls.getStructFieldOffset(record_idx, 1),
        .list_layout = if (field0_is_list) field0_layout else field1_layout,
    };
}

/// Generate list_split_first: split list into first element and rest
fn generateLLListSplitFirst(self: *Self, args: anytype, _ret_layout: layout.Idx) Allocator.Error!void {
    const pair = self.resolveListElementPairLayout(_ret_layout);
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListSplitFirst.builtin_list_abi", pair.list_layout);
    const elem_size = list_abi.elem_size;
    // Generate list arg
    try self.emitProcLocal(args[0]);
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

    const result_offset = try self.allocStackMemory(pair.result_size, pair.result_align);
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
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(pair.elem_offset)) catch return error.OutOfMemory;
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
    try self.emitPrepareListSliceMetadata(list_ptr, list_abi.elements_refcounted, encoded_cap);

    // Store rest list in result struct
    const rest_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(result_ptr);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(pair.list_offset)) catch return error.OutOfMemory;
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
fn generateLLListSplitLast(self: *Self, args: anytype, _ret_layout: layout.Idx) Allocator.Error!void {
    const pair = self.resolveListElementPairLayout(_ret_layout);
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListSplitLast.builtin_list_abi", pair.list_layout);
    const elem_size = list_abi.elem_size;

    // Generate list arg
    try self.emitProcLocal(args[0]);
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

    const result_offset = try self.allocStackMemory(pair.result_size, pair.result_align);
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
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(pair.list_offset)) catch return error.OutOfMemory;
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
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(pair.elem_offset)) catch return error.OutOfMemory;
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

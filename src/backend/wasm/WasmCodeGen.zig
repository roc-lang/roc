//! Statement-only LIR -> WebAssembly code generator.
//!
//! Walks explicit `CFStmt` procedure bodies and emits wasm instructions.
//! All value-producing work is expressed through explicit local assignments;
//! there is no runtime expression-tree interpretation in the active code path.
//!
//! RC boundary:
//! - explicit RC lowering happens through `generateRcStmt`
//! - each RC statement's count-update atomicity selects which compiled helper
//!   entry it reaches (see `rcHelperCacheKey`)
//! - builtin/runtime helper implementations may perform primitive-internal RC
//! - ordinary wasm lowering is forbidden from inventing ownership policy

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const base = @import("base");
const builtins = @import("builtins");
const layout = @import("layout");

const lir = @import("lir");
const LIR = lir.LIR;
const LirStore = lir.LirStore;
const RcHelperKey = layout.RcHelperKey;
const RcHelperPlan = layout.RcHelperPlan;
const RcAtomicity = LIR.RcAtomicity;
const RcListPlan = layout.ListPlan;
const ProcLocalId = LIR.LocalId;
const ProcLocalSpan = LIR.LocalSpan;
const RefOp = LIR.RefOp;
const WasmModule = @import("WasmModule.zig");
const WasmLinking = @import("WasmLinking.zig");
const WasmLayout = @import("WasmLayout.zig");
const Storage = @import("Storage.zig");
const CodeBuilder = @import("CodeBuilder.zig");
const BuiltinSignatures = @import("builtin_signatures.zig");
const index_types = @import("index_types.zig");
const Op = WasmModule.Op;
const ValType = WasmModule.ValType;
const BlockType = WasmModule.BlockType;
const FunctionIndex = index_types.FunctionIndex;
const LocalFunctionIndex = index_types.LocalFunctionIndex;
const SymbolIndex = index_types.SymbolIndex;
const BuiltinKind = BuiltinSignatures.BuiltinKind;

const LirProcSpec = LIR.LirProcSpec;
const CFStmtId = LIR.CFStmtId;
const RcOpKind = enum { incref, decref, free };
const ExternalCalls = union(enum) {
    unconfigured,
    host_imports,
    builtin_relocs: BuiltinSignatures.SymbolTable,
};
const DataAddress = struct {
    offset: u32,
    symbol: SymbolIndex,
};

/// Stack prologues emit exactly two relocatable stack-pointer operands:
/// one `global.get __stack_pointer` and one `global.set __stack_pointer`.
const max_stack_prefix_relocations = 2;

// A wasm u32/i32 LEB operand is at most 5 bytes.
const max_wasm_leb32_bytes = 5;
// Relocatable wasm indices are emitted as 5-byte padded LEB operands.
const max_reloc_leb32_bytes = 5;

const max_stack_frame_prefix_bytes =
    (1 + max_reloc_leb32_bytes) + // global.get __stack_pointer
    (1 + max_wasm_leb32_bytes) + // i32.const frame_size
    1 + // i32.sub
    (1 + max_wasm_leb32_bytes) + // local.tee fp
    (1 + max_reloc_leb32_bytes); // global.set __stack_pointer

const max_i32_store_local_prefix_bytes =
    (1 + max_wasm_leb32_bytes) + // i32.const base
    (1 + max_wasm_leb32_bytes) + // local.get value
    (1 + max_wasm_leb32_bytes + max_wasm_leb32_bytes); // i32.store align offset

const max_i32_store_const_prefix_bytes =
    (1 + max_wasm_leb32_bytes) + // i32.const base
    (1 + max_wasm_leb32_bytes) + // i32.const value
    (1 + max_wasm_leb32_bytes + max_wasm_leb32_bytes); // i32.store align offset

const max_roc_ops_prefix_bytes =
    (1 + max_wasm_leb32_bytes) + // i32.const RocOps address
    (1 + max_wasm_leb32_bytes) + // local.set roc_ops_local
    max_i32_store_local_prefix_bytes + // env pointer
    8 * max_i32_store_const_prefix_bytes; // alloc/dealloc/realloc/dbg/expect/crashed/hosted count/hosted ptr

/// Stack prefixes are assembled after body generation because the frame size is
/// only known then. The synthetic standalone main prefix additionally initializes
/// RocOps, so the bound includes both the stack-frame prologue and RocOps stores.
const max_stack_prefix_bytes = max_stack_frame_prefix_bytes + max_roc_ops_prefix_bytes;

const StackPrefixRelocs = struct {
    buffer: [max_stack_prefix_relocations]CodeBuilder.Relocation = undefined,
    len: usize = 0,

    fn append(self: *StackPrefixRelocs, reloc: CodeBuilder.Relocation) void {
        if (self.len >= self.buffer.len) {
            wasmInvariantFmt("WASM/codegen invariant violated: stack prefix relocation buffer exceeded {d}", .{max_stack_prefix_relocations});
        }
        self.buffer[self.len] = reloc;
        self.len += 1;
    }

    fn items(self: *const StackPrefixRelocs) []const CodeBuilder.Relocation {
        return self.buffer[0..self.len];
    }
};

const StackPrefixBytes = struct {
    fixed: std.heap.FixedBufferAllocator,
    bytes: std.ArrayList(u8) = .empty,

    fn init(buffer: []u8) StackPrefixBytes {
        return .{ .fixed = std.heap.FixedBufferAllocator.init(buffer) };
    }

    fn allocator(self: *StackPrefixBytes) Allocator {
        return self.fixed.allocator();
    }
};

const LayoutStore = layout.Store;
const wasm_roc_ops_alloc_offset: u32 = 4;
const wasm_roc_ops_dealloc_offset: u32 = 8;
const wasm_roc_ops_dbg_offset: u32 = 16;
const wasm_roc_ops_expect_failed_offset: u32 = 20;
const wasm_roc_ops_crashed_offset: u32 = 24;
const wasm_roc_ops_hosted_fns_count_offset: u32 = 28;
const wasm_roc_ops_hosted_fns_ptr_offset: u32 = 32;
const wasm_erased_callable_on_drop_offset: u32 = 4;

const Self = @This();

fn wasmInvariantFmt(comptime fmt: []const u8, args: anytype) noreturn {
    if (builtin.mode == .Debug) std.debug.panic(fmt, args);
    unreachable;
}

fn builtinInternalLayoutContainsRefcounted(ls: *const LayoutStore, comptime _: []const u8, layout_idx: layout.Idx) bool {
    return ls.layoutContainsRefcounted(ls.getLayout(layout_idx));
}

const BuiltinListAbi = struct {
    elem_layout_idx: ?layout.Idx,
    elem_layout: layout.Layout,
    elem_size: u32,
    elem_align: u32,
    elements_refcounted: bool,
};

const RocListFields = struct {
    bytes: u32,
    len: u32,
    cap: u32,
};

const RocStrFields = struct {
    bytes: u32,
    len: u32,
    cap: u32,
};

const StrMatchSourceShape = struct {
    str: u32,
    bytes: u32,
    len: u32,
    is_small: u32,
    allocation: u32,
};

const RocListElementCallbacks = struct {
    elements_refcounted: u32,
    incref_table_idx: u32,
    decref_table_idx: u32,
};

const RcAllocationLayout = struct {
    data_offset: u32,
    allocation_alignment: u32,
};

fn rcAllocationLayout(element_alignment: u32, elements_refcounted: bool) RcAllocationLayout {
    const ptr_width: u32 = 4;
    const required_space: u32 = if (elements_refcounted) 2 * ptr_width else ptr_width;
    return .{
        .data_offset = @max(required_space, element_alignment),
        .allocation_alignment = @max(ptr_width, element_alignment),
    };
}

const TryUnsafeOffsets = struct {
    success: u32,
    value: u32,
};

fn tryUnsafeOffsets(self: *const Self, ret_layout: layout.Idx) TryUnsafeOffsets {
    const ret_layout_val = self.getLayoutStore().getLayout(ret_layout);
    if (ret_layout_val.tag != .struct_) {
        wasmInvariantFmt("try_unsafe result expected struct layout, got {s}", .{@tagName(ret_layout_val.tag)});
    }
    const struct_idx = ret_layout_val.getStruct().idx;
    return .{
        .success = self.getLayoutStore().getStructFieldOffsetByOriginalIndex(struct_idx, 0),
        .value = self.getLayoutStore().getStructFieldOffsetByOriginalIndex(struct_idx, 1),
    };
}

fn builtinInternalListAbi(self: *const Self, comptime _: []const u8, list_layout_idx: layout.Idx) BuiltinListAbi {
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
pending_bodies: std.AutoHashMap(LocalFunctionIndex, CodeBuilder),
active_fn_stack: std.ArrayList(LocalFunctionIndex),
storage: Storage,
/// Accumulated stack frame size for the current function (for stack memory values).
stack_frame_size: u32 = 0,
/// Whether the current function uses stack memory (needs prologue/epilogue).
uses_stack_memory: bool = false,
/// Local index of the frame pointer ($fp) - only valid when uses_stack_memory is true.
fp_local: u32 = 0,
/// Map from proc spec id → compiled wasm function index.
registered_procs: std.AutoHashMap(u32, u32),
/// Hosted platform functions resolved to linker symbols, keyed by dispatch
/// index. Populated before proc compilation so imports never shift function
/// indices mid-codegen.
hosted_symbol_targets: std.AutoHashMap(u32, HostedSymbolTarget),
/// Whether generated code uses the symbol ABI: allocation and diagnostics go
/// directly to the host's linker symbols instead of through the RocOps
/// vtable. Build output uses the symbol ABI; the playground/eval module
/// flavor keeps the vtable so its JS host contract is unchanged.
symbol_abi: bool = false,
/// The fixed runtime symbols, resolved like hosted symbols (symbol ABI only).
runtime_symbol_targets: ?RuntimeSymbolTargets = null,
/// Map from wasm function index → function symbol used by relocatable direct calls.
function_symbols_by_index: std.AutoHashMap(u32, SymbolIndex),
/// Owned names used by generated local function symbols.
function_symbol_names: std.ArrayList([]u8),
/// Map from RC helper key → compiled wasm function index.
rc_helper_funcs: std.AutoHashMap(u64, u32),
/// Map from RC helper key → wasm table index for erased-callable final-drop callbacks.
rc_helper_table_indices: std.AutoHashMap(u64, u32),
/// Map from proc spec id → wasm table index (for proc_ref literals).
proc_table_indices: std.AutoHashMap(u32, u32),
/// Map from LIR string backing id → wasm data offset for the backing bytes.
static_str_offsets: std.AutoHashMap(u32, DataAddress),
/// Owned names used by generated data segments and local data symbols.
static_data_names: std.ArrayList([]u8),
static_data_name_counter: u32 = 0,
/// Cache of function type signatures → wasm type index.
func_type_cache: std.StringHashMap(u32),
/// Scratch buffer for function type cache keys.
func_type_key_scratch: std.ArrayList(u8),
/// Type index for the generic 2-arg indirect call signature: (i32, i32) -> void.
/// Used for erased-callable on_drop function pointers.
roc_ops_type_idx: u32 = 0,
/// Type index for roc_alloc: (roc_ops, length, alignment) -> ptr.
roc_alloc_type_idx: u32 = 0,
/// Type index for roc_dealloc: (roc_ops, ptr, alignment) -> void.
roc_dealloc_type_idx: u32 = 0,
/// Type index for roc_realloc: (roc_ops, ptr, new_length, alignment) -> ptr.
roc_realloc_type_idx: u32 = 0,
/// Type index for roc_dbg/roc_expect_failed/roc_crashed: (roc_ops, bytes, len) -> void.
roc_diag_type_idx: u32 = 0,
indirect_call_types_registered: bool = false,
/// Table indices for RocOps functions (used with erased calls via wasm `call_indirect`).
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
/// Stack of loop-break label depths for lowering explicit LIR loop_break.
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
/// Wasm function index for imported roc_dec_pow host function.
dec_pow_import: ?u32 = null,
/// Wasm function index for imported roc_dec_sqrt host function.
dec_sqrt_import: ?u32 = null,
/// Wasm function index for imported roc_dec_sin host function.
dec_sin_import: ?u32 = null,
/// Wasm function index for imported roc_dec_cos host function.
dec_cos_import: ?u32 = null,
/// Wasm function index for imported roc_dec_tan host function.
dec_tan_import: ?u32 = null,
/// Wasm function index for imported roc_dec_asin host function.
dec_asin_import: ?u32 = null,
/// Wasm function index for imported roc_dec_acos host function.
dec_acos_import: ?u32 = null,
/// Wasm function index for imported roc_dec_atan host function.
dec_atan_import: ?u32 = null,
/// Wasm function index for imported roc_i128_to_str host function.
i128_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_u128_to_str host function.
u128_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_int_to_str host function.
int_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_float_to_str host function.
float_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_float_pow host function.
float_pow_import: ?u32 = null,
/// Wasm function index for imported roc_float_sin host function.
float_sin_import: ?u32 = null,
/// Wasm function index for imported roc_float_cos host function.
float_cos_import: ?u32 = null,
/// Wasm function index for imported roc_float_tan host function.
float_tan_import: ?u32 = null,
/// Wasm function index for imported roc_float_asin host function.
float_asin_import: ?u32 = null,
/// Wasm function index for imported roc_float_acos host function.
float_acos_import: ?u32 = null,
/// Wasm function index for imported roc_float_atan host function.
float_atan_import: ?u32 = null,
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
str_find_first_import: ?u32 = null,
str_drop_prefix_caseless_ascii_import: ?u32 = null,
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
list_reserve_import: ?u32 = null,
list_reverse_import: ?u32 = null,
list_replace_import: ?u32 = null,
list_swap_import: ?u32 = null,
/// Wasm function indices for the imported hasher host functions.
dict_pseudo_seed_import: ?u32 = null,
hasher_finish_import: ?u32 = null,
hasher_write_u64_import: ?u32 = null,
hasher_write_u128_import: ?u32 = null,
hasher_write_f32_bits_import: ?u32 = null,
hasher_write_f64_bits_import: ?u32 = null,
hasher_write_bytes_import: ?u32 = null,
hasher_write_str_import: ?u32 = null,
/// Wasm function indices for imported crypto host functions.
crypto_sha256_hash_bytes_import: ?u32 = null,
crypto_sha256_hasher_empty_import: ?u32 = null,
crypto_sha256_hasher_write_import: ?u32 = null,
crypto_sha256_hasher_finish_import: ?u32 = null,
crypto_blake3_hash_bytes_import: ?u32 = null,
crypto_blake3_hasher_empty_import: ?u32 = null,
crypto_blake3_hasher_write_import: ?u32 = null,
crypto_blake3_hasher_finish_import: ?u32 = null,
/// Configurable wasm stack size in bytes (default 1MB).
wasm_stack_bytes: u32 = 1024 * 1024,
/// Configurable wasm memory pages (0 = auto-compute from stack size).
wasm_memory_pages: u32 = 0,
/// Explicit strategy for calls to compiler-provided helper/builtin functions.
external_calls: ExternalCalls = .unconfigured,
/// Undefined `__stack_pointer` symbol used only while emitting relocatable app objects.
stack_pointer_symbol: ?SymbolIndex = null,
/// Undefined `__indirect_function_table` symbol used only while emitting relocatable app objects.
indirect_table_symbol: ?SymbolIndex = null,
/// Whether generated static-data addresses must remain relocatable for object output.
relocatable_object: bool = false,
/// Whether final in-memory codegen should still emit relocation edges for
/// static-data addresses so final DCE can trace data liveness explicitly.
track_static_data_addresses: bool = false,
pub fn init(allocator: Allocator, store: *const LirStore, layout_store: *const LayoutStore) Self {
    return .{
        .allocator = allocator,
        .store = store,
        .layout_store = layout_store,
        .module = WasmModule.init(allocator),
        .pending_bodies = std.AutoHashMap(LocalFunctionIndex, CodeBuilder).init(allocator),
        .active_fn_stack = .empty,
        .storage = Storage.init(allocator),
        .stack_frame_size = 0,
        .uses_stack_memory = false,
        .fp_local = 0,
        .registered_procs = std.AutoHashMap(u32, u32).init(allocator),
        .hosted_symbol_targets = std.AutoHashMap(u32, HostedSymbolTarget).init(allocator),
        .function_symbols_by_index = std.AutoHashMap(u32, SymbolIndex).init(allocator),
        .function_symbol_names = .empty,
        .rc_helper_funcs = std.AutoHashMap(u64, u32).init(allocator),
        .rc_helper_table_indices = std.AutoHashMap(u64, u32).init(allocator),
        .proc_table_indices = std.AutoHashMap(u32, u32).init(allocator),
        .static_str_offsets = std.AutoHashMap(u32, DataAddress).init(allocator),
        .static_data_names = .empty,
        .func_type_cache = std.StringHashMap(u32).init(allocator),
        .func_type_key_scratch = .empty,
        .join_point_depths = std.AutoHashMap(u32, u32).init(allocator),
        .join_point_param_locals = std.AutoHashMap(u32, []u32).init(allocator),
        .join_point_state_locals = std.AutoHashMap(u32, u32).init(allocator),
        .active_stmt_generations = std.AutoHashMap(u32, void).init(allocator),
        .stmt_generation_counts = std.AutoHashMap(u32, u32).init(allocator),
        .loop_continue_target_depths = .empty,
        .loop_break_target_depths = .empty,
    };
}

pub fn initWithModule(allocator: Allocator, store: *const LirStore, layout_store: *const LayoutStore, module: *WasmModule) Self {
    var self = Self.init(allocator, store, layout_store);
    self.module.deinit();
    self.module = module.*;
    module.* = WasmModule.init(allocator);
    return self;
}

/// Configure helper calls to use relocations against builtin wasm symbols.
pub fn configureBuiltinRelocs(self: *Self, symbols: BuiltinSignatures.SymbolTable) void {
    self.external_calls = .{ .builtin_relocs = symbols };
}

/// Configure relocatable stack-pointer operands to target the imported global symbol.
pub fn configureStackPointerReloc(self: *Self, symbol: SymbolIndex) void {
    self.stack_pointer_symbol = symbol;
}

/// Configure relocatable indirect-call table operands to target the imported table symbol.
pub fn configureTableReloc(self: *Self, symbol: SymbolIndex) void {
    self.indirect_table_symbol = symbol;
}

/// Configure generated data-address operands for relocatable object output.
pub fn configureRelocatableObject(self: *Self) void {
    self.relocatable_object = true;
}

/// Configure final in-memory codegen to keep explicit relocation edges from
/// instructions to static data. `resolveRelocations()` still patches the known
/// final address before encode; the relocation entry exists for DCE liveness.
pub fn configureStaticDataAddressTracking(self: *Self) void {
    self.track_static_data_addresses = true;
}

pub fn deinit(self: *Self) void {
    self.module.deinit();
    var body_it = self.pending_bodies.valueIterator();
    while (body_it.next()) |body| {
        body.deinit(self.allocator);
    }
    self.pending_bodies.deinit();
    self.active_fn_stack.deinit(self.allocator);
    self.storage.deinit();
    self.registered_procs.deinit();
    self.hosted_symbol_targets.deinit();
    self.function_symbols_by_index.deinit();
    for (self.function_symbol_names.items) |name| {
        self.allocator.free(name);
    }
    self.function_symbol_names.deinit(self.allocator);
    self.rc_helper_funcs.deinit();
    self.rc_helper_table_indices.deinit();
    self.proc_table_indices.deinit();
    self.static_str_offsets.deinit();
    for (self.static_data_names.items) |name| {
        self.allocator.free(name);
    }
    self.static_data_names.deinit(self.allocator);
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
    self.loop_break_target_depths.deinit(self.allocator);
}

fn beginFunction(self: *Self, local_idx: LocalFunctionIndex) Allocator.Error!void {
    const gop = try self.pending_bodies.getOrPut(local_idx);
    if (gop.found_existing) {
        if (builtin.mode == .Debug) {
            std.debug.panic("WasmCodeGen invariant violated: duplicate body for local function {d}", .{local_idx.raw()});
        }
        unreachable;
    }
    gop.value_ptr.* = CodeBuilder.init();
    try self.active_fn_stack.append(self.allocator, local_idx);
}

fn currentBody(self: *Self) *CodeBuilder {
    if (self.active_fn_stack.items.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("WasmCodeGen invariant violated: no active function body", .{});
        }
        unreachable;
    }
    const local_idx = self.active_fn_stack.items[self.active_fn_stack.items.len - 1];
    return self.pending_bodies.getPtr(local_idx).?;
}

fn currentCode(self: *Self) *std.ArrayList(u8) {
    return &self.currentBody().code;
}

fn endFunction(self: *Self) void {
    _ = self.active_fn_stack.pop();
}

fn emitBuiltinCall(self: *Self, kind: BuiltinKind, host_import: ?u32) Allocator.Error!void {
    switch (self.external_calls) {
        .host_imports => {
            const import_idx = host_import orelse wasmInvariantFmt(
                "WASM/codegen invariant violated: missing host import for builtin {s}",
                .{@tagName(kind)},
            );
            try self.emitCall(import_idx);
        },
        .builtin_relocs => |symbols| {
            const symbol = symbols.get(kind);
            try self.currentBody().emitRelocatableCall(self.allocator, symbol, self.functionIndexForSymbol(symbol));
        },
        .unconfigured => wasmInvariantFmt(
            "WASM/codegen invariant violated: external calls not configured before builtin {s}",
            .{@tagName(kind)},
        ),
    }
}

fn emitI32Const(self: *Self, value: i32) Allocator.Error!void {
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), value) catch return error.OutOfMemory;
}

/// Select a builtin wrapper's update-mode immediate from the statement's
/// statically-proven-unique argument mask: `.InPlace` when the bit for
/// `arg_index` says that argument's runtime uniqueness check is redundant,
/// `.Immutable` (checked) otherwise.
fn updateModeImmForArg(unique_args: u64, arg_index: u6) i32 {
    return @intFromEnum(if ((unique_args >> arg_index) & 1 != 0) builtins.utils.UpdateMode.InPlace else builtins.utils.UpdateMode.Immutable);
}

fn loadRocListFields(self: *Self, list_ptr: u32) Allocator.Error!RocListFields {
    const fields = RocListFields{
        .bytes = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
    };

    try self.emitLocalGet(list_ptr);
    try self.emitLoadOp(.i32, 0);
    try self.emitLocalSet(fields.bytes);

    try self.emitLocalGet(list_ptr);
    try self.emitLoadOp(.i32, 4);
    try self.emitLocalSet(fields.len);

    try self.emitLocalGet(list_ptr);
    try self.emitLoadOp(.i32, 8);
    try self.emitLocalSet(fields.cap);

    return fields;
}

fn emitRocListFields(self: *Self, fields: RocListFields) Allocator.Error!void {
    try self.emitLocalGet(fields.bytes);
    try self.emitLocalGet(fields.len);
    try self.emitLocalGet(fields.cap);
}

fn loadRocStrFields(self: *Self, str_ptr: u32) Allocator.Error!RocStrFields {
    const fields = RocStrFields{
        .bytes = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
    };

    try self.emitLocalGet(str_ptr);
    try self.emitLoadOp(.i32, 0);
    try self.emitLocalSet(fields.bytes);

    try self.emitLocalGet(str_ptr);
    try self.emitLoadOp(.i32, 8);
    try self.emitLocalSet(fields.len);

    try self.emitLocalGet(str_ptr);
    try self.emitLoadOp(.i32, 4);
    try self.emitLocalSet(fields.cap);

    return fields;
}

fn emitRocStrFields(self: *Self, fields: RocStrFields) Allocator.Error!void {
    try self.emitLocalGet(fields.bytes);
    try self.emitLocalGet(fields.len);
    try self.emitLocalGet(fields.cap);
}

fn emitAddressOffsetToLocal(self: *Self, base_local: u32, offset: u32) Allocator.Error!u32 {
    const result = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(base_local);
    if (offset > 0) {
        try self.emitI32Const(@intCast(offset));
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    }
    try self.emitLocalSet(result);
    return result;
}

fn emitStrEqCall(self: *Self, lhs_str_ptr: u32, rhs_str_ptr: u32) Allocator.Error!void {
    if (self.externalCallsUseRelocs()) {
        const lhs = try self.loadRocStrFields(lhs_str_ptr);
        const rhs = try self.loadRocStrFields(rhs_str_ptr);
        try self.emitRocStrFields(lhs);
        try self.emitRocStrFields(rhs);
        try self.emitBuiltinCall(.str_equal, self.str_eq_import);
    } else {
        try self.emitLocalGet(lhs_str_ptr);
        try self.emitLocalGet(rhs_str_ptr);
        try self.emitBuiltinCall(.str_equal, self.str_eq_import);
    }
}

fn emitListEqCall(
    self: *Self,
    lhs_list_ptr: u32,
    rhs_list_ptr: u32,
    kind: BuiltinKind,
    host_import: ?u32,
    elem_size: ?u32,
) Allocator.Error!void {
    if (self.externalCallsUseRelocs()) {
        const lhs = try self.loadRocListFields(lhs_list_ptr);
        const rhs = try self.loadRocListFields(rhs_list_ptr);
        try self.emitRocListFields(lhs);
        try self.emitRocListFields(rhs);
    } else {
        try self.emitLocalGet(lhs_list_ptr);
        try self.emitLocalGet(rhs_list_ptr);
    }

    if (elem_size) |size| {
        try self.emitI32Const(@intCast(size));
    }
    try self.emitBuiltinCall(kind, host_import);
}

fn hasherDomain(op: lir.LowLevel) u8 {
    return @intFromEnum(switch (op) {
        .hasher_write_bool => builtins.hash.HasherDomain.bool,
        .hasher_write_u8 => builtins.hash.HasherDomain.u8,
        .hasher_write_u16 => builtins.hash.HasherDomain.u16,
        .hasher_write_u32 => builtins.hash.HasherDomain.u32,
        .hasher_write_u64 => builtins.hash.HasherDomain.u64,
        .hasher_write_u128 => builtins.hash.HasherDomain.u128,
        .hasher_write_i8 => builtins.hash.HasherDomain.i8,
        .hasher_write_i16 => builtins.hash.HasherDomain.i16,
        .hasher_write_i32 => builtins.hash.HasherDomain.i32,
        .hasher_write_i64 => builtins.hash.HasherDomain.i64,
        .hasher_write_i128 => builtins.hash.HasherDomain.i128,
        .hasher_write_dec => builtins.hash.HasherDomain.dec,
        .hasher_write_bytes => builtins.hash.HasherDomain.bytes,
        else => unreachable,
    });
}

fn hasherWidth(op: lir.LowLevel) u8 {
    return switch (op) {
        .hasher_write_bool,
        .hasher_write_u8,
        .hasher_write_i8,
        => 1,
        .hasher_write_u16,
        .hasher_write_i16,
        => 2,
        .hasher_write_u32,
        .hasher_write_i32,
        => 4,
        .hasher_write_u64,
        .hasher_write_i64,
        => 8,
        else => unreachable,
    };
}

fn emitHasherState(self: *Self, hasher: ProcLocalId) Allocator.Error!void {
    try self.emitProcLocal(hasher);
    try self.emitLoadOp(.i64, 0);
}

fn emitHasherRecordFromI64(self: *Self) Allocator.Error!void {
    const state_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(state_local);

    const result_offset = try self.allocStackMemory(8, 8);
    const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_ptr);

    try self.emitLocalGet(result_ptr);
    try self.emitLocalGet(state_local);
    try self.emitStoreOp(.i64, 0);
    try self.emitLocalGet(result_ptr);
}

fn emitHasherScalarAsI64(self: *Self, value: ProcLocalId) Allocator.Error!void {
    try self.emitProcLocal(value);
    const vt = try self.procLocalValType(value);
    if (vt == .i32) {
        self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    }
}

fn emitHasherFloatBits(self: *Self, value: ProcLocalId, is_f32: bool) Allocator.Error!void {
    try self.emitProcLocal(value);
    if (is_f32) {
        const raw_f32 = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
        try self.emitLocalSet(raw_f32);
        try self.emitLocalGet(raw_f32);
        self.currentCode().append(self.allocator, Op.i32_reinterpret_f32) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    } else {
        const raw_f64 = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
        try self.emitLocalSet(raw_f64);
        try self.emitLocalGet(raw_f64);
        self.currentCode().append(self.allocator, Op.i64_reinterpret_f64) catch return error.OutOfMemory;
    }
}

fn emitHasherU128Parts(self: *Self, value: ProcLocalId) Allocator.Error!void {
    try self.emitProcLocal(value);
    const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(ptr);

    try self.emitLocalGet(ptr);
    try self.emitLoadOp(.i64, 0);

    try self.emitLocalGet(ptr);
    try self.emitLoadOp(.i64, 8);
}

fn emitHasherLowLevel(self: *Self, op: lir.LowLevel, args: []const ProcLocalId) Allocator.Error!void {
    switch (op) {
        .dict_pseudo_seed => {
            try self.emitBuiltinCall(.dict_pseudo_seed, self.dict_pseudo_seed_import);
        },
        .hasher_finish => {
            try self.emitHasherState(args[0]);
            try self.emitBuiltinCall(.hasher_finish, self.hasher_finish_import);
        },
        .hasher_write_bool,
        .hasher_write_u8,
        .hasher_write_u16,
        .hasher_write_u32,
        .hasher_write_u64,
        .hasher_write_i8,
        .hasher_write_i16,
        .hasher_write_i32,
        .hasher_write_i64,
        => {
            try self.emitHasherState(args[0]);
            try self.emitI32Const(@intCast(hasherDomain(op)));
            try self.emitHasherScalarAsI64(args[1]);
            try self.emitI32Const(@intCast(hasherWidth(op)));
            try self.emitBuiltinCall(.hasher_write_u64, self.hasher_write_u64_import);
            try self.emitHasherRecordFromI64();
        },
        .hasher_write_f32 => {
            try self.emitHasherState(args[0]);
            try self.emitHasherFloatBits(args[1], true);
            try self.emitBuiltinCall(.hasher_write_f32_bits, self.hasher_write_f32_bits_import);
            try self.emitHasherRecordFromI64();
        },
        .hasher_write_f64 => {
            try self.emitHasherState(args[0]);
            try self.emitHasherFloatBits(args[1], false);
            try self.emitBuiltinCall(.hasher_write_f64_bits, self.hasher_write_f64_bits_import);
            try self.emitHasherRecordFromI64();
        },
        .hasher_write_u128,
        .hasher_write_i128,
        .hasher_write_dec,
        => {
            try self.emitHasherState(args[0]);
            try self.emitI32Const(@intCast(hasherDomain(op)));
            try self.emitHasherU128Parts(args[1]);
            try self.emitBuiltinCall(.hasher_write_u128, self.hasher_write_u128_import);
            try self.emitHasherRecordFromI64();
        },
        .hasher_write_bytes => {
            try self.emitProcLocal(args[1]);
            const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_ptr);
            const fields = try self.loadRocListFields(list_ptr);

            try self.emitHasherState(args[0]);
            try self.emitI32Const(@intCast(hasherDomain(op)));
            try self.emitLocalGet(fields.bytes);
            try self.emitLocalGet(fields.len);
            try self.emitBuiltinCall(.hasher_write_bytes, self.hasher_write_bytes_import);
            try self.emitHasherRecordFromI64();
        },
        .hasher_write_str => {
            try self.emitProcLocal(args[1]);
            const str_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(str_ptr);
            // Decode the string SSO-aware: a small string stores its bytes inline
            // and its length in the high bits of the last byte, so a flat field
            // load would read garbage. emitExtractStrPtrLen yields the correct
            // (ptr, len) for both small and heap strings, matching every other
            // backend's str hashing/equality and keeping cross-backend hashes equal.
            const str_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const str_len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitExtractStrPtrLen(str_ptr, str_ptr_local, str_len_local);

            try self.emitHasherState(args[0]);
            try self.emitLocalGet(str_ptr_local);
            try self.emitLocalGet(str_len_local);
            // The host import ignores capacity; pass the decoded length as a filler
            // so the call shape stays (hasher, ptr, len, cap).
            try self.emitLocalGet(str_len_local);
            try self.emitBuiltinCall(.hasher_write_str, self.hasher_write_str_import);
            try self.emitHasherRecordFromI64();
        },
        else => unreachable,
    }
}

fn emitCryptoLowLevel(self: *Self, op: lir.LowLevel, args: []const ProcLocalId) Allocator.Error!void {
    const Arity = enum { zero, one, two };
    const CryptoCall = struct {
        arity: Arity,
        kind: BuiltinKind,
        host_import: ?u32,
    };

    const call: CryptoCall = switch (op) {
        .crypto_sha256_hash_bytes => .{ .arity = .one, .kind = .crypto_sha256_hash_bytes, .host_import = self.crypto_sha256_hash_bytes_import },
        .crypto_sha256_hasher_empty => .{ .arity = .zero, .kind = .crypto_sha256_hasher_empty, .host_import = self.crypto_sha256_hasher_empty_import },
        .crypto_sha256_hasher_write => .{ .arity = .two, .kind = .crypto_sha256_hasher_write, .host_import = self.crypto_sha256_hasher_write_import },
        .crypto_sha256_hasher_finish => .{ .arity = .one, .kind = .crypto_sha256_hasher_finish, .host_import = self.crypto_sha256_hasher_finish_import },
        .crypto_blake3_hash_bytes => .{ .arity = .one, .kind = .crypto_blake3_hash_bytes, .host_import = self.crypto_blake3_hash_bytes_import },
        .crypto_blake3_hasher_empty => .{ .arity = .zero, .kind = .crypto_blake3_hasher_empty, .host_import = self.crypto_blake3_hasher_empty_import },
        .crypto_blake3_hasher_write => .{ .arity = .two, .kind = .crypto_blake3_hasher_write, .host_import = self.crypto_blake3_hasher_write_import },
        .crypto_blake3_hasher_finish => .{ .arity = .one, .kind = .crypto_blake3_hasher_finish, .host_import = self.crypto_blake3_hasher_finish_import },
        else => unreachable,
    };

    const result_offset = try self.allocStackMemory(12, 4);
    switch (call.arity) {
        .zero => {
            try self.emitFpOffset(result_offset);
            try self.emitLocalGet(self.roc_ops_local);
        },
        .one => {
            try self.emitProcLocal(args[0]);
            const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_ptr);
            const fields = try self.loadRocListFields(list_ptr);

            try self.emitFpOffset(result_offset);
            try self.emitRocListFields(fields);
            try self.emitLocalGet(self.roc_ops_local);
        },
        .two => {
            try self.emitProcLocal(args[0]);
            const first_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(first_ptr);
            try self.emitProcLocal(args[1]);
            const second_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(second_ptr);
            const first = try self.loadRocListFields(first_ptr);
            const second = try self.loadRocListFields(second_ptr);

            try self.emitFpOffset(result_offset);
            try self.emitRocListFields(first);
            try self.emitRocListFields(second);
            try self.emitLocalGet(self.roc_ops_local);
        },
    }

    try self.emitBuiltinCall(call.kind, call.host_import);
    try self.emitFpOffset(result_offset);
}

fn compileBuiltinInternalIncrefCallback(self: *Self, helper_key: RcHelperKey) Allocator.Error!u32 {
    if (helper_key.op != .incref) {
        wasmInvariantFmt(
            "WASM/codegen invariant violated: incref callback requested for {s} helper",
            .{@tagName(helper_key.op)},
        );
    }

    // These callbacks serve runtime-checked list ops, whose RC is internal to
    // the op and makes no thread-confinement claim, so they always use the
    // atomic helper family.
    const helper_func_idx = try self.compileBuiltinInternalRcHelper(helper_key, .atomic);
    const type_idx = try self.internFuncType(&.{ .i32, .i32, .i32 }, &.{});
    const defined = self.module.addDefinedFunction(type_idx) catch return error.OutOfMemory;
    const func_idx = defined.function.raw();
    _ = try self.addOwnedLocalFunctionSymbol(defined, "roc_rc_incref_callback", rcHelperCacheKey(helper_key, .atomic));

    const saved = try self.saveState();

    try self.beginFunction(defined.local);
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
    const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.roc_ops_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    try self.emitLocalGet(value_ptr_local);
    try self.emitLocalGet(count_local);
    try self.emitLocalGet(self.roc_ops_local);
    try self.emitCall(helper_func_idx);

    try self.encodeLocalsDecl(&self.currentBody().preamble, 3);
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.endFunction();
    self.restoreState(saved);
    return func_idx;
}

fn builtinInternalRcHelperTableIndex(self: *Self, helper_key: RcHelperKey) Allocator.Error!u32 {
    const helper_plan = self.getLayoutStore().rcHelperPlan(helper_key);
    if (helper_plan == .noop) return 0;

    const cache_key = rcHelperCacheKey(helper_key, .atomic);
    if (self.rc_helper_table_indices.get(cache_key)) |table_idx| return table_idx;

    // Table entries serve runtime-checked list ops, whose RC is internal to
    // the op and makes no thread-confinement claim, so they always use the
    // atomic helper family.
    const func_idx = switch (helper_key.op) {
        .incref => try self.compileBuiltinInternalIncrefCallback(helper_key),
        .decref, .free => try self.compileBuiltinInternalRcHelper(helper_key, .atomic),
    };
    const table_idx = self.module.addTableElement(func_idx) catch return error.OutOfMemory;
    try self.rc_helper_table_indices.put(cache_key, table_idx);
    return table_idx;
}

fn listElementCallbacks(self: *Self, list_abi: BuiltinListAbi) Allocator.Error!RocListElementCallbacks {
    if (!list_abi.elements_refcounted) {
        return .{
            .elements_refcounted = 0,
            .incref_table_idx = 0,
            .decref_table_idx = 0,
        };
    }

    const elem_layout_idx = list_abi.elem_layout_idx orelse {
        wasmInvariantFmt("WASM/codegen invariant violated: refcounted list element missing layout", .{});
    };
    return .{
        .elements_refcounted = 1,
        .incref_table_idx = try self.builtinInternalRcHelperTableIndex(.{
            .op = .incref,
            .layout_idx = elem_layout_idx,
        }),
        .decref_table_idx = try self.builtinInternalRcHelperTableIndex(.{
            .op = .decref,
            .layout_idx = elem_layout_idx,
        }),
    };
}

fn functionIndexForSymbol(self: *const Self, symbol: SymbolIndex) u32 {
    const raw_symbol = symbol.raw();
    if (raw_symbol >= self.module.linking.symbol_table.items.len) {
        wasmInvariantFmt("WASM/codegen invariant violated: symbol index {d} outside linking symbol table", .{raw_symbol});
    }
    const sym = self.module.linking.symbol_table.items[raw_symbol];
    if (sym.kind != .function) {
        wasmInvariantFmt("WASM/codegen invariant violated: symbol index {d} is not a function symbol", .{raw_symbol});
    }
    return sym.index;
}

fn recordFunctionSymbol(self: *Self, func_idx: u32, symbol: SymbolIndex) Allocator.Error!void {
    const gop = self.function_symbols_by_index.getOrPut(func_idx) catch return error.OutOfMemory;
    if (gop.found_existing) {
        wasmInvariantFmt(
            "WASM/codegen invariant violated: duplicate symbol for function index {d}",
            .{func_idx},
        );
    }
    gop.value_ptr.* = symbol;
}

fn addTrackedDefinedFunctionSymbol(
    self: *Self,
    defined: index_types.DefinedFunction,
    name: []const u8,
    flags: u32,
) Allocator.Error!SymbolIndex {
    const symbol = self.module.addDefinedFunctionSymbol(defined.local, name, flags) catch return error.OutOfMemory;
    try self.recordFunctionSymbol(defined.function.raw(), symbol);
    return symbol;
}

fn addOwnedLocalFunctionSymbol(
    self: *Self,
    defined: index_types.DefinedFunction,
    prefix: []const u8,
    id: u64,
) Allocator.Error!SymbolIndex {
    const name = std.fmt.allocPrint(self.allocator, "{s}.{d}", .{ prefix, id }) catch return error.OutOfMemory;
    errdefer self.allocator.free(name);
    try self.function_symbol_names.append(self.allocator, name);
    return try self.addTrackedDefinedFunctionSymbol(
        defined,
        name,
        WasmLinking.SymFlag.BINDING_LOCAL | WasmLinking.SymFlag.VISIBILITY_HIDDEN,
    );
}

fn externalCallsUseRelocs(self: *const Self) bool {
    return switch (self.external_calls) {
        .builtin_relocs => true,
        .host_imports, .unconfigured => false,
    };
}

fn emitDataAddressConst(self: *Self, address: DataAddress, addend: i32) Allocator.Error!void {
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    if (self.relocatable_object or self.track_static_data_addresses) {
        const code_pos: u32 = @intCast(self.currentCode().items.len);
        try self.currentBody().addOffsetRelocation(
            self.allocator,
            .memory_addr_sleb,
            code_pos,
            address.symbol,
            addend,
        );
        const placeholder: i32 = if (self.relocatable_object)
            0
        else
            @intCast(@as(i64, @intCast(address.offset)) + @as(i64, addend));
        WasmModule.appendPaddedI32(self.allocator, self.currentCode(), placeholder) catch return error.OutOfMemory;
    } else {
        WasmModule.leb128WriteI32(
            self.allocator,
            self.currentCode(),
            @intCast(@as(i64, @intCast(address.offset)) + @as(i64, addend)),
        ) catch return error.OutOfMemory;
    }
}

fn emitCallIndirect(self: *Self, type_idx: u32) Allocator.Error!void {
    self.currentCode().append(self.allocator, Op.call_indirect) catch return error.OutOfMemory;
    if (self.relocatable_object) {
        const type_pos: u32 = @intCast(self.currentCode().items.len);
        try self.currentBody().addIndexRelocation(self.allocator, .type_index_leb, type_pos, SymbolIndex.fromRaw(type_idx));
        WasmModule.appendPaddedU32(self.allocator, self.currentCode(), type_idx) catch return error.OutOfMemory;

        const table_pos: u32 = @intCast(self.currentCode().items.len);
        const table_symbol = self.indirect_table_symbol orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("WasmCodeGen invariant violated: relocatable call_indirect without table symbol", .{});
            }
            unreachable;
        };
        try self.currentBody().addIndexRelocation(self.allocator, .table_number_leb, table_pos, table_symbol);
        WasmModule.appendPaddedU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    } else {
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), type_idx) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    }
}

fn allocStaticDataName(self: *Self, prefix: []const u8) Allocator.Error![]const u8 {
    const name = std.fmt.allocPrint(
        self.allocator,
        "{s}.{d}",
        .{ prefix, self.static_data_name_counter },
    ) catch return error.OutOfMemory;
    errdefer self.allocator.free(name);
    self.static_data_name_counter += 1;
    try self.static_data_names.append(self.allocator, name);
    return name;
}

fn addStaticDataSymbol(
    self: *Self,
    data: []const u8,
    align_bytes: u32,
    segment_name: []const u8,
    symbol_name: []const u8,
    symbol_offset: u32,
    symbol_size: u32,
) Allocator.Error!DataAddress {
    const segment_index: u32 = @intCast(self.module.data_segments.items.len);
    const segment_offset = self.module.addDataSegmentWithInfo(data, align_bytes, segment_name, 0) catch return error.OutOfMemory;
    const symbol = self.module.addDataSymbol(
        segment_index,
        symbol_name,
        symbol_offset,
        symbol_size,
        WasmLinking.SymFlag.BINDING_LOCAL | WasmLinking.SymFlag.VISIBILITY_HIDDEN,
    ) catch return error.OutOfMemory;
    return .{
        .offset = segment_offset + symbol_offset,
        .symbol = symbol,
    };
}

fn appendStackPointerGlobalTo(
    self: *Self,
    allocator: Allocator,
    code: *std.ArrayList(u8),
    relocs: ?*StackPrefixRelocs,
    op: u8,
) Allocator.Error!void {
    code.append(allocator, op) catch return error.OutOfMemory;
    if (self.stack_pointer_symbol) |symbol| {
        const code_pos: u32 = @intCast(code.items.len);
        if (relocs) |reloc_list| {
            reloc_list.append(.{ .index = .{
                .type_id = .global_index_leb,
                .code_pos = code_pos,
                .symbol_index = symbol,
            } });
        } else {
            try self.currentBody().addIndexRelocation(self.allocator, .global_index_leb, code_pos, symbol);
        }
        WasmModule.appendPaddedU32(allocator, code, 0) catch return error.OutOfMemory;
    } else {
        WasmModule.leb128WriteU32(allocator, code, 0) catch return error.OutOfMemory;
    }
}

fn emitStackPointerGlobal(self: *Self, op: u8) Allocator.Error!void {
    try self.appendStackPointerGlobalTo(self.allocator, self.currentCode(), null, op);
}

fn prependStackPrefix(
    self: *Self,
    prefix: []const u8,
    prefix_relocs: []const CodeBuilder.Relocation,
) Allocator.Error!void {
    try self.currentBody().prependToCode(self.allocator, prefix);
    for (prefix_relocs) |reloc| {
        switch (reloc) {
            .index => |idx| try self.currentBody().addIndexRelocation(self.allocator, idx.type_id, idx.code_pos, idx.symbol_index),
            .offset => |off| try self.currentBody().addOffsetRelocation(self.allocator, off.type_id, off.code_pos, off.symbol_index, off.addend),
        }
    }
}

/// Insert all generated function bodies into the module in function-section order.
pub fn flushPendingBodies(self: *Self) Allocator.Error!void {
    var keys = try std.ArrayList(LocalFunctionIndex).initCapacity(self.allocator, self.pending_bodies.count());
    defer keys.deinit(self.allocator);

    var it = self.pending_bodies.keyIterator();
    while (it.next()) |key| {
        try keys.append(self.allocator, key.*);
    }

    std.sort.heap(LocalFunctionIndex, keys.items, {}, struct {
        fn lessThan(_: void, a: LocalFunctionIndex, b: LocalFunctionIndex) bool {
            return a.raw() < b.raw();
        }
    }.lessThan);

    for (keys.items) |local_idx| {
        const expected: u32 = @intCast(self.module.function_offsets.items.len);
        if (local_idx.raw() != expected) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "WasmCodeGen invariant violated: pending body local index {d}, expected {d}",
                    .{ local_idx.raw(), expected },
                );
            }
            unreachable;
        }
        var body = self.pending_bodies.fetchRemove(local_idx).?.value;
        defer body.deinit(self.allocator);
        _ = try body.insertIntoModule(self.allocator, &self.module);
    }
}

fn localFunctionIndexFromGlobal(self: *const Self, global_func_idx: u32) LocalFunctionIndex {
    return index_types.functionToLocal(FunctionIndex.fromRaw(global_func_idx), self.module.importCount());
}

/// Register shared wasm types used by RocOps and hosted-function indirect calls.
pub fn registerIndirectCallTypes(self: *Self) Allocator.Error!void {
    if (self.indirect_call_types_registered) return;

    self.roc_ops_type_idx = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{},
    );
    self.roc_alloc_type_idx = try self.module.addFuncType(
        &.{ .i32, .i32, .i32 },
        &.{.i32},
    );
    self.roc_dealloc_type_idx = try self.module.addFuncType(
        &.{ .i32, .i32, .i32 },
        &.{},
    );
    self.roc_realloc_type_idx = try self.module.addFuncType(
        &.{ .i32, .i32, .i32, .i32 },
        &.{.i32},
    );
    self.roc_diag_type_idx = try self.module.addFuncType(
        &.{ .i32, .i32, .i32 },
        &.{},
    );
    self.module.enableTable();
    self.indirect_call_types_registered = true;
}

/// Register host function imports. Must be called before any addFunction calls
/// because wasm imports must come before locally-defined functions.
fn registerHostImports(self: *Self) Allocator.Error!void {
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

    // Hasher host functions. The Hasher is a u64 seed threaded through each
    // write; these mirror the `roc_builtins_hasher_*` C ABI in builtins/hash.zig.
    const dict_pseudo_seed_type = try self.module.addFuncType(&.{}, &.{.i64});
    self.dict_pseudo_seed_import = try self.module.addImport("env", "roc_dict_pseudo_seed", dict_pseudo_seed_type);

    const hasher_finish_type = try self.module.addFuncType(&.{.i64}, &.{.i64});
    self.hasher_finish_import = try self.module.addImport("env", "roc_hasher_finish", hasher_finish_type);

    // (seed i64, domain i32, value i64, width i32) -> i64
    const hasher_write_u64_type = try self.module.addFuncType(&.{ .i64, .i32, .i64, .i32 }, &.{.i64});
    self.hasher_write_u64_import = try self.module.addImport("env", "roc_hasher_write_u64", hasher_write_u64_type);

    // (seed i64, domain i32, low i64, high i64) -> i64
    const hasher_write_u128_type = try self.module.addFuncType(&.{ .i64, .i32, .i64, .i64 }, &.{.i64});
    self.hasher_write_u128_import = try self.module.addImport("env", "roc_hasher_write_u128", hasher_write_u128_type);

    // (seed i64, bits i64) -> i64
    const hasher_write_bits_type = try self.module.addFuncType(&.{ .i64, .i64 }, &.{.i64});
    self.hasher_write_f32_bits_import = try self.module.addImport("env", "roc_hasher_write_f32_bits", hasher_write_bits_type);
    self.hasher_write_f64_bits_import = try self.module.addImport("env", "roc_hasher_write_f64_bits", hasher_write_bits_type);

    // (seed i64, domain i32, ptr i32, len i32) -> i64
    const hasher_write_bytes_type = try self.module.addFuncType(&.{ .i64, .i32, .i32, .i32 }, &.{.i64});
    self.hasher_write_bytes_import = try self.module.addImport("env", "roc_hasher_write_bytes", hasher_write_bytes_type);

    // (seed i64, ptr i32, len i32, cap i32) -> i64
    const hasher_write_str_type = try self.module.addFuncType(&.{ .i64, .i32, .i32, .i32 }, &.{.i64});
    self.hasher_write_str_import = try self.module.addImport("env", "roc_hasher_write_str", hasher_write_str_type);

    // Crypto host functions mirror the builtin wrapper ABI so eval host
    // imports and relocatable builtin calls use the same lowering.
    const crypto_hash_bytes_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.crypto_sha256_hash_bytes_import = try self.module.addImport("env", "roc_crypto_sha256_hash_bytes", crypto_hash_bytes_type);
    self.crypto_blake3_hash_bytes_import = try self.module.addImport("env", "roc_crypto_blake3_hash_bytes", crypto_hash_bytes_type);

    const crypto_hasher_empty_type = try self.module.addFuncType(&.{ .i32, .i32 }, &.{});
    self.crypto_sha256_hasher_empty_import = try self.module.addImport("env", "roc_crypto_sha256_hasher_empty", crypto_hasher_empty_type);
    self.crypto_blake3_hasher_empty_import = try self.module.addImport("env", "roc_crypto_blake3_hasher_empty", crypto_hasher_empty_type);

    const crypto_hasher_write_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.crypto_sha256_hasher_write_import = try self.module.addImport("env", "roc_crypto_sha256_hasher_write", crypto_hasher_write_type);
    self.crypto_blake3_hasher_write_import = try self.module.addImport("env", "roc_crypto_blake3_hasher_write", crypto_hasher_write_type);

    const crypto_hasher_finish_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.crypto_sha256_hasher_finish_import = try self.module.addImport("env", "roc_crypto_sha256_hasher_finish", crypto_hasher_finish_type);
    self.crypto_blake3_hasher_finish_import = try self.module.addImport("env", "roc_crypto_blake3_hasher_finish", crypto_hasher_finish_type);

    // RocOps function imports follow the platform C ABI: each takes a leading `*RocOps`
    // (the i32 pointer to the RocOps struct in linear memory) followed by its natural
    // arguments. They are invoked via the wasm funcref table and `call_indirect`.
    try self.registerIndirectCallTypes();

    // Enable table and add each RocOps function as a table element
    const roc_alloc_idx = try self.module.addImport("env", "roc_alloc", self.roc_alloc_type_idx);
    self.roc_alloc_table_idx = try self.module.addTableElement(roc_alloc_idx);

    const roc_dealloc_idx = try self.module.addImport("env", "roc_dealloc", self.roc_dealloc_type_idx);
    self.roc_dealloc_table_idx = try self.module.addTableElement(roc_dealloc_idx);

    const roc_realloc_idx = try self.module.addImport("env", "roc_realloc", self.roc_realloc_type_idx);
    self.roc_realloc_table_idx = try self.module.addTableElement(roc_realloc_idx);

    const roc_dbg_idx = try self.module.addImport("env", "roc_dbg", self.roc_diag_type_idx);
    self.roc_dbg_table_idx = try self.module.addTableElement(roc_dbg_idx);

    const roc_expect_failed_idx = try self.module.addImport("env", "roc_expect_failed", self.roc_diag_type_idx);
    self.roc_expect_failed_table_idx = try self.module.addTableElement(roc_expect_failed_idx);

    const roc_crashed_idx = try self.module.addImport("env", "roc_crashed", self.roc_diag_type_idx);
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
    self.dec_pow_import = try self.module.addImport("env", "roc_dec_pow", i128_binop_type);

    const dec_unary_type = try self.module.addFuncType(
        &.{ .i32, .i32 },
        &.{},
    );
    self.dec_sqrt_import = try self.module.addImport("env", "roc_dec_sqrt", dec_unary_type);
    self.dec_sin_import = try self.module.addImport("env", "roc_dec_sin", dec_unary_type);
    self.dec_cos_import = try self.module.addImport("env", "roc_dec_cos", dec_unary_type);
    self.dec_tan_import = try self.module.addImport("env", "roc_dec_tan", dec_unary_type);
    self.dec_asin_import = try self.module.addImport("env", "roc_dec_asin", dec_unary_type);
    self.dec_acos_import = try self.module.addImport("env", "roc_dec_acos", dec_unary_type);
    self.dec_atan_import = try self.module.addImport("env", "roc_dec_atan", dec_unary_type);

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

    const float_pow_type = try self.module.addFuncType(
        &.{ .f64, .f64, .i32 },
        &.{.f64},
    );
    self.float_pow_import = try self.module.addImport("env", "roc_float_pow", float_pow_type);

    const float_unary_type = try self.module.addFuncType(
        &.{ .f64, .i32 },
        &.{.f64},
    );
    self.float_sin_import = try self.module.addImport("env", "roc_float_sin", float_unary_type);
    self.float_cos_import = try self.module.addImport("env", "roc_float_cos", float_unary_type);
    self.float_tan_import = try self.module.addImport("env", "roc_float_tan", float_unary_type);
    self.float_asin_import = try self.module.addImport("env", "roc_float_asin", float_unary_type);
    self.float_acos_import = try self.module.addImport("env", "roc_float_acos", float_unary_type);
    self.float_atan_import = try self.module.addImport("env", "roc_float_atan", float_unary_type);

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
        &.{ .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32, .i32 },
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

    const list_reserve_type = try self.module.addFuncType(&.{ .i32, .i64, .i32, .i32, .i32 }, &.{});
    self.list_reserve_import = try self.module.addImport("env", "roc_list_reserve", list_reserve_type);

    const list_reverse_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32 }, &.{});
    self.list_reverse_import = try self.module.addImport("env", "roc_list_reverse", list_reverse_type);

    // roc_list_replace(list_ptr, elem_width, alignment, index, element_ptr, out_element_ptr, result_ptr)
    const list_replace_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i64, .i32, .i32, .i32 }, &.{});
    self.list_replace_import = try self.module.addImport("env", "roc_list_replace", list_replace_type);

    // roc_list_swap(list_ptr, elem_width, alignment, index_1, index_2, result_ptr)
    const list_swap_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i64, .i64, .i32 }, &.{});
    self.list_swap_import = try self.module.addImport("env", "roc_list_swap", list_swap_type);

    // String ops: (arg1, arg2, result_ptr) -> void
    const str_binary_type = try self.module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
    self.str_drop_prefix_import = try self.module.addImport("env", "roc_str_drop_prefix", str_binary_type);
    self.str_drop_suffix_import = try self.module.addImport("env", "roc_str_drop_suffix", str_binary_type);
    self.str_split_import = try self.module.addImport("env", "roc_str_split", str_binary_type);
    self.str_join_with_import = try self.module.addImport("env", "roc_str_join_with", str_binary_type);
    self.str_concat_import = try self.module.addImport("env", "roc_str_concat", str_binary_type);
    self.str_repeat_import = try self.module.addImport("env", "roc_str_repeat", str_binary_type);
    self.str_reserve_import = try self.module.addImport("env", "roc_str_reserve", str_binary_type);

    // roc_str_find_first: (source, delimiter, result, after_off, before_off, found_off) -> void
    const str_find_first_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.str_find_first_import = try self.module.addImport("env", "roc_str_find_first", str_find_first_type);

    // roc_str_drop_prefix_caseless_ascii: (source, prefix, result, after_off, found_off) -> void
    const str_drop_prefix_caseless_ascii_type = try self.module.addFuncType(&.{ .i32, .i32, .i32, .i32, .i32 }, &.{});
    self.str_drop_prefix_caseless_ascii_import = try self.module.addImport("env", "roc_str_drop_prefix_caseless_ascii", str_drop_prefix_caseless_ascii_type);

    // Caseless equals: (str_a, str_b) -> i32
    self.str_caseless_ascii_equals_import = try self.module.addImport("env", "roc_str_caseless_ascii_equals", str_eq_type);
    self.external_calls = .host_imports;
}

/// Result of generating a wasm module
pub const GenerateResult = struct {
    wasm_bytes: []u8,
    result_layout: layout.Idx,
    has_imports: bool = false,
};

/// Generate the host-callable wrapper for a platform-exposed Roc entrypoint.
pub fn generateEntrypointWrapper(
    self: *Self,
    symbol_name: []const u8,
    entry_proc: LIR.LirProcSpecId,
    arg_layouts: []const layout.Idx,
    ret_layout: layout.Idx,
) Allocator.Error!u32 {
    const root_key: u32 = @intFromEnum(entry_proc);
    const root_func_idx = self.registered_procs.get(root_key) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("WASM/codegen invariant violated: missing compiled entry proc {d}", .{@intFromEnum(entry_proc)});
        }
        unreachable;
    };

    // Classify the natural C-ABI wasm signature for this entrypoint.
    var arena_state = std.heap.ArenaAllocator.init(self.allocator);
    defer arena_state.deinit();
    const lowered = layout.abi.lower(arena_state.allocator(), self.getLayoutStore(), .wasm32, arg_layouts, ret_layout, false) catch return error.OutOfMemory;

    var param_types = std.ArrayList(ValType).empty;
    defer param_types.deinit(self.allocator);
    var result_types = std.ArrayList(ValType).empty;
    defer result_types.deinit(self.allocator);

    if (lowered.ret == .indirect) try param_types.append(self.allocator, .i32);
    for (lowered.args) |placement| {
        switch (placement) {
            .none => {},
            .indirect => try param_types.append(self.allocator, .i32),
            .registers => |pieces| {
                for (pieces) |piece| try param_types.append(self.allocator, pieceValType(piece));
            },
        }
    }
    switch (lowered.ret) {
        .none, .indirect => {},
        .registers => |pieces| {
            // Wasm's C ABI returns at most one direct value; larger results
            // are indirect.
            std.debug.assert(pieces.len == 1);
            try result_types.append(self.allocator, pieceValType(pieces[0]));
        },
    }

    const type_idx = try self.internFuncType(param_types.items, result_types.items);
    const defined = self.module.addDefinedFunction(type_idx) catch return error.OutOfMemory;
    const func_idx = defined.function.raw();
    _ = try self.addTrackedDefinedFunctionSymbol(defined, symbol_name, 0);

    const saved = self.saveState() catch return error.OutOfMemory;
    try self.beginFunction(defined.local);
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

    // Allocate the wasm parameter locals in declaration order.
    var sret_local: u32 = 0;
    if (lowered.ret == .indirect) {
        sret_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    }
    var piece_locals = std.ArrayList(u32).empty;
    defer piece_locals.deinit(self.allocator);
    var arg_first_piece = try self.allocator.alloc(u32, arg_layouts.len);
    defer self.allocator.free(arg_first_piece);
    for (lowered.args, 0..) |placement, i| {
        arg_first_piece[i] = @intCast(piece_locals.items.len);
        switch (placement) {
            .none => {},
            .indirect => {
                const local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try piece_locals.append(self.allocator, local);
            },
            .registers => |pieces| {
                for (pieces) |piece| {
                    const local = self.storage.allocAnonymousLocal(pieceValType(piece)) catch return error.OutOfMemory;
                    try piece_locals.append(self.allocator, local);
                }
            },
        }
    }
    const param_count: u32 = @intCast((if (lowered.ret == .indirect) @as(usize, 1) else 0) + piece_locals.items.len);

    // Materialize the internal-convention argument for each entrypoint arg:
    // scalars travel as the incoming wasm value; aggregates are copied into
    // fresh stack memory and passed by pointer.
    const ArgValue = union(enum) { zst, direct: u32, ptr: u32 };
    const arg_values = try self.allocator.alloc(ArgValue, arg_layouts.len);
    defer self.allocator.free(arg_values);

    for (lowered.args, arg_layouts, 0..) |placement, arg_layout, i| {
        const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
        const size = try self.layoutStorageByteSize(runtime_layout);
        if (size == 0) {
            arg_values[i] = .zst;
            continue;
        }
        const composite = try self.isCompositeLayout(arg_layout);
        switch (placement) {
            .none => {
                arg_values[i] = .zst;
            },
            .indirect => {
                // Copy the pointed-at bytes into fresh stack memory; the
                // internal convention also takes aggregates by pointer.
                const arg_align = try self.layoutStorageByteAlign(runtime_layout);
                const slot = try self.allocStackMemory(size, arg_align);
                const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitFpOffset(slot);
                try self.emitLocalSet(dst_local);
                try self.emitMemCopy(dst_local, 0, piece_locals.items[arg_first_piece[i]], size);
                arg_values[i] = .{ .ptr = dst_local };
            },
            .registers => |pieces| {
                if (!composite and pieces.len == 1) {
                    arg_values[i] = .{ .direct = piece_locals.items[arg_first_piece[i]] };
                } else {
                    const arg_align = try self.layoutStorageByteAlign(runtime_layout);
                    const slot = try self.allocStackMemory(@max(size, 4), arg_align);
                    const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitFpOffset(slot);
                    try self.emitLocalSet(dst_local);
                    for (pieces, 0..) |piece, k| {
                        try self.emitLocalGet(piece_locals.items[arg_first_piece[i] + @as(u32, @intCast(k))]);
                        try self.emitStoreToMemSized(dst_local, piece.offset, pieceValType(piece), piece.size);
                    }
                    arg_values[i] = .{ .ptr = dst_local };
                }
            },
        }
    }

    for (arg_values) |value| {
        switch (value) {
            .zst => {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            },
            .direct, .ptr => |local| try self.emitLocalGet(local),
        }
    }
    try self.emitCall(root_func_idx);

    // Marshal the proc's result back out per the C ABI.
    const runtime_ret_layout = self.runtimeRepresentationLayoutIdx(ret_layout);
    const ret_size = try self.layoutStorageByteSize(runtime_ret_layout);
    if (ret_size == 0) {
        self.currentCode().append(self.allocator, Op.drop) catch return error.OutOfMemory;
    } else switch (lowered.ret) {
        .none => unreachable,
        .indirect => {
            if (try self.isCompositeLayout(ret_layout)) {
                const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(result_ptr);
                try self.emitMemCopy(sret_local, 0, result_ptr, ret_size);
            } else {
                try self.emitStoreToMemSized(sret_local, 0, try self.resolveValType(ret_layout), ret_size);
            }
        },
        .registers => |pieces| {
            const piece = pieces[0];
            if (try self.isCompositeLayout(ret_layout)) {
                const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(result_ptr);
                try self.emitLocalGet(result_ptr);
                try self.emitLoadOpSized(pieceValType(piece), piece.size, 0);
            }
            // A scalar result is already on the stack with the right type.
        },
    }

    try self.encodeLocalsDecl(&self.currentBody().preamble, param_count);

    if (self.uses_stack_memory) {
        var prefix_buffer: [max_stack_prefix_bytes]u8 = undefined;
        var prefix = StackPrefixBytes.init(prefix_buffer[0..]);
        const prefix_allocator = prefix.allocator();
        var prefix_relocs: StackPrefixRelocs = .{};

        try self.appendStackPointerGlobalTo(prefix_allocator, &prefix.bytes, &prefix_relocs, Op.global_get);
        prefix.bytes.append(prefix_allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(prefix_allocator, &prefix.bytes, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        prefix.bytes.append(prefix_allocator, Op.i32_sub) catch return error.OutOfMemory;
        prefix.bytes.append(prefix_allocator, Op.local_tee) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(prefix_allocator, &prefix.bytes, self.fp_local) catch return error.OutOfMemory;
        try self.appendStackPointerGlobalTo(prefix_allocator, &prefix.bytes, &prefix_relocs, Op.global_set);
        try self.prependStackPrefix(prefix.bytes.items, prefix_relocs.items());

        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), self.fp_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        try self.emitStackPointerGlobal(Op.global_set);
    }

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.endFunction();
    self.restoreState(saved);
    return func_idx;
}

/// Generate a complete wasm module for a zero-argument root proc.
/// The exported `main` function initializes RocOps and tail-calls the root proc.
pub fn generateModule(self: *Self, root_proc_id: LIR.LirProcSpecId, result_layout: layout.Idx) Allocator.Error!GenerateResult {
    // Register host function imports (must be done before addFunction calls)
    self.registerHostImports() catch return error.OutOfMemory;
    self.registerHostedSymbolTargets(self.store.getProcSpecs()) catch return error.OutOfMemory;

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

    const result_vt = try self.resolveValType(result_layout);

    // Add function type: (i32 env_ptr) -> (result_type)
    const type_idx = self.module.addFuncType(&.{.i32}, &.{result_vt}) catch return error.OutOfMemory;

    const defined = self.module.addDefinedFunction(type_idx) catch return error.OutOfMemory;
    const func_idx = defined.function.raw();

    try self.beginFunction(defined.local);
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

    var prefix_buffer: [max_stack_prefix_bytes]u8 = undefined;
    var prefix = StackPrefixBytes.init(prefix_buffer[0..]);
    const prefix_allocator = prefix.allocator();

    // Encode locals declaration (skip 1 for the env_ptr parameter)
    try self.encodeLocalsDecl(&self.currentBody().preamble, 1);

    // Prologue: allocate stack frame
    // global.get $__stack_pointer (global 0)
    var prefix_relocs: StackPrefixRelocs = .{};
    try self.appendStackPointerGlobalTo(prefix_allocator, &prefix.bytes, &prefix_relocs, Op.global_get);
    // i32.const frame_size
    prefix.bytes.append(prefix_allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(prefix_allocator, &prefix.bytes, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
    // i32.sub
    prefix.bytes.append(prefix_allocator, Op.i32_sub) catch return error.OutOfMemory;
    // local.tee $fp
    prefix.bytes.append(prefix_allocator, Op.local_tee) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(prefix_allocator, &prefix.bytes, self.fp_local) catch return error.OutOfMemory;
    // global.set $__stack_pointer
    try self.appendStackPointerGlobalTo(prefix_allocator, &prefix.bytes, &prefix_relocs, Op.global_set);

    // Build RocOps struct at memory offset 0 (36 bytes on wasm32)
    // Set roc_ops_local = 0 (constant address of the struct)
    prefix.bytes.append(prefix_allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(prefix_allocator, &prefix.bytes, 0) catch return error.OutOfMemory;
    prefix.bytes.append(prefix_allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(prefix_allocator, &prefix.bytes, self.roc_ops_local) catch return error.OutOfMemory;

    // Write env pointer (offset 0)
    try emitI32StoreToBody(prefix_allocator, &prefix.bytes, 0, env_ptr_local);
    // Write roc_alloc table index (offset 4)
    try emitI32StoreConstToBody(prefix_allocator, &prefix.bytes, 4, self.roc_alloc_table_idx);
    // Write roc_dealloc table index (offset 8)
    try emitI32StoreConstToBody(prefix_allocator, &prefix.bytes, 8, self.roc_dealloc_table_idx);
    // Write roc_realloc table index (offset 12)
    try emitI32StoreConstToBody(prefix_allocator, &prefix.bytes, 12, self.roc_realloc_table_idx);
    // Write roc_dbg table index (offset 16)
    try emitI32StoreConstToBody(prefix_allocator, &prefix.bytes, 16, self.roc_dbg_table_idx);
    // Write roc_expect_failed table index (offset 20)
    try emitI32StoreConstToBody(prefix_allocator, &prefix.bytes, 20, self.roc_expect_failed_table_idx);
    // Write roc_crashed table index (offset 24)
    try emitI32StoreConstToBody(prefix_allocator, &prefix.bytes, 24, self.roc_crashed_table_idx);
    // Write hosted_fns.count = 0 (offset 28)
    try emitI32StoreConstToBody(prefix_allocator, &prefix.bytes, wasm_roc_ops_hosted_fns_count_offset, 0);
    // Write hosted_fns.fns = 0 (offset 32)
    try emitI32StoreConstToBody(prefix_allocator, &prefix.bytes, wasm_roc_ops_hosted_fns_ptr_offset, 0);

    try self.prependStackPrefix(prefix.bytes.items, prefix_relocs.items());

    // Epilogue: restore stack pointer
    // local.get $fp
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), self.fp_local) catch return error.OutOfMemory;
    // i32.const frame_size
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
    // i32.add
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    // global.set $__stack_pointer
    try self.emitStackPointerGlobal(Op.global_set);

    // End opcode
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.endFunction();
    try self.flushPendingBodies();
    try self.module.materializeFuncBodies();

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

    var i: usize = 0;
    var group_count: u32 = 0;
    while (i < types.len) {
        const vt = types[i];
        var count: u32 = 1;
        while (i + count < types.len and types[i + count] == vt) {
            count += 1;
        }
        group_count += 1;
        i += count;
    }

    WasmModule.leb128WriteU32(self.allocator, func_body, group_count) catch return error.OutOfMemory;

    i = 0;
    while (i < types.len) {
        const vt = types[i];
        var count: u32 = 1;
        while (i + count < types.len and types[i + count] == vt) {
            count += 1;
        }
        WasmModule.leb128WriteU32(self.allocator, func_body, count) catch return error.OutOfMemory;
        func_body.append(self.allocator, @intFromEnum(vt)) catch return error.OutOfMemory;
        i += count;
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

    const expected_vt = try self.resolveValType(self.procLocalLayoutIdx(value));
    if (local_info.val_type != expected_vt) {
        try self.emitConversion(local_info.val_type, expected_vt);
    }
    try self.emitCanonicalizeScalarForLayout(self.procLocalLayoutIdx(value));
}

fn emitExplicitRcForValueLocal(
    self: *Self,
    helper_key: RcHelperKey,
    atomicity: RcAtomicity,
    value_local: u32,
    value_vt: ValType,
    inc_count: u16,
) Allocator.Error!void {
    if (self.getLayoutStore().rcHelperPlan(helper_key) == .noop) {
        wasmInvariantFmt(
            "WASM/codegen invariant violated: explicit RC statement used noop helper for layout {d}",
            .{@intFromEnum(helper_key.layout_idx)},
        );
    }
    if (value_vt != .i32) {
        wasmInvariantFmt(
            "WASM/codegen invariant violated: explicit RC local {d} was not represented as i32",
            .{value_local},
        );
    }

    const ls = self.getLayoutStore();
    const l = ls.getLayout(helper_key.layout_idx);
    if (try self.isCompositeLayout(helper_key.layout_idx)) {
        try self.emitExplicitRcHelperCallForValuePtr(helper_key, atomicity, value_local, inc_count);
        return;
    }

    const size_align = ls.layoutSizeAlign(l);
    if (size_align.size == 0) {
        wasmInvariantFmt(
            "WASM/codegen invariant violated: explicit RC statement used zero-sized helper layout {d}",
            .{@intFromEnum(helper_key.layout_idx)},
        );
    }

    const slot = try self.allocStackMemory(@intCast(size_align.size), @intCast(size_align.alignment.toByteUnits()));
    const ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(slot);
    try self.emitLocalSet(ptr_local);

    try self.emitLocalGet(ptr_local);
    try self.emitLocalGet(value_local);
    try self.emitStoreOpSized(.i32, @intCast(size_align.size), 0);

    try self.emitExplicitRcHelperCallForValuePtr(helper_key, atomicity, ptr_local, inc_count);
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

                self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
                try self.emitLocalGet(is_small_local);
                self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                try self.emitDataPtrIncrefByLocal(alloc_ptr_local, count);
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
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
            try self.emitBuiltinInternalListRcShallow(.decref, value_ptr_local, helper_key.layout_idx, 1);
            return true;
        },
        .list_free => |list_plan| {
            if (list_plan.child != null) return false;
            try self.emitBuiltinInternalListRcShallow(.free, value_ptr_local, helper_key.layout_idx, 1);
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
            // Boxes whose payload needs deep teardown require a compiled helper.
            if (box_plan.child != null) return false;
            const box_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr_local);
            try self.emitDataPtrDecref(box_ptr_local, box_plan.elem_alignment, false);
            return true;
        },
        .box_free => |box_plan| {
            // Boxes whose payload needs deep teardown require a compiled helper.
            if (box_plan.child != null) return false;
            const box_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr_local);
            try self.emitDataPtrFree(box_ptr_local, box_plan.elem_alignment, false);
            return true;
        },
        .erased_callable_incref => {
            const payload_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(payload_ptr_local);
            if (count_local) |count| {
                try self.emitDataPtrIncrefByLocal(payload_ptr_local, count);
            } else {
                try self.emitDataPtrIncref(payload_ptr_local, 1);
            }
            return true;
        },
        .erased_callable_decref => {
            const payload_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(payload_ptr_local);
            try self.emitErasedCallableOnDropIfUnique(payload_ptr_local);
            try self.emitDataPtrDecref(
                payload_ptr_local,
                builtins.erased_callable.payload_alignment,
                builtins.erased_callable.allocation_has_refcounted_children,
            );
            return true;
        },
        .erased_callable_free => {
            const payload_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(payload_ptr_local);
            try self.emitErasedCallableOnDrop(payload_ptr_local);
            try self.emitDataPtrFree(
                payload_ptr_local,
                builtins.erased_callable.payload_alignment,
                builtins.erased_callable.allocation_has_refcounted_children,
            );
            return true;
        },
        .struct_, .tag_union, .closure => return false,
    }
}

fn emitExplicitRcHelperCallForValuePtr(
    self: *Self,
    helper_key: RcHelperKey,
    atomicity: RcAtomicity,
    value_ptr_local: u32,
    inc_count: u16,
) Allocator.Error!void {
    const helper_plan = self.getLayoutStore().rcHelperPlan(helper_key);
    if (helper_plan == .noop) {
        wasmInvariantFmt(
            "WASM/codegen invariant violated: explicit RC statement used noop helper for layout {d}",
            .{@intFromEnum(helper_key.layout_idx)},
        );
    }
    if (try self.emitRawDirectRcPlan(helper_key, helper_plan, value_ptr_local, null)) return;

    const helper_func_idx = try self.compileBuiltinInternalRcHelper(helper_key, atomicity);
    try self.emitLocalGet(value_ptr_local);
    switch (helper_key.op) {
        .incref => {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(inc_count)) catch return error.OutOfMemory;
            try self.emitLocalGet(self.roc_ops_local);
        },
        .decref, .free => {
            try self.emitLocalGet(self.roc_ops_local);
        },
    }
    try self.emitCall(helper_func_idx);
}

fn emitDecodeListAllocPtr(self: *Self, list_ptr_local: u32, out_alloc_ptr: u32, out_is_slice: u32) Allocator.Error!void {
    const cap_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr_local);
    try self.emitLoadOp(.i32, 8);
    try self.emitLocalSet(cap_local);

    try self.emitLocalGet(cap_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(out_is_slice);

    try self.emitLocalGet(out_is_slice);
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(ValType.i32)) catch return error.OutOfMemory;
    try self.emitLocalGet(cap_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -2) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr_local);
    try self.emitLoadOp(.i32, 0);
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    try self.emitLocalSet(out_alloc_ptr);
}

fn emitDecodeStrAllocPtr(self: *Self, str_ptr_local: u32, out_alloc_ptr: u32, out_is_small: u32) Allocator.Error!void {
    const cap_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(str_ptr_local);
    try self.emitLoadOp(.i32, 4);
    try self.emitLocalSet(cap_local);

    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(str_ptr_local);
    try self.emitLoadOp(.i32, 8);
    try self.emitLocalSet(len_local);

    try self.emitLocalGet(len_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_lt_s) catch return error.OutOfMemory;
    try self.emitLocalSet(out_is_small);

    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(out_alloc_ptr);

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitLocalGet(out_is_small);
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    const is_slice = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(cap_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(is_slice);

    try self.emitLocalGet(is_slice);
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(ValType.i32)) catch return error.OutOfMemory;
    try self.emitLocalGet(cap_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -2) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.emitLocalGet(str_ptr_local);
    try self.emitLoadOp(.i32, 0);
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    try self.emitLocalSet(out_alloc_ptr);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitPtrWithOffset(self: *Self, ptr_local: u32, offset: i32) Allocator.Error!void {
    try self.emitLocalGet(ptr_local);
    if (offset != 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
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

        self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        try self.emitLocalGet(source_alloc_ptr);
        self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

        try self.emitLocalGet(source_is_slice);
        self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

        try self.emitLoadI32AtPtrOffset(source_alloc_ptr, -4, rc_val);
        try self.emitLocalGet(rc_val);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

        try self.emitPtrWithOffset(source_alloc_ptr, -8);
        try self.emitLocalGet(list_ptr_local);
        try self.emitLoadOp(.i32, 4);
        try self.emitStoreOp(.i32, 0);

        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    try self.emitLocalGet(source_alloc_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
    try self.emitLocalSet(out_encoded_cap);
}

fn emitCallRocDealloc(self: *Self, ptr_local: u32, alignment: u32) Allocator.Error!void {
    if (self.runtime_symbol_targets) |targets| {
        // Symbol ABI: roc_dealloc(ptr, alignment), called directly.
        try self.emitLocalGet(ptr_local);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(alignment)) catch return error.OutOfMemory;
        try self.currentBody().emitRelocatableCall(self.allocator, targets.dealloc.symbol, targets.dealloc.func_idx);
        return;
    }

    // roc_dealloc(*RocOps, ptr, alignment) -> ()
    try self.emitLocalGet(self.roc_ops_local);

    try self.emitLocalGet(ptr_local);

    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(alignment)) catch return error.OutOfMemory;

    // Load roc_dealloc table index from the RocOps struct (offset 8)
    try self.emitLocalGet(self.roc_ops_local);
    try self.emitLoadOp(.i32, wasm_roc_ops_dealloc_offset);

    try self.emitCallIndirect(self.roc_dealloc_type_idx);
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
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), alloc_adjust) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    }
    try self.emitLocalSet(alloc_ptr_local);
    try self.emitCallRocDealloc(alloc_ptr_local, alloc_alignment);
}

fn emitDataPtrIncref(self: *Self, data_ptr_local: u32, amount: u16) Allocator.Error!void {
    if (amount == 0) return;

    const masked_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -4) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(masked_ptr);

    try self.emitLocalGet(masked_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -4) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rc_ptr);

    try self.emitLoadI32AtPtrOffset(rc_ptr, 0, rc_val);
    try self.emitLocalGet(rc_val);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(rc_ptr);
    try self.emitLocalGet(rc_val);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(amount)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, 0);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitDataPtrIncrefByLocal(self: *Self, data_ptr_local: u32, amount_local: u32) Allocator.Error!void {
    const masked_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -4) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(masked_ptr);

    try self.emitLocalGet(masked_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -4) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rc_ptr);

    try self.emitLoadI32AtPtrOffset(rc_ptr, 0, rc_val);
    try self.emitLocalGet(rc_val);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(rc_ptr);
    try self.emitLocalGet(rc_val);
    try self.emitLocalGet(amount_local);
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, 0);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitDataPtrDecref(self: *Self, data_ptr_local: u32, alignment: u32, elements_refcounted: bool) Allocator.Error!void {
    const masked_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -4) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(masked_ptr);

    try self.emitLocalGet(masked_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -4) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rc_ptr);

    try self.emitLoadI32AtPtrOffset(rc_ptr, 0, rc_val);
    try self.emitLocalGet(rc_val);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(rc_val);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitBuiltinInternalFreeRcPtr(rc_ptr, alignment, elements_refcounted);
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.emitLocalGet(rc_ptr);
    try self.emitLocalGet(rc_val);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, 0);
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitDataPtrFree(self: *Self, data_ptr_local: u32, alignment: u32, elements_refcounted: bool) Allocator.Error!void {
    const masked_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const rc_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(data_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -4) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalSet(masked_ptr);

    try self.emitLocalGet(masked_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -4) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rc_ptr);
    try self.emitBuiltinInternalFreeRcPtr(rc_ptr, alignment, elements_refcounted);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitBuiltinInternalListElementDecrefsIfUnique(
    self: *Self,
    list_ptr_local: u32,
    alloc_ptr_local: u32,
    is_slice_local: u32,
    elem_width: usize,
    child_key: RcHelperKey,
    atomicity: RcAtomicity,
) Allocator.Error!void {
    const elem_size: u32 = @intCast(elem_width);
    if (elem_size == 0) return;

    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const idx_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const elem_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(alloc_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLoadI32AtPtrOffset(alloc_ptr_local, -4, rc_val);
    try self.emitLocalGet(rc_val);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(is_slice_local);
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitLoadI32AtPtrOffset(alloc_ptr_local, -8, count_local);
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr_local);
    try self.emitLoadOp(.i32, 4);
    try self.emitLocalSet(count_local);
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(idx_local);

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(idx_local);
    try self.emitLocalGet(count_local);
    self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;

    try self.emitLocalGet(alloc_ptr_local);
    try self.emitLocalGet(idx_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(elem_ptr_local);

    try self.emitRawRcHelperCallByKey(child_key, atomicity, elem_ptr_local, null);

    try self.emitLocalGet(idx_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(idx_local);

    self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Emit shallow list RC (allocation refcount only, no element teardown). Used by
/// the inline direct-plan path for lists whose elements need no deep teardown.
/// Equivalent to `emitBuiltinInternalListRc` with a null `list_plan`, but with no
/// reachable element-decref call site so the direct-plan path stays acyclic.
fn emitBuiltinInternalListRcShallow(
    self: *Self,
    comptime kind: RcOpKind,
    list_ptr_local: u32,
    list_layout_idx: layout.Idx,
    inc_count: u16,
) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.emitBuiltinInternalListRcShallow.builtin_list_abi", list_layout_idx);

    const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const is_slice_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitDecodeListAllocPtr(list_ptr_local, alloc_ptr_local, is_slice_local);

    switch (kind) {
        .incref => try self.emitDataPtrIncref(alloc_ptr_local, inc_count),
        .decref => try self.emitDataPtrDecref(alloc_ptr_local, list_abi.elem_align, list_abi.elements_refcounted),
        .free => try self.emitDataPtrFree(alloc_ptr_local, list_abi.elem_align, list_abi.elements_refcounted),
    }
}

fn emitBuiltinInternalListRc(
    self: *Self,
    comptime kind: RcOpKind,
    list_ptr_local: u32,
    list_layout_idx: layout.Idx,
    list_plan: ?layout.RcListPlan,
    atomicity: RcAtomicity,
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
                    try self.emitBuiltinInternalListElementDecrefsIfUnique(list_ptr_local, alloc_ptr_local, is_slice_local, plan.elem_width, child_key, atomicity);
                }
            }
            try self.emitDataPtrDecref(alloc_ptr_local, list_abi.elem_align, list_abi.elements_refcounted);
        },
        .free => {
            if (list_plan) |plan| {
                if (plan.child) |child_key| {
                    try self.emitBuiltinInternalListElementDecrefsIfUnique(list_ptr_local, alloc_ptr_local, is_slice_local, plan.elem_width, child_key, atomicity);
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
        self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        try self.emitLocalGet(alloc_ptr_local);
        self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalGet(is_slice_local);
        self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

        try self.emitLocalGet(alloc_ptr_local);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 8) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;

        try self.emitLocalGet(list_ptr_local);
        try self.emitLoadOp(.i32, 4);
        try self.emitStoreOp(.i32, 0);

        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    try self.emitDataPtrIncrefByLocal(alloc_ptr_local, count_local);
}

fn emitBuiltinInternalStrRc(self: *Self, comptime kind: RcOpKind, str_ptr_local: u32, inc_count: u16) Allocator.Error!void {
    const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const is_small_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitDecodeStrAllocPtr(str_ptr_local, alloc_ptr_local, is_small_local);

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitLocalGet(is_small_local);
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    switch (kind) {
        .incref => try self.emitDataPtrIncref(alloc_ptr_local, inc_count),
        .decref => try self.emitDataPtrDecref(alloc_ptr_local, 1, false),
        .free => try self.emitDataPtrFree(alloc_ptr_local, 1, false),
    }

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Cache identity of a compiled RC helper. The count-update atomicity an RC
/// statement selects is part of the identity, so a `.single_thread` statement
/// reaches a single-thread helper entry and never shares an `.atomic` one.
/// On wasm both count-update families lower to the same plain i32
/// load/store sequence (linear memory here is not shared and the module does
/// not enable the atomics feature), so the modes differ only in which helper
/// entry they reach. Helpers serving runtime-checked list ops and erased
/// on_drop slots belong to no RC statement and are always requested as
/// `.atomic`.
fn rcHelperCacheKey(helper_key: RcHelperKey, atomicity: RcAtomicity) u64 {
    // RcHelperKey.encode() occupies bits 0..33 (layout index + op).
    return helper_key.encode() | (@as(u64, @intFromEnum(atomicity)) << 34);
}

fn emitRawRcHelperCallByKey(
    self: *Self,
    helper_key: RcHelperKey,
    atomicity: RcAtomicity,
    value_ptr_local: u32,
    count_local: ?u32,
) Allocator.Error!void {
    const helper_plan = self.getLayoutStore().rcHelperPlan(helper_key);
    if (helper_plan == .noop) return;
    if (try self.emitRawDirectRcPlan(helper_key, helper_plan, value_ptr_local, count_local)) return;

    // This is only reached while emitting a compiled helper's body, where every
    // nested child helper slot has already been reserved by the compile driver, so
    // resolve the target from the cache instead of (re-)entering compilation.
    const helper_func_idx = self.rcHelperFuncIdx(helper_key, atomicity);
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

/// Look up a previously-reserved RC helper's global function index.
fn rcHelperFuncIdx(self: *Self, helper_key: RcHelperKey, atomicity: RcAtomicity) u32 {
    return self.rc_helper_funcs.get(rcHelperCacheKey(helper_key, atomicity)) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "WASM/codegen invariant violated: RC helper for layout {d} op {s} atomicity {s} was not reserved before body emission",
                .{ @intFromEnum(helper_key.layout_idx), @tagName(helper_key.op), @tagName(atomicity) },
            );
        }
        unreachable;
    };
}

fn emitBuiltinInternalBoxChildDropIfUnique(
    self: *Self,
    box_ptr_local: u32,
    child_key: RcHelperKey,
    atomicity: RcAtomicity,
) Allocator.Error!void {
    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(box_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLoadI32AtPtrOffset(box_ptr_local, -4, rc_val);
    try self.emitLocalGet(rc_val);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitRawRcHelperCallByKey(child_key, atomicity, box_ptr_local, null);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitErasedCallableOnDropIfUnique(
    self: *Self,
    payload_ptr_local: u32,
) Allocator.Error!void {
    const rc_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(payload_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLoadI32AtPtrOffset(payload_ptr_local, -4, rc_val);
    try self.emitLocalGet(rc_val);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitErasedCallableOnDrop(payload_ptr_local);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitErasedCallableOnDrop(
    self: *Self,
    payload_ptr_local: u32,
) Allocator.Error!void {
    const on_drop_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(payload_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(payload_ptr_local);
    try self.emitLoadOpSized(.i32, 4, wasm_erased_callable_on_drop_offset);
    try self.emitLocalSet(on_drop_local);

    try self.emitLocalGet(on_drop_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitLocalGet(payload_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(builtins.erased_callable.capture_offset)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalGet(self.roc_ops_local);
    try self.emitLocalGet(on_drop_local);
    try self.emitCallIndirect(self.roc_ops_type_idx);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn generateBuiltinInternalRcHelperBody(
    self: *Self,
    helper_key: RcHelperKey,
    atomicity: RcAtomicity,
    value_ptr_local: u32,
    count_local: ?u32,
) Allocator.Error!void {
    switch (self.getLayoutStore().rcHelperPlan(helper_key)) {
        .noop => {},
        .str_incref => {
            const alloc_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const is_small_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitDecodeStrAllocPtr(value_ptr_local, alloc_ptr_local, is_small_local);

            self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitLocalGet(is_small_local);
            self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitDataPtrIncrefByLocal(alloc_ptr_local, count_local.?);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .str_decref => try self.emitBuiltinInternalStrRc(.decref, value_ptr_local, 1),
        .str_free => try self.emitBuiltinInternalStrRc(.free, value_ptr_local, 1),
        .list_incref => |list_plan| try self.emitBuiltinInternalListIncrefByLocal(value_ptr_local, helper_key.layout_idx, list_plan, count_local.?),
        .list_decref => |list_plan| try self.emitBuiltinInternalListRc(.decref, value_ptr_local, helper_key.layout_idx, list_plan, atomicity, 1),
        .list_free => |list_plan| try self.emitBuiltinInternalListRc(.free, value_ptr_local, helper_key.layout_idx, list_plan, atomicity, 1),
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
                try self.emitBuiltinInternalBoxChildDropIfUnique(box_ptr_local, child_key, atomicity);
            }
            try self.emitDataPtrDecref(box_ptr_local, box_plan.elem_alignment, box_plan.child != null);
        },
        .box_free => |box_plan| {
            const box_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(box_ptr_local);
            if (box_plan.child) |child_key| {
                try self.emitBuiltinInternalBoxChildDropIfUnique(box_ptr_local, child_key, atomicity);
            }
            try self.emitDataPtrFree(box_ptr_local, box_plan.elem_alignment, box_plan.child != null);
        },
        .erased_callable_incref => {
            const payload_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(payload_ptr_local);
            try self.emitDataPtrIncrefByLocal(payload_ptr_local, count_local.?);
        },
        .erased_callable_decref => {
            const payload_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(payload_ptr_local);
            try self.emitErasedCallableOnDropIfUnique(payload_ptr_local);
            try self.emitDataPtrDecref(
                payload_ptr_local,
                builtins.erased_callable.payload_alignment,
                builtins.erased_callable.allocation_has_refcounted_children,
            );
        },
        .erased_callable_free => {
            const payload_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(payload_ptr_local);
            try self.emitErasedCallableOnDrop(payload_ptr_local);
            try self.emitDataPtrFree(
                payload_ptr_local,
                builtins.erased_callable.payload_alignment,
                builtins.erased_callable.allocation_has_refcounted_children,
            );
        },
        .struct_ => |struct_plan| {
            const field_count = self.getLayoutStore().rcHelperStructFieldCount(struct_plan);
            var i: u32 = 0;
            while (i < field_count) : (i += 1) {
                const field_plan = self.getLayoutStore().rcHelperStructFieldPlan(struct_plan, i) orelse continue;
                const field_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalGet(value_ptr_local);
                if (field_plan.offset > 0) {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(field_plan.offset)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
                try self.emitLocalSet(field_ptr_local);
                try self.emitRawRcHelperCallByKey(field_plan.child, atomicity, field_ptr_local, count_local);
            }
        },
        .tag_union => |tag_plan| {
            const variant_count = self.getLayoutStore().rcHelperTagUnionVariantCount(tag_plan);
            if (variant_count == 0) return;

            if (variant_count == 1) {
                if (self.getLayoutStore().rcHelperTagUnionVariantPlan(tag_plan, 0)) |child_key| {
                    try self.emitRawRcHelperCallByKey(child_key, atomicity, value_ptr_local, count_local);
                }
                return;
            }

            const disc_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            const ls = self.getLayoutStore();
            const tu_layout = try WasmLayout.tagUnionLayoutWithStore(tag_plan.tag_union_idx, ls);
            const disc_size = tu_layout.discriminant_size;
            if (disc_size == 0) {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            } else {
                try self.emitLocalGet(value_ptr_local);
                try self.emitLoadBySize(disc_size, @intCast(tu_layout.discriminant_offset));
            }
            try self.emitLocalSet(disc_local);

            var variant_i: u32 = 0;
            while (variant_i < variant_count) : (variant_i += 1) {
                const child_key = self.getLayoutStore().rcHelperTagUnionVariantPlan(tag_plan, variant_i) orelse continue;
                self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

                try self.emitLocalGet(disc_local);
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(variant_i)) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

                try self.emitRawRcHelperCallByKey(child_key, atomicity, value_ptr_local, count_local);

                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            }
        },
        .closure => |child_key| {
            const captures_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(value_ptr_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalSet(captures_ptr_local);
            try self.emitRawRcHelperCallByKey(child_key, atomicity, captures_ptr_local, count_local);
        },
    }
}

/// True when an RC plan cannot be emitted inline and requires a compiled helper
/// function. Mirrors exactly the `false`-returning cases of `emitRawDirectRcPlan`.
fn rcPlanNeedsCompiledHelper(helper_plan: RcHelperPlan) bool {
    return switch (helper_plan) {
        .list_incref, .list_decref, .list_free => |list_plan| list_plan.child != null,
        .box_decref, .box_free => |box_plan| box_plan.child != null,
        .struct_, .tag_union, .closure => true,
        else => false,
    };
}

/// Append, in body-emission order, the child helper keys called inside the body of
/// the helper for `helper_plan`. Mirrors `generateBuiltinInternalRcHelperBody`'s
/// child-helper call sites so the reservation pre-order matches on-demand compilation.
fn appendRcHelperChildKeys(self: *Self, helper_plan: RcHelperPlan, out: *std.ArrayList(RcHelperKey), oa: Allocator) Allocator.Error!void {
    const ls = self.getLayoutStore();
    switch (helper_plan) {
        // list_incref's body increments the allocation in place; no child call.
        .list_incref => {},
        .list_decref, .list_free => |list_plan| {
            if (list_plan.child) |child_key| try out.append(oa, child_key);
        },
        .box_decref, .box_free => |box_plan| {
            if (box_plan.child) |child_key| try out.append(oa, child_key);
        },
        .struct_ => |struct_plan| {
            const field_count = ls.rcHelperStructFieldCount(struct_plan);
            var i: u32 = 0;
            while (i < field_count) : (i += 1) {
                const field_plan = ls.rcHelperStructFieldPlan(struct_plan, i) orelse continue;
                try out.append(oa, field_plan.child);
            }
        },
        .tag_union => |tag_plan| {
            const variant_count = ls.rcHelperTagUnionVariantCount(tag_plan);
            if (variant_count == 0) return;
            if (variant_count == 1) {
                if (ls.rcHelperTagUnionVariantPlan(tag_plan, 0)) |child_key| try out.append(oa, child_key);
                return;
            }
            var variant_i: u32 = 0;
            while (variant_i < variant_count) : (variant_i += 1) {
                const child_key = ls.rcHelperTagUnionVariantPlan(tag_plan, variant_i) orelse continue;
                try out.append(oa, child_key);
            }
        },
        .closure => |child_key| try out.append(oa, child_key),
        else => {},
    }
}

/// Reserve a module function slot for an RC helper (no body emitted yet), caching
/// its global function index. Returns the reserved index.
fn reserveRcHelperFunc(self: *Self, helper_key: RcHelperKey, atomicity: RcAtomicity) Allocator.Error!u32 {
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
    const defined = self.module.addDefinedFunction(type_idx) catch return error.OutOfMemory;
    const func_idx = defined.function.raw();
    const cache_key = rcHelperCacheKey(helper_key, atomicity);
    try self.rc_helper_funcs.put(cache_key, func_idx);
    _ = try self.addOwnedLocalFunctionSymbol(defined, "roc_rc_helper", cache_key);
    return func_idx;
}

/// Compile an RC helper function (and transitively any nested child helpers it
/// references) without recursion. The whole compiled tree shares one
/// count-update atomicity: nested values live inside the same allocation tree
/// the root statement names, so child helpers inherit the root's mode.
///
/// Compilation has two phases. First a pre-order traversal of the helper-plan DAG
/// reserves a module function slot for every transitively-reachable helper that
/// needs a compiled body — the same pre-order in which on-demand compilation would
/// assign function indices, and the `rc_helper_funcs` cache doubles as the visited
/// set so runtime-recursive (self-referential) helpers terminate. Then each newly
/// reserved helper's body is emitted; because every child slot is already reserved,
/// body emission resolves child references from the cache without re-entering
/// compilation. Both phases are driven by explicit heap-backed work stacks.
fn compileBuiltinInternalRcHelper(self: *Self, helper_key: RcHelperKey, atomicity: RcAtomicity) Allocator.Error!u32 {
    if (self.rc_helper_funcs.get(rcHelperCacheKey(helper_key, atomicity))) |func_idx| {
        return func_idx;
    }

    var sfa = std.heap.stackFallback(64 * @sizeOf(RcHelperKey), self.allocator);
    const wa = sfa.get();

    // Pre-order reservation of every transitively-needed helper slot.
    var to_emit = std.ArrayList(RcHelperKey).empty;
    defer to_emit.deinit(wa);

    var reserve_stack = std.ArrayList(RcHelperKey).empty;
    defer reserve_stack.deinit(wa);

    var children = std.ArrayList(RcHelperKey).empty;
    defer children.deinit(wa);

    const root_func_idx = try self.reserveRcHelperFunc(helper_key, atomicity);
    try to_emit.append(wa, helper_key);
    try reserve_stack.append(wa, helper_key);

    while (reserve_stack.pop()) |key| {
        const plan = self.getLayoutStore().rcHelperPlan(key);
        children.clearRetainingCapacity();
        try self.appendRcHelperChildKeys(plan, &children, wa);

        // Push children in reverse so popping reserves them in body-emission order.
        var i: usize = children.items.len;
        while (i > 0) {
            i -= 1;
            const child_key = children.items[i];
            const child_plan = self.getLayoutStore().rcHelperPlan(child_key);
            if (!rcPlanNeedsCompiledHelper(child_plan)) continue;
            if (self.rc_helper_funcs.contains(rcHelperCacheKey(child_key, atomicity))) continue;
            _ = try self.reserveRcHelperFunc(child_key, atomicity);
            try to_emit.append(wa, child_key);
            try reserve_stack.append(wa, child_key);
        }
    }

    // Emit each reserved helper body. Child references resolve from the cache.
    for (to_emit.items) |key| {
        try self.emitRcHelperBody(key, atomicity);
    }

    return root_func_idx;
}

/// Emit the body of an already-reserved RC helper function. Child helper references
/// resolve from `rc_helper_funcs` (reserved up front), so this never recurses.
fn emitRcHelperBody(self: *Self, helper_key: RcHelperKey, atomicity: RcAtomicity) Allocator.Error!void {
    const func_idx = self.rc_helper_funcs.get(rcHelperCacheKey(helper_key, atomicity)).?;
    const defined_local = self.localFunctionIndexFromGlobal(func_idx);

    const param_types: []const ValType = switch (helper_key.op) {
        .incref => &.{ .i32, .i32, .i32 },
        .decref, .free => &.{ .i32, .i32 },
    };

    const saved = try self.saveState();

    try self.beginFunction(defined_local);
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

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitLocalGet(value_ptr_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.generateBuiltinInternalRcHelperBody(helper_key, atomicity, value_ptr_local, count_local);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    try self.encodeLocalsDecl(&self.currentBody().preamble, @intCast(param_types.len));

    if (self.uses_stack_memory) {
        var prefix_buffer: [max_stack_prefix_bytes]u8 = undefined;
        var prefix = StackPrefixBytes.init(prefix_buffer[0..]);
        const prefix_allocator = prefix.allocator();
        var prefix_relocs: StackPrefixRelocs = .{};
        try self.appendStackPointerGlobalTo(prefix_allocator, &prefix.bytes, &prefix_relocs, Op.global_get);
        prefix.bytes.append(prefix_allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(prefix_allocator, &prefix.bytes, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        prefix.bytes.append(prefix_allocator, Op.i32_sub) catch return error.OutOfMemory;
        prefix.bytes.append(prefix_allocator, Op.local_tee) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(prefix_allocator, &prefix.bytes, self.fp_local) catch return error.OutOfMemory;
        try self.appendStackPointerGlobalTo(prefix_allocator, &prefix.bytes, &prefix_relocs, Op.global_set);
        try self.prependStackPrefix(prefix.bytes.items, prefix_relocs.items());

        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), self.fp_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        try self.emitStackPointerGlobal(Op.global_set);
    }

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.endFunction();
    self.restoreState(saved);
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
        .list_reinterpret => |list_reinterpret| try recordProcLocal(locals, list_reinterpret.backing_ref),
        .nominal => |nominal| try recordProcLocal(locals, nominal.backing_ref),
    }
}

fn collectProcLocals(
    self: *Self,
    stmt_id: CFStmtId,
    locals: *std.AutoHashMap(u64, void),
    visited: *std.AutoHashMap(u32, void),
) Allocator.Error!void {
    // Order-independent traversal of the CFStmt graph: a simple worklist over
    // statement ids, guarded by the `visited` set to handle join/jump cycles.
    var sfa = std.heap.stackFallback(64 * @sizeOf(CFStmtId), self.allocator);
    const wa = sfa.get();
    var work = std.ArrayList(CFStmtId).empty;
    defer work.deinit(wa);
    try work.append(wa, stmt_id);

    while (work.pop()) |current| {
        const gop = try visited.getOrPut(@intFromEnum(current));
        if (gop.found_existing) continue;

        switch (self.store.getCFStmt(current)) {
            .assign_ref => |assign| {
                try recordProcLocal(locals, assign.target);
                try recordRefOpLocals(locals, assign.op);
                try work.append(wa, assign.next);
            },
            .assign_literal => |assign| {
                try recordProcLocal(locals, assign.target);
                try work.append(wa, assign.next);
            },
            .init_uninitialized => |uninit| {
                try recordProcLocal(locals, uninit.target);
                try work.append(wa, uninit.next);
            },
            .assign_call => |assign| {
                try recordProcLocal(locals, assign.target);
                for (self.store.getLocalSpan(assign.args)) |arg| try recordProcLocal(locals, arg);
                try work.append(wa, assign.next);
            },
            .assign_call_erased => |assign| {
                try recordProcLocal(locals, assign.target);
                try recordProcLocal(locals, assign.closure);
                for (self.store.getLocalSpan(assign.args)) |arg| try recordProcLocal(locals, arg);
                try work.append(wa, assign.next);
            },
            .assign_packed_erased_fn => |assign| {
                try recordProcLocal(locals, assign.target);
                if (assign.capture) |capture| try recordProcLocal(locals, capture);
                try work.append(wa, assign.next);
            },
            .assign_low_level => |assign| {
                try recordProcLocal(locals, assign.target);
                for (self.store.getLocalSpan(assign.args)) |arg| try recordProcLocal(locals, arg);
                try work.append(wa, assign.next);
            },
            .assign_list => |assign| {
                try recordProcLocal(locals, assign.target);
                for (self.store.getLocalSpan(assign.elems)) |elem| try recordProcLocal(locals, elem);
                try work.append(wa, assign.next);
            },
            .assign_struct => |assign| {
                try recordProcLocal(locals, assign.target);
                for (self.store.getLocalSpan(assign.fields)) |field| try recordProcLocal(locals, field);
                try work.append(wa, assign.next);
            },
            .assign_tag => |assign| {
                try recordProcLocal(locals, assign.target);
                if (assign.payload) |payload| {
                    try recordProcLocal(locals, payload);
                }
                try work.append(wa, assign.next);
            },
            .set_local => |assign| {
                try recordProcLocal(locals, assign.target);
                try recordProcLocal(locals, assign.value);
                try work.append(wa, assign.next);
            },
            .debug => |debug_stmt| {
                try recordProcLocal(locals, debug_stmt.message);
                try work.append(wa, debug_stmt.next);
            },
            .expect_err => |expect_err_stmt| {
                try recordProcLocal(locals, expect_err_stmt.message);
            },
            .expect => |expect_stmt| {
                try recordProcLocal(locals, expect_stmt.condition);
                try work.append(wa, expect_stmt.next);
            },
            .comptime_branch_taken => |marker| try work.append(wa, marker.next),
            .runtime_error, .comptime_exhaustiveness_failed => {},
            .switch_stmt => |switch_stmt| {
                try recordProcLocal(locals, switch_stmt.cond);
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try work.append(wa, branch.body);
                }
                try work.append(wa, switch_stmt.default_branch);
            },
            .switch_initialized_payload => |switch_stmt| {
                try recordProcLocal(locals, switch_stmt.cond);
                try recordProcLocal(locals, switch_stmt.payload);
                try work.append(wa, switch_stmt.initialized_branch);
                try work.append(wa, switch_stmt.uninitialized_branch);
            },
            .str_match => |str_match| {
                try recordProcLocal(locals, str_match.source);
                for (self.store.getStrMatchSteps(str_match.steps)) |step| {
                    switch (step.capture) {
                        .discard => {},
                        .view => |local| try recordProcLocal(locals, local),
                    }
                }
                try work.append(wa, str_match.on_match);
                try work.append(wa, str_match.on_miss);
            },
            .str_match_set => |str_match_set| {
                try recordProcLocal(locals, str_match_set.source);
                for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                    for (self.store.getStrMatchSteps(arm.steps)) |step| {
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| try recordProcLocal(locals, local),
                        }
                    }
                    try work.append(wa, arm.on_match);
                }
                try work.append(wa, str_match_set.on_miss);
            },
            .join => |join_stmt| {
                for (self.store.getLocalSpan(join_stmt.params)) |param| try recordProcLocal(locals, param);
                try work.append(wa, join_stmt.body);
                try work.append(wa, join_stmt.remainder);
            },
            .jump => {},
            .loop_break => {},
            .ret => |ret_stmt| try recordProcLocal(locals, ret_stmt.value),
            .incref => |inc| {
                try recordProcLocal(locals, inc.value);
                try work.append(wa, inc.next);
            },
            .decref => |dec| {
                try recordProcLocal(locals, dec.value);
                try work.append(wa, dec.next);
            },
            .decref_if_initialized => |dec| {
                try recordProcLocal(locals, dec.cond);
                try recordProcLocal(locals, dec.value);
                try work.append(wa, dec.next);
            },
            .free => |free_stmt| {
                try recordProcLocal(locals, free_stmt.value);
                try work.append(wa, free_stmt.next);
            },
            .crash => {},
            .loop_continue => {},
        }
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
        const vt = try self.procLocalValType(local_id);
        _ = try self.storage.allocLocal(local_id, vt);
    }
}

fn procLocalLayoutIdx(self: *Self, value: ProcLocalId) layout.Idx {
    return self.store.getLocal(value).layout_idx;
}

/// Infer the wasm ValType that an explicit local value will push onto the stack.
fn procLocalValType(self: *Self, value: ProcLocalId) Allocator.Error!ValType {
    return try self.resolveValType(self.procLocalLayoutIdx(value));
}

fn emitCanonicalizeScalarForLayout(self: *Self, layout_idx: layout.Idx) Allocator.Error!void {
    if (try self.resolveValType(layout_idx) != .i32) return;

    switch (layout_idx) {
        .i8 => self.currentCode().append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory,
        .i16 => self.currentCode().append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory,
        .u8 => {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xFF) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .u16 => {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xFFFF) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        else => {},
    }
}

/// Get the byte size of the value a local produces.
fn procLocalByteSize(self: *Self, value: ProcLocalId) Allocator.Error!u32 {
    return try self.layoutByteSize(self.procLocalLayoutIdx(value));
}

/// Check if a local produces a composite value (stored in stack memory).
fn isCompositeLocal(self: *const Self, value: ProcLocalId) Allocator.Error!bool {
    return try self.isCompositeLayout(self.store.getLocal(value).layout_idx);
}

/// Check if a layout represents a composite type stored in stack memory.
fn isCompositeLayout(self: *const Self, layout_idx: layout.Idx) Allocator.Error!bool {
    const repr = try WasmLayout.wasmReprWithStore(layout_idx, self.getLayoutStore());
    return switch (repr) {
        .stack_memory => |s| s > 0,
        .primitive => false,
    };
}

fn unwrapSingleFieldPayloadLayout(self: *const Self, layout_idx: layout.Idx) ?layout.Idx {
    const ls = self.getLayoutStore();
    const layout_val = ls.getLayout(layout_idx);
    if (layout_val.tag != .struct_) return null;

    const struct_data = ls.getStructData(layout_val.getStruct().idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    if (fields.len != 1) return null;

    const field = fields.get(0);
    if (field.index != 0) return null;
    return field.layout;
}

fn findBadUtf8Variant(self: *const Self, inner_tu: *const layout.TagUnionData) Allocator.Error!?struct { disc: u16, struct_idx: layout.StructIdx } {
    const ls = self.getLayoutStore();
    const variants = ls.getTagUnionVariants(inner_tu);
    for (0..variants.len) |i| {
        const payload = variants.get(@intCast(i)).payload_layout;
        const candidate = self.unwrapSingleFieldPayloadLayout(payload) orelse payload;
        const payload_layout = ls.getLayout(candidate);
        if (payload_layout.tag != .struct_) continue;

        const struct_idx = payload_layout.getStruct().idx;
        const struct_data = ls.getStructData(struct_idx);
        const fields = ls.struct_fields.sliceRange(struct_data.getFields());
        if (fields.len != 2) continue;

        var has_index_field = false;
        var has_problem_field = false;
        for (0..fields.len) |field_i| {
            const field = fields.get(field_i);
            const field_size = try self.layoutStorageByteSize(field.layout);
            switch (field.index) {
                0 => has_index_field = field_size == 8,
                1 => has_problem_field = field_size == 1,
                else => {},
            }
        }
        if (has_index_field and has_problem_field) {
            return .{ .disc = @intCast(i), .struct_idx = struct_idx };
        }
    }

    return null;
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
                    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
                }
                return;
            },
            .tag_union => {
                const tu_info = ls.getTagUnionInfo(l);
                if (tu_info.contains_refcounted) {
                    try self.compareTagUnionByLayout(lhs_local, rhs_local, lay_idx);
                    if (negate) {
                        self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
                    }
                    return;
                }
                // No heap types — fall through to bytewise comparison
            },
            else => {},
        }
    }

    // Fallback: bytewise comparison for types without heap-allocated fields
    const byte_size = try self.procLocalByteSize(lhs);
    try self.emitBytewiseEq(lhs_local, rhs_local, byte_size);

    if (negate) {
        self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    }
}

/// Work item for the explicit (non-recursive) structural-equality driver.
///
/// Structural equality recurses over nested layouts (records inside records,
/// tag-union payloads, list elements). To stay stack-safe the four mutually
/// recursive emitters are expressed as work items processed by `runEqWork`:
/// a frame that "would recurse" instead pushes its glue continuation(s) and the
/// child frame(s), preserving the exact interleaving of glue with child descent.
/// Items are pushed in reverse so LIFO popping keeps source emission order.
const EqWork = union(enum) {
    /// Emit composite (record/tuple) equality at lhs/rhs pointers. Leaves i32.
    composite: struct { lhs: u32, rhs: u32, layout_idx: layout.Idx },
    /// Emit a single field comparison. Leaves i32.
    field: struct {
        lhs: u32,
        rhs: u32,
        field_offset: u32,
        field_size: u32,
        field_layout_idx: layout.Idx,
    },
    /// Emit tag-union equality. Leaves i32.
    tag_union: struct { lhs: u32, rhs: u32, layout_idx: layout.Idx },
    /// Emit an inline list element-by-element comparison loop. Leaves i32.
    list_loop: struct { lhs: u32, rhs: u32, elem_layout_idx: layout.Idx, elem_size: u32 },
    /// Glue: emit a bare i32_and (used between struct field results).
    emit_and: void,
    /// Glue: emit the per-variant prefix (open block + br_if skip if disc mismatch).
    tu_variant_before: struct { lhs_disc: u32, variant_i: u32 },
    /// Glue: emit the per-variant suffix (local.set payload_eq, end block).
    tu_variant_after: *TuEqState,
    /// Glue: emit tag-union finish (and into result, end outer block, push result).
    tu_finish: *TuEqState,
    /// Glue: emit list-loop suffix after the element comparison child.
    list_loop_after: *ListEqState,
};

/// Heap-allocated state threaded through a tag-union comparison's continuations.
const TuEqState = struct {
    lhs_disc: u32,
    disc_eq_local: u32,
    payload_eq_local: u32,
    result_local: u32,
    have_payload: bool,
};

/// Heap-allocated state threaded through a list-loop comparison's continuation.
const ListEqState = struct {
    result_local: u32,
    idx_local: u32,
};

/// Compare a composite type (record or tuple) field-by-field using layout information.
/// Pushes an i32 (1=equal, 0=not equal) onto the WASM stack.
fn compareCompositeByLayout(self: *Self, lhs_local: u32, rhs_local: u32, layout_idx: layout.Idx) Allocator.Error!void {
    try self.runEqWork(.{ .composite = .{ .lhs = lhs_local, .rhs = rhs_local, .layout_idx = layout_idx } });
}

/// Compare a tag union by layout: compare discriminants first, then dispatch
/// per-variant payload comparison. Pushes i32 result onto the WASM stack.
fn compareTagUnionByLayout(self: *Self, lhs_local: u32, rhs_local: u32, layout_idx: layout.Idx) Allocator.Error!void {
    try self.runEqWork(.{ .tag_union = .{ .lhs = lhs_local, .rhs = rhs_local, .layout_idx = layout_idx } });
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
    try self.runEqWork(.{ .list_loop = .{
        .lhs = lhs_local,
        .rhs = rhs_local,
        .elem_layout_idx = elem_layout_idx,
        .elem_size = elem_size,
    } });
}

/// Explicit work-stack driver for the structural-equality emitters. Processes
/// `initial` and any frames it transitively pushes, preserving exact emission
/// order and the value-stack discipline of the original mutually recursive
/// `compareCompositeByLayout`/`compareTagUnionByLayout`/`compareFieldByLayout`/
/// `emitListEqLoop` functions.
fn runEqWork(self: *Self, initial: EqWork) Allocator.Error!void {
    var sfa = std.heap.stackFallback(64 * @sizeOf(EqWork), self.allocator);
    const wa = sfa.get();
    var work = std.ArrayList(EqWork).empty;
    defer work.deinit(wa);
    try work.append(wa, initial);

    while (work.pop()) |item| {
        switch (item) {
            .emit_and => {
                self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            },
            .composite => |c| try self.expandComposite(&work, wa, c.lhs, c.rhs, c.layout_idx),
            .field => |f| try self.expandField(&work, wa, f.lhs, f.rhs, f.field_offset, f.field_size, f.field_layout_idx),
            .tag_union => |t| try self.expandTagUnion(&work, wa, t.lhs, t.rhs, t.layout_idx),
            .list_loop => |ll| try self.expandListLoop(&work, wa, ll.lhs, ll.rhs, ll.elem_layout_idx, ll.elem_size),
            .tu_variant_before => |v| {
                // Check: if (disc == variant_i) { compare payload with this variant's layout }
                self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, 0x40) catch return error.OutOfMemory; // void block

                // Skip if disc != variant_i
                try self.emitLocalGet(v.lhs_disc);
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(v.variant_i)) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            },
            .tu_variant_after => |state| {
                try self.emitLocalSet(state.payload_eq_local);
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory; // end variant block
            },
            .tu_finish => |state| {
                if (state.have_payload) {
                    // result = disc_eq AND payload_eq
                    try self.emitLocalGet(state.disc_eq_local);
                    try self.emitLocalGet(state.payload_eq_local);
                    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                    try self.emitLocalSet(state.result_local);
                }
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory; // end outer block
                // Push result
                try self.emitLocalGet(state.result_local);
                self.allocator.destroy(state);
            },
            .list_loop_after => |state| {
                // result = result AND elem_eq
                try self.emitLocalGet(state.result_local);
                self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                try self.emitLocalSet(state.result_local);

                // idx++
                try self.emitLocalGet(state.idx_local);
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                try self.emitLocalSet(state.idx_local);

                // br 0 (continue loop)
                self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

                // end loop, end block
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

                // Push final result
                try self.emitLocalGet(state.result_local);
                self.allocator.destroy(state);
            },
        }
    }
}

/// Expand a `composite` work frame: emit struct field comparisons (pushing child
/// frames + `emit_and` glue), or a bytewise comparison for non-structs.
fn expandComposite(self: *Self, work: *std.ArrayList(EqWork), wa: Allocator, lhs_local: u32, rhs_local: u32, layout_idx: layout.Idx) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);

    switch (l.tag) {
        .struct_ => {
            const struct_idx = l.getStruct().idx;
            const struct_data = ls.getStructData(struct_idx);
            const field_count = struct_data.fields.count;
            if (field_count == 0) {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
                return;
            }

            // Collect the non-zero-size fields in source order, then push their
            // child frames (and the `i32_and` glue that follows all but the first)
            // in reverse so popping reproduces the original left-to-right emission.
            var sfa = std.heap.stackFallback(16 * @sizeOf(u32), self.allocator);
            const ta = sfa.get();
            var fields = std.ArrayList(u32).empty;
            defer fields.deinit(ta);

            var field_i: u32 = 0;
            while (field_i < field_count) : (field_i += 1) {
                // Padding spacers hold uninitialized bytes; never compare them.
                if (ls.getStructFieldIsPadding(struct_idx, @intCast(field_i))) continue;
                const field_size = try self.structFieldSizeBySortedIndexWasm(struct_idx, @intCast(field_i));
                if (field_size == 0) continue;
                try fields.append(ta, field_i);
            }

            if (fields.items.len == 0) {
                // All fields were zero-size
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
                return;
            }

            var i: usize = fields.items.len;
            while (i > 0) {
                i -= 1;
                const fi = fields.items[i];
                if (i != 0) {
                    // `i32_and` runs after this field's result is on the stack.
                    try work.append(wa, .emit_and);
                }
                const field_offset = try self.structFieldOffsetBySortedIndexWasm(struct_idx, @intCast(fi));
                const field_size = try self.structFieldSizeBySortedIndexWasm(struct_idx, @intCast(fi));
                const field_layout_idx = ls.getStructFieldLayout(struct_idx, @intCast(fi));
                try work.append(wa, .{ .field = .{
                    .lhs = lhs_local,
                    .rhs = rhs_local,
                    .field_offset = field_offset,
                    .field_size = field_size,
                    .field_layout_idx = field_layout_idx,
                } });
            }
        },
        else => {
            // Non-composite layouts compare directly as raw bytes.
            const byte_size = ls.layoutSizeAlign(l).size;
            try self.emitBytewiseEq(lhs_local, rhs_local, byte_size);
        },
    }
}

/// Expand a `tag_union` work frame: emit the discriminant comparison and block
/// scaffolding, then push per-variant payload children plus their glue.
fn expandTagUnion(self: *Self, work: *std.ArrayList(EqWork), wa: Allocator, lhs_local: u32, rhs_local: u32, layout_idx: layout.Idx) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);
    std.debug.assert(l.tag == .tag_union);

    const tu_layout = try WasmLayout.tagUnionLayoutWithStore(l.getTagUnion().idx, ls);
    const disc_offset = tu_layout.discriminant_offset;
    const disc_size = tu_layout.discriminant_size;

    // Allocate a local to hold the result
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // Load LHS discriminant
    if (disc_size == 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    } else {
        try self.emitLocalGet(lhs_local);
        try self.emitLoadBySize(disc_size, @intCast(disc_offset));
    }
    const lhs_disc = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_disc);

    // Load RHS discriminant
    if (disc_size == 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    } else {
        try self.emitLocalGet(rhs_local);
        try self.emitLoadBySize(disc_size, @intCast(disc_offset));
    }
    const rhs_disc = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(rhs_disc);

    // Compare discriminants
    try self.emitLocalGet(lhs_disc);
    try self.emitLocalGet(rhs_disc);
    self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, 0x40) catch return error.OutOfMemory; // void block type
    try self.emitLocalGet(disc_eq_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory; // disc_ne
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // break out of block

    const state = try self.allocator.create(TuEqState);
    state.* = .{
        .lhs_disc = lhs_disc,
        .disc_eq_local = disc_eq_local,
        .payload_eq_local = undefined,
        .result_local = result_local,
        .have_payload = false,
    };
    // `tu_finish` runs last: emit closing glue + push result, after every variant.
    try work.append(wa, .{ .tu_finish = state });

    // Payload comparison: compare based on variant
    // For simplicity, compare the payload bytes up to discriminant_offset
    // using layout-aware comparison for the variant's payload layout
    const variants = ls.getTagUnionVariants(ls.getTagUnionData(l.getTagUnion().idx));
    if (variants.len > 0) {
        const payload_size = disc_offset; // Payload occupies bytes [0..disc_offset)
        if (payload_size > 0) {
            // Compare payloads based on variant layout
            // For each variant, check if discriminant matches and compare payload
            // Use a local to accumulate payload equality
            const payload_eq_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            state.payload_eq_local = payload_eq_local;
            state.have_payload = true;

            // Default: payload equal (1) - will be overwritten
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            try self.emitLocalSet(payload_eq_local);

            // Each emitting variant (payload size > 0) emits, in source order:
            //   [open block + br_if skip] [payload child] [set payload_eq, end].
            // `compareFieldByLayout` reproduces both the layout-aware and the
            // scalar (bytewise-at-offset-0) cases of the original, so a single
            // `.field` child covers every payload kind. Push prefix/child/suffix in
            // reverse so popping reproduces the original left-to-right emission.
            var i: usize = variants.len;
            while (i > 0) {
                i -= 1;
                const variant_payload_layout = variants.get(i).payload_layout;
                const variant_payload_size = try self.layoutByteSize(variant_payload_layout);
                if (variant_payload_size == 0) continue;

                // Suffix runs after the child: set payload_eq, end block.
                try work.append(wa, .{ .tu_variant_after = state });

                // Payload child.
                try work.append(wa, .{ .field = .{
                    .lhs = lhs_local,
                    .rhs = rhs_local,
                    .field_offset = 0,
                    .field_size = variant_payload_size,
                    .field_layout_idx = variant_payload_layout,
                } });

                // Prefix runs before the child: open the variant block + br_if skip.
                try work.append(wa, .{ .tu_variant_before = .{ .lhs_disc = lhs_disc, .variant_i = @intCast(i) } });
            }
        }
    }
}

/// Compare a single field by layout type. Pushes i32 (1=equal, 0=not equal) onto the WASM stack.
/// lhs_local/rhs_local are i32 pointers to the parent struct, field_offset is the byte offset.
fn expandField(
    self: *Self,
    work: *std.ArrayList(EqWork),
    wa: Allocator,
    lhs_local: u32,
    rhs_local: u32,
    field_offset: u32,
    field_size: u32,
    field_layout_idx: layout.Idx,
) Allocator.Error!void {
    if (field_layout_idx == .str) {
        const lhs_field_local = try self.emitAddressOffsetToLocal(lhs_local, field_offset);
        const rhs_field_local = try self.emitAddressOffsetToLocal(rhs_local, field_offset);
        try self.emitStrEqCall(lhs_field_local, rhs_field_local);
        return;
    }

    const ls = self.getLayoutStore();

    const field_layout = ls.getLayout(field_layout_idx);
    switch (field_layout.tag) {
        .list => {
            const elem_layout = field_layout.getIdx();
            const lhs_list_local = try self.emitAddressOffsetToLocal(lhs_local, field_offset);
            const rhs_list_local = try self.emitAddressOffsetToLocal(rhs_local, field_offset);

            if (elem_layout == .str) {
                try self.emitListEqCall(lhs_list_local, rhs_list_local, .list_str_eq, self.list_str_eq_import, null);
            } else if (ls.getLayout(elem_layout).tag == .list) {
                const inner_elem_layout = ls.getLayout(elem_layout).getIdx();
                const inner_elem_size = try self.layoutByteSize(inner_elem_layout);
                try self.emitListEqCall(lhs_list_local, rhs_list_local, .list_list_eq, self.list_list_eq_import, inner_elem_size);
            } else if (builtinInternalLayoutContainsRefcounted(ls, "wasm.compareFieldByLayout.builtin_elem_rc", elem_layout)) {
                // Composite elements (records/tuples/tag-unions with refcounted fields):
                // inline element-by-element structural comparison loop.
                const elem_size = try self.layoutByteSize(elem_layout);
                try work.append(wa, .{ .list_loop = .{
                    .lhs = lhs_list_local,
                    .rhs = rhs_list_local,
                    .elem_layout_idx = elem_layout,
                    .elem_size = elem_size,
                } });
            } else {
                const elem_size = try self.layoutByteSize(elem_layout);
                try self.emitListEqCall(lhs_list_local, rhs_list_local, .list_eq, self.list_eq_import, elem_size);
            }
        },
        .struct_, .tag_union => {
            // Nested composite: create offset locals and recurse
            const lhs_field_local = try self.emitAddressOffsetToLocal(lhs_local, field_offset);
            const rhs_field_local = try self.emitAddressOffsetToLocal(rhs_local, field_offset);

            if (field_layout.tag == .tag_union) {
                const tu_info = ls.getTagUnionInfo(field_layout);
                if (tu_info.contains_refcounted) {
                    try work.append(wa, .{ .tag_union = .{
                        .lhs = lhs_field_local,
                        .rhs = rhs_field_local,
                        .layout_idx = field_layout_idx,
                    } });
                } else {
                    try self.emitBytewiseEq(lhs_field_local, rhs_field_local, field_size);
                }
            } else {
                try work.append(wa, .{ .composite = .{
                    .lhs = lhs_field_local,
                    .rhs = rhs_field_local,
                    .layout_idx = field_layout_idx,
                } });
            }
        },
        else => {
            // Scalar/other: bytewise comparison at offset
            try self.emitBytewiseEqAtOffset(lhs_local, rhs_local, field_offset, field_size);
        },
    }
}

/// Expand a `list_loop` work frame: emit the list element-comparison loop, pushing
/// the element comparison child plus the `list_loop_after` glue continuation.
fn expandListLoop(
    self: *Self,
    work: *std.ArrayList(EqWork),
    wa: Allocator,
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
    self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
    try self.emitLocalSet(result_local);

    // Load data pointers (offset 0 in RocList)
    try self.emitLocalGet(lhs_local);
    try self.emitLoadOp(.i32, 0);
    try self.emitLocalSet(lhs_data);

    try self.emitLocalGet(rhs_local);
    try self.emitLoadOp(.i32, 0);
    try self.emitLocalSet(rhs_data);

    // idx = 0
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(idx_local);

    // block { loop {
    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // if result == 0, break (lengths didn't match or previous elem failed)
    try self.emitLocalGet(result_local);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;

    // if idx >= lhs_len, break (all elements compared)
    try self.emitLocalGet(idx_local);
    try self.emitLocalGet(lhs_len);
    self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;

    // lhs_elem = lhs_data + idx * elem_size
    try self.emitLocalGet(lhs_data);
    try self.emitLocalGet(idx_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_elem);

    // rhs_elem = rhs_data + idx * elem_size
    try self.emitLocalGet(rhs_data);
    try self.emitLocalGet(idx_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rhs_elem);

    // The post-child glue (accumulate result, idx++, loop, close blocks, push
    // result) runs after the element comparison child completes.
    const state = try self.allocator.create(ListEqState);
    state.* = .{ .result_local = result_local, .idx_local = idx_local };
    try work.append(wa, .{ .list_loop_after = state });

    // Compare elements structurally: pushes i32 result onto stack
    try work.append(wa, .{ .field = .{
        .lhs = lhs_elem,
        .rhs = rhs_elem,
        .field_offset = 0,
        .field_size = elem_size,
        .field_layout_idx = elem_layout_idx,
    } });
}

/// Emit bytewise equality comparison of two memory regions.
/// lhs_local and rhs_local are i32 pointers, compared for byte_size bytes starting at offset 0.
/// Pushes an i32 (1=equal, 0=not equal) onto the WASM stack.
fn emitBytewiseEq(self: *Self, lhs_local: u32, rhs_local: u32, byte_size: u32) Allocator.Error!void {
    if (byte_size == 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        return;
    }

    var offset: u32 = 0;
    var first = true;

    while (offset + 4 <= byte_size) : (offset += 4) {
        try self.emitLocalGet(lhs_local);
        self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    if (offset + 2 <= byte_size) {
        try self.emitLocalGet(lhs_local);
        self.currentCode().append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.currentCode().append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
        offset += 2;
    }

    if (offset < byte_size) {
        try self.emitLocalGet(lhs_local);
        self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    if (first) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    }
}

/// Emit bytewise equality comparison at a specific offset within parent structs.
/// Pushes an i32 (1=equal, 0=not equal) onto the WASM stack.
fn emitBytewiseEqAtOffset(self: *Self, lhs_local: u32, rhs_local: u32, base_offset: u32, byte_size: u32) Allocator.Error!void {
    if (byte_size == 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        return;
    }

    var offset: u32 = 0;
    var first = true;

    while (offset + 4 <= byte_size) : (offset += 4) {
        try self.emitLocalGet(lhs_local);
        self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_offset + offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_offset + offset) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    if (offset + 2 <= byte_size) {
        try self.emitLocalGet(lhs_local);
        self.currentCode().append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_offset + offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.currentCode().append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_offset + offset) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
        offset += 2;
    }

    if (offset < byte_size) {
        try self.emitLocalGet(lhs_local);
        self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_offset + offset) catch return error.OutOfMemory;

        try self.emitLocalGet(rhs_local);
        self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_offset + offset) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        if (!first) {
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    if (first) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    }
}

/// Emit a load instruction appropriate for the discriminant size.
/// Loads an unsigned integer of disc_size bytes at the given offset from the address on the stack.
fn emitLoadBySize(self: *Self, disc_size: u8, offset: u16) Allocator.Error!void {
    switch (disc_size) {
        0 => {
            self.currentCode().append(self.allocator, Op.drop) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        },
        1 => {
            self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
        },
        2 => {
            self.currentCode().append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
        },
        4 => {
            self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
        },
        8 => {
            self.currentCode().append(self.allocator, Op.i64_load) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 3) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
            // Wrap to i32 since callers use i32 locals for discriminant values
            self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
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
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
                    try self.emitCall(import_idx);
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
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
                self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;
                try self.emitI128Sub(lhs_local, rhs_local);
                self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
                try self.emitI128Sub(rhs_local, lhs_local);
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            },
            .num_bitwise_and => try self.emitI128Bitwise(lhs_local, rhs_local, Op.i64_and),
            .num_bitwise_or => try self.emitI128Bitwise(lhs_local, rhs_local, Op.i64_or),
            .num_bitwise_xor => try self.emitI128Bitwise(lhs_local, rhs_local, Op.i64_xor),
            else => unreachable,
        }
    } else {
        // Unary composite op (num_neg handled before calling this function)
        switch (op) {
            .num_negate => try self.emitCompositeI128Negate(args[0], ret_layout),
            .num_bitwise_not => try self.emitCompositeI128BitwiseNot(args[0]),
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
    try self.emitCall(import_idx);

    // Push result pointer
    try self.emitLocalGet(result_local);
}

fn emitDecBinaryMath(self: *Self, args: []const ProcLocalId, import_idx: u32) Allocator.Error!void {
    try self.emitProcLocal(args[0]);
    const lhs_local = try self.stabilizeCompositeResult(16);

    try self.emitProcLocal(args[1]);
    const rhs_local = try self.stabilizeCompositeResult(16);

    try self.emitI128HostBinOp(lhs_local, rhs_local, import_idx);
}

fn emitDecUnaryMath(self: *Self, arg: ProcLocalId, import_idx: u32) Allocator.Error!void {
    try self.emitProcLocal(arg);
    const arg_local = try self.stabilizeCompositeResult(16);

    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_local);

    try self.emitLocalGet(arg_local);
    try self.emitLocalGet(result_local);
    try self.emitCall(import_idx);

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
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Pre-load all operand words into locals (prevents aliasing with result memory)
    const a_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_low) catch return error.OutOfMemory;

    const a_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_high) catch return error.OutOfMemory;

    const b_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_low) catch return error.OutOfMemory;

    const b_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_high) catch return error.OutOfMemory;

    // result_low = a_low + b_low
    const result_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_low) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_low) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_low) catch return error.OutOfMemory;

    // Store result_low
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // carry = (result_low < a_low) ? 1 : 0
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_low) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_low) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_lt_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;

    // result_high = a_high + b_high + carry
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;

    // Store result_high
    const result_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_high) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

/// Emit i128 subtraction: result = lhs - rhs
/// Pushes an i32 pointer to the 16-byte result on the wasm stack.
/// Pre-loads all operand words into locals to avoid aliasing issues
/// when result memory overlaps with lhs/rhs (e.g., in loops).
fn emitI128Sub(self: *Self, lhs_local: u32, rhs_local: u32) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Pre-load all operand words into locals (prevents aliasing with result memory)
    const a_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_low) catch return error.OutOfMemory;

    const a_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_high) catch return error.OutOfMemory;

    const b_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_low) catch return error.OutOfMemory;

    const b_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_high) catch return error.OutOfMemory;

    // result_low = a_low - b_low
    const result_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_low) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_low) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_low) catch return error.OutOfMemory;

    // Store result_low
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // borrow = (a_low < b_low) ? 1 : 0
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_low) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_low) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_lt_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    const borrow_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), borrow_local) catch return error.OutOfMemory;

    // result_high = (a_high - b_high) - borrow
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), borrow_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;

    // Store result_high
    const result_high_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_high_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_high_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    try self.emitLocalSet(shift_local);

    // Locals for result
    const r_low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const r_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;

    // Branch: if shift >= 64
    try self.emitLocalGet(shift_local);
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_ge_u) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(WasmModule.BlockType.void)) catch return error.OutOfMemory;

    // === shift >= 64 path ===
    switch (op) {
        .num_shift_left_by => {
            // r_low = 0, r_high = a_low << (shift - 64)
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(shift_local);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
        },
        .num_shift_right_by => {
            // r_high = a_high >> 63 (sign extend), r_low = a_high >> (shift - 64)  [arithmetic]
            try self.emitLocalGet(a_high);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 63) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
        },
        .num_shift_right_zf_by => {
            // r_high = 0, r_low = a_high >> (shift - 64)  [logical]
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
        },
        else => unreachable,
    }

    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;

    // === shift < 64 path ===
    // inv = 64 - shift
    const inv_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 64) catch return error.OutOfMemory;
    try self.emitLocalGet(shift_local);
    self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(inv_local);

    switch (op) {
        .num_shift_left_by => {
            // r_low = a_low << shift
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(shift_local);
            self.currentCode().append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
            // r_high = (a_high << shift) | (a_low >> inv)
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.currentCode().append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(inv_local);
            self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_or) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
        },
        .num_shift_right_by => {
            // r_high = a_high >> shift  [arithmetic]
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.currentCode().append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
            // r_low = (a_low >> shift) | (a_high << inv)
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(shift_local);
            self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(inv_local);
            self.currentCode().append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_or) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
        },
        .num_shift_right_zf_by => {
            // r_high = a_high >> shift  [logical]
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(shift_local);
            self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalSet(r_high);
            // r_low = (a_low >> shift) | (a_high << inv)
            try self.emitLocalGet(a_low);
            try self.emitLocalGet(shift_local);
            self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalGet(a_high);
            try self.emitLocalGet(inv_local);
            self.currentCode().append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_or) catch return error.OutOfMemory;
            try self.emitLocalSet(r_low);
        },
        else => unreachable,
    }

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

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
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Pre-load all operand words into locals
    const a_lo = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_lo) catch return error.OutOfMemory;

    const a_hi = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_hi) catch return error.OutOfMemory;

    const b_lo = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_lo) catch return error.OutOfMemory;

    const b_hi = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_hi) catch return error.OutOfMemory;

    // --- result_lo = a_lo * b_lo (truncating i64.mul) ---
    const result_lo_val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_lo) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_lo) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_lo_val) catch return error.OutOfMemory;

    // Store result_lo
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_lo_val) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // --- Compute high64(a_lo * b_lo) using 32-bit schoolbook method ---
    // Split a_lo into 32-bit halves: al0 = a_lo & 0xFFFFFFFF, al1 = a_lo >> 32
    const al0 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const al1 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_lo) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0xFFFFFFFF) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), al0) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_lo) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), al1) catch return error.OutOfMemory;

    // Split b_lo similarly
    const bl0 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const bl1 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_lo) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0xFFFFFFFF) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), bl0) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_lo) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), bl1) catch return error.OutOfMemory;

    // t = al0 * bl0
    const t = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), al0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), bl0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), t) catch return error.OutOfMemory;

    // cross = (t >> 32) + al1*bl0
    const cross = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), t) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), al1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), bl0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;

    // cross = cross + al0*bl1 (may overflow u64 — need to track carry)
    // Save old cross for overflow detection
    const old_cross = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), old_cross) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), al0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), bl1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;

    // carry = (cross < old_cross) ? 1 : 0  (unsigned overflow detection)
    const carry = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), old_cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_lt_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), carry) catch return error.OutOfMemory;

    // high64_of_lo_mul = al1*bl1 + (cross >> 32) + (carry << 32)
    const hi_of_lo_mul = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), al1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), bl1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    // Add carry << 32 (carry is 0 or 1, so carry << 32 is 0 or 0x100000000)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), carry) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), hi_of_lo_mul) catch return error.OutOfMemory;

    // --- result_hi = high64(a_lo * b_lo) + (a_lo * b_hi) + (a_hi * b_lo) ---
    // Start with hi_of_lo_mul
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), hi_of_lo_mul) catch return error.OutOfMemory;

    // + a_lo * b_hi (truncating, only lower 64 bits matter)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_lo) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_hi) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;

    // + a_hi * b_lo (truncating, only lower 64 bits matter)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_hi) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_lo) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;

    // Store result_hi
    const result_hi_val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_hi_val) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_hi_val) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const a_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_high) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const b_high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_high) catch return error.OutOfMemory;

    // if (a_high == b_high)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;
    // if (result is i32)
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;

    // Then: compare low words unsigned
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    const low_cmp: u8 = switch (cmp_op) {
        .lt => Op.i64_lt_u,
        .lte => Op.i64_le_u,
        .gt => Op.i64_gt_u,
        .gte => Op.i64_ge_u,
    };
    self.currentCode().append(self.allocator, low_cmp) catch return error.OutOfMemory;

    // Else: compare high words signed
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_high) catch return error.OutOfMemory;
    const high_cmp: u8 = switch (cmp_op) {
        .lt => if (is_signed) Op.i64_lt_s else Op.i64_lt_u,
        .lte => if (is_signed) Op.i64_le_s else Op.i64_le_u,
        .gt => if (is_signed) Op.i64_gt_s else Op.i64_gt_u,
        .gte => if (is_signed) Op.i64_ge_s else Op.i64_ge_u,
    };
    self.currentCode().append(self.allocator, high_cmp) catch return error.OutOfMemory;

    // End if
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Emit i128 bitwise operation (AND, OR, XOR) on both halves.
/// Result is a pointer to 16-byte stack memory.
/// Generate i128/Dec negation: result = -value (two's complement)
fn emitCompositeI128Negate(self: *Self, expr: ProcLocalId, _: layout.Idx) Allocator.Error!void {
    try self.emitProcLocal(expr);
    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;

    try self.emitCompositeI128NegateFromLocal(src_local);
}

fn emitCompositeI128NegateFromLocal(self: *Self, src_local: u32) Allocator.Error!void {
    // Allocate result
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Two's complement: -x = ~x + 1
    // low = ~a_low + 1
    // carry = (low == 0) ? 1 : 0  (overflow when ~a_low was 0xFFFF... i.e. a_low was 0)
    // high = ~a_high + carry

    // Compute ~a_low + 1
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), -1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.i64_xor) catch return error.OutOfMemory;
    // Stack: ~a_low
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    // Stack: result_low = ~a_low + 1

    const result_low_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_low_local) catch return error.OutOfMemory;

    // Store result_low
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_low_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // carry = (result_low == 0) ? 1 : 0
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_low_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;

    // high = ~a_high + carry
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), -1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.currentCode().append(self.allocator, Op.i64_xor) catch return error.OutOfMemory;
    // Stack: [carry, ~a_high]
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    // Stack: [result_high]

    // Store result_high
    const result_high_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_high_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_high_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

/// Emit an i128 bitwise binary op (AND/OR/XOR) by applying `wasm_op` to each
/// 64-bit word independently. Both operands are i32 pointers to 16-byte values.
/// Pushes an i32 pointer to the 16-byte result on the wasm stack.
fn emitI128Bitwise(self: *Self, lhs_local: u32, rhs_local: u32, wasm_op: u8) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Process both 64-bit words: result[word] = lhs[word] OP rhs[word]
    const word_offsets = [_]u32{ 0, 8 };
    for (word_offsets) |word_offset| {
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs_local) catch return error.OutOfMemory;
        try self.emitLoadOp(.i64, word_offset);
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs_local) catch return error.OutOfMemory;
        try self.emitLoadOp(.i64, word_offset);
        self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        try self.emitStoreOp(.i64, word_offset);
    }

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

/// Emit an i128 bitwise NOT (~value) by flipping every bit of each 64-bit word
/// (computed as `word ^ -1`). Pushes an i32 pointer to the 16-byte result.
fn emitCompositeI128BitwiseNot(self: *Self, expr: ProcLocalId) Allocator.Error!void {
    try self.emitProcLocal(expr);
    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;

    const result_offset = try self.allocStackMemory(16, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Process both 64-bit words: result[word] = src[word] ^ -1
    const word_offsets = [_]u32{ 0, 8 };
    for (word_offsets) |word_offset| {
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
        try self.emitLoadOp(.i64, word_offset);
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), -1) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_xor) catch return error.OutOfMemory;
        try self.emitStoreOp(.i64, word_offset);
    }

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

fn emitCompositeI128Abs(self: *Self, expr: ProcLocalId) Allocator.Error!void {
    try self.emitProcLocal(expr);
    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_lt_s) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;
    try self.emitCompositeI128NegateFromLocal(src_local);
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Split a into 32-bit halves: a0 = a & 0xFFFFFFFF, a1 = a >> 32
    const a0 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const a1 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    // a0 = a & 0xFFFFFFFF
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0xFFFFFFFF) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a0) catch return error.OutOfMemory;
    // a1 = a >>> 32 (unsigned shift)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a1) catch return error.OutOfMemory;

    // Split b similarly
    const b0 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    const b1 = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0xFFFFFFFF) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b1) catch return error.OutOfMemory;

    // Compute low = a * b (truncating multiply gives lower 64 bits)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // Compute high word using schoolbook method:
    // t = a0*b0
    // cross = (t >> 32) + a0*b1 + a1*b0  (can overflow, but we only need lower 64 bits + carry)
    // high = a1*b1 + (cross >> 32)

    // t = a0 * b0
    const t = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), t) catch return error.OutOfMemory;

    // cross1 = (t >> 32) + a1*b0
    const cross = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), t) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;

    // cross2 = cross1 + a0*b1 (can carry past 64 bits — must track carry)
    // Save old cross for overflow detection
    const old_cross = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), old_cross) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;

    // carry = (cross < old_cross) ? 1 : 0  (unsigned overflow detection)
    const carry = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), old_cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_lt_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), carry) catch return error.OutOfMemory;

    // high = a1*b1 + (cross >> 32) + (carry << 32)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), cross) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    // Add carry << 32
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), carry) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_add) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

fn emitI64MulToI128Signed(self: *Self, a_local: u32, b_local: u32) Allocator.Error!void {
    const is_neg = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_lt_s) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), is_neg) catch return error.OutOfMemory;

    const abs_val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), is_neg) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(WasmModule.BlockType.i64)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), abs_val) catch return error.OutOfMemory;

    try self.emitI64MulToI128(abs_val, b_local);
    const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_ptr) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), is_neg) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;
    try self.emitCompositeI128NegateFromLocal(result_ptr);
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_ptr) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Save the i64 value from the stack
    const val_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val_local) catch return error.OutOfMemory;

    // Store low word
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    // Store high word
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    if (signed) {
        // Sign extend: high = value >> 63 (arithmetic shift)
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 63) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
    } else {
        // Zero extend: high = 0
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    }
    try self.emitStoreOp(.i64, 8);

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

/// Convert an in-range f64 value to i128/u128 bits and store it at result_local.
fn emitF64ToI128(self: *Self, val_local: u32, result_local: u32, signed: bool, mem_offset: u32) Allocator.Error!void {
    const magnitude = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    var is_neg: u32 = undefined;
    if (signed) {
        is_neg = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(val_local);
        self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
        self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 0.0)))) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.f64_lt) catch return error.OutOfMemory;
        try self.emitLocalSet(is_neg);

        try self.emitLocalGet(val_local);
        self.currentCode().append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
        try self.emitLocalSet(magnitude);
    } else {
        try self.emitLocalGet(val_local);
        try self.emitLocalSet(magnitude);
    }

    // high = trunc(abs(val) / 2^64)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), magnitude) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.f64_div) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.f64_trunc) catch return error.OutOfMemory;
    const high_f = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high_f) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_trunc_f64_u) catch return error.OutOfMemory;
    const high_i = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high_i) catch return error.OutOfMemory;
    // low = (abs(val) - high_f * 2^64) as u64
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), magnitude) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high_f) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.f64_sub) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_trunc_f64_u) catch return error.OutOfMemory;
    const low_i = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low_i) catch return error.OutOfMemory;

    if (signed) {
        try self.emitLocalGet(is_neg);
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        const borrow = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(low_i);
        self.currentCode().append(self.allocator, Op.i64_eqz) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        try self.emitLocalSet(borrow);

        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalGet(low_i);
        self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
        try self.emitLocalSet(low_i);

        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalGet(high_i);
        self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
        try self.emitLocalGet(borrow);
        self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
        try self.emitLocalSet(high_i);

        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    // Store low and high words
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low_i) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, mem_offset);
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high_i) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, mem_offset + 8);
}

const DecIntTryInfo = struct {
    val_size: u32,
    signed: bool,
    min: i64,
    max_exclusive: i64,
};

fn decIntTryInfo(op: LIR.LowLevel) DecIntTryInfo {
    return switch (op) {
        .dec_to_i8_try_unsafe => .{ .val_size = 1, .signed = true, .min = -128, .max_exclusive = 128 },
        .dec_to_u8_try_unsafe => .{ .val_size = 1, .signed = false, .min = 0, .max_exclusive = 256 },
        .dec_to_i16_try_unsafe => .{ .val_size = 2, .signed = true, .min = -32768, .max_exclusive = 32768 },
        .dec_to_u16_try_unsafe => .{ .val_size = 2, .signed = false, .min = 0, .max_exclusive = 65536 },
        .dec_to_i32_try_unsafe => .{ .val_size = 4, .signed = true, .min = -2147483648, .max_exclusive = 2147483648 },
        .dec_to_u32_try_unsafe => .{ .val_size = 4, .signed = false, .min = 0, .max_exclusive = 4294967296 },
        .dec_to_i64_try_unsafe => .{ .val_size = 8, .signed = true, .min = std.math.minInt(i64), .max_exclusive = 0 },
        .dec_to_u64_try_unsafe => .{ .val_size = 8, .signed = false, .min = 0, .max_exclusive = 0 },
        else => unreachable,
    };
}

fn emitDecToIntTryUnsafe(self: *Self, op: LIR.LowLevel, ret_layout: layout.Idx, arg: ProcLocalId) Allocator.Error!void {
    const offsets = self.tryUnsafeOffsets(ret_layout);
    const info = decIntTryInfo(op);

    try self.emitProcLocal(arg);
    const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(src);

    const total_size: u32 = if (info.val_size == 8) 16 else 8;
    const alignment: u32 = if (info.val_size == 8) 8 else 4;
    const result_offset = try self.allocStackMemory(total_size, alignment);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_local);

    const int_offset = try self.allocStackMemory(16, 8);
    const int_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(int_offset);
    try self.emitLocalSet(int_local);

    try self.emitLocalGet(src);
    try self.emitLocalGet(int_local);
    try self.emitCall((if (info.signed) self.dec_to_i128_import else self.dec_to_u128_import) orelse unreachable);
    const host_success = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(host_success);

    try self.emitLocalGet(int_local);
    try self.emitLoadOp(.i64, 0);
    const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(low);

    try self.emitLocalGet(int_local);
    try self.emitLoadOp(.i64, 8);
    const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(high);

    const in_range = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    if (info.signed) {
        try self.emitLocalGet(high);
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), -1) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;

        try self.emitLocalGet(high);
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;

        try self.emitLocalGet(low);
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_lt_s) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.select) catch return error.OutOfMemory;

        if (info.val_size < 8) {
            try self.emitLocalGet(low);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), info.min) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

            try self.emitLocalGet(low);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), info.max_exclusive) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_lt_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
    } else {
        try self.emitLocalGet(high);
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;

        if (info.val_size < 8) {
            try self.emitLocalGet(low);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), info.max_exclusive) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_lt_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
    }
    try self.emitLocalSet(in_range);

    try self.emitLocalGet(host_success);
    try self.emitLocalGet(in_range);
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    const success = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(success);

    try self.emitLocalGet(result_local);
    try self.emitLocalGet(success);
    try self.emitStoreOpSized(.i32, 1, offsets.success);

    try self.emitLocalGet(success);
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitLocalGet(result_local);
    try self.emitLocalGet(low);
    if (info.val_size == 8) {
        try self.emitStoreOp(.i64, offsets.value);
    } else {
        self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        try self.emitStoreOpSized(.i32, info.val_size, offsets.value);
    }
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    try self.emitLocalGet(result_local);
}

/// Emit float-to-int try_unsafe conversion.
/// Returns a record { success: U8, val_or_memory_garbage: IntType } in stack memory.
/// The f64 value should already be on the wasm stack.
/// val_size is the byte size of the target integer (1, 2, 4, or 8).
/// min_f is inclusive; max_f is an exact exclusive power-of-two bound.
fn emitFloatToIntTryUnsafe(self: *Self, ret_layout: layout.Idx, val_size: u32, is_i64: bool, target_signed: bool, min_f: f64, max_f: f64) Allocator.Error!void {
    const offsets = self.tryUnsafeOffsets(ret_layout);
    const total_size = if (is_i64) @as(u32, 16) else @as(u32, 8);
    const alignment: u32 = if (is_i64) 8 else 4;
    const result_offset = try self.allocStackMemory(total_size, alignment);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Save and truncate the f64 value.
    const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;

    const truncated = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    try self.emitLocalGet(val);
    self.currentCode().append(self.allocator, Op.f64_trunc) catch return error.OutOfMemory;
    try self.emitLocalSet(truncated);

    // success = trunc(val) >= min_f && trunc(val) < max_f. NaN comparisons are false.
    try self.emitLocalGet(truncated);
    self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(min_f))) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.f64_ge) catch return error.OutOfMemory;
    try self.emitLocalGet(truncated);
    self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(max_f))) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.f64_lt) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    const success = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), success) catch return error.OutOfMemory;

    // Store success before the payload; only convert/store payload when success is true.
    try self.emitLocalGet(result_local);
    try self.emitLocalGet(success);
    try self.emitStoreOpSized(.i32, 1, offsets.success);

    try self.emitLocalGet(success);
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(result_local);
    try self.emitLocalGet(truncated);
    if (is_i64) {
        self.currentCode().append(self.allocator, if (target_signed) Op.i64_trunc_f64_s else Op.i64_trunc_f64_u) catch return error.OutOfMemory;
        try self.emitStoreOp(.i64, offsets.value);
    } else {
        self.currentCode().append(self.allocator, if (target_signed) Op.i32_trunc_f64_s else Op.i32_trunc_f64_u) catch return error.OutOfMemory;
        try self.emitStoreOpSized(.i32, val_size, offsets.value);
    }
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

/// Emit f64→i128/u128 try_unsafe conversion.
/// Returns { success: U8, val_or_memory_garbage: I128/U128 }.
/// f64 value is on the wasm stack. signed=true for i128, false for u128.
fn emitFloatToI128TryUnsafe(self: *Self, ret_layout: layout.Idx, signed: bool) Allocator.Error!void {
    const offsets = self.tryUnsafeOffsets(ret_layout);
    const result_offset = try self.allocStackMemory(24, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Save and truncate the f64 value.
    const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;

    const truncated = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    try self.emitLocalGet(val);
    self.currentCode().append(self.allocator, Op.f64_trunc) catch return error.OutOfMemory;
    try self.emitLocalSet(truncated);

    // success = trunc(val) within exact integer bounds. NaN comparisons are false.
    if (signed) {
        const min_f: f64 = -170141183460469231731687303715884105728.0;
        const max_f: f64 = 170141183460469231731687303715884105728.0;
        try self.emitLocalGet(truncated);
        self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
        self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(min_f))) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.f64_ge) catch return error.OutOfMemory;
        try self.emitLocalGet(truncated);
        self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
        self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(max_f))) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.f64_lt) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    } else {
        const max_f: f64 = 340282366920938463463374607431768211456.0;
        try self.emitLocalGet(truncated);
        self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
        self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 0.0)))) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.f64_ge) catch return error.OutOfMemory;
        try self.emitLocalGet(truncated);
        self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
        self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(max_f))) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.f64_lt) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    }
    const success = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), success) catch return error.OutOfMemory;

    try self.emitLocalGet(result_local);
    try self.emitLocalGet(success);
    try self.emitStoreOpSized(.i32, 1, offsets.success);

    try self.emitLocalGet(success);
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    try self.emitF64ToI128(truncated, result_local, signed, offsets.value);
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val_local) catch return error.OutOfMemory;

    // Allocate result
    const total_size = disc_offset + 1;
    const alignment: u32 = if (payload_size >= 8) 8 else if (payload_size >= 4) 4 else if (payload_size >= 2) 2 else 1;
    const padded = (total_size + alignment - 1) & ~(alignment - 1);
    const result_offset = try self.allocStackMemory(padded, alignment);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Zero out discriminant (Err by default)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val_local) catch return error.OutOfMemory;
    if (src_vt == .i64 and payload_size <= 4) {
        self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        try self.emitStoreOpSized(.i32, payload_size, 0);
    } else if (src_vt == .i32 and payload_size < 4) {
        try self.emitStoreOpSized(.i32, payload_size, 0);
    } else if (payload_size == 8) {
        try self.emitStoreOp(.i64, 0);
    } else {
        try self.emitStoreOp(.i32, 0);
    }

    // Set discriminant to 1 (Ok)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr) catch return error.OutOfMemory;

    // Load low i64 from [src_ptr + 0]
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;

    // Load high i64 from [src_ptr + 8]
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;

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
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Zero discriminant (Err by default)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, disc_offset);

    // Build the range check condition
    // For unsigned target: high must be 0, AND low must be <= max_unsigned
    // For signed target from unsigned source: high must be 0, AND low must be <= max_signed (as unsigned)
    // For signed target from signed source: high must be sign-extension of low's upper bits
    if (!signed_target) {
        // Unsigned target: high == 0
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_eqz) catch return error.OutOfMemory;

        if (target_bytes < 8) {
            // AND low <= max_unsigned_target
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            const max_val: i64 = (@as(i64, 1) << @intCast(target_bytes * 8)) - 1;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), max_val) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        // For target_bytes == 8: just high == 0 is sufficient
    } else if (!signed_source) {
        // Signed target from unsigned source: high == 0 AND low <= max_signed_target
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_eqz) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        const max_signed: i64 = (@as(i64, 1) << @intCast(target_bytes * 8 - 1)) - 1;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), max_signed) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;
            // Shift left by (64 - bit_count), then arithmetic shift right by (64 - bit_count)
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), @intCast(64 - bit_count)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), @intCast(64 - bit_count)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), sign_ext_low) catch return error.OutOfMemory;

            // Check: sign_ext_low == low
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), sign_ext_low) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;

            // AND high == (sign_ext_low >> 63)  (sign extension of the sign-extended low)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), sign_ext_low) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 63) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        } else {
            // i128 → i64: high must equal (low >> 63) (sign extension)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 63) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;
        }
    }

    // If condition is true, store Ok result
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // Store payload (truncated value)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;
    if (target_bytes <= 4) {
        self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        try self.emitStoreOpSized(.i32, target_bytes, 0);
    } else {
        try self.emitStoreOp(.i64, 0);
    }

    // Set discriminant = 1 (Ok)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, disc_offset);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Push result pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

/// Emit i128 → u128 try conversion (or signed widening → u128 try).
/// Source is an i32 pointer to 16 bytes. Check high word sign bit is 0.
/// Result is a Result(U128, {}) — 16-byte payload at offset 0, disc at offset 16.
fn emitI128TryToU128(self: *Self, _: bool) Allocator.Error!void {
    const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr) catch return error.OutOfMemory;

    // Load high word
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;

    // Load low word
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;

    // Allocate result: 16 bytes payload + 1 byte disc, aligned to 8
    const result_offset = try self.allocStackMemory(24, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Zero discriminant at offset 16
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 16);

    // Check: high >= 0 (sign bit not set)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // Store payload (copy both words)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Set disc = 1 (Ok)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 16);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

/// Emit u128 → i128 try conversion.
/// Source is an i32 pointer to 16 bytes. Check value < 2^127 (high bit not set).
fn emitI128TryToI128(self: *Self) Allocator.Error!void {
    const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr) catch return error.OutOfMemory;

    // Load high word
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 8);
    const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;

    // Load low word
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr) catch return error.OutOfMemory;
    try self.emitLoadOp(.i64, 0);
    const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;

    // Allocate result: 16 bytes payload + 1 byte disc, aligned to 8
    const result_offset = try self.allocStackMemory(24, 8);
    const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

    // Zero discriminant
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 16);

    // Check: high >= 0 (MSB not set, value < 2^127)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // Store payload
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    // Set disc = 1 (Ok)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 1, 16);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
}

/// Resolve a layout.Idx to its wasm ValType, using the layout store for dynamic indices.
/// This is the safe way to map layout indices that might be dynamically allocated
/// (not one of the well-known sentinel values like .bool, .i32, etc.).
/// If a lambda's body returns an unwrapped_capture closure, get the capture's layout.
fn resolveValType(self: *const Self, layout_idx: layout.Idx) Allocator.Error!ValType {
    return try WasmLayout.resultValTypeWithStore(layout_idx, self.getLayoutStore());
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

/// Emit heap allocation via roc_alloc (erased call through RocOps).
/// `size_local` holds the size to allocate; `alignment` is the byte alignment.
/// Leaves the raw allocated pointer on the wasm stack. Roc refcounted data
/// allocations must use emitHeapAllocWithRefcount instead.
fn emitHeapAlloc(self: *Self, size_local: u32, alignment: u32) Allocator.Error!void {
    if (self.runtime_symbol_targets) |targets| {
        // Symbol ABI: roc_alloc(length, alignment) -> ptr, called directly.
        try self.emitLocalGet(size_local);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(alignment)) catch return error.OutOfMemory;
        try self.currentBody().emitRelocatableCall(self.allocator, targets.alloc.symbol, targets.alloc.func_idx);
        return;
    }

    // roc_alloc(*RocOps, length, alignment) -> ptr
    // Push the RocOps pointer, length, and alignment as direct wasm values.
    try self.emitLocalGet(self.roc_ops_local);

    try self.emitLocalGet(size_local);

    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(alignment)) catch return error.OutOfMemory;

    // Load roc_alloc table index from the RocOps struct (offset 4)
    try self.emitLocalGet(self.roc_ops_local);
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), wasm_roc_ops_alloc_offset) catch return error.OutOfMemory;

    // call_indirect leaves the returned pointer on the wasm stack.
    try self.emitCallIndirect(self.roc_alloc_type_idx);
}

/// Emit heap allocation for Roc refcounted data.
/// Leaves the Roc data pointer on the wasm stack, not the raw allocation pointer.
fn emitHeapAllocWithRefcount(self: *Self, data_size_local: u32, element_alignment: u32, elements_refcounted: bool) Allocator.Error!void {
    const rc_layout = rcAllocationLayout(element_alignment, elements_refcounted);

    const total_size_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(data_size_local);
    try self.emitI32Const(@intCast(rc_layout.data_offset));
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size_local);

    try self.emitHeapAlloc(total_size_local, rc_layout.allocation_alignment);
    const raw_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(raw_ptr_local);

    try self.emitLocalGet(raw_ptr_local);
    if (rc_layout.data_offset > 4) {
        try self.emitI32Const(@intCast(rc_layout.data_offset - 4));
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    }
    try self.emitI32Const(1);
    try self.emitStoreOp(.i32, 0);

    try self.emitLocalGet(raw_ptr_local);
    try self.emitI32Const(@intCast(rc_layout.data_offset));
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
}

/// Emit heap allocation for Roc refcounted data with a constant data size.
fn emitHeapAllocWithRefcountConst(self: *Self, data_size: u32, element_alignment: u32, elements_refcounted: bool) Allocator.Error!void {
    const data_size_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitI32Const(@intCast(data_size));
    try self.emitLocalSet(data_size_local);
    try self.emitHeapAllocWithRefcount(data_size_local, element_alignment, elements_refcounted);
}

/// Emit i32.store to a func_body buffer: stores a local's value at memory offset 0 + field_offset.
/// Used during main() prologue to build the RocOps struct.
fn emitI32StoreToBody(allocator: Allocator, func_body: *std.ArrayList(u8), field_offset: u32, local_idx: u32) Allocator.Error!void {
    // i32.const 0  (base address of RocOps struct)
    func_body.append(allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(allocator, func_body, 0) catch return error.OutOfMemory;
    // local.get $local_idx
    func_body.append(allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(allocator, func_body, local_idx) catch return error.OutOfMemory;
    // i32.store offset=field_offset
    func_body.append(allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(allocator, func_body, 2) catch return error.OutOfMemory; // alignment log2(4)
    WasmModule.leb128WriteU32(allocator, func_body, field_offset) catch return error.OutOfMemory;
}

/// Emit i32.store to a func_body buffer: stores a constant value at memory offset 0 + field_offset.
/// Used during main() prologue to build the RocOps struct.
fn emitI32StoreConstToBody(allocator: Allocator, func_body: *std.ArrayList(u8), field_offset: u32, value: u32) Allocator.Error!void {
    // i32.const 0  (base address of RocOps struct)
    func_body.append(allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(allocator, func_body, 0) catch return error.OutOfMemory;
    // i32.const value
    func_body.append(allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(allocator, func_body, @intCast(value)) catch return error.OutOfMemory;
    // i32.store offset=field_offset
    func_body.append(allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(allocator, func_body, 2) catch return error.OutOfMemory; // alignment log2(4)
    WasmModule.leb128WriteU32(allocator, func_body, field_offset) catch return error.OutOfMemory;
}

/// Emit: local.get $fp; i32.const offset; i32.add
/// Leaves (fp + offset) on the stack as an i32 pointer.
fn emitFpOffset(self: *Self, offset: u32) Allocator.Error!void {
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), self.fp_local) catch return error.OutOfMemory;
    if (offset > 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(offset)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
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
        self.currentCode().append(self.allocator, opcode) catch return error.OutOfMemory;
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
    const key: u32 = @intFromEnum(proc_id);

    if (proc.abi == .erased_callable) {
        const type_idx = try self.internFuncType(&.{ .i32, .i32, .i32, .i32 }, &.{});
        const defined = self.module.addDefinedFunction(type_idx) catch return error.OutOfMemory;
        const func_idx = defined.function.raw();
        _ = try self.addOwnedLocalFunctionSymbol(defined, "roc_erased_proc", key);
        const table_idx = self.module.addTableElement(func_idx) catch return error.OutOfMemory;

        self.registered_procs.put(key, func_idx) catch return error.OutOfMemory;
        self.proc_table_indices.put(key, table_idx) catch return error.OutOfMemory;
        return;
    }

    // Build parameter types: under the symbol ABI procs carry no RocOps;
    // the playground/eval module flavor threads the linear-memory RocOps
    // pointer first.
    const args = self.store.getLocalSpan(proc.args);
    var param_types: std.ArrayList(ValType) = .empty;
    defer param_types.deinit(self.allocator);

    if (!self.symbol_abi) {
        param_types.append(self.allocator, .i32) catch return error.OutOfMemory;
    }
    for (args) |arg| {
        const vt = try self.resolveValType(self.store.getLocal(arg).layout_idx);
        param_types.append(self.allocator, vt) catch return error.OutOfMemory;
    }

    const ret_vt = try self.resolveValType(proc.ret_layout);
    const type_idx = try self.internFuncType(param_types.items, &.{ret_vt});
    const defined = self.module.addDefinedFunction(type_idx) catch return error.OutOfMemory;
    const func_idx = defined.function.raw();
    _ = try self.addOwnedLocalFunctionSymbol(defined, "roc_proc", key);
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
    const ret_vt = try self.resolveValType(proc.ret_layout);

    // Save current codegen state
    const saved = self.saveState() catch return error.OutOfMemory;

    // Initialize fresh state with ALL registered proc_specs (for mutual recursion)
    try self.beginFunction(self.localFunctionIndexFromGlobal(func_idx));
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

    const symbol_abi_proc = self.symbol_abi and proc.abi != .erased_callable;
    if (!symbol_abi_proc) {
        // Local 0 = roc_ops_ptr parameter.
        self.roc_ops_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    }

    const erased_ret_ptr_local: ?u32 = if (proc.abi == .erased_callable) blk: {
        const ret_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        const args_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        const capture_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.bindErasedCallableAdapterParams(args, args_ptr, capture_ptr);
        break :blk ret_ptr;
    } else blk: {
        // Bind parameters to locals (after roc_ops_ptr when present).
        for (args) |arg| {
            const local = self.store.getLocal(arg);
            const vt = try self.resolveValType(local.layout_idx);
            _ = self.storage.allocLocal(arg, vt) catch return error.OutOfMemory;
        }
        if (symbol_abi_proc) {
            // Symbol-ABI procs receive no RocOps. Builtins helper signatures
            // still carry an ops slot, which their extern flavor ignores;
            // feed those calls a null.
            self.roc_ops_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), self.roc_ops_local) catch return error.OutOfMemory;
        }
        break :blk null;
    };

    if (proc.hosted == null) {
        try self.prebindProcLocals(proc);
    }

    // Pre-allocate frame pointer local (after params, so it doesn't conflict).
    // Erased-callable adapter parameter unpacking may already have allocated stack
    // memory for composite arguments, which creates the frame-pointer local there.
    if (!self.uses_stack_memory) {
        self.fp_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    }
    self.proc_return_local = self.storage.allocAnonymousLocal(ret_vt) catch return error.OutOfMemory;

    if (proc.hosted) |hosted| {
        if (builtin.mode == .Debug and proc.body != null) {
            std.debug.panic(
                "WASM/codegen invariant violated: hosted proc {d} unexpectedly carried a statement body",
                .{proc.name.raw()},
            );
        }
        try self.generateHostedProcWrapper(hosted, args, proc.ret_layout);
    } else {
        // Emit proc body block (ret branches to this block after storing the return local)
        self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        self.cf_depth = 1; // inside the ret block

        self.generateCFStmt(requireProcBody(proc)) catch |err| {
            self.endFunction();
            self.restoreState(saved);
            return err;
        };

        // End of ret block
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

        if (proc.abi == .erased_callable) {
            try self.emitErasedCallableAdapterReturnStore(erased_ret_ptr_local.?, proc.ret_layout);
        }
    }

    // Locals declaration (beyond function parameters).
    const param_count: u32 = if (proc.abi == .erased_callable)
        4
    else if (symbol_abi_proc)
        @intCast(args.len)
    else
        @intCast(1 + args.len);
    try self.encodeLocalsDecl(&self.currentBody().preamble, param_count);

    // Prologue (if stack memory used)
    if (self.uses_stack_memory) {
        var prefix_buffer: [max_stack_prefix_bytes]u8 = undefined;
        var prefix = StackPrefixBytes.init(prefix_buffer[0..]);
        const prefix_allocator = prefix.allocator();
        var prefix_relocs: StackPrefixRelocs = .{};

        // global.get $__stack_pointer
        try self.appendStackPointerGlobalTo(prefix_allocator, &prefix.bytes, &prefix_relocs, Op.global_get);
        // i32.const frame_size
        prefix.bytes.append(prefix_allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(prefix_allocator, &prefix.bytes, @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        // i32.sub
        prefix.bytes.append(prefix_allocator, Op.i32_sub) catch return error.OutOfMemory;
        // local.tee $fp
        prefix.bytes.append(prefix_allocator, Op.local_tee) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(prefix_allocator, &prefix.bytes, self.fp_local) catch return error.OutOfMemory;
        // global.set $__stack_pointer
        try self.appendStackPointerGlobalTo(prefix_allocator, &prefix.bytes, &prefix_relocs, Op.global_set);
        try self.prependStackPrefix(prefix.bytes.items, prefix_relocs.items());

        // Epilogue: restore stack pointer
        // local.get $fp
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), self.fp_local) catch return error.OutOfMemory;
        // i32.const frame_size
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(self.stack_frame_size)) catch return error.OutOfMemory;
        // i32.add
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        // global.set $__stack_pointer
        try self.emitStackPointerGlobal(Op.global_set);
    }

    if (proc.abi != .erased_callable) {
        // Push the stored proc return value as the function result.
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), self.proc_return_local) catch return error.OutOfMemory;
    }

    // End opcode
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.endFunction();

    // Propagate stack memory usage to outer scope
    const proc_used_stack_memory = self.uses_stack_memory;

    // Restore state
    self.restoreState(saved);

    // If the proc used stack memory, the outer scope needs to know
    if (proc_used_stack_memory) self.uses_stack_memory = true;
}

fn requireProcBody(proc: LirProcSpec) LIR.CFStmtId {
    return proc.body orelse wasmInvariantFmt(
        "WASM/codegen invariant violated: non-hosted proc {d} missing statement body",
        .{proc.name.raw()},
    );
}

const HostedArgOrder = struct {
    index: usize,
    alignment: u32,
    size: u32,
};

fn computeHostedArgOffsets(self: *Self, arg_layouts: []const layout.Idx, offsets: []u32) Allocator.Error!u32 {
    std.debug.assert(arg_layouts.len == offsets.len);

    var ordered = try self.allocator.alloc(HostedArgOrder, arg_layouts.len);
    defer self.allocator.free(ordered);

    for (arg_layouts, 0..) |arg_layout, i| {
        const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
        const size_align = self.getLayoutStore().layoutSizeAlign(self.getLayoutStore().getLayout(runtime_layout));
        ordered[i] = .{
            .index = i,
            .alignment = @intCast(@max(size_align.alignment.toByteUnits(), 1)),
            .size = size_align.size,
        };
    }

    const SortCtx = struct {
        fn lessThan(_: void, lhs: HostedArgOrder, rhs: HostedArgOrder) bool {
            if (lhs.alignment != rhs.alignment) return lhs.alignment > rhs.alignment;
            return lhs.index < rhs.index;
        }
    };
    std.mem.sort(HostedArgOrder, ordered, {}, SortCtx.lessThan);

    var total: u32 = 0;
    for (ordered) |arg| {
        total = std.mem.alignForward(u32, total, arg.alignment);
        offsets[arg.index] = total;
        total += arg.size;
    }
    return total;
}

fn copyProcLocalToHostedArgs(
    self: *Self,
    arg: ProcLocalId,
    arg_layout: layout.Idx,
    args_base_local: u32,
    offset: u32,
) Allocator.Error!void {
    const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
    const size = try self.layoutStorageByteSize(runtime_layout);
    if (size == 0) return;

    if (try self.isCompositeLayout(arg_layout)) {
        try self.emitProcLocal(arg);
        const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(src_local);
        try self.emitMemCopy(args_base_local, offset, src_local, size);
    } else {
        try self.emitProcLocal(arg);
        try self.emitStoreToMemSized(args_base_local, offset, try self.resolveValType(arg_layout), size);
    }
}

/// Map a single ABI register piece to the wasm value type it travels in.
fn pieceValType(piece: layout.abi.RegPiece) ValType {
    return switch (piece.class) {
        .integer => if (piece.size <= 4) .i32 else .i64,
        .float => if (piece.size <= 4) .f32 else .f64,
    };
}

/// A hosted platform function resolved to its linker symbol: either a
/// function defined in an already-merged host module, or an undefined
/// import to be satisfied at link time.
const HostedSymbolTarget = struct {
    func_idx: u32,
    symbol: SymbolIndex,
};

/// The fixed runtime symbols every symbol-ABI host defines.
const RuntimeSymbolTargets = struct {
    alloc: HostedSymbolTarget,
    dealloc: HostedSymbolTarget,
    dbg: HostedSymbolTarget,
    expect_failed: HostedSymbolTarget,
    crashed: HostedSymbolTarget,
};

/// Mark this codegen as emitting symbol-ABI output. Must be called before
/// registerHostedSymbolTargets.
pub fn configureSymbolAbi(self: *Self) void {
    self.symbol_abi = true;
}

/// Resolve one runtime/hosted symbol to a function in the module, importing
/// it when undefined. Must run before any defined function is added.
fn resolveSymbolTarget(self: *Self, symbol_text: []const u8, params: []const ValType, results: []const ValType) Allocator.Error!HostedSymbolTarget {
    if (self.module.linking.findSymbolByName(symbol_text, self.module.imports.items, self.module.global_imports.items, self.module.table_imports.items)) |sym_idx| {
        const sym = self.module.linking.symbol_table.items[sym_idx];
        if (sym.kind == .function) {
            return .{ .func_idx = sym.index, .symbol = SymbolIndex.fromRaw(sym_idx) };
        }
    }
    const type_idx = try self.internFuncType(params, results);
    const imported = self.module.addFunctionImportWithSymbol("env", symbol_text, type_idx) catch return error.OutOfMemory;
    return .{ .func_idx = imported.function.raw(), .symbol = imported.symbol };
}

/// Resolve every hosted proc's linker symbol before proc compilation.
/// Symbols not yet defined in the module become function imports, which must
/// all exist before any defined function index is assigned (imports precede
/// defined functions in the wasm index space).
pub fn registerHostedSymbolTargets(self: *Self, proc_specs: []const LIR.LirProcSpec) Allocator.Error!void {
    if (self.symbol_abi and self.runtime_symbol_targets == null) {
        self.runtime_symbol_targets = .{
            .alloc = try self.resolveSymbolTarget("roc_alloc", &.{ .i32, .i32 }, &.{.i32}),
            .dealloc = try self.resolveSymbolTarget("roc_dealloc", &.{ .i32, .i32 }, &.{}),
            .dbg = try self.resolveSymbolTarget("roc_dbg", &.{ .i32, .i32 }, &.{}),
            .expect_failed = try self.resolveSymbolTarget("roc_expect_failed", &.{ .i32, .i32 }, &.{}),
            .crashed = try self.resolveSymbolTarget("roc_crashed", &.{ .i32, .i32 }, &.{}),
        };
    }
    for (proc_specs) |spec| {
        const hosted = spec.hosted orelse continue;
        if (self.hosted_symbol_targets.contains(hosted.dispatch_index)) continue;

        const symbol_text = self.store.getString(hosted.symbol);

        if (self.module.linking.findSymbolByName(symbol_text, self.module.imports.items, self.module.global_imports.items, self.module.table_imports.items)) |sym_idx| {
            const sym = self.module.linking.symbol_table.items[sym_idx];
            if (sym.kind == .function) {
                try self.hosted_symbol_targets.put(hosted.dispatch_index, .{
                    .func_idx = sym.index,
                    .symbol = SymbolIndex.fromRaw(sym_idx),
                });
                continue;
            }
        }

        // Build the natural C-ABI wasm type for the import declaration.
        const param_locals = self.store.getLocalSpan(spec.args);
        const hosted_arg_layouts = try self.allocator.alloc(layout.Idx, param_locals.len);
        defer self.allocator.free(hosted_arg_layouts);
        for (param_locals, 0..) |param, i| {
            hosted_arg_layouts[i] = self.procLocalLayoutIdx(param);
        }

        var arena_state = std.heap.ArenaAllocator.init(self.allocator);
        defer arena_state.deinit();
        const lowered = layout.abi.lower(arena_state.allocator(), self.getLayoutStore(), .wasm32, hosted_arg_layouts, spec.ret_layout, false) catch return error.OutOfMemory;

        var params = std.ArrayList(ValType).empty;
        defer params.deinit(self.allocator);
        var results = std.ArrayList(ValType).empty;
        defer results.deinit(self.allocator);

        if (lowered.ret == .indirect) try params.append(self.allocator, .i32);
        for (lowered.args) |placement| {
            switch (placement) {
                .none => {},
                .indirect => try params.append(self.allocator, .i32),
                .registers => |pieces| {
                    for (pieces) |piece| try params.append(self.allocator, pieceValType(piece));
                },
            }
        }
        switch (lowered.ret) {
            .none, .indirect => {},
            .registers => |pieces| {
                for (pieces) |piece| try results.append(self.allocator, pieceValType(piece));
            },
        }

        const type_idx = try self.internFuncType(params.items, results.items);
        const imported = self.module.addFunctionImportWithSymbol("env", symbol_text, type_idx) catch return error.OutOfMemory;
        try self.hosted_symbol_targets.put(hosted.dispatch_index, .{
            .func_idx = imported.function.raw(),
            .symbol = imported.symbol,
        });
    }
}

fn emitHostedCall(
    self: *Self,
    hosted: LIR.HostedProc,
    args: []const ProcLocalId,
    arg_layouts: []const layout.Idx,
    ret_layout: layout.Idx,
) Allocator.Error!void {
    std.debug.assert(args.len == arg_layouts.len);

    const runtime_ret_layout = self.runtimeRepresentationLayoutIdx(ret_layout);
    const ret_size = try self.layoutStorageByteSize(runtime_ret_layout);
    const ret_align = try self.layoutStorageByteAlign(runtime_ret_layout);
    const ret_slot = try self.allocStackMemory(@max(ret_size, 4), ret_align);
    const ret_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(ret_slot);
    try self.emitLocalSet(ret_ptr_local);

    const arg_offsets = try self.allocator.alloc(u32, args.len);
    defer self.allocator.free(arg_offsets);
    const args_size = try self.computeHostedArgOffsets(arg_layouts, arg_offsets);
    // Pad by 8 bytes so a register piece loaded as a full 64-bit word from the tail of the
    // slot never reads past it.
    const args_slot = try self.allocStackMemory(@max(args_size, 4) + 8, 4);
    const args_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(args_slot);
    try self.emitLocalSet(args_ptr_local);

    for (args, arg_layouts, 0..) |arg, arg_layout, i| {
        try self.copyProcLocalToHostedArgs(arg, arg_layout, args_ptr_local, arg_offsets[i]);
    }

    // Lower the call to the wasm C ABI (shared classifier, same as the other backends).
    var arena_state = std.heap.ArenaAllocator.init(self.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();
    const lowered = layout.abi.lower(arena, self.getLayoutStore(), .wasm32, arg_layouts, ret_layout, false) catch return error.OutOfMemory;

    // An indirect return is passed as a result pointer (wasm has no sret register).
    if (lowered.ret == .indirect) {
        try self.emitLocalGet(ret_ptr_local);
    }

    for (lowered.args, arg_offsets) |placement, arg_offset| {
        switch (placement) {
            .none => {},
            .indirect => {
                // Pass a pointer to the argument's bytes in the packed args slot.
                try self.emitLocalGet(args_ptr_local);
                if (arg_offset != 0) {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(arg_offset)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
            },
            .registers => |pieces| {
                for (pieces) |piece| {
                    const vt = pieceValType(piece);
                    try self.emitLocalGet(args_ptr_local);
                    try self.emitLoadOpSized(vt, piece.size, arg_offset + piece.offset);
                }
            },
        }
    }

    // The call goes directly to the host's linker symbol, registered before
    // proc compilation by registerHostedSymbolTargets.
    const target = self.hosted_symbol_targets.get(hosted.dispatch_index) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "WASM/codegen invariant violated: hosted dispatch index {d} has no registered symbol target",
                .{hosted.dispatch_index},
            );
        }
        unreachable;
    };
    try self.currentBody().emitRelocatableCall(self.allocator, target.symbol, target.func_idx);

    // Store a register-class return into the return slot. Multiple wasm results sit on the
    // stack with the last result on top, so store them in reverse order.
    switch (lowered.ret) {
        .none, .indirect => {},
        .registers => |pieces| {
            var i: usize = pieces.len;
            while (i > 0) {
                i -= 1;
                const piece = pieces[i];
                try self.emitStoreToMemSized(ret_ptr_local, piece.offset, pieceValType(piece), piece.size);
            }
        },
    }

    if (ret_size == 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    } else if (try self.isCompositeLayout(ret_layout)) {
        try self.emitLocalGet(ret_ptr_local);
    } else {
        try self.emitLocalGet(ret_ptr_local);
        try self.emitLoadOp(try self.resolveValType(ret_layout), 0);
    }
}

fn generateHostedProcWrapper(
    self: *Self,
    hosted: LIR.HostedProc,
    params: []const ProcLocalId,
    ret_layout: layout.Idx,
) Allocator.Error!void {
    const arg_layouts = try self.allocator.alloc(layout.Idx, params.len);
    defer self.allocator.free(arg_layouts);

    for (params, 0..) |param, i| {
        arg_layouts[i] = self.procLocalLayoutIdx(param);
    }

    try self.emitHostedCall(hosted, params, arg_layouts, ret_layout);
    try self.emitLocalSet(self.proc_return_local);
}

fn bindErasedCallableAdapterParams(
    self: *Self,
    args: []const ProcLocalId,
    args_ptr_local: u32,
    capture_ptr_local: u32,
) Allocator.Error!void {
    if (args.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("WASM/codegen invariant violated: erased callable adapter has no hidden capture arg", .{});
        }
        unreachable;
    }

    var arg_offset: u32 = 0;
    const explicit_count = args.len - 1;
    for (args[0..explicit_count]) |arg| {
        const local_layout = self.procLocalLayoutIdx(arg);
        const runtime_layout = self.runtimeRepresentationLayoutIdx(local_layout);
        const size_align = self.getLayoutStore().layoutSizeAlign(self.getLayoutStore().getLayout(runtime_layout));
        const arg_align: u32 = @intCast(@max(size_align.alignment.toByteUnits(), 1));
        arg_offset = std.mem.alignForward(u32, arg_offset, arg_align);

        const vt = try self.resolveValType(local_layout);
        const local_idx = self.storage.allocLocal(arg, vt) catch return error.OutOfMemory;
        if (size_align.size == 0) {
            switch (vt) {
                .i32 => {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                },
                .f32 => {
                    self.currentCode().append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                    try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f32, 0)));
                },
                .f64 => {
                    self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                    try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f64, 0)));
                },
            }
            try self.emitLocalSet(local_idx);
        } else if (try self.isCompositeLayout(local_layout)) {
            const dst_offset = try self.allocStackMemory(size_align.size, arg_align);
            const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(dst_offset);
            try self.emitLocalSet(dst_local);

            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(args_ptr_local);
            if (arg_offset != 0) {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(arg_offset)) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            }
            try self.emitLocalSet(src_local);

            try self.emitMemCopy(dst_local, 0, src_local, size_align.size);
            try self.emitLocalGet(dst_local);
            try self.emitLocalSet(local_idx);
        } else {
            try self.emitLocalGet(args_ptr_local);
            try self.emitLoadOpForLayout(local_layout, arg_offset);
            try self.emitLocalSet(local_idx);
        }

        arg_offset += size_align.size;
    }

    const hidden_capture_arg = args[explicit_count];
    const hidden_local = self.storage.allocLocal(hidden_capture_arg, .i32) catch return error.OutOfMemory;
    try self.emitLocalGet(capture_ptr_local);
    try self.emitLocalSet(hidden_local);
}

fn emitErasedCallableAdapterReturnStore(
    self: *Self,
    ret_ptr_local: u32,
    ret_layout: layout.Idx,
) Allocator.Error!void {
    const runtime_ret_layout = self.runtimeRepresentationLayoutIdx(ret_layout);
    const ret_size = try self.layoutStorageByteSize(runtime_ret_layout);
    if (ret_size == 0) return;

    if (try self.isCompositeLayout(ret_layout)) {
        try self.emitMemCopy(ret_ptr_local, 0, self.proc_return_local, ret_size);
    } else {
        try self.emitLocalGet(self.proc_return_local);
        try self.emitStoreToMemSized(ret_ptr_local, 0, try self.resolveValType(ret_layout), ret_size);
    }
}

/// Saved codegen state for restoring after compiling a nested function.
const SavedState = struct {
    locals: std.AutoHashMap(u64, Storage.LocalInfo),
    next_local_idx: u32,
    local_types_items: []ValType,
    local_types_capacity: usize,
    active_fn_stack_len: usize,
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
        .locals = self.storage.locals,
        .next_local_idx = self.storage.next_local_idx,
        .local_types_items = self.storage.local_types.items,
        .local_types_capacity = self.storage.local_types.capacity,
        .active_fn_stack_len = self.active_fn_stack.items.len,
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
    if (self.active_fn_stack.items.len != saved.active_fn_stack_len) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "WasmCodeGen invariant violated: active function stack len {d}, expected {d}",
                .{ self.active_fn_stack.items.len, saved.active_fn_stack_len },
            );
        }
        unreachable;
    }
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

/// Work item for the explicit (non-recursive) CFStmt walker.
///
/// The CFStmt graph is walked with an explicit heap-backed work stack instead of
/// recursion. `.node` processes one statement (and pushes its successor); the glue
/// variants emit the post-child scaffolding for switch/join nodes after their child
/// subtrees finish, preserving the exact emission order and `cf_depth` bookkeeping
/// of the original recursive walker. `.deactivate` mirrors the Debug re-entry guard:
/// a statement stays "active" until its whole subtree has been emitted.
const StmtWork = union(enum) {
    /// Process one statement, with the active `stop` boundary.
    node: struct { stmt_id: CFStmtId, stop: ?CFStmtId },
    /// Debug-only: remove a statement key from the active set once its subtree is done.
    deactivate: u32,
    /// Switch glue: emit the per-branch `if`/cond test before that branch's body.
    switch_test: struct { state: *SwitchEqState, branch_value: i64 },
    /// Switch glue: emit the `else` after a branch body.
    switch_else: void,
    /// Switch glue: close all branch `if/else` blocks (cf_depth bookkeeping).
    switch_close: *SwitchEqState,
    /// String-pattern glue: emit the `else` between match and miss arms.
    str_match_else: void,
    /// String-pattern glue: close the match/miss `if` block.
    str_match_close: void,
    /// Join glue: emit the `else` between join body and remainder.
    join_else: void,
    /// Join glue: emit the three closing `end`s of a join (cf_depth bookkeeping)
    /// and retire the join's bookkeeping now that its lexical scope has ended.
    join_close: u32,
};

/// Heap-allocated state shared across a switch node's branch continuations.
const SwitchEqState = struct {
    cond_local: u32,
    cond_vt: ValType,
    branch_count: usize,
};

/// Generate code for a control flow statement (used in LirProcSpec bodies).
fn generateCFStmt(self: *Self, stmt_id: CFStmtId) Allocator.Error!void {
    return self.generateCFStmtUntil(stmt_id, null);
}

/// Explicit work-stack driver for the CFStmt walker (stack-safe; no recursion).
fn generateCFStmtUntil(self: *Self, stmt_id: CFStmtId, stop: ?CFStmtId) Allocator.Error!void {
    var sfa = std.heap.stackFallback(64 * @sizeOf(StmtWork), self.allocator);
    const wa = sfa.get();
    var work = std.ArrayList(StmtWork).empty;
    defer work.deinit(wa);
    try work.append(wa, .{ .node = .{ .stmt_id = stmt_id, .stop = stop } });

    while (work.pop()) |item| {
        switch (item) {
            .deactivate => |key| {
                if (builtin.mode == .Debug) {
                    _ = self.active_stmt_generations.remove(key);
                }
            },
            .switch_test => |t| {
                try self.emitLocalGet(t.state.cond_local);
                if (t.state.cond_vt == .i64) {
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), t.branch_value) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;
                } else {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(t.branch_value)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
                }

                self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, 0x40) catch return error.OutOfMemory;
                self.cf_depth += 1;
            },
            .switch_else => {
                self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            },
            .switch_close => |state| {
                for (0..state.branch_count) |_| {
                    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                    self.cf_depth -= 1;
                }
                self.allocator.destroy(state);
            },
            .str_match_else => {
                self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            },
            .str_match_close => {
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.cf_depth -= 1;
            },
            .join_else => {
                self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            },
            .join_close => |jp_key| {
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.cf_depth -= 1;

                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.cf_depth -= 1;
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.cf_depth -= 1;

                // The join's lexical scope has ended: retire its bookkeeping so a
                // later jump can only ever resolve a join that is still in scope,
                // and so the locals/depths (which index only this proc) cannot be
                // read by an unrelated proc. join_point_param_locals owns its array.
                if (self.join_point_param_locals.fetchRemove(jp_key)) |entry| {
                    self.allocator.free(entry.value);
                }
                _ = self.join_point_depths.remove(jp_key);
                _ = self.join_point_state_locals.remove(jp_key);
            },
            .node => |n| try self.generateCFStmtNode(&work, wa, n.stmt_id, n.stop),
        }
    }
}

/// Process a single CFStmt node: emit its own code and push successor/glue work.
fn generateCFStmtNode(self: *Self, work: *std.ArrayList(StmtWork), wa: Allocator, stmt_id: CFStmtId, stop: ?CFStmtId) Allocator.Error!void {
    if (stop) |stop_id| {
        if (stmt_id == stop_id) return;
    }

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
        // This statement stays active until its whole subtree has been emitted.
        try work.append(wa, .{ .deactivate = stmt_key });
    }

    const stmt = self.store.getCFStmt(stmt_id);
    switch (stmt) {
        .assign_ref => |assign| {
            try self.generateRefOp(assign.op, self.procLocalLayoutIdx(assign.target));
            try self.bindAssignedLocal(assign.target);
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .assign_literal => |assign| {
            try self.generateLiteral(assign.value);
            try self.bindAssignedLocal(assign.target);
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .init_uninitialized => |uninit| {
            try work.append(wa, .{ .node = .{ .stmt_id = uninit.next, .stop = stop } });
        },
        .assign_call => |assign| {
            try self.generateCall(.{
                .proc = assign.proc,
                .args = assign.args,
                .ret_layout = self.procLocalLayoutIdx(assign.target),
            });
            try self.bindAssignedLocal(assign.target);
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .assign_call_erased => |assign| {
            try self.generateErasedCall(.{
                .closure = assign.closure,
                .args = assign.args,
                .ret_layout = self.procLocalLayoutIdx(assign.target),
            });
            try self.bindAssignedLocal(assign.target);
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .assign_packed_erased_fn => |assign| {
            try self.generatePackedErasedFn(.{
                .proc = assign.proc,
                .capture = assign.capture,
                .target_layout = self.procLocalLayoutIdx(assign.target),
                .capture_layout = assign.capture_layout,
                .on_drop = assign.on_drop,
            });
            try self.bindAssignedLocal(assign.target);
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .assign_low_level => |assign| {
            try self.generateLowLevel(.{
                .op = assign.op,
                .args = assign.args,
                .ret_layout = self.procLocalLayoutIdx(assign.target),
                .unique_args = assign.unique_args,
                .interchangeable = assign.interchangeable,
            });
            try self.bindAssignedLocal(assign.target);
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .assign_list => |assign| {
            try self.generateList(.{
                .elems = assign.elems,
                .elem_layout = self.listElemLayout(self.procLocalLayoutIdx(assign.target)),
            });
            try self.bindAssignedLocal(assign.target);
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .assign_struct => |assign| {
            try self.generateStruct(.{
                .fields = assign.fields,
                .struct_layout = self.procLocalLayoutIdx(assign.target),
            });
            try self.bindAssignedLocal(assign.target);
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .assign_tag => |assign| {
            try self.generateTag(.{
                .union_layout = self.procLocalLayoutIdx(assign.target),
                .variant_index = assign.variant_index,
                .discriminant = assign.discriminant,
                .payload = assign.payload,
            });
            try self.bindAssignedLocal(assign.target);
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .set_local => |assign| {
            try self.emitProcLocal(assign.value);
            try self.emitLocalSet(try self.getOrAllocTypedLocal(assign.target, try self.procLocalValType(assign.target)));
            try work.append(wa, .{ .node = .{ .stmt_id = assign.next, .stop = stop } });
        },
        .debug => |debug_stmt| {
            try self.emitRocDbg(debug_stmt.message);
            try work.append(wa, .{ .node = .{ .stmt_id = debug_stmt.next, .stop = stop } });
        },
        .expect => |expect_stmt| {
            const condition_vt = try self.procLocalValType(expect_stmt.condition);
            try self.emitProcLocal(expect_stmt.condition);
            switch (condition_vt) {
                .i32 => {},
                .i64 => {
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_ne) catch return error.OutOfMemory;
                },
                .f32, .f64 => wasmInvariantFmt(
                    "WasmCodeGen invariant violated: expect condition local {d} had non-integer value type {s}",
                    .{ @intFromEnum(expect_stmt.condition), @tagName(condition_vt) },
                ),
            }
            self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, 0x40) catch return error.OutOfMemory;
            self.cf_depth += 1;

            try self.emitRocStaticStringCall(wasm_roc_ops_expect_failed_offset, "expect failed");

            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;

            try work.append(wa, .{ .node = .{ .stmt_id = expect_stmt.next, .stop = stop } });
        },
        .ret => |r| {
            try self.emitProcLocal(r.value);
            try self.emitLocalSet(self.proc_return_local);
            self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), self.cf_depth - 1) catch return error.OutOfMemory;
        },
        .switch_stmt => |sw| {
            const cond_vt = try self.procLocalValType(sw.cond);
            const cond_local = self.storage.allocAnonymousLocal(cond_vt) catch return error.OutOfMemory;
            try self.emitProcLocal(sw.cond);
            try self.emitLocalSet(cond_local);
            if (cond_vt == .i32) {
                const cond_layout_idx = self.procLocalLayoutIdx(sw.cond);
                const cond_size = try self.layoutStorageByteSize(cond_layout_idx);
                if (cond_size > 0 and cond_size < 4) {
                    const mask: i32 = (@as(i32, 1) << @intCast(cond_size * 8)) - 1;
                    try self.emitLocalGet(cond_local);
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), mask) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                    try self.emitLocalSet(cond_local);
                }
            }

            const branches = self.store.getCFSwitchBranches(sw.branches);

            const branch_stop: ?CFStmtId = if (sw.continuation) |continuation| continuation else stop;

            const state = try self.allocator.create(SwitchEqState);
            state.* = .{ .cond_local = cond_local, .cond_vt = cond_vt, .branch_count = branches.len };

            // Source emission order is, per branch: [cond test + if] [body subtree]
            // [else]; then [default body subtree]; then close N `end`s; then the
            // optional continuation subtree. Push in reverse so popping reproduces it.
            if (sw.continuation) |continuation| {
                try work.append(wa, .{ .node = .{ .stmt_id = continuation, .stop = stop } });
            }
            try work.append(wa, .{ .switch_close = state });
            try work.append(wa, .{ .node = .{ .stmt_id = sw.default_branch, .stop = branch_stop } });

            var bi: usize = branches.len;
            while (bi > 0) {
                bi -= 1;
                const branch = branches[bi];
                try work.append(wa, .switch_else);
                try work.append(wa, .{ .node = .{ .stmt_id = branch.body, .stop = branch_stop } });
                try work.append(wa, .{ .switch_test = .{ .state = state, .branch_value = @bitCast(branch.value) } });
            }
        },
        .switch_initialized_payload => |sw| {
            const cond_vt = try self.procLocalValType(sw.cond);
            const cond_local = self.storage.allocAnonymousLocal(cond_vt) catch return error.OutOfMemory;
            try self.emitProcLocal(sw.cond);
            switch (cond_vt) {
                .i64 => {
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), @bitCast(sw.cond_mask)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
                },
                .i32 => {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(sw.cond_mask)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                },
                .f32, .f64 => wasmInvariantFmt(
                    "WASM/codegen invariant violated: switch_initialized_payload condition local {d} had non-integer value type {s}",
                    .{ @intFromEnum(sw.cond), @tagName(cond_vt) },
                ),
            }
            try self.emitLocalSet(cond_local);
            const state = try self.allocator.create(SwitchEqState);
            state.* = .{ .cond_local = cond_local, .cond_vt = cond_vt, .branch_count = 1 };
            try work.append(wa, .{ .switch_close = state });
            try work.append(wa, .{ .node = .{ .stmt_id = sw.uninitialized_branch, .stop = stop } });
            try work.append(wa, .switch_else);
            try work.append(wa, .{ .node = .{ .stmt_id = sw.initialized_branch, .stop = stop } });
            try work.append(wa, .{ .switch_test = .{ .state = state, .branch_value = @bitCast(sw.cond_mask) } });
        },
        .str_match => |str_match| {
            const matched_local = try self.generateStrMatch(str_match);
            try self.emitLocalGet(matched_local);
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.cf_depth += 1;

            try work.append(wa, .str_match_close);
            try work.append(wa, .{ .node = .{ .stmt_id = str_match.on_miss, .stop = stop } });
            try work.append(wa, .str_match_else);
            try work.append(wa, .{ .node = .{ .stmt_id = str_match.on_match, .stop = stop } });
        },
        .str_match_set => |str_match_set| {
            const matched_arm = try self.generateStrMatchSet(str_match_set);
            const arms = self.store.getStrMatchArms(str_match_set.arms);
            const state = try self.allocator.create(SwitchEqState);
            state.* = .{ .cond_local = matched_arm, .cond_vt = .i32, .branch_count = arms.len };

            try work.append(wa, .{ .switch_close = state });
            try work.append(wa, .{ .node = .{ .stmt_id = str_match_set.on_miss, .stop = stop } });

            var arm_index = arms.len;
            while (arm_index > 0) {
                arm_index -= 1;
                try work.append(wa, .switch_else);
                try work.append(wa, .{ .node = .{ .stmt_id = arms[arm_index].on_match, .stop = stop } });
                try work.append(wa, .{ .switch_test = .{ .state = state, .branch_value = @intCast(arm_index) } });
            }
        },
        .join => |j| {
            const jp_key = @intFromEnum(j.id);

            const jp_params = self.store.getLocalSpan(j.params);
            var param_locals = self.allocator.alloc(u32, jp_params.len) catch return error.OutOfMemory;

            for (jp_params, 0..) |param, i| {
                const vt = try self.procLocalValType(param);
                const local_idx = self.getOrAllocTypedLocal(param, vt) catch return error.OutOfMemory;
                param_locals[i] = local_idx;
            }
            self.join_point_param_locals.put(jp_key, param_locals) catch return error.OutOfMemory;
            const state_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.join_point_state_locals.put(jp_key, state_local) catch return error.OutOfMemory;

            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitLocalSet(state_local);

            self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, 0x40) catch return error.OutOfMemory;
            self.cf_depth += 1;

            self.currentCode().append(self.allocator, Op.loop_) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, 0x40) catch return error.OutOfMemory; // void block type
            self.cf_depth += 1;

            self.join_point_depths.put(jp_key, self.cf_depth) catch return error.OutOfMemory;

            try self.emitLocalGet(state_local);
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, 0x40) catch return error.OutOfMemory;
            self.cf_depth += 1;

            // Source emission order: [body subtree] [else] [remainder subtree]
            // [three closing ends]. Push in reverse so popping reproduces it.
            try work.append(wa, .{ .join_close = jp_key });
            try work.append(wa, .{ .node = .{ .stmt_id = j.remainder, .stop = stop } });
            try work.append(wa, .join_else);
            try work.append(wa, .{ .node = .{ .stmt_id = j.body, .stop = stop } });
        },
        .jump => |jmp| {
            const jp_key = @intFromEnum(jmp.target);

            const state_local = self.join_point_state_locals.get(jp_key) orelse wasmInvariantFmt(
                "WASM/codegen invariant violated: jump target {d} has no active join-point state",
                .{jp_key},
            );
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            try self.emitLocalSet(state_local);

            const loop_depth = self.join_point_depths.get(jp_key) orelse wasmInvariantFmt(
                "WASM/codegen invariant violated: jump target {d} has no active join-point depth",
                .{jp_key},
            );
            const br_target = self.cf_depth - loop_depth;
            self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), br_target) catch return error.OutOfMemory;
        },
        .incref => |inc| {
            try self.generateRcStmt(inc.value, inc.rc, inc.atomicity, inc.count);
            try work.append(wa, .{ .node = .{ .stmt_id = inc.next, .stop = stop } });
        },
        .decref => |dec| {
            try self.generateRcStmt(dec.value, dec.rc, dec.atomicity, 1);
            try work.append(wa, .{ .node = .{ .stmt_id = dec.next, .stop = stop } });
        },
        .decref_if_initialized => |dec| {
            const cond_vt = try self.procLocalValType(dec.cond);
            try self.emitProcLocal(dec.cond);
            switch (cond_vt) {
                .i64 => {
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), @bitCast(dec.cond_mask)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_and) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), @bitCast(dec.cond_mask)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_eq) catch return error.OutOfMemory;
                },
                .i32 => {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(dec.cond_mask)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(dec.cond_mask)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
                },
                .f32, .f64 => wasmInvariantFmt(
                    "WASM/codegen invariant violated: decref_if_initialized condition local {d} had non-integer value type {s}",
                    .{ @intFromEnum(dec.cond), @tagName(cond_vt) },
                ),
            }
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, 0x40) catch return error.OutOfMemory;
            self.cf_depth += 1;

            try self.generateRcStmt(dec.value, dec.rc, dec.atomicity, 1);

            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.cf_depth -= 1;
            try work.append(wa, .{ .node = .{ .stmt_id = dec.next, .stop = stop } });
        },
        .free => |free_stmt| {
            try self.generateRcStmt(free_stmt.value, free_stmt.rc, free_stmt.atomicity, 1);
            try work.append(wa, .{ .node = .{ .stmt_id = free_stmt.next, .stop = stop } });
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
            self.currentCode().append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },
        .comptime_exhaustiveness_failed => {
            try self.emitRocStaticStringCall(wasm_roc_ops_crashed_offset, "compile-time exhaustiveness failure reached runtime code");
            self.currentCode().append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },
        .comptime_branch_taken => |marker| {
            try work.append(wa, .{ .node = .{ .stmt_id = marker.next, .stop = stop } });
        },
        .crash => |crash| {
            const msg_bytes = self.store.getString(crash.msg);
            try self.emitRocStaticStringCall(wasm_roc_ops_crashed_offset, msg_bytes);

            self.currentCode().append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },
        .expect_err => |expect_err_stmt| {
            try self.emitRocStrCall(expect_err_stmt.message, wasm_roc_ops_crashed_offset);
            self.currentCode().append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },
        .loop_continue => {
            if (builtin.mode == .Debug and self.loop_continue_target_depths.items.len == 0) {
                std.debug.panic(
                    "WasmCodeGen invariant violated: loop_continue encountered outside a loop",
                    .{},
                );
            }
            const loop_depth = self.loop_continue_target_depths.items[self.loop_continue_target_depths.items.len - 1];
            const br_target = self.cf_depth - loop_depth;
            self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), br_target) catch return error.OutOfMemory;
        },
        .loop_break => {
            if (builtin.mode == .Debug and self.loop_break_target_depths.items.len == 0) {
                std.debug.panic(
                    "WasmCodeGen invariant violated: loop_break encountered outside a loop",
                    .{},
                );
            }
            const break_depth = self.loop_break_target_depths.items[self.loop_break_target_depths.items.len - 1];
            const br_target = self.cf_depth - break_depth;
            self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), br_target) catch return error.OutOfMemory;
        },
    }
}

fn generateLiteral(self: *Self, value: LIR.LiteralValue) Allocator.Error!void {
    switch (value) {
        .i64_literal => |lit| {
            switch (try self.resolveValType(lit.layout_idx)) {
                .i32 => {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @truncate(lit.value)) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), lit.value) catch return error.OutOfMemory;
                },
                .f32, .f64 => unreachable,
            }
        },
        .i128_literal => |lit| try self.generateIntLiteralForLayout(lit.value, lit.layout_idx),
        .f64_literal => |lit| {
            self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            var buf: [8]u8 = undefined;
            std.mem.writeInt(u64, &buf, @bitCast(lit), .little);
            try self.currentCode().appendSlice(self.allocator, &buf);
        },
        .f32_literal => |lit| {
            self.currentCode().append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
            var buf: [4]u8 = undefined;
            std.mem.writeInt(u32, &buf, @bitCast(lit), .little);
            try self.currentCode().appendSlice(self.allocator, &buf);
        },
        .dec_literal => |lit| try self.generateI128Literal(lit),
        .str_literal => |str_idx| try self.generateStrLiteral(str_idx),
        .bytes_literal => |bytes_idx| try self.generateBytesLiteral(bytes_idx),
        .null_ptr => {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        },
        .proc_ref => |proc_id| {
            const key: u32 = @intFromEnum(proc_id);
            const table_idx = self.proc_table_indices.get(key) orelse {
                wasmInvariantFmt(
                    "WasmCodeGen invariant violated: proc_ref target {d} missing table index",
                    .{@intFromEnum(proc_id)},
                );
            };
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(table_idx)) catch return error.OutOfMemory;
        },
    }
}

fn generateIntLiteralForLayout(self: *Self, value: i128, layout_idx: layout.Idx) Allocator.Error!void {
    const repr = try WasmLayout.wasmReprWithStore(layout_idx, self.getLayoutStore());
    switch (repr) {
        .primitive => |vt| switch (vt) {
            .i32 => {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @truncate(value)) catch return error.OutOfMemory;
            },
            .i64 => {
                self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, self.currentCode(), @truncate(value)) catch return error.OutOfMemory;
            },
            .f32, .f64 => unreachable,
        },
        .stack_memory => try self.generateI128Literal(value),
    }
}

/// Emit a diagnostic RocOps callback (dbg/expect_failed/crashed) following the C ABI:
/// (*RocOps, bytes_ptr, len) -> (). The bytes pointer and length are sourced from the
/// two i32 fields packed at `args_slot` (field 0 = bytes_ptr, field 4 = len).
fn emitRocOpsCall(self: *Self, args_slot: u32, table_offset: u32) Allocator.Error!void {
    if (self.runtime_symbol_targets) |targets| {
        // Symbol ABI: symbol(bytes, len), called directly.
        const target = switch (table_offset) {
            wasm_roc_ops_dbg_offset => targets.dbg,
            wasm_roc_ops_expect_failed_offset => targets.expect_failed,
            else => targets.crashed,
        };
        try self.emitFpOffset(args_slot);
        try self.emitLoadOp(.i32, 0);
        try self.emitFpOffset(args_slot);
        try self.emitLoadOp(.i32, 4);
        try self.currentBody().emitRelocatableCall(self.allocator, target.symbol, target.func_idx);
        return;
    }

    try self.emitLocalGet(self.roc_ops_local);

    // bytes_ptr from args_slot field 0
    try self.emitFpOffset(args_slot);
    try self.emitLoadOp(.i32, 0);

    // len from args_slot field 4
    try self.emitFpOffset(args_slot);
    try self.emitLoadOp(.i32, 4);

    // Load the callback's table index from the RocOps struct.
    try self.emitLocalGet(self.roc_ops_local);
    try self.emitLoadOp(.i32, table_offset);

    try self.emitCallIndirect(self.roc_diag_type_idx);
}

fn emitRocStaticStringCall(self: *Self, table_offset: u32, msg: []const u8) Allocator.Error!void {
    const segment_name = try self.allocStaticDataName(".rodata.roc_msg");
    const symbol_name = try self.allocStaticDataName("roc.msg");
    const msg_address = try self.addStaticDataSymbol(
        msg,
        1,
        segment_name,
        symbol_name,
        0,
        @intCast(msg.len),
    );
    const args_slot = try self.allocStackMemory(8, 4);

    try self.emitFpOffset(args_slot);
    try self.emitDataAddressConst(msg_address, 0);
    self.currentCode().append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitFpOffset(args_slot);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(msg.len)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 4) catch return error.OutOfMemory;

    try self.emitRocOpsCall(args_slot, table_offset);
}

fn emitRocDbg(self: *Self, message: ProcLocalId) Allocator.Error!void {
    try self.emitRocStrCall(message, wasm_roc_ops_dbg_offset);
}

/// Call a RocOps callback taking (bytes, len) with the contents of a
/// Str-layout proc local.
fn emitRocStrCall(self: *Self, message: ProcLocalId, ops_offset: u32) Allocator.Error!void {
    if (builtin.mode == .Debug and self.procLocalLayoutIdx(message) != .str) {
        std.debug.panic(
            "WasmCodeGen invariant violated: message local {d} did not have Str layout",
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
    self.currentCode().append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    try self.emitFpOffset(args_slot);
    try self.emitLocalGet(len_local);
    self.currentCode().append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 4) catch return error.OutOfMemory;

    try self.emitRocOpsCall(args_slot, ops_offset);
}

fn generateI128Literal(self: *Self, value: i128) Allocator.Error!void {
    const base_offset = try self.allocStackMemory(16, 8);

    const unsigned: u128 = @bitCast(value);
    const low: i64 = @bitCast(@as(u64, @truncate(unsigned)));
    const high: i64 = @bitCast(@as(u64, @truncate(unsigned >> 64)));

    try self.emitFpOffset(base_offset);
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), low) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 0);

    try self.emitFpOffset(base_offset);
    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), high) catch return error.OutOfMemory;
    try self.emitStoreOp(.i64, 8);

    try self.emitFpOffset(base_offset);
}

fn bindAssignedLocal(self: *Self, target: ProcLocalId) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const runtime_layout = self.runtimeRepresentationLayoutIdx(self.procLocalLayoutIdx(target));
    const repr = try WasmLayout.wasmReprWithStore(runtime_layout, ls);
    switch (repr) {
        .stack_memory => |size| {
            if (size > 0) {
                const stable_local = try self.stabilizeCompositeResult(size);
                try self.emitLocalGet(stable_local);
            }
        },
        .primitive => {},
    }
    const vt = try self.procLocalValType(target);
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
            const target_vt = try self.resolveValType(target_layout);
            const source_vt: ValType = switch (source_layout.tag) {
                .tag_union => blk: {
                    const tu_layout = try WasmLayout.tagUnionLayoutWithStore(source_layout.getTagUnion().idx, ls);
                    if (tu_layout.size <= 4 and tu_layout.discriminant_offset == 0) {
                        if (tu_layout.discriminant_size == 0) {
                            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                            break :blk .i32;
                        } else {
                            try self.emitProcLocal(disc.source);
                            if (tu_layout.discriminant_size < 4) {
                                const mask: i32 = (@as(i32, 1) << @intCast(tu_layout.discriminant_size * 8)) - 1;
                                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), mask) catch return error.OutOfMemory;
                                self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                            }
                        }
                        break :blk try self.procLocalValType(disc.source);
                    } else {
                        try self.emitProcLocal(disc.source);
                        try self.emitLoadBySize(tu_layout.discriminant_size, @intCast(tu_layout.discriminant_offset));
                        break :blk .i32;
                    }
                },
                .box => blk: {
                    const inner_layout = ls.getLayout(source_layout.getIdx());
                    if (inner_layout.tag != .tag_union) {
                        wasmInvariantFmt(
                            "WasmCodeGen invariant violated: discriminant access on boxed non-tag-union layout {s}",
                            .{@tagName(inner_layout.tag)},
                        );
                    }
                    const tu_layout = try WasmLayout.tagUnionLayoutWithStore(inner_layout.getTagUnion().idx, ls);
                    try self.emitProcLocal(disc.source);
                    try self.emitLoadBySize(tu_layout.discriminant_size, @intCast(tu_layout.discriminant_offset));
                    break :blk .i32;
                },
                .zst => blk: {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    break :blk .i32;
                },
                else => blk: {
                    try self.emitProcLocal(disc.source);
                    break :blk try self.procLocalValType(disc.source);
                },
            };

            switch (source_vt) {
                .i32 => switch (target_vt) {
                    .i32 => {},
                    .i64 => self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory,
                    else => wasmInvariantFmt(
                        "WasmCodeGen invariant violated: discriminant target layout lowered to non-integer value type {s}",
                        .{@tagName(target_vt)},
                    ),
                },
                .i64 => switch (target_vt) {
                    .i32 => self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory,
                    .i64 => {},
                    else => wasmInvariantFmt(
                        "WasmCodeGen invariant violated: discriminant target layout lowered to non-integer value type {s}",
                        .{@tagName(target_vt)},
                    ),
                },
                else => wasmInvariantFmt(
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
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(union_layout.getTagUnion().idx));
                    break :blk variants.get(payload.variant_index).payload_layout;
                },
                .box => blk: {
                    const inner = ls.getLayout(union_layout.getIdx());
                    if (inner.tag != .tag_union) break :blk .zst;
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(inner.getTagUnion().idx));
                    break :blk variants.get(payload.variant_index).payload_layout;
                },
                else => .zst,
            };
            const payload_layout = ls.getLayout(payload_layout_idx);
            if (payload_layout.tag == .struct_) {
                const field_offset = try self.structFieldOffsetByOriginalIndexWasm(payload_layout.getStruct().idx, payload.payload_idx);
                if (field_offset > 0) {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(field_offset)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                }
            } else if (builtin.mode == .Debug and payload.payload_idx != 0) {
                std.debug.panic(
                    "LIR/wasm invariant violated: scalar tag payload access requested payload_idx {d} from non-struct payload",
                    .{payload.payload_idx},
                );
            }
            if (!try self.isCompositeLayout(target_layout)) {
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
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(union_layout.getTagUnion().idx));
                    break :blk variants.get(payload.variant_index).payload_layout;
                },
                .box => blk: {
                    const inner = ls.getLayout(union_layout.getIdx());
                    if (inner.tag != .tag_union) break :blk .zst;
                    const variants = ls.getTagUnionVariants(ls.getTagUnionData(inner.getTagUnion().idx));
                    break :blk variants.get(payload.variant_index).payload_layout;
                },
                else => .zst,
            };
            if (builtin.mode == .Debug and payload_layout_idx != target_layout) {
                const payload_layout = ls.getLayout(payload_layout_idx);
                const target_layout_val = ls.getLayout(target_layout);
                std.debug.panic(
                    "LIR/wasm invariant violated: tag_payload_struct payload layout {d} ({s}) did not match target layout {d} ({s})",
                    .{
                        @intFromEnum(payload_layout_idx),
                        @tagName(payload_layout.tag),
                        @intFromEnum(target_layout),
                        @tagName(target_layout_val.tag),
                    },
                );
            }
            if (try self.layoutStorageByteSize(target_layout) == 0) {
                self.currentCode().append(self.allocator, Op.drop) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            } else if (!try self.isCompositeLayout(target_layout)) {
                try self.emitLoadOpForLayout(target_layout, 0);
            }
        },
        .list_reinterpret => |list_reinterpret| try self.emitProcLocal(list_reinterpret.backing_ref),
        .nominal => |nom| try self.emitProcLocal(nom.backing_ref),
    }
}

fn generateRcStmt(
    self: *Self,
    value: ProcLocalId,
    rc: RcHelperKey,
    atomicity: RcAtomicity,
    inc_count: u16,
) Allocator.Error!void {
    try self.emitProcLocal(value);
    const value_local = self.storage.allocAnonymousLocal(try self.procLocalValType(value)) catch return error.OutOfMemory;
    try self.emitLocalSet(value_local);
    try self.emitExplicitRcForValueLocal(rc, atomicity, value_local, try self.procLocalValType(value), inc_count);
}

fn listElemLayout(self: *Self, list_layout_idx: layout.Idx) layout.Idx {
    const ls = self.getLayoutStore();
    const list_layout = ls.getLayout(list_layout_idx);
    return switch (list_layout.tag) {
        .list => self.runtimeRepresentationLayoutIdx(list_layout.getIdx()),
        .list_of_zst => list_layout_idx,
        else => unreachable,
    };
}

fn runtimeRepresentationLayoutIdx(self: *const Self, layout_idx: layout.Idx) layout.Idx {
    const ls = self.getLayoutStore();
    var current = layout_idx;
    while (true) {
        const layout_val = ls.getLayout(current);
        switch (layout_val.tag) {
            .closure => current = layout_val.getClosure().captures_layout_idx,
            else => return current,
        }
    }
}

/// Generate code for a function call.
/// In the current pipeline, lowering generates all closure dispatch as generic LIR
/// constructs (discriminant_switch, tag_payload_access, direct calls). The backend
/// just handles explicit direct-call symbols plus the residual runtime
/// function-value expression path. No closure-specific dispatch.
fn generateCall(self: *Self, c: anytype) Allocator.Error!void {
    const proc_key: u32 = @intFromEnum(c.proc);
    const func_idx = self.registered_procs.get(proc_key) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("generateCall: unresolved proc call target {d}", .{@intFromEnum(c.proc)});
        }
        unreachable;
    };

    if (!self.symbol_abi) {
        try self.emitLocalGet(self.roc_ops_local);
    }
    try self.emitCallArgs(c.args);
    try self.emitCall(func_idx);

    if (try self.isCompositeLayout(c.ret_layout)) {
        const result_size = try self.layoutByteSize(self.runtimeRepresentationLayoutIdx(c.ret_layout));
        const stable_local = try self.stabilizeCompositeResult(result_size);
        try self.emitLocalGet(stable_local);
    }
}

fn generateErasedCall(self: *Self, c: anytype) Allocator.Error!void {
    if (builtin.mode == .Debug) {
        const closure_layout = self.procLocalLayoutIdx(c.closure);
        const closure_layout_val = self.getLayoutStore().getLayout(closure_layout);
        if (closure_layout_val.tag != .erased_callable) {
            std.debug.panic(
                "WasmCodeGen invariant violated: erased call closure layout {d} is not erased_callable",
                .{@intFromEnum(closure_layout)},
            );
        }
    }

    try self.emitProcLocal(c.closure);
    const payload_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(payload_ptr);

    const fn_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(payload_ptr);
    try self.emitLoadOpSized(.i32, 4, 0);
    try self.emitLocalSet(fn_ptr);

    const capture_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(payload_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(builtins.erased_callable.capture_offset)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(capture_ptr);

    const arg_refs = self.store.getLocalSpan(c.args);
    var total_args_size: u32 = 0;
    for (arg_refs) |arg| {
        const arg_layout = self.procLocalLayoutIdx(arg);
        const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
        const size_align = self.getLayoutStore().layoutSizeAlign(self.getLayoutStore().getLayout(runtime_layout));
        total_args_size = std.mem.alignForward(u32, total_args_size, @intCast(@max(size_align.alignment.toByteUnits(), 1)));
        total_args_size += size_align.size;
    }

    const args_ptr = if (arg_refs.len == 0)
        null
    else
        try self.allocStackMemory(if (total_args_size == 0) 1 else total_args_size, 16);
    if (args_ptr) |args_offset| {
        const args_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(args_offset);
        try self.emitLocalSet(args_base);
        var offset: u32 = 0;
        for (arg_refs) |arg| {
            const arg_layout = self.procLocalLayoutIdx(arg);
            const runtime_layout = self.runtimeRepresentationLayoutIdx(arg_layout);
            const size_align = self.getLayoutStore().layoutSizeAlign(self.getLayoutStore().getLayout(runtime_layout));
            offset = std.mem.alignForward(u32, offset, @intCast(@max(size_align.alignment.toByteUnits(), 1)));
            if (size_align.size > 0) {
                try self.emitProcLocal(arg);
                if (try self.isCompositeLayout(arg_layout)) {
                    const arg_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitLocalSet(arg_ptr);
                    try self.emitMemCopy(args_base, offset, arg_ptr, size_align.size);
                } else {
                    try self.emitStoreToMemSized(args_base, offset, try self.resolveValType(arg_layout), size_align.size);
                }
            }
            offset += size_align.size;
        }
    }

    const ret_size = try self.layoutStorageByteSize(self.runtimeRepresentationLayoutIdx(c.ret_layout));
    const ret_offset = if (ret_size == 0) null else try self.allocStackMemory(ret_size, try self.layoutStorageByteAlign(c.ret_layout));
    const type_idx = try self.internFuncType(&.{ .i32, .i32, .i32, .i32 }, &.{});
    try self.emitLocalGet(self.roc_ops_local);
    if (ret_offset) |offset| {
        try self.emitFpOffset(offset);
    } else {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    }
    if (args_ptr) |offset| {
        try self.emitFpOffset(offset);
    } else {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    }
    try self.emitLocalGet(capture_ptr);
    try self.emitLocalGet(fn_ptr);
    try self.emitCallIndirect(type_idx);

    if (ret_size == 0) {
        const ret_vt = try self.resolveValType(c.ret_layout);
        switch (ret_vt) {
            .i32 => {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            },
            .i64 => {
                self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            },
            .f32 => {
                self.currentCode().append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f32, 0)));
            },
            .f64 => {
                self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f64, 0)));
            },
        }
    } else if (try self.isCompositeLayout(c.ret_layout)) {
        try self.emitFpOffset(ret_offset.?);
    } else {
        try self.emitFpOffset(ret_offset.?);
        try self.emitLoadOpSized(try self.resolveValType(c.ret_layout), ret_size, 0);
    }
}

fn generatePackedErasedFn(self: *Self, c: anytype) Allocator.Error!void {
    if (builtin.mode == .Debug) {
        const target_layout_val = self.getLayoutStore().getLayout(c.target_layout);
        if (target_layout_val.tag != .erased_callable) {
            std.debug.panic(
                "WasmCodeGen invariant violated: packed erased fn target layout {d} is not erased_callable",
                .{@intFromEnum(c.target_layout)},
            );
        }
    }
    if (builtin.mode == .Debug and (c.capture != null) != (c.capture_layout != null)) {
        std.debug.panic("WasmCodeGen invariant violated: packed erased fn capture value/layout presence differed", .{});
    }

    const capture_size = if (c.capture_layout) |capture_layout| try self.layoutStorageByteSize(capture_layout) else 0;
    if (builtin.mode == .Debug) {
        if (c.capture_layout) |capture_layout| {
            const capture_align = try self.layoutStorageByteAlign(capture_layout);
            if (capture_align > builtins.erased_callable.capture_alignment) {
                std.debug.panic(
                    "WasmCodeGen invariant violated: erased callable capture layout alignment {d} exceeds fixed capture alignment {d}",
                    .{ capture_align, builtins.erased_callable.capture_alignment },
                );
            }
        }
    }
    const payload_size = builtins.erased_callable.payloadSize(capture_size);
    try self.emitHeapAllocWithRefcountConst(
        @intCast(payload_size),
        builtins.erased_callable.payload_alignment,
        builtins.erased_callable.allocation_has_refcounted_children,
    );
    const payload_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(payload_ptr);

    const proc_key: u32 = @intFromEnum(c.proc);
    const table_idx = self.proc_table_indices.get(proc_key) orelse {
        wasmInvariantFmt(
            "WasmCodeGen invariant violated: packed erased fn target {d} missing table index",
            .{@intFromEnum(c.proc)},
        );
    };
    try self.emitLocalGet(payload_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(table_idx)) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 4, 0);

    const on_drop_table_idx: u32 = try self.erasedCallableOnDropTableIndex(c.on_drop);
    try self.emitLocalGet(payload_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(on_drop_table_idx)) catch return error.OutOfMemory;
    try self.emitStoreOpSized(.i32, 4, wasm_erased_callable_on_drop_offset);

    if (c.capture) |capture| {
        const capture_layout = c.capture_layout orelse unreachable;
        if (capture_size > 0) {
            try self.emitProcLocal(capture);
            const capture_value = self.storage.allocAnonymousLocal(try self.procLocalValType(capture)) catch return error.OutOfMemory;
            try self.emitLocalSet(capture_value);

            if (try self.isCompositeLayout(capture_layout)) {
                try self.emitMemCopy(payload_ptr, @intCast(builtins.erased_callable.capture_offset), capture_value, @intCast(capture_size));
            } else {
                try self.emitLocalGet(capture_value);
                try self.emitStoreToMemSized(payload_ptr, @intCast(builtins.erased_callable.capture_offset), try self.resolveValType(capture_layout), @intCast(capture_size));
            }
        }
    }

    try self.emitLocalGet(payload_ptr);
}

fn erasedCallableOnDropTableIndex(self: *Self, on_drop: LIR.ErasedCallableOnDrop) Allocator.Error!u32 {
    return switch (on_drop) {
        .none => 0,
        .rc_helper => |helper_key| blk: {
            if (self.getLayoutStore().rcHelperPlan(helper_key) == .noop) break :blk 0;
            // The on_drop slot is filled at closure creation, which is not an
            // RC statement and makes no thread-confinement claim, so it always
            // uses the atomic helper family (atomic is always sound).
            const cache_key = rcHelperCacheKey(helper_key, .atomic);
            if (self.rc_helper_table_indices.get(cache_key)) |table_idx| break :blk table_idx;
            const func_idx = try self.compileBuiltinInternalRcHelper(helper_key, .atomic);
            const table_idx = self.module.addTableElement(func_idx) catch return error.OutOfMemory;
            try self.rc_helper_table_indices.put(cache_key, table_idx);
            break :blk table_idx;
        },
        .interpreter_context_drop => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "WasmCodeGen invariant violated: interpreter_context_drop reached wasm backend",
                    .{},
                );
            }
            unreachable;
        },
    };
}

/// Emit a call instruction.
fn emitCall(self: *Self, func_idx: u32) Allocator.Error!void {
    // Keep direct-call relocations whenever a target symbol exists so final-link
    // DCE can trace generated call edges after surgical linking.
    if (self.function_symbols_by_index.get(func_idx)) |symbol| {
        try self.currentBody().emitRelocatableCall(self.allocator, symbol, func_idx);
        return;
    }

    if (self.relocatable_object) {
        wasmInvariantFmt(
            "WASM/codegen invariant violated: relocatable direct call target {d} has no function symbol",
            .{func_idx},
        );
    }

    try self.currentCode().append(self.allocator, Op.call);
    try WasmModule.leb128WriteU32(self.allocator, self.currentCode(), func_idx);
}

/// Generate call arguments (helper to avoid duplication).
fn emitCallArgs(self: *Self, args: ProcLocalSpan) Allocator.Error!void {
    const arg_refs = self.store.getLocalSpan(args);
    for (arg_refs) |arg| {
        try self.emitProcLocal(arg);
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
fn layoutByteSize(self: *const Self, layout_idx: layout.Idx) Allocator.Error!u32 {
    const ls = self.getLayoutStore();
    return switch (try WasmLayout.wasmReprWithStore(layout_idx, ls)) {
        .primitive => |vt| switch (vt) {
            .i32, .f32 => 4,
            .i64, .f64 => 8,
        },
        .stack_memory => |size| size,
    };
}

fn layoutStorageByteSize(self: *const Self, layout_idx: layout.Idx) Allocator.Error!u32 {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);
    return switch (l.tag) {
        .zst => 0,
        .scalar => switch (l.getScalar().tag) {
            .str => 12,
            .opaque_ptr => 4,
            .int => switch (l.getScalar().getInt()) {
                .u8, .i8 => 1,
                .u16, .i16 => 2,
                .u32, .i32 => 4,
                .u64, .i64 => 8,
                .u128, .i128 => 16,
            },
            .frac => switch (l.getScalar().getFrac()) {
                .f32 => 4,
                .f64 => 8,
                .dec => 16,
            },
        },
        .list, .list_of_zst => 12,
        .box, .box_of_zst => 4,
        .tag_union => (try WasmLayout.tagUnionLayoutWithStore(l.getTagUnion().idx, ls)).size,
        .struct_ => try WasmLayout.structSizeWithStore(l.getStruct().idx, ls),
        else => try self.layoutByteSize(layout_idx),
    };
}

fn layoutByteAlign(self: *const Self, layout_idx: layout.Idx) Allocator.Error!u32 {
    const ls = self.getLayoutStore();
    return switch (try WasmLayout.wasmReprWithStore(layout_idx, ls)) {
        .primitive => |vt| switch (vt) {
            .i32, .f32 => 4,
            .i64, .f64 => 8,
        },
        .stack_memory => {
            const l = ls.getLayout(layout_idx);
            return switch (l.tag) {
                .list, .list_of_zst, .box, .box_of_zst => 4,
                .scalar => if (l.getScalar().tag == .str) 4 else @intCast(ls.layoutSizeAlign(l).alignment.toByteUnits()),
                else => @intCast(ls.layoutSizeAlign(l).alignment.toByteUnits()),
            };
        },
    };
}

fn layoutStorageByteAlign(self: *const Self, layout_idx: layout.Idx) Allocator.Error!u32 {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(layout_idx);
    return switch (l.tag) {
        .zst => 1,
        .scalar => switch (l.getScalar().tag) {
            .str => 4,
            .opaque_ptr => 4,
            .int => switch (l.getScalar().getInt()) {
                .u8, .i8 => 1,
                .u16, .i16 => 2,
                .u32, .i32 => 4,
                .u64, .i64 => 8,
                .u128, .i128 => 16,
            },
            .frac => switch (l.getScalar().getFrac()) {
                .f32 => 4,
                .f64 => 8,
                .dec => 16,
            },
        },
        .list, .list_of_zst, .box, .box_of_zst => 4,
        .tag_union => (try WasmLayout.tagUnionLayoutWithStore(l.getTagUnion().idx, ls)).alignment,
        .struct_ => try WasmLayout.structAlignWithStore(l.getStruct().idx, ls),
        else => try self.layoutByteAlign(layout_idx),
    };
}

fn alignUp(value: u32, alignment: u32) u32 {
    const mask = alignment - 1;
    return (value + mask) & ~mask;
}

fn structFieldOffsetByOriginalIndexWasm(self: *const Self, struct_idx: layout.StructIdx, original_idx: u16) Allocator.Error!u32 {
    const ls = self.getLayoutStore();
    const struct_data = ls.getStructData(struct_idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    var offset: u32 = 0;
    for (0..fields.len) |i| {
        const field = fields.get(i);
        // Padding spacers are alignment 1 (their value layout's alignment is ignored).
        const field_align: u32 = if (field.is_padding) 1 else try self.layoutStorageByteAlign(field.layout);
        offset = alignUp(offset, field_align);
        if (field.index == original_idx) return offset;
        offset += try self.layoutStorageByteSize(field.layout);
    }
    unreachable;
}

fn structFieldSizeByOriginalIndexWasm(self: *const Self, struct_idx: layout.StructIdx, original_idx: u16) Allocator.Error!u32 {
    const ls = self.getLayoutStore();
    const struct_data = ls.getStructData(struct_idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    for (0..fields.len) |i| {
        const field = fields.get(i);
        if (field.index == original_idx) return try self.layoutStorageByteSize(field.layout);
    }
    unreachable;
}

fn structFieldOffsetBySortedIndexWasm(self: *const Self, struct_idx: layout.StructIdx, sorted_index: u32) Allocator.Error!u32 {
    const ls = self.getLayoutStore();
    const struct_data = ls.getStructData(struct_idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    var offset: u32 = 0;
    for (0..fields.len) |i| {
        const field = fields.get(i);
        // Padding spacers are alignment 1 (their value layout's alignment is ignored).
        const field_align: u32 = if (field.is_padding) 1 else try self.layoutStorageByteAlign(field.layout);
        offset = alignUp(offset, field_align);
        if (i == sorted_index) return offset;
        offset += try self.layoutStorageByteSize(field.layout);
    }
    unreachable;
}

fn structFieldSizeBySortedIndexWasm(self: *const Self, struct_idx: layout.StructIdx, sorted_index: u32) Allocator.Error!u32 {
    const ls = self.getLayoutStore();
    const struct_data = ls.getStructData(struct_idx);
    const fields = ls.struct_fields.sliceRange(struct_data.getFields());
    if (sorted_index >= fields.len) unreachable;
    return try self.layoutStorageByteSize(fields.get(sorted_index).layout);
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
    self.currentCode().append(self.allocator, op) catch return error.OutOfMemory;
    // Alignment (log2): i32=2, i64=3, f32=2, f64=3
    const align_log2: u32 = switch (vt) {
        .i32, .f32 => 2,
        .i64, .f64 => 3,
    };
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), align_log2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), mem_offset) catch return error.OutOfMemory;
}

/// Emit a size-aware store instruction for a field with a known byte size.
/// For sub-32-bit fields (1 or 2 bytes), uses i32.store8/i32.store16.
fn emitStoreOpSized(self: *Self, vt: ValType, byte_size: u32, mem_offset: u32) Allocator.Error!void {
    if (vt == .i32 and byte_size < 4) {
        // Sub-word store for i32 values in small fields
        const op: u8 = if (byte_size == 1) Op.i32_store8 else Op.i32_store16;
        self.currentCode().append(self.allocator, op) catch return error.OutOfMemory;
        const align_log2: u32 = if (byte_size == 1) 0 else 1;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), align_log2) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), mem_offset) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, op) catch return error.OutOfMemory;
    const align_log2: u32 = switch (vt) {
        .i32, .f32 => 2,
        .i64, .f64 => 3,
    };
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), align_log2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), mem_offset) catch return error.OutOfMemory;
}

/// Emit a size-aware load instruction for a field with a known byte size.
/// For sub-32-bit fields (1 or 2 bytes), uses i32.load8_u/i32.load16_u.
fn emitLoadOpSized(self: *Self, vt: ValType, byte_size: u32, mem_offset: u32) Allocator.Error!void {
    if (byte_size == 0) {
        self.currentCode().append(self.allocator, Op.drop) catch return error.OutOfMemory;
        switch (vt) {
            .i32 => {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            },
            .i64 => {
                self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            },
            .f32 => {
                self.currentCode().append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f32, 0)));
            },
            .f64 => {
                self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f64, 0)));
            },
        }
        return;
    }

    if (vt == .i32 and byte_size < 4) {
        // Sub-word load for i32 values in small fields
        const op: u8 = if (byte_size == 1) Op.i32_load8_u else Op.i32_load16_u;
        self.currentCode().append(self.allocator, op) catch return error.OutOfMemory;
        const align_log2: u32 = if (byte_size == 1) 0 else 1;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), align_log2) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), mem_offset) catch return error.OutOfMemory;
    } else {
        try self.emitLoadOp(vt, mem_offset);
    }
}

/// Like emitLoadOpSized but uses sign-extending loads for signed sub-word types.
fn emitLoadOpForLayout(self: *Self, lay: layout.Idx, mem_offset: u32) Allocator.Error!void {
    const vt = try self.resolveValType(lay);
    const byte_size = try self.layoutStorageByteSize(lay);
    if (vt == .i32 and byte_size < 4) {
        const is_signed = (lay == .i8 or lay == .i16);
        const op: u8 = if (byte_size == 1)
            (if (is_signed) Op.i32_load8_s else Op.i32_load8_u)
        else
            (if (is_signed) Op.i32_load16_s else Op.i32_load16_u);
        self.currentCode().append(self.allocator, op) catch return error.OutOfMemory;
        const align_log2: u32 = if (byte_size == 1) 0 else 1;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), align_log2) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), mem_offset) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
    // local.get base
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
    // local.get temp (push value)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
    // store with field_offset as immediate
    try self.emitStoreOp(vt, field_offset);
}

/// Store a value into memory with size-aware opcodes for sub-32-bit fields.
fn emitStoreToMemSized(self: *Self, base_local: u32, field_offset: u32, vt: ValType, byte_size: u32) Allocator.Error!void {
    if (byte_size == 0) {
        const temp = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
        return;
    }

    const temp = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
    try self.emitStoreOpSized(vt, byte_size, field_offset);
}

/// Copy `byte_count` bytes from src_local pointer to (dst_local + dst_offset).
/// Uses i32.load/i32.store in chunks, with i32.load8_u/i32.store8 for remainder.
fn emitMemCopy(self: *Self, dst_local: u32, dst_offset: u32, src_local: u32, byte_count: u32) Allocator.Error!void {
    var offset: u32 = 0;

    // Copy i32-sized chunks
    while (offset + 4 <= byte_count) : (offset += 4) {
        // dst_local
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dst_local) catch return error.OutOfMemory;
        // load from src
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
        // store to dst
        self.currentCode().append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dst_offset + offset) catch return error.OutOfMemory;
    }

    // Copy i16 chunk
    if (offset + 2 <= byte_count) {
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dst_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_store16) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dst_offset + offset) catch return error.OutOfMemory;
        offset += 2;
    }

    // Copy remaining byte
    if (offset < byte_count) {
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dst_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dst_offset + offset) catch return error.OutOfMemory;
    }
}

/// Zero-initialize a region of memory.
/// Emits i32.store 0 for each 4-byte chunk, plus i32.store8 0 for remaining bytes.
fn emitZeroInit(self: *Self, base_local: u32, byte_count: u32) Allocator.Error!void {
    var offset: u32 = 0;
    while (offset + 4 <= byte_count) : (offset += 4) {
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
    }
    while (offset < byte_count) : (offset += 1) {
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
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
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        return;
    }

    const size = try WasmLayout.structSizeWithStore(l.getStruct().idx, ls);
    if (size == 0) {
        // Zero-sized struct — push dummy pointer
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        return;
    }

    const align_val: u32 = try WasmLayout.structAlignWithStore(l.getStruct().idx, ls);

    const frame_offset = try self.allocStackMemory(size, align_val);

    // Allocate a local to hold the base pointer
    const base_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(frame_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;

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
        const field_byte_size = try self.structFieldSizeByOriginalIndexWasm(l.getStruct().idx, @intCast(i));
        if (field_byte_size == 0) continue;
        const field_layout_idx = ls.getStructFieldLayoutByOriginalIndex(l.getStruct().idx, @intCast(i));
        const is_composite = try self.isCompositeLayout(field_layout_idx);
        const field_vt = try WasmLayout.resultValTypeWithStore(field_layout_idx, ls);

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
            const expr_vt = try self.procLocalValType(field_expr_id);
            try self.emitConversion(expr_vt, field_vt);
        }

        // Save value to a local (i32 pointer for composite, value type for primitive)
        const save_vt: ValType = if (is_composite) .i32 else field_vt;
        const local_idx = self.storage.allocAnonymousLocal(save_vt) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), local_idx) catch return error.OutOfMemory;

        field_val_locals[i] = local_idx;
        field_val_types[i] = save_vt;
    }

    // Zero-initialize record memory for consistent padding in structural equality
    try self.emitZeroInit(base_local, size);

    // Store each field from pre-computed locals
    for (fields, 0..) |_, i| {
        const field_offset = try self.structFieldOffsetByOriginalIndexWasm(l.getStruct().idx, @intCast(i));
        const field_layout_idx = ls.getStructFieldLayoutByOriginalIndex(l.getStruct().idx, @intCast(i));
        const field_byte_size = try self.structFieldSizeByOriginalIndexWasm(l.getStruct().idx, @intCast(i));
        if (field_byte_size == 0) continue;
        const is_composite = try self.isCompositeLayout(field_layout_idx);

        if (is_composite and field_byte_size > 0) {
            // The local holds an i32 pointer to source data. Copy to record slot.
            try self.emitMemCopy(base_local, field_offset, field_val_locals[i], field_byte_size);
        } else {
            // Primitive — push value from local, then store to record
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), field_val_locals[i]) catch return error.OutOfMemory;
            try self.emitStoreToMemSized(base_local, field_offset, field_val_types[i], field_byte_size);
        }
    }

    // Push the base pointer as the result
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
}

/// Generate a field access from a struct/tuple pointer value.
fn generateStructAccess(self: *Self, sa: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();

    // Generate the struct expression → pushes i32 pointer
    try self.emitProcLocal(sa.struct_expr);
    const struct_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(struct_ptr);

    // Get the field offset
    const source_layout = ls.getLayout(sa.struct_layout);
    const struct_layout_idx = switch (source_layout.tag) {
        .box => source_layout.getIdx(),
        else => sa.struct_layout,
    };
    const struct_layout = ls.getLayout(struct_layout_idx);
    if (struct_layout.tag != .struct_) {
        wasmInvariantFmt(
            "WasmCodeGen invariant violated: field access expected struct layout, got {s}",
            .{@tagName(struct_layout.tag)},
        );
    }

    const field_offset = try self.structFieldOffsetByOriginalIndexWasm(struct_layout.getStruct().idx, sa.field_idx);
    const field_byte_size = try self.structFieldSizeByOriginalIndexWasm(struct_layout.getStruct().idx, sa.field_idx);
    const source_field_layout_idx = ls.getStructFieldLayoutByOriginalIndex(struct_layout.getStruct().idx, sa.field_idx);
    const field_layout = ls.getLayout(source_field_layout_idx);

    // Check if the field is a composite type
    if (try self.isCompositeLayout(sa.field_layout) and field_byte_size > 0) {
        const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(struct_ptr);
        if (field_offset > 0) {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(field_offset)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
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
        const field_vt = try WasmLayout.resultValTypeWithStore(sa.field_layout, ls);
        // Load the field: [struct_ptr + field_offset] (size-aware for sub-32-bit fields)
        try self.emitLocalGet(struct_ptr);
        try self.emitLoadOpSized(field_vt, field_byte_size, field_offset);
    }
}

/// Generate a tag expression with an optional payload.
fn generateTag(self: *Self, t: anytype) Allocator.Error!void {
    const ls = self.getLayoutStore();
    const l = ls.getLayout(t.union_layout);

    if (l.tag == .zst) {
        if (t.discriminant != 0) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "WASM/codegen invariant violated: zero-sized tag layout cannot encode discriminant {d}",
                    .{t.discriminant},
                );
            }
            unreachable;
        }
        if (t.payload) |payload_local| {
            try self.emitProcLocal(payload_local);
            self.currentCode().append(self.allocator, Op.drop) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        return;
    }

    if (l.tag != .tag_union) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "WASM/codegen invariant violated: tag assignment target must be tag_union or zst, got {s}",
                .{@tagName(l.tag)},
            );
        }
        unreachable;
    }

    const tu_layout = try WasmLayout.tagUnionLayoutWithStore(l.getTagUnion().idx, ls);
    const tu_size = tu_layout.size;
    const disc_offset = tu_layout.discriminant_offset;
    const tu_data = ls.getTagUnionData(l.getTagUnion().idx);
    const variants = ls.getTagUnionVariants(tu_data);
    if (@as(usize, t.variant_index) >= variants.len) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "WASM/codegen invariant violated: tag assignment variant index {d} exceeded variant count {d}",
                .{ t.variant_index, variants.len },
            );
        }
        unreachable;
    }
    const variant_payload_layout = variants.get(t.variant_index).payload_layout;
    if (tu_size <= 4 and disc_offset == 0) {
        // Small tag union — discriminant only, no payload (enum).
        // Still evaluate payload for side effects (e.g., early_return from ? operator).
        // Payload must be zero-sized since the tag has no payload room.
        if (t.payload) |payload_local| {
            try self.emitProcLocal(payload_local);
            self.currentCode().append(self.allocator, Op.drop) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(t.discriminant)) catch return error.OutOfMemory;
        return;
    }

    const align_val: u32 = tu_layout.alignment;
    const frame_offset = try self.allocStackMemory(tu_size, align_val);

    const base_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(frame_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;

    // Store payload FIRST (payload may overlap discriminant if it is wider than
    // the payload slot, e.g. i64 for a u32 tag payload — the i64 store would
    // clobber the discriminant).
    if (t.payload) |payload_local| {
        const payload_byte_size = try self.layoutByteSize(variant_payload_layout);
        if (builtin.mode == .Debug and self.procLocalLayoutIdx(payload_local) != variant_payload_layout) {
            std.debug.panic(
                "WASM/codegen invariant violated: tag payload local layout {d} did not match variant payload layout {d}",
                .{ @intFromEnum(self.procLocalLayoutIdx(payload_local)), @intFromEnum(variant_payload_layout) },
            );
        }
        try self.emitProcLocal(payload_local);
        if (try self.isCompositeLocal(payload_local)) {
            // Composite types (Str, List, records, etc.) produce a pointer on
            // the stack. Copy the full data from the source to the tag union.
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
            try self.emitMemCopy(base_local, 0, src_local, payload_byte_size);
        } else {
            const payload_vt = try self.procLocalValType(payload_local);
            try self.emitStoreToMemSized(base_local, 0, payload_vt, payload_byte_size);
        }
    }

    // Store discriminant AFTER payload (so it can't be overwritten)
    const disc_size: u32 = tu_layout.discriminant_size;
    if (disc_size != 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(t.discriminant)) catch return error.OutOfMemory;
        try self.emitStoreToMemSized(base_local, disc_offset, .i32, disc_size);
    }

    // Push base pointer
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
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
        self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        try self.emitZeroInit(base_local, 12);
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        return;
    }

    // Get element layout and size
    const elem_size: u32 = try self.layoutStorageByteSize(l.elem_layout);
    const elem_align: u32 = try self.layoutStorageByteAlign(l.elem_layout);

    // Allocate space for all elements on the heap so list literals remain valid
    // when returned from functions (callee stack frames are reclaimed on return).
    const total_data_size = elem_size * @as(u32, @intCast(elems.len));
    const elements_refcounted = builtinInternalLayoutContainsRefcounted(
        ls,
        "wasm.generateList.elem_layout_refcounted",
        l.elem_layout,
    );
    const data_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    if (total_data_size > 0) {
        try self.emitHeapAllocWithRefcountConst(total_data_size, elem_align, elements_refcounted);
        self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), data_base) catch return error.OutOfMemory;

        // Zero-initialize element data for consistent padding
        try self.emitZeroInit(data_base, total_data_size);
    } else {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), data_base) catch return error.OutOfMemory;
    }

    // Store each element
    const elem_vt = try WasmLayout.resultValTypeWithStore(l.elem_layout, ls);
    for (elems, 0..) |elem_expr_id, i| {
        try self.emitProcLocal(elem_expr_id);

        const offset = @as(u32, @intCast(i)) * elem_size;
        if (try self.isCompositeLayout(l.elem_layout) and elem_size > 0) {
            // Composite element — copy from source pointer
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_local) catch return error.OutOfMemory;
            try self.emitMemCopy(data_base, offset, src_local, elem_size);
        } else {
            // Primitive element — store directly
            const expr_vt = try self.procLocalValType(elem_expr_id);
            try self.emitConversion(expr_vt, elem_vt);
            try self.emitStoreToMemSized(data_base, offset, elem_vt, elem_size);
        }
    }

    // Construct the 12-byte RocList struct on the stack frame
    const list_offset = try self.allocStackMemory(12, 4);
    const list_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(list_offset);
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_base) catch return error.OutOfMemory;

    // Store elements pointer (offset 0)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), data_base) catch return error.OutOfMemory;
    try self.emitStoreToMem(list_base, 0, .i32);

    // Store length (offset 4)
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elems.len)) catch return error.OutOfMemory;
    try self.emitStoreToMem(list_base, 4, .i32);

    // Store encoded capacity (offset 8).
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elems.len << 1)) catch return error.OutOfMemory;
    try self.emitStoreToMem(list_base, 8, .i32);

    // Push pointer to the RocList struct
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_base) catch return error.OutOfMemory;
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
        .num_bitwise_and,
        .num_bitwise_or,
        .num_bitwise_xor,
        .num_bitwise_not,
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
            self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        },

        .dict_pseudo_seed,
        .hasher_finish,
        .hasher_write_bool,
        .hasher_write_u8,
        .hasher_write_u16,
        .hasher_write_u32,
        .hasher_write_u64,
        .hasher_write_u128,
        .hasher_write_i8,
        .hasher_write_i16,
        .hasher_write_i32,
        .hasher_write_i64,
        .hasher_write_i128,
        .hasher_write_f32,
        .hasher_write_f64,
        .hasher_write_dec,
        .hasher_write_bytes,
        .hasher_write_str,
        => {
            return self.emitHasherLowLevel(ll.op, args);
        },

        .crypto_sha256_hash_bytes,
        .crypto_sha256_hasher_empty,
        .crypto_sha256_hasher_write,
        .crypto_sha256_hasher_finish,
        .crypto_blake3_hash_bytes,
        .crypto_blake3_hasher_empty,
        .crypto_blake3_hasher_write,
        .crypto_blake3_hasher_finish,
        => {
            return self.emitCryptoLowLevel(ll.op, args);
        },

        // Safe integer widenings (no-op or single instruction)
        .u8_to_i16, .u8_to_i32, .u8_to_u16, .u8_to_u32 => {
            // u8 is already i32 in wasm, and widening to larger types is a no-op
            try self.emitProcLocal(args[0]);
            // If arg produces i64 (e.g. from i64_literal), wrap to i32
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
        },
        .i8_to_i16, .i8_to_i32 => {
            // i8 is i32 in wasm, sign-extend from 8 bits
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        },
        .u16_to_i32, .u16_to_u32 => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
        },
        .i16_to_i32 => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
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
            const arg_vt = try self.procLocalValType(args[0]);
            if (arg_vt == .i64) {
                // Already i64 — no extension needed
                return;
            }
            self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
        },
        .i8_to_i64, .i16_to_i64, .i32_to_i64 => {
            try self.emitProcLocal(args[0]);
            const arg_vt = try self.procLocalValType(args[0]);
            if (arg_vt == .i64) {
                // Already i64 — no extension needed
                return;
            }
            self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },

        // Narrowing/wrapping conversions
        .i64_to_i32_wrap, .u64_to_u32_wrap, .u64_to_i32_wrap, .i64_to_u32_wrap => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
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
            const arg_vt = try self.procLocalValType(args[0]);
            if (arg_vt == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            // Mask to 8 bits
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xFF) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
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
            const arg_vt = try self.procLocalValType(args[0]);
            if (arg_vt == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            // Mask to 16 bits
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xFFFF) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        },
        .i16_to_u32_wrap => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
        },
        .i8_to_u16_wrap => {
            try self.emitProcLocal(args[0]);
            // Sign-extend from 8 bits then mask to 16 bits
            self.currentCode().append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xFFFF) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i8_to_u64_wrap => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i16_to_u64_wrap => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i32_to_u64_wrap => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i32_to_u128_wrap => {
            // Signed i32→u128 wrap: sign-extend to i64, then to i128
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                // Already i64
            } else {
                self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            }
            try self.emitIntToI128(true);
        },
        .i32_to_u64_try => {
            // Signed i32 → unsigned u64: check >= 0, then sign-extend to i64
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 8, 8);
            // Check: val >= 0
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            // Ok path: sign-extend i32 to i64, store payload
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 0);
            // Set discriminant = 1 (Ok)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            try self.emitStoreOpSized(.i32, 1, 8);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .i32_to_u128_try => {
            // Signed i32 → unsigned u128: check >= 0, then sign-extend to i128
            try self.emitProcLocal(args[0]);
            // Sign-extend to i64 first
            if (try self.procLocalValType(args[0]) != .i64) {
                self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            }
            try self.emitIntToI128(true);
            try self.emitI128TryToU128(true);
        },

        // Float conversions
        .f32_to_f64 => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
        },
        .f64_to_f32_wrap => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },

        // Int to float
        .i32_to_f32, .i8_to_f32, .i16_to_f32 => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.f32_convert_i32_s) catch return error.OutOfMemory;
        },
        .u32_to_f32, .u8_to_f32, .u16_to_f32 => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.f32_convert_i32_u) catch return error.OutOfMemory;
        },
        .i32_to_f64, .i8_to_f64, .i16_to_f64 => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.f64_convert_i32_s) catch return error.OutOfMemory;
        },
        .u32_to_f64, .u8_to_f64, .u16_to_f64 => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.f64_convert_i32_u) catch return error.OutOfMemory;
        },
        .i64_to_f32 => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.f32_convert_i64_s) catch return error.OutOfMemory;
        },
        .u64_to_f32 => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.f32_convert_i64_u) catch return error.OutOfMemory;
        },
        .i64_to_f64 => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
        },
        .u64_to_f64 => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
        },

        // Float to int (truncating)
        .f32_to_i32_trunc, .f32_to_i8_trunc, .f32_to_i16_trunc => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i32_trunc_f32_s) catch return error.OutOfMemory;
        },
        .f32_to_u32_trunc, .f32_to_u8_trunc, .f32_to_u16_trunc => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i32_trunc_f32_u) catch return error.OutOfMemory;
        },
        .f64_to_i32_trunc, .f64_to_i8_trunc, .f64_to_i16_trunc => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i32_trunc_f64_s) catch return error.OutOfMemory;
        },
        .f64_to_u32_trunc, .f64_to_u8_trunc, .f64_to_u16_trunc => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i32_trunc_f64_u) catch return error.OutOfMemory;
        },
        .f32_to_i64_trunc => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i64_trunc_f32_s) catch return error.OutOfMemory;
        },
        .f32_to_u64_trunc => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i64_trunc_f32_u) catch return error.OutOfMemory;
        },
        .f64_to_i64_trunc => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i64_trunc_f64_s) catch return error.OutOfMemory;
        },
        .f64_to_u64_trunc => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i64_trunc_f64_u) catch return error.OutOfMemory;
        },
        .f32_to_bits => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i32_reinterpret_f32) catch return error.OutOfMemory;
        },
        .f32_from_bits => {
            try self.emitProcLocal(args[0]);
            if (try self.procLocalValType(args[0]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.f32_reinterpret_i32) catch return error.OutOfMemory;
        },
        .f64_to_bits => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.i64_reinterpret_f64) catch return error.OutOfMemory;
        },
        .f64_from_bits => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.f64_reinterpret_i64) catch return error.OutOfMemory;
        },

        // Float math functions (direct wasm opcodes)
        .num_pow => {
            if (ll.ret_layout == .dec) {
                try self.emitDecBinaryMath(args, self.dec_pow_import orelse unreachable);
            } else {
                try self.emitFloatPow(args, ll.ret_layout);
            }
        },
        .num_sin => {
            if (ll.ret_layout == .dec) {
                try self.emitDecUnaryMath(args[0], self.dec_sin_import orelse unreachable);
            } else {
                try self.emitFloatUnaryMath(args[0], ll.ret_layout, .float_sin, self.float_sin_import);
            }
        },
        .num_cos => {
            if (ll.ret_layout == .dec) {
                try self.emitDecUnaryMath(args[0], self.dec_cos_import orelse unreachable);
            } else {
                try self.emitFloatUnaryMath(args[0], ll.ret_layout, .float_cos, self.float_cos_import);
            }
        },
        .num_tan => {
            if (ll.ret_layout == .dec) {
                try self.emitDecUnaryMath(args[0], self.dec_tan_import orelse unreachable);
            } else {
                try self.emitFloatUnaryMath(args[0], ll.ret_layout, .float_tan, self.float_tan_import);
            }
        },
        .num_asin => {
            if (ll.ret_layout == .dec) {
                try self.emitDecUnaryMath(args[0], self.dec_asin_import orelse unreachable);
            } else {
                try self.emitFloatUnaryMath(args[0], ll.ret_layout, .float_asin, self.float_asin_import);
            }
        },
        .num_acos => {
            if (ll.ret_layout == .dec) {
                try self.emitDecUnaryMath(args[0], self.dec_acos_import orelse unreachable);
            } else {
                try self.emitFloatUnaryMath(args[0], ll.ret_layout, .float_acos, self.float_acos_import);
            }
        },
        .num_atan => {
            if (ll.ret_layout == .dec) {
                try self.emitDecUnaryMath(args[0], self.dec_atan_import orelse unreachable);
            } else {
                try self.emitFloatUnaryMath(args[0], ll.ret_layout, .float_atan, self.float_atan_import);
            }
        },
        .num_sqrt => {
            if (ll.ret_layout == .dec) {
                try self.emitDecUnaryMath(args[0], self.dec_sqrt_import orelse unreachable);
            } else {
                try self.emitProcLocal(args[0]);
                const vt = try self.resolveValType(ll.ret_layout);
                const wasm_op: u8 = switch (vt) {
                    .f32 => Op.f32_sqrt,
                    .f64 => Op.f64_sqrt,
                    .i32, .i64 => unreachable,
                };
                self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
            }
        },
        .num_floor => {
            try self.emitProcLocal(args[0]);
            const vt = try self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_floor,
                .f64 => Op.f64_floor,
                .i32, .i64 => unreachable,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_ceiling => {
            try self.emitProcLocal(args[0]);
            const vt = try self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_ceil,
                .f64 => Op.f64_ceil,
                .i32, .i64 => unreachable,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_round => {
            try self.emitProcLocal(args[0]);
            const vt = try self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_nearest,
                .f64 => Op.f64_nearest,
                .i32, .i64 => unreachable,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
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
            const ret_vt = try self.resolveValType(ll.ret_layout);
            if (ret_vt == .i64) {
                self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            }
        },
        .list_get_unsafe => {
            // args[0] = list, args[1] = index
            // Returns bare element without bounds checking.
            const ls = self.getLayoutStore();
            const list_layout_idx = self.procLocalLayoutIdx(args[0]);
            const elem_layout_idx = self.listElemLayout(list_layout_idx);
            const elem_size: u32 = try self.layoutStorageByteSize(elem_layout_idx);
            const elem_is_composite = try self.isCompositeLayout(elem_layout_idx);

            // Generate list expression and save pointer
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            // Generate index as i32
            const index_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitProcLocal(args[1]);
            if (try self.procLocalValType(args[1]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            try self.emitLocalSet(index_local);

            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalGet(index_local);
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

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

        .list_map_can_reuse => {
            // On a width where the element layouts are not interchangeable, the
            // in-place branch is statically dead, so the result is a constant 0
            // and the uniqueness check must not run.
            if (!ll.interchangeable.get(self.getLayoutStore().targetUsize())) {
                try self.emitI32Const(0);
                return;
            }
            // list_map_can_reuse(list, transform) -> U8: the list uniquely owns
            // its allocation and is not a seamless slice. Wasm is
            // single-threaded, so a plain refcount load suffices.
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            const cap_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 8);
            try self.emitLocalSet(cap_local);

            // Seamless slices tag the low bit of the capacity field.
            try self.emitLocalGet(cap_local);
            try self.emitI32Const(1);
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(ValType.i32)) catch return error.OutOfMemory;
            try self.emitI32Const(0);
            self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            // Zero capacity has no heap refcount and counts as unique.
            try self.emitLocalGet(cap_local);
            try self.emitI32Const(1);
            self.currentCode().append(self.allocator, Op.i32_shr_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(ValType.i32)) catch return error.OutOfMemory;
            try self.emitI32Const(1);
            self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            // The refcount lives one usize before the data pointer.
            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitI32Const(4);
            self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            try self.emitI32Const(1);
            self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        },

        .list_map_cast_unsafe => {
            // Same bits, new element type: copy the 12-byte list struct.
            try self.emitProcLocal(args[0]);
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(src_local);
            const dst_offset = try self.allocStackMemory(12, 4);
            const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(dst_offset);
            try self.emitLocalSet(dst_local);
            try self.emitMemCopy(dst_local, 0, src_local, 12);
            try self.emitLocalGet(dst_local);
        },

        .list_map_extract_unsafe => {
            // Like list_get_unsafe, but the element layout comes from the
            // result (input element type) while the buffer is typed with the
            // output element type; lowering guarantees the strides match.
            const ls = self.getLayoutStore();
            const elem_layout_idx = ll.ret_layout;
            const elem_size: u32 = try self.layoutStorageByteSize(elem_layout_idx);
            const elem_is_composite = try self.isCompositeLayout(elem_layout_idx);
            if (elem_size == 0) {
                // ZST element: no data to read.
                try self.emitI32Const(0);
                return;
            }

            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            const index_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitProcLocal(args[1]);
            if (try self.procLocalValType(args[1]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            try self.emitLocalSet(index_local);

            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalGet(index_local);
            try self.emitI32Const(@intCast(elem_size));
            self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

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

        .list_map_write_unsafe => {
            // list_map_write_unsafe(list, index, element) -> the same list,
            // with the owned element's bytes stored into the vacated slot.
            const elem_layout_idx = self.procLocalLayoutIdx(args[2]);
            const elem_size: u32 = try self.layoutStorageByteSize(elem_layout_idx);
            const elem_is_composite = try self.isCompositeLayout(elem_layout_idx);

            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            if (elem_size == 0) {
                // ZST element: nothing to store; pass the list through.
                const zst_result_offset = try self.allocStackMemory(12, 4);
                const zst_result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitFpOffset(zst_result_offset);
                try self.emitLocalSet(zst_result_local);
                try self.emitMemCopy(zst_result_local, 0, list_local, 12);
                try self.emitLocalGet(zst_result_local);
                return;
            }

            const index_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitProcLocal(args[1]);
            if (try self.procLocalValType(args[1]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            try self.emitLocalSet(index_local);

            const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalGet(index_local);
            try self.emitI32Const(@intCast(elem_size));
            self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(dst_local);

            if (elem_is_composite) {
                try self.emitProcLocal(args[2]);
                const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(src_local);
                try self.emitMemCopy(dst_local, 0, src_local, elem_size);
            } else {
                const elem_vt = try self.resolveValType(elem_layout_idx);
                try self.emitLocalGet(dst_local);
                try self.emitProcLocal(args[2]);
                try self.emitConversion(try self.procLocalValType(args[2]), elem_vt);
                try self.emitStoreOpSized(elem_vt, elem_size, 0);
            }

            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitLocalSet(result_local);
            try self.emitMemCopy(result_local, 0, list_local, 12);
            try self.emitLocalGet(result_local);
        },

        // String operations
        // Bitwise operations
        .num_log => unreachable, // Resolved by earlier lowering

        .num_abs_diff => {
            // abs_diff(a, b) -> |a - b|
            return self.emitNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        .num_shift_left_by, .num_shift_right_by, .num_shift_right_zf_by => {
            // Shift operations: shift(value, amount)
            return self.emitNumericLowLevel(ll.op, args, ll.ret_layout);
        },

        .list_drop_at => {
            try self.generateLLListDropAt(args, ll.ret_layout, ll.unique_args);
        },

        .list_replace_unsafe => {
            try self.generateLLListReplaceUnsafe(args, ll.ret_layout, ll.unique_args);
        },

        .list_swap => {
            try self.generateLLListSwap(args, ll.ret_layout, ll.unique_args);
        },

        // List element access operations (no heap allocation needed)
        .list_first => {
            const ls = self.getLayoutStore();
            const list_layout_idx = self.procLocalLayoutIdx(args[0]);
            const elem_layout_idx = self.listElemLayout(list_layout_idx);
            const elem_size: u32 = try self.layoutStorageByteSize(elem_layout_idx);
            const elem_is_composite = try self.isCompositeLayout(elem_layout_idx);

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
            const elem_size: u32 = try self.layoutStorageByteSize(elem_layout_idx);
            const elem_is_composite = try self.isCompositeLayout(elem_layout_idx);

            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(list_local);

            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 0);
            try self.emitLocalGet(list_local);
            try self.emitLoadOp(.i32, 4);
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

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
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;

            try self.emitProcLocal(args[1]);
            if (try self.procLocalValType(args[1]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;

            // Allocate result RocList (12 bytes)
            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

            // Get element size from ret_layout (which is the list layout, not elem)
            const list_abi = self.builtinInternalListAbi("wasm.list_drop_first.builtin_list_abi", ll.ret_layout);
            const elem_size = list_abi.elem_size;

            // new_ptr = old_ptr + count * elem_size
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 0);

            // new_len = old_len - count
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Encode seamless-slice cap from the source allocation pointer.
            const encoded_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitPrepareListSliceMetadata(list_local, list_abi.elements_refcounted, encoded_cap);

            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), encoded_cap) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 8);

            // Push result pointer
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
        },
        .list_drop_last => {
            // list_drop_last(list, count) -> list
            // Returns a RocList with adjusted length (pointer stays same)
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;

            try self.emitProcLocal(args[1]);
            if (try self.procLocalValType(args[1]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;

            // Allocate result RocList (12 bytes)
            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

            // Same elements_ptr
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            try self.emitStoreOp(.i32, 0);

            // new_len = old_len - count
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Same capacity
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 8);
            try self.emitStoreOp(.i32, 8);

            // Push result pointer
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
        },
        .list_take_first => {
            // list_take_first(list, count) -> list
            // Same as list but with length = min(count, len)
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;

            try self.emitProcLocal(args[1]);
            if (try self.procLocalValType(args[1]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;

            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

            // Same elements_ptr
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            try self.emitStoreOp(.i32, 0);

            // new_len = min(count, old_len) using select
            // Stack: count, old_len, count <= old_len
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.select) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Same capacity
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 8);
            try self.emitStoreOp(.i32, 8);

            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
        },
        .list_take_last => {
            // list_take_last(list, count) -> list
            // elements_ptr += (len - min(count, len)) * elem_size
            // length = min(count, len)
            try self.emitProcLocal(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;

            try self.emitProcLocal(args[1]);
            if (try self.procLocalValType(args[1]) == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const count_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;

            // Load length
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), len_local) catch return error.OutOfMemory;

            // actual_count = min(count, len)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), len_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), count_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), len_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.select) catch return error.OutOfMemory;
            const actual_count = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), actual_count) catch return error.OutOfMemory;

            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

            const list_abi = self.builtinInternalListAbi("wasm.list_take_last.builtin_list_abi", ll.ret_layout);
            const elem_size = list_abi.elem_size;

            // new_ptr = old_ptr + (len - actual_count) * elem_size
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), len_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), actual_count) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 0);

            // new_len = actual_count
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), actual_count) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Encode seamless-slice cap from the source allocation pointer.
            const encoded_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitPrepareListSliceMetadata(list_local, list_abi.elements_refcounted, encoded_cap);

            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), encoded_cap) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 8);

            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
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
            try self.generateLLListConcat(args, ll.ret_layout, ll.unique_args);
        },
        .list_reverse => {
            // list_reverse(list) -> reversed list
            try self.generateLLListReverse(args, ll.ret_layout, ll.unique_args);
        },
        // list_with_capacity(capacity) -> empty list with given capacity
        .list_with_capacity => {
            try self.generateLLListWithCapacity(args, ll.ret_layout);
        },
        // list_set(list, index, value) -> list with value at index
        .list_set => {
            try self.generateLLListSet(args, ll.ret_layout, ll.unique_args);
        },
        // list_reserve(list, capacity) -> list with at least that capacity
        .list_reserve => {
            try self.generateLLListReserve(args, ll.ret_layout, ll.unique_args);
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
            const record_idx = record_layout.getStruct().idx;
            const len_field_off = try self.structFieldOffsetByOriginalIndexWasm(record_idx, 0);
            const start_field_off = try self.structFieldOffsetByOriginalIndexWasm(record_idx, 1);
            if (builtin.mode == .Debug) {
                const sd = ls.getStructData(record_idx);
                const sorted_fields = ls.struct_fields.sliceRange(sd.getFields());
                if (sorted_fields.len != 2) {
                    std.debug.panic(
                        "LIR/wasm invariant violated: list_sublist record expected 2 fields, got {d}",
                        .{sorted_fields.len},
                    );
                }
                const record_size = try self.layoutStorageByteSize(record_layout_idx);
                if (ls.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .u64 or
                    ls.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .u64 or
                    try self.structFieldSizeByOriginalIndexWasm(record_idx, 0) != 8 or
                    try self.structFieldSizeByOriginalIndexWasm(record_idx, 1) != 8 or
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
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;

            // Generate record arg (pointer to the config record)
            try self.emitProcLocal(args[1]);
            const rec_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rec_local) catch return error.OutOfMemory;

            // Load "len" field by original semantic index, wrap to i32
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rec_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, len_field_off);
            self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            const sub_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), sub_len) catch return error.OutOfMemory;

            // Load "start" field by original semantic index, wrap to i32
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rec_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, start_field_off);
            self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            const start_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), start_local) catch return error.OutOfMemory;

            // Load old_len from list
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), old_len) catch return error.OutOfMemory;

            // actual_start = min(start, old_len)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), start_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), old_len) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), start_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), old_len) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.select) catch return error.OutOfMemory;
            const actual_start = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), actual_start) catch return error.OutOfMemory;

            // remaining = old_len - actual_start
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), old_len) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), actual_start) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            const remaining = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), remaining) catch return error.OutOfMemory;

            // actual_len = min(sub_len, remaining)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), sub_len) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), remaining) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), sub_len) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), remaining) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.select) catch return error.OutOfMemory;
            const actual_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), actual_len) catch return error.OutOfMemory;

            // Allocate result RocList (12 bytes)
            const result_offset = try self.allocStackMemory(12, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

            const list_abi = self.builtinInternalListAbi("wasm.list_sublist.builtin_list_abi", ll.ret_layout);
            const elem_size = list_abi.elem_size;

            // new_ptr = old_ptr + actual_start * elem_size
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), actual_start) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 0);

            // new_len = actual_len
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), actual_len) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 4);

            // Encode seamless-slice cap from the source allocation pointer.
            const encoded_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitPrepareListSliceMetadata(list_local, list_abi.elements_refcounted, encoded_cap);

            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), encoded_cap) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 8);

            // Push result pointer
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
        },

        .str_count_utf8_bytes => {
            // Returns the length of the string in UTF-8 bytes
            try self.emitProcLocal(args[0]);
            // For SSO (byte 11 high bit set): length = byte 11 & 0x7F
            // For heap: length at offset 4
            // We use the simplified approach: load byte 11, check SSO bit
            const str_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), str_local) catch return error.OutOfMemory;

            // Load byte 11 (SSO tag byte)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), str_local) catch return error.OutOfMemory;
            try self.emitLoadOpSized(.i32, 1, 11);
            const tag_byte = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), tag_byte) catch return error.OutOfMemory;

            // Check if SSO: high bit set
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x80) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

            // if SSO: len = tag_byte & 0x7F; else: len = load i32 from offset 4
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(ValType.i32)) catch return error.OutOfMemory;
            // SSO path
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), tag_byte) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x7F) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            // Heap path
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), str_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 4);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

            // Result is length as i32. If ret_layout expects i64, extend.
            const ret_vt = try self.resolveValType(ll.ret_layout);
            if (ret_vt == .i64) {
                self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            }
        },

        .str_find_first => {
            const import_idx = self.str_find_first_import orelse unreachable;
            try self.emitProcLocal(args[0]);
            const source = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(source);
            try self.emitProcLocal(args[1]);
            const delimiter = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(delimiter);

            const ls = self.getLayoutStore();
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            if (ret_layout_val.tag != .struct_) unreachable;
            const record_idx = ret_layout_val.getStruct().idx;
            const record_data = ls.getStructData(record_idx);
            const fields = ls.struct_fields.sliceRange(record_data.getFields());
            if (fields.len != 3 or
                ls.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .str or
                ls.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .str or
                ls.getStructFieldLayoutByOriginalIndex(record_idx, 2) != .bool)
            {
                unreachable;
            }

            const result_size = try self.layoutStorageByteSize(ll.ret_layout);
            const result_align = try self.layoutStorageByteAlign(ll.ret_layout);
            const result_offset = try self.allocStackMemory(result_size, result_align);
            try self.emitLocalGet(source);
            try self.emitLocalGet(delimiter);
            try self.emitFpOffset(result_offset);
            try self.emitI32Const(@intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 0)));
            try self.emitI32Const(@intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 1)));
            try self.emitI32Const(@intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 2)));
            try self.emitCall(import_idx);
            try self.emitFpOffset(result_offset);
        },

        .str_drop_prefix_caseless_ascii => {
            const import_idx = self.str_drop_prefix_caseless_ascii_import orelse unreachable;
            try self.emitProcLocal(args[0]);
            const source = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(source);
            try self.emitProcLocal(args[1]);
            const prefix = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(prefix);

            const ls = self.getLayoutStore();
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            if (ret_layout_val.tag != .struct_) unreachable;
            const record_idx = ret_layout_val.getStruct().idx;
            const record_data = ls.getStructData(record_idx);
            const fields = ls.struct_fields.sliceRange(record_data.getFields());
            if (fields.len != 2 or
                ls.getStructFieldLayoutByOriginalIndex(record_idx, 0) != .str or
                ls.getStructFieldLayoutByOriginalIndex(record_idx, 1) != .bool)
            {
                unreachable;
            }

            const result_size = try self.layoutStorageByteSize(ll.ret_layout);
            const result_align = try self.layoutStorageByteAlign(ll.ret_layout);
            const result_offset = try self.allocStackMemory(result_size, result_align);
            try self.emitLocalGet(source);
            try self.emitLocalGet(prefix);
            try self.emitFpOffset(result_offset);
            try self.emitI32Const(@intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 0)));
            try self.emitI32Const(@intCast(ls.getStructFieldOffsetByOriginalIndex(record_idx, 1)));
            try self.emitCall(import_idx);
            try self.emitFpOffset(result_offset);
        },

        .str_is_eq => {
            try self.emitProcLocal(args[0]);
            const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(a);
            try self.emitProcLocal(args[1]);
            const b = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(b);
            try self.emitStrEqCall(a, b);
        },
        .str_is_eq_static_small => {
            try self.generateLLStrEqStaticSmall(args);
        },
        .str_static_small_word_eq => {
            try self.generateLLStrStaticSmallWordEq(args);
        },
        .str_static_small_word_caseless_eq => {
            try self.generateLLStrStaticSmallWordCaselessEq(args);
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
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(total);

            const result_offset = try self.allocStackMemory(12, 4);
            const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitLocalSet(result_ptr);

            const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitI32Const(0);
            try self.emitLocalSet(zero);

            try self.emitLocalGet(total);
            try self.emitI32Const(12);
            self.currentCode().append(self.allocator, Op.i32_lt_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            {
                try self.emitZeroInit(result_ptr, 12);
                try self.emitMemCopyLoop(result_ptr, zero, a_ptr, a_len);
                try self.emitMemCopyLoop(result_ptr, a_len, b_ptr, b_len);

                try self.emitLocalGet(result_ptr);
                try self.emitLocalGet(total);
                try self.emitI32Const(0x80);
                self.currentCode().append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 11) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            {
                try self.emitHeapAllocWithRefcount(total, 1, false);
                const buf = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(buf);

                try self.emitMemCopyLoop(buf, zero, a_ptr, a_len);
                try self.emitMemCopyLoop(buf, a_len, b_ptr, b_len);

                try self.emitLocalGet(result_ptr);
                try self.emitLocalGet(buf);
                try self.emitStoreOp(.i32, 0);

                try self.emitLocalGet(result_ptr);
                try self.emitLocalGet(total);
                try self.emitI32Const(1);
                self.currentCode().append(self.allocator, Op.i32_shl) catch return error.OutOfMemory;
                try self.emitStoreOp(.i32, 4);

                try self.emitLocalGet(result_ptr);
                try self.emitLocalGet(total);
                try self.emitStoreOp(.i32, 8);
            }
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

            try self.emitLocalGet(result_ptr);
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
            try self.emitCall(import_idx);
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
            try self.emitCall(import_idx);
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
            try self.emitCall(import_idx);
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
            const int_vt = try self.procLocalValType(args[1]);
            if (int_vt == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const int_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(int_local);
            const result_offset = try self.allocStackMemory(12, 4);
            try self.emitLocalGet(str_local);
            try self.emitLocalGet(int_local);
            try self.emitFpOffset(result_offset);
            try self.emitCall(import_idx);
            try self.emitFpOffset(result_offset);
        },
        .str_with_capacity => {
            const import_idx = self.str_with_capacity_import orelse unreachable;
            try self.emitProcLocal(args[0]);
            const int_vt = try self.procLocalValType(args[0]);
            if (int_vt == .i64) {
                self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }
            const int_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(int_local);
            const result_offset = try self.allocStackMemory(12, 4);
            try self.emitLocalGet(int_local);
            try self.emitFpOffset(result_offset);
            try self.emitCall(import_idx);
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
            try self.emitCall(import_idx);
        },
        .str_from_utf8 => {
            const ls = self.getLayoutStore();
            const ret_layout_val = ls.getLayout(ll.ret_layout);
            if (ret_layout_val.tag != .tag_union) unreachable;
            const tu_data = ls.getTagUnionData(ret_layout_val.getTagUnion().idx);
            const tu_layout = try WasmLayout.tagUnionLayoutWithStore(ret_layout_val.getTagUnion().idx, ls);
            const variants = ls.getTagUnionVariants(tu_data);
            var ok_disc: ?u16 = null;
            var err_disc: ?u16 = null;
            var err_record_idx: ?layout.StructIdx = null;
            var inner_disc_offset: u32 = 0;
            var inner_disc_size: u32 = 0;
            var inner_bad_utf8_disc: u32 = 0;
            for (0..variants.len) |i| {
                const v_payload = variants.get(@intCast(i)).payload_layout;
                const candidate = self.unwrapSingleFieldPayloadLayout(v_payload) orelse v_payload;
                if (candidate == .str) {
                    ok_disc = @intCast(i);
                } else {
                    err_disc = @intCast(i);
                    const err_layout = ls.getLayout(candidate);
                    switch (err_layout.tag) {
                        .struct_ => err_record_idx = err_layout.getStruct().idx,
                        .tag_union => {
                            const inner_tu = ls.getTagUnionData(err_layout.getTagUnion().idx);
                            if (try self.findBadUtf8Variant(inner_tu)) |info| {
                                err_record_idx = info.struct_idx;
                                inner_disc_offset = inner_tu.discriminant_offset.get(ls.targetUsize());
                                inner_disc_size = inner_tu.discriminant_size;
                                inner_bad_utf8_disc = info.disc;
                            }
                        },
                        else => {},
                    }
                }
            }
            const resolved_ok = ok_disc orelse wasmInvariantFmt(
                "WasmCodeGen invariant violated: str_from_utf8 had no Ok(Str) variant",
                .{},
            );
            const resolved_err = err_disc orelse wasmInvariantFmt(
                "WasmCodeGen invariant violated: str_from_utf8 had no Err variant",
                .{},
            );
            const rec_idx = err_record_idx orelse wasmInvariantFmt(
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
                const field_size = try self.layoutStorageByteSize(field.layout);
                const field_offset = try self.structFieldOffsetByOriginalIndexWasm(rec_idx, field.index);
                const is_index = switch (field_layout.tag) {
                    .scalar => field_layout.getScalar().tag == .int and switch (field_layout.getScalar().getInt()) {
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
            const resolved_index_off = index_off orelse wasmInvariantFmt(
                "WasmCodeGen invariant violated: str_from_utf8 could not resolve index offset",
                .{},
            );
            const resolved_index_size = index_size orelse wasmInvariantFmt(
                "WasmCodeGen invariant violated: str_from_utf8 could not resolve index size",
                .{},
            );
            const resolved_problem_off = problem_off orelse wasmInvariantFmt(
                "WasmCodeGen invariant violated: str_from_utf8 could not resolve problem offset",
                .{},
            );
            const resolved_problem_size = problem_size orelse wasmInvariantFmt(
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
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(tu_layout.size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(tu_layout.discriminant_offset)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(tu_layout.discriminant_size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(resolved_ok)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(resolved_err)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(index_offset)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(resolved_index_size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(problem_offset)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(resolved_problem_size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(inner_disc_offset)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(inner_disc_size)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(inner_bad_utf8_disc)) catch return error.OutOfMemory;
            try self.emitCall(import_idx);
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
            const tu_layout = try WasmLayout.tagUnionLayoutWithStore(ret_layout_val.getTagUnion().idx, ls);
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
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(disc_offset)) catch return error.OutOfMemory;
                    try self.emitCall(import_idx);
                },
                .float => |float| {
                    const import_idx = self.float_from_str_import orelse unreachable;
                    try self.emitLocalGet(input);
                    try self.emitFpOffset(result_offset);
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), float.width_bytes) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(disc_offset)) catch return error.OutOfMemory;
                    try self.emitCall(import_idx);
                },
                .int => |int| {
                    const import_idx = self.int_from_str_import orelse unreachable;
                    try self.emitLocalGet(input);
                    try self.emitFpOffset(result_offset);
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), int.width_bytes) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), if (int.signed) 1 else 0) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(disc_offset)) catch return error.OutOfMemory;
                    try self.emitCall(import_idx);
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
            else => wasmInvariantFmt(
                "WasmCodeGen invariant violated: num_to_str received non-numeric layout {s}",
                .{@tagName(self.procLocalLayoutIdx(args[0]))},
            ),
        },
        .num_from_numeral => unreachable, // Folded to a constant during Monotype lowering

        // Box operations
        .box_box => {
            // box_box(value) -> Box value (pointer to heap-allocated copy)
            const value_expr = args[0];
            const ls = self.getLayoutStore();
            const ret_layout = ls.getLayout(ll.ret_layout);

            if (ret_layout.tag == .box_of_zst) {
                _ = try self.emitProcLocal(value_expr);
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            } else {
                const box_abi = ls.builtinBoxAbi(ll.ret_layout);
                const value_size = box_abi.elem_size;
                const value_vt = try self.procLocalValType(value_expr);
                if (value_size == 0) {
                    _ = try self.emitProcLocal(value_expr);
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                } else {
                    const alignment: u32 = box_abi.elem_alignment;

                    try self.emitHeapAllocWithRefcountConst(value_size, alignment, box_abi.contains_refcounted);
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
                                self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
                                self.currentCode().append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
                            }
                            while (offset < value_size) : (offset += 1) {
                                try self.emitLocalGet(box_ptr);
                                try self.emitLocalGet(src_ptr);
                                self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
                                self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
                            }
                        } else {
                            const i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                            try self.emitLocalSet(i);

                            self.currentCode().append(self.allocator, Op.loop_) catch return error.OutOfMemory;
                            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

                            try self.emitLocalGet(box_ptr);
                            try self.emitLocalGet(i);
                            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                            try self.emitLocalGet(src_ptr);
                            try self.emitLocalGet(i);
                            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                            self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                            self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

                            try self.emitLocalGet(i);
                            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
                            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                            try self.emitLocalSet(i);

                            try self.emitLocalGet(i);
                            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(value_size)) catch return error.OutOfMemory;
                            self.currentCode().append(self.allocator, Op.i32_lt_u) catch return error.OutOfMemory;
                            self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

                            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
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
            const box_layout_idx = self.procLocalLayoutIdx(box_expr);
            const box_layout = ls.getLayout(box_layout_idx);
            const erased_box_ptr = box_layout.tag == .scalar and box_layout.getScalar().tag == .opaque_ptr;

            if (box_layout.tag == .box_of_zst or
                (erased_box_ptr and try self.layoutByteSize(ll.ret_layout) == 0))
            {
                _ = try self.emitProcLocal(box_expr);
                const result_vt = try self.resolveValType(ll.ret_layout);
                switch (result_vt) {
                    .i32 => {
                        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    },
                    .i64 => {
                        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    },
                    .f32 => {
                        self.currentCode().append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                        try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f32, 0)));
                    },
                    .f64 => {
                        self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                        try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f64, 0)));
                    },
                }
            } else {
                const elem_size = if (erased_box_ptr)
                    try self.layoutByteSize(ll.ret_layout)
                else
                    ls.builtinBoxAbi(box_layout_idx).elem_size;
                if (elem_size == 0) {
                    _ = try self.emitProcLocal(box_expr);
                    const result_vt = try self.resolveValType(ll.ret_layout);
                    switch (result_vt) {
                        .i32 => {
                            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                        },
                        .i64 => {
                            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                        },
                        .f32 => {
                            self.currentCode().append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                            try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f32, 0)));
                        },
                        .f64 => {
                            self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                            try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f64, 0)));
                        },
                    }
                } else {
                    try self.emitProcLocal(box_expr);

                    const result_vt = try self.resolveValType(ll.ret_layout);
                    const result_size = try self.layoutByteSize(ll.ret_layout);
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
                                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                                },
                                .i64 => {
                                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                                },
                                .f32 => {
                                    self.currentCode().append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                                    try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f32, 0)));
                                },
                                .f64 => {
                                    self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                                    try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f64, 0)));
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
        .erased_capture_load => {
            const capture_ptr_expr = args[0];
            const result_size = try self.layoutByteSize(ll.ret_layout);
            const result_vt = try self.resolveValType(ll.ret_layout);

            if (result_size == 0) {
                _ = try self.emitProcLocal(capture_ptr_expr);
                switch (result_vt) {
                    .i32 => {
                        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    },
                    .i64 => {
                        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    },
                    .f32 => {
                        self.currentCode().append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
                        try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f32, 0)));
                    },
                    .f64 => {
                        self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
                        try self.currentCode().appendSlice(self.allocator, std.mem.asBytes(&@as(f64, 0)));
                    },
                }
            } else {
                try self.emitProcLocal(capture_ptr_expr);
                const capture_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(capture_ptr);

                if (result_vt == .i32 and result_size > 4) {
                    const result_align: u32 = @intCast(@max(self.getLayoutStore().layoutSizeAlign(self.getLayoutStore().getLayout(ll.ret_layout)).alignment.toByteUnits(), 1));
                    const dst_offset = try self.allocStackMemory(result_size, result_align);
                    const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitFpOffset(dst_offset);
                    try self.emitLocalSet(dst_local);

                    try self.emitMemCopy(dst_local, 0, capture_ptr, result_size);
                    try self.emitLocalGet(dst_local);
                } else {
                    try self.emitLocalGet(capture_ptr);
                    try self.emitLoadOpSized(result_vt, result_size, 0);
                    try self.emitCanonicalizeScalarForLayout(ll.ret_layout);
                }
            }
        },

        .ptr_alloca => {
            // ptr_alloca: () -> Ptr(T). Reserve a zeroed shadow-stack slot for T
            // and leave its i32 address on the stack. Target layout is ptr(T).
            const ls = self.getLayoutStore();
            const elem_idx = ls.getLayout(ll.ret_layout).getIdx();
            const elem_size = try self.layoutByteSize(elem_idx);
            const elem_align: u32 = @intCast(@max(ls.layoutSizeAlign(ls.getLayout(elem_idx)).alignment.toByteUnits(), 1));

            const slot_offset = try self.allocStackMemory(@max(elem_size, 1), @max(elem_align, 4));
            const slot_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(slot_offset);
            try self.emitLocalSet(slot_local);
            if (elem_size > 0) {
                try self.emitZeroInit(slot_local, elem_size);
            }
            try self.emitLocalGet(slot_local);
        },
        .box_alloc_zeroed => {
            // box_alloc_zeroed: () -> Box(T). Heap cell with a zero-filled payload
            // (same allocation shape as box_box; only the payload copy differs).
            const ls = self.getLayoutStore();
            const ret_layout = ls.getLayout(ll.ret_layout);

            if (ret_layout.tag == .box_of_zst) {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            } else {
                const box_abi = ls.builtinBoxAbi(ll.ret_layout);
                const elem_size = box_abi.elem_size;
                if (elem_size == 0) {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                } else {
                    try self.emitHeapAllocWithRefcountConst(elem_size, box_abi.elem_alignment, box_abi.contains_refcounted);
                    const box_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitLocalSet(box_ptr);
                    try self.emitZeroInit(box_ptr, elem_size);
                    try self.emitLocalGet(box_ptr);
                }
            }
        },
        .ptr_store => {
            // ptr_store: (Ptr(T), T) -> {}. Copy sizeOf(T) bytes into *ptr.
            // Result is unit; leave a dummy i32 0 (zst convention).
            const value_size = try self.layoutByteSize(self.procLocalLayoutIdx(args[1]));
            const value_vt = try self.procLocalValType(args[1]);

            if (value_size == 0) {
                _ = try self.emitProcLocal(args[0]);
                self.currentCode().append(self.allocator, Op.drop) catch return error.OutOfMemory;
            } else {
                try self.emitProcLocal(args[0]);
                const ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(ptr_local);

                try self.emitProcLocal(args[1]);
                if (value_vt == .i32 and value_size > 4) {
                    // Composite value: arg is an i32 pointer into linear memory.
                    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitLocalSet(src_local);
                    try self.emitMemCopy(ptr_local, 0, src_local, value_size);
                } else {
                    try self.emitStoreToMemSized(ptr_local, 0, value_vt, value_size);
                }
            }
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        },
        .ptr_load => {
            // ptr_load: (Ptr(T)) -> T. Copy sizeOf(T) bytes out of *ptr.
            // Same shape as erased_capture_load above.
            const result_size = try self.layoutByteSize(ll.ret_layout);
            const result_vt = try self.resolveValType(ll.ret_layout);

            if (result_size == 0) {
                _ = try self.emitProcLocal(args[0]);
                self.currentCode().append(self.allocator, Op.drop) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            } else {
                try self.emitProcLocal(args[0]);
                const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitLocalSet(src_ptr);

                if (result_vt == .i32 and result_size > 4) {
                    const ls = self.getLayoutStore();
                    const result_align: u32 = @intCast(@max(ls.layoutSizeAlign(ls.getLayout(ll.ret_layout)).alignment.toByteUnits(), 1));
                    const dst_offset = try self.allocStackMemory(result_size, result_align);
                    const dst_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    try self.emitFpOffset(dst_offset);
                    try self.emitLocalSet(dst_local);

                    try self.emitMemCopy(dst_local, 0, src_ptr, result_size);
                    try self.emitLocalGet(dst_local);
                } else {
                    try self.emitLocalGet(src_ptr);
                    try self.emitLoadOpSized(result_vt, result_size, 0);
                    try self.emitCanonicalizeScalarForLayout(ll.ret_layout);
                }
            }
        },
        .ptr_cast => {
            // ptr_cast: identity bits — both sides are i32 pointers.
            try self.emitProcLocal(args[0]);
        },

        // Compare — returns Ordering enum (EQ=0, GT=1, LT=2)
        .compare => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            // Determine arg type from first arg's layout
            const arg_layout = self.procLocalLayoutIdx(args[0]);
            const arg_vt = try self.procLocalValType(args[0]);

            // Determine if unsigned from layout
            const is_unsigned = switch (arg_layout) {
                .u8, .u16, .u32, .u64, .u128 => true,
                else => false,
            };

            switch (arg_vt) {
                .i32 => {
                    const a = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    const b = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;

                    switch (arg_layout) {
                        .u128, .i128, .dec => {
                            const is_signed = arg_layout == .i128 or arg_layout == .dec;
                            try self.emitI128CompareWithSignedness(a, b, .gt, is_signed);
                            try self.emitI128CompareWithSignedness(a, b, .lt, is_signed);
                            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
                            self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                            return;
                        },
                        else => {},
                    }

                    // gt_flag = (a > b) ? 1 : 0
                    // gt_flag
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, if (is_unsigned) Op.i32_gt_u else Op.i32_gt_s) catch return error.OutOfMemory;
                    // lt_flag * 2
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, if (is_unsigned) Op.i32_lt_u else Op.i32_lt_s) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                    // result = gt_flag + lt_flag * 2
                    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                },
                .i64 => {
                    const a = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    const b = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, if (is_unsigned) Op.i64_gt_u else Op.i64_gt_s) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, if (is_unsigned) Op.i64_lt_u else Op.i64_lt_s) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                },
                .f32 => {
                    const a = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
                    const b = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.f32_gt) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.f32_lt) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                },
                .f64 => {
                    const a = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
                    const b = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.f64_gt) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), a) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), b) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.f64_lt) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                },
            }
        },

        // Crash
        .crash => {
            self.currentCode().append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },

        // Integer try conversions — return Result(TargetInt, {}) tag union
        // Layout: payload at offset 0, discriminant (1 byte) after payload. Ok=1, Err=0.
        // Narrowing i32 → smaller signed
        .i32_to_i8_try, .i16_to_i8_try, .u16_to_i8_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            // Check: val >= -128 && val <= 127
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -128) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 127) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u8_to_i8_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            // u8 → i8: check val <= 127
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 127) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        // Narrowing to u8
        .i32_to_u8_try, .i16_to_u8_try, .u16_to_u8_try, .i8_to_u8_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 255) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        // Narrowing to i16
        .i32_to_i16_try, .u32_to_i16_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 2, 2);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -32768) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 32767) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 2, 2);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u16_to_i16_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 2, 2);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 32767) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 2, 2);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        // Narrowing to u16
        .i32_to_u16_try, .u32_to_u16_try, .i16_to_u16_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 2, 2);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 65535) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 2, 2);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        // i32 <-> u32 try
        .i32_to_u32_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 4, 4);
            // i32 → u32: check val >= 0
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 4, 4);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u32_to_i32_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 4, 4);
            // u32 → i32: check high bit is 0 (val <= 0x7FFFFFFF)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 4, 4);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u32_to_i8_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 127) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u32_to_u8_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i32, 1, 1);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 255) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i32, 1, 1);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
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
                self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
                const r = try self.emitIntTryResult(.i64, payload_size, disc_offset);
                self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
                try self.emitIntTryOk(r.result_local, r.val_local, .i64, payload_size, disc_offset);
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
            } else {
                const r = try self.emitIntTryResult(.i32, payload_size, disc_offset);
                self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
                try self.emitIntTryOk(r.result_local, r.val_local, .i32, payload_size, disc_offset);
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
            }
        },
        // i64 → narrowing try conversions
        .i64_to_i8_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 1, 1);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), -128) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 127) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 1, 1);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_i16_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 2, 2);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), -32768) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32767) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 2, 2);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_i32_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 4, 4);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), -2147483648) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 2147483647) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 4, 4);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_u8_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 1, 1);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 255) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 1, 1);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_u16_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 2, 2);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 65535) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 2, 2);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_u32_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 4, 4);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 4294967295) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 4, 4);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .i64_to_u64_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 8, 8);
            // i64 → u64: check val >= 0
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 8, 8);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        // u64 → narrowing try conversions
        .u64_to_i8_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 1, 1);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 127) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 1, 1);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_i16_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 2, 2);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 32767) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 2, 2);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_i32_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 4, 4);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 2147483647) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 4, 4);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_i64_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 8, 8);
            // u64 → i64: check high bit is 0
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 8, 8);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_u8_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 1, 1);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 255) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 1, 1);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_u16_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 2, 2);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 65535) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 2, 2);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
        },
        .u64_to_u32_try => {
            try self.emitProcLocal(args[0]);
            const r = try self.emitIntTryResult(.i64, 4, 4);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.val_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 4294967295) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_le_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            try self.emitIntTryOk(r.result_local, r.val_local, .i64, 4, 4);
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), r.result_local) catch return error.OutOfMemory;
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
            const src_vt = try self.procLocalValType(args[0]);
            if (src_vt == .i32) {
                self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xFF) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i128_to_i16_wrap,
        .i128_to_u16_wrap,
        .u128_to_i16_wrap,
        .u128_to_u16_wrap,
        => {
            try self.emitProcLocal(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xFFFF) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i128_to_i32_wrap,
        .i128_to_u32_wrap,
        .u128_to_i32_wrap,
        .u128_to_u32_wrap,
        => {
            try self.emitProcLocal(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            // high_f64 = (f64)high * 2^64
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 8);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory; // 2^64
            self.currentCode().append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
            // low_f64 = (f64)(u64)low
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            // result = high_f64 + low_f64
            self.currentCode().append(self.allocator, Op.f64_add) catch return error.OutOfMemory;
        },
        .u128_to_f64 => {
            // Same as i128 but high word is unsigned
            try self.emitProcLocal(args[0]);
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 8);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_add) catch return error.OutOfMemory;
        },
        .i128_to_f32 => {
            // Convert via f64 then demote
            try self.emitProcLocal(args[0]);
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 8);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_add) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },
        .u128_to_f32 => {
            try self.emitProcLocal(args[0]);
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 8);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            self.currentCode().appendSlice(self.allocator, &@as([8]u8, @bitCast(@as(f64, 18446744073709551616.0)))) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src) catch return error.OutOfMemory;
            try self.emitLoadOp(.i64, 0);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_add) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },
        // float → i128/u128 truncating conversions
        .f64_to_i128_trunc => {
            try self.emitProcLocal(args[0]);
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            const result_offset = try self.allocStackMemory(16, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            try self.emitF64ToI128(val, result_local, true, 0);
            try self.emitLocalGet(result_local);
        },
        .f64_to_u128_trunc => {
            try self.emitProcLocal(args[0]);
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            const result_offset = try self.allocStackMemory(16, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            try self.emitF64ToI128(val, result_local, false, 0);
            try self.emitLocalGet(result_local);
        },
        .f32_to_i128_trunc => {
            // Promote f32 to f64, then use f64_to_i128 logic
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            const result_offset = try self.allocStackMemory(16, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            try self.emitF64ToI128(val, result_local, true, 0);
            try self.emitLocalGet(result_local);
        },
        .f32_to_u128_trunc => {
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            const result_offset = try self.allocStackMemory(16, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            try self.emitF64ToI128(val, result_local, false, 0);
            try self.emitLocalGet(result_local);
        },
        // 128-bit → Dec conversions: multiply by 10^18, check overflow
        .u128_to_dec_try_unsafe, .i128_to_dec_try_unsafe => {
            const offsets = self.tryUnsafeOffsets(ll.ret_layout);
            const is_signed = ll.op == .i128_to_dec_try_unsafe;
            const import_idx = if (is_signed) self.i128_to_dec_import else self.u128_to_dec_import;

            // Generate the 128-bit value (pointer to 16 bytes in stack memory)
            try self.emitProcLocal(args[0]);
            const val_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(val_ptr);

            // Allocate result: { success: U8, val_or_memory_garbage: Dec }
            const result_offset = try self.allocStackMemory(17, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitLocalSet(result_local);

            // Call host function: (val_ptr, result_ptr) -> i32 (success)
            try self.emitLocalGet(val_ptr);
            try self.emitLocalGet(result_local);
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(offsets.value)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitCall(import_idx orelse unreachable);

            // Store success flag.
            const success_flag = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(success_flag);
            try self.emitLocalGet(result_local);
            try self.emitLocalGet(success_flag);
            self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offsets.success) catch return error.OutOfMemory; // offset

            // Push result pointer
            try self.emitLocalGet(result_local);
        },

        // Decimal conversions: int → Dec (multiply by 10^18)
        .u8_to_dec, .u16_to_dec, .u32_to_dec => {
            // Unsigned small int → Dec: zero-extend to i64, multiply by 10^18
            try self.emitProcLocal(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dec_factor) catch return error.OutOfMemory;
            try self.emitI64MulToI128(val, dec_factor);
        },
        .u64_to_dec => {
            // u64 → Dec: already i64
            try self.emitProcLocal(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dec_factor) catch return error.OutOfMemory;
            try self.emitI64MulToI128(val, dec_factor);
        },
        .i8_to_dec, .i16_to_dec, .i32_to_dec => {
            // Signed small int → Dec: sign-extend to i64, multiply by 10^18
            try self.emitProcLocal(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dec_factor) catch return error.OutOfMemory;
            try self.emitI64MulToI128Signed(val, dec_factor);
        },
        .i64_to_dec => {
            // i64 → Dec: already i64
            try self.emitProcLocal(args[0]);
            const val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            const dec_factor = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dec_factor) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 0);
            // high word = 0
            try self.emitLocalGet(divisor_local);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 8);

            // Call roc_i128_div_s(dec_ptr, divisor_ptr, result_ptr)
            try self.emitI128HostBinOp(dec_local, divisor_local, self.i128_div_s_import orelse unreachable);
            // Result is an i32 pointer to the 16-byte quotient; load low i64
            try self.emitLoadOp(.i64, 0);

            // Truncate to target size
            switch (ll.op) {
                .dec_to_i64_trunc, .dec_to_u64_trunc => {},
                .dec_to_i32_trunc, .dec_to_u32_trunc => {
                    self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                },
                .dec_to_i16_trunc, .dec_to_i8_trunc, .dec_to_u16_trunc, .dec_to_u8_trunc => {
                    self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                    const mask: i32 = switch (ll.op) {
                        .dec_to_i8_trunc, .dec_to_u8_trunc => 0xFF,
                        .dec_to_i16_trunc, .dec_to_u16_trunc => 0xFFFF,
                        else => unreachable,
                    };
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), mask) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 0);
            try self.emitLocalGet(divisor_local);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitStoreOp(.i64, 8);

            try self.emitI128HostBinOp(dec_local, divisor_local, self.i128_div_s_import orelse unreachable);
        },
        .dec_to_f64 => {
            // Dec → f64: load i128 as i64 (low word), convert to f64, divide by 10^18.0
            try self.emitProcLocal(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            // 10^18 as f64 bytes (IEEE 754 double for 1e18)
            const dec_f64_bytes = @as([8]u8, @bitCast(@as(f64, 1_000_000_000_000_000_000.0)));
            self.currentCode().appendSlice(self.allocator, &dec_f64_bytes) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_div) catch return error.OutOfMemory;
        },
        .dec_to_f32_wrap => {
            // Dec → f32: same approach as f64, then demote
            try self.emitProcLocal(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.currentCode().append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            const dec_f64_bytes = @as([8]u8, @bitCast(@as(f64, 1_000_000_000_000_000_000.0)));
            self.currentCode().appendSlice(self.allocator, &dec_f64_bytes) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_div) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },
        // Dec try_unsafe conversions return { success, val_or_memory_garbage }.
        // The fractional part is truncated toward zero before range checking.
        .dec_to_i8_try_unsafe,
        .dec_to_i16_try_unsafe,
        .dec_to_i32_try_unsafe,
        .dec_to_i64_try_unsafe,
        .dec_to_u8_try_unsafe,
        .dec_to_u16_try_unsafe,
        .dec_to_u32_try_unsafe,
        .dec_to_u64_try_unsafe,
        => {
            try self.emitDecToIntTryUnsafe(ll.op, ll.ret_layout, args[0]);
        },
        // Dec → i128/u128: divide by 10^18
        .dec_to_i128_try_unsafe, .dec_to_u128_try_unsafe => {
            const offsets = self.tryUnsafeOffsets(ll.ret_layout);
            const is_signed = ll.op == .dec_to_i128_try_unsafe;
            const import_idx = if (is_signed) self.dec_to_i128_import else self.dec_to_u128_import;

            // Generate the Dec value (pointer to 16 bytes in stack memory)
            try self.emitProcLocal(args[0]);
            const val_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(val_ptr);

            // Allocate result: { success: U8, val_or_memory_garbage: i128/u128 }
            const result_offset = try self.allocStackMemory(17, 8);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitLocalSet(result_local);

            // Call host function: (val_ptr, result_ptr) -> i32 (success)
            try self.emitLocalGet(val_ptr);
            try self.emitLocalGet(result_local);
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(offsets.value)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitCall(import_idx orelse unreachable);

            // Store success flag.
            const success_flag = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(success_flag);
            try self.emitLocalGet(result_local);
            try self.emitLocalGet(success_flag);
            self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offsets.success) catch return error.OutOfMemory; // offset

            // Push result pointer
            try self.emitLocalGet(result_local);
        },
        // Dec → f32: convert Dec to floating point
        .dec_to_f32_try_unsafe => {
            const offsets = self.tryUnsafeOffsets(ll.ret_layout);
            const import_idx = self.dec_to_f32_import orelse unreachable;

            // Generate the Dec value (pointer to 16 bytes in stack memory)
            try self.emitProcLocal(args[0]);
            const val_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(val_ptr);

            // Allocate result: { success: U8, val_or_memory_garbage: F32 }
            const result_offset = try self.allocStackMemory(8, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitLocalSet(result_local);

            // Call host function: (val_ptr) -> f32
            try self.emitLocalGet(val_ptr);
            try self.emitCall(import_idx);
            const f32_val = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
            try self.emitLocalSet(f32_val);

            // Store f32 value.
            try self.emitLocalGet(result_local);
            try self.emitLocalGet(f32_val);
            self.currentCode().append(self.allocator, Op.f32_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offsets.value) catch return error.OutOfMemory; // offset

            // Store success.
            try self.emitLocalGet(result_local);
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offsets.success) catch return error.OutOfMemory; // offset

            // Push result pointer
            try self.emitLocalGet(result_local);
        },

        // Float try_unsafe conversions return { success, val_or_memory_garbage }.
        .f32_to_i8_try_unsafe, .f64_to_i8_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i8_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(ll.ret_layout, 1, false, true, -128.0, 128.0);
        },
        .f32_to_u8_try_unsafe, .f64_to_u8_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u8_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(ll.ret_layout, 1, false, false, 0.0, 256.0);
        },
        .f32_to_i16_try_unsafe, .f64_to_i16_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i16_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(ll.ret_layout, 2, false, true, -32768.0, 32768.0);
        },
        .f32_to_u16_try_unsafe, .f64_to_u16_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u16_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(ll.ret_layout, 2, false, false, 0.0, 65536.0);
        },
        .f32_to_i32_try_unsafe, .f64_to_i32_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i32_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(ll.ret_layout, 4, false, true, -2147483648.0, 2147483648.0);
        },
        .f32_to_u32_try_unsafe, .f64_to_u32_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u32_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(ll.ret_layout, 4, false, false, 0.0, 4294967296.0);
        },
        .f32_to_i64_try_unsafe, .f64_to_i64_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i64_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(ll.ret_layout, 8, true, true, -9223372036854775808.0, 9223372036854775808.0);
        },
        .f32_to_u64_try_unsafe, .f64_to_u64_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u64_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToIntTryUnsafe(ll.ret_layout, 8, true, false, 0.0, 18446744073709551616.0);
        },
        // 128-bit float try_unsafe: truncate, range-check, then write the result record.
        .f32_to_i128_try_unsafe, .f64_to_i128_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_i128_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToI128TryUnsafe(ll.ret_layout, true);
        },
        .f32_to_u128_try_unsafe, .f64_to_u128_try_unsafe => {
            try self.emitProcLocal(args[0]);
            if (ll.op == .f32_to_u128_try_unsafe) self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
            try self.emitFloatToI128TryUnsafe(ll.ret_layout, false);
        },
        .f64_to_f32_try_unsafe => {
            const offsets = self.tryUnsafeOffsets(ll.ret_layout);
            try self.emitProcLocal(args[0]);
            const val = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;

            const result_offset = try self.allocStackMemory(8, 4);
            const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;

            // Convert f64 to f32
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
            const f32_val = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), f32_val) catch return error.OutOfMemory;

            // Store f32 value.
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), f32_val) catch return error.OutOfMemory;
            try self.emitStoreOp(.f32, offsets.value);

            // success = !isInf(f32_val) && (!isNaN(val) || isNaN(f32_val))

            // not_inf = abs(f32_val) != inf
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), f32_val) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f32_abs) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f32_const) catch return error.OutOfMemory;
            self.currentCode().appendSlice(self.allocator, &@as([4]u8, @bitCast(std.math.inf(f32)))) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f32_ne) catch return error.OutOfMemory;

            // is_not_nan = (val == val)  (NaN != NaN)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), val) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f64_eq) catch return error.OutOfMemory;

            // is_nan_f32 = (f32_val != f32_val)
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), f32_val) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), f32_val) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.f32_ne) catch return error.OutOfMemory;

            // is_not_nan OR is_nan_f32
            self.currentCode().append(self.allocator, Op.i32_or) catch return error.OutOfMemory;

            // not_inf AND (is_not_nan OR is_nan_f32)
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

            // Store success.
            const success = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), success) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), success) catch return error.OutOfMemory;
            try self.emitStoreOpSized(.i32, 1, offsets.success);

            // Push result pointer
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), result_local) catch return error.OutOfMemory;
        },
    }
}

/// Generate numeric low-level operations (num_add, num_sub, etc.)
/// Handles both scalar and composite (i128/Dec) types.
fn emitNumericLowLevel(self: *Self, op: anytype, args: []const ProcLocalId, ret_layout: layout.Idx) Allocator.Error!void {
    const operand_layout = self.procLocalLayoutIdx(args[0]);
    const requires_matching_operands = switch (op) {
        .num_plus,
        .num_minus,
        .num_times,
        .num_div_by,
        .num_div_trunc_by,
        .num_rem_by,
        .num_mod_by,
        .num_is_eq,
        .num_is_gt,
        .num_is_gte,
        .num_is_lt,
        .num_is_lte,
        .num_abs_diff,
        .num_bitwise_and,
        .num_bitwise_or,
        .num_bitwise_xor,
        => true,
        else => false,
    };
    if (requires_matching_operands and args.len >= 2) {
        const rhs_layout = self.procLocalLayoutIdx(args[1]);
        if (rhs_layout != operand_layout) {
            if (rhs_layout == .zst or operand_layout == .zst) {
                self.currentCode().append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
                return;
            }
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "wasm numeric lowering invariant violated: operand layouts differ for {s}: lhs={s} rhs={s}",
                    .{ @tagName(op), @tagName(operand_layout), @tagName(rhs_layout) },
                );
            }
            unreachable;
        }
    }

    // Check for composite types (i128/Dec)
    const is_shift = op == .num_shift_left_by or op == .num_shift_right_by or op == .num_shift_right_zf_by;
    if (!is_shift and (try self.isCompositeLocal(args[0]) or try self.isCompositeLayout(operand_layout))) {
        return self.emitCompositeNumericOp(op, args, ret_layout, operand_layout);
    }
    // I128/U128 shifts: LHS is composite but RHS is U8 — needs dedicated handling.
    if (is_shift and (try self.isCompositeLocal(args[0]) or try self.isCompositeLayout(operand_layout))) {
        return self.emitI128Shift(op, args);
    }

    if (op == .num_negate and try self.isCompositeLayout(operand_layout)) {
        return self.emitCompositeI128Negate(args[0], ret_layout);
    }

    const vt = try self.procLocalValType(args[0]);
    const layout_idx = operand_layout;

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
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_div_by => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const is_unsigned = isUnsignedLayout(operand_layout);
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_div_u else Op.i32_div_s,
                .i64 => if (is_unsigned) Op.i64_div_u else Op.i64_div_s,
                .f32 => Op.f32_div,
                .f64 => Op.f64_div,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_div_trunc_by => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const is_unsigned = isUnsignedLayout(operand_layout);
            const wasm_op: u8 = switch (vt) {
                .i32 => if (is_unsigned) Op.i32_div_u else Op.i32_div_s,
                .i64 => if (is_unsigned) Op.i64_div_u else Op.i64_div_s,
                .f32 => Op.f32_div,
                .f64 => Op.f64_div,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
            switch (vt) {
                .f32 => self.currentCode().append(self.allocator, Op.f32_trunc) catch return error.OutOfMemory,
                .f64 => self.currentCode().append(self.allocator, Op.f64_trunc) catch return error.OutOfMemory,
                .i32, .i64 => {},
            }
        },
        .num_rem_by => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const is_unsigned = isUnsignedLayout(operand_layout);
            switch (vt) {
                .i32 => self.currentCode().append(self.allocator, if (is_unsigned) Op.i32_rem_u else Op.i32_rem_s) catch return error.OutOfMemory,
                .i64 => self.currentCode().append(self.allocator, if (is_unsigned) Op.i64_rem_u else Op.i64_rem_s) catch return error.OutOfMemory,
                .f32, .f64 => try self.emitFloatMod(vt),
            }
        },
        .num_negate => {
            switch (vt) {
                .i32 => {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    try self.emitProcLocal(args[0]);
                    self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    try self.emitProcLocal(args[0]);
                    self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                },
                .f32 => {
                    try self.emitProcLocal(args[0]);
                    self.currentCode().append(self.allocator, Op.f32_neg) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.emitProcLocal(args[0]);
                    self.currentCode().append(self.allocator, Op.f64_neg) catch return error.OutOfMemory;
                },
            }
        },
        .num_is_eq => {
            // Check for structural equality (strings, lists, records, etc.)
            const lay_idx = self.procLocalLayoutIdx(args[0]);
            if (lay_idx == .str or try self.isCompositeLayout(lay_idx)) {
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
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_abs => {
            switch (vt) {
                .f32 => {
                    try self.emitProcLocal(args[0]);
                    self.currentCode().append(self.allocator, Op.f32_abs) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.emitProcLocal(args[0]);
                    self.currentCode().append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
                },
                .i32 => {
                    // abs(x) = select(x, -x, x >= 0)
                    try self.emitProcLocal(args[0]);
                    const temp = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
                    // Stack: [x]. Compute -x.
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                    // Stack: [x, -x]. Compute condition: x >= 0.
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_ge_s) catch return error.OutOfMemory;
                    // select(x, -x, x >= 0) — returns x if true, -x if false
                    self.currentCode().append(self.allocator, Op.select) catch return error.OutOfMemory;
                },
                .i64 => {
                    try self.emitProcLocal(args[0]);
                    const temp = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), temp) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_ge_s) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.select) catch return error.OutOfMemory;
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
                    try self.emitBuiltinCall(.i8_mod_by, self.i8_mod_by_import);
                },
                .u8 => {
                    try self.emitBuiltinCall(.u8_mod_by, self.u8_mod_by_import);
                },
                .i16 => {
                    try self.emitBuiltinCall(.i16_mod_by, self.i16_mod_by_import);
                },
                .u16 => {
                    try self.emitBuiltinCall(.u16_mod_by, self.u16_mod_by_import);
                },
                .i32 => {
                    try self.emitBuiltinCall(.i32_mod_by, self.i32_mod_by_import);
                },
                .u32 => {
                    try self.emitBuiltinCall(.u32_mod_by, self.u32_mod_by_import);
                },
                .i64 => {
                    try self.emitBuiltinCall(.i64_mod_by, self.i64_mod_by_import);
                },
                .u64 => {
                    try self.emitBuiltinCall(.u64_mod_by, self.u64_mod_by_import);
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
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs) catch return error.OutOfMemory;
                    try self.emitProcLocal(args[1]);
                    const rhs = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs) catch return error.OutOfMemory;

                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, if (is_unsigned) Op.i32_ge_u else Op.i32_ge_s) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, @intFromEnum(WasmModule.BlockType.i32)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                },
                .i64 => {
                    try self.emitProcLocal(args[0]);
                    const lhs = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs) catch return error.OutOfMemory;
                    try self.emitProcLocal(args[1]);
                    const rhs = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs) catch return error.OutOfMemory;

                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, if (is_unsigned) Op.i64_ge_u else Op.i64_ge_s) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, @intFromEnum(WasmModule.BlockType.i64)) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), rhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), lhs) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
                },
                .f32 => {
                    try self.emitProcLocal(args[0]);
                    try self.emitProcLocal(args[1]);
                    self.currentCode().append(self.allocator, Op.f32_sub) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.f32_abs) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.emitProcLocal(args[0]);
                    try self.emitProcLocal(args[1]);
                    self.currentCode().append(self.allocator, Op.f64_sub) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
                },
            }
        },
        .num_shift_left_by => {
            const shift_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitProcLocal(args[1]);
            try self.emitLocalSet(shift_local);

            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), shift_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(shiftBitWidth(layout_idx))) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(if (vt == .i64) WasmModule.BlockType.i64 else WasmModule.BlockType.i32)) catch return error.OutOfMemory;

            if (vt == .i64) {
                self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            } else {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            }

            self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.emitProcLocal(args[0]);
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), shift_local) catch return error.OutOfMemory;
            if (vt == .i64) self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_shl,
                .i64 => Op.i64_shl,
                .f32, .f64 => unreachable,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .num_shift_right_by => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            if (vt == .i64) self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_shr_s,
                .i64 => Op.i64_shr_s,
                .f32, .f64 => unreachable,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_shift_right_zf_by => {
            const shift_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitProcLocal(args[1]);
            try self.emitLocalSet(shift_local);

            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), shift_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(shiftBitWidth(layout_idx))) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(if (vt == .i64) WasmModule.BlockType.i64 else WasmModule.BlockType.i32)) catch return error.OutOfMemory;

            if (vt == .i64) {
                self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            } else {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            }

            self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.emitProcLocal(args[0]);
            if (shiftNeedsZeroFillMask(layout_idx) and vt == .i32) {
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                const mask: i32 = if (layout_idx == .i8) 0xFF else 0xFFFF;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), mask) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), shift_local) catch return error.OutOfMemory;
            if (vt == .i64) self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_shr_u,
                .i64 => Op.i64_shr_u,
                .f32, .f64 => unreachable,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .num_bitwise_and => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_and,
                .i64 => Op.i64_and,
                .f32, .f64 => unreachable,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_bitwise_or => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_or,
                .i64 => Op.i64_or,
                .f32, .f64 => unreachable,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_bitwise_xor => {
            try self.emitProcLocal(args[0]);
            try self.emitProcLocal(args[1]);
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_xor,
                .i64 => Op.i64_xor,
                .f32, .f64 => unreachable,
            };
            self.currentCode().append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_bitwise_not => {
            // Bitwise NOT has no dedicated wasm opcode; compute value ^ -1.
            try self.emitProcLocal(args[0]);
            switch (vt) {
                .i32 => {
                    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), -1) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i32_xor) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, self.currentCode(), -1) catch return error.OutOfMemory;
                    self.currentCode().append(self.allocator, Op.i64_xor) catch return error.OutOfMemory;
                },
                .f32, .f64 => unreachable,
            }
        },
        else => unreachable,
    }
}

/// Generate string equality comparison.
/// Both lhs and rhs should produce i32 pointers to 12-byte RocStr values.
fn emitStrEq(self: *Self, lhs: ProcLocalId, rhs: ProcLocalId, negate: bool) Allocator.Error!void {
    // Generate both string expressions, store to locals
    try self.emitProcLocal(lhs);
    const lhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(lhs_local);

    try self.emitProcLocal(rhs);
    const rhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(rhs_local);

    try self.emitStrEqCall(lhs_local, rhs_local);

    // If negate, flip the result
    if (negate) {
        self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
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
        try self.emitListEqCall(lhs_local, rhs_local, .list_str_eq, self.list_str_eq_import, null);
    } else {
        const ls = self.getLayoutStore();
        const elem_l = ls.getLayout(elem_layout);
        if (elem_l.tag == .list) {
            const inner_elem_layout = elem_l.getIdx();
            const inner_elem_size = try self.layoutByteSize(inner_elem_layout);
            try self.emitListEqCall(lhs_local, rhs_local, .list_list_eq, self.list_list_eq_import, inner_elem_size);
        } else if (builtinInternalLayoutContainsRefcounted(ls, "wasm.emitListEqWithElemLayout.builtin_elem_rc", elem_layout)) {
            // Composite elements with refcounted fields: inline structural loop
            const elem_size = try self.layoutByteSize(elem_layout);
            try self.emitListEqLoop(lhs_local, rhs_local, elem_layout, elem_size);
        } else {
            const elem_size = try self.layoutByteSize(elem_layout);
            try self.emitListEqCall(lhs_local, rhs_local, .list_eq, self.list_eq_import, elem_size);
        }
    }

    // If negate, flip the result
    if (negate) {
        self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    }
}

/// Generate a RocStr for a string literal.
/// On wasm32, RocStr is 12 bytes: { ptr/bytes[0..3], encoded cap/bytes[4..7], len/bytes[8..11] }.
/// Small strings (≤11 bytes) use SSO: bytes inline, byte 11 = len | 0x80.
/// Large strings (>11 bytes) use a data segment in linear memory.
fn generateStrLiteral(self: *Self, literal: LIR.StrLiteral) Allocator.Error!void {
    const str_bytes = self.store.getStringLiteral(literal);
    const backing_bytes = self.store.getStringLiteralBacking(literal);
    const whole_backing = literal.offset == 0 and @as(usize, literal.len) == backing_bytes.len;
    const len = str_bytes.len;

    // Allocate 12 bytes on stack frame for the RocStr struct
    const base_offset = try self.allocStackMemory(12, 4);
    const base_local = self.fp_local;

    if (backing_bytes.len <= 11 and len <= 11) {
        // Small string optimization (SSO)
        // Store string bytes inline in the 12-byte struct
        // First, zero out the 12 bytes (3 × i32.store)
        for (0..3) |i| {
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, base_offset + @as(u32, @intCast(i)) * 4);
        }

        // Store string bytes one at a time
        for (str_bytes, 0..) |byte, i| {
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(byte)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
            // alignment = 0 (byte-aligned)
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            // offset
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_offset + @as(u32, @intCast(i))) catch return error.OutOfMemory;
        }

        // Store SSO marker: byte 11 = len | 0x80
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @as(i32, @intCast(len)) | @as(i32, 0x80)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_offset + 11) catch return error.OutOfMemory; // offset
    } else {
        const data_address = try self.staticStrDataOffset(literal.backing);

        // Store ptr (offset 0)
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        try self.emitDataAddressConst(data_address, @intCast(literal.offset));
        try self.emitStoreOp(.i32, base_offset);

        // Store encoded capacity (offset 4)
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        if (whole_backing) {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(len << 1)) catch return error.OutOfMemory;
        } else {
            try self.emitDataAddressConst(data_address, 1);
        }
        try self.emitStoreOp(.i32, base_offset + 4);

        // Store len (offset 8)
        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(len)) catch return error.OutOfMemory;
        try self.emitStoreOp(.i32, base_offset + 8);
    }

    // Push pointer to the RocStr on the stack
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
    if (base_offset > 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(base_offset)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    }
}

/// Generate a RocList(U8) for a byte-list literal.
/// On wasm32, RocList is 12 bytes: { ptr/bytes[0..3], len/bytes[4..7], encoded cap/bytes[8..11] }.
fn generateBytesLiteral(self: *Self, literal: LIR.StrLiteral) Allocator.Error!void {
    const bytes = self.store.getStringLiteral(literal);
    const backing_bytes = self.store.getStringLiteralBacking(literal);
    const whole_backing = literal.offset == 0 and @as(usize, literal.len) == backing_bytes.len;

    const base_offset = try self.allocStackMemory(12, 4);
    const base_local = self.fp_local;

    if (bytes.len == 0) {
        for (0..3) |i| {
            self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, base_offset + @as(u32, @intCast(i)) * 4);
        }
    } else {
        const data_address = try self.staticStrDataOffset(literal.backing);

        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        try self.emitDataAddressConst(data_address, @intCast(literal.offset));
        try self.emitStoreOp(.i32, base_offset);

        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(bytes.len)) catch return error.OutOfMemory;
        try self.emitStoreOp(.i32, base_offset + 4);

        self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
        if (whole_backing) {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(bytes.len << 1)) catch return error.OutOfMemory;
        } else {
            try self.emitDataAddressConst(data_address, 1);
        }
        try self.emitStoreOp(.i32, base_offset + 8);
    }

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
    if (base_offset > 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(base_offset)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    }
}

fn staticStrDataOffset(self: *Self, backing_idx: base.StringLiteral.Idx) Allocator.Error!DataAddress {
    const key: u32 = @intFromEnum(backing_idx);
    if (self.static_str_offsets.get(key)) |offset| return offset;

    const backing_bytes = self.store.getString(backing_idx);
    var segment_data = std.ArrayList(u8).empty;
    defer segment_data.deinit(self.allocator);
    try segment_data.appendNTimes(self.allocator, 0, 4);
    try segment_data.appendSlice(self.allocator, backing_bytes);
    const segment_name = try self.allocStaticDataName(".rodata.roc_str");
    const symbol_name = try self.allocStaticDataName("roc.str");
    const data_address = try self.addStaticDataSymbol(
        segment_data.items,
        4,
        segment_name,
        symbol_name,
        4,
        @intCast(backing_bytes.len),
    );
    try self.static_str_offsets.put(key, data_address);
    return data_address;
}

/// Generate code for str_concat: concatenate multiple RocStr values into one.
/// Each sub-expression produces a RocStr pointer (12 bytes: ptr/bytes, encoded cap/bytes, len/bytes).
fn emitExtractStrPtrLen(self: *Self, str_local: u32, ptr_local: u32, len_local: u32) Allocator.Error!void {
    // Load byte 11 to check SSO bit
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), str_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 11) catch return error.OutOfMemory; // offset = 11

    const sso_marker = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), sso_marker) catch return error.OutOfMemory;

    // Check if SSO: bit 7 set
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x80) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

    // if (is_sso)
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // SSO path: len = sso_marker & 0x7F, ptr = str_local (bytes inline)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), sso_marker) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x7F) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), len_local) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), str_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), ptr_local) catch return error.OutOfMemory;

    // else — heap path
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;

    // len = *(str_local + 8)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), str_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory; // align = 2
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 8) catch return error.OutOfMemory; // offset = 8
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), len_local) catch return error.OutOfMemory;

    // ptr = *(str_local + 0)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), str_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory; // align = 2
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // offset = 0
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), ptr_local) catch return error.OutOfMemory;

    // end if
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Emit a byte-by-byte copy loop: memcpy(dst_base + dst_offset, src_ptr, len).
/// Uses a wasm block+loop construct with a counter local.
fn emitMemCopyLoop(self: *Self, dst_base_local: u32, dst_offset_local: u32, src_ptr_local: u32, len_local: u32) Allocator.Error!void {
    const loop_i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

    // loop_i = 0
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), loop_i) catch return error.OutOfMemory;

    // block (void)
    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // loop (void)
    self.currentCode().append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // if loop_i >= len, break
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), loop_i) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), len_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory; // break out of block

    // dst address = dst_base + dst_offset + loop_i
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dst_base_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), dst_offset_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), loop_i) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

    // src value = *(src_ptr + loop_i)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), src_ptr_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), loop_i) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align = 0
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // offset = 0

    // store byte
    self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align = 0
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // offset = 0

    // loop_i++
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), loop_i) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), loop_i) catch return error.OutOfMemory;

    // br back to loop
    self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // continue loop

    // end loop
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    // end block
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Build a heap-format RocStr on the stack frame from ptr and len locals.
/// Leaves a pointer to the 12-byte RocStr on the wasm value stack.
fn buildHeapRocStr(self: *Self, ptr_local: u32, len_local: u32) Allocator.Error!void {
    const result_offset = try self.allocStackMemory(12, 4);
    const base_local = self.fp_local;

    // Store ptr (offset 0)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), ptr_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, result_offset);

    // Store encoded capacity (offset 4)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), len_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_shl) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, result_offset + 4);

    // Store len (offset 8)
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), base_local) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), len_local) catch return error.OutOfMemory;
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

    const value_vt = try self.procLocalValType(value);
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
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), shift_amount) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shl) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), shift_amount) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, if (is_signed) Op.i64_shr_s else Op.i64_shr_u) catch return error.OutOfMemory;
            try self.emitLocalSet(low);
        } else {
            try self.emitLocalGet(raw);
            try self.emitLocalSet(low);
        }

        const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
        if (is_signed) {
            try self.emitLocalGet(low);
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 63) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
        } else {
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
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
            self.currentCode().append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        } else {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xFF) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        2 => if (is_signed) {
            self.currentCode().append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
        } else {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xFFFF) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        4 => {},
        else => unreachable,
    }
    try self.emitLocalSet(normalized);

    const low = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalGet(normalized);
    self.currentCode().append(self.allocator, if (is_signed) Op.i64_extend_i32_s else Op.i64_extend_i32_u) catch return error.OutOfMemory;
    try self.emitLocalSet(low);

    const high = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    if (is_signed) {
        try self.emitLocalGet(low);
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 63) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_shr_s) catch return error.OutOfMemory;
    } else {
        self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI64(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    }
    try self.emitLocalSet(high);

    return .{ .low = low, .high = high };
}

fn emitIntToStr(self: *Self, value: ProcLocalId, int_width_bytes: u8, is_signed: bool) Allocator.Error!void {
    const parts = try self.emitNormalizedIntParts(value, int_width_bytes, is_signed);

    if (self.externalCallsUseRelocs()) {
        const result_offset = try self.allocStackMemory(12, 4);
        const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(result_offset);
        try self.emitLocalSet(result_local);

        try self.emitLocalGet(result_local);
        try self.emitLocalGet(parts.low);
        try self.emitLocalGet(parts.high);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), int_width_bytes) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), if (is_signed) 1 else 0) catch return error.OutOfMemory;
        try self.emitLocalGet(self.roc_ops_local);
        try self.emitBuiltinCall(.int_to_str, self.int_to_str_import);
        try self.emitLocalGet(result_local);
        return;
    }

    const buf_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitHeapAllocWithRefcountConst(48, 1, false);
    try self.emitLocalSet(buf_ptr);

    try self.emitLocalGet(parts.low);
    try self.emitLocalGet(parts.high);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), int_width_bytes) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), if (is_signed) 1 else 0) catch return error.OutOfMemory;
    try self.emitLocalGet(buf_ptr);
    try self.emitBuiltinCall(.int_to_str, self.int_to_str_import);
    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(len_local);

    try self.buildHeapRocStr(buf_ptr, len_local);
}

fn emitDecToStr(self: *Self, value: ProcLocalId) Allocator.Error!void {
    try self.emitProcLocal(value);
    const dec_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(dec_ptr);

    if (self.externalCallsUseRelocs()) {
        const result_offset = try self.allocStackMemory(12, 4);
        const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(result_offset);
        try self.emitLocalSet(result_local);

        try self.emitLocalGet(result_local);
        try self.emitLocalGet(dec_ptr);
        try self.emitLoadOp(.i64, 0);
        try self.emitLocalGet(dec_ptr);
        try self.emitLoadOp(.i64, 8);
        try self.emitLocalGet(self.roc_ops_local);
        try self.emitBuiltinCall(.dec_to_str, self.dec_to_str_import);
        try self.emitLocalGet(result_local);
        return;
    }

    const buf_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitHeapAllocWithRefcountConst(48, 1, false);
    try self.emitLocalSet(buf_ptr);

    try self.emitLocalGet(dec_ptr);
    try self.emitLocalGet(buf_ptr);
    try self.emitBuiltinCall(.dec_to_str, self.dec_to_str_import);
    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(len_local);

    try self.buildHeapRocStr(buf_ptr, len_local);
}

fn emitFloatToStr(self: *Self, value: ProcLocalId, is_f32: bool) Allocator.Error!void {
    if (is_f32) {
        try self.emitProcLocal(value);
        const raw_f32 = self.storage.allocAnonymousLocal(.f32) catch return error.OutOfMemory;
        try self.emitLocalSet(raw_f32);
        try self.emitLocalGet(raw_f32);
        self.currentCode().append(self.allocator, Op.i32_reinterpret_f32) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
    } else {
        try self.emitProcLocal(value);
        const raw_f64 = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
        try self.emitLocalSet(raw_f64);
        try self.emitLocalGet(raw_f64);
        self.currentCode().append(self.allocator, Op.i64_reinterpret_f64) catch return error.OutOfMemory;
    }

    const bits_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(bits_local);

    if (self.externalCallsUseRelocs()) {
        const result_offset = try self.allocStackMemory(12, 4);
        const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(result_offset);
        try self.emitLocalSet(result_local);

        try self.emitLocalGet(result_local);
        try self.emitLocalGet(bits_local);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), if (is_f32) 1 else 0) catch return error.OutOfMemory;
        try self.emitLocalGet(self.roc_ops_local);
        try self.emitBuiltinCall(.float_to_str, self.float_to_str_import);
        try self.emitLocalGet(result_local);
        return;
    }

    const buf_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitHeapAllocWithRefcountConst(400, 1, false);
    try self.emitLocalSet(buf_ptr);

    try self.emitLocalGet(bits_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), if (is_f32) 1 else 0) catch return error.OutOfMemory;
    try self.emitLocalGet(buf_ptr);
    try self.emitBuiltinCall(.float_to_str, self.float_to_str_import);
    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(len_local);

    try self.buildHeapRocStr(buf_ptr, len_local);
}

fn emitFloatPow(self: *Self, args: []const ProcLocalId, ret_layout: layout.Idx) Allocator.Error!void {
    const is_f32 = switch (ret_layout) {
        .f32 => true,
        .f64 => false,
        else => wasmInvariantFmt(
            "WASM/codegen invariant violated: num_pow received non-float return layout {s}",
            .{@tagName(ret_layout)},
        ),
    };

    try self.emitProcLocal(args[0]);
    if (is_f32) {
        self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
    }
    try self.emitProcLocal(args[1]);
    if (is_f32) {
        self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
    }
    try self.emitI32Const(if (is_f32) 4 else 8);
    try self.emitBuiltinCall(.float_pow, self.float_pow_import);
    if (is_f32) {
        self.currentCode().append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
    }
}

fn emitFloatUnaryMath(self: *Self, arg: ProcLocalId, ret_layout: layout.Idx, kind: BuiltinKind, host_import: ?u32) Allocator.Error!void {
    const is_f32 = switch (ret_layout) {
        .f32 => true,
        .f64 => false,
        else => wasmInvariantFmt(
            "WASM/codegen invariant violated: {s} received non-float return layout {s}",
            .{ @tagName(kind), @tagName(ret_layout) },
        ),
    };

    try self.emitProcLocal(arg);
    if (is_f32) {
        self.currentCode().append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
    }
    try self.emitI32Const(if (is_f32) 4 else 8);
    try self.emitBuiltinCall(kind, host_import);
    if (is_f32) {
        self.currentCode().append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
    }
}

fn emitStrEscapeAndQuote(self: *Self, value: ProcLocalId) Allocator.Error!void {
    const import_idx = self.str_escape_and_quote_import orelse unreachable;

    try self.emitProcLocal(value);
    const str_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(str_ptr);

    const result_offset = try self.allocStackMemory(12, 4);
    try self.emitLocalGet(str_ptr);
    try self.emitFpOffset(result_offset);
    try self.emitCall(import_idx);
    try self.emitFpOffset(result_offset);
}

/// Generate str_to_utf8: convert RocStr to RocList(U8).
/// SSO strings have their bytes copied to heap memory.
/// Non-SSO strings are converted field-by-field because RocStr stores encoded
/// capacity before length, while RocList stores length before encoded capacity.
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
    self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 11) catch return error.OutOfMemory; // offset
    const last_byte = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(last_byte);

    // Check SSO flag: last_byte & 0x80
    try self.emitLocalGet(last_byte);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x80) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

    // if (is_sso)
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    {
        // SSO case: extract len = last_byte & 0x7F
        try self.emitLocalGet(last_byte);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x7F) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        const sso_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(sso_len);

        // Allocate sso_len bytes on heap via roc_alloc
        try self.emitHeapAllocWithRefcount(sso_len, 1, false);
        const heap_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(heap_ptr);

        // Copy SSO bytes from str_ptr to heap: memcpy(heap_ptr+0, str_ptr, sso_len)
        const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalSet(zero);
        try self.emitMemCopyLoop(heap_ptr, zero, str_ptr, sso_len);

        // Write RocList {heap_ptr, sso_len, sso_len << 1} to result_ptr
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
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_shl) catch return error.OutOfMemory;
        try self.emitStoreOp(.i32, 8);
    }
    // else (non-SSO)
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    {
        const str_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        const str_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        const str_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

        try self.emitLocalGet(str_ptr);
        try self.emitLoadOp(.i32, 0);
        try self.emitLocalSet(str_data);

        try self.emitLocalGet(str_ptr);
        try self.emitLoadOp(.i32, 8);
        try self.emitLocalSet(str_len);

        try self.emitLocalGet(str_ptr);
        try self.emitLoadOp(.i32, 4);
        try self.emitLocalSet(str_cap);

        // Non-SSO Str.to_utf8 returns a List(U8) that aliases the string bytes.
        // Retain the shared backing so dropping the list does not invalidate the
        // input Str that ARC still owns.
        try self.emitDataPtrIncref(str_data, 1);

        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(str_data);
        try self.emitStoreOp(.i32, 0);

        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(str_len);
        try self.emitStoreOp(.i32, 4);

        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(str_cap);
        try self.emitStoreOp(.i32, 8);
    }
    // end if
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Leave result pointer on stack
    try self.emitLocalGet(result_ptr);
}

/// Generate str_from_utf8_lossy: convert RocList(U8) to RocStr.
/// Short lists (len <= 11) produce SSO strings.
/// Longer lists are converted field-by-field because RocList stores length
/// before encoded capacity, while RocStr stores encoded capacity before length.
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
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory; // align = 4-byte
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 4) catch return error.OutOfMemory; // offset
    const len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(len);

    // Check if len <= 11 (fits in SSO)
    try self.emitLocalGet(len);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 12) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_lt_u) catch return error.OutOfMemory;

    // if (len < 12) — SSO
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    {
        // Zero-initialize the 12-byte result (so unused SSO bytes are 0)
        try self.emitZeroInit(result_ptr, 12);

        // Read data_ptr from list struct (offset 0)
        try self.emitLocalGet(list_ptr);
        self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // offset
        const data_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(data_ptr);

        // Copy len bytes from data_ptr to result_ptr: memcpy(result_ptr+0, data_ptr, len)
        const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalSet(zero);
        try self.emitMemCopyLoop(result_ptr, zero, data_ptr, len);

        // Set byte 11 = len | 0x80 (SSO marker)
        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(len);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x80) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory; // align
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 11) catch return error.OutOfMemory; // offset
    }
    // else (non-SSO)
    self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    {
        const data_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        const list_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

        try self.emitLocalGet(list_ptr);
        try self.emitLoadOp(.i32, 0);
        try self.emitLocalSet(data_ptr);

        try self.emitLocalGet(list_ptr);
        try self.emitLoadOp(.i32, 8);
        try self.emitLocalSet(list_cap);

        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(data_ptr);
        try self.emitStoreOp(.i32, 0);

        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(list_cap);
        try self.emitStoreOp(.i32, 4);

        try self.emitLocalGet(result_ptr);
        try self.emitLocalGet(len);
        try self.emitStoreOp(.i32, 8);
    }
    // end if
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Leave result pointer on stack
    try self.emitLocalGet(result_ptr);
}

/// Generate int_to_str: convert an integer value to its decimal string representation.
/// Supports all integer types including i128/u128.
fn emitLocalGet(self: *Self, local: u32) Allocator.Error!void {
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), local) catch return error.OutOfMemory;
}

/// Helper: emit local.set instruction
fn emitLocalSet(self: *Self, local: u32) Allocator.Error!void {
    self.currentCode().append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), local) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, div_op) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, trunc_op) catch return error.OutOfMemory;
    // * b
    try self.emitLocalGet(b);
    self.currentCode().append(self.allocator, mul_op) catch return error.OutOfMemory;
    // a - ...
    self.currentCode().append(self.allocator, sub_op) catch return error.OutOfMemory;
}

const StrSearchMode = enum { contains, starts_with, ends_with };

fn generateLLStrEqStaticSmall(self: *Self, args: anytype) Allocator.Error!void {
    if (args.len != 5) unreachable;

    try self.emitProcLocal(args[0]);
    const str_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(str_local);

    const ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitExtractStrPtrLen(str_local, ptr_local, len_local);

    try self.emitProcLocal(args[1]);
    if (try self.procLocalValType(args[1]) == .i64) {
        self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
    }
    const static_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(static_len);

    const words = [3]u32{
        self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory,
        self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory,
        self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory,
    };
    inline for (0..3) |index| {
        try self.emitProcLocal(args[2 + index]);
        if (try self.procLocalValType(args[2 + index]) != .i64) unreachable;
        try self.emitLocalSet(words[index]);
    }

    const result = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(result);

    try self.emitLocalGet(len_local);
    try self.emitLocalGet(static_len);
    self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
    try self.emitLocalGet(static_len);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 24) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    try self.emitLocalSet(result);

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    inline for (0..24) |index| {
        try self.emitLocalGet(static_len);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(index)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_gt_u) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        try self.emitLocalGet(ptr_local);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(index)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

        try self.emitLocalGet(words[index / 8]);
        if ((index % 8) != 0) {
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), @intCast((index % 8) * 8)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xff) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalSet(result);
        self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    try self.emitLocalGet(result);
}

const StaticSmallWordCompareMode = enum {
    exact,
    caseless,
};

fn generateLLStrStaticSmallWordEq(self: *Self, args: anytype) Allocator.Error!void {
    try self.generateLLStrStaticSmallWordCompare(args, .exact);
}

fn generateLLStrStaticSmallWordCaselessEq(self: *Self, args: anytype) Allocator.Error!void {
    try self.generateLLStrStaticSmallWordCompare(args, .caseless);
}

fn generateLLStrStaticSmallWordCompare(self: *Self, args: anytype, comptime mode: StaticSmallWordCompareMode) Allocator.Error!void {
    if (args.len != 4) unreachable;

    try self.emitProcLocal(args[0]);
    const str_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(str_local);

    const ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitExtractStrPtrLen(str_local, ptr_local, len_local);

    try self.emitProcLocal(args[1]);
    if (try self.procLocalValType(args[1]) == .i64) {
        self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
    }
    const offset_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(offset_local);

    try self.emitProcLocal(args[2]);
    if (try self.procLocalValType(args[2]) == .i64) {
        self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
    }
    const active_len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(active_len_local);

    try self.emitProcLocal(args[3]);
    if (try self.procLocalValType(args[3]) != .i64) unreachable;
    const word_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(word_local);

    const result = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(result);

    try self.emitLocalGet(active_len_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 8) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
    try self.emitLocalGet(offset_local);
    try self.emitLocalGet(len_local);
    self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitLocalGet(active_len_local);
    try self.emitLocalGet(len_local);
    try self.emitLocalGet(offset_local);
    self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    try self.emitLocalSet(result);

    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    const runtime_byte_local = if (mode == .caseless) self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory else undefined;
    const static_byte_local = if (mode == .caseless) self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory else undefined;

    inline for (0..8) |index| {
        try self.emitLocalGet(active_len_local);
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(index)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_gt_u) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        try self.emitLocalGet(ptr_local);
        try self.emitLocalGet(offset_local);
        if (index != 0) {
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(index)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        if (mode == .caseless) {
            try self.emitLocalSet(runtime_byte_local);
        }

        try self.emitLocalGet(word_local);
        if (index != 0) {
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), @intCast(index * 8)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i64_shr_u) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0xff) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        switch (mode) {
            .exact => {
                self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
            },
            .caseless => {
                try self.emitLocalSet(static_byte_local);

                // ASCII-caseless byte equality:
                //   same byte, or bytes differ by 0x20 and the runtime byte is
                //   an ASCII letter after setting the case bit. Exact-equal
                //   non-ASCII and punctuation bytes are accepted; punctuation
                //   pairs such as "_" and "\x7f" are rejected.
                try self.emitLocalGet(runtime_byte_local);
                try self.emitLocalGet(static_byte_local);
                self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;

                try self.emitLocalGet(runtime_byte_local);
                try self.emitLocalGet(static_byte_local);
                self.currentCode().append(self.allocator, Op.i32_xor) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x20) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;

                try self.emitLocalGet(runtime_byte_local);
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x20) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 'a') catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;

                try self.emitLocalGet(runtime_byte_local);
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0x20) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 'z') catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_le_u) catch return error.OutOfMemory;

                self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
            },
        }

        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalSet(result);
        self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    try self.emitLocalGet(result);
}

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
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitLocalSet(result_local);

            // if a_len >= b_len
            try self.emitLocalGet(a_len);
            try self.emitLocalGet(b_len);
            self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

            // Compare b_len bytes starting at a_ptr + (a_len - b_len) vs b_ptr
            const offset_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(a_len);
            try self.emitLocalGet(b_len);
            self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            try self.emitLocalSet(offset_local);

            const a_end_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(a_ptr);
            try self.emitLocalGet(offset_local);
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(a_end_ptr);

            try self.emitBytewiseCompare(a_end_ptr, b_ptr, b_len, result_local);

            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .contains => {
            // contains: search for b as a substring of a
            // Naive O(n*m): for each position i in [0..a_len-b_len], compare b_len bytes
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitLocalSet(result_local);

            // if b_len == 0, result = true (empty string is always contained)
            try self.emitLocalGet(b_len);
            self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            try self.emitLocalSet(result_local);
            self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;

            // if a_len >= b_len, search
            try self.emitLocalGet(a_len);
            try self.emitLocalGet(b_len);
            self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

            // search_end = a_len - b_len + 1
            const search_end = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(a_len);
            try self.emitLocalGet(b_len);
            self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(search_end);

            const search_i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
            try self.emitLocalSet(search_i);

            // block { loop {
            self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.loop_) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

            // if search_i >= search_end: break
            try self.emitLocalGet(search_i);
            try self.emitLocalGet(search_end);
            self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;

            // Compare b_len bytes at a_ptr+search_i vs b_ptr
            const cand_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(a_ptr);
            try self.emitLocalGet(search_i);
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(cand_ptr);

            const match_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitBytewiseCompare(cand_ptr, b_ptr, b_len, match_local);

            // if match: result = 1, break
            try self.emitLocalGet(match_local);
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            try self.emitLocalSet(result_local);
            self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 3) catch return error.OutOfMemory; // break out of block (past loop + block)
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

            // search_i++
            try self.emitLocalGet(search_i);
            self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(search_i);

            // br loop
            self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory; // end loop
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory; // end block

            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory; // end if a_len >= b_len
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory; // end if b_len == 0
        },
    }

    // Push result
    try self.emitLocalGet(result_local);
}

/// Compare a_ptr[0..b_len] with b_ptr[0..b_len], assuming a is long enough.
/// Sets result_local to 1 if equal, 0 otherwise.
fn emitStrPrefixCompare(self: *Self, a_ptr: u32, a_len: u32, b_ptr: u32, b_len: u32, result_local: u32) Allocator.Error!void {
    // Default result = 0
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(result_local);

    // if a_len >= b_len
    try self.emitLocalGet(a_len);
    try self.emitLocalGet(b_len);
    self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitBytewiseCompare(a_ptr, b_ptr, b_len, result_local);

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Compare len bytes at ptr_a vs ptr_b, store result (1=equal, 0=not) in result_local.
fn emitBytewiseCompare(self: *Self, ptr_a: u32, ptr_b: u32, len: u32, result_local: u32) Allocator.Error!void {
    // Assume equal
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    try self.emitLocalSet(result_local);

    const cmp_i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(cmp_i);

    // block { loop {
    self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // if cmp_i >= len: break (all bytes matched)
    try self.emitLocalGet(cmp_i);
    try self.emitLocalGet(len);
    self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;

    // Load byte from a[cmp_i]
    try self.emitLocalGet(ptr_a);
    try self.emitLocalGet(cmp_i);
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    // Load byte from b[cmp_i]
    try self.emitLocalGet(ptr_b);
    try self.emitLocalGet(cmp_i);
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    // If not equal: result = 0, break
    self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(result_local);
    self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory; // break out of block (skip if + loop + block)
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    // cmp_i++
    try self.emitLocalGet(cmp_i);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(cmp_i);

    // continue loop
    self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory; // end loop
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory; // end block
}

fn emitSetI32Local(self: *Self, local: u32, value: i32) Allocator.Error!void {
    try self.emitI32Const(value);
    try self.emitLocalSet(local);
}

fn emitSimdOp(self: *Self, opcode: u32) Allocator.Error!void {
    self.currentCode().append(self.allocator, Op.simd_prefix) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), opcode) catch return error.OutOfMemory;
}

fn emitV128Load(self: *Self, align_exponent: u32, offset: u32) Allocator.Error!void {
    try self.emitSimdOp(Op.v128_load);
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), align_exponent) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), offset) catch return error.OutOfMemory;
}

fn emitStrMatchSourceShape(self: *Self, source: ProcLocalId) Allocator.Error!StrMatchSourceShape {
    const shape = StrMatchSourceShape{
        .str = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .bytes = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .is_small = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .allocation = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
    };

    try self.emitProcLocal(source);
    try self.emitLocalSet(shape.str);
    try self.emitExtractStrPtrLen(shape.str, shape.bytes, shape.len);

    try self.emitLocalGet(shape.str);
    self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 11) catch return error.OutOfMemory;
    try self.emitI32Const(0x80);
    self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    try self.emitI32Const(0);
    self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
    try self.emitLocalSet(shape.is_small);

    try self.emitSetI32Local(shape.allocation, 0);
    try self.emitLocalGet(shape.is_small);
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    {
        const cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(shape.str);
        try self.emitLoadOp(.i32, 4);
        try self.emitLocalSet(cap);

        try self.emitLocalGet(cap);
        try self.emitI32Const(1);
        self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        {
            try self.emitLocalGet(cap);
            try self.emitLocalSet(shape.allocation);
        }
        self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
        {
            try self.emitLocalGet(shape.bytes);
            try self.emitI32Const(1);
            self.currentCode().append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
            try self.emitLocalSet(shape.allocation);
        }
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    return shape;
}

fn emitStaticBytesEqualAtOffset(
    self: *Self,
    source_bytes: u32,
    offset_local: u32,
    expected: []const u8,
    result_local: u32,
) Allocator.Error!void {
    try self.emitSetI32Local(result_local, 1);

    for (expected, 0..) |byte, index| {
        try self.emitLocalGet(result_local);

        try self.emitLocalGet(source_bytes);
        try self.emitLocalGet(offset_local);
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), @intCast(index)) catch return error.OutOfMemory;

        try self.emitI32Const(@intCast(byte));
        self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        try self.emitLocalSet(result_local);
    }
}

fn emitStrMatchLiteralAtCursor(
    self: *Self,
    source_bytes: u32,
    source_len: u32,
    cursor: u32,
    literal: []const u8,
    matched: u32,
) Allocator.Error!void {
    if (literal.len == 0) return;

    try self.emitLocalGet(matched);
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    {
        try self.emitLocalGet(source_len);
        try self.emitLocalGet(cursor);
        self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
        try self.emitI32Const(@intCast(literal.len));
        self.currentCode().append(self.allocator, Op.i32_lt_u) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        {
            try self.emitSetI32Local(matched, 0);
        }
        self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
        {
            const literal_matches = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitStaticBytesEqualAtOffset(source_bytes, cursor, literal, literal_matches);

            try self.emitLocalGet(literal_matches);
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            {
                try self.emitLocalGet(cursor);
                try self.emitI32Const(@intCast(literal.len));
                self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                try self.emitLocalSet(cursor);
            }
            self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            {
                try self.emitSetI32Local(matched, 0);
            }
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
}

fn emitStrMatchFindDelimiter(
    self: *Self,
    source_bytes: u32,
    source_len: u32,
    cursor: u32,
    delimiter: []const u8,
    found_pos: u32,
    found: u32,
) Allocator.Error!void {
    try self.emitSetI32Local(found, 0);
    try self.emitLocalGet(cursor);
    try self.emitLocalSet(found_pos);

    if (delimiter.len == 0) {
        try self.emitSetI32Local(found, 1);
        return;
    }

    try self.emitLocalGet(source_len);
    try self.emitLocalGet(cursor);
    self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitI32Const(@intCast(delimiter.len));
    self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    {
        const search_limit = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(source_len);
        try self.emitI32Const(@intCast(delimiter.len));
        self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
        try self.emitI32Const(1);
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        try self.emitLocalSet(search_limit);

        const pos = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalGet(cursor);
        try self.emitLocalSet(pos);

        const simd_width = 16;
        const mask = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.loop_) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        try self.emitLocalGet(search_limit);
        try self.emitLocalGet(pos);
        self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
        try self.emitI32Const(simd_width);
        self.currentCode().append(self.allocator, Op.i32_lt_u) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;

        try self.emitLocalGet(source_bytes);
        try self.emitLocalGet(pos);
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        try self.emitV128Load(0, 0);
        try self.emitI32Const(@intCast(delimiter[0]));
        try self.emitSimdOp(Op.i8x16_splat);
        try self.emitSimdOp(Op.i8x16_eq);
        try self.emitSimdOp(Op.i8x16_bitmask);
        try self.emitLocalSet(mask);

        try self.emitLocalGet(mask);
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        {
            try self.emitSetI32Local(found, 1);
            try self.emitLocalGet(pos);
            try self.emitLocalGet(mask);
            self.currentCode().append(self.allocator, Op.i32_ctz) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
            try self.emitLocalSet(found_pos);
            self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

        try self.emitLocalGet(pos);
        try self.emitI32Const(simd_width);
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        try self.emitLocalSet(pos);

        self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

        try self.emitLocalGet(found);
        self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.block) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.loop_) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

        try self.emitLocalGet(pos);
        try self.emitLocalGet(search_limit);
        self.currentCode().append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.br_if) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;

        const delimiter_start_matches = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitStaticBytesEqualAtOffset(source_bytes, pos, delimiter[0..1], delimiter_start_matches);

        try self.emitLocalGet(delimiter_start_matches);
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        {
            try self.emitSetI32Local(found, 1);
            try self.emitLocalGet(pos);
            try self.emitLocalSet(found_pos);
            self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

        try self.emitLocalGet(pos);
        try self.emitI32Const(1);
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        try self.emitLocalSet(pos);

        self.currentCode().append(self.allocator, Op.br) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;

        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    if (delimiter.len > 1) {
        try self.emitLocalGet(found);
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        {
            const delimiter_matches = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitStaticBytesEqualAtOffset(source_bytes, found_pos, delimiter, delimiter_matches);

            try self.emitLocalGet(delimiter_matches);
            self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            {
                try self.emitSetI32Local(found, 0);
            }
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }
}

fn emitStoreSmallStrMatchCapture(
    self: *Self,
    result_ptr: u32,
    source_bytes: u32,
    capture_start: u32,
    capture_len: u32,
) Allocator.Error!void {
    try self.emitZeroInit(result_ptr, 12);

    const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(source_bytes);
    try self.emitLocalGet(capture_start);
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(src_ptr);

    const dst_offset = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitSetI32Local(dst_offset, 0);
    try self.emitMemCopyLoop(result_ptr, dst_offset, src_ptr, capture_len);

    try self.emitLocalGet(result_ptr);
    try self.emitLocalGet(capture_len);
    try self.emitI32Const(0x80);
    self.currentCode().append(self.allocator, Op.i32_or) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 11) catch return error.OutOfMemory;
}

fn emitStoreHeapStrMatchCapture(
    self: *Self,
    result_ptr: u32,
    source_bytes: u32,
    source_allocation: u32,
    capture_start: u32,
    capture_len: u32,
) Allocator.Error!void {
    try self.emitLocalGet(result_ptr);
    try self.emitLocalGet(source_bytes);
    try self.emitLocalGet(capture_start);
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, 0);

    try self.emitLocalGet(result_ptr);
    try self.emitLocalGet(source_allocation);
    try self.emitStoreOp(.i32, 4);

    try self.emitLocalGet(result_ptr);
    try self.emitLocalGet(capture_len);
    try self.emitStoreOp(.i32, 8);
}

fn emitStrMatchCapture(
    self: *Self,
    capture: LIR.StrMatchCapture,
    source: StrMatchSourceShape,
    capture_start: u32,
    capture_end: u32,
) Allocator.Error!void {
    switch (capture) {
        .discard => {},
        .view => |target| {
            const capture_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(capture_end);
            try self.emitLocalGet(capture_start);
            self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
            try self.emitLocalSet(capture_len);

            const result_offset = try self.allocStackMemory(12, 4);
            const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitLocalSet(result_ptr);

            try self.emitLocalGet(source.is_small);
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            {
                try self.emitStoreSmallStrMatchCapture(result_ptr, source.bytes, capture_start, capture_len);
            }
            self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            {
                try self.emitStoreHeapStrMatchCapture(result_ptr, source.bytes, source.allocation, capture_start, capture_len);
            }
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

            const target_vt = try self.procLocalValType(target);
            if (target_vt != .i32) {
                wasmInvariantFmt(
                    "WASM/codegen invariant violated: string-pattern capture local {d} was not represented as i32",
                    .{@intFromEnum(target)},
                );
            }
            const target_local = try self.getOrAllocTypedLocal(target, target_vt);
            try self.emitLocalGet(result_ptr);
            try self.emitLocalSet(target_local);
        },
    }
}

fn generateStrMatch(self: *Self, str_match: anytype) Allocator.Error!u32 {
    const source = try self.emitStrMatchSourceShape(str_match.source);
    return self.generateStrMatchWithSource(source, str_match);
}

fn generateStrMatchWithSource(self: *Self, source: StrMatchSourceShape, str_match: anytype) Allocator.Error!u32 {
    const cursor = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const matched = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitSetI32Local(cursor, 0);
    try self.emitSetI32Local(matched, 1);

    try self.emitStrMatchLiteralAtCursor(
        source.bytes,
        source.len,
        cursor,
        self.store.getStringLiteral(str_match.prefix),
        matched,
    );

    const steps = self.store.getStrMatchSteps(str_match.steps);
    for (steps, 0..) |step, step_index| {
        const delimiter = self.store.getStringLiteral(step.delimiter);
        const is_tail_final_step = step_index + 1 == steps.len and str_match.end == .tail and delimiter.len == 0;

        try self.emitLocalGet(matched);
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        {
            const capture_start = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalGet(cursor);
            try self.emitLocalSet(capture_start);

            if (is_tail_final_step) {
                try self.emitStrMatchCapture(step.capture, source, capture_start, source.len);
                try self.emitLocalGet(source.len);
                try self.emitLocalSet(cursor);
            } else {
                const found_pos = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                const found = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitStrMatchFindDelimiter(source.bytes, source.len, cursor, delimiter, found_pos, found);

                try self.emitLocalGet(found);
                self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
                {
                    try self.emitSetI32Local(matched, 0);
                }
                self.currentCode().append(self.allocator, Op.@"else") catch return error.OutOfMemory;
                {
                    try self.emitStrMatchCapture(step.capture, source, capture_start, found_pos);
                    try self.emitLocalGet(found_pos);
                    try self.emitI32Const(@intCast(delimiter.len));
                    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                    try self.emitLocalSet(cursor);
                }
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            }
        }
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    switch (str_match.end) {
        .exact => {
            try self.emitLocalGet(matched);
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            {
                try self.emitLocalGet(cursor);
                try self.emitLocalGet(source.len);
                self.currentCode().append(self.allocator, Op.i32_ne) catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
                self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
                {
                    try self.emitSetI32Local(matched, 0);
                }
                self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
            }
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .tail => {},
    }

    return matched;
}

fn generateStrMatchSet(self: *Self, str_match_set: anytype) Allocator.Error!u32 {
    const source = try self.emitStrMatchSourceShape(str_match_set.source);
    const matched_arm = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitSetI32Local(matched_arm, -1);

    for (self.store.getStrMatchArms(str_match_set.arms), 0..) |arm, index| {
        try self.emitLocalGet(matched_arm);
        try self.emitI32Const(-1);
        self.currentCode().append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
        {
            const matched = try self.generateStrMatchWithSource(source, arm);
            try self.emitLocalGet(matched);
            self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
            {
                try self.emitSetI32Local(matched_arm, @intCast(index));
            }
            self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
        }
        self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;
    }

    return matched_arm;
}

/// Generate LowLevel list_append: create new list with one element appended.
fn generateLLListAppend(self: *Self, args: anytype, ret_layout: layout.Idx) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListAppend.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;
    const elem_layout_idx = list_abi.elem_layout_idx orelse ret_layout;

    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);
    self.currentCode().append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), list_ptr) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    const empty_list_offset = try self.allocStackMemory(12, 4);
    const empty_list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(empty_list_offset);
    try self.emitLocalSet(empty_list_ptr);
    try self.emitZeroInit(empty_list_ptr, 12);
    try self.emitLocalGet(empty_list_ptr);
    try self.emitLocalSet(list_ptr);
    self.currentCode().append(self.allocator, Op.end) catch return error.OutOfMemory;

    const elem_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    if (elem_size == 0) {
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalSet(elem_ptr);
    } else {
        const target_is_composite = try self.isCompositeLayout(elem_layout_idx);
        if (target_is_composite) {
            try self.emitProcLocal(args[1]);
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(src_local);
            const dst_offset = try self.allocStackMemory(elem_size, elem_align);
            try self.emitFpOffset(dst_offset);
            try self.emitLocalSet(elem_ptr);
            try self.emitMemCopy(elem_ptr, 0, src_local, elem_size);
        } else {
            const elem_vt = try self.resolveValType(elem_layout_idx);
            try self.emitProcLocal(args[1]);
            try self.emitConversion(try self.procLocalValType(args[1]), elem_vt);
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
    switch (self.external_calls) {
        .host_imports => {
            const import_idx = self.list_append_unsafe_import orelse unreachable;
            try self.emitLocalGet(list_ptr);
            try self.emitLocalGet(elem_ptr);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(elem_align));
            try self.emitFpOffset(result_offset);
            try self.emitCall(import_idx);
        },
        .builtin_relocs => {
            const fields = try self.loadRocListFields(list_ptr);
            try self.emitFpOffset(result_offset);
            try self.emitRocListFields(fields);
            try self.emitLocalGet(elem_ptr);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitLocalGet(self.roc_ops_local);
            try self.emitBuiltinCall(.list_append_unsafe, null);
        },
        .unconfigured => wasmInvariantFmt("WASM/codegen invariant violated: external calls not configured before list_append_unsafe", .{}),
    }
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
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    const old_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const new_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(new_len);

    // Allocate new buffer
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(new_len);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAllocWithRefcount(total_size, elem_align, list_abi.elements_refcounted);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // Store new element at position 0
    if (elem_size <= 8) {
        try self.emitLocalGet(new_data);
        try self.emitLocalGet(elem_val);
        if (elem_size <= 4) {
            self.currentCode().append(self.allocator, Op.i32_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
        } else {
            self.currentCode().append(self.allocator, Op.i64_store) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 3) catch return error.OutOfMemory;
        }
        WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    } else {
        const zero2 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalSet(zero2);
        const es = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
        try self.emitLocalSet(es);
        try self.emitMemCopyLoop(new_data, zero2, elem_val, es);
    }

    // Copy old elements at offset elem_size
    const old_byte_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(old_byte_len);

    const dst_off = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    try self.emitLocalSet(dst_off);

    try self.emitMemCopyLoop(new_data, dst_off, old_data, old_byte_len);

    try self.buildRocList(new_data, new_len);
}

/// Generate LowLevel list_concat: concatenate two lists.
fn generateLLListConcat(self: *Self, args: anytype, ret_layout: layout.Idx, unique_args: u64) Allocator.Error!void {
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
        self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        try self.emitLocalSet(new_len);

        const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
        try self.emitLocalSet(zero);

        try self.buildRocListWithCap(zero, new_len, new_len);
        return;
    }

    try self.emitProcLocal(args[0]);
    const a_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(a_ptr);
    try self.emitProcLocal(args[1]);
    const b_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(b_ptr);

    const result_offset = try self.allocStackMemory(12, 4);
    switch (self.external_calls) {
        .host_imports => {
            const import_idx = self.list_concat_import orelse unreachable;
            try self.emitLocalGet(a_ptr);
            try self.emitLocalGet(b_ptr);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(elem_align));
            try self.emitFpOffset(result_offset);
            try self.emitCall(import_idx);
        },
        .builtin_relocs => {
            const a_fields = try self.loadRocListFields(a_ptr);
            const b_fields = try self.loadRocListFields(b_ptr);
            const callbacks = try self.listElementCallbacks(list_abi);

            try self.emitFpOffset(result_offset);
            try self.emitRocListFields(a_fields);
            try self.emitRocListFields(b_fields);
            try self.emitI32Const(@intCast(elem_align));
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(callbacks.elements_refcounted));
            try self.emitI32Const(@intCast(callbacks.incref_table_idx));
            try self.emitI32Const(@intCast(callbacks.decref_table_idx));
            // One bit per list argument (bit 0 = a, bit 1 = b), as one wide
            // parameter.
            self.currentCode().append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, self.currentCode(), @intCast(unique_args & 0b11)) catch return error.OutOfMemory;
            try self.emitLocalGet(self.roc_ops_local);
            try self.emitBuiltinCall(.list_concat, null);
        },
        .unconfigured => wasmInvariantFmt("WASM/codegen invariant violated: external calls not configured before list_concat", .{}),
    }
    try self.emitFpOffset(result_offset);
}

/// Generate LowLevel list_drop_at: remove element at index, returning new list.
fn generateLLListDropAt(self: *Self, args: anytype, ret_layout: layout.Idx, unique_args: u64) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListDropAt.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    try self.emitProcLocal(args[1]);
    try self.emitConversion(try self.procLocalValType(args[1]), .i64);
    const index_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(index_local);

    const result_offset = try self.allocStackMemory(12, 4);
    switch (self.external_calls) {
        .host_imports => {
            const import_idx = self.list_drop_at_import orelse unreachable;
            try self.emitLocalGet(list_ptr);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(elem_align));
            try self.emitLocalGet(index_local);
            self.currentCode().append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            try self.emitFpOffset(result_offset);
            try self.emitCall(import_idx);
        },
        .builtin_relocs => {
            const fields = try self.loadRocListFields(list_ptr);
            const callbacks = try self.listElementCallbacks(list_abi);

            try self.emitFpOffset(result_offset);
            try self.emitRocListFields(fields);
            try self.emitI32Const(@intCast(elem_align));
            try self.emitI32Const(@intCast(elem_size));
            try self.emitLocalGet(index_local);
            try self.emitI32Const(@intCast(callbacks.elements_refcounted));
            try self.emitI32Const(@intCast(callbacks.incref_table_idx));
            try self.emitI32Const(@intCast(callbacks.decref_table_idx));
            try self.emitI32Const(updateModeImmForArg(unique_args, 0));
            try self.emitLocalGet(self.roc_ops_local);
            try self.emitBuiltinCall(.list_drop_at, null);
        },
        .unconfigured => wasmInvariantFmt("WASM/codegen invariant violated: external calls not configured before list_drop_at", .{}),
    }
    try self.emitFpOffset(result_offset);
}

/// Generate LowLevel list_reverse: create new list with elements in reverse order.
fn generateLLListReverse(self: *Self, args: anytype, ret_layout: layout.Idx, unique_args: u64) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListReverse.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    if (elem_size == 0) {
        try self.emitProcLocal(args[0]);
        return;
    }

    const elem_align = list_abi.elem_align;

    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    const result_offset = try self.allocStackMemory(12, 4);
    switch (self.external_calls) {
        .host_imports => {
            const import_idx = self.list_reverse_import orelse unreachable;
            try self.emitLocalGet(list_ptr);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(elem_align));
            try self.emitFpOffset(result_offset);
            try self.emitCall(import_idx);
        },
        .builtin_relocs => {
            const fields = try self.loadRocListFields(list_ptr);
            const callbacks = try self.listElementCallbacks(list_abi);

            try self.emitFpOffset(result_offset);
            try self.emitRocListFields(fields);
            try self.emitI32Const(@intCast(elem_align));
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(callbacks.elements_refcounted));
            try self.emitI32Const(@intCast(callbacks.incref_table_idx));
            try self.emitI32Const(@intCast(callbacks.decref_table_idx));
            try self.emitI32Const(updateModeImmForArg(unique_args, 0));
            try self.emitLocalGet(self.roc_ops_local);
            try self.emitBuiltinCall(.list_reverse, null);
        },
        .unconfigured => wasmInvariantFmt("WASM/codegen invariant violated: external calls not configured before list_reverse", .{}),
    }
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

    // encoded cap (offset 8) = len << 1
    try self.emitLocalGet(len_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_shl) catch return error.OutOfMemory;
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

    // encoded cap (offset 8)
    try self.emitLocalGet(cap_local);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_shl) catch return error.OutOfMemory;
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
    try self.emitConversion(try self.procLocalValType(args[0]), .i32);
    const cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(cap);

    // Allocate cap * elem_size bytes on heap
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(cap);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAllocWithRefcount(total_size, elem_align, list_abi.elements_refcounted);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // len = 0
    const len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(len);

    try self.buildRocListWithCap(new_data, len, cap);
}

/// Materialize an element value (`elem_arg`) into stack memory and return a
/// local holding its pointer. Caller guarantees `elem_size > 0`.
fn materializeElementPtr(self: *Self, elem_arg: anytype, elem_layout_idx: layout.Idx, elem_size: u32, elem_align: u32) Allocator.Error!u32 {
    const elem_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    if (try self.isCompositeLayout(elem_layout_idx)) {
        try self.emitProcLocal(elem_arg);
        const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitLocalSet(src_local);
        const dst_offset = try self.allocStackMemory(elem_size, elem_align);
        try self.emitFpOffset(dst_offset);
        try self.emitLocalSet(elem_ptr);
        try self.emitMemCopy(elem_ptr, 0, src_local, elem_size);
    } else {
        const elem_vt = try self.resolveValType(elem_layout_idx);
        try self.emitProcLocal(elem_arg);
        try self.emitConversion(try self.procLocalValType(elem_arg), elem_vt);
        const elem_val = self.storage.allocAnonymousLocal(elem_vt) catch return error.OutOfMemory;
        try self.emitLocalSet(elem_val);

        const elem_offset = try self.allocStackMemory(elem_size, elem_align);
        try self.emitFpOffset(elem_offset);
        try self.emitLocalSet(elem_ptr);

        try self.emitLocalGet(elem_ptr);
        try self.emitLocalGet(elem_val);
        try self.emitStoreOpSized(elem_vt, elem_size, 0);
    }
    return elem_ptr;
}

/// Materialize a list-index argument (widened to i64 for the builtin ABI).
fn materializeListIndex(self: *Self, index_arg: anytype) Allocator.Error!u32 {
    try self.emitProcLocal(index_arg);
    try self.emitConversion(try self.procLocalValType(index_arg), .i64);
    const index_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(index_local);
    return index_local;
}

/// Emit the `roc_builtins_list_replace` call (used by both `list_set` and
/// `list_replace_unsafe`). The new list is written to `out_list_offset` and the
/// displaced element to `out_element_offset` (both frame-pointer offsets).
fn emitListReplaceCall(
    self: *Self,
    list_abi: BuiltinListAbi,
    list_ptr: u32,
    index_local: u32,
    elem_ptr: u32,
    out_element_offset: u32,
    out_list_offset: u32,
    unique_args: u64,
) Allocator.Error!void {
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;
    switch (self.external_calls) {
        .host_imports => {
            const import_idx = self.list_replace_import orelse unreachable;
            try self.emitLocalGet(list_ptr);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(elem_align));
            try self.emitLocalGet(index_local);
            try self.emitLocalGet(elem_ptr);
            try self.emitFpOffset(out_element_offset);
            try self.emitFpOffset(out_list_offset);
            try self.emitCall(import_idx);
        },
        .builtin_relocs => {
            const fields = try self.loadRocListFields(list_ptr);
            const callbacks = try self.listElementCallbacks(list_abi);
            try self.emitFpOffset(out_list_offset);
            try self.emitRocListFields(fields);
            try self.emitI32Const(@intCast(elem_align));
            try self.emitLocalGet(index_local);
            try self.emitLocalGet(elem_ptr);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitFpOffset(out_element_offset);
            try self.emitI32Const(@intCast(callbacks.elements_refcounted));
            try self.emitI32Const(@intCast(callbacks.incref_table_idx));
            try self.emitI32Const(@intCast(callbacks.decref_table_idx));
            try self.emitI32Const(updateModeImmForArg(unique_args, 0));
            try self.emitLocalGet(self.roc_ops_local);
            try self.emitBuiltinCall(.list_replace, null);
        },
        .unconfigured => wasmInvariantFmt("WASM/codegen invariant violated: external calls not configured before list_replace", .{}),
    }
}

/// Generate list_set: replace the element at `index`, mutating in place when the
/// list is statically unique and copy-on-writing otherwise. Routes through the
/// shared `roc_builtins_list_replace`, so refcounted elements and allocation
/// ownership are handled identically to the other backends.
fn generateLLListSet(self: *Self, args: anytype, ret_layout: layout.Idx, unique_args: u64) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListSet.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    const result_offset = try self.allocStackMemory(12, 4);

    // ZST elements: the element write is a no-op, so list_set returns the input
    // list unchanged. (listReplace would dereference a null element pointer.)
    if (elem_size == 0) {
        const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(result_offset);
        try self.emitLocalSet(result_ptr);
        try self.emitMemCopy(result_ptr, 0, list_ptr, 12);
        try self.emitFpOffset(result_offset);
        return;
    }

    const elem_layout_idx = list_abi.elem_layout_idx orelse unreachable;
    const index_local = try self.materializeListIndex(args[1]);
    const elem_ptr = try self.materializeElementPtr(args[2], elem_layout_idx, elem_size, elem_align);
    // list_set discards the displaced element; the builtin still needs a slot.
    const out_element_offset = try self.allocStackMemory(elem_size, elem_align);

    try self.emitListReplaceCall(list_abi, list_ptr, index_local, elem_ptr, out_element_offset, result_offset, unique_args);
    try self.emitFpOffset(result_offset);
}

/// Generate list_replace_unsafe: like list_set, but returns a `{ list, prev }`
/// record. The builtin writes the new list and displaced element straight into
/// the record's fields.
fn generateLLListReplaceUnsafe(self: *Self, args: anytype, ret_layout: layout.Idx, unique_args: u64) Allocator.Error!void {
    const pair = self.resolveListElementPairLayout(ret_layout);
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListReplaceUnsafe.builtin_list_abi", pair.list_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    const result_offset = try self.allocStackMemory(pair.result_size, pair.result_align);

    // ZST elements: the result list equals the input and the prev field is
    // zero-sized, so just copy the input list into the record's list field.
    if (elem_size == 0) {
        const list_field_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(result_offset + pair.list_offset);
        try self.emitLocalSet(list_field_ptr);
        try self.emitMemCopy(list_field_ptr, 0, list_ptr, 12);
        try self.emitFpOffset(result_offset);
        return;
    }

    const elem_layout_idx = list_abi.elem_layout_idx orelse unreachable;
    const index_local = try self.materializeListIndex(args[1]);
    const elem_ptr = try self.materializeElementPtr(args[2], elem_layout_idx, elem_size, elem_align);

    try self.emitListReplaceCall(
        list_abi,
        list_ptr,
        index_local,
        elem_ptr,
        result_offset + pair.elem_offset,
        result_offset + pair.list_offset,
        unique_args,
    );
    try self.emitFpOffset(result_offset);
}

/// Generate list_swap: swap the elements at two indices, mutating in place when
/// the list is statically unique. Routes through `roc_builtins_list_swap`.
fn generateLLListSwap(self: *Self, args: anytype, ret_layout: layout.Idx, unique_args: u64) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListSwap.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    const result_offset = try self.allocStackMemory(12, 4);

    // ZST elements: swapping is a no-op on the payload.
    if (elem_size == 0) {
        const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitFpOffset(result_offset);
        try self.emitLocalSet(result_ptr);
        try self.emitMemCopy(result_ptr, 0, list_ptr, 12);
        try self.emitFpOffset(result_offset);
        return;
    }

    const index_1_local = try self.materializeListIndex(args[1]);
    const index_2_local = try self.materializeListIndex(args[2]);

    switch (self.external_calls) {
        .host_imports => {
            const import_idx = self.list_swap_import orelse unreachable;
            try self.emitLocalGet(list_ptr);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(elem_align));
            try self.emitLocalGet(index_1_local);
            try self.emitLocalGet(index_2_local);
            try self.emitFpOffset(result_offset);
            try self.emitCall(import_idx);
        },
        .builtin_relocs => {
            const fields = try self.loadRocListFields(list_ptr);
            const callbacks = try self.listElementCallbacks(list_abi);
            try self.emitFpOffset(result_offset);
            try self.emitRocListFields(fields);
            try self.emitI32Const(@intCast(elem_align));
            try self.emitI32Const(@intCast(elem_size));
            try self.emitLocalGet(index_1_local);
            try self.emitLocalGet(index_2_local);
            try self.emitI32Const(@intCast(callbacks.elements_refcounted));
            try self.emitI32Const(@intCast(callbacks.incref_table_idx));
            try self.emitI32Const(@intCast(callbacks.decref_table_idx));
            try self.emitI32Const(updateModeImmForArg(unique_args, 0));
            try self.emitLocalGet(self.roc_ops_local);
            try self.emitBuiltinCall(.list_swap, null);
        },
        .unconfigured => wasmInvariantFmt("WASM/codegen invariant violated: external calls not configured before list_swap", .{}),
    }
    try self.emitFpOffset(result_offset);
}

/// Generate list_reserve: ensure list has at least given capacity
fn generateLLListReserve(self: *Self, args: anytype, ret_layout: layout.Idx, unique_args: u64) Allocator.Error!void {
    const list_abi = self.builtinInternalListAbi("wasm.generateLLListReserve.builtin_list_abi", ret_layout);
    const elem_size = list_abi.elem_size;
    const elem_align = list_abi.elem_align;

    try self.emitProcLocal(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    try self.emitProcLocal(args[1]);
    try self.emitConversion(try self.procLocalValType(args[1]), .i64);
    const spare_local = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(spare_local);

    const result_offset = try self.allocStackMemory(12, 4);
    switch (self.external_calls) {
        .host_imports => {
            const import_idx = self.list_reserve_import orelse unreachable;
            try self.emitLocalGet(list_ptr);
            try self.emitLocalGet(spare_local);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(elem_align));
            try self.emitFpOffset(result_offset);
            try self.emitCall(import_idx);
        },
        .builtin_relocs => {
            const fields = try self.loadRocListFields(list_ptr);
            const callbacks = try self.listElementCallbacks(list_abi);

            try self.emitFpOffset(result_offset);
            try self.emitRocListFields(fields);
            try self.emitI32Const(@intCast(elem_align));
            try self.emitLocalGet(spare_local);
            try self.emitI32Const(@intCast(elem_size));
            try self.emitI32Const(@intCast(callbacks.elements_refcounted));
            try self.emitI32Const(@intCast(callbacks.incref_table_idx));
            try self.emitI32Const(@intCast(callbacks.decref_table_idx));
            try self.emitI32Const(updateModeImmForArg(unique_args, 0));
            try self.emitLocalGet(self.roc_ops_local);
            try self.emitBuiltinCall(.list_reserve, null);
        },
        .unconfigured => wasmInvariantFmt("WASM/codegen invariant violated: external calls not configured before list_reserve", .{}),
    }
    try self.emitFpOffset(result_offset);
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
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    // Allocate new_len * elem_size bytes
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAllocWithRefcount(total_size, elem_align, list_abi.elements_refcounted);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // Copy old data
    const zero5 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
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

    const record_idx = ret_layout_val.getStruct().idx;
    const record_data = ls.getStructData(record_idx);
    const field0_layout = ls.getStructFieldLayout(record_idx, 0);
    const field1_layout = ls.getStructFieldLayout(record_idx, 1);
    const field0_val = ls.getLayout(field0_layout);
    const field0_is_list = field0_val.tag == .list or field0_val.tag == .list_of_zst;

    return .{
        .result_size = record_data.size.get(ls.targetUsize()),
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
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    const result_offset = try self.allocStackMemory(pair.result_size, pair.result_align);
    const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_ptr);

    // Copy first element to result
    const bytes_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    try self.emitLocalSet(bytes_local);

    const first_dst = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(result_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(pair.elem_offset)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(first_dst);

    const zero6 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero6);
    try self.emitMemCopyLoop(first_dst, zero6, old_data, bytes_local);

    // Build rest list (pointing to old_data + elem_size, len = old_len - 1)
    const rest_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_data);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rest_data);

    const rest_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(rest_len);

    const encoded_cap = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitPrepareListSliceMetadata(list_ptr, list_abi.elements_refcounted, encoded_cap);

    // Store rest list in result struct
    const rest_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(result_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(pair.list_offset)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);

    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.currentCode().append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, self.currentCode(), 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    const result_offset = try self.allocStackMemory(pair.result_size, pair.result_align);
    const result_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(result_offset);
    try self.emitLocalSet(result_ptr);

    // rest_len = old_len - 1
    const rest_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 1) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(rest_len);

    // Store rest list in result struct (shares data ptr, but with reduced length)
    const rest_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(result_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(pair.list_offset)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
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
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(last_src);

    const bytes_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(elem_size)) catch return error.OutOfMemory;
    try self.emitLocalSet(bytes_local);

    const last_dst = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(result_ptr);
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), @intCast(pair.elem_offset)) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(last_dst);

    const zero7 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.currentCode().append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, self.currentCode(), 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero7);
    try self.emitMemCopyLoop(last_dst, zero7, last_src, bytes_local);

    // Push result pointer
    try self.emitLocalGet(result_ptr);
}

test "rcAllocationLayout matches wasm32 builtin refcount allocation layout" {
    const expectEqual = std.testing.expectEqual;

    const flat_u8 = rcAllocationLayout(1, false);
    try expectEqual(@as(u32, 4), flat_u8.data_offset);
    try expectEqual(@as(u32, 4), flat_u8.allocation_alignment);

    const flat_aligned = rcAllocationLayout(8, false);
    try expectEqual(@as(u32, 8), flat_aligned.data_offset);
    try expectEqual(@as(u32, 8), flat_aligned.allocation_alignment);

    const refcounted_u32 = rcAllocationLayout(4, true);
    try expectEqual(@as(u32, 8), refcounted_u32.data_offset);
    try expectEqual(@as(u32, 4), refcounted_u32.allocation_alignment);

    const refcounted_aligned = rcAllocationLayout(16, true);
    try expectEqual(@as(u32, 16), refcounted_aligned.data_offset);
    try expectEqual(@as(u32, 16), refcounted_aligned.allocation_alignment);
}

test "final static data address tracking keeps referenced data through DCE" {
    const allocator = std.testing.allocator;
    const fake_store: *const LirStore = undefined;
    const fake_layouts: *const LayoutStore = undefined;

    var codegen = Self.init(allocator, fake_store, fake_layouts);
    defer codegen.deinit();
    codegen.configureStaticDataAddressTracking();

    const type_idx = try codegen.module.addFuncType(&.{}, &.{});
    const defined = try codegen.module.addDefinedFunction(type_idx);
    try codegen.module.addExport("main", .func, defined.function.raw());

    const live_address = try codegen.addStaticDataSymbol(&.{ 0, 0, 0, 0, 'l', 'i', 'v', 'e' }, 4, ".rodata.live", "live.data", 4, 4);
    _ = try codegen.addStaticDataSymbol(&.{ 0, 0, 0, 0, 'd', 'e', 'a', 'd' }, 4, ".rodata.dead", "dead.data", 4, 4);

    try codegen.beginFunction(defined.local);
    try codegen.emitDataAddressConst(live_address, 0);
    try codegen.currentCode().append(allocator, Op.drop);
    try codegen.currentCode().append(allocator, Op.end);
    codegen.endFunction();
    try codegen.flushPendingBodies();

    try std.testing.expectEqual(@as(usize, 1), codegen.module.reloc_code.entries.items.len);
    try codegen.module.resolveRelocations();

    const called_fns = try allocator.alloc(bool, codegen.module.liveFunctionCount());
    defer allocator.free(called_fns);
    @memset(called_fns, false);
    try codegen.module.eliminateDeadCode(called_fns);

    try std.testing.expectEqual(@as(usize, 1), codegen.module.data_segments.items.len);
    try std.testing.expect(std.mem.find(u8, codegen.module.data_segments.items[0].data, "live") != null);
    try std.testing.expect(std.mem.find(u8, codegen.module.data_segments.items[0].data, "dead") == null);
}

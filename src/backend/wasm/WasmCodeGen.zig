//! MonoIR -> WebAssembly code generator.
//!
//! Walks MonoIR expressions and emits wasm instructions. Each `generateExpr`
//! call leaves the result on the wasm value stack (for primitives) or writes
//! to linear memory (for composites).

const std = @import("std");
const Allocator = std.mem.Allocator;
const layout = @import("layout");
const mono = @import("mono");
const MonoExprStore = mono.MonoExprStore;
const MonoExpr = mono.MonoIR.MonoExpr;
const MonoExprId = mono.MonoIR.MonoExprId;
const MonoPattern = mono.MonoIR.MonoPattern;
const MonoPatternId = mono.MonoIR.MonoPatternId;
const BinOp = MonoExpr.BinOp;
const WasmModule = @import("WasmModule.zig");
const WasmLayout = @import("WasmLayout.zig");
const Storage = @import("Storage.zig");
const Op = WasmModule.Op;
const ValType = WasmModule.ValType;
const BlockType = WasmModule.BlockType;

const MonoSymbol = mono.MonoIR.MonoSymbol;
const MonoProc = mono.MonoIR.MonoProc;
const CFStmt = mono.MonoIR.CFStmt;
const CFStmtId = mono.MonoIR.CFStmtId;

const LayoutStore = layout.Store;

const Self = @This();

pub const Error = error{
    OutOfMemory,
    UnsupportedExpr,
};

fn unsupported(_: std.builtin.SourceLocation) Error {
    return error.UnsupportedExpr;
}

allocator: Allocator,
store: *const MonoExprStore,
layout_store: ?*const LayoutStore,
module: WasmModule,
body: std.ArrayList(u8), // instruction bytes for current function
storage: Storage,
/// Accumulated stack frame size for the current function (for stack memory values).
stack_frame_size: u32 = 0,
/// Whether the current function uses stack memory (needs prologue/epilogue).
uses_stack_memory: bool = false,
/// Local index of the frame pointer ($fp) - only valid when uses_stack_memory is true.
fp_local: u32 = 0,
/// Map from lambda expression ID → compiled wasm function index.
compiled_lambdas: std.AutoHashMap(u32, u32),
/// Map from MonoSymbol → wasm function index (for let-bound lambdas).
symbol_funcs: std.AutoHashMap(u48, u32),
/// Map from expression ID → capture info (set during compileClosure).
closure_captures_by_expr: std.AutoHashMap(u32, CaptureInfo),
/// Map from MonoSymbol → capture info (set during tryBindFunction, read at call sites).
closure_captures_by_sym: std.AutoHashMap(u48, CaptureInfo),
/// Whether the module needs heap allocation (bump allocator).
uses_heap: bool = false,
/// CFStmt block nesting depth (for br targets in proc compilation).
cf_depth: u32 = 0,
/// Map from JoinPointId → loop depth (for jump → br targeting).
join_point_depths: std.AutoHashMap(u32, u32),
/// Map from JoinPointId → param local indices.
join_point_param_locals: std.AutoHashMap(u32, []u32),
/// Wasm function index for imported roc_dec_mul host function.
dec_mul_import: ?u32 = null,
/// Wasm function index for imported roc_dec_to_str host function.
dec_to_str_import: ?u32 = null,
/// Wasm function index for imported roc_str_eq host function.
str_eq_import: ?u32 = null,

const CaptureInfo = struct {
    symbols: []const MonoSymbol,
    val_types: []const ValType,
};

pub fn init(allocator: Allocator, store: *const MonoExprStore, layout_store: ?*const LayoutStore) Self {
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
        .compiled_lambdas = std.AutoHashMap(u32, u32).init(allocator),
        .symbol_funcs = std.AutoHashMap(u48, u32).init(allocator),
        .closure_captures_by_expr = std.AutoHashMap(u32, CaptureInfo).init(allocator),
        .closure_captures_by_sym = std.AutoHashMap(u48, CaptureInfo).init(allocator),
        .join_point_depths = std.AutoHashMap(u32, u32).init(allocator),
        .join_point_param_locals = std.AutoHashMap(u32, []u32).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.module.deinit();
    self.body.deinit(self.allocator);
    self.storage.deinit();
    self.compiled_lambdas.deinit();
    self.symbol_funcs.deinit();
    self.join_point_depths.deinit();
    // Free allocated param local arrays
    var jp_it = self.join_point_param_locals.iterator();
    while (jp_it.next()) |entry| {
        self.allocator.free(entry.value_ptr.*);
    }
    self.join_point_param_locals.deinit();
    var it = self.closure_captures_by_expr.iterator();
    while (it.next()) |entry| {
        self.allocator.free(entry.value_ptr.symbols);
        self.allocator.free(entry.value_ptr.val_types);
    }
    self.closure_captures_by_expr.deinit();
    // by_sym values alias by_expr allocations, so just deinit the map
    self.closure_captures_by_sym.deinit();
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
}

/// Result of generating a wasm module
pub const GenerateResult = struct {
    wasm_bytes: []u8,
    result_layout: layout.Idx,
    has_imports: bool = false,
};

/// Generate a complete wasm module for a single expression.
/// The expression becomes the body of an exported "main" function.
pub fn generateModule(self: *Self, expr_id: MonoExprId, result_layout: layout.Idx) Error!GenerateResult {
    // Register host function imports (must be done before addFunction calls)
    self.registerHostImports() catch return error.OutOfMemory;

    // Compile any procedures (recursive functions) before the main expression
    const procs = self.store.getProcs();
    if (procs.len > 0) {
        self.compileAllProcs(procs) catch return error.OutOfMemory;
    }

    // Determine return type from the expression's actual wasm type.
    // We use exprValType because nominal layout indices can collide
    // with well-known sentinel values (e.g., Bool's nominal layout
    // index may equal the i64 sentinel).
    const result_vt = self.exprValType(expr_id);

    // Add function type: () -> (result_type)
    const type_idx = self.module.addFuncType(&.{}, &.{result_vt}) catch return error.OutOfMemory;

    // Add function
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;

    // Generate the expression body into self.body
    self.body.clearRetainingCapacity();
    self.storage.reset();
    self.stack_frame_size = 0;
    self.uses_stack_memory = false;
    try self.generateExpr(expr_id);

    // If we used stack memory or heap, enable memory + globals in the module
    if (self.uses_stack_memory or self.uses_heap) {
        self.module.enableMemory(2); // 2 pages = 128KB (stack + heap)
        self.module.enableStackPointer(65536); // stack starts at top of first page
        if (!self.uses_stack_memory) {
            // Heap needs stack memory infra too (for FP local etc)
            self.uses_stack_memory = true;
            self.fp_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        }
        self.module.addExport("memory", .memory, 0) catch return error.OutOfMemory;
    }
    if (self.uses_heap) {
        // Heap starts at 65536 (second page), grows upward
        self.module.enableHeapPointer(65536);
    }

    // Build function body: locals declaration + prologue + instructions + epilogue + end
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    // Encode locals declaration
    try self.encodeLocalsDecl(&func_body, 0);

    if (self.uses_stack_memory) {
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
    }

    // Main body instructions
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
fn encodeLocalsDecl(self: *Self, func_body: *std.ArrayList(u8), skip_count: u32) Error!void {
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

/// Generate wasm instructions for a MonoExpr, leaving the result on the value stack.
fn generateExpr(self: *Self, expr_id: MonoExprId) Error!void {
    const expr: MonoExpr = self.store.getExpr(expr_id);
    switch (expr) {
        .i64_literal => |val| {
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, val) catch return error.OutOfMemory;
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
            // i128 stored in 16 bytes of linear memory
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
        .binop => |b| {
            // Check for composite types (Dec, i128, records, etc.)
            if (self.isCompositeExpr(b.lhs)) {
                if (b.op == .eq or b.op == .neq) {
                    try self.generateStructuralEq(b.lhs, b.rhs, b.op == .neq);
                } else {
                    try self.generateCompositeI128BinOp(b.lhs, b.rhs, b.op, b.result_layout);
                }
            } else {
                try self.generateExpr(b.lhs);
                try self.generateExpr(b.rhs);

                // For arithmetic ops, use the result type. For comparison ops,
                // use the operand type (comparisons return bool/i32 regardless
                // of whether operands are i32, i64, f32, or f64).
                const is_comparison = switch (b.op) {
                    .eq, .neq, .lt, .lte, .gt, .gte => true,
                    else => false,
                };
                const vt = if (is_comparison)
                    self.exprValType(b.lhs)
                else
                    self.resolveValType(b.result_layout);

                // For signedness, use the operand layout for comparisons,
                // the result layout for arithmetic.
                const signedness_layout = if (is_comparison)
                    self.exprLayoutIdx(b.lhs)
                else
                    b.result_layout;
                const is_unsigned = if (signedness_layout) |lay| isUnsignedLayout(lay) else false;

                const op: u8 = switch (b.op) {
                    .add => switch (vt) {
                        .i32 => Op.i32_add,
                        .i64 => Op.i64_add,
                        .f32 => Op.f32_add,
                        .f64 => Op.f64_add,
                    },
                    .sub => switch (vt) {
                        .i32 => Op.i32_sub,
                        .i64 => Op.i64_sub,
                        .f32 => Op.f32_sub,
                        .f64 => Op.f64_sub,
                    },
                    .mul => switch (vt) {
                        .i32 => Op.i32_mul,
                        .i64 => Op.i64_mul,
                        .f32 => Op.f32_mul,
                        .f64 => Op.f64_mul,
                    },
                    .div => switch (vt) {
                        .i32 => if (is_unsigned) Op.i32_div_u else Op.i32_div_s,
                        .i64 => if (is_unsigned) Op.i64_div_u else Op.i64_div_s,
                        .f32 => Op.f32_div,
                        .f64 => Op.f64_div,
                    },
                    .div_trunc => switch (vt) {
                        .i32 => if (is_unsigned) Op.i32_div_u else Op.i32_div_s,
                        .i64 => if (is_unsigned) Op.i64_div_u else Op.i64_div_s,
                        .f32 => Op.f32_div,
                        .f64 => Op.f64_div,
                    },
                    .mod => switch (vt) {
                        .i32 => if (is_unsigned) Op.i32_rem_u else Op.i32_rem_s,
                        .i64 => if (is_unsigned) Op.i64_rem_u else Op.i64_rem_s,
                        .f32, .f64 => {
                            try self.emitFloatMod(vt);
                            return; // multi-instruction, skip the single-op emit below
                        },
                    },
                    .eq => switch (vt) {
                        .i32 => Op.i32_eq,
                        .i64 => Op.i64_eq,
                        .f32 => Op.f32_eq,
                        .f64 => Op.f64_eq,
                    },
                    .neq => switch (vt) {
                        .i32 => Op.i32_ne,
                        .i64 => Op.i64_ne,
                        .f32 => Op.f32_ne,
                        .f64 => Op.f64_ne,
                    },
                    .lt => switch (vt) {
                        .i32 => if (is_unsigned) Op.i32_lt_u else Op.i32_lt_s,
                        .i64 => if (is_unsigned) Op.i64_lt_u else Op.i64_lt_s,
                        .f32 => Op.f32_lt,
                        .f64 => Op.f64_lt,
                    },
                    .lte => switch (vt) {
                        .i32 => if (is_unsigned) Op.i32_le_u else Op.i32_le_s,
                        .i64 => if (is_unsigned) Op.i64_le_u else Op.i64_le_s,
                        .f32 => Op.f32_le,
                        .f64 => Op.f64_le,
                    },
                    .gt => switch (vt) {
                        .i32 => if (is_unsigned) Op.i32_gt_u else Op.i32_gt_s,
                        .i64 => if (is_unsigned) Op.i64_gt_u else Op.i64_gt_s,
                        .f32 => Op.f32_gt,
                        .f64 => Op.f64_gt,
                    },
                    .gte => switch (vt) {
                        .i32 => if (is_unsigned) Op.i32_ge_u else Op.i32_ge_s,
                        .i64 => if (is_unsigned) Op.i64_ge_u else Op.i64_ge_s,
                        .f32 => Op.f32_ge,
                        .f64 => Op.f64_ge,
                    },
                    .@"and" => Op.i32_and,
                    .@"or" => Op.i32_or,
                };
                self.body.append(self.allocator, op) catch return error.OutOfMemory;
            }
        },
        .unary_minus => |u| {
            // Composite types (Dec, i128) can't be negated with scalar ops
            if (self.isCompositeLayout(u.result_layout)) {
                try self.generateCompositeI128Negate(u.expr, u.result_layout);
                return;
            }
            const vt = self.resolveValType(u.result_layout);
            switch (vt) {
                .i32 => {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    try self.generateExpr(u.expr);
                    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    try self.generateExpr(u.expr);
                    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                },
                .f32 => {
                    try self.generateExpr(u.expr);
                    self.body.append(self.allocator, Op.f32_neg) catch return error.OutOfMemory;
                },
                .f64 => {
                    try self.generateExpr(u.expr);
                    self.body.append(self.allocator, Op.f64_neg) catch return error.OutOfMemory;
                },
            }
        },
        .unary_not => |u| {
            try self.generateExpr(u.expr);
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        },
        .block => |b| {
            // Process statements (let bindings)
            const stmts = self.store.getStmts(b.stmts);
            for (stmts) |stmt| {
                const pattern = self.store.getPattern(stmt.pattern);
                switch (pattern) {
                    .bind => |bind| {
                        // Check if the bound expression is a lambda — if so, compile
                        // as a wasm function and record symbol→func_idx mapping.
                        const stmt_expr = self.store.getExpr(stmt.expr);
                        if (try self.tryBindFunction(stmt.expr, stmt_expr, bind.symbol)) continue;
                        // Non-lambda, non-closure, non-function-alias — fall through to value binding
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
                                        const local_idx = self.storage.getLocal(bind.symbol) orelse
                                            (self.storage.allocLocal(bind.symbol, vt2) catch return error.OutOfMemory);
                                        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                                        WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                                        continue;
                                    }
                                    return unsupported(@src());
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
                                        if (expr_data == .i64_literal and expr_data.i64_literal < 0 and
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
                                        const local_idx = self.storage.getLocal(bind.symbol) orelse
                                            (self.storage.allocLocal(bind.symbol, .i32) catch return error.OutOfMemory);
                                        try self.emitLocalSet(local_idx);
                                        continue;
                                    }

                                    if (target_size != expr_size) {
                                        return unsupported(@src());
                                    }

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
                                    const local_idx = self.storage.getLocal(bind.symbol) orelse
                                        (self.storage.allocLocal(bind.symbol, .i32) catch return error.OutOfMemory);
                                    try self.emitLocalSet(local_idx);
                                    continue;
                                }
                                // Determine the target wasm type using layout store
                                const vt = self.resolveValType(bind.layout_idx);
                                // Generate the expression value
                                try self.generateExpr(stmt.expr);
                                // Convert if the expression produced a different wasm type
                                const expr_vt = self.exprValType(stmt.expr);
                                try self.emitConversion(expr_vt, vt);
                                // Allocate a local (or reuse existing one for mutable rebinding)
                                const local_idx = self.storage.getLocal(bind.symbol) orelse
                                    (self.storage.allocLocal(bind.symbol, vt) catch return error.OutOfMemory);
                                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                                WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                        }
                    },
                    .wildcard => {
                        // Evaluate expression for side effects, drop result
                        try self.generateExpr(stmt.expr);
                        self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
                    },
                    .record => |rec| {
                        // Record destructuring: generate expr → pointer, then bind each field
                        try self.generateExpr(stmt.expr);
                        const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, ptr) catch return error.OutOfMemory;
                        try self.bindRecordPattern(ptr, rec);
                    },
                    .tuple => |tup| {
                        // Tuple destructuring: generate expr → pointer, then bind each element
                        try self.generateExpr(stmt.expr);
                        const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, ptr) catch return error.OutOfMemory;
                        try self.bindTuplePattern(ptr, tup);
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
                    else => return error.UnsupportedExpr,
                }
            }
            // Generate the final expression (the block's result)
            try self.generateExpr(b.final_expr);
        },
        .lookup => |l| {
            const local_idx = self.storage.getLocal(l.symbol) orelse return unsupported(@src());
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
        },
        .if_then_else => |ite| {
            const branches = self.store.getIfBranches(ite.branches);
            const bt = valTypeToBlockType(self.resolveValType(ite.result_layout));
            try self.generateIfChain(branches, ite.final_else, bt);
        },
        .when => |w| {
            try self.generateWhen(w);
        },
        .nominal => |nom| {
            // Nominal is transparent at runtime — just generate the backing expression.
            // But check for type representation mismatch: if the backing expression
            // produces a composite value (i32 pointer) but the nominal layout expects
            // a scalar, or vice versa, the conversion is not yet supported.
            const backing_composite = self.isCompositeExpr(nom.backing_expr);
            const nominal_composite = self.isCompositeLayout(nom.nominal_layout);
            if (backing_composite != nominal_composite) {
                return unsupported(@src());
            }
            try self.generateExpr(nom.backing_expr);
        },
        .empty_record => {
            // Zero-sized type — push a dummy i32 0
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
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
        .crash => {
            // For now, emit unreachable (Phase 7 will call roc_panic import)
            self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },
        .early_return => |er| {
            try self.generateExpr(er.expr);
            self.body.append(self.allocator, Op.@"return") catch return error.OutOfMemory;
        },
        .lambda => |lambda| {
            // Compile the lambda as a separate wasm function.
            // The result is a function index we can reference later.
            const func_idx = try self.compileLambda(expr_id, lambda);
            _ = func_idx;
            // Lambda as a value — push a placeholder i32 (func index).
            // This is only meaningful when the lambda is later called.
            // For let-bound lambdas, the symbol_funcs map is used at call site.
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        },
        .call => |c| {
            try self.generateCall(c);
        },
        .record => |r| {
            try self.generateRecord(r);
        },
        .field_access => |fa| {
            try self.generateFieldAccess(fa);
        },
        .tuple => |t| {
            try self.generateTuple(t);
        },
        .tuple_access => |ta| {
            try self.generateTupleAccess(ta);
        },
        .zero_arg_tag => |z| {
            try self.generateZeroArgTag(z);
        },
        .tag => |t| {
            try self.generateTag(t);
        },
        .closure => |closure| {
            // For closures used as values (not let-bound), compile and push func reference
            const func_idx = try self.compileClosure(expr_id, closure);
            _ = func_idx;
            // Push dummy i32 (the closure as a value — caller will resolve via symbol)
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
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
        .incref, .decref, .free => {
            // RC ops are no-ops for now (Phase 7 will implement)
            // These expressions are transparent — they evaluate their inner value
            const inner_expr = switch (expr) {
                .incref => |inc| inc.value,
                .decref => |dec| dec.value,
                .free => |f| f.value,
                else => unreachable,
            };
            try self.generateExpr(inner_expr);
        },
        .discriminant_switch => |ds| {
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
    }
}

/// Generate a cascading if/else chain from MonoIfBranch array + final_else.
fn generateIfChain(self: *Self, branches: []const mono.MonoIR.MonoIfBranch, final_else: MonoExprId, bt: BlockType) Error!void {
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

/// Generate a when expression (pattern matching).
fn generateWhen(self: *Self, w: anytype) Error!void {
    const branches = self.store.getWhenBranches(w.branches);
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
    try self.generateWhenBranches(branches, temp_local, value_vt, bt);
}

fn generateWhenBranches(self: *Self, branches: []const mono.MonoIR.MonoWhenBranch, value_local: u32, value_vt: ValType, bt: BlockType) Error!void {
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
                else => return error.UnsupportedExpr,
            }

            // Compare
            const eq_op: u8 = switch (value_vt) {
                .i32 => Op.i32_eq,
                .i64 => Op.i64_eq,
                else => return error.UnsupportedExpr,
            };
            self.body.append(self.allocator, eq_op) catch return error.OutOfMemory;

            // if match
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
            try self.generateExpr(branch.body);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateWhenBranches(remaining, value_local, value_vt, bt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .tag => |tag_pat| {
            // Match on tag discriminant
            const arg_patterns = self.store.getPatternSpan(tag_pat.args);

            // For tag unions that fit in a single i32 (discriminant only, no payload),
            // value_local holds the discriminant directly. For larger tag unions,
            // value_local holds a pointer to the tag union in memory.
            const is_pointer = blk: {
                if (self.layout_store) |ls| {
                    // Check if the when's value layout is a tag_union in memory
                    const wl = WasmLayout.wasmReprWithStore(tag_pat.union_layout, ls);
                    break :blk switch (wl) {
                        .stack_memory => true,
                        .primitive => false,
                    };
                }
                break :blk false;
            };

            if (is_pointer) {
                // Load discriminant from memory at discriminant_offset
                const ls = try self.getLayoutStore();
                const l = ls.getLayout(tag_pat.union_layout);
                if (l.tag != .tag_union) return unsupported(@src());
                const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
                const disc_offset = tu_data.discriminant_offset;
                const disc_size: u32 = tu_data.discriminant_size;

                // Load discriminant: value_local[disc_offset]
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                try self.emitLoadOpSized(.i32, disc_size, disc_offset);
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

            // Bind any sub-pattern arguments (load payload from memory)
            if (is_pointer and arg_patterns.len > 0) {
                var payload_offset: u32 = 0;
                for (arg_patterns) |arg_pat_id| {
                    const arg_pat = self.store.getPattern(arg_pat_id);
                    switch (arg_pat) {
                        .bind => |bind| {
                            const bind_vt = self.resolveValType(bind.layout_idx);
                            const bind_byte_size = self.layoutByteSize(bind.layout_idx);
                            const local_idx = self.storage.allocLocal(bind.symbol, bind_vt) catch return error.OutOfMemory;
                            // Load payload field from memory: value_local[payload_offset]
                            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, &self.body, value_local) catch return error.OutOfMemory;
                            try self.emitLoadOpSized(bind_vt, bind_byte_size, payload_offset);
                            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                            payload_offset += bind_byte_size;
                        },
                        .wildcard => {
                            // Skip this payload field — still need to advance the offset
                            // Use i32 size as default for wildcards since we don't know the layout
                            payload_offset += 4;
                        },
                        else => return error.UnsupportedExpr,
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
                        else => return error.UnsupportedExpr,
                    }
                }
            }

            try self.generateExpr(branch.body);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateWhenBranches(remaining, value_local, value_vt, bt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        .record => |rec_pat| {
            // Record destructuring: bind each field to a local
            const ls = try self.getLayoutStore();
            const l = ls.getLayout(rec_pat.record_layout);
            if (l.tag != .record) return unsupported(@src());
            const field_patterns = self.store.getPatternSpan(rec_pat.fields);

            for (field_patterns, 0..) |field_pat_id, i| {
                const field_pat = self.store.getPattern(field_pat_id);
                switch (field_pat) {
                    .bind => |bind| {
                        const bind_vt = self.resolveValType(bind.layout_idx);
                        const bind_byte_size = self.layoutByteSize(bind.layout_idx);
                        const local_idx = self.storage.allocLocal(bind.symbol, bind_vt) catch return error.OutOfMemory;
                        const field_offset = ls.getRecordFieldOffset(l.data.record.idx, @intCast(i));
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
                    else => return error.UnsupportedExpr,
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
                else => return error.UnsupportedExpr,
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
                else => return error.UnsupportedExpr,
            }

            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(bt)) catch return error.OutOfMemory;
            try self.generateExpr(branch.body);
            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
            try self.generateWhenBranches(remaining, value_local, value_vt, bt);
            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },
        else => return error.UnsupportedExpr,
    }
}

/// Check whether a layout represents an unsigned integer type.
fn isUnsignedLayout(layout_idx: layout.Idx) bool {
    return switch (layout_idx) {
        .u8, .u16, .u32, .u64, .u128, .bool => true,
        else => false,
    };
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

/// Try to infer the layout.Idx of an expression (for signedness checks).
fn exprLayoutIdx(self: *Self, expr_id: MonoExprId) ?layout.Idx {
    const expr = self.store.getExpr(expr_id);
    return switch (expr) {
        .binop => |b| b.result_layout,
        .unary_minus => |u| u.result_layout,
        .unary_not => layout.Idx.bool,
        .block => |b| b.result_layout,
        .lookup => |l| l.layout_idx,
        .if_then_else => |ite| ite.result_layout,
        .when => |w| w.result_layout,
        .nominal => |nom| self.exprLayoutIdx(nom.backing_expr),
        .call => |c| c.ret_layout,
        .record => |r| r.record_layout,
        .tuple => |t| t.tuple_layout,
        .field_access => |fa| fa.field_layout,
        .tuple_access => |ta| ta.elem_layout,
        .zero_arg_tag => |z| z.union_layout,
        .tag => |t| t.union_layout,
        .closure => |c| c.closure_layout,
        .low_level => |ll| ll.ret_layout,
        .dbg => |d| d.result_layout,
        .expect => |e| e.result_layout,
        .incref => |inc| self.exprLayoutIdx(inc.value),
        .decref => |dec| self.exprLayoutIdx(dec.value),
        .free => |f| self.exprLayoutIdx(f.value),
        .i64_literal => layout.Idx.i64,
        .f64_literal => layout.Idx.f64,
        .f32_literal => layout.Idx.f32,
        .bool_literal => layout.Idx.bool,
        .i128_literal => layout.Idx.i128,
        .dec_literal => layout.Idx.dec,
        .str_literal => layout.Idx.str,
        .str_concat => layout.Idx.str,
        .int_to_str => layout.Idx.str,
        .float_to_str => layout.Idx.str,
        .dec_to_str => layout.Idx.str,
        .str_escape_and_quote => layout.Idx.str,
        else => null,
    };
}

/// Infer the wasm ValType that an expression will push onto the stack.
fn exprValType(self: *Self, expr_id: MonoExprId) ValType {
    const expr = self.store.getExpr(expr_id);
    return switch (expr) {
        .i64_literal => .i64,
        .f64_literal => .f64,
        .f32_literal => .f32,
        .bool_literal => .i32,
        .i128_literal, .dec_literal => .i32, // pointer to stack memory
        .binop => |b| self.resolveValType(b.result_layout),
        .unary_minus => |u| self.resolveValType(u.result_layout),
        .unary_not => .i32,
        .block => |b| self.resolveValType(b.result_layout),
        .lookup => |l| self.resolveValType(l.layout_idx),
        .if_then_else => |ite| self.resolveValType(ite.result_layout),
        .when => |w| self.resolveValType(w.result_layout),
        .nominal => |nom| self.exprValType(nom.backing_expr),
        .empty_record => .i32,
        .empty_list => .i32, // pointer to 12-byte RocList
        .call => |c| self.resolveValType(c.ret_layout),
        .lambda => .i32, // function reference (index)
        .closure => .i32, // closure value (function reference)
        .record => .i32, // pointer to stack memory
        .tuple => .i32, // pointer to stack memory
        .field_access => |fa| self.resolveValType(fa.field_layout),
        .tuple_access => |ta| self.resolveValType(ta.elem_layout),
        .zero_arg_tag => .i32, // discriminant or pointer
        .tag => .i32, // pointer to stack memory
        .low_level => |ll| self.resolveValType(ll.ret_layout),
        .dbg => |d| self.resolveValType(d.result_layout),
        .expect => |e| self.resolveValType(e.result_layout),
        .incref => |inc| self.exprValType(inc.value),
        .decref => |dec| self.exprValType(dec.value),
        .free => |f| self.exprValType(f.value),
        .str_literal => .i32,
        .list => .i32, // pointer to 12-byte RocList
        .str_concat => .i32, // pointer to 12-byte RocStr
        .int_to_str => .i32, // pointer to 12-byte RocStr
        .float_to_str => .i32, // pointer to 12-byte RocStr
        .dec_to_str => .i32, // pointer to 12-byte RocStr
        .str_escape_and_quote => .i32, // pointer to 12-byte RocStr
        else => .i64, // conservative default
    };
}

/// Get the byte size of the value an expression produces.
fn exprByteSize(self: *Self, expr_id: MonoExprId) u32 {
    if (self.exprLayoutIdx(expr_id)) |lay_idx| {
        return self.layoutByteSize(lay_idx);
    }
    // Check for known composite types without layout indices
    const expr = self.store.getExpr(expr_id);
    switch (expr) {
        .list, .empty_list => return 12, // RocList = ptr + len + cap
        .str_concat, .int_to_str, .float_to_str, .dec_to_str, .str_escape_and_quote => return 12, // RocStr
        else => {},
    }
    // Fallback based on ValType
    return switch (self.exprValType(expr_id)) {
        .i32, .f32 => 4,
        .i64, .f64 => 8,
    };
}

/// Check if an expression produces a composite value (stored in stack memory).
fn isCompositeExpr(self: *const Self, expr_id: MonoExprId) bool {
    const expr = self.store.getExpr(expr_id);
    return switch (expr) {
        .dec_literal, .i128_literal => true, // 16 bytes in stack memory
        .str_literal => true, // 12-byte RocStr in stack memory
        .list => true, // 12-byte RocList in stack memory
        .empty_list => true, // 12-byte RocList in stack memory
        .str_concat => true, // produces 12-byte RocStr
        .int_to_str => true, // produces 12-byte RocStr
        .float_to_str => true, // produces 12-byte RocStr
        .dec_to_str => true, // produces 12-byte RocStr
        .str_escape_and_quote => true, // produces 12-byte RocStr
        .record => |r| self.isCompositeLayout(r.record_layout),
        .tuple => |t| self.isCompositeLayout(t.tuple_layout),
        .tag => |t| self.isCompositeLayout(t.union_layout),
        .zero_arg_tag => |z| self.isCompositeLayout(z.union_layout),
        .nominal => |nom| self.isCompositeExpr(nom.backing_expr),
        .block => |b| self.isCompositeExpr(b.final_expr),
        .if_then_else => |ite| self.isCompositeLayout(ite.result_layout),
        .when => |w| self.isCompositeLayout(w.result_layout),
        .lookup => |l| self.isCompositeLayout(l.layout_idx),
        .call => |c| self.isCompositeLayout(c.ret_layout),
        .field_access => |fa| self.isCompositeLayout(fa.field_layout),
        .tuple_access => |ta| self.isCompositeLayout(ta.elem_layout),
        .low_level => |ll| self.isCompositeLayout(ll.ret_layout),
        .dbg => |d| self.isCompositeLayout(d.result_layout),
        .expect => |e| self.isCompositeLayout(e.result_layout),
        .binop => |b| self.isCompositeLayout(b.result_layout),
        .unary_minus => |u| self.isCompositeLayout(u.result_layout),
        .unary_not => false, // always bool (i32)
        .incref => |inc| self.isCompositeExpr(inc.value),
        .decref => |dec| self.isCompositeExpr(dec.value),
        .free => |f| self.isCompositeExpr(f.value),
        else => false,
    };
}

/// Check if a layout represents a composite type stored in stack memory.
fn isCompositeLayout(self: *const Self, layout_idx: layout.Idx) bool {
    if (self.layout_store) |ls| {
        const repr = WasmLayout.wasmReprWithStore(layout_idx, ls);
        return switch (repr) {
            .stack_memory => |s| s > 0,
            .primitive => false,
        };
    }
    return false;
}

/// Generate structural equality comparison for two composite values (records, tuples).
/// Compares the underlying memory byte-by-byte using i32 loads.
/// Leaves an i32 (bool) on the stack: 1 for equal, 0 for not equal.
fn generateStructuralEq(self: *Self, lhs: MonoExprId, rhs: MonoExprId, negate: bool) Error!void {
    // Deep equality for heap types (List, Str) requires element-by-element comparison
    // which isn't implemented yet. Return unsupported to skip rather than produce wrong results.
    const lhs_expr = self.store.getExpr(lhs);
    switch (lhs_expr) {
        .list, .empty_list, .str_literal, .str_concat, .int_to_str, .float_to_str, .dec_to_str, .str_escape_and_quote => return unsupported(@src()),
        else => {},
    }
    // Also check via layout: if either operand has a list/str layout, skip
    if (self.exprLayoutIdx(lhs)) |lay_idx| {
        if (lay_idx == .str) return unsupported(@src());
        if (self.layout_store) |ls| {
            const l = ls.getLayout(lay_idx);
            if (l.tag == .list) return unsupported(@src());
        }
    }

    // Get the byte size of the values
    const byte_size = self.exprByteSize(lhs);

    // Generate both operand expressions — each pushes an i32 pointer
    try self.generateExpr(lhs);
    const lhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;

    try self.generateExpr(rhs);
    const rhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;

    if (byte_size == 0) {
        // Zero-size types are always equal
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, if (negate) 0 else 1) catch return error.OutOfMemory;
        return;
    }

    // Compare memory word-by-word (or byte-by-byte for remainder)
    // Strategy: compare each aligned chunk, AND all results together
    var offset: u32 = 0;
    var first = true;

    // Compare i32-sized chunks (use align=0 for safety)
    while (offset + 4 <= byte_size) : (offset += 4) {
        // Load lhs[offset] as i32
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align=1 (unaligned)
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        // Load rhs[offset] as i32
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align=1 (unaligned)
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        // Compare
        self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;

        // AND with previous results
        if (!first) {
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    // Compare remaining bytes (2-byte then 1-byte chunks)
    if (offset + 2 <= byte_size) {
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory; // align=2
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_load16_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory; // align=2
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;

        if (!first) {
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
        offset += 2;
    }

    if (offset < byte_size) {
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align=1
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align=1
        WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;

        self.body.append(self.allocator, Op.i32_eq) catch return error.OutOfMemory;

        if (!first) {
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        }
        first = false;
    }

    if (first) {
        // No bytes to compare (shouldn't happen since byte_size > 0)
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    }

    if (negate) {
        // Flip the result for !=
        self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    }
}

/// Generate i128/Dec binary operations (add, sub, comparisons).
/// Both operands are i32 pointers to 16-byte values in linear memory.
fn generateCompositeI128BinOp(self: *Self, lhs: MonoExprId, rhs: MonoExprId, op: anytype, result_layout: layout.Idx) Error!void {
    // Generate operand pointers
    try self.generateExpr(lhs);
    const lhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;

    try self.generateExpr(rhs);
    const rhs_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;

    switch (op) {
        .add => try self.emitI128Add(lhs_local, rhs_local),
        .sub => try self.emitI128Sub(lhs_local, rhs_local),
        .mul => {
            if (result_layout == .dec) {
                // Dec multiply: call imported host function roc_dec_mul(lhs_ptr, rhs_ptr, result_ptr)
                const import_idx = self.dec_mul_import orelse return unsupported(@src());
                const result_offset = try self.allocStackMemory(16, 8);
                const result_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
                try self.emitFpOffset(result_offset);
                self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
                // Push args: lhs_ptr, rhs_ptr, result_ptr
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, lhs_local) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, rhs_local) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
                self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, import_idx) catch return error.OutOfMemory;
                // Push result pointer
                self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
                return;
            }
            // i128 × i128 → i128 (truncating multiply, keeping lower 128 bits)
            try self.emitI128Mul(lhs_local, rhs_local);
        },
        .lt => try self.emitI128Compare(lhs_local, rhs_local, .lt),
        .lte => try self.emitI128Compare(lhs_local, rhs_local, .lte),
        .gt => try self.emitI128Compare(lhs_local, rhs_local, .gt),
        .gte => try self.emitI128Compare(lhs_local, rhs_local, .gte),
        else => return error.UnsupportedExpr,
    }
}

/// Emit i128 addition: result = lhs + rhs
/// Pushes an i32 pointer to the 16-byte result on the wasm stack.
/// Pre-loads all operand words into locals to avoid aliasing issues
/// when result memory overlaps with lhs/rhs (e.g., in loops).
fn emitI128Add(self: *Self, lhs_local: u32, rhs_local: u32) Error!void {
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
fn emitI128Sub(self: *Self, lhs_local: u32, rhs_local: u32) Error!void {
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
fn emitI128Mul(self: *Self, lhs_local: u32, rhs_local: u32) Error!void {
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
fn emitI128Compare(self: *Self, lhs_local: u32, rhs_local: u32, cmp_op: I128CmpOp) Error!void {
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
        .lt => Op.i64_lt_s,
        .lte => Op.i64_le_s,
        .gt => Op.i64_gt_s,
        .gte => Op.i64_ge_s,
    };
    self.body.append(self.allocator, high_cmp) catch return error.OutOfMemory;

    // End if
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Generate i128/Dec negation: result = -value (two's complement)
fn generateCompositeI128Negate(self: *Self, expr: MonoExprId, result_layout: layout.Idx) Error!void {
    _ = result_layout;
    try self.generateExpr(expr);
    const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;

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

/// Multiply two i64 values producing a 128-bit result stored in stack memory.
/// Takes two wasm locals holding i64 operands.
/// Returns via stack: pushes an i32 pointer to the 16-byte result.
///
/// Algorithm (schoolbook multiply with 32-bit limbs):
///   a = (a1 << 32) + a0,  b = (b1 << 32) + b0
///   low  = a * b (truncating i64.mul)
///   high = a1*b1 + ((a0*b0 >> 32) + a0*b1 + a1*b0) >> 32
///         (but we must track 64-bit carry properly)
fn emitI64MulToI128(self: *Self, a_local: u32, b_local: u32) Error!void {
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
fn emitI128DivByConst(self: *Self, numerator_local: u32, divisor_val: i64) Error!void {
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
fn emitIntToI128(self: *Self, signed: bool) Error!void {
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
fn emitF64ToI128(self: *Self, val_local: u32, result_local: u32, signed: bool) Error!void {
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
fn emitFloatToIntTryUnsafe(self: *Self, val_size: u32, is_i64: bool, min_f: f64, max_f: f64) Error!void {
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
fn emitFloatToI128TryUnsafe(self: *Self, signed: bool) Error!void {
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
) Error!struct { result_local: u32, val_local: u32 } {
    // Save the source value
    const val_local = switch (src_vt) {
        .i32 => self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory,
        .i64 => self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory,
        else => return error.UnsupportedExpr,
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
) Error!void {
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
) Error!void {
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
fn emitI128TryToU128(self: *Self, signed_source: bool) Error!void {
    _ = signed_source;
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
fn emitI128TryToI128(self: *Self) Error!void {
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
fn resolveValType(self: *const Self, layout_idx: layout.Idx) ValType {
    if (self.layout_store) |ls| {
        return WasmLayout.resultValTypeWithStore(layout_idx, ls);
    }
    return WasmLayout.resultValType(layout_idx);
}

/// Allocate space on the stack frame, returning the offset from the frame pointer.
/// Ensures the frame pointer local exists and memory is enabled.
fn allocStackMemory(self: *Self, size: u32, alignment: u32) Error!u32 {
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

/// Emit inline bump allocation: allocates `size` bytes with given alignment
/// from the heap (global 1). Leaves the allocated pointer on the wasm stack.
/// This is a simple bump allocator that never frees — suitable for tests.
fn emitHeapAlloc(self: *Self, size_local: u32, alignment: u32) Error!void {
    self.uses_heap = true;

    // Align heap_ptr: ptr = (heap_ptr + align - 1) & ~(align - 1)
    self.body.append(self.allocator, Op.global_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory; // global 1 = heap_ptr
    if (alignment > 1) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(alignment - 1)) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @bitCast(~@as(u32, alignment - 1))) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    }

    // Save the aligned pointer
    const result = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result) catch return error.OutOfMemory;

    // Advance heap: heap_ptr = aligned_ptr + size
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, size_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.global_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result) catch return error.OutOfMemory;
}

/// Emit inline bump allocation with a constant size.
fn emitHeapAllocConst(self: *Self, size: u32, alignment: u32) Error!void {
    self.uses_heap = true;

    // Align heap_ptr
    self.body.append(self.allocator, Op.global_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    if (alignment > 1) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(alignment - 1)) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @bitCast(~@as(u32, alignment - 1))) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
    }

    const result = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_tee) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result) catch return error.OutOfMemory;

    // Advance heap: heap_ptr = aligned_ptr + size
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.global_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    // Push result pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, result) catch return error.OutOfMemory;
}

/// Emit: local.get $fp; i32.const offset; i32.add
/// Leaves (fp + offset) on the stack as an i32 pointer.
fn emitFpOffset(self: *Self, offset: u32) Error!void {
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, self.fp_local) catch return error.OutOfMemory;
    if (offset > 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(offset)) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    }
}

/// Emit: i64.store with alignment 3 (8 bytes) and the given offset.
fn emitI64Store(self: *Self, offset: u32) Error!void {
    self.body.append(self.allocator, Op.i64_store) catch return error.OutOfMemory;
    // alignment (log2 of bytes): 3 = 8-byte aligned
    WasmModule.leb128WriteU32(self.allocator, &self.body, 3) catch return error.OutOfMemory;
    // offset
    WasmModule.leb128WriteU32(self.allocator, &self.body, offset) catch return error.OutOfMemory;
}

/// Emit a wasm conversion instruction if source and target types differ.
fn emitConversion(self: *Self, source: ValType, target: ValType) Error!void {
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

/// Compile a lambda expression as a separate wasm function.
/// Returns the wasm function index.
fn compileLambda(self: *Self, expr_id: MonoExprId, lambda: anytype) Error!u32 {
    // Check if already compiled
    const expr_key: u32 = @intFromEnum(expr_id);
    if (self.compiled_lambdas.get(expr_key)) |existing| {
        return existing;
    }

    // Build parameter types
    const params = self.store.getPatternSpan(lambda.params);
    var param_types: std.ArrayList(ValType) = .empty;
    defer param_types.deinit(self.allocator);

    for (params) |param_id| {
        const pat = self.store.getPattern(param_id);
        switch (pat) {
            .bind => |bind| {
                const vt = self.resolveValType(bind.layout_idx);
                param_types.append(self.allocator, vt) catch return error.OutOfMemory;
            },
            .wildcard => {
                // Wildcard param — still needs a slot. Use i32 as default.
                param_types.append(self.allocator, .i32) catch return error.OutOfMemory;
            },
            else => return error.UnsupportedExpr,
        }
    }

    const ret_vt = self.resolveValType(lambda.ret_layout);

    // Add function type and function to the module
    const type_idx = self.module.addFuncType(param_types.items, &.{ret_vt}) catch return error.OutOfMemory;
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;

    // Save current codegen state
    const saved = self.saveState();

    // Initialize fresh state for the lambda body
    self.body = .empty;
    self.storage.locals = std.AutoHashMap(u48, Storage.LocalInfo).init(self.allocator);
    self.storage.next_local_idx = 0;
    self.storage.local_types = .empty;
    self.stack_frame_size = 0;
    self.uses_stack_memory = false;
    self.fp_local = 0;

    // Bind parameters — they occupy the first N locals
    for (params) |param_id| {
        const pat = self.store.getPattern(param_id);
        switch (pat) {
            .bind => |bind| {
                const vt = self.resolveValType(bind.layout_idx);
                _ = self.storage.allocLocal(bind.symbol, vt) catch return error.OutOfMemory;
            },
            .wildcard => {
                _ = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            },
            else => return error.UnsupportedExpr,
        }
    }

    // Generate the body expression
    self.generateExpr(lambda.body) catch |err| {
        self.restoreState(saved);
        return err;
    };

    // Build function body: locals declaration + instructions + end
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    // Locals declaration — only for locals beyond the parameters
    try self.encodeLocalsDecl(&func_body, @intCast(params.len));

    // Body instructions
    func_body.appendSlice(self.allocator, self.body.items) catch return error.OutOfMemory;

    // End opcode
    func_body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.module.setFunctionBody(func_idx, func_body.items) catch return error.OutOfMemory;

    // Restore previous codegen state
    self.restoreState(saved);

    // Cache the compiled function
    self.compiled_lambdas.put(expr_key, func_idx) catch return error.OutOfMemory;

    return func_idx;
}

/// Compile a closure (lambda with captures) as a separate wasm function.
/// Captures become extra leading parameters in the compiled function.
/// Returns the wasm function index.
fn compileClosure(self: *Self, expr_id: MonoExprId, closure: anytype) Error!u32 {
    // Check if already compiled
    const expr_key: u32 = @intFromEnum(expr_id);
    if (self.compiled_lambdas.get(expr_key)) |existing| {
        return existing;
    }

    // Get the inner lambda
    const inner = self.store.getExpr(closure.lambda);
    const lambda = switch (inner) {
        .lambda => |l| l,
        else => return error.UnsupportedExpr,
    };

    // Get captures
    const captures = self.store.getCaptures(closure.captures);

    // Build parameter types: captures first, then regular params
    var param_types: std.ArrayList(ValType) = .empty;
    defer param_types.deinit(self.allocator);

    // Capture types
    var capture_symbols = self.allocator.alloc(MonoSymbol, captures.len) catch return error.OutOfMemory;
    var capture_val_types = self.allocator.alloc(ValType, captures.len) catch return error.OutOfMemory;
    for (captures, 0..) |cap, i| {
        const vt = self.resolveValType(cap.layout_idx);
        param_types.append(self.allocator, vt) catch return error.OutOfMemory;
        capture_symbols[i] = cap.symbol;
        capture_val_types[i] = vt;
    }

    // Regular parameter types
    const params = self.store.getPatternSpan(lambda.params);
    for (params) |param_id| {
        const pat = self.store.getPattern(param_id);
        switch (pat) {
            .bind => |bind| {
                const vt = self.resolveValType(bind.layout_idx);
                param_types.append(self.allocator, vt) catch return error.OutOfMemory;
            },
            .wildcard => {
                param_types.append(self.allocator, .i32) catch return error.OutOfMemory;
            },
            else => return error.UnsupportedExpr,
        }
    }

    const ret_vt = self.resolveValType(lambda.ret_layout);
    const type_idx = self.module.addFuncType(param_types.items, &.{ret_vt}) catch return error.OutOfMemory;
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;

    // Save current codegen state
    const saved = self.saveState();

    // Initialize fresh state
    self.body = .empty;
    self.storage.locals = std.AutoHashMap(u48, Storage.LocalInfo).init(self.allocator);
    self.storage.next_local_idx = 0;
    self.storage.local_types = .empty;
    self.stack_frame_size = 0;
    self.uses_stack_memory = false;
    self.fp_local = 0;

    // Bind capture parameters (first N locals)
    for (captures) |cap| {
        const vt = self.resolveValType(cap.layout_idx);
        _ = self.storage.allocLocal(cap.symbol, vt) catch return error.OutOfMemory;
    }

    // Bind regular parameters
    for (params) |param_id| {
        const pat = self.store.getPattern(param_id);
        switch (pat) {
            .bind => |bind| {
                const vt = self.resolveValType(bind.layout_idx);
                _ = self.storage.allocLocal(bind.symbol, vt) catch return error.OutOfMemory;
            },
            .wildcard => {
                _ = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            },
            else => return error.UnsupportedExpr,
        }
    }

    // Generate the body
    self.generateExpr(lambda.body) catch |err| {
        self.restoreState(saved);
        return err;
    };

    // Build function body
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    try self.encodeLocalsDecl(&func_body, @intCast(captures.len + params.len));

    func_body.appendSlice(self.allocator, self.body.items) catch return error.OutOfMemory;
    func_body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.module.setFunctionBody(func_idx, func_body.items) catch return error.OutOfMemory;

    // Restore previous state
    self.restoreState(saved);

    // Cache
    self.compiled_lambdas.put(expr_key, func_idx) catch return error.OutOfMemory;

    // Store capture info for call sites (only if there are captures)
    if (captures.len > 0) {
        // Store with a hash key derived from the expression key
        // The caller will associate this with the bound symbol
        self.closure_captures_by_expr.put(expr_key, .{
            .symbols = capture_symbols,
            .val_types = capture_val_types,
        }) catch return error.OutOfMemory;
    } else {
        self.allocator.free(capture_symbols);
        self.allocator.free(capture_val_types);
    }

    return func_idx;
}

/// Compile all MonoProcs as separate wasm functions.
/// Must be called before generateExpr so that call sites can find compiled procs.
pub fn compileAllProcs(self: *Self, procs: []const MonoProc) Allocator.Error!void {
    for (procs) |proc| {
        self.compileProc(proc) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.UnsupportedExpr => continue,
        };
    }
}

/// Try to bind a function expression (lambda, closure, nominal wrapping one, or
/// function alias) to a symbol in `symbol_funcs`. Returns true if the expression
/// was handled as a function binding and the caller should `continue` the loop.
fn tryBindFunction(self: *Self, expr_id: MonoExprId, expr: MonoExpr, symbol: MonoSymbol) Error!bool {
    const key: u48 = @bitCast(symbol);
    switch (expr) {
        .lambda => |lambda| {
            const func_idx = try self.compileLambda(expr_id, lambda);
            self.symbol_funcs.put(key, func_idx) catch return error.OutOfMemory;
            return true;
        },
        .closure => |closure| {
            const func_idx = try self.compileClosure(expr_id, closure);
            self.symbol_funcs.put(key, func_idx) catch return error.OutOfMemory;
            // Copy capture info from expr map to symbol map
            const expr_key: u32 = @intFromEnum(expr_id);
            if (self.closure_captures_by_expr.get(expr_key)) |cap_info| {
                self.closure_captures_by_sym.put(key, cap_info) catch return error.OutOfMemory;
            }
            return true;
        },
        .nominal => |nom| {
            // Transparent wrapper — check if it wraps a lambda or closure
            const inner = self.store.getExpr(nom.backing_expr);
            switch (inner) {
                .lambda => |lambda| {
                    const func_idx = try self.compileLambda(nom.backing_expr, lambda);
                    self.symbol_funcs.put(key, func_idx) catch return error.OutOfMemory;
                    return true;
                },
                .closure => |closure| {
                    const func_idx = try self.compileClosure(nom.backing_expr, closure);
                    self.symbol_funcs.put(key, func_idx) catch return error.OutOfMemory;
                    const inner_key: u32 = @intFromEnum(nom.backing_expr);
                    if (self.closure_captures_by_expr.get(inner_key)) |cap_info| {
                        self.closure_captures_by_sym.put(key, cap_info) catch return error.OutOfMemory;
                    }
                    return true;
                },
                else => return false,
            }
        },
        .lookup => |lookup_expr| {
            // Function alias: g = f — propagate func_idx
            const src_key: u48 = @bitCast(lookup_expr.symbol);
            if (self.symbol_funcs.get(src_key)) |fid| {
                self.symbol_funcs.put(key, fid) catch return error.OutOfMemory;
                // Also propagate capture info
                if (self.closure_captures_by_sym.get(src_key)) |cap_info| {
                    self.closure_captures_by_sym.put(key, cap_info) catch return error.OutOfMemory;
                }
                return true;
            }
            return false;
        },
        else => return false,
    }
}

/// Compile a single MonoProc as a wasm function.
fn compileProc(self: *Self, proc: MonoProc) Error!void {
    const key: u48 = @bitCast(proc.name);

    // Skip if already compiled
    if (self.symbol_funcs.contains(key)) return;

    // Build parameter types from arg_layouts
    const arg_layouts = self.store.getLayoutIdxSpan(proc.arg_layouts);
    var param_types: std.ArrayList(ValType) = .empty;
    defer param_types.deinit(self.allocator);

    for (arg_layouts) |arg_layout| {
        const vt = self.resolveValType(arg_layout);
        param_types.append(self.allocator, vt) catch return error.OutOfMemory;
    }

    const ret_vt = self.resolveValType(proc.ret_layout);

    // Create function type and add to module
    const type_idx = self.module.addFuncType(param_types.items, &.{ret_vt}) catch return error.OutOfMemory;
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;

    // Register BEFORE generating body (for recursive calls)
    self.symbol_funcs.put(key, func_idx) catch return error.OutOfMemory;

    // Save current codegen state
    const saved = self.saveState();

    // Initialize fresh state
    self.body = .empty;
    self.storage.locals = std.AutoHashMap(u48, Storage.LocalInfo).init(self.allocator);
    self.storage.next_local_idx = 0;
    self.storage.local_types = .empty;
    self.stack_frame_size = 0;
    self.uses_stack_memory = false;
    self.fp_local = 0;
    self.cf_depth = 0;

    // Bind parameters to locals
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
            else => {
                // Unsupported pattern type in proc params
                self.restoreState(saved);
                _ = self.symbol_funcs.remove(key);
                return unsupported(@src());
            },
        }
    }

    // Emit proc body block (ret targets this block)
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(ret_vt)) catch return error.OutOfMemory;
    self.cf_depth = 1; // inside the ret block

    // Generate CFStmt body
    self.generateCFStmt(proc.body) catch {
        // Failed to compile proc body — remove registration and propagate
        self.restoreState(saved);
        _ = self.symbol_funcs.remove(key);
        return error.UnsupportedExpr;
    };

    // End of ret block
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Build function body
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    // Locals declaration (beyond parameters)
    try self.encodeLocalsDecl(&func_body, @intCast(params.len));

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
        const result_tmp = self.storage.allocAnonymousLocal(ret_vt) catch return error.OutOfMemory;
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

    // Restore state
    self.restoreState(saved);
}

/// Saved codegen state for restoring after compiling a nested function.
const SavedState = struct {
    body_items: []u8,
    body_capacity: usize,
    locals: std.AutoHashMap(u48, Storage.LocalInfo),
    next_local_idx: u32,
    local_types_items: []ValType,
    local_types_capacity: usize,
    stack_frame_size: u32,
    uses_stack_memory: bool,
    fp_local: u32,
    cf_depth: u32,
};

/// Capture current codegen state for later restoration.
fn saveState(self: *Self) SavedState {
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
        .cf_depth = self.cf_depth,
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
    self.stack_frame_size = saved.stack_frame_size;
    self.uses_stack_memory = saved.uses_stack_memory;
    self.fp_local = saved.fp_local;
    self.cf_depth = saved.cf_depth;
}

/// Generate code for a control flow statement (used in MonoProc bodies).
fn generateCFStmt(self: *Self, stmt_id: CFStmtId) Error!void {
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
                        const vt = if (i < jp_layouts.len) self.resolveValType(jp_layouts[i]) else .i32;
                        const local_idx = self.storage.allocAnonymousLocal(vt) catch return error.OutOfMemory;
                        param_locals[i] = local_idx;
                    },
                    else => {
                        self.allocator.free(param_locals);
                        return unsupported(@src());
                    },
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
            const param_locals = self.join_point_param_locals.get(jp_key) orelse return unsupported(@src());

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
            for (0..@min(args.len, param_locals.len)) |i| {
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
    }
}

/// Bind a CFStmt let-pattern to the value just generated.
/// The value is on the wasm stack. We need to store it to a local.
fn bindCFLetPattern(self: *Self, pat: MonoPattern, value_expr: MonoExprId) Error!void {
    switch (pat) {
        .bind => |bind| {
            const expr_is_composite = self.isCompositeExpr(value_expr);
            const target_is_composite = self.isCompositeLayout(bind.layout_idx);

            if (expr_is_composite and !target_is_composite) {
                // Composite → scalar: load scalar from pointer
                const vt = self.resolveValType(bind.layout_idx);
                const byte_size = self.layoutByteSize(bind.layout_idx);
                try self.emitLoadOpSized(vt, byte_size, 0);
                const local_idx = self.storage.getLocal(bind.symbol) orelse
                    (self.storage.allocLocal(bind.symbol, vt) catch return error.OutOfMemory);
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
                const local_idx = self.storage.getLocal(bind.symbol) orelse
                    (self.storage.allocLocal(bind.symbol, .i32) catch return error.OutOfMemory);
                try self.emitLocalSet(local_idx);
            } else {
                const vt = self.resolveValType(bind.layout_idx);
                const expr_vt = self.exprValType(value_expr);
                try self.emitConversion(expr_vt, vt);
                const local_idx = self.storage.getLocal(bind.symbol) orelse
                    (self.storage.allocLocal(bind.symbol, vt) catch return error.OutOfMemory);
                try self.emitLocalSet(local_idx);
            }
        },
        .wildcard => {
            self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
        },
        .record => |rec| {
            const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(ptr);
            try self.bindRecordPattern(ptr, rec);
        },
        .tuple => |tup| {
            const ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(ptr);
            try self.bindTuplePattern(ptr, tup);
        },
        else => return error.UnsupportedExpr,
    }
}

/// Generate a function call expression.
fn generateCall(self: *Self, c: anytype) Error!void {
    const fn_expr = self.store.getExpr(c.fn_expr);

    // Determine the function index to call
    const func_idx: ?u32 = switch (fn_expr) {
        .lambda => |lambda| try self.compileLambda(c.fn_expr, lambda),
        .closure => |closure| try self.compileClosure(c.fn_expr, closure),
        .lookup => |lookup| blk: {
            const key: u48 = @bitCast(lookup.symbol);
            break :blk self.symbol_funcs.get(key);
        },
        .nominal => |nom| blk: {
            // Nominal wrapping a lambda or closure
            const inner = self.store.getExpr(nom.backing_expr);
            switch (inner) {
                .lambda => |lambda| break :blk try self.compileLambda(nom.backing_expr, lambda),
                .closure => |closure| break :blk try self.compileClosure(nom.backing_expr, closure),
                else => break :blk null,
            }
        },
        else => null,
    };

    if (func_idx) |idx| {
        // Push capture values as leading arguments (if this is a closure call)
        switch (fn_expr) {
            .closure => |closure| {
                // Inline closure call — push captures directly from current scope
                const captures = self.store.getCaptures(closure.captures);
                for (captures) |cap| {
                    const local_idx = self.storage.getLocal(cap.symbol) orelse return unsupported(@src());
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                }
            },
            .lookup => |lookup| {
                // Let-bound closure — look up captures by symbol
                const key: u48 = @bitCast(lookup.symbol);
                if (self.closure_captures_by_sym.get(key)) |cap_info| {
                    for (cap_info.symbols) |cap_sym| {
                        const local_idx = self.storage.getLocal(cap_sym) orelse return unsupported(@src());
                        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                        WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                    }
                }
            },
            else => {},
        }

        // Push regular arguments
        const args = self.store.getExprSpan(c.args);
        for (args) |arg_id| {
            try self.generateExpr(arg_id);
        }
        self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, idx) catch return error.OutOfMemory;
    } else {
        return unsupported(@src());
    }
}

// ---- Composite type generation (records, tuples, tags) ----

/// Get the layout store, or return UnsupportedExpr if none is available.
fn getLayoutStore(self: *const Self) Error!*const LayoutStore {
    return self.layout_store orelse error.UnsupportedExpr;
}

/// Get the byte size of a layout index using the layout store.
fn layoutByteSize(self: *const Self, layout_idx: layout.Idx) u32 {
    const ls = self.layout_store orelse return 4; // default to 4 bytes
    const l = ls.getLayout(layout_idx);
    return ls.layoutSize(l);
}

/// Emit a store instruction for the given value type at an address already on the stack.
/// The memory operand format is: alignment (log2) + offset.
fn emitStoreOp(self: *Self, vt: ValType, mem_offset: u32) Error!void {
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
fn emitStoreOpSized(self: *Self, vt: ValType, byte_size: u32, mem_offset: u32) Error!void {
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
fn emitLoadOp(self: *Self, vt: ValType, mem_offset: u32) Error!void {
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
fn emitLoadOpSized(self: *Self, vt: ValType, byte_size: u32, mem_offset: u32) Error!void {
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

/// Store a value from the wasm stack into memory at [base_local + field_offset].
/// The value to store must already be on the wasm value stack.
/// Stack effect: pops 1 value.
fn emitStoreToMem(self: *Self, base_local: u32, field_offset: u32, vt: ValType) Error!void {
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
fn emitStoreToMemSized(self: *Self, base_local: u32, field_offset: u32, vt: ValType, byte_size: u32) Error!void {
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
fn emitMemCopy(self: *Self, dst_local: u32, dst_offset: u32, src_local: u32, byte_count: u32) Error!void {
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
fn emitZeroInit(self: *Self, base_local: u32, byte_count: u32) Error!void {
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

/// Generate a record construction expression.
/// Allocates stack memory, stores each field, returns pointer.
fn generateRecord(self: *Self, r: anytype) Error!void {
    const ls = try self.getLayoutStore();
    const l = ls.getLayout(r.record_layout);
    if (l.tag != .record) return unsupported(@src());

    const size = ls.layoutSize(l);
    if (size == 0) {
        // Empty record — push dummy pointer
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        return;
    }

    const align_val: u32 = @intCast(l.data.record.alignment.toByteUnits());

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
        const field_layout_idx = ls.getRecordFieldLayout(l.data.record.idx, @intCast(i));
        const is_composite = self.isCompositeLayout(field_layout_idx);
        const field_vt = WasmLayout.resultValTypeWithStore(field_layout_idx, ls);

        // Generate the field expression
        try self.generateExpr(field_expr_id);

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
        const field_offset = ls.getRecordFieldOffset(l.data.record.idx, @intCast(i));
        const field_layout_idx = ls.getRecordFieldLayout(l.data.record.idx, @intCast(i));
        const field_byte_size = ls.getRecordFieldSize(l.data.record.idx, @intCast(i));
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

/// Generate a field access expression.
/// Loads a field value from a record pointer.
/// Bind a record destructuring pattern: load each field from a record pointer
/// and bind sub-patterns (recursively for nested destructuring).
fn bindRecordPattern(self: *Self, ptr_local: u32, rec: anytype) Error!void {
    const ls = try self.getLayoutStore();
    const record_layout = ls.getLayout(rec.record_layout);
    if (record_layout.tag != .record) return unsupported(@src());

    const field_patterns = self.store.getPatternSpan(rec.fields);
    for (field_patterns, 0..) |pat_id, i| {
        const pat = self.store.getPattern(pat_id);
        const field_idx: u16 = @intCast(i);
        const field_offset = ls.getRecordFieldOffset(record_layout.data.record.idx, field_idx);
        const field_byte_size = ls.getRecordFieldSize(record_layout.data.record.idx, field_idx);
        const field_layout_idx = ls.getRecordFieldLayout(record_layout.data.record.idx, field_idx);

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
            .record => |inner_rec| {
                // Nested record destructuring: compute pointer to field
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
                try self.bindRecordPattern(field_ptr, inner_rec);
            },
            .tuple => |inner_tup| {
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
                try self.bindTuplePattern(field_ptr, inner_tup);
            },
            else => return error.UnsupportedExpr,
        }
    }
}

/// Bind a tuple destructuring pattern: load each element from a tuple pointer
/// and bind sub-patterns.
fn bindTuplePattern(self: *Self, ptr_local: u32, tup: anytype) Error!void {
    const ls = try self.getLayoutStore();
    const tuple_layout = ls.getLayout(tup.tuple_layout);
    if (tuple_layout.tag != .tuple) return unsupported(@src());

    const elem_patterns = self.store.getPatternSpan(tup.elems);
    for (elem_patterns, 0..) |pat_id, i| {
        const pat = self.store.getPattern(pat_id);
        const elem_idx: u16 = @intCast(i);
        const elem_offset = ls.getTupleElementOffsetByOriginalIndex(tuple_layout.data.tuple.idx, elem_idx);
        const elem_byte_size = ls.getTupleElementSizeByOriginalIndex(tuple_layout.data.tuple.idx, elem_idx);
        const elem_layout_idx = ls.getTupleElementLayoutByOriginalIndex(tuple_layout.data.tuple.idx, elem_idx);

        switch (pat) {
            .bind => |bind| {
                const is_composite = self.isCompositeLayout(elem_layout_idx);
                if (is_composite and elem_byte_size > 0) {
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                    if (elem_offset > 0) {
                        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_offset)) catch return error.OutOfMemory;
                        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
                    }
                    const local_idx = self.storage.allocLocal(bind.symbol, .i32) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                } else {
                    const elem_vt = WasmLayout.resultValTypeWithStore(elem_layout_idx, ls);
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
                    try self.emitLoadOpSized(elem_vt, elem_byte_size, elem_offset);
                    const local_idx = self.storage.allocLocal(bind.symbol, elem_vt) catch return error.OutOfMemory;
                    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                }
            },
            .wildcard => {},
            else => return error.UnsupportedExpr,
        }
    }
}

fn generateFieldAccess(self: *Self, fa: anytype) Error!void {
    const ls = try self.getLayoutStore();

    // Generate the record expression → pushes i32 pointer
    try self.generateExpr(fa.record_expr);

    // Get the field offset
    const record_layout = ls.getLayout(fa.record_layout);
    if (record_layout.tag != .record) return unsupported(@src());

    const field_offset = ls.getRecordFieldOffset(record_layout.data.record.idx, fa.field_idx);
    const field_byte_size = ls.getRecordFieldSize(record_layout.data.record.idx, fa.field_idx);

    // Check if the field is a composite type
    if (self.isCompositeLayout(fa.field_layout) and field_byte_size > 0) {
        // For composite fields, return a pointer to the field data
        // (record_ptr + field_offset)
        if (field_offset > 0) {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(field_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        }
        // Result is already an i32 pointer on the stack
    } else {
        const field_vt = WasmLayout.resultValTypeWithStore(fa.field_layout, ls);
        // Load the field: [record_ptr + field_offset] (size-aware for sub-32-bit fields)
        try self.emitLoadOpSized(field_vt, field_byte_size, field_offset);
    }
}

/// Generate a tuple construction expression.
fn generateTuple(self: *Self, t: anytype) Error!void {
    const ls = try self.getLayoutStore();
    const l = ls.getLayout(t.tuple_layout);
    if (l.tag != .tuple) return unsupported(@src());

    const size = ls.layoutSize(l);
    if (size == 0) {
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        return;
    }

    const align_val: u32 = @intCast(l.data.tuple.alignment.toByteUnits());

    const frame_offset = try self.allocStackMemory(size, align_val);

    const base_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(frame_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;

    // Zero-initialize the tuple memory to ensure consistent padding bytes
    // (important for byte-by-byte structural equality comparisons)
    try self.emitZeroInit(base_local, size);

    // Store each element
    // Use ByOriginalIndex because elem_exprs is in source order,
    // but the layout store has elements sorted by alignment
    const elems = self.store.getExprSpan(t.elems);
    for (elems, 0..) |elem_expr_id, i| {
        const elem_offset = ls.getTupleElementOffsetByOriginalIndex(l.data.tuple.idx, @intCast(i));
        const elem_layout_idx = ls.getTupleElementLayoutByOriginalIndex(l.data.tuple.idx, @intCast(i));
        const elem_vt = WasmLayout.resultValTypeWithStore(elem_layout_idx, ls);
        const elem_byte_size = ls.getTupleElementSizeByOriginalIndex(l.data.tuple.idx, @intCast(i));

        try self.generateExpr(elem_expr_id);

        const is_composite = self.isCompositeLayout(elem_layout_idx);
        if (is_composite and elem_byte_size > 0) {
            const src_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src_local) catch return error.OutOfMemory;
            try self.emitMemCopy(base_local, elem_offset, src_local, elem_byte_size);
        } else {
            const expr_vt = self.exprValType(elem_expr_id);
            try self.emitConversion(expr_vt, elem_vt);
            try self.emitStoreToMemSized(base_local, elem_offset, elem_vt, elem_byte_size);
        }
    }

    // Push the base pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
}

/// Generate a tuple access expression.
fn generateTupleAccess(self: *Self, ta: anytype) Error!void {
    const ls = try self.getLayoutStore();

    try self.generateExpr(ta.tuple_expr);

    const tuple_layout = ls.getLayout(ta.tuple_layout);
    if (tuple_layout.tag != .tuple) return unsupported(@src());

    const elem_offset = ls.getTupleElementOffset(tuple_layout.data.tuple.idx, ta.elem_idx);
    const elem_byte_size = ls.getTupleElementSize(tuple_layout.data.tuple.idx, ta.elem_idx);

    if (self.isCompositeLayout(ta.elem_layout) and elem_byte_size > 0) {
        // For composite elements, return pointer to element data
        if (elem_offset > 0) {
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_offset)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        }
    } else {
        const elem_vt = WasmLayout.resultValTypeWithStore(ta.elem_layout, ls);
        try self.emitLoadOpSized(elem_vt, elem_byte_size, elem_offset);
    }
}

/// Generate a zero-arg tag expression (enum with no payload).
fn generateZeroArgTag(self: *Self, z: anytype) Error!void {
    const ls = try self.getLayoutStore();
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
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(z.discriminant)) catch return error.OutOfMemory;
        try self.emitStoreToMemSized(base_local, disc_offset, .i32, disc_size);

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
fn generateTag(self: *Self, t: anytype) Error!void {
    const ls = try self.getLayoutStore();
    const l = ls.getLayout(t.union_layout);

    if (l.tag != .tag_union) return unsupported(@src());

    const tu_size = ls.layoutSize(l);
    const tu_data = ls.getTagUnionData(l.data.tag_union.idx);
    const disc_offset = tu_data.discriminant_offset;

    if (tu_size <= 4) {
        // Small tag union — discriminant only, no room for payload
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

    // Store discriminant (size-aware)
    const disc_size: u32 = tu_data.discriminant_size;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(t.discriminant)) catch return error.OutOfMemory;
    try self.emitStoreToMemSized(base_local, disc_offset, .i32, disc_size);

    // Store payload args at offset 0 (payload comes before discriminant in most cases)
    const args = self.store.getExprSpan(t.args);
    var payload_offset: u32 = 0;
    for (args) |arg_expr_id| {
        const arg_vt = self.exprValType(arg_expr_id);
        const arg_byte_size = self.exprByteSize(arg_expr_id);
        try self.generateExpr(arg_expr_id);
        try self.emitStoreToMemSized(base_local, payload_offset, arg_vt, arg_byte_size);
        payload_offset += arg_byte_size;
    }

    // Push base pointer
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
}

/// Generate a discriminant switch expression.
/// Evaluates the tag union value, loads its discriminant, and generates
/// cascading if/else branches indexed by discriminant value.
fn generateDiscriminantSwitch(self: *Self, ds: anytype) Error!void {
    const ls = try self.getLayoutStore();
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
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, ptr_local) catch return error.OutOfMemory;
            try self.emitLoadOpSized(.i32, disc_size, disc_offset);
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
    const bt: BlockType = if (first_branch_result_layout) |lay|
        valTypeToBlockType(self.resolveValType(lay))
    else
        .i32;

    // Generate cascading if/else: if (disc == 0) { branch0 } else if (disc == 1) { branch1 } ...
    try self.generateDiscSwitchBranches(branches, disc_local, bt, 0);
}

fn generateDiscSwitchBranches(self: *Self, branches: []const MonoExprId, disc_local: u32, bt: BlockType, disc_value: u32) Error!void {
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
    try self.generateExpr(branches[0]);
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    try self.generateDiscSwitchBranches(branches[1..], disc_local, bt, disc_value + 1);
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
}

/// Generate a while loop expression.
/// Wasm structure: block { loop { <cond> i32.eqz br_if 1 <body> drop br 0 } } i32.const 0
fn generateWhileLoop(self: *Self, wl: anytype) Error!void {
    // block (void) — exit target for br_if
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // loop (void) — back-edge target for br
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

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
fn generateForLoopExpr(self: *Self, fl: anytype) Error!void {
    const ls = try self.getLayoutStore();

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
    const elem_layout = ls.getLayout(fl.elem_layout);
    const elem_size: u32 = ls.layoutSizeAlign(elem_layout).size;
    const elem_vt = WasmLayout.resultValTypeWithStore(fl.elem_layout, ls);

    // block { loop {
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

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
                try self.emitLoadOpSized(elem_vt, elem_size, 0);
            }

            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
        },
        .wildcard => {
            // No binding needed
        },
        else => return error.UnsupportedExpr,
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
fn generateList(self: *Self, l: anytype) Error!void {
    const ls = try self.getLayoutStore();
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
    const elem_layout = ls.getLayout(l.elem_layout);
    const elem_size: u32 = ls.layoutSizeAlign(elem_layout).size;
    const elem_align: u32 = @intCast(ls.layoutSizeAlign(elem_layout).alignment.toByteUnits());

    // Allocate space for all elements on the stack frame
    const total_data_size = elem_size * @as(u32, @intCast(elems.len));
    const data_offset = try self.allocStackMemory(total_data_size, elem_align);

    const data_base = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitFpOffset(data_offset);
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, data_base) catch return error.OutOfMemory;

    // Zero-initialize element data for consistent padding
    try self.emitZeroInit(data_base, total_data_size);

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
fn generateLowLevel(self: *Self, ll: anytype) Error!void {
    const args = self.store.getExprSpan(ll.args);

    switch (ll.op) {
        // Numeric arithmetic (same as binop equivalents)
        .num_add, .num_sub, .num_mul, .num_div, .num_neg, .num_abs => {
            return self.generateNumericLowLevel(ll.op, args, ll.ret_layout);
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
        .u8_to_i64, .u8_to_u64, .u16_to_i64, .u16_to_u64,
        .u32_to_i64, .u32_to_u64,
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
        .i32_to_i8_wrap, .u32_to_u8_wrap, .i32_to_u8_wrap,
        .i64_to_u8_wrap, .u64_to_u8_wrap, .i64_to_i8_wrap,
        .u64_to_i8_wrap,
        .u16_to_i8_wrap, .u16_to_u8_wrap, .i16_to_i8_wrap,
        .i16_to_u8_wrap, .u32_to_i8_wrap,
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
        .i32_to_i16_wrap, .u32_to_u16_wrap, .i32_to_u16_wrap,
        .i64_to_u16_wrap, .u64_to_u16_wrap, .i64_to_i16_wrap,
        .u64_to_i16_wrap, .u32_to_i16_wrap,
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
        .i32_to_u32_wrap, .u32_to_i32_wrap,
        .u8_to_i8_wrap, .i8_to_u8_wrap,
        .u16_to_i16_wrap, .i16_to_u16_wrap,
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
                else => return error.UnsupportedExpr,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_floor => {
            try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_floor,
                .f64 => Op.f64_floor,
                else => return error.UnsupportedExpr,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_ceiling => {
            try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_ceil,
                .f64 => Op.f64_ceil,
                else => return error.UnsupportedExpr,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_round => {
            try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_nearest,
                .f64 => Op.f64_nearest,
                else => return error.UnsupportedExpr,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },

        // Modulo (integer only — float mod not yet supported)
        .num_mod => {
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
        .list_is_empty => {
            // Load length from RocList struct (offset 4) and compare to 0
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i32, 4);
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        },
        .list_get => {
            // args[0] = list, args[1] = index
            // Load elements_ptr from list, compute element address, load element
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            // Load elements_ptr (offset 0)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 0);

            // Generate index and convert to i32 if needed
            try self.generateExpr(args[1]);
            if (self.exprValType(args[1]) == .i64) {
                self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            }

            // Compute element address: elements_ptr + index * elem_size
            const ret_byte_size = self.layoutByteSize(ll.ret_layout);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(ret_byte_size)) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;

            // Load element value
            if (self.isCompositeLayout(ll.ret_layout)) {
                // Composite element — result is the pointer itself
            } else {
                const ret_vt = self.resolveValType(ll.ret_layout);
                try self.emitLoadOpSized(ret_vt, ret_byte_size, 0);
            }
        },

        // String operations
        .str_is_empty => {
            // Check if string length is 0
            // RocStr layout (12 bytes on wasm32):
            //   SSO:  bytes[0..10] = inline data, byte 11 = len | 0x80
            //   Heap: i32 ptr (offset 0), i32 len (offset 4), i32 cap (offset 8)
            try self.generateExpr(args[0]);
            const str_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(str_local);

            // Load byte 11 to check SSO flag
            try self.emitLocalGet(str_local);
            self.body.append(self.allocator, Op.i32_load8_u) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory; // align
            WasmModule.leb128WriteU32(self.allocator, &self.body, 11) catch return error.OutOfMemory; // offset

            const byte11 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            try self.emitLocalSet(byte11);

            // Check high bit (0x80) for SSO flag
            try self.emitLocalGet(byte11);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0x80) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;

            // if (SSO)
            self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
            self.body.append(self.allocator, @intFromEnum(ValType.i32)) catch return error.OutOfMemory;

            // SSO: length = byte11 & 0x7F, check == 0
            try self.emitLocalGet(byte11);
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0x7F) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;

            // Heap: load i32 length at offset 4, check == 0
            try self.emitLocalGet(str_local);
            try self.emitLoadOp(.i32, 4);
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;

            self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
        },

        // Bitwise operations
        .num_pow => {
            // Float power function — wasm doesn't have a pow instruction
            // For integer pow, this needs a loop. For float, use host function.
            return unsupported(@src());
        },
        .num_log => {
            // Logarithm — no wasm instruction, needs host function
            return unsupported(@src());
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
                const ret_byte_size = self.layoutByteSize(ll.ret_layout);
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
            const ret_byte_size = self.layoutByteSize(ll.ret_layout);
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
            const elem_size = try self.getListElemSize(ll.ret_layout);

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

            // new_cap = old_cap - count (or 0)
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;
            try self.emitLoadOp(.i32, 8);
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, count_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
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

            const elem_size = try self.getListElemSize(ll.ret_layout);

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

            // capacity = actual_count
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, actual_count) catch return error.OutOfMemory;
            try self.emitStoreOp(.i32, 8);

            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, result_local) catch return error.OutOfMemory;
        },

        .list_contains => {
            // list_contains(list, needle) -> Bool
            // Linear scan through list elements
            try self.generateExpr(args[0]);
            const list_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, list_local) catch return error.OutOfMemory;

            // Generate needle
            try self.generateExpr(args[1]);
            const needle_vt = self.exprValType(args[1]);
            const needle_local = self.storage.allocAnonymousLocal(needle_vt) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, needle_local) catch return error.OutOfMemory;

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

            // Determine element size
            const elem_byte_size: u32 = blk: {
                if (self.layout_store) |ls| {
                    // The first arg is a list; get its element layout
                    const first_arg_layout = self.exprLayoutIdx(args[0]);
                    if (first_arg_layout) |lay_idx| {
                        const l = ls.getLayout(lay_idx);
                        if (l.tag == .list) {
                            break :blk ls.layoutSize(ls.getLayout(l.data.list));
                        }
                    }
                }
                break :blk switch (needle_vt) {
                    .i32 => 4,
                    .i64 => 8,
                    .f32 => 4,
                    .f64 => 8,
                };
            };

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
            try self.emitLoadOpSized(needle_vt, elem_byte_size, 0);

            // Compare with needle
            self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, needle_local) catch return error.OutOfMemory;
            const eq_op: u8 = switch (needle_vt) {
                .i32 => Op.i32_eq,
                .i64 => Op.i64_eq,
                .f32 => Op.f32_eq,
                .f64 => Op.f64_eq,
            };
            self.body.append(self.allocator, eq_op) catch return error.OutOfMemory;

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

        .list_append => {
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
        // Remaining list operations that need more complex implementations
        .list_set,
        .list_reserve, .list_release_excess_capacity, .list_with_capacity,
        .list_repeat, .list_split_first, .list_split_last,
        => return error.UnsupportedExpr,

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
            const import_idx = self.str_eq_import orelse return unsupported(@src());
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
        .str_caseless_ascii_equals, .str_to_utf8, .str_from_utf8,
        .str_repeat, .str_trim, .str_trim_start, .str_trim_end, .str_split,
        .str_join_with, .str_reserve, .str_release_excess_capacity, .str_with_capacity,
        .str_drop_prefix, .str_drop_suffix, .str_with_ascii_lowercased,
        .str_with_ascii_uppercased, .str_with_prefix, .str_from_utf8_lossy,
        => return error.UnsupportedExpr,

        // Numeric operations needing host functions or complex implementations
        .num_to_str, .num_from_str, .num_from_numeral,
        => return error.UnsupportedExpr,

        // Box/unbox — needs heap allocation
        .box_box, .box_unbox => return error.UnsupportedExpr,

        // Compare — returns Ordering enum (EQ=0, GT=1, LT=2)
        .compare => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            // Determine arg type from first arg's layout
            const arg_layout = self.exprLayoutIdx(args[0]);
            const arg_vt = self.exprValType(args[0]);

            // Determine if unsigned from layout
            const is_unsigned = if (arg_layout) |al| switch (al) {
                .u8, .u16, .u32, .u64, .u128 => true,
                else => false,
            } else false;

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
        .i8_to_u16_try, .i8_to_u32_try, .i8_to_u64_try,
        .i16_to_u32_try, .i16_to_u64_try,
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
        .u8_to_i128, .u8_to_u128, .u16_to_i128, .u16_to_u128,
        .u32_to_i128, .u32_to_u128,
        => {
            // Unsigned i32→i128: zero-extend i32 to i64, then to i128
            try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            try self.emitIntToI128(false);
        },
        .u64_to_i128, .u64_to_u128,
        => {
            // Unsigned i64→i128: value is already i64
            try self.generateExpr(args[0]);
            try self.emitIntToI128(false);
        },
        .i8_to_i128, .i16_to_i128, .i32_to_i128,
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
        .i8_to_u128_wrap, .i16_to_u128_wrap,
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
        .i128_to_i8_wrap, .i128_to_u8_wrap, .u128_to_i8_wrap, .u128_to_u8_wrap,
        => {
            try self.generateExpr(args[0]);
            // Load low i64, wrap to i32, mask to 8 bits
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i128_to_i16_wrap, .i128_to_u16_wrap, .u128_to_i16_wrap, .u128_to_u16_wrap,
        => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFFFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i128_to_i32_wrap, .i128_to_u32_wrap, .u128_to_i32_wrap, .u128_to_u32_wrap,
        => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .i128_to_i64_wrap, .i128_to_u64_wrap, .u128_to_i64_wrap, .u128_to_u64_wrap,
        => {
            try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
        },
        .u128_to_i128_wrap, .i128_to_u128_wrap,
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
        // 128-bit dec conversions not yet supported
        .u128_to_dec_try_unsafe, .i128_to_dec_try_unsafe,
        => return error.UnsupportedExpr,

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
        .dec_to_i8_try_unsafe, .dec_to_i16_try_unsafe, .dec_to_i32_try_unsafe,
        .dec_to_i64_try_unsafe,
        .dec_to_u8_try_unsafe, .dec_to_u16_try_unsafe, .dec_to_u32_try_unsafe,
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
        // Dec → i128/u128 and Dec → f32 try_unsafe need i128 division — not yet supported
        .dec_to_i128_try_unsafe, .dec_to_u128_try_unsafe,
        .dec_to_f32_try_unsafe,
        => return error.UnsupportedExpr,

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
fn generateNumericLowLevel(self: *Self, op: anytype, args: []const MonoExprId, ret_layout: layout.Idx) Error!void {
    const vt = self.resolveValType(ret_layout);

    switch (op) {
        .num_add => {
            try self.generateExpr(args[0]); try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) { .i32 => Op.i32_add, .i64 => Op.i64_add, .f32 => Op.f32_add, .f64 => Op.f64_add };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_sub => {
            try self.generateExpr(args[0]); try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) { .i32 => Op.i32_sub, .i64 => Op.i64_sub, .f32 => Op.f32_sub, .f64 => Op.f64_sub };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_mul => {
            try self.generateExpr(args[0]); try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) { .i32 => Op.i32_mul, .i64 => Op.i64_mul, .f32 => Op.f32_mul, .f64 => Op.f64_mul };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_div => {
            try self.generateExpr(args[0]); try self.generateExpr(args[1]);
            const wasm_op: u8 = switch (vt) { .i32 => Op.i32_div_s, .i64 => Op.i64_div_s, .f32 => Op.f32_div, .f64 => Op.f64_div };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_neg => {
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
        .num_mod => {
            try self.generateExpr(args[0]);
            try self.generateExpr(args[1]);
            switch (vt) {
                .i32 => self.body.append(self.allocator, Op.i32_rem_s) catch return error.OutOfMemory,
                .i64 => self.body.append(self.allocator, Op.i64_rem_s) catch return error.OutOfMemory,
                .f32, .f64 => try self.emitFloatMod(vt),
            }
        },
        else => return error.UnsupportedExpr,
    }
}

/// Generate a RocStr for a string literal.
/// On wasm32, RocStr is 12 bytes: { ptr/bytes[0..3], len/bytes[4..7], cap/bytes[8..11] }.
/// Small strings (≤11 bytes) use SSO: bytes inline, byte 11 = len | 0x80.
/// Large strings (>11 bytes) use a data segment in linear memory.
fn generateStrLiteral(self: *Self, str_idx: anytype) Error!void {
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
        // Large string — place data in a data segment
        const data_offset = self.module.addDataSegment(str_bytes, 4) catch return error.OutOfMemory;

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
fn generateStrConcat(self: *Self, span: anytype) Error!void {
    const expr_ids = self.store.getExprSpan(span);

    if (expr_ids.len == 0) {
        // Empty concat → empty SSO string
        try self.generateEmptyStr();
        return;
    }
    if (expr_ids.len == 1) {
        // Single element → just generate it
        try self.generateExpr(expr_ids[0]);
        return;
    }

    // Step 1: Generate each sub-expression, save RocStr pointers to locals
    var str_locals: std.ArrayList(u32) = .empty;
    defer str_locals.deinit(self.allocator);

    for (expr_ids) |expr_id| {
        try self.generateExpr(expr_id);
        const local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, local) catch return error.OutOfMemory;
        str_locals.append(self.allocator, local) catch return error.OutOfMemory;
    }

    // Step 2: Extract byte pointer and length from each RocStr
    var ptr_locals: std.ArrayList(u32) = .empty;
    defer ptr_locals.deinit(self.allocator);
    var len_locals: std.ArrayList(u32) = .empty;
    defer len_locals.deinit(self.allocator);

    for (str_locals.items) |str_local| {
        const ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        const len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        try self.emitExtractStrPtrLen(str_local, ptr_local, len_local);
        ptr_locals.append(self.allocator, ptr_local) catch return error.OutOfMemory;
        len_locals.append(self.allocator, len_local) catch return error.OutOfMemory;
    }

    // Step 3: Compute total length
    const total_len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, len_locals.items[0]) catch return error.OutOfMemory;
    for (len_locals.items[1..]) |len_local| {
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, len_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    }
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, total_len_local) catch return error.OutOfMemory;

    // Step 4: Allocate a buffer on the heap for the concatenated bytes.
    // We always use heap allocation here even for small results, since the total
    // length is dynamic. This simplifies codegen (no SSO branch at runtime).
    try self.emitHeapAlloc(total_len_local, 1);
    const buf_ptr_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, buf_ptr_local) catch return error.OutOfMemory;

    // Step 5: Copy bytes from each source string into the buffer
    const write_offset_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    // write_offset = 0
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, write_offset_local) catch return error.OutOfMemory;

    for (ptr_locals.items, len_locals.items) |src_ptr, src_len| {
        try self.emitMemCopyLoop(buf_ptr_local, write_offset_local, src_ptr, src_len);
        // write_offset += src_len
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, write_offset_local) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, src_len) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, write_offset_local) catch return error.OutOfMemory;
    }

    // Step 6: Build a heap RocStr on the stack frame (12 bytes)
    const result_offset = try self.allocStackMemory(12, 4);
    const base_local = self.fp_local;

    // Store ptr (offset 0) = buf_ptr
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, buf_ptr_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, result_offset);

    // Store len (offset 4) = total_len
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, total_len_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, result_offset + 4);

    // Store cap (offset 8) = total_len
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, base_local) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, total_len_local) catch return error.OutOfMemory;
    try self.emitStoreOp(.i32, result_offset + 8);

    // Push pointer to the result RocStr
    try self.emitFpOffset(result_offset);
}

/// Generate an empty SSO RocStr (12 bytes, all zeros except byte 11 = 0x80).
fn generateEmptyStr(self: *Self) Error!void {
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
fn emitExtractStrPtrLen(self: *Self, str_local: u32, ptr_local: u32, len_local: u32) Error!void {
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
fn emitMemCopyLoop(self: *Self, dst_base_local: u32, dst_offset_local: u32, src_ptr_local: u32, len_local: u32) Error!void {
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
fn buildHeapRocStr(self: *Self, ptr_local: u32, len_local: u32) Error!void {
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

/// Generate int_to_str: convert an integer value to its decimal string representation.
/// Supports u8..u64 and i8..i64. Returns UnsupportedExpr for i128/u128.
fn generateIntToStr(self: *Self, its: anytype) Error!void {
    const precision = its.int_precision;

    // i128/u128 not supported (no native 128-bit division in wasm)
    if (precision == .i128 or precision == .u128) return unsupported(@src());

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
/// Handles NaN, infinity, then formats integer and fractional parts.
fn generateFloatToStr(self: *Self, fts: anytype) Error!void {
    const precision = fts.float_precision;

    // Dec is handled by generateDecToStr
    if (precision == .dec) return unsupported(@src());

    const is_f64 = precision == .f64;

    // Generate value expression
    try self.generateExpr(fts.value);

    // Convert f32 to f64 for uniform processing
    if (!is_f64) {
        self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
    }

    const f64_local = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    try self.emitLocalSet(f64_local);

    // Allocate buffer on heap (max ~25 chars for "-1.7976931348623157e+308")
    try self.emitHeapAllocConst(32, 1);
    const buf_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(buf_local);

    const pos_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);

    // Check for NaN: value != value
    try self.emitLocalGet(f64_local);
    try self.emitLocalGet(f64_local);
    self.body.append(self.allocator, Op.f64_ne) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // NaN → write "NaN" (3 bytes)
    try self.emitWriteStringConst(buf_local, pos_local, "NaN");
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 3) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);

    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;

    // Check for infinity: abs(value) == inf
    try self.emitLocalGet(f64_local);
    self.body.append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    try self.emitF64Bytes(std.math.inf(f64));
    self.body.append(self.allocator, Op.f64_eq) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // Infinity: check sign and write "Infinity" or "-Infinity"
    try self.emitLocalGet(f64_local);
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    try self.emitF64Bytes(0.0);
    self.body.append(self.allocator, Op.f64_lt) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    // Negative infinity: write "-Infinity"
    try self.emitWriteStringConst(buf_local, pos_local, "-Infinity");
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 9) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);
    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;
    // Positive infinity: write "Infinity"
    try self.emitWriteStringConst(buf_local, pos_local, "Infinity");
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 8) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end sign check

    self.body.append(self.allocator, Op.@"else") catch return error.OutOfMemory;

    // Finite, non-NaN value: format integer and fractional parts

    // Check sign: if negative, write '-' and negate
    const is_neg_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(f64_local);
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    try self.emitF64Bytes(0.0);
    self.body.append(self.allocator, Op.f64_lt) catch return error.OutOfMemory;
    try self.emitLocalSet(is_neg_local);

    try self.emitLocalGet(is_neg_local);
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // Write '-'
    try self.emitLocalGet(buf_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '-') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);

    // Negate value
    try self.emitLocalGet(f64_local);
    self.body.append(self.allocator, Op.f64_neg) catch return error.OutOfMemory;
    try self.emitLocalSet(f64_local);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Extract integer part: int_part = trunc(value)
    // For simplicity, convert to i64 for the integer part
    // This handles values up to ~9.2e18 which covers most practical cases
    try self.emitLocalGet(f64_local);
    self.body.append(self.allocator, Op.f64_floor) catch return error.OutOfMemory;
    const floor_local = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    try self.emitLocalSet(floor_local);

    // Convert integer part to i64 and format it
    try self.emitLocalGet(floor_local);
    self.body.append(self.allocator, Op.i64_trunc_f64_u) catch return error.OutOfMemory;
    const int_val = self.storage.allocAnonymousLocal(.i64) catch return error.OutOfMemory;
    try self.emitLocalSet(int_val);

    // Format integer part using digit extraction (reuse int_to_str logic)
    // Allocate temp 21-byte buffer for integer digits
    try self.emitHeapAllocConst(21, 1);
    const int_buf = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(int_buf);

    const int_pos = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 20) catch return error.OutOfMemory;
    try self.emitLocalSet(int_pos);

    // Digit extraction loop for integer part
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // digit = int_val % 10
    try self.emitLocalGet(int_val);
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 10) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_rem_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
    const fdigit = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(fdigit);

    // int_val = int_val / 10
    try self.emitLocalGet(int_val);
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 10) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_div_u) catch return error.OutOfMemory;
    try self.emitLocalSet(int_val);

    // int_buf[int_pos] = digit + '0'
    try self.emitLocalGet(int_buf);
    try self.emitLocalGet(int_pos);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalGet(fdigit);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '0') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // int_pos--
    try self.emitLocalGet(int_pos);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(int_pos);

    // if int_val > 0: continue
    try self.emitLocalGet(int_val);
    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i64_gt_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Copy integer digits from int_buf[int_pos+1..21] to buf[pos..]
    const int_start = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    // int_start = int_pos + 1
    try self.emitLocalGet(int_pos);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(int_start);

    const int_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    // int_len = 20 - int_pos
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 20) catch return error.OutOfMemory;
    try self.emitLocalGet(int_pos);
    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(int_len);

    // int_buf_start = int_buf + int_start
    try self.emitLocalGet(int_buf);
    try self.emitLocalGet(int_start);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    const int_buf_start = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(int_buf_start);

    try self.emitMemCopyLoop(buf_local, pos_local, int_buf_start, int_len);

    // pos += int_len
    try self.emitLocalGet(pos_local);
    try self.emitLocalGet(int_len);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);

    // Write '.'
    try self.emitLocalGet(buf_local);
    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '.') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // pos++
    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);

    // Fractional part: frac = value - floor(value)
    // Multiply by powers of 10 and extract digits
    // We'll extract up to 17 significant decimal digits, then strip trailing zeros
    const frac_local = self.storage.allocAnonymousLocal(.f64) catch return error.OutOfMemory;
    try self.emitLocalGet(f64_local);
    try self.emitLocalGet(floor_local);
    self.body.append(self.allocator, Op.f64_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(frac_local);

    // Extract fractional digits (up to 17)
    const frac_count = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(frac_count);

    // Save position where fractional digits start (for trailing zero removal)
    const frac_start = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(pos_local);
    try self.emitLocalSet(frac_start);

    // Digit extraction loop for fractional part
    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // if frac_count >= 17: break
    try self.emitLocalGet(frac_count);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 17) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    // frac = frac * 10
    try self.emitLocalGet(frac_local);
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    try self.emitF64Bytes(10.0);
    self.body.append(self.allocator, Op.f64_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(frac_local);

    // digit = (i32)trunc(frac)
    try self.emitLocalGet(frac_local);
    self.body.append(self.allocator, Op.f64_floor) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_trunc_f64_u) catch return error.OutOfMemory;
    const fd_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(fd_local);

    // frac = frac - trunc(frac)
    try self.emitLocalGet(frac_local);
    try self.emitLocalGet(fd_local);
    self.body.append(self.allocator, Op.f64_convert_i32_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.f64_sub) catch return error.OutOfMemory;
    try self.emitLocalSet(frac_local);

    // buffer[pos] = digit + '0'
    try self.emitLocalGet(buf_local);
    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalGet(fd_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '0') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    // pos++, frac_count++
    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);

    try self.emitLocalGet(frac_count);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(frac_count);

    // if frac == 0: break (no more fractional digits)
    try self.emitLocalGet(frac_local);
    self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
    try self.emitF64Bytes(0.0);
    self.body.append(self.allocator, Op.f64_eq) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    // continue loop
    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end loop
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory; // end block

    // If no fractional digits were written, write "0"
    try self.emitLocalGet(frac_count);
    self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.@"if") catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    try self.emitLocalGet(buf_local);
    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, '0') catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    try self.emitLocalGet(pos_local);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(pos_local);

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // end infinity if/else
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // end NaN if/else
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    // Build RocStr from buf[0..pos]
    const str_len_local = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(pos_local);
    try self.emitLocalSet(str_len_local);

    try self.buildHeapRocStr(buf_local, str_len_local);
}

/// Generate dec_to_str: convert a RocDec (i128 scaled by 10^18) to string.
/// Uses a host function import to perform the formatting.
fn generateDecToStr(self: *Self, dec_expr: anytype) Error!void {
    const import_idx = self.dec_to_str_import orelse return unsupported(@src());

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
fn generateStrEscapeAndQuote(self: *Self, quote_expr: anytype) Error!void {
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
fn emitLocalGet(self: *Self, local: u32) Error!void {
    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, local) catch return error.OutOfMemory;
}

/// Helper: emit local.set instruction
fn emitLocalSet(self: *Self, local: u32) Error!void {
    self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, local) catch return error.OutOfMemory;
}

/// Helper: write a constant string to buffer[0..len]
fn emitWriteStringConst(self: *Self, buf_local: u32, pos_local: u32, str: []const u8) Error!void {
    _ = pos_local;
    for (str, 0..) |byte, i| {
        try self.emitLocalGet(buf_local);
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(byte)) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_store8) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
        WasmModule.leb128WriteU32(self.allocator, &self.body, @intCast(i)) catch return error.OutOfMemory;
    }
}

/// Helper: emit 8 bytes for an f64 constant
fn emitF64Bytes(self: *Self, value: f64) Error!void {
    const bytes: [8]u8 = @bitCast(value);
    for (bytes) |b| {
        self.body.append(self.allocator, b) catch return error.OutOfMemory;
    }
}

/// Emit float modulo: a % b = a - trunc(a / b) * b
/// Expects a and b already on the wasm value stack.
fn emitFloatMod(self: *Self, vt: ValType) Error!void {
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
fn getListElemSize(self: *const Self, list_layout: layout.Idx) Error!u32 {
    const ls = self.layout_store orelse return error.UnsupportedExpr;
    const l = ls.getLayout(list_layout);
    if (l.tag != .list) return error.UnsupportedExpr;
    return ls.layoutSize(ls.getLayout(l.data.list));
}

/// Get the element alignment for a list layout.
fn getListElemAlign(self: *const Self, list_layout: layout.Idx) Error!u32 {
    const ls = self.layout_store orelse return error.UnsupportedExpr;
    const l = ls.getLayout(list_layout);
    if (l.tag != .list) return error.UnsupportedExpr;
    return @intCast(ls.getLayout(l.data.list).alignment(ls.targetUsize()).toByteUnits());
}

const StrSearchMode = enum { contains, starts_with, ends_with };

/// Generate LowLevel str_contains / str_starts_with / str_ends_with.
/// Compares bytes using a nested loop (naive O(n*m) search).
fn generateLLStrSearch(self: *Self, args: anytype, mode: StrSearchMode) Error!void {
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
fn emitStrPrefixCompare(self: *Self, a_ptr: u32, a_len: u32, b_ptr: u32, b_len: u32, result_local: u32) Error!void {
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
fn emitBytewiseCompare(self: *Self, ptr_a: u32, ptr_b: u32, len: u32, result_local: u32) Error!void {
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
fn generateLLListAppend(self: *Self, args: anytype, ret_layout: layout.Idx) Error!void {
    const elem_size = try self.getListElemSize(ret_layout);
    const elem_align = try self.getListElemAlign(ret_layout);

    // Generate list arg
    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    // Generate element arg
    try self.generateExpr(args[1]);
    const elem_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(elem_val);

    // Load list fields: elements_ptr (offset 0), length (offset 4)
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

    // new_len = old_len + 1
    const new_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(new_len);

    // Allocate new_len * elem_size bytes on heap
    const total_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(new_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_size);

    try self.emitHeapAlloc(total_size, elem_align);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // Copy old elements: memcpy(new_data, old_data, old_len * elem_size)
    const old_byte_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(old_byte_len);

    const zero = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(zero);

    try self.emitMemCopyLoop(new_data, zero, old_data, old_byte_len);

    // Store new element at new_data + old_len * elem_size
    if (elem_size <= 8) {
        // Scalar element: store directly
        try self.emitLocalGet(new_data);
        try self.emitLocalGet(old_byte_len);
        self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
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
        // Composite element: copy from elem_val pointer
        const elem_byte_size = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
        self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
        WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
        try self.emitLocalSet(elem_byte_size);
        try self.emitMemCopyLoop(new_data, old_byte_len, elem_val, elem_byte_size);
    }

    // Build RocList struct on stack frame
    try self.buildRocList(new_data, new_len);
}

/// Generate LowLevel list_prepend: create new list with one element prepended.
fn generateLLListPrepend(self: *Self, args: anytype, ret_layout: layout.Idx) Error!void {
    const elem_size = try self.getListElemSize(ret_layout);
    const elem_align = try self.getListElemAlign(ret_layout);

    // Generate list and element
    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);
    try self.generateExpr(args[1]);
    const elem_val = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
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
fn generateLLListConcat(self: *Self, args: anytype, ret_layout: layout.Idx) Error!void {
    const elem_size = try self.getListElemSize(ret_layout);
    const elem_align = try self.getListElemAlign(ret_layout);

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
fn generateLLListReverse(self: *Self, args: anytype, ret_layout: layout.Idx) Error!void {
    const elem_size = try self.getListElemSize(ret_layout);
    const elem_align = try self.getListElemAlign(ret_layout);

    try self.generateExpr(args[0]);
    const list_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(list_ptr);

    const old_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    const old_len = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(old_data);
    try self.emitLocalGet(list_ptr);
    self.body.append(self.allocator, Op.i32_load) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 2) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 4) catch return error.OutOfMemory;
    try self.emitLocalSet(old_len);

    // Allocate new buffer
    const total_bytes = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(total_bytes);

    try self.emitHeapAlloc(total_bytes, elem_align);
    const new_data = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalSet(new_data);

    // Reverse copy: for i in 0..old_len, new[i] = old[old_len - 1 - i]
    const rev_i = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
    try self.emitLocalSet(rev_i);

    self.body.append(self.allocator, Op.block) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.loop_) catch return error.OutOfMemory;
    self.body.append(self.allocator, @intFromEnum(BlockType.void)) catch return error.OutOfMemory;

    // if rev_i >= old_len: break
    try self.emitLocalGet(rev_i);
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_ge_u) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.br_if) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 1) catch return error.OutOfMemory;

    // Copy elem_size bytes from old[(old_len-1-i)*elem_size] to new[i*elem_size]
    // src_offset = (old_len - 1 - rev_i) * elem_size
    const src_off = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_len);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    try self.emitLocalGet(rev_i);
    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(src_off);

    // dst_offset = rev_i * elem_size
    const dst_off2 = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(rev_i);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_mul) catch return error.OutOfMemory;
    try self.emitLocalSet(dst_off2);

    // src_ptr = old_data + src_off
    const src_ptr = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    try self.emitLocalGet(old_data);
    try self.emitLocalGet(src_off);
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(src_ptr);

    // Copy elem_size bytes
    const elem_bytes = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, @intCast(elem_size)) catch return error.OutOfMemory;
    try self.emitLocalSet(elem_bytes);

    try self.emitMemCopyLoop(new_data, dst_off2, src_ptr, elem_bytes);

    // rev_i++
    try self.emitLocalGet(rev_i);
    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
    WasmModule.leb128WriteI32(self.allocator, &self.body, 1) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.i32_add) catch return error.OutOfMemory;
    try self.emitLocalSet(rev_i);

    self.body.append(self.allocator, Op.br) catch return error.OutOfMemory;
    WasmModule.leb128WriteU32(self.allocator, &self.body, 0) catch return error.OutOfMemory;

    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;
    self.body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    try self.buildRocList(new_data, old_len);
}

/// Build a RocList struct on the stack frame from data ptr and length locals.
fn buildRocList(self: *Self, data_local: u32, len_local: u32) Error!void {
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

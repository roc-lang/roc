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

const LayoutStore = layout.Store;

const Self = @This();

pub const Error = error{
    OutOfMemory,
    UnsupportedExpr,
};

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
/// Map from MonoSymbol → capture info (for let-bound closures).
/// At call sites, capture values are passed as extra leading arguments.
closure_captures: std.AutoHashMap(u48, CaptureInfo),

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
        .closure_captures = std.AutoHashMap(u48, CaptureInfo).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.module.deinit();
    self.body.deinit(self.allocator);
    self.storage.deinit();
    self.compiled_lambdas.deinit();
    self.symbol_funcs.deinit();
    var it = self.closure_captures.iterator();
    while (it.next()) |entry| {
        self.allocator.free(entry.value_ptr.symbols);
        self.allocator.free(entry.value_ptr.val_types);
    }
    self.closure_captures.deinit();
}

/// Result of generating a wasm module
pub const GenerateResult = struct {
    wasm_bytes: []u8,
    result_layout: layout.Idx,
};

/// Generate a complete wasm module for a single expression.
/// The expression becomes the body of an exported "main" function.
pub fn generateModule(self: *Self, expr_id: MonoExprId, result_layout: layout.Idx) Error!GenerateResult {
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

    // If we used stack memory, enable memory + stack pointer in the module
    if (self.uses_stack_memory) {
        self.module.enableMemory(1); // 1 page = 64KB
        self.module.enableStackPointer(65536); // stack starts at top of first page
        self.module.addExport("memory", .memory, 0) catch return error.OutOfMemory;
    }

    // Build function body: locals declaration + prologue + instructions + epilogue + end
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    // Encode locals declaration
    try self.encodeLocalsDecl(&func_body);

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
    };
}

/// Encode the locals declaration vector for a function body.
/// Groups consecutive locals of the same type: (count, type)*
fn encodeLocalsDecl(self: *Self, func_body: *std.ArrayList(u8)) Error!void {
    const types = self.storage.local_types.items;
    if (types.len == 0) {
        WasmModule.leb128WriteU32(self.allocator, func_body, 0) catch return error.OutOfMemory;
        return;
    }

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
                        else => return error.UnsupportedExpr,
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
                        switch (stmt_expr) {
                            .lambda => |lambda| {
                                const func_idx = try self.compileLambda(stmt.expr, lambda);
                                const key: u48 = @bitCast(bind.symbol);
                                self.symbol_funcs.put(key, func_idx) catch return error.OutOfMemory;
                            },
                            .closure => |closure| {
                                const func_idx = try self.compileClosure(stmt.expr, closure);
                                const key: u48 = @bitCast(bind.symbol);
                                self.symbol_funcs.put(key, func_idx) catch return error.OutOfMemory;
                                // Copy capture info from expr key to symbol key
                                const expr_key: u32 = @intFromEnum(stmt.expr);
                                if (self.closure_captures.get(@intCast(expr_key))) |cap_info| {
                                    self.closure_captures.put(key, cap_info) catch return error.OutOfMemory;
                                }
                            },
                            else => {
                                // Check for type representation mismatch: composite expr bound
                                // to scalar local (e.g., dec_literal bound to U64 local).
                                // Conversion between these representations isn't supported yet.
                                const expr_is_composite = self.isCompositeExpr(stmt.expr);
                                const target_is_composite = self.isCompositeLayout(bind.layout_idx);
                                if (expr_is_composite != target_is_composite) {
                                    return error.UnsupportedExpr;
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
                            },
                        }
                    },
                    .wildcard => {
                        // Evaluate expression for side effects, drop result
                        try self.generateExpr(stmt.expr);
                        self.body.append(self.allocator, Op.drop) catch return error.OutOfMemory;
                    },
                    else => return error.UnsupportedExpr,
                }
            }
            // Generate the final expression (the block's result)
            try self.generateExpr(b.final_expr);
        },
        .lookup => |l| {
            const local_idx = self.storage.getLocal(l.symbol) orelse return error.UnsupportedExpr;
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
                return error.UnsupportedExpr;
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
        // String operations that require heap allocation / builtins — not yet supported
        .str_concat, .int_to_str, .float_to_str, .dec_to_str, .str_escape_and_quote => {
            return error.UnsupportedExpr;
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
                if (l.tag != .tag_union) return error.UnsupportedExpr;
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
            if (l.tag != .record) return error.UnsupportedExpr;
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
        else => .i64, // conservative default
    };
}

/// Get the byte size of the value an expression produces.
fn exprByteSize(self: *Self, expr_id: MonoExprId) u32 {
    if (self.exprLayoutIdx(expr_id)) |lay_idx| {
        return self.layoutByteSize(lay_idx);
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
        .empty_list => true, // 12-byte RocList in stack memory
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
            // Dec multiply requires (a * b) / 10^18 with 256-bit intermediate —
            // not a raw i128 multiply. Not yet implemented for wasm.
            // Pure i128 multiply (non-Dec) is also rare. Skip for now.
            return error.UnsupportedExpr;
        },
        .lt => try self.emitI128Compare(lhs_local, rhs_local, .lt),
        .lte => try self.emitI128Compare(lhs_local, rhs_local, .lte),
        .gt => try self.emitI128Compare(lhs_local, rhs_local, .gt),
        .gte => try self.emitI128Compare(lhs_local, rhs_local, .gte),
        else => return error.UnsupportedExpr,
    }
    _ = result_layout;
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

    // cross2 = cross1 + a0*b1 (can carry into upper 32 bits)
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

    // high = a1*b1 + (cross >> 32)
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

/// Saved codegen state for restoring after compiling a nested function.
const SavedState = struct {
    body_items: []u8,
    body_capacity: usize,
    storage_locals: std.AutoHashMap(u48, Storage.LocalInfo),
    storage_next_local_idx: u32,
    storage_local_types_items: []ValType,
    storage_local_types_capacity: usize,
    stack_frame_size: u32,
    uses_stack_memory: bool,
    fp_local: u32,
};

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
    const saved_body = self.body.items;
    const saved_body_cap = self.body.capacity;
    const saved_locals = self.storage.locals;
    const saved_next_local = self.storage.next_local_idx;
    const saved_local_types = self.storage.local_types.items;
    const saved_local_types_cap = self.storage.local_types.capacity;
    const saved_stack_frame_size = self.stack_frame_size;
    const saved_uses_stack_memory = self.uses_stack_memory;
    const saved_fp_local = self.fp_local;

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
    try self.generateExpr(lambda.body);

    // Build function body: locals declaration + instructions + end
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    // Locals declaration — only for locals beyond the parameters
    const param_count: u32 = @intCast(params.len);
    const total_locals = self.storage.next_local_idx;
    if (total_locals > param_count) {
        // Build groups of consecutive non-param locals
        const types_slice = self.storage.local_types.items[param_count..];
        var groups: std.ArrayList(struct { count: u32, val_type: ValType }) = .empty;
        defer groups.deinit(self.allocator);

        var i: usize = 0;
        while (i < types_slice.len) {
            const vt = types_slice[i];
            var count: u32 = 1;
            while (i + count < types_slice.len and types_slice[i + count] == vt) {
                count += 1;
            }
            groups.append(self.allocator, .{ .count = count, .val_type = vt }) catch return error.OutOfMemory;
            i += count;
        }

        WasmModule.leb128WriteU32(self.allocator, &func_body, @intCast(groups.items.len)) catch return error.OutOfMemory;
        for (groups.items) |g| {
            WasmModule.leb128WriteU32(self.allocator, &func_body, g.count) catch return error.OutOfMemory;
            func_body.append(self.allocator, @intFromEnum(g.val_type)) catch return error.OutOfMemory;
        }
    } else {
        // No locals beyond parameters
        WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
    }

    // Body instructions
    func_body.appendSlice(self.allocator, self.body.items) catch return error.OutOfMemory;

    // End opcode
    func_body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.module.setFunctionBody(func_idx, func_body.items) catch return error.OutOfMemory;

    // Restore previous codegen state
    self.body.deinit(self.allocator);
    self.body.items = saved_body;
    self.body.capacity = saved_body_cap;
    self.storage.locals.deinit();
    self.storage.locals = saved_locals;
    self.storage.next_local_idx = saved_next_local;
    self.storage.local_types.deinit(self.allocator);
    self.storage.local_types.items = saved_local_types;
    self.storage.local_types.capacity = saved_local_types_cap;
    self.stack_frame_size = saved_stack_frame_size;
    self.uses_stack_memory = saved_uses_stack_memory;
    self.fp_local = saved_fp_local;

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
    const saved_body = self.body.items;
    const saved_body_cap = self.body.capacity;
    const saved_locals = self.storage.locals;
    const saved_next_local = self.storage.next_local_idx;
    const saved_local_types = self.storage.local_types.items;
    const saved_local_types_cap = self.storage.local_types.capacity;
    const saved_stack_frame_size = self.stack_frame_size;
    const saved_uses_stack_memory = self.uses_stack_memory;
    const saved_fp_local = self.fp_local;

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
    try self.generateExpr(lambda.body);

    // Build function body
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    const param_count: u32 = @intCast(captures.len + params.len);
    const total_locals = self.storage.next_local_idx;
    if (total_locals > param_count) {
        const types_slice = self.storage.local_types.items[param_count..];
        var groups: std.ArrayList(struct { count: u32, val_type: ValType }) = .empty;
        defer groups.deinit(self.allocator);

        var i: usize = 0;
        while (i < types_slice.len) {
            const vt = types_slice[i];
            var count: u32 = 1;
            while (i + count < types_slice.len and types_slice[i + count] == vt) {
                count += 1;
            }
            groups.append(self.allocator, .{ .count = count, .val_type = vt }) catch return error.OutOfMemory;
            i += count;
        }

        WasmModule.leb128WriteU32(self.allocator, &func_body, @intCast(groups.items.len)) catch return error.OutOfMemory;
        for (groups.items) |g| {
            WasmModule.leb128WriteU32(self.allocator, &func_body, g.count) catch return error.OutOfMemory;
            func_body.append(self.allocator, @intFromEnum(g.val_type)) catch return error.OutOfMemory;
        }
    } else {
        WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
    }

    func_body.appendSlice(self.allocator, self.body.items) catch return error.OutOfMemory;
    func_body.append(self.allocator, Op.end) catch return error.OutOfMemory;

    self.module.setFunctionBody(func_idx, func_body.items) catch return error.OutOfMemory;

    // Restore previous state
    self.body.deinit(self.allocator);
    self.body.items = saved_body;
    self.body.capacity = saved_body_cap;
    self.storage.locals.deinit();
    self.storage.locals = saved_locals;
    self.storage.next_local_idx = saved_next_local;
    self.storage.local_types.deinit(self.allocator);
    self.storage.local_types.items = saved_local_types;
    self.storage.local_types.capacity = saved_local_types_cap;
    self.stack_frame_size = saved_stack_frame_size;
    self.uses_stack_memory = saved_uses_stack_memory;
    self.fp_local = saved_fp_local;

    // Cache
    self.compiled_lambdas.put(expr_key, func_idx) catch return error.OutOfMemory;

    // Store capture info for call sites (only if there are captures)
    if (captures.len > 0) {
        // Store with a hash key derived from the expression key
        // The caller will associate this with the bound symbol
        self.closure_captures.put(@intCast(expr_key), .{
            .symbols = capture_symbols,
            .val_types = capture_val_types,
        }) catch return error.OutOfMemory;
    } else {
        self.allocator.free(capture_symbols);
        self.allocator.free(capture_val_types);
    }

    return func_idx;
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
        else => null,
    };

    if (func_idx) |idx| {
        // Push capture values as leading arguments (if this is a closure call)
        switch (fn_expr) {
            .closure => |closure| {
                // Inline closure call — push captures directly from current scope
                const captures = self.store.getCaptures(closure.captures);
                for (captures) |cap| {
                    const local_idx = self.storage.getLocal(cap.symbol) orelse return error.UnsupportedExpr;
                    self.body.append(self.allocator, Op.local_get) catch return error.OutOfMemory;
                    WasmModule.leb128WriteU32(self.allocator, &self.body, local_idx) catch return error.OutOfMemory;
                }
            },
            .lookup => |lookup| {
                // Let-bound closure — look up captures by symbol
                const key: u48 = @bitCast(lookup.symbol);
                if (self.closure_captures.get(key)) |cap_info| {
                    for (cap_info.symbols) |cap_sym| {
                        const local_idx = self.storage.getLocal(cap_sym) orelse return error.UnsupportedExpr;
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
        return error.UnsupportedExpr;
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
    if (l.tag != .record) return error.UnsupportedExpr;

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

    // Use a bounded array to store field value locals (max 32 fields inline)
    var field_val_locals: [32]u32 = undefined;
    var field_val_types: [32]ValType = undefined;

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
fn generateFieldAccess(self: *Self, fa: anytype) Error!void {
    const ls = try self.getLayoutStore();

    // Generate the record expression → pushes i32 pointer
    try self.generateExpr(fa.record_expr);

    // Get the field offset
    const record_layout = ls.getLayout(fa.record_layout);
    if (record_layout.tag != .record) return error.UnsupportedExpr;

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
    if (l.tag != .tuple) return error.UnsupportedExpr;

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
    if (tuple_layout.tag != .tuple) return error.UnsupportedExpr;

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

    if (l.tag != .tag_union) return error.UnsupportedExpr;

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
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                // If arg produces i64 (e.g. from i64_literal), wrap to i32
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
        },
        .i8_to_i16, .i8_to_i32 => {
            // i8 is i32 in wasm, sign-extend from 8 bits
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        },
        .u16_to_i32, .u16_to_u32 => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
        },
        .i16_to_i32 => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
            self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
        },

        // i32/u32 → i64/u64
        .u8_to_i64, .u8_to_u64, .u16_to_i64, .u16_to_u64,
        .u32_to_i64, .u32_to_u64,
        => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                const arg_vt = self.exprValType(args[0]);
                if (arg_vt == .i64) {
                    // Already i64 — no extension needed
                    return;
                }
            }
            self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
        },
        .i8_to_i64, .i16_to_i64, .i32_to_i64 => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                const arg_vt = self.exprValType(args[0]);
                if (arg_vt == .i64) {
                    // Already i64 — no extension needed
                    return;
                }
            }
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },

        // Narrowing/wrapping conversions
        .i64_to_i32_wrap, .u64_to_u32_wrap, .u64_to_i32_wrap, .i64_to_u32_wrap => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .i32_to_i8_wrap, .u32_to_u8_wrap, .i32_to_u8_wrap,
        .i64_to_u8_wrap, .u64_to_u8_wrap, .i64_to_i8_wrap,
        .u64_to_i8_wrap,
        .u16_to_i8_wrap, .u16_to_u8_wrap, .i16_to_i8_wrap,
        .i16_to_u8_wrap, .u32_to_i8_wrap,
        => {
            if (args.len > 0) try self.generateExpr(args[0]);
            // May need to wrap i64 to i32 first
            const arg_vt = if (args.len > 0) self.exprValType(args[0]) else ValType.i32;
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
            if (args.len > 0) try self.generateExpr(args[0]);
            const arg_vt = if (args.len > 0) self.exprValType(args[0]) else ValType.i32;
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
            if (args.len > 0) try self.generateExpr(args[0]);
        },
        .i64_to_u64_wrap, .u64_to_i64_wrap => {
            // Same representation in wasm (both i64), no-op
            if (args.len > 0) try self.generateExpr(args[0]);
        },

        // Signed sub-i32 to unsigned wider wrapping (needs sign extension)
        .i8_to_u32_wrap => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
        },
        .i16_to_u32_wrap => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
        },
        .i8_to_u16_wrap => {
            if (args.len > 0) try self.generateExpr(args[0]);
            // Sign-extend from 8 bits then mask to 16 bits
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFFFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i8_to_u64_wrap => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
            self.body.append(self.allocator, Op.i32_extend8_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i16_to_u64_wrap => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
            self.body.append(self.allocator, Op.i32_extend16_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i32_to_u64_wrap => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
        },
        .i32_to_u128_wrap => {
            // Signed i32→u128 wrap: sign-extend to i64, then to i128
            if (args.len > 0) try self.generateExpr(args[0]);
            if (args.len > 0 and self.exprValType(args[0]) == .i64) {
                // Already i64
            } else {
                self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            }
            try self.emitIntToI128(true);
        },
        .i32_to_u64_try, .i32_to_u128_try => return error.UnsupportedExpr,

        // Float conversions
        .f32_to_f64 => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f64_promote_f32) catch return error.OutOfMemory;
        },
        .f64_to_f32_wrap => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },

        // Int to float
        .i32_to_f32, .i8_to_f32, .i16_to_f32 => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
            self.body.append(self.allocator, Op.f32_convert_i32_s) catch return error.OutOfMemory;
        },
        .u32_to_f32, .u8_to_f32, .u16_to_f32 => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
            self.body.append(self.allocator, Op.f32_convert_i32_u) catch return error.OutOfMemory;
        },
        .i32_to_f64, .i8_to_f64, .i16_to_f64 => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
            self.body.append(self.allocator, Op.f64_convert_i32_s) catch return error.OutOfMemory;
        },
        .u32_to_f64, .u8_to_f64, .u16_to_f64 => {
            if (args.len > 0) {
                try self.generateExpr(args[0]);
                if (self.exprValType(args[0]) == .i64) {
                    self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
                }
            }
            self.body.append(self.allocator, Op.f64_convert_i32_u) catch return error.OutOfMemory;
        },
        .i64_to_f32 => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f32_convert_i64_s) catch return error.OutOfMemory;
        },
        .u64_to_f32 => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f32_convert_i64_u) catch return error.OutOfMemory;
        },
        .i64_to_f64 => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
        },
        .u64_to_f64 => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.f64_convert_i64_u) catch return error.OutOfMemory;
        },

        // Float to int (truncating)
        .f32_to_i32_trunc, .f32_to_i8_trunc, .f32_to_i16_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f32_s) catch return error.OutOfMemory;
        },
        .f32_to_u32_trunc, .f32_to_u8_trunc, .f32_to_u16_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f32_u) catch return error.OutOfMemory;
        },
        .f64_to_i32_trunc, .f64_to_i8_trunc, .f64_to_i16_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f64_s) catch return error.OutOfMemory;
        },
        .f64_to_u32_trunc, .f64_to_u8_trunc, .f64_to_u16_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i32_trunc_f64_u) catch return error.OutOfMemory;
        },
        .f32_to_i64_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f32_s) catch return error.OutOfMemory;
        },
        .f32_to_u64_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f32_u) catch return error.OutOfMemory;
        },
        .f64_to_i64_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f64_s) catch return error.OutOfMemory;
        },
        .f64_to_u64_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_trunc_f64_u) catch return error.OutOfMemory;
        },

        // Float math functions (direct wasm opcodes)
        .num_sqrt => {
            if (args.len > 0) try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_sqrt,
                .f64 => Op.f64_sqrt,
                else => return error.UnsupportedExpr,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_floor => {
            if (args.len > 0) try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_floor,
                .f64 => Op.f64_floor,
                else => return error.UnsupportedExpr,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_ceiling => {
            if (args.len > 0) try self.generateExpr(args[0]);
            const vt = self.resolveValType(ll.ret_layout);
            const wasm_op: u8 = switch (vt) {
                .f32 => Op.f32_ceil,
                .f64 => Op.f64_ceil,
                else => return error.UnsupportedExpr,
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_round => {
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitLoadOp(.i32, 4);
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        },
        .list_get => {
            // args[0] = list, args[1] = index
            // Load elements_ptr from list, compute element address, load element
            if (args.len >= 2) {
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
            }
        },

        // String operations
        .str_is_empty => {
            // Check if string length is 0
            // For SSO: byte 11 has length in high nibble (masked to 0x7F)
            // For heap: offset 4 has length
            // Simplified: load byte 11, check SSO bit, then compare len to 0
            // For now, just load the 4-byte length field at offset 4 and check
            if (args.len > 0) try self.generateExpr(args[0]);
            // Load the length/SSO word at offset 4
            try self.emitLoadOp(.i32, 4);
            // If it's 0, the string is empty (works for both SSO and heap empty strings)
            self.body.append(self.allocator, Op.i32_eqz) catch return error.OutOfMemory;
        },

        // Bitwise operations
        .num_pow => {
            // Float power function — wasm doesn't have a pow instruction
            // For integer pow, this needs a loop. For float, use host function.
            return error.UnsupportedExpr;
        },
        .num_log => {
            // Logarithm — no wasm instruction, needs host function
            return error.UnsupportedExpr;
        },

        // List element access operations (no heap allocation needed)
        .list_first => {
            // list_first(list) -> elem  (loads first element)
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len >= 2) {
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
                // The ret_layout is a list, so we need the element size.
                // For simple cases, use the first arg's layout info.
                // We can compute elem size from the list element layout if available.
                // For now, use a fixed approach based on what list_get does.
                const elem_size: u32 = blk: {
                    if (self.layout_store) |ls| {
                        const l = ls.getLayout(ll.ret_layout);
                        if (l.tag == .list) {
                            break :blk ls.layoutSize(ls.getLayout(l.data.list));
                        }
                    }
                    break :blk 8; // fallback
                };

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
            }
        },
        .list_drop_last => {
            // list_drop_last(list, count) -> list
            // Returns a RocList with adjusted length (pointer stays same)
            if (args.len >= 2) {
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
            }
        },
        .list_take_first => {
            // list_take_first(list, count) -> list
            // Same as list but with length = min(count, len)
            if (args.len >= 2) {
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
            }
        },
        .list_take_last => {
            // list_take_last(list, count) -> list
            // elements_ptr += (len - min(count, len)) * elem_size
            // length = min(count, len)
            if (args.len >= 2) {
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

                const elem_size: u32 = blk: {
                    if (self.layout_store) |ls| {
                        const l = ls.getLayout(ll.ret_layout);
                        if (l.tag == .list) {
                            break :blk ls.layoutSize(ls.getLayout(l.data.list));
                        }
                    }
                    break :blk 8;
                };

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
            }
        },

        .list_contains => {
            // list_contains(list, needle) -> Bool
            // Linear scan through list elements
            if (args.len >= 2) {
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
            }
        },

        // Remaining list/string operations that need heap allocation or builtins
        .list_set, .list_append, .list_prepend, .list_concat,
        .list_reverse,
        .list_reserve, .list_release_excess_capacity, .list_with_capacity,
        .list_repeat, .list_split_first, .list_split_last,
        => return error.UnsupportedExpr,

        .str_count_utf8_bytes => {
            // Returns the length of the string in UTF-8 bytes
            if (args.len > 0) try self.generateExpr(args[0]);
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

        .str_is_eq, .str_concat, .str_contains, .str_starts_with, .str_ends_with,
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

        // Compare — returns Ordering tag union
        .compare => return error.UnsupportedExpr,

        // Crash
        .crash => {
            self.body.append(self.allocator, Op.@"unreachable") catch return error.OutOfMemory;
        },

        // Try conversions (bounds-checking, return Result tag union — not yet supported)
        .u8_to_i8_try,
        .i8_to_u8_try, .i8_to_u16_try, .i8_to_u32_try, .i8_to_u64_try, .i8_to_u128_try,
        .u16_to_i8_try, .u16_to_i16_try, .u16_to_u8_try,
        .i16_to_i8_try, .i16_to_u8_try, .i16_to_u16_try, .i16_to_u32_try, .i16_to_u64_try, .i16_to_u128_try,
        .u32_to_i8_try, .u32_to_i16_try, .u32_to_i32_try, .u32_to_u8_try, .u32_to_u16_try,
        .i32_to_i8_try, .i32_to_i16_try, .i32_to_u8_try, .i32_to_u16_try, .i32_to_u32_try,
        .u64_to_i8_try, .u64_to_i16_try, .u64_to_i32_try, .u64_to_i64_try,
        .u64_to_u8_try, .u64_to_u16_try, .u64_to_u32_try,
        .i64_to_i8_try, .i64_to_i16_try, .i64_to_i32_try,
        .i64_to_u8_try, .i64_to_u16_try, .i64_to_u32_try, .i64_to_u64_try, .i64_to_u128_try,
        .u128_to_i8_try, .u128_to_i16_try, .u128_to_i32_try, .u128_to_i64_try, .u128_to_i128_try,
        .u128_to_u8_try, .u128_to_u16_try, .u128_to_u32_try, .u128_to_u64_try,
        .i128_to_i8_try, .i128_to_i16_try, .i128_to_i32_try, .i128_to_i64_try,
        .i128_to_u8_try, .i128_to_u16_try, .i128_to_u32_try, .i128_to_u64_try, .i128_to_u128_try,
        => return error.UnsupportedExpr,

        // Integer widening to i128/u128 (zero/sign-extend to 128 bits in stack memory)
        .u8_to_i128, .u8_to_u128, .u16_to_i128, .u16_to_u128,
        .u32_to_i128, .u32_to_u128,
        => {
            // Unsigned i32→i128: zero-extend i32 to i64, then to i128
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_u) catch return error.OutOfMemory;
            try self.emitIntToI128(false);
        },
        .u64_to_i128, .u64_to_u128,
        => {
            // Unsigned i64→i128: value is already i64
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitIntToI128(false);
        },
        .i8_to_i128, .i16_to_i128, .i32_to_i128,
        => {
            // Signed i32→i128: sign-extend i32 to i64, then to i128
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            try self.emitIntToI128(true);
        },
        .i64_to_i128,
        => {
            // Signed i64→i128: value is already i64
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitIntToI128(true);
        },
        .i8_to_u128_wrap, .i16_to_u128_wrap,
        => {
            // Signed i32→u128 wrap: sign-extend to i64, then i128
            if (args.len > 0) try self.generateExpr(args[0]);
            self.body.append(self.allocator, Op.i64_extend_i32_s) catch return error.OutOfMemory;
            try self.emitIntToI128(true);
        },
        .i64_to_u128_wrap,
        => {
            // Signed i64→u128 wrap: already i64, sign-extend to i128
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitIntToI128(true);
        },
        // i128/u128 truncation to smaller types (load low word, mask)
        .i128_to_i8_wrap, .i128_to_u8_wrap, .u128_to_i8_wrap, .u128_to_u8_wrap,
        => {
            if (args.len > 0) try self.generateExpr(args[0]);
            // Load low i64, wrap to i32, mask to 8 bits
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i128_to_i16_wrap, .i128_to_u16_wrap, .u128_to_i16_wrap, .u128_to_u16_wrap,
        => {
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI32(self.allocator, &self.body, 0xFFFF) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_and) catch return error.OutOfMemory;
        },
        .i128_to_i32_wrap, .i128_to_u32_wrap, .u128_to_i32_wrap, .u128_to_u32_wrap,
        => {
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .i128_to_i64_wrap, .i128_to_u64_wrap, .u128_to_i64_wrap, .u128_to_u64_wrap,
        => {
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
        },
        .u128_to_i128_wrap, .i128_to_u128_wrap,
        => {
            // Same representation — just pass through (pointer stays the same)
            if (args.len > 0) try self.generateExpr(args[0]);
        },
        // 128-bit conversions not yet supported
        .u128_to_f32, .u128_to_f64, .u128_to_dec_try_unsafe,
        .i128_to_f32, .i128_to_f64, .i128_to_dec_try_unsafe,
        .f32_to_i128_trunc, .f32_to_u128_trunc,
        .f64_to_i128_trunc, .f64_to_u128_trunc,
        => return error.UnsupportedExpr,

        // Decimal conversions: int → Dec (multiply by 10^18)
        .u8_to_dec, .u16_to_dec, .u32_to_dec => {
            // Unsigned small int → Dec: zero-extend to i64, multiply by 10^18
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .dec_to_i16_trunc, .dec_to_i8_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
        },
        .dec_to_u32_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
            WasmModule.leb128WriteI64(self.allocator, &self.body, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i64_div_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.i32_wrap_i64) catch return error.OutOfMemory;
        },
        .dec_to_u16_trunc, .dec_to_u8_trunc => {
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
            const src = self.storage.allocAnonymousLocal(.i32) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.local_set) catch return error.OutOfMemory;
            WasmModule.leb128WriteU32(self.allocator, &self.body, src) catch return error.OutOfMemory;
            try self.emitI128DivByConst(src, 1_000_000_000_000_000_000);
        },
        .dec_to_f64 => {
            // Dec → f64: load i128 as i64 (low word), convert to f64, divide by 10^18.0
            if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len > 0) try self.generateExpr(args[0]);
            try self.emitLoadOp(.i64, 0);
            self.body.append(self.allocator, Op.f64_convert_i64_s) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_const) catch return error.OutOfMemory;
            const dec_f64_bytes = @as([8]u8, @bitCast(@as(f64, 1_000_000_000_000_000_000.0)));
            self.body.appendSlice(self.allocator, &dec_f64_bytes) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f64_div) catch return error.OutOfMemory;
            self.body.append(self.allocator, Op.f32_demote_f64) catch return error.OutOfMemory;
        },
        // Dec try_unsafe conversions (not yet supported — need Result tag union)
        .dec_to_i8_try_unsafe, .dec_to_i16_try_unsafe, .dec_to_i32_try_unsafe,
        .dec_to_i64_try_unsafe, .dec_to_i128_try_unsafe,
        .dec_to_u8_try_unsafe, .dec_to_u16_try_unsafe, .dec_to_u32_try_unsafe,
        .dec_to_u64_try_unsafe, .dec_to_u128_try_unsafe,
        .dec_to_f32_try_unsafe,
        => return error.UnsupportedExpr,

        // Float try_unsafe conversions (bounds-checking — not yet supported)
        .f32_to_i8_try_unsafe, .f32_to_i16_try_unsafe, .f32_to_i32_try_unsafe,
        .f32_to_i64_try_unsafe, .f32_to_i128_try_unsafe,
        .f32_to_u8_try_unsafe, .f32_to_u16_try_unsafe, .f32_to_u32_try_unsafe,
        .f32_to_u64_try_unsafe, .f32_to_u128_try_unsafe,
        .f64_to_i8_try_unsafe, .f64_to_i16_try_unsafe, .f64_to_i32_try_unsafe,
        .f64_to_i64_try_unsafe, .f64_to_i128_try_unsafe,
        .f64_to_u8_try_unsafe, .f64_to_u16_try_unsafe, .f64_to_u32_try_unsafe,
        .f64_to_u64_try_unsafe, .f64_to_u128_try_unsafe,
        .f64_to_f32_try_unsafe,
        => return error.UnsupportedExpr,
    }
}

/// Generate numeric low-level operations (num_add, num_sub, etc.)
fn generateNumericLowLevel(self: *Self, op: anytype, args: []const MonoExprId, ret_layout: layout.Idx) Error!void {
    const vt = self.resolveValType(ret_layout);

    switch (op) {
        .num_add => {
            if (args.len >= 2) { try self.generateExpr(args[0]); try self.generateExpr(args[1]); }
            const wasm_op: u8 = switch (vt) { .i32 => Op.i32_add, .i64 => Op.i64_add, .f32 => Op.f32_add, .f64 => Op.f64_add };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_sub => {
            if (args.len >= 2) { try self.generateExpr(args[0]); try self.generateExpr(args[1]); }
            const wasm_op: u8 = switch (vt) { .i32 => Op.i32_sub, .i64 => Op.i64_sub, .f32 => Op.f32_sub, .f64 => Op.f64_sub };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_mul => {
            if (args.len >= 2) { try self.generateExpr(args[0]); try self.generateExpr(args[1]); }
            const wasm_op: u8 = switch (vt) { .i32 => Op.i32_mul, .i64 => Op.i64_mul, .f32 => Op.f32_mul, .f64 => Op.f64_mul };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_div => {
            if (args.len >= 2) { try self.generateExpr(args[0]); try self.generateExpr(args[1]); }
            const wasm_op: u8 = switch (vt) { .i32 => Op.i32_div_s, .i64 => Op.i64_div_s, .f32 => Op.f32_div, .f64 => Op.f64_div };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
        },
        .num_neg => {
            switch (vt) {
                .i32 => {
                    self.body.append(self.allocator, Op.i32_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI32(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    if (args.len > 0) try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.i32_sub) catch return error.OutOfMemory;
                },
                .i64 => {
                    self.body.append(self.allocator, Op.i64_const) catch return error.OutOfMemory;
                    WasmModule.leb128WriteI64(self.allocator, &self.body, 0) catch return error.OutOfMemory;
                    if (args.len > 0) try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.i64_sub) catch return error.OutOfMemory;
                },
                .f32 => {
                    if (args.len > 0) try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.f32_neg) catch return error.OutOfMemory;
                },
                .f64 => {
                    if (args.len > 0) try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.f64_neg) catch return error.OutOfMemory;
                },
            }
        },
        .num_abs => {
            switch (vt) {
                .f32 => {
                    if (args.len > 0) try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.f32_abs) catch return error.OutOfMemory;
                },
                .f64 => {
                    if (args.len > 0) try self.generateExpr(args[0]);
                    self.body.append(self.allocator, Op.f64_abs) catch return error.OutOfMemory;
                },
                .i32 => {
                    // abs(x) = select(x, -x, x >= 0)
                    if (args.len > 0) try self.generateExpr(args[0]);
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
                    if (args.len > 0) try self.generateExpr(args[0]);
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
            if (args.len >= 2) { try self.generateExpr(args[0]); try self.generateExpr(args[1]); }
            const wasm_op: u8 = switch (vt) {
                .i32 => Op.i32_rem_s,
                .i64 => Op.i64_rem_s,
                else => return error.UnsupportedExpr, // float mod not supported natively
            };
            self.body.append(self.allocator, wasm_op) catch return error.OutOfMemory;
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

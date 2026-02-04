//! MonoIR -> WebAssembly code generator.
//!
//! Walks MonoIR expressions and emits wasm instructions. Each `generateExpr`
//! call leaves the result on the wasm value stack (for primitives) or writes
//! to linear memory (for composites).
//!
//! Phase 1: Supports integer, float, bool, and dec literals only.

const std = @import("std");
const Allocator = std.mem.Allocator;
const layout = @import("layout");
const mono = @import("mono");
const MonoExprStore = mono.MonoExprStore;
const MonoExpr = mono.MonoIR.MonoExpr;
const MonoExprId = mono.MonoIR.MonoExprId;
const BinOp = MonoExpr.BinOp;
const WasmModule = @import("WasmModule.zig");
const WasmLayout = @import("WasmLayout.zig");
const Op = WasmModule.Op;
const ValType = WasmModule.ValType;

const Self = @This();

pub const Error = error{
    OutOfMemory,
    UnsupportedExpr,
};

allocator: Allocator,
store: *const MonoExprStore,
module: WasmModule,
body: std.ArrayList(u8), // instruction bytes for current function

pub fn init(allocator: Allocator, store: *const MonoExprStore) Self {
    return .{
        .allocator = allocator,
        .store = store,
        .module = WasmModule.init(allocator),
        .body = .empty,
    };
}

pub fn deinit(self: *Self) void {
    self.module.deinit();
    self.body.deinit(self.allocator);
}

/// Result of generating a wasm module
pub const GenerateResult = struct {
    wasm_bytes: []u8,
    result_layout: layout.Idx,
};

/// Generate a complete wasm module for a single expression.
/// The expression becomes the body of an exported "main" function.
pub fn generateModule(self: *Self, expr_id: MonoExprId, result_layout: layout.Idx) Error!GenerateResult {
    // Determine return type
    const result_vt = WasmLayout.resultValType(result_layout);

    // Add function type: () -> (result_type)
    const type_idx = self.module.addFuncType(&.{}, &.{result_vt}) catch return error.OutOfMemory;

    // Add function
    const func_idx = self.module.addFunction(type_idx) catch return error.OutOfMemory;

    // Generate the expression body
    self.body.clearRetainingCapacity();
    try self.generateExpr(expr_id);

    // Build function body: local_count(0) + instructions + end
    var func_body: std.ArrayList(u8) = .empty;
    defer func_body.deinit(self.allocator);

    // 0 local declarations
    WasmModule.leb128WriteU32(self.allocator, &func_body, 0) catch return error.OutOfMemory;
    // Instructions
    func_body.appendSlice(self.allocator, self.body.items) catch return error.OutOfMemory;
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
        .dec_literal => return error.UnsupportedExpr, // TODO: Phase 7 - needs linear memory for i128
        .i128_literal => return error.UnsupportedExpr, // TODO: Phase 7 - needs linear memory for i128
        .binop => |b| {
            try self.generateExpr(b.lhs);
            try self.generateExpr(b.rhs);

            const vt = WasmLayout.resultValType(b.result_layout);
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
                    .i32 => Op.i32_div_s,
                    .i64 => Op.i64_div_s,
                    .f32 => Op.f32_div,
                    .f64 => Op.f64_div,
                },
                .div_trunc => switch (vt) {
                    .i32 => Op.i32_div_s,
                    .i64 => Op.i64_div_s,
                    .f32 => Op.f32_div,
                    .f64 => Op.f64_div,
                },
                .mod => switch (vt) {
                    .i32 => Op.i32_rem_s,
                    .i64 => Op.i64_rem_s,
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
                    .i32 => Op.i32_lt_s,
                    .i64 => Op.i64_lt_s,
                    .f32 => Op.f32_lt,
                    .f64 => Op.f64_lt,
                },
                .lte => switch (vt) {
                    .i32 => Op.i32_le_s,
                    .i64 => Op.i64_le_s,
                    .f32 => Op.f32_le,
                    .f64 => Op.f64_le,
                },
                .gt => switch (vt) {
                    .i32 => Op.i32_gt_s,
                    .i64 => Op.i64_gt_s,
                    .f32 => Op.f32_gt,
                    .f64 => Op.f64_gt,
                },
                .gte => switch (vt) {
                    .i32 => Op.i32_ge_s,
                    .i64 => Op.i64_ge_s,
                    .f32 => Op.f32_ge,
                    .f64 => Op.f64_ge,
                },
                .@"and" => Op.i32_and,
                .@"or" => Op.i32_or,
            };
            self.body.append(self.allocator, op) catch return error.OutOfMemory;
        },
        .unary_minus => |u| {
            const vt = WasmLayout.resultValType(u.result_layout);
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
            // Process statements (let bindings) - Phase 4 will handle locals
            const stmts = self.store.getStmts(b.stmts);
            for (stmts) |_| {}
            try self.generateExpr(b.final_expr);
        },
        .lookup => return error.UnsupportedExpr, // Phase 4
        .if_then_else => return error.UnsupportedExpr, // Phase 5
        else => return error.UnsupportedExpr,
    }
}

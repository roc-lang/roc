//! CIR to Mono IR Lowering Pass
//!
//! This module lowers the Canonical Intermediate Representation (CIR) to Mono IR.
//! The key transformation is converting module-local indices to globally unique symbols.
//!
//! Entry points:
//! - `lowerExpression`: Lower a single expression (for REPL/constant folding)
//! - `lowerFromEntryPoints`: Lower all reachable expressions from entry points
//!
//! The lowering process:
//! 1. Walks CIR expressions recursively
//! 2. Converts CIR.Pattern.Idx → MonoSymbol (using pattern's module + ident)
//! 3. Resolves type variables → layout.Idx via the layout store
//! 4. Handles cross-module references via Import resolution
//! 5. Produces a flat MonoExprStore consumable by any backend

const std = @import("std");
const base = @import("base");
const can = @import("can");
const layout_mod = @import("layout");
const types = @import("types");

const ir = @import("MonoIR.zig");
const store_mod = @import("MonoExprStore.zig");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const NodeStore = CIR.NodeStore;
const LambdaSetInference = can.LambdaSetInference;
const Region = base.Region;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Allocator = std.mem.Allocator;

const MonoExpr = ir.MonoExpr;
const MonoPattern = ir.MonoPattern;
const MonoExprId = ir.MonoExprId;
const MonoPatternId = ir.MonoPatternId;
const MonoExprSpan = ir.MonoExprSpan;
const MonoPatternSpan = ir.MonoPatternSpan;
const MonoSymbol = ir.MonoSymbol;
const MonoCapture = ir.MonoCapture;
const MonoCaptureSpan = ir.MonoCaptureSpan;
const MonoWhenBranch = ir.MonoWhenBranch;
const MonoIfBranch = ir.MonoIfBranch;
const MonoStmt = ir.MonoStmt;

const MonoExprStore = store_mod;
const LayoutIdx = layout_mod.Idx;
const LayoutStore = layout_mod.Store;

const Self = @This();

/// Lowering context
allocator: Allocator,

/// The target store we're lowering into
store: *MonoExprStore,

/// All module environments (for resolving cross-module references)
all_module_envs: []const *ModuleEnv,

/// Lambda set inference results (for closure dispatch)
lambda_inference: ?*LambdaSetInference,

/// Layout store (for computing layouts from types)
layout_store: ?*LayoutStore,

/// Track which (module_idx, pattern_idx) pairs have been lowered to avoid duplicates
/// Maps to the MonoSymbol that was created
lowered_patterns: std.AutoHashMap(u64, MonoSymbol),

/// Track which top-level symbols have been lowered
lowered_symbols: std.AutoHashMap(u48, MonoExprId),

/// Current module index during lowering
current_module_idx: u16 = 0,

/// Errors that can occur during lowering
pub const Error = error{
    OutOfMemory,
    InvalidExpression,
    ModuleNotFound,
    LayoutError,
};

/// Initialize a new Lowerer
pub fn init(
    allocator: Allocator,
    store: *MonoExprStore,
    all_module_envs: []const *ModuleEnv,
    lambda_inference: ?*LambdaSetInference,
    layout_store: ?*LayoutStore,
) Self {
    return .{
        .allocator = allocator,
        .store = store,
        .all_module_envs = all_module_envs,
        .lambda_inference = lambda_inference,
        .layout_store = layout_store,
        .lowered_patterns = std.AutoHashMap(u64, MonoSymbol).init(allocator),
        .lowered_symbols = std.AutoHashMap(u48, MonoExprId).init(allocator),
    };
}

/// Cleanup
pub fn deinit(self: *Self) void {
    self.lowered_patterns.deinit();
    self.lowered_symbols.deinit();
}

/// Get the module environment at the given index
fn getModuleEnv(self: *const Self, module_idx: u16) ?*ModuleEnv {
    if (module_idx >= self.all_module_envs.len) return null;
    return self.all_module_envs[module_idx];
}

/// Create a MonoSymbol from a pattern in the current module
fn patternToSymbol(self: *Self, pattern_idx: CIR.Pattern.Idx) MonoSymbol {
    const key = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(pattern_idx);

    if (self.lowered_patterns.get(key)) |existing| {
        return existing;
    }

    // Get the pattern's identifier from the current module
    const module_env = self.all_module_envs[self.current_module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);

    const ident_idx: Ident.Idx = switch (pattern) {
        .assign => |a| a.ident,
        .as => |as| as.ident,
        else => Ident.Idx.NONE,
    };

    const symbol = MonoSymbol{
        .module_idx = self.current_module_idx,
        .ident_idx = ident_idx,
    };

    self.lowered_patterns.put(key, symbol) catch {};
    return symbol;
}

/// Create a MonoSymbol from an external reference
fn externalToSymbol(self: *Self, import_idx: CIR.Import.Idx, ident_idx: Ident.Idx) MonoSymbol {
    // Resolve the import to a module index
    const module_env = self.all_module_envs[self.current_module_idx];
    const resolved_module = module_env.imports.getResolvedModule(import_idx);

    if (resolved_module) |mod_idx| {
        return MonoSymbol{
            .module_idx = @intCast(mod_idx),
            .ident_idx = ident_idx,
        };
    }

    // Unresolved import - use sentinel
    return MonoSymbol.none;
}

/// Get layout for an expression (placeholder - needs type info)
fn getExprLayout(self: *Self, module_env: *ModuleEnv, expr: CIR.Expr) LayoutIdx {
    _ = module_env;
    // For now, use a default layout. In full implementation, this would:
    // 1. Get the type variable from the expression
    // 2. Resolve it via the layout store
    if (self.layout_store) |_| {
        // TODO: Proper layout resolution from type variable
    }

    // Default fallback based on expression type
    return switch (expr) {
        .e_num => |num| switch (num.kind) {
            .u8 => .u8,
            .i8 => .i8,
            .u16 => .u16,
            .i16 => .i16,
            .u32 => .u32,
            .i32 => .i32,
            .u64 => .u64,
            .i64, .int_unbound, .num_unbound => .i64,
            .u128 => .u128,
            .i128 => .i128,
            .f32 => .f32,
            .f64 => .f64,
            .dec => .dec,
        },
        .e_frac_f32 => .f32,
        .e_frac_f64 => .f64,
        .e_dec, .e_dec_small => .dec,
        .e_str, .e_str_segment => .str,
        .e_zero_argument_tag => .bool, // Common case for Bool tags
        else => .i64, // Default
    };
}

/// Lower a single expression
pub fn lowerExpr(self: *Self, module_idx: u16, expr_idx: CIR.Expr.Idx) Error!MonoExprId {
    const old_module = self.current_module_idx;
    self.current_module_idx = module_idx;
    defer self.current_module_idx = old_module;

    const module_env = self.getModuleEnv(module_idx) orelse return error.ModuleNotFound;
    const expr = module_env.store.getExpr(expr_idx);
    const region = module_env.store.getExprRegion(expr_idx);

    return self.lowerExprInner(module_env, expr, region);
}

/// Lower an expression with its CIR representation
fn lowerExprInner(self: *Self, module_env: *ModuleEnv, expr: CIR.Expr, region: Region) Error!MonoExprId {
    const mono_expr: MonoExpr = switch (expr) {
        // ============ Literals ============
        .e_num => |num| blk: {
            const val = num.value.toI128();
            if (val >= std.math.minInt(i64) and val <= std.math.maxInt(i64)) {
                break :blk .{ .i64_literal = @intCast(val) };
            }
            break :blk .{ .i128_literal = val };
        },

        .e_frac_f32 => |f| .{ .f32_literal = f.value },
        .e_frac_f64 => |f| .{ .f64_literal = f.value },
        .e_dec => |d| .{ .dec_literal = d.value.num },
        .e_dec_small => |d| .{ .dec_literal = d.value.toRocDec().num },

        .e_str_segment => |seg| .{ .str_literal = seg.literal },
        .e_str => |str| blk: {
            // For now, handle simple strings - interpolation needs more work
            const segments = module_env.store.sliceExpr(str.span);
            if (segments.len == 1) {
                const seg_expr = module_env.store.getExpr(segments[0]);
                if (seg_expr == .e_str_segment) {
                    break :blk .{ .str_literal = seg_expr.e_str_segment.literal };
                }
            }
            // Complex string - lower all segments
            const lowered = try self.lowerExprSpan(module_env, str.span);
            break :blk .{ .list = .{ .elem_layout = .str, .elems = lowered } };
        },

        .e_empty_record => .{ .empty_record = {} },
        .e_empty_list => .{ .empty_list = .{ .elem_layout = .i64 } }, // TODO: get actual elem layout

        // ============ Lookups ============
        .e_lookup_local => |lookup| blk: {
            const symbol = self.patternToSymbol(lookup.pattern_idx);
            break :blk .{ .lookup = .{
                .symbol = symbol,
                .layout_idx = self.getExprLayout(module_env, expr),
            } };
        },

        .e_lookup_external => |lookup| blk: {
            const symbol = self.externalToSymbol(lookup.module_idx, lookup.ident_idx);
            break :blk .{ .lookup = .{
                .symbol = symbol,
                .layout_idx = self.getExprLayout(module_env, expr),
            } };
        },

        // ============ Function calls ============
        .e_call => |call| blk: {
            const fn_id = try self.lowerExprFromIdx(module_env, call.func);
            const args = try self.lowerExprSpan(module_env, call.args);
            break :blk .{ .call = .{
                .fn_expr = fn_id,
                .fn_layout = .i64, // TODO: proper function layout
                .args = args,
                .ret_layout = self.getExprLayout(module_env, expr),
                .called_via = call.called_via,
            } };
        },

        .e_lambda => |lambda| blk: {
            const params = try self.lowerPatternSpan(module_env, lambda.args);
            const body = try self.lowerExprFromIdx(module_env, lambda.body);
            break :blk .{ .lambda = .{
                .fn_layout = .i64, // TODO
                .params = params,
                .body = body,
                .ret_layout = .i64, // TODO
            } };
        },

        .e_closure => |closure| blk: {
            const lambda_id = try self.lowerExprFromIdx(module_env, closure.lambda_idx);
            const captures = try self.lowerCaptures(module_env, closure.captures);
            break :blk .{ .closure = .{
                .closure_layout = .i64, // TODO
                .lambda = lambda_id,
                .captures = captures,
            } };
        },

        // ============ Data structures ============
        .e_list => |list| blk: {
            const elems = try self.lowerExprSpan(module_env, list.elems);
            break :blk .{ .list = .{
                .elem_layout = .i64, // TODO: get from type
                .elems = elems,
            } };
        },

        .e_tuple => |tuple| blk: {
            const elems = try self.lowerExprSpan(module_env, tuple.elems);
            break :blk .{ .tuple = .{
                .tuple_layout = .i64, // TODO
                .elems = elems,
            } };
        },

        .e_record => |rec| blk: {
            const fields = try self.lowerRecordFields(module_env, rec.fields);
            break :blk .{ .record = .{
                .record_layout = .i64, // TODO
                .fields = fields,
            } };
        },

        // ============ Field access ============
        .e_dot_access => |dot| blk: {
            const receiver = try self.lowerExprFromIdx(module_env, dot.receiver);
            break :blk .{ .field_access = .{
                .record_expr = receiver,
                .record_layout = .i64, // TODO
                .field_layout = .i64, // TODO
                .field_idx = 0, // TODO: resolve field index from layout
            } };
        },

        // ============ Tags ============
        .e_zero_argument_tag => |tag| blk: {
            // Get discriminant from tag name
            // For Bool, True = 1, False = 0
            const discriminant: u16 = if (tag.name == module_env.idents.true_tag)
                1
            else if (tag.name == module_env.idents.false_tag)
                0
            else
                0; // TODO: proper discriminant lookup

            break :blk .{ .zero_arg_tag = .{
                .discriminant = discriminant,
                .union_layout = .bool,
            } };
        },

        .e_tag => |tag| blk: {
            const args = try self.lowerExprSpan(module_env, tag.args);
            break :blk .{ .tag = .{
                .discriminant = 0, // TODO
                .union_layout = .i64, // TODO
                .args = args,
            } };
        },

        // ============ Control flow ============
        .e_if => |if_expr| blk: {
            const branches = try self.lowerIfBranches(module_env, if_expr.branches);
            const final_else = try self.lowerExprFromIdx(module_env, if_expr.final_else);
            break :blk .{ .if_then_else = .{
                .branches = branches,
                .final_else = final_else,
                .result_layout = self.getExprLayout(module_env, expr),
            } };
        },

        .e_match => |match_expr| blk: {
            const cond = try self.lowerExprFromIdx(module_env, match_expr.cond);
            const branches = try self.lowerWhenBranches(module_env, match_expr.branches);
            break :blk .{ .when = .{
                .value = cond,
                .value_layout = .i64, // TODO
                .branches = branches,
                .result_layout = self.getExprLayout(module_env, expr),
            } };
        },

        // ============ Blocks ============
        .e_block => |block| blk: {
            const stmts = try self.lowerStmts(module_env, block.stmts);
            const final_expr = try self.lowerExprFromIdx(module_env, block.final_expr);
            break :blk .{ .block = .{
                .stmts = stmts,
                .final_expr = final_expr,
                .result_layout = self.getExprLayout(module_env, expr),
            } };
        },

        .e_return => |ret| blk: {
            const inner = try self.lowerExprFromIdx(module_env, ret.expr);
            break :blk .{ .early_return = .{
                .expr = inner,
                .ret_layout = self.getExprLayout(module_env, expr),
            } };
        },

        // ============ Operations ============
        .e_binop => |binop| blk: {
            const lhs = try self.lowerExprFromIdx(module_env, binop.lhs);
            const rhs = try self.lowerExprFromIdx(module_env, binop.rhs);
            const op = cirBinopToMonoBinop(binop.op);
            break :blk .{ .binop = .{
                .op = op,
                .lhs = lhs,
                .rhs = rhs,
                .result_layout = self.getExprLayout(module_env, expr),
            } };
        },

        .e_unary_minus => |unary| blk: {
            const inner = try self.lowerExprFromIdx(module_env, unary.expr);
            break :blk .{ .unary_minus = .{
                .expr = inner,
                .result_layout = self.getExprLayout(module_env, expr),
            } };
        },

        .e_unary_not => |unary| blk: {
            const inner = try self.lowerExprFromIdx(module_env, unary.expr);
            break :blk .{ .unary_not = .{ .expr = inner } };
        },

        // ============ Debugging/errors ============
        .e_dbg => |dbg| blk: {
            const inner = try self.lowerExprFromIdx(module_env, dbg.expr);
            break :blk .{ .dbg = .{
                .msg = @enumFromInt(std.math.maxInt(u32)), // dbg doesn't have a msg in CIR
                .expr = inner,
                .result_layout = self.getExprLayout(module_env, expr),
            } };
        },

        .e_expect => |expect| blk: {
            // e_expect only has a body (the condition expression)
            const body = try self.lowerExprFromIdx(module_env, expect.body);
            break :blk .{ .expect = .{
                .cond = body, // The body is the condition
                .body = body, // Same expression for both
                .result_layout = self.getExprLayout(module_env, expr),
            } };
        },

        .e_crash => |crash| .{ .crash = .{ .msg = crash.msg } },
        .e_runtime_error => .{ .runtime_error = {} },

        // ============ Nominal ============
        .e_nominal => |nom| blk: {
            const backing = try self.lowerExprFromIdx(module_env, nom.backing_expr);
            break :blk .{ .nominal = .{
                .backing_expr = backing,
                .nominal_layout = .i64, // TODO
            } };
        },

        .e_nominal_external => |nom| blk: {
            const backing = try self.lowerExprFromIdx(module_env, nom.backing_expr);
            break :blk .{ .nominal = .{
                .backing_expr = backing,
                .nominal_layout = .i64, // TODO
            } };
        },

        // Expressions that need more context or aren't supported yet
        .e_typed_int => |ti| blk: {
            const val = ti.value.toI128();
            if (val >= std.math.minInt(i64) and val <= std.math.maxInt(i64)) {
                break :blk .{ .i64_literal = @intCast(val) };
            }
            break :blk .{ .i128_literal = val };
        },

        .e_typed_frac => |tf| .{ .dec_literal = tf.value.toI128() },

        else => .{ .runtime_error = {} }, // Placeholder for unsupported expressions
    };

    return self.store.addExpr(mono_expr, region);
}

/// Lower an expression by its index in the current module
fn lowerExprFromIdx(self: *Self, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) Error!MonoExprId {
    const expr = module_env.store.getExpr(expr_idx);
    const region = module_env.store.getExprRegion(expr_idx);
    return self.lowerExprInner(module_env, expr, region);
}

/// Lower a span of expressions
fn lowerExprSpan(self: *Self, module_env: *ModuleEnv, span: CIR.Expr.Span) Error!MonoExprSpan {
    const expr_indices = module_env.store.sliceExpr(span);
    if (expr_indices.len == 0) return MonoExprSpan.empty();

    var lowered = std.ArrayList(MonoExprId).empty;
    defer lowered.deinit(self.allocator);

    for (expr_indices) |idx| {
        const id = try self.lowerExprFromIdx(module_env, idx);
        try lowered.append(self.allocator, id);
    }

    return self.store.addExprSpan(lowered.items);
}

/// Lower record fields
fn lowerRecordFields(self: *Self, module_env: *ModuleEnv, fields: CIR.RecordField.Span) Error!MonoExprSpan {
    const field_indices = module_env.store.sliceRecordFields(fields);
    if (field_indices.len == 0) return MonoExprSpan.empty();

    var lowered = std.ArrayList(MonoExprId).empty;
    defer lowered.deinit(self.allocator);

    for (field_indices) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        const id = try self.lowerExprFromIdx(module_env, field.value);
        try lowered.append(self.allocator, id);
    }

    return self.store.addExprSpan(lowered.items);
}

/// Lower a pattern
fn lowerPattern(self: *Self, module_env: *ModuleEnv, pattern_idx: CIR.Pattern.Idx) Error!MonoPatternId {
    const pattern = module_env.store.getPattern(pattern_idx);
    const region = module_env.store.getPatternRegion(pattern_idx);

    const mono_pattern: MonoPattern = switch (pattern) {
        .assign => |_| .{ .bind = .{
            .symbol = self.patternToSymbol(pattern_idx),
            .layout_idx = .i64, // TODO: get from type
        } },

        .underscore => .{ .wildcard = {} },

        .num_literal => |n| .{ .int_literal = .{
            .value = n.value.toI128(),
            .layout_idx = .i64,
        } },

        .str_literal => |s| .{ .str_literal = s.literal },

        .applied_tag => |t| blk: {
            const args = try self.lowerPatternSpan(module_env, t.args);
            break :blk .{ .tag = .{
                .discriminant = 0, // TODO
                .union_layout = .i64, // TODO
                .args = args,
            } };
        },

        .record_destructure => |r| blk: {
            // Record destructures contain RecordDestruct entries, not direct patterns
            // For now, we create binding patterns for each destruct entry
            const destruct_indices = module_env.store.sliceRecordDestructs(r.destructs);
            var field_patterns = std.ArrayList(MonoPatternId).empty;
            defer field_patterns.deinit(self.allocator);

            for (destruct_indices) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                // Create a binding pattern for this destruct
                // Both Required and SubPattern contain a pattern index
                const sub_pattern_idx = destruct.kind.toPatternIdx();
                const lowered_id = try self.lowerPattern(module_env, sub_pattern_idx);
                try field_patterns.append(self.allocator, lowered_id);
            }

            const fields = try self.store.addPatternSpan(field_patterns.items);
            break :blk .{ .record = .{
                .record_layout = .i64, // TODO
                .fields = fields,
            } };
        },

        .tuple => |t| blk: {
            const elems = try self.lowerPatternSpan(module_env, t.patterns);
            break :blk .{ .tuple = .{
                .tuple_layout = .i64, // TODO
                .elems = elems,
            } };
        },

        .as => |a| blk: {
            const inner = try self.lowerPattern(module_env, a.pattern);
            break :blk .{ .as_pattern = .{
                .symbol = self.patternToSymbol(pattern_idx),
                .layout_idx = .i64, // TODO
                .inner = inner,
            } };
        },

        else => .{ .wildcard = {} }, // Fallback for unsupported patterns
    };

    return self.store.addPattern(mono_pattern, region);
}

/// Lower a span of patterns
fn lowerPatternSpan(self: *Self, module_env: *ModuleEnv, span: CIR.Pattern.Span) Error!MonoPatternSpan {
    const indices = module_env.store.slicePatterns(span);
    if (indices.len == 0) return MonoPatternSpan.empty();

    var lowered = std.ArrayList(MonoPatternId).empty;
    defer lowered.deinit(self.allocator);

    for (indices) |idx| {
        const id = try self.lowerPattern(module_env, idx);
        try lowered.append(self.allocator, id);
    }

    return self.store.addPatternSpan(lowered.items);
}

/// Lower closure captures
fn lowerCaptures(self: *Self, module_env: *ModuleEnv, captures: CIR.Expr.Capture.Span) Error!MonoCaptureSpan {
    const capture_indices = module_env.store.sliceCaptures(captures);
    if (capture_indices.len == 0) return MonoCaptureSpan.empty();

    var lowered = std.ArrayList(MonoCapture).empty;
    defer lowered.deinit(self.allocator);

    for (capture_indices) |capture_idx| {
        const cap = module_env.store.getCapture(capture_idx);
        const symbol = self.patternToSymbol(cap.pattern_idx);
        try lowered.append(self.allocator, .{
            .symbol = symbol,
            .layout_idx = .i64, // TODO: get from type
        });
    }

    return self.store.addCaptures(lowered.items);
}

/// Lower if branches
fn lowerIfBranches(self: *Self, module_env: *ModuleEnv, branches: CIR.Expr.IfBranch.Span) Error!ir.MonoIfBranchSpan {
    const branch_indices = module_env.store.sliceIfBranches(branches);

    var lowered = std.ArrayList(MonoIfBranch).empty;
    defer lowered.deinit(self.allocator);

    for (branch_indices) |branch_idx| {
        const branch = module_env.store.getIfBranch(branch_idx);
        const cond = try self.lowerExprFromIdx(module_env, branch.cond);
        const body = try self.lowerExprFromIdx(module_env, branch.body);
        try lowered.append(self.allocator, .{
            .cond = cond,
            .body = body,
        });
    }

    return self.store.addIfBranches(lowered.items);
}

/// Lower when/match branches
fn lowerWhenBranches(self: *Self, module_env: *ModuleEnv, branches: CIR.Expr.Match.Branch.Span) Error!ir.MonoWhenBranchSpan {
    const branch_indices = module_env.store.sliceMatchBranches(branches);

    var lowered = std.ArrayList(MonoWhenBranch).empty;
    defer lowered.deinit(self.allocator);

    for (branch_indices) |branch_idx| {
        const branch = module_env.store.getMatchBranch(branch_idx);

        // Match branches can have multiple patterns (for or-patterns like `A | B => ...`)
        // For now, we lower just the first pattern
        const pattern_indices = module_env.store.sliceMatchBranchPatterns(branch.patterns);
        if (pattern_indices.len == 0) continue;

        const first_bp = module_env.store.getMatchBranchPattern(pattern_indices[0]);
        const pattern = try self.lowerPattern(module_env, first_bp.pattern);

        const guard = if (branch.guard) |guard_idx|
            try self.lowerExprFromIdx(module_env, guard_idx)
        else
            MonoExprId.none;
        const body = try self.lowerExprFromIdx(module_env, branch.value);

        try lowered.append(self.allocator, .{
            .pattern = pattern,
            .guard = guard,
            .body = body,
        });
    }

    return self.store.addWhenBranches(lowered.items);
}

/// Lower statements in a block
fn lowerStmts(self: *Self, module_env: *ModuleEnv, stmts: CIR.Statement.Span) Error!ir.MonoStmtSpan {
    const stmt_slice = module_env.store.sliceStatements(stmts);
    if (stmt_slice.len == 0) return ir.MonoStmtSpan.empty();

    var lowered = std.ArrayList(MonoStmt).empty;
    defer lowered.deinit(self.allocator);

    for (stmt_slice) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |decl| {
                const pattern = try self.lowerPattern(module_env, decl.pattern);
                const value = try self.lowerExprFromIdx(module_env, decl.expr);
                try lowered.append(self.allocator, .{
                    .pattern = pattern,
                    .expr = value,
                });
            },
            .s_decl_gen => |decl| {
                const pattern = try self.lowerPattern(module_env, decl.pattern);
                const value = try self.lowerExprFromIdx(module_env, decl.expr);
                try lowered.append(self.allocator, .{
                    .pattern = pattern,
                    .expr = value,
                });
            },
            else => {}, // Skip non-decl statements for now
        }
    }

    return self.store.addStmts(lowered.items);
}

/// Convert CIR binary operator to Mono IR binary operator
fn cirBinopToMonoBinop(op: CIR.Expr.Binop.Op) MonoExpr.BinOp {
    return switch (op) {
        .add => .add,
        .sub => .sub,
        .mul => .mul,
        .div => .div,
        .rem => .mod,
        .eq => .eq,
        .ne => .neq,
        .lt => .lt,
        .le => .lte,
        .gt => .gt,
        .ge => .gte,
        .@"and" => .@"and",
        .@"or" => .@"or",
        .div_trunc => .div, // Truncating division maps to div for now
    };
}

// ============ Public API ============

/// Lower a single expression (for REPL/constant folding)
/// Returns the MonoExprId of the lowered expression
pub fn lowerExpression(
    allocator: Allocator,
    store: *MonoExprStore,
    all_module_envs: []const *ModuleEnv,
    module_idx: u16,
    expr_idx: CIR.Expr.Idx,
) Error!MonoExprId {
    var lowerer = init(allocator, store, all_module_envs, null, null);
    defer lowerer.deinit();
    return lowerer.lowerExpr(module_idx, expr_idx);
}

/// Entry point specification
pub const EntryPoint = struct {
    module_idx: u16,
    expr_idx: CIR.Expr.Idx,
};

/// Lower all reachable expressions from entry points (for roc build)
pub fn lowerFromEntryPoints(
    allocator: Allocator,
    store: *MonoExprStore,
    all_module_envs: []const *ModuleEnv,
    lambda_inference: ?*LambdaSetInference,
    layout_store: ?*LayoutStore,
    entry_points: []const EntryPoint,
) Error!void {
    var lowerer = init(allocator, store, all_module_envs, lambda_inference, layout_store);
    defer lowerer.deinit();

    for (entry_points) |entry| {
        _ = try lowerer.lowerExpr(entry.module_idx, entry.expr_idx);
    }
}

// ============ Tests ============

test "basic lowering" {
    // This is a smoke test - proper testing requires a full ModuleEnv
    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Test passes if no crash
    try std.testing.expect(store.exprCount() == 0);
}

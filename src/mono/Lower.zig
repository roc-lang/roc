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
const MonoFieldNameSpan = ir.MonoFieldNameSpan;
const MonoSymbol = ir.MonoSymbol;
const MonoCapture = ir.MonoCapture;
const MonoCaptureSpan = ir.MonoCaptureSpan;
const MonoWhenBranch = ir.MonoWhenBranch;
const ClosureRepresentation = ir.ClosureRepresentation;
const Recursive = ir.Recursive;
const SelfRecursive = ir.SelfRecursive;
const JoinPointId = ir.JoinPointId;
const MonoIfBranch = ir.MonoIfBranch;
const MonoStmt = ir.MonoStmt;

const ClosureTransformer = can.ClosureTransformer;

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

/// Current binding pattern (for detecting recursive closures)
/// When lowering a statement like `f = |x| ...`, this holds the pattern for `f`
/// so we can detect if the closure body references itself.
current_binding_pattern: ?CIR.Pattern.Idx = null,

/// Counter for generating unique join point IDs
next_join_point_id: u32 = 0,

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

/// Get layout for an expression from its index
fn getExprLayoutFromIdx(self: *Self, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) LayoutIdx {
    const expr = module_env.store.getExpr(expr_idx);
    return self.getExprLayout(module_env, expr);
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

        .e_str_segment => |seg| blk: {
            // Copy the string from the module to the mono store
            const str_text = module_env.common.getString(seg.literal);
            const mono_idx = self.store.insertString(str_text) catch return error.OutOfMemory;
            break :blk .{ .str_literal = mono_idx };
        },
        .e_str => |str| blk: {
            // For now, handle simple strings - interpolation needs more work
            const segments = module_env.store.sliceExpr(str.span);
            if (segments.len == 1) {
                const seg_expr = module_env.store.getExpr(segments[0]);
                if (seg_expr == .e_str_segment) {
                    // Copy the string from the module to the mono store
                    const str_text = module_env.common.getString(seg_expr.e_str_segment.literal);
                    const mono_idx = self.store.insertString(str_text) catch return error.OutOfMemory;
                    break :blk .{ .str_literal = mono_idx };
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
            // Check if this is a call to a low-level lambda (like str_inspekt)
            const fn_expr = module_env.store.getExpr(call.func);
            if (fn_expr == .e_low_level_lambda) {
                const ll = fn_expr.e_low_level_lambda;
                if (ll.op == .str_inspekt) {
                    // Expand str_inspekt at lowering time
                    const arg_indices = module_env.store.sliceExpr(call.args);
                    if (arg_indices.len == 1) {
                        const arg_idx = arg_indices[0];
                        const arg_id = try self.lowerExprFromIdx(module_env, arg_idx);
                        // Get the type variable for the argument
                        // In the Zig implementation, expr indices ARE type variables
                        const arg_type_var = ModuleEnv.varFrom(arg_idx);
                        const arg_layout = self.getExprLayoutFromIdx(module_env, arg_idx);
                        return self.lowerStrInspekt(arg_id, arg_type_var, arg_layout, module_env, region);
                    }
                }
                // For other low-level ops, we would need to convert the op enum
                // For now, return a placeholder - full low-level op support would
                // require converting between CIR.Expr.LowLevel and MonoExpr.LowLevel
                break :blk .{ .runtime_error = {} };
            }

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

            // Select the closure representation based on captures
            const representation = self.selectClosureRepresentation(captures);

            // Detect if this closure is recursive (references itself)
            const recursion_info = self.detectClosureRecursion(module_env, closure.lambda_idx);

            break :blk .{ .closure = .{
                .closure_layout = .i64, // TODO: compute from representation
                .lambda = lambda_id,
                .captures = captures,
                .representation = representation,
                .recursion = recursion_info.recursion,
                .self_recursive = recursion_info.self_recursive,
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
            const result = try self.lowerRecordFields(module_env, rec.fields);
            break :blk .{ .record = .{
                .record_layout = .i64, // TODO
                .fields = result.fields,
                .field_names = result.field_names,
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
                .field_name = dot.field_name,
            } };
        },

        // ============ Tags ============
        .e_zero_argument_tag => |tag| blk: {
            // Get discriminant from tag name by comparing text
            // For Bool, True = 1, False = 0
            // We compare by text rather than ident index to handle cross-module cases
            const tag_name_text = module_env.getIdent(tag.name);
            // Handle both "True"/"False" (canonical form) and potential variations
            const discriminant: u16 = if (std.mem.eql(u8, tag_name_text, "True") or std.mem.eql(u8, tag_name_text, "true"))
                1
            else if (std.mem.eql(u8, tag_name_text, "False") or std.mem.eql(u8, tag_name_text, "false"))
                0
            else if (std.ascii.eqlIgnoreCase(tag_name_text, "true"))
                1
            else if (std.ascii.eqlIgnoreCase(tag_name_text, "false"))
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
            const args_slice = module_env.store.sliceExpr(tag.args);

            // Get discriminant from tag name - handle True/False specially
            const tag_name_text = module_env.getIdent(tag.name);
            const discriminant: u16 = if (args_slice.len == 0) disc_blk: {
                // Zero-argument tag - check for True/False
                if (std.mem.eql(u8, tag_name_text, "True") or std.ascii.eqlIgnoreCase(tag_name_text, "true")) {
                    break :disc_blk 1;
                } else if (std.mem.eql(u8, tag_name_text, "False") or std.ascii.eqlIgnoreCase(tag_name_text, "false")) {
                    break :disc_blk 0;
                } else {
                    break :disc_blk 0; // TODO: proper discriminant lookup
                }
            } else 0; // TODO: proper discriminant lookup for tags with args

            // For zero-argument tags like True/False, use zero_arg_tag
            if (args_slice.len == 0 and (std.mem.eql(u8, tag_name_text, "True") or
                std.mem.eql(u8, tag_name_text, "False") or
                std.ascii.eqlIgnoreCase(tag_name_text, "true") or
                std.ascii.eqlIgnoreCase(tag_name_text, "false")))
            {
                break :blk .{ .zero_arg_tag = .{
                    .discriminant = discriminant,
                    .union_layout = .bool,
                } };
            }

            break :blk .{ .tag = .{
                .discriminant = discriminant,
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

        // Low-level lambda - these are compiler-generated intrinsics
        .e_low_level_lambda => |ll| blk: {
            // Low-level lambdas are typically called, not evaluated directly
            // When called, we intercept them in e_call handling
            // If we encounter one directly, it's like a closure reference
            _ = ll;
            break :blk .{ .runtime_error = {} };
        },

        // ============ Reference counting operations ============
        // These are inserted by the RC insertion pass after canonicalization

        .e_incref => |rc_op| blk: {
            // Convert pattern reference to a lookup expression
            const symbol = self.patternToSymbol(rc_op.pattern_idx);
            const lookup_id = try self.store.addExpr(.{ .lookup = .{
                .symbol = symbol,
                .layout_idx = .i64, // TODO: get actual layout from pattern
            } }, region);
            break :blk .{ .incref = .{
                .value = lookup_id,
                .layout_idx = .i64, // TODO: get actual layout
                .count = rc_op.count,
            } };
        },

        .e_decref => |rc_op| blk: {
            // Convert pattern reference to a lookup expression
            const symbol = self.patternToSymbol(rc_op.pattern_idx);
            const lookup_id = try self.store.addExpr(.{ .lookup = .{
                .symbol = symbol,
                .layout_idx = .i64, // TODO: get actual layout from pattern
            } }, region);
            break :blk .{ .decref = .{
                .value = lookup_id,
                .layout_idx = .i64, // TODO: get actual layout
            } };
        },

        .e_free => |rc_op| blk: {
            // Convert pattern reference to a lookup expression
            const symbol = self.patternToSymbol(rc_op.pattern_idx);
            const lookup_id = try self.store.addExpr(.{ .lookup = .{
                .symbol = symbol,
                .layout_idx = .i64, // TODO: get actual layout from pattern
            } }, region);
            break :blk .{ .free = .{
                .value = lookup_id,
                .layout_idx = .i64, // TODO: get actual layout
            } };
        },

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

/// Result of lowering record fields
const LowerRecordFieldsResult = struct {
    fields: MonoExprSpan,
    field_names: MonoFieldNameSpan,
};

/// Lower record fields - sorted alphabetically by field name to match layout order
fn lowerRecordFields(self: *Self, module_env: *ModuleEnv, fields: CIR.RecordField.Span) Error!LowerRecordFieldsResult {
    const field_indices = module_env.store.sliceRecordFields(fields);
    if (field_indices.len == 0) return .{
        .fields = MonoExprSpan.empty(),
        .field_names = MonoFieldNameSpan.empty(),
    };

    // Collect (name, value) pairs for sorting
    const FieldPair = struct {
        name: base.Ident.Idx,
        value: CIR.Expr.Idx,
    };

    var pairs = std.ArrayList(FieldPair).empty;
    defer pairs.deinit(self.allocator);

    for (field_indices) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        try pairs.append(self.allocator, .{ .name = field.name, .value = field.value });
    }

    // Sort fields alphabetically by name (matches layout order for same-alignment fields)
    const SortContext = struct {
        module_env: *ModuleEnv,
        pub fn lessThan(ctx: @This(), a: FieldPair, b: FieldPair) bool {
            const a_str = ctx.module_env.getIdent(a.name);
            const b_str = ctx.module_env.getIdent(b.name);
            return std.mem.order(u8, a_str, b_str) == .lt;
        }
    };
    std.mem.sort(FieldPair, pairs.items, SortContext{ .module_env = module_env }, SortContext.lessThan);

    // Lower fields and collect names in sorted order
    var lowered = std.ArrayList(MonoExprId).empty;
    defer lowered.deinit(self.allocator);

    var names = std.ArrayList(base.Ident.Idx).empty;
    defer names.deinit(self.allocator);

    for (pairs.items) |pair| {
        const id = try self.lowerExprFromIdx(module_env, pair.value);
        try lowered.append(self.allocator, id);
        try names.append(self.allocator, pair.name);
    }

    return .{
        .fields = self.store.addExprSpan(lowered.items) catch return error.OutOfMemory,
        .field_names = self.store.addFieldNameSpan(names.items) catch return error.OutOfMemory,
    };
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

/// Select the optimal closure representation based on captures.
/// This implements Roc-style representation selection:
/// - 0 captures: direct_call (no runtime representation needed)
/// - 1 capture: unwrapped_capture (zero overhead)
/// - N captures: struct_captures (sorted by alignment)
///
/// For lambda sets with multiple functions, we'd use enum_dispatch or union_repr,
/// but that requires lambda set inference results which we'll add later.
fn selectClosureRepresentation(self: *Self, captures: MonoCaptureSpan) ClosureRepresentation {
    const capture_list = self.store.getCaptures(captures);

    if (capture_list.len == 0) {
        // No captures - function can be called directly
        return .{ .direct_call = {} };
    } else if (capture_list.len == 1) {
        // Single capture - unwrapped, zero overhead
        return .{ .unwrapped_capture = .{
            .capture_layout = capture_list[0].layout_idx,
        } };
    } else {
        // Multiple captures - store in a struct
        // TODO: Sort captures by alignment (largest first) for memory efficiency
        // For now, just use the captures as-is
        return .{ .struct_captures = .{
            .captures = captures,
            .struct_layout = .i64, // TODO: compute actual struct layout
        } };
    }
}

/// Result of recursion detection for a closure
const RecursionInfo = struct {
    recursion: Recursive,
    self_recursive: SelfRecursive,
};

/// Detect if a closure is recursive (references itself).
///
/// Uses the ClosureTransformer's detectRecursion functionality to check
/// if the closure body contains a reference to the binding pattern.
/// If recursive, generates a join point ID for the recursive entry.
fn detectClosureRecursion(self: *Self, module_env: *ModuleEnv, lambda_idx: CIR.Expr.Idx) RecursionInfo {
    // If we have a current binding pattern, check if the closure body references it
    if (self.current_binding_pattern) |binding_pattern| {
        // Get the lambda body to check for self-references
        const lambda_expr = module_env.store.getExpr(lambda_idx);
        if (lambda_expr == .e_lambda) {
            const body_expr = lambda_expr.e_lambda.body;

            // Check if the body contains a reference to the binding pattern
            if (self.exprContainsPatternRef(module_env, body_expr, binding_pattern)) {
                // Generate a unique join point ID for this recursive closure
                const join_point_id: JoinPointId = @enumFromInt(self.next_join_point_id);
                self.next_join_point_id += 1;

                // Check if this is tail-recursive
                // TODO: Proper tail-recursion detection (check if all recursive calls are in tail position)
                // For now, just mark as recursive
                return .{
                    .recursion = .recursive,
                    .self_recursive = .{ .self_recursive = join_point_id },
                };
            }
        }
    }

    // Not recursive
    return .{
        .recursion = .not_recursive,
        .self_recursive = .not_self_recursive,
    };
}

/// Check if an expression contains a reference to the given pattern.
/// Used for recursive closure detection.
fn exprContainsPatternRef(
    self: *Self,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    target_pattern: CIR.Pattern.Idx,
) bool {
    const expr = module_env.store.getExpr(expr_idx);

    switch (expr) {
        .e_lookup_local => |lookup| {
            return lookup.pattern_idx == target_pattern;
        },
        .e_call => |call| {
            // Check function expression
            if (self.exprContainsPatternRef(module_env, call.func, target_pattern)) {
                return true;
            }
            // Check arguments
            const args = module_env.store.sliceExpr(call.args);
            for (args) |arg_idx| {
                if (self.exprContainsPatternRef(module_env, arg_idx, target_pattern)) {
                    return true;
                }
            }
            return false;
        },
        .e_lambda => |lambda| {
            return self.exprContainsPatternRef(module_env, lambda.body, target_pattern);
        },
        .e_closure => |closure| {
            const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
            if (lambda_expr == .e_lambda) {
                return self.exprContainsPatternRef(module_env, lambda_expr.e_lambda.body, target_pattern);
            }
            return false;
        },
        .e_block => |block| {
            // Check statements
            const stmts = module_env.store.sliceStatements(block.stmts);
            for (stmts) |stmt_idx| {
                const stmt = module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        if (self.exprContainsPatternRef(module_env, decl.expr, target_pattern)) {
                            return true;
                        }
                    },
                    .s_decl_gen => |decl| {
                        if (self.exprContainsPatternRef(module_env, decl.expr, target_pattern)) {
                            return true;
                        }
                    },
                    else => {},
                }
            }
            // Check final expression
            return self.exprContainsPatternRef(module_env, block.final_expr, target_pattern);
        },
        .e_if => |if_expr| {
            // Check all branches
            const branches = module_env.store.sliceIfBranches(if_expr.branches);
            for (branches) |branch_idx| {
                const branch = module_env.store.getIfBranch(branch_idx);
                if (self.exprContainsPatternRef(module_env, branch.cond, target_pattern) or
                    self.exprContainsPatternRef(module_env, branch.body, target_pattern))
                {
                    return true;
                }
            }
            return self.exprContainsPatternRef(module_env, if_expr.final_else, target_pattern);
        },
        .e_binop => |binop| {
            return self.exprContainsPatternRef(module_env, binop.lhs, target_pattern) or
                self.exprContainsPatternRef(module_env, binop.rhs, target_pattern);
        },
        .e_unary_minus => |unary| {
            return self.exprContainsPatternRef(module_env, unary.expr, target_pattern);
        },
        .e_unary_not => |unary| {
            return self.exprContainsPatternRef(module_env, unary.expr, target_pattern);
        },
        // Leaf expressions that can't contain references
        else => return false,
    }
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
                // Set current binding pattern for recursive closure detection
                const old_binding = self.current_binding_pattern;
                self.current_binding_pattern = decl.pattern;
                const value = try self.lowerExprFromIdx(module_env, decl.expr);
                self.current_binding_pattern = old_binding;
                try lowered.append(self.allocator, .{
                    .pattern = pattern,
                    .expr = value,
                });
            },
            .s_decl_gen => |decl| {
                const pattern = try self.lowerPattern(module_env, decl.pattern);
                // Set current binding pattern for recursive closure detection
                const old_binding = self.current_binding_pattern;
                self.current_binding_pattern = decl.pattern;
                const value = try self.lowerExprFromIdx(module_env, decl.expr);
                self.current_binding_pattern = old_binding;
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

// ============ str_inspekt lowering ============

/// Lower str_inspekt(value) by expanding it into a tree of MonoExprs
/// that directly build the result string with all names embedded as literals.
///
/// This traverses the TYPE (for field/tag names) alongside the LAYOUT (for offsets)
/// to generate specialized inspection code at compile time.
pub fn lowerStrInspekt(
    self: *Self,
    value_expr: MonoExprId,
    type_var: types.Var,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Error!MonoExprId {
    // Need layout store to resolve layouts
    const layout_store = self.layout_store orelse return error.LayoutError;

    // Resolve the type to get its structure
    const resolved = module_env.types.resolveVar(type_var);

    return switch (resolved.desc.content) {
        .structure => |flat_type| switch (flat_type) {
            .record => |record| try self.lowerInspectRecord(
                value_expr,
                record,
                value_layout,
                module_env,
                region,
            ),
            .tuple => |tuple| try self.lowerInspectTuple(
                value_expr,
                tuple,
                value_layout,
                module_env,
                region,
            ),
            .tag_union => |tu| try self.lowerInspectTagUnion(
                value_expr,
                tu,
                value_layout,
                module_env,
                region,
            ),
            .empty_record => try self.addStrLiteral("{}", region),
            .empty_tag_union => try self.addStrLiteral("[]", region),
            .fn_pure, .fn_effectful, .fn_unbound => try self.addStrLiteral("<function>", region),
            .nominal_type => |nom| try self.lowerInspectNominal(
                value_expr,
                nom,
                value_layout,
                module_env,
                region,
            ),
            .record_unbound => try self.addStrLiteral("{}", region),
        },
        .flex, .rigid => {
            // Unresolved type variable - get layout info to determine how to render
            const layout_val = layout_store.getLayout(value_layout);
            return try self.lowerInspectByLayout(value_expr, layout_val, value_layout, region);
        },
        .alias => {
            // Alias - inspect the underlying type
            // For now, just treat as the layout indicates
            const layout_val = layout_store.getLayout(value_layout);
            return try self.lowerInspectByLayout(value_expr, layout_val, value_layout, region);
        },
        .err => try self.addStrLiteral("<error>", region),
    };
}

/// Inspect based on layout when we don't have full type info
fn lowerInspectByLayout(
    self: *Self,
    value_expr: MonoExprId,
    layout_val: layout_mod.Layout,
    value_layout: LayoutIdx,
    region: Region,
) Error!MonoExprId {
    _ = value_layout;

    return switch (layout_val.tag) {
        .scalar => switch (layout_val.data.scalar.tag) {
            .int => try self.store.addExpr(.{ .int_to_str = .{
                .value = value_expr,
                .int_precision = layout_val.data.scalar.data.int,
            } }, region),
            .frac => blk: {
                if (layout_val.data.scalar.data.frac == .dec) {
                    break :blk try self.store.addExpr(.{ .dec_to_str = value_expr }, region);
                }
                break :blk try self.store.addExpr(.{ .float_to_str = .{
                    .value = value_expr,
                    .float_precision = layout_val.data.scalar.data.frac,
                } }, region);
            },
            .str => try self.store.addExpr(.{ .str_escape_and_quote = value_expr }, region),
            .opaque_ptr => try self.addStrLiteral("<opaque>", region),
        },
        .list, .list_of_zst => try self.addStrLiteral("[...]", region),
        else => try self.addStrLiteral("<value>", region),
    };
}

/// Check if layout represents a Bool type (int with u8 precision)
fn isBoolLayout(layout_val: layout_mod.Layout) bool {
    return layout_val.tag == .scalar and
        layout_val.data.scalar.tag == .int and
        layout_val.data.scalar.data.int == .u8;
}

/// Inspect a boolean value
fn lowerInspectBool(self: *Self, value_expr: MonoExprId, region: Region) Error!MonoExprId {
    // Create branches for True (discriminant 1) and False (discriminant 0)
    const false_str = try self.addStrLiteral("Bool.false", region);
    const true_str = try self.addStrLiteral("Bool.true", region);

    const branches = try self.store.addExprSpan(&[_]MonoExprId{ false_str, true_str });

    return self.store.addExpr(.{ .discriminant_switch = .{
        .value = value_expr,
        .union_layout = .bool,
        .branches = branches,
    } }, region);
}

/// Inspect a record: { field1: value1, field2: value2, ... }
fn lowerInspectRecord(
    self: *Self,
    value_expr: MonoExprId,
    record: types.Record,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Error!MonoExprId {
    const layout_store = self.layout_store orelse return error.LayoutError;

    // Get field info from TYPE (for names)
    const fields_slice = module_env.types.getRecordFieldsSlice(record.fields);
    const field_names = fields_slice.items(.name);
    const field_vars = fields_slice.items(.var_);

    if (field_names.len == 0) {
        return try self.addStrLiteral("{}", region);
    }

    var parts = std.ArrayList(MonoExprId).empty;
    defer parts.deinit(self.allocator);

    // Opening brace
    try parts.append(self.allocator, try self.addStrLiteral("{ ", region));

    // Get layout data for field offsets
    const layout_val = layout_store.getLayout(value_layout);

    for (field_names, field_vars, 0..) |field_name_idx, field_var, i| {
        if (i > 0) {
            try parts.append(self.allocator, try self.addStrLiteral(", ", region));
        }

        // Get field name string from ident store
        const field_name = module_env.getIdent(field_name_idx);

        // Build "fieldName: "
        const name_with_colon = try std.fmt.allocPrint(self.allocator, "{s}: ", .{field_name});
        defer self.allocator.free(name_with_colon);
        try parts.append(self.allocator, try self.addStrLiteral(name_with_colon, region));

        // Get field layout from record layout
        // For now, we create field access based on index
        const field_layout = self.getFieldLayoutFromRecord(layout_val, @intCast(i), layout_store);

        // Create field access expression
        const field_access_expr = try self.store.addExpr(.{ .field_access = .{
            .record_expr = value_expr,
            .record_layout = value_layout,
            .field_layout = field_layout,
            .field_idx = @intCast(i),
            .field_name = field_name_idx,
        } }, region);

        // Recursively inspect the field value
        const field_inspect = try self.lowerStrInspekt(
            field_access_expr,
            field_var,
            field_layout,
            module_env,
            region,
        );
        try parts.append(self.allocator, field_inspect);
    }

    // Closing brace
    try parts.append(self.allocator, try self.addStrLiteral(" }", region));

    // Concatenate all parts
    return self.store.addExpr(.{ .str_concat = try self.store.addExprSpan(parts.items) }, region);
}

/// Inspect a tuple: (elem0, elem1, ...)
fn lowerInspectTuple(
    self: *Self,
    value_expr: MonoExprId,
    tuple: types.Tuple,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Error!MonoExprId {
    const layout_store = self.layout_store orelse return error.LayoutError;

    const elem_vars = module_env.types.sliceVars(tuple.elems);

    if (elem_vars.len == 0) {
        return try self.addStrLiteral("()", region);
    }

    var parts = std.ArrayList(MonoExprId).empty;
    defer parts.deinit(self.allocator);

    // Opening paren
    try parts.append(self.allocator, try self.addStrLiteral("(", region));

    const layout_val = layout_store.getLayout(value_layout);

    for (elem_vars, 0..) |elem_var, i| {
        if (i > 0) {
            try parts.append(self.allocator, try self.addStrLiteral(", ", region));
        }

        // Get element layout
        const elem_layout = self.getTupleElemLayout(layout_val, @intCast(i), layout_store);

        // Create tuple access expression
        const tuple_access_expr = try self.store.addExpr(.{ .tuple_access = .{
            .tuple_expr = value_expr,
            .tuple_layout = value_layout,
            .elem_layout = elem_layout,
            .elem_idx = @intCast(i),
        } }, region);

        // Recursively inspect the element
        const elem_inspect = try self.lowerStrInspekt(
            tuple_access_expr,
            elem_var,
            elem_layout,
            module_env,
            region,
        );
        try parts.append(self.allocator, elem_inspect);
    }

    // Closing paren
    try parts.append(self.allocator, try self.addStrLiteral(")", region));

    return self.store.addExpr(.{ .str_concat = try self.store.addExprSpan(parts.items) }, region);
}

/// Inspect a tag union: Ok(value) or Err(msg) or None
fn lowerInspectTagUnion(
    self: *Self,
    value_expr: MonoExprId,
    tag_union: types.TagUnion,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Error!MonoExprId {
    // Get tag info from TYPE
    const tags_slice = module_env.types.getTagsSlice(tag_union.tags);
    const tag_names = tags_slice.items(.name);
    const tag_args = tags_slice.items(.args);

    if (tag_names.len == 0) {
        return try self.addStrLiteral("[]", region);
    }

    // Build a branch for each tag variant
    var branches = std.ArrayList(MonoExprId).empty;
    defer branches.deinit(self.allocator);

    for (tag_names, tag_args) |tag_name_idx, args_range| {
        const tag_name = module_env.getIdent(tag_name_idx);
        const args_slice = module_env.types.sliceVars(args_range);

        if (args_slice.len == 0) {
            // No payload: just emit tag name
            try branches.append(self.allocator, try self.addStrLiteral(tag_name, region));
        } else {
            // Has payload: emit "TagName(payload)" or "TagName(p1, p2, ...)"
            var branch_parts = std.ArrayList(MonoExprId).empty;
            defer branch_parts.deinit(self.allocator);

            try branch_parts.append(self.allocator, try self.addStrLiteral(tag_name, region));
            try branch_parts.append(self.allocator, try self.addStrLiteral("(", region));

            // For multi-arg payloads, we'd need to access each argument
            // For now, just show a placeholder for payload
            for (args_slice, 0..) |_, arg_i| {
                if (arg_i > 0) {
                    try branch_parts.append(self.allocator, try self.addStrLiteral(", ", region));
                }
                // TODO: Access actual payload values using layout info
                try branch_parts.append(self.allocator, try self.addStrLiteral("_", region));
            }

            try branch_parts.append(self.allocator, try self.addStrLiteral(")", region));
            try branches.append(self.allocator, try self.store.addExpr(.{
                .str_concat = try self.store.addExprSpan(branch_parts.items),
            }, region));
        }
    }

    // Create discriminant switch
    return self.store.addExpr(.{ .discriminant_switch = .{
        .value = value_expr,
        .union_layout = value_layout,
        .branches = try self.store.addExprSpan(branches.items),
    } }, region);
}

/// Inspect a nominal type: TypeName.value
fn lowerInspectNominal(
    self: *Self,
    value_expr: MonoExprId,
    nom: types.NominalType,
    value_layout: LayoutIdx,
    module_env: *const ModuleEnv,
    region: Region,
) Error!MonoExprId {
    const type_name = module_env.getIdent(nom.ident.ident_idx);

    // Check for Bool type (special case: render as Bool.true/Bool.false)
    if (std.mem.eql(u8, type_name, "Bool")) {
        return try self.lowerInspectBool(value_expr, region);
    }

    if (nom.is_opaque) {
        // Opaque types render as <opaque>
        return try self.addStrLiteral("<opaque>", region);
    }

    // For transparent nominal types, inspect the backing value
    // Get the first type variable which should be the backing type
    const vars = module_env.types.sliceVars(nom.vars.nonempty);
    if (vars.len > 0) {
        const backing_var = vars[0];
        return self.lowerStrInspekt(value_expr, backing_var, value_layout, module_env, region);
    }

    // Fallback: just show the type name with the value
    var parts = std.ArrayList(MonoExprId).empty;
    defer parts.deinit(self.allocator);

    try parts.append(self.allocator, try self.addStrLiteral(type_name, region));
    try parts.append(self.allocator, try self.addStrLiteral(".", region));
    // For backing value, inspect by layout
    const layout_store = self.layout_store orelse return error.LayoutError;
    const layout_val = layout_store.getLayout(value_layout);
    try parts.append(self.allocator, try self.lowerInspectByLayout(value_expr, layout_val, value_layout, region));

    return self.store.addExpr(.{ .str_concat = try self.store.addExprSpan(parts.items) }, region);
}

/// Helper to add a string literal expression
fn addStrLiteral(self: *Self, text: []const u8, region: Region) Error!MonoExprId {
    // Add the string literal to the MonoExprStore's string table
    const lit_idx = self.store.insertString(text) catch return error.OutOfMemory;
    return self.store.addExpr(.{ .str_literal = lit_idx }, region);
}

/// Helper to get field layout from record layout
fn getFieldLayoutFromRecord(
    self: *Self,
    layout_val: layout_mod.Layout,
    field_idx: u16,
    layout_store: *LayoutStore,
) LayoutIdx {
    _ = self;
    if (layout_val.tag == .record) {
        const record_data = layout_store.getRecordData(layout_val.data.record.idx);
        const fields_range = record_data.getFields();
        const fields = layout_store.record_fields.sliceRange(fields_range);
        if (field_idx < fields.len) {
            return fields.get(field_idx).layout;
        }
    }
    // Fallback to a default layout
    return .i64;
}

/// Helper to get tuple element layout
fn getTupleElemLayout(
    self: *Self,
    layout_val: layout_mod.Layout,
    elem_idx: u16,
    layout_store: *LayoutStore,
) LayoutIdx {
    _ = self;
    if (layout_val.tag == .tuple) {
        const tuple_data = layout_store.getTupleData(layout_val.data.tuple.idx);
        const fields_range = tuple_data.getFields();
        const fields = layout_store.tuple_fields.sliceRange(fields_range);
        if (elem_idx < fields.len) {
            return fields.get(elem_idx).layout;
        }
    }
    // Fallback to a default layout
    return .i64;
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

/// A lowered constant: maps a definition to its Mono IR expression.
/// This is used to track constants after lowering so they can be
/// evaluated at compile time.
pub const LoweredConstant = struct {
    /// The module this constant belongs to
    module_idx: u16,
    /// The original CIR definition index
    def_idx: CIR.Def.Idx,
    /// The Mono IR expression ID for this constant's value
    mono_expr_id: MonoExprId,
};

/// Result of lowering all constants in a module.
pub const LoweredConstants = struct {
    /// Array of lowered constants in dependency order
    constants: []LoweredConstant,
    allocator: Allocator,

    pub fn deinit(self: *LoweredConstants) void {
        self.allocator.free(self.constants);
    }
};

/// Lower all constants from the given SCCs and return the mapping.
///
/// This lowers each constant definition to Mono IR and tracks the mapping
/// from def index to MonoExprId, enabling later compile-time evaluation.
///
/// Parameters:
/// - lowerer: The initialized lowerer to use
/// - module_idx: The module these constants belong to
/// - sccs: The SCCs containing constant definitions in dependency order
/// - allocator: Allocator for the result
///
/// Returns a LoweredConstants struct containing the mapping from each
/// constant's def_idx to its mono_expr_id.
pub fn lowerConstants(
    lowerer: *Self,
    module_idx: u16,
    sccs: []const can.DependencyGraph.SCC,
    allocator: Allocator,
) Error!LoweredConstants {
    // Count total constants across all SCCs
    var total_constants: usize = 0;
    for (sccs) |scc| {
        total_constants += scc.defs.len;
    }

    if (total_constants == 0) {
        return LoweredConstants{
            .constants = &[_]LoweredConstant{},
            .allocator = allocator,
        };
    }

    var constants = allocator.alloc(LoweredConstant, total_constants) catch return error.OutOfMemory;
    errdefer allocator.free(constants);

    const module_env = lowerer.getModuleEnv(module_idx) orelse return error.ModuleNotFound;

    var i: usize = 0;
    for (sccs) |scc| {
        for (scc.defs) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const mono_expr_id = try lowerer.lowerExpr(module_idx, def.expr);

            constants[i] = .{
                .module_idx = module_idx,
                .def_idx = def_idx,
                .mono_expr_id = mono_expr_id,
            };
            i += 1;
        }
    }

    return LoweredConstants{
        .constants = constants,
        .allocator = allocator,
    };
}

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

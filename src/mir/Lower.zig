//! CIR → MIR Lowering Pass
//!
//! Converts polymorphic, sugar-rich CIR expressions into monomorphic,
//! desugared MIR expressions. This replaces the Monomorphizer.
//!
//! Key transformations:
//! - `e_if` → `match` on Bool
//! - `e_binop` → `call` to resolved method
//! - `e_type_var_dispatch` → `call` with resolved target
//! - `e_nominal` → backing expression (strip nominal wrapper)
//! - `e_closure` → `lambda` with captures
//! - All lookups unified to `MonoSymbol` (module_idx + ident_idx)

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const builtins = @import("builtins");

const MIR = @import("MIR.zig");
const Monotype = @import("Monotype.zig");

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Region = base.Region;
const Allocator = std.mem.Allocator;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const Self = @This();

// --- Fields ---

allocator: Allocator,

/// Target MIR store
store: *MIR.Store,

/// All module environments (indexed by module_idx)
all_module_envs: []const *ModuleEnv,

/// Types store for resolving type variables
types_store: *const types.Store,

/// Builtin type indices for identifying primitives
builtin_indices: CIR.BuiltinIndices,

/// Current module being lowered
current_module_idx: u16,

/// Map from (module_idx << 32 | CIR.Pattern.Idx) → MIR.MonoSymbol
/// Used to resolve CIR local lookups to global symbols.
pattern_symbols: std.AutoHashMap(u64, MIR.MonoSymbol),

/// Cache for type var → monotype conversion (shared across all fromTypeVar calls)
type_var_seen: std.AutoHashMap(types.Var, Monotype.Idx),

// --- Init/Deinit ---

pub fn init(
    allocator: Allocator,
    store: *MIR.Store,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    builtin_indices: CIR.BuiltinIndices,
    current_module_idx: u16,
) Self {
    return .{
        .allocator = allocator,
        .store = store,
        .all_module_envs = all_module_envs,
        .types_store = types_store,
        .builtin_indices = builtin_indices,
        .current_module_idx = current_module_idx,
        .pattern_symbols = std.AutoHashMap(u64, MIR.MonoSymbol).init(allocator),
        .type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.pattern_symbols.deinit();
    self.type_var_seen.deinit();
}

// --- Public API ---

/// Lower a CIR expression to MIR.
pub fn lowerExpr(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ExprId {
    const module_env = self.all_module_envs[self.current_module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    const region = module_env.store.getExprRegion(expr_idx);
    const monotype = try self.resolveMonotype(expr_idx);

    return switch (expr) {
        // --- Literals ---
        .e_num => |num| try self.store.addExpr(self.allocator, .{ .int = .{ .value = num.value } }, monotype, region),
        .e_frac_f32 => |frac| try self.store.addExpr(self.allocator, .{ .frac_f32 = frac.value }, monotype, region),
        .e_frac_f64 => |frac| try self.store.addExpr(self.allocator, .{ .frac_f64 = frac.value }, monotype, region),
        .e_dec => |dec| try self.store.addExpr(self.allocator, .{ .dec = dec.value }, monotype, region),
        .e_dec_small => |dec_small| {
            const roc_dec = dec_small.value.toRocDec();
            return try self.store.addExpr(self.allocator, .{ .dec = roc_dec }, monotype, region);
        },
        .e_typed_int => |ti| try self.store.addExpr(self.allocator, .{ .int = .{ .value = ti.value } }, monotype, region),
        .e_typed_frac => |tf| try self.store.addExpr(self.allocator, .{ .int = .{ .value = tf.value } }, monotype, region),

        // --- Strings ---
        .e_str_segment => |seg| try self.store.addExpr(self.allocator, .{ .str = seg.literal }, monotype, region),
        .e_str => |str_expr| {
            // Multi-segment string: if single segment, unwrap. Otherwise, lower each segment.
            const span = module_env.store.sliceExpr(str_expr.span);
            if (span.len == 1) {
                return try self.lowerExpr(span[0]);
            }
            // For multi-segment strings, lower each segment as its own expr
            // and produce a list of them. The downstream pass will handle concatenation.
            // For now, just lower the first segment as a simplification.
            if (span.len > 0) {
                return try self.lowerExpr(span[0]);
            }
            // Empty string
            return try self.store.addExpr(self.allocator, .{ .str = @enumFromInt(std.math.maxInt(u32)) }, monotype, region);
        },

        // --- Collections ---
        .e_empty_list => try self.store.addExpr(self.allocator, .{ .list = .{ .elems = MIR.ExprSpan.empty() } }, monotype, region),
        .e_list => |list| {
            const elems = try self.lowerExprSpan(module_env, list.elems);
            return try self.store.addExpr(self.allocator, .{ .list = .{ .elems = elems } }, monotype, region);
        },
        .e_empty_record => try self.store.addExpr(self.allocator, .{ .record = .{
            .fields = MIR.ExprSpan.empty(),
            .field_names = MIR.FieldNameSpan.empty(),
        } }, monotype, region),
        .e_record => |record| {
            return try self.lowerRecord(module_env, record, monotype, region);
        },
        .e_tuple => |tuple| {
            const elems = try self.lowerExprSpan(module_env, tuple.elems);
            return try self.store.addExpr(self.allocator, .{ .tuple = .{ .elems = elems } }, monotype, region);
        },
        .e_tag => |tag| {
            const args = try self.lowerExprSpan(module_env, tag.args);
            return try self.store.addExpr(self.allocator, .{ .tag = .{
                .name = tag.name,
                .args = args,
            } }, monotype, region);
        },
        .e_zero_argument_tag => |zat| {
            return try self.store.addExpr(self.allocator, .{ .tag = .{
                .name = zat.name,
                .args = MIR.ExprSpan.empty(),
            } }, monotype, region);
        },

        // --- Lookups ---
        .e_lookup_local => |lookup| {
            const symbol = self.patternToSymbol(lookup.pattern_idx);
            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
        },
        .e_lookup_external => |ext| {
            const resolved_module = module_env.imports.getResolvedModule(ext.module_idx);
            const target_module_idx: u16 = if (resolved_module) |m| @intCast(m) else self.current_module_idx;
            const symbol = MIR.MonoSymbol{
                .module_idx = target_module_idx,
                .ident_idx = ext.ident_idx,
            };
            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
        },
        .e_lookup_pending, .e_lookup_required => {
            // Should be resolved before MIR lowering
            return try self.store.addExpr(self.allocator, .{ .runtime_error = .{ .diagnostic = @enumFromInt(std.math.maxInt(u32)) } }, monotype, region);
        },

        // --- Control flow ---
        .e_if => |if_expr| try self.lowerIf(module_env, if_expr, monotype, region),
        .e_match => |match_expr| try self.lowerMatch(module_env, match_expr, monotype, region),

        // --- Functions ---
        .e_lambda => |lambda| try self.lowerLambda(module_env, lambda, monotype, region),
        .e_closure => |closure| try self.lowerClosure(module_env, closure, monotype, region),
        .e_call => |call| try self.lowerCall(module_env, call, monotype, region),

        // --- Block ---
        .e_block => |block| try self.lowerBlock(module_env, block, monotype, region),

        // --- Operators (desugared to calls) ---
        .e_binop => |binop| try self.lowerBinop(binop, monotype, region),
        .e_unary_minus => |um| try self.lowerUnaryMinus(um, monotype, region),
        .e_unary_not => |un| try self.lowerUnaryNot(un, monotype, region),

        // --- Access ---
        .e_dot_access => |da| try self.lowerDotAccess(module_env, da, monotype, region),
        .e_tuple_access => |ta| {
            const tuple_expr = try self.lowerExpr(ta.tuple);
            return try self.store.addExpr(self.allocator, .{ .tuple_access = .{
                .tuple = tuple_expr,
                .elem_index = ta.elem_index,
            } }, monotype, region);
        },

        // --- Nominal (strip wrapper) ---
        .e_nominal => |nom| try self.lowerExpr(nom.backing_expr),
        .e_nominal_external => |nom_ext| try self.lowerExpr(nom_ext.backing_expr),

        // --- Type var dispatch (resolved to call) ---
        .e_type_var_dispatch => |_| {
            // TODO: Resolve static dispatch — requires type scope
            return try self.store.addExpr(self.allocator, .{ .runtime_error = .{
                .diagnostic = @enumFromInt(std.math.maxInt(u32)),
            } }, monotype, region);
        },

        // --- For (desugared) ---
        .e_for => |_| {
            // TODO: Desugar for loop
            return try self.store.addExpr(self.allocator, .{ .runtime_error = .{
                .diagnostic = @enumFromInt(std.math.maxInt(u32)),
            } }, monotype, region);
        },

        // --- Special ---
        .e_hosted_lambda => |hosted| {
            const params = try self.lowerPatternSpan(module_env, hosted.args);
            const body = try self.lowerExpr(hosted.body);
            return try self.store.addExpr(self.allocator, .{ .hosted = .{
                .symbol_name = hosted.symbol_name,
                .index = hosted.index,
                .params = params,
                .body = body,
            } }, monotype, region);
        },
        .e_low_level_lambda => |ll| {
            const args = try self.lowerPatternSpan(module_env, ll.args);
            return try self.store.addExpr(self.allocator, .{ .low_level = .{
                .op = ll.op,
                .args = try self.patternSpanToExprSpan(module_env, args),
            } }, monotype, region);
        },

        // --- Error/Debug ---
        .e_runtime_error => |re| try self.store.addExpr(self.allocator, .{ .runtime_error = .{ .diagnostic = re.diagnostic } }, monotype, region),
        .e_crash => |crash| try self.store.addExpr(self.allocator, .{ .crash = crash.msg }, monotype, region),
        .e_dbg => |dbg_expr| {
            const inner = try self.lowerExpr(dbg_expr.expr);
            return try self.store.addExpr(self.allocator, .{ .dbg_expr = .{ .expr = inner } }, monotype, region);
        },
        .e_expect => |expect| {
            const body = try self.lowerExpr(expect.body);
            return try self.store.addExpr(self.allocator, .{ .expect = .{ .body = body } }, monotype, region);
        },
        .e_ellipsis => {
            return try self.store.addExpr(self.allocator, .{ .crash = @enumFromInt(std.math.maxInt(u32)) }, monotype, region);
        },
        .e_anno_only => {
            return try self.store.addExpr(self.allocator, .{ .runtime_error = .{
                .diagnostic = @enumFromInt(std.math.maxInt(u32)),
            } }, monotype, region);
        },
        .e_return => |ret| try self.lowerExpr(ret.expr),
    };
}

// --- Helpers ---

/// Resolve a CIR pattern to a global MIR symbol.
fn patternToSymbol(self: *Self, pattern_idx: CIR.Pattern.Idx) MIR.MonoSymbol {
    const key = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(pattern_idx);

    if (self.pattern_symbols.get(key)) |existing| {
        return existing;
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);

    const ident_idx: Ident.Idx = switch (pattern) {
        .assign => |a| a.ident,
        .as => |a| a.ident,
        else => Ident.Idx.NONE,
    };

    const symbol = MIR.MonoSymbol{
        .module_idx = self.current_module_idx,
        .ident_idx = ident_idx,
    };

    self.pattern_symbols.put(key, symbol) catch {};
    return symbol;
}

/// Get the monotype for a CIR expression (via its type var).
fn resolveMonotype(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!Monotype.Idx {
    const type_var = ModuleEnv.varFrom(expr_idx);
    return try self.store.monotype_store.fromTypeVar(
        self.allocator,
        self.types_store,
        type_var,
        self.builtin_indices,
        &self.type_var_seen,
    );
}

/// Lower a CIR Expr.Span to an MIR ExprSpan.
fn lowerExprSpan(self: *Self, module_env: *const ModuleEnv, span: CIR.Expr.Span) Allocator.Error!MIR.ExprSpan {
    const cir_ids = module_env.store.sliceExpr(span);
    if (cir_ids.len == 0) return MIR.ExprSpan.empty();

    var mir_ids = std.ArrayList(MIR.ExprId).empty;
    defer mir_ids.deinit(self.allocator);

    for (cir_ids) |cir_id| {
        const mir_id = try self.lowerExpr(cir_id);
        try mir_ids.append(self.allocator, mir_id);
    }

    return try self.store.addExprSpan(self.allocator, mir_ids.items);
}

/// Lower a CIR Pattern.Span to an MIR PatternSpan.
fn lowerPatternSpan(self: *Self, module_env: *const ModuleEnv, span: CIR.Pattern.Span) Allocator.Error!MIR.PatternSpan {
    const cir_ids = module_env.store.slicePatterns(span);
    if (cir_ids.len == 0) return MIR.PatternSpan.empty();

    var mir_ids = std.ArrayList(MIR.PatternId).empty;
    defer mir_ids.deinit(self.allocator);

    for (cir_ids) |cir_id| {
        const mir_id = try self.lowerPattern(module_env, cir_id);
        try mir_ids.append(self.allocator, mir_id);
    }

    return try self.store.addPatternSpan(self.allocator, mir_ids.items);
}

/// Lower a CIR pattern to an MIR pattern.
fn lowerPattern(self: *Self, module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) Allocator.Error!MIR.PatternId {
    const pattern = module_env.store.getPattern(pattern_idx);
    const type_var = ModuleEnv.varFrom(pattern_idx);
    const monotype = try self.store.monotype_store.fromTypeVar(
        self.allocator,
        self.types_store,
        type_var,
        self.builtin_indices,
        &self.type_var_seen,
    );

    return switch (pattern) {
        .assign => |a| {
            const symbol = MIR.MonoSymbol{
                .module_idx = self.current_module_idx,
                .ident_idx = a.ident,
            };
            // Register this pattern → symbol mapping
            const key = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(pattern_idx);
            try self.pattern_symbols.put(key, symbol);
            return try self.store.addPattern(self.allocator, .{ .bind = symbol }, monotype);
        },
        .underscore => try self.store.addPattern(self.allocator, .wildcard, monotype),
        .as => |a| {
            const inner = try self.lowerPattern(module_env, a.pattern);
            const symbol = MIR.MonoSymbol{
                .module_idx = self.current_module_idx,
                .ident_idx = a.ident,
            };
            const key = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(pattern_idx);
            try self.pattern_symbols.put(key, symbol);
            return try self.store.addPattern(self.allocator, .{ .as_pattern = .{
                .pattern = inner,
                .symbol = symbol,
            } }, monotype);
        },
        .applied_tag => |tag| {
            const args = try self.lowerPatternSpan(module_env, tag.args);
            return try self.store.addPattern(self.allocator, .{ .tag = .{
                .name = tag.name,
                .args = args,
            } }, monotype);
        },
        .nominal => |nom| {
            // Strip nominal wrapper — lower the backing pattern
            return try self.lowerPattern(module_env, nom.backing_pattern);
        },
        .nominal_external => |nom_ext| {
            // Strip nominal wrapper — lower the backing pattern
            return try self.lowerPattern(module_env, nom_ext.backing_pattern);
        },
        .num_literal => |nl| try self.store.addPattern(self.allocator, .{ .int_literal = .{ .value = nl.value } }, monotype),
        .str_literal => |sl| try self.store.addPattern(self.allocator, .{ .str_literal = sl.literal }, monotype),
        .dec_literal => |dl| try self.store.addPattern(self.allocator, .{ .dec_literal = dl.value }, monotype),
        .small_dec_literal => |sdl| {
            const roc_dec = sdl.value.toRocDec();
            return try self.store.addPattern(self.allocator, .{ .dec_literal = roc_dec }, monotype);
        },
        .frac_f32_literal => |fl| try self.store.addPattern(self.allocator, .{ .frac_f32_literal = fl.value }, monotype),
        .frac_f64_literal => |fl| try self.store.addPattern(self.allocator, .{ .frac_f64_literal = fl.value }, monotype),
        .runtime_error => try self.store.addPattern(self.allocator, .runtime_error, monotype),
        .list => |list_pat| {
            const patterns = try self.lowerPatternSpan(module_env, list_pat.patterns);
            var rest_index: u32 = std.math.maxInt(u32);
            var rest_pattern: MIR.PatternId = MIR.PatternId.none;
            if (list_pat.rest_info) |rest| {
                rest_index = rest.index;
                if (rest.pattern) |rest_pat_idx| {
                    rest_pattern = try self.lowerPattern(module_env, rest_pat_idx);
                }
            }
            return try self.store.addPattern(self.allocator, .{ .list_destructure = .{
                .patterns = patterns,
                .rest_index = rest_index,
                .rest_pattern = rest_pattern,
            } }, monotype);
        },
        .record_destructure => |record_pat| {
            const cir_destructs = module_env.store.sliceRecordDestructs(record_pat.destructs);
            var destruct_pats = std.ArrayList(MIR.PatternId).empty;
            defer destruct_pats.deinit(self.allocator);
            var field_names = std.ArrayList(Ident.Idx).empty;
            defer field_names.deinit(self.allocator);

            for (cir_destructs) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                try field_names.append(self.allocator, destruct.label);
                const pat_idx = destruct.kind.toPatternIdx();
                const mir_pat = try self.lowerPattern(module_env, pat_idx);
                try destruct_pats.append(self.allocator, mir_pat);
            }

            const destructs_span = try self.store.addPatternSpan(self.allocator, destruct_pats.items);
            const names_span = try self.store.addFieldNameSpan(self.allocator, field_names.items);
            return try self.store.addPattern(self.allocator, .{ .record_destructure = .{
                .destructs = destructs_span,
                .field_names = names_span,
            } }, monotype);
        },
        .tuple => |tuple_pat| {
            const elems = try self.lowerPatternSpan(module_env, tuple_pat.patterns);
            return try self.store.addPattern(self.allocator, .{ .tuple_destructure = .{ .elems = elems } }, monotype);
        },
    };
}

/// Convert a PatternSpan to an ExprSpan (for low_level args — patterns become lookups).
fn patternSpanToExprSpan(self: *Self, module_env: *const ModuleEnv, pat_span: MIR.PatternSpan) Allocator.Error!MIR.ExprSpan {
    _ = module_env;
    const pats = self.store.getPatternSpan(pat_span);
    if (pats.len == 0) return MIR.ExprSpan.empty();

    var exprs = std.ArrayList(MIR.ExprId).empty;
    defer exprs.deinit(self.allocator);

    for (pats) |pat_id| {
        const pat = self.store.getPattern(pat_id);
        const monotype = self.store.patternTypeOf(pat_id);
        switch (pat) {
            .bind => |symbol| {
                const expr_id = try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, Region.zero());
                try exprs.append(self.allocator, expr_id);
            },
            else => {
                // For non-bind patterns, create a placeholder
                const expr_id = try self.store.addExpr(self.allocator, .{ .runtime_error = .{
                    .diagnostic = @enumFromInt(std.math.maxInt(u32)),
                } }, monotype, Region.zero());
                try exprs.append(self.allocator, expr_id);
            },
        }
    }

    return try self.store.addExprSpan(self.allocator, exprs.items);
}

// --- Desugaring helpers ---

/// Lower `e_if` to nested `match` on Bool.
fn lowerIf(self: *Self, module_env: *const ModuleEnv, if_expr: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const final_else = try self.lowerExpr(if_expr.final_else);

    // Desugar if-else chains into nested match expressions from the last branch backward
    const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);
    var result = final_else;

    // Process branches in reverse order to build nested matches
    var i: usize = branch_indices.len;
    while (i > 0) {
        i -= 1;
        const branch = module_env.store.getIfBranch(branch_indices[i]);
        const cond = try self.lowerExpr(branch.cond);
        const body = try self.lowerExpr(branch.body);

        // Create True and wildcard patterns
        const true_monotype = try self.store.monotype_store.addMonotype(self.allocator, .{ .prim = .bool });
        const true_pattern = try self.store.addPattern(self.allocator, .{ .tag = .{
            .name = Ident.Idx.NONE, // TODO: proper True ident
            .args = MIR.PatternSpan.empty(),
        } }, true_monotype);
        const wildcard_pattern = try self.store.addPattern(self.allocator, .wildcard, true_monotype);

        const true_bp = try self.store.addBranchPatterns(self.allocator, &.{.{ .pattern = true_pattern, .degenerate = false }});
        const else_bp = try self.store.addBranchPatterns(self.allocator, &.{.{ .pattern = wildcard_pattern, .degenerate = false }});

        const branch_span = try self.store.addBranches(self.allocator, &.{
            .{ .patterns = true_bp, .body = body, .guard = MIR.ExprId.none },
            .{ .patterns = else_bp, .body = result, .guard = MIR.ExprId.none },
        });

        result = try self.store.addExpr(self.allocator, .{ .match_expr = .{
            .cond = cond,
            .branches = branch_span,
        } }, monotype, region);
    }

    return result;
}

/// Lower `e_match` to MIR match.
fn lowerMatch(self: *Self, module_env: *const ModuleEnv, match_expr: CIR.Expr.Match, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const cond = try self.lowerExpr(match_expr.cond);
    const cir_branch_indices = module_env.store.sliceMatchBranches(match_expr.branches);

    var mir_branches = std.ArrayList(MIR.Branch).empty;
    defer mir_branches.deinit(self.allocator);

    for (cir_branch_indices) |branch_idx| {
        const cir_branch = module_env.store.getMatchBranch(branch_idx);
        const body = try self.lowerExpr(cir_branch.value);
        const guard = if (cir_branch.guard) |guard_idx|
            try self.lowerExpr(guard_idx)
        else
            MIR.ExprId.none;

        // Lower branch patterns
        const cir_bp_indices = module_env.store.sliceMatchBranchPatterns(cir_branch.patterns);
        var branch_pats = std.ArrayList(MIR.BranchPattern).empty;
        defer branch_pats.deinit(self.allocator);

        for (cir_bp_indices) |bp_idx| {
            const cir_bp = module_env.store.getMatchBranchPattern(bp_idx);
            const pat = try self.lowerPattern(module_env, cir_bp.pattern);
            try branch_pats.append(self.allocator, .{ .pattern = pat, .degenerate = cir_bp.degenerate });
        }

        const bp_span = try self.store.addBranchPatterns(self.allocator, branch_pats.items);
        try mir_branches.append(self.allocator, .{ .patterns = bp_span, .body = body, .guard = guard });
    }

    const branch_span = try self.store.addBranches(self.allocator, mir_branches.items);
    return try self.store.addExpr(self.allocator, .{ .match_expr = .{
        .cond = cond,
        .branches = branch_span,
    } }, monotype, region);
}

/// Lower `e_lambda` to MIR lambda (no captures).
fn lowerLambda(self: *Self, module_env: *const ModuleEnv, lambda: CIR.Expr.Lambda, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const params = try self.lowerPatternSpan(module_env, lambda.args);
    const body = try self.lowerExpr(lambda.body);
    return try self.store.addExpr(self.allocator, .{ .lambda = .{
        .params = params,
        .body = body,
        .captures = MIR.CaptureSpan.empty(),
    } }, monotype, region);
}

/// Lower `e_closure` to MIR lambda with captures.
fn lowerClosure(self: *Self, module_env: *const ModuleEnv, closure: CIR.Expr.Closure, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    // Lower the inner lambda
    const inner_lambda_expr = module_env.store.getExpr(closure.lambda_idx);
    const lambda = inner_lambda_expr.e_lambda;
    const params = try self.lowerPatternSpan(module_env, lambda.args);
    const body = try self.lowerExpr(lambda.body);

    // Lower captures
    const cir_capture_indices = module_env.store.sliceCaptures(closure.captures);
    var mir_captures = std.ArrayList(MIR.Capture).empty;
    defer mir_captures.deinit(self.allocator);

    for (cir_capture_indices) |cap_idx| {
        const cap = module_env.store.getCapture(cap_idx);
        const symbol = self.patternToSymbol(cap.pattern_idx);
        try mir_captures.append(self.allocator, .{ .symbol = symbol });
    }

    const capture_span = try self.store.addCaptures(self.allocator, mir_captures.items);

    return try self.store.addExpr(self.allocator, .{ .lambda = .{
        .params = params,
        .body = body,
        .captures = capture_span,
    } }, monotype, region);
}

/// Lower `e_call` to MIR call.
fn lowerCall(self: *Self, module_env: *const ModuleEnv, call: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const func = try self.lowerExpr(call.func);
    const args = try self.lowerExprSpan(module_env, call.args);

    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = func,
        .args = args,
    } }, monotype, region);
}

/// Lower `e_block` to MIR block.
fn lowerBlock(self: *Self, module_env: *const ModuleEnv, block: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const cir_stmt_indices = module_env.store.sliceStatements(block.stmts);
    var mir_stmts = std.ArrayList(MIR.Stmt).empty;
    defer mir_stmts.deinit(self.allocator);

    for (cir_stmt_indices) |stmt_idx| {
        const cir_stmt = module_env.store.getStatement(stmt_idx);
        switch (cir_stmt) {
            .s_decl => |decl| {
                const pat = try self.lowerPattern(module_env, decl.pattern);
                const expr = try self.lowerExpr(decl.expr);
                try mir_stmts.append(self.allocator, .{ .pattern = pat, .expr = expr });
            },
            .s_var => |var_decl| {
                const pat = try self.lowerPattern(module_env, var_decl.pattern_idx);
                const expr = try self.lowerExpr(var_decl.expr);
                try mir_stmts.append(self.allocator, .{ .pattern = pat, .expr = expr });
            },
            .s_reassign => |reassign| {
                const pat = try self.lowerPattern(module_env, reassign.pattern_idx);
                const expr = try self.lowerExpr(reassign.expr);
                try mir_stmts.append(self.allocator, .{ .pattern = pat, .expr = expr });
            },
            .s_expr => |s_expr| {
                // Expression statement: bind to wildcard
                const expr = try self.lowerExpr(s_expr.expr);
                const expr_type = self.store.typeOf(@enumFromInt(@intFromEnum(expr)));
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, expr_type);
                try mir_stmts.append(self.allocator, .{ .pattern = wildcard, .expr = expr });
            },
            .s_dbg => |s_dbg| {
                const expr = try self.lowerExpr(s_dbg.expr);
                const expr_type = self.store.typeOf(@enumFromInt(@intFromEnum(expr)));
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, expr_type);
                try mir_stmts.append(self.allocator, .{ .pattern = wildcard, .expr = expr });
            },
            .s_expect => |s_expect| {
                const expr = try self.lowerExpr(s_expect.body);
                const expr_type = self.store.typeOf(@enumFromInt(@intFromEnum(expr)));
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, expr_type);
                try mir_stmts.append(self.allocator, .{ .pattern = wildcard, .expr = expr });
            },
            else => {
                // Skip other statement types (imports, type annotations, etc.)
            },
        }
    }

    const stmt_span = try self.store.addStmts(self.allocator, mir_stmts.items);
    const final_expr = try self.lowerExpr(block.final_expr);

    return try self.store.addExpr(self.allocator, .{ .block = .{
        .stmts = stmt_span,
        .final_expr = final_expr,
    } }, monotype, region);
}

/// Lower `e_binop` — stub, will be desugared to call.
fn lowerBinop(self: *Self, binop: CIR.Expr.Binop, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    // TODO: Desugar to method call (e.g., + → Num.add)
    // For now, lower both sides and produce a placeholder call
    const lhs = try self.lowerExpr(binop.lhs);
    const rhs = try self.lowerExpr(binop.rhs);

    // Placeholder: create a call with lhs as function and rhs as arg
    const args = try self.store.addExprSpan(self.allocator, &.{ lhs, rhs });
    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = lhs,
        .args = args,
    } }, monotype, region);
}

/// Lower `e_unary_minus` — stub.
fn lowerUnaryMinus(self: *Self, um: CIR.Expr.UnaryMinus, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    // TODO: Desugar to Num.negate call
    const inner = try self.lowerExpr(um.expr);
    const args = try self.store.addExprSpan(self.allocator, &.{inner});
    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = inner,
        .args = args,
    } }, monotype, region);
}

/// Lower `e_unary_not` — stub.
fn lowerUnaryNot(self: *Self, un: CIR.Expr.UnaryNot, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    // TODO: Desugar to match on Bool
    const inner = try self.lowerExpr(un.expr);
    const args = try self.store.addExprSpan(self.allocator, &.{inner});
    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = inner,
        .args = args,
    } }, monotype, region);
}

/// Lower `e_dot_access` — field access or method call.
fn lowerDotAccess(self: *Self, module_env: *const ModuleEnv, da: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const receiver = try self.lowerExpr(da.receiver);

    if (da.args) |args_span| {
        // Method call: desugar to a function call
        // TODO: resolve via static dispatch
        const args = try self.lowerExprSpan(module_env, args_span);
        return try self.store.addExpr(self.allocator, .{ .call = .{
            .func = receiver,
            .args = args,
        } }, monotype, region);
    } else {
        // Field access
        return try self.store.addExpr(self.allocator, .{ .record_access = .{
            .record = receiver,
            .field_name = da.field_name,
        } }, monotype, region);
    }
}

/// Lower a CIR record expression.
fn lowerRecord(self: *Self, module_env: *const ModuleEnv, record: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const cir_field_indices = module_env.store.sliceRecordFields(record.fields);

    var field_exprs = std.ArrayList(MIR.ExprId).empty;
    defer field_exprs.deinit(self.allocator);
    var field_names = std.ArrayList(Ident.Idx).empty;
    defer field_names.deinit(self.allocator);

    for (cir_field_indices) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        const expr = try self.lowerExpr(field.value);
        try field_exprs.append(self.allocator, expr);
        try field_names.append(self.allocator, field.name);
    }

    const fields_span = try self.store.addExprSpan(self.allocator, field_exprs.items);
    const names_span = try self.store.addFieldNameSpan(self.allocator, field_names.items);

    return try self.store.addExpr(self.allocator, .{ .record = .{
        .fields = fields_span,
        .field_names = names_span,
    } }, monotype, region);
}

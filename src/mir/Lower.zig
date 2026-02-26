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
//! - All lookups unified to `Symbol` (module_idx + ident_idx)

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const types = @import("types");

const MIR = @import("MIR.zig");
const Monotype = @import("Monotype.zig");

const Ident = base.Ident;
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
current_module_idx: u32,

/// App module index (for resolving `e_lookup_required` from platform modules)
app_module_idx: ?u32,

/// Map from (module_idx << 32 | CIR.Pattern.Idx) → MIR.Symbol
/// Used to resolve CIR local lookups to global symbols.
pattern_symbols: std.AutoHashMap(u64, MIR.Symbol),

/// Cache for type var → monotype conversion (shared across all fromTypeVar calls)
type_var_seen: std.AutoHashMap(types.Var, Monotype.Idx),

/// Cache for already-lowered symbol definitions (avoids re-lowering).
/// Key is @bitCast(MIR.Symbol) → u64.
lowered_symbols: std.AutoHashMap(u64, MIR.ExprId),

/// Tracks symbols currently being lowered (recursion guard).
in_progress_defs: std.AutoHashMap(u64, void),

/// Pre-built lookup for findModuleForOrigin: (module_idx, import_ident) → resolved module index.
/// Key is (module_idx << 32 | @bitCast(import_ident)), value is resolved module u32.
origin_lookup: std.AutoHashMap(u64, u32),

scratch_expr_ids: base.Scratch(MIR.ExprId),
scratch_pattern_ids: base.Scratch(MIR.PatternId),
scratch_ident_idxs: base.Scratch(Ident.Idx),
scratch_branches: base.Scratch(MIR.Branch),
scratch_branch_patterns: base.Scratch(MIR.BranchPattern),
scratch_stmts: base.Scratch(MIR.Stmt),
scratch_captures: base.Scratch(MIR.Capture),
mono_scratches: Monotype.Store.Scratches,

recursion_placeholders: std.AutoHashMapUnmanaged(u64, std.ArrayListUnmanaged(MIR.ExprId)) = .{},

// --- Init/Deinit ---

pub fn init(
    allocator: Allocator,
    store: *MIR.Store,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    builtin_indices: CIR.BuiltinIndices,
    current_module_idx: u32,
    app_module_idx: ?u32,
) Allocator.Error!Self {
    // Pre-build origin lookup for all modules' imports
    var origin_lookup = std.AutoHashMap(u64, u32).init(allocator);
    for (all_module_envs, 0..) |env, mod_idx| {
        const import_count: usize = @intCast(env.imports.imports.len());
        for (0..import_count) |i| {
            const import_idx: CIR.Import.Idx = @enumFromInt(i);
            if (env.imports.getIdentIdx(import_idx)) |import_ident| {
                if (env.imports.getResolvedModule(import_idx)) |resolved_mod| {
                    const key = (@as(u64, @intCast(mod_idx)) << 32) | @as(u64, @as(u32, @bitCast(import_ident)));
                    try origin_lookup.put(key, resolved_mod);
                }
            }
        }
    }

    return .{
        .allocator = allocator,
        .store = store,
        .all_module_envs = all_module_envs,
        .types_store = types_store,
        .builtin_indices = builtin_indices,
        .current_module_idx = current_module_idx,
        .app_module_idx = app_module_idx,
        .pattern_symbols = std.AutoHashMap(u64, MIR.Symbol).init(allocator),
        .type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .lowered_symbols = std.AutoHashMap(u64, MIR.ExprId).init(allocator),
        .in_progress_defs = std.AutoHashMap(u64, void).init(allocator),
        .origin_lookup = origin_lookup,
        .scratch_expr_ids = try base.Scratch(MIR.ExprId).init(allocator),
        .scratch_pattern_ids = try base.Scratch(MIR.PatternId).init(allocator),
        .scratch_ident_idxs = try base.Scratch(Ident.Idx).init(allocator),
        .scratch_branches = try base.Scratch(MIR.Branch).init(allocator),
        .scratch_branch_patterns = try base.Scratch(MIR.BranchPattern).init(allocator),
        .scratch_stmts = try base.Scratch(MIR.Stmt).init(allocator),
        .scratch_captures = try base.Scratch(MIR.Capture).init(allocator),
        .mono_scratches = try Monotype.Store.Scratches.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    if (builtin.mode == .Debug) {
        // Verify all recursion placeholders were patched to the correct monotype
        var ph_it = self.recursion_placeholders.iterator();
        while (ph_it.next()) |entry| {
            const symbol_key = entry.key_ptr.*;
            if (self.lowered_symbols.get(symbol_key)) |resolved_expr| {
                const resolved_monotype = self.store.typeOf(resolved_expr);
                for (entry.value_ptr.items) |expr_id| {
                    const placeholder_monotype = self.store.typeOf(expr_id);
                    if (placeholder_monotype != resolved_monotype) {
                        std.debug.panic(
                            "Recursion guard placeholder has wrong monotype: placeholder has {d} but resolved symbol has {d}",
                            .{ @intFromEnum(placeholder_monotype), @intFromEnum(resolved_monotype) },
                        );
                    }
                }
            }
        }
    }
    {
        var ph_it = self.recursion_placeholders.iterator();
        while (ph_it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.recursion_placeholders.deinit(self.allocator);
    }
    self.pattern_symbols.deinit();
    self.type_var_seen.deinit();
    self.lowered_symbols.deinit();
    self.in_progress_defs.deinit();
    self.origin_lookup.deinit();
    self.scratch_expr_ids.deinit();
    self.scratch_pattern_ids.deinit();
    self.scratch_ident_idxs.deinit();
    self.scratch_branches.deinit();
    self.scratch_branch_patterns.deinit();
    self.scratch_stmts.deinit();
    self.scratch_captures.deinit();
    self.mono_scratches.deinit();
}

// --- Public API ---

/// Lower a CIR expression to MIR.
pub fn lowerExpr(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ExprId {
    const module_env = self.all_module_envs[self.current_module_idx];
    const region = module_env.store.getExprRegion(expr_idx);

    // Error types from the type checker become runtime_error nodes.
    // This early return ensures resolveMonotype (below) is never called
    // on error types, which could otherwise fail or produce nonsense.
    const type_var = ModuleEnv.varFrom(expr_idx);
    const resolved = self.types_store.resolveVar(type_var);
    if (resolved.desc.content == .err) {
        return try self.store.addExpr(self.allocator, .{ .runtime_err_type = {} }, self.store.monotype_store.unit_idx, region);
    }

    const expr = module_env.store.getExpr(expr_idx);
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
        .e_typed_frac => |tf| {
            const roc_dec = builtins.dec.RocDec{ .num = tf.value.toI128() };
            if (tf.type_name == module_env.idents.f64) {
                return try self.store.addExpr(self.allocator, .{ .frac_f64 = roc_dec.toF64() }, monotype, region);
            } else if (tf.type_name == module_env.idents.f32) {
                return try self.store.addExpr(self.allocator, .{ .frac_f32 = @floatCast(roc_dec.toF64()) }, monotype, region);
            } else {
                // Dec or fallback
                return try self.store.addExpr(self.allocator, .{ .dec = roc_dec }, monotype, region);
            }
        },

        // --- Strings ---
        .e_str_segment => |seg| try self.store.addExpr(self.allocator, .{ .str = seg.literal }, monotype, region),
        .e_str => |str_expr| {
            const span = module_env.store.sliceExpr(str_expr.span);
            if (span.len == 0) {
                const empty_str_idx = try module_env.insertString("");
                return try self.store.addExpr(self.allocator, .{ .str = empty_str_idx }, monotype, region);
            }
            if (span.len == 1) {
                return try self.lowerExpr(span[0]);
            }
            // Multi-segment string: left fold with str_concat
            var acc = try self.lowerExpr(span[0]);
            for (span[1..]) |seg_idx| {
                const seg = try self.lowerExpr(seg_idx);
                const args = try self.store.addExprSpan(self.allocator, &.{ acc, seg });
                acc = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
                    .op = .str_concat,
                    .args = args,
                } }, monotype, region);
            }
            return acc;
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
            const symbol = try self.patternToSymbol(lookup.pattern_idx);
            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
        },
        .e_lookup_external => |ext| {
            // Import must be resolved before MIR lowering; reaching here
            // with an unresolved import means a compiler bug in an earlier phase.
            const target_module_idx: u32 = @intCast(module_env.imports.getResolvedModule(ext.module_idx) orelse
                unreachable);
            const symbol = MIR.Symbol{
                .module_idx = target_module_idx,
                .ident_idx = ext.ident_idx,
            };
            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
        },
        .e_lookup_pending => {
            // Must be resolved to e_lookup_external before MIR lowering;
            // reaching here means a compiler bug in an earlier phase.
            unreachable;
        },
        .e_lookup_required => |lookup| {
            const app_idx = self.app_module_idx orelse {
                return try self.store.addExpr(self.allocator, .runtime_err_type, monotype, region);
            };
            const required_type = module_env.requires_types.get(lookup.requires_idx);
            const required_name = module_env.getIdent(required_type.ident);

            // Find matching export in app module
            const app_env = self.all_module_envs[app_idx];
            const app_exports = app_env.store.sliceDefs(app_env.exports);
            for (app_exports) |def_idx| {
                const def = app_env.store.getDef(def_idx);
                const pat = app_env.store.getPattern(def.pattern);
                switch (pat) {
                    .assign => |assign| {
                        if (std.mem.eql(u8, app_env.getIdent(assign.ident), required_name)) {
                            const symbol = MIR.Symbol{ .module_idx = app_idx, .ident_idx = assign.ident };
                            _ = try self.lowerExternalDef(symbol, def.expr);
                            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
                        }
                    },
                    else => {},
                }
            }
            return try self.store.addExpr(self.allocator, .runtime_err_type, monotype, region);
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
        .e_type_var_dispatch => |tvd| {
            return try self.lowerTypeVarDispatch(module_env, tvd, monotype, region);
        },

        // --- For loop ---
        .e_for => |for_expr| {
            const list_expr = try self.lowerExpr(for_expr.expr);
            const pat = try self.lowerPattern(module_env, for_expr.patt);
            const body = try self.lowerExpr(for_expr.body);
            return try self.store.addExpr(self.allocator, .{ .for_loop = .{
                .list = list_expr,
                .elem_pattern = pat,
                .body = body,
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
        .e_run_low_level => |run_ll| {
            const args = try self.lowerExprSpan(module_env, run_ll.args);
            return try self.store.addExpr(self.allocator, .{ .run_low_level = .{
                .op = run_ll.op,
                .args = args,
            } }, monotype, region);
        },

        // --- Error/Debug ---
        .e_runtime_error => |re| try self.store.addExpr(self.allocator, .{ .runtime_err_can = .{ .diagnostic = re.diagnostic } }, monotype, region),
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
            return try self.store.addExpr(self.allocator, .{ .runtime_err_ellipsis = {} }, monotype, region);
        },
        .e_anno_only => {
            return try self.store.addExpr(self.allocator, .{ .runtime_err_anno_only = {} }, monotype, region);
        },
        .e_return => |ret| {
            const inner = try self.lowerExpr(ret.expr);
            return try self.store.addExpr(self.allocator, .{ .return_expr = .{
                .expr = inner,
            } }, monotype, region);
        },
    };
}

// --- Helpers ---

/// Resolve a CIR pattern to a global MIR symbol.
fn patternToSymbol(self: *Self, pattern_idx: CIR.Pattern.Idx) Allocator.Error!MIR.Symbol {
    const key = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(pattern_idx);

    if (self.pattern_symbols.get(key)) |existing| {
        return existing;
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);

    const ident_idx: Ident.Idx = switch (pattern) {
        .assign => |a| a.ident,
        .as => |a| a.ident,
        .applied_tag,
        .nominal,
        .nominal_external,
        .record_destructure,
        .list,
        .tuple,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => Ident.Idx.NONE,
    };

    const symbol = MIR.Symbol{
        .module_idx = self.current_module_idx,
        .ident_idx = ident_idx,
    };

    try self.pattern_symbols.put(key, symbol);
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
        &self.mono_scratches,
    );
}

/// Build a function monotype from argument types, return type, and effectfulness.
fn buildFuncMonotype(self: *Self, arg_monotypes: []const Monotype.Idx, ret: Monotype.Idx, effectful: bool) Allocator.Error!Monotype.Idx {
    const args_span = try self.store.monotype_store.addIdxSpan(self.allocator, arg_monotypes);
    return try self.store.monotype_store.addMonotype(self.allocator, .{ .func = .{
        .args = args_span,
        .ret = ret,
        .effectful = effectful,
    } });
}

/// Lower a CIR Expr.Span to an MIR ExprSpan.
fn lowerExprSpan(self: *Self, module_env: *const ModuleEnv, span: CIR.Expr.Span) Allocator.Error!MIR.ExprSpan {
    const cir_ids = module_env.store.sliceExpr(span);
    if (cir_ids.len == 0) return MIR.ExprSpan.empty();

    const scratch_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(scratch_top);

    for (cir_ids) |cir_id| {
        const mir_id = try self.lowerExpr(cir_id);
        try self.scratch_expr_ids.append(mir_id);
    }

    return try self.store.addExprSpan(self.allocator, self.scratch_expr_ids.sliceFromStart(scratch_top));
}

/// Lower a CIR Pattern.Span to an MIR PatternSpan.
fn lowerPatternSpan(self: *Self, module_env: *const ModuleEnv, span: CIR.Pattern.Span) Allocator.Error!MIR.PatternSpan {
    const cir_ids = module_env.store.slicePatterns(span);
    if (cir_ids.len == 0) return MIR.PatternSpan.empty();

    const scratch_top = self.scratch_pattern_ids.top();
    defer self.scratch_pattern_ids.clearFrom(scratch_top);

    for (cir_ids) |cir_id| {
        const mir_id = try self.lowerPattern(module_env, cir_id);
        try self.scratch_pattern_ids.append(mir_id);
    }

    return try self.store.addPatternSpan(self.allocator, self.scratch_pattern_ids.sliceFromStart(scratch_top));
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
        &self.mono_scratches,
    );

    return switch (pattern) {
        .assign => |a| {
            const symbol = MIR.Symbol{
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
            const symbol = MIR.Symbol{
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
            var rest_index: MIR.RestIndex = .none;
            var rest_pattern: MIR.PatternId = MIR.PatternId.none;
            if (list_pat.rest_info) |rest| {
                rest_index = @enumFromInt(rest.index);
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
            const pats_top = self.scratch_pattern_ids.top();
            defer self.scratch_pattern_ids.clearFrom(pats_top);
            const names_top = self.scratch_ident_idxs.top();
            defer self.scratch_ident_idxs.clearFrom(names_top);

            for (cir_destructs) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                try self.scratch_ident_idxs.append(destruct.label);
                const pat_idx = destruct.kind.toPatternIdx();
                const mir_pat = try self.lowerPattern(module_env, pat_idx);
                try self.scratch_pattern_ids.append(mir_pat);
            }

            const destructs_span = try self.store.addPatternSpan(self.allocator, self.scratch_pattern_ids.sliceFromStart(pats_top));
            const names_span = try self.store.addFieldNameSpan(self.allocator, self.scratch_ident_idxs.sliceFromStart(names_top));
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
        result = try self.createBoolMatch(module_env, cond, body, result, monotype, region);
    }

    return result;
}

/// Lower `e_match` to MIR match.
fn lowerMatch(self: *Self, module_env: *const ModuleEnv, match_expr: CIR.Expr.Match, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const cond = try self.lowerExpr(match_expr.cond);
    const cir_branch_indices = module_env.store.sliceMatchBranches(match_expr.branches);

    const branches_top = self.scratch_branches.top();
    defer self.scratch_branches.clearFrom(branches_top);

    for (cir_branch_indices) |branch_idx| {
        const cir_branch = module_env.store.getMatchBranch(branch_idx);
        const body = try self.lowerExpr(cir_branch.value);
        const guard = if (cir_branch.guard) |guard_idx|
            try self.lowerExpr(guard_idx)
        else
            MIR.ExprId.none;

        // Lower branch patterns
        const cir_bp_indices = module_env.store.sliceMatchBranchPatterns(cir_branch.patterns);
        const bp_top = self.scratch_branch_patterns.top();
        defer self.scratch_branch_patterns.clearFrom(bp_top);

        for (cir_bp_indices) |bp_idx| {
            const cir_bp = module_env.store.getMatchBranchPattern(bp_idx);
            const pat = try self.lowerPattern(module_env, cir_bp.pattern);
            try self.scratch_branch_patterns.append(.{ .pattern = pat, .degenerate = cir_bp.degenerate });
        }

        const bp_span = try self.store.addBranchPatterns(self.allocator, self.scratch_branch_patterns.sliceFromStart(bp_top));
        try self.scratch_branches.append(.{ .patterns = bp_span, .body = body, .guard = guard });
    }

    const branch_span = try self.store.addBranches(self.allocator, self.scratch_branches.sliceFromStart(branches_top));
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
    const captures_top = self.scratch_captures.top();
    defer self.scratch_captures.clearFrom(captures_top);

    for (cir_capture_indices) |cap_idx| {
        const cap = module_env.store.getCapture(cap_idx);
        const symbol = try self.patternToSymbol(cap.pattern_idx);
        try self.scratch_captures.append(.{ .symbol = symbol });
    }

    const capture_span = try self.store.addCaptures(self.allocator, self.scratch_captures.sliceFromStart(captures_top));

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
    const stmts_top = self.scratch_stmts.top();
    defer self.scratch_stmts.clearFrom(stmts_top);

    for (cir_stmt_indices) |stmt_idx| {
        const cir_stmt = module_env.store.getStatement(stmt_idx);
        switch (cir_stmt) {
            .s_decl => |decl| {
                const pat = try self.lowerPattern(module_env, decl.pattern);
                const expr = try self.lowerExpr(decl.expr);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = pat, .expr = expr } });
            },
            .s_var => |var_decl| {
                const pat = try self.lowerPattern(module_env, var_decl.pattern_idx);
                const expr = try self.lowerExpr(var_decl.expr);
                try self.scratch_stmts.append(.{ .decl_var = .{ .pattern = pat, .expr = expr } });
            },
            .s_reassign => |reassign| {
                const pat = try self.lowerPattern(module_env, reassign.pattern_idx);
                const expr = try self.lowerExpr(reassign.expr);
                try self.scratch_stmts.append(.{ .mutate_var = .{ .pattern = pat, .expr = expr } });
            },
            .s_expr => |s_expr| {
                // Expression statement: bind to wildcard
                const expr = try self.lowerExpr(s_expr.expr);
                const expr_type = self.store.typeOf(expr);
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, expr_type);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_dbg => |s_dbg| {
                const expr = try self.lowerExpr(s_dbg.expr);
                const expr_type = self.store.typeOf(expr);
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, expr_type);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_expect => |s_expect| {
                const expr = try self.lowerExpr(s_expect.body);
                const expr_type = self.store.typeOf(expr);
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, expr_type);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_crash => |s_crash| {
                const unit_monotype = self.store.monotype_store.unit_idx;
                const expr = try self.store.addExpr(self.allocator, .{ .crash = s_crash.msg }, unit_monotype, Region.zero());
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_for => |s_for| {
                const list_expr = try self.lowerExpr(s_for.expr);
                const pat = try self.lowerPattern(module_env, s_for.patt);
                const body = try self.lowerExpr(s_for.body);
                const unit_monotype = self.store.monotype_store.unit_idx;
                const expr = try self.store.addExpr(self.allocator, .{ .for_loop = .{
                    .list = list_expr,
                    .elem_pattern = pat,
                    .body = body,
                } }, unit_monotype, Region.zero());
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_while => |s_while| {
                const cond = try self.lowerExpr(s_while.cond);
                const body = try self.lowerExpr(s_while.body);
                const unit_monotype = self.store.monotype_store.unit_idx;
                const expr = try self.store.addExpr(self.allocator, .{ .while_loop = .{
                    .cond = cond,
                    .body = body,
                } }, unit_monotype, Region.zero());
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_break => {
                const unit_monotype = self.store.monotype_store.unit_idx;
                const expr = try self.store.addExpr(self.allocator, .{ .break_expr = {} }, unit_monotype, Region.zero());
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_return => |s_return| {
                const inner = try self.lowerExpr(s_return.expr);
                const unit_monotype = self.store.monotype_store.unit_idx;
                const expr = try self.store.addExpr(self.allocator, .{ .return_expr = .{
                    .expr = inner,
                } }, unit_monotype, Region.zero());
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_runtime_error => |s_re| {
                const unit_monotype = self.store.monotype_store.unit_idx;
                const expr = try self.store.addExpr(self.allocator, .{ .runtime_err_can = .{
                    .diagnostic = s_re.diagnostic,
                } }, unit_monotype, Region.zero());
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            // Compile-time declarations — no runtime behavior
            .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno, .s_type_var_alias => {},
        }
    }

    const stmt_span = try self.store.addStmts(self.allocator, self.scratch_stmts.sliceFromStart(stmts_top));
    const final_expr = try self.lowerExpr(block.final_expr);

    return try self.store.addExpr(self.allocator, .{ .block = .{
        .stmts = stmt_span,
        .final_expr = final_expr,
    } }, monotype, region);
}

/// Lower `e_binop` to either a method call or a match (for short-circuit and/or).
fn lowerBinop(self: *Self, binop: CIR.Expr.Binop, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const module_env = self.all_module_envs[self.current_module_idx];

    switch (binop.op) {
        // Short-circuit `and`: desugar to match on Bool
        // `a and b` → `match a { True => b, _ => False }`
        .@"and" => {
            const cond = try self.lowerExpr(binop.lhs);
            const body_true = try self.lowerExpr(binop.rhs);
            const bool_monotype = self.store.monotype_store.primIdx(.bool);
            const false_expr = try self.store.addExpr(self.allocator, .{ .tag = .{
                .name = module_env.idents.false_tag,
                .args = MIR.ExprSpan.empty(),
            } }, bool_monotype, region);
            return try self.createBoolMatch(module_env, cond, body_true, false_expr, monotype, region);
        },
        // Short-circuit `or`: desugar to match on Bool
        // `a or b` → `match a { True => True, _ => b }`
        .@"or" => {
            const cond = try self.lowerExpr(binop.lhs);
            const body_else = try self.lowerExpr(binop.rhs);
            const bool_monotype = self.store.monotype_store.primIdx(.bool);
            const true_expr = try self.store.addExpr(self.allocator, .{ .tag = .{
                .name = module_env.idents.true_tag,
                .args = MIR.ExprSpan.empty(),
            } }, bool_monotype, region);
            return try self.createBoolMatch(module_env, cond, true_expr, body_else, monotype, region);
        },
        // All other operators desugar to method calls
        .add, .sub, .mul, .div, .div_trunc, .rem, .lt, .le, .gt, .ge, .eq, .ne => {
            const method_ident: Ident.Idx = switch (binop.op) {
                .add => module_env.idents.plus,
                .sub => module_env.idents.minus,
                .mul => module_env.idents.times,
                .div => module_env.idents.div_by,
                .div_trunc => module_env.idents.div_trunc_by,
                .rem => module_env.idents.rem_by,
                .lt => module_env.idents.is_lt,
                .le => module_env.idents.is_lte,
                .gt => module_env.idents.is_gt,
                .ge => module_env.idents.is_gte,
                .eq, .ne => module_env.idents.is_eq,
                .@"and", .@"or" => unreachable,
            };

            const lhs = try self.lowerExpr(binop.lhs);
            const rhs = try self.lowerExpr(binop.rhs);

            // Resolve the method via type-directed dispatch on the LHS operand
            const lhs_type_var = ModuleEnv.varFrom(binop.lhs);
            const method_symbol = try self.resolveMethodForTypeVar(
                module_env,
                lhs_type_var,
                method_ident,
            ) orelse {
                // No nominal method found — emit run_low_level directly.
                //
                // Two cases reach here:
                // 1. Flex/rigid type vars (e.g. unresolved numerals like `1 + 2`) —
                //    all arithmetic/comparison ops are valid, defaulting to Dec.
                // 2. Structural types (records, tuples, tag unions) — only eq/ne are
                //    valid (the type checker rejects arithmetic/ordering on these).
                //    num_is_eq maps to LIR .eq, which the backend handles via
                //    layout-based structural comparison.
                //
                // All concrete primitive types (Bool, Str, U8-U128, I8-I128, F32,
                // F64, Dec) are nominal and resolve via method dispatch above.
                const ll_op: CIR.Expr.LowLevel = switch (binop.op) {
                    .eq, .ne => .num_is_eq,
                    .lt => .num_is_lt,
                    .le => .num_is_lte,
                    .gt => .num_is_gt,
                    .ge => .num_is_gte,
                    .add => .num_plus,
                    .sub => .num_minus,
                    .mul => .num_times,
                    .div => .num_div_by,
                    .div_trunc => .num_div_trunc_by,
                    .rem => .num_rem_by,
                    .@"and", .@"or" => unreachable,
                };
                const ll_args = try self.store.addExprSpan(self.allocator, &.{ lhs, rhs });
                const result = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
                    .op = ll_op,
                    .args = ll_args,
                } }, monotype, region);
                if (binop.op == .ne) return try self.negBool(module_env, result, monotype, region);
                return result;
            };

            // Ensure the method body is lowered so codegen can find it
            if (method_symbol.module_idx != self.current_module_idx) {
                try self.ensureMethodLowered(method_symbol);
            }

            const lhs_monotype = try self.resolveMonotype(binop.lhs);
            const rhs_monotype = try self.resolveMonotype(binop.rhs);
            const func_monotype = try self.buildFuncMonotype(&.{ lhs_monotype, rhs_monotype }, monotype, false);
            const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = method_symbol }, func_monotype, region);

            const args = try self.store.addExprSpan(self.allocator, &.{ lhs, rhs });
            const result = try self.store.addExpr(self.allocator, .{ .call = .{
                .func = func_expr,
                .args = args,
            } }, monotype, region);

            // For != (ne), wrap result in Bool.not
            if (binop.op == .ne) {
                return try self.negBool(module_env, result, monotype, region);
            }

            return result;
        },
    }
}

/// Lower `e_unary_minus` to a call to `negate` (type-directed dispatch).
fn lowerUnaryMinus(self: *Self, um: CIR.Expr.UnaryMinus, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const module_env = self.all_module_envs[self.current_module_idx];
    const inner = try self.lowerExpr(um.expr);

    // Resolve the method via type-directed dispatch on the operand's type var
    const type_var = ModuleEnv.varFrom(um.expr);
    const method_symbol = try self.resolveMethodForTypeVar(
        module_env,
        type_var,
        module_env.idents.negate,
    ) orelse {
        // No nominal method found — emit run_low_level directly.
        // This happens for flex/rigid type vars (e.g. unresolved numerals like `-1`)
        // and for .err types (where the type checker already reported the error).
        //
        // Nominal and structural types must not reach here: the type checker
        // validates all dispatch constraints before lowering, replacing failures
        // with .err.
        if (std.debug.runtime_safety) {
            var resolved = self.types_store.resolveVar(type_var);
            while (resolved.desc.content == .alias) {
                const alias = resolved.desc.content.alias;
                const backing = self.types_store.getAliasBackingVar(alias);
                resolved = self.types_store.resolveVar(backing);
            }
            switch (resolved.desc.content) {
                .flex, .rigid, .err => {}, // expected fallback cases
                .structure => |s| switch (s) {
                    .nominal_type => std.debug.panic(
                        "lowerUnaryMinus: nominal type reached fallback — type checker should have validated this",
                        .{},
                    ),
                    else => std.debug.panic(
                        "lowerUnaryMinus: structural type reached fallback — type checker should have rejected this",
                        .{},
                    ),
                },
                .alias => unreachable, // already followed aliases above
            }
        }
        const ll_args = try self.store.addExprSpan(self.allocator, &.{inner});
        return try self.store.addExpr(self.allocator, .{ .run_low_level = .{
            .op = .num_negate,
            .args = ll_args,
        } }, monotype, region);
    };

    // Ensure the method body is lowered so codegen can find it
    if (method_symbol.module_idx != self.current_module_idx) {
        try self.ensureMethodLowered(method_symbol);
    }

    const inner_monotype = try self.resolveMonotype(um.expr);
    const func_monotype = try self.buildFuncMonotype(&.{inner_monotype}, monotype, false);
    const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = method_symbol }, func_monotype, region);
    const args = try self.store.addExprSpan(self.allocator, &.{inner});
    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = func_expr,
        .args = args,
    } }, monotype, region);
}

/// Lower `e_unary_not` to match on Bool: `not x` → `match x { True => False, _ => True }`
fn lowerUnaryNot(self: *Self, un: CIR.Expr.UnaryNot, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const inner = try self.lowerExpr(un.expr);
    const module_env = self.all_module_envs[self.current_module_idx];
    return try self.negBool(module_env, inner, monotype, region);
}

/// Create `match cond { True => true_body, _ => false_body }`.
/// Used by if-desugaring, and/or short-circuit, and Bool negation.
fn createBoolMatch(
    self: *Self,
    module_env: *const ModuleEnv,
    cond: MIR.ExprId,
    true_body: MIR.ExprId,
    false_body: MIR.ExprId,
    monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const bool_monotype = self.store.monotype_store.primIdx(.bool);

    const true_pattern = try self.store.addPattern(self.allocator, .{ .tag = .{
        .name = module_env.idents.true_tag,
        .args = MIR.PatternSpan.empty(),
    } }, bool_monotype);
    const wildcard_pattern = try self.store.addPattern(self.allocator, .wildcard, bool_monotype);

    const true_bp = try self.store.addBranchPatterns(self.allocator, &.{.{ .pattern = true_pattern, .degenerate = false }});
    const else_bp = try self.store.addBranchPatterns(self.allocator, &.{.{ .pattern = wildcard_pattern, .degenerate = false }});

    const branch_span = try self.store.addBranches(self.allocator, &.{
        .{ .patterns = true_bp, .body = true_body, .guard = MIR.ExprId.none },
        .{ .patterns = else_bp, .body = false_body, .guard = MIR.ExprId.none },
    });

    return try self.store.addExpr(self.allocator, .{ .match_expr = .{
        .cond = cond,
        .branches = branch_span,
    } }, monotype, region);
}

/// Negate a Bool: `match expr { True => False, _ => True }`
fn negBool(self: *Self, module_env: *const ModuleEnv, expr: MIR.ExprId, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const bool_monotype = self.store.monotype_store.primIdx(.bool);
    const false_expr = try self.store.addExpr(self.allocator, .{ .tag = .{
        .name = module_env.idents.false_tag,
        .args = MIR.ExprSpan.empty(),
    } }, bool_monotype, region);
    const true_expr = try self.store.addExpr(self.allocator, .{ .tag = .{
        .name = module_env.idents.true_tag,
        .args = MIR.ExprSpan.empty(),
    } }, bool_monotype, region);
    return try self.createBoolMatch(module_env, expr, false_expr, true_expr, monotype, region);
}

/// Lower `e_dot_access` — field access or method call.
fn lowerDotAccess(self: *Self, module_env: *const ModuleEnv, da: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const receiver = try self.lowerExpr(da.receiver);

    if (da.args) |args_span| {
        // Method call: resolve via the receiver's type
        const receiver_type_var = ModuleEnv.varFrom(da.receiver);
        const method_symbol = try self.resolveMethodForTypeVar(
            module_env,
            receiver_type_var,
            da.field_name,
        ) orelse MIR.Symbol{
            .module_idx = self.current_module_idx,
            .ident_idx = da.field_name,
        };

        // Ensure the method body is lowered so codegen can find it
        if (method_symbol.module_idx != self.current_module_idx) {
            try self.ensureMethodLowered(method_symbol);
        }

        // Build args as [receiver] ++ explicit_args
        // e.g. list.map(fn) → List.map(list, fn)
        const explicit_args = module_env.store.sliceExpr(args_span);

        const mono_top = self.mono_scratches.idxs.top();
        defer self.mono_scratches.idxs.clearFrom(mono_top);
        const receiver_monotype = try self.resolveMonotype(da.receiver);
        try self.mono_scratches.idxs.append(receiver_monotype);
        for (explicit_args) |arg_idx| {
            try self.mono_scratches.idxs.append(try self.resolveMonotype(arg_idx));
        }
        const func_monotype = try self.buildFuncMonotype(self.mono_scratches.idxs.sliceFromStart(mono_top), monotype, false);

        const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = method_symbol }, func_monotype, region);

        const args_top = self.scratch_expr_ids.top();
        defer self.scratch_expr_ids.clearFrom(args_top);
        try self.scratch_expr_ids.append(receiver);
        for (explicit_args) |arg_idx| {
            const arg = try self.lowerExpr(arg_idx);
            try self.scratch_expr_ids.append(arg);
        }
        const args = try self.store.addExprSpan(self.allocator, self.scratch_expr_ids.sliceFromStart(args_top));

        return try self.store.addExpr(self.allocator, .{ .call = .{
            .func = func_expr,
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

    const exprs_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(exprs_top);
    const names_top = self.scratch_ident_idxs.top();
    defer self.scratch_ident_idxs.clearFrom(names_top);

    for (cir_field_indices) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        const expr = try self.lowerExpr(field.value);
        try self.scratch_expr_ids.append(expr);
        try self.scratch_ident_idxs.append(field.name);
    }

    const fields_span = try self.store.addExprSpan(self.allocator, self.scratch_expr_ids.sliceFromStart(exprs_top));
    const names_span = try self.store.addFieldNameSpan(self.allocator, self.scratch_ident_idxs.sliceFromStart(names_top));

    return try self.store.addExpr(self.allocator, .{ .record = .{
        .fields = fields_span,
        .field_names = names_span,
    } }, monotype, region);
}

// --- Type var dispatch & cross-module resolution ---

/// Lower `e_type_var_dispatch` by resolving the type alias and dispatching to the method.
fn lowerTypeVarDispatch(self: *Self, module_env: *const ModuleEnv, tvd: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    // Get the type variable from the alias statement
    const stmt = module_env.store.getStatement(tvd.type_var_alias_stmt);
    const type_var_binding = stmt.s_type_var_alias;
    const type_var = ModuleEnv.varFrom(type_var_binding.type_var_anno);

    // Resolve the method via the shared helper
    const method_symbol = try self.resolveMethodForTypeVar(
        module_env,
        type_var,
        tvd.method_name,
    ) orelse MIR.Symbol{
        .module_idx = self.current_module_idx,
        .ident_idx = tvd.method_name,
    };

    // Ensure the method body is lowered so codegen can find it
    if (method_symbol.module_idx != self.current_module_idx) {
        try self.ensureMethodLowered(method_symbol);
    }

    const cir_args = module_env.store.sliceExpr(tvd.args);
    const mono_top = self.mono_scratches.idxs.top();
    defer self.mono_scratches.idxs.clearFrom(mono_top);
    for (cir_args) |arg_idx| {
        try self.mono_scratches.idxs.append(try self.resolveMonotype(arg_idx));
    }
    const func_monotype = try self.buildFuncMonotype(self.mono_scratches.idxs.sliceFromStart(mono_top), monotype, false);

    const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = method_symbol }, func_monotype, region);
    const args = try self.lowerExprSpan(module_env, tvd.args);

    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = func_expr,
        .args = args,
    } }, monotype, region);
}

/// Find the module index for a given origin module ident.
fn findModuleForOrigin(self: *Self, source_env: *const ModuleEnv, origin_module: Ident.Idx) u32 {
    // Check if origin is source_env itself
    if (origin_module == source_env.qualified_module_ident) {
        return self.current_module_idx;
    }

    // O(1) lookup in pre-built HashMap
    const key = (@as(u64, self.current_module_idx) << 32) | @as(u64, @as(u32, @bitCast(origin_module)));
    return self.origin_lookup.get(key) orelse {
        std.debug.panic(
            "findModuleForOrigin: origin module not found (current_module_idx={d}, origin_ident={d})",
            .{ self.current_module_idx, @as(u32, @bitCast(origin_module)) },
        );
    };
}

/// Resolve a type variable to a method symbol via nominal type dispatch.
/// Returns null if the type variable doesn't resolve to a nominal type or if the method can't be found.
fn resolveMethodForTypeVar(
    self: *Self,
    source_env: *const ModuleEnv,
    type_var: types.Var,
    method_name: Ident.Idx,
) Allocator.Error!?MIR.Symbol {
    var resolved = self.types_store.resolveVar(type_var);

    // Follow aliases to get to the underlying type
    while (resolved.desc.content == .alias) {
        const alias = resolved.desc.content.alias;
        const backing = self.types_store.getAliasBackingVar(alias);
        resolved = self.types_store.resolveVar(backing);
    }

    // Check if it's a nominal type
    const nominal_info: ?struct { origin: Ident.Idx, ident: Ident.Idx } = switch (resolved.desc.content) {
        .structure => |s| switch (s) {
            .nominal_type => |nom| .{
                .origin = nom.origin_module,
                .ident = nom.ident.ident_idx,
            },
            .record,
            .record_unbound,
            .tuple,
            .fn_pure,
            .fn_effectful,
            .fn_unbound,
            .empty_record,
            .tag_union,
            .empty_tag_union,
            => null,
        },
        .flex, .rigid, .alias, .err => null,
    };

    const info = nominal_info orelse return null;

    // Find the origin module
    const origin_module_idx = self.findModuleForOrigin(source_env, info.origin);

    const origin_env = self.all_module_envs[origin_module_idx];

    // Look up the method in the origin module
    const qualified_method = origin_env.lookupMethodIdentFromTwoEnvsConst(
        source_env,
        info.ident,
        source_env,
        method_name,
    ) orelse return null;

    return MIR.Symbol{
        .module_idx = @intCast(origin_module_idx),
        .ident_idx = qualified_method,
    };
}

/// Ensure a method definition is lowered (for cross-module binop dispatch).
fn ensureMethodLowered(self: *Self, symbol: MIR.Symbol) Allocator.Error!void {
    const symbol_key: u64 = @bitCast(symbol);
    if (self.lowered_symbols.contains(symbol_key)) return;

    const target_env = self.all_module_envs[symbol.module_idx];
    const defs = target_env.store.sliceDefs(target_env.all_defs);
    for (defs) |def_idx| {
        const def = target_env.store.getDef(def_idx);
        const pat = target_env.store.getPattern(def.pattern);
        switch (pat) {
            .assign => |assign| {
                if (@as(u32, @bitCast(assign.ident)) == @as(u32, @bitCast(symbol.ident_idx))) {
                    _ = try self.lowerExternalDef(symbol, def.expr);
                    return;
                }
            },
            else => {},
        }
    }
    unreachable; // Method must exist — type checking should have caught this
}

/// Lower an external definition by symbol, caching the result.
pub fn lowerExternalDef(self: *Self, symbol: MIR.Symbol, cir_expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ExprId {
    const symbol_key: u64 = @bitCast(symbol);

    // Check cache
    if (self.lowered_symbols.get(symbol_key)) |cached| {
        return cached;
    }

    // Recursion guard
    if (self.in_progress_defs.contains(symbol_key)) {
        // Recursive reference — return a placeholder lookup with unit type.
        // The correct monotype is patched below after lowerExpr completes.
        const placeholder = try self.store.addExpr(self.allocator, .{ .lookup = symbol }, self.store.monotype_store.unit_idx, Region.zero());
        const gop = try self.recursion_placeholders.getOrPut(self.allocator, symbol_key);
        if (!gop.found_existing) {
            gop.value_ptr.* = .{};
        }
        try gop.value_ptr.append(self.allocator, placeholder);
        return placeholder;
    }

    try self.in_progress_defs.put(symbol_key, {});

    // When crossing module boundaries, switch types_store and isolate the
    // type_var_seen cache — the same numeric type variable means different
    // things in different modules' type stores.
    const switching_module = symbol.module_idx != self.current_module_idx;
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_type_var_seen = self.type_var_seen;
    if (switching_module) {
        self.current_module_idx = symbol.module_idx;
        self.types_store = &self.all_module_envs[symbol.module_idx].types;
        self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    }
    defer if (switching_module) {
        self.type_var_seen.deinit();
        self.type_var_seen = saved_type_var_seen;
        self.types_store = saved_types_store;
        self.current_module_idx = saved_module_idx;
    };

    const result = try self.lowerExpr(cir_expr_idx);

    // Cache the result and register the symbol definition
    try self.lowered_symbols.put(symbol_key, result);
    try self.store.registerSymbolDef(self.allocator, symbol, result);

    // Patch any recursion placeholders for this symbol with the correct monotype.
    // During lowerExpr, recursive references create placeholder lookups with unit type;
    // now that the real definition is resolved, update them to the actual type.
    const resolved_monotype = self.store.typeOf(result);
    if (self.recursion_placeholders.getPtr(symbol_key)) |expr_list| {
        for (expr_list.items) |expr_id| {
            self.store.type_map.items[@intFromEnum(expr_id)] = resolved_monotype;
        }
        expr_list.deinit(self.allocator);
        _ = self.recursion_placeholders.remove(symbol_key);
    }

    _ = self.in_progress_defs.remove(symbol_key);

    return result;
}

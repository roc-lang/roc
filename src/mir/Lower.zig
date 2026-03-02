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
//! - All lookups unified to opaque global `Symbol`

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
const StringLiteral = base.StringLiteral;
const Allocator = std.mem.Allocator;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const Self = @This();

const DeferredBlockLambda = struct {
    cir_expr: CIR.Expr.Idx,
    scratch_stmt_idx: u32,
    module_idx: u32,
};

const ResolvedDispatchTarget = struct {
    origin: Ident.Idx,
    method_ident: Ident.Idx,
    fn_var: types.Var,
};

const SymbolMetadata = struct {
    module_idx: u32,
    ident_idx: Ident.Idx,
};

// --- Fields ---

allocator: Allocator,

/// Target MIR store
store: *MIR.Store,

/// All module environments (indexed by module_idx)
all_module_envs: []const *ModuleEnv,

/// Types store for resolving type variables
types_store: *const types.Store,

/// Current module being lowered
current_module_idx: u32,

/// Scope key used to make local pattern symbols specialization-specific.
/// 0 means unscoped (module/pattern only).
current_pattern_scope: u64,

/// App module index (for resolving `e_lookup_required` from platform modules)
app_module_idx: ?u32,

/// Map from ((scope_key << 64) | (module_idx << 32 | CIR.Pattern.Idx)) → MIR.Symbol
/// Used to resolve CIR local lookups to global symbols.
pattern_symbols: std.AutoHashMap(u128, MIR.Symbol),

/// Cache for type var → monotype conversion (shared across all fromTypeVar calls)
type_var_seen: std.AutoHashMap(types.Var, Monotype.Idx),

/// Cache for already-lowered symbol definitions (avoids re-lowering).
/// Key is @bitCast(MIR.Symbol) → u64.
lowered_symbols: std.AutoHashMap(u64, MIR.ExprId),

/// Polymorphic specialization cache: maps (symbol_key, monotype) → specialized MIR.Symbol.
/// When a polymorphic function is called with a different type than what was first lowered,
/// a new synthetic symbol is created, lowered with the new type, and cached here.
/// Key is (symbol_key << 32 | @intFromEnum(monotype)) as u128.
poly_specializations: std.AutoHashMap(u128, MIR.Symbol),

/// Metadata for opaque symbol IDs; populated at symbol construction time.
symbol_metadata: std.AutoHashMap(u64, SymbolMetadata),

/// Counter for generating synthetic ident indices for polymorphic specializations.
/// Counts down from NONE - 1 to avoid collision with real idents.
next_synthetic_ident: u29,

/// Tracks symbols currently being lowered (recursion guard).
in_progress_defs: std.AutoHashMap(u64, void),

/// Monotype currently being lowered for each in-progress symbol.
/// Used to detect in-progress calls that need a distinct specialization symbol.
in_progress_symbol_monotypes: std.AutoHashMap(u64, Monotype.Idx),

/// Block-local polymorphic lambda defs waiting for call-site type information.
/// Key is CIR Pattern.Idx (as u32). Value holds the CIR expression to lower and
/// the index into `scratch_stmts` where the decl_const placeholder was emitted.
/// Populated by `lowerBlock`, consumed by `e_lookup_local`.
deferred_block_lambdas: std.AutoHashMap(u32, DeferredBlockLambda),

/// Pre-built lookup for findModuleForOrigin: (module_idx, import_ident) → resolved module index.
/// Key is (module_idx << 32 | @bitCast(import_ident)), value is resolved module u32.
origin_lookup: std.AutoHashMap(u64, u32),

/// Pre-resolved static dispatch targets keyed by (module_idx, expr_idx).
/// Filled from type-checker constraints so MIR lowering uses authoritative
/// dispatch resolution data directly.
resolved_dispatch_targets: std.AutoHashMap(u64, ResolvedDispatchTarget),

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

    // Pre-build resolved static dispatch targets for all modules.
    var resolved_dispatch_targets = std.AutoHashMap(u64, ResolvedDispatchTarget).init(allocator);
    for (all_module_envs, 0..) |env, mod_idx| {
        const constraints = env.types.sliceAllStaticDispatchConstraints();
        for (constraints) |constraint| {
            if (constraint.source_expr_idx == types.StaticDispatchConstraint.no_source_expr) continue;
            if (constraint.resolved_target.isNone()) continue;

            const key = (@as(u64, @intCast(mod_idx)) << 32) | @as(u64, constraint.source_expr_idx);
            try resolved_dispatch_targets.put(key, .{
                .origin = constraint.resolved_target.origin_module,
                .method_ident = constraint.resolved_target.method_ident,
                .fn_var = constraint.fn_var,
            });
        }
    }

    return .{
        .allocator = allocator,
        .store = store,
        .all_module_envs = all_module_envs,
        .types_store = types_store,
        .current_module_idx = current_module_idx,
        .current_pattern_scope = 0,
        .app_module_idx = app_module_idx,
        .pattern_symbols = std.AutoHashMap(u128, MIR.Symbol).init(allocator),
        .type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .lowered_symbols = std.AutoHashMap(u64, MIR.ExprId).init(allocator),
        .poly_specializations = std.AutoHashMap(u128, MIR.Symbol).init(allocator),
        .symbol_metadata = std.AutoHashMap(u64, SymbolMetadata).init(allocator),
        .next_synthetic_ident = Ident.Idx.NONE.idx - 1,
        .in_progress_defs = std.AutoHashMap(u64, void).init(allocator),
        .in_progress_symbol_monotypes = std.AutoHashMap(u64, Monotype.Idx).init(allocator),
        .deferred_block_lambdas = std.AutoHashMap(u32, DeferredBlockLambda).init(allocator),
        .origin_lookup = origin_lookup,
        .resolved_dispatch_targets = resolved_dispatch_targets,
        .scratch_expr_ids = try base.Scratch(MIR.ExprId).init(allocator),
        .scratch_pattern_ids = try base.Scratch(MIR.PatternId).init(allocator),
        .scratch_ident_idxs = try base.Scratch(Ident.Idx).init(allocator),
        .scratch_branches = try base.Scratch(MIR.Branch).init(allocator),
        .scratch_branch_patterns = try base.Scratch(MIR.BranchPattern).init(allocator),
        .scratch_stmts = try base.Scratch(MIR.Stmt).init(allocator),
        .scratch_captures = try base.Scratch(MIR.Capture).init(allocator),
        .mono_scratches = blk: {
            var ms = try Monotype.Store.Scratches.init(allocator);
            ms.ident_store = all_module_envs[current_module_idx].getIdentStoreConst();
            break :blk ms;
        },
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
    self.poly_specializations.deinit();
    self.symbol_metadata.deinit();
    self.in_progress_defs.deinit();
    self.in_progress_symbol_monotypes.deinit();
    self.deferred_block_lambdas.deinit();
    self.origin_lookup.deinit();
    self.resolved_dispatch_targets.deinit();
    self.scratch_expr_ids.deinit();
    self.scratch_pattern_ids.deinit();
    self.scratch_ident_idxs.deinit();
    self.scratch_branches.deinit();
    self.scratch_branch_patterns.deinit();
    self.scratch_stmts.deinit();
    self.scratch_captures.deinit();
    self.mono_scratches.deinit();
}

fn packSymbolId(namespace_idx: u32, ident_idx: Ident.Idx) u64 {
    const ident_bits: u32 = @bitCast(ident_idx);
    return (@as(u64, namespace_idx) << 32) | @as(u64, ident_bits);
}

fn internSymbol(self: *Self, namespace_idx: u32, ident_idx: Ident.Idx) Allocator.Error!MIR.Symbol {
    const raw = packSymbolId(namespace_idx, ident_idx);
    const gop = try self.symbol_metadata.getOrPut(raw);
    if (!gop.found_existing) {
        gop.value_ptr.* = .{ .module_idx = namespace_idx, .ident_idx = ident_idx };
    } else if (builtin.mode == .Debug) {
        const existing = gop.value_ptr.*;
        if (existing.module_idx != namespace_idx or !existing.ident_idx.eql(ident_idx)) {
            std.debug.panic(
                "Symbol metadata mismatch for raw id {d}: existing module={d} ident={d}, new module={d} ident={d}",
                .{ raw, existing.module_idx, existing.ident_idx.idx, namespace_idx, ident_idx.idx },
            );
        }
    }
    const symbol = MIR.Symbol.fromRaw(raw);
    try self.store.registerSymbolReassignable(self.allocator, symbol, ident_idx.attributes.reassignable);
    return symbol;
}

fn getSymbolMetadata(self: *const Self, symbol: MIR.Symbol) SymbolMetadata {
    const key = symbol.raw();
    return self.symbol_metadata.get(key) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("Missing symbol metadata for symbol key {d}", .{key});
        }
        unreachable;
    };
}

/// Copy a CIR string literal into MIR's own string store.
/// This ensures MIR is self-contained and downstream passes (LIR, codegen)
/// never need to reach back into CIR module envs for string data.
fn copyStringToMir(self: *Self, module_env: *const ModuleEnv, cir_str_idx: StringLiteral.Idx) Allocator.Error!StringLiteral.Idx {
    if (cir_str_idx == .none) return .none;
    const str_bytes = module_env.getString(cir_str_idx);
    return self.store.strings.insert(self.allocator, str_bytes);
}

fn emitMirStrLiteral(self: *Self, text: []const u8, region: Region) Allocator.Error!MIR.ExprId {
    const str_idx = try self.store.strings.insert(self.allocator, text);
    return try self.store.addExpr(
        self.allocator,
        .{ .str = str_idx },
        self.store.monotype_store.primIdx(.str),
        region,
    );
}

fn emitMirStrConcat(self: *Self, left: MIR.ExprId, right: MIR.ExprId, region: Region) Allocator.Error!MIR.ExprId {
    const args = try self.store.addExprSpan(self.allocator, &.{ left, right });
    return try self.store.addExpr(
        self.allocator,
        .{ .run_low_level = .{ .op = .str_concat, .args = args } },
        self.store.monotype_store.primIdx(.str),
        region,
    );
}

fn foldMirStrConcat(self: *Self, parts: []const MIR.ExprId, region: Region) Allocator.Error!MIR.ExprId {
    std.debug.assert(parts.len > 0);
    var acc = parts[0];
    for (parts[1..]) |part| {
        acc = try self.emitMirStrConcat(acc, part, region);
    }
    return acc;
}

fn emitMirLookup(self: *Self, symbol: MIR.Symbol, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
}

fn emitMirUnitExpr(self: *Self, region: Region) Allocator.Error!MIR.ExprId {
    return try self.store.addExpr(
        self.allocator,
        .{ .record = .{
            .fields = MIR.ExprSpan.empty(),
            .field_names = MIR.FieldNameSpan.empty(),
        } },
        self.store.monotype_store.unit_idx,
        region,
    );
}

fn emitMirBoolLiteral(self: *Self, module_env: *const ModuleEnv, value: bool, region: Region) Allocator.Error!MIR.ExprId {
    return try self.store.addExpr(
        self.allocator,
        .{ .tag = .{
            .name = if (value) module_env.idents.true_tag else module_env.idents.false_tag,
            .args = MIR.ExprSpan.empty(),
        } },
        self.store.monotype_store.primIdx(.bool),
        region,
    );
}

fn makeSyntheticBind(
    self: *Self,
    monotype: Monotype.Idx,
    reassignable: bool,
) Allocator.Error!struct { symbol: MIR.Symbol, pattern: MIR.PatternId } {
    const template: Ident.Idx = .{
        .attributes = .{
            .effectful = false,
            .ignored = false,
            .reassignable = reassignable,
        },
        .idx = 0,
    };
    const sym_ident = self.makeSyntheticIdent(template);
    const symbol = try self.internSymbol(self.current_module_idx, sym_ident);
    const pattern = try self.store.addPattern(self.allocator, .{ .bind = symbol }, monotype);
    return .{ .symbol = symbol, .pattern = pattern };
}

fn toStrLowLevelForPrim(prim: Monotype.Prim) ?CIR.Expr.LowLevel {
    return switch (prim) {
        .u8 => .u8_to_str,
        .i8 => .i8_to_str,
        .u16 => .u16_to_str,
        .i16 => .i16_to_str,
        .u32 => .u32_to_str,
        .i32 => .i32_to_str,
        .u64 => .u64_to_str,
        .i64 => .i64_to_str,
        .u128 => .u128_to_str,
        .i128 => .i128_to_str,
        .f32 => .f32_to_str,
        .f64 => .f64_to_str,
        .dec => .dec_to_str,
        .bool, .str => null,
    };
}

fn lowerStrInspekt(self: *Self, module_env: *const ModuleEnv, run_ll: anytype, region: Region) Allocator.Error!MIR.ExprId {
    const args = module_env.store.sliceExpr(run_ll.args);
    if (args.len != 1) {
        if (builtin.mode == .Debug) {
            std.debug.panic("str_inspekt expected 1 arg in CIR->MIR lowering, got {d}", .{args.len});
        }
        unreachable;
    }

    const arg_cir_expr = args[0];
    const arg_mono = try self.resolveMonotype(arg_cir_expr);
    const lowered_arg = try self.lowerExpr(arg_cir_expr);

    // Evaluate the argument once, then inspect the bound lookup.
    const arg_bind = try self.makeSyntheticBind(arg_mono, false);
    const arg_lookup = try self.emitMirLookup(arg_bind.symbol, arg_mono, region);
    const inspected = try self.lowerStrInspektExpr(module_env, arg_lookup, arg_mono, region);

    const stmts = try self.store.addStmts(self.allocator, &.{MIR.Stmt{
        .decl_const = .{ .pattern = arg_bind.pattern, .expr = lowered_arg },
    }});

    return try self.store.addExpr(
        self.allocator,
        .{ .block = .{
            .stmts = stmts,
            .final_expr = inspected,
        } },
        self.store.monotype_store.primIdx(.str),
        region,
    );
}

fn lowerStrInspektExpr(
    self: *Self,
    module_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    mono_idx: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const mono = self.store.monotype_store.getMonotype(mono_idx);
    return switch (mono) {
        .prim => |prim| switch (prim) {
            .str => blk: {
                break :blk try self.store.addExpr(
                    self.allocator,
                    .{ .str_escape_and_quote = value_expr },
                    self.store.monotype_store.primIdx(.str),
                    region,
                );
            },
            .bool => blk: {
                const true_str = try self.emitMirStrLiteral("True", region);
                const false_str = try self.emitMirStrLiteral("False", region);
                break :blk try self.createBoolMatch(
                    module_env,
                    value_expr,
                    true_str,
                    false_str,
                    self.store.monotype_store.primIdx(.str),
                    region,
                );
            },
            else => |p| blk: {
                const ll = toStrLowLevelForPrim(p) orelse unreachable;
                const args = try self.store.addExprSpan(self.allocator, &.{value_expr});
                break :blk try self.store.addExpr(
                    self.allocator,
                    .{ .run_low_level = .{ .op = ll, .args = args } },
                    self.store.monotype_store.primIdx(.str),
                    region,
                );
            },
        },
        .record => |record| self.lowerStrInspektRecord(module_env, value_expr, record, region),
        .tuple => |tup| self.lowerStrInspektTuple(module_env, value_expr, tup, region),
        .tag_union => |tu| self.lowerStrInspektTagUnion(module_env, value_expr, tu, mono_idx, region),
        .list => |list_data| self.lowerStrInspektList(module_env, value_expr, list_data, region),
        .unit => self.emitMirStrLiteral("{}", region),
        .box => {
            if (builtin.mode == .Debug) {
                std.debug.panic("str_inspekt on Box monotype is unsupported in CIR->MIR lowering", .{});
            }
            unreachable;
        },
        .func => self.emitMirStrLiteral("<function>", region),
    };
}

fn lowerStrInspektRecord(
    self: *Self,
    module_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    record: anytype,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const fields = self.store.monotype_store.getFields(record.fields);
    if (fields.len == 0) return self.emitMirStrLiteral("{}", region);

    const save_exprs = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(save_exprs);

    if (fields.len == 1) {
        const field_name = module_env.getIdent(fields[0].name);
        const open = try std.fmt.allocPrint(self.allocator, "{{ {s}: ", .{field_name});
        defer self.allocator.free(open);
        try self.scratch_expr_ids.append(try self.emitMirStrLiteral(open, region));
        try self.scratch_expr_ids.append(try self.lowerStrInspektExpr(module_env, value_expr, fields[0].type_idx, region));
        try self.scratch_expr_ids.append(try self.emitMirStrLiteral(" }", region));
        return self.foldMirStrConcat(self.scratch_expr_ids.sliceFromStart(save_exprs), region);
    }

    try self.scratch_expr_ids.append(try self.emitMirStrLiteral("{ ", region));
    for (fields, 0..) |field, i| {
        if (i > 0) {
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
        }
        const field_name = module_env.getIdent(field.name);
        const label = try std.fmt.allocPrint(self.allocator, "{s}: ", .{field_name});
        defer self.allocator.free(label);
        try self.scratch_expr_ids.append(try self.emitMirStrLiteral(label, region));

        const field_expr = try self.store.addExpr(
            self.allocator,
            .{ .record_access = .{ .record = value_expr, .field_name = field.name } },
            field.type_idx,
            region,
        );
        try self.scratch_expr_ids.append(try self.lowerStrInspektExpr(module_env, field_expr, field.type_idx, region));
    }
    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(" }", region));
    return self.foldMirStrConcat(self.scratch_expr_ids.sliceFromStart(save_exprs), region);
}

fn lowerStrInspektTuple(
    self: *Self,
    module_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    tup: anytype,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const elems = self.store.monotype_store.getIdxSpan(tup.elems);
    if (elems.len == 0) return self.emitMirStrLiteral("()", region);

    const save_exprs = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(save_exprs);

    try self.scratch_expr_ids.append(try self.emitMirStrLiteral("(", region));
    for (elems, 0..) |elem_mono, i| {
        if (i > 0) {
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
        }
        const elem_expr = try self.store.addExpr(
            self.allocator,
            .{ .tuple_access = .{ .tuple = value_expr, .elem_index = @intCast(i) } },
            elem_mono,
            region,
        );
        try self.scratch_expr_ids.append(try self.lowerStrInspektExpr(module_env, elem_expr, elem_mono, region));
    }
    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(")", region));

    return self.foldMirStrConcat(self.scratch_expr_ids.sliceFromStart(save_exprs), region);
}

fn lowerStrInspektTagUnion(
    self: *Self,
    module_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    tu: anytype,
    mono_idx: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const tags = self.store.monotype_store.getTags(tu.tags);
    if (tags.len == 0) return self.emitMirStrLiteral("<empty_tag_union>", region);

    const save_branches = self.scratch_branches.top();
    defer self.scratch_branches.clearFrom(save_branches);

    for (tags) |tag| {
        const payloads = self.store.monotype_store.getIdxSpan(tag.payloads);

        const save_payload_patterns = self.scratch_pattern_ids.top();
        defer self.scratch_pattern_ids.clearFrom(save_payload_patterns);
        const save_payload_symbols = self.scratch_captures.top();
        defer self.scratch_captures.clearFrom(save_payload_symbols);

        for (payloads) |payload_mono| {
            const bind = try self.makeSyntheticBind(payload_mono, false);
            try self.scratch_pattern_ids.append(bind.pattern);
            try self.scratch_captures.append(.{ .symbol = bind.symbol });
        }

        const payload_pattern_span = try self.store.addPatternSpan(self.allocator, self.scratch_pattern_ids.sliceFromStart(save_payload_patterns));
        const tag_pattern = try self.store.addPattern(
            self.allocator,
            .{ .tag = .{ .name = tag.name, .args = payload_pattern_span } },
            mono_idx,
        );
        const branch_patterns = try self.store.addBranchPatterns(self.allocator, &.{MIR.BranchPattern{
            .pattern = tag_pattern,
            .degenerate = false,
        }});

        const body = if (payloads.len == 0) blk: {
            break :blk try self.emitMirStrLiteral(module_env.getIdent(tag.name), region);
        } else blk: {
            const save_parts = self.scratch_expr_ids.top();
            defer self.scratch_expr_ids.clearFrom(save_parts);

            const tag_open = try std.fmt.allocPrint(self.allocator, "{s}(", .{module_env.getIdent(tag.name)});
            defer self.allocator.free(tag_open);
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(tag_open, region));

            const payload_symbols = self.scratch_captures.sliceFromStart(save_payload_symbols);
            for (payloads, payload_symbols, 0..) |payload_mono, payload_capture, i| {
                if (i > 0) {
                    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
                }
                const payload_lookup = try self.emitMirLookup(payload_capture.symbol, payload_mono, region);
                try self.scratch_expr_ids.append(try self.lowerStrInspektExpr(module_env, payload_lookup, payload_mono, region));
            }
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(")", region));
            break :blk try self.foldMirStrConcat(self.scratch_expr_ids.sliceFromStart(save_parts), region);
        };

        try self.scratch_branches.append(.{
            .patterns = branch_patterns,
            .body = body,
            .guard = MIR.ExprId.none,
        });
    }

    const branches = try self.store.addBranches(self.allocator, self.scratch_branches.sliceFromStart(save_branches));
    return try self.store.addExpr(
        self.allocator,
        .{ .match_expr = .{
            .cond = value_expr,
            .branches = branches,
        } },
        self.store.monotype_store.primIdx(.str),
        region,
    );
}

fn lowerStrInspektList(
    self: *Self,
    module_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    list_data: anytype,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const str_mono = self.store.monotype_store.primIdx(.str);
    const bool_mono = self.store.monotype_store.primIdx(.bool);
    const unit_mono = self.store.monotype_store.unit_idx;

    const acc_bind = try self.makeSyntheticBind(str_mono, true);
    const first_bind = try self.makeSyntheticBind(bool_mono, true);
    const elem_bind = try self.makeSyntheticBind(list_data.elem, false);

    const open_bracket = try self.emitMirStrLiteral("[", region);
    const close_bracket = try self.emitMirStrLiteral("]", region);
    const comma = try self.emitMirStrLiteral(", ", region);
    const empty = try self.emitMirStrLiteral("", region);
    const first_true = try self.emitMirBoolLiteral(module_env, true, region);
    const first_false = try self.emitMirBoolLiteral(module_env, false, region);

    // Build loop body:
    //   prefix = if first then "" else ", "
    //   first = False
    //   acc = acc ++ prefix ++ inspect(elem)
    const first_lookup = try self.emitMirLookup(first_bind.symbol, bool_mono, region);
    const prefix = try self.createBoolMatch(module_env, first_lookup, empty, comma, str_mono, region);
    const elem_lookup = try self.emitMirLookup(elem_bind.symbol, list_data.elem, region);
    const elem_inspected = try self.lowerStrInspektExpr(module_env, elem_lookup, list_data.elem, region);
    const prefixed_elem = try self.emitMirStrConcat(prefix, elem_inspected, region);
    const acc_lookup = try self.emitMirLookup(acc_bind.symbol, str_mono, region);
    const new_acc = try self.emitMirStrConcat(acc_lookup, prefixed_elem, region);

    const unit_expr = try self.emitMirUnitExpr(region);
    const body_stmts = try self.store.addStmts(self.allocator, &.{
        MIR.Stmt{ .mutate_var = .{ .pattern = first_bind.pattern, .expr = first_false } },
        MIR.Stmt{ .mutate_var = .{ .pattern = acc_bind.pattern, .expr = new_acc } },
    });
    const body_expr = try self.store.addExpr(
        self.allocator,
        .{ .block = .{
            .stmts = body_stmts,
            .final_expr = unit_expr,
        } },
        unit_mono,
        region,
    );

    const for_expr = try self.store.addExpr(
        self.allocator,
        .{ .for_loop = .{
            .list = value_expr,
            .elem_pattern = elem_bind.pattern,
            .body = body_expr,
        } },
        unit_mono,
        region,
    );
    const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_mono);
    const loop_stmt: MIR.Stmt = .{ .decl_const = .{ .pattern = wildcard, .expr = for_expr } };

    const final_acc_lookup = try self.emitMirLookup(acc_bind.symbol, str_mono, region);
    const final_expr = try self.emitMirStrConcat(final_acc_lookup, close_bracket, region);

    const outer_stmts = try self.store.addStmts(self.allocator, &.{
        MIR.Stmt{ .decl_var = .{ .pattern = acc_bind.pattern, .expr = open_bracket } },
        MIR.Stmt{ .decl_var = .{ .pattern = first_bind.pattern, .expr = first_true } },
        loop_stmt,
    });
    return try self.store.addExpr(
        self.allocator,
        .{ .block = .{
            .stmts = outer_stmts,
            .final_expr = final_expr,
        } },
        str_mono,
        region,
    );
}

// --- Public API ---

/// Create a symbol using MIR lowering's current opaque ID encoding.
/// Intended for callers that need to invoke APIs like `lowerExternalDef`.
pub fn makeSymbol(self: *Self, module_idx: u32, ident_idx: Ident.Idx) Allocator.Error!MIR.Symbol {
    return self.internSymbol(module_idx, ident_idx);
}

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
            const mono = self.store.monotype_store.getMonotype(monotype);
            return switch (mono) {
                .prim => |p| switch (p) {
                    .f64 => try self.store.addExpr(self.allocator, .{ .frac_f64 = roc_dec.toF64() }, monotype, region),
                    .f32 => try self.store.addExpr(self.allocator, .{ .frac_f32 = @floatCast(roc_dec.toF64()) }, monotype, region),
                    .dec => try self.store.addExpr(self.allocator, .{ .dec = roc_dec }, monotype, region),
                    else => {
                        if (std.debug.runtime_safety) {
                            const type_name = module_env.getIdent(tf.type_name);
                            std.debug.panic(
                                "lowerExpr(e_typed_frac): unsupported monotype for type '{s}' (checker/lowering invariant broken)",
                                .{type_name},
                            );
                        }
                        unreachable;
                    },
                },
                else => {
                    if (std.debug.runtime_safety) {
                        const type_name = module_env.getIdent(tf.type_name);
                        std.debug.panic(
                            "lowerExpr(e_typed_frac): non-prim monotype for type '{s}' (checker/lowering invariant broken)",
                            .{type_name},
                        );
                    }
                    unreachable;
                },
            };
        },

        // --- Strings ---
        .e_str_segment => |seg| blk: {
            const mir_str = try self.copyStringToMir(module_env, seg.literal);
            break :blk try self.store.addExpr(self.allocator, .{ .str = mir_str }, monotype, region);
        },
        .e_str => |str_expr| {
            const span = module_env.store.sliceExpr(str_expr.span);
            if (span.len == 0) {
                const mir_str = try self.store.strings.insert(self.allocator, "");
                return try self.store.addExpr(self.allocator, .{ .str = mir_str }, monotype, region);
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

            // Check for a deferred block-local polymorphic lambda. If found,
            // bind the definition's type vars from the call-site monotype and
            // lower with concrete types, patching the block's placeholder stmt.
            const symbol_key: u64 = @bitCast(symbol);
            if (self.deferred_block_lambdas.get(@intFromEnum(lookup.pattern_idx))) |deferred| {
                const deferred_is_unlowered =
                    self.scratch_stmts.items.items[deferred.scratch_stmt_idx].decl_const.expr == MIR.ExprId.none;
                if (deferred_is_unlowered) {
                    // Remove from deferred BEFORE lowering to prevent infinite
                    // recursion: if the lambda's body references itself (e.g.,
                    // recursive fib), the nested lookup must not re-enter this
                    // deferred path. Mark as in-progress so recursive references
                    // get placeholder lookups via lowerExternalDefWithType's guard.
                    const saved_deferred = deferred;
                    _ = self.deferred_block_lambdas.remove(@intFromEnum(lookup.pattern_idx));

                    try self.in_progress_defs.put(symbol_key, {});

                    // Use a fresh type_var_seen scope so the call-site's concrete
                    // types aren't shadowed by stale Dec/unit entries from the
                    // pattern lowering that happened when the block processed the
                    // s_decl statement.
                    const saved_type_var_seen = self.type_var_seen;
                    self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
                    defer {
                        self.type_var_seen.deinit();
                        self.type_var_seen = saved_type_var_seen;
                    }
                    try self.bindTypeVarMonotypes(ModuleEnv.varFrom(saved_deferred.cir_expr), monotype);
                    const saved_pattern_scope = self.current_pattern_scope;
                    self.current_pattern_scope = symbol_key;
                    defer self.current_pattern_scope = saved_pattern_scope;
                    const lowered = try self.lowerExpr(saved_deferred.cir_expr);
                    self.scratch_stmts.items.items[saved_deferred.scratch_stmt_idx].decl_const.expr = lowered;

                    // Track the base symbol so later call sites can trigger true
                    // polymorphic specializations for this block-local lambda.
                    try self.lowered_symbols.put(symbol_key, lowered);

                    _ = self.in_progress_defs.remove(symbol_key);

                    // Keep the CIR expression metadata for future
                    // re-specializations from other call sites in this block.
                    try self.deferred_block_lambdas.put(@intFromEnum(lookup.pattern_idx), saved_deferred);

                    // Patch any recursion placeholders with the correct monotype.
                    const resolved_monotype = self.store.typeOf(lowered);
                    if (self.recursion_placeholders.getPtr(symbol_key)) |expr_list| {
                        for (expr_list.items) |expr_id| {
                            self.store.type_map.items[@intFromEnum(expr_id)] = resolved_monotype;
                        }
                        expr_list.deinit(self.allocator);
                        _ = self.recursion_placeholders.remove(symbol_key);
                    }
                }
            }

            // Ensure the local definition is lowered if it's a top-level def.
            // This is needed so that cross-module lowering (via lowerExternalDef)
            // properly registers all transitively-referenced definitions.
            if (!self.lowered_symbols.contains(symbol_key) and !self.in_progress_defs.contains(symbol_key)) {
                // Find the CIR def for this pattern in the current module
                const defs = module_env.store.sliceDefs(module_env.all_defs);
                for (defs) |def_idx| {
                    const def = module_env.store.getDef(def_idx);
                    if (def.pattern == lookup.pattern_idx) {
                        // Bind the definition's type vars from the call-site
                        // monotype. The definition has its own type scheme
                        // (e.g. range_to's `b`) separate from the caller's
                        // instantiated copy (e.g. `a` in `to`), so we bind
                        // before lowering to connect them.
                        try self.bindTypeVarMonotypes(ModuleEnv.varFrom(def.expr), monotype);
                        _ = try self.lowerExternalDef(symbol, def.expr);
                        break;
                    }
                }
            } else if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                // Symbol is already lowered. Check if this is a polymorphic
                // re-specialization: the same function called with a different type.
                const cached_monotype = self.store.typeOf(cached_expr);
                const monotype_matches_cached = try self.monotypesStructurallyEqual(cached_monotype, monotype);
                if (!monotype.isNone() and !monotype_matches_cached) {
                    // Check for an existing specialization with this monotype.
                    if (try self.lookupPolySpecialization(symbol_key, monotype)) |spec_symbol| {
                        return try self.store.addExpr(self.allocator, .{ .lookup = spec_symbol }, monotype, region);
                    }

                    // Try to create a new specialization. Check both module-level
                    // defs and deferred block lambdas for the CIR expression.
                    const cir_expr: ?CIR.Expr.Idx = blk: {
                        // Check module-level defs first.
                        const defs = module_env.store.sliceDefs(module_env.all_defs);
                        for (defs) |def_idx| {
                            const def = module_env.store.getDef(def_idx);
                            if (def.pattern == lookup.pattern_idx) {
                                break :blk def.expr;
                            }
                        }
                        // Check deferred block lambdas (restored after first specialization).
                        if (self.deferred_block_lambdas.get(@intFromEnum(lookup.pattern_idx))) |deferred_entry| {
                            break :blk deferred_entry.cir_expr;
                        }
                        break :blk null;
                    };

                    if (cir_expr) |cir_def_expr| {
                        const spec_key = polySpecKey(symbol_key, monotype);
                        const spec_symbol = try self.makeSyntheticSymbol(symbol);
                        const spec_symbol_key: u64 = @bitCast(spec_symbol);

                        try self.in_progress_defs.put(spec_symbol_key, {});

                        const saved_type_var_seen = self.type_var_seen;
                        self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
                        defer {
                            self.type_var_seen.deinit();
                            self.type_var_seen = saved_type_var_seen;
                        }
                        try self.bindTypeVarMonotypes(ModuleEnv.varFrom(cir_def_expr), monotype);
                        const saved_pattern_scope = self.current_pattern_scope;
                        self.current_pattern_scope = spec_symbol_key;
                        defer self.current_pattern_scope = saved_pattern_scope;
                        const spec_lowered = try self.lowerExpr(cir_def_expr);

                        _ = self.in_progress_defs.remove(spec_symbol_key);

                        try self.lowered_symbols.put(spec_symbol_key, spec_lowered);
                        try self.store.registerSymbolDef(self.allocator, spec_symbol, spec_lowered);
                        try self.poly_specializations.put(spec_key, spec_symbol);

                        // Emit a decl_const for the specialization in the current block.
                        const spec_pattern = try self.store.addPattern(self.allocator, .{ .bind = spec_symbol }, monotype);
                        try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = spec_pattern, .expr = spec_lowered } });

                        return try self.store.addExpr(self.allocator, .{ .lookup = spec_symbol }, monotype, region);
                    }
                }
            }

            const pattern_monotype = try self.store.monotype_store.fromTypeVar(
                self.allocator,
                self.types_store,
                ModuleEnv.varFrom(lookup.pattern_idx),
                self.currentCommonIdents(),
                &self.type_var_seen,
                &self.mono_scratches,
            );
            if (try self.monotypesStructurallyEqual(pattern_monotype, monotype)) {
                return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, pattern_monotype, region);
            }

            if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                const cached_monotype = self.store.typeOf(cached_expr);
                const monotype_matches_cached = try self.monotypesStructurallyEqual(cached_monotype, monotype);
                if (monotype_matches_cached) {
                    return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, cached_monotype, region);
                }
            }

            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
        },
        .e_lookup_external => |ext| {
            // Import must be resolved before MIR lowering; reaching here
            // with an unresolved import means a compiler bug in an earlier phase.
            const target_module_idx: u32 = @intCast(module_env.imports.getResolvedModule(ext.module_idx) orelse
                unreachable);
            const symbol = try self.internSymbol(target_module_idx, ext.ident_idx);

            // Ensure the external definition is lowered.
            const symbol_key: u64 = @bitCast(symbol);
            if (!self.lowered_symbols.contains(symbol_key) and !self.in_progress_defs.contains(symbol_key)) {
                const target_env = self.all_module_envs[target_module_idx];
                if (target_env.store.isDefNode(ext.target_node_idx)) {
                    const def_idx: CIR.Def.Idx = @enumFromInt(ext.target_node_idx);
                    const def = target_env.store.getDef(def_idx);
                    _ = try self.lowerExternalDefWithType(symbol, def.expr, monotype);
                }
                return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
            }

            // Symbol already lowered. Check for polymorphic re-specialization.
            if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                const cached_monotype = self.store.typeOf(cached_expr);
                const monotype_matches_cached = try self.monotypesStructurallyEqual(cached_monotype, monotype);
                if (!monotype.isNone() and !monotype_matches_cached) {
                    if (try self.lookupPolySpecialization(symbol_key, monotype)) |spec_symbol| {
                        return try self.store.addExpr(self.allocator, .{ .lookup = spec_symbol }, monotype, region);
                    }

                    // Create a new specialization.
                    const target_env = self.all_module_envs[target_module_idx];
                    if (target_env.store.isDefNode(ext.target_node_idx)) {
                        const spec_key = polySpecKey(symbol_key, monotype);
                        const def_idx: CIR.Def.Idx = @enumFromInt(ext.target_node_idx);
                        const def = target_env.store.getDef(def_idx);
                        const spec_symbol = try self.makeSyntheticSymbol(symbol);

                        // Lower with lowerExternalDefWithType using the spec_symbol
                        // so it gets its own cache entry and symbol_def registration.
                        _ = try self.lowerExternalDefWithType(spec_symbol, def.expr, monotype);
                        try self.poly_specializations.put(spec_key, spec_symbol);

                        return try self.store.addExpr(self.allocator, .{ .lookup = spec_symbol }, monotype, region);
                    }
                }
            }

            if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                const cached_monotype = self.store.typeOf(cached_expr);
                if (try self.monotypesStructurallyEqual(cached_monotype, monotype)) {
                    return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, cached_monotype, region);
                }
            }

            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
        },
        .e_lookup_pending => {
            // Must be resolved to e_lookup_external before MIR lowering;
            // reaching here means a compiler bug in an earlier phase.
            unreachable;
        },
        .e_lookup_required => |lookup| {
            const app_idx = self.app_module_idx orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "e_lookup_required encountered without app module (module_idx={d}, requires_idx={d})",
                        .{ self.current_module_idx, lookup.requires_idx.toU32() },
                    );
                }
                unreachable;
            };
            const required_type = module_env.requires_types.get(lookup.requires_idx);
            const required_name = module_env.getIdent(required_type.ident);

            const app_env = self.all_module_envs[app_idx];
            const app_ident = app_env.common.findIdent(required_name) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "required lookup not translated into app ident space: {s}",
                        .{required_name},
                    );
                }
                unreachable;
            };

            const app_exports = app_env.store.sliceDefs(app_env.exports);
            var exported_def: ?CIR.Def.Idx = null;
            for (app_exports) |def_idx| {
                const def = app_env.store.getDef(def_idx);
                const pat = app_env.store.getPattern(def.pattern);
                if (pat == .assign and pat.assign.ident.eql(app_ident)) {
                    exported_def = def_idx;
                    break;
                }
            }

            const def_idx = exported_def orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "required lookup resolved to non-exported app ident: {s}",
                        .{required_name},
                    );
                }
                unreachable;
            };
            const def = app_env.store.getDef(def_idx);
            const symbol = try self.internSymbol(app_idx, app_ident);
            _ = try self.lowerExternalDefWithType(symbol, def.expr, monotype);
            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
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
        .e_binop => |binop| try self.lowerBinop(expr_idx, binop, monotype, region),
        .e_unary_minus => |um| try self.lowerUnaryMinus(expr_idx, um, monotype, region),
        .e_unary_not => |un| try self.lowerUnaryNot(un, monotype, region),

        // --- Access ---
        .e_dot_access => |da| try self.lowerDotAccess(module_env, expr_idx, da, monotype, region),
        .e_tuple_access => |ta| {
            const tuple_expr = try self.lowerExpr(ta.tuple);
            return try self.store.addExpr(self.allocator, .{ .tuple_access = .{
                .tuple = tuple_expr,
                .elem_index = ta.elem_index,
            } }, monotype, region);
        },

        // --- Nominal (strip wrapper, keep nominal monotype) ---
        .e_nominal => |nom| {
            const result = try self.lowerExpr(nom.backing_expr);
            self.store.type_map.items[@intFromEnum(result)] = monotype;
            return result;
        },
        .e_nominal_external => |nom_ext| {
            const result = try self.lowerExpr(nom_ext.backing_expr);
            self.store.type_map.items[@intFromEnum(result)] = monotype;
            return result;
        },

        // --- Type var dispatch (resolved to call) ---
        .e_type_var_dispatch => |tvd| {
            return try self.lowerTypeVarDispatch(module_env, expr_idx, tvd, monotype, region);
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
            if (run_ll.op == .str_inspekt) {
                return try self.lowerStrInspekt(module_env, run_ll, region);
            }
            const args = try self.lowerExprSpan(module_env, run_ll.args);
            return try self.store.addExpr(self.allocator, .{ .run_low_level = .{
                .op = run_ll.op,
                .args = args,
            } }, monotype, region);
        },

        // --- Error/Debug ---
        .e_runtime_error => |re| try self.store.addExpr(self.allocator, .{ .runtime_err_can = .{ .diagnostic = re.diagnostic } }, monotype, region),
        .e_crash => |crash| blk: {
            const mir_str = try self.copyStringToMir(module_env, crash.msg);
            break :blk try self.store.addExpr(self.allocator, .{ .crash = mir_str }, monotype, region);
        },
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
    const base_key: u64 = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(pattern_idx);
    const key: u128 = (@as(u128, self.current_pattern_scope) << 64) | @as(u128, base_key);

    if (self.pattern_symbols.get(key)) |existing| {
        return existing;
    }

    // In a specialized scope, captures can reference symbols from an outer
    // unscoped block. Reuse the existing unscoped mapping when available.
    if (self.current_pattern_scope != 0) {
        const unscoped_key: u128 = @as(u128, base_key);
        if (self.pattern_symbols.get(unscoped_key)) |outer| {
            try self.pattern_symbols.put(key, outer);
            return outer;
        }
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);
    const is_top_level_pattern = blk: {
        const defs = module_env.store.sliceDefs(module_env.all_defs);
        for (defs) |def_idx| {
            if (module_env.store.getDef(def_idx).pattern == pattern_idx) break :blk true;
        }
        break :blk false;
    };
    const use_scoped_local_ident = self.current_pattern_scope != 0 and !is_top_level_pattern;

    const ident_idx: Ident.Idx = switch (pattern) {
        .assign => |a| if (use_scoped_local_ident) self.makeSyntheticIdent(a.ident) else a.ident,
        .as => |a| if (use_scoped_local_ident) self.makeSyntheticIdent(a.ident) else a.ident,
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

    const symbol = try self.internSymbol(self.current_module_idx, ident_idx);

    try self.pattern_symbols.put(key, symbol);
    return symbol;
}

fn makeSyntheticIdent(self: *Self, original_ident: Ident.Idx) Ident.Idx {
    const idx = self.next_synthetic_ident;
    self.next_synthetic_ident -= 1;
    return .{
        .attributes = original_ident.attributes,
        .idx = idx,
    };
}

/// Create a synthetic symbol for a polymorphic specialization.
/// Uses a counter that decrements from NONE-1 to generate unique ident indices.
fn makeSyntheticSymbol(self: *Self, original: MIR.Symbol) Allocator.Error!MIR.Symbol {
    const original_meta = self.getSymbolMetadata(original);
    const synthetic_ident = self.makeSyntheticIdent(original_meta.ident_idx);
    return self.internSymbol(original_meta.module_idx, synthetic_ident);
}

/// Compute the composite cache key for polymorphic specializations.
fn polySpecKey(symbol_key: u64, monotype: Monotype.Idx) u128 {
    return (@as(u128, symbol_key) << 32) | @as(u128, @intFromEnum(monotype));
}

fn lookupPolySpecialization(self: *Self, symbol_key: u64, caller_monotype: Monotype.Idx) Allocator.Error!?MIR.Symbol {
    const exact_key = polySpecKey(symbol_key, caller_monotype);
    if (self.poly_specializations.get(exact_key)) |exact| return exact;

    // Monotype IDs are not globally canonicalized. Reuse an existing specialization
    // when its cached caller monotype is structurally equal to this call site.
    var it = self.poly_specializations.iterator();
    while (it.next()) |entry| {
        const cache_key = entry.key_ptr.*;
        const cached_symbol_key: u64 = @intCast(cache_key >> 32);
        if (cached_symbol_key != symbol_key) continue;

        const cached_mono_raw: u32 = @truncate(cache_key);
        const cached_mono: Monotype.Idx = @enumFromInt(cached_mono_raw);
        if (try self.monotypesStructurallyEqual(cached_mono, caller_monotype)) {
            return entry.value_ptr.*;
        }
    }

    return null;
}

fn monotypesStructurallyEqual(self: *Self, lhs: Monotype.Idx, rhs: Monotype.Idx) Allocator.Error!bool {
    if (lhs == rhs) return true;

    var seen = std.AutoHashMap(u64, void).init(self.allocator);
    defer seen.deinit();

    return try self.monotypesStructurallyEqualRec(lhs, rhs, &seen);
}

fn monotypesStructurallyEqualRec(
    self: *Self,
    lhs: Monotype.Idx,
    rhs: Monotype.Idx,
    seen: *std.AutoHashMap(u64, void),
) Allocator.Error!bool {
    if (lhs == rhs) return true;

    const lhs_u32: u32 = @intFromEnum(lhs);
    const rhs_u32: u32 = @intFromEnum(rhs);
    const key: u64 = (@as(u64, lhs_u32) << 32) | @as(u64, rhs_u32);

    if (seen.contains(key)) return true;
    try seen.put(key, {});

    const lhs_mono = self.store.monotype_store.getMonotype(lhs);
    const rhs_mono = self.store.monotype_store.getMonotype(rhs);
    if (std.meta.activeTag(lhs_mono) != std.meta.activeTag(rhs_mono)) return false;

    return switch (lhs_mono) {
        .unit => true,
        .prim => |lhs_prim| lhs_prim == rhs_mono.prim,
        .list => |lhs_list| try self.monotypesStructurallyEqualRec(lhs_list.elem, rhs_mono.list.elem, seen),
        .box => |lhs_box| try self.monotypesStructurallyEqualRec(lhs_box.inner, rhs_mono.box.inner, seen),
        .tuple => |lhs_tuple| blk: {
            const lhs_elems = self.store.monotype_store.getIdxSpan(lhs_tuple.elems);
            const rhs_elems = self.store.monotype_store.getIdxSpan(rhs_mono.tuple.elems);
            if (lhs_elems.len != rhs_elems.len) break :blk false;
            for (lhs_elems, rhs_elems) |lhs_elem, rhs_elem| {
                if (!try self.monotypesStructurallyEqualRec(lhs_elem, rhs_elem, seen)) {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .func => |lhs_func| blk: {
            const rhs_func = rhs_mono.func;
            if (lhs_func.effectful != rhs_func.effectful) break :blk false;
            const lhs_args = self.store.monotype_store.getIdxSpan(lhs_func.args);
            const rhs_args = self.store.monotype_store.getIdxSpan(rhs_func.args);
            if (lhs_args.len != rhs_args.len) break :blk false;
            for (lhs_args, rhs_args) |lhs_arg, rhs_arg| {
                if (!try self.monotypesStructurallyEqualRec(lhs_arg, rhs_arg, seen)) {
                    break :blk false;
                }
            }
            break :blk try self.monotypesStructurallyEqualRec(lhs_func.ret, rhs_func.ret, seen);
        },
        .record => |lhs_record| blk: {
            const lhs_fields = self.store.monotype_store.getFields(lhs_record.fields);
            const rhs_fields = self.store.monotype_store.getFields(rhs_mono.record.fields);
            if (lhs_fields.len != rhs_fields.len) break :blk false;
            for (lhs_fields, rhs_fields) |lhs_field, rhs_field| {
                if (!lhs_field.name.eql(rhs_field.name)) break :blk false;
                if (!try self.monotypesStructurallyEqualRec(lhs_field.type_idx, rhs_field.type_idx, seen)) {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .tag_union => |lhs_union| blk: {
            const lhs_tags = self.store.monotype_store.getTags(lhs_union.tags);
            const rhs_tags = self.store.monotype_store.getTags(rhs_mono.tag_union.tags);
            if (lhs_tags.len != rhs_tags.len) break :blk false;
            for (lhs_tags, rhs_tags) |lhs_tag, rhs_tag| {
                if (!lhs_tag.name.eql(rhs_tag.name)) break :blk false;
                const lhs_payloads = self.store.monotype_store.getIdxSpan(lhs_tag.payloads);
                const rhs_payloads = self.store.monotype_store.getIdxSpan(rhs_tag.payloads);
                if (lhs_payloads.len != rhs_payloads.len) break :blk false;
                for (lhs_payloads, rhs_payloads) |lhs_payload, rhs_payload| {
                    if (!try self.monotypesStructurallyEqualRec(lhs_payload, rhs_payload, seen)) {
                        break :blk false;
                    }
                }
            }
            break :blk true;
        },
    };
}

/// Get the monotype for a CIR expression (via its type var).
fn resolveMonotype(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!Monotype.Idx {
    const type_var = ModuleEnv.varFrom(expr_idx);
    return try self.store.monotype_store.fromTypeVar(
        self.allocator,
        self.types_store,
        type_var,
        self.currentCommonIdents(),
        &self.type_var_seen,
        &self.mono_scratches,
    );
}

fn currentCommonIdents(self: *const Self) ModuleEnv.CommonIdents {
    return self.all_module_envs[self.current_module_idx].idents;
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
        self.currentCommonIdents(),
        &self.type_var_seen,
        &self.mono_scratches,
    );

    return switch (pattern) {
        .assign => {
            const symbol = try self.patternToSymbol(pattern_idx);
            return try self.store.addPattern(self.allocator, .{ .bind = symbol }, monotype);
        },
        .underscore => try self.store.addPattern(self.allocator, .wildcard, monotype),
        .as => |a| {
            const inner = try self.lowerPattern(module_env, a.pattern);
            const symbol = try self.patternToSymbol(pattern_idx);
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
            // Strip nominal wrapper, but keep the nominal monotype
            // (same pattern as e_nominal expression lowering).
            const result = try self.lowerPattern(module_env, nom.backing_pattern);
            self.store.pattern_type_map.items[@intFromEnum(result)] = monotype;
            return result;
        },
        .nominal_external => |nom_ext| {
            // Strip nominal wrapper, but keep the nominal monotype
            // (same pattern as e_nominal_external expression lowering).
            const result = try self.lowerPattern(module_env, nom_ext.backing_pattern);
            self.store.pattern_type_map.items[@intFromEnum(result)] = monotype;
            return result;
        },
        .num_literal => |nl| try self.store.addPattern(self.allocator, .{ .int_literal = .{ .value = nl.value } }, monotype),
        .str_literal => |sl| blk: {
            const mir_str = try self.copyStringToMir(module_env, sl.literal);
            break :blk try self.store.addPattern(self.allocator, .{ .str_literal = mir_str }, monotype);
        },
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
/// If the call target is an external lookup to a low-level builtin
/// (e.g., List.concat, Str.concat), emit `run_low_level` instead of `call`.
fn lowerCall(self: *Self, module_env: *const ModuleEnv, call: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    // Check if the call target is an external low-level builtin.
    const func_cir = module_env.store.getExpr(call.func);
    if (func_cir == .e_lookup_external) {
        if (self.getExternalLowLevelOp(module_env, func_cir.e_lookup_external)) |ll_op| {
            const args = try self.lowerExprSpan(module_env, call.args);
            return try self.store.addExpr(self.allocator, .{ .run_low_level = .{
                .op = ll_op,
                .args = args,
            } }, monotype, region);
        }
    }

    var func = try self.lowerExpr(call.func);

    const resolved_func_monotype_for_args = try self.resolveMonotype(call.func);
    const expected_arg_monos: []const Monotype.Idx = blk: {
        if (resolved_func_monotype_for_args.isNone()) break :blk &.{};
        const fn_mono = self.store.monotype_store.getMonotype(resolved_func_monotype_for_args);
        if (fn_mono != .func) break :blk &.{};
        break :blk self.store.monotype_store.getIdxSpan(fn_mono.func.args);
    };

    const call_arg_exprs = module_env.store.sliceExpr(call.args);
    const args_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(args_top);
    for (call_arg_exprs, 0..) |arg_idx, i| {
        if (i < expected_arg_monos.len and !expected_arg_monos[i].isNone()) {
            try self.bindTypeVarMonotypes(ModuleEnv.varFrom(arg_idx), expected_arg_monos[i]);
        }
        const arg = try self.lowerExpr(arg_idx);
        try self.scratch_expr_ids.append(arg);
    }
    const args = try self.store.addExprSpan(self.allocator, self.scratch_expr_ids.sliceFromStart(args_top));

    // If call target lowered to a symbol lookup, route through method lowering
    // so polymorphic recursive calls can get a distinct specialization symbol.
    if (self.store.getExpr(func) == .lookup) {
        const callee_symbol = self.store.getExpr(func).lookup;

        var desired_func_monotype = try self.resolveMonotype(call.func);
        if (desired_func_monotype.isNone()) {
            const mono_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(mono_top);
            const cir_args = module_env.store.sliceExpr(call.args);
            for (cir_args) |arg_idx| {
                try self.mono_scratches.idxs.append(try self.resolveMonotype(arg_idx));
            }
            desired_func_monotype = try self.buildFuncMonotype(
                self.mono_scratches.idxs.sliceFromStart(mono_top),
                monotype,
                false,
            );
        }

        if (!desired_func_monotype.isNone()) {
            const lowered_symbol = try self.ensureMethodLowered(callee_symbol, desired_func_monotype);
            if (@as(u64, @bitCast(lowered_symbol)) != @as(u64, @bitCast(callee_symbol))) {
                func = try self.store.addExpr(
                    self.allocator,
                    .{ .lookup = lowered_symbol },
                    desired_func_monotype,
                    region,
                );
            }
        }
    }

    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = func,
        .args = args,
    } }, monotype, region);
}

/// Check if an external definition is a low-level wrapper (e_lambda wrapping e_run_low_level).
/// Returns the low-level op if found, null otherwise.
fn getExternalLowLevelOp(self: *Self, caller_env: *const ModuleEnv, lookup: anytype) ?CIR.Expr.LowLevel {
    const ext_module_idx = caller_env.imports.getResolvedModule(lookup.module_idx) orelse return null;
    if (ext_module_idx >= self.all_module_envs.len) return null;
    const ext_env = self.all_module_envs[ext_module_idx];
    if (!ext_env.store.isDefNode(lookup.target_node_idx)) return null;
    const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
    const def = ext_env.store.getDef(def_idx);
    const def_expr = ext_env.store.getExpr(def.expr);
    if (def_expr == .e_lambda) {
        const body_expr = ext_env.store.getExpr(def_expr.e_lambda.body);
        if (body_expr == .e_run_low_level) return body_expr.e_run_low_level.op;
    }
    return null;
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

                // Polymorphic lambda/closure: defer lowering until a call site
                // provides concrete type information. This prevents flex type
                // vars from prematurely defaulting to Dec.
                const cir_expr = module_env.store.getExpr(decl.expr);
                const is_lambda = cir_expr == .e_lambda or cir_expr == .e_closure;
                const needs_inst = if (is_lambda) self.types_store.needsInstantiation(ModuleEnv.varFrom(decl.expr)) else false;
                if (is_lambda and needs_inst) {
                    const scratch_idx: u32 = @intCast(self.scratch_stmts.items.items.len);
                    // Emit a placeholder decl_const — the expr will be filled in
                    // when a call site triggers lowering via e_lookup_local.
                    try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = pat, .expr = MIR.ExprId.none } });
                    try self.deferred_block_lambdas.put(@intFromEnum(decl.pattern), .{
                        .cir_expr = decl.expr,
                        .scratch_stmt_idx = scratch_idx,
                        .module_idx = self.current_module_idx,
                    });
                } else {
                    const expr = try self.lowerExpr(decl.expr);
                    try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = pat, .expr = expr } });
                }
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
                const mir_str = try self.copyStringToMir(module_env, s_crash.msg);
                const expr = try self.store.addExpr(self.allocator, .{ .crash = mir_str }, unit_monotype, Region.zero());
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

    // Lower the final expression BEFORE committing stmts. Call sites in the
    // final expression (and in subsequent stmts) trigger deferred lambda
    // lowering via e_lookup_local, which patches the placeholder stmts.
    const final_expr = try self.lowerExpr(block.final_expr);

    // Lower any remaining deferred lambdas from THIS block that weren't
    // referenced by a call site. Only process lambdas whose scratch_stmt_idx
    // falls within this block's range (>= stmts_top) and whose module matches
    // the current module. This prevents cross-module contamination when inner
    // blocks from different modules are lowered during the final expression.
    {
        const stmts_top_idx: u32 = @intCast(stmts_top);
        var deferred_iter = self.deferred_block_lambdas.iterator();
        while (deferred_iter.next()) |entry| {
            const deferred = entry.value_ptr.*;
            if (deferred.module_idx != self.current_module_idx or deferred.scratch_stmt_idx < stmts_top_idx) continue;
            if (self.scratch_stmts.items.items[deferred.scratch_stmt_idx].decl_const.expr == MIR.ExprId.none) {
                const expr = try self.lowerExpr(deferred.cir_expr);
                self.scratch_stmts.items.items[deferred.scratch_stmt_idx].decl_const.expr = expr;
            }
            // Remove this entry since the block is done
            self.deferred_block_lambdas.removeByPtr(entry.key_ptr);
        }
    }

    const stmt_span = try self.store.addStmts(self.allocator, self.scratch_stmts.sliceFromStart(stmts_top));

    return try self.store.addExpr(self.allocator, .{ .block = .{
        .stmts = stmt_span,
        .final_expr = final_expr,
    } }, monotype, region);
}

/// Lower `e_binop` to either a method call or a match (for short-circuit and/or).
fn lowerBinop(self: *Self, expr_idx: CIR.Expr.Idx, binop: CIR.Expr.Binop, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
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
            const lhs = try self.lowerExpr(binop.lhs);
            const lhs_monotype = try self.resolveMonotype(binop.lhs);
            if (!lhs_monotype.isNone()) {
                // Binops dispatch off the LHS nominal method (e.g. U32.plus),
                // so bind RHS lowering to the same monotype for polymorphic literals.
                try self.bindTypeVarMonotypes(ModuleEnv.varFrom(binop.rhs), lhs_monotype);
            }
            const rhs = try self.lowerExpr(binop.rhs);

            // Use checker-resolved target for static dispatch.
            const resolved_target = self.lookupResolvedDispatchTarget(expr_idx) orelse {
                if (std.debug.runtime_safety) {
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
                    const method_name = module_env.getIdent(method_ident);
                    std.debug.panic(
                        "lowerBinop: missing resolved dispatch target for method '{s}' op {s}",
                        .{ method_name, @tagName(binop.op) },
                    );
                }
                unreachable;
            };
            const method_symbol = try self.resolvedDispatchTargetToSymbol(module_env, resolved_target);

            const func_monotype = try self.buildFuncMonotype(&.{ lhs_monotype, lhs_monotype }, monotype, false);

            // Ensure the method body is lowered so codegen can find it.
            const lowered_method_symbol = try self.ensureMethodLowered(method_symbol, func_monotype);

            const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = lowered_method_symbol }, func_monotype, region);

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
fn lowerUnaryMinus(self: *Self, expr_idx: CIR.Expr.Idx, um: CIR.Expr.UnaryMinus, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const module_env = self.all_module_envs[self.current_module_idx];
    const inner = try self.lowerExpr(um.expr);

    const resolved_target = self.lookupResolvedDispatchTarget(expr_idx) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic("lowerUnaryMinus: missing resolved dispatch target for negate", .{});
        }
        unreachable;
    };
    const method_symbol = try self.resolvedDispatchTargetToSymbol(module_env, resolved_target);

    const inner_monotype = try self.resolveMonotype(um.expr);
    const func_monotype = try self.buildFuncMonotype(&.{inner_monotype}, monotype, false);

    // Ensure the method body is lowered so codegen can find it.
    const lowered_method_symbol = try self.ensureMethodLowered(method_symbol, func_monotype);

    const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = lowered_method_symbol }, func_monotype, region);
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
fn lowerDotAccess(self: *Self, module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, da: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const receiver = try self.lowerExpr(da.receiver);

    if (da.args) |args_span| {
        const resolved_target = self.lookupResolvedDispatchTarget(expr_idx) orelse {
            if (std.debug.runtime_safety) {
                const method_name = module_env.getIdent(da.field_name);
                std.debug.panic("lowerDotAccess: missing resolved dispatch target for method '{s}'", .{method_name});
            }
            unreachable;
        };
        const method_symbol = try self.resolvedDispatchTargetToSymbol(module_env, resolved_target);

        // Build args as [receiver] ++ explicit_args
        // e.g. list.map(fn) → List.map(list, fn)
        const explicit_args = module_env.store.sliceExpr(args_span);
        const receiver_monotype = try self.resolveMonotype(da.receiver);
        var expected_param_monos: []const Monotype.Idx = &.{};
        var resolved_fn = self.types_store.resolveVar(resolved_target.fn_var);
        while (resolved_fn.desc.content == .alias) {
            const alias = resolved_fn.desc.content.alias;
            resolved_fn = self.types_store.resolveVar(self.types_store.getAliasBackingVar(alias));
        }
        if (resolved_fn.desc.content == .structure) {
            const fn_args = switch (resolved_fn.desc.content.structure) {
                .fn_pure => |f| self.types_store.sliceVars(f.args),
                .fn_effectful => |f| self.types_store.sliceVars(f.args),
                .fn_unbound => |f| self.types_store.sliceVars(f.args),
                else => &.{},
            };
            if (fn_args.len > 0 and !receiver_monotype.isNone()) {
                const arg0_root = self.types_store.resolveVar(fn_args[0]).var_;
                try self.type_var_seen.put(arg0_root, receiver_monotype);
            }
        }

        const dispatch_func_monotype = try self.store.monotype_store.fromTypeVar(
            self.allocator,
            self.types_store,
            resolved_target.fn_var,
            self.currentCommonIdents(),
            &self.type_var_seen,
            &self.mono_scratches,
        );
        if (!dispatch_func_monotype.isNone()) {
            const dispatch_mono = self.store.monotype_store.getMonotype(dispatch_func_monotype);
            if (dispatch_mono == .func) {
                const dispatch_args = self.store.monotype_store.getIdxSpan(dispatch_mono.func.args);
                if (dispatch_args.len > 0 and try self.monotypesStructurallyEqual(dispatch_args[0], receiver_monotype)) {
                    expected_param_monos = dispatch_args;
                }
            }
        }

        const args_top = self.scratch_expr_ids.top();
        defer self.scratch_expr_ids.clearFrom(args_top);
        try self.scratch_expr_ids.append(receiver);
        for (explicit_args, 0..) |arg_idx, i| {
            if (i + 1 < expected_param_monos.len and !expected_param_monos[i + 1].isNone()) {
                try self.bindTypeVarMonotypes(ModuleEnv.varFrom(arg_idx), expected_param_monos[i + 1]);
            }
            const arg = try self.lowerExpr(arg_idx);
            try self.scratch_expr_ids.append(arg);
        }
        const lowered_call_args = self.scratch_expr_ids.sliceFromStart(args_top);
        const args = try self.store.addExprSpan(self.allocator, lowered_call_args);

        const mono_top = self.mono_scratches.idxs.top();
        defer self.mono_scratches.idxs.clearFrom(mono_top);
        try self.mono_scratches.idxs.append(receiver_monotype);
        for (lowered_call_args[1..]) |arg_expr| {
            try self.mono_scratches.idxs.append(self.store.typeOf(arg_expr));
        }
        const func_monotype = try self.buildFuncMonotype(self.mono_scratches.idxs.sliceFromStart(mono_top), monotype, false);

        // Ensure the method body is lowered so codegen can find it.
        const lowered_method_symbol = try self.ensureMethodLowered(method_symbol, func_monotype);
        const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = lowered_method_symbol }, func_monotype, region);

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

    const ProvidedField = struct {
        name: Ident.Idx,
        expr: MIR.ExprId,
    };
    const ExtensionBinding = struct {
        pattern: MIR.PatternId,
        expr: MIR.ExprId,
        lookup: MIR.ExprId,
    };

    var provided_fields = std.ArrayList(ProvidedField).empty;
    defer provided_fields.deinit(self.allocator);
    var extension_binding: ?ExtensionBinding = null;

    for (cir_field_indices) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        const expr = try self.lowerExpr(field.value);
        try provided_fields.append(self.allocator, .{
            .name = field.name,
            .expr = expr,
        });
    }

    const exprs_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(exprs_top);
    const names_top = self.scratch_ident_idxs.top();
    defer self.scratch_ident_idxs.clearFrom(names_top);

    if (record.ext) |ext_expr_idx| {
        const ext_expr = try self.lowerExpr(ext_expr_idx);
        const ext_expr_mono = self.store.typeOf(ext_expr);

        // Bind the update base once so:
        // 1) `{ ..expr, all_fields_overridden }` still evaluates `expr`, and
        // 2) synthesized field accesses never re-evaluate `expr`.
        const ext_base_symbol = try self.internSymbol(self.current_module_idx, Ident.Idx.NONE);
        const ext_symbol = try self.makeSyntheticSymbol(ext_base_symbol);
        const ext_pattern = try self.store.addPattern(self.allocator, .{ .bind = ext_symbol }, ext_expr_mono);
        const ext_lookup = try self.store.addExpr(self.allocator, .{ .lookup = ext_symbol }, ext_expr_mono, region);
        extension_binding = .{
            .pattern = ext_pattern,
            .expr = ext_expr,
            .lookup = ext_lookup,
        };

        const mono = self.store.monotype_store.getMonotype(monotype);
        const mono_record = switch (mono) {
            .record => |r| r,
            else => unreachable,
        };
        const mono_fields = self.store.monotype_store.getFields(mono_record.fields);

        // Record update: include all fields in the resulting record.
        // Updated fields use explicit expressions; missing fields become accesses on the
        // base record expression from `..record`.
        for (mono_fields) |mono_field| {
            var maybe_expr: ?MIR.ExprId = null;
            for (provided_fields.items) |provided| {
                if (provided.name.eql(mono_field.name)) {
                    maybe_expr = provided.expr;
                    break;
                }
            }

            const field_expr = maybe_expr orelse try self.store.addExpr(self.allocator, .{ .record_access = .{
                .record = extension_binding.?.lookup,
                .field_name = mono_field.name,
            } }, mono_field.type_idx, region);

            try self.scratch_expr_ids.append(field_expr);
            try self.scratch_ident_idxs.append(mono_field.name);
        }
    } else {
        for (provided_fields.items) |provided| {
            try self.scratch_expr_ids.append(provided.expr);
            try self.scratch_ident_idxs.append(provided.name);
        }
    }

    const fields_span = try self.store.addExprSpan(self.allocator, self.scratch_expr_ids.sliceFromStart(exprs_top));
    const names_span = try self.store.addFieldNameSpan(self.allocator, self.scratch_ident_idxs.sliceFromStart(names_top));

    const record_expr = try self.store.addExpr(self.allocator, .{ .record = .{
        .fields = fields_span,
        .field_names = names_span,
    } }, monotype, region);

    if (extension_binding) |ext| {
        const stmts = try self.store.addStmts(self.allocator, &.{MIR.Stmt{
            .decl_const = .{
                .pattern = ext.pattern,
                .expr = ext.expr,
            },
        }});

        return try self.store.addExpr(self.allocator, .{ .block = .{
            .stmts = stmts,
            .final_expr = record_expr,
        } }, monotype, region);
    }

    return record_expr;
}

// --- Type var dispatch & cross-module resolution ---

/// Lower `e_type_var_dispatch` using checker-resolved dispatch target.
fn lowerTypeVarDispatch(self: *Self, module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, tvd: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const resolved_target = self.lookupResolvedDispatchTarget(expr_idx) orelse {
        if (std.debug.runtime_safety) {
            const method_name = module_env.getIdent(tvd.method_name);
            std.debug.panic("lowerTypeVarDispatch: missing resolved dispatch target for method '{s}'", .{method_name});
        }
        unreachable;
    };
    const method_symbol = try self.resolvedDispatchTargetToSymbol(module_env, resolved_target);

    const cir_args = module_env.store.sliceExpr(tvd.args);
    const mono_top = self.mono_scratches.idxs.top();
    defer self.mono_scratches.idxs.clearFrom(mono_top);
    for (cir_args) |arg_idx| {
        try self.mono_scratches.idxs.append(try self.resolveMonotype(arg_idx));
    }
    const func_monotype = try self.buildFuncMonotype(self.mono_scratches.idxs.sliceFromStart(mono_top), monotype, false);

    const args = try self.lowerExprSpan(module_env, tvd.args);
    const lowered_method_symbol = try self.ensureMethodLowered(method_symbol, func_monotype);
    const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = lowered_method_symbol }, func_monotype, region);

    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = func_expr,
        .args = args,
    } }, monotype, region);
}

/// Find the module index for a given origin module ident.
fn findModuleForOrigin(self: *Self, source_env: *const ModuleEnv, origin_module: Ident.Idx) u32 {
    // Check if origin is source_env itself
    if (origin_module.eql(source_env.qualified_module_ident)) {
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

fn lookupResolvedDispatchTarget(self: *const Self, expr_idx: CIR.Expr.Idx) ?ResolvedDispatchTarget {
    const key = (@as(u64, self.current_module_idx) << 32) | @as(u64, @intFromEnum(expr_idx));
    return self.resolved_dispatch_targets.get(key);
}

fn resolvedDispatchTargetToSymbol(self: *Self, source_env: *const ModuleEnv, target: ResolvedDispatchTarget) Allocator.Error!MIR.Symbol {
    const target_module_idx = self.findModuleForOrigin(source_env, target.origin);
    return self.internSymbol(@intCast(target_module_idx), target.method_ident);
}

fn findDefExprBySymbol(self: *Self, module_idx: u32, ident_idx: Ident.Idx) ?CIR.Expr.Idx {
    const target_env = self.all_module_envs[module_idx];

    if (target_env.getExposedNodeIndexById(ident_idx)) |node_idx| {
        if (target_env.store.isDefNode(node_idx)) {
            const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
            return target_env.store.getDef(def_idx).expr;
        }
    }

    const defs = target_env.store.sliceDefs(target_env.all_defs);
    for (defs) |def_idx| {
        const def = target_env.store.getDef(def_idx);
        const pat = target_env.store.getPattern(def.pattern);
        switch (pat) {
            .assign => |assign| {
                if (assign.ident.eql(ident_idx)) return def.expr;
            },
            else => {},
        }
    }
    return null;
}

/// Ensure a method definition is lowered (for cross-module dispatch),
/// returning the symbol to call. For polymorphic re-specialization this may
/// be a synthetic symbol unique to (method symbol, caller monotype).
fn ensureMethodLowered(self: *Self, symbol: MIR.Symbol, caller_func_monotype: ?Monotype.Idx) Allocator.Error!MIR.Symbol {
    const symbol_key: u64 = @bitCast(symbol);
    const symbol_meta = self.getSymbolMetadata(symbol);

    if (caller_func_monotype) |caller_monotype| {
        if (!caller_monotype.isNone()) {
            const spec_key = polySpecKey(symbol_key, caller_monotype);

            if (try self.lookupPolySpecialization(symbol_key, caller_monotype)) |spec_symbol| {
                return spec_symbol;
            }

            // Recursive lowering for a different caller monotype needs a new
            // specialization symbol; placeholders are only valid for the same
            // active specialization.
            if (self.in_progress_defs.contains(symbol_key)) {
                const active_monotype = self.in_progress_symbol_monotypes.get(symbol_key) orelse Monotype.Idx.none;
                const same_specialization = if (active_monotype.isNone())
                    false
                else
                    try self.monotypesStructurallyEqual(active_monotype, caller_monotype);

                if (!same_specialization) {
                    if (self.findDefExprBySymbol(symbol_meta.module_idx, symbol_meta.ident_idx)) |def_expr| {
                        const spec_symbol = try self.makeSyntheticSymbol(symbol);
                        try self.poly_specializations.put(spec_key, spec_symbol);
                        errdefer _ = self.poly_specializations.remove(spec_key);
                        _ = try self.lowerExternalDefWithType(spec_symbol, def_expr, caller_monotype);
                        return spec_symbol;
                    }

                    return symbol;
                }
            }

            if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                const cached_monotype = self.store.typeOf(cached_expr);
                if (!try self.monotypesStructurallyEqual(cached_monotype, caller_monotype)) {
                    if (self.findDefExprBySymbol(symbol_meta.module_idx, symbol_meta.ident_idx)) |def_expr| {
                        const spec_symbol = try self.makeSyntheticSymbol(symbol);
                        try self.poly_specializations.put(spec_key, spec_symbol);
                        errdefer _ = self.poly_specializations.remove(spec_key);
                        _ = try self.lowerExternalDefWithType(spec_symbol, def_expr, caller_monotype);
                        return spec_symbol;
                    }

                    return symbol;
                }
            }
        }
    }

    if (self.lowered_symbols.contains(symbol_key)) return symbol;

    if (self.findDefExprBySymbol(symbol_meta.module_idx, symbol_meta.ident_idx)) |def_expr| {
        _ = try self.lowerExternalDefWithType(symbol, def_expr, caller_func_monotype);
    }

    // Method not found in target module's defs. This can happen for same-module
    // methods where the definition is accessible through other means (e.g., via
    // e_lookup_local processing when the call func expression is lowered).
    return symbol;
}

/// Lower an external definition by symbol, caching the result.
pub fn lowerExternalDef(self: *Self, symbol: MIR.Symbol, cir_expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ExprId {
    return self.lowerExternalDefWithType(symbol, cir_expr_idx, null);
}

/// Lower an external definition, with an optional caller monotype for
/// cross-module polymorphism resolution. When the caller provides a concrete
/// monotype, flex type variables in the target module are pre-seeded so they
/// resolve to the caller's concrete types instead of unit/ZST.
fn lowerExternalDefWithType(self: *Self, symbol: MIR.Symbol, cir_expr_idx: CIR.Expr.Idx, caller_monotype: ?Monotype.Idx) Allocator.Error!MIR.ExprId {
    const symbol_key: u64 = @bitCast(symbol);
    const symbol_meta = self.getSymbolMetadata(symbol);

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
    const active_monotype: Monotype.Idx = if (caller_monotype) |cm| cm else Monotype.Idx.none;
    try self.in_progress_symbol_monotypes.put(symbol_key, active_monotype);
    errdefer _ = self.in_progress_symbol_monotypes.remove(symbol_key);

    // Switch module context if needed.
    const switching_module = symbol_meta.module_idx != self.current_module_idx;
    const saved_module_idx = self.current_module_idx;
    const saved_pattern_scope = self.current_pattern_scope;
    const saved_types_store = self.types_store;
    const saved_type_var_seen = self.type_var_seen;
    const saved_ident_store = self.mono_scratches.ident_store;
    self.current_pattern_scope = symbol_key;
    if (switching_module) {
        self.current_module_idx = symbol_meta.module_idx;
        self.types_store = &self.all_module_envs[symbol_meta.module_idx].types;
        self.mono_scratches.ident_store = self.all_module_envs[symbol_meta.module_idx].getIdentStoreConst();
    }

    // Always isolate type_var_seen per external definition lowering.
    // Reusing a shared cache across polymorphic specializations can pin flex
    // and rigid vars to an earlier specialization.
    self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);

    // Pre-bind type_var_seen with the caller's concrete types so that
    // flex/rigid vars in the definition resolve to concrete monotypes instead
    // of unit. Each definition has its own type vars (not unified with
    // the caller's instantiated copies), so they must be seeded here.
    // This must happen for BOTH cross-module AND same-module calls, because
    // same-module polymorphic functions (e.g., List.append called from
    // range_to within the Builtin module) have their own rigid type vars
    // from type annotations that are not unified with the caller's vars.
    if (caller_monotype) |cmt| {
        if (!cmt.isNone()) {
            const target_type_var = ModuleEnv.varFrom(cir_expr_idx);
            try self.bindTypeVarMonotypes(target_type_var, cmt);
        }
    }
    defer {
        self.type_var_seen.deinit();
        self.type_var_seen = saved_type_var_seen;
        self.current_pattern_scope = saved_pattern_scope;
        if (switching_module) {
            self.types_store = saved_types_store;
            self.current_module_idx = saved_module_idx;
            self.mono_scratches.ident_store = saved_ident_store;
        }
    }

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
    _ = self.in_progress_symbol_monotypes.remove(symbol_key);

    return result;
}

fn typeBindingInvariant(comptime fmt: []const u8, args: anytype) noreturn {
    if (std.debug.runtime_safety) {
        std.debug.panic(fmt, args);
    }
    unreachable;
}

fn builtinPrimForNominal(ident: Ident.Idx, common: ModuleEnv.CommonIdents) ?Monotype.Prim {
    if (ident.eql(common.bool)) return .bool;
    if (ident.eql(common.str)) return .str;
    if (ident.eql(common.u8_type)) return .u8;
    if (ident.eql(common.i8_type)) return .i8;
    if (ident.eql(common.u16_type)) return .u16;
    if (ident.eql(common.i16_type)) return .i16;
    if (ident.eql(common.u32_type)) return .u32;
    if (ident.eql(common.i32_type)) return .i32;
    if (ident.eql(common.u64_type)) return .u64;
    if (ident.eql(common.i64_type)) return .i64;
    if (ident.eql(common.u128_type)) return .u128;
    if (ident.eql(common.i128_type)) return .i128;
    if (ident.eql(common.f32_type)) return .f32;
    if (ident.eql(common.f64_type)) return .f64;
    if (ident.eql(common.dec_type)) return .dec;
    return null;
}

fn bindRecordFieldByName(
    self: *Self,
    field_name: Ident.Idx,
    field_var: types.Var,
    mono_fields: []const Monotype.Field,
) Allocator.Error!void {
    for (mono_fields) |mono_field| {
        if (mono_field.name.eql(field_name)) {
            try self.bindTypeVarMonotypes(field_var, mono_field.type_idx);
            return;
        }
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    typeBindingInvariant(
        "bindFlatTypeMonotypes(record): field '{s}' missing from monotype",
        .{module_env.getIdent(field_name)},
    );
}

fn bindTagPayloadsByName(
    self: *Self,
    tag_name: Ident.Idx,
    payload_vars: []const types.Var,
    mono_tags: []const Monotype.Tag,
) Allocator.Error!void {
    for (mono_tags) |mono_tag| {
        if (!mono_tag.name.eql(tag_name)) continue;

        const mono_payloads = self.store.monotype_store.getIdxSpan(mono_tag.payloads);
        if (payload_vars.len != mono_payloads.len) {
            const module_env = self.all_module_envs[self.current_module_idx];
            typeBindingInvariant(
                "bindFlatTypeMonotypes(tag_union): payload arity mismatch for tag '{s}'",
                .{module_env.getIdent(tag_name)},
            );
        }
        for (payload_vars, mono_payloads) |payload_var, mono_payload| {
            try self.bindTypeVarMonotypes(payload_var, mono_payload);
        }
        return;
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    typeBindingInvariant(
        "bindFlatTypeMonotypes(tag_union): tag '{s}' missing from monotype",
        .{module_env.getIdent(tag_name)},
    );
}

/// Bind concrete monotypes to polymorphic vars for the current lowering scope.
fn bindTypeVarMonotypes(self: *Self, type_var: types.Var, monotype: Monotype.Idx) Allocator.Error!void {
    if (monotype.isNone()) return;

    const resolved = self.types_store.resolveVar(type_var);

    if (self.type_var_seen.get(resolved.var_)) |existing| {
        if (!(try self.monotypesStructurallyEqual(existing, monotype))) {
            typeBindingInvariant(
                "bindTypeVarMonotypes: conflicting monotype binding for type var root {d}",
                .{@intFromEnum(resolved.var_)},
            );
        }
        return;
    }

    switch (resolved.desc.content) {
        .flex, .rigid => {
            try self.type_var_seen.put(resolved.var_, monotype);
        },
        .alias => |alias| {
            const backing_var = self.types_store.getAliasBackingVar(alias);
            try self.bindTypeVarMonotypes(backing_var, monotype);
        },
        .structure => |flat_type| {
            // Register before recursing so recursive structures short-circuit.
            try self.type_var_seen.put(resolved.var_, monotype);
            try self.bindFlatTypeMonotypes(flat_type, monotype);
        },
        .err => {},
    }
}

fn bindFlatTypeMonotypes(self: *Self, flat_type: types.FlatType, monotype: Monotype.Idx) Allocator.Error!void {
    if (monotype.isNone()) return;

    const mono = self.store.monotype_store.getMonotype(monotype);
    const module_env = self.all_module_envs[self.current_module_idx];

    switch (flat_type) {
        .fn_pure, .fn_effectful, .fn_unbound => |func| {
            const mfunc = switch (mono) {
                .func => |mfunc| mfunc,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(fn): expected function monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const type_args = self.types_store.sliceVars(func.args);
            const mono_args = self.store.monotype_store.getIdxSpan(mfunc.args);
            if (type_args.len != mono_args.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(fn): arity mismatch (type={d}, monotype={d})",
                    .{ type_args.len, mono_args.len },
                );
            }
            for (type_args, mono_args) |ta, ma| {
                try self.bindTypeVarMonotypes(ta, ma);
            }
            try self.bindTypeVarMonotypes(func.ret, mfunc.ret);
        },
        .nominal_type => |nominal| {
            const common = self.currentCommonIdents();
            const ident = nominal.ident.ident_idx;
            const origin = nominal.origin_module;

            if (origin.eql(common.builtin_module) and ident.eql(common.list)) {
                const mlist = switch (mono) {
                    .list => |mlist| mlist,
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal List): expected list monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                const type_args = self.types_store.sliceNominalArgs(nominal);
                if (type_args.len != 1) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal List): expected exactly 1 type arg, found {d}",
                        .{type_args.len},
                    );
                }
                try self.bindTypeVarMonotypes(type_args[0], mlist.elem);
                return;
            }
            if (origin.eql(common.builtin_module) and ident.eql(common.box)) {
                const mbox = switch (mono) {
                    .box => |mbox| mbox,
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal Box): expected box monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                const type_args = self.types_store.sliceNominalArgs(nominal);
                if (type_args.len != 1) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal Box): expected exactly 1 type arg, found {d}",
                        .{type_args.len},
                    );
                }
                try self.bindTypeVarMonotypes(type_args[0], mbox.inner);
                return;
            }

            if (origin.eql(common.builtin_module)) {
                if (builtinPrimForNominal(ident, common)) |expected_prim| {
                    switch (mono) {
                        .prim => |actual_prim| {
                            if (actual_prim != expected_prim) {
                                typeBindingInvariant(
                                    "bindFlatTypeMonotypes(nominal prim): expected '{s}', found '{s}'",
                                    .{ @tagName(expected_prim), @tagName(actual_prim) },
                                );
                            }
                        },
                        else => typeBindingInvariant(
                            "bindFlatTypeMonotypes(nominal prim): expected prim monotype, found '{s}'",
                            .{@tagName(mono)},
                        ),
                    }
                    return;
                }
            }

            // Non-builtin nominals (and non-primitive builtin nominals) resolve by backing var.
            const backing_var = self.types_store.getNominalBackingVar(nominal);
            try self.bindTypeVarMonotypes(backing_var, monotype);
        },
        .record => |record| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(record): expected record monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const mono_fields = self.store.monotype_store.getFields(mrec.fields);
            const seen_top = self.scratch_ident_idxs.top();
            defer self.scratch_ident_idxs.clearFrom(seen_top);

            var current_row = record;
            rows: while (true) {
                const fields_slice = self.types_store.getRecordFieldsSlice(current_row.fields);
                const field_names = fields_slice.items(.name);
                const field_vars = fields_slice.items(.var_);

                for (field_names, field_vars) |field_name, field_var| {
                    var seen_name = false;
                    for (self.scratch_ident_idxs.sliceFromStart(seen_top)) |existing_name| {
                        if (existing_name.eql(field_name)) {
                            seen_name = true;
                            break;
                        }
                    }
                    if (!seen_name) {
                        try self.scratch_ident_idxs.append(field_name);
                    }
                    try self.bindRecordFieldByName(field_name, field_var, mono_fields);
                }

                var ext_var = current_row.ext;
                while (true) {
                    const ext_resolved = self.types_store.resolveVar(ext_var);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            ext_var = self.types_store.getAliasBackingVar(alias);
                            continue;
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue :rows;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = self.types_store.getRecordFieldsSlice(fields_range);
                                const ext_field_names = ext_fields.items(.name);
                                const ext_field_vars = ext_fields.items(.var_);
                                for (ext_field_names, ext_field_vars) |field_name, field_var| {
                                    var seen_name = false;
                                    for (self.scratch_ident_idxs.sliceFromStart(seen_top)) |existing_name| {
                                        if (existing_name.eql(field_name)) {
                                            seen_name = true;
                                            break;
                                        }
                                    }
                                    if (!seen_name) {
                                        try self.scratch_ident_idxs.append(field_name);
                                    }
                                    try self.bindRecordFieldByName(field_name, field_var, mono_fields);
                                }
                                break :rows;
                            },
                            .empty_record => break :rows,
                            else => typeBindingInvariant(
                                "bindFlatTypeMonotypes(record): unexpected ext flat type '{s}'",
                                .{@tagName(ext_flat)},
                            ),
                        },
                        .flex => typeBindingInvariant(
                            "bindFlatTypeMonotypes(record): unresolved flex extension",
                            .{},
                        ),
                        .rigid => typeBindingInvariant(
                            "bindFlatTypeMonotypes(record): unresolved rigid extension",
                            .{},
                        ),
                        .err => typeBindingInvariant(
                            "bindFlatTypeMonotypes(record): error extension",
                            .{},
                        ),
                    }
                }
            }

            for (mono_fields) |mono_field| {
                var found = false;
                for (self.scratch_ident_idxs.sliceFromStart(seen_top)) |seen_name| {
                    if (seen_name.eql(mono_field.name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(record): monotype field '{s}' missing from type row",
                        .{module_env.getIdent(mono_field.name)},
                    );
                }
            }
        },
        .record_unbound => |fields_range| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(record_unbound): expected record monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const fields_slice = self.types_store.getRecordFieldsSlice(fields_range);
            const field_names = fields_slice.items(.name);
            const field_vars = fields_slice.items(.var_);
            const mono_fields = self.store.monotype_store.getFields(mrec.fields);

            if (field_names.len != mono_fields.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(record_unbound): field count mismatch (type={d}, monotype={d})",
                    .{ field_names.len, mono_fields.len },
                );
            }

            for (field_names, field_vars) |field_name, field_var| {
                try self.bindRecordFieldByName(field_name, field_var, mono_fields);
            }
        },
        .tuple => |tuple| {
            const mtuple = switch (mono) {
                .tuple => |mtuple| mtuple,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(tuple): expected tuple monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const elem_vars = self.types_store.sliceVars(tuple.elems);
            const mono_elems = self.store.monotype_store.getIdxSpan(mtuple.elems);
            if (elem_vars.len != mono_elems.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(tuple): arity mismatch (type={d}, monotype={d})",
                    .{ elem_vars.len, mono_elems.len },
                );
            }
            for (elem_vars, mono_elems) |ev, me| {
                try self.bindTypeVarMonotypes(ev, me);
            }
        },
        .tag_union => |tag_union_row| {
            const mono_tags = switch (mono) {
                .tag_union => |mtu| self.store.monotype_store.getTags(mtu.tags),
                .prim => |p| {
                    if (p == .bool) {
                        const common = self.currentCommonIdents();
                        var found_true = false;
                        var found_false = false;
                        var current_row = tag_union_row;
                        rows: while (true) {
                            const type_tags = self.types_store.getTagsSlice(current_row.tags);
                            const type_tag_names = type_tags.items(.name);
                            const type_tag_args = type_tags.items(.args);
                            for (type_tag_names, type_tag_args) |tag_name, tag_args| {
                                const payload_vars = self.types_store.sliceVars(tag_args);
                                if (payload_vars.len != 0) {
                                    typeBindingInvariant(
                                        "bindFlatTypeMonotypes(tag_union Bool): non-empty payload for tag '{s}'",
                                        .{module_env.getIdent(tag_name)},
                                    );
                                }
                                if (tag_name.eql(common.true_tag)) {
                                    found_true = true;
                                } else if (tag_name.eql(common.false_tag)) {
                                    found_false = true;
                                } else {
                                    typeBindingInvariant(
                                        "bindFlatTypeMonotypes(tag_union Bool): unexpected tag '{s}'",
                                        .{module_env.getIdent(tag_name)},
                                    );
                                }
                            }

                            var ext_var = current_row.ext;
                            while (true) {
                                const ext_resolved = self.types_store.resolveVar(ext_var);
                                switch (ext_resolved.desc.content) {
                                    .alias => |alias| {
                                        ext_var = self.types_store.getAliasBackingVar(alias);
                                        continue;
                                    },
                                    .structure => |ext_flat| switch (ext_flat) {
                                        .tag_union => |next_row| {
                                            current_row = next_row;
                                            continue :rows;
                                        },
                                        .empty_tag_union => break :rows,
                                        else => typeBindingInvariant(
                                            "bindFlatTypeMonotypes(tag_union Bool): unexpected ext flat type '{s}'",
                                            .{@tagName(ext_flat)},
                                        ),
                                    },
                                    .flex => typeBindingInvariant(
                                        "bindFlatTypeMonotypes(tag_union Bool): unresolved flex extension",
                                        .{},
                                    ),
                                    .rigid => typeBindingInvariant(
                                        "bindFlatTypeMonotypes(tag_union Bool): unresolved rigid extension",
                                        .{},
                                    ),
                                    .err => typeBindingInvariant(
                                        "bindFlatTypeMonotypes(tag_union Bool): error extension",
                                        .{},
                                    ),
                                }
                            }
                        }
                        if (!(found_true and found_false)) {
                            typeBindingInvariant(
                                "bindFlatTypeMonotypes(tag_union Bool): missing True/False tags",
                                .{},
                            );
                        }
                        return;
                    }
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(tag_union): expected tag_union/bool monotype, found prim '{s}'",
                        .{@tagName(p)},
                    );
                },
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(tag_union): expected tag_union monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };

            const seen_top = self.scratch_ident_idxs.top();
            defer self.scratch_ident_idxs.clearFrom(seen_top);

            var current_row = tag_union_row;
            rows: while (true) {
                const type_tags = self.types_store.getTagsSlice(current_row.tags);
                const type_tag_names = type_tags.items(.name);
                const type_tag_args = type_tags.items(.args);

                for (type_tag_names, type_tag_args) |tag_name, tag_args| {
                    var seen_tag = false;
                    for (self.scratch_ident_idxs.sliceFromStart(seen_top)) |existing_name| {
                        if (existing_name.eql(tag_name)) {
                            seen_tag = true;
                            break;
                        }
                    }
                    if (!seen_tag) {
                        try self.scratch_ident_idxs.append(tag_name);
                    }
                    const payload_vars = self.types_store.sliceVars(tag_args);
                    try self.bindTagPayloadsByName(tag_name, payload_vars, mono_tags);
                }

                var ext_var = current_row.ext;
                while (true) {
                    const ext_resolved = self.types_store.resolveVar(ext_var);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            ext_var = self.types_store.getAliasBackingVar(alias);
                            continue;
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .tag_union => |next_row| {
                                current_row = next_row;
                                continue :rows;
                            },
                            .empty_tag_union => break :rows,
                            else => typeBindingInvariant(
                                "bindFlatTypeMonotypes(tag_union): unexpected ext flat type '{s}'",
                                .{@tagName(ext_flat)},
                            ),
                        },
                        .flex => typeBindingInvariant(
                            "bindFlatTypeMonotypes(tag_union): unresolved flex extension",
                            .{},
                        ),
                        .rigid => typeBindingInvariant(
                            "bindFlatTypeMonotypes(tag_union): unresolved rigid extension",
                            .{},
                        ),
                        .err => typeBindingInvariant(
                            "bindFlatTypeMonotypes(tag_union): error extension",
                            .{},
                        ),
                    }
                }
            }

            for (mono_tags) |mono_tag| {
                var found = false;
                for (self.scratch_ident_idxs.sliceFromStart(seen_top)) |seen_name| {
                    if (seen_name.eql(mono_tag.name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(tag_union): monotype tag '{s}' missing from type row",
                        .{module_env.getIdent(mono_tag.name)},
                    );
                }
            }
        },
        .empty_record => {
            switch (mono) {
                .unit => {},
                .record => |mrec| {
                    const fields = self.store.monotype_store.getFields(mrec.fields);
                    if (fields.len != 0) {
                        typeBindingInvariant(
                            "bindFlatTypeMonotypes(empty_record): expected zero record fields, found {d}",
                            .{fields.len},
                        );
                    }
                },
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(empty_record): expected unit/empty-record monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            }
        },
        .empty_tag_union => {
            const mono_tags = switch (mono) {
                .tag_union => |mtu| self.store.monotype_store.getTags(mtu.tags),
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(empty_tag_union): expected empty tag union monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            if (mono_tags.len != 0) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(empty_tag_union): expected zero tags, found {d}",
                    .{mono_tags.len},
                );
            }
        },
    }
}

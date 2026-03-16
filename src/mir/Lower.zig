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
//! - `e_closure` → lifted top-level lambda + captures tuple at use site
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
    pattern_idx: CIR.Pattern.Idx,
    cir_expr: CIR.Expr.Idx,
    module_idx: u32,
    symbol: MIR.Symbol,
};

const ResolvedDispatchTarget = struct {
    origin: Ident.Idx,
    method_ident: Ident.Idx,
    fn_var: types.Var,
};

const SymbolMetadata = union(enum) {
    local_ident: struct {
        module_idx: u32,
        ident_idx: Ident.Idx,
    },
    external_def: struct {
        module_idx: u32,
        def_node_idx: u16,
        display_ident_idx: Ident.Idx,
    },
};

const PatternBinding = struct {
    ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
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

/// Specialization bindings: maps polymorphic type vars to concrete monotypes.
/// Written by `bindTypeVarMonotypes`, read by `fromTypeVar`.
type_var_seen: std.AutoHashMap(types.Var, Monotype.Idx),

/// Cycle breakers for recursive nominal types (e.g. Tree := [Leaf, Node(Tree)]).
/// Used only by `fromNominalType` during monotype construction; separate from
/// specialization bindings so monotype construction never pollutes them.
nominal_cycle_breakers: std.AutoHashMap(types.Var, Monotype.Idx),

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
/// Key is CIR Pattern.Idx (as u32). Value holds the CIR expression and symbol.
/// Populated by `lowerBlock`, consumed by `e_lookup_local`.
deferred_block_lambdas: std.AutoHashMap(u32, DeferredBlockLambda),

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
scratch_capture_bindings: base.Scratch(MIR.CaptureBinding),
mono_scratches: Monotype.Store.Scratches,

// --- Init/Deinit ---

pub fn init(
    allocator: Allocator,
    store: *MIR.Store,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
) Allocator.Error!Self {
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
        .nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .lowered_symbols = std.AutoHashMap(u64, MIR.ExprId).init(allocator),
        .poly_specializations = std.AutoHashMap(u128, MIR.Symbol).init(allocator),
        .symbol_metadata = std.AutoHashMap(u64, SymbolMetadata).init(allocator),
        .next_synthetic_ident = Ident.Idx.NONE.idx - 1,
        .in_progress_defs = std.AutoHashMap(u64, void).init(allocator),
        .in_progress_symbol_monotypes = std.AutoHashMap(u64, Monotype.Idx).init(allocator),
        .deferred_block_lambdas = std.AutoHashMap(u32, DeferredBlockLambda).init(allocator),
        .resolved_dispatch_targets = resolved_dispatch_targets,
        .scratch_expr_ids = try base.Scratch(MIR.ExprId).init(allocator),
        .scratch_pattern_ids = try base.Scratch(MIR.PatternId).init(allocator),
        .scratch_ident_idxs = try base.Scratch(Ident.Idx).init(allocator),
        .scratch_branches = try base.Scratch(MIR.Branch).init(allocator),
        .scratch_branch_patterns = try base.Scratch(MIR.BranchPattern).init(allocator),
        .scratch_stmts = try base.Scratch(MIR.Stmt).init(allocator),
        .scratch_captures = try base.Scratch(MIR.Capture).init(allocator),
        .scratch_capture_bindings = try base.Scratch(MIR.CaptureBinding).init(allocator),
        .mono_scratches = blk: {
            var ms = try Monotype.Store.Scratches.init(allocator);
            ms.ident_store = all_module_envs[current_module_idx].getIdentStoreConst();
            ms.module_env = all_module_envs[current_module_idx];
            ms.all_module_envs = all_module_envs;
            break :blk ms;
        },
    };
}

pub fn deinit(self: *Self) void {
    self.pattern_symbols.deinit();
    self.type_var_seen.deinit();
    self.nominal_cycle_breakers.deinit();
    self.lowered_symbols.deinit();
    self.poly_specializations.deinit();
    self.symbol_metadata.deinit();
    self.in_progress_defs.deinit();
    self.in_progress_symbol_monotypes.deinit();
    self.deferred_block_lambdas.deinit();
    self.resolved_dispatch_targets.deinit();
    self.scratch_expr_ids.deinit();
    self.scratch_pattern_ids.deinit();
    self.scratch_ident_idxs.deinit();
    self.scratch_branches.deinit();
    self.scratch_branch_patterns.deinit();
    self.scratch_stmts.deinit();
    self.scratch_captures.deinit();
    self.scratch_capture_bindings.deinit();
    self.mono_scratches.deinit();
}

const symbol_namespace_local: u64 = 0;
const symbol_namespace_external_def: u64 = 1;

fn packLocalSymbolId(module_idx: u32, ident_idx: Ident.Idx) u64 {
    if (builtin.mode == .Debug) std.debug.assert(module_idx <= std.math.maxInt(u31));
    const ident_bits: u32 = @bitCast(ident_idx);
    return (symbol_namespace_local << 63) | (@as(u64, module_idx) << 32) | @as(u64, ident_bits);
}

fn packExternalDefSymbolId(module_idx: u32, def_node_idx: u16) u64 {
    if (builtin.mode == .Debug) std.debug.assert(module_idx <= std.math.maxInt(u31));
    return (symbol_namespace_external_def << 63) | (@as(u64, module_idx) << 32) | @as(u64, def_node_idx);
}

fn symbolMetadataModuleIdx(meta: SymbolMetadata) u32 {
    return switch (meta) {
        .local_ident => |m| m.module_idx,
        .external_def => |m| m.module_idx,
    };
}

fn symbolMetadataDisplayIdent(meta: SymbolMetadata) Ident.Idx {
    return switch (meta) {
        .local_ident => |m| m.ident_idx,
        .external_def => |m| m.display_ident_idx,
    };
}

fn symbolMetadataDisplayName(self: *const Self, meta: SymbolMetadata) []const u8 {
    const module_idx = symbolMetadataModuleIdx(meta);
    return self.all_module_envs[module_idx].getIdent(symbolMetadataDisplayIdent(meta));
}

fn identTextIfOwnedBy(env: *const ModuleEnv, ident: Ident.Idx) ?[]const u8 {
    const ident_store = env.getIdentStoreConst();
    const bytes = ident_store.interner.bytes.items.items;
    const start: usize = @intCast(ident.idx);
    if (start >= bytes.len) return null;

    const tail = bytes[start..];
    const end_rel = std.mem.indexOfScalar(u8, tail, 0) orelse return null;
    const text = tail[0..end_rel];

    const roundtrip = ident_store.findByString(text) orelse return null;
    if (!roundtrip.eql(ident)) return null;
    return text;
}

fn identTextForCompare(self: *const Self, ident: Ident.Idx) ?[]const u8 {
    if (identTextIfOwnedBy(self.all_module_envs[self.current_module_idx], ident)) |text| return text;
    return null;
}

fn identsStructurallyEqual(self: *const Self, lhs: Ident.Idx, rhs: Ident.Idx) bool {
    if (lhs.eql(rhs)) return true;
    const lhs_text = self.identTextForCompare(lhs) orelse return false;
    const rhs_text = self.identTextForCompare(rhs) orelse return false;
    return std.mem.eql(u8, lhs_text, rhs_text);
}

fn identLastSegment(text: []const u8) []const u8 {
    const dot = std.mem.lastIndexOfScalar(u8, text, '.') orelse return text;
    return text[dot + 1 ..];
}

fn identsTagNameEquivalent(self: *const Self, lhs: Ident.Idx, rhs: Ident.Idx) bool {
    if (self.identsStructurallyEqual(lhs, rhs)) return true;

    const lhs_text = self.identTextForCompare(lhs) orelse return false;
    const rhs_text = self.identTextForCompare(rhs) orelse return false;
    return std.mem.eql(u8, identLastSegment(lhs_text), identLastSegment(rhs_text));
}

fn remapIdentBetweenModules(
    self: *Self,
    ident: Ident.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
) Allocator.Error!Ident.Idx {
    if (from_module_idx == to_module_idx) return ident;

    const from_env = self.all_module_envs[from_module_idx];
    const to_env = self.all_module_envs[to_module_idx];

    if (identTextIfOwnedBy(from_env, ident)) |ident_text| {
        if (to_env.common.findIdent(ident_text)) |mapped| return mapped;

        // If the source monotype uses the source module's self type name,
        // map that self-name directly to the target module's self-name.
        if (std.mem.eql(u8, ident_text, from_env.module_name)) {
            if (to_env.common.findIdent(to_env.module_name)) |target_self| return target_self;
            return try to_env.insertIdent(Ident.for_text(to_env.module_name));
        }

        // Structural names (record fields, tag names) can be introduced by the
        // caller module and legitimately absent in the callee module's ident store.
        // Canonicalize them by interning into the target module.
        return try to_env.insertIdent(Ident.for_text(ident_text));
    }

    if (std.debug.runtime_safety) {
        std.debug.panic(
            "remapIdentBetweenModules: source ident {d} not owned by source module {d}",
            .{ ident.idx, from_module_idx },
        );
    }
    unreachable;
}

fn remapMonotypeBetweenModules(
    self: *Self,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone() or from_module_idx == to_module_idx) return monotype;

    var remapped = std.AutoHashMap(Monotype.Idx, Monotype.Idx).init(self.allocator);
    defer remapped.deinit();

    return self.remapMonotypeBetweenModulesRec(
        monotype,
        from_module_idx,
        to_module_idx,
        &remapped,
    );
}

fn remapMonotypeBetweenModulesRec(
    self: *Self,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
    remapped: *std.AutoHashMap(Monotype.Idx, Monotype.Idx),
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone() or from_module_idx == to_module_idx) return monotype;
    if (remapped.get(monotype)) |existing| return existing;

    const mono = self.store.monotype_store.getMonotype(monotype);
    switch (mono) {
        .unit => return self.store.monotype_store.unit_idx,
        .prim => |prim| return self.store.monotype_store.primIdx(prim),
        .recursive_placeholder => {
            if (builtin.mode == .Debug) {
                std.debug.panic("remapMonotypeBetweenModules: unexpected recursive_placeholder", .{});
            }
            unreachable;
        },
        .list, .box, .tuple, .func, .record, .tag_union => {},
    }

    const placeholder = try self.store.monotype_store.addMonotype(self.allocator, .recursive_placeholder);
    try remapped.put(monotype, placeholder);

    const mapped_mono: Monotype.Monotype = switch (mono) {
        .list => |list_mono| .{ .list = .{
            .elem = try self.remapMonotypeBetweenModulesRec(
                list_mono.elem,
                from_module_idx,
                to_module_idx,
                remapped,
            ),
        } },
        .box => |box_mono| .{ .box = .{
            .inner = try self.remapMonotypeBetweenModulesRec(
                box_mono.inner,
                from_module_idx,
                to_module_idx,
                remapped,
            ),
        } },
        .tuple => |tuple_mono| blk: {
            const idx_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(idx_top);

            const elem_span = tuple_mono.elems;
            var elem_i: u32 = 0;
            while (elem_i < @as(u32, elem_span.len)) : (elem_i += 1) {
                const elem_pos_u64 = @as(u64, elem_span.start) + elem_i;
                if (builtin.mode == .Debug and elem_pos_u64 >= self.store.monotype_store.extra_idx.items.len) {
                    std.debug.panic(
                        "remapMonotypeBetweenModulesRec: tuple elem span out of bounds (start={d}, len={d}, i={d}, extra_len={d})",
                        .{ elem_span.start, elem_span.len, elem_i, self.store.monotype_store.extra_idx.items.len },
                    );
                }
                const elem_pos: usize = @intCast(elem_pos_u64);
                const elem_mono: Monotype.Idx = @enumFromInt(self.store.monotype_store.extra_idx.items[elem_pos]);
                try self.mono_scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                    elem_mono,
                    from_module_idx,
                    to_module_idx,
                    remapped,
                ));
            }

            const mapped_elems = try self.store.monotype_store.addIdxSpan(
                self.allocator,
                self.mono_scratches.idxs.sliceFromStart(idx_top),
            );
            break :blk .{ .tuple = .{ .elems = mapped_elems } };
        },
        .func => |func_mono| blk: {
            const idx_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(idx_top);

            const arg_span = func_mono.args;
            var arg_i: u32 = 0;
            while (arg_i < @as(u32, arg_span.len)) : (arg_i += 1) {
                const arg_pos_u64 = @as(u64, arg_span.start) + arg_i;
                if (builtin.mode == .Debug and arg_pos_u64 >= self.store.monotype_store.extra_idx.items.len) {
                    std.debug.panic(
                        "remapMonotypeBetweenModulesRec: func arg span out of bounds (start={d}, len={d}, i={d}, extra_len={d})",
                        .{ arg_span.start, arg_span.len, arg_i, self.store.monotype_store.extra_idx.items.len },
                    );
                }
                const arg_pos: usize = @intCast(arg_pos_u64);
                const arg_mono: Monotype.Idx = @enumFromInt(self.store.monotype_store.extra_idx.items[arg_pos]);
                try self.mono_scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                    arg_mono,
                    from_module_idx,
                    to_module_idx,
                    remapped,
                ));
            }
            const mapped_args = try self.store.monotype_store.addIdxSpan(
                self.allocator,
                self.mono_scratches.idxs.sliceFromStart(idx_top),
            );

            const mapped_ret = try self.remapMonotypeBetweenModulesRec(
                func_mono.ret,
                from_module_idx,
                to_module_idx,
                remapped,
            );

            break :blk .{ .func = .{
                .args = mapped_args,
                .ret = mapped_ret,
                .effectful = func_mono.effectful,
            } };
        },
        .record => |record_mono| blk: {
            const fields_top = self.mono_scratches.fields.top();
            defer self.mono_scratches.fields.clearFrom(fields_top);

            const field_span = record_mono.fields;
            var field_i: u32 = 0;
            while (field_i < @as(u32, field_span.len)) : (field_i += 1) {
                const field_pos_u64 = @as(u64, field_span.start) + field_i;
                if (builtin.mode == .Debug and field_pos_u64 >= self.store.monotype_store.fields.items.len) {
                    std.debug.panic(
                        "remapMonotypeBetweenModulesRec: record field span out of bounds (start={d}, len={d}, i={d}, fields_len={d})",
                        .{ field_span.start, field_span.len, field_i, self.store.monotype_store.fields.items.len },
                    );
                }
                const field_pos: usize = @intCast(field_pos_u64);
                const field = self.store.monotype_store.fields.items[field_pos];
                try self.mono_scratches.fields.append(.{
                    .name = try self.remapIdentBetweenModules(field.name, from_module_idx, to_module_idx),
                    .type_idx = try self.remapMonotypeBetweenModulesRec(
                        field.type_idx,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                    ),
                });
            }

            const mapped_fields = try self.store.monotype_store.addFields(
                self.allocator,
                self.mono_scratches.fields.sliceFromStart(fields_top),
            );
            break :blk .{ .record = .{ .fields = mapped_fields } };
        },
        .tag_union => |tag_union_mono| blk: {
            const tags_top = self.mono_scratches.tags.top();
            defer self.mono_scratches.tags.clearFrom(tags_top);

            const tag_span = tag_union_mono.tags;
            var tag_i: u32 = 0;
            while (tag_i < @as(u32, tag_span.len)) : (tag_i += 1) {
                const tag_pos_u64 = @as(u64, tag_span.start) + tag_i;
                if (builtin.mode == .Debug and tag_pos_u64 >= self.store.monotype_store.tags.items.len) {
                    std.debug.panic(
                        "remapMonotypeBetweenModulesRec: tag span out of bounds (start={d}, len={d}, i={d}, tags_len={d})",
                        .{ tag_span.start, tag_span.len, tag_i, self.store.monotype_store.tags.items.len },
                    );
                }
                const tag_pos: usize = @intCast(tag_pos_u64);
                const tag = self.store.monotype_store.tags.items[tag_pos];

                const payload_top = self.mono_scratches.idxs.top();
                defer self.mono_scratches.idxs.clearFrom(payload_top);

                const payload_span = tag.payloads;
                var payload_i: u32 = 0;
                while (payload_i < @as(u32, payload_span.len)) : (payload_i += 1) {
                    const payload_pos_u64 = @as(u64, payload_span.start) + payload_i;
                    if (builtin.mode == .Debug and payload_pos_u64 >= self.store.monotype_store.extra_idx.items.len) {
                        std.debug.panic(
                            "remapMonotypeBetweenModulesRec: tag payload span out of bounds (start={d}, len={d}, i={d}, extra_len={d}, tag={d})",
                            .{
                                payload_span.start,
                                payload_span.len,
                                payload_i,
                                self.store.monotype_store.extra_idx.items.len,
                                tag.name.idx,
                            },
                        );
                    }
                    const payload_pos: usize = @intCast(payload_pos_u64);
                    const payload_mono: Monotype.Idx = @enumFromInt(self.store.monotype_store.extra_idx.items[payload_pos]);
                    try self.mono_scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                        payload_mono,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                    ));
                }

                const mapped_payloads = try self.store.monotype_store.addIdxSpan(
                    self.allocator,
                    self.mono_scratches.idxs.sliceFromStart(payload_top),
                );
                try self.mono_scratches.tags.append(.{
                    .name = try self.remapIdentBetweenModules(tag.name, from_module_idx, to_module_idx),
                    .payloads = mapped_payloads,
                });
            }

            const mapped_tags = try self.store.monotype_store.addTags(
                self.allocator,
                self.mono_scratches.tags.sliceFromStart(tags_top),
            );
            break :blk .{ .tag_union = .{ .tags = mapped_tags } };
        },
        .unit, .prim, .recursive_placeholder => unreachable,
    };

    self.store.monotype_store.monotypes.items[@intFromEnum(placeholder)] = mapped_mono;
    return placeholder;
}

fn normalizeCallerMonotypeForSymbolModule(
    self: *Self,
    symbol_module_idx: u32,
    caller_monotype: Monotype.Idx,
) Allocator.Error!Monotype.Idx {
    if (caller_monotype.isNone()) return caller_monotype;
    if (self.current_module_idx == symbol_module_idx) return caller_monotype;
    return self.remapMonotypeBetweenModules(
        caller_monotype,
        self.current_module_idx,
        symbol_module_idx,
    );
}

fn internSymbol(self: *Self, namespace_idx: u32, ident_idx: Ident.Idx) Allocator.Error!MIR.Symbol {
    const raw = packLocalSymbolId(namespace_idx, ident_idx);
    const gop = try self.symbol_metadata.getOrPut(raw);
    if (!gop.found_existing) {
        gop.value_ptr.* = .{ .local_ident = .{ .module_idx = namespace_idx, .ident_idx = ident_idx } };
    } else if (builtin.mode == .Debug) {
        switch (gop.value_ptr.*) {
            .local_ident => |existing| {
                if (existing.module_idx != namespace_idx or !existing.ident_idx.eql(ident_idx)) {
                    std.debug.panic(
                        "Local symbol metadata mismatch for raw id {d}: existing module={d} ident={d}, new module={d} ident={d}",
                        .{ raw, existing.module_idx, existing.ident_idx.idx, namespace_idx, ident_idx.idx },
                    );
                }
            },
            .external_def => |existing| std.debug.panic(
                "Symbol namespace mismatch for raw id {d}: existing external def (module={d}, node={d}), new local ident (module={d}, ident={d})",
                .{ raw, existing.module_idx, existing.def_node_idx, namespace_idx, ident_idx.idx },
            ),
        }
    }
    const symbol = MIR.Symbol.fromRaw(raw);
    try self.store.registerSymbolReassignable(self.allocator, symbol, ident_idx.attributes.reassignable);
    return symbol;
}

fn internExternalDefSymbol(self: *Self, module_idx: u32, def_node_idx: u16) Allocator.Error!MIR.Symbol {
    const module_env = self.all_module_envs[module_idx];
    if (!module_env.store.isDefNode(def_node_idx)) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "internExternalDefSymbol: non-def node index {d} for module_idx={d}",
                .{ def_node_idx, module_idx },
            );
        }
        unreachable;
    }

    const def_idx: CIR.Def.Idx = @enumFromInt(def_node_idx);
    const def = module_env.store.getDef(def_idx);
    const pattern = module_env.store.getPattern(def.pattern);
    const display_ident: Ident.Idx = switch (pattern) {
        .assign => |assign| assign.ident,
        .as => |as_pattern| as_pattern.ident,
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "internExternalDefSymbol: expected top-level assign/as pattern for module_idx={d} node={d}, found '{s}'",
                    .{ module_idx, def_node_idx, @tagName(pattern) },
                );
            }
            unreachable;
        },
    };

    const raw = packExternalDefSymbolId(module_idx, def_node_idx);
    const gop = try self.symbol_metadata.getOrPut(raw);
    if (!gop.found_existing) {
        gop.value_ptr.* = .{ .external_def = .{
            .module_idx = module_idx,
            .def_node_idx = def_node_idx,
            .display_ident_idx = display_ident,
        } };
    } else if (builtin.mode == .Debug) {
        switch (gop.value_ptr.*) {
            .external_def => |existing| {
                if (existing.module_idx != module_idx or existing.def_node_idx != def_node_idx or !existing.display_ident_idx.eql(display_ident)) {
                    std.debug.panic(
                        "External def symbol metadata mismatch for raw id {d}: existing module={d} node={d} ident={d}, new module={d} node={d} ident={d}",
                        .{
                            raw,
                            existing.module_idx,
                            existing.def_node_idx,
                            existing.display_ident_idx.idx,
                            module_idx,
                            def_node_idx,
                            display_ident.idx,
                        },
                    );
                }
            },
            .local_ident => |existing| std.debug.panic(
                "Symbol namespace mismatch for raw id {d}: existing local ident (module={d}, ident={d}), new external def (module={d}, node={d})",
                .{ raw, existing.module_idx, existing.ident_idx.idx, module_idx, def_node_idx },
            ),
        }
    }

    const symbol = MIR.Symbol.fromRaw(raw);
    try self.store.registerSymbolReassignable(self.allocator, symbol, display_ident.attributes.reassignable);
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
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(self.allocator, self.currentCommonIdents());
    return try self.store.addExpr(
        self.allocator,
        .{ .tag = .{
            .name = if (value) module_env.idents.true_tag else module_env.idents.false_tag,
            .args = MIR.ExprSpan.empty(),
        } },
        bool_mono,
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

fn registerBoundSymbolDefIfNeeded(self: *Self, pattern: MIR.PatternId, expr: MIR.ExprId) Allocator.Error!void {
    if (self.patternBoundSymbol(pattern)) |symbol| {
        if (self.store.getSymbolDef(symbol) == null) {
            try self.store.registerSymbolDef(self.allocator, symbol, expr);
        }
    }
}

fn debugAssertLookupExpr(self: *Self, expr_id: MIR.ExprId, context: []const u8) void {
    if (!std.debug.runtime_safety) return;

    const expr = self.store.getExpr(expr_id);
    if (expr != .lookup) {
        std.debug.panic(
            "{s}: expected stable lookup operand, got {s}",
            .{ context, @tagName(expr) },
        );
    }
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
        .str => null,
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
    const lowered_arg = try self.lowerExpr(arg_cir_expr);
    const arg_mono = self.store.typeOf(lowered_arg);

    // Evaluate the argument once, then inspect the bound lookup.
    const arg_bind = try self.makeSyntheticBind(arg_mono, false);
    const arg_lookup = try self.emitMirLookup(arg_bind.symbol, arg_mono, region);
    const inspected = try self.lowerStrInspektExpr(module_env, arg_lookup, arg_mono, region);
    try self.registerBoundSymbolDefIfNeeded(arg_bind.pattern, lowered_arg);

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
        .box => |box_data| blk: {
            const common = self.currentCommonIdents();
            const unbox_fn_mono = try self.buildFuncMonotype(&.{mono_idx}, box_data.inner, false);
            const unbox_target: ResolvedDispatchTarget = .{
                .origin = common.builtin_module,
                .method_ident = common.builtin_box_unbox,
                // resolvedDispatchTargetToSymbol only uses origin/method_ident.
                .fn_var = undefined,
            };

            const method_symbol = try self.resolvedDispatchTargetToSymbol(module_env, unbox_target);
            const lowered_method_symbol = try self.specializeMethod(method_symbol, unbox_fn_mono);
            const func_expr = try self.store.addExpr(
                self.allocator,
                .{ .lookup = lowered_method_symbol },
                unbox_fn_mono,
                region,
            );
            const unbox_args = try self.store.addExprSpan(self.allocator, &.{value_expr});
            const unboxed = try self.store.addExpr(
                self.allocator,
                .{ .call = .{ .func = func_expr, .args = unbox_args } },
                box_data.inner,
                region,
            );

            const inner_str = try self.lowerStrInspektExpr(module_env, unboxed, box_data.inner, region);
            const open = try self.emitMirStrLiteral("Box(", region);
            const close = try self.emitMirStrLiteral(")", region);
            break :blk self.foldMirStrConcat(&.{ open, inner_str, close }, region);
        },
        .func => self.emitMirStrLiteral("<function>", region),
        .recursive_placeholder => {
            if (std.debug.runtime_safety) {
                std.debug.panic("recursive_placeholder survived monotype construction", .{});
            }
            unreachable;
        },
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
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(self.allocator, self.currentCommonIdents());
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
        MIR.Stmt{ .mutate_var = .{ .pattern = acc_bind.pattern, .expr = new_acc } },
        MIR.Stmt{ .mutate_var = .{ .pattern = first_bind.pattern, .expr = first_false } },
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
            const symbol_key: u64 = @bitCast(symbol);
            try self.ensureDeferredBlockLambdaLowered(lookup.pattern_idx, symbol, monotype);

            // Ensure the local definition is lowered if it's a top-level def.
            // This is needed so that cross-module lowering (via lowerExternalDef)
            // properly registers all transitively-referenced definitions.
            if (!self.lowered_symbols.contains(symbol_key) and !self.in_progress_defs.contains(symbol_key)) {
                // Find the CIR def for this pattern in the current module
                const defs = module_env.store.sliceDefs(module_env.all_defs);
                for (defs) |def_idx| {
                    const def = module_env.store.getDef(def_idx);
                    if (def.pattern == lookup.pattern_idx) {
                        // Resolve the canonical (unscoped) symbol for this
                        // top-level def, not the potentially-scoped capture
                        // alias from lowerClosure. Inside a closure body,
                        // patternToSymbol returns a capture-local symbol due
                        // to lowerClosure Step 5's override, but the function
                        // def must be lowered under its canonical identity so
                        // it can be shared across all call sites.
                        const saved_scope = self.current_pattern_scope;
                        self.current_pattern_scope = 0;
                        const def_symbol = try self.patternToSymbol(lookup.pattern_idx);
                        self.current_pattern_scope = saved_scope;
                        const def_key: u64 = @bitCast(def_symbol);

                        if (!self.lowered_symbols.contains(def_key) and !self.in_progress_defs.contains(def_key)) {
                            _ = try self.lowerExternalDefWithType(def_symbol, def.expr, monotype);
                        }

                        break;
                    }
                }
            } else if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                // Symbol is already lowered. Check if this is a polymorphic
                // specialization: the same function called with a different type.
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
                        // Check deferred block lambdas from the current block scope.
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
                        const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;
                        self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
                        defer {
                            self.type_var_seen.deinit();
                            self.type_var_seen = saved_type_var_seen;
                            self.nominal_cycle_breakers.deinit();
                            self.nominal_cycle_breakers = saved_nominal_cycle_breakers;
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
                &self.nominal_cycle_breakers,
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
            const target_env = self.all_module_envs[target_module_idx];
            if (!target_env.store.isDefNode(ext.target_node_idx)) {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "e_lookup_external: target node {d} is not a def node (target_module_idx={d})",
                        .{ ext.target_node_idx, target_module_idx },
                    );
                }
                unreachable;
            }

            const symbol = try self.internExternalDefSymbol(target_module_idx, ext.target_node_idx);
            const symbol_meta = self.getSymbolMetadata(symbol);
            const normalized_lookup_monotype = try self.normalizeCallerMonotypeForSymbolModule(
                symbolMetadataModuleIdx(symbol_meta),
                monotype,
            );

            // Ensure the external definition is lowered.
            const symbol_key: u64 = @bitCast(symbol);
            if (!self.lowered_symbols.contains(symbol_key) and !self.in_progress_defs.contains(symbol_key)) {
                const def_idx: CIR.Def.Idx = @enumFromInt(ext.target_node_idx);
                const def = target_env.store.getDef(def_idx);
                _ = try self.lowerExternalDefWithType(symbol, def.expr, monotype);
                return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
            }

            // Symbol already lowered. Check for polymorphic specialization.
            if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                const cached_monotype = self.store.typeOf(cached_expr);
                const monotype_matches_cached = try self.monotypesStructurallyEqual(cached_monotype, normalized_lookup_monotype);
                if (!normalized_lookup_monotype.isNone() and !monotype_matches_cached) {
                    if (try self.lookupPolySpecialization(symbol_key, normalized_lookup_monotype)) |spec_symbol| {
                        return try self.store.addExpr(self.allocator, .{ .lookup = spec_symbol }, monotype, region);
                    }

                    // Create a new specialization.
                    const spec_key = polySpecKey(symbol_key, normalized_lookup_monotype);
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

fn isTopLevelPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) bool {
    const defs = module_env.store.sliceDefs(module_env.all_defs);
    for (defs) |def_idx| {
        if (module_env.store.getDef(def_idx).pattern == pattern_idx) return true;
    }
    return false;
}

fn patternBoundSymbol(self: *Self, pattern_id: MIR.PatternId) ?MIR.Symbol {
    const pattern = self.store.getPattern(pattern_id);
    return switch (pattern) {
        .bind => |sym| sym,
        .as_pattern => |as_pat| as_pat.symbol,
        .wildcard,
        .tag,
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .record_destructure,
        .tuple_destructure,
        .list_destructure,
        .runtime_error,
        => null,
    };
}

fn collectPatternBindings(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    out: *std.ArrayList(PatternBinding),
) Allocator.Error!void {
    switch (module_env.store.getPattern(pattern_idx)) {
        .assign => |assign| try out.append(self.allocator, .{ .ident = assign.ident, .pattern_idx = pattern_idx }),
        .as => |as_pat| {
            try out.append(self.allocator, .{ .ident = as_pat.ident, .pattern_idx = pattern_idx });
            try self.collectPatternBindings(module_env, as_pat.pattern, out);
        },
        .tuple => |tuple| {
            for (module_env.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                try self.collectPatternBindings(module_env, elem_pattern_idx, out);
            }
        },
        .applied_tag => |tag| {
            for (module_env.store.slicePatterns(tag.args)) |arg_pattern_idx| {
                try self.collectPatternBindings(module_env, arg_pattern_idx, out);
            }
        },
        .record_destructure => |record_pat| {
            for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required => |sub_pattern_idx| try self.collectPatternBindings(module_env, sub_pattern_idx, out),
                    .SubPattern => |sub_pattern_idx| try self.collectPatternBindings(module_env, sub_pattern_idx, out),
                }
            }
        },
        .list => |list_pat| {
            for (module_env.store.slicePatterns(list_pat.patterns)) |elem_pattern_idx| {
                try self.collectPatternBindings(module_env, elem_pattern_idx, out);
            }
            if (list_pat.rest_info) |rest| {
                if (rest.pattern) |rest_pattern_idx| {
                    try self.collectPatternBindings(module_env, rest_pattern_idx, out);
                }
            }
        },
        .nominal => |nom| try self.collectPatternBindings(module_env, nom.backing_pattern, out),
        .nominal_external => |nom| try self.collectPatternBindings(module_env, nom.backing_pattern, out),
        .underscore,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .runtime_error,
        => {},
    }
}

fn alignAlternativePatternSymbols(
    self: *Self,
    module_env: *const ModuleEnv,
    representative_pattern_idx: CIR.Pattern.Idx,
    alternative_pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    var representative_bindings = std.ArrayList(PatternBinding).empty;
    defer representative_bindings.deinit(self.allocator);

    var alternative_bindings = std.ArrayList(PatternBinding).empty;
    defer alternative_bindings.deinit(self.allocator);

    try self.collectPatternBindings(module_env, representative_pattern_idx, &representative_bindings);
    try self.collectPatternBindings(module_env, alternative_pattern_idx, &alternative_bindings);

    for (alternative_bindings.items) |alt_binding| {
        for (representative_bindings.items) |rep_binding| {
            if (!rep_binding.ident.eql(alt_binding.ident)) continue;

            const rep_symbol = try self.patternToSymbol(rep_binding.pattern_idx);
            const base_key: u64 = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(alt_binding.pattern_idx);
            const key: u128 = (@as(u128, self.current_pattern_scope) << 64) | @as(u128, base_key);
            try self.pattern_symbols.put(key, rep_symbol);
            break;
        }
    }
}

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
    const is_top_level_pattern = isTopLevelPattern(module_env, pattern_idx);
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
    const synthetic_ident = self.makeSyntheticIdent(symbolMetadataDisplayIdent(original_meta));
    return self.internSymbol(symbolMetadataModuleIdx(original_meta), synthetic_ident);
}

/// If `arg` is a `.lambda` expression, hoist it into a synthetic symbol def
/// and return a `.lookup` to that symbol. This ensures every lambda passed as
/// a call argument has a symbol, which lambda set inference needs.
fn hoistLambdaArg(self: *Self, arg: MIR.ExprId) Allocator.Error!MIR.ExprId {
    if (self.store.getExpr(arg) != .lambda) return arg;
    const ident = self.makeSyntheticIdent(.{ .idx = 0, .attributes = .{ .effectful = false, .ignored = false, .reassignable = false } });
    const sym = try self.internSymbol(self.current_module_idx, ident);
    try self.store.registerSymbolDef(self.allocator, sym, arg);
    return self.store.addExpr(self.allocator, .{ .lookup = sym }, self.store.typeOf(arg), self.store.getRegion(arg));
}

fn isLambdaExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId) bool {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .block => |block| isLambdaExpr(mir_store, block.final_expr),
        .lambda => true,
        else => false,
    };
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
        .recursive_placeholder => {
            if (std.debug.runtime_safety) {
                std.debug.panic("recursive_placeholder survived monotype construction", .{});
            }
            unreachable;
        },
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
                if (!self.identsStructurallyEqual(lhs_field.name, rhs_field.name)) break :blk false;
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
                const lhs_payloads = self.store.monotype_store.getIdxSpan(lhs_tag.payloads);
                const rhs_payloads = self.store.monotype_store.getIdxSpan(rhs_tag.payloads);

                if (!self.identsTagNameEquivalent(lhs_tag.name, rhs_tag.name)) break :blk false;

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
        &self.nominal_cycle_breakers,
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
        &self.nominal_cycle_breakers,
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

        const representative_pattern_idx = if (cir_bp_indices.len > 0)
            module_env.store.getMatchBranchPattern(cir_bp_indices[0]).pattern
        else
            null;

        for (cir_bp_indices, 0..) |bp_idx, bp_index| {
            const cir_bp = module_env.store.getMatchBranchPattern(bp_idx);
            if (bp_index != 0) {
                if (representative_pattern_idx) |rep_pattern_idx| {
                    try self.alignAlternativePatternSymbols(module_env, rep_pattern_idx, cir_bp.pattern);
                }
            }
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

/// Lower `e_closure` by lifting it to a top-level function with an explicit captures tuple parameter.
/// At the use site, returns a tuple of the captured values and registers explicit
/// MIR closure-member metadata for downstream analysis and lowering.
fn lowerClosure(self: *Self, module_env: *const ModuleEnv, closure: CIR.Expr.Closure, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const inner_lambda_expr = module_env.store.getExpr(closure.lambda_idx);
    const lambda = inner_lambda_expr.e_lambda;
    const cir_capture_indices_all = module_env.store.sliceCaptures(closure.captures);
    const CaptureIdx = @TypeOf(cir_capture_indices_all[0]);
    var cir_capture_indices_filtered = std.ArrayList(CaptureIdx).empty;
    defer cir_capture_indices_filtered.deinit(self.allocator);

    // Captures that point at deferred block lambdas are function references, not
    // tuple captures for closure lifting. Keep those as symbol lookups.
    for (cir_capture_indices_all) |cap_idx| {
        const cap = module_env.store.getCapture(cap_idx);
        if (self.deferred_block_lambdas.contains(@intFromEnum(cap.pattern_idx))) continue;
        try cir_capture_indices_filtered.append(self.allocator, cap_idx);
    }
    const cir_capture_indices = cir_capture_indices_filtered.items;

    if (cir_capture_indices.len == 0) {
        // No captures — just lower as a plain lambda (no lifting needed).
        return self.lowerLambda(module_env, lambda, monotype, region);
    }

    // --- Step 1: Collect outer-scope capture symbols and their monotypes ---
    const idxs_top = self.mono_scratches.idxs.top();
    defer self.mono_scratches.idxs.clearFrom(idxs_top);

    const expr_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(expr_top);

    for (cir_capture_indices) |cap_idx| {
        const cap = module_env.store.getCapture(cap_idx);
        // Resolve the outer-scope symbol (before we change scopes)
        const outer_symbol = try self.patternToSymbol(cap.pattern_idx);
        // Get the monotype of the captured variable
        const cap_type_var = ModuleEnv.varFrom(cap.pattern_idx);
        const cap_monotype = try self.store.monotype_store.fromTypeVar(
            self.allocator,
            self.types_store,
            cap_type_var,
            self.currentCommonIdents(),
            &self.type_var_seen,
            &self.nominal_cycle_breakers,
            &self.mono_scratches,
        );
        try self.mono_scratches.idxs.append(cap_monotype);

        // Build the outer-scope lookup expression for this capture
        const lookup_expr = try self.store.addExpr(self.allocator, .{ .lookup = outer_symbol }, cap_monotype, region);
        try self.scratch_expr_ids.append(lookup_expr);
    }

    const capture_monotypes = self.mono_scratches.idxs.sliceFromStart(idxs_top);
    const capture_lookup_exprs = self.scratch_expr_ids.sliceFromStart(expr_top);

    var capture_monotypes_snapshot = std.ArrayList(Monotype.Idx).empty;
    defer capture_monotypes_snapshot.deinit(self.allocator);
    try capture_monotypes_snapshot.appendSlice(self.allocator, capture_monotypes);

    var capture_lookup_exprs_snapshot = std.ArrayList(MIR.ExprId).empty;
    defer capture_lookup_exprs_snapshot.deinit(self.allocator);
    try capture_lookup_exprs_snapshot.appendSlice(self.allocator, capture_lookup_exprs);

    // --- Step 2: Create captures tuple monotype ---
    const captures_tuple_elems = try self.store.monotype_store.addIdxSpan(self.allocator, capture_monotypes_snapshot.items);
    const captures_tuple_monotype = try self.store.monotype_store.addMonotype(self.allocator, .{ .tuple = .{
        .elems = captures_tuple_elems,
    } });

    // --- Step 3: Create the lifted function symbol ---
    // Use closure's tag_name as the basis if available, else synthesize
    const lifted_ident = self.makeSyntheticIdent(closure.tag_name);
    const lifted_fn_symbol = try self.internSymbol(self.current_module_idx, lifted_ident);

    // --- Step 4: Create captures param symbol and pattern ---
    const captures_param_ident = self.makeSyntheticIdent(closure.tag_name);
    const captures_param_symbol = try self.internSymbol(self.current_module_idx, captures_param_ident);
    const captures_param_pattern = try self.store.addPattern(self.allocator, .{ .bind = captures_param_symbol }, captures_tuple_monotype);

    // --- Step 5: Enter a new scope for the lifted function body ---
    const saved_pattern_scope = self.current_pattern_scope;
    self.current_pattern_scope = lifted_fn_symbol.raw();
    defer self.current_pattern_scope = saved_pattern_scope;

    // Explicitly create fresh local symbols for each captured variable in the new scope.
    // patternToSymbol would resolve these to the outer scope's symbols (correct for
    // normal scoping), but here we need distinct symbols that get their values from
    // destructuring the captures tuple parameter.
    for (cir_capture_indices) |cap_idx| {
        const cap = module_env.store.getCapture(cap_idx);
        const base_key: u64 = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(cap.pattern_idx);
        const scoped_key: u128 = (@as(u128, self.current_pattern_scope) << 64) | @as(u128, base_key);
        const local_ident = self.makeSyntheticIdent(cap.name);
        const local_symbol = try self.internSymbol(self.current_module_idx, local_ident);
        try self.pattern_symbols.put(scoped_key, local_symbol);
    }

    // --- Step 6: Lower the lambda params and body in the new scope ---
    const params = try self.lowerPatternSpan(module_env, lambda.args);
    const body = try self.lowerExpr(lambda.body);

    // --- Step 7: Build destructuring preamble ---
    // For each capture: `let local_sym = tuple_access(lookup(captures_param), i)`
    const stmts_top = self.scratch_stmts.top();
    defer self.scratch_stmts.clearFrom(stmts_top);

    for (cir_capture_indices, 0..) |cap_idx, i| {
        const cap = module_env.store.getCapture(cap_idx);
        const local_symbol = try self.patternToSymbol(cap.pattern_idx);
        const cap_monotype = capture_monotypes_snapshot.items[i];

        // lookup(captures_param)
        const captures_lookup = try self.store.addExpr(self.allocator, .{ .lookup = captures_param_symbol }, captures_tuple_monotype, region);

        // tuple_access(captures_lookup, i)
        const tuple_access_expr = try self.store.addExpr(self.allocator, .{ .tuple_access = .{
            .tuple = captures_lookup,
            .elem_index = @intCast(i),
        } }, cap_monotype, region);

        // let local_sym = tuple_access_expr
        const bind_pat = try self.store.addPattern(self.allocator, .{ .bind = local_symbol }, cap_monotype);
        try self.registerBoundSymbolDefIfNeeded(bind_pat, tuple_access_expr);
        try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = bind_pat, .expr = tuple_access_expr } });
    }

    // --- Step 8: Wrap body in block with destructuring stmts ---
    const preamble_stmts = try self.store.addStmts(self.allocator, self.scratch_stmts.sliceFromStart(stmts_top));
    const lifted_body = if (preamble_stmts.isEmpty())
        body
    else
        try self.store.addExpr(self.allocator, .{ .block = .{
            .stmts = preamble_stmts,
            .final_expr = body,
        } }, self.store.typeOf(body), region);

    // --- Step 9: Build the lifted lambda's param list (original params + captures param) ---
    const orig_param_ids = self.store.getPatternSpan(params);
    const pat_top = self.scratch_pattern_ids.top();
    defer self.scratch_pattern_ids.clearFrom(pat_top);
    for (orig_param_ids) |pid| {
        try self.scratch_pattern_ids.append(pid);
    }
    try self.scratch_pattern_ids.append(captures_param_pattern);
    const all_params = try self.store.addPatternSpan(self.allocator, self.scratch_pattern_ids.sliceFromStart(pat_top));

    // Build the lifted function's monotype: func(orig_args..., captures_tuple) -> ret
    const orig_monotype = self.store.monotype_store.getMonotype(monotype);
    const orig_func = orig_monotype.func;
    const orig_arg_monos = self.store.monotype_store.getIdxSpan(orig_func.args);
    const arg_top = self.mono_scratches.idxs.top();
    defer self.mono_scratches.idxs.clearFrom(arg_top);
    for (orig_arg_monos) |am| {
        try self.mono_scratches.idxs.append(am);
    }
    try self.mono_scratches.idxs.append(captures_tuple_monotype);
    const lifted_func_monotype = try self.buildFuncMonotype(
        self.mono_scratches.idxs.sliceFromStart(arg_top),
        orig_func.ret,
        orig_func.effectful,
    );

    // Create the lifted lambda expression
    const lifted_lambda_expr = try self.store.addExpr(self.allocator, .{ .lambda = .{
        .params = all_params,
        .body = lifted_body,
        .captures = MIR.CaptureSpan.empty(),
    } }, lifted_func_monotype, region);

    const binding_top = self.scratch_capture_bindings.top();
    defer self.scratch_capture_bindings.clearFrom(binding_top);
    for (cir_capture_indices, 0..) |cap_idx, i| {
        const cap = module_env.store.getCapture(cap_idx);
        const local_symbol = try self.patternToSymbol(cap.pattern_idx);
        try self.scratch_capture_bindings.append(.{
            .local_symbol = local_symbol,
            .source_expr = capture_lookup_exprs_snapshot.items[i],
            .monotype = capture_monotypes_snapshot.items[i],
        });
    }
    const capture_binding_span = try self.store.addCaptureBindings(self.allocator, self.scratch_capture_bindings.sliceFromStart(binding_top));

    // --- Step 10: Register the lifted function and its semantic closure member ---
    try self.store.registerSymbolDef(self.allocator, lifted_fn_symbol, lifted_lambda_expr);
    const member_id = try self.store.addClosureMember(self.allocator, .{
        .fn_symbol = lifted_fn_symbol,
        .capture_bindings = capture_binding_span,
    });

    // --- Step 11: At the use site, return a tuple of capture values ---
    const captures_tuple_span = try self.store.addExprSpan(self.allocator, capture_lookup_exprs_snapshot.items);
    const captures_tuple_expr = try self.store.addExpr(self.allocator, .{ .tuple = .{
        .elems = captures_tuple_span,
    } }, captures_tuple_monotype, region);

    try self.store.registerExprClosureMember(self.allocator, captures_tuple_expr, member_id);

    return captures_tuple_expr;
}

fn lowerDeferredBlockLambda(
    self: *Self,
    deferred: DeferredBlockLambda,
    caller_monotype: Monotype.Idx,
) Allocator.Error!MIR.ExprId {
    const symbol = deferred.symbol;
    const symbol_key: u64 = @bitCast(symbol);

    if (self.lowered_symbols.get(symbol_key)) |cached| return cached;
    if (self.in_progress_defs.contains(symbol_key)) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "MIR Lower invariant: deferred block lambda lowering re-entered while already in progress (symbol={d})",
                .{symbol.raw()},
            );
        }
        unreachable;
    }

    try self.in_progress_defs.put(symbol_key, {});
    errdefer _ = self.in_progress_defs.remove(symbol_key);

    const active_monotype: Monotype.Idx = if (!caller_monotype.isNone()) caller_monotype else Monotype.Idx.none;
    try self.in_progress_symbol_monotypes.put(symbol_key, active_monotype);
    errdefer _ = self.in_progress_symbol_monotypes.remove(symbol_key);

    // Use an isolated type_var_seen scope so call-site bindings are not
    // contaminated by earlier entries from this block/module.
    const saved_type_var_seen = self.type_var_seen;
    self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;
    self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    defer {
        self.type_var_seen.deinit();
        self.type_var_seen = saved_type_var_seen;
        self.nominal_cycle_breakers.deinit();
        self.nominal_cycle_breakers = saved_nominal_cycle_breakers;
    }

    if (!caller_monotype.isNone()) {
        // Bind via the declaration pattern's type root. This is the
        // generalized let-binding type that call sites instantiate.
        try self.bindTypeVarMonotypes(ModuleEnv.varFrom(deferred.pattern_idx), caller_monotype);
    }

    const saved_pattern_scope = self.current_pattern_scope;
    self.current_pattern_scope = symbol_key;
    defer self.current_pattern_scope = saved_pattern_scope;

    const lowered = try self.lowerExpr(deferred.cir_expr);

    try self.lowered_symbols.put(symbol_key, lowered);
    if (self.store.getSymbolDef(symbol) == null) {
        try self.store.registerSymbolDef(self.allocator, symbol, lowered);
    }

    _ = self.in_progress_defs.remove(symbol_key);
    _ = self.in_progress_symbol_monotypes.remove(symbol_key);

    return lowered;
}

fn ensureDeferredBlockLambdaLowered(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    symbol: MIR.Symbol,
    caller_monotype: Monotype.Idx,
) Allocator.Error!void {
    const deferred = self.deferred_block_lambdas.get(@intFromEnum(pattern_idx)) orelse return;
    const symbol_key: u64 = @bitCast(symbol);
    if (self.lowered_symbols.contains(symbol_key) or self.in_progress_defs.contains(symbol_key)) return;

    if (std.debug.runtime_safety) {
        if (deferred.module_idx != self.current_module_idx) {
            std.debug.panic(
                "MIR Lower invariant: deferred block lambda module mismatch (entry={d}, current={d})",
                .{ deferred.module_idx, self.current_module_idx },
            );
        }
        if (deferred.symbol.raw() != symbol.raw()) {
            std.debug.panic(
                "MIR Lower invariant: deferred block lambda symbol mismatch (entry={d}, lookup={d})",
                .{ deferred.symbol.raw(), symbol.raw() },
            );
        }
    }

    _ = try self.lowerDeferredBlockLambda(deferred, caller_monotype);
}

/// Lower `e_call` to MIR call.
/// If the call target is a lookup to a low-level builtin wrapper
/// (e.g., List.concat, Str.concat), emit `run_low_level` instead of `call`.
fn lowerCall(self: *Self, module_env: *const ModuleEnv, call: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const func_cir = module_env.store.getExpr(call.func);
    if (self.getCallLowLevelOp(module_env, func_cir)) |ll_op| {
        if (ll_op == .str_inspekt) {
            return try self.lowerStrInspekt(module_env, .{
                .op = ll_op,
                .args = call.args,
            }, region);
        }
        const args = try self.lowerExprSpan(module_env, call.args);
        return try self.store.addExpr(self.allocator, .{ .run_low_level = .{
            .op = ll_op,
            .args = args,
        } }, monotype, region);
    }

    return self.lowerCallWithLoweredFunc(
        try self.lowerExpr(call.func),
        module_env.store.sliceExpr(call.args),
        monotype,
        region,
    );
}

fn lowerCallWithLoweredFunc(
    self: *Self,
    lowered_func_input: MIR.ExprId,
    call_arg_exprs: []const CIR.Expr.Idx,
    monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const lowered_func = try self.hoistLambdaArg(lowered_func_input);
    const func_mono = self.store.typeOf(lowered_func);
    if (self.store.monotype_store.getMonotype(func_mono) == .func and self.store.getExpr(lowered_func) != .lookup) {
        const func_bind = try self.makeSyntheticBind(func_mono, false);
        try self.registerBoundSymbolDefIfNeeded(func_bind.pattern, lowered_func);
        const func_lookup = try self.emitMirLookup(func_bind.symbol, func_mono, region);

        const args_top = self.scratch_expr_ids.top();
        defer self.scratch_expr_ids.clearFrom(args_top);
        for (call_arg_exprs) |arg_idx| {
            const arg = try self.hoistLambdaArg(try self.lowerExpr(arg_idx));
            try self.scratch_expr_ids.append(arg);
        }
        const lowered_call_args = self.scratch_expr_ids.sliceFromStart(args_top);
        const args = try self.store.addExprSpan(self.allocator, lowered_call_args);
        const call_expr = try self.store.addExpr(self.allocator, .{ .call = .{
            .func = func_lookup,
            .args = args,
        } }, monotype, region);

        const stmts = try self.store.addStmts(self.allocator, &.{MIR.Stmt{
            .decl_const = .{ .pattern = func_bind.pattern, .expr = lowered_func },
        }});

        return try self.store.addExpr(self.allocator, .{ .block = .{
            .stmts = stmts,
            .final_expr = call_expr,
        } }, monotype, region);
    }
    const func = lowered_func;

    const args_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(args_top);
    for (call_arg_exprs) |arg_idx| {
        const arg = try self.hoistLambdaArg(try self.lowerExpr(arg_idx));
        try self.scratch_expr_ids.append(arg);
    }
    const lowered_call_args = self.scratch_expr_ids.sliceFromStart(args_top);
    const args = try self.store.addExprSpan(self.allocator, lowered_call_args);

    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = func,
        .args = args,
    } }, monotype, region);
}

fn getCallLowLevelOp(self: *Self, caller_env: *const ModuleEnv, func_expr: CIR.Expr) ?CIR.Expr.LowLevel {
    return switch (func_expr) {
        .e_lookup_external => |lookup| self.getExternalLowLevelOp(caller_env, lookup),
        .e_lookup_local => |lookup| self.getLocalLowLevelOp(caller_env, lookup.pattern_idx),
        else => null,
    };
}

fn getLocalLowLevelOp(self: *Self, module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Expr.LowLevel {
    const defs = module_env.store.sliceDefs(module_env.all_defs);
    for (defs) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (def.pattern != pattern_idx) continue;
        const def_expr = module_env.store.getExpr(def.expr);
        if (def_expr == .e_lambda) {
            const body_expr = module_env.store.getExpr(def_expr.e_lambda.body);
            if (body_expr == .e_run_low_level) return body_expr.e_run_low_level.op;
        }
        return null;
    }

    // Deferred block lambdas are not low-level wrappers.
    _ = self;
    return null;
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

    const DeferredDeclSlot = struct {
        insert_idx: u32,
        order: u32,
        pattern: MIR.PatternId,
        pattern_idx: CIR.Pattern.Idx,
        symbol: MIR.Symbol,
        cir_expr: CIR.Expr.Idx,
    };

    var deferred_decl_slots: std.ArrayList(DeferredDeclSlot) = .empty;
    defer deferred_decl_slots.deinit(self.allocator);
    defer {
        for (deferred_decl_slots.items) |slot| {
            _ = self.deferred_block_lambdas.remove(@intFromEnum(slot.pattern_idx));
        }
    }

    var deferred_order: u32 = 0;

    for (cir_stmt_indices) |stmt_idx| {
        const cir_stmt = module_env.store.getStatement(stmt_idx);
        const stmt_region = module_env.store.getStatementRegion(stmt_idx);
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
                    const symbol = try self.patternToSymbol(decl.pattern);
                    deferred_order += 1;
                    // Track insertion order; materialize with concrete expression later.
                    try deferred_decl_slots.append(self.allocator, .{
                        .insert_idx = @intCast(self.scratch_stmts.items.items.len),
                        .order = deferred_order,
                        .pattern = pat,
                        .pattern_idx = decl.pattern,
                        .symbol = symbol,
                        .cir_expr = decl.expr,
                    });
                    try self.deferred_block_lambdas.put(@intFromEnum(decl.pattern), .{
                        .pattern_idx = decl.pattern,
                        .cir_expr = decl.expr,
                        .module_idx = self.current_module_idx,
                        .symbol = symbol,
                    });
                } else {
                    const expr = try self.lowerExpr(decl.expr);
                    try self.registerBoundSymbolDefIfNeeded(pat, expr);
                    try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = pat, .expr = expr } });
                }
            },
            .s_var => |var_decl| {
                const pat = try self.lowerPattern(module_env, var_decl.pattern_idx);
                const expr = try self.lowerExpr(var_decl.expr);
                try self.registerBoundSymbolDefIfNeeded(pat, expr);
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
                const expr = try self.store.addExpr(self.allocator, .{ .crash = mir_str }, unit_monotype, stmt_region);
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
                } }, unit_monotype, stmt_region);
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
                } }, unit_monotype, stmt_region);
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_break => {
                const unit_monotype = self.store.monotype_store.unit_idx;
                const expr = try self.store.addExpr(self.allocator, .{ .break_expr = {} }, unit_monotype, stmt_region);
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_return => |s_return| {
                const inner = try self.lowerExpr(s_return.expr);
                const unit_monotype = self.store.monotype_store.unit_idx;
                const expr = try self.store.addExpr(self.allocator, .{ .return_expr = .{
                    .expr = inner,
                } }, unit_monotype, stmt_region);
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            .s_runtime_error => |s_re| {
                const unit_monotype = self.store.monotype_store.unit_idx;
                const expr = try self.store.addExpr(self.allocator, .{ .runtime_err_can = .{
                    .diagnostic = s_re.diagnostic,
                } }, unit_monotype, stmt_region);
                const wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_monotype);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = wildcard, .expr = expr } });
            },
            // Compile-time declarations — no runtime behavior
            .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno, .s_type_var_alias => {},
        }
    }

    // Lower the final expression BEFORE committing stmts. Call sites in the
    // final expression trigger deferred lambda lowering via e_lookup_local.
    const final_expr = try self.lowerExpr(block.final_expr);

    // Ensure every deferred declaration has a concrete lowered expression.
    for (deferred_decl_slots.items) |slot| {
        const symbol_key: u64 = @bitCast(slot.symbol);
        if (!self.lowered_symbols.contains(symbol_key)) {
            _ = try self.lowerDeferredBlockLambda(.{
                .pattern_idx = slot.pattern_idx,
                .cir_expr = slot.cir_expr,
                .module_idx = self.current_module_idx,
                .symbol = slot.symbol,
            }, Monotype.Idx.none);
        }
    }

    // Insert deferred declarations in reverse insertion order so earlier
    // insertions do not disturb later indices.
    std.mem.sort(DeferredDeclSlot, deferred_decl_slots.items, {}, struct {
        fn lessThan(_: void, a: DeferredDeclSlot, b: DeferredDeclSlot) bool {
            if (a.insert_idx == b.insert_idx) return a.order > b.order;
            return a.insert_idx > b.insert_idx;
        }
    }.lessThan);

    for (deferred_decl_slots.items) |slot| {
        const symbol_key: u64 = @bitCast(slot.symbol);
        const lowered = self.lowered_symbols.get(symbol_key) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "MIR Lower invariant: deferred block lambda has no lowered expression (symbol={d})",
                    .{slot.symbol.raw()},
                );
            }
            unreachable;
        };

        const lowered_monotype = self.store.typeOf(lowered);
        const pattern_monotype = self.store.patternTypeOf(slot.pattern);
        const stmt_pattern = if (try self.monotypesStructurallyEqual(pattern_monotype, lowered_monotype))
            slot.pattern
        else
            try self.store.addPattern(self.allocator, .{ .bind = slot.symbol }, lowered_monotype);

        try self.registerBoundSymbolDefIfNeeded(stmt_pattern, lowered);
        try self.scratch_stmts.items.insert(
            @intCast(slot.insert_idx),
            .{ .decl_const = .{ .pattern = stmt_pattern, .expr = lowered } },
        );
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
            const bool_monotype = try self.store.monotype_store.addBoolTagUnion(self.allocator, self.currentCommonIdents());
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
            const bool_monotype = try self.store.monotype_store.addBoolTagUnion(self.allocator, self.currentCommonIdents());
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

            // Equality on structural types is decomposed field-by-field in MIR
            // rather than dispatched to a nominal method.
            if (binop.op == .eq or binop.op == .ne) {
                const lhs_mono = self.store.monotype_store.getMonotype(lhs_monotype);
                switch (lhs_mono) {
                    // Records, tuples, and lists are always structural.
                    .record, .tuple, .list => {
                        const result = try self.lowerStructuralEquality(lhs, rhs, lhs_monotype, monotype, region);
                        if (binop.op == .ne) return try self.negBool(module_env, result, monotype, region);
                        return result;
                    },
                    // Tag unions may be nominal (with dispatch target) or anonymous structural.
                    .tag_union => {
                        const cached_dispatch = self.lookupResolvedDispatchTarget(expr_idx);
                        const eq_constraint = self.lookupDispatchConstraintForExpr(expr_idx, module_env.idents.is_eq);
                        const constraint_resolved = if (eq_constraint) |constraint| !constraint.resolved_target.isNone() else false;
                        const has_nominal_dispatch = cached_dispatch != null or constraint_resolved;
                        if (!has_nominal_dispatch) {
                            const result = try self.lowerStructuralEquality(lhs, rhs, lhs_monotype, monotype, region);
                            if (binop.op == .ne) return try self.negBool(module_env, result, monotype, region);
                            return result;
                        }
                        // Nominal tag union — fall through to method call dispatch below.
                    },
                    // Unit is always equal.
                    .unit => return try self.emitMirBoolLiteral(module_env, binop.op == .eq, region),
                    // Primitives: emit low-level eq op directly.
                    .prim => |p| {
                        const op: CIR.Expr.LowLevel = switch (p) {
                            .str => .str_is_eq,
                            else => .num_is_eq,
                        };
                        const args = try self.store.addExprSpan(self.allocator, &.{ lhs, rhs });
                        const result = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
                            .op = op,
                            .args = args,
                        } }, monotype, region);
                        if (binop.op == .ne) return try self.negBool(module_env, result, monotype, region);
                        return result;
                    },
                    else => {},
                }
            }

            // Use checker-resolved target for static dispatch.
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
            const resolved_target = try self.resolveDispatchTargetForExpr(module_env, expr_idx, method_ident);
            const method_symbol = try self.resolvedDispatchTargetToSymbol(module_env, resolved_target);

            const func_monotype = try self.buildFuncMonotype(&.{ lhs_monotype, lhs_monotype }, monotype, false);

            // Ensure the method body is lowered so codegen can find it.
            const lowered_method_symbol = try self.specializeMethod(method_symbol, func_monotype);

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

    const resolved_target = try self.resolveDispatchTargetForExpr(module_env, expr_idx, module_env.idents.negate);
    const method_symbol = try self.resolvedDispatchTargetToSymbol(module_env, resolved_target);

    const inner_monotype = try self.resolveMonotype(um.expr);
    const func_monotype = try self.buildFuncMonotype(&.{inner_monotype}, monotype, false);

    // Ensure the method body is lowered so codegen can find it.
    const lowered_method_symbol = try self.specializeMethod(method_symbol, func_monotype);

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
    const bool_monotype = try self.store.monotype_store.addBoolTagUnion(self.allocator, self.currentCommonIdents());

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
    const bool_monotype = try self.store.monotype_store.addBoolTagUnion(self.allocator, self.currentCommonIdents());
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

// --- Structural equality lowering ---
//
// For structural types (records, tuples, anonymous tag unions, lists),
// equality is decomposed into field-level/element-level primitive comparisons
// during MIR lowering. This means backends only need to handle primitive
// equality (num_is_eq, str_is_eq).

/// Dispatch structural equality based on the operand's monotype.
fn lowerStructuralEquality(
    self: *Self,
    lhs: MIR.ExprId,
    rhs: MIR.ExprId,
    operand_monotype: Monotype.Idx,
    ret_monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const module_env = self.all_module_envs[self.current_module_idx];
    const mono = self.store.monotype_store.getMonotype(operand_monotype);
    return switch (mono) {
        .unit => try self.emitMirBoolLiteral(module_env, true, region),
        .record, .tuple, .tag_union, .list => blk: {
            // Structural equality may inspect the same operand many times; bind
            // each side once so downstream helpers only work with stable lookups.
            const lhs_bind = try self.makeSyntheticBind(operand_monotype, false);
            const rhs_bind = try self.makeSyntheticBind(operand_monotype, false);
            const lhs_lookup = try self.emitMirLookup(lhs_bind.symbol, operand_monotype, region);
            const rhs_lookup = try self.emitMirLookup(rhs_bind.symbol, operand_monotype, region);

            const inner = switch (mono) {
                .record => |rec| try self.lowerRecordEquality(module_env, lhs_lookup, rhs_lookup, rec, ret_monotype, region),
                .tuple => |tup| try self.lowerTupleEquality(module_env, lhs_lookup, rhs_lookup, tup, ret_monotype, region),
                .tag_union => |tu| try self.lowerTagUnionEquality(module_env, lhs_lookup, rhs_lookup, tu, operand_monotype, ret_monotype, region),
                .list => |lst| try self.lowerListEquality(module_env, lhs_lookup, rhs_lookup, lst, ret_monotype, region),
                else => unreachable,
            };

            const bindings = try self.store.addBorrowBindings(self.allocator, &.{
                .{ .pattern = lhs_bind.pattern, .expr = lhs },
                .{ .pattern = rhs_bind.pattern, .expr = rhs },
            });

            break :blk try self.store.addExpr(self.allocator, .{ .borrow_scope = .{
                .bindings = bindings,
                .body = inner,
            } }, ret_monotype, region);
        },
        else => {
            if (std.debug.runtime_safety) {
                std.debug.panic("lowerStructuralEquality: unexpected monotype {s}", .{@tagName(mono)});
            }
            unreachable;
        },
    };
}

/// Lower equality for a single field/element — dispatches to the appropriate
/// low-level op for primitives, or recurses into lowerStructuralEquality for
/// composite types.
fn lowerFieldEquality(
    self: *Self,
    module_env: *const ModuleEnv,
    lhs: MIR.ExprId,
    rhs: MIR.ExprId,
    field_monotype: Monotype.Idx,
    ret_monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const mono = self.store.monotype_store.getMonotype(field_monotype);
    switch (mono) {
        .prim => |p| {
            const op: CIR.Expr.LowLevel = switch (p) {
                .str => .str_is_eq,
                else => .num_is_eq,
            };
            const args = try self.store.addExprSpan(self.allocator, &.{ lhs, rhs });
            return try self.store.addExpr(self.allocator, .{ .run_low_level = .{
                .op = op,
                .args = args,
            } }, ret_monotype, region);
        },
        .record, .tuple, .tag_union, .list => {
            return try self.lowerStructuralEquality(lhs, rhs, field_monotype, ret_monotype, region);
        },
        .unit => {
            return try self.emitMirBoolLiteral(module_env, true, region);
        },
        else => {
            if (std.debug.runtime_safety) {
                std.debug.panic("lowerFieldEquality: unexpected field monotype {s}", .{@tagName(mono)});
            }
            unreachable;
        },
    }
}

/// Record equality: field-by-field comparison with short-circuit AND.
/// Generates nested `match` on Bool:
///   field0_eq and (field1_eq and (... and fieldN_eq))
fn lowerRecordEquality(
    self: *Self,
    module_env: *const ModuleEnv,
    lhs: MIR.ExprId,
    rhs: MIR.ExprId,
    rec: anytype,
    ret_monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    self.debugAssertLookupExpr(lhs, "lowerRecordEquality(lhs)");
    self.debugAssertLookupExpr(rhs, "lowerRecordEquality(rhs)");

    const fields = self.store.monotype_store.getFields(rec.fields);

    // Empty record: always equal
    if (fields.len == 0) return try self.emitMirBoolLiteral(module_env, true, region);

    // Build field comparisons from last to first (innermost result first).
    var result: MIR.ExprId = undefined;
    var i: usize = fields.len;
    while (i > 0) {
        i -= 1;
        const field = fields[i];

        const lhs_field = try self.store.addExpr(self.allocator, .{ .record_access = .{
            .record = lhs,
            .field_name = field.name,
        } }, field.type_idx, region);

        const rhs_field = try self.store.addExpr(self.allocator, .{ .record_access = .{
            .record = rhs,
            .field_name = field.name,
        } }, field.type_idx, region);

        const field_eq = try self.lowerFieldEquality(module_env, lhs_field, rhs_field, field.type_idx, ret_monotype, region);

        if (i == fields.len - 1) {
            result = field_eq;
        } else {
            // Short-circuit AND: match field_eq { True => <rest>, _ => False }
            const false_expr = try self.emitMirBoolLiteral(module_env, false, region);
            result = try self.createBoolMatch(module_env, field_eq, result, false_expr, ret_monotype, region);
        }
    }

    return result;
}

/// Tuple equality: element-by-element comparison with short-circuit AND.
fn lowerTupleEquality(
    self: *Self,
    module_env: *const ModuleEnv,
    lhs: MIR.ExprId,
    rhs: MIR.ExprId,
    tup: anytype,
    ret_monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    self.debugAssertLookupExpr(lhs, "lowerTupleEquality(lhs)");
    self.debugAssertLookupExpr(rhs, "lowerTupleEquality(rhs)");

    const elems = self.store.monotype_store.getIdxSpan(tup.elems);

    // Empty tuple: always equal
    if (elems.len == 0) return try self.emitMirBoolLiteral(module_env, true, region);

    // Build element comparisons from last to first.
    var result: MIR.ExprId = undefined;
    var i: usize = elems.len;
    while (i > 0) {
        i -= 1;

        const lhs_elem = try self.store.addExpr(self.allocator, .{ .tuple_access = .{
            .tuple = lhs,
            .elem_index = @intCast(i),
        } }, elems[i], region);

        const rhs_elem = try self.store.addExpr(self.allocator, .{ .tuple_access = .{
            .tuple = rhs,
            .elem_index = @intCast(i),
        } }, elems[i], region);

        const elem_eq = try self.lowerFieldEquality(module_env, lhs_elem, rhs_elem, elems[i], ret_monotype, region);

        if (i == elems.len - 1) {
            result = elem_eq;
        } else {
            const false_expr = try self.emitMirBoolLiteral(module_env, false, region);
            result = try self.createBoolMatch(module_env, elem_eq, result, false_expr, ret_monotype, region);
        }
    }

    return result;
}

/// Tag union equality: match on lhs variant, then nested match on rhs.
/// For each variant, if both lhs and rhs match the same tag, compare payloads.
/// If tags differ, return False.
fn lowerTagUnionEquality(
    self: *Self,
    module_env: *const ModuleEnv,
    lhs: MIR.ExprId,
    rhs: MIR.ExprId,
    tu: anytype,
    tu_monotype: Monotype.Idx,
    ret_monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    self.debugAssertLookupExpr(lhs, "lowerTagUnionEquality(lhs)");
    self.debugAssertLookupExpr(rhs, "lowerTagUnionEquality(rhs)");

    const tags = self.store.monotype_store.getTags(tu.tags);

    // Empty tag union: vacuously true
    if (tags.len == 0) return try self.emitMirBoolLiteral(module_env, true, region);

    const save_branches = self.scratch_branches.top();
    defer self.scratch_branches.clearFrom(save_branches);

    for (tags) |tag| {
        const payloads = self.store.monotype_store.getIdxSpan(tag.payloads);

        // --- LHS pattern: Tag(lhs_p0, lhs_p1, ...) ---
        const save_pats = self.scratch_pattern_ids.top();
        const save_caps = self.scratch_captures.top();
        defer self.scratch_captures.clearFrom(save_caps);

        for (payloads) |payload_mono| {
            const bind = try self.makeSyntheticBind(payload_mono, false);
            try self.scratch_pattern_ids.append(bind.pattern);
            try self.scratch_captures.append(.{ .symbol = bind.symbol });
        }
        const lhs_payload_patterns = try self.store.addPatternSpan(
            self.allocator,
            self.scratch_pattern_ids.sliceFromStart(save_pats),
        );
        self.scratch_pattern_ids.clearFrom(save_pats);

        const lhs_tag_pattern = try self.store.addPattern(self.allocator, .{ .tag = .{
            .name = tag.name,
            .args = lhs_payload_patterns,
        } }, tu_monotype);
        const lhs_bp = try self.store.addBranchPatterns(self.allocator, &.{.{
            .pattern = lhs_tag_pattern,
            .degenerate = false,
        }});

        // --- RHS pattern: Tag(rhs_p0, rhs_p1, ...) ---
        for (payloads) |payload_mono| {
            const bind = try self.makeSyntheticBind(payload_mono, false);
            try self.scratch_pattern_ids.append(bind.pattern);
            try self.scratch_captures.append(.{ .symbol = bind.symbol });
        }
        const rhs_payload_patterns = try self.store.addPatternSpan(
            self.allocator,
            self.scratch_pattern_ids.sliceFromStart(save_pats),
        );
        self.scratch_pattern_ids.clearFrom(save_pats);

        const rhs_tag_pattern = try self.store.addPattern(self.allocator, .{ .tag = .{
            .name = tag.name,
            .args = rhs_payload_patterns,
        } }, tu_monotype);
        const rhs_bp = try self.store.addBranchPatterns(self.allocator, &.{.{
            .pattern = rhs_tag_pattern,
            .degenerate = false,
        }});

        // --- Build payload comparison ---
        const all_caps = self.scratch_captures.sliceFromStart(save_caps);
        const payload_eq = if (payloads.len == 0)
            try self.emitMirBoolLiteral(module_env, true, region)
        else blk: {
            const lhs_caps = all_caps[0..payloads.len];
            const rhs_caps = all_caps[payloads.len..][0..payloads.len];

            // Chain payload comparisons with short-circuit AND (last to first)
            var payload_result: MIR.ExprId = undefined;
            var j: usize = payloads.len;
            while (j > 0) {
                j -= 1;
                const lhs_lookup = try self.emitMirLookup(lhs_caps[j].symbol, payloads[j], region);
                const rhs_lookup = try self.emitMirLookup(rhs_caps[j].symbol, payloads[j], region);
                const field_eq = try self.lowerFieldEquality(module_env, lhs_lookup, rhs_lookup, payloads[j], ret_monotype, region);

                if (j == payloads.len - 1) {
                    payload_result = field_eq;
                } else {
                    const false_expr = try self.emitMirBoolLiteral(module_env, false, region);
                    payload_result = try self.createBoolMatch(module_env, field_eq, payload_result, false_expr, ret_monotype, region);
                }
            }
            break :blk payload_result;
        };

        // --- Inner match on rhs: Tag(...) => payload_eq, _ => False ---
        const wildcard_pattern = try self.store.addPattern(self.allocator, .wildcard, tu_monotype);
        const wildcard_bp = try self.store.addBranchPatterns(self.allocator, &.{.{
            .pattern = wildcard_pattern,
            .degenerate = false,
        }});
        const false_expr = try self.emitMirBoolLiteral(module_env, false, region);

        const inner_branches = try self.store.addBranches(self.allocator, &.{
            .{ .patterns = rhs_bp, .body = payload_eq, .guard = MIR.ExprId.none },
            .{ .patterns = wildcard_bp, .body = false_expr, .guard = MIR.ExprId.none },
        });
        const inner_match = try self.store.addExpr(self.allocator, .{ .match_expr = .{
            .cond = rhs,
            .branches = inner_branches,
        } }, ret_monotype, region);

        try self.scratch_branches.append(.{
            .patterns = lhs_bp,
            .body = inner_match,
            .guard = MIR.ExprId.none,
        });
    }

    const branches = try self.store.addBranches(self.allocator, self.scratch_branches.sliceFromStart(save_branches));
    return try self.store.addExpr(self.allocator, .{ .match_expr = .{
        .cond = lhs,
        .branches = branches,
    } }, ret_monotype, region);
}

/// List equality: compare lengths, then iterate elements pairwise.
/// Generates:
///   {
///     len = list_len(lhs)
///     match num_is_eq(list_len(rhs), len) {
///       True => {
///         var result = True
///         var i = 0
///         _ = while (num_is_lt(i, len)) {
///           match lowerFieldEquality(list_get_unsafe(lhs, i), list_get_unsafe(rhs, i)) {
///             True => ()
///             _ => { result = False; break }
///           }
///           i = num_plus(i, 1)
///         }
///         result
///       }
///       _ => False
///     }
///   }
fn lowerListEquality(
    self: *Self,
    module_env: *const ModuleEnv,
    lhs: MIR.ExprId,
    rhs: MIR.ExprId,
    lst: anytype,
    ret_monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    self.debugAssertLookupExpr(lhs, "lowerListEquality(lhs)");
    self.debugAssertLookupExpr(rhs, "lowerListEquality(rhs)");

    const u64_mono = self.store.monotype_store.primIdx(.u64);
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(self.allocator, self.currentCommonIdents());
    const unit_mono = self.store.monotype_store.unit_idx;

    // len = list_len(lhs)
    const len_lhs_args = try self.store.addExprSpan(self.allocator, &.{lhs});
    const len_lhs = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
        .op = .list_len,
        .args = len_lhs_args,
    } }, u64_mono, region);
    const len_bind = try self.makeSyntheticBind(u64_mono, false);

    // list_len(rhs)
    const len_rhs_args = try self.store.addExprSpan(self.allocator, &.{rhs});
    const len_rhs = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
        .op = .list_len,
        .args = len_rhs_args,
    } }, u64_mono, region);

    // num_is_eq(list_len(rhs), len)
    const len_lookup = try self.emitMirLookup(len_bind.symbol, u64_mono, region);
    const len_eq_args = try self.store.addExprSpan(self.allocator, &.{ len_rhs, len_lookup });
    const len_eq = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
        .op = .num_is_eq,
        .args = len_eq_args,
    } }, bool_mono, region);

    // --- True branch: iterate elements ---
    const result_bind = try self.makeSyntheticBind(bool_mono, true);
    const i_bind = try self.makeSyntheticBind(u64_mono, true);

    const true_init = try self.emitMirBoolLiteral(module_env, true, region);
    const zero = try self.store.addExpr(self.allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 },
    } }, u64_mono, region);
    const one = try self.store.addExpr(self.allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, u64_mono, region);

    // While condition: if result then num_is_lt(i, len) else False
    const result_lookup_cond = try self.emitMirLookup(result_bind.symbol, bool_mono, region);
    const i_lookup_cond = try self.emitMirLookup(i_bind.symbol, u64_mono, region);
    const len_lookup_cond = try self.emitMirLookup(len_bind.symbol, u64_mono, region);
    const cond_args = try self.store.addExprSpan(self.allocator, &.{ i_lookup_cond, len_lookup_cond });
    const while_len_cond = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
        .op = .num_is_lt,
        .args = cond_args,
    } }, bool_mono, region);
    const while_cond_false = try self.emitMirBoolLiteral(module_env, false, region);
    const while_cond = try self.createBoolMatch(module_env, result_lookup_cond, while_len_cond, while_cond_false, bool_mono, region);

    // list_get_unsafe(lhs, i) and list_get_unsafe(rhs, i)
    const i_lookup_lhs = try self.emitMirLookup(i_bind.symbol, u64_mono, region);
    const lhs_get_args = try self.store.addExprSpan(self.allocator, &.{ lhs, i_lookup_lhs });
    const lhs_elem = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
        .op = .list_get_unsafe,
        .args = lhs_get_args,
    } }, lst.elem, region);

    const i_lookup_rhs = try self.emitMirLookup(i_bind.symbol, u64_mono, region);
    const rhs_get_args = try self.store.addExprSpan(self.allocator, &.{ rhs, i_lookup_rhs });
    const rhs_elem = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
        .op = .list_get_unsafe,
        .args = rhs_get_args,
    } }, lst.elem, region);

    // Compare elements
    const elem_eq = try self.lowerFieldEquality(module_env, lhs_elem, rhs_elem, lst.elem, ret_monotype, region);

    // i = num_plus(i, 1)
    const i_lookup_inc = try self.emitMirLookup(i_bind.symbol, u64_mono, region);
    const inc_args = try self.store.addExprSpan(self.allocator, &.{ i_lookup_inc, one });
    const i_plus_one = try self.store.addExpr(self.allocator, .{ .run_low_level = .{
        .op = .num_plus,
        .args = inc_args,
    } }, u64_mono, region);

    // While body block
    const match_wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_mono);
    const unit_final = try self.emitMirUnitExpr(region);
    const while_body_stmts = try self.store.addStmts(self.allocator, &.{
        .{ .mutate_var = .{ .pattern = result_bind.pattern, .expr = elem_eq } },
        .{ .decl_const = .{ .pattern = match_wildcard, .expr = unit_final } },
        .{ .mutate_var = .{ .pattern = i_bind.pattern, .expr = i_plus_one } },
    });
    const while_body = try self.store.addExpr(self.allocator, .{ .block = .{
        .stmts = while_body_stmts,
        .final_expr = unit_final,
    } }, unit_mono, region);

    // While loop
    const while_expr = try self.store.addExpr(self.allocator, .{ .while_loop = .{
        .cond = while_cond,
        .body = while_body,
    } }, unit_mono, region);

    // True branch block: { var result = True; var i = 0; _ = while(...); result }
    const while_wildcard = try self.store.addPattern(self.allocator, .wildcard, unit_mono);
    const result_lookup = try self.emitMirLookup(result_bind.symbol, bool_mono, region);
    const true_branch_stmts = try self.store.addStmts(self.allocator, &.{
        .{ .decl_var = .{ .pattern = result_bind.pattern, .expr = true_init } },
        .{ .decl_var = .{ .pattern = i_bind.pattern, .expr = zero } },
        .{ .decl_const = .{ .pattern = while_wildcard, .expr = while_expr } },
    });
    try self.registerBoundSymbolDefIfNeeded(result_bind.pattern, true_init);
    try self.registerBoundSymbolDefIfNeeded(i_bind.pattern, zero);
    const true_branch = try self.store.addExpr(self.allocator, .{ .block = .{
        .stmts = true_branch_stmts,
        .final_expr = result_lookup,
    } }, bool_mono, region);

    // match len_eq { True => <loop>, _ => False }
    const false_branch = try self.emitMirBoolLiteral(module_env, false, region);
    const len_match = try self.createBoolMatch(module_env, len_eq, true_branch, false_branch, ret_monotype, region);

    // Outer block: { len = list_len(lhs); match ... }
    const outer_stmts = try self.store.addStmts(self.allocator, &.{
        .{ .decl_const = .{ .pattern = len_bind.pattern, .expr = len_lhs } },
    });
    try self.registerBoundSymbolDefIfNeeded(len_bind.pattern, len_lhs);
    return try self.store.addExpr(self.allocator, .{ .block = .{
        .stmts = outer_stmts,
        .final_expr = len_match,
    } }, ret_monotype, region);
}

/// Lower `e_dot_access` — field access or method call.
fn lowerDotAccess(self: *Self, module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, da: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const receiver = try self.lowerExpr(da.receiver);

    if (da.args) |args_span| {
        // Structural types: .is_eq() is decomposed field-by-field in MIR.
        structural_eq: {
            const rcv_mono_idx = try self.resolveMonotype(da.receiver);
            if (rcv_mono_idx.isNone()) break :structural_eq;
            const rcv_mono = self.store.monotype_store.getMonotype(rcv_mono_idx);
            switch (rcv_mono) {
                // Records, tuples, lists, and unit are always structural.
                .record, .tuple, .list, .unit => {},
                // Tag unions may be nominal or anonymous structural.
                .tag_union => if (self.lookupResolvedDispatchTarget(expr_idx) != null) break :structural_eq,
                else => break :structural_eq,
            }
            if (!std.mem.eql(u8, module_env.getIdent(da.field_name), "is_eq")) break :structural_eq;

            const explicit_args = module_env.store.sliceExpr(args_span);
            try self.bindTypeVarMonotypes(ModuleEnv.varFrom(explicit_args[0]), rcv_mono_idx);
            const rhs = try self.lowerExpr(explicit_args[0]);
            return try self.lowerStructuralEquality(receiver, rhs, rcv_mono_idx, monotype, region);
        }

        const resolved_target = try self.resolveDispatchTargetForDotCall(
            module_env,
            expr_idx,
            da.field_name,
            self.store.typeOf(receiver),
        );
        const method_symbol = try self.resolvedDispatchTargetToSymbol(module_env, resolved_target);

        // Build args as [receiver] ++ explicit_args
        // e.g. list.map(fn) → List.map(list, fn)
        const explicit_args = module_env.store.sliceExpr(args_span);
        // Method-call type bindings are per-call-site state. Keep them scoped.
        const saved_type_var_seen = self.type_var_seen;
        var call_type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        errdefer call_type_var_seen.deinit();
        {
            var it = saved_type_var_seen.iterator();
            while (it.next()) |entry| {
                try call_type_var_seen.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }
        self.type_var_seen = call_type_var_seen;
        const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;
        var call_nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        errdefer call_nominal_cycle_breakers.deinit();
        {
            var it = saved_nominal_cycle_breakers.iterator();
            while (it.next()) |entry| {
                try call_nominal_cycle_breakers.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }
        self.nominal_cycle_breakers = call_nominal_cycle_breakers;
        defer {
            self.type_var_seen.deinit();
            self.type_var_seen = saved_type_var_seen;
            self.nominal_cycle_breakers.deinit();
            self.nominal_cycle_breakers = saved_nominal_cycle_breakers;
        }

        var method_effectful = false;
        var fn_arg_vars: []const types.Var = &.{};
        var resolved_fn = self.types_store.resolveVar(resolved_target.fn_var);
        while (resolved_fn.desc.content == .alias) {
            resolved_fn = self.types_store.resolveVar(self.types_store.getAliasBackingVar(resolved_fn.desc.content.alias));
        }
        if (resolved_fn.desc.content == .structure) {
            fn_arg_vars = switch (resolved_fn.desc.content.structure) {
                .fn_pure => |f| blk: {
                    method_effectful = false;
                    break :blk self.types_store.sliceVars(f.args);
                },
                .fn_effectful => |f| blk: {
                    method_effectful = true;
                    break :blk self.types_store.sliceVars(f.args);
                },
                .fn_unbound => |f| blk: {
                    method_effectful = false;
                    break :blk self.types_store.sliceVars(f.args);
                },
                .record, .record_unbound, .tuple, .nominal_type, .empty_record, .tag_union, .empty_tag_union => {
                    if (builtin.mode == .Debug) std.debug.panic(
                        "CIR→MIR invariant violated: dispatch fn_var resolved to non-function structure '{s}'",
                        .{@tagName(resolved_fn.desc.content.structure)},
                    );
                    unreachable;
                },
            };
        }

        const receiver_monotype = self.store.typeOf(receiver);
        if (fn_arg_vars.len > 0 and !receiver_monotype.isNone()) {
            try self.bindTypeVarMonotypes(fn_arg_vars[0], receiver_monotype);
        }

        var expected_param_monos: []const Monotype.Idx = &.{};
        const dispatch_func_monotype = try self.store.monotype_store.fromTypeVar(
            self.allocator,
            self.types_store,
            resolved_target.fn_var,
            self.currentCommonIdents(),
            &self.type_var_seen,
            &self.nominal_cycle_breakers,
            &self.mono_scratches,
        );
        if (!dispatch_func_monotype.isNone()) {
            const dispatch_mono = self.store.monotype_store.getMonotype(dispatch_func_monotype);
            const dispatch_func = switch (dispatch_mono) {
                .func => |f| f,
                else => {
                    typeBindingInvariant(
                        "lowerDotAccess: dispatch fn_var monotype is not function (method='{s}', monotype='{s}')",
                        .{ module_env.getIdent(da.field_name), @tagName(dispatch_mono) },
                    );
                },
            };
            const dispatch_args = self.store.monotype_store.getIdxSpan(dispatch_func.args);
            if (dispatch_args.len > 0) {
                expected_param_monos = dispatch_args;
            }
        }

        const args_top = self.scratch_expr_ids.top();
        defer self.scratch_expr_ids.clearFrom(args_top);
        try self.scratch_expr_ids.append(receiver);
        for (explicit_args, 0..) |arg_idx, i| {
            if (i + 1 < expected_param_monos.len and !expected_param_monos[i + 1].isNone()) {
                try self.bindTypeVarMonotypes(ModuleEnv.varFrom(arg_idx), expected_param_monos[i + 1]);
            }
            const arg = try self.hoistLambdaArg(try self.lowerExpr(arg_idx));
            try self.scratch_expr_ids.append(arg);
        }
        const lowered_call_args = self.scratch_expr_ids.sliceFromStart(args_top);

        for (explicit_args, 0..) |_, i| {
            const param_i = i + 1;
            if (param_i >= fn_arg_vars.len) break;
            const arg_mono = self.store.typeOf(lowered_call_args[param_i]);
            if (arg_mono.isNone()) continue;
            try self.bindTypeVarMonotypes(fn_arg_vars[param_i], arg_mono);
        }

        const method_func_monotype = blk: {
            const refined = try self.store.monotype_store.fromTypeVar(
                self.allocator,
                self.types_store,
                resolved_target.fn_var,
                self.currentCommonIdents(),
                &self.type_var_seen,
                &self.nominal_cycle_breakers,
                &self.mono_scratches,
            );
            if (!refined.isNone()) {
                const refined_mono = self.store.monotype_store.getMonotype(refined);
                if (refined_mono != .func) {
                    typeBindingInvariant(
                        "lowerDotAccess: refined dispatch fn_var monotype is not function (method='{s}', monotype='{s}')",
                        .{ module_env.getIdent(da.field_name), @tagName(refined_mono) },
                    );
                }
                break :blk refined;
            }

            const mono_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(mono_top);
            for (lowered_call_args) |arg_expr| {
                try self.mono_scratches.idxs.append(self.store.typeOf(arg_expr));
            }
            break :blk try self.buildFuncMonotype(
                self.mono_scratches.idxs.sliceFromStart(mono_top),
                monotype,
                method_effectful,
            );
        };

        // Ensure the method body is lowered so codegen can find it.
        const lowered_method_symbol = try self.specializeMethod(method_symbol, method_func_monotype);
        const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = lowered_method_symbol }, method_func_monotype, region);

        const args = try self.store.addExprSpan(self.allocator, lowered_call_args);

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
        try self.registerBoundSymbolDefIfNeeded(ext.pattern, ext.expr);
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
    const cir_args = module_env.store.sliceExpr(tvd.args);
    const mono_top = self.mono_scratches.idxs.top();
    defer self.mono_scratches.idxs.clearFrom(mono_top);
    for (cir_args) |arg_idx| {
        try self.mono_scratches.idxs.append(try self.resolveMonotype(arg_idx));
    }
    const func_monotype = try self.buildFuncMonotype(self.mono_scratches.idxs.sliceFromStart(mono_top), monotype, false);
    const resolved_target = try self.resolveDispatchTargetForExpr(module_env, expr_idx, tvd.method_name);
    const method_symbol = try self.resolvedDispatchTargetToSymbol(module_env, resolved_target);

    const args = try self.lowerExprSpan(module_env, tvd.args);
    const lowered_method_symbol = try self.specializeMethod(method_symbol, func_monotype);
    const func_expr = try self.store.addExpr(self.allocator, .{ .lookup = lowered_method_symbol }, func_monotype, region);

    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = func_expr,
        .args = args,
    } }, monotype, region);
}

/// Find the module index for a given origin module ident.
fn moduleIndexForEnv(self: *Self, env: *const ModuleEnv) ?u32 {
    for (self.all_module_envs, 0..) |candidate, idx| {
        if (candidate == env) return @intCast(idx);
    }
    return null;
}

/// Find the module index for a given origin module ident.
/// Resolution is by qualified module name text, so it remains correct even
/// when lowering specialized external definitions across module contexts.
fn findModuleForOrigin(self: *Self, source_env: *const ModuleEnv, origin_module: Ident.Idx) u32 {
    const source_module_idx = self.moduleIndexForEnv(source_env) orelse {
        std.debug.panic(
            "findModuleForOrigin: source module env not found in all_module_envs (current_module_idx={d})",
            .{self.current_module_idx},
        );
    };

    if (origin_module.eql(source_env.qualified_module_ident)) {
        return source_module_idx;
    }

    const origin_name = source_env.getIdent(origin_module);
    for (self.all_module_envs, 0..) |candidate_env, idx| {
        const candidate_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
        if (std.mem.eql(u8, origin_name, candidate_name)) {
            return @intCast(idx);
        }
    }

    std.debug.panic(
        "findModuleForOrigin: origin module not found (source_module_idx={d}, origin='{s}', origin_ident={d})",
        .{ source_module_idx, origin_name, @as(u32, @bitCast(origin_module)) },
    );
}

fn findModuleForOriginMaybe(self: *Self, source_env: *const ModuleEnv, origin_module: Ident.Idx) ?u32 {
    const source_module_idx = self.moduleIndexForEnv(source_env) orelse return null;
    if (origin_module.eql(source_env.qualified_module_ident)) return source_module_idx;

    const origin_name = identTextIfOwnedBy(source_env, origin_module) orelse return null;
    for (self.all_module_envs, 0..) |candidate_env, idx| {
        const candidate_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
        if (std.mem.eql(u8, origin_name, candidate_name)) return @intCast(idx);
    }
    return null;
}

fn resolvedTargetIsUsable(
    self: *Self,
    source_env: *const ModuleEnv,
    method_name: Ident.Idx,
    resolved_target: types.StaticDispatchConstraint.ResolvedTarget,
) bool {
    const method_name_text = source_env.getIdent(method_name);
    const target_method_text = identTextIfOwnedBy(source_env, resolved_target.method_ident) orelse return false;
    if (!identMatchesMethodName(target_method_text, method_name_text)) return false;
    return self.findModuleForOriginMaybe(source_env, resolved_target.origin_module) != null;
}

fn lookupResolvedDispatchTarget(self: *const Self, expr_idx: CIR.Expr.Idx) ?ResolvedDispatchTarget {
    const key = (@as(u64, self.current_module_idx) << 32) | @as(u64, @intFromEnum(expr_idx));
    return self.resolved_dispatch_targets.get(key);
}

fn staticDispatchKey(self: *const Self, expr_idx: CIR.Expr.Idx) u64 {
    return (@as(u64, self.current_module_idx) << 32) | @as(u64, @intFromEnum(expr_idx));
}

fn lookupDispatchConstraintForExpr(
    self: *const Self,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
) ?types.StaticDispatchConstraint {
    var found: ?types.StaticDispatchConstraint = null;
    for (self.types_store.sliceAllStaticDispatchConstraints()) |constraint| {
        if (constraint.source_expr_idx != @intFromEnum(expr_idx)) continue;
        if (!constraint.fn_name.eql(method_name)) continue;
        if (found) |existing| {
            const existing_resolved = !existing.resolved_target.isNone();
            const candidate_resolved = !constraint.resolved_target.isNone();

            // Constraint solving can emit multiple constraints for the same
            // expression+method pair (different fn_vars), especially for
            // desugared binops and merged constraint sets. Prefer a resolved
            // target when available; otherwise keep the first unresolved one.
            if (existing_resolved and candidate_resolved) {
                if (std.debug.runtime_safety and
                    (!existing.resolved_target.origin_module.eql(constraint.resolved_target.origin_module) or
                        !existing.resolved_target.method_ident.eql(constraint.resolved_target.method_ident)))
                {
                    std.debug.panic(
                        "lookupDispatchConstraintForExpr: conflicting resolved targets for expr={d} method={d}",
                        .{ @intFromEnum(expr_idx), method_name.idx },
                    );
                }
            } else if (!existing_resolved and candidate_resolved) {
                found = constraint;
            }
            continue;
        }
        found = constraint;
    }
    return found;
}

fn resolveDispatchTargetForExpr(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
) Allocator.Error!ResolvedDispatchTarget {
    if (self.lookupResolvedDispatchTarget(expr_idx)) |cached| return cached;

    const constraint = self.lookupDispatchConstraintForExpr(expr_idx, method_name) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "resolveDispatchTargetForExpr: no static dispatch constraint for expr={d} method='{s}'",
                .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
            );
        }
        unreachable;
    };

    const resolved = blk: {
        if (!constraint.resolved_target.isNone() and
            self.resolvedTargetIsUsable(module_env, method_name, constraint.resolved_target))
        {
            break :blk ResolvedDispatchTarget{
                .origin = constraint.resolved_target.origin_module,
                .method_ident = constraint.resolved_target.method_ident,
                .fn_var = constraint.fn_var,
            };
        }
        break :blk try self.resolveUnresolvedTypeVarDispatchTarget(module_env, method_name, constraint);
    };

    try self.resolved_dispatch_targets.put(self.staticDispatchKey(expr_idx), resolved);
    return resolved;
}

fn monotypeDispatchCompatible(
    self: *Self,
    expected: Monotype.Idx,
    actual: Monotype.Idx,
) Allocator.Error!bool {
    if (expected.isNone() or actual.isNone()) return true;
    return try self.monotypesStructurallyEqual(expected, actual);
}

fn resolveDispatchTargetForDotCall(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
    receiver_monotype: Monotype.Idx,
) Allocator.Error!ResolvedDispatchTarget {
    var first_candidate: ?ResolvedDispatchTarget = null;
    var unresolved_match: ?ResolvedDispatchTarget = null;
    var resolved_match: ?ResolvedDispatchTarget = null;

    for (self.types_store.sliceAllStaticDispatchConstraints()) |constraint| {
        if (constraint.source_expr_idx != @intFromEnum(expr_idx)) continue;
        if (!constraint.fn_name.eql(method_name)) continue;

        const maybe_candidate: ?ResolvedDispatchTarget = blk: {
            if (!constraint.resolved_target.isNone() and
                self.resolvedTargetIsUsable(module_env, method_name, constraint.resolved_target))
            {
                break :blk ResolvedDispatchTarget{
                    .origin = constraint.resolved_target.origin_module,
                    .method_ident = constraint.resolved_target.method_ident,
                    .fn_var = constraint.fn_var,
                };
            }
            break :blk null;
        };
        const candidate = maybe_candidate orelse continue;

        if (first_candidate == null) first_candidate = candidate;

        const fn_mono = try self.store.monotype_store.fromTypeVar(
            self.allocator,
            self.types_store,
            constraint.fn_var,
            self.currentCommonIdents(),
            &self.type_var_seen,
            &self.nominal_cycle_breakers,
            &self.mono_scratches,
        );
        if (fn_mono.isNone()) {
            if (!constraint.resolved_target.isNone()) {
                if (resolved_match == null) resolved_match = candidate;
            } else if (unresolved_match == null) {
                unresolved_match = candidate;
            }
            continue;
        }

        const mono = self.store.monotype_store.getMonotype(fn_mono);
        if (mono != .func) continue;
        const fn_args = self.store.monotype_store.getIdxSpan(mono.func.args);
        const compatible = if (fn_args.len == 0)
            true
        else
            try self.monotypeDispatchCompatible(fn_args[0], receiver_monotype);
        if (!compatible) continue;

        if (!constraint.resolved_target.isNone()) {
            if (resolved_match == null) resolved_match = candidate;
        } else if (unresolved_match == null) {
            unresolved_match = candidate;
        }
    }

    if (resolved_match) |target| return target;
    if (unresolved_match) |target| return target;
    if (first_candidate) |target| return target;
    return self.resolveDispatchTargetForExpr(module_env, expr_idx, method_name);
}

fn identMatchesMethodName(full_name: []const u8, method_name: []const u8) bool {
    if (std.mem.eql(u8, full_name, method_name)) return true;
    if (full_name.len <= method_name.len + 1) return false;
    const suffix_start = full_name.len - method_name.len;
    return full_name[suffix_start - 1] == '.' and std.mem.eql(u8, full_name[suffix_start..], method_name);
}

fn resolveUnresolvedTypeVarDispatchTarget(
    self: *Self,
    module_env: *const ModuleEnv,
    method_name: Ident.Idx,
    constraint: types.StaticDispatchConstraint,
) Allocator.Error!ResolvedDispatchTarget {
    const desired_func_monotype = try self.store.monotype_store.fromTypeVar(
        self.allocator,
        self.types_store,
        constraint.fn_var,
        self.currentCommonIdents(),
        &self.type_var_seen,
        &self.nominal_cycle_breakers,
        &self.mono_scratches,
    );
    if (desired_func_monotype.isNone()) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "resolveUnresolvedTypeVarDispatchTarget: unresolved fn_var monotype for method '{s}'",
                .{module_env.getIdent(method_name)},
            );
        }
        unreachable;
    }

    const method_name_text = module_env.getIdent(method_name);
    var found_target: ?ResolvedDispatchTarget = null;

    for (self.all_module_envs, 0..) |candidate_env, candidate_module_idx_usize| {
        const candidate_module_idx: u32 = @intCast(candidate_module_idx_usize);
        const defs = candidate_env.store.sliceDefs(candidate_env.all_defs);
        for (defs) |def_idx| {
            const def = candidate_env.store.getDef(def_idx);
            const pattern = candidate_env.store.getPattern(def.pattern);
            if (pattern != .assign) continue;

            const method_ident = pattern.assign.ident;
            const full_name = candidate_env.getIdent(method_ident);
            if (!identMatchesMethodName(full_name, method_name_text)) continue;
            if (candidate_env.getExposedNodeIndexById(method_ident) == null) continue;

            const candidate_expr_var: types.Var = ModuleEnv.varFrom(def.expr);
            var candidate_mono = try self.monotypeFromTypeVarInStore(
                candidate_module_idx,
                &candidate_env.types,
                candidate_expr_var,
            );
            if (candidate_mono.isNone()) continue;
            if (candidate_module_idx != self.current_module_idx) {
                candidate_mono = try self.remapMonotypeBetweenModules(
                    candidate_mono,
                    candidate_module_idx,
                    self.current_module_idx,
                );
            }
            if (!try self.monotypesStructurallyEqual(candidate_mono, desired_func_monotype)) continue;

            const candidate_origin_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
            const mapped_origin = module_env.common.findIdent(candidate_origin_name) orelse
                try @constCast(module_env).insertIdent(Ident.for_text(candidate_origin_name));
            const candidate_method_name = candidate_env.getIdent(method_ident);
            const mapped_method_ident = module_env.common.findIdent(candidate_method_name) orelse
                try @constCast(module_env).insertIdent(Ident.for_text(candidate_method_name));

            const candidate_target = ResolvedDispatchTarget{
                .origin = mapped_origin,
                .method_ident = mapped_method_ident,
                .fn_var = constraint.fn_var,
            };
            if (found_target) |existing| {
                if (std.debug.runtime_safety and (!existing.origin.eql(candidate_target.origin) or !existing.method_ident.eql(candidate_target.method_ident))) {
                    std.debug.panic(
                        "resolveUnresolvedTypeVarDispatchTarget: ambiguous dispatch for method '{s}'",
                        .{method_name_text},
                    );
                }
                continue;
            }
            found_target = candidate_target;
        }
    }

    return found_target orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "resolveUnresolvedTypeVarDispatchTarget: no candidate for method '{s}'",
                .{method_name_text},
            );
        }
        unreachable;
    };
}

fn monotypeFromTypeVarInStore(
    self: *Self,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
) Allocator.Error!Monotype.Idx {
    var local_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    defer local_seen.deinit();
    var local_cycles = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    defer local_cycles.deinit();

    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    self.mono_scratches.ident_store = self.all_module_envs[module_idx].getIdentStoreConst();
    self.mono_scratches.module_env = self.all_module_envs[module_idx];
    defer {
        self.mono_scratches.ident_store = saved_ident_store;
        self.mono_scratches.module_env = saved_module_env;
    }

    return self.store.monotype_store.fromTypeVar(
        self.allocator,
        store_types,
        var_,
        ModuleEnv.CommonIdents.find(&self.all_module_envs[module_idx].common),
        &local_seen,
        &local_cycles,
        &self.mono_scratches,
    );
}

fn resolvedDispatchTargetToSymbol(self: *Self, source_env: *const ModuleEnv, target: ResolvedDispatchTarget) Allocator.Error!MIR.Symbol {
    const target_module_idx = self.findModuleForOrigin(source_env, target.origin);
    const target_env = self.all_module_envs[target_module_idx];
    const method_name = source_env.getIdent(target.method_ident);

    const target_ident = target_env.common.findIdent(method_name) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "resolvedDispatchTargetToSymbol: method '{s}' not found in target module {d}",
                .{ method_name, target_module_idx },
            );
        }
        unreachable;
    };
    const target_node_idx = target_env.getExposedNodeIndexById(target_ident) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "resolvedDispatchTargetToSymbol: exposed node not found for method '{s}' in module {d}",
                .{ method_name, target_module_idx },
            );
        }
        unreachable;
    };
    if (!target_env.store.isDefNode(target_node_idx)) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "resolvedDispatchTargetToSymbol: exposed node for method '{s}' is not a def node (module={d}, node={d})",
                .{ method_name, target_module_idx, target_node_idx },
            );
        }
        unreachable;
    }
    return self.internExternalDefSymbol(target_module_idx, target_node_idx);
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
            .as,
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
            => {
                if (builtin.mode == .Debug) std.debug.panic(
                    "CIR→MIR invariant violated: top-level def has non-assign pattern '{s}'",
                    .{@tagName(pat)},
                );
                unreachable;
            },
        }
    }
    return null;
}

fn findDefExprByMetadata(self: *Self, symbol_meta: SymbolMetadata) ?CIR.Expr.Idx {
    return switch (symbol_meta) {
        .local_ident => |local| self.findDefExprBySymbol(local.module_idx, local.ident_idx),
        .external_def => |ext| blk: {
            const target_env = self.all_module_envs[ext.module_idx];
            if (!target_env.store.isDefNode(ext.def_node_idx)) break :blk null;
            const def_idx: CIR.Def.Idx = @enumFromInt(ext.def_node_idx);
            break :blk target_env.store.getDef(def_idx).expr;
        },
    };
}

/// Ensure a method definition is lowered (for cross-module dispatch),
/// returning the symbol to call. For polymorphic specialization this may
/// be a synthetic symbol unique to (method symbol, caller monotype).
fn specializeMethod(self: *Self, symbol: MIR.Symbol, caller_func_monotype: ?Monotype.Idx) Allocator.Error!MIR.Symbol {
    const symbol_key: u64 = @bitCast(symbol);
    const symbol_meta = self.getSymbolMetadata(symbol);
    const symbol_module_idx = symbolMetadataModuleIdx(symbol_meta);
    const symbol_display_name = symbolMetadataDisplayName(self, symbol_meta);

    if (caller_func_monotype) |caller_monotype| {
        if (!caller_monotype.isNone()) {
            const normalized_caller_monotype = try self.normalizeCallerMonotypeForSymbolModule(
                symbol_module_idx,
                caller_monotype,
            );
            const spec_key = polySpecKey(symbol_key, normalized_caller_monotype);

            if (try self.lookupPolySpecialization(symbol_key, normalized_caller_monotype)) |spec_symbol| {
                return spec_symbol;
            }

            // Recursive lowering for a different caller monotype needs a new
            // specialization symbol; a symbol that is currently in-progress is
            // only valid for the same active specialization.
            if (self.in_progress_defs.contains(symbol_key)) {
                const active_monotype = self.in_progress_symbol_monotypes.get(symbol_key) orelse Monotype.Idx.none;
                const same_specialization = if (active_monotype.isNone())
                    false
                else
                    try self.monotypesStructurallyEqual(active_monotype, normalized_caller_monotype);

                if (!same_specialization) {
                    const def_expr = self.findDefExprByMetadata(symbol_meta) orelse {
                        if (std.debug.runtime_safety) {
                            std.debug.panic(
                                "specializeMethod: definition not found for in-progress '{s}' (module={d})",
                                .{ symbol_display_name, symbol_module_idx },
                            );
                        }
                        unreachable;
                    };
                    const spec_symbol = try self.makeSyntheticSymbol(symbol);
                    try self.poly_specializations.put(spec_key, spec_symbol);
                    errdefer _ = self.poly_specializations.remove(spec_key);
                    _ = try self.lowerExternalDefWithType(spec_symbol, def_expr, caller_monotype);
                    return spec_symbol;
                }
            }

            if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                const cached_monotype = self.store.typeOf(cached_expr);
                if (!try self.monotypesStructurallyEqual(cached_monotype, normalized_caller_monotype)) {
                    const def_expr = self.findDefExprByMetadata(symbol_meta) orelse {
                        if (std.debug.runtime_safety) {
                            std.debug.panic(
                                "specializeMethod: definition not found for cached '{s}' (module={d})",
                                .{ symbol_display_name, symbol_module_idx },
                            );
                        }
                        unreachable;
                    };
                    const spec_symbol = try self.makeSyntheticSymbol(symbol);
                    try self.poly_specializations.put(spec_key, spec_symbol);
                    errdefer _ = self.poly_specializations.remove(spec_key);
                    _ = try self.lowerExternalDefWithType(spec_symbol, def_expr, caller_monotype);
                    return spec_symbol;
                }
            }
        }
    }

    if (self.lowered_symbols.contains(symbol_key)) return symbol;

    const def_expr = self.findDefExprByMetadata(symbol_meta) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "specializeMethod: definition not found for '{s}' (module={d})",
                .{ symbol_display_name, symbol_module_idx },
            );
        }
        unreachable;
    };
    _ = try self.lowerExternalDefWithType(symbol, def_expr, caller_func_monotype);
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
    const symbol_module_idx = symbolMetadataModuleIdx(symbol_meta);
    const normalized_caller_monotype = if (caller_monotype) |cm|
        try self.normalizeCallerMonotypeForSymbolModule(symbol_module_idx, cm)
    else
        null;
    if (builtin.mode == .Debug) {
        switch (symbol_meta) {
            .external_def => |ext| {
                const target_env = self.all_module_envs[ext.module_idx];
                std.debug.assert(target_env.store.isDefNode(ext.def_node_idx));
                const def_idx: CIR.Def.Idx = @enumFromInt(ext.def_node_idx);
                const expected_expr = target_env.store.getDef(def_idx).expr;
                if (expected_expr != cir_expr_idx) {
                    std.debug.panic(
                        "lowerExternalDefWithType: CIR expr mismatch for external symbol (module={d}, node={d}): expected expr={d}, got expr={d}",
                        .{ ext.module_idx, ext.def_node_idx, @intFromEnum(expected_expr), @intFromEnum(cir_expr_idx) },
                    );
                }
            },
            .local_ident => {},
        }
    }
    const target_type_var: types.Var = switch (symbol_meta) {
        .external_def => |ext| @enumFromInt(ext.def_node_idx),
        .local_ident => ModuleEnv.varFrom(cir_expr_idx),
    };

    // Check cache
    if (self.lowered_symbols.get(symbol_key)) |cached| {
        return cached;
    }

    // Recursion guard
    if (self.in_progress_defs.contains(symbol_key)) {
        const recursive_lookup_monotype = blk: {
            if (normalized_caller_monotype) |cm| {
                if (!cm.isNone()) break :blk cm;
            }

            if (self.in_progress_symbol_monotypes.get(symbol_key)) |active| {
                if (!active.isNone()) break :blk active;
            }

            const derived = try self.store.monotype_store.fromTypeVar(
                self.allocator,
                self.types_store,
                target_type_var,
                self.currentCommonIdents(),
                &self.type_var_seen,
                &self.nominal_cycle_breakers,
                &self.mono_scratches,
            );
            if (!derived.isNone()) break :blk derived;

            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "MIR Lower invariant: recursive external def lookup monotype unresolved for symbol={d}",
                    .{symbol.raw()},
                );
            }
            unreachable;
        };
        return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, recursive_lookup_monotype, Region.zero());
    }

    try self.in_progress_defs.put(symbol_key, {});
    const active_monotype: Monotype.Idx = if (normalized_caller_monotype) |cm| cm else Monotype.Idx.none;
    try self.in_progress_symbol_monotypes.put(symbol_key, active_monotype);
    errdefer _ = self.in_progress_symbol_monotypes.remove(symbol_key);

    // Switch module context if needed.
    const switching_module = symbol_module_idx != self.current_module_idx;
    const saved_module_idx = self.current_module_idx;
    const saved_pattern_scope = self.current_pattern_scope;
    const saved_types_store = self.types_store;
    const saved_type_var_seen = self.type_var_seen;
    const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    self.current_pattern_scope = symbol_key;
    if (switching_module) {
        self.current_module_idx = symbol_module_idx;
        self.types_store = &self.all_module_envs[symbol_module_idx].types;
        self.mono_scratches.ident_store = self.all_module_envs[symbol_module_idx].getIdentStoreConst();
        self.mono_scratches.module_env = self.all_module_envs[symbol_module_idx];
    }

    // Always isolate type_var_seen per external definition lowering.
    // Reusing a shared cache across polymorphic specializations can pin flex
    // and rigid vars to an earlier specialization.
    self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);

    // Pre-bind type_var_seen with the caller's concrete types so that
    // flex/rigid vars in the definition resolve to concrete monotypes instead
    // of unit. Each definition has its own type vars (not unified with
    // the caller's instantiated copies), so they must be seeded here.
    // This must happen for BOTH cross-module AND same-module calls, because
    // same-module polymorphic functions (e.g., List.append called from
    // range_to within the Builtin module) have their own rigid type vars
    // from type annotations that are not unified with the caller's vars.
    if (normalized_caller_monotype) |cmt| {
        if (!cmt.isNone()) {
            try self.bindTypeVarMonotypes(target_type_var, cmt);
        }
    }

    defer {
        self.type_var_seen.deinit();
        self.type_var_seen = saved_type_var_seen;
        self.nominal_cycle_breakers.deinit();
        self.nominal_cycle_breakers = saved_nominal_cycle_breakers;
        self.current_pattern_scope = saved_pattern_scope;
        if (switching_module) {
            self.types_store = saved_types_store;
            self.current_module_idx = saved_module_idx;
            self.mono_scratches.ident_store = saved_ident_store;
            self.mono_scratches.module_env = saved_module_env;
        }
    }

    const result = try self.lowerExpr(cir_expr_idx);

    // Cache the result and register the symbol definition
    try self.lowered_symbols.put(symbol_key, result);
    try self.store.registerSymbolDef(self.allocator, symbol, result);

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
        if (self.identsStructurallyEqual(mono_field.name, field_name)) {
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
        if (!self.identsTagNameEquivalent(mono_tag.name, tag_name)) continue;

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
                "bindTypeVarMonotypes: conflicting monotype binding for type var root {d} (existing={d}, new={d})",
                .{ @intFromEnum(resolved.var_), @intFromEnum(existing), @intFromEnum(monotype) },
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

            if (origin.eql(common.builtin_module) and builtinPrimForNominal(ident, common) != null) {
                switch (mono) {
                    .prim => {},
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal prim): expected prim monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                }
                return;
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
                        .flex, .rigid => break :rows,
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
                        if (self.identsTagNameEquivalent(existing_name, tag_name)) {
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
                        .flex, .rigid => break :rows,
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
                    if (self.identsTagNameEquivalent(seen_name, mono_tag.name)) {
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

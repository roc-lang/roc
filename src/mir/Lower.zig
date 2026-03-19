//! CIR → MIR Lowering Pass
//!
//! Converts polymorphic, sugar-rich CIR expressions into monomorphic,
//! desugared MIR expressions. Callable instantiation must already be decided
//! by explicit monomorphization before this pass runs.
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
const Monomorphize = @import("Monomorphize.zig");

const Ident = base.Ident;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Allocator = std.mem.Allocator;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const Self = @This();

const ResolvedDispatchTarget = struct {
    origin: Ident.Idx,
    method_ident: Ident.Idx,
    fn_var: types.Var,
    module_idx: ?u32 = null,
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

/// Explicit callable-instantiation decisions computed before lowering.
monomorphization: *const Monomorphize.Result,

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

/// Optional for-clause type substitutions for lowering platform modules
/// against concrete app types.
type_scope: ?*const types.TypeScope,
type_scope_module_idx: ?u32,
type_scope_caller_module_idx: ?u32,

/// Map from ((scope_key << 64) | (module_idx << 32 | CIR.Pattern.Idx)) → MIR.Symbol
/// Used to resolve CIR local lookups to global symbols.
pattern_symbols: std.AutoHashMap(u128, MIR.Symbol),

/// Concrete monotype for each bound MIR symbol introduced from a lowered pattern.
/// This is authoritative for local lookups that do not have a symbol_def yet
/// (lambda params, destructures, closure locals, synthetic temporaries).
symbol_monotypes: std.AutoHashMap(u64, Monotype.Idx),

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

/// Cache for already-lowered proc instances chosen by monomorphization.
lowered_proc_insts: std.AutoHashMap(u32, MIR.ExprId),

/// Metadata for opaque symbol IDs; populated at symbol construction time.
symbol_metadata: std.AutoHashMap(u64, SymbolMetadata),

/// Counter for generating synthetic ident indices for polymorphic specializations.
/// Counts down from NONE - 1 to avoid collision with real idents.
next_synthetic_ident: u29,

/// Tracks symbols currently being lowered (recursion guard).
in_progress_defs: std.AutoHashMap(u64, void),

/// Tracks proc instances currently being lowered (recursion guard).
in_progress_proc_insts: std.AutoHashMap(u32, MIR.ExprId),

/// Proc-inst context for context-sensitive monomorphized lookup/call resolution.
current_proc_inst_context: Monomorphize.ProcInstId,

/// Root expression currently being lowered when no proc-inst context is active.
current_root_expr_context: ?CIR.Expr.Idx,

/// Monotype currently being lowered for each in-progress symbol.
/// Used to detect in-progress calls that need a distinct specialization symbol.
in_progress_symbol_monotypes: std.AutoHashMap(u64, Monotype.Idx),

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
    monomorphization: *const Monomorphize.Result,
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
        .monomorphization = monomorphization,
        .all_module_envs = all_module_envs,
        .types_store = types_store,
        .current_module_idx = current_module_idx,
        .current_pattern_scope = 0,
        .app_module_idx = app_module_idx,
        .type_scope = null,
        .type_scope_module_idx = null,
        .type_scope_caller_module_idx = null,
        .pattern_symbols = std.AutoHashMap(u128, MIR.Symbol).init(allocator),
        .symbol_monotypes = std.AutoHashMap(u64, Monotype.Idx).init(allocator),
        .type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .lowered_symbols = std.AutoHashMap(u64, MIR.ExprId).init(allocator),
        .lowered_proc_insts = std.AutoHashMap(u32, MIR.ExprId).init(allocator),
        .symbol_metadata = std.AutoHashMap(u64, SymbolMetadata).init(allocator),
        .next_synthetic_ident = Ident.Idx.NONE.idx - 1,
        .in_progress_defs = std.AutoHashMap(u64, void).init(allocator),
        .in_progress_proc_insts = std.AutoHashMap(u32, MIR.ExprId).init(allocator),
        .current_proc_inst_context = .none,
        .current_root_expr_context = null,
        .in_progress_symbol_monotypes = std.AutoHashMap(u64, Monotype.Idx).init(allocator),
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
            ms.module_idx = current_module_idx;
            ms.all_module_envs = all_module_envs;
            break :blk ms;
        },
    };
}

pub fn deinit(self: *Self) void {
    self.pattern_symbols.deinit();
    self.symbol_monotypes.deinit();
    self.type_var_seen.deinit();
    self.nominal_cycle_breakers.deinit();
    self.lowered_symbols.deinit();
    self.lowered_proc_insts.deinit();
    self.symbol_metadata.deinit();
    self.in_progress_defs.deinit();
    self.in_progress_proc_insts.deinit();
    self.in_progress_symbol_monotypes.deinit();
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

/// Provide platform for-clause type substitutions for the given module so
/// MIR monotype resolution can use the concrete caller types during lowering.
pub fn setTypeScope(
    self: *Self,
    module_idx: u32,
    type_scope: *const types.TypeScope,
    caller_module_idx: u32,
) Allocator.Error!void {
    self.type_scope = type_scope;
    self.type_scope_module_idx = module_idx;
    self.type_scope_caller_module_idx = caller_module_idx;
    try self.seedTypeScopeBindingsInStore(
        self.current_module_idx,
        self.types_store,
        &self.type_var_seen,
    );
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

fn moduleOwnsIdent(env: *const ModuleEnv, ident: Ident.Idx) bool {
    const ident_store = env.getIdentStoreConst();
    const bytes = ident_store.interner.bytes.items.items;
    const start: usize = @intCast(ident.idx);
    if (start >= bytes.len) return false;

    const tail = bytes[start..];
    const end_rel = std.mem.indexOfScalar(u8, tail, 0) orelse return false;
    const text = tail[0..end_rel];

    const roundtrip = ident_store.findByString(text) orelse return false;
    return roundtrip.eql(ident);
}

fn getOwnedIdentText(env: *const ModuleEnv, ident: Ident.Idx) []const u8 {
    if (builtin.mode == .Debug) std.debug.assert(moduleOwnsIdent(env, ident));
    return env.getIdent(ident);
}

fn monomorphizationRootExprContext(self: *const Self, context_proc_inst: Monomorphize.ProcInstId) ?CIR.Expr.Idx {
    return if (context_proc_inst.isNone()) self.current_root_expr_context else null;
}

fn patternScopeForProcInst(proc_inst_id: Monomorphize.ProcInstId) u64 {
    if (proc_inst_id.isNone()) return 0;
    return (@as(u64, 1) << 63) | (@as(u64, @intFromEnum(proc_inst_id)) + 1);
}

fn identTextForCompare(self: *const Self, ident: Ident.Idx) ?[]const u8 {
    if (moduleOwnsIdent(self.all_module_envs[self.current_module_idx], ident)) {
        return getOwnedIdentText(self.all_module_envs[self.current_module_idx], ident);
    }

    for (self.all_module_envs, 0..) |module_env, module_idx| {
        if (module_idx == self.current_module_idx) continue;
        if (moduleOwnsIdent(module_env, ident)) {
            return getOwnedIdentText(module_env, ident);
        }
    }

    return null;
}

fn labelTextForCompare(self: *const Self, label: anytype) ?[]const u8 {
    return switch (@TypeOf(label)) {
        Ident.Idx => self.identTextForCompare(label),
        Monotype.Name => label.text(self.all_module_envs),
        else => @compileError("unsupported label type"),
    };
}

fn identsStructurallyEqual(self: *const Self, lhs: anytype, rhs: anytype) bool {
    if (@TypeOf(lhs) == Ident.Idx and @TypeOf(rhs) == Ident.Idx and lhs.eql(rhs)) return true;
    if (@TypeOf(lhs) == Monotype.Name and @TypeOf(rhs) == Monotype.Name and lhs.eql(rhs)) return true;

    const lhs_text = self.labelTextForCompare(lhs) orelse return false;
    const rhs_text = self.labelTextForCompare(rhs) orelse return false;
    return std.mem.eql(u8, lhs_text, rhs_text);
}

fn identLastSegment(text: []const u8) []const u8 {
    const dot = std.mem.lastIndexOfScalar(u8, text, '.') orelse return text;
    return text[dot + 1 ..];
}

fn identsTagNameEquivalent(self: *const Self, lhs: anytype, rhs: anytype) bool {
    if (self.identsStructurallyEqual(lhs, rhs)) return true;

    const lhs_text = self.labelTextForCompare(lhs) orelse return false;
    const rhs_text = self.labelTextForCompare(rhs) orelse return false;
    return std.mem.eql(u8, identLastSegment(lhs_text), identLastSegment(rhs_text));
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
                    .name = field.name,
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
                                tag.name.ident.idx,
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
                    .name = tag.name,
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

fn importMonotypeFromStore(
    self: *Self,
    source_store: *const Monotype.Store,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone()) return monotype;

    if (source_store == &self.store.monotype_store and from_module_idx == to_module_idx) {
        return monotype;
    }

    var imported = std.AutoHashMap(Monotype.Idx, Monotype.Idx).init(self.allocator);
    defer imported.deinit();

    return self.importMonotypeFromStoreRec(
        source_store,
        monotype,
        from_module_idx,
        to_module_idx,
        &imported,
    );
}

fn importMonotypeFromStoreRec(
    self: *Self,
    source_store: *const Monotype.Store,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
    imported: *std.AutoHashMap(Monotype.Idx, Monotype.Idx),
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone()) return monotype;
    if (source_store == &self.store.monotype_store and from_module_idx == to_module_idx) {
        return monotype;
    }
    if (imported.get(monotype)) |existing| return existing;

    const mono = source_store.getMonotype(monotype);
    switch (mono) {
        .unit => return self.store.monotype_store.unit_idx,
        .prim => |prim| return self.store.monotype_store.primIdx(prim),
        .recursive_placeholder => {
            if (builtin.mode == .Debug) {
                std.debug.panic("importMonotypeFromStore: unexpected recursive_placeholder", .{});
            }
            unreachable;
        },
        .list, .box, .tuple, .func, .record, .tag_union => {},
    }

    const placeholder = try self.store.monotype_store.addMonotype(self.allocator, .recursive_placeholder);
    try imported.put(monotype, placeholder);

    const mapped_mono: Monotype.Monotype = switch (mono) {
        .list => |list_mono| .{ .list = .{
            .elem = try self.importMonotypeFromStoreRec(
                source_store,
                list_mono.elem,
                from_module_idx,
                to_module_idx,
                imported,
            ),
        } },
        .box => |box_mono| .{ .box = .{
            .inner = try self.importMonotypeFromStoreRec(
                source_store,
                box_mono.inner,
                from_module_idx,
                to_module_idx,
                imported,
            ),
        } },
        .tuple => |tuple_mono| blk: {
            const idx_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(idx_top);

            for (source_store.getIdxSpan(tuple_mono.elems)) |elem_mono| {
                try self.mono_scratches.idxs.append(try self.importMonotypeFromStoreRec(
                    source_store,
                    elem_mono,
                    from_module_idx,
                    to_module_idx,
                    imported,
                ));
            }

            break :blk .{ .tuple = .{
                .elems = try self.store.monotype_store.addIdxSpan(
                    self.allocator,
                    self.mono_scratches.idxs.sliceFromStart(idx_top),
                ),
            } };
        },
        .func => |func_mono| blk: {
            const idx_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(idx_top);

            for (source_store.getIdxSpan(func_mono.args)) |arg_mono| {
                try self.mono_scratches.idxs.append(try self.importMonotypeFromStoreRec(
                    source_store,
                    arg_mono,
                    from_module_idx,
                    to_module_idx,
                    imported,
                ));
            }

            break :blk .{ .func = .{
                .args = try self.store.monotype_store.addIdxSpan(
                    self.allocator,
                    self.mono_scratches.idxs.sliceFromStart(idx_top),
                ),
                .ret = try self.importMonotypeFromStoreRec(
                    source_store,
                    func_mono.ret,
                    from_module_idx,
                    to_module_idx,
                    imported,
                ),
                .effectful = func_mono.effectful,
            } };
        },
        .record => |record_mono| blk: {
            const fields_top = self.mono_scratches.fields.top();
            defer self.mono_scratches.fields.clearFrom(fields_top);

            for (source_store.getFields(record_mono.fields)) |field| {
                try self.mono_scratches.fields.append(.{
                    .name = field.name,
                    .type_idx = try self.importMonotypeFromStoreRec(
                        source_store,
                        field.type_idx,
                        from_module_idx,
                        to_module_idx,
                        imported,
                    ),
                });
            }

            break :blk .{ .record = .{
                .fields = try self.store.monotype_store.addFields(
                    self.allocator,
                    self.mono_scratches.fields.sliceFromStart(fields_top),
                ),
            } };
        },
        .tag_union => |tag_union_mono| blk: {
            const tags_top = self.mono_scratches.tags.top();
            defer self.mono_scratches.tags.clearFrom(tags_top);

            for (source_store.getTags(tag_union_mono.tags)) |tag| {
                const payload_top = self.mono_scratches.idxs.top();
                defer self.mono_scratches.idxs.clearFrom(payload_top);

                for (source_store.getIdxSpan(tag.payloads)) |payload_mono| {
                    try self.mono_scratches.idxs.append(try self.importMonotypeFromStoreRec(
                        source_store,
                        payload_mono,
                        from_module_idx,
                        to_module_idx,
                        imported,
                    ));
                }

                try self.mono_scratches.tags.append(.{
                    .name = tag.name,
                    .payloads = try self.store.monotype_store.addIdxSpan(
                        self.allocator,
                        self.mono_scratches.idxs.sliceFromStart(payload_top),
                    ),
                });
            }

            break :blk .{ .tag_union = .{
                .tags = try self.store.monotype_store.addTags(
                    self.allocator,
                    self.mono_scratches.tags.sliceFromStart(tags_top),
                ),
            } };
        },
        .unit, .prim, .recursive_placeholder => unreachable,
    };

    self.store.monotype_store.monotypes.items[@intFromEnum(placeholder)] = mapped_mono;
    return placeholder;
}

fn monotypeIdxSpanIsValid(self: *const Self, span: Monotype.Span) bool {
    const start: usize = @intCast(span.start);
    return start <= self.store.monotype_store.extra_idx.items.len and
        start + span.len <= self.store.monotype_store.extra_idx.items.len;
}

fn monotypeTagSpanIsValid(self: *const Self, span: Monotype.TagSpan) bool {
    const start: usize = @intCast(span.start);
    return start <= self.store.monotype_store.tags.items.len and
        start + span.len <= self.store.monotype_store.tags.items.len;
}

fn monotypeFieldSpanIsValid(self: *const Self, span: Monotype.FieldSpan) bool {
    const start: usize = @intCast(span.start);
    return start <= self.store.monotype_store.fields.items.len and
        start + span.len <= self.store.monotype_store.fields.items.len;
}

fn monotypeIdxIsValid(self: *const Self, monotype: Monotype.Idx) bool {
    return !monotype.isNone() and @intFromEnum(monotype) < self.store.monotype_store.monotypes.items.len;
}

fn monotypeIsUnit(self: *const Self, monotype: Monotype.Idx) bool {
    return self.monotypeIdxIsValid(monotype) and self.store.monotype_store.getMonotype(monotype) == .unit;
}

fn monotypeIsWellFormed(self: *const Self, monotype: Monotype.Idx) bool {
    var seen = std.AutoHashMap(Monotype.Idx, void).init(self.allocator);
    defer seen.deinit();
    return self.monotypeIsWellFormedRec(monotype, &seen);
}

fn monotypeIsWellFormedRec(
    self: *const Self,
    monotype: Monotype.Idx,
    seen: *std.AutoHashMap(Monotype.Idx, void),
) bool {
    if (!self.monotypeIdxIsValid(monotype)) return false;
    if (seen.contains(monotype)) return true;
    seen.put(monotype, {}) catch return false;

    return switch (self.store.monotype_store.getMonotype(monotype)) {
        .unit, .prim => true,
        .recursive_placeholder => false,
        .list => |list_mono| self.monotypeIsWellFormedRec(list_mono.elem, seen),
        .box => |box_mono| self.monotypeIsWellFormedRec(box_mono.inner, seen),
        .tuple => |tuple_mono| blk: {
            if (!self.monotypeIdxSpanIsValid(tuple_mono.elems)) break :blk false;
            for (self.store.monotype_store.getIdxSpan(tuple_mono.elems)) |elem| {
                if (!self.monotypeIsWellFormedRec(elem, seen)) break :blk false;
            }
            break :blk true;
        },
        .func => |func_mono| blk: {
            if (!self.monotypeIdxSpanIsValid(func_mono.args)) break :blk false;
            for (self.store.monotype_store.getIdxSpan(func_mono.args)) |arg| {
                if (!self.monotypeIsWellFormedRec(arg, seen)) break :blk false;
            }
            break :blk self.monotypeIsWellFormedRec(func_mono.ret, seen);
        },
        .record => |record_mono| blk: {
            if (!self.monotypeFieldSpanIsValid(record_mono.fields)) break :blk false;
            for (self.store.monotype_store.getFields(record_mono.fields)) |field| {
                if (!self.monotypeIsWellFormedRec(field.type_idx, seen)) break :blk false;
            }
            break :blk true;
        },
        .tag_union => |tag_union_mono| blk: {
            if (!self.monotypeTagSpanIsValid(tag_union_mono.tags)) break :blk false;
            for (self.store.monotype_store.getTags(tag_union_mono.tags)) |tag| {
                if (!self.monotypeIdxSpanIsValid(tag.payloads)) break :blk false;
                for (self.store.monotype_store.getIdxSpan(tag.payloads)) |payload| {
                    if (!self.monotypeIsWellFormedRec(payload, seen)) break :blk false;
                }
            }
            break :blk true;
        },
    };
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

fn emitMirStructExpr(
    self: *Self,
    field_exprs: []const MIR.ExprId,
    monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const fields = if (field_exprs.len == 0)
        MIR.ExprSpan.empty()
    else
        try self.store.addExprSpan(self.allocator, field_exprs);

    return try self.emitMirStructExprFromSpan(fields, monotype, region);
}

fn emitMirStructExprFromSpan(
    self: *Self,
    fields: MIR.ExprSpan,
    monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    return try self.store.addExpr(
        self.allocator,
        .{ .struct_ = .{ .fields = fields } },
        monotype,
        region,
    );
}

fn emitMirStructAccess(
    self: *Self,
    struct_expr: MIR.ExprId,
    field_idx: u32,
    field_monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    return try self.store.addExpr(
        self.allocator,
        .{ .struct_access = .{ .struct_ = struct_expr, .field_idx = field_idx } },
        field_monotype,
        region,
    );
}

fn emitMirUnitExpr(self: *Self, region: Region) Allocator.Error!MIR.ExprId {
    return try self.emitMirStructExpr(&.{}, self.store.monotype_store.unit_idx, region);
}

fn emitMirBoolLiteral(self: *Self, module_env: *const ModuleEnv, value: bool, region: Region) Allocator.Error!MIR.ExprId {
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );
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
    try self.symbol_monotypes.put(symbol.raw(), monotype);
    return .{ .symbol = symbol, .pattern = pattern };
}

fn registerPatternSymbolMonotypes(self: *Self, pattern_id: MIR.PatternId) Allocator.Error!void {
    const monotype = self.store.patternTypeOf(pattern_id);

    switch (self.store.getPattern(pattern_id)) {
        .bind => |symbol| try self.symbol_monotypes.put(symbol.raw(), monotype),
        .as_pattern => |as_pat| {
            try self.symbol_monotypes.put(as_pat.symbol.raw(), monotype);
            try self.registerPatternSymbolMonotypes(as_pat.pattern);
        },
        .tag => |tag| {
            for (self.store.getPatternSpan(tag.args)) |arg_pat| {
                try self.registerPatternSymbolMonotypes(arg_pat);
            }
        },
        .struct_destructure => |destructure| {
            for (self.store.getPatternSpan(destructure.fields)) |field_pat| {
                try self.registerPatternSymbolMonotypes(field_pat);
            }
        },
        .list_destructure => |destructure| {
            for (self.store.getPatternSpan(destructure.patterns)) |elem_pat| {
                try self.registerPatternSymbolMonotypes(elem_pat);
            }
            if (!destructure.rest_pattern.isNone()) {
                try self.registerPatternSymbolMonotypes(destructure.rest_pattern);
            }
        },
        .wildcard,
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => {},
    }
}

fn registerBoundSymbolDefIfNeeded(self: *Self, pattern: MIR.PatternId, expr: MIR.ExprId) Allocator.Error!void {
    if (self.patternBoundSymbol(pattern)) |symbol| {
        if (self.store.getValueDef(symbol) == null) {
            try self.store.registerValueDef(self.allocator, symbol, expr);
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
    const arg_type_var = ModuleEnv.varFrom(arg_cir_expr);

    // Evaluate the argument once, then inspect the bound lookup.
    const arg_bind = try self.makeSyntheticBind(arg_mono, false);
    const arg_lookup = try self.emitMirLookup(arg_bind.symbol, arg_mono, region);
    const inspected = try self.lowerStrInspektExpr(module_env, arg_lookup, arg_type_var, region);
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
    type_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    type_var: types.Var,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const resolved = type_env.types.resolveVar(type_var);
    return switch (resolved.desc.content) {
        .alias => |alias| self.lowerStrInspektExpr(type_env, value_expr, type_env.types.getAliasBackingVar(alias), region),
        .structure => |flat_type| switch (flat_type) {
            .nominal_type => |nominal| self.lowerStrInspektNominal(type_env, value_expr, type_var, nominal, region),
            .record => |record| self.lowerStrInspektRecord(type_env, value_expr, record, region),
            .record_unbound => |fields_range| self.lowerStrInspektRecordUnbound(type_env, value_expr, fields_range, region),
            .tuple => |tup| self.lowerStrInspektTuple(type_env, value_expr, tup, region),
            .tag_union => |tu| self.lowerStrInspektTagUnion(type_env, value_expr, type_var, tu, region),
            .empty_record => self.emitMirStrLiteral("{}", region),
            .empty_tag_union => self.emitMirStrLiteral("<empty_tag_union>", region),
            .fn_pure, .fn_effectful, .fn_unbound => self.emitMirStrLiteral("<function>", region),
        },
        .flex, .rigid => {
            const mono_idx = try self.monotypeFromTypeVarInEnv(type_env, type_var);
            return self.lowerStrInspektExprByMonotype(type_env, value_expr, mono_idx, region);
        },
        .err => try self.store.addExpr(
            self.allocator,
            .{ .runtime_err_type = {} },
            self.store.monotype_store.primIdx(.str),
            region,
        ),
    };
}

fn monotypeFromTypeVarInEnv(
    self: *Self,
    type_env: *const ModuleEnv,
    type_var: types.Var,
) Allocator.Error!Monotype.Idx {
    const module_idx = self.moduleIndexForEnv(type_env) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic("monotypeFromTypeVarInEnv: module env not found", .{});
        }
        unreachable;
    };

    return self.monotypeFromTypeVarInStore(module_idx, &type_env.types, type_var);
}

fn monotypeFromTypeVarInStoreWithBindings(
    self: *Self,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
) Allocator.Error!Monotype.Idx {
    var local_cycles = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    defer local_cycles.deinit();

    return self.monotypeFromTypeVarWithBindings(
        module_idx,
        store_types,
        var_,
        bindings,
        &local_cycles,
    );
}

fn resolveFuncTypeInStore(types_store: *const types.Store, type_var: types.Var) ?struct { func: types.Func, effectful: bool } {
    var resolved = types_store.resolveVar(type_var);
    while (resolved.desc.content == .alias) {
        resolved = types_store.resolveVar(types_store.getAliasBackingVar(resolved.desc.content.alias));
    }

    if (resolved.desc.content != .structure) return null;
    return switch (resolved.desc.content.structure) {
        .fn_pure => |func| .{ .func = func, .effectful = false },
        .fn_effectful => |func| .{ .func = func, .effectful = true },
        .fn_unbound => |func| .{ .func = func, .effectful = false },
        else => null,
    };
}

fn lookupAssociatedMethodExternalDef(
    self: *Self,
    source_env: *const ModuleEnv,
    nominal: types.NominalType,
    method_ident: Ident.Idx,
) Allocator.Error!?struct {
    target_env: *const ModuleEnv,
    module_idx: u32,
    def_node_idx: u16,
    type_var: types.Var,
} {
    const target_module_idx = self.findModuleForOriginMaybe(source_env, nominal.origin_module) orelse return null;
    const target_env = self.all_module_envs[target_module_idx];
    const qualified_method_ident = target_env.lookupMethodIdentFromEnvConst(
        source_env,
        nominal.ident.ident_idx,
        method_ident,
    ) orelse return null;
    const node_idx = target_env.getExposedNodeIndexById(qualified_method_ident) orelse return null;
    if (!target_env.store.isDefNode(node_idx)) return null;

    const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
    return .{
        .target_env = target_env,
        .module_idx = target_module_idx,
        .def_node_idx = node_idx,
        .type_var = ModuleEnv.varFrom(target_env.store.getDef(def_idx).expr),
    };
}

fn bindTypeVarMonotypesInStore(
    self: *Self,
    store_types: *const types.Store,
    common_idents: ModuleEnv.CommonIdents,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    type_var: types.Var,
    monotype: Monotype.Idx,
) Allocator.Error!void {
    if (monotype.isNone()) return;

    const resolved = store_types.resolveVar(type_var);
    if (bindings.get(resolved.var_)) |existing| {
        if (!(try self.monotypesStructurallyEqual(existing, monotype))) {
            typeBindingInvariant(
                "bindTypeVarMonotypesInStore: conflicting monotype binding for type var root {d} (existing={d}, new={d})",
                .{ @intFromEnum(resolved.var_), @intFromEnum(existing), @intFromEnum(monotype) },
            );
        }
        return;
    }

    switch (resolved.desc.content) {
        .flex, .rigid => try bindings.put(resolved.var_, monotype),
        .alias => |alias| try self.bindTypeVarMonotypesInStore(
            store_types,
            common_idents,
            bindings,
            store_types.getAliasBackingVar(alias),
            monotype,
        ),
        .structure => |flat_type| {
            try bindings.put(resolved.var_, monotype);
            try self.bindFlatTypeMonotypesInStore(store_types, common_idents, bindings, flat_type, monotype);
        },
        .err => {},
    }
}

fn seedTypeScopeBindingsInStore(
    self: *Self,
    module_idx: u32,
    store_types: *const types.Store,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
) Allocator.Error!void {
    const type_scope = self.type_scope orelse return;
    const type_scope_module_idx = self.type_scope_module_idx orelse return;
    const caller_module_idx = self.type_scope_caller_module_idx orelse return;

    if (module_idx != type_scope_module_idx) return;

    const common_idents = ModuleEnv.CommonIdents.find(&self.all_module_envs[module_idx].common);
    const caller_types = &self.all_module_envs[caller_module_idx].types;

    for (type_scope.scopes.items) |*scope| {
        var it = scope.iterator();
        while (it.next()) |entry| {
            const platform_var = entry.key_ptr.*;
            const caller_var = entry.value_ptr.*;
            const caller_mono = try self.monotypeFromTypeVarInStore(
                caller_module_idx,
                caller_types,
                caller_var,
            );
            if (caller_mono.isNone()) continue;
            const normalized_mono = if (caller_module_idx == module_idx)
                caller_mono
            else
                try self.remapMonotypeBetweenModules(caller_mono, caller_module_idx, module_idx);
            try self.bindTypeVarMonotypesInStore(
                store_types,
                common_idents,
                bindings,
                platform_var,
                normalized_mono,
            );
        }
    }
}

fn bindNamedTypeScopeMatchInStore(
    self: *Self,
    module_idx: u32,
    store_types: *const types.Store,
    resolved: types.store.ResolvedVarDesc,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
) Allocator.Error!void {
    const type_scope = self.type_scope orelse return;
    const type_scope_module_idx = self.type_scope_module_idx orelse return;
    const caller_module_idx = self.type_scope_caller_module_idx orelse return;

    if (module_idx != type_scope_module_idx) return;
    if (bindings.contains(resolved.var_)) return;

    const current_name = switch (resolved.desc.content) {
        .rigid => |rigid| rigid.name,
        .flex => |flex| flex.name orelse return,
        else => return,
    };

    const common_idents = ModuleEnv.CommonIdents.find(&self.all_module_envs[module_idx].common);
    const caller_types = &self.all_module_envs[caller_module_idx].types;

    for (type_scope.scopes.items) |*scope| {
        var it = scope.iterator();
        while (it.next()) |entry| {
            const platform_resolved = store_types.resolveVar(entry.key_ptr.*);
            const platform_name = switch (platform_resolved.desc.content) {
                .rigid => |rigid| rigid.name,
                .flex => |flex| flex.name orelse continue,
                else => continue,
            };
            if (!platform_name.eql(current_name)) continue;

            const caller_mono = try self.monotypeFromTypeVarInStore(
                caller_module_idx,
                caller_types,
                entry.value_ptr.*,
            );
            if (caller_mono.isNone()) continue;
            const normalized_mono = if (caller_module_idx == module_idx)
                caller_mono
            else
                try self.remapMonotypeBetweenModules(caller_mono, caller_module_idx, module_idx);

            try self.bindTypeVarMonotypesInStore(
                store_types,
                common_idents,
                bindings,
                resolved.var_,
                normalized_mono,
            );
            return;
        }
    }
}

fn monotypeFromTypeVarWithBindings(
    self: *Self,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    cycles: *std.AutoHashMap(types.Var, Monotype.Idx),
) Allocator.Error!Monotype.Idx {
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_idx_for_mono = self.mono_scratches.module_idx;
    self.mono_scratches.ident_store = self.all_module_envs[module_idx].getIdentStoreConst();
    self.mono_scratches.module_idx = module_idx;
    defer self.mono_scratches.ident_store = saved_ident_store;
    defer self.mono_scratches.module_idx = saved_module_idx_for_mono;

    try self.seedTypeScopeBindingsInStore(module_idx, store_types, bindings);
    try self.bindNamedTypeScopeMatchInStore(module_idx, store_types, store_types.resolveVar(var_), bindings);

    return self.store.monotype_store.fromTypeVar(
        self.allocator,
        store_types,
        var_,
        ModuleEnv.CommonIdents.find(&self.all_module_envs[module_idx].common),
        bindings,
        cycles,
        &self.mono_scratches,
    );
}

fn bindFlatTypeMonotypesInStore(
    self: *Self,
    store_types: *const types.Store,
    common_idents: ModuleEnv.CommonIdents,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    flat_type: types.FlatType,
    monotype: Monotype.Idx,
) Allocator.Error!void {
    if (monotype.isNone()) return;

    const mono = self.store.monotype_store.getMonotype(monotype);
    switch (flat_type) {
        .fn_pure, .fn_effectful, .fn_unbound => |func| {
            const mfunc = switch (mono) {
                .func => |mfunc| mfunc,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypesInStore(fn): expected function monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };

            const type_args = store_types.sliceVars(func.args);
            const mono_args = self.store.monotype_store.getIdxSpan(mfunc.args);
            if (type_args.len != mono_args.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypesInStore(fn): arity mismatch (type={d}, monotype={d})",
                    .{ type_args.len, mono_args.len },
                );
            }
            for (type_args, 0..) |arg_var, i| {
                try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, arg_var, mono_args[i]);
            }
            try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, func.ret, mfunc.ret);
        },
        .nominal_type => |nominal| {
            const ident = nominal.ident.ident_idx;
            const origin = nominal.origin_module;

            if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.list)) {
                const mlist = switch (mono) {
                    .list => |mlist| mlist,
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypesInStore(nominal List): expected list monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                const type_args = store_types.sliceNominalArgs(nominal);
                if (type_args.len != 1) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypesInStore(nominal List): expected 1 arg, found {d}",
                        .{type_args.len},
                    );
                }
                try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, type_args[0], mlist.elem);
                return;
            }

            if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.box)) {
                const mbox = switch (mono) {
                    .box => |mbox| mbox,
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypesInStore(nominal Box): expected box monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                const type_args = store_types.sliceNominalArgs(nominal);
                if (type_args.len != 1) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypesInStore(nominal Box): expected 1 arg, found {d}",
                        .{type_args.len},
                    );
                }
                try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, type_args[0], mbox.inner);
                return;
            }

            if (origin.eql(common_idents.builtin_module) and builtinPrimForNominal(ident, common_idents) != null) {
                switch (mono) {
                    .prim => {},
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypesInStore(nominal prim): expected prim monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                }
                return;
            }

            try self.bindTypeVarMonotypesInStore(
                store_types,
                common_idents,
                bindings,
                store_types.getNominalBackingVar(nominal),
                monotype,
            );
        },
        .record => |record| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypesInStore(record): expected record monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const mono_fields = self.store.monotype_store.getFields(mrec.fields);
            var seen_field_indices: std.ArrayListUnmanaged(u32) = .empty;
            defer seen_field_indices.deinit(self.allocator);

            var current_row = record;
            rows: while (true) {
                const fields_slice = store_types.getRecordFieldsSlice(current_row.fields);
                const field_names = fields_slice.items(.name);
                const field_vars = fields_slice.items(.var_);
                for (field_names, field_vars) |field_name, field_var| {
                    const field_idx = self.recordFieldIndexByName(field_name, mono_fields);
                    try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                    try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, field_var, mono_fields[field_idx].type_idx);
                }

                var ext_var = current_row.ext;
                while (true) {
                    const ext_resolved = store_types.resolveVar(ext_var);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            ext_var = store_types.getAliasBackingVar(alias);
                            continue;
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue :rows;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = store_types.getRecordFieldsSlice(fields_range);
                                const ext_names = ext_fields.items(.name);
                                const ext_vars = ext_fields.items(.var_);
                                for (ext_names, ext_vars) |field_name, field_var| {
                                    const field_idx = self.recordFieldIndexByName(field_name, mono_fields);
                                    try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                                    try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, field_var, mono_fields[field_idx].type_idx);
                                }
                                break :rows;
                            },
                            .empty_record => break :rows,
                            else => typeBindingInvariant(
                                "bindFlatTypeMonotypesInStore(record): unexpected row extension '{s}'",
                                .{@tagName(ext_flat)},
                            ),
                        },
                        .flex, .rigid => {
                            try self.bindRecordRowTailInStore(store_types, common_idents, bindings, ext_var, mono_fields, seen_field_indices.items);
                            break :rows;
                        },
                        .err => unreachable,
                    }
                }
            }
        },
        .record_unbound => |fields_range| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypesInStore(record_unbound): expected record monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const mono_fields = self.store.monotype_store.getFields(mrec.fields);
            const fields_slice = store_types.getRecordFieldsSlice(fields_range);
            const field_names = fields_slice.items(.name);
            const field_vars = fields_slice.items(.var_);
            for (field_names, field_vars) |field_name, field_var| {
                const field_idx = self.recordFieldIndexByName(field_name, mono_fields);
                try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, field_var, mono_fields[field_idx].type_idx);
            }
        },
        .tuple => |tuple| {
            const mtup = switch (mono) {
                .tuple => |mtup| mtup,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypesInStore(tuple): expected tuple monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const elem_vars = store_types.sliceVars(tuple.elems);
            const elem_monos = self.store.monotype_store.getIdxSpan(mtup.elems);
            if (elem_vars.len != elem_monos.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypesInStore(tuple): arity mismatch (type={d}, monotype={d})",
                    .{ elem_vars.len, elem_monos.len },
                );
            }
            for (elem_vars, 0..) |elem_var, i| {
                try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, elem_var, elem_monos[i]);
            }
        },
        .tag_union => |tag_union| {
            const mtag = switch (mono) {
                .tag_union => |mtag| mtag,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypesInStore(tag_union): expected tag union monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const mono_tags = self.store.monotype_store.getTags(mtag.tags);
            var seen_tag_indices: std.ArrayListUnmanaged(u32) = .empty;
            defer seen_tag_indices.deinit(self.allocator);

            var current_row = tag_union;
            rows: while (true) {
                const type_tags = store_types.getTagsSlice(current_row.tags);
                const tag_names = type_tags.items(.name);
                const tag_args = type_tags.items(.args);
                for (tag_names, tag_args) |tag_name, args_range| {
                    const tag_idx = self.tagIndexByName(tag_name, mono_tags);
                    try appendSeenIndex(self.allocator, &seen_tag_indices, tag_idx);
                    const payload_vars = store_types.sliceVars(args_range);
                    try self.bindTagPayloadsByName(tag_name, payload_vars, mono_tags);
                }

                var ext_var = current_row.ext;
                while (true) {
                    const ext_resolved = store_types.resolveVar(ext_var);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            ext_var = store_types.getAliasBackingVar(alias);
                            continue;
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .tag_union => |next_row| {
                                current_row = next_row;
                                continue :rows;
                            },
                            .empty_tag_union => break :rows,
                            else => typeBindingInvariant(
                                "bindFlatTypeMonotypesInStore(tag_union): unexpected row extension '{s}'",
                                .{@tagName(ext_flat)},
                            ),
                        },
                        .flex, .rigid => {
                            try self.bindTagUnionRowTailInStore(store_types, common_idents, bindings, ext_var, mono_tags, seen_tag_indices.items);
                            break :rows;
                        },
                        .err => unreachable,
                    }
                }
            }
        },
        .empty_record => switch (mono) {
            .unit => {},
            else => typeBindingInvariant(
                "bindFlatTypeMonotypesInStore(empty_record): expected unit monotype, found '{s}'",
                .{@tagName(mono)},
            ),
        },
        .empty_tag_union => switch (mono) {
            .tag_union => {},
            else => typeBindingInvariant(
                "bindFlatTypeMonotypesInStore(empty_tag_union): expected tag union monotype, found '{s}'",
                .{@tagName(mono)},
            ),
        },
    }
}

fn lowerStrInspektNominal(
    self: *Self,
    type_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    type_var: types.Var,
    nominal: types.NominalType,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const common = ModuleEnv.CommonIdents.find(&type_env.common);
    const ident = nominal.ident.ident_idx;

    if (nominal.origin_module.eql(common.builtin_module)) {
        if (builtinPrimForNominal(ident, common)) |prim| {
            return self.lowerStrInspektExprByMonotype(
                type_env,
                value_expr,
                self.store.monotype_store.primIdx(prim),
                region,
            );
        }
        if (ident.eql(common.bool)) {
            return self.lowerStrInspektExpr(type_env, value_expr, type_env.types.getNominalBackingVar(nominal), region);
        }
        if (ident.eql(common.list)) {
            const type_args = type_env.types.sliceNominalArgs(nominal);
            if (type_args.len != 1) {
                typeBindingInvariant("lowerStrInspektNominal(List): expected 1 arg, found {d}", .{type_args.len});
            }
            return self.lowerStrInspektList(type_env, value_expr, type_args[0], region);
        }
        if (ident.eql(common.box)) {
            const type_args = type_env.types.sliceNominalArgs(nominal);
            if (type_args.len != 1) {
                typeBindingInvariant("lowerStrInspektNominal(Box): expected 1 arg, found {d}", .{type_args.len});
            }

            const outer_mono = try self.monotypeFromTypeVarInEnv(type_env, type_var);
            const box_mono = self.store.monotype_store.getMonotype(outer_mono);
            const box_data = switch (box_mono) {
                .box => |box| box,
                else => typeBindingInvariant(
                    "lowerStrInspektNominal(Box): expected box monotype, found '{s}'",
                    .{@tagName(box_mono)},
                ),
            };

            const unbox_args = try self.store.addExprSpan(self.allocator, &.{value_expr});
            const unboxed = try self.store.addExpr(
                self.allocator,
                .{ .run_low_level = .{ .op = .box_unbox, .args = unbox_args } },
                box_data.inner,
                region,
            );

            const inner_str = try self.lowerStrInspektExpr(type_env, unboxed, type_args[0], region);
            const open = try self.emitMirStrLiteral("Box(", region);
            const close = try self.emitMirStrLiteral(")", region);
            return self.foldMirStrConcat(&.{ open, inner_str, close }, region);
        }
    }

    if (try self.lookupAssociatedMethodExternalDef(type_env, nominal, type_env.idents.to_inspect)) |method_info| {
        const resolved_func = resolveFuncTypeInStore(&method_info.target_env.types, method_info.type_var) orelse
            return self.lowerStrInspektExpr(type_env, value_expr, type_env.types.getNominalBackingVar(nominal), region);
        if (resolved_func.effectful) {
            return self.lowerStrInspektExpr(type_env, value_expr, type_env.types.getNominalBackingVar(nominal), region);
        }

        const param_vars = method_info.target_env.types.sliceVars(resolved_func.func.args);
        if (param_vars.len != 1) {
            return self.lowerStrInspektExpr(type_env, value_expr, type_env.types.getNominalBackingVar(nominal), region);
        }

        var type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer type_var_seen.deinit();

        const arg_mono = try self.monotypeFromTypeVarInEnv(type_env, type_var);
        try self.bindTypeVarMonotypesInStore(
            &method_info.target_env.types,
            ModuleEnv.CommonIdents.find(&method_info.target_env.common),
            &type_var_seen,
            param_vars[0],
            arg_mono,
        );

        const method_func_mono = try self.monotypeFromTypeVarInStoreWithBindings(
            method_info.module_idx,
            &method_info.target_env.types,
            method_info.type_var,
            &type_var_seen,
        );
        if (method_func_mono.isNone()) {
            return self.lowerStrInspektExpr(type_env, value_expr, type_env.types.getNominalBackingVar(nominal), region);
        }

        const method_func = switch (self.store.monotype_store.getMonotype(method_func_mono)) {
            .func => |func| func,
            else => typeBindingInvariant(
                "lowerStrInspektNominal: expected function monotype for to_inspect, found '{s}'",
                .{@tagName(self.store.monotype_store.getMonotype(method_func_mono))},
            ),
        };

        const func_expr = try self.lowerMonomorphizedExternalProcInst(
            method_info.module_idx,
            method_info.def_node_idx,
            method_func_mono,
            method_info.module_idx,
        );
        const call_args = try self.store.addExprSpan(self.allocator, &.{value_expr});
        const call_expr = try self.store.addExpr(
            self.allocator,
            .{ .call = .{ .func = func_expr, .args = call_args } },
            method_func.ret,
            region,
        );

        const ret_mono = self.store.monotype_store.getMonotype(method_func.ret);
        if (ret_mono == .prim and ret_mono.prim == .str) {
            return call_expr;
        }

        return self.lowerStrInspektExpr(method_info.target_env, call_expr, resolved_func.func.ret, region);
    }

    return self.lowerStrInspektExpr(type_env, value_expr, type_env.types.getNominalBackingVar(nominal), region);
}

fn lowerStrInspektRecord(
    self: *Self,
    type_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    record: types.Record,
    region: Region,
) Allocator.Error!MIR.ExprId {
    var collected = std.ArrayList(types.RecordField).empty;
    defer collected.deinit(self.allocator);

    var current_row = record;
    rows: while (true) {
        const fields_slice = type_env.types.getRecordFieldsSlice(current_row.fields);
        const field_names = fields_slice.items(.name);
        const field_vars = fields_slice.items(.var_);

        for (field_names, field_vars) |field_name, field_var| {
            var seen_name = false;
            for (collected.items) |existing| {
                if (existing.name.eql(field_name)) {
                    seen_name = true;
                    break;
                }
            }
            if (!seen_name) {
                try collected.append(self.allocator, .{ .name = field_name, .var_ = field_var });
            }
        }

        var ext_var = current_row.ext;
        while (true) {
            const ext_resolved = type_env.types.resolveVar(ext_var);
            switch (ext_resolved.desc.content) {
                .alias => |alias| {
                    ext_var = type_env.types.getAliasBackingVar(alias);
                    continue;
                },
                .structure => |ext_flat| switch (ext_flat) {
                    .record => |next_row| {
                        current_row = next_row;
                        continue :rows;
                    },
                    .record_unbound => |fields_range| {
                        const ext_fields = type_env.types.getRecordFieldsSlice(fields_range);
                        const ext_names = ext_fields.items(.name);
                        const ext_vars = ext_fields.items(.var_);
                        for (ext_names, ext_vars) |field_name, field_var| {
                            var seen_name = false;
                            for (collected.items) |existing| {
                                if (existing.name.eql(field_name)) {
                                    seen_name = true;
                                    break;
                                }
                            }
                            if (!seen_name) {
                                try collected.append(self.allocator, .{ .name = field_name, .var_ = field_var });
                            }
                        }
                        break :rows;
                    },
                    .empty_record => break :rows,
                    else => typeBindingInvariant(
                        "lowerStrInspektRecord: unexpected row extension '{s}'",
                        .{@tagName(ext_flat)},
                    ),
                },
                .flex, .rigid => break :rows,
                .err => unreachable,
            }
        }
    }

    if (collected.items.len == 0) return self.emitMirStrLiteral("{}", region);

    std.mem.sort(types.RecordField, collected.items, type_env.getIdentStoreConst(), types.RecordField.sortByNameAsc);

    const save_exprs = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(save_exprs);

    try self.scratch_expr_ids.append(try self.emitMirStrLiteral("{ ", region));
    for (collected.items, 0..) |field, i| {
        if (i > 0) {
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
        }
        const field_name = type_env.getIdent(field.name);
        const label = try std.fmt.allocPrint(self.allocator, "{s}: ", .{field_name});
        defer self.allocator.free(label);
        try self.scratch_expr_ids.append(try self.emitMirStrLiteral(label, region));

        const field_mono = try self.monotypeFromTypeVarInEnv(type_env, field.var_);
        const field_expr = try self.emitMirStructAccess(value_expr, @intCast(i), field_mono, region);
        try self.scratch_expr_ids.append(try self.lowerStrInspektExpr(type_env, field_expr, field.var_, region));
    }
    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(" }", region));
    return self.foldMirStrConcat(self.scratch_expr_ids.sliceFromStart(save_exprs), region);
}

fn lowerStrInspektRecordUnbound(
    self: *Self,
    type_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    fields_range: types.RecordField.SafeMultiList.Range,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const fields_slice = type_env.types.getRecordFieldsSlice(fields_range);
    var collected = std.ArrayList(types.RecordField).empty;
    defer collected.deinit(self.allocator);

    const field_names = fields_slice.items(.name);
    const field_vars = fields_slice.items(.var_);
    try collected.ensureTotalCapacity(self.allocator, field_names.len);
    for (field_names, field_vars) |field_name, field_var| {
        collected.appendAssumeCapacity(.{ .name = field_name, .var_ = field_var });
    }

    if (collected.items.len == 0) return self.emitMirStrLiteral("{}", region);

    std.mem.sort(types.RecordField, collected.items, type_env.getIdentStoreConst(), types.RecordField.sortByNameAsc);

    const save_exprs = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(save_exprs);

    try self.scratch_expr_ids.append(try self.emitMirStrLiteral("{ ", region));
    for (collected.items, 0..) |field, i| {
        if (i > 0) {
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
        }
        const field_name = type_env.getIdent(field.name);
        const label = try std.fmt.allocPrint(self.allocator, "{s}: ", .{field_name});
        defer self.allocator.free(label);
        try self.scratch_expr_ids.append(try self.emitMirStrLiteral(label, region));

        const field_mono = try self.monotypeFromTypeVarInEnv(type_env, field.var_);
        const field_expr = try self.emitMirStructAccess(value_expr, @intCast(i), field_mono, region);
        try self.scratch_expr_ids.append(try self.lowerStrInspektExpr(type_env, field_expr, field.var_, region));
    }
    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(" }", region));
    return self.foldMirStrConcat(self.scratch_expr_ids.sliceFromStart(save_exprs), region);
}

fn lowerStrInspektTuple(
    self: *Self,
    type_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    tup: types.Tuple,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const elems = type_env.types.sliceVars(tup.elems);
    if (elems.len == 0) return self.emitMirStrLiteral("()", region);

    const save_exprs = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(save_exprs);

    try self.scratch_expr_ids.append(try self.emitMirStrLiteral("(", region));
    for (elems, 0..) |elem_var, i| {
        if (i > 0) {
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
        }
        const elem_mono = try self.monotypeFromTypeVarInEnv(type_env, elem_var);
        const elem_expr = try self.emitMirStructAccess(value_expr, @intCast(i), elem_mono, region);
        try self.scratch_expr_ids.append(try self.lowerStrInspektExpr(type_env, elem_expr, elem_var, region));
    }
    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(")", region));

    return self.foldMirStrConcat(self.scratch_expr_ids.sliceFromStart(save_exprs), region);
}

fn lowerStrInspektTagUnion(
    self: *Self,
    type_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    type_var: types.Var,
    tu: types.TagUnion,
    region: Region,
) Allocator.Error!MIR.ExprId {
    var collected = std.ArrayList(types.Tag).empty;
    defer collected.deinit(self.allocator);

    var current_row = tu;
    rows: while (true) {
        const tags_slice = type_env.types.getTagsSlice(current_row.tags);
        const tag_names = tags_slice.items(.name);
        const tag_args = tags_slice.items(.args);

        try collected.ensureTotalCapacity(self.allocator, collected.items.len + tag_names.len);
        for (tag_names, tag_args) |tag_name, args| {
            collected.appendAssumeCapacity(.{ .name = tag_name, .args = args });
        }

        var ext_var = current_row.ext;
        while (true) {
            const ext_resolved = type_env.types.resolveVar(ext_var);
            switch (ext_resolved.desc.content) {
                .alias => |alias| {
                    ext_var = type_env.types.getAliasBackingVar(alias);
                    continue;
                },
                .structure => |ext_flat| switch (ext_flat) {
                    .tag_union => |next_row| {
                        current_row = next_row;
                        continue :rows;
                    },
                    .empty_tag_union => break :rows,
                    else => typeBindingInvariant(
                        "lowerStrInspektTagUnion: unexpected row extension '{s}'",
                        .{@tagName(ext_flat)},
                    ),
                },
                .flex, .rigid => break :rows,
                .err => unreachable,
            }
        }
    }

    if (collected.items.len == 0) return self.emitMirStrLiteral("<empty_tag_union>", region);

    std.mem.sort(types.Tag, collected.items, type_env.getIdentStoreConst(), types.Tag.sortByNameAsc);

    const union_mono = try self.monotypeFromTypeVarInEnv(type_env, type_var);
    const save_branches = self.scratch_branches.top();
    defer self.scratch_branches.clearFrom(save_branches);

    for (collected.items) |tag| {
        const payload_vars = type_env.types.sliceVars(tag.args);

        const save_payload_patterns = self.scratch_pattern_ids.top();
        defer self.scratch_pattern_ids.clearFrom(save_payload_patterns);
        const save_payload_symbols = self.scratch_captures.top();
        defer self.scratch_captures.clearFrom(save_payload_symbols);

        for (payload_vars) |payload_var| {
            const payload_mono = try self.monotypeFromTypeVarInEnv(type_env, payload_var);
            const bind = try self.makeSyntheticBind(payload_mono, false);
            try self.scratch_pattern_ids.append(bind.pattern);
            try self.scratch_captures.append(.{ .symbol = bind.symbol });
        }

        const payload_pattern_span = try self.store.addPatternSpan(
            self.allocator,
            self.scratch_pattern_ids.sliceFromStart(save_payload_patterns),
        );
        const tag_args = try self.wrapMultiPayloadTagPatterns(tag.name, union_mono, payload_pattern_span);
        const tag_pattern = try self.store.addPattern(
            self.allocator,
            .{ .tag = .{ .name = tag.name, .args = tag_args } },
            union_mono,
        );
        const branch_patterns = try self.store.addBranchPatterns(self.allocator, &.{MIR.BranchPattern{
            .pattern = tag_pattern,
            .degenerate = false,
        }});

        const body = if (payload_vars.len == 0) blk: {
            break :blk try self.emitMirStrLiteral(type_env.getIdent(tag.name), region);
        } else blk: {
            const save_parts = self.scratch_expr_ids.top();
            defer self.scratch_expr_ids.clearFrom(save_parts);

            const tag_open = try std.fmt.allocPrint(self.allocator, "{s}(", .{type_env.getIdent(tag.name)});
            defer self.allocator.free(tag_open);
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(tag_open, region));

            const payload_symbols = self.scratch_captures.sliceFromStart(save_payload_symbols);
            for (payload_vars, 0..) |payload_var, i| {
                if (i > 0) {
                    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
                }
                const payload_mono = try self.monotypeFromTypeVarInEnv(type_env, payload_var);
                const payload_lookup = try self.emitMirLookup(payload_symbols[i].symbol, payload_mono, region);
                try self.scratch_expr_ids.append(try self.lowerStrInspektExpr(type_env, payload_lookup, payload_var, region));
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
    type_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    elem_var: types.Var,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const str_mono = self.store.monotype_store.primIdx(.str);
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );
    const unit_mono = self.store.monotype_store.unit_idx;
    const elem_mono = try self.monotypeFromTypeVarInEnv(type_env, elem_var);
    const current_env = self.all_module_envs[self.current_module_idx];

    const acc_bind = try self.makeSyntheticBind(str_mono, true);
    const first_bind = try self.makeSyntheticBind(bool_mono, true);
    const elem_bind = try self.makeSyntheticBind(elem_mono, false);

    const open_bracket = try self.emitMirStrLiteral("[", region);
    const close_bracket = try self.emitMirStrLiteral("]", region);
    const comma = try self.emitMirStrLiteral(", ", region);
    const empty = try self.emitMirStrLiteral("", region);
    const first_true = try self.emitMirBoolLiteral(current_env, true, region);
    const first_false = try self.emitMirBoolLiteral(current_env, false, region);

    const first_lookup = try self.emitMirLookup(first_bind.symbol, bool_mono, region);
    const prefix = try self.createBoolMatch(current_env, first_lookup, empty, comma, str_mono, region);
    const elem_lookup = try self.emitMirLookup(elem_bind.symbol, elem_mono, region);
    const elem_inspected = try self.lowerStrInspektExpr(type_env, elem_lookup, elem_var, region);
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

fn lowerStrInspektExprByMonotype(
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
        .record => |record| self.lowerStrInspektRecordByMonotype(module_env, value_expr, record, region),
        .tuple => |tup| self.lowerStrInspektTupleByMonotype(module_env, value_expr, tup, region),
        .tag_union => |tu| self.lowerStrInspektTagUnionByMonotype(module_env, value_expr, tu, mono_idx, region),
        .list => |list_data| self.lowerStrInspektListByMonotype(module_env, value_expr, list_data, region),
        .unit => self.emitMirStrLiteral("{}", region),
        .box => |box_data| blk: {
            const unbox_args = try self.store.addExprSpan(self.allocator, &.{value_expr});
            const unboxed = try self.store.addExpr(
                self.allocator,
                .{ .run_low_level = .{ .op = .box_unbox, .args = unbox_args } },
                box_data.inner,
                region,
            );

            const inner_str = try self.lowerStrInspektExprByMonotype(module_env, unboxed, box_data.inner, region);
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

fn lowerStrInspektRecordByMonotype(
    self: *Self,
    module_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    record: anytype,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const field_span = record.fields;
    const fields = self.store.monotype_store.getFields(field_span);
    if (fields.len == 0) return self.emitMirStrLiteral("{}", region);

    const save_exprs = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(save_exprs);

    try self.scratch_expr_ids.append(try self.emitMirStrLiteral("{ ", region));
    for (0..field_span.len) |i| {
        const current_fields = self.store.monotype_store.getFields(field_span);
        const field = current_fields[i];
        if (i > 0) {
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
        }
        const field_name = field.name.text(self.all_module_envs);
        const label = try std.fmt.allocPrint(self.allocator, "{s}: ", .{field_name});
        defer self.allocator.free(label);
        try self.scratch_expr_ids.append(try self.emitMirStrLiteral(label, region));

        const field_expr = try self.emitMirStructAccess(value_expr, @intCast(i), field.type_idx, region);
        try self.scratch_expr_ids.append(try self.lowerStrInspektExprByMonotype(module_env, field_expr, field.type_idx, region));
    }
    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(" }", region));
    return self.foldMirStrConcat(self.scratch_expr_ids.sliceFromStart(save_exprs), region);
}

fn lowerStrInspektTupleByMonotype(
    self: *Self,
    module_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    tup: anytype,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const elem_span = tup.elems;
    const elems = self.store.monotype_store.getIdxSpan(elem_span);
    if (elems.len == 0) return self.emitMirStrLiteral("()", region);

    const save_exprs = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(save_exprs);

    try self.scratch_expr_ids.append(try self.emitMirStrLiteral("(", region));
    for (0..elem_span.len) |i| {
        const current_elems = self.store.monotype_store.getIdxSpan(elem_span);
        const elem_mono = current_elems[i];
        if (i > 0) {
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
        }
        const elem_expr = try self.emitMirStructAccess(value_expr, @intCast(i), elem_mono, region);
        try self.scratch_expr_ids.append(try self.lowerStrInspektExprByMonotype(module_env, elem_expr, elem_mono, region));
    }
    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(")", region));

    return self.foldMirStrConcat(self.scratch_expr_ids.sliceFromStart(save_exprs), region);
}

fn lowerStrInspektTagUnionByMonotype(
    self: *Self,
    module_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    tu: anytype,
    mono_idx: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const tag_span = tu.tags;
    const tags = self.store.monotype_store.getTags(tag_span);
    if (tags.len == 0) return self.emitMirStrLiteral("<empty_tag_union>", region);

    const save_branches = self.scratch_branches.top();
    defer self.scratch_branches.clearFrom(save_branches);

    for (0..tag_span.len) |tag_i| {
        const current_tags = self.store.monotype_store.getTags(tag_span);
        const tag = current_tags[tag_i];
        const payload_span = tag.payloads;
        const payloads = self.store.monotype_store.getIdxSpan(payload_span);

        const save_payload_patterns = self.scratch_pattern_ids.top();
        defer self.scratch_pattern_ids.clearFrom(save_payload_patterns);
        const save_payload_symbols = self.scratch_captures.top();
        defer self.scratch_captures.clearFrom(save_payload_symbols);

        for (self.store.monotype_store.getIdxSpan(payload_span)) |payload_mono| {
            const bind = try self.makeSyntheticBind(payload_mono, false);
            try self.scratch_pattern_ids.append(bind.pattern);
            try self.scratch_captures.append(.{ .symbol = bind.symbol });
        }

        const payload_pattern_span = try self.store.addPatternSpan(self.allocator, self.scratch_pattern_ids.sliceFromStart(save_payload_patterns));
        const tag_args = try self.wrapMultiPayloadTagPatterns(tag.name.ident, mono_idx, payload_pattern_span);
        const tag_pattern = try self.store.addPattern(
            self.allocator,
            .{ .tag = .{ .name = tag.name.ident, .args = tag_args } },
            mono_idx,
        );
        const branch_patterns = try self.store.addBranchPatterns(self.allocator, &.{MIR.BranchPattern{
            .pattern = tag_pattern,
            .degenerate = false,
        }});

        const body = if (payloads.len == 0) blk: {
            break :blk try self.emitMirStrLiteral(tag.name.text(self.all_module_envs), region);
        } else blk: {
            const save_parts = self.scratch_expr_ids.top();
            defer self.scratch_expr_ids.clearFrom(save_parts);

            const tag_open = try std.fmt.allocPrint(self.allocator, "{s}(", .{tag.name.text(self.all_module_envs)});
            defer self.allocator.free(tag_open);
            try self.scratch_expr_ids.append(try self.emitMirStrLiteral(tag_open, region));

            const payload_symbols = self.scratch_captures.sliceFromStart(save_payload_symbols);
            for (0..payload_span.len) |i| {
                const payload_mono = self.store.monotype_store.getIdxSpan(payload_span)[i];
                const payload_capture = payload_symbols[i];
                if (i > 0) {
                    try self.scratch_expr_ids.append(try self.emitMirStrLiteral(", ", region));
                }
                const payload_lookup = try self.emitMirLookup(payload_capture.symbol, payload_mono, region);
                try self.scratch_expr_ids.append(try self.lowerStrInspektExprByMonotype(module_env, payload_lookup, payload_mono, region));
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

fn lowerStrInspektListByMonotype(
    self: *Self,
    module_env: *const ModuleEnv,
    value_expr: MIR.ExprId,
    list_data: anytype,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const str_mono = self.store.monotype_store.primIdx(.str);
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );
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
    const elem_inspected = try self.lowerStrInspektExprByMonotype(module_env, elem_lookup, list_data.elem, region);
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
    const saved_root_expr_context = self.current_root_expr_context;
    if (self.current_proc_inst_context.isNone() and self.current_root_expr_context == null) {
        self.current_root_expr_context = expr_idx;
    }
    defer self.current_root_expr_context = saved_root_expr_context;
    return self.lowerExprWithMonotypeOverride(expr_idx, null);
}

fn lowerExprWithMonotypeOverrideIsolated(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    monotype_override: ?Monotype.Idx,
) Allocator.Error!MIR.ExprId {
    const saved_type_var_seen = self.type_var_seen;
    const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;
    self.type_var_seen = try saved_type_var_seen.clone();
    errdefer self.type_var_seen.deinit();
    self.nominal_cycle_breakers = try saved_nominal_cycle_breakers.clone();
    defer {
        self.type_var_seen.deinit();
        self.type_var_seen = saved_type_var_seen;
        self.nominal_cycle_breakers.deinit();
        self.nominal_cycle_breakers = saved_nominal_cycle_breakers;
    }

    return self.lowerExprWithMonotypeOverride(expr_idx, monotype_override);
}

fn lowerExprWithMonotypeOverride(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    monotype_override: ?Monotype.Idx,
) Allocator.Error!MIR.ExprId {
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
    const monotype = if (monotype_override) |override| override else try self.resolveMonotype(expr_idx);

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
        // bytes_literal (List(U8) file import) - not yet supported in MIR/LLVM backend
        .e_bytes_literal => try self.store.addExpr(self.allocator, .{ .runtime_err_anno_only = {} }, monotype, region),
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
            const elems = try self.stabilizeEscapingFunctionSpan(try self.lowerExprSpan(module_env, list.elems));
            return try self.store.addExpr(self.allocator, .{ .list = .{ .elems = elems } }, monotype, region);
        },
        .e_empty_record => try self.emitMirStructExpr(&.{}, monotype, region),
        .e_record => |record| {
            return try self.lowerRecord(module_env, record, monotype, region);
        },
        .e_tuple => |tuple| {
            const elems = try self.stabilizeEscapingFunctionSpan(try self.lowerExprSpan(module_env, tuple.elems));
            return try self.emitMirStructExprFromSpan(elems, monotype, region);
        },
        .e_tag => |tag| {
            const lowered_args = try self.stabilizeEscapingFunctionSpan(try self.lowerExprSpan(module_env, tag.args));
            const args = try self.wrapMultiPayloadTagExprs(tag.name, monotype, lowered_args, region);
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
            if (isTopLevelPattern(module_env, lookup.pattern_idx)) {
                if (self.monomorphization.getContextPatternProcInsts(self.current_proc_inst_context, self.current_module_idx, lookup.pattern_idx)) |proc_inst_ids| {
                    if (proc_inst_ids.len == 0) unreachable;
                    if (proc_inst_ids.len == 1) {
                        return try self.lowerProcInst(proc_inst_ids[0]);
                    }
                } else if (self.monomorphization.getLookupExprProcInst(
                    self.current_proc_inst_context,
                    self.monomorphizationRootExprContext(self.current_proc_inst_context),
                    self.current_module_idx,
                    expr_idx,
                )) |proc_inst_id| {
                    return try self.lowerProcInst(proc_inst_id);
                }
            }

            const symbol = try self.patternToSymbol(lookup.pattern_idx);
            const symbol_key: u64 = @bitCast(symbol);

            // Ensure the local definition is lowered if it's a top-level def.
            // This is needed so that cross-module lowering (via lowerExternalDef)
            // properly registers all transitively-referenced definitions.
            if (!self.lowered_symbols.contains(symbol_key) and !self.in_progress_defs.contains(symbol_key)) {
                // Find the CIR def for this pattern in the current module
                const defs = module_env.store.sliceDefs(module_env.all_defs);
                for (defs) |def_idx| {
                    const def = module_env.store.getDef(def_idx);
                    if (def.pattern == lookup.pattern_idx) {
                        if (cirExprIsProcBacked(module_env, def.expr)) break;
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
                            _ = try self.lowerExternalDefWithType(def_symbol, def.expr);
                        }

                        break;
                    }
                }
            }

            const lookup_var = ModuleEnv.varFrom(lookup.pattern_idx);
            if (self.symbol_monotypes.get(symbol.raw())) |bound_monotype| {
                const same_bound_mono = try self.monotypesStructurallyEqual(bound_monotype, monotype);
                if (same_bound_mono) {
                    return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, bound_monotype, region);
                }
                if (self.monotypeIsUnit(monotype) and !self.monotypeIsUnit(bound_monotype)) {
                    return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, bound_monotype, region);
                }
            }
            const pattern_monotype = try self.monotypeFromTypeVarWithBindings(
                self.current_module_idx,
                self.types_store,
                lookup_var,
                &self.type_var_seen,
                &self.nominal_cycle_breakers,
            );
            if (try self.monotypesStructurallyEqual(pattern_monotype, monotype)) {
                return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, pattern_monotype, region);
            }
            if (self.monotypeIsUnit(monotype) and !self.monotypeIsUnit(pattern_monotype)) {
                return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, pattern_monotype, region);
            }

            if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                const cached_monotype = self.store.typeOf(cached_expr);
                const monotype_matches_cached = try self.monotypesStructurallyEqual(cached_monotype, monotype);
                if (monotype_matches_cached) {
                    return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, cached_monotype, region);
                }
                if (self.monotypeIsUnit(monotype) and !self.monotypeIsUnit(cached_monotype)) {
                    return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, cached_monotype, region);
                }
            }

            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
        },
        .e_lookup_external => |ext| {
            if (self.monomorphization.getLookupExprProcInst(
                self.current_proc_inst_context,
                self.monomorphizationRootExprContext(self.current_proc_inst_context),
                self.current_module_idx,
                expr_idx,
            )) |proc_inst_id| {
                return try self.lowerProcInst(proc_inst_id);
            }

            // Import must be resolved before MIR lowering; reaching here
            // with an unresolved import means a compiler bug in an earlier phase.
            const target_module_idx: u32 = self.resolveImportedModuleIdx(module_env, ext.module_idx) orelse unreachable;
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

            // Ensure the external definition is lowered.
            const symbol_key: u64 = @bitCast(symbol);
            if (!self.lowered_symbols.contains(symbol_key) and !self.in_progress_defs.contains(symbol_key)) {
                const def_idx: CIR.Def.Idx = @enumFromInt(ext.target_node_idx);
                const def = target_env.store.getDef(def_idx);
                _ = try self.lowerExternalDefWithType(symbol, def.expr);
                if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                    const cached_monotype = self.store.typeOf(cached_expr);
                    if (self.monotypeIsUnit(monotype) and !self.monotypeIsUnit(cached_monotype)) {
                        return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, cached_monotype, region);
                    }
                }
                return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, monotype, region);
            }

            if (self.lowered_symbols.get(symbol_key)) |cached_expr| {
                const cached_monotype = self.store.typeOf(cached_expr);
                if (try self.monotypesStructurallyEqual(cached_monotype, monotype)) {
                    return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, cached_monotype, region);
                }
                if (self.monotypeIsUnit(monotype) and !self.monotypeIsUnit(cached_monotype)) {
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
            const required_lookup_monotype = try self.monotypeFromTypeVarInStore(
                self.current_module_idx,
                self.types_store,
                ModuleEnv.varFrom(required_type.type_anno),
            );
            _ = try self.lowerExternalDef(symbol, def.expr);
            return try self.store.addExpr(self.allocator, .{ .lookup = symbol }, required_lookup_monotype, region);
        },

        // --- Control flow ---
        .e_if => |if_expr| try self.lowerIf(module_env, if_expr, monotype, region),
        .e_match => |match_expr| try self.lowerMatch(module_env, match_expr, monotype, region),

        // --- Functions ---
        .e_lambda, .e_closure => try self.lowerExprProcInst(expr_idx, monotype),
        .e_call => |call| try self.lowerCall(module_env, expr_idx, call, monotype, region),

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
            return try self.emitMirStructAccess(tuple_expr, ta.elem_index, monotype, region);
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
        .e_hosted_lambda => try self.lowerExprProcInst(expr_idx, monotype),
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
        .struct_destructure,
        .list_destructure,
        .runtime_error,
        => null,
    };
}

fn resolveProcIdFromCallableExpr(self: *Self, expr_id: MIR.ExprId) ?MIR.ProcId {
    return switch (self.store.getExpr(expr_id)) {
        .proc_ref => |proc_id| proc_id,
        .closure_make => |closure| closure.proc,
        .block => |block| self.resolveProcIdFromCallableExpr(block.final_expr),
        .dbg_expr => |dbg_expr| self.resolveProcIdFromCallableExpr(dbg_expr.expr),
        .expect => |expect| self.resolveProcIdFromCallableExpr(expect.body),
        .return_expr => |ret| self.resolveProcIdFromCallableExpr(ret.expr),
        else => null,
    };
}

fn seedCallableParamSymbols(
    self: *Self,
    original_patterns: []const CIR.Pattern.Idx,
    lowered_params: MIR.PatternSpan,
) Allocator.Error!void {
    if (self.current_proc_inst_context.isNone()) return;

    const lowered_param_ids = self.store.getPatternSpan(lowered_params);
    if (builtin.mode == .Debug and lowered_param_ids.len != original_patterns.len) {
        std.debug.panic(
            "MIR Lower invariant: callable param seed arity mismatch (orig={d}, lowered={d})",
            .{ original_patterns.len, lowered_param_ids.len },
        );
    }

    for (original_patterns, lowered_param_ids) |original_pattern_idx, lowered_pattern_id| {
        const proc_inst_ids = self.monomorphization.getContextPatternProcInsts(
            self.current_proc_inst_context,
            self.current_module_idx,
            original_pattern_idx,
        ) orelse continue;
        const symbol = self.patternBoundSymbol(lowered_pattern_id) orelse continue;

        var proc_ids = std.ArrayList(MIR.ProcId).empty;
        defer proc_ids.deinit(self.allocator);
        for (proc_inst_ids) |proc_inst_id| {
            const proc_expr = try self.lowerProcInst(proc_inst_id);
            const proc_id = self.resolveProcIdFromCallableExpr(proc_expr) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "MIR Lower invariant: callable param pattern {d} lowered proc inst {d} to non-callable expr {}",
                        .{
                            @intFromEnum(original_pattern_idx),
                            @intFromEnum(proc_inst_id),
                            @intFromEnum(proc_expr),
                        },
                    );
                }
                unreachable;
            };

            var seen = false;
            for (proc_ids.items) |existing_proc_id| {
                if (existing_proc_id == proc_id) {
                    seen = true;
                    break;
                }
            }
            if (!seen) {
                try proc_ids.append(self.allocator, proc_id);
            }
        }

        if (proc_ids.items.len != 0) {
            std.mem.sortUnstable(
                MIR.ProcId,
                proc_ids.items,
                {},
                struct {
                    fn lessThan(_: void, lhs: MIR.ProcId, rhs: MIR.ProcId) bool {
                        return @intFromEnum(lhs) < @intFromEnum(rhs);
                    }
                }.lessThan,
            );
            try self.store.registerSymbolSeedProcSet(self.allocator, symbol, proc_ids.items);
        }
    }
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
                    .Rest => |sub_pattern_idx| try self.collectPatternBindings(module_env, sub_pattern_idx, out),
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

/// Function values are already proc-backed, so no extra identity stabilization is needed.
fn stabilizeEscapingFunctionExpr(_: *Self, expr: MIR.ExprId) Allocator.Error!MIR.ExprId {
    return expr;
}

fn stabilizeEscapingFunctionSpan(self: *Self, expr_span: MIR.ExprSpan) Allocator.Error!MIR.ExprSpan {
    const exprs = self.store.getExprSpan(expr_span);
    if (exprs.len == 0) return expr_span;

    const scratch_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(scratch_top);

    var changed = false;
    for (exprs) |expr| {
        const stabilized = try self.stabilizeEscapingFunctionExpr(expr);
        if (stabilized != expr) changed = true;
        try self.scratch_expr_ids.append(stabilized);
    }

    if (!changed) return expr_span;
    return try self.store.addExprSpan(self.allocator, self.scratch_expr_ids.sliceFromStart(scratch_top));
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
            const lhs_elem_span = lhs_tuple.elems;
            const rhs_elem_span = rhs_mono.tuple.elems;
            if (lhs_elem_span.len != rhs_elem_span.len) break :blk false;
            for (0..lhs_elem_span.len) |i| {
                const lhs_elems = self.store.monotype_store.getIdxSpan(lhs_elem_span);
                const rhs_elems = self.store.monotype_store.getIdxSpan(rhs_elem_span);
                const lhs_elem = lhs_elems[i];
                const rhs_elem = rhs_elems[i];
                if (!try self.monotypesStructurallyEqualRec(lhs_elem, rhs_elem, seen)) {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .func => |lhs_func| blk: {
            const rhs_func = rhs_mono.func;
            if (lhs_func.effectful != rhs_func.effectful) break :blk false;
            if (lhs_func.args.len != rhs_func.args.len) break :blk false;
            for (0..lhs_func.args.len) |i| {
                const lhs_args = self.store.monotype_store.getIdxSpan(lhs_func.args);
                const rhs_args = self.store.monotype_store.getIdxSpan(rhs_func.args);
                const lhs_arg = lhs_args[i];
                const rhs_arg = rhs_args[i];
                if (!try self.monotypesStructurallyEqualRec(lhs_arg, rhs_arg, seen)) {
                    break :blk false;
                }
            }
            break :blk try self.monotypesStructurallyEqualRec(lhs_func.ret, rhs_func.ret, seen);
        },
        .record => |lhs_record| blk: {
            const lhs_field_span = lhs_record.fields;
            const rhs_field_span = rhs_mono.record.fields;
            if (lhs_field_span.len != rhs_field_span.len) break :blk false;
            for (0..lhs_field_span.len) |i| {
                const lhs_fields = self.store.monotype_store.getFields(lhs_field_span);
                const rhs_fields = self.store.monotype_store.getFields(rhs_field_span);
                const lhs_field = lhs_fields[i];
                const rhs_field = rhs_fields[i];
                if (!self.identsStructurallyEqual(lhs_field.name, rhs_field.name)) break :blk false;
                if (!try self.monotypesStructurallyEqualRec(lhs_field.type_idx, rhs_field.type_idx, seen)) {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .tag_union => |lhs_union| blk: {
            const lhs_tag_span = lhs_union.tags;
            const rhs_tag_span = rhs_mono.tag_union.tags;
            if (lhs_tag_span.len != rhs_tag_span.len) break :blk false;
            for (0..lhs_tag_span.len) |tag_i| {
                const lhs_tags = self.store.monotype_store.getTags(lhs_tag_span);
                const rhs_tags = self.store.monotype_store.getTags(rhs_tag_span);
                const lhs_tag = lhs_tags[tag_i];
                const rhs_tag = rhs_tags[tag_i];

                if (!self.identsTagNameEquivalent(lhs_tag.name, rhs_tag.name)) break :blk false;

                if (lhs_tag.payloads.len != rhs_tag.payloads.len) break :blk false;
                for (0..lhs_tag.payloads.len) |payload_i| {
                    const lhs_payloads = self.store.monotype_store.getIdxSpan(lhs_tag.payloads);
                    const rhs_payloads = self.store.monotype_store.getIdxSpan(rhs_tag.payloads);
                    const lhs_payload = lhs_payloads[payload_i];
                    const rhs_payload = rhs_payloads[payload_i];
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
    if (!self.current_proc_inst_context.isNone()) {
        if (self.monomorphization.getExprMonotype(
            self.current_proc_inst_context,
            self.monomorphizationRootExprContext(self.current_proc_inst_context),
            self.current_module_idx,
            expr_idx,
        )) |mono| {
            return self.importMonotypeFromStore(
                &self.monomorphization.monotype_store,
                mono.idx,
                mono.module_idx,
                self.current_module_idx,
            );
        }
    }

    const type_var = ModuleEnv.varFrom(expr_idx);
    return try self.monotypeFromTypeVarWithBindings(
        self.current_module_idx,
        self.types_store,
        type_var,
        &self.type_var_seen,
        &self.nominal_cycle_breakers,
    );
}

fn currentCommonIdents(self: *const Self) ModuleEnv.CommonIdents {
    return self.all_module_envs[self.current_module_idx].idents;
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
    const monotype = try self.monotypeFromTypeVarWithBindings(
        self.current_module_idx,
        self.types_store,
        type_var,
        &self.type_var_seen,
        &self.nominal_cycle_breakers,
    );

    const lowered = switch (pattern) {
        .assign => blk: {
            const symbol = try self.patternToSymbol(pattern_idx);
            break :blk try self.store.addPattern(self.allocator, .{ .bind = symbol }, monotype);
        },
        .underscore => try self.store.addPattern(self.allocator, .wildcard, monotype),
        .as => |a| blk: {
            const inner = try self.lowerPattern(module_env, a.pattern);
            const symbol = try self.patternToSymbol(pattern_idx);
            break :blk try self.store.addPattern(self.allocator, .{ .as_pattern = .{
                .pattern = inner,
                .symbol = symbol,
            } }, monotype);
        },
        .applied_tag => |tag| blk: {
            const lowered_args = try self.lowerPatternSpan(module_env, tag.args);
            const args = try self.wrapMultiPayloadTagPatterns(tag.name, monotype, lowered_args);
            break :blk try self.store.addPattern(self.allocator, .{ .tag = .{
                .name = tag.name,
                .args = args,
            } }, monotype);
        },
        .nominal => |nom| blk: {
            // Strip nominal wrapper, but keep the nominal monotype
            // (same pattern as e_nominal expression lowering).
            const result = try self.lowerPattern(module_env, nom.backing_pattern);
            self.store.pattern_type_map.items[@intFromEnum(result)] = monotype;
            break :blk result;
        },
        .nominal_external => |nom_ext| blk: {
            // Strip nominal wrapper, but keep the nominal monotype
            // (same pattern as e_nominal_external expression lowering).
            const result = try self.lowerPattern(module_env, nom_ext.backing_pattern);
            self.store.pattern_type_map.items[@intFromEnum(result)] = monotype;
            break :blk result;
        },
        .num_literal => |nl| try self.store.addPattern(self.allocator, .{ .int_literal = .{ .value = nl.value } }, monotype),
        .str_literal => |sl| blk: {
            const mir_str = try self.copyStringToMir(module_env, sl.literal);
            break :blk try self.store.addPattern(self.allocator, .{ .str_literal = mir_str }, monotype);
        },
        .dec_literal => |dl| try self.store.addPattern(self.allocator, .{ .dec_literal = dl.value }, monotype),
        .small_dec_literal => |sdl| blk: {
            const roc_dec = sdl.value.toRocDec();
            break :blk try self.store.addPattern(self.allocator, .{ .dec_literal = roc_dec }, monotype);
        },
        .frac_f32_literal => |fl| try self.store.addPattern(self.allocator, .{ .frac_f32_literal = fl.value }, monotype),
        .frac_f64_literal => |fl| try self.store.addPattern(self.allocator, .{ .frac_f64_literal = fl.value }, monotype),
        .runtime_error => try self.store.addPattern(self.allocator, .runtime_error, monotype),
        .list => |list_pat| blk: {
            const patterns = try self.lowerPatternSpan(module_env, list_pat.patterns);
            var rest_index: MIR.RestIndex = .none;
            var rest_pattern: MIR.PatternId = MIR.PatternId.none;
            if (list_pat.rest_info) |rest| {
                rest_index = @enumFromInt(rest.index);
                if (rest.pattern) |rest_pat_idx| {
                    rest_pattern = try self.lowerPattern(module_env, rest_pat_idx);
                }
            }
            break :blk try self.store.addPattern(self.allocator, .{ .list_destructure = .{
                .patterns = patterns,
                .rest_index = rest_index,
                .rest_pattern = rest_pattern,
            } }, monotype);
        },
        .record_destructure => |record_pat| blk: {
            const cir_destructs = module_env.store.sliceRecordDestructs(record_pat.destructs);
            const pats_top = self.scratch_pattern_ids.top();
            defer self.scratch_pattern_ids.clearFrom(pats_top);
            const mono_field_span = switch (self.store.monotype_store.getMonotype(monotype)) {
                .record => |record_mono| record_mono.fields,
                .unit => Monotype.FieldSpan.empty(),
                else => typeBindingInvariant(
                    "lowerPattern(record_destructure): expected record monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(monotype))},
                ),
            };
            const mono_fields_for_defaults = self.store.monotype_store.getFields(mono_field_span);

            for (mono_fields_for_defaults) |mono_field| {
                try self.scratch_pattern_ids.append(
                    try self.store.addPattern(self.allocator, .wildcard, mono_field.type_idx),
                );
            }

            for (cir_destructs) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                const pat_idx = destruct.kind.toPatternIdx();
                const mir_pat = try self.lowerPattern(module_env, pat_idx);
                const mono_fields = self.store.monotype_store.getFields(mono_field_span);
                const field_idx = self.recordFieldIndexByName(destruct.label, mono_fields);
                self.scratch_pattern_ids.items.items[@intCast(pats_top + field_idx)] = mir_pat;
            }

            const destructs_span = try self.store.addPatternSpan(self.allocator, self.scratch_pattern_ids.sliceFromStart(pats_top));
            break :blk try self.store.addPattern(self.allocator, .{ .struct_destructure = .{
                .fields = destructs_span,
            } }, monotype);
        },
        .tuple => |tuple_pat| blk: {
            const elems = try self.lowerPatternSpan(module_env, tuple_pat.patterns);
            break :blk try self.store.addPattern(self.allocator, .{ .struct_destructure = .{ .fields = elems } }, monotype);
        },
    };

    try self.registerPatternSymbolMonotypes(lowered);
    return lowered;
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

/// Lower `e_lambda` to a proc-backed MIR function value (no captures).
fn lowerExprProcInst(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    _: Monotype.Idx,
) Allocator.Error!MIR.ExprId {
    const rooted_proc_inst_id = self.monomorphization.getExprProcInst(
        self.current_proc_inst_context,
        self.monomorphizationRootExprContext(self.current_proc_inst_context),
        self.current_module_idx,
        expr_idx,
    );
    const proc_inst_id = rooted_proc_inst_id orelse lookup_proc_inst: {
        if (self.current_proc_inst_context.isNone()) {
            if (self.monomorphization.getExprProcInst(.none, null, self.current_module_idx, expr_idx)) |canonical_proc_inst_id| {
                break :lookup_proc_inst canonical_proc_inst_id;
            }
        }
        if (builtin.mode == .Debug) {
            const module_env = self.all_module_envs[self.current_module_idx];
            const expr = module_env.store.getExpr(expr_idx);
            const template_id = self.monomorphization.getExprProcTemplate(self.current_module_idx, expr_idx);
            const root_proc_inst = self.monomorphization.getExprProcInst(
                .none,
                self.monomorphizationRootExprContext(.none),
                self.current_module_idx,
                expr_idx,
            );
            std.debug.panic(
                "MIR Lower invariant: callable expr {d} in module {d} ({s}) has no proc inst in context {d} template={d} root_context_proc_inst={d}",
                .{
                    @intFromEnum(expr_idx),
                    self.current_module_idx,
                    @tagName(expr),
                    @intFromEnum(self.current_proc_inst_context),
                    if (template_id) |id| @intFromEnum(id) else std.math.maxInt(u32),
                    if (root_proc_inst) |id| @intFromEnum(id) else std.math.maxInt(u32),
                },
            );
        }
        unreachable;
    };
    return self.lowerProcInst(proc_inst_id);
}

fn bindProcTemplateBoundaryMonotypes(
    self: *Self,
    module_env: *const ModuleEnv,
    proc_expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
) Allocator.Error!void {
    try self.bindTypeVarMonotypes(ModuleEnv.varFrom(proc_expr_idx), fn_monotype);

    const func = switch (self.store.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func,
        else => unreachable,
    };
    const arg_monos = self.store.monotype_store.getIdxSpan(func.args);

    const ProcBoundary = struct {
        arg_patterns: []const CIR.Pattern.Idx,
        body_expr: CIR.Expr.Idx,
    };

    const proc_expr = module_env.store.getExpr(proc_expr_idx);
    const boundary: ProcBoundary = switch (proc_expr) {
        .e_lambda => |lambda| .{
            .arg_patterns = module_env.store.slicePatterns(lambda.args),
            .body_expr = lambda.body,
        },
        .e_closure => |closure| blk: {
            const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
            if (lambda_expr != .e_lambda) unreachable;
            break :blk .{
                .arg_patterns = module_env.store.slicePatterns(lambda_expr.e_lambda.args),
                .body_expr = lambda_expr.e_lambda.body,
            };
        },
        .e_hosted_lambda => |hosted| .{
            .arg_patterns = module_env.store.slicePatterns(hosted.args),
            .body_expr = hosted.body,
        },
        else => unreachable,
    };

    if (builtin.mode == .Debug and boundary.arg_patterns.len != arg_monos.len) {
        std.debug.panic(
            "bindProcTemplateBoundaryMonotypes: arity mismatch for expr {d} (patterns={d}, monos={d})",
            .{ @intFromEnum(proc_expr_idx), boundary.arg_patterns.len, arg_monos.len },
        );
    }

    for (boundary.arg_patterns, arg_monos) |pattern_idx, arg_mono| {
        try self.bindTypeVarMonotypes(ModuleEnv.varFrom(pattern_idx), arg_mono);
    }
    try self.bindTypeVarMonotypes(ModuleEnv.varFrom(boundary.body_expr), func.ret);
}

fn lowerLambdaSpecialized(
    self: *Self,
    module_env: *const ModuleEnv,
    lambda: CIR.Expr.Lambda,
    monotype: Monotype.Idx,
    region: Region,
    proc_inst_id: ?Monomorphize.ProcInstId,
) Allocator.Error!MIR.ExprId {
    const ret_monotype = switch (self.store.monotype_store.getMonotype(monotype)) {
        .func => |func| func.ret,
        else => unreachable,
    };
    const proc_id = try self.store.addProc(self.allocator, .{
        .fn_monotype = monotype,
        .params = MIR.PatternSpan.empty(),
        .body = MIR.ExprId.none,
        .ret_monotype = ret_monotype,
        .debug_name = MIR.Symbol.none,
        .source_region = region,
        .capture_bindings = MIR.CaptureBindingSpan.empty(),
        .captures_param = .none,
        .recursion = .not_recursive,
        .hosted = null,
    });
    const proc_expr = try self.store.addExpr(self.allocator, .{ .proc_ref = proc_id }, monotype, region);
    if (proc_inst_id) |inst_id| {
        try self.in_progress_proc_insts.put(@intFromEnum(inst_id), proc_expr);
        errdefer _ = self.in_progress_proc_insts.remove(@intFromEnum(inst_id));
    }

    const saved_pattern_scope = self.current_pattern_scope;
    self.current_pattern_scope = if (proc_inst_id) |inst_id|
        patternScopeForProcInst(inst_id)
    else
        @intFromEnum(proc_id);
    defer self.current_pattern_scope = saved_pattern_scope;

    const params = try self.lowerPatternSpan(module_env, lambda.args);
    try self.seedCallableParamSymbols(module_env.store.slicePatterns(lambda.args), params);
    const body = try self.lowerExpr(lambda.body);

    self.store.getProcPtr(proc_id).* = .{
        .fn_monotype = monotype,
        .params = params,
        .body = body,
        .ret_monotype = ret_monotype,
        .debug_name = MIR.Symbol.none,
        .source_region = region,
        .capture_bindings = MIR.CaptureBindingSpan.empty(),
        .captures_param = .none,
        .recursion = .not_recursive,
        .hosted = null,
    };

    if (proc_inst_id) |inst_id| {
        _ = self.in_progress_proc_insts.remove(@intFromEnum(inst_id));
        try self.lowered_proc_insts.put(@intFromEnum(inst_id), proc_expr);
    }

    return proc_expr;
}

/// Lower `e_closure` by lifting it to a top-level function with an explicit captures tuple parameter.
/// At the use site, returns a tuple of the captured values and registers explicit
/// MIR closure-member metadata for downstream analysis and lowering.
fn lowerClosureSpecialized(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    closure: CIR.Expr.Closure,
    monotype: Monotype.Idx,
    region: Region,
    proc_inst_id: ?Monomorphize.ProcInstId,
) Allocator.Error!MIR.ExprId {
    const inner_lambda_expr = module_env.store.getExpr(closure.lambda_idx);
    const lambda = inner_lambda_expr.e_lambda;
    const cir_capture_indices_all = module_env.store.sliceCaptures(closure.captures);
    const CaptureIdx = @TypeOf(cir_capture_indices_all[0]);
    const self_binding_pattern = blk: {
        if (proc_inst_id) |inst_id| {
            const proc_inst = self.monomorphization.getProcInst(inst_id);
            const template = self.monomorphization.getProcTemplate(proc_inst.template);
            break :blk template.binding_pattern;
        }
        break :blk null;
    };
    var cir_capture_indices_filtered = std.ArrayList(CaptureIdx).empty;
    defer cir_capture_indices_filtered.deinit(self.allocator);
    var self_capture_name: ?Ident.Idx = null;
    var has_self_capture = false;

    for (cir_capture_indices_all) |cap_idx| {
        const cap = module_env.store.getCapture(cap_idx);
        if (self_binding_pattern) |pattern_idx| {
            if (cap.pattern_idx == pattern_idx) {
                has_self_capture = true;
                if (self_capture_name == null) self_capture_name = cap.name;
                continue;
            }
        }
        try cir_capture_indices_filtered.append(self.allocator, cap_idx);
    }
    const cir_capture_indices = cir_capture_indices_filtered.items;

    if (cir_capture_indices.len == 0 and !has_self_capture) {
        // No captures — just lower as a plain lambda (no lifting needed).
        return self.lowerLambdaSpecialized(module_env, lambda, monotype, region, proc_inst_id);
    }

    var capture_monotypes_snapshot = std.ArrayList(Monotype.Idx).empty;
    defer capture_monotypes_snapshot.deinit(self.allocator);
    var capture_lookup_exprs_snapshot = std.ArrayList(MIR.ExprId).empty;
    defer capture_lookup_exprs_snapshot.deinit(self.allocator);
    var capture_local_symbols = std.ArrayList(MIR.Symbol).empty;
    defer capture_local_symbols.deinit(self.allocator);

    const orig_monotype = self.store.monotype_store.getMonotype(monotype);
    const orig_func = orig_monotype.func;
    var captures_tuple_monotype = Monotype.Idx.none;
    const closure_proc_inst_id = proc_inst_id orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "MIR Lower invariant: captured closure expr {d} in module {d} is missing a proc inst",
                .{ @intFromEnum(expr_idx), self.current_module_idx },
            );
        }
        unreachable;
    };
    const proc_id = try self.store.addProc(self.allocator, .{
        .fn_monotype = monotype,
        .params = MIR.PatternSpan.empty(),
        .body = MIR.ExprId.none,
        .ret_monotype = orig_func.ret,
        .debug_name = MIR.Symbol.none,
        .source_region = region,
        .capture_bindings = MIR.CaptureBindingSpan.empty(),
        .captures_param = .none,
        .recursion = if (has_self_capture) .recursive else .not_recursive,
        .hosted = null,
    });
    var captures_param_pattern = MIR.PatternId.none;
    const proc_expr = if (cir_capture_indices.len == 0) blk: {
        break :blk try self.store.addExpr(self.allocator, .{ .proc_ref = proc_id }, monotype, region);
    } else blk: {
        const idxs_top = self.mono_scratches.idxs.top();
        defer self.mono_scratches.idxs.clearFrom(idxs_top);
        const expr_top = self.scratch_expr_ids.top();
        defer self.scratch_expr_ids.clearFrom(expr_top);

        for (cir_capture_indices) |cap_idx| {
            const cap = module_env.store.getCapture(cap_idx);
            const outer_symbol = try self.patternToSymbol(cap.pattern_idx);
            const resolved_cap_monotype = self.monomorphization.getClosureCaptureMonotype(
                closure_proc_inst_id,
                self.current_module_idx,
                expr_idx,
                cap.pattern_idx,
            ) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "MIR Lower invariant: missing capture monotype for closure expr {d} pattern {d} proc_inst={d}",
                        .{
                            @intFromEnum(expr_idx),
                            @intFromEnum(cap.pattern_idx),
                            @intFromEnum(closure_proc_inst_id),
                        },
                    );
                }
                unreachable;
            };
            const cap_monotype = try self.importMonotypeFromStore(
                &self.monomorphization.monotype_store,
                resolved_cap_monotype.idx,
                resolved_cap_monotype.module_idx,
                self.current_module_idx,
            );
            try self.mono_scratches.idxs.append(cap_monotype);
            const capture_proc_inst = self.monomorphization.getClosureCaptureProcInst(
                closure_proc_inst_id,
                self.current_module_idx,
                expr_idx,
                cap.pattern_idx,
            );
            const capture_expr = if (capture_proc_inst) |capture_proc_inst_id|
                try self.lowerProcInst(capture_proc_inst_id)
            else
                try self.store.addExpr(self.allocator, .{ .lookup = outer_symbol }, cap_monotype, region);
            try self.scratch_expr_ids.append(capture_expr);
        }

        const capture_monotypes = self.mono_scratches.idxs.sliceFromStart(idxs_top);
        const capture_lookup_exprs = self.scratch_expr_ids.sliceFromStart(expr_top);
        try capture_monotypes_snapshot.appendSlice(self.allocator, capture_monotypes);
        try capture_lookup_exprs_snapshot.appendSlice(self.allocator, capture_lookup_exprs);

        const captures_tuple_elems = try self.store.monotype_store.addIdxSpan(self.allocator, capture_monotypes_snapshot.items);
        captures_tuple_monotype = try self.store.monotype_store.addMonotype(self.allocator, .{ .tuple = .{
            .elems = captures_tuple_elems,
        } });

        const captures_param_ident = self.makeSyntheticIdent(closure.tag_name);
        const captures_param_symbol = try self.internSymbol(self.current_module_idx, captures_param_ident);
        captures_param_pattern = try self.store.addPattern(self.allocator, .{ .bind = captures_param_symbol }, captures_tuple_monotype);

        const captures_tuple_span = try self.store.addExprSpan(self.allocator, capture_lookup_exprs_snapshot.items);
        const captures_tuple_expr = try self.emitMirStructExprFromSpan(captures_tuple_span, captures_tuple_monotype, region);
        break :blk try self.store.addExpr(self.allocator, .{ .closure_make = .{
            .proc = proc_id,
            .captures = captures_tuple_expr,
        } }, monotype, region);
    };

    // --- Step 4: Enter a new scope for the lifted function body ---
    const saved_pattern_scope = self.current_pattern_scope;
    self.current_pattern_scope = if (proc_inst_id) |inst_id|
        patternScopeForProcInst(inst_id)
    else
        @intFromEnum(proc_id);
    defer self.current_pattern_scope = saved_pattern_scope;

    var self_local_symbol: ?MIR.Symbol = null;
    if (has_self_capture) {
        const self_pattern_idx = self_binding_pattern orelse unreachable;
        const base_key: u64 = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(self_pattern_idx);
        const scoped_key: u128 = (@as(u128, self.current_pattern_scope) << 64) | @as(u128, base_key);
        const local_ident = self.makeSyntheticIdent(self_capture_name orelse closure.tag_name);
        const local_symbol = try self.internSymbol(self.current_module_idx, local_ident);
        try self.pattern_symbols.put(scoped_key, local_symbol);
        self_local_symbol = local_symbol;
    }

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
        try capture_local_symbols.append(self.allocator, local_symbol);
    }

    if (proc_inst_id) |inst_id| {
        const in_progress_expr = if (self_local_symbol) |self_symbol|
            try self.store.addExpr(self.allocator, .{ .lookup = self_symbol }, monotype, region)
        else
            proc_expr;
        try self.in_progress_proc_insts.put(@intFromEnum(inst_id), in_progress_expr);
        errdefer _ = self.in_progress_proc_insts.remove(@intFromEnum(inst_id));
    }

    // --- Step 6: Lower the lambda params and body in the new scope ---
    const params = try self.lowerPatternSpan(module_env, lambda.args);
    try self.seedCallableParamSymbols(module_env.store.slicePatterns(lambda.args), params);
    const body = try self.lowerExpr(lambda.body);

    // --- Step 7: Build destructuring preamble ---
    // For each capture: `let local_sym = tuple_access(lookup(captures_param), i)`
    const stmts_top = self.scratch_stmts.top();
    defer self.scratch_stmts.clearFrom(stmts_top);

    for (cir_capture_indices, 0..) |_, i| {
        const cap_monotype = capture_monotypes_snapshot.items[i];
        const local_symbol = capture_local_symbols.items[i];

        // lookup(captures_param)
        const captures_lookup = try self.store.addExpr(self.allocator, .{ .lookup = self.store.getPattern(captures_param_pattern).bind }, captures_tuple_monotype, region);

        // struct_access(captures_lookup, i)
        const tuple_access_expr = try self.emitMirStructAccess(
            captures_lookup,
            @intCast(i),
            cap_monotype,
            region,
        );

        // let local_sym = tuple_access_expr
        const bind_pat = try self.store.addPattern(self.allocator, .{ .bind = local_symbol }, cap_monotype);
        try self.registerBoundSymbolDefIfNeeded(bind_pat, tuple_access_expr);
        try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = bind_pat, .expr = tuple_access_expr } });
    }

    if (self_local_symbol) |local_symbol| {
        const self_expr = if (cir_capture_indices.len == 0) blk: {
            break :blk try self.store.addExpr(self.allocator, .{ .proc_ref = proc_id }, monotype, region);
        } else blk: {
            const self_expr_top = self.scratch_expr_ids.top();
            defer self.scratch_expr_ids.clearFrom(self_expr_top);

            for (capture_local_symbols.items, capture_monotypes_snapshot.items) |capture_symbol, cap_monotype| {
                const capture_lookup = try self.store.addExpr(self.allocator, .{ .lookup = capture_symbol }, cap_monotype, region);
                try self.scratch_expr_ids.append(capture_lookup);
            }

            const self_capture_span = try self.store.addExprSpan(self.allocator, self.scratch_expr_ids.sliceFromStart(self_expr_top));
            const self_captures_tuple = try self.emitMirStructExprFromSpan(self_capture_span, captures_tuple_monotype, region);
            break :blk try self.store.addExpr(self.allocator, .{ .closure_make = .{
                .proc = proc_id,
                .captures = self_captures_tuple,
            } }, monotype, region);
        };

        const self_bind_pat = try self.store.addPattern(self.allocator, .{ .bind = local_symbol }, monotype);
        try self.registerBoundSymbolDefIfNeeded(self_bind_pat, self_expr);
        try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = self_bind_pat, .expr = self_expr } });
    }

    // --- Step 7: Wrap body in block with destructuring stmts ---
    const preamble_stmts = try self.store.addStmts(self.allocator, self.scratch_stmts.sliceFromStart(stmts_top));
    const lifted_body = if (preamble_stmts.isEmpty())
        body
    else
        try self.store.addExpr(self.allocator, .{ .block = .{
            .stmts = preamble_stmts,
            .final_expr = body,
        } }, self.store.typeOf(body), region);

    // --- Step 8: Build the lifted lambda's param list (original params + captures param) ---
    const orig_param_ids = self.store.getPatternSpan(params);
    const pat_top = self.scratch_pattern_ids.top();
    defer self.scratch_pattern_ids.clearFrom(pat_top);
    for (orig_param_ids) |pid| {
        try self.scratch_pattern_ids.append(pid);
    }
    if (!captures_param_pattern.isNone()) {
        try self.scratch_pattern_ids.append(captures_param_pattern);
    }
    const all_params = try self.store.addPatternSpan(self.allocator, self.scratch_pattern_ids.sliceFromStart(pat_top));

    const capture_binding_span = if (cir_capture_indices.len == 0) MIR.CaptureBindingSpan.empty() else blk: {
        const binding_top = self.scratch_capture_bindings.top();
        defer self.scratch_capture_bindings.clearFrom(binding_top);
        for (cir_capture_indices, 0..) |_, i| {
            try self.scratch_capture_bindings.append(.{
                .local_symbol = capture_local_symbols.items[i],
                .source_expr = capture_lookup_exprs_snapshot.items[i],
                .monotype = capture_monotypes_snapshot.items[i],
            });
        }
        break :blk try self.store.addCaptureBindings(self.allocator, self.scratch_capture_bindings.sliceFromStart(binding_top));
    };

    self.store.getProcPtr(proc_id).* = .{
        .fn_monotype = monotype,
        .params = all_params,
        .body = lifted_body,
        .ret_monotype = orig_func.ret,
        .debug_name = MIR.Symbol.none,
        .source_region = region,
        .capture_bindings = capture_binding_span,
        .captures_param = captures_param_pattern,
        .recursion = if (has_self_capture) .recursive else .not_recursive,
        .hosted = null,
    };

    if (cir_capture_indices.len != 0) {
        // --- Step 9: Register the lifted proc and its semantic closure member ---
        const member_id = try self.store.addClosureMember(self.allocator, .{
            .proc = proc_id,
            .capture_bindings = capture_binding_span,
        });

        try self.store.registerExprClosureMember(self.allocator, proc_expr, member_id);
    }

    if (proc_inst_id) |inst_id| {
        _ = self.in_progress_proc_insts.remove(@intFromEnum(inst_id));
        try self.lowered_proc_insts.put(@intFromEnum(inst_id), proc_expr);
    }

    return proc_expr;
}

fn lowerHostedLambdaSpecialized(
    self: *Self,
    module_env: *const ModuleEnv,
    hosted: anytype,
    monotype: Monotype.Idx,
    region: Region,
    proc_inst_id: ?Monomorphize.ProcInstId,
) Allocator.Error!MIR.ExprId {
    const ret_monotype = switch (self.store.monotype_store.getMonotype(monotype)) {
        .func => |func| func.ret,
        else => unreachable,
    };
    const proc_id = try self.store.addProc(self.allocator, .{
        .fn_monotype = monotype,
        .params = MIR.PatternSpan.empty(),
        .body = MIR.ExprId.none,
        .ret_monotype = ret_monotype,
        .debug_name = MIR.Symbol.none,
        .source_region = region,
        .capture_bindings = MIR.CaptureBindingSpan.empty(),
        .captures_param = .none,
        .recursion = .not_recursive,
        .hosted = .{
            .symbol_name = hosted.symbol_name,
            .index = hosted.index,
        },
    });
    const proc_expr = try self.store.addExpr(self.allocator, .{ .proc_ref = proc_id }, monotype, region);
    if (proc_inst_id) |inst_id| {
        try self.in_progress_proc_insts.put(@intFromEnum(inst_id), proc_expr);
        errdefer _ = self.in_progress_proc_insts.remove(@intFromEnum(inst_id));
    }

    const saved_pattern_scope = self.current_pattern_scope;
    self.current_pattern_scope = if (proc_inst_id) |inst_id|
        patternScopeForProcInst(inst_id)
    else
        @intFromEnum(proc_id);
    defer self.current_pattern_scope = saved_pattern_scope;

    const params = try self.lowerPatternSpan(module_env, hosted.args);
    try self.seedCallableParamSymbols(module_env.store.slicePatterns(hosted.args), params);
    const body = try self.lowerExpr(hosted.body);

    self.store.getProcPtr(proc_id).* = .{
        .fn_monotype = monotype,
        .params = params,
        .body = body,
        .ret_monotype = ret_monotype,
        .debug_name = MIR.Symbol.none,
        .source_region = region,
        .capture_bindings = MIR.CaptureBindingSpan.empty(),
        .captures_param = .none,
        .recursion = .not_recursive,
        .hosted = .{
            .symbol_name = hosted.symbol_name,
            .index = hosted.index,
        },
    };

    if (proc_inst_id) |inst_id| {
        _ = self.in_progress_proc_insts.remove(@intFromEnum(inst_id));
        try self.lowered_proc_insts.put(@intFromEnum(inst_id), proc_expr);
    }

    return proc_expr;
}

fn cirExprNeedsCallableOverrideIsolation(module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx) Allocator.Error!bool {
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_lambda, .e_closure, .e_hosted_lambda => true,
        else => false,
    };
}

fn monotypeCanRefine(
    self: *Self,
    existing: Monotype.Idx,
    candidate: Monotype.Idx,
) Allocator.Error!bool {
    if (existing == candidate) return true;
    if (!self.monotypeIdxIsValid(existing) or !self.monotypeIdxIsValid(candidate)) return false;

    const existing_mono = self.store.monotype_store.getMonotype(existing);
    const candidate_mono = self.store.monotype_store.getMonotype(candidate);

    return switch (existing_mono) {
        .unit => true,
        .prim => false,
        .recursive_placeholder => false,
        .list => |existing_list| switch (candidate_mono) {
            .list => |candidate_list| self.monotypeCanRefine(existing_list.elem, candidate_list.elem),
            else => false,
        },
        .box => |existing_box| switch (candidate_mono) {
            .box => |candidate_box| self.monotypeCanRefine(existing_box.inner, candidate_box.inner),
            else => false,
        },
        .tuple => |existing_tuple| switch (candidate_mono) {
            .tuple => |candidate_tuple| blk: {
                const existing_elems = self.store.monotype_store.getIdxSpan(existing_tuple.elems);
                const candidate_elems = self.store.monotype_store.getIdxSpan(candidate_tuple.elems);
                if (existing_elems.len != candidate_elems.len) break :blk false;
                for (existing_elems, candidate_elems) |existing_elem, candidate_elem| {
                    if (!(try self.monotypeCanRefine(existing_elem, candidate_elem))) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
        .func => |existing_func| switch (candidate_mono) {
            .func => |candidate_func| blk: {
                const existing_args = self.store.monotype_store.getIdxSpan(existing_func.args);
                const candidate_args = self.store.monotype_store.getIdxSpan(candidate_func.args);
                if (existing_args.len != candidate_args.len) break :blk false;
                if (existing_func.effectful != candidate_func.effectful) break :blk false;
                for (existing_args, candidate_args) |existing_arg, candidate_arg| {
                    if (!(try self.monotypeCanRefine(existing_arg, candidate_arg))) break :blk false;
                }
                break :blk try self.monotypeCanRefine(existing_func.ret, candidate_func.ret);
            },
            else => false,
        },
        .record => |existing_record| switch (candidate_mono) {
            .record => |candidate_record| blk: {
                const existing_fields = self.store.monotype_store.getFields(existing_record.fields);
                const candidate_fields = self.store.monotype_store.getFields(candidate_record.fields);
                if (existing_fields.len != candidate_fields.len) break :blk false;
                for (existing_fields, candidate_fields) |existing_field, candidate_field| {
                    if (!self.identsStructurallyEqual(existing_field.name, candidate_field.name)) break :blk false;
                    if (!(try self.monotypeCanRefine(existing_field.type_idx, candidate_field.type_idx))) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
        .tag_union => |existing_union| switch (candidate_mono) {
            .tag_union => |candidate_union| blk: {
                const existing_tags = self.store.monotype_store.getTags(existing_union.tags);
                const candidate_tags = self.store.monotype_store.getTags(candidate_union.tags);
                if (existing_tags.len != candidate_tags.len) break :blk false;
                for (existing_tags, candidate_tags) |existing_tag, candidate_tag| {
                    if (!self.identsTagNameEquivalent(existing_tag.name, candidate_tag.name)) break :blk false;
                    const existing_payloads = self.store.monotype_store.getIdxSpan(existing_tag.payloads);
                    const candidate_payloads = self.store.monotype_store.getIdxSpan(candidate_tag.payloads);
                    if (existing_payloads.len != candidate_payloads.len) break :blk false;
                    for (existing_payloads, candidate_payloads) |existing_payload, candidate_payload| {
                        if (!(try self.monotypeCanRefine(existing_payload, candidate_payload))) break :blk false;
                    }
                }
                break :blk true;
            },
            else => false,
        },
    };
}

/// Lower `e_call` to MIR call.
/// If the call target is a lookup to a low-level builtin wrapper
/// (e.g., List.concat, Str.concat), emit `run_low_level` instead of `call`.
fn lowerProcInst(self: *Self, proc_inst_id: Monomorphize.ProcInstId) Allocator.Error!MIR.ExprId {
    const proc_inst_key = @intFromEnum(proc_inst_id);
    if (self.lowered_proc_insts.get(proc_inst_key)) |cached| return cached;
    if (self.in_progress_proc_insts.get(proc_inst_key)) |in_progress| return in_progress;

    const proc_inst = self.monomorphization.getProcInst(proc_inst_id);
    const template = self.monomorphization.getProcTemplate(proc_inst.template);
    const module_idx = template.module_idx;
    const module_env = self.all_module_envs[module_idx];
    const proc_monotype = try self.importMonotypeFromStore(
        &self.monomorphization.monotype_store,
        proc_inst.fn_monotype,
        proc_inst.fn_monotype_module_idx,
        module_idx,
    );

    const switching_module = module_idx != self.current_module_idx;
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_type_var_seen = self.type_var_seen;
    const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_mono_module_idx = self.mono_scratches.module_idx;
    const saved_proc_inst_context = self.current_proc_inst_context;
    const saved_pattern_scope = self.current_pattern_scope;
    if (switching_module) {
        self.current_module_idx = module_idx;
        self.types_store = &module_env.types;
        self.mono_scratches.ident_store = module_env.getIdentStoreConst();
        self.mono_scratches.module_env = module_env;
        self.mono_scratches.module_idx = module_idx;
    }

    self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.current_proc_inst_context = proc_inst_id;
    self.current_pattern_scope = patternScopeForProcInst(proc_inst.defining_context_proc_inst);

    try self.seedTypeScopeBindingsInStore(
        self.current_module_idx,
        self.types_store,
        &self.type_var_seen,
    );

    if (!proc_inst.subst.isNone()) {
        const subst = self.monomorphization.getTypeSubst(proc_inst.subst);
        for (self.monomorphization.getTypeSubstEntries(subst.entries)) |entry| {
            if (std.debug.runtime_safety and entry.key.module_idx != module_idx) {
                std.debug.panic(
                    "Lower: proc inst subst entry from module {d} imported into module {d}",
                    .{ entry.key.module_idx, module_idx },
                );
            }
            const imported_mono = try self.importMonotypeFromStore(
                &self.monomorphization.monotype_store,
                entry.monotype.idx,
                entry.monotype.module_idx,
                module_idx,
            );
            try self.type_var_seen.put(entry.key.type_var, imported_mono);
        }
    }

    try self.bindProcTemplateBoundaryMonotypes(module_env, template.cir_expr, proc_monotype);

    defer {
        self.type_var_seen.deinit();
        self.type_var_seen = saved_type_var_seen;
        self.nominal_cycle_breakers.deinit();
        self.nominal_cycle_breakers = saved_nominal_cycle_breakers;
        self.current_proc_inst_context = saved_proc_inst_context;
        self.current_pattern_scope = saved_pattern_scope;
        if (switching_module) {
            self.current_module_idx = saved_module_idx;
            self.types_store = saved_types_store;
            self.mono_scratches.ident_store = saved_ident_store;
            self.mono_scratches.module_env = saved_module_env;
            self.mono_scratches.module_idx = saved_mono_module_idx;
        }
    }

    const lowered_proc_expr = switch (module_env.store.getExpr(template.cir_expr)) {
        .e_lambda => |lambda| try self.lowerLambdaSpecialized(
            module_env,
            lambda,
            proc_monotype,
            template.source_region,
            proc_inst_id,
        ),
        .e_closure => |closure| try self.lowerClosureSpecialized(
            module_env,
            template.cir_expr,
            closure,
            proc_monotype,
            template.source_region,
            proc_inst_id,
        ),
        .e_hosted_lambda => |hosted| try self.lowerHostedLambdaSpecialized(
            module_env,
            hosted,
            proc_monotype,
            template.source_region,
            proc_inst_id,
        ),
        else => unreachable,
    };

    return lowered_proc_expr;
}

fn lowerDispatchProcInstForExpr(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ExprId {
    const proc_inst_id = self.monomorphization.getDispatchExprProcInst(
        self.current_proc_inst_context,
        self.monomorphizationRootExprContext(self.current_proc_inst_context),
        self.current_module_idx,
        expr_idx,
    ) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "MIR Lower invariant: monomorphization missing dispatch proc inst for expr {d} in module {d}",
                .{ @intFromEnum(expr_idx), self.current_module_idx },
            );
        }
        unreachable;
    };
    return self.lowerProcInst(proc_inst_id);
}

fn lookupMonomorphizedProcInst(
    self: *Self,
    template_id: Monomorphize.ProcTemplateId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!Monomorphize.ProcInstId {
    for (self.monomorphization.proc_insts.items, 0..) |proc_inst, idx| {
        if (proc_inst.template != template_id) continue;
        if (proc_inst.fn_monotype_module_idx != fn_monotype_module_idx) continue;

        const imported_proc_mono = try self.importMonotypeFromStore(
            &self.monomorphization.monotype_store,
            proc_inst.fn_monotype,
            proc_inst.fn_monotype_module_idx,
            fn_monotype_module_idx,
        );
        if (try self.monotypesStructurallyEqual(imported_proc_mono, fn_monotype)) {
            return @enumFromInt(idx);
        }
    }

    if (std.debug.runtime_safety) {
        const template = self.monomorphization.getProcTemplate(template_id);
        std.debug.panic(
            "MIR Lower invariant: monomorphization missing proc inst for template={d} kind={s} template_module={d} template_expr={d} module={d} monotype={d} monotype_repr={any}",
            .{
                @intFromEnum(template_id),
                @tagName(template.kind),
                template.module_idx,
                @intFromEnum(template.cir_expr),
                fn_monotype_module_idx,
                @intFromEnum(fn_monotype),
                self.store.monotype_store.getMonotype(fn_monotype),
            },
        );
    }
    unreachable;
}

fn lowerMonomorphizedExternalProcInst(
    self: *Self,
    target_module_idx: u32,
    def_node_idx: u16,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!MIR.ExprId {
    const template_id = self.monomorphization.getExternalProcTemplate(target_module_idx, def_node_idx) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "MIR Lower invariant: monomorphization missing external proc template for module={d} node={d}",
                .{ target_module_idx, def_node_idx },
            );
        }
        unreachable;
    };
    const proc_inst_id = try self.lookupMonomorphizedProcInst(template_id, fn_monotype, fn_monotype_module_idx);
    return self.lowerProcInst(proc_inst_id);
}

fn procInstReturnMonotype(self: *Self, proc_inst_id: Monomorphize.ProcInstId) Allocator.Error!Monotype.Idx {
    const proc_inst = self.monomorphization.getProcInst(proc_inst_id);
    const imported_fn_mono = try self.importMonotypeFromStore(
        &self.monomorphization.monotype_store,
        proc_inst.fn_monotype,
        proc_inst.fn_monotype_module_idx,
        self.current_module_idx,
    );
    return switch (self.store.monotype_store.getMonotype(imported_fn_mono)) {
        .func => |func| func.ret,
        else => unreachable,
    };
}

fn lowerCall(
    self: *Self,
    module_env: *const ModuleEnv,
    call_expr_idx: CIR.Expr.Idx,
    call: anytype,
    monotype: Monotype.Idx,
    region: Region,
) Allocator.Error!MIR.ExprId {
    const callee_expr = module_env.store.getExpr(call.func);
    const value_flow_lookup_callee = switch (callee_expr) {
        .e_lookup_local => |lookup| !isTopLevelPattern(module_env, lookup.pattern_idx),
        .e_lookup_external => false,
        .e_lookup_required => false,
        else => false,
    };
    const call_site_proc_inst = if (value_flow_lookup_callee)
        null
    else
        self.monomorphization.getCallSiteProcInst(
            self.current_proc_inst_context,
            self.monomorphizationRootExprContext(self.current_proc_inst_context),
            self.current_module_idx,
            call_expr_idx,
        );
    const call_result_monotype = if (call_site_proc_inst) |proc_inst_id|
        try self.procInstReturnMonotype(proc_inst_id)
    else
        monotype;

    if (self.getCallLowLevelOp(module_env, callee_expr)) |ll_op| {
        if (ll_op == .list_append_unsafe) {
            const arg_exprs = module_env.store.sliceExpr(call.args);
            if (arg_exprs.len == 2) {
                const lowered_list = try self.lowerExpr(arg_exprs[0]);
                const list_mono_idx = self.store.typeOf(lowered_list);
                const elem_monotype = switch (self.store.monotype_store.getMonotype(list_mono_idx)) {
                    .list => |list_mono| list_mono.elem,
                    else => Monotype.Idx.none,
                };
                const lowered_elem = if (!elem_monotype.isNone() and self.monotypeIsWellFormed(elem_monotype))
                    try self.lowerExprWithMonotypeOverride(arg_exprs[1], elem_monotype)
                else
                    try self.lowerExpr(arg_exprs[1]);
                const args = try self.store.addExprSpan(self.allocator, &.{ lowered_list, lowered_elem });
                return try self.store.addExpr(self.allocator, .{ .run_low_level = .{
                    .op = ll_op,
                    .args = args,
                } }, call_result_monotype, region);
            }
        }
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
        } }, call_result_monotype, region);
    }

    const lowered_func = if (call_site_proc_inst) |proc_inst_id|
        try self.lowerProcInst(proc_inst_id)
    else
        try self.lowerExpr(call.func);

    return self.lowerCallWithLoweredFunc(
        lowered_func,
        module_env.store.sliceExpr(call.args),
        call_result_monotype,
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
    const lowered_func = try self.stabilizeEscapingFunctionExpr(lowered_func_input);
    const func_mono = self.store.typeOf(lowered_func);
    const call_result_monotype = switch (self.store.monotype_store.getMonotype(func_mono)) {
        .func => |f| f.ret,
        else => monotype,
    };

    const args_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(args_top);
    const expected_arg_monotypes = switch (self.store.monotype_store.getMonotype(func_mono)) {
        .func => |func| self.store.monotype_store.getIdxSpan(func.args),
        else => &.{},
    };
    for (call_arg_exprs, 0..) |arg_idx, i| {
        const expected_mono = if (i < expected_arg_monotypes.len) expected_arg_monotypes[i] else Monotype.Idx.none;
        const use_override = !expected_mono.isNone() and self.monotypeIsWellFormed(expected_mono);
        const lowered_arg = if (use_override)
            try self.lowerExprWithMonotypeOverrideIsolated(arg_idx, expected_mono)
        else
            try self.lowerExpr(arg_idx);
        const arg = try self.stabilizeEscapingFunctionExpr(lowered_arg);
        try self.scratch_expr_ids.append(arg);
    }
    const lowered_call_args = self.scratch_expr_ids.sliceFromStart(args_top);
    const args = try self.store.addExprSpan(self.allocator, lowered_call_args);

    return try self.store.addExpr(self.allocator, .{ .call = .{
        .func = lowered_func,
        .args = args,
    } }, call_result_monotype, region);
}

fn cirExprIsProcBacked(module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx) bool {
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_lambda, .e_closure, .e_hosted_lambda => true,
        .e_block => |block| cirExprIsProcBacked(module_env, block.final_expr),
        .e_dbg => |dbg_expr| cirExprIsProcBacked(module_env, dbg_expr.expr),
        .e_expect => |expect_expr| cirExprIsProcBacked(module_env, expect_expr.body),
        .e_return => |return_expr| cirExprIsProcBacked(module_env, return_expr.expr),
        .e_nominal => |nominal_expr| cirExprIsProcBacked(module_env, nominal_expr.backing_expr),
        .e_nominal_external => |nominal_expr| cirExprIsProcBacked(module_env, nominal_expr.backing_expr),
        else => false,
    };
}

fn getCallLowLevelOp(self: *Self, caller_env: *const ModuleEnv, func_expr: CIR.Expr) ?CIR.Expr.LowLevel {
    return switch (func_expr) {
        .e_lookup_external => |lookup| self.getExternalLowLevelOp(caller_env, lookup),
        .e_lookup_local => |lookup| getLocalLowLevelOp(caller_env, lookup.pattern_idx),
        else => null,
    };
}

fn getLocalLowLevelOp(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Expr.LowLevel {
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
    return null;
}

fn resolveImportedModuleIdx(
    self: *Self,
    caller_env: *const ModuleEnv,
    import_idx: CIR.Import.Idx,
) ?u32 {
    if (caller_env.imports.getResolvedModule(import_idx)) |module_idx| {
        if (module_idx < self.all_module_envs.len) return module_idx;
    }

    const import_pos = @intFromEnum(import_idx);
    if (import_pos >= caller_env.imports.imports.len()) return null;

    const import_name = caller_env.common.getString(caller_env.imports.imports.items.items[import_pos]);
    const base_name = identLastSegment(import_name);

    for (self.all_module_envs, 0..) |candidate_env, module_idx| {
        if (std.mem.eql(u8, candidate_env.module_name, import_name) or
            std.mem.eql(u8, candidate_env.module_name, base_name))
        {
            @constCast(&caller_env.imports).setResolvedModule(import_idx, @intCast(module_idx));
            return @intCast(module_idx);
        }
    }

    return null;
}

/// Check if an external definition is a low-level wrapper (e_lambda wrapping e_run_low_level).
/// Returns the low-level op if found, null otherwise.
fn getExternalLowLevelOp(self: *Self, caller_env: *const ModuleEnv, lookup: anytype) ?CIR.Expr.LowLevel {
    const ext_module_idx = self.resolveImportedModuleIdx(caller_env, lookup.module_idx) orelse return null;
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
        const stmt_region = module_env.store.getStatementRegion(stmt_idx);
        switch (cir_stmt) {
            .s_decl => |decl| {
                if (cirExprIsCallable(module_env.store.getExpr(decl.expr)) and
                    self.monomorphization.getExprProcInst(
                        self.current_proc_inst_context,
                        self.monomorphizationRootExprContext(self.current_proc_inst_context),
                        self.current_module_idx,
                        decl.expr,
                    ) == null)
                {
                    continue;
                }
                const pat = try self.lowerPattern(module_env, decl.pattern);
                const expr = try self.lowerExpr(decl.expr);
                try self.registerBoundSymbolDefIfNeeded(pat, expr);
                try self.scratch_stmts.append(.{ .decl_const = .{ .pattern = pat, .expr = expr } });
            },
            .s_var => |var_decl| {
                if (cirExprIsCallable(module_env.store.getExpr(var_decl.expr)) and
                    self.monomorphization.getExprProcInst(
                        self.current_proc_inst_context,
                        self.monomorphizationRootExprContext(self.current_proc_inst_context),
                        self.current_module_idx,
                        var_decl.expr,
                    ) == null)
                {
                    continue;
                }
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

    const final_expr = try self.lowerExpr(block.final_expr);

    const stmt_span = try self.store.addStmts(self.allocator, self.scratch_stmts.sliceFromStart(stmts_top));

    return try self.store.addExpr(self.allocator, .{ .block = .{
        .stmts = stmt_span,
        .final_expr = final_expr,
    } }, monotype, region);
}

fn cirExprIsCallable(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_lambda, .e_closure, .e_hosted_lambda => true,
        else => false,
    };
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
            const bool_monotype = try self.store.monotype_store.addBoolTagUnion(
                self.allocator,
                self.current_module_idx,
                self.currentCommonIdents(),
            );
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
            const bool_monotype = try self.store.monotype_store.addBoolTagUnion(
                self.allocator,
                self.current_module_idx,
                self.currentCommonIdents(),
            );
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
            const func_expr = try self.lowerDispatchProcInstForExpr(expr_idx);

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
    const inner = try self.lowerExpr(um.expr);

    const func_expr = try self.lowerDispatchProcInstForExpr(expr_idx);
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
    const bool_monotype = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );

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
    const bool_monotype = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );
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

    const field_span = rec.fields;
    const fields = self.store.monotype_store.getFields(field_span);

    // Empty record: always equal
    if (fields.len == 0) return try self.emitMirBoolLiteral(module_env, true, region);

    // Build field comparisons from last to first (innermost result first).
    var result: MIR.ExprId = undefined;
    var i: usize = field_span.len;
    while (i > 0) {
        i -= 1;
        const field = self.store.monotype_store.getFields(field_span)[i];

        const lhs_field = try self.emitMirStructAccess(lhs, @intCast(i), field.type_idx, region);
        const rhs_field = try self.emitMirStructAccess(rhs, @intCast(i), field.type_idx, region);

        const field_eq = try self.lowerFieldEquality(module_env, lhs_field, rhs_field, field.type_idx, ret_monotype, region);

        if (i == field_span.len - 1) {
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

    const elem_span = tup.elems;
    const elems = self.store.monotype_store.getIdxSpan(elem_span);

    // Empty tuple: always equal
    if (elems.len == 0) return try self.emitMirBoolLiteral(module_env, true, region);

    // Build element comparisons from last to first.
    var result: MIR.ExprId = undefined;
    var i: usize = elem_span.len;
    while (i > 0) {
        i -= 1;
        const elem_mono = self.store.monotype_store.getIdxSpan(elem_span)[i];

        const lhs_elem = try self.emitMirStructAccess(lhs, @intCast(i), elem_mono, region);
        const rhs_elem = try self.emitMirStructAccess(rhs, @intCast(i), elem_mono, region);

        const elem_eq = try self.lowerFieldEquality(module_env, lhs_elem, rhs_elem, elem_mono, ret_monotype, region);

        if (i == elem_span.len - 1) {
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

    const tag_span = tu.tags;
    const tags = self.store.monotype_store.getTags(tag_span);

    // Empty tag union: vacuously true
    if (tags.len == 0) return try self.emitMirBoolLiteral(module_env, true, region);

    const save_branches = self.scratch_branches.top();
    defer self.scratch_branches.clearFrom(save_branches);

    for (0..tag_span.len) |tag_i| {
        const current_tags = self.store.monotype_store.getTags(tag_span);
        const tag = current_tags[tag_i];
        const payload_span = tag.payloads;

        // --- LHS pattern: Tag(lhs_p0, lhs_p1, ...) ---
        const save_pats = self.scratch_pattern_ids.top();
        const save_caps = self.scratch_captures.top();
        defer self.scratch_captures.clearFrom(save_caps);

        for (self.store.monotype_store.getIdxSpan(payload_span)) |payload_mono| {
            const bind = try self.makeSyntheticBind(payload_mono, false);
            try self.scratch_pattern_ids.append(bind.pattern);
            try self.scratch_captures.append(.{ .symbol = bind.symbol });
        }
        const lhs_payload_patterns = try self.store.addPatternSpan(
            self.allocator,
            self.scratch_pattern_ids.sliceFromStart(save_pats),
        );
        self.scratch_pattern_ids.clearFrom(save_pats);
        const lhs_tag_args = try self.wrapMultiPayloadTagPatterns(tag.name.ident, tu_monotype, lhs_payload_patterns);

        const lhs_tag_pattern = try self.store.addPattern(self.allocator, .{ .tag = .{
            .name = tag.name.ident,
            .args = lhs_tag_args,
        } }, tu_monotype);
        const lhs_bp = try self.store.addBranchPatterns(self.allocator, &.{.{
            .pattern = lhs_tag_pattern,
            .degenerate = false,
        }});

        // --- RHS pattern: Tag(rhs_p0, rhs_p1, ...) ---
        for (self.store.monotype_store.getIdxSpan(payload_span)) |payload_mono| {
            const bind = try self.makeSyntheticBind(payload_mono, false);
            try self.scratch_pattern_ids.append(bind.pattern);
            try self.scratch_captures.append(.{ .symbol = bind.symbol });
        }
        const rhs_payload_patterns = try self.store.addPatternSpan(
            self.allocator,
            self.scratch_pattern_ids.sliceFromStart(save_pats),
        );
        self.scratch_pattern_ids.clearFrom(save_pats);
        const rhs_tag_args = try self.wrapMultiPayloadTagPatterns(tag.name.ident, tu_monotype, rhs_payload_patterns);

        const rhs_tag_pattern = try self.store.addPattern(self.allocator, .{ .tag = .{
            .name = tag.name.ident,
            .args = rhs_tag_args,
        } }, tu_monotype);
        const rhs_bp = try self.store.addBranchPatterns(self.allocator, &.{.{
            .pattern = rhs_tag_pattern,
            .degenerate = false,
        }});

        // --- Build payload comparison ---
        const all_caps = self.scratch_captures.sliceFromStart(save_caps);
        const payload_eq = if (payload_span.len == 0)
            try self.emitMirBoolLiteral(module_env, true, region)
        else blk: {
            const lhs_caps = all_caps[0..payload_span.len];
            const rhs_caps = all_caps[payload_span.len..][0..payload_span.len];

            // Chain payload comparisons with short-circuit AND (last to first)
            var payload_result: MIR.ExprId = undefined;
            var j: usize = payload_span.len;
            while (j > 0) {
                j -= 1;
                const payload_mono = self.store.monotype_store.getIdxSpan(payload_span)[j];
                const lhs_lookup = try self.emitMirLookup(lhs_caps[j].symbol, payload_mono, region);
                const rhs_lookup = try self.emitMirLookup(rhs_caps[j].symbol, payload_mono, region);
                const field_eq = try self.lowerFieldEquality(module_env, lhs_lookup, rhs_lookup, payload_mono, ret_monotype, region);

                if (j == payload_span.len - 1) {
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
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );
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

fn dotCallUsesRuntimeReceiver(module_env: *const ModuleEnv, receiver_expr_idx: CIR.Expr.Idx) bool {
    return switch (module_env.store.getExpr(receiver_expr_idx)) {
        .e_nominal, .e_nominal_external => false,
        else => true,
    };
}

/// Lower `e_dot_access` — field access, receiver-style method call, or
/// associated-item/static call on a nominal type qualifier.
fn lowerDotAccess(self: *Self, module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, da: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    if (da.args) |args_span| {
        const uses_runtime_receiver = dotCallUsesRuntimeReceiver(module_env, da.receiver);

        if (uses_runtime_receiver and std.mem.eql(u8, module_env.getIdent(da.field_name), "contains")) {
            const explicit_args = module_env.store.sliceExpr(args_span);
            if (explicit_args.len == 1) {
                const receiver_monotype = try self.resolveMonotype(da.receiver);
                if (!receiver_monotype.isNone()) switch (self.store.monotype_store.getMonotype(receiver_monotype)) {
                    .list => |list_mono| {
                        const receiver = try self.lowerExpr(da.receiver);
                        const lowered_arg = if (!list_mono.elem.isNone() and self.monotypeIsWellFormed(list_mono.elem))
                            try self.lowerExprWithMonotypeOverride(explicit_args[0], list_mono.elem)
                        else
                            try self.lowerExpr(explicit_args[0]);
                        const args = try self.store.addExprSpan(self.allocator, &.{ receiver, lowered_arg });
                        return try self.store.addExpr(self.allocator, .{ .run_low_level = .{
                            .op = .list_contains,
                            .args = args,
                        } }, monotype, region);
                    },
                    else => {},
                };
            }
        }

        // Structural types: .is_eq() is decomposed field-by-field in MIR.
        structural_eq: {
            if (!uses_runtime_receiver) break :structural_eq;

            const receiver = try self.lowerExpr(da.receiver);
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
            const rhs = try self.lowerExpr(explicit_args[0]);
            return try self.lowerStructuralEquality(receiver, rhs, rcv_mono_idx, monotype, region);
        }

        const receiver: MIR.ExprId = if (uses_runtime_receiver) try self.lowerExpr(da.receiver) else .none;

        // Build args as either:
        // - [receiver] ++ explicit_args for instance methods
        // - explicit_args only for associated-item/static calls like
        //   `Simple.leaf("hello")`
        const explicit_args = module_env.store.sliceExpr(args_span);
        const func_expr = try self.lowerDispatchProcInstForExpr(expr_idx);
        const func_mono = self.store.typeOf(func_expr);
        const expected_arg_monotypes = switch (self.store.monotype_store.getMonotype(func_mono)) {
            .func => |func| self.store.monotype_store.getIdxSpan(func.args),
            else => {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "MIR Lower invariant: dispatch proc for dot access '{s}' did not lower to function monotype",
                        .{module_env.getIdent(da.field_name)},
                    );
                }
                unreachable;
            },
        };

        const receiver_param_offset: usize = if (uses_runtime_receiver) 1 else 0;

        const args_top = self.scratch_expr_ids.top();
        defer self.scratch_expr_ids.clearFrom(args_top);
        if (uses_runtime_receiver) {
            try self.scratch_expr_ids.append(receiver);
        }
        for (explicit_args, 0..) |arg_idx, i| {
            const param_i = i + receiver_param_offset;
            if (builtin.mode == .Debug and param_i >= expected_arg_monotypes.len) {
                std.debug.panic(
                    "MIR Lower invariant: dispatch proc arg arity mismatch for dot access '{s}' ({d} params, arg index {d})",
                    .{ module_env.getIdent(da.field_name), expected_arg_monotypes.len, param_i },
                );
            }
            const arg_override = if (param_i < expected_arg_monotypes.len and self.monotypeIsWellFormed(expected_arg_monotypes[param_i]))
                expected_arg_monotypes[param_i]
            else
                Monotype.Idx.none;
            const isolate_override = !arg_override.isNone() and try cirExprNeedsCallableOverrideIsolation(module_env, arg_idx);
            const lowered_arg = if (!arg_override.isNone() and isolate_override)
                try self.lowerExprWithMonotypeOverrideIsolated(arg_idx, arg_override)
            else if (!arg_override.isNone())
                try self.lowerExprWithMonotypeOverride(arg_idx, arg_override)
            else
                try self.lowerExpr(arg_idx);
            const arg = try self.stabilizeEscapingFunctionExpr(lowered_arg);
            try self.scratch_expr_ids.append(arg);
        }
        const lowered_call_args = self.scratch_expr_ids.sliceFromStart(args_top);

        const call_result_monotype = monotype;

        const args = try self.store.addExprSpan(self.allocator, lowered_call_args);

        return try self.store.addExpr(self.allocator, .{ .call = .{
            .func = func_expr,
            .args = args,
        } }, call_result_monotype, region);
    } else {
        // Field access
        const receiver = try self.lowerExpr(da.receiver);
        const receiver_monotype = self.store.typeOf(receiver);
        const receiver_record = switch (self.store.monotype_store.getMonotype(receiver_monotype)) {
            .record => |record| record,
            else => typeBindingInvariant(
                "lowerDotAccess: field access receiver is not a record monotype (field='{s}', monotype='{s}')",
                .{ module_env.getIdent(da.field_name), @tagName(self.store.monotype_store.getMonotype(receiver_monotype)) },
            ),
        };
        const field_idx = self.recordFieldIndexByName(
            da.field_name,
            self.store.monotype_store.getFields(receiver_record.fields),
        );
        return try self.emitMirStructAccess(receiver, field_idx, monotype, region);
    }
}

/// Lower a CIR record expression.
fn lowerRecord(self: *Self, module_env: *const ModuleEnv, record: anytype, monotype: Monotype.Idx, region: Region) Allocator.Error!MIR.ExprId {
    const cir_field_indices = module_env.store.sliceRecordFields(record.fields);
    const mono_record = switch (self.store.monotype_store.getMonotype(monotype)) {
        .record => |mono_record| mono_record,
        else => typeBindingInvariant(
            "lowerRecord: expected record monotype, found '{s}'",
            .{@tagName(self.store.monotype_store.getMonotype(monotype))},
        ),
    };
    const mono_field_span = mono_record.fields;

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
        const expr = try self.stabilizeEscapingFunctionExpr(try self.lowerExpr(field.value));
        try provided_fields.append(self.allocator, .{
            .name = field.name,
            .expr = expr,
        });
    }

    const exprs_top = self.scratch_expr_ids.top();
    defer self.scratch_expr_ids.clearFrom(exprs_top);

    const canonical_field_exprs = try self.allocator.alloc(?MIR.ExprId, mono_field_span.len);
    defer self.allocator.free(canonical_field_exprs);
    @memset(canonical_field_exprs, null);

    const mono_fields = self.store.monotype_store.getFields(mono_field_span);
    for (provided_fields.items) |provided| {
        const field_idx = self.recordFieldIndexByName(provided.name, mono_fields);
        canonical_field_exprs[@intCast(field_idx)] = provided.expr;
    }

    if (record.ext) |ext_expr_idx| {
        const ext_expr = try self.lowerExpr(ext_expr_idx);
        const ext_expr_mono = self.store.typeOf(ext_expr);

        // Bind the update base once so:
        // 1) `{ ..expr, all_fields_overridden }` still evaluates `expr`, and
        // 2) synthesized field accesses never re-evaluate `expr`.
        const ext_symbol = try self.internSymbol(self.current_module_idx, self.makeSyntheticIdent(Ident.Idx.NONE));
        const ext_pattern = try self.store.addPattern(self.allocator, .{ .bind = ext_symbol }, ext_expr_mono);
        const ext_lookup = try self.store.addExpr(self.allocator, .{ .lookup = ext_symbol }, ext_expr_mono, region);
        extension_binding = .{
            .pattern = ext_pattern,
            .expr = ext_expr,
            .lookup = ext_lookup,
        };

        // Record update: include all fields in the resulting record.
        // Updated fields use explicit expressions; missing fields become accesses on the
        // base record expression from `..record`.
        const updated_mono_fields = self.store.monotype_store.getFields(mono_field_span);
        for (updated_mono_fields, 0..) |mono_field, field_idx| {
            const field_expr = canonical_field_exprs[field_idx] orelse try self.emitMirStructAccess(
                extension_binding.?.lookup,
                @intCast(field_idx),
                mono_field.type_idx,
                region,
            );

            try self.scratch_expr_ids.append(field_expr);
        }
    } else {
        for (canonical_field_exprs, 0..) |maybe_field_expr, field_idx| {
            const field_expr = maybe_field_expr orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "CIR→MIR invariant violated: record literal missing canonical field {d}",
                        .{field_idx},
                    );
                }
                unreachable;
            };
            try self.scratch_expr_ids.append(field_expr);
        }
    }

    const fields_span = try self.store.addExprSpan(self.allocator, self.scratch_expr_ids.sliceFromStart(exprs_top));

    const record_expr = try self.emitMirStructExprFromSpan(fields_span, monotype, region);

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
    const args = try self.lowerExprSpan(module_env, tvd.args);
    const func_expr = try self.lowerDispatchProcInstForExpr(expr_idx);

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

fn findModuleForOriginMaybe(self: *Self, source_env: *const ModuleEnv, origin_module: Ident.Idx) ?u32 {
    const source_module_idx = self.moduleIndexForEnv(source_env) orelse return null;
    if (origin_module.eql(source_env.qualified_module_ident)) return source_module_idx;

    if (!moduleOwnsIdent(source_env, origin_module)) return null;
    const origin_name = getOwnedIdentText(source_env, origin_module);
    for (self.all_module_envs, 0..) |candidate_env, idx| {
        const candidate_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
        if (std.mem.eql(u8, origin_name, candidate_name)) return @intCast(idx);
    }
    return null;
}

fn lookupResolvedDispatchTarget(self: *const Self, expr_idx: CIR.Expr.Idx) ?ResolvedDispatchTarget {
    const key = (@as(u64, self.current_module_idx) << 32) | @as(u64, @intFromEnum(expr_idx));
    return self.resolved_dispatch_targets.get(key);
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
    const saved_mono_module_idx = self.mono_scratches.module_idx;
    self.mono_scratches.ident_store = self.all_module_envs[module_idx].getIdentStoreConst();
    self.mono_scratches.module_env = self.all_module_envs[module_idx];
    self.mono_scratches.module_idx = module_idx;
    defer {
        self.mono_scratches.ident_store = saved_ident_store;
        self.mono_scratches.module_env = saved_module_env;
        self.mono_scratches.module_idx = saved_mono_module_idx;
    }

    return self.monotypeFromTypeVarWithBindings(
        module_idx,
        store_types,
        var_,
        &local_seen,
        &local_cycles,
    );
}

/// Lower an external definition by symbol, caching the result.
pub fn lowerExternalDef(self: *Self, symbol: MIR.Symbol, cir_expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ExprId {
    const saved_root_expr_context = self.current_root_expr_context;
    if (self.current_proc_inst_context.isNone() and self.current_root_expr_context == null) {
        self.current_root_expr_context = cir_expr_idx;
    }
    defer self.current_root_expr_context = saved_root_expr_context;
    return self.lowerExternalDefWithType(symbol, cir_expr_idx);
}

/// Lower an external definition by its own declared identity.
fn lowerExternalDefWithType(self: *Self, symbol: MIR.Symbol, cir_expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ExprId {
    const symbol_key: u64 = @bitCast(symbol);
    const symbol_meta = self.getSymbolMetadata(symbol);
    const symbol_module_idx = symbolMetadataModuleIdx(symbol_meta);
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
            if (self.in_progress_symbol_monotypes.get(symbol_key)) |active| {
                if (!active.isNone()) break :blk active;
            }

            const derived = try self.monotypeFromTypeVarWithBindings(
                self.current_module_idx,
                self.types_store,
                target_type_var,
                &self.type_var_seen,
                &self.nominal_cycle_breakers,
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
    try self.in_progress_symbol_monotypes.put(symbol_key, Monotype.Idx.none);
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
    const saved_mono_module_idx = self.mono_scratches.module_idx;
    // Lower external defs in canonical module scope. A def-lowering context is
    // not a lexical scope, and reusing current_pattern_scope here causes local
    // sibling bindings to be resolved to synthetic per-def symbols instead of
    // their canonical block-local/module-local identities.
    self.current_pattern_scope = 0;
    if (switching_module) {
        self.current_module_idx = symbol_module_idx;
        self.types_store = &self.all_module_envs[symbol_module_idx].types;
        self.mono_scratches.ident_store = self.all_module_envs[symbol_module_idx].getIdentStoreConst();
        self.mono_scratches.module_env = self.all_module_envs[symbol_module_idx];
        self.mono_scratches.module_idx = symbol_module_idx;
    }

    // Always isolate type_var_seen per external definition lowering.
    // Reusing a shared cache across polymorphic specializations can pin flex
    // and rigid vars to an earlier specialization.
    self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);

    try self.seedTypeScopeBindingsInStore(
        self.current_module_idx,
        self.types_store,
        &self.type_var_seen,
    );

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
            self.mono_scratches.module_idx = saved_mono_module_idx;
        }
    }

    const current_env = self.all_module_envs[self.current_module_idx];
    const result = if (cirExprIsProcBacked(current_env, cir_expr_idx)) blk: {
        const template_id = self.monomorphization.getExprProcTemplate(self.current_module_idx, cir_expr_idx) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "MIR Lower invariant: callable def symbol={d} expr={d} in module {d} has no proc template",
                    .{ symbol.raw(), @intFromEnum(cir_expr_idx), self.current_module_idx },
                );
            }
            unreachable;
        };
        const rooted_proc_inst_id = self.monomorphization.getExprProcInst(
            self.current_proc_inst_context,
            self.monomorphizationRootExprContext(self.current_proc_inst_context),
            self.current_module_idx,
            cir_expr_idx,
        );
        const proc_inst_id = rooted_proc_inst_id orelse lookup_proc_inst: {
            if (self.current_proc_inst_context.isNone()) {
                if (self.monomorphization.getExprProcInst(.none, null, self.current_module_idx, cir_expr_idx)) |canonical_proc_inst_id| {
                    break :lookup_proc_inst canonical_proc_inst_id;
                }
            }
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "MIR Lower invariant: callable def symbol={d} expr={d} in module {d} template={d} has no monomorphized proc inst in context {d} root_expr={d}",
                    .{
                        symbol.raw(),
                        @intFromEnum(cir_expr_idx),
                        self.current_module_idx,
                        @intFromEnum(template_id),
                        @intFromEnum(self.current_proc_inst_context),
                        if (self.current_root_expr_context) |root_expr_idx| @intFromEnum(root_expr_idx) else std.math.maxInt(u32),
                    },
                );
            }
            unreachable;
        };
        break :blk try self.lowerProcInst(proc_inst_id);
    } else try self.lowerExpr(cir_expr_idx);

    // Cache the result and register the symbol definition
    try self.lowered_symbols.put(symbol_key, result);
    try self.store.registerValueDef(self.allocator, symbol, result);

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
    const field_idx = self.recordFieldIndexByName(field_name, mono_fields);
    try self.bindTypeVarMonotypes(field_var, mono_fields[@intCast(field_idx)].type_idx);
}

fn recordFieldIndexByName(
    self: *Self,
    field_name: Ident.Idx,
    mono_fields: []const Monotype.Field,
) u32 {
    for (mono_fields, 0..) |mono_field, field_idx| {
        if (self.identsStructurallyEqual(mono_field.name, field_name)) {
            return @intCast(field_idx);
        }
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    typeBindingInvariant(
        "record field '{s}' missing from monotype",
        .{module_env.getIdent(field_name)},
    );
}

fn tagIndexByName(
    self: *Self,
    tag_name: Ident.Idx,
    mono_tags: []const Monotype.Tag,
) u32 {
    for (mono_tags, 0..) |mono_tag, tag_idx| {
        if (self.identsTagNameEquivalent(mono_tag.name, tag_name)) {
            return @intCast(tag_idx);
        }
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    typeBindingInvariant(
        "tag '{s}' missing from monotype",
        .{module_env.getIdent(tag_name)},
    );
}

fn seenIndex(seen_indices: []const u32, idx: u32) bool {
    for (seen_indices) |seen_idx| {
        if (seen_idx == idx) return true;
    }
    return false;
}

fn appendSeenIndex(
    allocator: Allocator,
    seen_indices: *std.ArrayListUnmanaged(u32),
    idx: u32,
) Allocator.Error!void {
    if (seenIndex(seen_indices.items, idx)) return;
    try seen_indices.append(allocator, idx);
}

fn remainingRecordTailMonotype(
    self: *Self,
    mono_fields: []const Monotype.Field,
    seen_indices: []const u32,
) Allocator.Error!Monotype.Idx {
    var remaining_fields: std.ArrayListUnmanaged(Monotype.Field) = .empty;
    defer remaining_fields.deinit(self.allocator);

    for (mono_fields, 0..) |field, field_idx| {
        if (seenIndex(seen_indices, @intCast(field_idx))) continue;
        try remaining_fields.append(self.allocator, field);
    }

    if (remaining_fields.items.len == 0) {
        return self.store.monotype_store.unit_idx;
    }

    const field_span = try self.store.monotype_store.addFields(self.allocator, remaining_fields.items);
    return try self.store.monotype_store.addMonotype(self.allocator, .{ .record = .{ .fields = field_span } });
}

fn remainingTagUnionTailMonotype(
    self: *Self,
    mono_tags: []const Monotype.Tag,
    seen_indices: []const u32,
) Allocator.Error!Monotype.Idx {
    var remaining_tags: std.ArrayListUnmanaged(Monotype.Tag) = .empty;
    defer remaining_tags.deinit(self.allocator);

    for (mono_tags, 0..) |tag, tag_idx| {
        if (seenIndex(seen_indices, @intCast(tag_idx))) continue;
        try remaining_tags.append(self.allocator, tag);
    }

    const tag_span = try self.store.monotype_store.addTags(self.allocator, remaining_tags.items);
    return try self.store.monotype_store.addMonotype(self.allocator, .{ .tag_union = .{ .tags = tag_span } });
}

fn bindRecordRowTailInStore(
    self: *Self,
    store_types: *const types.Store,
    common_idents: ModuleEnv.CommonIdents,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    ext_var: types.Var,
    mono_fields: []const Monotype.Field,
    seen_indices: []const u32,
) Allocator.Error!void {
    const tail_mono = try self.remainingRecordTailMonotype(mono_fields, seen_indices);
    try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, ext_var, tail_mono);
}

fn bindRecordRowTail(
    self: *Self,
    ext_var: types.Var,
    mono_fields: []const Monotype.Field,
    seen_indices: []const u32,
) Allocator.Error!void {
    const tail_mono = try self.remainingRecordTailMonotype(mono_fields, seen_indices);
    try self.bindTypeVarMonotypes(ext_var, tail_mono);
}

fn bindTagUnionRowTailInStore(
    self: *Self,
    store_types: *const types.Store,
    common_idents: ModuleEnv.CommonIdents,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    ext_var: types.Var,
    mono_tags: []const Monotype.Tag,
    seen_indices: []const u32,
) Allocator.Error!void {
    const tail_mono = try self.remainingTagUnionTailMonotype(mono_tags, seen_indices);
    try self.bindTypeVarMonotypesInStore(store_types, common_idents, bindings, ext_var, tail_mono);
}

fn bindTagUnionRowTail(
    self: *Self,
    ext_var: types.Var,
    mono_tags: []const Monotype.Tag,
    seen_indices: []const u32,
) Allocator.Error!void {
    const tail_mono = try self.remainingTagUnionTailMonotype(mono_tags, seen_indices);
    try self.bindTypeVarMonotypes(ext_var, tail_mono);
}

fn tupleMonotypeForFields(self: *Self, field_monotypes: []const Monotype.Idx) Allocator.Error!Monotype.Idx {
    const elems = try self.store.monotype_store.addIdxSpan(self.allocator, field_monotypes);
    return try self.store.monotype_store.addMonotype(self.allocator, .{ .tuple = .{ .elems = elems } });
}

fn tagPayloadMonotypesByName(
    self: *Self,
    union_monotype: Monotype.Idx,
    tag_name: Ident.Idx,
) []const Monotype.Idx {
    const mono = self.store.monotype_store.getMonotype(union_monotype);
    const tags = switch (mono) {
        .tag_union => |tu| self.store.monotype_store.getTags(tu.tags),
        else => typeBindingInvariant(
            "tag payload lookup expected tag_union monotype, found '{s}'",
            .{@tagName(mono)},
        ),
    };

    for (tags) |tag| {
        if (self.identsTagNameEquivalent(tag.name, tag_name)) {
            return self.store.monotype_store.getIdxSpan(tag.payloads);
        }
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    typeBindingInvariant(
        "tag '{s}' missing from monotype",
        .{module_env.getIdent(tag_name)},
    );
}

fn wrapMultiPayloadTagExprs(
    self: *Self,
    tag_name: Ident.Idx,
    union_monotype: Monotype.Idx,
    args: MIR.ExprSpan,
    region: Region,
) Allocator.Error!MIR.ExprSpan {
    const arg_exprs = self.store.getExprSpan(args);
    if (arg_exprs.len <= 1) return args;

    const payload_monotypes = self.tagPayloadMonotypesByName(union_monotype, tag_name);
    if (payload_monotypes.len != arg_exprs.len) {
        const module_env = self.all_module_envs[self.current_module_idx];
        typeBindingInvariant(
            "tag '{s}' payload arity mismatch while wrapping MIR struct (args={d}, mono={d})",
            .{ module_env.getIdent(tag_name), arg_exprs.len, payload_monotypes.len },
        );
    }

    const payload_struct_mono = try self.tupleMonotypeForFields(payload_monotypes);
    const payload_struct_expr = try self.emitMirStructExprFromSpan(args, payload_struct_mono, region);
    return try self.store.addExprSpan(self.allocator, &.{payload_struct_expr});
}

fn wrapMultiPayloadTagPatterns(
    self: *Self,
    tag_name: Ident.Idx,
    union_monotype: Monotype.Idx,
    args: MIR.PatternSpan,
) Allocator.Error!MIR.PatternSpan {
    const arg_patterns = self.store.getPatternSpan(args);
    if (arg_patterns.len <= 1) return args;

    const payload_monotypes = self.tagPayloadMonotypesByName(union_monotype, tag_name);
    if (payload_monotypes.len != arg_patterns.len) {
        const module_env = self.all_module_envs[self.current_module_idx];
        typeBindingInvariant(
            "tag '{s}' payload arity mismatch while wrapping MIR struct pattern (args={d}, mono={d})",
            .{ module_env.getIdent(tag_name), arg_patterns.len, payload_monotypes.len },
        );
    }

    const payload_struct_mono = try self.tupleMonotypeForFields(payload_monotypes);
    const payload_struct_pattern = try self.store.addPattern(self.allocator, .{
        .struct_destructure = .{ .fields = args },
    }, payload_struct_mono);
    return try self.store.addPatternSpan(self.allocator, &.{payload_struct_pattern});
}

fn bindTagPayloadsByName(
    self: *Self,
    tag_name: Ident.Idx,
    payload_vars: []const types.Var,
    mono_tags: []const Monotype.Tag,
) Allocator.Error!void {
    for (mono_tags) |mono_tag| {
        if (!self.identsTagNameEquivalent(mono_tag.name, tag_name)) continue;

        const mono_payload_span = mono_tag.payloads;
        if (payload_vars.len != mono_payload_span.len) {
            const module_env = self.all_module_envs[self.current_module_idx];
            typeBindingInvariant(
                "bindFlatTypeMonotypes(tag_union): payload arity mismatch for tag '{s}'",
                .{module_env.getIdent(tag_name)},
            );
        }
        for (payload_vars, 0..) |payload_var, i| {
            const mono_payload = self.store.monotype_store.getIdxSpan(mono_payload_span)[i];
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
            const mono_arg_span = mfunc.args;
            if (type_args.len != mono_arg_span.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(fn): arity mismatch (type={d}, monotype={d})",
                    .{ type_args.len, mono_arg_span.len },
                );
            }
            for (type_args, 0..) |ta, i| {
                const ma = self.store.monotype_store.getIdxSpan(mono_arg_span)[i];
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
            const mono_field_span = mrec.fields;
            const mono_fields = self.store.monotype_store.getFields(mono_field_span);
            var seen_field_indices: std.ArrayListUnmanaged(u32) = .empty;
            defer seen_field_indices.deinit(self.allocator);

            var current_row = record;
            rows: while (true) {
                const fields_slice = self.types_store.getRecordFieldsSlice(current_row.fields);
                const field_names = fields_slice.items(.name);
                const field_vars = fields_slice.items(.var_);

                for (field_names, field_vars) |field_name, field_var| {
                    const field_idx = self.recordFieldIndexByName(field_name, mono_fields);
                    try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                    try self.bindTypeVarMonotypes(field_var, mono_fields[field_idx].type_idx);
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
                                    const field_idx = self.recordFieldIndexByName(field_name, mono_fields);
                                    try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                                    try self.bindTypeVarMonotypes(field_var, mono_fields[field_idx].type_idx);
                                }
                                break :rows;
                            },
                            .empty_record => break :rows,
                            else => typeBindingInvariant(
                                "bindFlatTypeMonotypes(record): unexpected ext flat type '{s}'",
                                .{@tagName(ext_flat)},
                            ),
                        },
                        .flex, .rigid => {
                            try self.bindRecordRowTail(ext_var, mono_fields, seen_field_indices.items);
                            for (mono_fields, 0..) |_, field_idx| {
                                try appendSeenIndex(self.allocator, &seen_field_indices, @intCast(field_idx));
                            }
                            break :rows;
                        },
                        .err => typeBindingInvariant(
                            "bindFlatTypeMonotypes(record): error extension",
                            .{},
                        ),
                    }
                }
            }

            for (mono_fields, 0..) |mono_field, field_idx| {
                if (!seenIndex(seen_field_indices.items, @intCast(field_idx))) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(record): monotype field '{s}' missing from type row",
                        .{mono_field.name.text(self.all_module_envs)},
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
            const mono_field_span = mrec.fields;

            if (field_names.len != mono_field_span.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(record_unbound): field count mismatch (type={d}, monotype={d})",
                    .{ field_names.len, mono_field_span.len },
                );
            }

            for (field_names, field_vars) |field_name, field_var| {
                try self.bindRecordFieldByName(field_name, field_var, self.store.monotype_store.getFields(mono_field_span));
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
            const mono_elem_span = mtuple.elems;
            if (elem_vars.len != mono_elem_span.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(tuple): arity mismatch (type={d}, monotype={d})",
                    .{ elem_vars.len, mono_elem_span.len },
                );
            }
            for (elem_vars, 0..) |ev, i| {
                const me = self.store.monotype_store.getIdxSpan(mono_elem_span)[i];
                try self.bindTypeVarMonotypes(ev, me);
            }
        },
        .tag_union => |tag_union_row| {
            const mono_tag_span = switch (mono) {
                .tag_union => |mtu| mtu.tags,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(tag_union): expected tag_union monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const mono_tags = self.store.monotype_store.getTags(mono_tag_span);
            var seen_tag_indices: std.ArrayListUnmanaged(u32) = .empty;
            defer seen_tag_indices.deinit(self.allocator);

            var current_row = tag_union_row;
            rows: while (true) {
                const type_tags = self.types_store.getTagsSlice(current_row.tags);
                const type_tag_names = type_tags.items(.name);
                const type_tag_args = type_tags.items(.args);

                for (type_tag_names, type_tag_args) |tag_name, tag_args| {
                    const tag_idx = self.tagIndexByName(tag_name, mono_tags);
                    try appendSeenIndex(self.allocator, &seen_tag_indices, tag_idx);
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
                        .flex, .rigid => {
                            try self.bindTagUnionRowTail(ext_var, mono_tags, seen_tag_indices.items);
                            for (mono_tags, 0..) |_, tag_idx| {
                                try appendSeenIndex(self.allocator, &seen_tag_indices, @intCast(tag_idx));
                            }
                            break :rows;
                        },
                        .err => typeBindingInvariant(
                            "bindFlatTypeMonotypes(tag_union): error extension",
                            .{},
                        ),
                    }
                }
            }

            for (mono_tags, 0..) |mono_tag, tag_idx| {
                if (!seenIndex(seen_tag_indices.items, @intCast(tag_idx))) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(tag_union): monotype tag '{s}' missing from type row",
                        .{mono_tag.name.text(self.all_module_envs)},
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

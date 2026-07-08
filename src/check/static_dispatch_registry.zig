//! Checked static-dispatch target registry and normalized dispatch-site records.
//!
//! The registry is built at checked-module publication. Post-check lowering uses
//! it as a target table only; the dispatch-site record chooses the dispatcher
//! type variable explicitly.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const canonical = @import("canonical_names.zig");
const checked_ids = @import("checked_ids.zig");
const collections = @import("collections");
const artifact_serialize = @import("artifact_serialize.zig");
const dispatch_evidence = @import("dispatch_evidence.zig");
const SerializedSlice = artifact_serialize.SerializedSlice;
const CompactWriter = collections.CompactWriter;

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
const CheckedTypeId = checked_ids.CheckedTypeId;
const CheckedExprId = checked_ids.CheckedExprId;
const CheckedStringLiteralId = checked_ids.CheckedStringLiteralId;
const PatternBinderId = checked_ids.PatternBinderId;

fn typeDispatchOwnerVar(module: TypedCIR.Module, stmt_idx: CIR.Statement.Idx) Var {
    return switch (module.getStatement(stmt_idx)) {
        .s_type_var_alias => |alias| ModuleEnv.varFrom(alias.type_var_anno),
        .s_alias_decl => ModuleEnv.varFrom(stmt_idx),
        else => @panic("type dispatch owner statement was not a type-var alias or type alias"),
    };
}

/// Public `ProcedureTemplateLookup` declaration.
pub const ProcedureTemplateLookup = struct {
    module_idx: u32,
    by_def: []const ProcedureTemplateLookupEntry = &.{},

    pub fn templateForDef(self: *const ProcedureTemplateLookup, def_idx: CIR.Def.Idx) ?canonical.ProcedureTemplateRef {
        const found = artifact_serialize.binarySearchByKey(ProcedureTemplateLookupEntry, CIR.Def.Idx, self.by_def, def_idx, templateEntryOrder) orelse return null;
        return found.template;
    }
};

fn templateEntryOrder(e: ProcedureTemplateLookupEntry, key: CIR.Def.Idx) std.math.Order {
    return std.math.order(@intFromEnum(e.def), @intFromEnum(key));
}

/// Public `ProcedureTemplateLookupEntry` declaration.
pub const ProcedureTemplateLookupEntry = struct {
    def: CIR.Def.Idx,
    template: canonical.ProcedureTemplateRef,

    pub fn lessThan(_: void, lhs: ProcedureTemplateLookupEntry, rhs: ProcedureTemplateLookupEntry) bool {
        return @intFromEnum(lhs.def) < @intFromEnum(rhs.def);
    }
};

/// Public `MethodOwner` declaration.
///
/// A method owner is identified by CONTENT: the declaring module's deep
/// content identity plus the declared type name (see `base.module_identity`
/// and `canonical.NominalTypeKey`). Statement indices and module name text
/// never participate. Compiler-builtin owners keep their dedicated enum so
/// builtin dispatch stays exact across differently-spelled builtin idents.
pub const MethodOwner = union(enum) {
    nominal: canonical.NominalTypeKey,
    builtin: BuiltinOwner,
};

/// Public `BuiltinOwner` declaration.
pub const BuiltinOwner = enum(u8) {
    list,
    box,
    dict,
    set,
    fields,
    field,
    bool,
    str,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
    parse_tag_union_spec,
    crypto_sha256_digest,
    crypto_sha256_hasher,
    crypto_blake3_digest,
    crypto_blake3_hasher,
};

/// Public `MethodKey` declaration.
pub const MethodKey = struct {
    owner: MethodOwner,
    method: canonical.MethodNameId,
};

/// Public `ProcedureMethodTarget` declaration.
pub const ProcedureMethodTarget = struct {
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
};

/// Public `LocalProcedureMethodTarget` declaration.
pub const LocalProcedureMethodTarget = struct {
    binder: PatternBinderId,
    expr: CheckedExprId,
};

/// Public `MethodTargetKind` declaration.
pub const MethodTargetKind = union(enum) {
    procedure: ProcedureMethodTarget,
    local_proc: LocalProcedureMethodTarget,
    generated_structural_parser,
    generated_structural_encoder,
};

/// Public `MethodTarget` declaration.
pub const MethodTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    kind: MethodTargetKind,
    callable_ty: CheckedTypeId,
};

/// Public `MethodRegistryEntry` declaration.
pub const MethodRegistryEntry = struct {
    key: MethodKey,
    target: MethodTarget,
};

/// Public `MethodRegistry` declaration.
pub const MethodRegistry = struct {
    entries: []MethodRegistryEntry = &.{},

    pub const Serialized = extern struct {
        entries: SerializedSlice(MethodRegistryEntry) = .{},
        pub fn serialize(self: *Serialized, t: *const MethodRegistry, gpa: Allocator, writer: *CompactWriter) Allocator.Error!void {
            try self.entries.serialize(t.entries, gpa, writer);
        }
        pub fn deserialize(self: *const Serialized, base_addr: usize) MethodRegistry {
            return .{ .entries = self.entries.deserialize(base_addr) };
        }
    };

    pub fn lookup(self: *const MethodRegistry, key: MethodKey) ?MethodTarget {
        // Stack-built keys carry undefined bytes in the owner union's padding
        // and inactive-variant region; ReleaseFast fuses the comparator's
        // field reads into wide loads that touch them. Zero those bytes so
        // every load is defined (entries are zeroed at build/serialization).
        var normalized = key;
        collections.CompactWriter.zeroValuePadding(MethodKey, @ptrCast(&normalized));
        const found = artifact_serialize.binarySearchByKey(MethodRegistryEntry, MethodKey, self.entries, normalized, methodEntryOrder) orelse return null;
        return found.target;
    }

    /// Build-time-only teardown (see `StaticDispatchPlanTable.deinit`): a frozen
    /// table's `entries` alias the artifact buffer and are freed wholesale by the
    /// artifact, never here.
    pub fn deinit(self: *MethodRegistry, allocator: Allocator) void {
        allocator.free(self.entries);
        self.* = .{};
    }

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        local_templates: *const ProcedureTemplateLookup,
        available_artifacts: anytype,
        checked_types: anytype,
        checked_bodies: anytype,
    ) Allocator.Error!MethodRegistry {
        var entries = std.ArrayList(MethodRegistryEntry).empty;
        errdefer entries.deinit(allocator);

        const module_idx = module.moduleIndex();
        if (module_idx != local_templates.module_idx) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic(
                    "checked static dispatch registry invariant violated: template lookup module {d} does not match module {d}",
                    .{ local_templates.module_idx, module_idx },
                );
            }
            unreachable;
        }

        const module_env = module.moduleEnvConst();
        const idents = module.identStoreConst();
        const module_name = try names.internModuleIdent(idents, module.qualifiedModuleIdent());

        for (module.methodDefEntries()) |entry| {
            const method_ident = module_env.lookupMethodIdentForMethodOwnerConst(entry.key.ownerIdent(), entry.key.methodIdent()) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch registry invariant violated: method def for owner {d} method {d} has no method ident",
                        .{ @intFromEnum(entry.key.owner), entry.key.method_ident_bits },
                    );
                }
                unreachable;
            };
            const def_idx = entry.value.def_idx;
            var referenced_callable_var: ?Var = null;
            const target_kind: MethodTargetKind = if (generatedStructuralTargetForMethodBinding(module, entry.value, entry.key.methodIdent())) |generated|
                generated
            else if (local_templates.templateForDef(def_idx)) |template| blk: {
                const export_name = try names.internExportIdent(idents, method_ident);
                const proc_base = try names.internProcBase(.{
                    .module_name = module_name,
                    .export_name = export_name,
                    .kind = .checked_source,
                    .ordinal = @intFromEnum(def_idx),
                    .source_def_idx = @intFromEnum(def_idx),
                });
                break :blk .{ .procedure = .{
                    .proc = .{ .artifact = template.artifact, .proc_base = proc_base },
                    .template = template,
                } };
            } else if (localProcedureTargetForMethodBinding(module, checked_bodies, entry.value)) |local|
                .{ .local_proc = local }
            else if (referencedProcedureTargetForMethodBinding(module, local_templates, checked_bodies, entry.value)) |referenced| blk: {
                referenced_callable_var = referenced.callable_var;
                break :blk referenced.kind;
            } else
                // Associated values that do not resolve to a procedure are
                // checked field access, not static-dispatch call targets. The
                // method registry is a procedure-target table for Monotype
                // static dispatch lowering, so only procedure-backed entries
                // belong here.
                continue;
            const callable_var = referenced_callable_var orelse methodTargetCallableVar(module, def_idx, entry.value, target_kind);

            try entries.append(allocator, .{
                .key = .{
                    .owner = try methodOwnerForRegistryEntry(module, names, available_artifacts, entry.key.ownerIdent()),
                    .method = try names.internMethodIdent(idents, entry.key.methodIdent()),
                },
                .target = .{
                    .module_idx = module_idx,
                    .def_idx = def_idx,
                    .kind = target_kind,
                    .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, callable_var),
                },
            });
        }

        finalizeMethodRegistryEntries(entries.items);

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }
};

fn methodTargetCallableVar(
    module: TypedCIR.Module,
    def_idx: CIR.Def.Idx,
    binding: ModuleEnv.MethodBinding,
    target_kind: MethodTargetKind,
) Var {
    return switch (target_kind) {
        .procedure => module.defType(def_idx),
        .generated_structural_parser,
        .generated_structural_encoder,
        => ModuleEnv.varFrom(binding.type_node_idx),
        .local_proc => blk: {
            const raw_node = @intFromEnum(binding.type_node_idx);
            const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
            const decl = switch (module.getStatement(statement)) {
                .s_decl => |decl| decl,
                else => unreachable,
            };
            break :blk module.exprType(decl.expr);
        },
    };
}

fn generatedStructuralTargetForMethodBinding(
    module: TypedCIR.Module,
    binding: ModuleEnv.MethodBinding,
    method_ident: Ident.Idx,
) ?MethodTargetKind {
    const expr_idx = methodBindingExpr(module, binding) orelse return null;
    switch (module.expr(expr_idx).data) {
        .e_anno_only,
        .e_hosted_lambda,
        => {},
        else => return null,
    }
    const annotation_idx = methodBindingAnnotation(module, binding) orelse return null;
    if (module.moduleEnvConst().store.getTypeAnno(module.moduleEnvConst().store.getAnnotation(annotation_idx).anno) != .underscore) return null;

    const common = module.commonIdents();
    if (method_ident.eql(common.parser_for)) return .generated_structural_parser;
    if (method_ident.eql(common.encoder_for)) return .generated_structural_encoder;
    return null;
}

fn methodBindingAnnotation(
    module: TypedCIR.Module,
    binding: ModuleEnv.MethodBinding,
) ?CIR.Annotation.Idx {
    const raw_node = @intFromEnum(binding.type_node_idx);
    if (raw_node >= module.nodeCount()) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: method binding node {d} is outside the module node store",
                .{raw_node},
            );
        }
        unreachable;
    }

    return switch (module.nodeTag(binding.type_node_idx)) {
        .def => module.moduleEnvConst().store.getDef(binding.def_idx).annotation,
        .statement_decl => blk: {
            const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
            const decl = switch (module.getStatement(statement)) {
                .s_decl => |decl| decl,
                else => return null,
            };
            break :blk decl.anno;
        },
        else => null,
    };
}

fn methodBindingExpr(
    module: TypedCIR.Module,
    binding: ModuleEnv.MethodBinding,
) ?CIR.Expr.Idx {
    const raw_node = @intFromEnum(binding.type_node_idx);
    if (raw_node >= module.nodeCount()) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: method binding node {d} is outside the module node store",
                .{raw_node},
            );
        }
        unreachable;
    }

    return switch (module.nodeTag(binding.type_node_idx)) {
        .def => module.moduleEnvConst().store.getDef(binding.def_idx).expr,
        .statement_decl => blk: {
            const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
            const decl = switch (module.getStatement(statement)) {
                .s_decl => |decl| decl,
                else => return null,
            };
            break :blk decl.expr;
        },
        else => null,
    };
}

fn localProcedureTargetForMethodBinding(
    module: TypedCIR.Module,
    checked_bodies: anytype,
    binding: ModuleEnv.MethodBinding,
) ?LocalProcedureMethodTarget {
    const raw_node = @intFromEnum(binding.type_node_idx);
    if (raw_node >= module.nodeCount()) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: method binding node {d} is outside the module node store",
                .{raw_node},
            );
        }
        unreachable;
    }
    if (module.nodeTag(binding.type_node_idx) != .statement_decl) return null;

    const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
    const decl = switch (module.getStatement(statement)) {
        .s_decl => |decl| decl,
        else => return null,
    };

    if (!localProcedureExpr(module, decl.expr)) return null;

    const expr = checked_bodies.exprIdForSource(decl.expr) orelse return null;
    const binder = checked_bodies.patternBinderForSource(decl.pattern) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: local method pattern {d} has no checked binder",
                .{@intFromEnum(decl.pattern)},
            );
        }
        unreachable;
    };

    return .{ .binder = binder, .expr = expr };
}

fn localProcedureExpr(module: TypedCIR.Module, expr_idx: CIR.Expr.Idx) bool {
    return switch (module.expr(expr_idx).data) {
        .e_lambda, .e_closure => true,
        else => false,
    };
}

const ReferencedProcedureTarget = struct {
    kind: MethodTargetKind,
    callable_var: Var,
};

/// Resolve a function-typed associated value bound by reference
/// (`method = top_level_fn`) to the referenced procedure. The reference chain
/// is followed through top-level defs and associated declarations until it
/// reaches a procedure-backed binding; a chain that never reaches one is an
/// associated value, not a call target, and resolves to null.
fn referencedProcedureTargetForMethodBinding(
    module: TypedCIR.Module,
    local_templates: *const ProcedureTemplateLookup,
    checked_bodies: anytype,
    binding: ModuleEnv.MethodBinding,
) ?ReferencedProcedureTarget {
    const module_env = module.moduleEnvConst();
    var expr_idx = methodBindingExpr(module, binding) orelse return null;
    // Each hop follows one value binding, and a chain can visit each binding
    // at most once before repeating, so the node count bounds the walk.
    var remaining: usize = module.nodeCount();
    while (remaining > 0) : (remaining -= 1) {
        const pattern_idx = switch (module.expr(expr_idx).data) {
            .e_lookup_local => |lookup| lookup.pattern_idx,
            else => return null,
        };
        if (defForBoundPattern(module_env, pattern_idx)) |target_def_idx| {
            if (local_templates.templateForDef(target_def_idx)) |template| {
                return .{
                    .kind = .{ .procedure = .{
                        .proc = .{ .artifact = template.artifact, .proc_base = template.proc_base },
                        .template = template,
                    } },
                    .callable_var = module.defType(target_def_idx),
                };
            }
            expr_idx = module_env.store.getDef(target_def_idx).expr;
            continue;
        }
        if (statementDeclForBoundPattern(module, pattern_idx)) |decl| {
            if (localProcedureExpr(module, decl.expr)) {
                const expr = checked_bodies.exprIdForSource(decl.expr) orelse return null;
                const binder = checked_bodies.patternBinderForSource(decl.pattern) orelse return null;
                return .{
                    .kind = .{ .local_proc = .{ .binder = binder, .expr = expr } },
                    .callable_var = module.exprType(decl.expr),
                };
            }
            expr_idx = decl.expr;
            continue;
        }
        return null;
    }
    return null;
}

fn defForBoundPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Def.Idx {
    for (module_env.store.sliceDefs(module_env.global_value_defs)) |def_idx| {
        if (module_env.store.getDef(def_idx).pattern == pattern_idx) return def_idx;
    }
    return null;
}

const BoundDecl = struct { pattern: CIR.Pattern.Idx, expr: CIR.Expr.Idx };

fn statementDeclForBoundPattern(module: TypedCIR.Module, pattern_idx: CIR.Pattern.Idx) ?BoundDecl {
    var raw_node: u32 = 0;
    while (raw_node < module.nodeCount()) : (raw_node += 1) {
        if (module.nodeTag(@enumFromInt(raw_node)) != .statement_decl) continue;
        const decl = switch (module.getStatement(@enumFromInt(raw_node))) {
            .s_decl => |decl| decl,
            else => continue,
        };
        if (decl.pattern == pattern_idx) return .{ .pattern = decl.pattern, .expr = decl.expr };
    }
    return null;
}

fn methodOwnerForRegistryEntry(
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    available_artifacts: anytype,
    owner: ModuleEnv.MethodOwner,
) Allocator.Error!MethodOwner {
    const owner_env = methodOwnerEnvForRegistryEntry(module, available_artifacts, owner);
    if (builtinOwnerForRegistryEntry(owner_env, owner.owner)) |builtin_owner| {
        return .{ .builtin = builtin_owner };
    }

    const identity_hash = owner_env.contentIdentityHash() orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: module '{s}' has no content identity",
                .{owner_env.module_name},
            );
        }
        unreachable;
    };
    const stmt = owner_env.store.getStatement(owner.owner);
    const header_idx = switch (stmt) {
        .s_nominal_decl => |nominal| nominal.header,
        .s_alias_decl => |alias| alias.header,
        else => {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic(
                    "checked static dispatch registry invariant violated: method owner statement {d} is not a type declaration",
                    .{@intFromEnum(owner.owner)},
                );
            }
            unreachable;
        },
    };
    const header = owner_env.store.getTypeHeader(header_idx);
    return .{ .nominal = .{
        .module = try names.internModuleIdentity(identity_hash),
        .type_name = try names.internTypeIdent(owner_env.getIdentStoreConst(), header.relative_name),
        .source_decl = @intFromEnum(owner.owner),
    } };
}

fn methodOwnerEnvForRegistryEntry(
    module: TypedCIR.Module,
    available_artifacts: anytype,
    owner: ModuleEnv.MethodOwner,
) *const ModuleEnv {
    const module_env = module.moduleEnvConst();
    const owner_hash = methodOwnerIdentityHashForRegistryEntry(module_env, owner);

    if (ownerEnvIdentityMatches(module_env, owner_hash)) return module_env;

    for (available_artifacts) |artifact| {
        const candidate = artifact.module_env;
        if (ownerEnvIdentityMatches(candidate, owner_hash)) return candidate;
    }

    if (@import("builtin").mode == .Debug) {
        std.debug.panic(
            "checked static dispatch registry invariant violated: could not find owner module '{s}' for receiver method",
            .{module.getIdent(owner.moduleIdent())},
        );
    }
    unreachable;
}

fn methodOwnerIdentityHashForRegistryEntry(
    module_env: *const ModuleEnv,
    owner: ModuleEnv.MethodOwner,
) *const base.ModuleIdentity.Hash {
    if (owner.moduleIdent().eql(module_env.qualified_module_ident)) {
        return module_env.contentIdentityHash() orelse {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic(
                    "checked static dispatch registry invariant violated: local module '{s}' has no content identity",
                    .{module_env.module_name},
                );
            }
            unreachable;
        };
    }

    const owner_identity = module_env.moduleIdentityForDisplayIdent(owner.moduleIdent()) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: receiver owner module '{s}' has no content identity in module '{s}'",
                .{ module_env.getIdent(owner.moduleIdent()), module_env.module_name },
            );
        }
        unreachable;
    };
    return module_env.moduleIdentityHash(owner_identity);
}

fn ownerEnvIdentityMatches(candidate: *const ModuleEnv, owner_hash: *const base.ModuleIdentity.Hash) bool {
    const candidate_hash = candidate.contentIdentityHash() orelse return false;
    return base.ModuleIdentity.eql(candidate_hash, owner_hash);
}

fn builtinOwnerForRegistryEntry(
    module_env: *const ModuleEnv,
    owner_stmt: CIR.Statement.Idx,
) ?BuiltinOwner {
    const common = module_env.idents;
    if (module_env.module_role != .builtin) return null;

    const stmt = module_env.store.getStatement(owner_stmt);
    const type_ident = switch (stmt) {
        .s_nominal_decl => |nominal| module_env.store.getTypeHeader(nominal.header).name,
        .s_alias_decl => |alias| module_env.store.getTypeHeader(alias.header).name,
        else => return null,
    };

    if (type_ident.eql(common.bool) or type_ident.eql(common.bool_type)) return .bool;
    if (type_ident.eql(common.str) or type_ident.eql(common.builtin_str)) return .str;
    if (type_ident.eql(common.u8) or type_ident.eql(common.u8_type)) return .u8;
    if (type_ident.eql(common.i8) or type_ident.eql(common.i8_type)) return .i8;
    if (type_ident.eql(common.u16) or type_ident.eql(common.u16_type)) return .u16;
    if (type_ident.eql(common.i16) or type_ident.eql(common.i16_type)) return .i16;
    if (type_ident.eql(common.u32) or type_ident.eql(common.u32_type)) return .u32;
    if (type_ident.eql(common.i32) or type_ident.eql(common.i32_type)) return .i32;
    if (type_ident.eql(common.u64) or type_ident.eql(common.u64_type)) return .u64;
    if (type_ident.eql(common.i64) or type_ident.eql(common.i64_type)) return .i64;
    if (type_ident.eql(common.u128) or type_ident.eql(common.u128_type)) return .u128;
    if (type_ident.eql(common.i128) or type_ident.eql(common.i128_type)) return .i128;
    if (type_ident.eql(common.f32) or type_ident.eql(common.f32_type)) return .f32;
    if (type_ident.eql(common.f64) or type_ident.eql(common.f64_type)) return .f64;
    if (type_ident.eql(common.dec) or type_ident.eql(common.dec_type)) return .dec;

    if (type_ident.eql(common.list) or type_ident.eql(common.builtin_list)) return .list;
    if (type_ident.eql(common.box) or type_ident.eql(common.builtin_box)) return .box;
    if (type_ident.eql(common.dict) or type_ident.eql(common.builtin_dict)) return .dict;
    if (type_ident.eql(common.set) or type_ident.eql(common.builtin_set)) return .set;
    if (type_ident.eql(common.builtin_encoding_field_names)) return .fields;
    if (type_ident.eql(common.builtin_encoding_field_name)) return .field;
    if (type_ident.eql(common.builtin_encoding_parse_tag_union_spec)) return .parse_tag_union_spec;
    if (type_ident.eql(common.builtin_crypto_sha256_digest)) return .crypto_sha256_digest;
    if (type_ident.eql(common.builtin_crypto_sha256_hasher)) return .crypto_sha256_hasher;
    if (type_ident.eql(common.builtin_crypto_blake3_digest)) return .crypto_blake3_digest;
    if (type_ident.eql(common.builtin_crypto_blake3_hasher)) return .crypto_blake3_hasher;
    return null;
}

fn methodRegistryEntryLessThan(_: void, a: MethodRegistryEntry, b: MethodRegistryEntry) bool {
    return methodKeyOrder(a.key, b.key) == .lt;
}

fn finalizeMethodRegistryEntries(entries: []MethodRegistryEntry) void {
    // Zero padding and inactive-union bytes first: at ReleaseFast the sorted
    // entries are compared with fused wide loads that touch those bytes, and
    // runtime-built entries would otherwise carry undefined memory there
    // (serialized registries are already zeroed by appendSlicePodZeroed).
    for (entries) |*entry| {
        collections.CompactWriter.zeroValuePadding(MethodRegistryEntry, @ptrCast(entry));
    }
    std.mem.sort(MethodRegistryEntry, entries, {}, methodRegistryEntryLessThan);
    assertMethodRegistryKeysUnique(entries);
}

fn assertMethodRegistryKeysUnique(entries: []const MethodRegistryEntry) void {
    if (entries.len < 2) return;
    var i: usize = 1;
    while (i < entries.len) : (i += 1) {
        if (methodKeyOrder(entries[i - 1].key, entries[i].key) != .eq) continue;
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch registry invariant violated: duplicate method registry key", .{});
        }
        unreachable;
    }
}

fn methodKeyOrder(a: MethodKey, b: MethodKey) std.math.Order {
    const owner_order = methodOwnerOrder(a.owner, b.owner);
    if (owner_order != .eq) return owner_order;
    return orderEnum(canonical.MethodNameId, a.method, b.method);
}

fn methodEntryOrder(e: MethodRegistryEntry, key: MethodKey) std.math.Order {
    return methodKeyOrder(e.key, key);
}

fn methodOwnerOrder(a: MethodOwner, b: MethodOwner) std.math.Order {
    return methodOwnerSortKey(a).order(methodOwnerSortKey(b));
}

/// A fully-defined scalar projection of a `MethodOwner` for ordering.
/// Comparing the union directly reads memory whose inactive-variant bytes are
/// undefined for runtime-built registries and stack keys; projecting first
/// writes every compared scalar explicitly. The order matches the previous
/// per-variant comparison (nominal < builtin; module identity then type name;
/// `source_decl == null` sorts before any value), so registries sorted by
/// earlier builds search identically.
const MethodOwnerSortKey = struct {
    tag: u32,
    first: u32,
    second: u32,
    third: u32,

    fn order(a: @This(), b: @This()) std.math.Order {
        if (a.tag != b.tag) return orderU32(a.tag, b.tag);
        if (a.first != b.first) return orderU32(a.first, b.first);
        if (a.second != b.second) return orderU32(a.second, b.second);
        return orderU32(a.third, b.third);
    }
};

fn methodOwnerSortKey(owner: MethodOwner) MethodOwnerSortKey {
    return switch (owner) {
        .nominal => |nominal| .{
            .tag = 0,
            .first = @intFromEnum(nominal.module),
            .second = @intFromEnum(nominal.type_name),
            // null sorts before any statement value.
            .third = if (nominal.source_decl) |source_decl| source_decl +| 1 else 0,
        },
        .builtin => |builtin_owner| .{
            .tag = 1,
            .first = @intFromEnum(builtin_owner),
            .second = 0,
            .third = 0,
        },
    };
}

fn orderEnum(comptime T: type, a: T, b: T) std.math.Order {
    return orderU32(@intFromEnum(a), @intFromEnum(b));
}

fn orderU32(a: u32, b: u32) std.math.Order {
    if (a == b) return .eq;
    return if (a < b) .lt else .gt;
}

/// Public `StaticDispatchResultMode` declaration.
pub const StaticDispatchResultMode = union(enum) {
    value,
    equality: struct {
        structural_allowed: bool,
        negated: bool,
    },
    /// A `to_hash : self, Hasher -> Hasher` dispatch whose receiver is an
    /// anonymous structural type. When `structural_allowed` is set, lowering
    /// decomposes the hash structurally instead of dispatching to a method.
    hash: struct {
        structural_allowed: bool,
    },
    parser_for: struct {
        structural_allowed: bool,
    },
    encoder_for: struct {
        structural_allowed: bool,
    },
};

/// Public `StaticDispatchDispatcher` declaration.
pub const StaticDispatchDispatcher = union(enum) {
    arg: u32,
    type_only,
};

/// Public `StaticDispatchOperand` declaration.
pub const StaticDispatchOperand = union(enum) {
    checked_expr: CheckedExprId,
    /// Compiler-generated finite `Iter` for string interpolation. The checked
    /// expression owns the first segment and flat interpolation parts.
    generated_interpolation_iter: CheckedExprId,
    generated_numeral: ModuleEnv.NumeralLiteral,
    /// A string literal's post-escape contents, passed to `from_quote` as Str.
    generated_quote: CheckedStringLiteralId,
};

/// Public `StructuralKind` declaration.
///
/// The compiler-derived structural implementations the checker can choose for
/// a dispatch instead of a method target.
pub const StructuralKind = enum(u8) {
    equality,
    hash,
    parser,
    encoder,
};

/// Public `EvidenceNodeId` declaration. Index into
/// `StaticDispatchPlanTable.evidence_nodes`.
pub const EvidenceNodeId = enum(u32) { _ };

/// Public `EvidenceChainIndex` declaration.
///
/// A dispatch obligation forwarded to the enclosing callable's evidence
/// params: `index` is the canonical evidence-param index (see
/// `dispatch_evidence.zig`), and `depth` counts enclosing generalized
/// callables outward from the reference (0 = the innermost generalized
/// callable the reference appears in).
pub const EvidenceChainIndex = struct {
    depth: u16,
    index: u16,
};

/// Public `CheckedEvidence` declaration.
///
/// How one dispatch obligation was satisfied: with a concrete target (plus
/// nested evidence for the target's own obligations), by forwarding to the
/// enclosing callable's evidence params, or with a compiler-derived structural
/// implementation. `checked_error` marks an obligation at a site checking
/// already rejected; consuming it after checking is a compiler bug.
pub const CheckedEvidence = union(enum) {
    direct: EvidenceNodeId,
    constraint: EvidenceChainIndex,
    structural: StructuralKind,
    checked_error,
    /// The edge left this obligation's dispatcher unsolved: no value of that
    /// type can ever reach the dispatch (e.g. the `Ok` payload of a `Try` that
    /// is always `Err` at this edge). The obligation is vacuous; consuming it
    /// lowers to an unreachable crash, never to a resolved call.
    unreachable_value,
};

/// Public `EvidenceNode` declaration.
///
/// A concrete method target together with evidence for the target's own
/// evidence params (in the target scheme's canonical order); `nested` is a
/// range into `StaticDispatchPlanTable.evidence_refs`.
pub const EvidenceNode = struct {
    target: MethodTarget,
    nested: artifact_serialize.Span = .{},
};

/// Public `SiteEvidenceEntry` declaration.
///
/// Evidence for one instantiation site (keyed by the checked expression of the
/// use), covering the instantiated scheme's evidence params in canonical
/// order: a range into `StaticDispatchPlanTable.evidence_refs`. Sorted by key
/// for binary search (transform D).
pub const SiteEvidenceEntry = extern struct {
    /// `@intFromEnum` of the site's `CheckedExprId`.
    key: u32,
    start: u32,
    len: u32,
};

/// Public `EvidencePathStep` declaration: one semantic step from a type to a
/// component, in the artifact's canonical names (`data` is a positional index,
/// a `canonical.RecordFieldLabelId`, or a `canonical.TagNameId` per kind).
pub const EvidencePathStep = dispatch_evidence.PathStep;

/// Public `EvidenceParamRecord` declaration.
///
/// One published evidence param of a procedure template's scheme, in canonical
/// order (see `dispatch_evidence.zig`). Consumers index these by position; the
/// method name identifies the obligation, and `path` locates the dispatcher
/// within the scheme's callable so compiler-generated call edges (which have
/// no checked instantiation records) can resolve the obligation from the
/// concrete monomorphic callable. An empty path means the dispatcher is only
/// reachable through a constraint's fn type.
pub const EvidenceParamRecord = struct {
    method: canonical.MethodNameId,
    path: artifact_serialize.Span = .{},
};

/// Public `StaticDispatchResolution` declaration.
pub const StaticDispatchResolution = union(enum) {
    /// Checking proved the concrete target (with nested evidence for the
    /// target's own obligations). Later stages must call this target directly
    /// instead of rediscovering it from source or type names.
    direct: EvidenceNodeId,
    /// The dispatcher is one of the enclosing callable's constrained scheme
    /// vars; each specialization edge supplies the target as evidence.
    constraint: EvidenceChainIndex,
    /// The checker chose a compiler-derived structural implementation.
    structural: StructuralKind,
    /// Checking rejected this site; lowering must never consume the plan.
    checked_error,
    /// The dispatcher is a constrained var no specialization edge can ever
    /// supply (not an evidence param of any enclosing callable and not a
    /// defaulting literal): the dispatch is statically unreachable and lowers
    /// to an explicit crash.
    unreachable_dispatch,
};

/// Public `StaticDispatchCallPlan` declaration.
pub const StaticDispatchCallPlan = struct {
    expr: CheckedExprId,
    method: canonical.MethodNameId,
    dispatcher: StaticDispatchDispatcher,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    /// Range into `StaticDispatchPlanTable.operand_pool` (transform B).
    args: artifact_serialize.Span = .{},
    result_mode: StaticDispatchResultMode,
    /// Assigned by `resolveTotalDispatchPlans` at publication; the default is
    /// a construction placeholder the pass overwrites for every plan.
    resolution: StaticDispatchResolution = .checked_error,

    /// The plan's operands within its table's pool.
    pub fn argsSlice(self: StaticDispatchCallPlan, table: *const StaticDispatchPlanTable) []const StaticDispatchOperand {
        return table.operand_pool[self.args.start .. self.args.start + self.args.len];
    }
};

/// Public `StaticDispatchPlanId` declaration.
pub const StaticDispatchPlanId = enum(u32) { _ };

/// Public `IteratorForPlanId` declaration.
pub const IteratorForPlanId = enum(u32) { _ };

/// Public `IteratorDispatchOperand` declaration.
pub const IteratorDispatchOperand = union(enum) {
    checked_expr: CheckedExprId,
    loop_iterator_state,
};

/// Public `IteratorDispatchCall` declaration.
pub const IteratorDispatchCall = struct {
    method: canonical.MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    dispatcher_arg_index: u32,
    /// Range into `StaticDispatchPlanTable.iter_operand_pool` (transform B).
    args: artifact_serialize.Span = .{},
    /// Assigned by `resolveTotalDispatchPlans` at publication; the default is
    /// a construction placeholder the pass overwrites for every plan.
    resolution: StaticDispatchResolution = .checked_error,

    pub fn argsSlice(self: IteratorDispatchCall, table: *const StaticDispatchPlanTable) []const IteratorDispatchOperand {
        return table.iter_operand_pool[self.args.start .. self.args.start + self.args.len];
    }
};

/// Public `IteratorForPlan` declaration.
pub const IteratorForPlan = struct {
    iter: IteratorDispatchCall,
    next: IteratorDispatchCall,
    iterable: CheckedExprId,
    item_ty: CheckedTypeId,
    iterator_ty: CheckedTypeId,
    step_ty: CheckedTypeId,
};

/// Public `StaticDispatchPlanTable` declaration.
/// Relocatable replacement for an `AutoHashMap(idx -> id)`: a `(key, val)` pair
/// (both `@intFromEnum` u32s) stored in a sorted, binary-searchable POD slice
/// (transform D). Keys are unique (each source node/expr maps to one plan).
pub const PlanKV = extern struct { key: u32, val: u32 };

fn planKvLessThan(_: void, a: PlanKV, b: PlanKV) bool {
    return a.key < b.key;
}

fn planKvOrder(e: PlanKV, key: u32) std.math.Order {
    return std.math.order(e.key, key);
}

/// Binary-search a sorted `PlanKV` slice; returns the value (`@intFromEnum` of
/// the id) or null.
fn lookupPlanKV(sorted: []const PlanKV, key: u32) ?u32 {
    const found = artifact_serialize.binarySearchByKey(PlanKV, u32, sorted, key, planKvOrder) orelse return null;
    return found.val;
}

/// Append `ops` to `pool` and return their `(start, len)` range. Used to flatten
/// per-plan operand slices into the table's shared operand pools (transform B).
fn pushOperands(comptime T: type, pool: *std.ArrayList(T), allocator: Allocator, ops: []const T) Allocator.Error!artifact_serialize.Span {
    return artifact_serialize.appendSpan(artifact_serialize.Span, T, pool, allocator, ops);
}

fn sortedFromMap(allocator: Allocator, map: anytype) Allocator.Error![]PlanKV {
    const out = try allocator.alloc(PlanKV, map.count());
    errdefer allocator.free(out);
    var it = map.iterator();
    var i: usize = 0;
    while (it.next()) |entry| : (i += 1) {
        out[i] = .{ .key = @intFromEnum(entry.key_ptr.*), .val = @intFromEnum(entry.value_ptr.*) };
    }
    std.mem.sort(PlanKV, out, {}, planKvLessThan);
    return out;
}

/// Resolved static-dispatch plans for a checked module: the per-call-site plans, the
/// sorted expr/node → plan indexes, and the shared operand pools the plans reference
/// (transform D). Reconstituted as plain slices on deserialize.
pub const StaticDispatchPlanTable = struct {
    plans: []StaticDispatchCallPlan = &.{},
    /// `CIR.Expr.Idx` -> `StaticDispatchPlanId`, sorted by key (transform D).
    by_expr: []PlanKV = &.{},
    /// `CIR.Node.Idx` -> `StaticDispatchPlanId`, sorted by key.
    numeral_by_node: []PlanKV = &.{},
    /// `CIR.Node.Idx` -> `StaticDispatchPlanId`, sorted by key.
    quote_by_node: []PlanKV = &.{},
    iterator_for_plans: []IteratorForPlan = &.{},
    /// `CIR.Node.Idx` -> `IteratorForPlanId`, sorted by key.
    iterator_for_by_node: []PlanKV = &.{},
    template_refs: []StaticDispatchPlanId = &.{},
    /// Shared flat pool of plan operands (transform-B side list).
    operand_pool: []const StaticDispatchOperand = &.{},
    /// Shared flat pool of iterator-plan operands.
    iter_operand_pool: []const IteratorDispatchOperand = &.{},
    /// Concrete dispatch targets with nested evidence (`EvidenceNodeId`s).
    evidence_nodes: []EvidenceNode = &.{},
    /// Flat pool of evidence: node `nested` ranges and site-evidence ranges.
    evidence_refs: []CheckedEvidence = &.{},
    /// Checked-expr-keyed evidence for instantiation sites, sorted by key.
    site_evidence: []SiteEvidenceEntry = &.{},

    pub const Serialized = extern struct {
        plans: SerializedSlice(StaticDispatchCallPlan) = .{},
        by_expr: SerializedSlice(PlanKV) = .{},
        numeral_by_node: SerializedSlice(PlanKV) = .{},
        quote_by_node: SerializedSlice(PlanKV) = .{},
        iterator_for_plans: SerializedSlice(IteratorForPlan) = .{},
        iterator_for_by_node: SerializedSlice(PlanKV) = .{},
        template_refs: SerializedSlice(StaticDispatchPlanId) = .{},
        operand_pool: SerializedSlice(StaticDispatchOperand) = .{},
        iter_operand_pool: SerializedSlice(IteratorDispatchOperand) = .{},
        evidence_nodes: SerializedSlice(EvidenceNode) = .{},
        evidence_refs: SerializedSlice(CheckedEvidence) = .{},
        site_evidence: SerializedSlice(SiteEvidenceEntry) = .{},

        comptime {
            // 12 side lists → 12 base-pointer fixups on deserialize, never a
            // function of how many plans/operands the table holds.
            std.debug.assert(artifact_serialize.relocatablePointerCount(Serialized) == 12);
        }

        const Serde = artifact_serialize.SliceStoreSerde(StaticDispatchPlanTable, @This());
        pub const serialize = Serde.serialize;
        pub const deserialize = Serde.deserialize;
    };

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        checked_types: anytype,
        checked_bodies: anytype,
        build_data: *PlanTableBuildData,
    ) Allocator.Error!StaticDispatchPlanTable {
        var plans = std.ArrayList(StaticDispatchCallPlan).empty;
        errdefer plans.deinit(allocator);
        var plan_sources = std.ArrayList(PlanSource).empty;
        errdefer plan_sources.deinit(allocator);
        var iterator_plan_sources = std.ArrayList(IteratorPlanSource).empty;
        errdefer iterator_plan_sources.deinit(allocator);
        // Operand side-pools; per-plan operand slices are flattened into these.
        var operand_pool = std.ArrayList(StaticDispatchOperand).empty;
        errdefer operand_pool.deinit(allocator);
        var iter_operand_pool = std.ArrayList(IteratorDispatchOperand).empty;
        errdefer iter_operand_pool.deinit(allocator);
        var by_expr: std.AutoHashMapUnmanaged(CIR.Expr.Idx, StaticDispatchPlanId) = .{};
        errdefer by_expr.deinit(allocator);
        var numeral_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, StaticDispatchPlanId) = .{};
        errdefer numeral_by_node.deinit(allocator);
        var quote_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, StaticDispatchPlanId) = .{};
        errdefer quote_by_node.deinit(allocator);
        var iterator_for_plans = std.ArrayList(IteratorForPlan).empty;
        errdefer iterator_for_plans.deinit(allocator);
        var iterator_for_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, IteratorForPlanId) = .{};
        errdefer iterator_for_by_node.deinit(allocator);

        var constraint_index = try StaticDispatchConstraintIndex.fromModule(allocator, module, checked_bodies);
        defer constraint_index.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const tag = module.nodeTag(@enumFromInt(node_idx));
            switch (tag) {
                .expr_dispatch_call,
                .expr_interpolation,
                .expr_type_dispatch_call,
                .expr_method_eq,
                => {},
                else => continue,
            }

            const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
            const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse continue;
            const expr = module.expr(expr_idx);
            const checked_expr_data = checked_bodies.expr(checked_expr).data;
            const idents = module.identStoreConst();
            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            switch (expr.data) {
                .e_dispatch_call => |dispatch_call| {
                    const explicit_args = module.sliceExpr(dispatch_call.args);
                    const args = try allocator.alloc(StaticDispatchOperand, explicit_args.len + 1);
                    defer allocator.free(args);
                    args[0] = .{ .checked_expr = checkedExprIdForSource(checked_bodies, dispatch_call.receiver) };
                    for (explicit_args, 0..) |arg, i| {
                        args[i + 1] = .{ .checked_expr = checkedExprIdForSource(checked_bodies, arg) };
                    }
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher = .{ .arg = 0 },
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(dispatch_call.receiver)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = ar,
                        .result_mode = try staticDispatchResultModeForCheckedValueCall(allocator, module, checked_types, &constraint_index, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = module.exprType(dispatch_call.receiver),
                        .constraint_fn_var = dispatch_call.constraint_fn_var,
                    });
                },
                .e_interpolation => |interpolation| {
                    const checked_interpolation = switch (checked_expr_data) {
                        .interpolation => |checked_interpolation| checked_interpolation,
                        else => continue,
                    };
                    const args = try allocator.alloc(StaticDispatchOperand, 2);
                    defer allocator.free(args);
                    args[0] = .{ .checked_expr = checked_interpolation.first };
                    args[1] = .{ .generated_interpolation_iter = checked_expr };
                    const from_interpolation = try names.internMethodName("from_interpolation");
                    const constraint_fn_var = interpolation.constraint_fn_var orelse unreachable;
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = from_interpolation,
                        .dispatcher = .type_only,
                        .dispatcher_ty = try interpolationDispatcherTypeId(allocator, module, checked_types, expr_idx),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, constraint_fn_var),
                        .args = ar,
                        .result_mode = .value,
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = interpolationDispatcherVar(module, expr_idx),
                        .constraint_fn_var = constraint_fn_var,
                    });
                },
                .e_type_dispatch_call => |dispatch_call| {
                    const args = try staticDispatchOperandsForSlice(allocator, checked_bodies, module.sliceExpr(dispatch_call.args));
                    defer allocator.free(args);
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher = .type_only,
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, typeDispatchOwnerVar(module, dispatch_call.type_dispatch_stmt)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = ar,
                        .result_mode = try staticDispatchResultModeForCheckedValueCall(allocator, module, checked_types, &constraint_index, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = typeDispatchOwnerVar(module, dispatch_call.type_dispatch_stmt),
                        .constraint_fn_var = dispatch_call.constraint_fn_var,
                    });
                },
                .e_method_eq => |eq| {
                    const args = try staticDispatchOperandsForSlice(allocator, checked_bodies, &.{ eq.lhs, eq.rhs });
                    defer allocator.free(args);
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, module.commonIdents().is_eq),
                        .dispatcher = .{ .arg = 0 },
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(eq.lhs)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, eq.constraint_fn_var),
                        .args = ar,
                        .result_mode = .{ .equality = .{
                            .structural_allowed = true,
                            .negated = eq.negated,
                        } },
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = module.exprType(eq.lhs),
                        .constraint_fn_var = eq.constraint_fn_var,
                    });
                },
                else => unreachable,
            }
            try by_expr.put(allocator, expr_idx, plan_id);
        }

        const module_env = module.moduleEnvConst();
        for (module_env.numeral_dispatch_plans.items.items) |numeral_plan| {
            const node: CIR.Node.Idx = @enumFromInt(numeral_plan.node_idx);
            const expr_idx: CIR.Expr.Idx = @enumFromInt(numeral_plan.node_idx);
            const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse
                checked_bodies.numeralConversionExprAtRawNode(numeral_plan.node_idx) orelse
                continue;
            switch (checked_bodies.expr(checked_expr).data) {
                .num_from_numeral,
                .typed_num_from_numeral,
                => {},
                .num,
                .typed_int,
                .frac_f32,
                .frac_f64,
                .dec,
                .dec_small,
                .typed_frac,
                => continue,
                else => {
                    if (@import("builtin").mode == .Debug) {
                        std.debug.panic(
                            "checked static dispatch invariant violated: numeral dispatch plan {d} points at a non-numeric checked expression",
                            .{numeral_plan.node_idx},
                        );
                    }
                    unreachable;
                },
            }
            const literal = module_env.numeralLiteralForNode(node) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch invariant violated: runtime from_numeral plan {d} has no exact literal",
                        .{numeral_plan.node_idx},
                    );
                }
                unreachable;
            };
            if (!literal.isMaterialized()) {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch invariant violated: runtime from_numeral plan {d} has an unmaterialized literal",
                        .{numeral_plan.node_idx},
                    );
                }
                unreachable;
            }
            var args = [_]StaticDispatchOperand{.{ .generated_numeral = literal }};
            const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, &args);

            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            try plans.append(allocator, .{
                .expr = checked_expr,
                .method = try names.internMethodName("from_numeral"),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(numeral_plan.target_var)),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(numeral_plan.fn_var)),
                .args = ar,
                .result_mode = .value,
            });
            try plan_sources.append(allocator, .{
                .dispatcher_var = @enumFromInt(numeral_plan.target_var),
                .constraint_fn_var = @enumFromInt(numeral_plan.fn_var),
            });
            try numeral_by_node.put(allocator, node, plan_id);
        }

        for (module_env.quote_dispatch_plans.items.items) |quote_plan| {
            const node: CIR.Node.Idx = @enumFromInt(quote_plan.node_idx);
            const expr_idx: CIR.Expr.Idx = @enumFromInt(quote_plan.node_idx);
            const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse
                checked_bodies.numeralConversionExprAtRawNode(quote_plan.node_idx) orelse
                continue;
            const literal = switch (checked_bodies.expr(checked_expr).data) {
                .str_from_quote => |quote| quote.literal,
                // Builtin Str literals keep the direct string encoding.
                .str, .str_segment => continue,
                else => {
                    if (@import("builtin").mode == .Debug) {
                        std.debug.panic(
                            "checked static dispatch invariant violated: quote dispatch plan {d} points at a non-string checked expression",
                            .{quote_plan.node_idx},
                        );
                    }
                    unreachable;
                },
            };
            var args = [_]StaticDispatchOperand{.{ .generated_quote = literal }};
            const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, &args);

            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            try plans.append(allocator, .{
                .expr = checked_expr,
                .method = try names.internMethodName("from_quote"),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(quote_plan.target_var)),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(quote_plan.fn_var)),
                .args = ar,
                .result_mode = .value,
            });
            try plan_sources.append(allocator, .{
                .dispatcher_var = @enumFromInt(quote_plan.target_var),
                .constraint_fn_var = @enumFromInt(quote_plan.fn_var),
            });
            try quote_by_node.put(allocator, node, plan_id);
        }

        for (module_env.for_loop_dispatch_plans.items.items) |for_plan| {
            const for_node_idx: CIR.Node.Idx = @enumFromInt(for_plan.node_idx);
            const pattern_idx: CIR.Pattern.Idx = @enumFromInt(for_plan.pattern_idx);
            const iterable_idx: CIR.Expr.Idx = @enumFromInt(for_plan.iterable_idx);

            if (checked_bodies.exprIdForSource(iterable_idx) == null) continue;
            const for_has_checked_node = switch (module.nodeTag(for_node_idx)) {
                .expr_for => checked_bodies.exprIdForSource(@enumFromInt(for_plan.node_idx)) != null,
                .statement_for => checked_bodies.statementIdForSource(@enumFromInt(for_plan.node_idx)) != null,
                else => false,
            };
            if (!for_has_checked_node) continue;

            const iterable_expr = checkedExprIdForSource(checked_bodies, iterable_idx);
            const item_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.patternType(pattern_idx));
            const iter_callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.iter_fn_var));
            const next_callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.next_fn_var));
            const iterator_ty = checkedFunctionReturnTypeId(checked_types, iter_callable_ty);
            const step_ty = checkedFunctionReturnTypeId(checked_types, next_callable_ty);

            const iterator_for_id: IteratorForPlanId = @enumFromInt(@as(u32, @intCast(iterator_for_plans.items.len)));
            {
                var iter_args = [_]IteratorDispatchOperand{.{ .checked_expr = iterable_expr }};
                const iter_ar = try pushOperands(IteratorDispatchOperand, &iter_operand_pool, allocator, &iter_args);

                var next_args = [_]IteratorDispatchOperand{.loop_iterator_state};
                const next_ar = try pushOperands(IteratorDispatchOperand, &iter_operand_pool, allocator, &next_args);

                try iterator_for_plans.append(allocator, .{
                    .iter = .{
                        .method = try names.internMethodName("iter"),
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(iterable_idx)),
                        .callable_ty = iter_callable_ty,
                        .dispatcher_arg_index = 0,
                        .args = iter_ar,
                    },
                    .next = .{
                        .method = try names.internMethodName("next"),
                        .dispatcher_ty = iterator_ty,
                        .callable_ty = next_callable_ty,
                        .dispatcher_arg_index = 0,
                        .args = next_ar,
                    },
                    .iterable = iterable_expr,
                    .item_ty = item_ty,
                    .iterator_ty = iterator_ty,
                    .step_ty = step_ty,
                });
                try iterator_plan_sources.append(allocator, .{
                    .iter_dispatcher_var = module.exprType(iterable_idx),
                    .iter_fn_var = @enumFromInt(for_plan.iter_fn_var),
                    .next_fn_var = @enumFromInt(for_plan.next_fn_var),
                });
            }
            try iterator_for_by_node.put(allocator, for_node_idx, iterator_for_id);
        }

        // Convert the construction-time hashmaps into sorted, relocatable
        // PlanKV slices (transform D), then release the maps.
        const by_expr_sorted = try sortedFromMap(allocator, by_expr);
        errdefer allocator.free(by_expr_sorted);
        const numeral_sorted = try sortedFromMap(allocator, numeral_by_node);
        errdefer allocator.free(numeral_sorted);
        const quote_sorted = try sortedFromMap(allocator, quote_by_node);
        errdefer allocator.free(quote_sorted);
        const iterator_for_sorted = try sortedFromMap(allocator, iterator_for_by_node);
        errdefer allocator.free(iterator_for_sorted);
        by_expr.deinit(allocator);
        numeral_by_node.deinit(allocator);
        quote_by_node.deinit(allocator);
        iterator_for_by_node.deinit(allocator);

        build_data.* = .{
            .plan_sources = try plan_sources.toOwnedSlice(allocator),
            .iterator_plan_sources = try iterator_plan_sources.toOwnedSlice(allocator),
        };

        return .{
            .plans = try plans.toOwnedSlice(allocator),
            .by_expr = by_expr_sorted,
            .numeral_by_node = numeral_sorted,
            .quote_by_node = quote_sorted,
            .iterator_for_plans = try iterator_for_plans.toOwnedSlice(allocator),
            .iterator_for_by_node = iterator_for_sorted,
            .operand_pool = try operand_pool.toOwnedSlice(allocator),
            .iter_operand_pool = try iter_operand_pool.toOwnedSlice(allocator),
        };
    }

    pub fn lookupByExpr(self: *const StaticDispatchPlanTable, expr: CIR.Expr.Idx) ?StaticDispatchPlanId {
        return if (lookupPlanKV(self.by_expr, @intFromEnum(expr))) |v| @enumFromInt(v) else null;
    }

    pub fn lookupNumeralByNode(self: *const StaticDispatchPlanTable, node: CIR.Node.Idx) ?StaticDispatchPlanId {
        return if (lookupPlanKV(self.numeral_by_node, @intFromEnum(node))) |v| @enumFromInt(v) else null;
    }

    pub fn lookupQuoteByNode(self: *const StaticDispatchPlanTable, node: CIR.Node.Idx) ?StaticDispatchPlanId {
        return if (lookupPlanKV(self.quote_by_node, @intFromEnum(node))) |v| @enumFromInt(v) else null;
    }

    pub fn lookupIteratorForByNode(self: *const StaticDispatchPlanTable, node: CIR.Node.Idx) ?IteratorForPlanId {
        return if (lookupPlanKV(self.iterator_for_by_node, @intFromEnum(node))) |v| @enumFromInt(v) else null;
    }

    pub fn evidenceNode(self: *const StaticDispatchPlanTable, id: EvidenceNodeId) EvidenceNode {
        return self.evidence_nodes[@intFromEnum(id)];
    }

    /// The evidence node's nested evidence, in the target scheme's canonical
    /// evidence-param order.
    pub fn nestedEvidence(self: *const StaticDispatchPlanTable, node: EvidenceNode) []const CheckedEvidence {
        return self.evidence_refs[node.nested.start .. node.nested.start + node.nested.len];
    }

    /// Evidence for the scheme instantiated at `expr` (a value use of a
    /// constrained definition), in the callee scheme's canonical
    /// evidence-param order; null when the use needed no evidence.
    pub fn siteEvidence(self: *const StaticDispatchPlanTable, expr: CheckedExprId) ?[]const CheckedEvidence {
        const found = artifact_serialize.binarySearchByKey(SiteEvidenceEntry, u32, self.site_evidence, @intFromEnum(expr), siteEvidenceOrder) orelse return null;
        return self.evidence_refs[found.start .. found.start + found.len];
    }

    /// Build-time-only teardown: frees the heap-owned slices. A frozen
    /// (deserialized) table's slices alias the artifact's single backing buffer and are
    /// NEVER freed here — the artifact's `deinitInternal` frees the buffer wholesale and
    /// does not call any sub-store `deinit` on the frozen path. (No `serialized` flag is
    /// needed because, unlike the mutation-guarded stores, this table has no post-load
    /// mutators.)
    pub fn deinit(self: *StaticDispatchPlanTable, allocator: Allocator) void {
        allocator.free(self.template_refs);
        allocator.free(self.by_expr);
        allocator.free(self.numeral_by_node);
        allocator.free(self.quote_by_node);
        allocator.free(self.iterator_for_by_node);
        allocator.free(self.plans);
        allocator.free(self.iterator_for_plans);
        allocator.free(@constCast(self.operand_pool));
        allocator.free(@constCast(self.iter_operand_pool));
        allocator.free(self.evidence_nodes);
        allocator.free(self.evidence_refs);
        allocator.free(self.site_evidence);
        self.* = .{};
    }
};

fn siteEvidenceOrder(e: SiteEvidenceEntry, key: u32) std.math.Order {
    return std.math.order(e.key, key);
}

/// Build-time-only side data recorded by `StaticDispatchPlanTable.fromModule`
/// so the total-resolution pass (`dispatch_evidence.zig`) can resolve each
/// plan from the checker type store: the source dispatcher var and the source
/// constraint fn var (the discharge-record key). Parallel to `plans`; never
/// serialized.
pub const PlanSource = struct {
    dispatcher_var: Var,
    constraint_fn_var: ?Var,
};

/// Build-time-only side data for iterator plans, parallel to
/// `iterator_for_plans`.
pub const IteratorPlanSource = struct {
    iter_dispatcher_var: Var,
    iter_fn_var: Var,
    next_fn_var: Var,
};

/// Build-time-only outputs of `StaticDispatchPlanTable.fromModule` consumed by
/// the total-resolution pass.
pub const PlanTableBuildData = struct {
    plan_sources: []PlanSource = &.{},
    iterator_plan_sources: []IteratorPlanSource = &.{},

    pub fn deinit(self: *PlanTableBuildData, allocator: Allocator) void {
        allocator.free(self.plan_sources);
        allocator.free(self.iterator_plan_sources);
        self.* = .{};
    }
};

const StaticDispatchConstraintIndex = struct {
    constraints: []const types.StaticDispatchConstraint = &.{},
    by_fn_var: std.AutoHashMapUnmanaged(Var, u32) = .{},

    fn fromModule(allocator: Allocator, module: TypedCIR.Module, checked_bodies: anytype) Allocator.Error!StaticDispatchConstraintIndex {
        const store = module.typeStoreConst();
        var live_fn_vars: std.AutoHashMapUnmanaged(Var, void) = .{};
        defer live_fn_vars.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
            const constraint_fn_var: ?Var = switch (module.nodeTag(@enumFromInt(node_idx))) {
                .expr_dispatch_call => module.expr(expr_idx).data.e_dispatch_call.constraint_fn_var,
                .expr_interpolation => module.expr(expr_idx).data.e_interpolation.constraint_fn_var,
                .expr_type_dispatch_call => module.expr(expr_idx).data.e_type_dispatch_call.constraint_fn_var,
                .expr_method_eq => module.expr(expr_idx).data.e_method_eq.constraint_fn_var,
                else => null,
            };
            if (constraint_fn_var) |fn_var| {
                const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse continue;
                if (module.nodeTag(@enumFromInt(node_idx)) == .expr_interpolation and
                    std.meta.activeTag(checked_bodies.expr(checked_expr).data) != .interpolation) continue;
                try live_fn_vars.put(allocator, fn_var, {});
            }
        }

        var index = StaticDispatchConstraintIndex{
            .constraints = store.static_dispatch_constraints.items.items,
        };
        errdefer index.deinit(allocator);

        try index.by_fn_var.ensureTotalCapacity(allocator, @intCast(live_fn_vars.count()));
        for (index.constraints, 0..) |constraint, i| {
            if (!live_fn_vars.contains(constraint.fn_var)) continue;
            const entry = try index.by_fn_var.getOrPut(allocator, constraint.fn_var);
            if (entry.found_existing) {
                const existing = index.constraints[entry.value_ptr.*];
                if (staticDispatchConstraintsEquivalent(existing, constraint)) continue;
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch constraint invariant violated: duplicate fn_var {d}; existing idx={d} name={s} origin={s} negated={} new idx={d} name={s} origin={s} negated={}",
                        .{
                            @intFromEnum(constraint.fn_var),
                            entry.value_ptr.*,
                            module.identStoreConst().getText(existing.fn_name),
                            @tagName(existing.origin),
                            existing.origin.binopNegated(),
                            i,
                            module.identStoreConst().getText(constraint.fn_name),
                            @tagName(constraint.origin),
                            constraint.origin.binopNegated(),
                        },
                    );
                }
                continue;
            }
            entry.value_ptr.* = @intCast(i);
        }

        return index;
    }

    fn lookup(self: *const StaticDispatchConstraintIndex, fn_var: Var) ?types.StaticDispatchConstraint {
        const constraint_idx = self.by_fn_var.get(fn_var) orelse return null;
        return self.constraints[constraint_idx];
    }

    fn deinit(self: *StaticDispatchConstraintIndex, allocator: Allocator) void {
        self.by_fn_var.deinit(allocator);
        self.* = .{};
    }
};

fn staticDispatchConstraintsEquivalent(a: types.StaticDispatchConstraint, b: types.StaticDispatchConstraint) bool {
    // origin now carries the binop-negation and literal payloads, so structural
    // equality of origin subsumes the former separate field comparisons.
    return a.fn_name == b.fn_name and
        a.fn_var == b.fn_var and
        std.meta.eql(a.origin, b.origin);
}

fn staticDispatchResultModeForCheckedValueCall(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    constraint_index: *const StaticDispatchConstraintIndex,
    method_name: Ident.Idx,
    constraint_fn_var: Var,
) Allocator.Error!StaticDispatchResultMode {
    const common = module.commonIdents();
    if (method_name.eql(common.to_hash)) {
        if (sourceCallableHasHashShape(module, constraint_fn_var)) {
            return .{ .hash = .{ .structural_allowed = true } };
        }
        return .value;
    }
    if (method_name.eql(common.parser_for)) {
        return .{ .parser_for = .{
            .structural_allowed = true,
        } };
    }
    if (method_name.eql(common.encoder_for)) {
        return .{ .encoder_for = .{
            .structural_allowed = true,
        } };
    }

    if (!method_name.eql(common.is_eq)) return .value;

    if (constraint_index.lookup(constraint_fn_var)) |constraint| {
        if (constraint.origin == .desugared_binop) {
            return .{ .equality = .{
                .structural_allowed = true,
                .negated = constraint.origin.binopNegated(),
            } };
        }
    }

    if (try sourceCallableHasEqualityShape(allocator, module, checked_types, constraint_fn_var)) {
        return .{ .equality = .{
            .structural_allowed = true,
            .negated = false,
        } };
    }

    return .value;
}

/// True when `fn_var` has the `to_hash` shape `(self, Hasher) -> Hasher`: two
/// arguments where the second (the Hasher) is threaded straight through to the
/// return type.
fn sourceCallableHasHashShape(
    module: TypedCIR.Module,
    fn_var: Var,
) bool {
    const store = module.typeStoreConst();
    const resolved = store.resolveVar(fn_var);
    const func = resolved.desc.content.unwrapFunc() orelse return false;
    const args = store.sliceVars(func.args);
    // `to_hash : self, Hasher -> Hasher` always has two arguments. Arity is the
    // only check needed here: the `to_hash` method name has already been matched
    // and this is only reached for an anonymous-structural dispatcher with no
    // method owner, so the constraint is the derived to_hash signature. (Unlike
    // the equality-shape check we cannot tie the second arg to the return — the
    // two `Hasher` occurrences are distinct vars, not a shared one like is_eq's
    // `self`, and there is no builtin-Hasher owner to match against.)
    return args.len == 2;
}

fn sourceCallableHasEqualityShape(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    fn_var: Var,
) Allocator.Error!bool {
    const store = module.typeStoreConst();
    const resolved = store.resolveVar(fn_var);
    const func = resolved.desc.content.unwrapFunc() orelse return false;
    const args = store.sliceVars(func.args);
    if (args.len != 2) return false;
    if (store.resolveVar(args[0]).var_ != store.resolveVar(args[1]).var_) return false;
    const ret_ty = try checkedTypeIdForVar(allocator, module, checked_types, func.ret);
    return checkedTypeIsBuiltinBool(checked_types, ret_ty);
}

fn checkedTypeIsBuiltinBool(checked_types: anytype, ty: CheckedTypeId) bool {
    const raw = @intFromEnum(ty);
    if (raw >= checked_types.store.payloadCount()) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: equality return type root was outside the checked type store", .{});
        }
        unreachable;
    }
    return switch (checked_types.store.payload(ty)) {
        .nominal => |nominal| if (nominal.builtin) |builtin_owner| builtin_owner == .bool else false,
        else => false,
    };
}

/// Public `methodOwnerForCheckedType` declaration: the method owner of a
/// published checked type, walking alias chains transparently.
pub fn methodOwnerForCheckedType(checked_types: anytype, ty: CheckedTypeId) ?MethodOwner {
    var current = ty;
    // Aliases are transparent for static dispatch: an alias's method owner is its
    // backing's owner. Walk the (finite) alias chain so an alias-over-nominal,
    // alias-over-alias, or alias-over-builtin resolves to the underlying owner
    // rather than the alias's own identity, where no methods are registered. The
    // bound on iterations is the store size, so a cyclic chain cannot loop here.
    var remaining = checked_types.store.payloads.items.len;
    while (true) {
        const raw = @intFromEnum(current);
        if (raw >= checked_types.store.payloads.items.len) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("checked static dispatch invariant violated: dispatcher type root was outside the checked type store", .{});
            }
            unreachable;
        }
        switch (checked_types.store.payloads.items[raw]) {
            .alias => |alias| {
                if (remaining == 0) {
                    if (@import("builtin").mode == .Debug) {
                        std.debug.panic("checked static dispatch invariant violated: checked type alias chain was cyclic", .{});
                    }
                    unreachable;
                }
                remaining -= 1;
                current = alias.backing;
            },
            else => |payload| return methodOwnerForCheckedPayload(payload),
        }
    }
}

fn methodOwnerForCheckedPayload(payload: anytype) ?MethodOwner {
    return switch (payload) {
        .nominal => |nominal| if (nominal.builtin) |builtin|
            .{ .builtin = builtinOwnerForCheckedBuiltin(builtin) }
        else
            .{ .nominal = .{
                .module = nominal.origin_module,
                .type_name = nominal.name,
                .source_decl = nominal.source_decl,
            } },
        else => null,
    };
}

/// Public `builtinOwnerForCheckedBuiltin` declaration: the registry owner key
/// for a checked builtin nominal.
pub fn builtinOwnerForCheckedBuiltin(builtin: anytype) BuiltinOwner {
    return switch (builtin) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .list => .list,
        .box => .box,
        .dict => .dict,
        .set => .set,
        .fields => .fields,
        .field => .field,
        .parse_tag_union_spec => .parse_tag_union_spec,
        .crypto_sha256_digest => .crypto_sha256_digest,
        .crypto_sha256_hasher => .crypto_sha256_hasher,
        .crypto_blake3_digest => .crypto_blake3_digest,
        .crypto_blake3_hasher => .crypto_blake3_hasher,
    };
}

/// Public `lookupCheckedMethodTarget` declaration: exact registry lookup in
/// the local registry, then the imported views.
pub fn lookupCheckedMethodTarget(
    names: *canonical.CanonicalNameStore,
    local_method_registry: *const MethodRegistry,
    imported_views: anytype,
    owner: MethodOwner,
    method: canonical.MethodNameId,
) ?MethodTarget {
    if (local_method_registry.lookup(.{ .owner = owner, .method = method })) |target| return target;

    const method_name = names.methodNameText(method);
    for (imported_views) |imported| {
        const imported_owner = methodOwnerInImportedStore(names, imported.canonical_names, owner) orelse continue;
        const imported_method = imported.canonical_names.lookupMethodName(method_name) orelse continue;
        if (imported.method_registry.lookup(.{ .owner = imported_owner, .method = imported_method })) |target| {
            switch (target.kind) {
                .procedure => return target,
                .generated_structural_parser,
                .generated_structural_encoder,
                => return target,
                .local_proc => continue,
            }
        }
    }
    return null;
}

/// Rebase a method owner into an imported artifact's store: the module
/// component crosses by 32-byte content identity (one map probe, full-value
/// comparison), the type-name component by declared-name interning. This is
/// the single cross-artifact owner resolution point — no module name text.
fn methodOwnerInImportedStore(
    source_names: *const canonical.CanonicalNameStore,
    imported_names: *const canonical.CanonicalNameStore,
    owner: MethodOwner,
) ?MethodOwner {
    return switch (owner) {
        .builtin => |builtin| .{ .builtin = builtin },
        .nominal => |nominal| .{ .nominal = .{
            .module = imported_names.lookupModuleIdentity(source_names.moduleIdentityBytes(nominal.module)) orelse return null,
            .type_name = imported_names.lookupTypeName(source_names.typeNameText(nominal.type_name)) orelse return null,
            .source_decl = nominal.source_decl,
        } },
    };
}

fn checkedTypeIdForVar(
    _: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    var_: Var,
) Allocator.Error!CheckedTypeId {
    return checked_types.rootForSourceVar(module, var_) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: dispatch type root was not published", .{});
        }
        unreachable;
    };
}

fn interpolationDispatcherTypeId(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!CheckedTypeId {
    const suffix_target = module.moduleEnvConst().numericSuffixTargetForNode(ModuleEnv.nodeIdxFrom(expr_idx)) orelse
        return checkedTypeIdForVar(allocator, module, checked_types, module.exprType(expr_idx));

    return switch (suffix_target.target()) {
        .local => |stmt_idx| checkedTypeIdForVar(allocator, module, checked_types, ModuleEnv.varFrom(stmt_idx)),
        .invalid => checkedTypeIdForVar(allocator, module, checked_types, module.exprType(expr_idx)),
        .builtin, .external => if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: interpolation suffix target was not published as a local type", .{});
        } else unreachable,
    };
}

/// Source-var mirror of `interpolationDispatcherTypeId`, for the plan-source
/// side data the total-resolution pass consumes.
fn interpolationDispatcherVar(module: TypedCIR.Module, expr_idx: CIR.Expr.Idx) Var {
    const suffix_target = module.moduleEnvConst().numericSuffixTargetForNode(ModuleEnv.nodeIdxFrom(expr_idx)) orelse
        return module.exprType(expr_idx);

    return switch (suffix_target.target()) {
        .local => |stmt_idx| ModuleEnv.varFrom(stmt_idx),
        .invalid => module.exprType(expr_idx),
        .builtin, .external => if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: interpolation suffix target was not published as a local type", .{});
        } else unreachable,
    };
}

fn checkedFunctionReturnTypeId(
    checked_types: anytype,
    callable_ty: CheckedTypeId,
) CheckedTypeId {
    const raw = @intFromEnum(callable_ty);
    if (raw >= checked_types.store.payloadCount()) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: callable type root was outside the checked type store", .{});
        }
        unreachable;
    }
    return switch (checked_types.store.payload(callable_ty)) {
        .function => |func| func.ret,
        else => if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: for-loop dispatch constraint was not a function", .{});
        } else unreachable,
    };
}

fn staticDispatchOperandsForSlice(
    allocator: Allocator,
    checked_bodies: anytype,
    exprs: []const CIR.Expr.Idx,
) Allocator.Error![]const StaticDispatchOperand {
    if (exprs.len == 0) return &.{};
    const out = try allocator.alloc(StaticDispatchOperand, exprs.len);
    errdefer allocator.free(out);
    for (exprs, 0..) |expr, i| {
        out[i] = .{ .checked_expr = checkedExprIdForSource(checked_bodies, expr) };
    }
    return out;
}

fn checkedExprIdForSource(checked_bodies: anytype, expr: CIR.Expr.Idx) CheckedExprId {
    return checked_bodies.exprIdForSource(expr) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch invariant violated: dispatch expression {d} has no checked expression id",
                .{@intFromEnum(expr)},
            );
        }
        unreachable;
    };
}

test "method registry can be empty" {
    var registry: MethodRegistry = .{};
    registry.deinit(std.testing.allocator);
}

test "method registry finalization sorts entries for binary lookup" {
    const allocator = std.testing.allocator;

    const entries = try allocator.alloc(MethodRegistryEntry, 3);
    defer allocator.free(entries);

    entries[0] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @enumFromInt(2) },
        .target = testMethodTarget(@enumFromInt(20)),
    };
    entries[1] = .{
        .key = .{ .owner = .{ .builtin = .list }, .method = @enumFromInt(1) },
        .target = testMethodTarget(@enumFromInt(10)),
    };
    entries[2] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @enumFromInt(1) },
        .target = testMethodTarget(@enumFromInt(15)),
    };

    finalizeMethodRegistryEntries(entries);

    var registry = MethodRegistry{ .entries = entries };
    const found = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @enumFromInt(1) }) orelse return error.MissingSortedMethodTarget;
    try std.testing.expectEqual(@as(CIR.Def.Idx, @enumFromInt(15)), found.def_idx);
    try std.testing.expect(registry.lookup(.{ .owner = .{ .builtin = .list }, .method = @enumFromInt(2) }) == null);
}

fn testPlan(expr_raw: u32, args_start: u32, args_len: u32) StaticDispatchCallPlan {
    return .{
        .expr = @enumFromInt(expr_raw),
        .method = @enumFromInt(1),
        .dispatcher = .{ .arg = 0 },
        .dispatcher_ty = @enumFromInt(2),
        .callable_ty = @enumFromInt(3),
        .args = .{ .start = args_start, .len = args_len },
        .result_mode = .value,
    };
}

test "StaticDispatchPlanTable: relocates with a constant number of fixups, operands resolve post-deserialize" {
    const gpa = std.testing.allocator;

    // The fixup count is fixed by the number of serialized base pointers, never
    // by how much data each pool holds. The two tables below differ in operand
    // count by three orders of magnitude yet relocate identically.
    comptime std.debug.assert(@typeInfo(StaticDispatchPlanTable.Serialized).@"struct".fields.len == 12);

    inline for (.{ @as(u32, 4), @as(u32, 4000) }) |operand_count| {
        const operands = try gpa.alloc(StaticDispatchOperand, operand_count);
        defer gpa.free(operands);
        for (operands, 0..) |*op, i| op.* = .{ .checked_expr = @enumFromInt(@as(u32, @intCast(i)) + 100) };

        var plans = [_]StaticDispatchCallPlan{
            testPlan(10, 0, 2),
            testPlan(11, 2, operand_count - 2),
        };
        var by_expr = [_]PlanKV{
            .{ .key = 10, .val = 0 },
            .{ .key = 11, .val = 1 },
        };

        const table = StaticDispatchPlanTable{
            .plans = &plans,
            .by_expr = &by_expr,
            .operand_pool = operands,
        };

        const rt = try artifact_serialize.roundTripForTest(gpa, StaticDispatchPlanTable, &table);
        defer gpa.free(rt.buffer);

        const loaded = rt.loaded;
        try std.testing.expectEqual(@as(usize, 2), loaded.plans.len);
        try std.testing.expectEqual(@as(usize, operand_count), loaded.operand_pool.len);

        const first_args = loaded.plans[0].argsSlice(&loaded);
        try std.testing.expectEqual(@as(usize, 2), first_args.len);
        try std.testing.expectEqual(@as(CheckedExprId, @enumFromInt(100)), first_args[0].checked_expr);

        const second_args = loaded.plans[1].argsSlice(&loaded);
        try std.testing.expectEqual(@as(usize, operand_count - 2), second_args.len);
        try std.testing.expectEqual(
            @as(CheckedExprId, @enumFromInt(operand_count - 1 + 100)),
            second_args[second_args.len - 1].checked_expr,
        );

        try std.testing.expectEqual(@as(?u32, 1), lookupPlanKV(loaded.by_expr, 11));
    }
}

fn testMethodTarget(def_idx: CIR.Def.Idx) MethodTarget {
    return .{
        .module_idx = 0,
        .def_idx = def_idx,
        .kind = .{
            .local_proc = .{
                .binder = undefined, // The lookup test only asserts def_idx; target kind is never read.
                .expr = undefined, // The lookup test only asserts def_idx; target kind is never read.
            },
        },
        .callable_ty = undefined, // The lookup test only asserts def_idx; callable type is never read.
    };
}

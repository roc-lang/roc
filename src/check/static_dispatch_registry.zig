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
pub const MethodOwner = union(enum) {
    nominal: canonical.NominalTypeKey,
    source_decl: struct {
        module_name: canonical.ModuleNameId,
        statement: u32,
    },
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
        const found = artifact_serialize.binarySearchByKey(MethodRegistryEntry, MethodKey, self.entries, key, methodEntryOrder) orelse return null;
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
            const method_ident = module_env.lookupMethodIdentForOwnerConst(entry.key.owner, entry.key.methodIdent()) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch registry invariant violated: method def for owner {d} method {d} has no method ident",
                        .{ @intFromEnum(entry.key.owner), entry.key.method_ident_bits },
                    );
                }
                unreachable;
            };
            const def_idx = entry.value.def_idx;
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
            else
                // Associated values without arguments are checked field access,
                // not static-dispatch call targets. The method registry is a
                // procedure-target table for Monotype static dispatch lowering,
                // so only procedure-backed entries belong here.
                continue;
            const callable_var = methodTargetCallableVar(module, def_idx, entry.value, target_kind);

            try entries.append(allocator, .{
                .key = .{
                    .owner = try methodOwnerForRegistryEntry(module, module_name, entry.key.owner),
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
    if (method_ident.eql(common.encode_to)) return .generated_structural_encoder;
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

fn methodOwnerForRegistryEntry(
    module: TypedCIR.Module,
    module_name: canonical.ModuleNameId,
    owner_stmt: CIR.Statement.Idx,
) Allocator.Error!MethodOwner {
    if (builtinOwnerForRegistryEntry(module, owner_stmt)) |owner| {
        return .{ .builtin = owner };
    }
    return .{ .source_decl = .{
        .module_name = module_name,
        .statement = @intFromEnum(owner_stmt),
    } };
}

fn builtinOwnerForRegistryEntry(
    module: TypedCIR.Module,
    owner_stmt: CIR.Statement.Idx,
) ?BuiltinOwner {
    const module_env = module.moduleEnvConst();
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
    const a_tag = methodOwnerTagRank(a);
    const b_tag = methodOwnerTagRank(b);
    if (a_tag != b_tag) return orderU32(a_tag, b_tag);

    return switch (a) {
        .nominal => |a_nominal| switch (b) {
            .nominal => |b_nominal| blk: {
                const module_order = orderEnum(canonical.ModuleNameId, a_nominal.module_name, b_nominal.module_name);
                if (module_order != .eq) break :blk module_order;
                const type_order = orderEnum(canonical.TypeNameId, a_nominal.type_name, b_nominal.type_name);
                if (type_order != .eq) break :blk type_order;
                break :blk orderOptionalU32(a_nominal.source_decl, b_nominal.source_decl);
            },
            else => unreachable,
        },
        .source_decl => |a_decl| switch (b) {
            .source_decl => |b_decl| blk: {
                const module_order = orderEnum(canonical.ModuleNameId, a_decl.module_name, b_decl.module_name);
                if (module_order != .eq) break :blk module_order;
                break :blk orderU32(a_decl.statement, b_decl.statement);
            },
            else => unreachable,
        },
        .builtin => |a_builtin| switch (b) {
            .builtin => |b_builtin| orderEnum(BuiltinOwner, a_builtin, b_builtin),
            else => unreachable,
        },
    };
}

fn methodOwnerTagRank(owner: MethodOwner) u32 {
    return switch (owner) {
        .nominal => 0,
        .source_decl => 1,
        .builtin => 2,
    };
}

fn orderOptionalU32(a: ?u32, b: ?u32) std.math.Order {
    if (a) |a_value| {
        return if (b) |b_value| orderU32(a_value, b_value) else .gt;
    }
    return if (b == null) .eq else .lt;
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
    encode_to: struct {
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

/// Public `StaticDispatchResolution` declaration.
pub const StaticDispatchResolution = union(enum) {
    /// The dispatch target was not published as a concrete procedure target in
    /// this checked module's static-dispatch inputs.
    unresolved_checked_plan,
    /// Checking proved the concrete target. Later stages must call this target
    /// directly instead of rediscovering it from source or type names.
    resolved_target: MethodTarget,
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
    resolution: StaticDispatchResolution,

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

        comptime {
            // 9 side lists → 9 base-pointer fixups on deserialize, never a
            // function of how many plans/operands the table holds.
            std.debug.assert(artifact_serialize.relocatablePointerCount(Serialized) == 9);
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
        local_method_registry: *const MethodRegistry,
        imported_views: anytype,
    ) Allocator.Error!StaticDispatchPlanTable {
        var plans = std.ArrayList(StaticDispatchCallPlan).empty;
        errdefer plans.deinit(allocator);
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

                    const plan = StaticDispatchCallPlan{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher = .{ .arg = 0 },
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(dispatch_call.receiver)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = ar,
                        .result_mode = try staticDispatchResultModeForCheckedValueCall(allocator, module, checked_types, &constraint_index, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                        .resolution = .unresolved_checked_plan,
                    };
                    try plans.append(allocator, resolveStaticDispatchPlan(names, checked_types, local_method_registry, imported_views, plan));
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

                    const plan = StaticDispatchCallPlan{
                        .expr = checked_expr,
                        .method = from_interpolation,
                        .dispatcher = .type_only,
                        .dispatcher_ty = try interpolationDispatcherTypeId(allocator, module, checked_types, expr_idx),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, constraint_fn_var),
                        .args = ar,
                        .result_mode = .value,
                        .resolution = .unresolved_checked_plan,
                    };
                    try plans.append(allocator, resolveStaticDispatchPlan(names, checked_types, local_method_registry, imported_views, plan));
                },
                .e_type_dispatch_call => |dispatch_call| {
                    const args = try staticDispatchOperandsForSlice(allocator, checked_bodies, module.sliceExpr(dispatch_call.args));
                    defer allocator.free(args);
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    const plan = StaticDispatchCallPlan{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher = .type_only,
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, typeDispatchOwnerVar(module, dispatch_call.type_dispatch_stmt)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = ar,
                        .result_mode = try staticDispatchResultModeForCheckedValueCall(allocator, module, checked_types, &constraint_index, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                        .resolution = .unresolved_checked_plan,
                    };
                    try plans.append(allocator, resolveStaticDispatchPlan(names, checked_types, local_method_registry, imported_views, plan));
                },
                .e_method_eq => |eq| {
                    const args = try staticDispatchOperandsForSlice(allocator, checked_bodies, &.{ eq.lhs, eq.rhs });
                    defer allocator.free(args);
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    const plan = StaticDispatchCallPlan{
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
                        .resolution = .unresolved_checked_plan,
                    };
                    try plans.append(allocator, resolveStaticDispatchPlan(names, checked_types, local_method_registry, imported_views, plan));
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
            var args = [_]StaticDispatchOperand{.{ .generated_numeral = literal }};
            const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, &args);

            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            const plan = StaticDispatchCallPlan{
                .expr = checked_expr,
                .method = try names.internMethodName("from_numeral"),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(numeral_plan.target_var)),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(numeral_plan.fn_var)),
                .args = ar,
                .result_mode = .value,
                .resolution = .unresolved_checked_plan,
            };
            try plans.append(allocator, resolveStaticDispatchPlan(names, checked_types, local_method_registry, imported_views, plan));
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
            const plan = StaticDispatchCallPlan{
                .expr = checked_expr,
                .method = try names.internMethodName("from_quote"),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(quote_plan.target_var)),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(quote_plan.fn_var)),
                .args = ar,
                .result_mode = .value,
                .resolution = .unresolved_checked_plan,
            };
            try plans.append(allocator, resolveStaticDispatchPlan(names, checked_types, local_method_registry, imported_views, plan));
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
    if (method_name.eql(common.encode_to)) {
        return .{ .encode_to = .{
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

fn resolveStaticDispatchPlan(
    names: *canonical.CanonicalNameStore,
    checked_types: anytype,
    local_method_registry: *const MethodRegistry,
    imported_views: anytype,
    plan: StaticDispatchCallPlan,
) StaticDispatchCallPlan {
    const owner = methodOwnerForCheckedType(checked_types, plan.dispatcher_ty) orelse return plan;
    const target = lookupCheckedMethodTarget(names, local_method_registry, imported_views, owner, plan.method) orelse {
        if (plan.result_mode == .equality and plan.result_mode.equality.structural_allowed) return plan;
        return plan;
    };

    var resolved = plan;
    resolved.resolution = .{ .resolved_target = target };
    return resolved;
}

fn methodOwnerForCheckedType(checked_types: anytype, ty: CheckedTypeId) ?MethodOwner {
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
        else if (nominal.source_decl) |source_decl|
            .{ .source_decl = .{
                .module_name = nominal.origin_module,
                .statement = source_decl,
            } }
        else
            .{ .nominal = .{
                .module_name = nominal.origin_module,
                .type_name = nominal.name,
                .source_decl = null,
            } },
        else => null,
    };
}

fn builtinOwnerForCheckedBuiltin(builtin: anytype) BuiltinOwner {
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

fn lookupCheckedMethodTarget(
    names: *canonical.CanonicalNameStore,
    local_method_registry: *const MethodRegistry,
    imported_views: anytype,
    owner: MethodOwner,
    method: canonical.MethodNameId,
) ?MethodTarget {
    if (local_method_registry.lookup(.{ .owner = owner, .method = method })) |target| return target;

    const method_name = names.methodNameText(method);
    for (imported_views) |imported| {
        const imported_owner = methodOwnerInImportedNames(names, imported.canonical_names, owner) orelse continue;
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

fn methodOwnerInImportedNames(
    source_names: *const canonical.CanonicalNameStore,
    imported_names: *const canonical.CanonicalNameStore,
    owner: MethodOwner,
) ?MethodOwner {
    return switch (owner) {
        .builtin => |builtin| .{ .builtin = builtin },
        .source_decl => |decl| .{ .source_decl = .{
            .module_name = imported_names.lookupModuleName(source_names.moduleNameText(decl.module_name)) orelse return null,
            .statement = decl.statement,
        } },
        .nominal => |nominal| .{ .nominal = .{
            .module_name = imported_names.lookupModuleName(source_names.moduleNameText(nominal.module_name)) orelse return null,
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
        .resolution = .unresolved_checked_plan,
    };
}

test "StaticDispatchPlanTable: relocates with a constant number of fixups, operands resolve post-deserialize" {
    const gpa = std.testing.allocator;

    // The fixup count is fixed by the number of serialized base pointers, never
    // by how much data each pool holds. The two tables below differ in operand
    // count by three orders of magnitude yet relocate identically.
    comptime std.debug.assert(@typeInfo(StaticDispatchPlanTable.Serialized).@"struct".fields.len == 9);

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

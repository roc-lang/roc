//! Specialization-driven mono MIR construction state.
//!
//! This is the only API that may turn checked procedure templates into mono MIR
//! procedures. It reserves procedure identities before body lowering so recursive
//! and mutually-recursive mono specializations cannot allocate duplicate
//! procedure values.

const std = @import("std");
const check = @import("check");
const base = @import("base");
const symbol_mod = @import("symbol");

const Ast = @import("ast.zig");
const ConcreteSourceType = @import("../concrete_source_type.zig");
const ArtifactNames = @import("../artifact_names.zig");
const ids = @import("../ids.zig");
const LowerType = @import("lower_type.zig");
const Type = @import("type.zig");
const debug = @import("../debug_verify.zig");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const static_dispatch = check.StaticDispatchRegistry;

pub const MonoProcHandle = enum(u32) { _ };

pub const MonoSpecializationReason = union(enum) {
    root: checked_artifact.RootRequest,
    call_proc: Ast.ExprId,
    proc_value: Ast.ExprId,
    static_dispatch_target: checked_artifact.RootSource,
    comptime_dependency_summary: u32,
};

pub const Input = struct {
    root: checked_artifact.LoweringModuleView,
    imports: []const checked_artifact.ImportedModuleView = &.{},
};

pub const MonoSpecializationRequest = struct {
    template: canonical.ProcedureTemplateRef,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    reason: MonoSpecializationReason,
};

pub const ReservedState = enum {
    reserved,
    lowering,
    lowered,
};

pub const ReservedMonoProc = struct {
    proc: canonical.MonoSpecializedProcRef,
    local_handle: MonoProcHandle,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    state: ReservedState,
};

pub const Proc = struct {
    key: canonical.MonoSpecializationKey,
    proc: canonical.MonoSpecializedProcRef,
    local_handle: MonoProcHandle,
    fn_ty: Type.TypeId,
    body: Ast.DefId,
};

pub const Program = struct {
    allocator: Allocator,
    root_artifact_key: checked_artifact.CheckedModuleArtifactKey,
    canonical_names: canonical.CanonicalNameStore,
    concrete_source_types: ConcreteSourceType.Store,
    literal_pool: ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    root_procs: std.ArrayList(canonical.MonoSpecializedProcRef),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .root_artifact_key = .{},
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
            .concrete_source_types = ConcreteSourceType.Store.init(allocator),
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .root_procs = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.root_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.concrete_source_types.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }

    pub fn addProc(
        self: *Program,
        key: canonical.MonoSpecializationKey,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
        body: Ast.DefId,
    ) Allocator.Error!void {
        try self.procs.append(self.allocator, .{
            .key = key,
            .proc = reserved.proc,
            .local_handle = reserved.local_handle,
            .fn_ty = fn_ty,
            .body = body,
        });
    }

    pub fn addPatternBinderSymbol(
        self: *Program,
        binder: checked_artifact.PatternBinderId,
    ) Allocator.Error!Ast.Symbol {
        return try self.symbols.add(base.Ident.Idx.NONE, .{ .checked_pattern_binder = .{
            .binder_idx = @intFromEnum(binder),
        } });
    }

    pub fn addProcSymbol(
        self: *Program,
        handle: MonoProcHandle,
    ) Allocator.Error!Ast.Symbol {
        return try self.symbols.add(base.Ident.Idx.NONE, .{ .specialized_top_level_def = .{
            .source_symbol = @intFromEnum(handle),
        } });
    }
};

pub fn run(
    allocator: Allocator,
    input: Input,
    roots: []const checked_artifact.RootRequest,
) Allocator.Error!Program {
    var program = Program.init(allocator);
    errdefer program.deinit();
    program.root_artifact_key = input.root.artifact.key;

    var queue = Queue.init(allocator);
    defer queue.deinit();
    var name_resolver = ArtifactNames.ArtifactNameResolver.init(
        &program.canonical_names,
        input.root.artifact,
        input.imports,
        input.root.relation_artifacts,
    );

    for (roots) |root| {
        const template = templateForRoot(input, root) orelse continue;
        const requested_fn_ty = try program.concrete_source_types.registerArtifactRoot(
            input.root.artifact.key,
            input.root.artifact.checked_types.view(),
            root.checked_type,
        );
        const request = MonoSpecializationRequest{
            .template = template,
            .requested_fn_ty = requested_fn_ty,
            .reason = .{ .root = root },
        };
        const reserved = try queue.reserve(&program.concrete_source_types, request);
        try program.root_procs.append(allocator, reserved.proc);
    }

    while (queue.pending.items.len != 0) {
        const key = queue.pending.orderedRemove(0);
        queue.markLowering(key);
        const reserved = queue.requested.get(key) orelse unreachable;
        const template_lookup = checkedTemplateForKey(input, key.template);
        var type_instantiator = TypeInstantiator.init(allocator, input, &program, template_lookup.checked_types, &name_resolver, template_lookup.artifact);
        defer type_instantiator.deinit();
        try type_instantiator.buildFromRequest(template_lookup.template.checked_fn_root, reserved.requested_fn_ty);
        const fn_ty = try type_instantiator.lowerTemplateType(template_lookup.template.checked_fn_root);
        var body_lowerer = BodyLowerer.init(allocator, input, &program, template_lookup, &type_instantiator, &name_resolver);
        defer body_lowerer.deinit();
        const body = try body_lowerer.lowerTemplateBody(reserved, fn_ty);
        try program.addProc(key, reserved, fn_ty, body);
        queue.markLowered(key);
    }

    verifyProgram(&program);
    return program;
}

const CheckedTemplateLookup = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    checked_types: checked_artifact.CheckedTypeStoreView,
    checked_bodies: checked_artifact.CheckedBodyStoreView,
    resolved_value_refs: *const checked_artifact.ResolvedValueRefTable,
    template: checked_artifact.CheckedProcedureTemplate,
};

fn checkedTemplateForKey(
    input: Input,
    template_ref: canonical.ProcedureTemplateRef,
) CheckedTemplateLookup {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &template_ref.artifact.bytes)) {
        return .{
            .artifact = input.root.artifact.key,
            .checked_types = input.root.artifact.checked_types.view(),
            .checked_bodies = input.root.artifact.checked_bodies.view(),
            .resolved_value_refs = &input.root.artifact.resolved_value_refs,
            .template = input.root.artifact.checked_procedure_templates.get(template_ref.template),
        };
    }

    for (input.imports) |imported| {
        if (!std.mem.eql(u8, &imported.key.bytes, &template_ref.artifact.bytes)) continue;
        for (imported.exported_procedure_templates.templates) |exported| {
            if (exported.template.proc_base == template_ref.proc_base and
                exported.template.template == template_ref.template)
            {
                return .{
                    .artifact = imported.key,
                    .checked_types = imported.checked_types,
                    .checked_bodies = imported.checked_bodies,
                    .resolved_value_refs = imported.resolved_value_refs,
                    .template = exported.template_data,
                };
            }
        }
        debug.invariant(false, "mono specialization invariant violated: imported template was not exported or present in the imported closure");
        unreachable;
    }

    for (input.root.relation_artifacts) |related| {
        if (!std.mem.eql(u8, &related.key.bytes, &template_ref.artifact.bytes)) continue;
        for (related.exported_procedure_templates.templates) |exported| {
            if (exported.template.proc_base == template_ref.proc_base and
                exported.template.template == template_ref.template)
            {
                return .{
                    .artifact = related.key,
                    .checked_types = related.checked_types,
                    .checked_bodies = related.checked_bodies,
                    .resolved_value_refs = related.resolved_value_refs,
                    .template = exported.template_data,
                };
            }
        }
        debug.invariant(false, "mono specialization invariant violated: relation template was not exported or present in the relation closure");
        unreachable;
    }

    debug.invariant(false, "mono specialization invariant violated: template artifact was not available to lowering");
    unreachable;
}

const TypeInstantiator = struct {
    allocator: Allocator,
    input: Input,
    program: *Program,
    template_types: checked_artifact.CheckedTypeStoreView,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    template_artifact: checked_artifact.CheckedModuleArtifactKey,
    substitutions: std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef),
    lowered_template: std.AutoHashMap(checked_artifact.CheckedTypeId, Type.TypeId),
    materialized_template_roots: std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    materialized_concrete_roots: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId),
    concrete_template_refs: std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef),

    fn init(
        allocator: Allocator,
        input: Input,
        program: *Program,
        template_types: checked_artifact.CheckedTypeStoreView,
        name_resolver: *ArtifactNames.ArtifactNameResolver,
        template_artifact: checked_artifact.CheckedModuleArtifactKey,
    ) TypeInstantiator {
        return .{
            .allocator = allocator,
            .input = input,
            .program = program,
            .template_types = template_types,
            .name_resolver = name_resolver,
            .template_artifact = template_artifact,
            .substitutions = std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef).init(allocator),
            .lowered_template = std.AutoHashMap(checked_artifact.CheckedTypeId, Type.TypeId).init(allocator),
            .materialized_template_roots = std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId).init(allocator),
            .materialized_concrete_roots = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId).init(allocator),
            .concrete_template_refs = std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef).init(allocator),
        };
    }

    fn deinit(self: *TypeInstantiator) void {
        self.concrete_template_refs.deinit();
        self.materialized_concrete_roots.deinit();
        self.materialized_template_roots.deinit();
        self.lowered_template.deinit();
        self.substitutions.deinit();
    }

    fn buildFromRequest(
        self: *TypeInstantiator,
        template_fn_root: checked_artifact.CheckedTypeId,
        requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        try self.unifyTemplateWithConcrete(template_fn_root, requested_fn_ty);
    }

    fn lowerTemplateType(self: *TypeInstantiator, id: checked_artifact.CheckedTypeId) Allocator.Error!Type.TypeId {
        if (self.substitutions.get(id)) |concrete| {
            return try self.lowerConcreteRef(concrete);
        }
        if (self.lowered_template.get(id)) |existing| return existing;

        const placeholder = try self.program.types.addType(.placeholder);
        try self.lowered_template.put(id, placeholder);
        const lowered = try self.lowerTemplatePayload(self.templatePayload(id));
        self.program.types.setType(placeholder, lowered);
        self.program.types.debugValidateTypeGraph(placeholder);
        return try self.program.types.internTypeId(placeholder);
    }

    fn concreteRefForTemplateType(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        if (self.substitutions.get(id)) |concrete| return concrete;
        if (self.concrete_template_refs.get(id)) |existing| return existing;

        const local_root = try self.materializeTemplateType(id);
        var key_builder = ConcreteSourceType.PayloadKeyBuilder.init(
            self.allocator,
            &self.program.canonical_names,
            self.program.concrete_source_types.local_payloads.items,
        );
        defer key_builder.deinit();
        const key = try key_builder.keyForRoot(local_root);
        const concrete = try self.program.concrete_source_types.sealLocalRoot(local_root, key);
        try self.concrete_template_refs.put(id, concrete);
        return concrete;
    }

    fn materializeTemplateType(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        if (self.substitutions.get(id)) |concrete| return try self.materializeConcreteRef(concrete);
        if (self.materialized_template_roots.get(id)) |existing| return existing;

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.materialized_template_roots.put(id, local_root);
        errdefer _ = self.materialized_template_roots.remove(id);

        const payload = try self.materializeTemplatePayload(self.templatePayload(id));
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        return local_root;
    }

    fn materializeTemplatePayload(
        self: *TypeInstantiator,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished checked type payload"),
            .flex, .rigid => invariantViolation("mono specialization reached an unmapped generic type variable while materializing a concrete source type"),
            .alias => |alias| .{ .alias = .{
                .name = try self.name_resolver.typeName(self.template_artifact, alias.name),
                .origin_module = try self.name_resolver.moduleName(self.template_artifact, alias.origin_module),
                .backing = try self.materializeTemplateType(alias.backing),
                .args = try self.materializeTemplateTypeIds(alias.args),
            } },
            .record_unbound => |fields| .{ .record_unbound = try self.materializeTemplateRecordFields(fields) },
            .record => |record| .{ .record = .{
                .fields = try self.materializeTemplateRecordFields(record.fields),
                .ext = try self.materializeTemplateType(record.ext),
            } },
            .tuple => |items| .{ .tuple = try self.materializeTemplateTypeIds(items) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.name_resolver.typeName(self.template_artifact, nominal.name),
                .origin_module = try self.name_resolver.moduleName(self.template_artifact, nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.materializeTemplateType(nominal.backing),
                .args = try self.materializeTemplateTypeIds(nominal.args),
            } },
            .function => |func| .{ .function = .{
                .kind = func.kind,
                .args = try self.materializeTemplateTypeIds(func.args),
                .ret = try self.materializeTemplateType(func.ret),
                .needs_instantiation = false,
            } },
            .empty_record => .empty_record,
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.materializeTemplateTags(tag_union.tags),
                .ext = try self.materializeTemplateType(tag_union.ext),
            } },
            .empty_tag_union => .empty_tag_union,
        };
    }

    fn materializeTemplateTypeIds(
        self: *TypeInstantiator,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const checked_artifact.CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.materializeTemplateType(id);
        }
        return out;
    }

    fn materializeTemplateRecordFields(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error![]const checked_artifact.CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name),
                .ty = try self.materializeTemplateType(field.ty),
            };
        }
        return out;
    }

    fn materializeTemplateTags(
        self: *TypeInstantiator,
        tags: []const checked_artifact.CheckedTag,
    ) Allocator.Error![]const checked_artifact.CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.name_resolver.tagLabel(self.template_artifact, tag.name),
                .args = try self.materializeTemplateTypeIds(tag.args),
            };
        }
        return out;
    }

    fn lowerTemplatePayload(
        self: *TypeInstantiator,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!Type.Content {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished checked type payload"),
            .flex, .rigid => invariantViolation("mono specialization reached an unmapped generic type variable"),
            .alias => |alias| .{ .link = try self.lowerTemplateType(alias.backing) },
            .record_unbound => |fields| .{ .record = .{ .fields = try self.lowerTemplateRecordFieldsOnly(fields) } },
            .record => |record| .{ .record = .{ .fields = try self.lowerTemplateRecord(record) } },
            .tuple => |elems| .{ .tuple = try self.lowerTemplateTypeIds(elems) },
            .nominal => |nominal| try self.lowerTemplateNominal(nominal),
            .function => |func| .{ .func = .{
                .args = try self.lowerTemplateTypeIds(func.args),
                .lambdas = &.{},
                .ret = try self.lowerTemplateType(func.ret),
            } },
            .empty_record => .{ .record = .{ .fields = &.{} } },
            .tag_union => |tag_union| .{ .tag_union = .{ .tags = try self.lowerTemplateTagUnion(tag_union) } },
            .empty_tag_union => .{ .tag_union = .{ .tags = &.{} } },
        };
    }

    fn lowerTemplateTypeIds(
        self: *TypeInstantiator,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const Type.TypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.TypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.lowerTemplateType(id);
        }
        return out;
    }

    fn lowerTemplateRecordFieldsOnly(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error![]const Type.Field {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.Field, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name),
                .ty = try self.lowerTemplateType(field.ty),
            };
        }
        return out;
    }

    fn lowerTemplateRecord(
        self: *TypeInstantiator,
        record: checked_artifact.CheckedRecordType,
    ) Allocator.Error![]const Type.Field {
        var fields = std.ArrayList(Type.Field).empty;
        errdefer fields.deinit(self.allocator);
        try self.collectTemplateRecordFields(record.fields, record.ext, &fields);
        std.mem.sort(Type.Field, fields.items, {}, struct {
            fn lessThan(_: void, a: Type.Field, b: Type.Field) bool {
                return @intFromEnum(a.name) < @intFromEnum(b.name);
            }
        }.lessThan);
        return try fields.toOwnedSlice(self.allocator);
    }

    fn collectTemplateRecordFields(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
        ext: checked_artifact.CheckedTypeId,
        out: *std.ArrayList(Type.Field),
    ) Allocator.Error!void {
        for (fields) |field| {
            try out.append(self.allocator, .{
                .name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name),
                .ty = try self.lowerTemplateType(field.ty),
            });
        }

        if (self.substitutions.get(ext)) |concrete| {
            try self.collectConcreteRecordFields(concrete, out);
            return;
        }

        switch (self.templatePayload(ext)) {
            .alias => |alias| try self.collectTemplateRecordFields(&.{}, alias.backing, out),
            .empty_record => {},
            .record_unbound => |ext_fields| {
                for (ext_fields) |field| {
                    try out.append(self.allocator, .{
                        .name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name),
                        .ty = try self.lowerTemplateType(field.ty),
                    });
                }
            },
            .record => |ext_record| try self.collectTemplateRecordFields(ext_record.fields, ext_record.ext, out),
            else => invariantViolation("mono specialization record extension resolved to a non-record type"),
        }
    }

    fn collectConcreteRecordFields(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        out: *std.ArrayList(Type.Field),
    ) Allocator.Error!void {
        const payload = self.concretePayload(ref);
        switch (payload) {
            .alias => |alias| try self.collectConcreteRecordFields(try self.concreteChildRef(ref, alias.backing), out),
            .empty_record => {},
            .record_unbound => |fields| {
                for (fields) |field| {
                    try out.append(self.allocator, .{
                        .name = try self.recordFieldNameForConcreteRef(ref, field.name),
                        .ty = try self.lowerConcreteRef(try self.concreteChildRef(ref, field.ty)),
                    });
                }
            },
            .record => |record| {
                for (record.fields) |field| {
                    try out.append(self.allocator, .{
                        .name = try self.recordFieldNameForConcreteRef(ref, field.name),
                        .ty = try self.lowerConcreteRef(try self.concreteChildRef(ref, field.ty)),
                    });
                }
                try self.collectConcreteRecordFields(try self.concreteChildRef(ref, record.ext), out);
            },
            else => invariantViolation("mono specialization concrete record extension resolved to a non-record type"),
        }
    }

    fn lowerTemplateTagUnion(
        self: *TypeInstantiator,
        tag_union: checked_artifact.CheckedTagUnionType,
    ) Allocator.Error![]const Type.Tag {
        var tags = std.ArrayList(Type.Tag).empty;
        errdefer {
            for (tags.items) |tag| self.allocator.free(tag.args);
            tags.deinit(self.allocator);
        }
        try self.collectTemplateTags(tag_union.tags, tag_union.ext, &tags);
        std.mem.sort(Type.Tag, tags.items, {}, struct {
            fn lessThan(_: void, a: Type.Tag, b: Type.Tag) bool {
                return @intFromEnum(a.name) < @intFromEnum(b.name);
            }
        }.lessThan);
        return try tags.toOwnedSlice(self.allocator);
    }

    fn collectTemplateTags(
        self: *TypeInstantiator,
        tags: []const checked_artifact.CheckedTag,
        ext: checked_artifact.CheckedTypeId,
        out: *std.ArrayList(Type.Tag),
    ) Allocator.Error!void {
        for (tags) |tag| {
            try out.append(self.allocator, .{
                .name = try self.name_resolver.tagLabel(self.template_artifact, tag.name),
                .args = try self.lowerTemplateTypeIds(tag.args),
            });
        }

        if (self.substitutions.get(ext)) |concrete| {
            try self.collectConcreteTags(concrete, out);
            return;
        }

        switch (self.templatePayload(ext)) {
            .alias => |alias| try self.collectTemplateTags(&.{}, alias.backing, out),
            .empty_tag_union => {},
            .tag_union => |ext_tags| try self.collectTemplateTags(ext_tags.tags, ext_tags.ext, out),
            else => invariantViolation("mono specialization tag-union extension resolved to a non-tag-union type"),
        }
    }

    fn collectConcreteTags(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        out: *std.ArrayList(Type.Tag),
    ) Allocator.Error!void {
        switch (self.concretePayload(ref)) {
            .alias => |alias| try self.collectConcreteTags(try self.concreteChildRef(ref, alias.backing), out),
            .empty_tag_union => {},
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| {
                    try out.append(self.allocator, .{
                        .name = try self.tagNameForConcreteRef(ref, tag.name),
                        .args = try self.lowerConcreteTypeIds(ref, tag.args),
                    });
                }
                try self.collectConcreteTags(try self.concreteChildRef(ref, tag_union.ext), out);
            },
            else => invariantViolation("mono specialization concrete tag-union extension resolved to a non-tag-union type"),
        }
    }

    fn lowerConcreteTypeIds(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const Type.TypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.TypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.lowerConcreteRef(try self.concreteChildRef(parent, id));
        }
        return out;
    }

    fn lowerTemplateNominal(
        self: *TypeInstantiator,
        nominal: checked_artifact.CheckedNominalType,
    ) Allocator.Error!Type.Content {
        if (nominal.builtin) |builtin_nominal| {
            switch (builtin_nominal) {
                .bool => return .{ .primitive = .bool },
                .str => return .{ .primitive = .str },
                .u8 => return .{ .primitive = .u8 },
                .i8 => return .{ .primitive = .i8 },
                .u16 => return .{ .primitive = .u16 },
                .i16 => return .{ .primitive = .i16 },
                .u32 => return .{ .primitive = .u32 },
                .i32 => return .{ .primitive = .i32 },
                .u64 => return .{ .primitive = .u64 },
                .i64 => return .{ .primitive = .i64 },
                .u128 => return .{ .primitive = .u128 },
                .i128 => return .{ .primitive = .i128 },
                .f32 => return .{ .primitive = .f32 },
                .f64 => return .{ .primitive = .f64 },
                .dec => return .{ .primitive = .dec },
                .list => {
                    if (nominal.args.len != 1) invariantViolation("List nominal type did not have exactly one argument");
                    return .{ .list = try self.lowerTemplateType(nominal.args[0]) };
                },
                .box => {
                    if (nominal.args.len != 1) invariantViolation("Box nominal type did not have exactly one argument");
                    return .{ .box = try self.lowerTemplateType(nominal.args[0]) };
                },
            }
        }

        return .{ .nominal = .{
            .nominal = .{
                .module_name = try self.name_resolver.moduleName(self.template_artifact, nominal.origin_module),
                .type_name = try self.name_resolver.typeName(self.template_artifact, nominal.name),
            },
            .is_opaque = nominal.is_opaque,
            .args = try self.lowerTemplateTypeIds(nominal.args),
            .backing = try self.lowerTemplateType(nominal.backing),
        } };
    }

    fn lowerArtifactRef(
        self: *TypeInstantiator,
        ref: checked_artifact.ArtifactCheckedTypeRef,
    ) Allocator.Error!Type.TypeId {
        const checked_types = checkedTypesForKey(self.input, ref.artifact) orelse {
            debug.invariant(false, "mono specialization invariant violated: concrete type ref artifact was not available");
            unreachable;
        };
        var lowerer = LowerType.Lowerer.initWithResolver(
            self.allocator,
            checked_types,
            &self.program.types,
            self.name_resolver,
            ref.artifact,
        );
        defer lowerer.deinit();
        return try lowerer.lowerChecked(ref.ty);
    }

    fn lowerConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!Type.TypeId {
        const root = self.program.concrete_source_types.root(ref);
        return switch (root.source) {
            .artifact => |artifact_ref| try self.lowerArtifactRef(artifact_ref),
            .local => |local| blk: {
                var lowerer = LowerType.Lowerer.init(
                    self.allocator,
                    self.program.concrete_source_types.localView(),
                    &self.program.types,
                );
                defer lowerer.deinit();
                break :blk try lowerer.lowerChecked(local);
            },
        };
    }

    fn unifyTemplateWithConcrete(
        self: *TypeInstantiator,
        template_id: checked_artifact.CheckedTypeId,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        switch (self.templatePayload(template_id)) {
            .flex, .rigid => {
                try self.bindTemplateVariable(template_id, concrete);
                return;
            },
            .alias => |alias| {
                try self.unifyTemplateWithConcrete(alias.backing, concrete);
                return;
            },
            else => {},
        }

        switch (self.concretePayload(concrete)) {
            .alias => |alias| {
                try self.unifyTemplateWithConcrete(template_id, try self.concreteChildRef(concrete, alias.backing));
                return;
            },
            else => {},
        }

        try self.unifyConcretePayload(template_id, concrete);
    }

    fn unifyConcretePayload(
        self: *TypeInstantiator,
        template_id: checked_artifact.CheckedTypeId,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        const template = self.templatePayload(template_id);
        const concrete_payload = self.concretePayload(concrete);
        switch (template) {
            .record => |record| switch (concrete_payload) {
                .record => |concrete_record| try self.unifyRecords(record, concrete, concrete_record),
                .empty_record => if (record.fields.len != 0) invariantViolation("mono specialization record arity mismatch"),
                else => invariantViolation("mono specialization expected a concrete record"),
            },
            .record_unbound => |fields| switch (concrete_payload) {
                .record, .record_unbound => try self.unifyRecordFieldSet(fields, concrete),
                .empty_record => if (fields.len != 0) invariantViolation("mono specialization record arity mismatch"),
                else => invariantViolation("mono specialization expected a concrete record row"),
            },
            .tuple => |items| switch (concrete_payload) {
                .tuple => |concrete_items| try self.unifyTypeLists(items, concrete, concrete_items),
                else => invariantViolation("mono specialization expected a concrete tuple"),
            },
            .nominal => |nominal| switch (concrete_payload) {
                .nominal => |concrete_nominal| try self.unifyNominals(nominal, concrete, concrete_nominal),
                else => invariantViolation("mono specialization expected a concrete nominal"),
            },
            .function => |func| switch (concrete_payload) {
                .function => |concrete_func| {
                    try self.unifyTypeLists(func.args, concrete, concrete_func.args);
                    try self.unifyTemplateWithConcrete(func.ret, try self.concreteChildRef(concrete, concrete_func.ret));
                },
                else => invariantViolation("mono specialization expected a concrete function"),
            },
            .empty_record => switch (concrete_payload) {
                .empty_record => {},
                .record => |record| if (record.fields.len != 0) invariantViolation("mono specialization empty record mismatch"),
                else => invariantViolation("mono specialization expected an empty concrete record"),
            },
            .tag_union => |tag_union| switch (concrete_payload) {
                .tag_union => |concrete_tag_union| try self.unifyTagUnions(tag_union, concrete, concrete_tag_union),
                .empty_tag_union => if (tag_union.tags.len != 0) invariantViolation("mono specialization tag-union mismatch"),
                else => invariantViolation("mono specialization expected a concrete tag union"),
            },
            .empty_tag_union => switch (concrete_payload) {
                .empty_tag_union => {},
                .tag_union => |tag_union| if (tag_union.tags.len != 0) invariantViolation("mono specialization empty tag-union mismatch"),
                else => invariantViolation("mono specialization expected an empty concrete tag union"),
            },
            .pending, .flex, .rigid, .alias => unreachable,
        }
    }

    fn unifyRecords(
        self: *TypeInstantiator,
        template: checked_artifact.CheckedRecordType,
        concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete_record: checked_artifact.CheckedRecordType,
    ) Allocator.Error!void {
        try self.unifyRecordFields(template.fields, concrete_ref, concrete_record.fields);
        try self.unifyTemplateWithConcrete(template.ext, try self.concreteChildRef(concrete_ref, concrete_record.ext));
    }

    fn unifyRecordFieldSet(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        switch (self.concretePayload(concrete)) {
            .record => |record| try self.unifyRecordFields(fields, concrete, record.fields),
            .record_unbound => |concrete_fields| try self.unifyRecordFields(fields, concrete, concrete_fields),
            else => invariantViolation("mono specialization expected concrete record fields"),
        }
    }

    fn unifyRecordFields(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete_fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error!void {
        for (fields) |field| {
            const expected_name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name);
            const concrete_field = (try self.findConcreteRecordField(concrete, concrete_fields, expected_name)) orelse {
                invariantViolation("mono specialization record field was missing in concrete type");
            };
            try self.unifyTemplateWithConcrete(field.ty, try self.concreteChildRef(concrete, concrete_field.ty));
        }
    }

    fn unifyNominals(
        self: *TypeInstantiator,
        template: checked_artifact.CheckedNominalType,
        concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete: checked_artifact.CheckedNominalType,
    ) Allocator.Error!void {
        if (template.builtin != concrete.builtin or template.args.len != concrete.args.len) {
            invariantViolation("mono specialization nominal mismatch");
        }
        if (template.builtin == null) {
            const template_key = canonical.NominalTypeKey{
                .module_name = try self.name_resolver.moduleName(self.template_artifact, template.origin_module),
                .type_name = try self.name_resolver.typeName(self.template_artifact, template.name),
            };
            const concrete_key = try self.nominalKeyForConcreteRef(concrete_ref, concrete.origin_module, concrete.name);
            if (template_key.module_name != concrete_key.module_name or template_key.type_name != concrete_key.type_name) {
                invariantViolation("mono specialization nominal mismatch");
            }
        }
        try self.unifyTypeLists(template.args, concrete_ref, concrete.args);
    }

    fn unifyTagUnions(
        self: *TypeInstantiator,
        template: checked_artifact.CheckedTagUnionType,
        concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete: checked_artifact.CheckedTagUnionType,
    ) Allocator.Error!void {
        for (template.tags) |tag| {
            const expected_name = try self.name_resolver.tagLabel(self.template_artifact, tag.name);
            const concrete_tag = (try self.findConcreteTag(concrete_ref, concrete.tags, expected_name)) orelse {
                invariantViolation("mono specialization tag was missing in concrete type");
            };
            try self.unifyTypeLists(tag.args, concrete_ref, concrete_tag.args);
        }
        try self.unifyTemplateWithConcrete(template.ext, try self.concreteChildRef(concrete_ref, concrete.ext));
    }

    fn unifyTypeLists(
        self: *TypeInstantiator,
        template: []const checked_artifact.CheckedTypeId,
        concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error!void {
        if (template.len != concrete.len) invariantViolation("mono specialization type arity mismatch");
        for (template, concrete) |template_id, concrete_id| {
            try self.unifyTemplateWithConcrete(template_id, try self.concreteChildRef(concrete_ref, concrete_id));
        }
    }

    fn bindTemplateVariable(
        self: *TypeInstantiator,
        template_id: checked_artifact.CheckedTypeId,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        if (self.substitutions.get(template_id)) |existing| {
            const existing_key = self.program.concrete_source_types.key(existing);
            const concrete_key = self.program.concrete_source_types.key(concrete);
            if (!std.mem.eql(u8, &existing_key.bytes, &concrete_key.bytes)) {
                invariantViolation("mono specialization generic variable mapped to incompatible concrete types");
            }
            return;
        }
        try self.substitutions.put(template_id, concrete);
    }

    fn templatePayload(self: *const TypeInstantiator, id: checked_artifact.CheckedTypeId) checked_artifact.CheckedTypePayload {
        const raw = @intFromEnum(id);
        if (raw >= self.template_types.payloads.len) invariantViolation("mono specialization template type id was outside published payloads");
        return self.template_types.payloads[raw];
    }

    fn concretePayload(self: *const TypeInstantiator, ref: ConcreteSourceType.ConcreteSourceTypeRef) checked_artifact.CheckedTypePayload {
        const root = self.program.concrete_source_types.root(ref);
        return switch (root.source) {
            .artifact => |artifact_ref| blk: {
                const checked_types = checkedTypesForKey(self.input, artifact_ref.artifact) orelse {
                    debug.invariant(false, "mono specialization invariant violated: concrete type artifact was not available");
                    unreachable;
                };
                const raw = @intFromEnum(artifact_ref.ty);
                if (raw >= checked_types.payloads.len) invariantViolation("mono specialization concrete type id was outside published payloads");
                break :blk checked_types.payloads[raw];
            },
            .local => |local| blk: {
                const local_view = self.program.concrete_source_types.localView();
                const raw = @intFromEnum(local);
                if (raw >= local_view.payloads.len) invariantViolation("mono specialization local concrete type id was outside payloads");
                break :blk local_view.payloads[raw];
            },
        };
    }

    fn concreteChildRef(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        child: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const root = self.program.concrete_source_types.root(parent);
        return switch (root.source) {
            .artifact => |artifact_ref| blk: {
                const checked_types = checkedTypesForKey(self.input, artifact_ref.artifact) orelse {
                    debug.invariant(false, "mono specialization invariant violated: concrete child artifact was not available");
                    unreachable;
                };
                break :blk try self.program.concrete_source_types.registerArtifactRoot(
                    artifact_ref.artifact,
                    checked_types,
                    child,
                );
            },
            .local => try self.program.concrete_source_types.registerLocalRoot(child),
        };
    }

    fn materializeConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const root = self.program.concrete_source_types.root(ref);
        switch (root.source) {
            .local => |local| return local,
            .artifact => {},
        }
        if (self.materialized_concrete_roots.get(ref)) |existing| return existing;

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.materialized_concrete_roots.put(ref, local_root);
        errdefer _ = self.materialized_concrete_roots.remove(ref);

        const payload = try self.materializeConcretePayload(ref, self.concretePayload(ref));
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        return local_root;
    }

    fn materializeConcretePayload(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished concrete checked type payload"),
            .flex, .rigid => invariantViolation("mono specialization reached an unsolved generic variable in a concrete source type"),
            .alias => |alias| .{ .alias = .{
                .name = try self.typeNameForConcreteRef(ref, alias.name),
                .origin_module = try self.moduleNameForConcreteRef(ref, alias.origin_module),
                .backing = try self.materializeConcreteRef(try self.concreteChildRef(ref, alias.backing)),
                .args = try self.materializeConcreteTypeIds(ref, alias.args),
            } },
            .record_unbound => |fields| .{ .record_unbound = try self.materializeConcreteRecordFields(ref, fields) },
            .record => |record| .{ .record = .{
                .fields = try self.materializeConcreteRecordFields(ref, record.fields),
                .ext = try self.materializeConcreteRef(try self.concreteChildRef(ref, record.ext)),
            } },
            .tuple => |items| .{ .tuple = try self.materializeConcreteTypeIds(ref, items) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.typeNameForConcreteRef(ref, nominal.name),
                .origin_module = try self.moduleNameForConcreteRef(ref, nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.materializeConcreteRef(try self.concreteChildRef(ref, nominal.backing)),
                .args = try self.materializeConcreteTypeIds(ref, nominal.args),
            } },
            .function => |func| .{ .function = .{
                .kind = func.kind,
                .args = try self.materializeConcreteTypeIds(ref, func.args),
                .ret = try self.materializeConcreteRef(try self.concreteChildRef(ref, func.ret)),
                .needs_instantiation = false,
            } },
            .empty_record => .empty_record,
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.materializeConcreteTags(ref, tag_union.tags),
                .ext = try self.materializeConcreteRef(try self.concreteChildRef(ref, tag_union.ext)),
            } },
            .empty_tag_union => .empty_tag_union,
        };
    }

    fn materializeConcreteTypeIds(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const checked_artifact.CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.materializeConcreteRef(try self.concreteChildRef(parent, id));
        }
        return out;
    }

    fn materializeConcreteRecordFields(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error![]const checked_artifact.CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.recordFieldNameForConcreteRef(parent, field.name),
                .ty = try self.materializeConcreteRef(try self.concreteChildRef(parent, field.ty)),
            };
        }
        return out;
    }

    fn materializeConcreteTags(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        tags: []const checked_artifact.CheckedTag,
    ) Allocator.Error![]const checked_artifact.CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.tagNameForConcreteRef(parent, tag.name),
                .args = try self.materializeConcreteTypeIds(parent, tag.args),
            };
        }
        return out;
    }

    fn artifactPayload(self: *const TypeInstantiator, ref: checked_artifact.ArtifactCheckedTypeRef) checked_artifact.CheckedTypePayload {
        const checked_types = checkedTypesForKey(self.input, ref.artifact) orelse {
            debug.invariant(false, "mono specialization invariant violated: concrete type artifact was not available");
            unreachable;
        };
        const raw = @intFromEnum(ref.ty);
        if (raw >= checked_types.payloads.len) invariantViolation("mono specialization concrete type id was outside published payloads");
        return checked_types.payloads[raw];
    }

    fn recordFieldNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        const root = self.program.concrete_source_types.root(ref);
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.recordFieldLabel(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn tagNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        const root = self.program.concrete_source_types.root(ref);
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.tagLabel(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn moduleNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.ModuleNameId,
    ) Allocator.Error!canonical.ModuleNameId {
        const root = self.program.concrete_source_types.root(ref);
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.moduleName(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn typeNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.TypeNameId,
    ) Allocator.Error!canonical.TypeNameId {
        const root = self.program.concrete_source_types.root(ref);
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.typeName(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn nominalKeyForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        module_name: canonical.ModuleNameId,
        type_name: canonical.TypeNameId,
    ) Allocator.Error!canonical.NominalTypeKey {
        return .{
            .module_name = try self.moduleNameForConcreteRef(ref, module_name),
            .type_name = try self.typeNameForConcreteRef(ref, type_name),
        };
    }

    fn findConcreteRecordField(
        self: *TypeInstantiator,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
        fields: []const checked_artifact.CheckedRecordField,
        name: canonical.RecordFieldLabelId,
    ) Allocator.Error!?checked_artifact.CheckedRecordField {
        for (fields) |field| {
            if ((try self.recordFieldNameForConcreteRef(concrete, field.name)) == name) return field;
        }
        return null;
    }

    fn findConcreteTag(
        self: *TypeInstantiator,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
        tags: []const checked_artifact.CheckedTag,
        name: canonical.TagLabelId,
    ) Allocator.Error!?checked_artifact.CheckedTag {
        for (tags) |tag| {
            if ((try self.tagNameForConcreteRef(concrete, tag.name)) == name) return tag;
        }
        return null;
    }
};

const BodyLowerer = struct {
    allocator: Allocator,
    input: Input,
    program: *Program,
    template_lookup: CheckedTemplateLookup,
    type_instantiator: *TypeInstantiator,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    local_symbols: std.AutoHashMap(checked_artifact.PatternBinderId, Ast.Symbol),

    fn init(
        allocator: Allocator,
        input: Input,
        program: *Program,
        template_lookup: CheckedTemplateLookup,
        type_instantiator: *TypeInstantiator,
        name_resolver: *ArtifactNames.ArtifactNameResolver,
    ) BodyLowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .program = program,
            .template_lookup = template_lookup,
            .type_instantiator = type_instantiator,
            .name_resolver = name_resolver,
            .local_symbols = std.AutoHashMap(checked_artifact.PatternBinderId, Ast.Symbol).init(allocator),
        };
    }

    fn deinit(self: *BodyLowerer) void {
        self.local_symbols.deinit();
    }

    fn lowerTemplateBody(
        self: *BodyLowerer,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.DefId {
        return switch (self.template_lookup.template.body) {
            .checked_body => |body_id| try self.lowerCheckedBody(reserved, fn_ty, body_id),
            .promoted_callable_wrapper,
            .hosted_wrapper,
            .intrinsic_wrapper,
            .entry_wrapper,
            => invariantViolation("mono body lowering reached a wrapper template before wrapper lowering was implemented"),
        };
    }

    fn lowerCheckedBody(
        self: *BodyLowerer,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
        body_id: checked_artifact.CheckedBodyId,
    ) Allocator.Error!Ast.DefId {
        const body = self.checkedBody(body_id);
        const root = self.checkedExpr(body.root_expr);
        return switch (root.data) {
            .lambda => |lambda| try self.lowerLambdaDef(reserved, fn_ty, lambda.args, lambda.body),
            .closure => |closure| blk: {
                const lambda_expr = self.checkedExpr(closure.lambda);
                switch (lambda_expr.data) {
                    .lambda => |lambda| break :blk try self.lowerLambdaDef(reserved, fn_ty, lambda.args, lambda.body),
                    else => invariantViolation("mono body lowering expected checked closure to reference a lambda body"),
                }
            },
            .hosted_lambda => invariantViolation("mono body lowering reached hosted lambda before hosted procedure lowering was implemented"),
            .anno_only => invariantViolation("mono body lowering reached annotation-only procedure body without checked backing expression"),
            else => invariantViolation("mono body lowering expected a checked procedure body to be a lambda-like expression"),
        };
    }

    fn lowerLambdaDef(
        self: *BodyLowerer,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
        arg_patterns: []const checked_artifact.CheckedPatternId,
        body_expr: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.DefId {
        const args = try self.lowerParamSpan(arg_patterns);
        const body = try self.lowerExpr(body_expr);
        const bind = Ast.TypedSymbol{
            .ty = fn_ty,
            .symbol = try self.program.addProcSymbol(reserved.local_handle),
        };
        return try self.program.ast.addDef(.{
            .proc = reserved.proc.proc,
            .debug_name = null,
            .value = .{ .fn_ = .{
                .recursive = false,
                .bind = bind,
                .args = args,
                .body = body,
            } },
        });
    }

    fn lowerParamSpan(
        self: *BodyLowerer,
        patterns: []const checked_artifact.CheckedPatternId,
    ) Allocator.Error!Ast.Span(Ast.TypedSymbol) {
        if (patterns.len == 0) return Ast.Span(Ast.TypedSymbol).empty();
        const args = try self.allocator.alloc(Ast.TypedSymbol, patterns.len);
        defer self.allocator.free(args);
        for (patterns, 0..) |pattern, i| {
            args[i] = try self.lowerParamPattern(pattern);
        }
        return try self.program.ast.addTypedSymbolSpan(args);
    }

    fn lowerParamPattern(
        self: *BodyLowerer,
        pattern_id: checked_artifact.CheckedPatternId,
    ) Allocator.Error!Ast.TypedSymbol {
        const pattern = self.checkedPattern(pattern_id);
        const binder = self.binderForSimplePattern(pattern.data);
        return .{
            .ty = try self.type_instantiator.lowerTemplateType(pattern.ty),
            .symbol = try self.symbolForBinder(binder),
        };
    }

    fn binderForSimplePattern(
        self: *BodyLowerer,
        data: checked_artifact.CheckedPatternData,
    ) checked_artifact.PatternBinderId {
        _ = self;
        return switch (data) {
            .assign => |binder| binder,
            .as => |as| as.binder,
            else => invariantViolation("mono body lowering requires destructuring parameters to be lowered into explicit local bindings before procedure entry"),
        };
    }

    fn symbolForBinder(
        self: *BodyLowerer,
        binder: checked_artifact.PatternBinderId,
    ) Allocator.Error!Ast.Symbol {
        if (self.local_symbols.get(binder)) |symbol| return symbol;
        const symbol = try self.program.addPatternBinderSymbol(binder);
        try self.local_symbols.put(binder, symbol);
        return symbol;
    }

    fn lowerExpr(
        self: *BodyLowerer,
        expr_id: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.checkedExpr(expr_id);
        const ty = try self.type_instantiator.lowerTemplateType(expr.ty);
        return switch (expr.data) {
            .num => |num| try self.program.ast.addExpr(ty, .{ .int_lit = num.value.toI128() }),
            .typed_int => |num| try self.program.ast.addExpr(ty, .{ .int_lit = num.value.toI128() }),
            .frac_f32 => |frac| try self.program.ast.addExpr(ty, .{ .frac_f32_lit = frac.value }),
            .frac_f64 => |frac| try self.program.ast.addExpr(ty, .{ .frac_f64_lit = frac.value }),
            .dec => |dec| try self.program.ast.addExpr(ty, .{ .dec_lit = dec.value.num }),
            .dec_small => |dec| try self.program.ast.addExpr(ty, .{ .dec_lit = dec.value.toRocDec().num }),
            .typed_frac => |frac| try self.program.ast.addExpr(ty, .{ .dec_lit = frac.value.toI128() }),
            .str_segment => |literal| try self.program.ast.addExpr(ty, .{ .str_lit = try self.lowerCheckedStringLiteral(literal) }),
            .str => |segments| try self.lowerStringExpr(ty, segments),
            .bytes_literal => |literal| try self.lowerBytesLiteral(ty, literal),
            .lookup_local => |lookup| try self.lowerResolvedLookup(ty, lookup.resolved orelse invariantViolation("checked lookup_local reached mono without a resolved value ref")),
            .lookup_external => |ref_id| try self.lowerResolvedLookup(ty, ref_id orelse invariantViolation("checked lookup_external reached mono without a resolved value ref")),
            .lookup_required => |ref_id| try self.lowerResolvedLookup(ty, ref_id orelse invariantViolation("checked lookup_required reached mono without a resolved value ref")),
            .list => |items| try self.lowerList(ty, items),
            .empty_list => try self.program.ast.addExpr(ty, .{ .list = Ast.Span(Ast.ExprId).empty() }),
            .tuple => |items| try self.lowerTuple(ty, items),
            .block => |block| try self.lowerBlock(ty, block.statements, block.final_expr),
            .record => |record| try self.lowerRecord(ty, record),
            .empty_record => try self.program.ast.addExpr(ty, .{ .record = Ast.Span(Ast.FieldExpr).empty() }),
            .lambda => |lambda| try self.lowerClosureExpr(ty, lambda.args, lambda.body),
            .call => |call| try self.lowerCall(ty, call),
            .structural_eq => |eq| try self.lowerStructuralEq(ty, eq),
            .unary_not => |child| blk: {
                const value = try self.lowerExpr(child);
                break :blk try self.program.ast.addExpr(ty, .{ .bool_not = value });
            },
            .if_ => |if_| try self.lowerIf(ty, if_.branches, if_.final_else),
            .match_ => |match_| try self.lowerMatch(ty, match_),
            .tag => |tag| try self.lowerTag(ty, try self.tagLabel(tag.name), tag.args),
            .zero_argument_tag => |tag| blk: {
                _ = tag.closure_name;
                break :blk try self.lowerTag(ty, try self.tagLabel(tag.name), &.{});
            },
            .closure => |closure| try self.lowerCheckedClosureExpr(ty, closure),
            .field_access => |access| try self.lowerFieldAccess(ty, access.receiver, try self.recordFieldLabel(access.field_name)),
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                break :blk try self.program.ast.addExpr(ty, .{ .tuple_access = .{
                    .tuple = tuple,
                    .elem_index = access.elem_index,
                } });
            },
            .return_ => |ret| blk: {
                _ = ret.lambda;
                _ = ret.context;
                const child = try self.lowerExpr(ret.expr);
                break :blk try self.program.ast.addExpr(ty, .{ .return_ = child });
            },
            .binop => |binop| try self.lowerBinop(ty, binop),
            .unary_minus => |child| try self.lowerUnaryMinus(ty, child),
            .for_ => |for_| try self.lowerForExpr(ty, for_.pattern, for_.expr, for_.body),
            .run_low_level => |run| try self.lowerRunLowLevel(ty, run.op, run.args),
            .nominal => |nominal| blk: {
                _ = nominal.backing_type;
                const backing = try self.lowerExpr(nominal.backing_expr);
                break :blk try self.program.ast.addExpr(ty, .{ .nominal_reinterpret = backing });
            },
            .dispatch_call => |plan| try self.lowerStaticDispatch(ty, plan orelse invariantViolation("checked dispatch call reached mono without a StaticDispatchCallPlan")),
            .method_eq => |plan| try self.lowerStaticDispatch(ty, plan orelse invariantViolation("checked method equality reached mono without a StaticDispatchCallPlan")),
            .type_dispatch_call => |plan| try self.lowerStaticDispatch(ty, plan orelse invariantViolation("checked type dispatch call reached mono without a StaticDispatchCallPlan")),
            .method_call,
            .type_method_call,
            .hosted_lambda,
            => invariantViolation("mono body lowering reached a checked expression form whose lowering is still missing"),
            .dbg => |child| blk: {
                const value = try self.lowerExpr(child);
                break :blk try self.program.ast.addExpr(ty, .{ .inspect = value });
            },
            .expect => |child| blk: {
                const condition = try self.lowerExpr(child);
                const expect_stmt = try self.program.ast.addStmt(.{ .expect = condition });
                const stmts = try self.program.ast.addStmtSpan(&.{expect_stmt});
                const unit = try self.program.ast.addExpr(ty, .unit);
                break :blk try self.program.ast.addExpr(ty, .{ .block = .{
                    .stmts = stmts,
                    .final_expr = unit,
                } });
            },
            .runtime_error => try self.program.ast.addExpr(ty, .runtime_error),
            .crash => |literal| try self.program.ast.addExpr(ty, .{ .crash = try self.lowerCheckedStringLiteral(literal) }),
            .ellipsis, .anno_only, .pending => invariantViolation("mono body lowering received a non-runtime checked expression form"),
        };
    }

    fn lowerStringExpr(
        self: *BodyLowerer,
        ty: Type.TypeId,
        segments: []const checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        if (segments.len == 0) invariantViolation("mono body lowering received string expression with no segments");

        var current = try self.lowerExpr(segments[0]);
        for (segments[1..]) |segment| {
            const rhs = try self.lowerExpr(segment);
            const args = [_]Ast.ExprId{ current, rhs };
            current = try self.program.ast.addExpr(ty, .{ .low_level = .{
                .op = .str_concat,
                .args = try self.program.ast.addExprSpan(&args),
                .source_constraint_ty = ty,
            } });
        }
        return current;
    }

    fn lowerBytesLiteral(
        self: *BodyLowerer,
        ty: Type.TypeId,
        literal: checked_artifact.CheckedStringLiteralId,
    ) Allocator.Error!Ast.ExprId {
        const elem_ty = switch (self.program.types.getType(ty)) {
            .list => |elem| elem,
            else => invariantViolation("mono body lowering bytes literal expected List(U8) type"),
        };
        const bytes = self.checkedStringLiteral(literal);
        if (bytes.len == 0) return try self.program.ast.addExpr(ty, .{ .list = Ast.Span(Ast.ExprId).empty() });

        const elems = try self.allocator.alloc(Ast.ExprId, bytes.len);
        defer self.allocator.free(elems);
        for (bytes, 0..) |byte, i| {
            elems[i] = try self.program.ast.addExpr(elem_ty, .{ .int_lit = @intCast(byte) });
        }
        return try self.program.ast.addExpr(ty, .{ .list = try self.program.ast.addExprSpan(elems) });
    }

    fn lowerResolvedLookup(
        self: *BodyLowerer,
        ty: Type.TypeId,
        ref_id: checked_artifact.ResolvedValueRefId,
    ) Allocator.Error!Ast.ExprId {
        const record = self.resolvedValueRef(ref_id);
        return switch (record.ref) {
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .local_proc,
            => |local| try self.program.ast.addExpr(ty, .{ .var_ = try self.symbolForBinder(local.binder) }),
            .top_level_const,
            .imported_const,
            .platform_required_const,
            => |const_use| try self.program.ast.addExpr(ty, .{ .const_ref = const_use.const_ref }),
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .platform_required_proc,
            .promoted_top_level_proc,
            => |proc_use| try self.program.ast.addExpr(ty, .{ .proc_value = .{
                .proc = self.procedureValueForUse(proc_use),
                .captures = Ast.Span(Ast.CaptureArg).empty(),
                .fn_ty = ty,
            } }),
            .platform_required_declaration => invariantViolation("mono body lowering reached platform-required declaration lookup as a runtime value"),
        };
    }

    fn lowerList(
        self: *BodyLowerer,
        ty: Type.TypeId,
        items: []const checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const span = try self.lowerExprSpan(items);
        return try self.program.ast.addExpr(ty, .{ .list = span });
    }

    fn lowerTuple(
        self: *BodyLowerer,
        ty: Type.TypeId,
        items: []const checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const span = try self.lowerExprSpan(items);
        return try self.program.ast.addExpr(ty, .{ .tuple = span });
    }

    fn lowerBlock(
        self: *BodyLowerer,
        ty: Type.TypeId,
        statements: []const checked_artifact.CheckedStatementId,
        final_expr: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const stmt_span = try self.lowerStmtSpan(statements);
        const final = try self.lowerExpr(final_expr);
        return try self.program.ast.addExpr(ty, .{ .block = .{
            .stmts = stmt_span,
            .final_expr = final,
        } });
    }

    fn lowerRecord(
        self: *BodyLowerer,
        ty: Type.TypeId,
        record: anytype,
    ) Allocator.Error!Ast.ExprId {
        if (record.ext) |ext| return try self.lowerRecordUpdate(ty, ext, record.fields);
        if (record.fields.len == 0) return try self.program.ast.addExpr(ty, .{ .record = Ast.Span(Ast.FieldExpr).empty() });
        const fields = try self.allocator.alloc(Ast.FieldExpr, record.fields.len);
        defer self.allocator.free(fields);
        for (record.fields, 0..) |field, i| {
            fields[i] = .{
                .field = try self.recordFieldLabel(field.label),
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.program.ast.addExpr(ty, .{ .record = try self.program.ast.addFieldExprSpan(fields) });
    }

    fn lowerRecordUpdate(
        self: *BodyLowerer,
        ty: Type.TypeId,
        ext: checked_artifact.CheckedExprId,
        update_fields: []const checked_artifact.CheckedRecordExprField,
    ) Allocator.Error!Ast.ExprId {
        const ext_ty = try self.type_instantiator.lowerTemplateType(self.checkedExpr(ext).ty);
        const ext_expr = try self.lowerExpr(ext);
        const ext_symbol = try self.program.symbols.add(base.Ident.Idx.NONE, .synthetic);
        const ext_var = try self.program.ast.addExpr(ext_ty, .{ .var_ = ext_symbol });

        const record_ty = switch (self.program.types.getType(ty)) {
            .record => |record| record,
            else => invariantViolation("mono body lowering record update expected record result type"),
        };

        const fields = try self.allocator.alloc(Ast.FieldExpr, record_ty.fields.len);
        defer self.allocator.free(fields);
        var field_count: usize = 0;

        for (update_fields) |field| {
            const label = try self.recordFieldLabel(field.label);
            if (try self.recordUpdateHasRemappedField(update_fields[0..field_count], label)) {
                invariantViolation("mono body lowering record update contained duplicate field labels");
            }
            _ = self.recordFieldIndex(ty, label);
            if (field_count >= fields.len) invariantViolation("mono body lowering record update had more fields than its result type");
            fields[field_count] = .{
                .field = label,
                .value = try self.lowerExpr(field.value),
            };
            field_count += 1;
        }

        for (record_ty.fields) |field| {
            if (try self.recordUpdateHasRemappedField(update_fields, field.name)) continue;
            if (field_count >= fields.len) invariantViolation("mono body lowering record update had more fields than its result type");
            fields[field_count] = .{
                .field = field.name,
                .value = try self.program.ast.addExpr(field.ty, .{ .access = .{
                    .record = ext_var,
                    .field = field.name,
                    .field_index = self.recordFieldIndex(ext_ty, field.name),
                } }),
            };
            field_count += 1;
        }

        if (field_count != fields.len) invariantViolation("mono body lowering record update did not produce every result field exactly once");

        const rest = try self.program.ast.addExpr(ty, .{
            .record = try self.program.ast.addFieldExprSpan(fields),
        });
        return try self.program.ast.addExpr(ty, .{ .let_ = .{
            .def = .{ .let_val = .{
                .bind = .{
                    .ty = ext_ty,
                    .symbol = ext_symbol,
                },
                .body = ext_expr,
            } },
            .rest = rest,
        } });
    }

    fn lowerClosureExpr(
        self: *BodyLowerer,
        ty: Type.TypeId,
        arg_patterns: []const checked_artifact.CheckedPatternId,
        body_expr: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const args = try self.lowerParamSpan(arg_patterns);
        const body = try self.lowerExpr(body_expr);
        return try self.program.ast.addExpr(ty, .{ .clos = .{
            .args = args,
            .body = body,
        } });
    }

    fn lowerCheckedClosureExpr(
        self: *BodyLowerer,
        ty: Type.TypeId,
        closure: anytype,
    ) Allocator.Error!Ast.ExprId {
        _ = closure.captures;
        _ = closure.tag_name;
        const lambda_expr = self.checkedExpr(closure.lambda);
        return switch (lambda_expr.data) {
            .lambda => |lambda| try self.lowerClosureExpr(ty, lambda.args, lambda.body),
            else => invariantViolation("mono body lowering expected closure expression to reference a checked lambda"),
        };
    }

    fn lowerCall(
        self: *BodyLowerer,
        ty: Type.TypeId,
        call: anytype,
    ) Allocator.Error!Ast.ExprId {
        _ = call.called_via;
        const func = try self.lowerExpr(call.func);
        const args = try self.lowerExprSpan(call.args);
        const func_ty = try self.type_instantiator.lowerTemplateType(self.checkedExpr(call.func).ty);
        return try self.program.ast.addExpr(ty, .{ .call_value = .{
            .func = func,
            .args = args,
            .requested_fn_ty = func_ty,
        } });
    }

    fn lowerStructuralEq(
        self: *BodyLowerer,
        ty: Type.TypeId,
        eq: anytype,
    ) Allocator.Error!Ast.ExprId {
        const lhs = try self.lowerExpr(eq.lhs);
        const rhs = try self.lowerExpr(eq.rhs);
        const structural = try self.program.ast.addExpr(ty, .{ .structural_eq = .{ .lhs = lhs, .rhs = rhs } });
        if (!eq.negated) return structural;
        return try self.program.ast.addExpr(ty, .{ .bool_not = structural });
    }

    fn lowerStaticDispatch(
        self: *BodyLowerer,
        ty: Type.TypeId,
        plan_id: checked_artifact.StaticDispatchPlanId,
    ) Allocator.Error!Ast.ExprId {
        const plan = self.staticDispatchPlan(plan_id);
        const callable_ty = try self.type_instantiator.lowerTemplateType(plan.callable_ty);
        const callable = switch (self.program.types.getType(callable_ty)) {
            .func => |func| func,
            else => invariantViolation("mono static dispatch callable type did not resolve to a fixed-arity function"),
        };
        if (callable.args.len != plan.args.len) invariantViolation("mono static dispatch argument count did not match callable arity");

        const lowered_args = try self.lowerExprSpan(plan.args);
        const arg_items = self.program.ast.sliceExprSpan(lowered_args);
        for (arg_items, callable.args) |arg, expected_arg_ty| {
            const actual_arg_ty = self.program.ast.getExpr(arg).ty;
            if (!self.program.types.equalIds(actual_arg_ty, expected_arg_ty)) {
                invariantViolation("mono static dispatch argument type did not match callable type");
            }
        }

        const dispatcher_ty = try self.type_instantiator.lowerTemplateType(plan.dispatcher_ty);
        const owner = methodOwnerForDispatcherType(&self.program.types, dispatcher_ty);
        const method = try self.methodName(plan.method);
        const target = try self.lookupMethodTarget(owner, method);

        if (target) |_| {
            invariantViolation("mono static dispatch target specialization is not implemented yet");
        }

        return switch (plan.result_mode) {
            .value => invariantViolation("mono static dispatch value call had no checked method target"),
            .equality => |equality| blk: {
                if (!equality.structural_allowed) invariantViolation("mono static dispatch equality had no checked method target and structural equality is not allowed");
                if (plan.args.len != 2) invariantViolation("mono static dispatch equality did not have exactly two operands");
                const lhs = arg_items[0];
                const rhs = arg_items[1];
                const structural = try self.program.ast.addExpr(ty, .{ .structural_eq = .{ .lhs = lhs, .rhs = rhs } });
                if (!equality.negated) break :blk structural;
                break :blk try self.program.ast.addExpr(ty, .{ .bool_not = structural });
            },
        };
    }

    fn lookupMethodTarget(
        self: *BodyLowerer,
        owner: static_dispatch.MethodOwner,
        method: canonical.MethodNameId,
    ) Allocator.Error!?static_dispatch.MethodTarget {
        const registry = methodRegistryForKey(self.input, self.template_lookup.artifact) orelse {
            debug.invariant(false, "mono static dispatch invariant violated: method registry artifact was not available");
            unreachable;
        };
        for (registry.entries) |entry| {
            const entry_owner = try self.name_resolver.methodOwner(self.template_lookup.artifact, entry.key.owner);
            const entry_method = try self.name_resolver.methodName(self.template_lookup.artifact, entry.key.method);
            if (methodOwnerEql(entry_owner, owner) and entry_method == method) return entry.target;
        }
        return null;
    }

    fn lowerIf(
        self: *BodyLowerer,
        ty: Type.TypeId,
        branches: []const checked_artifact.CheckedIfBranch,
        final_else: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        var current = try self.lowerExpr(final_else);
        var i = branches.len;
        while (i > 0) {
            i -= 1;
            current = try self.program.ast.addExpr(ty, .{ .if_ = .{
                .cond = try self.lowerExpr(branches[i].cond),
                .then_body = try self.lowerExpr(branches[i].body),
                .else_body = current,
            } });
        }
        return current;
    }

    fn lowerMatch(
        self: *BodyLowerer,
        ty: Type.TypeId,
        match_: anytype,
    ) Allocator.Error!Ast.ExprId {
        const cond = try self.lowerExpr(match_.cond);
        if (match_.branches.len == 0) invariantViolation("mono body lowering received a checked match with no branches");

        const branches = try self.allocator.alloc(Ast.Branch, match_.branches.len);
        defer self.allocator.free(branches);
        const cond_ty = self.program.ast.getExpr(cond).ty;
        for (match_.branches, 0..) |branch, i| {
            if (branch.guard != null) invariantViolation("mono body lowering reached match guard before guarded pattern decision lowering was implemented");
            if (branch.patterns.len != 1) invariantViolation("mono body lowering reached multi-scrutinee match before match decision lowering was implemented");
            branches[i] = .{
                .pat = try self.lowerPattern(cond_ty, branch.patterns[0].pattern),
                .body = try self.lowerExpr(branch.value),
            };
        }

        return try self.program.ast.addExpr(ty, .{ .match_ = .{
            .cond = cond,
            .branches = try self.program.ast.addBranchSpan(branches),
            .is_try_suffix = match_.is_try_suffix,
        } });
    }

    fn lowerTag(
        self: *BodyLowerer,
        ty: Type.TypeId,
        name: canonical.TagLabelId,
        args: []const checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const tag_info = self.tagInfoForUnionType(ty, name);
        if (tag_info.payload_count != args.len) invariantViolation("mono body lowering tag constructor arity did not match its resolved type");
        return try self.program.ast.addExpr(ty, .{ .tag = .{
            .name = name,
            .discriminant = tag_info.discriminant,
            .args = try self.lowerExprSpan(args),
            .constructor_ty = ty,
        } });
    }

    fn lowerFieldAccess(
        self: *BodyLowerer,
        ty: Type.TypeId,
        receiver: checked_artifact.CheckedExprId,
        field_name: canonical.RecordFieldLabelId,
    ) Allocator.Error!Ast.ExprId {
        const record = try self.lowerExpr(receiver);
        const record_ty = self.program.ast.getExpr(record).ty;
        return try self.program.ast.addExpr(ty, .{ .access = .{
            .record = record,
            .field = field_name,
            .field_index = self.recordFieldIndex(record_ty, field_name),
        } });
    }

    fn lowerForExpr(
        self: *BodyLowerer,
        ty: Type.TypeId,
        pattern: checked_artifact.CheckedPatternId,
        iterable: checked_artifact.CheckedExprId,
        body: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const pattern_ty = try self.type_instantiator.lowerTemplateType(self.checkedPattern(pattern).ty);
        return try self.program.ast.addExpr(ty, .{ .for_ = .{
            .patt = try self.lowerPattern(pattern_ty, pattern),
            .iterable = try self.lowerExpr(iterable),
            .body = try self.lowerExpr(body),
        } });
    }

    fn lowerRunLowLevel(
        self: *BodyLowerer,
        ty: Type.TypeId,
        op: base.LowLevel,
        args: []const checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        return try self.program.ast.addExpr(ty, .{ .low_level = .{
            .op = op,
            .args = try self.lowerExprSpan(args),
            .source_constraint_ty = ty,
        } });
    }

    fn lowerBinop(
        self: *BodyLowerer,
        ty: Type.TypeId,
        binop: anytype,
    ) Allocator.Error!Ast.ExprId {
        return switch (binop.op) {
            .add => try self.lowerBinaryLowLevel(ty, .num_plus, binop.lhs, binop.rhs),
            .sub => try self.lowerBinaryLowLevel(ty, .num_minus, binop.lhs, binop.rhs),
            .mul => try self.lowerBinaryLowLevel(ty, .num_times, binop.lhs, binop.rhs),
            .div => try self.lowerBinaryLowLevel(ty, .num_div_by, binop.lhs, binop.rhs),
            .rem => try self.lowerBinaryLowLevel(ty, .num_rem_by, binop.lhs, binop.rhs),
            .div_trunc => try self.lowerBinaryLowLevel(ty, .num_div_trunc_by, binop.lhs, binop.rhs),
            .lt => try self.lowerBinaryLowLevel(ty, .num_is_lt, binop.lhs, binop.rhs),
            .gt => try self.lowerBinaryLowLevel(ty, .num_is_gt, binop.lhs, binop.rhs),
            .le => try self.lowerBinaryLowLevel(ty, .num_is_lte, binop.lhs, binop.rhs),
            .ge => try self.lowerBinaryLowLevel(ty, .num_is_gte, binop.lhs, binop.rhs),
            .eq => blk: {
                const lhs = try self.lowerExpr(binop.lhs);
                const rhs = try self.lowerExpr(binop.rhs);
                break :blk try self.program.ast.addExpr(ty, .{ .structural_eq = .{ .lhs = lhs, .rhs = rhs } });
            },
            .ne => blk: {
                const lhs = try self.lowerExpr(binop.lhs);
                const rhs = try self.lowerExpr(binop.rhs);
                const eq = try self.program.ast.addExpr(ty, .{ .structural_eq = .{ .lhs = lhs, .rhs = rhs } });
                break :blk try self.program.ast.addExpr(ty, .{ .bool_not = eq });
            },
            .@"and" => blk: {
                const false_expr = try self.program.ast.addExpr(ty, .{ .bool_lit = false });
                break :blk try self.program.ast.addExpr(ty, .{ .if_ = .{
                    .cond = try self.lowerExpr(binop.lhs),
                    .then_body = try self.lowerExpr(binop.rhs),
                    .else_body = false_expr,
                } });
            },
            .@"or" => blk: {
                const true_expr = try self.program.ast.addExpr(ty, .{ .bool_lit = true });
                break :blk try self.program.ast.addExpr(ty, .{ .if_ = .{
                    .cond = try self.lowerExpr(binop.lhs),
                    .then_body = true_expr,
                    .else_body = try self.lowerExpr(binop.rhs),
                } });
            },
        };
    }

    fn lowerBinaryLowLevel(
        self: *BodyLowerer,
        ty: Type.TypeId,
        op: base.LowLevel,
        lhs_expr: checked_artifact.CheckedExprId,
        rhs_expr: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const lhs = try self.lowerExpr(lhs_expr);
        const rhs = try self.lowerExpr(rhs_expr);
        const args = [_]Ast.ExprId{ lhs, rhs };
        const source_constraint_ty = try self.type_instantiator.lowerTemplateType(self.checkedExpr(lhs_expr).ty);
        return try self.program.ast.addExpr(ty, .{ .low_level = .{
            .op = op,
            .args = try self.program.ast.addExprSpan(&args),
            .source_constraint_ty = source_constraint_ty,
        } });
    }

    fn lowerUnaryMinus(
        self: *BodyLowerer,
        ty: Type.TypeId,
        child_expr: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const child = try self.lowerExpr(child_expr);
        const args = [_]Ast.ExprId{child};
        const source_constraint_ty = try self.type_instantiator.lowerTemplateType(self.checkedExpr(child_expr).ty);
        return try self.program.ast.addExpr(ty, .{ .low_level = .{
            .op = .num_negate,
            .args = try self.program.ast.addExprSpan(&args),
            .source_constraint_ty = source_constraint_ty,
        } });
    }

    fn lowerPattern(
        self: *BodyLowerer,
        expected_ty: Type.TypeId,
        pattern_id: checked_artifact.CheckedPatternId,
    ) Allocator.Error!Ast.PatId {
        const pattern = self.checkedPattern(pattern_id);
        const ty = try self.type_instantiator.lowerTemplateType(pattern.ty);
        if (!self.program.types.equalIds(ty, expected_ty)) invariantViolation("mono body lowering pattern type did not match its scrutinee type");
        return switch (pattern.data) {
            .assign => |binder| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .var_ = try self.symbolForBinder(binder) } }),
            .as => |as| {
                _ = try self.symbolForBinder(as.binder);
                return try self.lowerPattern(ty, as.pattern);
            },
            .applied_tag => |tag| blk: {
                const tag_name = try self.tagLabel(tag.name);
                const tag_info = self.tagInfoForUnionType(ty, tag_name);
                if (tag_info.payload_count != tag.args.len) invariantViolation("mono body lowering tag pattern arity did not match its resolved type");
                const args = try self.allocator.alloc(Ast.PatId, tag.args.len);
                defer self.allocator.free(args);
                for (tag.args, 0..) |arg, i| {
                    args[i] = try self.lowerPattern(tag_info.payload_types[i], arg);
                }
                break :blk try self.program.ast.addPat(.{ .ty = ty, .data = .{ .tag = .{
                    .name = tag_name,
                    .discriminant = tag_info.discriminant,
                    .args = try self.program.ast.addPatSpan(args),
                } } });
            },
            .num_literal => |num| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .int_lit = num.value.toI128() } }),
            .small_dec_literal => |dec| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .dec_lit = dec.value.toRocDec().num } }),
            .dec_literal => |dec| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .dec_lit = dec.value.num } }),
            .frac_f32_literal => |value| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f32_lit = value } }),
            .frac_f64_literal => |value| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f64_lit = value } }),
            .str_literal => |literal| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .str_lit = try self.lowerCheckedStringLiteral(literal) } }),
            .underscore => try self.program.ast.addPat(.{ .ty = ty, .data = .wildcard }),
            else => invariantViolation("mono body lowering reached pattern form whose lowering is still missing"),
        };
    }

    const TagInfo = struct {
        discriminant: u16,
        payload_count: usize,
        payload_types: []const Type.TypeId,
    };

    fn tagInfoForUnionType(
        self: *BodyLowerer,
        union_ty: Type.TypeId,
        name: canonical.TagLabelId,
    ) TagInfo {
        return switch (self.program.types.getType(union_ty)) {
            .tag_union => |tag_union| {
                for (tag_union.tags, 0..) |tag, i| {
                    if (tag.name == name) return .{
                        .discriminant = @intCast(i),
                        .payload_count = tag.args.len,
                        .payload_types = tag.args,
                    };
                }
                invariantViolation("mono body lowering could not find tag constructor in resolved union type");
            },
            else => invariantViolation("mono body lowering expected a resolved tag-union type"),
        };
    }

    fn recordFieldIndex(
        self: *BodyLowerer,
        record_ty: Type.TypeId,
        field_name: canonical.RecordFieldLabelId,
    ) u16 {
        return switch (self.program.types.getType(record_ty)) {
            .record => |record| {
                for (record.fields, 0..) |field, i| {
                    if (field.name == field_name) return @intCast(i);
                }
                invariantViolation("mono body lowering could not find field in resolved record type");
            },
            else => invariantViolation("mono body lowering expected a resolved record type"),
        };
    }

    fn lowerExprSpan(
        self: *BodyLowerer,
        exprs: []const checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        if (exprs.len == 0) return Ast.Span(Ast.ExprId).empty();
        const lowered = try self.allocator.alloc(Ast.ExprId, exprs.len);
        defer self.allocator.free(lowered);
        for (exprs, 0..) |expr, i| {
            lowered[i] = try self.lowerExpr(expr);
        }
        return try self.program.ast.addExprSpan(lowered);
    }

    fn lowerStmtSpan(
        self: *BodyLowerer,
        statements: []const checked_artifact.CheckedStatementId,
    ) Allocator.Error!Ast.Span(Ast.StmtId) {
        if (statements.len == 0) return Ast.Span(Ast.StmtId).empty();
        const lowered = try self.allocator.alloc(Ast.StmtId, statements.len);
        defer self.allocator.free(lowered);
        for (statements, 0..) |statement, i| {
            lowered[i] = try self.lowerStmt(statement);
        }
        return try self.program.ast.addStmtSpan(lowered);
    }

    fn lowerStmt(
        self: *BodyLowerer,
        statement_id: checked_artifact.CheckedStatementId,
    ) Allocator.Error!Ast.StmtId {
        const statement = self.checkedStatement(statement_id);
        return switch (statement.data) {
            .decl => |decl| blk: {
                const bind = try self.lowerParamPattern(decl.pattern);
                const body = try self.lowerExpr(decl.expr);
                break :blk try self.program.ast.addStmt(.{ .decl = .{ .bind = bind, .body = body } });
            },
            .var_ => |var_| blk: {
                const bind = try self.lowerParamPattern(var_.pattern);
                const body = try self.lowerExpr(var_.expr);
                break :blk try self.program.ast.addStmt(.{ .var_decl = .{ .bind = bind, .body = body } });
            },
            .reassign => |reassign| blk: {
                const bind = try self.lowerParamPattern(reassign.pattern);
                const body = try self.lowerExpr(reassign.expr);
                break :blk try self.program.ast.addStmt(.{ .reassign = .{
                    .target = bind.symbol,
                    .body = body,
                } });
            },
            .dbg => |expr| try self.program.ast.addStmt(.{ .debug = try self.lowerExpr(expr) }),
            .expr => |expr| try self.program.ast.addStmt(.{ .expr = try self.lowerExpr(expr) }),
            .expect => |expr| try self.program.ast.addStmt(.{ .expect = try self.lowerExpr(expr) }),
            .crash => |literal| try self.program.ast.addStmt(.{ .crash = try self.lowerCheckedStringLiteral(literal) }),
            .return_ => |ret| try self.program.ast.addStmt(.{ .return_ = try self.lowerExpr(ret.expr) }),
            .break_ => try self.program.ast.addStmt(.break_),
            .for_ => |for_| try self.lowerForStmt(for_.pattern, for_.expr, for_.body),
            .while_ => |while_| try self.program.ast.addStmt(.{ .while_ = .{
                .cond = try self.lowerExpr(while_.cond),
                .body = try self.lowerExpr(while_.body),
            } }),
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            .runtime_error,
            .pending,
            => invariantViolation("mono body lowering received a non-runtime checked statement form"),
        };
    }

    fn lowerForStmt(
        self: *BodyLowerer,
        pattern: checked_artifact.CheckedPatternId,
        iterable: checked_artifact.CheckedExprId,
        body: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.StmtId {
        const pattern_ty = try self.type_instantiator.lowerTemplateType(self.checkedPattern(pattern).ty);
        return try self.program.ast.addStmt(.{ .for_ = .{
            .patt = try self.lowerPattern(pattern_ty, pattern),
            .iterable = try self.lowerExpr(iterable),
            .body = try self.lowerExpr(body),
        } });
    }

    fn lowerCheckedStringLiteral(
        self: *BodyLowerer,
        literal: checked_artifact.CheckedStringLiteralId,
    ) Allocator.Error!ids.ProgramLiteralId {
        return try self.program.literal_pool.intern(self.checkedStringLiteral(literal));
    }

    fn recordFieldLabel(
        self: *BodyLowerer,
        label: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        return try self.name_resolver.recordFieldLabel(self.template_lookup.artifact, label);
    }

    fn tagLabel(
        self: *BodyLowerer,
        label: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        return try self.name_resolver.tagLabel(self.template_lookup.artifact, label);
    }

    fn methodName(
        self: *BodyLowerer,
        method: canonical.MethodNameId,
    ) Allocator.Error!canonical.MethodNameId {
        return try self.name_resolver.methodName(self.template_lookup.artifact, method);
    }

    fn recordUpdateHasRemappedField(
        self: *BodyLowerer,
        fields: []const checked_artifact.CheckedRecordExprField,
        label: canonical.RecordFieldLabelId,
    ) Allocator.Error!bool {
        for (fields) |field| {
            if ((try self.recordFieldLabel(field.label)) == label) return true;
        }
        return false;
    }

    fn checkedStringLiteral(
        self: *BodyLowerer,
        literal: checked_artifact.CheckedStringLiteralId,
    ) []const u8 {
        const raw = @intFromEnum(literal);
        if (raw >= self.template_lookup.checked_bodies.string_literals.len) {
            invariantViolation("mono body lowering received a checked string literal outside the owning checked body store");
        }
        return self.template_lookup.checked_bodies.string_literals[raw];
    }

    fn checkedBody(self: *const BodyLowerer, id: checked_artifact.CheckedBodyId) checked_artifact.CheckedBody {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.checked_bodies.bodies.len) invariantViolation("mono body lowering received body id outside checked body store");
        return self.template_lookup.checked_bodies.bodies[raw];
    }

    fn checkedExpr(self: *const BodyLowerer, id: checked_artifact.CheckedExprId) checked_artifact.CheckedExpr {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.checked_bodies.exprs.len) invariantViolation("mono body lowering received expr id outside checked body store");
        return self.template_lookup.checked_bodies.exprs[raw];
    }

    fn checkedPattern(self: *const BodyLowerer, id: checked_artifact.CheckedPatternId) checked_artifact.CheckedPattern {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.checked_bodies.patterns.len) invariantViolation("mono body lowering received pattern id outside checked body store");
        return self.template_lookup.checked_bodies.patterns[raw];
    }

    fn checkedStatement(self: *const BodyLowerer, id: checked_artifact.CheckedStatementId) checked_artifact.CheckedStatement {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.checked_bodies.statements.len) invariantViolation("mono body lowering received statement id outside checked body store");
        return self.template_lookup.checked_bodies.statements[raw];
    }

    fn staticDispatchPlan(self: *const BodyLowerer, id: checked_artifact.StaticDispatchPlanId) static_dispatch.StaticDispatchCallPlan {
        const table = staticDispatchPlansForKey(self.input, self.template_lookup.artifact) orelse {
            debug.invariant(false, "mono body lowering invariant violated: static dispatch plan artifact was not available");
            unreachable;
        };
        const raw = @intFromEnum(id);
        if (raw >= table.plans.len) invariantViolation("mono body lowering received static dispatch plan id outside table");
        return table.plans[raw];
    }

    fn resolvedValueRef(self: *const BodyLowerer, id: checked_artifact.ResolvedValueRefId) checked_artifact.ResolvedValueRefRecord {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.resolved_value_refs.records.len) invariantViolation("mono body lowering received resolved value ref id outside table");
        return self.template_lookup.resolved_value_refs.records[raw];
    }

    fn procedureValueForUse(
        self: *const BodyLowerer,
        use: checked_artifact.ProcedureUseTemplate,
    ) canonical.ProcedureValueRef {
        return switch (use.binding) {
            .top_level => |binding_ref| self.directProcedureBinding(
                topLevelProcedureBindingsForKey(self.input, self.template_lookup.artifact) orelse {
                    debug.invariant(false, "mono body lowering invariant violated: template artifact has no top-level procedure binding table");
                    unreachable;
                },
                binding_ref,
            ).proc_value,
            .imported => |imported| self.importedProcedureBinding(imported).proc_value,
            .hosted => |hosted| hosted.proc,
            .platform_required => |required| self.directProcedureBinding(
                topLevelProcedureBindingsForKey(self.input, required.artifact) orelse {
                    debug.invariant(false, "mono body lowering invariant violated: platform-required artifact has no procedure binding table");
                    unreachable;
                },
                required.procedure_binding,
            ).proc_value,
            .promoted => |promoted| promoted.proc,
        };
    }

    fn directProcedureBinding(
        self: *const BodyLowerer,
        bindings: *const checked_artifact.TopLevelProcedureBindingTable,
        binding_ref: checked_artifact.TopLevelProcedureBindingRef,
    ) checked_artifact.DirectProcedureBinding {
        _ = self;
        const binding = bindings.get(binding_ref);
        return switch (binding.body) {
            .direct_template => |direct| direct,
            .callable_eval_template => invariantViolation("mono body lowering reached callable-eval procedure binding before callable instance sealing was implemented"),
        };
    }

    fn importedProcedureBinding(
        self: *const BodyLowerer,
        imported: checked_artifact.ImportedProcedureBindingRef,
    ) checked_artifact.DirectProcedureBinding {
        for (self.input.imports) |view| {
            if (!std.mem.eql(u8, &view.key.bytes, &imported.artifact.bytes)) continue;
            for (view.exported_procedure_bindings.bindings) |binding| {
                if (binding.binding.module_idx == imported.module_idx and
                    binding.binding.def == imported.def and
                    binding.binding.pattern == imported.pattern)
                {
                    return switch (binding.body) {
                        .direct_template => |direct| direct,
                        .callable_eval_template => invariantViolation("mono body lowering reached imported callable-eval binding before callable instance sealing was implemented"),
                    };
                }
            }
        }
        for (self.input.root.relation_artifacts) |view| {
            if (!std.mem.eql(u8, &view.key.bytes, &imported.artifact.bytes)) continue;
            for (view.exported_procedure_bindings.bindings) |binding| {
                if (binding.binding.module_idx == imported.module_idx and
                    binding.binding.def == imported.def and
                    binding.binding.pattern == imported.pattern)
                {
                    return switch (binding.body) {
                        .direct_template => |direct| direct,
                        .callable_eval_template => invariantViolation("mono body lowering reached relation callable-eval binding before callable instance sealing was implemented"),
                    };
                }
            }
        }
        invariantViolation("mono body lowering could not find imported procedure binding in published artifact views");
    }
};

fn invariantViolation(comptime message: []const u8) noreturn {
    debug.invariant(false, message);
    unreachable;
}

fn checkedTypesForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?checked_artifact.CheckedTypeStoreView {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) return input.root.artifact.checked_types.view();
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.checked_types;
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.checked_types;
    }
    return null;
}

fn templateForRoot(
    input: Input,
    root: checked_artifact.RootRequest,
) ?canonical.ProcedureTemplateRef {
    const artifact = input.root.artifact;
    switch (root.source) {
        .def => |def_idx| return artifact.checked_procedure_templates.lookupByDef(def_idx),
        .required_binding => |binding_id| {
            const binding = artifact.platform_required_bindings.lookupByBindingId(binding_id) orelse {
                debug.invariantFmt(
                    false,
                    "mono specialization invariant violated: platform-required root {d} has no sealed binding",
                    .{binding_id},
                );
                unreachable;
            };
            return switch (binding.value_use) {
                .const_value => null,
                .procedure_value => |proc_use| templateForProcedureUse(input, proc_use),
            };
        },
        .expr, .statement => return null,
    }
}

fn templateForProcedureUse(
    input: Input,
    proc_use: checked_artifact.ProcedureUseTemplate,
) ?canonical.ProcedureTemplateRef {
    return switch (proc_use.binding) {
        .top_level => |binding_ref| templateFromTopLevelBinding(
            &input.root.artifact.top_level_procedure_bindings,
            binding_ref,
        ),
        .platform_required => |required| {
            const bindings = topLevelProcedureBindingsForKey(input, required.artifact) orelse {
                debug.invariant(false, "mono specialization invariant violated: platform-required procedure binding references unavailable app artifact");
                unreachable;
            };
            return templateFromTopLevelBinding(
                bindings,
                required.procedure_binding,
            );
        },
        .imported, .hosted, .promoted => {
            debug.invariant(false, "mono specialization invariant violated: platform-required root resolved to unsupported procedure binding kind");
            unreachable;
        },
    };
}

fn templateFromTopLevelBinding(
    bindings: *const checked_artifact.TopLevelProcedureBindingTable,
    binding_ref: checked_artifact.TopLevelProcedureBindingRef,
) ?canonical.ProcedureTemplateRef {
    const binding = bindings.get(binding_ref);
    return switch (binding.body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |template| template,
            .lifted, .synthetic => {
                debug.invariant(false, "mono specialization invariant violated: root procedure binding must have a checked template before mono lowering");
                unreachable;
            },
        },
        .callable_eval_template => {
            debug.invariant(false, "mono specialization invariant violated: callable-eval platform-required roots need a sealed concrete callable binding instance before mono lowering");
            unreachable;
        },
    };
}

fn topLevelProcedureBindingsForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.TopLevelProcedureBindingTable {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return &input.root.artifact.top_level_procedure_bindings;
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.top_level_procedure_bindings;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.top_level_procedure_bindings;
        }
    }
    return null;
}

fn staticDispatchPlansForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const static_dispatch.StaticDispatchPlanTable {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return &input.root.artifact.static_dispatch_plans;
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.static_dispatch_plans;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.static_dispatch_plans;
        }
    }
    return null;
}

fn methodRegistryForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const static_dispatch.MethodRegistry {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return &input.root.artifact.method_registry;
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.method_registry;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.method_registry;
        }
    }
    return null;
}

fn methodOwnerForDispatcherType(
    types: *const Type.Store,
    ty: Type.TypeId,
) static_dispatch.MethodOwner {
    return switch (types.getTypePreservingNominal(ty)) {
        .nominal => |nominal| .{ .nominal = nominal.nominal },
        .primitive => |prim| .{ .builtin = switch (prim) {
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
            .erased => invariantViolation("mono static dispatch reached erased function representation before lambda solving"),
        } },
        .list => .{ .builtin = .list },
        .box => .{ .builtin = .box },
        .link => unreachable,
        .placeholder,
        .unbd,
        .func,
        .tuple,
        .tag_union,
        .record,
        => invariantViolation("mono static dispatch dispatcher type did not resolve to an allowed method owner"),
    };
}

fn methodOwnerEql(a: static_dispatch.MethodOwner, b: static_dispatch.MethodOwner) bool {
    return switch (a) {
        .nominal => |a_nominal| switch (b) {
            .nominal => |b_nominal| a_nominal.module_name == b_nominal.module_name and
                a_nominal.type_name == b_nominal.type_name,
            else => false,
        },
        .builtin => |a_builtin| switch (b) {
            .builtin => |b_builtin| a_builtin == b_builtin,
            else => false,
        },
    };
}

fn verifyProgram(program: *const Program) void {
    if (@import("builtin").mode != .Debug) return;
    for (program.root_procs.items) |root| {
        var found = false;
        for (program.procs.items) |proc| {
            if (canonical.monoSpecializedProcRefEql(proc.proc, root))
            {
                found = true;
                break;
            }
        }
        std.debug.assert(found);
    }
}

pub const Queue = struct {
    allocator: Allocator,
    requested: std.AutoHashMap(canonical.MonoSpecializationKey, ReservedMonoProc),
    pending: std.ArrayList(canonical.MonoSpecializationKey),

    pub fn init(allocator: Allocator) Queue {
        return .{
            .allocator = allocator,
            .requested = std.AutoHashMap(canonical.MonoSpecializationKey, ReservedMonoProc).init(allocator),
            .pending = .empty,
        };
    }

    pub fn deinit(self: *Queue) void {
        self.pending.deinit(self.allocator);
        self.requested.deinit();
        self.* = Queue.init(self.allocator);
    }

    pub fn reserve(
        self: *Queue,
        concrete_source_types: *const ConcreteSourceType.Store,
        request: MonoSpecializationRequest,
    ) Allocator.Error!ReservedMonoProc {
        const requested_mono_fn_ty = concrete_source_types.key(request.requested_fn_ty);
        const key = canonical.MonoSpecializationKey{
            .template = request.template,
            .requested_mono_fn_ty = requested_mono_fn_ty,
        };
        if (self.requested.get(key)) |existing| {
            if (existing.requested_fn_ty != request.requested_fn_ty) {
                debug.invariant(false, "mono specialization invariant violated: same specialization key registered with a different concrete payload ref");
                unreachable;
            }
            return existing;
        }

        const reserved = ReservedMonoProc{
            .proc = .{
                .proc = .{ .artifact = request.template.artifact, .proc_base = request.template.proc_base },
                .specialization = key,
            },
            .local_handle = @enumFromInt(@as(u32, @intCast(self.requested.count()))),
            .requested_fn_ty = request.requested_fn_ty,
            .state = .reserved,
        };
        try self.requested.put(key, reserved);
        try self.pending.append(self.allocator, key);
        return reserved;
    }

    pub fn markLowering(self: *Queue, key: canonical.MonoSpecializationKey) void {
        const entry = self.requested.getPtr(key) orelse unreachable;
        switch (entry.state) {
            .reserved => entry.state = .lowering,
            .lowering, .lowered => unreachable,
        }
    }

    pub fn markLowered(self: *Queue, key: canonical.MonoSpecializationKey) void {
        const entry = self.requested.getPtr(key) orelse unreachable;
        switch (entry.state) {
            .lowering => entry.state = .lowered,
            .reserved, .lowered => unreachable,
        }
    }
};

test "mono specialization queue reserves once" {
    var queue = Queue.init(std.testing.allocator);
    defer queue.deinit();
    var concrete = ConcreteSourceType.Store.init(std.testing.allocator);
    defer concrete.deinit();

    const requested_key = canonical.CanonicalTypeKey{ .bytes = [_]u8{1} ** 32 };
    const owned_key = try std.testing.allocator.dupe(u8, requested_key.bytes[0..]);
    try concrete.roots.append(std.testing.allocator, .{
        .key = requested_key,
        .source = .{
            .artifact = .{},
            .ty = @enumFromInt(0),
        },
    });
    try concrete.by_key.put(owned_key, @enumFromInt(0));

    const template = canonical.ProcedureTemplateRef{
        .proc_base = @enumFromInt(0),
        .template = @enumFromInt(0),
    };
    const request = MonoSpecializationRequest{
        .template = template,
        .requested_fn_ty = @enumFromInt(0),
        .reason = .{ .comptime_dependency_summary = 0 },
    };

    const first = try queue.reserve(&concrete, request);
    const second = try queue.reserve(&concrete, request);
    try std.testing.expectEqual(first.local_handle, second.local_handle);
    try std.testing.expectEqual(@as(usize, 1), queue.pending.items.len);
}

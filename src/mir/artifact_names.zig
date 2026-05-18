//! Lowering-run canonical name remapping for checked artifacts.
//!
//! Checked artifacts own dense canonical-name ids. MIR-family lowering works in
//! one lowering-run name space, so imported artifact-local ids must be remapped
//! through published canonical bytes before their payloads reach mono MIR.

const std = @import("std");
const check = @import("check");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const static_dispatch = check.StaticDispatchRegistry;
const debug = @import("debug_verify.zig");

/// Public `ArtifactNameResolver` declaration.
pub const ArtifactNameResolver = struct {
    lowering_names: *canonical.CanonicalNameStore,
    root_key: checked_artifact.CheckedModuleArtifactKey,
    root_names: *const canonical.CanonicalNameStore,
    imports: []const checked_artifact.ImportedModuleView = &.{},
    relation_artifacts: []const checked_artifact.ImportedModuleView = &.{},

    pub fn init(
        lowering_names: *canonical.CanonicalNameStore,
        root: *const checked_artifact.CheckedModuleArtifact,
        imports: []const checked_artifact.ImportedModuleView,
        relation_artifacts: []const checked_artifact.ImportedModuleView,
    ) ArtifactNameResolver {
        return .{
            .lowering_names = lowering_names,
            .root_key = root.key,
            .root_names = &root.canonical_names,
            .imports = imports,
            .relation_artifacts = relation_artifacts,
        };
    }

    pub fn moduleName(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        id: canonical.ModuleNameId,
    ) Allocator.Error!canonical.ModuleNameId {
        const source = self.namesForArtifact(artifact);
        return try self.lowering_names.internModuleName(moduleNameText(source, id));
    }

    pub fn typeName(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        id: canonical.TypeNameId,
    ) Allocator.Error!canonical.TypeNameId {
        const source = self.namesForArtifact(artifact);
        return try self.lowering_names.internTypeName(typeNameText(source, id));
    }

    pub fn methodName(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        id: canonical.MethodNameId,
    ) Allocator.Error!canonical.MethodNameId {
        const source = self.namesForArtifact(artifact);
        return try self.lowering_names.internMethodName(methodNameText(source, id));
    }

    pub fn recordFieldLabel(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        id: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        const source = self.namesForArtifact(artifact);
        return try self.lowering_names.internRecordFieldLabel(recordFieldLabelText(source, id));
    }

    pub fn tagLabel(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        id: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        const source = self.namesForArtifact(artifact);
        return try self.lowering_names.internTagLabel(tagLabelText(source, id));
    }

    pub fn exportName(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        id: canonical.ExportNameId,
    ) Allocator.Error!canonical.ExportNameId {
        const source = self.namesForArtifact(artifact);
        return try self.lowering_names.internExportName(exportNameText(source, id));
    }

    pub fn externalSymbolName(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        id: canonical.ExternalSymbolNameId,
    ) Allocator.Error!canonical.ExternalSymbolNameId {
        const source = self.namesForArtifact(artifact);
        return try self.lowering_names.internExternalSymbolName(externalSymbolNameText(source, id));
    }

    pub fn procBase(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        id: canonical.ProcBaseKeyRef,
    ) Allocator.Error!canonical.ProcBaseKeyRef {
        const source = self.namesForArtifact(artifact);
        const key = source.procBase(id);
        return try self.lowering_names.internProcBase(.{
            .module_name = try self.moduleName(artifact, key.module_name),
            .export_name = if (key.export_name) |export_name| try self.exportName(artifact, export_name) else null,
            .kind = key.kind,
            .ordinal = key.ordinal,
            .source_def_idx = key.source_def_idx,
            .nested_proc_site = if (key.nested_proc_site) |site| .{
                .owner_template = try self.procedureTemplateRef(site.owner_template),
                .site = site.site,
            } else null,
            .owner_mono_specialization = if (key.owner_mono_specialization) |owner| .{
                .template = try self.procedureTemplateRef(owner.template),
                .requested_mono_fn_ty = owner.requested_mono_fn_ty,
            } else null,
        });
    }

    pub fn procedureValueRef(
        self: *ArtifactNameResolver,
        ref: canonical.ProcedureValueRef,
    ) Allocator.Error!canonical.ProcedureValueRef {
        const artifact: checked_artifact.CheckedModuleArtifactKey = .{ .bytes = ref.artifact.bytes };
        return .{
            .artifact = ref.artifact,
            .proc_base = try self.procBase(artifact, ref.proc_base),
        };
    }

    pub fn procedureTemplateRef(
        self: *ArtifactNameResolver,
        ref: canonical.ProcedureTemplateRef,
    ) Allocator.Error!canonical.ProcedureTemplateRef {
        const artifact: checked_artifact.CheckedModuleArtifactKey = .{ .bytes = ref.artifact.bytes };
        return .{
            .artifact = ref.artifact,
            .proc_base = try self.procBase(artifact, ref.proc_base),
            .template = ref.template,
        };
    }

    pub fn callableProcedureTemplateRef(
        self: *ArtifactNameResolver,
        ref: canonical.CallableProcedureTemplateRef,
    ) Allocator.Error!canonical.CallableProcedureTemplateRef {
        return switch (ref) {
            .checked => |checked| .{ .checked = try self.procedureTemplateRef(checked) },
            .synthetic => |synthetic| .{ .synthetic = .{
                .template = try self.procedureTemplateRef(synthetic.template),
            } },
            .lifted => |lifted| .{ .lifted = .{
                .owner_mono_specialization = .{
                    .template = try self.procedureTemplateRef(lifted.owner_mono_specialization.template),
                    .requested_mono_fn_ty = lifted.owner_mono_specialization.requested_mono_fn_ty,
                },
                .site = lifted.site,
            } },
        };
    }

    pub fn procedureCallableRef(
        self: *ArtifactNameResolver,
        ref: canonical.ProcedureCallableRef,
    ) Allocator.Error!canonical.ProcedureCallableRef {
        return .{
            .template = try self.callableProcedureTemplateRef(ref.template),
            .source_fn_ty = ref.source_fn_ty,
        };
    }

    pub fn mirProcedureRef(
        self: *ArtifactNameResolver,
        ref: canonical.MirProcedureRef,
    ) Allocator.Error!canonical.MirProcedureRef {
        return .{
            .proc = try self.procedureValueRef(ref.proc),
            .callable = try self.procedureCallableRef(ref.callable),
        };
    }

    pub fn nominalTypeKey(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        key: canonical.NominalTypeKey,
    ) Allocator.Error!canonical.NominalTypeKey {
        return .{
            .module_name = try self.moduleName(artifact, key.module_name),
            .type_name = try self.typeName(artifact, key.type_name),
        };
    }

    pub fn methodOwner(
        self: *ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        owner: static_dispatch.MethodOwner,
    ) Allocator.Error!static_dispatch.MethodOwner {
        return switch (owner) {
            .nominal => |nominal| .{ .nominal = try self.nominalTypeKey(artifact, nominal) },
            .builtin => |builtin| .{ .builtin = builtin },
        };
    }

    fn namesForArtifact(
        self: *const ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
    ) *const canonical.CanonicalNameStore {
        if (sameArtifact(self.root_key, artifact)) return self.root_names;
        for (self.imports) |imported| {
            if (sameArtifact(imported.key, artifact)) return imported.canonical_names;
        }
        for (self.relation_artifacts) |related| {
            if (sameArtifact(related.key, artifact)) return related.canonical_names;
        }
        debug.invariant(false, "artifact name resolver invariant violated: artifact was not available to lowering");
        unreachable;
    }
};

fn sameArtifact(a: checked_artifact.CheckedModuleArtifactKey, b: checked_artifact.CheckedModuleArtifactKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

fn moduleNameText(names: *const canonical.CanonicalNameStore, id: canonical.ModuleNameId) []const u8 {
    const raw = @intFromEnum(id);
    if (raw >= names.module_names.items.len) invalidNameId();
    return names.module_names.items[raw];
}

fn typeNameText(names: *const canonical.CanonicalNameStore, id: canonical.TypeNameId) []const u8 {
    const raw = @intFromEnum(id);
    if (raw >= names.type_names.items.len) invalidNameId();
    return names.type_names.items[raw];
}

fn methodNameText(names: *const canonical.CanonicalNameStore, id: canonical.MethodNameId) []const u8 {
    const raw = @intFromEnum(id);
    if (raw >= names.method_names.items.len) invalidNameId();
    return names.method_names.items[raw];
}

fn recordFieldLabelText(names: *const canonical.CanonicalNameStore, id: canonical.RecordFieldLabelId) []const u8 {
    const raw = @intFromEnum(id);
    if (raw >= names.record_field_labels.items.len) invalidNameId();
    return names.record_field_labels.items[raw];
}

fn tagLabelText(names: *const canonical.CanonicalNameStore, id: canonical.TagLabelId) []const u8 {
    const raw = @intFromEnum(id);
    if (raw >= names.tag_labels.items.len) invalidNameId();
    return names.tag_labels.items[raw];
}

fn exportNameText(names: *const canonical.CanonicalNameStore, id: canonical.ExportNameId) []const u8 {
    const raw = @intFromEnum(id);
    if (raw >= names.export_names.items.len) invalidNameId();
    return names.export_names.items[raw];
}

fn externalSymbolNameText(names: *const canonical.CanonicalNameStore, id: canonical.ExternalSymbolNameId) []const u8 {
    const raw = @intFromEnum(id);
    if (raw >= names.external_symbol_names.items.len) invalidNameId();
    return names.external_symbol_names.items[raw];
}

fn invalidNameId() noreturn {
    debug.invariant(false, "artifact name resolver invariant violated: canonical name id was outside the source artifact name table");
    unreachable;
}

test "artifact name resolver declarations are referenced" {
    std.testing.refAllDecls(@This());
}

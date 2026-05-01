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

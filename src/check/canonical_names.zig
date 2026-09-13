//! Canonical post-check names and procedure identities.
//!
//! These ids are checked-module boundary data. They are derived from source
//! spellings during checking finalization so post-check stages do not consume
//! module-local `Ident.Idx` values or raw `Symbol` values as checked identity.

const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;

const collections = @import("collections");
const SafeList = collections.SafeList;
const CompactWriter = collections.CompactWriter;
const artifact_serialize = @import("artifact_serialize.zig");

/// Relocatable, serial-id string interner used to back the name stores so the
/// published artifact can be serialized and deserialized with a constant number
/// of relocation fixups.
/// Re-exported from `base` (a general collections-layer data structure, sibling
/// to `SmallStringInterner`). Kept under the `canonical.` namespace because
/// `CanonicalNameStore` is its primary consumer and call sites reference it as
/// `canonical.NameInterner`.
pub const NameInterner = base.SerialStringInterner;

/// Public `ModuleNameId` declaration.
pub const ModuleNameId = enum(u32) { _ };
/// Dense id of a 32-byte deep module content identity interned in a
/// `CanonicalNameStore` (see `base.module_identity`). Store-local: crossing
/// stores rebases through the 32-byte hash, never through name text.
pub const ModuleIdentityId = enum(u32) { _ };
/// Public `TypeNameId` declaration.
pub const TypeNameId = enum(u32) { _ };
/// Public `MethodNameId` declaration.
pub const MethodNameId = enum(u32) { _ };
/// Public `RecordFieldLabelId` declaration.
pub const RecordFieldLabelId = enum(u32) { _ };
/// Public `TagLabelId` declaration.
pub const TagLabelId = enum(u32) { _ };
/// Public `ExportNameId` declaration.
pub const ExportNameId = enum(u32) { _ };
/// Public `ExternalSymbolNameId` declaration.
pub const ExternalSymbolNameId = enum(u32) { _ };

/// Public `ProcBaseKeyRef` declaration.
pub const ProcBaseKeyRef = enum(u32) { _ };
/// Public `CheckedProcedureTemplateId` declaration.
pub const CheckedProcedureTemplateId = enum(u32) { _ };
/// Public `NestedProcSiteId` declaration.
pub const NestedProcSiteId = enum(u32) { _ };
/// Public `HostedWrapperId` declaration.
pub const HostedWrapperId = enum(u32) { _ };
/// Public `IntrinsicWrapperId` declaration.
pub const IntrinsicWrapperId = enum(u32) { _ };
/// Public `EntryWrapperId` declaration.
pub const EntryWrapperId = enum(u32) { _ };

/// Public `ArtifactRef` declaration.
pub const ArtifactRef = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

/// 32-byte deep module content identity carried raw at post-check boundaries
/// (the hash `base.module_identity` computes). Store-independent, unlike the
/// dense `ModuleIdentityId`: consumers resolve it against loaded module views
/// by content identity (design.md "Defaulted Fields", `moduleForIdentityHash`).
pub const ModuleContentIdentity = struct {
    bytes: [32]u8,
};

/// Digest for checked module identity at post-check boundaries.
pub const CheckedModuleDigest = ArtifactRef;

/// Public `ProcedureValueRef` declaration.
pub const ProcedureValueRef = struct {
    artifact: ArtifactRef = .{},
    proc_base: ProcBaseKeyRef,
};

/// Public `ProcedureTemplateRef` declaration.
pub const ProcedureTemplateRef = struct {
    artifact: ArtifactRef = .{},
    proc_base: ProcBaseKeyRef,
    template: CheckedProcedureTemplateId,
};

/// Short name for a checked procedure template reference.
pub const ProcTemplate = ProcedureTemplateRef;

/// Return the checked module digest that owns a procedure value.
pub fn procedureValueModuleDigest(procedure: ProcedureValueRef) CheckedModuleDigest {
    return procedure.artifact;
}

/// Return the checked module digest that owns a procedure template.
pub fn procTemplateModuleDigest(template: ProcTemplate) CheckedModuleDigest {
    return template.artifact;
}

/// Public `MonoSpecializationKey` declaration.
pub const MonoSpecializationKey = struct {
    template: ProcedureTemplateRef,
    requested_mono_fn_ty: CanonicalTypeKey,
};

/// Digest for a checked type shape at post-check boundaries.
pub const TypeDigest = CanonicalTypeKey;
/// Digest for a checked value type that requests a runtime layout.
pub const ExecValueDigest = CanonicalExecValueTypeKey;
/// Short name for the checked boundary name store.
pub const NameStore = CanonicalNameStore;
/// Short name used by post-check records for record field labels.
pub const RecordFieldNameId = RecordFieldLabelId;
/// Short name used by post-check records for tag labels.
pub const TagNameId = TagLabelId;
/// Short name used by post-check records for nested procedure sites.
pub const ProcSiteId = NestedProcSiteId;
/// Short name for a procedure template that may come from checked or lifted code.
pub const CallableProcTemplate = CallableProcedureTemplateRef;

/// Public `MonoSpecializedProcRef` declaration.
pub const MonoSpecializedProcRef = struct {
    proc: ProcedureValueRef,
    specialization: MonoSpecializationKey,
};

/// Public `ProcCallable` declaration.
pub const ProcCallable = struct {
    proc: ProcedureValueRef,
    callable: ProcedureCallableRef,
};

/// Public `procedureValueRefEql` function.
pub fn procedureValueRefEql(a: ProcedureValueRef, b: ProcedureValueRef) bool {
    return std.meta.eql(a.artifact.bytes, b.artifact.bytes) and
        a.proc_base == b.proc_base;
}

/// Public `procedureTemplateRefEql` function.
pub fn procedureTemplateRefEql(a: ProcedureTemplateRef, b: ProcedureTemplateRef) bool {
    return std.meta.eql(a.artifact.bytes, b.artifact.bytes) and
        a.proc_base == b.proc_base and
        a.template == b.template;
}

/// Public `monoSpecializationKeyEql` function.
pub fn monoSpecializationKeyEql(a: MonoSpecializationKey, b: MonoSpecializationKey) bool {
    return std.meta.eql(a.requested_mono_fn_ty.bytes, b.requested_mono_fn_ty.bytes) and
        procedureTemplateRefEql(a.template, b.template);
}

/// Public `monoSpecializedProcRefEql` function.
pub fn monoSpecializedProcRefEql(a: MonoSpecializedProcRef, b: MonoSpecializedProcRef) bool {
    return procedureValueRefEql(a.proc, b.proc) and
        monoSpecializationKeyEql(a.specialization, b.specialization);
}

/// Public `procCallableFromMono` function.
pub fn procCallableFromMono(proc: MonoSpecializedProcRef) ProcCallable {
    return .{
        .proc = proc.proc,
        .callable = .{
            .template = .{ .checked = proc.specialization.template },
            .source_fn_ty = proc.specialization.requested_mono_fn_ty,
        },
    };
}

/// Public `procCallableEql` function.
pub fn procCallableEql(a: ProcCallable, b: ProcCallable) bool {
    return procedureValueRefEql(a.proc, b.proc) and
        procedureCallableRefEql(a.callable, b.callable);
}

/// Public `LiftedProcedureTemplateRef` declaration.
pub const LiftedProcedureTemplateRef = struct {
    owner_mono_specialization: MonoSpecializationKey,
    site: NestedProcSiteId,
};

/// Public `SyntheticProcedureTemplateRef` declaration.
pub const SyntheticProcedureTemplateRef = struct {
    template: ProcedureTemplateRef,
};

/// Public `CallableProcedureTemplateRef` declaration.
pub const CallableProcedureTemplateRef = union(enum) {
    checked: ProcedureTemplateRef,
    lifted: LiftedProcedureTemplateRef,
    synthetic: SyntheticProcedureTemplateRef,
};

/// Public `ProcedureCallableRef` declaration.
pub const ProcedureCallableRef = struct {
    template: CallableProcedureTemplateRef,
    source_fn_ty: CanonicalTypeKey,
};

/// Public `CanonicalExecValueTypeKey` declaration.
pub const CanonicalExecValueTypeKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

/// Public `procedureCallableRefEql` function.
pub fn procedureCallableRefEql(a: ProcedureCallableRef, b: ProcedureCallableRef) bool {
    return callableProcedureTemplateRefEql(a.template, b.template) and
        std.meta.eql(a.source_fn_ty.bytes, b.source_fn_ty.bytes);
}

/// Public `callableProcedureTemplateRefEql` function.
pub fn callableProcedureTemplateRefEql(a: CallableProcedureTemplateRef, b: CallableProcedureTemplateRef) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .checked => |left| procedureTemplateRefEql(left, b.checked),
        .lifted => |left| liftedProcedureTemplateRefEql(left, b.lifted),
        .synthetic => |left| procedureTemplateRefEql(left.template, b.synthetic.template),
    };
}

/// Public `liftedProcedureTemplateRefEql` function.
pub fn liftedProcedureTemplateRefEql(a: LiftedProcedureTemplateRef, b: LiftedProcedureTemplateRef) bool {
    return monoSpecializationKeyEql(a.owner_mono_specialization, b.owner_mono_specialization) and
        a.site == b.site;
}

/// Public `CanonicalTypeKey` declaration.
pub const CanonicalTypeKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

/// Public `CanonicalTypeTemplateKey` declaration.
pub const CanonicalTypeTemplateKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

/// Public `CanonicalTypeSchemeKey` declaration.
pub const CanonicalTypeSchemeKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

/// Public `ProcBaseKind` declaration.
pub const ProcBaseKind = enum {
    checked_source,
    hosted_wrapper,
    intrinsic_wrapper,
    entry_wrapper,
};

/// Public `NestedProcSiteKey` declaration.
pub const NestedProcSiteKey = struct {
    owner_template: ProcedureTemplateRef,
    site: NestedProcSiteId,
};

/// Public `ProcBaseKey` declaration.
pub const ProcBaseKey = struct {
    module_name: ModuleNameId,
    export_name: ?ExportNameId,
    kind: ProcBaseKind,
    ordinal: u32,
    /// Source definition ordinal within the checked CIR module, when this
    /// procedure originates from a source definition.
    source_def_idx: ?u32 = null,
    /// Explicit nested source site for local functions, closures, and desugared
    /// nested procedures. Null for ordinary top-level source procedures.
    nested_proc_site: ?NestedProcSiteKey = null,
    /// Owning mono specialization for lifted local procedures. This is part of
    /// procedure identity because the same nested source site can be lifted from
    /// different monomorphic owner instantiations.
    owner_mono_specialization: ?MonoSpecializationKey = null,
};

/// Public `NominalTypeKey` declaration.
///
/// Identifies a nominal type by (declaring module's deep content identity,
/// declared module-relative name, declaring statement). The statement is a
/// within-module discriminator: block-local declarations may share one name
/// (e.g. two `Local :=` decls in different blocks), and statement indices are
/// stable across artifacts because equal content identities imply
/// byte-identical module source. No module name text participates.
pub const NominalTypeKey = struct {
    module: ModuleIdentityId,
    type_name: TypeNameId,
    source_decl: ?u32 = null,
};

/// Public `CanonicalNameStore` declaration.
pub const CanonicalNameStore = struct {
    /// Build-time allocator used for interner inserts. Not serialized; a
    /// deserialized (frozen) store carries the load allocator only for the
    /// build-only fields below.
    allocator: Allocator,
    // Each name-kind is a relocatable, serial-id interner (transform C). Default
    // `.{}` is a valid empty interner that lazily initializes on first insert.
    module_names: NameInterner = .{},
    /// 32-byte deep module content identities (see `base.module_identity`),
    /// interned like names: dense ids, deduplicated, relocatable. The map
    /// compares full 256-bit values on insert, so a hash collision is
    /// detected (distinct entries), never silently merged.
    module_identities: NameInterner = .{},
    type_names: NameInterner = .{},
    method_names: NameInterner = .{},
    record_field_labels: NameInterner = .{},
    tag_labels: NameInterner = .{},
    export_names: NameInterner = .{},
    external_symbol_names: NameInterner = .{},
    /// Serial id -> structured proc-base key. Relocatable (POD elements).
    proc_bases: SafeList(ProcBaseKey) = .{},
    /// Build-only dedup index for `internProcBase`. NOT serialized: a frozen
    /// store is consumed by id via `procBase`, never re-interned.
    proc_base_by_key: std.StringHashMap(ProcBaseKeyRef),
    /// Build-only scratch buffer for proc-base key encoding. NOT serialized.
    scratch_key: std.ArrayList(u8) = .empty,
    /// True for a store reconstructed from a serialized buffer: its interners
    /// and `proc_bases` point into buffer-owned memory and must not be freed.
    serialized: bool = false,

    /// Build-only dedup/scratch fields excluded from serialization: empty on a
    /// frozen store, so the mixin's `deserialize` resets them (`proc_base_by_key`
    /// via `init(allocator)`, `scratch_key` to its default). Declared so a *data*
    /// field accidentally omitted from `Serialized` is a compile error.
    pub const serde_transient_fields = [_][]const u8{ "proc_base_by_key", "scratch_key" };

    pub fn init(allocator: Allocator) CanonicalNameStore {
        return .{
            .allocator = allocator,
            .proc_base_by_key = std.StringHashMap(ProcBaseKeyRef).init(allocator),
            .scratch_key = .empty,
        };
    }

    pub fn deinit(self: *CanonicalNameStore) void {
        if (!self.serialized) {
            // Interners no-op their own free when frozen, but `proc_bases` is a
            // plain SafeList with no frozen flag, so guard the whole owned set.
            self.module_names.deinit(self.allocator);
            self.module_identities.deinit(self.allocator);
            self.type_names.deinit(self.allocator);
            self.method_names.deinit(self.allocator);
            self.record_field_labels.deinit(self.allocator);
            self.tag_labels.deinit(self.allocator);
            self.export_names.deinit(self.allocator);
            self.external_symbol_names.deinit(self.allocator);
            self.proc_bases.deinit(self.allocator);
        }
        // Build-only fields are always heap-owned (empty on a frozen store).
        freeStringHashMapKeys(ProcBaseKeyRef, &self.proc_base_by_key, self.allocator);
        self.proc_base_by_key.deinit();
        self.scratch_key.deinit(self.allocator);
        self.* = CanonicalNameStore.init(self.allocator);
    }

    /// Immutable append boundary for every canonical-id backing array.
    pub const EpochBoundary = struct {
        module_names: NameInterner.EpochBoundary,
        module_identities: NameInterner.EpochBoundary,
        type_names: NameInterner.EpochBoundary,
        method_names: NameInterner.EpochBoundary,
        record_field_labels: NameInterner.EpochBoundary,
        tag_labels: NameInterner.EpochBoundary,
        export_names: NameInterner.EpochBoundary,
        external_symbol_names: NameInterner.EpochBoundary,
        proc_bases: u32,
    };

    pub fn epochBoundary(self: *const CanonicalNameStore) EpochBoundary {
        return .{
            .module_names = self.module_names.epochBoundary(),
            .module_identities = self.module_identities.epochBoundary(),
            .type_names = self.type_names.epochBoundary(),
            .method_names = self.method_names.epochBoundary(),
            .record_field_labels = self.record_field_labels.epochBoundary(),
            .tag_labels = self.tag_labels.epochBoundary(),
            .export_names = self.export_names.epochBoundary(),
            .external_symbol_names = self.external_symbol_names.epochBoundary(),
            .proc_bases = @intCast(self.proc_bases.items.items.len),
        };
    }

    /// Result-owned canonical-name suffix. String indexes are derived on append;
    /// the proc-base dedup map remains absent because copied epoch stores
    /// are immutable coordinator sources rather than interning destinations.
    pub const EpochDelta = struct {
        allocator: Allocator,
        begin: EpochBoundary,
        end: EpochBoundary,
        module_names: NameInterner.EpochDelta,
        module_identities: NameInterner.EpochDelta,
        type_names: NameInterner.EpochDelta,
        method_names: NameInterner.EpochDelta,
        record_field_labels: NameInterner.EpochDelta,
        tag_labels: NameInterner.EpochDelta,
        export_names: NameInterner.EpochDelta,
        external_symbol_names: NameInterner.EpochDelta,
        proc_bases: []ProcBaseKey,

        pub fn capture(
            allocator: Allocator,
            source: *const CanonicalNameStore,
            begin: EpochBoundary,
            end: EpochBoundary,
        ) Allocator.Error!EpochDelta {
            std.debug.assert(begin.proc_bases <= end.proc_bases);
            std.debug.assert(end.proc_bases <= source.proc_bases.items.items.len);
            var module_names = try NameInterner.EpochDelta.capture(
                allocator,
                &source.module_names,
                begin.module_names,
                end.module_names,
            );
            errdefer module_names.deinit();
            var module_identities = try NameInterner.EpochDelta.capture(
                allocator,
                &source.module_identities,
                begin.module_identities,
                end.module_identities,
            );
            errdefer module_identities.deinit();
            var type_names = try NameInterner.EpochDelta.capture(
                allocator,
                &source.type_names,
                begin.type_names,
                end.type_names,
            );
            errdefer type_names.deinit();
            var method_names = try NameInterner.EpochDelta.capture(
                allocator,
                &source.method_names,
                begin.method_names,
                end.method_names,
            );
            errdefer method_names.deinit();
            var record_field_labels = try NameInterner.EpochDelta.capture(
                allocator,
                &source.record_field_labels,
                begin.record_field_labels,
                end.record_field_labels,
            );
            errdefer record_field_labels.deinit();
            var tag_labels = try NameInterner.EpochDelta.capture(
                allocator,
                &source.tag_labels,
                begin.tag_labels,
                end.tag_labels,
            );
            errdefer tag_labels.deinit();
            var export_names = try NameInterner.EpochDelta.capture(
                allocator,
                &source.export_names,
                begin.export_names,
                end.export_names,
            );
            errdefer export_names.deinit();
            var external_symbol_names = try NameInterner.EpochDelta.capture(
                allocator,
                &source.external_symbol_names,
                begin.external_symbol_names,
                end.external_symbol_names,
            );
            errdefer external_symbol_names.deinit();
            const proc_bases = try allocator.dupe(
                ProcBaseKey,
                source.proc_bases.items.items[begin.proc_bases..end.proc_bases],
            );
            return .{
                .allocator = allocator,
                .begin = begin,
                .end = end,
                .module_names = module_names,
                .module_identities = module_identities,
                .type_names = type_names,
                .method_names = method_names,
                .record_field_labels = record_field_labels,
                .tag_labels = tag_labels,
                .export_names = export_names,
                .external_symbol_names = external_symbol_names,
                .proc_bases = proc_bases,
            };
        }

        /// Reserve every allocation needed to append this exact suffix.
        pub fn prepareAppend(
            self: *const EpochDelta,
            destination: *CanonicalNameStore,
        ) Allocator.Error!void {
            std.debug.assert(std.meta.eql(destination.epochBoundary(), self.begin));
            try self.module_names.prepareAppend(&destination.module_names, destination.allocator);
            try self.module_identities.prepareAppend(&destination.module_identities, destination.allocator);
            try self.type_names.prepareAppend(&destination.type_names, destination.allocator);
            try self.method_names.prepareAppend(&destination.method_names, destination.allocator);
            try self.record_field_labels.prepareAppend(&destination.record_field_labels, destination.allocator);
            try self.tag_labels.prepareAppend(&destination.tag_labels, destination.allocator);
            try self.export_names.prepareAppend(&destination.export_names, destination.allocator);
            try self.external_symbol_names.prepareAppend(&destination.external_symbol_names, destination.allocator);
            try destination.proc_bases.items.ensureTotalCapacity(
                destination.allocator,
                self.end.proc_bases,
            );
        }

        /// Append after `prepareAppend`; no logical mutation can fail.
        pub fn appendPrepared(
            self: *const EpochDelta,
            destination: *CanonicalNameStore,
        ) void {
            std.debug.assert(std.meta.eql(destination.epochBoundary(), self.begin));
            self.module_names.appendPrepared(&destination.module_names, destination.allocator);
            self.module_identities.appendPrepared(&destination.module_identities, destination.allocator);
            self.type_names.appendPrepared(&destination.type_names, destination.allocator);
            self.method_names.appendPrepared(&destination.method_names, destination.allocator);
            self.record_field_labels.appendPrepared(&destination.record_field_labels, destination.allocator);
            self.tag_labels.appendPrepared(&destination.tag_labels, destination.allocator);
            self.export_names.appendPrepared(&destination.export_names, destination.allocator);
            self.external_symbol_names.appendPrepared(&destination.external_symbol_names, destination.allocator);
            destination.proc_bases.items.appendSlice(
                destination.allocator,
                self.proc_bases,
            ) catch unreachable;
            std.debug.assert(std.meta.eql(destination.epochBoundary(), self.end));
        }

        /// Append this suffix after an identical prefix, preserving every id.
        pub fn appendTo(
            self: *const EpochDelta,
            destination: *CanonicalNameStore,
        ) Allocator.Error!void {
            try self.prepareAppend(destination);
            self.appendPrepared(destination);
        }

        pub fn deinit(self: *EpochDelta) void {
            self.allocator.free(self.proc_bases);
            self.external_symbol_names.deinit();
            self.export_names.deinit();
            self.tag_labels.deinit();
            self.record_field_labels.deinit();
            self.method_names.deinit();
            self.type_names.deinit();
            self.module_identities.deinit();
            self.module_names.deinit();
            self.* = undefined;
        }
    };

    /// Relocatable serialized form (build-only dedup/scratch fields excluded).
    pub const Serialized = extern struct {
        module_names: NameInterner.Serialized,
        module_identities: NameInterner.Serialized,
        type_names: NameInterner.Serialized,
        method_names: NameInterner.Serialized,
        record_field_labels: NameInterner.Serialized,
        tag_labels: NameInterner.Serialized,
        export_names: NameInterner.Serialized,
        external_symbol_names: NameInterner.Serialized,
        proc_bases: SafeList(ProcBaseKey).Serialized,

        const Serde = artifact_serialize.SliceStoreSerde(CanonicalNameStore, @This());
        pub const serialize = Serde.serialize;
        pub const deserialize = Serde.deserializeWithAllocator;
    };

    pub fn internModuleName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!ModuleNameId {
        return @enumFromInt(try self.module_names.insert(self.allocator, text));
    }

    pub fn internModuleIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!ModuleNameId {
        return self.internModuleName(idents.getText(ident));
    }

    /// Intern a 32-byte deep module content identity, returning its dense id.
    pub fn internModuleIdentity(self: *CanonicalNameStore, hash: *const [32]u8) Allocator.Error!ModuleIdentityId {
        return @enumFromInt(try self.module_identities.insert(self.allocator, hash));
    }

    /// Look up a module content identity without inserting. This is the single
    /// cross-artifact identity resolution operation (rebase): one map probe per
    /// distinct identity, comparing full 256-bit values.
    pub fn lookupModuleIdentity(self: *const CanonicalNameStore, hash: *const [32]u8) ?ModuleIdentityId {
        const id = self.module_identities.lookup(hash) orelse return null;
        return @enumFromInt(id);
    }

    /// The 32-byte content identity for a dense id in this store.
    pub fn moduleIdentityBytes(self: *const CanonicalNameStore, id: ModuleIdentityId) *const [32]u8 {
        const bytes = self.module_identities.getText(@intFromEnum(id));
        std.debug.assert(bytes.len == 32);
        return @ptrCast(bytes.ptr);
    }

    pub fn internTypeIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!TypeNameId {
        return self.internTypeName(idents.getText(ident));
    }

    pub fn internTypeName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!TypeNameId {
        return @enumFromInt(try self.type_names.insert(self.allocator, text));
    }

    pub fn internMethodIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!MethodNameId {
        return self.internMethodName(idents.getText(ident));
    }

    pub fn internMethodName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!MethodNameId {
        return @enumFromInt(try self.method_names.insert(self.allocator, text));
    }

    pub fn internRecordFieldIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!RecordFieldLabelId {
        return self.internRecordFieldLabel(idents.getText(ident));
    }

    pub fn internRecordFieldLabel(self: *CanonicalNameStore, text: []const u8) Allocator.Error!RecordFieldLabelId {
        return @enumFromInt(try self.record_field_labels.insert(self.allocator, text));
    }

    pub fn internTagIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!TagLabelId {
        return self.internTagLabel(idents.getText(ident));
    }

    pub fn internTagLabel(self: *CanonicalNameStore, text: []const u8) Allocator.Error!TagLabelId {
        return @enumFromInt(try self.tag_labels.insert(self.allocator, text));
    }

    pub fn internExportIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!ExportNameId {
        return self.internExportName(idents.getText(ident));
    }

    pub fn internExportName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!ExportNameId {
        return @enumFromInt(try self.export_names.insert(self.allocator, text));
    }

    pub fn internExternalSymbolIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!ExternalSymbolNameId {
        return self.internExternalSymbolName(idents.getText(ident));
    }

    pub fn internExternalSymbolName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!ExternalSymbolNameId {
        return @enumFromInt(try self.external_symbol_names.insert(self.allocator, text));
    }

    fn lookupId(comptime Id: type, it: *const NameInterner, text: []const u8) ?Id {
        return if (it.lookup(text)) |id| @as(Id, @enumFromInt(id)) else null;
    }

    pub fn lookupModuleIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?ModuleNameId {
        return lookupId(ModuleNameId, &self.module_names, idents.getText(ident));
    }

    pub fn lookupTypeIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?TypeNameId {
        return lookupId(TypeNameId, &self.type_names, idents.getText(ident));
    }

    pub fn lookupMethodIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?MethodNameId {
        return lookupId(MethodNameId, &self.method_names, idents.getText(ident));
    }

    pub fn lookupRecordFieldIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?RecordFieldLabelId {
        return lookupId(RecordFieldLabelId, &self.record_field_labels, idents.getText(ident));
    }

    pub fn lookupTagIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?TagLabelId {
        return lookupId(TagLabelId, &self.tag_labels, idents.getText(ident));
    }

    pub fn lookupExportIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?ExportNameId {
        return lookupId(ExportNameId, &self.export_names, idents.getText(ident));
    }

    pub fn lookupExternalSymbolIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?ExternalSymbolNameId {
        return lookupId(ExternalSymbolNameId, &self.external_symbol_names, idents.getText(ident));
    }

    pub fn lookupModuleName(self: *const CanonicalNameStore, text: []const u8) ?ModuleNameId {
        return lookupId(ModuleNameId, &self.module_names, text);
    }

    pub fn lookupTypeName(self: *const CanonicalNameStore, text: []const u8) ?TypeNameId {
        return lookupId(TypeNameId, &self.type_names, text);
    }

    pub fn lookupMethodName(self: *const CanonicalNameStore, text: []const u8) ?MethodNameId {
        return lookupId(MethodNameId, &self.method_names, text);
    }

    pub fn lookupTagLabel(self: *const CanonicalNameStore, text: []const u8) ?TagLabelId {
        return lookupId(TagLabelId, &self.tag_labels, text);
    }

    pub fn lookupExportName(self: *const CanonicalNameStore, text: []const u8) ?ExportNameId {
        return lookupId(ExportNameId, &self.export_names, text);
    }

    pub fn internProcBase(self: *CanonicalNameStore, key: ProcBaseKey) Allocator.Error!ProcBaseKeyRef {
        self.scratch_key.clearRetainingCapacity();
        try self.scratch_key.print(self.allocator, "proc:{d}:{s}:{d}:{d}:{d}|", .{
            @intFromEnum(key.module_name),
            @tagName(key.kind),
            if (key.export_name) |name| @intFromEnum(name) else std.math.maxInt(u32),
            key.ordinal,
            key.source_def_idx orelse std.math.maxInt(u32),
        });
        try appendOptionalNestedProcSiteKey(&self.scratch_key, key.nested_proc_site, self.allocator);
        try appendOptionalMonoSpecializationKey(&self.scratch_key, key.owner_mono_specialization, self.allocator);

        if (self.proc_base_by_key.get(self.scratch_key.items)) |existing| return existing;

        const id: ProcBaseKeyRef = @enumFromInt(@as(u32, @intCast(self.proc_bases.items.items.len)));
        const owned_key = try self.allocator.dupe(u8, self.scratch_key.items);
        errdefer self.allocator.free(owned_key);

        _ = try self.proc_bases.append(self.allocator, key);
        try self.proc_base_by_key.put(owned_key, id);
        return id;
    }

    pub fn procBase(self: *const CanonicalNameStore, id: ProcBaseKeyRef) ProcBaseKey {
        return self.proc_bases.items.items[@intFromEnum(id)];
    }

    pub fn exportNameText(self: *const CanonicalNameStore, id: ExportNameId) []const u8 {
        return self.export_names.getText(@intFromEnum(id));
    }

    pub fn moduleNameText(self: *const CanonicalNameStore, id: ModuleNameId) []const u8 {
        return self.module_names.getText(@intFromEnum(id));
    }

    pub fn typeNameText(self: *const CanonicalNameStore, id: TypeNameId) []const u8 {
        return self.type_names.getText(@intFromEnum(id));
    }

    pub fn methodNameText(self: *const CanonicalNameStore, id: MethodNameId) []const u8 {
        return self.method_names.getText(@intFromEnum(id));
    }

    pub fn recordFieldLabelText(self: *const CanonicalNameStore, id: RecordFieldLabelId) []const u8 {
        return self.record_field_labels.getText(@intFromEnum(id));
    }

    /// Whether a record field label id has interned text. Real compilation
    /// always interns every label; minimal test fixtures may reference label
    /// ids without registering their text.
    pub fn recordFieldLabelTextInterned(self: *const CanonicalNameStore, id: RecordFieldLabelId) bool {
        return @intFromEnum(id) < self.record_field_labels.count();
    }

    pub fn recordFieldLabelCount(self: *const CanonicalNameStore) u32 {
        return self.record_field_labels.count();
    }

    /// Compare two record field label ids by their canonical text.
    pub fn recordFieldLabelTextEql(self: *const CanonicalNameStore, a: RecordFieldLabelId, b: RecordFieldLabelId) bool {
        return Ident.textEql(self.recordFieldLabelText(a), self.recordFieldLabelText(b));
    }

    /// Order record field labels by their canonical text.
    pub fn recordFieldLabelTextLessThan(self: *const CanonicalNameStore, a: RecordFieldLabelId, b: RecordFieldLabelId) bool {
        return Ident.textLessThan(self.recordFieldLabelText(a), self.recordFieldLabelText(b));
    }

    pub fn tagLabelText(self: *const CanonicalNameStore, id: TagLabelId) []const u8 {
        return self.tag_labels.getText(@intFromEnum(id));
    }

    pub fn tagLabelCount(self: *const CanonicalNameStore) u32 {
        return self.tag_labels.count();
    }

    /// Compare two tag label ids by their canonical text.
    pub fn tagLabelTextEql(self: *const CanonicalNameStore, a: TagLabelId, b: TagLabelId) bool {
        return Ident.textEql(self.tagLabelText(a), self.tagLabelText(b));
    }

    /// Order tag labels by their canonical text.
    pub fn tagLabelTextLessThan(self: *const CanonicalNameStore, a: TagLabelId, b: TagLabelId) bool {
        return Ident.textLessThan(self.tagLabelText(a), self.tagLabelText(b));
    }

    pub fn externalSymbolNameText(self: *const CanonicalNameStore, id: ExternalSymbolNameId) []const u8 {
        return self.external_symbol_names.getText(@intFromEnum(id));
    }
};

/// Cumulative, store-qualified translation for dense canonical-name ids.
///
/// Type and syntax transfer can share this object so a source name is interned
/// in the destination once even when it appears in both representations. Name
/// interning is monotonic: an allocation failure may leave destination text
/// interned, while the corresponding id mapping is added only after it succeeds.
/// Procedure-base refs remain coordinator identities rather than body-local names.
pub const NameRelocation = struct {
    source: *const CanonicalNameStore,
    destination: *CanonicalNameStore,
    module_names: collections.DenseMap(ModuleNameId, ModuleNameId),
    module_identities: collections.DenseMap(ModuleIdentityId, ModuleIdentityId),
    type_names: collections.DenseMap(TypeNameId, TypeNameId),
    method_names: collections.DenseMap(MethodNameId, MethodNameId),
    record_field_labels: collections.DenseMap(RecordFieldLabelId, RecordFieldLabelId),
    tag_labels: collections.DenseMap(TagLabelId, TagLabelId),
    export_names: collections.DenseMap(ExportNameId, ExportNameId),
    external_symbol_names: collections.DenseMap(ExternalSymbolNameId, ExternalSymbolNameId),

    pub fn init(
        allocator: Allocator,
        source: *const CanonicalNameStore,
        destination: *CanonicalNameStore,
    ) NameRelocation {
        return .{
            .source = source,
            .destination = destination,
            .module_names = collections.DenseMap(ModuleNameId, ModuleNameId).init(allocator),
            .module_identities = collections.DenseMap(ModuleIdentityId, ModuleIdentityId).init(allocator),
            .type_names = collections.DenseMap(TypeNameId, TypeNameId).init(allocator),
            .method_names = collections.DenseMap(MethodNameId, MethodNameId).init(allocator),
            .record_field_labels = collections.DenseMap(RecordFieldLabelId, RecordFieldLabelId).init(allocator),
            .tag_labels = collections.DenseMap(TagLabelId, TagLabelId).init(allocator),
            .export_names = collections.DenseMap(ExportNameId, ExportNameId).init(allocator),
            .external_symbol_names = collections.DenseMap(ExternalSymbolNameId, ExternalSymbolNameId).init(allocator),
        };
    }

    pub fn deinit(self: *NameRelocation) void {
        self.external_symbol_names.deinit();
        self.export_names.deinit();
        self.tag_labels.deinit();
        self.record_field_labels.deinit();
        self.method_names.deinit();
        self.type_names.deinit();
        self.module_identities.deinit();
        self.module_names.deinit();
        self.* = undefined;
    }

    pub fn mappedCount(self: *const NameRelocation) usize {
        return self.module_names.count() +
            self.module_identities.count() +
            self.type_names.count() +
            self.method_names.count() +
            self.record_field_labels.count() +
            self.tag_labels.count() +
            self.export_names.count() +
            self.external_symbol_names.count();
    }

    fn requireSource(self: *const NameRelocation, source: *const CanonicalNameStore) void {
        if (source != self.source) {
            @panic("canonical name relocation used with an unrelated source store");
        }
    }

    fn relocateText(
        self: *NameRelocation,
        comptime Id: type,
        source: *const CanonicalNameStore,
        id: Id,
        map: *collections.DenseMap(Id, Id),
    ) Allocator.Error!Id {
        self.requireSource(source);
        if (source == self.destination) return id;
        if (map.get(id)) |mapped| return mapped;

        const mapped: Id = if (Id == ModuleNameId)
            try self.destination.internModuleName(source.moduleNameText(id))
        else if (Id == TypeNameId)
            try self.destination.internTypeName(source.typeNameText(id))
        else if (Id == MethodNameId)
            try self.destination.internMethodName(source.methodNameText(id))
        else if (Id == RecordFieldLabelId)
            try self.destination.internRecordFieldLabel(source.recordFieldLabelText(id))
        else if (Id == TagLabelId)
            try self.destination.internTagLabel(source.tagLabelText(id))
        else if (Id == ExportNameId)
            try self.destination.internExportName(source.exportNameText(id))
        else if (Id == ExternalSymbolNameId)
            try self.destination.internExternalSymbolName(source.externalSymbolNameText(id))
        else
            @compileError("unsupported canonical text-name relocation domain");

        try map.put(id, mapped);
        return mapped;
    }

    pub fn relocateModuleName(
        self: *NameRelocation,
        source: *const CanonicalNameStore,
        id: ModuleNameId,
    ) Allocator.Error!ModuleNameId {
        return self.relocateText(ModuleNameId, source, id, &self.module_names);
    }

    pub fn relocateModuleIdentity(
        self: *NameRelocation,
        source: *const CanonicalNameStore,
        id: ModuleIdentityId,
    ) Allocator.Error!ModuleIdentityId {
        self.requireSource(source);
        if (source == self.destination) return id;
        if (self.module_identities.get(id)) |mapped| return mapped;
        const mapped = try self.destination.internModuleIdentity(source.moduleIdentityBytes(id));
        try self.module_identities.put(id, mapped);
        return mapped;
    }

    pub fn relocateTypeName(
        self: *NameRelocation,
        source: *const CanonicalNameStore,
        id: TypeNameId,
    ) Allocator.Error!TypeNameId {
        return self.relocateText(TypeNameId, source, id, &self.type_names);
    }

    pub fn relocateMethodName(
        self: *NameRelocation,
        source: *const CanonicalNameStore,
        id: MethodNameId,
    ) Allocator.Error!MethodNameId {
        return self.relocateText(MethodNameId, source, id, &self.method_names);
    }

    pub fn relocateRecordFieldLabel(
        self: *NameRelocation,
        source: *const CanonicalNameStore,
        id: RecordFieldLabelId,
    ) Allocator.Error!RecordFieldLabelId {
        return self.relocateText(RecordFieldLabelId, source, id, &self.record_field_labels);
    }

    pub fn relocateTagLabel(
        self: *NameRelocation,
        source: *const CanonicalNameStore,
        id: TagLabelId,
    ) Allocator.Error!TagLabelId {
        return self.relocateText(TagLabelId, source, id, &self.tag_labels);
    }

    pub fn relocateExportName(
        self: *NameRelocation,
        source: *const CanonicalNameStore,
        id: ExportNameId,
    ) Allocator.Error!ExportNameId {
        return self.relocateText(ExportNameId, source, id, &self.export_names);
    }

    pub fn relocateExternalSymbolName(
        self: *NameRelocation,
        source: *const CanonicalNameStore,
        id: ExternalSymbolNameId,
    ) Allocator.Error!ExternalSymbolNameId {
        return self.relocateText(ExternalSymbolNameId, source, id, &self.external_symbol_names);
    }
};

fn appendOptionalNestedProcSiteKey(
    scratch: *std.ArrayList(u8),
    maybe_key: ?NestedProcSiteKey,
    allocator: Allocator,
) Allocator.Error!void {
    if (maybe_key) |key| {
        try scratch.append(allocator, 1);
        try appendProcedureTemplateRef(scratch, key.owner_template, allocator);
        try scratch.print(allocator, "site:{d}|", .{@intFromEnum(key.site)});
    } else {
        try scratch.append(allocator, 0);
    }
}

fn appendOptionalMonoSpecializationKey(
    scratch: *std.ArrayList(u8),
    maybe_key: ?MonoSpecializationKey,
    allocator: Allocator,
) Allocator.Error!void {
    if (maybe_key) |key| {
        try scratch.append(allocator, 1);
        try appendMonoSpecializationKey(scratch, key, allocator);
    } else {
        try scratch.append(allocator, 0);
    }
}

fn appendMonoSpecializationKey(
    scratch: *std.ArrayList(u8),
    key: MonoSpecializationKey,
    allocator: Allocator,
) Allocator.Error!void {
    try appendProcedureTemplateRef(scratch, key.template, allocator);
    try scratch.appendSlice(allocator, key.requested_mono_fn_ty.bytes[0..]);
    try scratch.append(allocator, '|');
}

fn appendProcedureTemplateRef(
    scratch: *std.ArrayList(u8),
    ref: ProcedureTemplateRef,
    allocator: Allocator,
) Allocator.Error!void {
    try scratch.print(allocator, "template:{d}:{d}:", .{
        @intFromEnum(ref.proc_base),
        @intFromEnum(ref.template),
    });
    try scratch.appendSlice(allocator, ref.artifact.bytes[0..]);
    try scratch.append(allocator, '|');
}

fn freeStringHashMapKeys(comptime V: type, map: *std.StringHashMap(V), allocator: Allocator) void {
    var keys = map.keyIterator();
    while (keys.next()) |key| allocator.free(key.*);
}

test "canonical names dedupe by text" {
    var names = CanonicalNameStore.init(std.testing.allocator);
    defer names.deinit();

    const a = try names.internModuleName("Main");
    const b = try names.internModuleName("Main");
    try std.testing.expectEqual(a, b);
}

test "canonical name relocation qualifies every text domain by owning stores" {
    const allocator = std.testing.allocator;
    var source = CanonicalNameStore.init(allocator);
    defer source.deinit();
    var destination = CanonicalNameStore.init(allocator);
    defer destination.deinit();

    _ = try destination.internModuleName("Unrelated");
    _ = try destination.internModuleIdentity(&([_]u8{0xFF} ** 32));
    _ = try destination.internTypeName("Unrelated");
    _ = try destination.internMethodName("unrelated");
    _ = try destination.internRecordFieldLabel("unrelated");
    _ = try destination.internTagLabel("Unrelated");
    _ = try destination.internExportName("unrelated!");
    _ = try destination.internExternalSymbolName("unrelated_external");

    const module_name = try source.internModuleName("Main");
    const module_identity = try source.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try source.internTypeName("Model");
    const method_name = try source.internMethodName("render");
    const field_name = try source.internRecordFieldLabel("value");
    const tag_name = try source.internTagLabel("Value");
    const export_name = try source.internExportName("main!");
    const external_name = try source.internExternalSymbolName("roc_main");

    var relocation = NameRelocation.init(allocator, &source, &destination);
    defer relocation.deinit();

    const relocated_module_name = try relocation.relocateModuleName(&source, module_name);
    const relocated_module_identity = try relocation.relocateModuleIdentity(&source, module_identity);
    const relocated_type_name = try relocation.relocateTypeName(&source, type_name);
    const relocated_method_name = try relocation.relocateMethodName(&source, method_name);
    const relocated_field_name = try relocation.relocateRecordFieldLabel(&source, field_name);
    const relocated_tag_name = try relocation.relocateTagLabel(&source, tag_name);
    const relocated_export_name = try relocation.relocateExportName(&source, export_name);
    const relocated_external_name = try relocation.relocateExternalSymbolName(&source, external_name);

    try std.testing.expect(module_name != relocated_module_name);
    try std.testing.expect(module_identity != relocated_module_identity);
    try std.testing.expect(type_name != relocated_type_name);
    try std.testing.expect(method_name != relocated_method_name);
    try std.testing.expect(field_name != relocated_field_name);
    try std.testing.expect(tag_name != relocated_tag_name);
    try std.testing.expect(export_name != relocated_export_name);
    try std.testing.expect(external_name != relocated_external_name);
    try std.testing.expectEqualStrings("Main", destination.moduleNameText(relocated_module_name));
    try std.testing.expectEqualSlices(u8, source.moduleIdentityBytes(module_identity), destination.moduleIdentityBytes(relocated_module_identity));
    try std.testing.expectEqualStrings("Model", destination.typeNameText(relocated_type_name));
    try std.testing.expectEqualStrings("render", destination.methodNameText(relocated_method_name));
    try std.testing.expectEqualStrings("value", destination.recordFieldLabelText(relocated_field_name));
    try std.testing.expectEqualStrings("Value", destination.tagLabelText(relocated_tag_name));
    try std.testing.expectEqualStrings("main!", destination.exportNameText(relocated_export_name));
    try std.testing.expectEqualStrings("roc_main", destination.externalSymbolNameText(relocated_external_name));
    try std.testing.expectEqual(@as(usize, 8), relocation.mappedCount());

    try std.testing.expectEqual(relocated_field_name, try relocation.relocateRecordFieldLabel(&source, field_name));
    try std.testing.expectEqual(@as(usize, 8), relocation.mappedCount());

    var shared = NameRelocation.init(allocator, &source, &source);
    defer shared.deinit();
    try std.testing.expectEqual(tag_name, try shared.relocateTagLabel(&source, tag_name));
    try std.testing.expectEqual(@as(usize, 0), shared.mappedCount());
}

test "canonical name epoch deltas own consecutive id ranges" {
    const allocator = std.testing.allocator;
    var source = CanonicalNameStore.init(allocator);
    const start = source.epochBoundary();
    var empty = try CanonicalNameStore.EpochDelta.capture(
        allocator,
        &source,
        start,
        start,
    );
    defer empty.deinit();
    try std.testing.expectEqual(@as(usize, 0), empty.proc_bases.len);

    const module_name = try source.internModuleName("Main");
    const module_identity = try source.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try source.internTypeName("Model");
    const method_name = try source.internMethodName("render");
    const field_name = try source.internRecordFieldLabel("value");
    const tag_name = try source.internTagLabel("Value");
    const export_name = try source.internExportName("main!");
    const external_name = try source.internExternalSymbolName("roc_main");
    const proc_base = try source.internProcBase(.{
        .module_name = module_name,
        .export_name = export_name,
        .kind = .checked_source,
        .ordinal = 7,
    });
    const middle = source.epochBoundary();
    var first = try CanonicalNameStore.EpochDelta.capture(
        allocator,
        &source,
        start,
        middle,
    );
    defer first.deinit();

    const second_field = try source.internRecordFieldLabel("next");
    const second_tag = try source.internTagLabel("Next");
    const end = source.epochBoundary();
    var second = try CanonicalNameStore.EpochDelta.capture(
        allocator,
        &source,
        middle,
        end,
    );
    defer second.deinit();

    var index: usize = 0;
    while (index < 256) : (index += 1) {
        var buffer: [32]u8 = undefined;
        const text = try std.fmt.bufPrint(&buffer, "growth_{d}", .{index});
        _ = try source.internRecordFieldLabel(text);
        _ = try source.internTagLabel(text);
    }
    source.deinit();

    var destination = CanonicalNameStore.init(allocator);
    defer destination.deinit();
    try empty.appendTo(&destination);
    try first.appendTo(&destination);
    try second.appendTo(&destination);
    try std.testing.expectEqualStrings("Main", destination.moduleNameText(module_name));
    try std.testing.expectEqualSlices(u8, &([_]u8{0xAB} ** 32), destination.moduleIdentityBytes(module_identity));
    try std.testing.expectEqualStrings("Model", destination.typeNameText(type_name));
    try std.testing.expectEqualStrings("render", destination.methodNameText(method_name));
    try std.testing.expectEqualStrings("value", destination.recordFieldLabelText(field_name));
    try std.testing.expectEqualStrings("Value", destination.tagLabelText(tag_name));
    try std.testing.expectEqualStrings("main!", destination.exportNameText(export_name));
    try std.testing.expectEqualStrings("roc_main", destination.externalSymbolNameText(external_name));
    try std.testing.expectEqualStrings("next", destination.recordFieldLabelText(second_field));
    try std.testing.expectEqualStrings("Next", destination.tagLabelText(second_tag));
    try std.testing.expectEqual(module_name, destination.procBase(proc_base).module_name);
    try std.testing.expectEqual(export_name, destination.procBase(proc_base).export_name.?);
}

test "proc base identity includes nested owner mono specialization" {
    var names = CanonicalNameStore.init(std.testing.allocator);
    defer names.deinit();

    const module_name = try names.internModuleName("Main");
    const owner_base = try names.internProcBase(.{
        .module_name = module_name,
        .export_name = null,
        .kind = .checked_source,
        .ordinal = 1,
        .source_def_idx = 1,
    });
    const first_template_index: u32 = 0;
    const owner_template = ProcedureTemplateRef{
        .artifact = .{ .bytes = [_]u8{1} ** 32 },
        .proc_base = owner_base,
        .template = @enumFromInt(first_template_index),
    };

    var i64_key = CanonicalTypeKey{};
    i64_key.bytes[0] = 1;
    var str_key = CanonicalTypeKey{};
    str_key.bytes[0] = 2;

    const first_site_index: u32 = 0;
    const nested_site = NestedProcSiteKey{
        .owner_template = owner_template,
        .site = @enumFromInt(first_site_index),
    };
    const lifted_i64 = try names.internProcBase(.{
        .module_name = module_name,
        .export_name = null,
        .kind = .checked_source,
        .ordinal = 2,
        .nested_proc_site = nested_site,
        .owner_mono_specialization = .{
            .template = owner_template,
            .requested_mono_fn_ty = i64_key,
        },
    });
    const lifted_str = try names.internProcBase(.{
        .module_name = module_name,
        .export_name = null,
        .kind = .checked_source,
        .ordinal = 2,
        .nested_proc_site = nested_site,
        .owner_mono_specialization = .{
            .template = owner_template,
            .requested_mono_fn_ty = str_key,
        },
    });

    try std.testing.expect(lifted_i64 != lifted_str);
}

test "CanonicalNameStore: serialize/deserialize round-trip preserves names, ids, and proc bases" {
    const gpa = std.testing.allocator;
    var store = CanonicalNameStore.init(gpa);
    defer store.deinit();

    const m = try store.internModuleName("Builtin");
    const t_list = try store.internTypeName("List");
    const t_dict = try store.internTypeName("Dict");
    const meth = try store.internMethodName("map");
    const exp = try store.internExportName("main!");
    const tag = try store.internTagLabel("Ok");
    const field = try store.internRecordFieldLabel("x");

    const pb = try store.internProcBase(.{
        .module_name = m,
        .export_name = exp,
        .kind = .checked_source,
        .ordinal = 7,
    });
    // dedup returns the same ref
    try std.testing.expectEqual(pb, try store.internProcBase(.{
        .module_name = m,
        .export_name = exp,
        .kind = .checked_source,
        .ordinal = 7,
    }));

    // Serialize via CompactWriter into a 16-byte-aligned buffer, then deserialize.
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();
    var writer = collections.CompactWriter.init();
    const hdr = try writer.appendAlloc(aa, CanonicalNameStore.Serialized);
    try hdr.serialize(&store, aa, &writer);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);

    const ser: *const CanonicalNameStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var loaded = ser.deserialize(@intFromPtr(buffer.ptr), gpa);
    defer loaded.deinit();

    // id -> text resolves against the relocated buffer
    try std.testing.expectEqualStrings("Builtin", loaded.moduleNameText(m));
    try std.testing.expectEqualStrings("List", loaded.typeNameText(t_list));
    try std.testing.expectEqualStrings("Dict", loaded.typeNameText(t_dict));
    try std.testing.expectEqualStrings("map", loaded.methodNameText(meth));
    try std.testing.expectEqualStrings("main!", loaded.exportNameText(exp));
    try std.testing.expectEqualStrings("Ok", loaded.tagLabelText(tag));
    try std.testing.expectEqualStrings("x", loaded.recordFieldLabelText(field));

    // text -> id (frozen lookup, no re-form) returns the same ids
    try std.testing.expectEqual(@as(?TypeNameId, t_list), loaded.lookupTypeName("List"));
    try std.testing.expectEqual(@as(?TypeNameId, t_dict), loaded.lookupTypeName("Dict"));
    try std.testing.expectEqual(@as(?TypeNameId, null), loaded.lookupTypeName("Set"));
    try std.testing.expectEqual(@as(?ModuleNameId, m), loaded.lookupModuleName("Builtin"));

    // proc base resolves by id against the relocated SafeList
    const loaded_pb = loaded.procBase(pb);
    try std.testing.expectEqual(m, loaded_pb.module_name);
    try std.testing.expectEqual(@as(?ExportNameId, exp), loaded_pb.export_name);
    try std.testing.expectEqual(@as(u32, 7), loaded_pb.ordinal);
}

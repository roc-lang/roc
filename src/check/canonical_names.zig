//! Canonical post-check names and procedure identities.
//!
//! These ids are checked-module boundary data. They are derived from source
//! spellings during checking finalization so post-check stages do not consume
//! module-local `Ident.Idx` values or raw `Symbol` values as checked identity.

const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;

/// Public `ModuleNameId` declaration.
pub const ModuleNameId = enum(u32) { _ };
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
pub const NominalTypeKey = struct {
    module_name: ModuleNameId,
    type_name: TypeNameId,
    source_decl: ?u32 = null,
};

/// Public `CanonicalNameStore` declaration.
pub const CanonicalNameStore = struct {
    allocator: Allocator,
    module_names: std.ArrayList([]const u8),
    module_name_by_text: std.StringHashMap(ModuleNameId),
    type_names: std.ArrayList([]const u8),
    type_name_by_text: std.StringHashMap(TypeNameId),
    method_names: std.ArrayList([]const u8),
    method_name_by_text: std.StringHashMap(MethodNameId),
    record_field_labels: std.ArrayList([]const u8),
    record_field_label_by_text: std.StringHashMap(RecordFieldLabelId),
    tag_labels: std.ArrayList([]const u8),
    tag_label_by_text: std.StringHashMap(TagLabelId),
    export_names: std.ArrayList([]const u8),
    export_name_by_text: std.StringHashMap(ExportNameId),
    external_symbol_names: std.ArrayList([]const u8),
    external_symbol_name_by_text: std.StringHashMap(ExternalSymbolNameId),
    proc_bases: std.ArrayList(ProcBaseKey),
    proc_base_by_key: std.StringHashMap(ProcBaseKeyRef),
    scratch_key: std.ArrayList(u8),

    pub fn init(allocator: Allocator) CanonicalNameStore {
        return .{
            .allocator = allocator,
            .module_names = .empty,
            .module_name_by_text = std.StringHashMap(ModuleNameId).init(allocator),
            .type_names = .empty,
            .type_name_by_text = std.StringHashMap(TypeNameId).init(allocator),
            .method_names = .empty,
            .method_name_by_text = std.StringHashMap(MethodNameId).init(allocator),
            .record_field_labels = .empty,
            .record_field_label_by_text = std.StringHashMap(RecordFieldLabelId).init(allocator),
            .tag_labels = .empty,
            .tag_label_by_text = std.StringHashMap(TagLabelId).init(allocator),
            .export_names = .empty,
            .export_name_by_text = std.StringHashMap(ExportNameId).init(allocator),
            .external_symbol_names = .empty,
            .external_symbol_name_by_text = std.StringHashMap(ExternalSymbolNameId).init(allocator),
            .proc_bases = .empty,
            .proc_base_by_key = std.StringHashMap(ProcBaseKeyRef).init(allocator),
            .scratch_key = .empty,
        };
    }

    pub fn deinit(self: *CanonicalNameStore) void {
        self.freeTextList(self.module_names.items);
        self.freeTextList(self.type_names.items);
        self.freeTextList(self.method_names.items);
        self.freeTextList(self.record_field_labels.items);
        self.freeTextList(self.tag_labels.items);
        self.freeTextList(self.export_names.items);
        self.freeTextList(self.external_symbol_names.items);
        freeStringHashMapKeys(ProcBaseKeyRef, &self.proc_base_by_key, self.allocator);
        self.scratch_key.deinit(self.allocator);
        self.proc_base_by_key.deinit();
        self.proc_bases.deinit(self.allocator);
        self.external_symbol_name_by_text.deinit();
        self.external_symbol_names.deinit(self.allocator);
        self.export_name_by_text.deinit();
        self.export_names.deinit(self.allocator);
        self.tag_label_by_text.deinit();
        self.tag_labels.deinit(self.allocator);
        self.record_field_label_by_text.deinit();
        self.record_field_labels.deinit(self.allocator);
        self.method_name_by_text.deinit();
        self.method_names.deinit(self.allocator);
        self.type_name_by_text.deinit();
        self.type_names.deinit(self.allocator);
        self.module_name_by_text.deinit();
        self.module_names.deinit(self.allocator);
        self.* = CanonicalNameStore.init(self.allocator);
    }

    pub fn internModuleName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!ModuleNameId {
        return internText(ModuleNameId, self.allocator, &self.module_names, &self.module_name_by_text, text);
    }

    pub fn internModuleIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!ModuleNameId {
        return self.internModuleName(idents.getText(ident));
    }

    pub fn internTypeIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!TypeNameId {
        return internText(TypeNameId, self.allocator, &self.type_names, &self.type_name_by_text, idents.getText(ident));
    }

    pub fn internTypeName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!TypeNameId {
        return internText(TypeNameId, self.allocator, &self.type_names, &self.type_name_by_text, text);
    }

    pub fn internMethodIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!MethodNameId {
        return internText(MethodNameId, self.allocator, &self.method_names, &self.method_name_by_text, idents.getText(ident));
    }

    pub fn internMethodName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!MethodNameId {
        return internText(MethodNameId, self.allocator, &self.method_names, &self.method_name_by_text, text);
    }

    pub fn internRecordFieldIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!RecordFieldLabelId {
        return internText(RecordFieldLabelId, self.allocator, &self.record_field_labels, &self.record_field_label_by_text, idents.getText(ident));
    }

    pub fn internRecordFieldLabel(self: *CanonicalNameStore, text: []const u8) Allocator.Error!RecordFieldLabelId {
        return internText(RecordFieldLabelId, self.allocator, &self.record_field_labels, &self.record_field_label_by_text, text);
    }

    pub fn internTagIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!TagLabelId {
        return internText(TagLabelId, self.allocator, &self.tag_labels, &self.tag_label_by_text, idents.getText(ident));
    }

    pub fn internTagLabel(self: *CanonicalNameStore, text: []const u8) Allocator.Error!TagLabelId {
        return internText(TagLabelId, self.allocator, &self.tag_labels, &self.tag_label_by_text, text);
    }

    pub fn internExportIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!ExportNameId {
        return internText(ExportNameId, self.allocator, &self.export_names, &self.export_name_by_text, idents.getText(ident));
    }

    pub fn internExportName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!ExportNameId {
        return internText(ExportNameId, self.allocator, &self.export_names, &self.export_name_by_text, text);
    }

    pub fn internExternalSymbolIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!ExternalSymbolNameId {
        return internText(ExternalSymbolNameId, self.allocator, &self.external_symbol_names, &self.external_symbol_name_by_text, idents.getText(ident));
    }

    pub fn internExternalSymbolName(self: *CanonicalNameStore, text: []const u8) Allocator.Error!ExternalSymbolNameId {
        return internText(ExternalSymbolNameId, self.allocator, &self.external_symbol_names, &self.external_symbol_name_by_text, text);
    }

    pub fn lookupModuleIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?ModuleNameId {
        return self.module_name_by_text.get(idents.getText(ident));
    }

    pub fn lookupTypeIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?TypeNameId {
        return self.type_name_by_text.get(idents.getText(ident));
    }

    pub fn lookupMethodIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?MethodNameId {
        return self.method_name_by_text.get(idents.getText(ident));
    }

    pub fn lookupRecordFieldIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?RecordFieldLabelId {
        return self.record_field_label_by_text.get(idents.getText(ident));
    }

    pub fn lookupTagIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?TagLabelId {
        return self.tag_label_by_text.get(idents.getText(ident));
    }

    pub fn lookupExportIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?ExportNameId {
        return self.export_name_by_text.get(idents.getText(ident));
    }

    pub fn lookupExternalSymbolIdent(self: *const CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) ?ExternalSymbolNameId {
        return self.external_symbol_name_by_text.get(idents.getText(ident));
    }

    pub fn lookupModuleName(self: *const CanonicalNameStore, text: []const u8) ?ModuleNameId {
        return self.module_name_by_text.get(text);
    }

    pub fn lookupTypeName(self: *const CanonicalNameStore, text: []const u8) ?TypeNameId {
        return self.type_name_by_text.get(text);
    }

    pub fn lookupMethodName(self: *const CanonicalNameStore, text: []const u8) ?MethodNameId {
        return self.method_name_by_text.get(text);
    }

    pub fn internProcBase(self: *CanonicalNameStore, key: ProcBaseKey) Allocator.Error!ProcBaseKeyRef {
        self.scratch_key.clearRetainingCapacity();
        const writer = self.scratch_key.writer(self.allocator);
        try writer.print("proc:{d}:{s}:{d}:{d}:{d}|", .{
            @intFromEnum(key.module_name),
            @tagName(key.kind),
            if (key.export_name) |name| @intFromEnum(name) else std.math.maxInt(u32),
            key.ordinal,
            key.source_def_idx orelse std.math.maxInt(u32),
        });
        try appendOptionalNestedProcSiteKey(&self.scratch_key, key.nested_proc_site, self.allocator);
        try appendOptionalMonoSpecializationKey(&self.scratch_key, key.owner_mono_specialization, self.allocator);

        if (self.proc_base_by_key.get(self.scratch_key.items)) |existing| return existing;

        const id: ProcBaseKeyRef = @enumFromInt(@as(u32, @intCast(self.proc_bases.items.len)));
        const owned_key = try self.allocator.dupe(u8, self.scratch_key.items);
        errdefer self.allocator.free(owned_key);

        try self.proc_bases.append(self.allocator, key);
        try self.proc_base_by_key.put(owned_key, id);
        return id;
    }

    pub fn procBase(self: *const CanonicalNameStore, id: ProcBaseKeyRef) ProcBaseKey {
        return self.proc_bases.items[@intFromEnum(id)];
    }

    pub fn exportNameText(self: *const CanonicalNameStore, id: ExportNameId) []const u8 {
        return self.export_names.items[@intFromEnum(id)];
    }

    pub fn moduleNameText(self: *const CanonicalNameStore, id: ModuleNameId) []const u8 {
        return self.module_names.items[@intFromEnum(id)];
    }

    pub fn typeNameText(self: *const CanonicalNameStore, id: TypeNameId) []const u8 {
        return self.type_names.items[@intFromEnum(id)];
    }

    pub fn methodNameText(self: *const CanonicalNameStore, id: MethodNameId) []const u8 {
        return self.method_names.items[@intFromEnum(id)];
    }

    pub fn recordFieldLabelText(self: *const CanonicalNameStore, id: RecordFieldLabelId) []const u8 {
        return self.record_field_labels.items[@intFromEnum(id)];
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
        return self.tag_labels.items[@intFromEnum(id)];
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
        return self.external_symbol_names.items[@intFromEnum(id)];
    }

    fn freeTextList(self: *CanonicalNameStore, values: []const []const u8) void {
        for (values) |value| self.allocator.free(value);
    }
};

fn internText(
    comptime Id: type,
    allocator: Allocator,
    list: *std.ArrayList([]const u8),
    map: *std.StringHashMap(Id),
    text: []const u8,
) Allocator.Error!Id {
    if (map.get(text)) |existing| return existing;

    const id: Id = @enumFromInt(@as(u32, @intCast(list.items.len)));
    const owned = try allocator.dupe(u8, text);
    errdefer allocator.free(owned);

    try list.append(allocator, owned);
    try map.put(owned, id);
    return id;
}

fn appendOptionalNestedProcSiteKey(
    scratch: *std.ArrayList(u8),
    maybe_key: ?NestedProcSiteKey,
    allocator: Allocator,
) Allocator.Error!void {
    if (maybe_key) |key| {
        try scratch.append(allocator, 1);
        try appendProcedureTemplateRef(scratch, key.owner_template, allocator);
        try scratch.writer(allocator).print("site:{d}|", .{@intFromEnum(key.site)});
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
    try scratch.writer(allocator).print("template:{d}:{d}:", .{
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

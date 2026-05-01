//! Canonical post-check names and procedure identities.
//!
//! These ids are artifact-boundary data. They are derived from source spellings
//! during checking finalization so post-check stages do not consume module-local
//! `Ident.Idx` values or raw `Symbol` values as semantic identity.

const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;

pub const ModuleNameId = enum(u32) { _ };
pub const TypeNameId = enum(u32) { _ };
pub const MethodNameId = enum(u32) { _ };
pub const RecordFieldLabelId = enum(u32) { _ };
pub const TagLabelId = enum(u32) { _ };
pub const ExportNameId = enum(u32) { _ };
pub const ExternalSymbolNameId = enum(u32) { _ };

pub const ProcBaseKeyRef = enum(u32) { _ };
pub const CheckedProcedureTemplateId = enum(u32) { _ };
pub const NestedProcSiteId = enum(u32) { _ };
pub const PromotedCallableWrapperId = enum(u32) { _ };
pub const HostedWrapperId = enum(u32) { _ };
pub const IntrinsicWrapperId = enum(u32) { _ };
pub const EntryWrapperId = enum(u32) { _ };
pub const PromotedCallableNodeId = enum(u32) { _ };
pub const PromotedCallableBodyPlanId = enum(u32) { _ };

pub const ArtifactRef = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

pub const ProcedureValueRef = struct {
    artifact: ArtifactRef = .{},
    proc_base: ProcBaseKeyRef,
};

pub const ProcedureTemplateRef = struct {
    artifact: ArtifactRef = .{},
    proc_base: ProcBaseKeyRef,
    template: CheckedProcedureTemplateId,
};

pub const MonoSpecializationKey = struct {
    template: ProcedureTemplateRef,
    requested_mono_fn_ty: CanonicalTypeKey,
};

pub const MonoSpecializedProcRef = struct {
    proc: ProcedureValueRef,
    specialization: MonoSpecializationKey,
};

pub fn procedureValueRefEql(a: ProcedureValueRef, b: ProcedureValueRef) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and
        a.proc_base == b.proc_base;
}

pub fn procedureTemplateRefEql(a: ProcedureTemplateRef, b: ProcedureTemplateRef) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and
        a.proc_base == b.proc_base and
        a.template == b.template;
}

pub fn monoSpecializationKeyEql(a: MonoSpecializationKey, b: MonoSpecializationKey) bool {
    return std.mem.eql(u8, &a.requested_mono_fn_ty.bytes, &b.requested_mono_fn_ty.bytes) and
        procedureTemplateRefEql(a.template, b.template);
}

pub fn monoSpecializedProcRefEql(a: MonoSpecializedProcRef, b: MonoSpecializedProcRef) bool {
    return procedureValueRefEql(a.proc, b.proc) and
        monoSpecializationKeyEql(a.specialization, b.specialization);
}

pub const LiftedProcedureTemplateRef = struct {
    owner_mono_specialization: MonoSpecializationKey,
    site: NestedProcSiteId,
};

pub const SyntheticProcedureTemplateRef = struct {
    template: ProcedureTemplateRef,
};

pub const CallableProcedureTemplateRef = union(enum) {
    checked: ProcedureTemplateRef,
    lifted: LiftedProcedureTemplateRef,
    synthetic: SyntheticProcedureTemplateRef,
};

pub const ProcedureCallableRef = struct {
    template: CallableProcedureTemplateRef,
    source_fn_ty: CanonicalTypeKey,
};

pub const CanonicalTypeKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

pub const CanonicalTypeTemplateKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

pub const CanonicalTypeSchemeKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

pub const ProcBaseKind = enum {
    checked_source,
    hosted_wrapper,
    promoted_callable_wrapper,
    intrinsic_wrapper,
    entry_wrapper,
};

pub const NestedProcSiteKey = struct {
    owner_template: ProcedureTemplateRef,
    site: NestedProcSiteId,
};

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

pub const NominalTypeKey = struct {
    module_name: ModuleNameId,
    type_name: TypeNameId,
};

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

    pub fn internMethodIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!MethodNameId {
        return internText(MethodNameId, self.allocator, &self.method_names, &self.method_name_by_text, idents.getText(ident));
    }

    pub fn internRecordFieldIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!RecordFieldLabelId {
        return internText(RecordFieldLabelId, self.allocator, &self.record_field_labels, &self.record_field_label_by_text, idents.getText(ident));
    }

    pub fn internTagIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!TagLabelId {
        return internText(TagLabelId, self.allocator, &self.tag_labels, &self.tag_label_by_text, idents.getText(ident));
    }

    pub fn internExportIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!ExportNameId {
        return internText(ExportNameId, self.allocator, &self.export_names, &self.export_name_by_text, idents.getText(ident));
    }

    pub fn internExternalSymbolIdent(self: *CanonicalNameStore, idents: *const Ident.Store, ident: Ident.Idx) Allocator.Error!ExternalSymbolNameId {
        return internText(ExternalSymbolNameId, self.allocator, &self.external_symbol_names, &self.external_symbol_name_by_text, idents.getText(ident));
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

    pub fn tagLabelText(self: *const CanonicalNameStore, id: TagLabelId) []const u8 {
        return self.tag_labels.items[@intFromEnum(id)];
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
    const owner_template = ProcedureTemplateRef{
        .artifact = .{ .bytes = [_]u8{1} ** 32 },
        .proc_base = owner_base,
        .template = @enumFromInt(0),
    };

    var i64_key = CanonicalTypeKey{};
    i64_key.bytes[0] = 1;
    var str_key = CanonicalTypeKey{};
    str_key.bytes[0] = 2;

    const nested_site = NestedProcSiteKey{
        .owner_template = owner_template,
        .site = @enumFromInt(0),
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

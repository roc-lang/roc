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
    platform_required_wrapper,
    promoted_callable_wrapper,
    intrinsic_wrapper,
    entry_wrapper,
};

pub const ProcBaseKey = struct {
    module_name: ModuleNameId,
    export_name: ?ExportNameId,
    kind: ProcBaseKind,
    ordinal: u32,
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
        try self.scratch_key.writer(self.allocator).print("proc:{d}:{s}:{d}:{d}|", .{
            @intFromEnum(key.module_name),
            @tagName(key.kind),
            if (key.export_name) |name| @intFromEnum(name) else std.math.maxInt(u32),
            key.ordinal,
        });

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

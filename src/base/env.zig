const std = @import("std");
const base = @import("../base.zig");
const cols = @import("../collections.zig");
const problem = @import("../problem.zig");

pub const ModuleEnv = struct {
    symbols: base.SymbolStore,
    modules: base.ModuleStore,
    strings: cols.LargeStringInterner,
    tag_names: cols.TagNameInterner,
    tag_ids_for_slicing: cols.SafeList(cols.TagNameId),
    field_names: cols.FieldNameInterner,
    field_ids_for_slicing: cols.SafeList(cols.FieldNameId),
    problems: cols.SafeList(problem.Problem),
    // TODO: where are these used, and how do we manage them?
    // pub tuple_elem_indices: Vec<usize>,
    // pub record_fields: Vec<RecordField<()>>,

    pub fn init(allocator: std.mem.Allocator) ModuleEnv {
        return ModuleEnv{
            .symbols = base.SymbolStore.init(allocator),
            .modules = base.ModuleStore.init(allocator),
            .strings = cols.LargeStringInterner.init(allocator),
            .tag_names = cols.TagNameInterner.init(allocator),
            .tag_ids_for_slicing = cols.SafeList(cols.TagNameId).init(allocator),
            .field_names = cols.FieldNameInterner.init(allocator),
            .field_ids_for_slicing = cols.SafeList(cols.FieldNameId).init(allocator),
            .problems = cols.SafeList(problem.Problem).init(allocator),
        };
    }

    pub fn deinit(self: *ModuleEnv) void {
        self.symbols.deinit();
        self.modules.deinit();
        self.strings.deinit();
        self.tag_names.deinit();
        self.tag_ids_for_slicing.deinit();
        self.field_names.deinit();
        self.field_ids_for_slicing.deinit();
        self.problems.deinit();
    }

    pub fn addTagNameSlice(
        self: *ModuleEnv,
        name_ids: []cols.TagNameId,
    ) cols.SafeList(cols.TagNameId).Slice {
        return self.tag_ids_for_slicing.appendSlice(name_ids);
    }

    pub fn addFieldNameSlice(
        self: *ModuleEnv,
        name_ids: []cols.FieldNameId,
    ) cols.SafeList(cols.FieldNameId).Slice {
        return self.field_ids_for_slicing.appendSlice(name_ids);
    }
};

pub const GlobalEnv = struct {
    // TODO: do we need this if each module manages this?
    modules: base.ModuleStore,
    packages: base.PackageStore,

    pub fn init(allocator: std.mem.Allocator) GlobalEnv {
        return GlobalEnv{
            .modules = base.ModuleStore.init(allocator),
            .packages = base.PackageStore.init(allocator),
        };
    }

    pub fn deinit(self: *GlobalEnv) void {
        self.modules.deinit();
        self.packages.deinit();
    }
};

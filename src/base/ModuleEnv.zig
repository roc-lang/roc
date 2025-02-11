//! The common state or environment for a given module, and stores all the intered data
//! like symbols, strings, tag names, field names, and problems.
//!
//! This reduces the size the IR as it can use references to these interned values.
const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const problem = @import("../problem.zig");

const Ident = base.Ident;
const Module = base.Module;
const Problem = problem.Problem;

const ModuleEnv = @This();

// stores information about modules, includes each of
// this module's dependencies
modules: base.Module.Store,
idents: base.Ident.Store,
strings: collections.StringLiteral.Interner,
tag_names: collections.TagName.Interner,
tag_ids_for_slicing: collections.SafeList(collections.TagName.Idx),
field_names: collections.FieldName.Interner,
field_ids_for_slicing: collections.SafeList(collections.FieldName.Idx),
problems: problem.Problem.List,
// TODO: where are these used, and how do we manage them?
// pub tuple_elem_indices: Vec<usize>,
// pub record_fields: Vec<RecordField<()>>,

pub fn init(allocator: std.mem.Allocator) ModuleEnv {
    return ModuleEnv{
        .modules = base.Module.Store.init(allocator),
        .idents = base.Ident.Store.init(allocator),
        .strings = collections.StringLiteral.Interner.init(allocator),
        .tag_names = collections.TagName.Interner.init(allocator),
        .tag_ids_for_slicing = collections.SafeList(collections.TagName.Idx).init(allocator),
        .field_names = collections.FieldName.Interner.init(allocator),
        .field_ids_for_slicing = collections.SafeList(collections.FieldName.Idx).init(allocator),
        .problems = problem.Problem.List.init(allocator),
    };
}

pub fn deinit(self: *ModuleEnv) void {
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
    names: []collections.TagName.Idx,
) collections.SafeList(collections.TagName.Idx).Slice {
    return self.tag_ids_for_slicing.appendSlice(names);
}

pub fn addFieldNameSlice(
    self: *ModuleEnv,
    names: []collections.FieldName.Idx,
) collections.SafeList(collections.FieldName.Idx).Slice {
    return self.field_ids_for_slicing.appendSlice(names);
}

pub fn addExposedIdentForModule(self: *ModuleEnv, ident: Ident.Idx, module: Module.Idx) void {
    self.modules.addExposedIdent(module, ident, &self.problems);
    self.idents.setExposingModule(ident, module);
}

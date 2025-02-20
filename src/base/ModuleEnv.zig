//! The common state or environment for a module for things that live for the duration of the compilation.
//!
//! Stores all interned data like symbols, strings, tag names, field names, and problems.
//!
//! This reduces the size of this module's IRs as they can store references to this
//! interned (and deduplicated) data instead of storing the values themselves.
const std = @import("std");
const collections = @import("../collections.zig");
const problem = @import("../problem.zig");

const Ident = @import("Ident.zig");
const TagName = @import("TagName.zig");
const FieldName = @import("FieldName.zig");
const ModuleImport = @import("ModuleImport.zig");
const StringLiteral = @import("StringLiteral.zig");
const Type = @import("../types/type.zig").Type;

const Problem = problem.Problem;

const Self = @This();

idents: Ident.Store,
ident_ids_for_slicing: collections.SafeList(Ident.Idx),
module_imports: ModuleImport.Store,
tag_names: TagName.Store,
tag_name_ids_for_slicing: collections.SafeList(TagName.Idx),
field_names: FieldName.Store,
strings: StringLiteral.Store,
problems: std.ArrayList(Problem),
type_store: Type.Store,

pub fn init(allocator: std.mem.Allocator) Self {
    var ident_store = Ident.Store.init(allocator);

    return Self{
        .idents = ident_store,
        .ident_ids_for_slicing = collections.SafeList(Ident.Idx).init(allocator),
        .module_imports = ModuleImport.Store.init(allocator, &ident_store),
        .tag_names = TagName.Store.init(allocator),
        .tag_name_ids_for_slicing = collections.SafeList(TagName.Idx).init(allocator),
        .field_names = FieldName.Store.init(allocator),
        .strings = StringLiteral.Store.init(allocator),
        .problems = std.ArrayList(Problem).init(allocator),
        .type_store = Type.Store.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.idents.deinit();
    self.ident_ids_for_slicing.deinit();
    self.module_imports.deinit();
    self.tag_names.deinit();
    self.tag_name_ids_for_slicing.deinit();
    self.field_names.deinit();
    self.strings.deinit();
    self.problems.deinit();
    self.type_store.deinit();
}

pub fn addExposedIdentForModule(self: *Self, ident: Ident.Idx, module_import: ModuleImport.Idx) void {
    self.module_imports.addExposedIdent(module_import, ident, &self.problems);
    self.idents.setExposingModule(ident, module_import);
}

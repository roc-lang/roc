//! The common state or environment for a module for things that live for the duration of the compilation.
//!
//! Stores all interned data like symbols, strings, tag names, field names, and problems.
//!
//! This reduces the size of this module's IRs as they can store references to this
//! interned (and deduplicated) data instead of storing the values themselves.
const std = @import("std");
const collections = @import("../collections.zig");
const problem = @import("../problem.zig");

const Ident = @import("./Ident.zig");
const Module = @import("./Module.zig");
const StringLiteral = @import("./StringLiteral.zig");
const Type = @import("../types/type.zig").Type;

const Problem = problem.Problem;

const Self = @This();

idents: Ident.Store,
ident_ids_for_slicing: collections.SafeList(Ident.Idx),
modules: Module.Store,
strings: StringLiteral.Store,
problems: std.ArrayList(Problem),
type_store: Type.Store,

pub fn init(gpa: std.mem.Allocator) Self {
    var ident_store = Ident.Store.init(gpa);

    return Self{
        .idents = ident_store,
        .ident_ids_for_slicing = collections.SafeList(Ident.Idx).init(gpa),
        .modules = Module.Store.init(gpa, &ident_store),
        .strings = StringLiteral.Store.init(gpa),
        .problems = std.ArrayList(Problem).init(gpa),
        .type_store = Type.Store.init(gpa),
    };
}

pub fn deinit(self: *Self) void {
    self.idents.deinit();
    self.ident_ids_for_slicing.deinit();
    self.modules.deinit();
    self.strings.deinit();
    self.problems.deinit();
    self.type_store.deinit();
}

pub fn addExposedIdentForModule(self: *Self, ident: Ident.Idx, module: Module.Idx) void {
    self.modules.addExposedIdent(module, ident, &self.problems);
    self.idents.setExposingModule(ident, module);
}

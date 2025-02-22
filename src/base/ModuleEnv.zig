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
arena: *std.heap.ArenaAllocator,

pub fn init(arena: *std.heap.ArenaAllocator) Self {
    // var arena = std.heap.ArenaAllocator.init(allocator);
    var ident_store = Ident.Store.init(arena);

    return Self{
        .idents = ident_store,
        .ident_ids_for_slicing = collections.SafeList(Ident.Idx).init(arena.allocator()),
        .modules = Module.Store.init(arena, &ident_store),
        .strings = StringLiteral.Store.init(arena.allocator()),
        .problems = std.ArrayList(Problem).init(arena.allocator()),
        .type_store = Type.Store.init(arena.allocator()),
        .arena = arena,
    };
}

pub fn addExposedIdentForModule(self: *Self, ident: Ident.Idx, module: Module.Idx) void {
    self.modules.addExposedIdent(module, ident, &self.problems);
    self.idents.setExposingModule(ident, module);
}

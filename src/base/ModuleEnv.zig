//! The common state for a module: any data useful over the full lifetime of its compilation.
//!
//! Stores all interned data like idents, strings, and problems.
//!
//! This reduces the size of this module's IRs as they can store references to this
//! interned (and deduplicated) data instead of storing the values themselves.

const std = @import("std");
const type_mod = @import("../types.zig");
const problem = @import("../problem.zig");
const collections = @import("../collections.zig");
const Ident = @import("Ident.zig");
const StringLiteral = @import("StringLiteral.zig");
const Target = @import("./target.zig").Target;

const Type = type_mod.Type;
const Problem = problem.Problem;

const Self = @This();

gpa: std.mem.Allocator,
idents: Ident.Store = .{},
ident_ids_for_slicing: collections.SafeList(Ident.Idx),
strings: StringLiteral.Store,
problems: Problem.List,
target: Target,

/// Initialize the module environment.
pub fn init(gpa: std.mem.Allocator) Self {
    // TODO: maybe wire in smarter default based on the initial input text size.
    return Self{
        .gpa = gpa,
        .idents = Ident.Store.initCapacity(gpa, 1024),
        .ident_ids_for_slicing = collections.SafeList(Ident.Idx).initCapacity(gpa, 256),
        .strings = StringLiteral.Store.initCapacityBytes(gpa, 4096),
        .problems = Problem.List.initCapacity(gpa, 64),
        .target = Target.native,
    };
}

/// Deinitialize the module environment.
pub fn deinit(self: *Self) void {
    self.idents.deinit(self.gpa);
    self.ident_ids_for_slicing.deinit(self.gpa);
    self.strings.deinit(self.gpa);
    self.problems.deinit(self.gpa);
}

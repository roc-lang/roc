//! The common state or environment for a module.
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
const TagName = @import("./TagName.zig");
const FieldName = @import("./FieldName.zig");
const StringLiteral = @import("./StringLiteral.zig");

const Problem = problem.Problem;

const Self = @This();

/// This siloed module's view of other modules based only on import statements.
idents: Ident.Store,
ident_ids_for_slicing: collections.SafeList(Ident.Idx),
modules: Module.Store,
tag_names: TagName.Store,
tag_name_ids_for_slicing: collections.SafeList(TagName.Idx),
field_names: FieldName.Store,
strings: StringLiteral.Store,
problems: std.ArrayList(Problem),
next_type_id: TypeId,

pub fn init(allocator: std.mem.Allocator) Self {
    var ident_store = Ident.Store.init(allocator);

    return Self{
        .idents = ident_store,
        .ident_ids_for_slicing = collections.SafeList(Ident.Idx).init(allocator),
        .modules = Module.Store.init(allocator, &ident_store),
        .tag_names = TagName.Store.init(allocator),
        .tag_name_ids_for_slicing = collections.SafeList(TagName.Idx).init(allocator),
        .field_names = FieldName.Store.init(allocator),
        .strings = StringLiteral.Store.init(allocator),
        .problems = std.ArrayList(Problem).init(allocator),
        .next_type_id = @enumFromInt(0),
    };
}

pub fn deinit(self: *Self) void {
    self.idents.deinit();
    self.ident_ids_for_slicing.deinit();
    self.modules.deinit();
    self.tag_names.deinit();
    self.tag_name_ids_for_slicing.deinit();
    self.field_names.deinit();
    self.strings.deinit();
    self.problems.deinit();
}

pub fn addExposedIdentForModule(self: *Self, ident: Ident.Idx, module: Module.Idx) void {
    self.modules.addExposedIdent(module, ident, &self.problems);
    self.idents.setExposingModule(ident, module);
}

/// Generate a new type ID.
pub fn newTypeId(self: *Self) TypeId {
    const id = self.next_type_id;
    self.next_type_id = self.next_type_id.next();
    return id;
}

pub const TypeId = enum(u32) {
    string = 0,
    // Add builtins here
    first_user_space = 1,

    fn index(self: TypeId) u32 {
        return @intFromEnum(self);
    }

    fn next(self: TypeId) TypeId {
        return @enumFromInt(self.index() + 1);
    }

    pub fn format(self: TypeId, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.writeAll("TypeId(");
        try std.fmt.format(writer, "{any}", .{self.index()});
        try writer.writeAll(")");
    }
};

//! Store-local identities for native code relocation targets.
//!
//! Producers intern linker declarations once and pass compact IDs to emitters.
//! Names are borrowed from the producer and live until its code is consumed.

const std = @import("std");

/// Index in one code generator's symbol-name column.
pub const Id = enum(u32) { _ };

/// Whether a symbol's name means the same thing in every program, which
/// decides whether code referring to it can be cached and linked into
/// another program. See design.md, "Object Symbol Names".
pub const Scope = enum {
    /// Named by content or by a fixed interface: procedures, refcount
    /// helpers, builtins, host and hosted functions, compile-time hooks, and
    /// data named by content.
    shared,
    /// Meaningful only in the program whose code declared it: the value in a
    /// static-data slot and the evaluator's cell holding its address, and the
    /// Boxy runtime, whose calls index this program's descriptor sidecar.
    program,
};

/// Append-only symbol declarations, shared by code generation and object emission.
pub const Table = struct {
    names: std.ArrayList([]const u8) = .empty,
    /// The scope code generation declared for each symbol it refers to.
    /// Null for a symbol only object emission named.
    scopes: std.ArrayList(?Scope) = .empty,
    required_definitions: std.ArrayList(Id) = .empty,
    indices: std.StringHashMapUnmanaged(Id) = .empty,

    pub fn deinit(self: *Table, allocator: std.mem.Allocator) void {
        self.names.deinit(allocator);
        self.scopes.deinit(allocator);
        self.required_definitions.deinit(allocator);
        self.indices.deinit(allocator);
    }

    /// Declare a symbol generated code refers to, with its scope. One name
    /// has one scope.
    pub fn intern(self: *Table, allocator: std.mem.Allocator, name: []const u8, scope_: Scope) std.mem.Allocator.Error!Id {
        const id = try self.internEmitted(allocator, name);
        const declared = &self.scopes.items[@intFromEnum(id)];
        if (declared.*) |existing| {
            if (existing != scope_) std.debug.panic("symbol {s} was declared with two scopes", .{name});
        } else {
            declared.* = scope_;
        }
        return id;
    }

    /// Declare an internal target once at its producer's identity-cache insertion.
    pub fn internInternal(self: *Table, allocator: std.mem.Allocator, name: []const u8, scope_: Scope) std.mem.Allocator.Error!Id {
        const id = try self.intern(allocator, name, scope_);
        try self.required_definitions.append(allocator, id);
        return id;
    }

    /// Name a symbol object emission defines or lists. It declares no scope:
    /// only a reference from generated code does.
    pub fn internEmitted(self: *Table, allocator: std.mem.Allocator, name: []const u8) std.mem.Allocator.Error!Id {
        const entry = try self.indices.getOrPut(allocator, name);
        if (!entry.found_existing) {
            errdefer _ = self.indices.remove(name);
            const id: Id = @enumFromInt(self.names.items.len);
            try self.names.ensureUnusedCapacity(allocator, 1);
            try self.scopes.ensureUnusedCapacity(allocator, 1);
            self.names.appendAssumeCapacity(name);
            self.scopes.appendAssumeCapacity(null);
            entry.value_ptr.* = id;
        }
        return entry.value_ptr.*;
    }

    /// The scope generated code declared for `id`.
    pub fn scope(self: *const Table, id: Id) Scope {
        return self.scopes.items[@intFromEnum(id)] orelse
            std.debug.panic("generated code refers to {s} without declaring its scope", .{self.names.items[@intFromEnum(id)]});
    }

    pub fn clearRetainingCapacity(self: *Table) void {
        self.names.clearRetainingCapacity();
        self.scopes.clearRetainingCapacity();
        self.required_definitions.clearRetainingCapacity();
        self.indices.clearRetainingCapacity();
    }
};

test "symbol declarations retain IDs across growth and reset" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, exerciseTable, .{});
}

fn exerciseTable(allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
    var table: Table = .{};
    defer table.deinit(allocator);
    const first = try table.internInternal(allocator, "first", .program);
    const other = try table.intern(allocator, "other", .shared);
    var same_bytes = [_]u8{ 'f', 'i', 'r', 's', 't' };
    std.debug.assert(try table.intern(allocator, &same_bytes, .program) == first);
    std.debug.assert(table.scope(first) == .program and table.scope(other) == .shared);
    std.debug.assert(first != other and table.names.items.len == 2);
    var names: [128][16]u8 = undefined;
    for (&names, 0..) |*buffer, index| {
        const name = std.fmt.bufPrint(buffer, "symbol_{d}", .{index}) catch unreachable;
        _ = try table.internEmitted(allocator, name);
    }
    std.debug.assert(try table.internEmitted(allocator, "first") == first);
    table.clearRetainingCapacity();
    std.debug.assert(table.names.items.len == 0 and table.required_definitions.items.len == 0);
    std.debug.assert(@intFromEnum(try table.internEmitted(allocator, "after_reset")) == 0);
}

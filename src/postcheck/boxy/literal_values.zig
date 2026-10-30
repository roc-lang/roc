//! The values compile-time evaluation converted a `.boxy` program's
//! instance-dependent literals to (design.md "Literal Tables"): each literal
//! root of the program's compile-time specialization, keyed by its literal
//! and the concrete type it converted the literal at.

const std = @import("std");
const check = @import("check");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const ConstStore = check.ConstStore;

/// Every converted literal root's outcome, its value and type in `store`,
/// whose type names `names` interns.
pub const LiteralValues = struct {
    allocator: Allocator,
    store: ConstStore.ConstStore,
    names: check.CheckedNames.CanonicalNameStore,
    entries: std.ArrayList(Entry) = .empty,

    pub const Entry = struct {
        /// The module whose checked expression `expr` is the literal.
        module: checked.ModuleId,
        expr: checked.CheckedExprId,
        /// The concrete type the literal was converted at.
        ty: ConstStore.ConstTypeId,
        outcome: Outcome,
    };

    pub const Outcome = union(enum) {
        value: checked.ConstNodeId,
        /// The conversion failed with this message (owned).
        failed: []const u8,
    };

    pub fn init(allocator: Allocator) LiteralValues {
        return .{
            .allocator = allocator,
            .store = ConstStore.ConstStore.init(allocator),
            .names = check.CheckedNames.CanonicalNameStore.init(allocator),
        };
    }

    pub fn deinit(self: *LiteralValues) void {
        for (self.entries.items) |entry| switch (entry.outcome) {
            .failed => |message| self.allocator.free(message),
            .value => {},
        };
        self.entries.deinit(self.allocator);
        self.names.deinit();
        self.store.deinit();
    }
};

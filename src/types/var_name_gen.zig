const std = @import("std");
const base = @import("../base.zig");
const types = @import("../types/types.zig");
const store = @import("../types/store.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = base.ModuleEnv;

/// Generates unique type variable names that don't conflict with existing names
pub const TypeVarNameGenerator = struct {
    allocator: Allocator,
    taken_names: std.StringHashMap(void),
    allocated_names: std.ArrayList([]const u8),

    const Self = @This();

    pub fn init(allocator: Allocator) !Self {
        return .{
            .allocator = allocator,
            .taken_names = std.StringHashMap(void).init(allocator),
            .allocated_names = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // Free all allocated names
        for (self.allocated_names.items) |name| {
            self.allocator.free(name);
        }
        self.allocated_names.deinit();
        self.taken_names.deinit();
    }

    /// Mark a name as taken
    pub fn markNameTaken(self: *Self, name: []const u8) !void {
        try self.taken_names.put(name, {});
    }

    /// Generate the next available type variable name
    /// Generate a new unique type variable name
    pub fn generateName(self: *Self) ![]const u8 {
        // Start with single letters a-z
        var buf: [16]u8 = undefined;

        // Try single letters first
        var letter: u8 = 'a';
        while (letter <= 'z') : (letter += 1) {
            buf[0] = letter;
            const name = buf[0..1];
            if (!self.taken_names.contains(name)) {
                const allocated_name = try self.allocator.dupe(u8, name);
                try self.taken_names.put(allocated_name, {});
                try self.allocated_names.append(allocated_name);
                return allocated_name;
            }
        }

        // If all single letters are taken, start with suffixes
        var suffix: u32 = 2;
        while (suffix <= 1000) : (suffix += 1) {
            letter = 'a';
            while (letter <= 'z') : (letter += 1) {
                const name = std.fmt.bufPrint(&buf, "{c}{d}", .{ letter, suffix }) catch unreachable; // 16 byte buffer is more than enough
                if (!self.taken_names.contains(name)) {
                    const allocated_name = try self.allocator.dupe(u8, name);
                    try self.taken_names.put(allocated_name, {});
                    try self.allocated_names.append(allocated_name);
                    return allocated_name;
                }
            }
        }

        // This should never happen in practice - we have 26 * 999 = 25,974 possible names
        unreachable;
    }
};

// Tests are in test/test_var_name_gen_simple.zig

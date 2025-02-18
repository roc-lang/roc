const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const Ident = @import("../base/Ident.zig");

/// Manages types
pub const TypeStore = struct {
    next: u32,

    /// Create a new VarStore initialized to the first user space variable.
    pub fn init() TypeStore {
        return TypeStore.initWithNext(Variable.FIRST_USER_SPACE_VAR);
    }

    /// Create a new VarStore initialized with a specific next variable ID.
    /// The next ID must be >= the first user space variable.
    pub fn initWithNext(next: Variable) TypeStore {
        std.debug.assert(next.val >= Variable.FIRST_USER_SPACE_VAR.val);
        return TypeStore{ .next = next.val };
    }

    /// Returns the next variable ID that would be generated, without actually generating it.
    /// Useful for looking ahead at variable allocation without affecting the counter.
    pub fn peek(self: *TypeStore) u32 {
        return self.next;
    }

    /// Generates and returns a new unique variable ID.
    /// Guarantees each call returns a different ID by incrementing the internal counter.
    /// Used to create fresh type variables during type inference.
    pub fn fresh(self: *TypeStore) Variable {
        // Increment the counter and return the value it had before being incremented
        const answer = self.next;
        self.next += 1;
        return Variable{ .val = answer };
    }
};

/// Companion type to represent type variables
pub const Variable = struct {
    val: u32,

    /// First variable ID available for user-defined types
    pub const FIRST_USER_SPACE_VAR = Variable{ .val = NUM_RESERVED_VARS };

    /// Number of reserved variable IDs for built-in types
    pub const NUM_RESERVED_VARS: u32 = 128; // Example value, should match Roc's definition

    pub fn index(self: Variable) u32 {
        return self.val;
    }

    pub fn format(self: Variable, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.writeAll("Var(");
        try std.fmt.format(writer, "{any}", .{self.val});
        try writer.writeAll(")");
    }
};

pub const Content = union(enum) {
    /// A type variable which the user did not name in an annotation,
    ///
    /// When we auto-generate a type var name, e.g. the "a" in (a -> a), we
    /// change the Option in here from None to Some.
    FlexVar: ?[]usize,
    // FlexVar(Option<SubsIndex<Lowercase>>),

    /// Name given in a user-written annotation
    RigidVar: []usize,

    Structure: FlatType,

    Error,

    /// The fx type variable for a given function
    Pure,

    /// The fx type variable for a given function
    Effectful,

    pub fn format(
        self: Content,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .FlexVar => |maybe_indices| {
                try writer.writeAll("FlexVar(");
                if (maybe_indices) |indices| {
                    try std.fmt.format(writer, "{any}", .{indices});
                } else {
                    try writer.writeAll("null");
                }
                try writer.writeAll(")");
            },
            .RigidVar => |indices| {
                try writer.writeAll("RigidVar(");
                try std.fmt.format(writer, "{any}", .{indices});
                try writer.writeAll(")");
            },
            .Structure => |flat_type| {
                try writer.writeAll("Structure(");
                switch (flat_type) {
                    .Apply => |apply| {
                        try writer.writeAll("Apply{name: ");
                        try std.fmt.format(writer, "{}, arguments: {any}", .{ apply.name, apply.arguments });
                        try writer.writeAll("}");
                    },
                    .Func => |func| {
                        try writer.writeAll("Func{arguments: ");
                        try std.fmt.format(writer, "{any}, lambda_set: {}, result: {}, fx: {}", .{ func.arguments, func.lambda_set, func.result, func.fx });
                        try writer.writeAll("}");
                    },
                    .EmptyRecord => try writer.writeAll("EmptyRecord"),
                    .EmptyTagUnion => try writer.writeAll("EmptyTagUnion"),
                }
                try writer.writeAll(")");
            },
            .Error => try writer.writeAll("Error"),
            .Pure => try writer.writeAll("Pure"),
            .Effectful => try writer.writeAll("Effectful"),
        }
    }
};

pub const FlatType = union(enum) {
    Apply: Apply,
    Func: Func,
    EmptyRecord,
    EmptyTagUnion,

    pub const Func = struct {
        arguments: []Variable,
        lambda_set: Variable,
        result: Variable,
        fx: Variable,
    };

    pub const Apply = struct {
        name: Variable,
        arguments: []Variable,
    };
};

/// Descriptor holds the complete type information for a variable in the type system,
/// including its content, rank, mark, and copy status.
pub const Descriptor = struct {
    /// The actual type content/structure
    content: Content,

    /// The rank indicating the scope level where this type was introduced
    rank: Rank,

    /// Marks used during various type system operations like occurs checks
    mark: Mark,

    /// Optional reference to a copied version of this variable
    copy: ?Variable,

    const Self = @This();

    /// Creates a default Descriptor with an unnamed flex variable
    pub fn default() Self {
        return Self{
            .content = Content{ .FlexVar = null },
            .rank = Rank.GENERALIZED,
            .mark = Mark.NONE,
            .copy = null,
        };
    }

    /// Create a new Descriptor from just Content.
    /// Initializes with default values for rank, mark, and copy.
    pub fn fromContent(content: Content) Self {
        return .{
            .content = content,
            .rank = Rank.GENERALIZED,
            .mark = Mark.NONE,
            .copy = null,
        };
    }

    /// Create a new Descriptor with all fields specified
    pub fn init(
        content: Content,
        rank: Rank,
        mark: Mark,
        copy: ?Variable,
    ) Self {
        return .{
            .content = content,
            .rank = rank,
            .mark = mark,
            .copy = copy,
        };
    }

    /// Format the Descriptor for debug output
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.writeAll("{");
        try self.content.format("", .{}, writer);
        try std.fmt.format(writer, ", rank: {}, mark: {}, copy: {?}", .{
            self.rank,
            self.mark,
            self.copy,
        });
        try writer.writeAll("}");
    }
};

// Default implementation returns an unnamed flex variable descriptor
pub const defaultDescriptor = Descriptor{
    .content = Content{ .FlexVar = null },
    .rank = Rank.GENERALIZED,
    .mark = Mark.NONE,
    .copy = null,
};

pub const Mark = struct {
    /// Represents different states
    value: i32,

    /// Represents an unmarked state
    pub const NONE: Mark = .{ .value = 3 };

    /// Indicates a variable has been visited during an occurs check
    pub const VISITED_IN_OCCURS_CHECK: Mark = .{ .value = 2 };

    /// Indicates an occurrence was found during unification
    pub const OCCURS: Mark = .{ .value = 1 };

    /// Used when collecting variable names during pretty printing
    pub const GET_VAR_NAMES: Mark = .{ .value = 0 };

    /// Returns a new Mark with a value incremented by 1
    pub inline fn next(self: Mark) Mark {
        return .{ .value = self.value + 1 };
    }

    /// Implements custom formatting for Mark values
    /// Displays predefined marks as their semantic names and others as "Mark(value)"
    pub fn format(
        self: Mark,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self.value) {
            NONE.value => try writer.writeAll("NONE"),
            VISITED_IN_OCCURS_CHECK.value => try writer.writeAll("VISITED_IN_OCCURS_CHECK"),
            OCCURS.value => try writer.writeAll("OCCURS"),
            GET_VAR_NAMES.value => try writer.writeAll("GET_VAR_NAMES"),
            else => try std.fmt.format(writer, "Mark({})", .{self.value}),
        }
    }

    /// Compares two Mark values for equality
    pub fn eql(self: Mark, other: Mark) bool {
        return self.value == other.value;
    }

    /// Generates a hash value for use in hash maps
    pub fn hash(self: Mark) u64 {
        return @intCast(self.value);
    }
};

/// Rank represents a hierarchical level in type checking and generalization.
/// It's used to track the scope level of type variables and ensure proper
/// type generalization. Each rank indicates a different scope or phase in
/// type checking.
pub const Rank = struct {
    value: u32,

    /// Special rank value used to mark variables that have been generalized.
    /// This rank is reserved specifically for variables that have completed
    /// the generalization process and is distinct from all other ranks used
    /// in type checking.
    pub const GENERALIZED: Rank = .{ .value = 0 };

    /// Checks if this rank represents a generalized variable.
    pub fn isGeneralized(self: Rank) bool {
        return std.meta.eql(self, GENERALIZED);
    }

    /// Creates a new Rank representing the top level scope.
    /// This is the starting rank for type checking and represents
    /// the outermost scope level before any imports or nested scopes.
    pub fn toplevel() Rank {
        return .{ .value = 1 };
    }

    /// Creates a new Rank for handling imports (rank 2).
    ///
    /// Type checking starts at rank 1 (toplevel). When there are rigid/flex variables
    /// introduced by a constraint, these must be generalized relative to toplevel,
    /// and hence are introduced at rank 2.
    ///
    /// Even if there are no rigid imports, introducing at rank 2 is correct
    /// (if slightly inefficient) because there are no rigids anyway so
    /// generalization is trivial.
    pub fn import() Rank {
        return .{ .value = 2 };
    }

    /// Returns a new Rank that is one level higher than the current rank.
    /// Used when entering a new scope level during type checking or when
    /// new variables need to be introduced at a deeper level.
    pub fn next(self: Rank) Rank {
        return .{ .value = self.value + 1 };
    }

    /// Converts the rank to a usize value.
    /// Useful for array indexing and other operations requiring usize.
    pub fn intoUsize(self: Rank) usize {
        return @as(usize, self.value);
    }

    /// Implements formatting for Rank.
    /// Displays the rank as a decimal number.
    pub fn format(
        self: Rank,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try std.fmt.format(writer, "{d}", .{self.value});
    }

    /// Creates a new Rank from a usize value.
    pub fn fromUsize(value: usize) Rank {
        return .{ .value = @as(u32, @intCast(value)) };
    }
};

pub const UnificationError = error{
    OccursCheck,
    TypeMismatch,
    OutOfMemory,
};

/// UnificationTable manages unification operations between types.
/// It provides functionality for building, querying, and manipulating a table
/// of type variables and their descriptors.
pub const UnificationTable = struct {

    // Fields to track entries
    entries: []Entry,
    allocator: Allocator,
    len: usize,

    debug_capture: ?std.fs.File.Writer = null,

    // State tracking
    const Entry = struct {
        descriptor: Descriptor,
        parent: ?Variable, // The parent entry in the union-find tree
    };

    /// Creates a new UnificationTable with pre-allocated space for the given capacity.
    /// The table manages type variable relationships and their descriptors during unification.
    pub fn init(allocator: Allocator, capacity: usize) !UnificationTable {
        const entries = try allocator.alloc(Entry, capacity);
        return UnificationTable{
            .entries = entries,
            .allocator = allocator,
            .len = 0,
        };
    }

    /// Free allocated memory
    pub fn deinit(self: *UnificationTable) void {
        self.allocator.free(self.entries);
    }

    /// Ensure capacity for the table
    pub fn ensureCapacity(self: *UnificationTable, needed: usize) !void {
        if (self.len + needed > self.entries.len) {
            const new_cap = std.math.max(self.entries.len * 2, self.len + needed);
            self.entries = try self.allocator.realloc(self.entries, new_cap);
        }
    }

    /// Adds a new type variable to the table with specified attributes.
    /// Returns a Variable representing the new entry.
    /// Used when introducing new type variables into the type system.
    pub fn push(self: *UnificationTable, content: Content, rank: Rank, mark: Mark, copy: ?Variable) Variable {
        const descriptor = Descriptor{
            .content = content,
            .rank = rank,
            .mark = mark,
            .copy = copy,
        };

        const index = self.len;
        self.entries[index] = Entry{
            .descriptor = descriptor,
            .parent = null,
        };
        self.len += 1;

        self.debugLog("New type variable: {s}\n", .{descriptor});

        return Variable{ .val = @intCast(index) };
    }

    /// Retrieves the type information for a variable without path compression.
    /// Returns the descriptor from the root of the variable's equivalence class.
    /// Used to query type information without modifying the union-find structure.
    pub fn getDescriptor(self: *const UnificationTable, variable: Variable) Descriptor {
        const root = self.rootKeyWithoutCompacting(variable);
        return self.entries[root.val].descriptor;
    }

    /// Finds the representative (root) variable for an equivalence class.
    /// Performs path compression to optimize future lookups.
    /// Core operation for maintaining the union-find data structure.
    pub fn rootKey(self: *UnificationTable, variable: Variable) Variable {
        const root = self.rootKeyWithoutCompacting(variable);

        self.debugLog("Root for variable {}: {}\n", .{ variable.val, root.val });

        // Perform path compression
        var current = variable;
        while (self.entries[current.val].parent) |parent| {
            const next = parent;
            self.entries[current.val].parent = root;
            current = next;
        }

        return root;
    }

    /// Like rootKey() but doesn't perform path compression.
    /// Useful when you need to inspect the structure without modifying it.
    /// Important for debugging and maintaining parent-child relationships.
    pub fn rootKeyWithoutCompacting(self: *const UnificationTable, variable: Variable) Variable {
        // self.debugLog("\n--- rootKeyWithoutCompacting for variable {} ---\n", .{variable.val});
        var key = variable;
        var depth: usize = 0;

        while (self.entries[key.val].parent) |parent| {
            depth += 1;
            // self.debugLog("  Level {}: {} -> {}\n", .{ depth, key.val, parent.val });
            key = parent;
        }

        // self.debugLog("  Found root: {} (depth: {})\n", .{ key.val, depth });
        // self.debugLog("--- rootKeyWithoutCompacting finished ---\n", .{});
        return key;
    }

    /// High-level unification operation that merges two type variables.
    /// Makes the second variable point to the first, sharing type information.
    /// Main entry point for type unification during type checking.
    pub fn unify(self: *UnificationTable, to: Variable, from: Variable) UnificationError!void {
        self.debugLog("Unifying {} <- {}\n", .{ to.val, from.val });

        // Add occurs check to avoid infinite types
        if (try self.occurs(to, from)) {
            return error.OccursCheck;
        }

        // Get the current descriptor to replace
        const desc = self.getDescriptor(to);

        // Perform the unification
        if (from.val != to.val) {
            var current = from;
            while (self.entries[current.val].parent) |p| {
                current = p;
            }

            // Important: Set from's parent to point to 'to' directly
            self.entries[from.val].parent = to; // Changed this line
            self.entries[to.val].descriptor = desc;

            current = from;
            while (self.entries[current.val].parent) |p| {
                current = p;
            }
        } else {
            self.debugLog("Self-unification - no changes needed\n", .{});
        }
    }

    /// Checks if a variable occurs within another type structure.
    /// Returns true if there would be a circular reference.
    /// This is a crucial check during unification to prevent infinite types.
    pub fn occurs(self: *UnificationTable, variable: Variable, within: Variable) !bool {
        // Create a visited set to track variables we've seen
        var visited = std.AutoHashMap(u32, void).init(self.allocator);
        defer visited.deinit();

        // Get the root variables since we care about their actual representatives
        const var_root = self.rootKeyWithoutCompacting(variable);
        var current = within;

        while (true) {
            // Get the root of the current variable we're checking
            current = self.rootKeyWithoutCompacting(current);

            // If we've found the variable we're looking for, we have a circular reference
            if (current.val == var_root.val) {
                return true;
            }

            // If we've already visited this variable, no need to check again
            if (visited.contains(current.val)) {
                return false;
            }

            // Mark this variable as visited
            try visited.put(current.val, {});

            // Get the descriptor for the current variable
            const desc = self.entries[current.val].descriptor;

            // Check the content of the current variable
            switch (desc.content) {
                // Base cases - no occurrence possible
                .FlexVar, .Pure, .Effectful, .Error => return false,

                // For rigid variables, we need to check if it matches our target
                .RigidVar => |_| return false,

                // For structures, we need to recursively check all contained types
                .Structure => |flat_type| {
                    switch (flat_type) {
                        .Apply => |apply| {
                            // Check each argument type
                            for (apply.arguments) |arg| {
                                if (try self.occurs(variable, arg)) {
                                    return true;
                                }
                            }
                        },
                        .Func => |func| {
                            // Check argument types
                            for (func.arguments) |arg| {
                                if (try self.occurs(variable, arg)) {
                                    return true;
                                }
                            }

                            // Check lambda set
                            if (try self.occurs(variable, func.lambda_set)) {
                                return true;
                            }

                            // Check result type
                            if (try self.occurs(variable, func.result)) {
                                return true;
                            }

                            // Check effect type
                            if (try self.occurs(variable, func.fx)) {
                                return true;
                            }
                        },
                        // Empty structures have no types to check
                        .EmptyRecord, .EmptyTagUnion => return false,
                    }
                    return false;
                },
            }
        }
    }

    /// Updates the type information associated with a variable.
    /// Used when refining type information during type inference.
    pub fn setDescriptor(self: *UnificationTable, variable: Variable, descriptor: Descriptor) void {
        self.entries[variable.val].descriptor = descriptor;
    }

    /// Returns the rank (scope level) of a variable's equivalence class.
    /// Used for proper handling of type generalization and scope.
    pub fn getRank(self: *const UnificationTable, variable: Variable) Rank {
        const root = self.rootKeyWithoutCompacting(variable);
        return self.entries[root.val].descriptor.rank;
    }

    /// Gets the mark value associated with a variable's equivalence class.
    /// Used for various type system operations like occurs checks.
    pub fn getMark(self: *const UnificationTable, variable: Variable) Mark {
        const root = self.rootKeyWithoutCompacting(variable);
        return self.entries[root.val].descriptor.mark;
    }

    /// Updates the rank of a variable's equivalence class.
    /// Important for maintaining proper scope information during type checking.
    pub fn setRank(self: *UnificationTable, variable: Variable, rank: Rank) void {
        const root = self.rootKey(variable);
        self.entries[root.val].descriptor.rank = rank;
    }

    /// Sets the mark value for a variable's equivalence class.
    /// Used to track state during type system operations.
    pub fn setMark(self: *UnificationTable, variable: Variable, mark: Mark) void {
        const root = self.rootKey(variable);
        self.entries[root.val].descriptor.mark = mark;
    }

    /// Retrieves any copy reference associated with a variable.
    /// Part of the mechanism for handling type variable copies during generalization.
    pub fn getCopy(self: *const UnificationTable, variable: Variable) ?Variable {
        const root = self.rootKeyWithoutCompacting(variable);
        return self.entries[root.val].descriptor.copy;
    }

    /// Sets a copy reference for a variable.
    /// Used when creating copies of type variables during generalization.
    pub fn setCopy(self: *UnificationTable, variable: Variable, copy: ?Variable) void {
        const root = self.rootKey(variable);
        self.entries[root.val].descriptor.copy = copy;
    }

    /// Checks if two variables belong to the same equivalence class.
    /// Returns true if the variables have been unified.
    pub fn unioned(self: *UnificationTable, var1: Variable, var2: Variable) bool {
        self.debugLog("Checking union for {} and {}\n", .{ var1.val, var2.val });
        const is_unioned = self.rootKey(var1).val == self.rootKey(var2).val;
        self.debugLog("Union result: {}\n", .{is_unioned});
        return is_unioned;
    }

    /// Checks if a variable points to another (has a parent).
    /// Used to determine if a variable has been unified with another.
    pub fn isRedirect(self: *const UnificationTable, variable: Variable) bool {
        const has_parent = self.entries[variable.val].parent != null;
        self.debugLog("Redirect check for {}: {}\n", .{ variable.val, has_parent });
        if (has_parent) {
            self.debugLog("  Parent is: {}\n", .{self.entries[variable.val].parent.?.val});
        }
        return has_parent;
    }

    /// Returns the number of variables currently in the table.
    /// Used to track table size and manage capacity.
    pub fn len(self: *const UnificationTable) usize {
        return self.len;
    }

    /// Checks if the table contains any variables.
    /// Utility function for validation and initialization checks.
    pub fn isEmpty(self: *const UnificationTable) bool {
        return self.len == 0;
    }

    /// Ensures the table has capacity for additional variables.
    /// Grows the underlying storage if needed.
    /// Used to prevent repeated reallocations during type checking.
    pub fn reserve(self: *UnificationTable, additional: usize) !void {
        const new_capacity = self.len + additional;
        if (new_capacity > self.entries.len) {
            self.entries = try self.allocator.realloc(self.entries, new_capacity);
        }
    }

    pub fn format(
        self: UnificationTable,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("\nUnificationTable {\n");
        var i: usize = 0;
        while (i < self.len) : (i += 1) {
            const entry = self.entries[i];
            try writer.print("  {}: {} parent={?}\n", .{
                i,
                entry.descriptor,
                entry.parent,
            });
        }
        try writer.writeAll("}\n");
    }

    pub fn debugLog(self: *const UnificationTable, comptime fmt: []const u8, args: anytype) void {
        if (builtin.is_test and self.debug_capture != null) {
            self.debug_capture.?.print(fmt, args) catch unreachable;
        }
    }

    /// Prints a detailed representation of the UnificationTable's current state to the debug log.
    pub fn debugPrint(self: *const UnificationTable) void {
        self.debugLog("==================== Unification Table ====================\n", .{});

        if (self.len == 0) {
            self.debugLog("(empty table)\n", .{});
            return;
        }

        // First pass: Print direct information about each variable
        for (0..self.len) |i| {
            const entry = self.entries[i];
            self.debugLog("Var {d}: {any}", .{ i, entry.descriptor });
            if (entry.parent) |parent| {
                self.debugLog(" → {d}", .{parent.val});
            }
            self.debugLog("\n", .{});
        }

        // Track which variables we've already processed
        var processed = std.AutoHashMap(u32, void).init(self.allocator);
        defer processed.deinit();

        // Find and print each equivalence class
        for (0..self.len) |i| {
            if (processed.contains(@intCast(i))) continue;

            const current = Variable{ .val = @intCast(i) };
            const root = self.rootKeyWithoutCompacting(current);

            // Only print trees that have more than one node
            var has_children = false;
            const temp = current;
            while (self.entries[temp.val].parent) |_| {
                has_children = true;
                break;
            }

            if (has_children) {
                self.debugLog("Class with root {d}:\n", .{root.val});

                // Print the path from each variable to the root
                for (0..self.len) |j| {
                    if (processed.contains(@intCast(j))) continue;

                    const var_j = Variable{ .val = @intCast(j) };
                    if (self.rootKeyWithoutCompacting(var_j).val == root.val) {
                        processed.put(@intCast(j), {}) catch continue;

                        // Build and print the chain
                        var current_var = var_j;
                        self.debugLog("  {d}", .{current_var.val});

                        while (self.entries[current_var.val].parent) |parent| {
                            self.debugLog(" → {d}", .{parent.val});
                            current_var = parent;
                        }
                        self.debugLog("\n", .{});
                    }
                }
            }
        }

        self.debugLog("===========================================================\n", .{});
    }
};

/// Controls how type unification behaves
pub const UnificationMode = packed struct {
    eq: bool = false,
    present: bool = false,
    lambda_set_specialization: bool = false,
    _padding: u5 = 0, // To make it byte-aligned

    // Predefined modes
    pub const EQ = UnificationMode{ .eq = true };
    pub const PRESENT = UnificationMode{ .present = true };
    pub const LAMBDA_SET_SPECIALIZATION = UnificationMode{ .eq = true, .lambda_set_specialization = true };

    pub fn isEq(self: UnificationMode) bool {
        std.debug.assert(!(self.eq and self.present));
        return self.eq;
    }

    pub fn isPresent(self: UnificationMode) bool {
        std.debug.assert(!(self.eq and self.present));
        return self.present;
    }

    pub fn isLambdaSetSpecialization(self: UnificationMode) bool {
        std.debug.assert(!(self.eq and self.present));
        return self.lambda_set_specialization;
    }

    pub fn asEq(self: UnificationMode) UnificationMode {
        return .{
            .eq = true,
            .present = false,
            .lambda_set_specialization = self.lambda_set_specialization,
        };
    }

    pub fn format(
        self: UnificationMode,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (self.eq) {
            try writer.writeAll("~");
        } else if (self.present) {
            try writer.writeAll("+=");
        } else {
            @panic("Bad mode!");
        }
    }
};

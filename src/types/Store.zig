const std = @import("std");
const Allocator = std.mem.Allocator;
const Ident = @import("../base/Ident.zig");

/// VarStore manages generation of fresh type variables by tracking the next available ID.
pub const VarStore = struct {
    next: u32,

    /// Create a new VarStore initialized to the first user space variable.
    pub fn init() VarStore {
        return VarStore.initWithNext(Variable.FIRST_USER_SPACE_VAR);
    }

    /// Create a new VarStore initialized with a specific next variable ID.
    /// The next ID must be >= the first user space variable.
    pub fn initWithNext(next: Variable) VarStore {
        std.debug.assert(next.val >= Variable.FIRST_USER_SPACE_VAR.val);
        return VarStore{ .next = next.val };
    }

    /// Returns the next variable ID that would be generated, without actually generating it.
    /// Useful for looking ahead at variable allocation without affecting the counter.
    pub fn peek(self: *VarStore) u32 {
        return self.next;
    }

    /// Generates and returns a new unique variable ID.
    /// Guarantees each call returns a different ID by incrementing the internal counter.
    /// Used to create fresh type variables during type inference.
    pub fn fresh(self: *VarStore) Variable {
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
};

pub const Content = union(enum) {
    /// A type variable which the user did not name in an annotation,
    ///
    /// When we auto-generate a type var name, e.g. the "a" in (a -> a), we
    /// change the Option in here from None to Some.
    // FlexVar(Option<SubsIndex<Lowercase>>),
    FlexVar: ?[]usize,

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
        }
    }
};

pub const FlatType = union(enum) {
    Apply: struct {
        name: Ident.Idx,
        arguments: []Ident.Idx,
    },

    Func: struct {
        arguments: []Ident.Idx,
        lambda_set: Ident.Idx,
        result: Ident.Idx,
        fx: Ident.Idx,
    },

    EmptyRecord,

    EmptyTagUnion,
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
        try std.fmt.format(writer, ", r: {}, m: {}, c: {?}", .{ // Changed to {?} for optional
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
            NONE.value => try writer.writeAll("none"),
            VISITED_IN_OCCURS_CHECK.value => try writer.writeAll("visited_in_occurs_check"),
            OCCURS.value => try writer.writeAll("occurs"),
            GET_VAR_NAMES.value => try writer.writeAll("get_var_names"),
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

/// UnificationTable manages unification operations between types.
/// It provides functionality for building, querying, and manipulating a table
/// of type variables and their descriptors.
pub const UnificationTable = struct {

    // Fields to track entries
    entries: []Entry,
    allocator: Allocator,
    len: usize,

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
        std.debug.print("\n=== rootKey called for variable {} ===\n", .{variable.val});

        // First find the root without compressing
        const current = variable;
        var root = current;

        // Follow the chain to find the root
        while (self.entries[root.val].parent) |parent| {
            root = parent;
        }

        std.debug.print("Found root: {}\n", .{root.val});
        return root;
    }

    /// Like rootKey() but doesn't perform path compression.
    /// Useful when you need to inspect the structure without modifying it.
    /// Important for debugging and maintaining parent-child relationships.
    pub fn rootKeyWithoutCompacting(self: *const UnificationTable, variable: Variable) Variable {
        std.debug.print("\n--- rootKeyWithoutCompacting for variable {} ---\n", .{variable.val});
        var key = variable;
        var depth: usize = 0;

        while (self.entries[key.val].parent) |parent| {
            depth += 1;
            std.debug.print("  Level {}: {} -> {}\n", .{ depth, key.val, parent.val });
            key = parent;
        }

        std.debug.print("  Found root: {} (depth: {})\n", .{ key.val, depth });
        std.debug.print("--- rootKeyWithoutCompacting finished ---\n", .{});
        return key;
    }

    /// Merges two equivalence classes by making one root point to another.
    /// Core unification operation that maintains the union-find invariants.
    /// Takes a descriptor to associate with the merged class.
    pub fn unifyRoots(self: *UnificationTable, to: Variable, from: Variable, desc: Descriptor) void {
        std.debug.print("\n*** unifyRoots called ***\n", .{});
        std.debug.print("Unifying from={} to={}\n", .{ from.val, to.val });

        if (from.val != to.val) {
            // Debug: print current state
            std.debug.print("Current state before unification:\n", .{});
            std.debug.print("  'from' chain: ", .{});
            var current = from;
            while (self.entries[current.val].parent) |p| {
                std.debug.print("{} -> ", .{current.val});
                current = p;
            }
            std.debug.print("{}\n", .{current.val});

            // Important: Set from's parent to point to 'to' directly
            self.entries[from.val].parent = to; // Changed this line
            self.entries[to.val].descriptor = desc;

            // Debug: print final state
            std.debug.print("Final state after unification:\n", .{});
            std.debug.print("  Chain from original 'from': ", .{});
            current = from;
            while (self.entries[current.val].parent) |p| {
                std.debug.print("{} -> ", .{current.val});
                current = p;
            }
            std.debug.print("{}\n", .{current.val});
        } else {
            std.debug.print("Self-unification - no changes needed\n", .{});
        }
        std.debug.print("*** unifyRoots finished ***\n\n", .{});
    }

    /// High-level unification operation that merges two type variables.
    /// Makes the second variable point to the first, sharing type information.
    /// Main entry point for type unification during type checking.
    pub fn unify(self: *UnificationTable, v1: Variable, v2: Variable) !void {
        std.debug.print("\n=== Unifying {} and {} ===\n", .{ v1.val, v2.val });

        // Get the current descriptor of v1
        const desc = self.getDescriptor(v1);

        // Perform the unification by making v2 point to v1
        self.unifyRoots(v1, v2, desc);

        // Debug: print the resulting chain
        std.debug.print("After unification:\n", .{});
        var current = v2;
        std.debug.print("Chain from {}: ", .{current.val});
        while (self.entries[current.val].parent) |parent| {
            std.debug.print("{} -> ", .{current.val});
            current = parent;
        }
        std.debug.print("{} (root)\n", .{current.val});
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
        return self.rootKey(var1).val == self.rootKey(var2).val;
    }

    /// Checks if a variable points to another (has a parent).
    /// Used to determine if a variable has been unified with another.
    pub fn isRedirect(self: *const UnificationTable, variable: Variable) bool {
        const has_parent = self.entries[variable.val].parent != null;
        std.debug.print("isRedirect check for {}: {}\n", .{ variable.val, has_parent });
        if (has_parent) {
            std.debug.print("  Parent is: {}\n", .{self.entries[variable.val].parent.?.val});
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
};

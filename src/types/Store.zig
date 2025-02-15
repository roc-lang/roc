const std = @import("std");
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

    // /// Create a new VarStore initialized based on the size of a subs table.
    // /// The next ID will be the length of the subs table.
    // pub fn initFromSubs(subs: *const Subs) VarStore {
    //     const next = @as(u32, @intCast(subs.utable.len()));
    //     std.debug.assert(next >= Variable.FIRST_USER_SPACE_VAR.val);
    //     return VarStore{ .next = next };
    // }

    /// Get the next variable ID without incrementing
    pub fn peek(self: *VarStore) u32 {
        return self.next;
    }

    /// Get a fresh variable by incrementing the next ID counter
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

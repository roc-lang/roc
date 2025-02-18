const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const resolve = @import("resolve_imports.zig");
const TypeStore = @import("../types/Store.zig");
const Variable = @import("../types/store.zig").Variable;
const Descriptor = @import("../types/store.zig").Descriptor;
const UnificationTable = @import("../types/store.zig").UnificationTable;
const UnificationMode = @import("../types/store.zig").UnificationMode;
const Content = @import("../types/store.zig").Content;

/// Solves for the types of expressions in the ResolveIR and populates this
/// information in the module's type store.
pub fn checkTypes(
    resolve_ir: resolve.IR,
    other_modules: []resolve.IR,
) TypeStore {
    _ = resolve_ir;
    _ = other_modules;

    // constriain

    @panic("not implemented");
}

/// Represents the outcome of a unification operation
pub const Outcome = struct {
    mismatches: collections.SafeList(TypeMismatch),
    has_changed: bool,
    extra_metadata: MetaCollector,

    // pub fn union(self: *Outcome, other: Outcome) void {
    //     _ = self;
    //     _ = other;
    //     // TODO: Combine outcomes
    // }
};

/// Represents different kinds of type mismatches that can occur during unification
pub const TypeMismatch = union(enum) {
    /// General type mismatch between two types
    TypeMismatch,

    /// A type was outside the expected numeric range
    TypeNotInRange,

    pub fn format(
        self: TypeMismatch,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .TypeMismatch => try writer.writeAll("Type mismatch"),
            .TypeNotInRange => try writer.writeAll("Type not in range"),
        }
    }
};

/// Holds the context for a unification operation
pub const Context = struct {
    first: Variable,
    first_desc: Descriptor,
    second: Variable,
    second_desc: Descriptor,
    mode: UnificationMode,
};

/// The main unification function
pub fn unify(
    env: *Env,
    var1: Variable,
    var2: Variable,
    mode: UnificationMode,
    observed_pol: Polarity,
) Unified {
    _ = env;
    _ = var1;
    _ = var2;
    _ = mode;
    _ = observed_pol;
    // TODO: Implement unification
    @panic("not implemented");
}

/// Core unification helper that processes different content types
fn unifyContext(
    env: *Env,
    pool: *Pool,
    ctx: Context,
) Outcome {
    _ = env;
    _ = pool;
    _ = ctx;
    // TODO: Implement context-based unification
}

/// Pool of variables used during unification
/// TODO ;; this isn't right needs to be "growable"
pub const Pool = []Variable;

/// Result of unification operation
pub const Unified = union(enum) {
    Success: struct {
        vars: Pool,
        extra_metadata: MetaCollector,
    },
    Failure: struct {
        pool: Pool,
        error_type1: ErrorType,
        error_type2: ErrorType,
    },

    pub fn expectSuccess(
        self: Unified,
        err_msg: []const u8,
    ) struct {
        Pool,
        MetaCollector,
    } {
        _ = self;
        _ = err_msg;
        // TODO: Return success values or error
        @panic("not implemented");
    }
};

/// Helper to merge content during unification
fn merge(
    env: *Env,
    ctx: *const Context,
    content: Content,
) Outcome {
    _ = env;
    _ = ctx;
    _ = content;
    // TODO: Implement content merging
}

/// Environment for type unification and type variable management
pub const Env = struct {
    allocator: Allocator,

    /// Storage for type variables and their descriptors
    vars: UnificationTable,

    /// Track variables that have been fixed during recursion
    fixed_variables: std.AutoHashMap(Variable, void),

    /// Track recursion pairs to prevent infinite recursion
    recursion_pairs: std.AutoHashMap(RecursionPair, void),

    /// Storage for variable slices
    variable_slices: std.ArrayList(Variable),

    /// Storage for symbol names
    // symbol_names: std.ArrayList(Symbol),

    /// Storage for tag names
    // tag_names: std.ArrayList(TagName),

    pub fn init(allocator: Allocator) !Env {
        return Env{
            .allocator = allocator,
            .vars = try UnificationTable.init(allocator, 1024),
            .fixed_variables = std.AutoHashMap(Variable, void).init(allocator),
            .recursion_pairs = std.AutoHashMap(RecursionPair, void).init(allocator),
            .variable_slices = std.ArrayList(Variable).init(allocator),
            // .symbol_names = std.ArrayList(Symbol).init(allocator),
            // .tag_names = std.ArrayList(TagName).init(allocator),
        };
    }

    pub fn deinit(self: *Env) void {
        self.vars.deinit();
        self.fixed_variables.deinit();
        self.recursion_pairs.deinit();
        self.variable_slices.deinit();
    }

    /// Get descriptor for a variable
    pub fn get(self: *Env, typ_var: Variable) Descriptor {
        return self.vars.getDescriptor(typ_var);
    }

    /// Set descriptor for a variable
    pub fn set(self: *Env, typ_var: Variable, desc: Descriptor) void {
        self.vars.setDescriptor(typ_var, desc);
    }

    /// Create a fresh variable with given descriptor
    pub fn fresh(self: *Env, desc: Descriptor) Variable {
        return self.vars.push(desc.content, desc.rank, desc.mark, desc.copy);
    }

    /// Mark a variable as fixed
    pub fn markFixed(self: *Env, typ_var: Variable) !void {
        try self.fixed_variables.put(typ_var, {});
    }

    /// Check if a variable was fixed
    pub fn wasFixed(self: *Env, typ_var: Variable) bool {
        return self.fixed_variables.contains(typ_var);
    }

    /// Add a recursion pair
    pub fn addRecursionPair(self: *Env, var1: Variable, var2: Variable) !void {
        try self.recursion_pairs.put(.{ .first = var1, .second = var2 }, {});
    }

    /// Remove a recursion pair
    pub fn removeRecursionPair(self: *Env, var1: Variable, var2: Variable) void {
        _ = self.recursion_pairs.remove(.{ .first = var1, .second = var2 });
    }

    /// Check if a recursion pair exists
    pub fn seenRecursionPair(self: *Env, var1: Variable, var2: Variable) bool {
        return self.recursion_pairs.contains(.{ .first = var1, .second = var2 });
    }
};

const RecursionPair = struct {
    first: Variable,
    second: Variable,
};

/// Represents the direction or "polarity" of type checking
pub const Polarity = enum {
    /// Types that appear in positive positions represent values we're producing
    Positive,
    /// Types that appear in negative positions represent values we're consuming
    Negative,
    /// Types that appear in both positions (like record fields)
    Both,

    /// Value polarity - used when we want to observe the actual value type
    pub const OF_VALUE = Polarity.Positive;

    /// Equivalent to logical "and" of polarities
    pub fn andPol(self: Polarity, other: Polarity) Polarity {
        return switch (self) {
            .Positive => other,
            .Negative => other,
            .Both => .Both,
        };
    }

    /// Flips the polarity (positive becomes negative and vice versa)
    pub fn flip(self: Polarity) Polarity {
        return switch (self) {
            .Positive => .Negative,
            .Negative => .Positive,
            .Both => .Both,
        };
    }
};

/// Interface for collecting metadata during unification
pub const MetaCollector = struct {
    /// Function for recording specialization lambda sets
    recordSpecializationLambdaSetFn: ?*const fn (
        collector: *MetaCollector,
        // member: Symbol,
        region: u8,
        typ_var: Variable,
    ) void,

    /// Function for recording changed variables
    recordChangedVariableFn: ?*const fn (
        collector: *MetaCollector,
        // subs: *Subs,
        typ_var: Variable,
    ) void,

    /// Function for merging with another collector
    unionFn: ?*const fn (
        collector: *MetaCollector,
        other: MetaCollector,
    ) void,

    /// Whether we are performing member~specialization where member is an ability member
    /// signature and specialization is an ability specialization for a given type
    is_specialization: bool = false,

    /// Whether this is a late collection phase
    is_late: bool = false,

    /// Records a specialization lambda set if the collector supports it
    pub fn recordSpecializationLambdaSet(
        self: *MetaCollector,
        // member: Symbol,
        region: u8,
        typ_var: Variable,
    ) void {
        if (self.recordSpecializationLambdaSetFn) |f| {
            f(self, region, typ_var);
        }
    }

    /// Records a changed variable if the collector supports it
    pub fn recordChangedVariable(
        self: *MetaCollector,
        // subs: *Subs,
        typ_var: Variable,
    ) void {
        if (self.recordChangedVariableFn) |f| {
            f(self, typ_var);
        }
    }

    /// Merges with another collector if supported
    pub fn @"union"(self: *MetaCollector, other: MetaCollector) void {
        if (self.unionFn) |f| {
            f(self, other);
        }
    }
};

/// Default no-op collector implementation
pub const NoCollector = struct {
    collector: MetaCollector,

    pub fn init() NoCollector {
        return .{
            .collector = .{
                .recordSpecializationLambdaSetFn = null,
                .recordChangedVariableFn = null,
                .unionFn = null,
                .is_specialization = false,
                .is_late = false,
            },
        };
    }

    pub fn asMetaCollector(self: *NoCollector) *MetaCollector {
        return &self.collector;
    }
};

test "polarity operations" {
    const expect = std.testing.expect;

    try expect(Polarity.Positive.flip() == .Negative);
    try expect(Polarity.Negative.flip() == .Positive);
    try expect(Polarity.Both.flip() == .Both);

    try expect(Polarity.Positive.andPol(.Negative) == .Negative);
    try expect(Polarity.Both.andPol(.Positive) == .Both);
    try expect(Polarity.Negative.andPol(.Negative) == .Negative);
}

pub const ErrorType = union(enum) {
    Infinite,
    Type: usize, //(Symbol, Vec<ErrorType>),
    /// If the name was auto-generated, it will start with a `#`.
    FlexVar, //(Lowercase),
    RigidVar, //(Lowercase),
    InferenceVar,
    EffectfulFunc,
};

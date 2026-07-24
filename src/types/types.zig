//! This module defines the core data structures for representing types in the compiler's
//! Hindley-Milner type inference system. It includes:
//!
//! - `Var`: unique type variable identifiers
//! - `Descriptor`: the rank, mark, and structure of a type
//! - `Content`: the semantic meaning of a type (flex var, alias, function, record, etc.)
//! - `FlatType`: the 'flat' shape of a type (tuples, numbers, tag unions, etc.)
//! - `Alias`: nominal or structural type aliases
//! - `Func`, `Record`, `TagUnion`: structured type forms
//!
//! Special care is taken to keep memory layouts small and efficient. When modifying
//! these types, please consider their size impact and unification performance.
//!
//! Note: In other HM compilers (Elm, Roc Rust), marks are used to track intermediate
//! metadata around type variables. Here, we intentionally do _not_ use them. The
//! idea being that because marks are not used after type checking, if we store them
//! on the type descriptor, then that memory has to stay allocated until the end
//! of the program, even though they're not used! Instead, we allocate intermediate
//! data structures during type checking to do the job that marks do, then deallocate
//! after the phase, freeing that memory.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const numeral_mod = @import("numeral.zig");

const Ident = base.Ident;
const ModuleIdentity = base.ModuleIdentity;
const MkSafeList = collections.SafeList;
const MkSafeMultiList = collections.SafeMultiList;

test {
    // If your changes caused this number to go down, great! Please update it to the lower number.
    // If it went up, please make sure your changes are absolutely required!
    try std.testing.expectEqual(32, @sizeOf(Descriptor));
    try std.testing.expectEqual(28, @sizeOf(Content));
    try std.testing.expectEqual(20, @sizeOf(Alias));
    try std.testing.expectEqual(24, @sizeOf(FlatType));
    try std.testing.expectEqual(12, @sizeOf(Record));
    try std.testing.expectEqual(20, @sizeOf(NominalType)); // Increased from 16 due to source identity and opacity bits
    // Folding `binop_negated` and `num_literal` into the `origin` union is a
    // semantic regrouping (kind-specific payloads now live inside their variant),
    // not a size win: the literal-origin variant still embeds a full `NumeralInfo`,
    // so the `Origin` union dominates the struct. Provenance adds a raw expr index
    // (4B) plus a where-clause expect region (8B), both `maxInt`-sentinel packed.
    // The optional derived-map payload selection adds its tag, payload index,
    // and optional discriminant without affecting type identity.
    try std.testing.expectEqual(96, @sizeOf(StaticDispatchConstraint));
    // Directed effect dependencies must survive type generalization,
    // instantiation, and cross-module copying, so they live with the function
    // payload rather than in checker-only side state.
    try std.testing.expectEqual(20, @sizeOf(Func));
}

test "source declaration checked constructors enforce packed statement capacity" {
    const max_alias_statement = SourceDecl.max_statement;
    const max_alias_decl = try SourceDecl.fromStatementWithBuiltinOriginChecked(max_alias_statement, true);
    try std.testing.expectEqual(@as(?u32, max_alias_statement), max_alias_decl.toOptional());
    try std.testing.expect(max_alias_decl.originIsBuiltin());

    try std.testing.expectError(
        error.OutOfMemory,
        SourceDecl.fromStatementWithBuiltinOriginChecked(max_alias_statement + 1, false),
    );

    const max_nominal_statement = NominalType.Source.max_statement;
    const max_nominal_decl = try SourceDecl.fromStatementWithBuiltinOriginChecked(max_nominal_statement, true);
    const max_nominal_source = try NominalType.Source.initChecked(max_nominal_decl, true, true);
    try std.testing.expect(max_nominal_source.sourceDecl().eql(max_nominal_decl));
    try std.testing.expect(max_nominal_source.isOpaque());
    try std.testing.expect(max_nominal_source.originIsBuiltin());

    const too_large_for_nominal = try SourceDecl.fromStatementChecked(max_nominal_statement + 1);
    try std.testing.expectError(
        error.OutOfMemory,
        NominalType.Source.initChecked(too_large_for_nominal, false, false),
    );
}

/// A type variable
pub const Var = enum(u32) {
    _,

    /// A safe list of type variables
    pub const SafeList = MkSafeList(Var);

    /// Debug representation of a type variable, panics on allocation failure
    pub fn allocPrint(self: Var, gpa: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        return try std.fmt.allocPrint(gpa, "#{d}", .{@intFromEnum(self)});
    }
};

/// A mapping from polymorphic type variables to concrete type variables
pub const VarMap = std.hash_map.HashMap(Var, Var, std.hash_map.AutoContext(Var), 80);

/// TypeScope represents nested type scopes for resolving polymorphic type variables.
/// Each HashMap in the list represents a scope level, mapping polymorphic type variables
/// to their resolved monomorphic equivalents.
pub const TypeScope = struct {
    scopes: std.array_list.Managed(VarMap),

    pub fn init(allocator: std.mem.Allocator) TypeScope {
        return .{
            .scopes = std.array_list.Managed(VarMap).init(allocator),
        };
    }

    pub fn deinit(self: *TypeScope) void {
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit();
    }

    /// Look up a type variable in all nested scopes, returning the mapped variable if found
    pub fn lookup(self: *const TypeScope, var_to_find: Var) ?Var {
        for (self.scopes.items) |*scope| {
            if (scope.get(var_to_find)) |mapped_var| {
                return mapped_var;
            }
        }
        return null;
    }
};

/// A type descriptor
pub const Descriptor = struct {
    content: Content,
    rank: Rank,
};

/// In general, the rank tracks the number of let-bindings a variable is "under".
/// Top-level definitions have rank 1. A let inside a top-level definition gets rank 2, and so on.
///
/// An example:
/// ```
/// foo = 3
///
/// plus_five = |arg| {
///    x = 5
///    arg + x
/// }
/// ```
/// Here the rank of `foo` is 1 because it is at the top level and the rank of `x` is 2 because it is under or inside `plus_five`.
///
/// Imported variables get rank 2.
///
/// Rank 0 is special, it is used for variables that are generalized (generic).
///
/// Keeping track of ranks makes type inference faster.
///
pub const Rank = enum(u8) {
    /// When the corresponding type is generic, like in `List.len`.
    generalized = 0,
    outermost = 1,
    _,

    /// Get the lowest rank
    pub fn min(a: Rank, b: Rank) Rank {
        return @enumFromInt(@min(@intFromEnum(a), @intFromEnum(b)));
    }

    /// Get the lowest rank
    pub fn max(a: Rank, b: Rank) Rank {
        return @enumFromInt(@max(@intFromEnum(a), @intFromEnum(b)));
    }

    /// Get the next rank
    pub fn next(a: Rank) Rank {
        return @enumFromInt(@intFromEnum(a) + 1);
    }

    /// Get the prev rank
    pub fn prev(a: Rank) Rank {
        return @enumFromInt(@intFromEnum(a) - 1);
    }
};

// content //

/// Represents what the a type *is*
pub const Content = union(enum(u8)) {
    const Self = @This();

    flex: Flex,
    rigid: Rigid,
    alias: Alias,
    structure: FlatType,
    err,

    // helpers //

    /// Unwrap a record or return null
    pub fn unwrapRecord(content: Self) ?Record {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .record => |record| {
                        return record;
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a tag union or return null
    pub fn unwrapTagUnion(content: Self) ?TagUnion {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .tag_union => |tag_union| {
                        return tag_union;
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a nominal type or return null
    pub fn unwrapNominalType(content: Self) ?NominalType {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .nominal_type => |nominal_type| {
                        return nominal_type;
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a function (pure, eff, or unbound) and return it
    pub fn unwrapFunc(content: Self) ?Func {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .fn_pure => |func| return func,
                    .fn_effectful => |func| return func,
                    .fn_unbound => |func| return func,
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a function (pure, eff, or unbound) and return it
    pub fn unwrapFuncFull(content: Self) ?struct { func: Func, ext: enum { unbound, pure, effectful } } {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .fn_pure => |func| return .{ .func = func, .ext = .pure },
                    .fn_effectful => |func| return .{ .func = func, .ext = .effectful },
                    .fn_unbound => |func| return .{ .func = func, .ext = .unbound },
                    else => return null,
                }
            },
            else => return null,
        }
    }
};

// flex //

/// A flex var, with optional static dispatch constraints
pub const Flex = struct {
    name: ?Ident.Idx,
    constraints: StaticDispatchConstraint.SafeList.Range,

    pub fn init() Flex {
        return .{
            .name = null,
            .constraints = StaticDispatchConstraint.SafeList.Range.empty(),
        };
    }

    pub fn withName(self: Flex, name: ?Ident.Idx) Flex {
        return .{
            .name = name,
            .constraints = self.constraints,
        };
    }

    pub fn withConstraints(self: Flex, constraints: StaticDispatchConstraint.SafeList.Range) Flex {
        return .{
            .name = self.name,
            .constraints = constraints,
        };
    }
};

// rigid //

/// A rigid var, with optional static dispatch constraints
pub const Rigid = struct {
    name: Ident.Idx,
    constraints: StaticDispatchConstraint.SafeList.Range,

    pub fn init(name: Ident.Idx) Rigid {
        return .{
            .name = name,
            .constraints = StaticDispatchConstraint.SafeList.Range.empty(),
        };
    }

    pub fn withConstraints(self: Rigid, constraints: StaticDispatchConstraint.SafeList.Range) Rigid {
        return .{
            .name = self.name,
            .constraints = constraints,
        };
    }
};

// alias //

/// A named alias to a different type
pub const Alias = struct {
    ident: TypeIdent,
    vars: Var.SafeList.NonEmptyRange,
    /// Env-local index of the declaring module's deep content identity in the
    /// owning module env's identity table (see `base.module_identity`).
    origin_module: ModuleIdentity.Idx,
    /// CIR statement index of the source declaration in origin_module, when
    /// this alias came from a concrete source declaration. A decl LOCATOR for
    /// resolving method tables in the owning env — never part of identity.
    source_decl: SourceDecl = .none,
};

/// Represents an ident of a type
/// TODO: Should this be something like CanIdent???
pub const TypeIdent = struct {
    const Self = @This();

    ident_idx: Ident.Idx,
};

/// Source statement identity for a local nominal/alias declaration.
///
/// This is stored in hot type payloads, so it deliberately uses one u32 slot.
/// Alias source identity stores a u30 statement plus presence and builtin-origin
/// bits. Nominal source identity stores a u29 statement plus presence, opacity,
/// and builtin-origin bits. Producer paths must use the checked constructors so
/// oversized CIR statement ids return `error.OutOfMemory` before release builds
/// can truncate packed fields.
pub const SourceDecl = packed struct(u32) {
    statement: u30,
    present: bool,
    builtin_origin: bool,

    pub const max_statement: u32 = std.math.maxInt(u30);

    pub const none: SourceDecl = .{ .statement = 0, .present = false, .builtin_origin = false };

    pub fn fromOptional(source_decl: ?u32) SourceDecl {
        return fromOptionalWithBuiltinOrigin(source_decl, false);
    }

    pub fn fromOptionalChecked(source_decl: ?u32) std.mem.Allocator.Error!SourceDecl {
        return fromOptionalWithBuiltinOriginChecked(source_decl, false);
    }

    pub fn fromOptionalWithBuiltinOrigin(source_decl: ?u32, builtin_origin: bool) SourceDecl {
        const statement = source_decl orelse return .none;
        return fromStatementWithBuiltinOrigin(statement, builtin_origin);
    }

    pub fn fromOptionalWithBuiltinOriginChecked(source_decl: ?u32, builtin_origin: bool) std.mem.Allocator.Error!SourceDecl {
        const statement = source_decl orelse return .none;
        return fromStatementWithBuiltinOriginChecked(statement, builtin_origin);
    }

    pub fn fromStatement(statement: u32) SourceDecl {
        return fromStatementWithBuiltinOrigin(statement, false);
    }

    pub fn fromStatementChecked(statement: u32) std.mem.Allocator.Error!SourceDecl {
        return fromStatementWithBuiltinOriginChecked(statement, false);
    }

    pub fn fromStatementWithBuiltinOrigin(statement: u32, builtin_origin: bool) SourceDecl {
        std.debug.assert(statement <= max_statement);
        return .{ .statement = @intCast(statement), .present = true, .builtin_origin = builtin_origin };
    }

    pub fn fromStatementWithBuiltinOriginChecked(statement: u32, builtin_origin: bool) std.mem.Allocator.Error!SourceDecl {
        if (statement > max_statement) return error.OutOfMemory;
        return fromStatementWithBuiltinOrigin(statement, builtin_origin);
    }

    pub fn toOptional(self: SourceDecl) ?u32 {
        return if (self.present) self.statement else null;
    }

    pub fn originIsBuiltin(self: SourceDecl) bool {
        return self.present and self.builtin_origin;
    }

    pub fn eql(self: SourceDecl, other: SourceDecl) bool {
        if (self.present != other.present) return false;
        return !self.present or (self.statement == other.statement and self.builtin_origin == other.builtin_origin);
    }
};

const NominalSource = packed struct(u32) {
    statement: u29,
    present: bool,
    is_opaque: bool,
    builtin_origin: bool,

    pub const max_statement: u32 = std.math.maxInt(u29);

    pub fn init(source_decl: SourceDecl, is_opaque: bool, builtin_origin: bool) NominalSource {
        if (source_decl.toOptional()) |statement| {
            std.debug.assert(statement <= max_statement);
            return .{
                .statement = @intCast(statement),
                .present = true,
                .is_opaque = is_opaque,
                .builtin_origin = builtin_origin,
            };
        }

        return .{
            .statement = 0,
            .present = false,
            .is_opaque = is_opaque,
            .builtin_origin = builtin_origin,
        };
    }

    pub fn initChecked(source_decl: SourceDecl, is_opaque: bool, builtin_origin: bool) std.mem.Allocator.Error!NominalSource {
        if (source_decl.toOptional()) |statement| {
            if (statement > max_statement) return error.OutOfMemory;
        }

        return init(source_decl, is_opaque, builtin_origin);
    }

    pub fn sourceDecl(self: NominalSource) SourceDecl {
        return if (self.present) SourceDecl.fromStatementWithBuiltinOrigin(self.statement, self.builtin_origin) else .none;
    }

    pub fn isOpaque(self: NominalSource) bool {
        return self.is_opaque;
    }

    pub fn originIsBuiltin(self: NominalSource) bool {
        return self.builtin_origin;
    }
};

// flat types //

/// Represents type without indirection, it's the concrete form that a type
/// takes after resolving type variables and aliases.
pub const FlatType = union(enum(u8)) {
    record: Record,
    record_unbound: RecordField.SafeMultiList.Range,
    tuple: Tuple,
    nominal_type: NominalType,
    fn_pure: Func,
    fn_effectful: Func,
    fn_unbound: Func,
    empty_record,
    tag_union: TagUnion,
    empty_tag_union,
};

// tuples //

/// Represents a tuple
pub const Tuple = struct {
    elems: Var.SafeList.Range,
};

// number types (used by layout and canonicalization) //

/// Integer types - used by layout.zig
pub const Int = struct {
    /// The exact precision of an Int
    pub const Precision = enum(u4) {
        u8 = 0,
        i8 = 1,
        u16 = 2,
        i16 = 3,
        u32 = 4,
        i32 = 5,
        u64 = 6,
        i64 = 7,
        u128 = 8,
        i128 = 9,

        /// Size in bytes
        pub fn size(self: @This()) u32 {
            // int values always have the same size as their alignment
            return @as(u32, @intCast(self.alignment().toByteUnits()));
        }

        /// Alignment
        pub fn alignment(self: @This()) std.mem.Alignment {
            // Both self and std.mem.Alignment are stored as log2(alignment) integers,
            // although we have to divide self by 2 to get to that exact representation.
            return @enumFromInt(@intFromEnum(self) / 2);
        }
    };
};

/// Floating-point types - used by layout.zig
pub const Frac = struct {
    pub const Precision = enum(u3) {
        f32 = 2,
        f64 = 3,
        dec = 4,

        /// Size in bytes
        pub fn size(self: @This()) u32 {
            // frac values always have the same size as their alignment
            return @as(u32, @intCast(self.alignment().toByteUnits()));
        }

        /// Alignment
        pub fn alignment(self: @This()) std.mem.Alignment {
            // Map precision values to log2(alignment):
            // f32 (2) -> 4 bytes -> log2(4) = 2
            // f64 (3) -> 8 bytes -> log2(8) = 3
            // dec (4) -> 16 bytes -> log2(16) = 4
            return @enumFromInt(@intFromEnum(self));
        }
    };
};

// nominal types //

/// A nominal type application in the value type graph: the declaration's
/// identity plus the actual type arguments, and nothing else. The backing
/// type lives exclusively in the declaration table (see `NominalDecl`); code
/// that legitimately needs it instantiates the declaration's backing template
/// with these args substituted for the declaration's formals.
pub const NominalType = struct {
    pub const Source = NominalSource;

    ident: TypeIdent,
    /// The actual type arguments (may be empty).
    args: Var.SafeList.Range,
    /// Env-local index of the declaring module's deep content identity in the
    /// owning module env's identity table (see `base.module_identity`).
    origin_module: ModuleIdentity.Idx,
    /// Packed source-declaration and opacity bits. Together with
    /// `origin_module`, the statement is the declaration KEY for the
    /// declaration table; it also locates method tables in the owning env.
    source: NominalSource,

    pub fn sourceDecl(self: NominalType) SourceDecl {
        return self.source.sourceDecl();
    }

    pub fn sourceDeclOptional(self: NominalType) ?u32 {
        return self.source.sourceDecl().toOptional();
    }

    pub fn isOpaque(self: NominalType) bool {
        return self.source.isOpaque();
    }

    pub fn originIsBuiltin(self: NominalType) bool {
        return self.source.originIsBuiltin();
    }

    /// Checks if backing types can unify directly with this nominal type
    pub fn canLiftInner(self: NominalType, cur_module_identity: ModuleIdentity.Idx) bool {
        if (self.isOpaque()) {
            // If opaque, then can only lift inner type if the current module is
            // the same
            return self.origin_module == cur_module_identity;
        }

        // If not opaque, then the inner type can always be lifted
        return true;
    }
};

/// A nominal type declaration: the single owner of the declaration's formal
/// type parameters and backing template within one type store.
///
/// Declarations are keyed by (origin module identity, source declaration
/// statement) in the store's declaration table (`Store.nominal_decls`). Local
/// declarations are registered when the checker processes the declaration
/// statement; imported declarations are copied into the destination store the
/// first time a nominal application of theirs crosses the module boundary
/// (see `copy_import.zig`), so every store is self-contained: any nominal
/// application present in a store can resolve its declaration in that same
/// store.
pub const NominalDecl = struct {
    /// Display name of the declaration. Never part of identity.
    ident: TypeIdent,
    /// Env-local index of the declaring module's deep content identity in the
    /// owning module env's identity table (see `base.module_identity`).
    origin_module: ModuleIdentity.Idx,
    /// Packed statement locator plus opacity and builtin-origin bits — the
    /// same bits nominal applications of this declaration carry. The
    /// statement must be present: a declaration entry without a source
    /// statement has no key and cannot be registered.
    source: NominalType.Source,
    /// The declaration's formal type parameters (rigid vars), in declaration
    /// order. Instantiating the backing for a nominal application substitutes
    /// the application's actual args for these, positionally.
    formals: Var.SafeList.Range,
    /// The declaration's backing template. It references `formals` and is
    /// never unified against directly — backing access instantiates a copy
    /// with actual args substituted for formals.
    backing: Var,
    /// Declaration flags, padding-free so serialized bytes are deterministic.
    flags: Flags,

    /// Declaration flags. Packed to a full u32 so `NominalDecl` has no
    /// implicit padding bytes (the declaration table serializes raw).
    pub const Flags = packed struct(u32) {
        /// False once the declaration is known invalid (malformed backing or
        /// invalid recursion). Applications of invalid declarations poison
        /// to err.
        valid: bool,
        _unused: u31 = 0,
    };

    /// The statement index of this declaration in its origin module env.
    pub fn statement(self: NominalDecl) u32 {
        const source_decl = self.source.sourceDecl();
        std.debug.assert(source_decl.present);
        return source_decl.statement;
    }

    /// Whether this declaration is well-formed.
    pub fn isValid(self: NominalDecl) bool {
        return self.flags.valid;
    }

    /// A safe list of nominal declarations
    pub const SafeList = MkSafeList(@This());
    /// An index into a safe list of nominal declarations
    pub const Idx = SafeList.Idx;
};

// functions //

/// Represents a function
pub const Func = struct {
    args: Var.SafeList.Range,
    ret: Var,
    /// Function types whose effects flow into this function's effect.
    ///
    /// This is a directed dependency list, not type equality: an effectful
    /// dependency makes this function effectful, while an independently
    /// effectful caller does not make its dependencies effectful. The range is
    /// empty for functions whose effect kind is already self-contained.
    effect_deps: Var.SafeList.Range = Var.SafeList.Range.empty(),
};

// records //

/// Represents a record
pub const Record = struct {
    fields: RecordField.SafeMultiList.Range,
    ext: Var,

    const Self = @This();
};

/// A field on a record
pub const RecordField = struct {
    const Self = @This();

    /// The name of the field
    name: Ident.Idx,
    /// The type of the field's value
    var_: Var,

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByName(ident_store, a, b) == .lt;
    }

    /// Get the ordering of how a compares to b
    pub fn orderByName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.name);
        const b_text = store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }

    /// A safe multi list of record fields
    pub const SafeMultiList = MkSafeMultiList(Self);

    /// A safe list of record fields
    pub const SafeList = MkSafeList(Self);
};

/// Two record fields
pub const TwoRecordFields = struct {
    a: RecordField,
    b: RecordField,

    /// A safe list of tag union fields
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tag union fields
    pub const SafeMultiList = MkSafeMultiList(@This());
};

// tag unions //

/// Represents a tag union
pub const TagUnion = struct {
    tags: Tag.SafeMultiList.Range,
    ext: Var,
};

/// A tag entry in a tag union row
pub const Tag = struct {
    /// The name of the tag (e.g. "Ok", "Err")
    name: Ident.Idx,

    /// A list of argument types for the tag (0 = no payload)
    args: Var.SafeList.Range,

    const Self = @This();

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByName(ident_store, a, b) == .lt;
    }

    /// Get the ordering of how a compares to b
    pub fn orderByName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.name);
        const b_text = store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }

    /// A safe list of tags
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tags
    pub const SafeMultiList = MkSafeMultiList(@This());
};

/// Two tag union fields
pub const TwoTags = struct {
    a: Tag,
    b: Tag,

    /// A safe list of tag union fields
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tag union fields
    pub const SafeMultiList = MkSafeMultiList(@This());
};

// content //

/// Value facts for a numeric literal, carried on its `from_numeral` dispatch
/// constraint.
///
/// Derived once from the parser's exact digit facts (the module env's numeral
/// table) — never from a pre-baked concrete value — so every stage that asks
/// "does this literal fit type T?" reads the same precomputed answer from the
/// same computation (src/types/numeral.zig). Conversions to concrete bits do
/// not read this struct at all; they consume the exact digits directly at
/// monotype lowering.
pub const NumeralInfo = struct {
    /// The digits with the decimal point removed (before·10^scale + after) as
    /// a u128 magnitude, when they fit; meaningful only when `has_magnitude`.
    /// This is value identity for canonical type keys, not a conversion
    /// source.
    magnitude: [16]u8,

    /// Whether `magnitude` holds the combined digits. False for literals
    /// whose digits exceed u128 — their `fits` set is still exact.
    has_magnitude: bool,

    /// Count of decimal digits after the point (0 for integer literals).
    scale: u32,

    /// Which builtin numeric types can represent the exact value, computed
    /// once by `numeral.computeFitSet` and intersected when two literal vars
    /// unify.
    fits: numeral_mod.FitSet,

    /// Whether the literal had a leading minus sign.
    is_negative: bool,

    /// Whether the literal was written fractionally — with a decimal point or
    /// nonzero fractional digits. `1e5` is not fractional; `3.0` is.
    is_fractional: bool,

    /// Whether this literal had an explicit type suffix such as `12.U64`.
    explicit_suffix: bool = false,

    /// Whether the literal's exact digits were recorded and can be
    /// materialized as a `Num.Numeral` (a custom type's `from_numeral` needs
    /// the digit lists at runtime). False only for literals whose digit
    /// expansion exceeds the recordable bound (e.g. `3e6000000000`); such a
    /// literal also carries an empty `fits` set, since its exact value is
    /// unknowable.
    can_materialize_numeral: bool,

    /// Source region for error reporting
    region: base.Region,

    /// Build the constraint-carried facts from a literal's exact digits and
    /// its precomputed fit set.
    pub fn fromExact(exact: numeral_mod.Exact, fit_set: numeral_mod.FitSet, can_materialize_numeral: bool, region: base.Region) NumeralInfo {
        const combined = if (can_materialize_numeral) combinedMagnitude(exact) else null;
        return .{
            .magnitude = @bitCast(combined orelse 0),
            .has_magnitude = combined != null,
            .scale = exact.scale,
            .fits = fit_set,
            .is_negative = exact.is_negative,
            .is_fractional = exact.is_fractional,
            .can_materialize_numeral = can_materialize_numeral,
            .explicit_suffix = false,
            .region = region,
        };
    }

    /// The combined digit magnitude when meaningful.
    pub fn magnitudeU128(self: NumeralInfo) ?u128 {
        if (!self.has_magnitude) return null;
        return @bitCast(self.magnitude);
    }

    /// Merge two literals' facts when their type variables unify: the merged
    /// constraint must hold for both literals, so the fit sets intersect and
    /// the syntactic flags union. The base (magnitude, scale, region, suffix)
    /// comes from the side that refutes the canonical Dec default, if either
    /// does, so diagnostics anchor at the offending literal.
    pub fn merged(a: NumeralInfo, b: NumeralInfo) NumeralInfo {
        var result = if (!a.fits.contains(.dec)) a else b;
        result.fits = a.fits.intersectWith(b.fits);
        result.is_negative = a.is_negative or b.is_negative;
        result.is_fractional = a.is_fractional or b.is_fractional;
        result.can_materialize_numeral = a.can_materialize_numeral and b.can_materialize_numeral;
        return result;
    }

    /// Canonical key bytes for this literal's value facts, hashed into
    /// checked type keys: the combined magnitude, scale, fit set, and
    /// syntactic flags. Two literals with the same digit string hash the
    /// same; literals whose digits exceed u128 hash by their fit set alone.
    ///
    /// Identity is the recorded digits, NOT the normalized value: `1.50`
    /// records {magnitude 150, scale 2} and hashes differently from `1.5`'s
    /// {15, 1}. This is deliberate — leading/trailing-zero spellings are
    /// vanishingly rare in practice, so normalizing every literal's digits
    /// at key time to deduplicate them would be a net perf loss. The only
    /// cost of a missed match is a canonical-key/digest cache miss between
    /// equal-valued spellings, never a wrong type or wrong bits.
    pub fn keyBytes(self: NumeralInfo) [24]u8 {
        var bytes: [24]u8 = undefined;
        @memcpy(bytes[0..16], &self.magnitude);
        std.mem.writeInt(u32, bytes[16..20], self.scale, .little);
        std.mem.writeInt(u16, bytes[20..22], @as(u16, self.fits.bits.mask), .little);
        bytes[22] = @as(u8, @intFromBool(self.has_magnitude)) |
            (@as(u8, @intFromBool(self.is_negative)) << 1) |
            (@as(u8, @intFromBool(self.is_fractional)) << 2);
        bytes[23] = 0;
        return bytes;
    }

    fn combinedMagnitude(exact: numeral_mod.Exact) ?u128 {
        const before = numeral_mod.magnitudeU128(exact.before) orelse return null;
        const after = numeral_mod.magnitudeU128(exact.after) orelse return null;
        if (before == 0) return after;
        var shifted = before;
        var remaining = exact.scale;
        while (remaining > 0) : (remaining -= 1) {
            const product = @mulWithOverflow(shifted, 10);
            if (product[1] != 0) return null;
            shifted = product[0];
        }
        const sum = @addWithOverflow(shifted, after);
        if (sum[1] != 0) return null;
        return sum[0];
    }

    /// Test-only convenience: facts for an integer literal given as a value,
    /// for unit tests that need a numeral constraint without parsing source.
    pub fn testOnlyInt(value: u128, is_negative: bool, region: base.Region) NumeralInfo {
        var bytes_be: [16]u8 = @bitCast(std.mem.nativeToBig(u128, value));
        var start: usize = 0;
        while (start < bytes_be.len and bytes_be[start] == 0) : (start += 1) {}
        const exact = numeral_mod.Exact{
            .before = bytes_be[start..],
            .after = &.{},
            .scale = 0,
            .is_negative = is_negative,
            .is_fractional = false,
        };
        // Integer fit computation never allocates.
        var no_heap: [0]u8 = .{};
        var fba = std.heap.FixedBufferAllocator.init(&no_heap);
        const fit_set = numeral_mod.computeFitSet(fba.allocator(), exact) catch unreachable;
        return fromExact(exact, fit_set, true, region);
    }
};

/// Metadata for one expression embedded in a string interpolation.
pub const InterpolationPartMetadata = struct {
    var_: Var,
    /// The interpolation use site's region. This remains stable when `var_` is
    /// remapped to a fresh variable during instantiation or cross-module copy.
    region: base.Region,

    pub const SafeList = MkSafeList(@This());
};

/// Represents a static dispatch constraints on a variable
///
/// sort  : List(a) -> List(a) where [a.ord : a -> Ord]
///                                   ^^^^^^^^^^^^^^^
pub const StaticDispatchConstraint = struct {
    const Self = @This();

    /// the dispatch fn name
    fn_name: Ident.Idx,
    /// the dispatch fn var, a function
    fn_var: Var,
    /// the origin of this constraint (operator, method call, where clause, or
    /// literal). Kind-specific payloads (binop negation, literal info) live
    /// *inside* the variant, so they can't exist without or apart from it.
    origin: Origin,
    /// Where this constraint was introduced, so ambiguity can be reported at the
    /// user's own expression without reconstructing var->expr maps after the
    /// fact. Copied verbatim by instantiation and cross-module import. This is
    /// METADATA: it is deliberately excluded from type identity — canonical type
    /// keys (`writeConstraints`) and unification content-equality never read it,
    /// so two structurally identical constraints with different provenance stay
    /// equal.
    provenance: Provenance = .{},
    /// The direct tag payload selected by checked derived `map`/`map!`.
    /// This is checker-owned derivation metadata and, like `provenance`, does
    /// not participate in type identity or constraint equality.
    derived_map_plan: ?DerivedMapPlan = null,
    /// Extra checking metadata for `from_interpolation`. This travels with the
    /// constraint because the checker must re-check interpolated part types
    /// after instantiation and cross-module copying. It is metadata, not type
    /// identity: canonical type keys and constraint equivalence do not read it.
    interpolation: InterpolationMetadata = .none,

    /// The introducing site of a static dispatch constraint. `intro_expr` is the
    /// raw `CIR.Expr.Idx` of the expression that created the constraint, stored
    /// as a plain index because `types` sits below `canonicalize` in the layering
    /// and cannot name `CIR.Expr.Idx`; the checker converts it back. It is
    /// module-local — after cross-module import it refers to the ORIGINATING
    /// module's CIR. `expect_region` is the where-clause "expect" region, set
    /// only when the constraint was created inside a where-clause annotation
    /// context (distinct from the intro expr's own region). Both use a `maxInt`
    /// sentinel for "absent" so the record grows by only an index + a region.
    pub const Provenance = struct {
        intro_expr: OptExprIdx = .none,
        expect_region: OptRegion = OptRegion.none,

        /// An optional raw `CIR.Expr.Idx`. `none` marks a synthetic constraint
        /// with no introducing expression.
        pub const OptExprIdx = enum(u32) {
            none = std.math.maxInt(u32),
            _,

            pub fn from(raw: u32) OptExprIdx {
                std.debug.assert(raw != std.math.maxInt(u32));
                return @enumFromInt(raw);
            }

            /// The raw index, or null when absent.
            pub fn get(self: OptExprIdx) ?u32 {
                return if (self == .none) null else @intFromEnum(self);
            }
        };

        /// An optional region packed into a `Region` using a `maxInt` start
        /// offset as the "absent" sentinel (a real region never starts at
        /// `maxInt`), avoiding the extra tag byte a Zig optional would add.
        pub const OptRegion = struct {
            region: base.Region,

            pub const none = OptRegion{ .region = .{
                .start = .{ .offset = std.math.maxInt(u32) },
                .end = .{ .offset = std.math.maxInt(u32) },
            } };

            pub fn some(region: base.Region) OptRegion {
                std.debug.assert(region.start.offset != std.math.maxInt(u32));
                return .{ .region = region };
            }

            /// The region, or null when absent.
            pub fn get(self: OptRegion) ?base.Region {
                return if (self.region.start.offset == std.math.maxInt(u32)) null else self.region;
            }
        };
    };

    pub const InterpolationMetadata = struct {
        expr_region: Provenance.OptRegion = Provenance.OptRegion.none,
        item_var: Var = no_var,
        interpolated_parts: InterpolationPartMetadata.SafeList.Range = .empty(),

        const no_var: Var = @enumFromInt(std.math.maxInt(u32));

        pub const none = InterpolationMetadata{};

        pub fn isPresent(self: InterpolationMetadata) bool {
            return self.expr_region.get() != null;
        }
    };

    /// The kinds of literal that desugar to open literal-conversion constraints.
    /// Adding a variant makes every kind-keyed `switch` fail to compile until
    /// handled — the exhaustiveness *is* the checklist.
    pub const LiteralKind = enum(u4) {
        numeral, // numeric literal, dispatches `from_numeral`
        quote, // string literal, dispatches `from_quote`
        interpolation, // interpolated string literal, dispatches `from_interpolation`
    };

    /// The per-kind payload carried by a literal-conversion origin. The payload can't
    /// exist without its kind, nor a literal-origin without its payload.
    pub const LiteralInfo = union(LiteralKind) {
        numeral: NumeralInfo,
        /// No payload here; a string literal's bytes live in the dispatch plan,
        /// not the type store.
        quote,
        /// Interpolated string literals carry no payload here either; like
        /// quotes they default to Str.
        interpolation,
    };

    /// Tracks where a static dispatch constraint originated from
    pub const Origin = union(enum) {
        /// From binary operator desugaring (e.g., +, -, *). `negated` is true when
        /// the source operator was `!=` rather than `==`.
        desugared_binop: struct { negated: bool },
        desugared_unaryop, // From uniary operator desugaring (e.g., !)
        method_call, // From `.method()` syntax
        /// From a where clause in a type annotation. `body_required` is true when
        /// the originating scheme's body provably forces this method: during the
        /// scheme's own check a body dispatch of this method matched and unified
        /// against this where-clause. It distinguishes a contract the
        /// implementation actually dispatches (so an unpinnable instantiated
        /// receiver is a genuine ambiguity) from a phantom contract the body never
        /// uses (which stays a valid polymorphic signature). The bit rides the
        /// constraint itself through instantiation and unification merges (see
        /// unify.zig), including across module boundaries where the originating
        /// scheme's body is not otherwise visible — module-local provenance like
        /// `intro_expr` cannot substitute for it.
        where_clause: struct { body_required: bool = false },
        from_literal: LiteralInfo, // From a literal conversion (from_numeral, from_quote, or from_interpolation)

        /// The numeral payload, if this origin is a numeric literal conversion;
        /// null otherwise.
        pub fn numeralInfo(self: Origin) ?NumeralInfo {
            return switch (self) {
                .from_literal => |lit| switch (lit) {
                    .numeral => |info| info,
                    .quote => null,
                    .interpolation => null,
                },
                else => null,
            };
        }

        /// The literal kind, if this origin is a literal conversion; null otherwise.
        pub fn literalKind(self: Origin) ?LiteralKind {
            return switch (self) {
                .from_literal => |lit| lit,
                else => null,
            };
        }

        /// Whether this is a `desugared_binop` whose source operator was `!=`
        /// rather than `==`; false otherwise.
        pub fn binopNegated(self: Origin) bool {
            return switch (self) {
                .desugared_binop => |binop| binop.negated,
                else => false,
            };
        }
    };

    /// A safe list of static dispatch constraints
    pub const SafeList = MkSafeList(Self);

    /// A safe multi list of static dispatch constraints
    pub const SafeMultiList = MkSafeMultiList(Self);

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByFnNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByFnName(ident_store, a, b) == .lt;
    }

    /// Get the ordering of how a compares to b
    pub fn orderByFnName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.fn_name);
        const b_text = store.getText(b.fn_name);
        return std.mem.order(u8, a_text, b_text);
    }
};

/// Source-type identity for the payload slot selected by derived mapping.
/// The checked artifact translates `tag_name` into its canonical tag name.
pub const DerivedMapPlan = struct {
    tag_name: Ident.Idx,
    payload_index: u32,
};

/// Two record fields
pub const TwoStaticDispatchConstraints = struct {
    a: StaticDispatchConstraint,
    b: StaticDispatchConstraint,

    /// A safe list of tag union fields
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tag union fields
    pub const SafeMultiList = MkSafeMultiList(@This());
};

/// Polarity of a type, or roughly, what side of an arrow it appears on.
pub const Polarity = enum {
    /// A type that appears in negative/input position
    neg,
    /// A type that appears in positive/output position
    pos,

    pub const lhs = Polarity.neg;
    pub const rhs = Polarity.pos;
};

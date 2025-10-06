//! Common IR types and utilities
//! This module contains type definitions and utilities used across the canonicalization IR.

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const reporting = @import("reporting");
const builtins = @import("builtins");

const CompactWriter = collections.CompactWriter;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const RegionInfo = base.RegionInfo;
const Region = base.Region;
const SExprTree = base.SExprTree;
const SExpr = base.SExpr;
const TypeVar = types_mod.Var;

// Re-export these from other modules for convenience
pub const NodeStore = @import("NodeStore.zig");
pub const Node = @import("Node.zig");
pub const Expr = @import("Expression.zig").Expr;
pub const Pattern = @import("Pattern.zig").Pattern;
pub const Statement = @import("Statement.zig").Statement;
pub const TypeAnno = @import("TypeAnnotation.zig").TypeAnno;
pub const Diagnostic = @import("Diagnostic.zig").Diagnostic;

// Type definitions for module compilation

/// Represents a definition (binding of a pattern to an expression) in the CIR
pub const Def = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pattern: Pattern.Idx,
    expr: Expr.Idx,
    annotation: ?Annotation.Idx,
    kind: Kind,

    pub const Kind = union(enum) {
        /// A def that introduces identifiers
        let: void,
        /// A standalone statement with an fx variable
        stmt: TypeVar,
        /// Ignored result, must be effectful
        ignored: TypeVar,

        pub fn decode(encoded: [2]u32) Kind {
            if (encoded[0] == 0) {
                return .let;
            } else if (encoded[0] == 1) {
                return .{ .stmt = @as(TypeVar, @enumFromInt(encoded[1])) };
            } else {
                return .{ .ignored = @as(TypeVar, @enumFromInt(encoded[1])) };
            }
        }

        pub fn encode(self: Kind) [2]u32 {
            switch (self) {
                .let => return .{ 0, 0 },
                .stmt => |ty_var| return .{ 1, @intFromEnum(ty_var) },
                .ignored => |ty_var| return .{ 2, @intFromEnum(ty_var) },
            }
        }
    };

    pub fn pushToSExprTree(self: *const Def, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        const name: []const u8 = switch (self.kind) {
            .let => "d-let",
            .stmt => "d-stmt",
            .ignored => "d-ignored",
        };
        try tree.pushStaticAtom(name);

        const attrs = tree.beginNode();

        try cir.store.getPattern(self.pattern).pushToSExprTree(cir, tree, self.pattern);
        try cir.store.getExpr(self.expr).pushToSExprTree(cir, tree, self.expr);

        if (self.annotation) |annotation_idx| {
            try cir.store.getAnnotation(annotation_idx).pushToSExprTree(cir, tree, annotation_idx);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents a type header (e.g., 'Maybe a' or 'Result err ok') in type annotations
pub const TypeHeader = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { start: u32, len: u32 };

    name: base.Ident.Idx,
    args: TypeAnno.Span,

    pub fn pushToSExprTree(self: *const TypeHeader, cir: anytype, tree: anytype, idx: TypeHeader.Idx) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("ty-header");

        // Get the region for this TypeHeader
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
        const region = cir.store.getRegionAt(node_idx);
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        const name_str = cir.getIdent(self.name);
        try tree.pushStringPair("name", name_str);

        const attrs = tree.beginNode();

        if (self.args.span.len > 0) {
            const args_begin = tree.beginNode();
            try tree.pushStaticAtom("ty-args");
            const args_attrs = tree.beginNode();
            for (cir.store.sliceTypeAnnos(self.args)) |anno_idx| {
                try cir.store.getTypeAnno(anno_idx).pushToSExprTree(cir, tree, anno_idx);
            }
            try tree.endNode(args_begin, args_attrs);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents a where clause constraint in type definitions
pub const WhereClause = union(enum) {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    mod_method: struct {
        var_name: base.Ident.Idx,
        method_name: base.Ident.Idx,
        args: TypeAnno.Span,
        ret_anno: TypeAnno.Idx,
        external_decl: ExternalDecl.Idx,
    },
    mod_alias: struct {
        var_name: base.Ident.Idx,
        alias_name: base.Ident.Idx,
        external_decl: ExternalDecl.Idx,
    },
    malformed: struct {
        diagnostic: Diagnostic.Idx,
    },

    pub fn pushToSExprTree(self: *const WhereClause, cir: anytype, tree: anytype, idx: WhereClause.Idx) !void {
        switch (self.*) {
            .mod_method => |method| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("method");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                // Add module-of and ident information
                const var_name_str = cir.getIdent(method.var_name);
                try tree.pushStringPair("module-of", var_name_str);

                const method_name_str = cir.getIdent(method.method_name);
                try tree.pushStringPair("ident", method_name_str);

                const attrs = tree.beginNode();

                // Add actual argument types
                const args_begin = tree.beginNode();
                try tree.pushStaticAtom("args");
                const args_attrs = tree.beginNode();
                for (cir.store.sliceTypeAnnos(method.args)) |arg_idx| {
                    try cir.store.getTypeAnno(arg_idx).pushToSExprTree(cir, tree, arg_idx);
                }
                try tree.endNode(args_begin, args_attrs);

                // Add actual return type
                try cir.store.getTypeAnno(method.ret_anno).pushToSExprTree(cir, tree, method.ret_anno);
                try tree.endNode(begin, attrs);
            },
            .mod_alias => |alias| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("alias");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                const var_name_str = cir.getIdent(alias.var_name);
                try tree.pushStringPair("module-of", var_name_str);

                const alias_name_str = cir.getIdent(alias.alias_name);
                try tree.pushStringPair("ident", alias_name_str);

                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .malformed => |malformed| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("malformed");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                _ = malformed;
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }
};

/// Represents a type annotation associated with definitions
pub const Annotation = struct {
    pub const Idx = enum(u32) { _ };

    type_anno: TypeAnno.Idx,
    signature: TypeVar,
    where: ?WhereClause.Span,

    pub fn pushToSExprTree(self: *const Annotation, cir: anytype, tree: anytype, idx: Annotation.Idx) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("annotation");

        // Get the region for this Annotation
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
        const region = cir.store.getRegionAt(node_idx);
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        const attrs = tree.beginNode();

        const type_anno_begin = tree.beginNode();
        try tree.pushStaticAtom("declared-type");
        const type_anno_attrs = tree.beginNode();
        try cir.store.getTypeAnno(self.type_anno).pushToSExprTree(cir, tree, self.type_anno);
        try tree.endNode(type_anno_begin, type_anno_attrs);

        // Include where clauses if present
        if (self.where) |where_span| {
            const where_clauses = cir.store.sliceWhereClauses(where_span);
            if (where_clauses.len > 0) {
                const where_begin = tree.beginNode();
                try tree.pushStaticAtom("where");
                const where_attrs = tree.beginNode();
                for (where_clauses) |where_idx| {
                    const where_clause = cir.store.getWhereClause(where_idx);
                    try where_clause.pushToSExprTree(cir, tree, where_idx);
                }
                try tree.endNode(where_begin, where_attrs);
            }
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents an item exposed by a module's interface
pub const ExposedItem = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    name: base.Ident.Idx,
    alias: ?base.Ident.Idx,
    is_wildcard: bool,

    pub fn pushToSExprTree(self: *const ExposedItem, _: anytype, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("exposed");

        const name_str = cir.getIdent(self.name);
        try tree.pushStringPair("name", name_str);

        if (self.alias) |alias_idx| {
            const alias_str = cir.getIdent(alias_idx);
            try tree.pushStringPair("alias", alias_str);
        }

        try tree.pushBoolPair("wildcard", self.is_wildcard);

        const attrs = tree.beginNode();
        try tree.endNode(begin, attrs);
    }
};

/// Represents a field in a record pattern for pattern matching
pub const PatternRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { start: u32, len: u32 };
};

/// Represents an arbitrary precision smallish decimal value
pub const SmallDecValue = struct {
    numerator: i16,
    denominator_power_of_ten: u8,

    /// Convert a small dec to f64 (use for size comparisons)
    /// TODO: Review, claude generated
    pub fn toF64(self: @This()) f64 {
        const numerator_f64 = @as(f64, @floatFromInt(self.numerator));
        const divisor = std.math.pow(f64, 10, @as(f64, @floatFromInt(self.denominator_power_of_ten)));
        return numerator_f64 / divisor;
    }

    /// Calculate the int requirements of a SmallDecValue
    pub fn toFracRequirements(self: SmallDecValue) types_mod.Num.FracRequirements {
        const f64_val = self.toF64();
        return types_mod.Num.FracRequirements{
            .fits_in_f32 = fitsInF32(f64_val),
            .fits_in_dec = fitsInDec(f64_val),
        };
    }

    test "SmallDecValue.toF64 - basic cases" {
        // Test integer values
        {
            const val = SmallDecValue{ .numerator = 42, .denominator_power_of_ten = 0 };
            try std.testing.expectEqual(@as(f64, 42.0), val.toF64());
        }

        // Test decimal values
        {
            const val = SmallDecValue{ .numerator = 314, .denominator_power_of_ten = 2 };
            try std.testing.expectApproxEqAbs(@as(f64, 3.14), val.toF64(), 0.0001);
        }

        // Test negative values
        {
            const val = SmallDecValue{ .numerator = -500, .denominator_power_of_ten = 3 };
            try std.testing.expectApproxEqAbs(@as(f64, -0.5), val.toF64(), 0.0001);
        }

        // Test very small values
        {
            const val = SmallDecValue{ .numerator = 1, .denominator_power_of_ten = 10 };
            try std.testing.expectApproxEqAbs(@as(f64, 1e-10), val.toF64(), 1e-15);
        }

        // Test maximum numerator
        {
            const val = SmallDecValue{ .numerator = 32767, .denominator_power_of_ten = 0 };
            try std.testing.expectEqual(@as(f64, 32767.0), val.toF64());
        }

        // Test minimum numerator
        {
            const val = SmallDecValue{ .numerator = -32768, .denominator_power_of_ten = 0 };
            try std.testing.expectEqual(@as(f64, -32768.0), val.toF64());
        }
    }

    test "SmallDecValue.toFracRequirements - fits in all types" {
        // Small integer - fits in everything
        {
            const val = SmallDecValue{ .numerator = 100, .denominator_power_of_ten = 0 };
            const req = val.toFracRequirements();
            try std.testing.expect(req.fits_in_f32);
            try std.testing.expect(req.fits_in_dec);
        }

        // Pi approximation - fits in everything
        {
            const val = SmallDecValue{ .numerator = 31416, .denominator_power_of_ten = 4 };
            const req = val.toFracRequirements();
            try std.testing.expect(req.fits_in_f32);
            try std.testing.expect(req.fits_in_dec);
        }
    }
};

/// Check if the given f64 fits in f32 range (ignoring precision loss)
pub fn fitsInF32(f64_val: f64) bool {
    // Check if it's within the range that f32 can represent.
    // This includes normal, subnormal, and zero values.
    // (This is a magnitude check, so take the abs value to check
    // positive and negative at the same time.)
    const abs_val = @abs(f64_val);
    return abs_val == 0.0 or (abs_val >= std.math.floatTrueMin(f32) and abs_val <= std.math.floatMax(f32));
}

/// Check if a float value can be represented accurately in RocDec
pub fn fitsInDec(value: f64) bool {
    // RocDec uses i128 with 18 decimal places
    const max_dec_value = 170141183460469231731.0;
    const min_dec_value = -170141183460469231731.0;

    return value >= min_dec_value and value <= max_dec_value;
}

/// Represents an arbitrary precision integer value
pub const IntValue = struct {
    bytes: [16]u8,
    kind: IntKind,

    pub const IntKind = enum {
        i128,
        u128,
    };

    pub fn toI128(self: IntValue) i128 {
        return @bitCast(self.bytes);
    }

    pub fn bufPrint(self: IntValue, buf: []u8) ![]u8 {
        switch (self.kind) {
            .i128 => {
                const val: i128 = @bitCast(self.bytes);
                return std.fmt.bufPrint(buf, "{d}", .{val});
            },
            .u128 => {
                const val: u128 = @bitCast(self.bytes);
                return std.fmt.bufPrint(buf, "{d}", .{val});
            },
        }
    }

    /// Calculate the int requirements of an IntValue
    /// TODO: Review, claude generated
    pub fn toIntRequirements(self: IntValue) types_mod.Num.Int.Requirements {
        var is_negated = false;
        var u128_val: u128 = undefined;

        switch (self.kind) {
            .i128 => {
                const val: i128 = @bitCast(self.bytes);
                is_negated = val < 0;
                u128_val = if (val < 0) @abs(val) else @intCast(val);
            },
            .u128 => {
                const val: u128 = @bitCast(self.bytes);
                is_negated = false;
                u128_val = val;
            },
        }

        // Special handling for minimum signed values
        // These are the exact minimum values for each signed integer type.
        // They need special handling because their absolute value is one more
        // than the maximum positive value of the same signed type.
        // For example: i8 range is -128 to 127, so abs(-128) = 128 doesn't fit in i8's positive range
        const is_minimum_signed = is_negated and switch (u128_val) {
            @as(u128, @intCast(std.math.maxInt(i8))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i16))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i32))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i64))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i128))) + 1 => true,
            else => false,
        };

        // For minimum signed values, subtract 1 from the magnitude
        // This makes the bit calculation work correctly with the "n-1 bits for magnitude" rule
        const adjusted_val = if (is_minimum_signed) u128_val - 1 else u128_val;
        const bits_needed = types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val);
        return types_mod.Num.Int.Requirements{
            .sign_needed = is_negated and u128_val != 0, // -0 doesn't need a sign
            .bits_needed = bits_needed,
        };
    }

    /// Calculate the frac requirements of an IntValue
    /// TODO: Review, claude generated
    /// Calculate the frac requirements of an IntValue
    pub fn toFracRequirements(self: IntValue) types_mod.Num.FracRequirements {
        // Convert to f64 for checking
        const f64_val: f64 = switch (self.kind) {
            .i128 => @floatFromInt(@as(i128, @bitCast(self.bytes))),
            .u128 => blk: {
                const val = @as(u128, @bitCast(self.bytes));
                if (val > @as(u128, 1) << 64) {
                    break :blk std.math.inf(f64);
                }
                break :blk @floatFromInt(val);
            },
        };

        // For integers, check both range AND exact representability in f32
        const fits_in_f32 = blk: {
            // Check range
            if (!fitsInF32(f64_val)) {
                break :blk false;
            }

            // Additionally check exact representability for integers
            // F32 can exactly represent integers only up to 2^24
            const f32_max_exact_int = 16777216.0; // 2^24
            break :blk @abs(f64_val) <= f32_max_exact_int;
        };

        const fits_in_dec = fitsInDec(f64_val);

        return types_mod.Num.FracRequirements{
            .fits_in_f32 = fits_in_f32,
            .fits_in_dec = fits_in_dec,
        };
    }
};

/// Canonical information about a number
pub const NumKind = enum {
    // If this number has no restrictions
    num_unbound,

    // If this number is an int with no restrictions
    int_unbound,

    // If the number has an explicit suffix
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
};

// RocDec type definition (for missing export)
// Must match the structure of builtins.RocDec
pub const RocDec = builtins.dec.RocDec;

/// Converts a RocDec to an i128 integer
pub fn toI128(self: RocDec) i128 {
    return self.num;
}

/// Creates a RocDec from an f64 value, returns null if conversion fails
pub fn fromF64(f: f64) ?RocDec {
    // Simple conversion - the real implementation is in builtins/dec.zig
    const scaled = @as(i128, @intFromFloat(f * 1_000_000_000_000_000_000.0));
    return RocDec{ .num = scaled };
}

/// Represents an import statement in a module
pub const Import = struct {
    pub const Idx = enum(u32) { _ };

    pub const Store = struct {
        /// Map from interned string idx to Import.Idx for deduplication
        map: std.AutoHashMapUnmanaged(base.StringLiteral.Idx, Import.Idx) = .{},
        /// List of interned string IDs indexed by Import.Idx
        imports: collections.SafeList(base.StringLiteral.Idx) = .{},

        pub fn init() Store {
            return .{};
        }

        pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
            self.map.deinit(allocator);
            self.imports.deinit(allocator);
        }

        /// Get or create an Import.Idx for the given module name.
        /// The module name is first checked against existing imports by comparing strings.
        pub fn getOrPut(self: *Store, allocator: std.mem.Allocator, strings: *base.StringLiteral.Store, module_name: []const u8) !Import.Idx {
            // First check if we already have this module name by comparing strings
            for (self.imports.items.items, 0..) |existing_string_idx, i| {
                const existing_name = strings.get(existing_string_idx);
                if (std.mem.eql(u8, existing_name, module_name)) {
                    // Found existing import with same name
                    return @as(Import.Idx, @enumFromInt(i));
                }
            }

            // Not found - create new import
            const string_idx = try strings.insert(allocator, module_name);
            const idx = @as(Import.Idx, @enumFromInt(self.imports.len()));

            // Add to both the list and the map
            _ = try self.imports.append(allocator, string_idx);
            try self.map.put(allocator, string_idx, idx);

            return idx;
        }

        /// Serialize this Store to the given CompactWriter. The resulting Store
        /// in the writer's buffer will have offsets instead of pointers. Calling any
        /// methods on it or dereferencing its internal "pointers" (which are now
        /// offsets) is illegal behavior!
        pub fn serialize(
            self: *const Store,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!*const Store {
            // First, write the Store struct itself
            const offset_self = try writer.appendAlloc(allocator, Store);

            // Then serialize the sub-structures and update the struct
            offset_self.* = .{
                .map = .{}, // Map will be empty after deserialization (only used for deduplication during insertion)
                .imports = (try self.imports.serialize(allocator, writer)).*,
            };

            return @constCast(offset_self);
        }

        /// Add the given offset to the memory addresses of all pointers in `self`.
        pub fn relocate(self: *Store, offset: isize) void {
            self.imports.relocate(offset);
        }

        pub const Serialized = struct {
            // Placeholder to match Store size - not serialized
            map: std.AutoHashMapUnmanaged(base.StringLiteral.Idx, Import.Idx) = .{},
            imports: collections.SafeList(base.StringLiteral.Idx).Serialized,

            /// Serialize a Store into this Serialized struct, appending data to the writer
            pub fn serialize(
                self: *Serialized,
                store: *const Store,
                allocator: std.mem.Allocator,
                writer: *CompactWriter,
            ) std.mem.Allocator.Error!void {
                // Serialize the imports SafeList
                try self.imports.serialize(&store.imports, allocator, writer);
                // Note: The map is not serialized as it's only used for deduplication during insertion
            }

            /// Deserialize this Serialized struct into a Store
            pub fn deserialize(self: *Serialized, offset: i64, allocator: std.mem.Allocator) *Store {
                // Overwrite ourself with the deserialized version, and return our pointer after casting it to Store.
                const store = @as(*Store, @ptrFromInt(@intFromPtr(self)));

                store.* = .{
                    .map = .{}, // Will be repopulated below
                    .imports = self.imports.deserialize(offset).*,
                };

                // Pre-allocate the exact capacity needed for the map
                const import_count = store.imports.items.items.len;
                store.map.ensureTotalCapacity(allocator, @intCast(import_count)) catch unreachable;

                // Repopulate the map - we know there's enough capacity since we
                // are deserializing from a Serialized struct
                for (store.imports.items.items, 0..) |string_idx, i| {
                    const import_idx = @as(Import.Idx, @enumFromInt(i));
                    store.map.putAssumeCapacityNoClobber(string_idx, import_idx);
                }

                return store;
            }
        };
    };
};

/// Represents a field in a record expression
pub const RecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    name: base.Ident.Idx,
    value: Expr.Idx,

    pub fn pushToSExprTree(self: *const RecordField, cir: anytype, tree: anytype) !void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("field");
        try tree.pushStringPair("name", cir.getIdent(self.name));
        const attrs = tree.beginNode();
        try cir.store.getExpr(self.value).pushToSExprTree(cir, tree, self.value);
        try tree.endNode(begin, attrs);
    }
};

/// Represents an external declaration from another module
pub const ExternalDecl = struct {
    /// Fully qualified name (e.g., "json.Json.utf8")
    qualified_name: base.Ident.Idx,
    /// Module this decl comes from (e.g., "json.Json")
    module_name: base.Ident.Idx,
    /// Local name within that module (e.g., "utf8")
    local_name: base.Ident.Idx,
    /// Type variable for this declaration
    type_var: TypeVar,
    /// Kind of external declaration
    kind: enum { value, type },
    /// Region where this was referenced
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
    /// A safe list of external declarations
    pub const SafeList = collections.SafeList(ExternalDecl);

    pub fn pushToSExprTree(self: *const ExternalDecl, cir: anytype, tree: anytype) !void {
        const node = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, self.region);

        // Add fully qualified name
        try tree.pushStringPair("ident", cir.getIdent(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }

    pub fn pushToSExprTreeWithRegion(self: *const ExternalDecl, cir: anytype, tree: anytype, region: Region) !void {
        const node = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        // Add fully qualified name
        try tree.pushStringPair("ident", cir.getIdent(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }
};

// Real Report type from the reporting module
pub const Report = reporting.Report;

/// Checks if a type is castable for index type conversions
pub fn isCastable(comptime T: type) bool {
    return switch (T) {
        Expr.Idx,
        Pattern.Idx,
        Statement.Idx,
        TypeAnno.Idx,
        Def.Idx,
        TypeHeader.Idx,
        RecordField.Idx,
        Pattern.RecordDestruct.Idx,
        Expr.IfBranch.Idx,
        Expr.Match.Branch.Idx,
        WhereClause.Idx,
        Annotation.Idx,
        TypeAnno.RecordField.Idx,
        ExposedItem.Idx,
        Expr.Match.BranchPattern.Idx,
        PatternRecordField.Idx,
        Node.Idx,
        TypeVar,
        => true,
        else => false,
    };
}

/// Safely casts between compatible index types
pub fn castIdx(comptime From: type, comptime To: type, idx: From) To {
    return @as(To, @enumFromInt(@intFromEnum(idx)));
}

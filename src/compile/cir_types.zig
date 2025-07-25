//! Common types used by the Canonical Intermediate Representation (CIR)
//! These types are extracted from the canonicalize module to avoid circular dependencies

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const DataSpan = base.DataSpan;

// Simple span types used throughout CIR
/// Represents a contiguous span of data in the store
pub const Span = struct { span: DataSpan };
/// Represents a range with start index and length
pub const Range = struct { start: u32, len: u32 };

// Index types for various CIR nodes
/// Index to a definition in the store
pub const DefIdx = enum(u32) { _ };
/// Index to a statement in the store
pub const StatementIdx = enum(u32) { _ };
/// Index to an external declaration in the store
pub const ExternalDeclIdx = enum(u32) { _ };
/// Index to a diagnostic message in the store
pub const DiagnosticIdx = enum(u32) { _ };

// Span types for collections
/// Span of definitions in the store
pub const DefSpan = struct { span: DataSpan };
/// Span of statements in the store
pub const StatementSpan = struct { span: DataSpan };
/// Span of external declarations in the store
pub const ExternalDeclSpan = struct { span: DataSpan };

/// Represents an external declaration from another module
pub const ExternalDecl = struct {
    /// Fully qualified name (e.g., "json.Json.utf8")
    qualified_name: base.Ident.Idx,
    /// Module this decl comes from (e.g., "json.Json")
    module_name: base.Ident.Idx,
    /// Local name within that module (e.g., "utf8")
    local_name: base.Ident.Idx,
    /// Type information (populated later after dependency resolution)
    type_idx: ?u32,

    pub const Idx = ExternalDeclIdx;
    pub const Span = ExternalDeclSpan;
    /// A safe list of external declarations
    pub const SafeList = collections.SafeList(ExternalDecl);

    /// Converts this external declaration to an S-expression tree representation for debugging
    pub fn pushToSExprTree(self: *const ExternalDecl, cir: anytype, tree: anytype) !void {
        _ = self;
        _ = cir;
        try tree.pushStaticAtom("external-decl-stub");
    }
};

/// Manages module imports and their metadata
pub const Import = struct {
    pub const Idx = enum(u16) { _ };
    /// A store for interning imported module names
    pub const Store = struct {
        /// Map from module name string to Import.Idx
        map: std.StringHashMapUnmanaged(Import.Idx) = .{},
        /// List of imports indexed by Import.Idx
        imports: std.ArrayListUnmanaged([]u8) = .{},
        /// Storage for module name strings
        strings: std.ArrayListUnmanaged(u8) = .{},

        pub fn init() Store {
            return .{};
        }

        pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
            self.map.deinit(allocator);
            for (self.imports.items) |import| {
                allocator.free(import);
            }
            self.imports.deinit(allocator);
            self.strings.deinit(allocator);
        }

        pub fn intern(self: *Store, allocator: std.mem.Allocator, module_name: []const u8) !Import.Idx {
            if (self.map.get(module_name)) |idx| {
                return idx;
            }

            const idx = @as(Import.Idx, @enumFromInt(self.imports.items.len));
            const owned_name = try allocator.dupe(u8, module_name);
            try self.imports.append(allocator, owned_name);
            try self.map.put(allocator, owned_name, idx);
            return idx;
        }

        pub fn get(self: *const Store, idx: Import.Idx) []const u8 {
            return self.imports.items[@intFromEnum(idx)];
        }
    };
};

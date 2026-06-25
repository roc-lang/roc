//! Parser-owned declaration inventory.
//!
//! This is deliberately syntactic. The parser records where declaration-like
//! source forms live, what token names them, and which parser scope owns them.
//! Canonicalization decides source meaning, legality, visibility, and duplicate
//! behavior.

const std = @import("std");
const base = @import("base");
const Token = @import("tokenize.zig").Token;

const DeclIndex = @This();
const Ident = base.Ident;

/// Stable index of a parser declaration scope.
pub const ScopeIdx = enum(u32) { _ };
/// Stable index of one declaration-like source form.
pub const DeclIdx = enum(u32) { _ };
/// Stable index of one parser-owned type path.
pub const TypePathIdx = enum(u32) { _ };

/// Contiguous range into a side table.
pub const Span = struct {
    start: u32,
    len: u32,

    /// Return an empty side-table span.
    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }
};

/// Token range occupied by a parser-owned declaration-index item.
pub const TokenRegion = struct {
    start: Token.Idx,
    end: Token.Idx,

    /// Return an empty token range.
    pub fn empty() TokenRegion {
        return .{ .start = 0, .end = 0 };
    }
};

/// Parser scope categories that can own declarations.
pub const ScopeKind = enum {
    module,
    block,
    associated,
};

/// Source-order visibility policy for forward references in a scope.
pub const ForwardPolicy = enum {
    whole_scope,
    source_order,
};

/// AST entity that owns a parser declaration scope.
pub const ScopeOwner = union(enum) {
    none,
    file,
    expr: u32,
    associated_type_decl: u32,
};

/// Parser declaration scope with direct declaration membership.
pub const Scope = struct {
    parent: ?ScopeIdx,
    kind: ScopeKind,
    owner: ScopeOwner,
    forward_policy: ForwardPolicy,
    owner_type_path: ?TypePathIdx,
    decls: Span,
    value_decls: std.AutoHashMapUnmanaged(Ident.Idx, NameBucket),
    type_decls: std.AutoHashMapUnmanaged(Ident.Idx, NameBucket),
    region: TokenRegion,
};

/// All declarations sharing one source name in one parser scope.
pub const NameBucket = struct {
    first: ?DeclIdx = null,
    overflow: std.ArrayListUnmanaged(DeclIdx) = .empty,

    pub const Iterator = struct {
        bucket: NameBucket,
        index: usize = 0,

        pub fn next(self: *Iterator) ?DeclIdx {
            if (self.index == 0) {
                self.index = 1;
                return self.bucket.first;
            }

            const overflow_index = self.index - 1;
            if (overflow_index >= self.bucket.overflow.items.len) return null;

            self.index += 1;
            return self.bucket.overflow.items[overflow_index];
        }
    };

    pub fn append(self: *NameBucket, gpa: std.mem.Allocator, decl_idx: DeclIdx) std.mem.Allocator.Error!void {
        if (self.first == null) {
            self.first = decl_idx;
            return;
        }

        try self.overflow.append(gpa, decl_idx);
    }

    pub fn deinit(self: *NameBucket, gpa: std.mem.Allocator) void {
        self.overflow.deinit(gpa);
    }

    pub fn count(self: NameBucket) usize {
        return (if (self.first == null) @as(usize, 0) else 1) + self.overflow.items.len;
    }

    pub fn at(self: NameBucket, index: usize) ?DeclIdx {
        if (index == 0) return self.first;

        const overflow_index = index - 1;
        if (overflow_index >= self.overflow.items.len) return null;

        return self.overflow.items[overflow_index];
    }

    pub fn iter(self: NameBucket) Iterator {
        return .{ .bucket = self };
    }
};

/// Parser-owned type path such as `Parent.Nested`.
pub const TypePath = struct {
    parent: ?TypePathIdx,
    name: Ident.Idx,
    scope: ScopeIdx,
    depth: u16,
};

const TypePathKey = struct {
    parent: ?TypePathIdx,
    root_scope: ?ScopeIdx,
    name: Ident.Idx,
};

/// Syntactic type reference recorded from a type declaration annotation.
///
/// `segments` points into `type_dependency_segments`; unqualified references
/// have one segment, qualified references preserve their full source path.
pub const TypeDependency = struct {
    segments: Span,
};

/// Upper identifier exposed from a package header. These are syntactically
/// module names that should be auto-imported unless shadowed by an explicit
/// unqualified import in the same file.
pub const PackageHeaderModule = struct {
    module_name: Ident.Idx,
    region: TokenRegion,
};

/// Explicit source import recorded by the parser.
pub const Import = struct {
    module_name: Ident.Idx,
    qualifier: ?Ident.Idx,
    nested: bool,
    region: TokenRegion,
};

/// Side-table tops captured before parsing a declaration annotation.
pub const TypeDependencyMark = struct {
    dependencies_top: u32,
    segments_top: u32,
};

/// Associated value declaration lookup key.
pub const AssocValue = struct {
    owner: TypePathIdx,
    item: Ident.Idx,
};

/// Syntactic declaration category recorded by the parser.
pub const DeclKind = enum {
    value,
    value_anno,
    type_alias,
    nominal,
    @"opaque",
    import,
    file_import,
    var_decl,
    var_anno,
};

/// Syntactic shape of a value declaration's implementation.
pub const ValueDeclForm = enum {
    none,
    lambda,
};

/// Syntactic declaration record consumed by later compiler phases.
pub const Decl = struct {
    scope: ScopeIdx,
    statement: u32,
    kind: DeclKind,
    value_form: ValueDeclForm = .none,
    value_arity: u32 = 0,
    name_tok: ?Token.Idx,
    name_ident: ?Ident.Idx = null,
    pattern: ?u32,
    anno: ?u32,
    paired_decl: ?DeclIdx = null,
    paired_anno: ?DeclIdx = null,
    associated_scope: ?ScopeIdx = null,
    owner_type_path: ?TypePathIdx = null,
    type_path: ?TypePathIdx = null,
    type_dependencies: Span = Span.empty(),
    region: TokenRegion,
};

gpa: std.mem.Allocator,
scopes: std.ArrayList(Scope),
decls: std.ArrayList(Decl),
scope_decl_ids: std.ArrayList(DeclIdx),
scope_decl_builders: std.ArrayList(std.ArrayListUnmanaged(DeclIdx)),
decl_by_statement: std.AutoHashMapUnmanaged(u32, DeclIdx),
type_dependencies: std.ArrayList(TypeDependency),
type_dependency_segments: std.ArrayList(Ident.Idx),
type_paths: std.ArrayList(TypePath),
type_path_intern: std.AutoHashMapUnmanaged(TypePathKey, TypePathIdx),
type_path_by_statement: std.AutoHashMapUnmanaged(u32, TypePathIdx),
type_decls_by_path: std.AutoHashMapUnmanaged(TypePathIdx, NameBucket),
assoc_value_decls: std.AutoHashMapUnmanaged(AssocValue, NameBucket),
assoc_owner_value_decls: std.AutoHashMapUnmanaged(TypePathIdx, NameBucket),
package_header_modules: std.ArrayList(PackageHeaderModule),
imports: std.ArrayList(Import),
explicit_unqualified_imports: std.AutoHashMapUnmanaged(Ident.Idx, void),
scope_stack: std.ArrayList(ScopeIdx),

/// Create an empty declaration index.
pub fn init(gpa: std.mem.Allocator) DeclIndex {
    return .{
        .gpa = gpa,
        .scopes = .empty,
        .decls = .empty,
        .scope_decl_ids = .empty,
        .scope_decl_builders = .empty,
        .decl_by_statement = .{},
        .type_dependencies = .empty,
        .type_dependency_segments = .empty,
        .type_paths = .empty,
        .type_path_intern = .{},
        .type_path_by_statement = .{},
        .type_decls_by_path = .{},
        .assoc_value_decls = .{},
        .assoc_owner_value_decls = .{},
        .package_header_modules = .empty,
        .imports = .empty,
        .explicit_unqualified_imports = .{},
        .scope_stack = .empty,
    };
}

/// Release all declaration-index storage.
pub fn deinit(self: *DeclIndex) void {
    for (self.scope_decl_builders.items) |*builder| {
        builder.deinit(self.gpa);
    }
    for (self.scopes.items) |*scope| {
        deinitNameBuckets(Ident.Idx, self.gpa, &scope.value_decls);
        deinitNameBuckets(Ident.Idx, self.gpa, &scope.type_decls);
    }
    deinitNameBuckets(AssocValue, self.gpa, &self.assoc_value_decls);
    deinitNameBuckets(TypePathIdx, self.gpa, &self.assoc_owner_value_decls);
    self.scope_decl_builders.deinit(self.gpa);
    self.scopes.deinit(self.gpa);
    self.decls.deinit(self.gpa);
    self.scope_decl_ids.deinit(self.gpa);
    self.decl_by_statement.deinit(self.gpa);
    self.type_dependencies.deinit(self.gpa);
    self.type_dependency_segments.deinit(self.gpa);
    self.type_paths.deinit(self.gpa);
    self.type_path_intern.deinit(self.gpa);
    self.type_path_by_statement.deinit(self.gpa);
    deinitNameBuckets(TypePathIdx, self.gpa, &self.type_decls_by_path);
    self.package_header_modules.deinit(self.gpa);
    self.imports.deinit(self.gpa);
    self.explicit_unqualified_imports.deinit(self.gpa);
    self.scope_stack.deinit(self.gpa);
}

fn deinitNameBuckets(comptime K: type, gpa: std.mem.Allocator, map: *std.AutoHashMapUnmanaged(K, NameBucket)) void {
    var iter = map.valueIterator();
    while (iter.next()) |bucket| {
        bucket.deinit(gpa);
    }
    map.deinit(gpa);
}

/// Push a new current declaration scope and return its index.
pub fn enterScope(
    self: *DeclIndex,
    kind: ScopeKind,
    owner: ScopeOwner,
    region: TokenRegion,
) std.mem.Allocator.Error!ScopeIdx {
    const parent = if (self.scope_stack.items.len == 0) null else self.scope_stack.items[self.scope_stack.items.len - 1];
    const idx: ScopeIdx = @enumFromInt(self.scopes.items.len);
    const forward_policy: ForwardPolicy = switch (kind) {
        .module, .associated => .whole_scope,
        .block => .source_order,
    };
    try self.scopes.append(self.gpa, .{
        .parent = parent,
        .kind = kind,
        .owner = owner,
        .forward_policy = forward_policy,
        .owner_type_path = null,
        .decls = Span.empty(),
        .value_decls = .{},
        .type_decls = .{},
        .region = region,
    });
    try self.scope_decl_builders.append(self.gpa, .empty);
    try self.scope_stack.append(self.gpa, idx);
    return idx;
}

/// Pop the current declaration scope and freeze its direct declaration list.
pub fn exitScope(
    self: *DeclIndex,
    idx: ScopeIdx,
    region: TokenRegion,
) std.mem.Allocator.Error!void {
    std.debug.assert(self.scope_stack.items.len > 0);
    std.debug.assert(self.scope_stack.items[self.scope_stack.items.len - 1] == idx);
    _ = self.scope_stack.pop();

    const start: u32 = @intCast(self.scope_decl_ids.items.len);
    const builder = &self.scope_decl_builders.items[@intFromEnum(idx)];
    try self.scope_decl_ids.appendSlice(self.gpa, builder.items);
    const end: u32 = @intCast(self.scope_decl_ids.items.len);
    var scope = self.scopes.items[@intFromEnum(idx)];
    scope.decls = .{ .start = start, .len = end - start };
    scope.region = region;
    self.scopes.items[@intFromEnum(idx)] = scope;
}

/// Return the scope currently being parsed, if any.
pub fn currentScope(self: *const DeclIndex) ?ScopeIdx {
    if (self.scope_stack.items.len == 0) return null;
    return self.scope_stack.items[self.scope_stack.items.len - 1];
}

/// Update a scope owner once the owning AST node index is known.
pub fn setScopeOwner(self: *DeclIndex, idx: ScopeIdx, owner: ScopeOwner) void {
    self.scopes.items[@intFromEnum(idx)].owner = owner;
}

/// Update a scope's associated owner path once known.
pub fn setScopeOwnerTypePath(self: *DeclIndex, idx: ScopeIdx, owner_type_path: ?TypePathIdx) void {
    self.scopes.items[@intFromEnum(idx)].owner_type_path = owner_type_path;
}

/// Append a declaration to its owning scope.
pub fn addDecl(self: *DeclIndex, decl: Decl) std.mem.Allocator.Error!DeclIdx {
    const idx: DeclIdx = @enumFromInt(self.decls.items.len);
    try self.decls.append(self.gpa, decl);
    try self.scope_decl_builders.items[@intFromEnum(decl.scope)].append(self.gpa, idx);
    try self.decl_by_statement.put(self.gpa, decl.statement, idx);
    if (decl.name_ident) |ident| {
        if (declKindMayBindValue(decl.kind)) {
            try addDeclToBucket(Ident.Idx, self.gpa, &self.scopes.items[@intFromEnum(decl.scope)].value_decls, ident, idx);
            if (decl.owner_type_path) |owner_path| {
                try addDeclToBucket(AssocValue, self.gpa, &self.assoc_value_decls, .{
                    .owner = owner_path,
                    .item = ident,
                }, idx);
                try addDeclToBucket(TypePathIdx, self.gpa, &self.assoc_owner_value_decls, owner_path, idx);
            }
        } else if (declKindMayBindType(decl.kind)) {
            try addDeclToBucket(Ident.Idx, self.gpa, &self.scopes.items[@intFromEnum(decl.scope)].type_decls, ident, idx);
            if (decl.type_path) |path| {
                try self.type_path_by_statement.put(self.gpa, decl.statement, path);
                try addDeclToBucket(TypePathIdx, self.gpa, &self.type_decls_by_path, path, idx);
            }
        }
    }
    return idx;
}

fn addDeclToBucket(
    comptime K: type,
    gpa: std.mem.Allocator,
    map: *std.AutoHashMapUnmanaged(K, NameBucket),
    key: K,
    decl_idx: DeclIdx,
) std.mem.Allocator.Error!void {
    const gop = try map.getOrPut(gpa, key);
    if (!gop.found_existing) {
        gop.value_ptr.* = .{};
    }
    try gop.value_ptr.append(gpa, decl_idx);
}

fn declKindMayBindValue(kind: DeclKind) bool {
    return switch (kind) {
        .value,
        .value_anno,
        .var_decl,
        .var_anno,
        => true,
        .type_alias,
        .nominal,
        .@"opaque",
        .import,
        .file_import,
        => false,
    };
}

fn declKindMayBindType(kind: DeclKind) bool {
    return switch (kind) {
        .type_alias,
        .nominal,
        .@"opaque",
        => true,
        .value,
        .value_anno,
        .var_decl,
        .var_anno,
        .import,
        .file_import,
        => false,
    };
}

/// Return whether a scope directly declares a value-like name.
pub fn scopeDeclaresValue(self: *const DeclIndex, scope_idx: ScopeIdx, ident: Ident.Idx) bool {
    const scope = &self.scopes.items[@intFromEnum(scope_idx)];
    return scope.value_decls.contains(ident);
}

/// Return all value-like declarations for a name in a scope.
pub fn scopeValueDecls(self: *const DeclIndex, scope_idx: ScopeIdx, ident: Ident.Idx) NameBucket {
    const scope = &self.scopes.items[@intFromEnum(scope_idx)];
    return scope.value_decls.get(ident) orelse .{};
}

/// Return all type declarations for a name in a scope.
pub fn scopeTypeDecls(self: *const DeclIndex, scope_idx: ScopeIdx, ident: Ident.Idx) NameBucket {
    const scope = &self.scopes.items[@intFromEnum(scope_idx)];
    return scope.type_decls.get(ident) orelse .{};
}

/// Return all associated value declarations for an owner type path and item name.
pub fn assocValueDecls(self: *const DeclIndex, owner: TypePathIdx, item: Ident.Idx) NameBucket {
    return self.assoc_value_decls.get(.{ .owner = owner, .item = item }) orelse .{};
}

/// Return all associated value declarations owned by one type path.
pub fn assocOwnerValueDecls(self: *const DeclIndex, owner: TypePathIdx) NameBucket {
    return self.assoc_owner_value_decls.get(owner) orelse .{};
}

/// Record a package-header upper expose that canonicalization should consider
/// for auto-import.
pub fn addPackageHeaderModule(self: *DeclIndex, module_name: Ident.Idx, region: TokenRegion) std.mem.Allocator.Error!void {
    try self.package_header_modules.append(self.gpa, .{
        .module_name = module_name,
        .region = region,
    });
}

/// Record an explicit source import.
pub fn addImport(self: *DeclIndex, import: Import) std.mem.Allocator.Error!void {
    try self.imports.append(self.gpa, import);
}

/// Record an explicit unqualified import. Package-header auto-imports with
/// the same module name must not be applied.
pub fn addExplicitUnqualifiedImport(self: *DeclIndex, module_name: Ident.Idx) std.mem.Allocator.Error!void {
    try self.explicit_unqualified_imports.put(self.gpa, module_name, {});
}

/// Returns whether this module name was imported explicitly without a qualifier.
pub fn hasExplicitUnqualifiedImport(self: *const DeclIndex, module_name: Ident.Idx) bool {
    return self.explicit_unqualified_imports.contains(module_name);
}

/// Intern a parser-owned type path.
pub fn internTypePath(self: *DeclIndex, scope_idx: ScopeIdx, parent: ?TypePathIdx, name: Ident.Idx) std.mem.Allocator.Error!TypePathIdx {
    const key = TypePathKey{
        .parent = parent,
        .root_scope = if (parent == null) scope_idx else null,
        .name = name,
    };
    if (self.type_path_intern.get(key)) |existing| return existing;

    const depth: u16 = if (parent) |parent_idx|
        self.type_paths.items[@intFromEnum(parent_idx)].depth + 1
    else
        1;
    const idx: TypePathIdx = @enumFromInt(self.type_paths.items.len);
    try self.type_paths.append(self.gpa, .{
        .parent = parent,
        .name = name,
        .scope = scope_idx,
        .depth = depth,
    });
    try self.type_path_intern.put(self.gpa, key, idx);
    return idx;
}

/// Find an already-interned direct child type path.
pub fn findTypePath(self: *const DeclIndex, parent: ?TypePathIdx, name: Ident.Idx) ?TypePathIdx {
    if (parent == null) return null;
    return self.type_path_intern.get(.{ .parent = parent, .root_scope = null, .name = name });
}

/// Find an already-interned root type path declared in a parser scope.
pub fn findRootTypePath(self: *const DeclIndex, scope_idx: ScopeIdx, name: Ident.Idx) ?TypePathIdx {
    return self.type_path_intern.get(.{
        .parent = null,
        .root_scope = scope_idx,
        .name = name,
    });
}

/// Find an already-interned type path from source-order path segments.
pub fn findTypePathBySegments(self: *const DeclIndex, segments: []const Ident.Idx) ?TypePathIdx {
    for (self.scopes.items, 0..) |_, scope_pos| {
        const scope_idx: ScopeIdx = @enumFromInt(scope_pos);
        if (self.findTypePathBySegmentsInScope(scope_idx, segments)) |path| return path;
    }
    return null;
}

/// Find an already-interned type path from source-order path segments rooted in one parser scope.
pub fn findTypePathBySegmentsInScope(
    self: *const DeclIndex,
    scope_idx: ScopeIdx,
    segments: []const Ident.Idx,
) ?TypePathIdx {
    var parent: ?TypePathIdx = null;
    var result: ?TypePathIdx = null;
    for (segments, 0..) |segment, index| {
        const next = if (index == 0)
            self.findRootTypePath(scope_idx, segment) orelse return null
        else
            self.findTypePath(parent, segment) orelse return null;
        parent = next;
        result = next;
    }
    return result;
}

/// Find a path relative to an existing owner path.
pub fn findTypePathRelative(self: *const DeclIndex, owner: TypePathIdx, segments: []const Ident.Idx) ?TypePathIdx {
    var parent: ?TypePathIdx = owner;
    var result: ?TypePathIdx = null;
    for (segments) |segment| {
        const next = self.findTypePath(parent, segment) orelse return null;
        parent = next;
        result = next;
    }
    return result;
}

/// Return the parser-owned type path for an AST statement, when it declares a type.
pub fn typePathForStatement(self: *const DeclIndex, statement: u32) ?TypePathIdx {
    return self.type_path_by_statement.get(statement);
}

/// Return the parser declaration recorded for an AST statement, if any.
pub fn declForStatement(self: *const DeclIndex, statement: u32) ?DeclIdx {
    return self.decl_by_statement.get(statement);
}

/// Return the first parser type declaration recorded for a type path.
pub fn typeDeclForPath(self: *const DeclIndex, path: TypePathIdx) ?DeclIdx {
    return self.typeDeclsForPath(path).at(0);
}

/// Return all parser type declarations recorded for a type path in source order.
pub fn typeDeclsForPath(self: *const DeclIndex, path: TypePathIdx) NameBucket {
    return self.type_decls_by_path.get(path) orelse .{};
}

/// Mark an adjacent annotation and value declaration as one source pair.
pub fn pairAnnotation(self: *DeclIndex, anno_idx: DeclIdx, decl_idx: DeclIdx) void {
    self.decls.items[@intFromEnum(anno_idx)].paired_decl = decl_idx;
    self.decls.items[@intFromEnum(decl_idx)].paired_anno = anno_idx;
}

/// Return the direct declarations owned by a scope.
pub fn scopeDecls(self: *const DeclIndex, scope_idx: ScopeIdx) []const DeclIdx {
    const scope = self.scopes.items[@intFromEnum(scope_idx)];
    return self.scope_decl_ids.items[scope.decls.start..][0..scope.decls.len];
}

/// Return the current type-dependency side-table tops.
pub fn typeDependencyTop(self: *const DeclIndex) TypeDependencyMark {
    return .{
        .dependencies_top = @intCast(self.type_dependencies.items.len),
        .segments_top = @intCast(self.type_dependency_segments.items.len),
    };
}

/// Append one syntactic type dependency to the side table.
pub fn addTypeDependency(self: *DeclIndex, ident: Ident.Idx) std.mem.Allocator.Error!void {
    const segments = [_]Ident.Idx{ident};
    try self.addTypeDependencySegments(&segments);
}

/// Append one syntactic type dependency path to the side table.
pub fn addTypeDependencySegments(self: *DeclIndex, segments: []const Ident.Idx) std.mem.Allocator.Error!void {
    std.debug.assert(segments.len > 0);
    const start: u32 = @intCast(self.type_dependency_segments.items.len);
    try self.type_dependency_segments.appendSlice(self.gpa, segments);
    try self.type_dependencies.append(self.gpa, .{
        .segments = .{
            .start = start,
            .len = @intCast(segments.len),
        },
    });
}

/// Return a dependency span from a previously captured side-table top.
pub fn typeDependencySpanFrom(self: *const DeclIndex, mark: TypeDependencyMark) Span {
    const end: u32 = @intCast(self.type_dependencies.items.len);
    std.debug.assert(mark.dependencies_top <= end);
    std.debug.assert(mark.segments_top <= self.type_dependency_segments.items.len);
    return .{ .start = mark.dependencies_top, .len = end - mark.dependencies_top };
}

/// Discard dependencies appended after a previously captured side-table top.
pub fn clearTypeDependenciesFrom(self: *DeclIndex, mark: TypeDependencyMark) void {
    std.debug.assert(mark.dependencies_top <= self.type_dependencies.items.len);
    std.debug.assert(mark.segments_top <= self.type_dependency_segments.items.len);
    self.type_dependencies.shrinkRetainingCapacity(@intCast(mark.dependencies_top));
    self.type_dependency_segments.shrinkRetainingCapacity(@intCast(mark.segments_top));
}

/// Return the dependency paths covered by a span.
pub fn typeDependencies(self: *const DeclIndex, span: Span) []const TypeDependency {
    return self.type_dependencies.items[span.start..][0..span.len];
}

/// Return the source-order identifier segments for one dependency path.
pub fn typeDependencySegments(self: *const DeclIndex, dependency: TypeDependency) []const Ident.Idx {
    return self.type_dependency_segments.items[dependency.segments.start..][0..dependency.segments.len];
}

/// Number of scopes in this declaration index.
pub fn scopeCount(self: *const DeclIndex) usize {
    return self.scopes.items.len;
}

/// Number of declarations in this declaration index.
pub fn declCount(self: *const DeclIndex) usize {
    return self.decls.items.len;
}

test "scopeDeclaresValue records only value-binding declarations" {
    const gpa = std.testing.allocator;

    var index = DeclIndex.init(gpa);
    defer index.deinit();

    const scope_idx = try index.enterScope(.module, .file, TokenRegion.empty());
    const attrs = Ident.Attributes.fromString("name");

    const value_ident = Ident.Idx{ .attributes = attrs, .idx = 1 };
    const value_anno_ident = Ident.Idx{ .attributes = attrs, .idx = 2 };
    const var_decl_ident = Ident.Idx{ .attributes = attrs, .idx = 3 };
    const var_anno_ident = Ident.Idx{ .attributes = attrs, .idx = 4 };
    const import_ident = Ident.Idx{ .attributes = attrs, .idx = 5 };
    const type_ident = Ident.Idx{ .attributes = attrs, .idx = 6 };

    _ = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 0,
        .kind = .value,
        .name_tok = null,
        .name_ident = value_ident,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    _ = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 1,
        .kind = .value_anno,
        .name_tok = null,
        .name_ident = value_anno_ident,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    _ = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 2,
        .kind = .var_decl,
        .name_tok = null,
        .name_ident = var_decl_ident,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    _ = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 3,
        .kind = .var_anno,
        .name_tok = null,
        .name_ident = var_anno_ident,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    _ = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 4,
        .kind = .import,
        .name_tok = null,
        .name_ident = import_ident,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    _ = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 5,
        .kind = .type_alias,
        .name_tok = null,
        .name_ident = type_ident,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });

    try index.exitScope(scope_idx, TokenRegion.empty());

    try std.testing.expect(index.scopeDeclaresValue(scope_idx, value_ident));
    try std.testing.expect(index.scopeDeclaresValue(scope_idx, value_anno_ident));
    try std.testing.expect(index.scopeDeclaresValue(scope_idx, var_decl_ident));
    try std.testing.expect(index.scopeDeclaresValue(scope_idx, var_anno_ident));
    try std.testing.expect(!index.scopeDeclaresValue(scope_idx, import_ident));
    try std.testing.expect(!index.scopeDeclaresValue(scope_idx, type_ident));
}

test "type dependency spans preserve parser-recorded type references" {
    const gpa = std.testing.allocator;

    var index = DeclIndex.init(gpa);
    defer index.deinit();

    const attrs = Ident.Attributes.fromString("TypeName");
    const first_ident = Ident.Idx{ .attributes = attrs, .idx = 1 };
    const second_ident = Ident.Idx{ .attributes = attrs, .idx = 2 };

    const start = index.typeDependencyTop();
    try index.addTypeDependency(first_ident);
    try index.addTypeDependency(second_ident);
    try index.addTypeDependencySegments(&.{ first_ident, second_ident });
    const span = index.typeDependencySpanFrom(start);

    const deps = index.typeDependencies(span);
    try std.testing.expectEqual(@as(usize, 3), deps.len);

    const first_segments = index.typeDependencySegments(deps[0]);
    try std.testing.expectEqual(@as(usize, 1), first_segments.len);
    try std.testing.expectEqual(first_ident, first_segments[0]);

    const second_segments = index.typeDependencySegments(deps[1]);
    try std.testing.expectEqual(@as(usize, 1), second_segments.len);
    try std.testing.expectEqual(second_ident, second_segments[0]);

    const qualified_segments = index.typeDependencySegments(deps[2]);
    try std.testing.expectEqual(@as(usize, 2), qualified_segments.len);
    try std.testing.expectEqual(first_ident, qualified_segments[0]);
    try std.testing.expectEqual(second_ident, qualified_segments[1]);

    index.clearTypeDependenciesFrom(start);
    try std.testing.expectEqual(start, index.typeDependencyTop());
}

test "name buckets preserve duplicate declarations in source order" {
    const gpa = std.testing.allocator;

    var index = DeclIndex.init(gpa);
    defer index.deinit();

    const scope_idx = try index.enterScope(.module, .file, TokenRegion.empty());
    const attrs = Ident.Attributes.fromString("dup");
    const ident = Ident.Idx{ .attributes = attrs, .idx = 1 };

    const first = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 0,
        .kind = .value,
        .name_tok = null,
        .name_ident = ident,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    const second = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 1,
        .kind = .value_anno,
        .name_tok = null,
        .name_ident = ident,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });

    try index.exitScope(scope_idx, TokenRegion.empty());

    const decls = index.scopeValueDecls(scope_idx, ident);
    try std.testing.expectEqual(@as(usize, 2), decls.count());
    try std.testing.expectEqual(first, decls.at(0).?);
    try std.testing.expectEqual(second, decls.at(1).?);
}

test "associated value declarations are keyed by structural owner path" {
    const gpa = std.testing.allocator;

    var index = DeclIndex.init(gpa);
    defer index.deinit();

    const parent_attrs = Ident.Attributes.fromString("Parent");
    const nested_attrs = Ident.Attributes.fromString("Nested");
    const value_attrs = Ident.Attributes.fromString("val");
    const parent_ident = Ident.Idx{ .attributes = parent_attrs, .idx = 1 };
    const nested_ident = Ident.Idx{ .attributes = nested_attrs, .idx = 2 };
    const value_ident = Ident.Idx{ .attributes = value_attrs, .idx = 3 };

    const type_scope = try index.enterScope(.module, .file, TokenRegion.empty());

    const parent_path = try index.internTypePath(type_scope, null, parent_ident);
    const nested_path = try index.internTypePath(type_scope, parent_path, nested_ident);
    const found_nested = index.findTypePathBySegments(&.{ parent_ident, nested_ident });
    try std.testing.expectEqual(nested_path, found_nested.?);

    _ = try index.addDecl(.{
        .scope = type_scope,
        .statement = 42,
        .kind = .nominal,
        .name_tok = null,
        .name_ident = nested_ident,
        .type_path = nested_path,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    try std.testing.expectEqual(nested_path, index.typePathForStatement(42).?);
    try index.exitScope(type_scope, TokenRegion.empty());

    const scope_idx = try index.enterScope(.associated, .none, TokenRegion.empty());
    index.setScopeOwnerTypePath(scope_idx, nested_path);
    const decl_idx = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 0,
        .kind = .value,
        .name_tok = null,
        .name_ident = value_ident,
        .owner_type_path = nested_path,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    try index.exitScope(scope_idx, TokenRegion.empty());

    const nested_decls = index.assocValueDecls(nested_path, value_ident);
    try std.testing.expectEqual(@as(usize, 1), nested_decls.count());
    try std.testing.expectEqual(decl_idx, nested_decls.at(0).?);

    const owner_decls = index.assocOwnerValueDecls(nested_path);
    try std.testing.expectEqual(@as(usize, 1), owner_decls.count());
    try std.testing.expectEqual(decl_idx, owner_decls.at(0).?);

    const parent_decls = index.assocValueDecls(parent_path, value_ident);
    try std.testing.expectEqual(@as(usize, 0), parent_decls.count());
}

test "type path buckets preserve duplicate declarations in source order" {
    const gpa = std.testing.allocator;

    var index = DeclIndex.init(gpa);
    defer index.deinit();

    const attrs = Ident.Attributes.fromString("Repeated");
    const ident = Ident.Idx{ .attributes = attrs, .idx = 1 };
    const scope_idx = try index.enterScope(.module, .file, TokenRegion.empty());
    const path = try index.internTypePath(scope_idx, null, ident);

    const first = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 10,
        .kind = .type_alias,
        .name_tok = null,
        .name_ident = ident,
        .type_path = path,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    const second = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 11,
        .kind = .nominal,
        .name_tok = null,
        .name_ident = ident,
        .type_path = path,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });

    const decls = index.typeDeclsForPath(path);
    try std.testing.expectEqual(@as(usize, 2), decls.count());
    try std.testing.expectEqual(first, decls.at(0).?);
    try std.testing.expectEqual(second, decls.at(1).?);
    try std.testing.expectEqual(first, index.typeDeclForPath(path).?);
}

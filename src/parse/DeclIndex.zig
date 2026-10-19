//! Parser-owned declaration inventory.
//!
//! This is deliberately syntactic. The parser records where declaration-like
//! source forms live, what token names them, and which parser scope owns them.
//! Canonicalization decides source meaning, legality, visibility, and duplicate
//! behavior.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
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
    /// The scope's value, type, and import-alias names: spans into
    /// `scope_names`, each sorted by identifier. Filled when the scope closes.
    value_names: Span,
    type_names: Span,
    import_alias_names: Span,
    region: TokenRegion,
};

/// The name tables a scope keeps its declarations in.
pub const Namespace = enum(u8) {
    value,
    type,
    import_alias,
};

/// One source name a closed scope declares in one namespace, with every
/// declaration of it in source order (a span into `scope_name_decl_ids`).
pub const ScopeName = struct {
    ident: Ident.Idx,
    decls: Span,
};

/// A name recorded in a scope that is still open.
const OpenName = struct {
    namespace: Namespace,
    ident: Ident.Idx,
    decl: DeclIdx,
    /// Position among the scope's recorded names, so sorting keeps source order.
    seq: u32,
    /// Registered by `addValueName`, which records a declaration once per name.
    once: bool,

    fn lessThan(_: void, a: OpenName, b: OpenName) bool {
        if (a.namespace != b.namespace) return @backingInt(a.namespace) < @backingInt(b.namespace);
        const a_ident = a.ident.denseIndex();
        const b_ident = b.ident.denseIndex();
        if (a_ident != b_ident) return a_ident < b_ident;
        return a.seq < b.seq;
    }
};

/// All declarations sharing one source name, in source order.
pub const NameBucket = struct {
    first: ?DeclIdx = null,
    rest: []const DeclIdx = &.{},

    pub const Iterator = struct {
        bucket: NameBucket,
        index: usize = 0,

        pub fn next(self: *Iterator) ?DeclIdx {
            const decl = self.bucket.at(self.index) orelse return null;
            self.index += 1;
            return decl;
        }
    };

    fn fromSlice(decls: []const DeclIdx) NameBucket {
        if (decls.len == 0) return .{};
        return .{ .first = decls[0], .rest = decls[1..] };
    }

    pub fn count(self: NameBucket) usize {
        return (if (self.first == null) @as(usize, 0) else 1) + self.rest.len;
    }

    pub fn at(self: NameBucket, index: usize) ?DeclIdx {
        if (index == 0) return self.first;

        const rest_index = index - 1;
        if (rest_index >= self.rest.len) return null;

        return self.rest[rest_index];
    }

    pub fn iter(self: NameBucket) Iterator {
        return .{ .bucket = self };
    }
};

/// Declarations sharing one key in a module-wide table, in source order.
const GrowingBucket = struct {
    first: ?DeclIdx = null,
    overflow: std.ArrayListUnmanaged(DeclIdx) = .empty,

    fn append(self: *GrowingBucket, gpa: std.mem.Allocator, decl_idx: DeclIdx) std.mem.Allocator.Error!void {
        if (self.first == null) {
            self.first = decl_idx;
            return;
        }

        try self.overflow.append(gpa, decl_idx);
    }

    fn deinit(self: *GrowingBucket, gpa: std.mem.Allocator) void {
        self.overflow.deinit(gpa);
    }

    fn view(self: GrowingBucket) NameBucket {
        return .{ .first = self.first, .rest = self.overflow.items };
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
    pub const Origin = enum { local, package };
    pub const LocalBase = enum { importer, package_root, parent };

    /// Source target for the selected module, excluding nested types. Local
    /// targets retain their explicit base spelling; package targets include
    /// their lowercase qualifier.
    module_name: Ident.Idx,
    /// Binding introduced when this target selects a module directly.
    module_binding: ?Ident.Idx,
    qualifier: ?Ident.Idx,
    origin: Origin,
    base: LocalBase,
    parent_count: u16,
    nested_type_path: ?Ident.Idx,
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
    where_alias,
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
/// Declarations of the open scopes, innermost last; a declaration always
/// joins the innermost open scope, so each scope's run sits on top while it
/// is open and moves to `scope_decl_ids` when it closes.
open_scope_decls: std.ArrayList(DeclIdx),
/// Where each open scope's run in `open_scope_decls` starts, parallel to
/// `scope_stack`.
open_scope_decl_starts: std.ArrayList(u32),
/// Every closed scope's names, one sorted run per scope and namespace.
scope_names: std.ArrayList(ScopeName),
/// The declarations of each entry of `scope_names`.
scope_name_decl_ids: std.ArrayList(DeclIdx),
/// Names recorded in the open scopes, innermost last, in the same nesting
/// discipline as `open_scope_decls`.
open_scope_names: std.ArrayList(OpenName),
/// Where each open scope's run in `open_scope_names` starts, parallel to
/// `scope_stack`.
open_scope_name_starts: std.ArrayList(u32),
decl_by_statement: collections.DenseMap(u32, DeclIdx),
type_dependencies: std.ArrayList(TypeDependency),
type_dependency_segments: std.ArrayList(Ident.Idx),
type_paths: std.ArrayList(TypePath),
type_path_intern: std.AutoHashMapUnmanaged(TypePathKey, TypePathIdx),
type_path_by_statement: collections.DenseMap(u32, TypePathIdx),
type_decls_by_path: std.AutoHashMapUnmanaged(TypePathIdx, GrowingBucket),
assoc_value_decls: std.AutoHashMapUnmanaged(AssocValue, GrowingBucket),
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
        .open_scope_decls = .empty,
        .open_scope_decl_starts = .empty,
        .scope_names = .empty,
        .scope_name_decl_ids = .empty,
        .open_scope_names = .empty,
        .open_scope_name_starts = .empty,
        .decl_by_statement = .init(gpa),
        .type_dependencies = .empty,
        .type_dependency_segments = .empty,
        .type_paths = .empty,
        .type_path_intern = .{},
        .type_path_by_statement = .init(gpa),
        .type_decls_by_path = .{},
        .assoc_value_decls = .{},
        .package_header_modules = .empty,
        .imports = .empty,
        .explicit_unqualified_imports = .{},
        .scope_stack = .empty,
    };
}

/// Release all declaration-index storage.
pub fn deinit(self: *DeclIndex) void {
    deinitNameBuckets(AssocValue, self.gpa, &self.assoc_value_decls);
    self.open_scope_decls.deinit(self.gpa);
    self.open_scope_decl_starts.deinit(self.gpa);
    self.scope_names.deinit(self.gpa);
    self.scope_name_decl_ids.deinit(self.gpa);
    self.open_scope_names.deinit(self.gpa);
    self.open_scope_name_starts.deinit(self.gpa);
    self.scopes.deinit(self.gpa);
    self.decls.deinit(self.gpa);
    self.scope_decl_ids.deinit(self.gpa);
    self.decl_by_statement.deinit();
    self.type_dependencies.deinit(self.gpa);
    self.type_dependency_segments.deinit(self.gpa);
    self.type_paths.deinit(self.gpa);
    self.type_path_intern.deinit(self.gpa);
    self.type_path_by_statement.deinit();
    deinitNameBuckets(TypePathIdx, self.gpa, &self.type_decls_by_path);
    self.package_header_modules.deinit(self.gpa);
    self.imports.deinit(self.gpa);
    self.explicit_unqualified_imports.deinit(self.gpa);
    self.scope_stack.deinit(self.gpa);
}

fn deinitNameBuckets(comptime K: type, gpa: std.mem.Allocator, map: *std.AutoHashMapUnmanaged(K, GrowingBucket)) void {
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
    const idx: ScopeIdx = @fromBackingInt(@intCast(self.scopes.items.len));
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
        .value_names = Span.empty(),
        .type_names = Span.empty(),
        .import_alias_names = Span.empty(),
        .region = region,
    });
    try self.open_scope_decl_starts.ensureUnusedCapacity(self.gpa, 1);
    try self.open_scope_name_starts.ensureUnusedCapacity(self.gpa, 1);
    try self.scope_stack.append(self.gpa, idx);
    self.open_scope_decl_starts.appendAssumeCapacity(@intCast(self.open_scope_decls.items.len));
    self.open_scope_name_starts.appendAssumeCapacity(@intCast(self.open_scope_names.items.len));
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
    const run_start = self.open_scope_decl_starts.items[self.open_scope_decl_starts.items.len - 1];
    const start: u32 = @intCast(self.scope_decl_ids.items.len);
    try self.scope_decl_ids.appendSlice(self.gpa, self.open_scope_decls.items[run_start..]);
    const end: u32 = @intCast(self.scope_decl_ids.items.len);
    const names = try self.closeScopeNames();
    self.open_scope_decls.shrinkRetainingCapacity(run_start);
    _ = self.open_scope_decl_starts.pop();
    _ = self.scope_stack.pop();
    var scope = self.scopes.items[@backingInt(idx)];
    scope.decls = .{ .start = start, .len = end - start };
    scope.value_names = names.get(.value);
    scope.type_names = names.get(.type);
    scope.import_alias_names = names.get(.import_alias);
    scope.region = region;
    self.scopes.items[@backingInt(idx)] = scope;
}

/// Move the innermost open scope's recorded names into sorted per-namespace
/// runs of `scope_names` and drop them from the open-name stack.
fn closeScopeNames(self: *DeclIndex) std.mem.Allocator.Error!std.EnumArray(Namespace, Span) {
    const run_start = self.open_scope_name_starts.items[self.open_scope_name_starts.items.len - 1];
    const run = self.open_scope_names.items[run_start..];
    std.sort.pdq(OpenName, run, {}, OpenName.lessThan);

    var spans = std.EnumArray(Namespace, Span).initFill(Span.empty());
    try self.scope_names.ensureUnusedCapacity(self.gpa, run.len);
    try self.scope_name_decl_ids.ensureUnusedCapacity(self.gpa, run.len);
    if (self.scope_name_decl_ids.items.len + run.len > std.math.maxInt(u32) or
        self.scope_names.items.len + run.len > std.math.maxInt(u32)) return error.OutOfMemory;

    var i: usize = 0;
    while (i < run.len) {
        const namespace = run[i].namespace;
        const names_start: u32 = @intCast(self.scope_names.items.len);
        while (i < run.len and run[i].namespace == namespace) {
            const ident = run[i].ident;
            const decls_start: u32 = @intCast(self.scope_name_decl_ids.items.len);
            while (i < run.len and run[i].namespace == namespace and run[i].ident.eql(ident)) : (i += 1) {
                const decl = run[i].decl;
                if (run[i].once and std.mem.findScalar(DeclIdx, self.scope_name_decl_ids.items[decls_start..], decl) != null) continue;
                self.scope_name_decl_ids.appendAssumeCapacity(decl);
            }
            const decls_len: u32 = @intCast(self.scope_name_decl_ids.items.len - decls_start);
            self.scope_names.appendAssumeCapacity(.{ .ident = ident, .decls = .{ .start = decls_start, .len = decls_len } });
        }
        spans.set(namespace, .{ .start = names_start, .len = @as(u32, @intCast(self.scope_names.items.len)) - names_start });
    }

    self.open_scope_names.shrinkRetainingCapacity(run_start);
    _ = self.open_scope_name_starts.pop();
    return spans;
}

/// Record `ident` as naming `decl_idx` in `namespace` of the innermost open scope.
fn addOpenName(self: *DeclIndex, scope_idx: ScopeIdx, namespace: Namespace, ident: Ident.Idx, decl_idx: DeclIdx, once: bool) std.mem.Allocator.Error!void {
    if (self.currentScope() != scope_idx) base.invariant("parser declaration name joined a scope other than the innermost open one", .{});
    const run_start = self.open_scope_name_starts.items[self.open_scope_name_starts.items.len - 1];
    const seq = std.math.cast(u32, self.open_scope_names.items.len - run_start) orelse return error.OutOfMemory;
    try self.open_scope_names.append(self.gpa, .{
        .namespace = namespace,
        .ident = ident,
        .decl = decl_idx,
        .seq = seq,
        .once = once,
    });
}

/// The names a closed scope declares in `namespace`, sorted by identifier.
pub fn scopeNames(self: *const DeclIndex, scope_idx: ScopeIdx, namespace: Namespace) []const ScopeName {
    const scope = &self.scopes.items[@backingInt(scope_idx)];
    const span = switch (namespace) {
        .value => scope.value_names,
        .type => scope.type_names,
        .import_alias => scope.import_alias_names,
    };
    return self.scope_names.items[span.start..][0..span.len];
}

/// The declarations of one of a scope's names.
pub fn scopeNameDecls(self: *const DeclIndex, name: ScopeName) NameBucket {
    return .fromSlice(self.scope_name_decl_ids.items[name.decls.start..][0..name.decls.len]);
}

/// All declarations of `ident` in `namespace` of a closed scope.
pub fn scopeDeclsNamed(self: *const DeclIndex, scope_idx: ScopeIdx, namespace: Namespace, ident: Ident.Idx) NameBucket {
    const names = self.scopeNames(scope_idx, namespace);
    const key = ident.denseIndex();
    var low: usize = 0;
    var high: usize = names.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        const mid_key = names[mid].ident.denseIndex();
        if (mid_key == key) return self.scopeNameDecls(names[mid]);
        if (mid_key < key) low = mid + 1 else high = mid;
    }
    return .{};
}

/// Return the scope currently being parsed, if any.
pub fn currentScope(self: *const DeclIndex) ?ScopeIdx {
    if (self.scope_stack.items.len == 0) return null;
    return self.scope_stack.items[self.scope_stack.items.len - 1];
}

/// Update a scope owner once the owning AST node index is known.
pub fn setScopeOwner(self: *DeclIndex, idx: ScopeIdx, owner: ScopeOwner) void {
    self.scopes.items[@backingInt(idx)].owner = owner;
}

/// Update a scope's associated owner path once known.
pub fn setScopeOwnerTypePath(self: *DeclIndex, idx: ScopeIdx, owner_type_path: ?TypePathIdx) void {
    self.scopes.items[@backingInt(idx)].owner_type_path = owner_type_path;
}

/// Append a declaration to its owning scope.
pub fn addDecl(self: *DeclIndex, decl: Decl) std.mem.Allocator.Error!DeclIdx {
    if (self.currentScope() != decl.scope) base.invariant("parser declaration joined a scope other than the innermost open one", .{});
    const idx: DeclIdx = @fromBackingInt(@intCast(self.decls.items.len));
    try self.decls.append(self.gpa, decl);
    try self.open_scope_decls.append(self.gpa, idx);
    try self.decl_by_statement.put(decl.statement, idx);
    if (decl.name_ident) |ident| {
        if (declKindMayBindValue(decl.kind)) {
            try self.addOpenName(decl.scope, .value, ident, idx, false);
            if (decl.owner_type_path) |owner_path| {
                try addDeclToBucket(AssocValue, self.gpa, &self.assoc_value_decls, .{
                    .owner = owner_path,
                    .item = ident,
                }, idx);
            }
        } else if (declKindMayBindType(decl.kind)) {
            try self.addOpenName(decl.scope, .type, ident, idx, false);
            if (decl.type_path) |path| {
                try self.type_path_by_statement.put(decl.statement, path);
                try addDeclToBucket(TypePathIdx, self.gpa, &self.type_decls_by_path, path, idx);
            }
        }
    }
    return idx;
}

/// Record `ident` as a value that `decl_idx` declares in `scope_idx`. A
/// declaration whose pattern destructures its value binds several names and
/// has no single `name_ident`; each bound name is registered through here so
/// a reference ahead of the declaration resolves to it.
pub fn addValueName(self: *DeclIndex, scope_idx: ScopeIdx, ident: Ident.Idx, decl_idx: DeclIdx) std.mem.Allocator.Error!void {
    // A pattern binding the same name twice registers the declaration once.
    try self.addOpenName(scope_idx, .value, ident, decl_idx, true);
}

fn addDeclToBucket(
    comptime K: type,
    gpa: std.mem.Allocator,
    map: *std.AutoHashMapUnmanaged(K, GrowingBucket),
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
        .where_alias,
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
        .where_alias,
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

/// Return all value-like declarations for a name in a scope.
pub fn scopeValueDecls(self: *const DeclIndex, scope_idx: ScopeIdx, ident: Ident.Idx) NameBucket {
    return self.scopeDeclsNamed(scope_idx, .value, ident);
}

/// Record the module alias introduced by an import declaration.
pub fn addImportAliasDecl(self: *DeclIndex, scope_idx: ScopeIdx, ident: Ident.Idx, decl_idx: DeclIdx) std.mem.Allocator.Error!void {
    try self.addOpenName(scope_idx, .import_alias, ident, decl_idx, false);
}

/// Return all associated value declarations for an owner type path and item name.
pub fn assocValueDecls(self: *const DeclIndex, owner: TypePathIdx, item: Ident.Idx) NameBucket {
    const bucket = self.assoc_value_decls.get(.{ .owner = owner, .item = item }) orelse return .{};
    return bucket.view();
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
        self.type_paths.items[@backingInt(parent_idx)].depth + 1
    else
        1;
    const idx: TypePathIdx = @fromBackingInt(@intCast(self.type_paths.items.len));
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
        const scope_idx: ScopeIdx = @fromBackingInt(@intCast(scope_pos));
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

/// Return all parser type declarations recorded for a type path in source order.
pub fn typeDeclsForPath(self: *const DeclIndex, path: TypePathIdx) NameBucket {
    const bucket = self.type_decls_by_path.get(path) orelse return .{};
    return bucket.view();
}

/// Mark an adjacent annotation and value declaration as one source pair.
pub fn pairAnnotation(self: *DeclIndex, anno_idx: DeclIdx, decl_idx: DeclIdx) void {
    self.decls.items[@backingInt(anno_idx)].paired_decl = decl_idx;
    self.decls.items[@backingInt(decl_idx)].paired_anno = anno_idx;
}

/// Return the direct declarations owned by a scope.
pub fn scopeDecls(self: *const DeclIndex, scope_idx: ScopeIdx) []const DeclIdx {
    const scope = self.scopes.items[@backingInt(scope_idx)];
    return self.scope_decl_ids.items[scope.decls.start..][0..scope.decls.len];
}

/// Return the current type-dependency side-table tops.
pub fn typeDependencyTop(self: *const DeclIndex) TypeDependencyMark {
    return .{
        .dependencies_top = @intCast(self.type_dependencies.items.len),
        .segments_top = @intCast(self.type_dependency_segments.items.len),
    };
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

test "scopeValueDecls records only value-binding declarations" {
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
    const import_decl_idx = try index.addDecl(.{
        .scope = scope_idx,
        .statement = 4,
        .kind = .import,
        .name_tok = null,
        .name_ident = import_ident,
        .pattern = null,
        .anno = null,
        .region = TokenRegion.empty(),
    });
    try index.addImportAliasDecl(scope_idx, import_ident, import_decl_idx);
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

    try std.testing.expect(index.scopeValueDecls(scope_idx, value_ident).count() != 0);
    try std.testing.expect(index.scopeValueDecls(scope_idx, value_anno_ident).count() != 0);
    try std.testing.expect(index.scopeValueDecls(scope_idx, var_decl_ident).count() != 0);
    try std.testing.expect(index.scopeValueDecls(scope_idx, var_anno_ident).count() != 0);
    try std.testing.expect(index.scopeValueDecls(scope_idx, import_ident).count() == 0);
    try std.testing.expect(index.scopeValueDecls(scope_idx, type_ident).count() == 0);
    const import_alias_decls = index.scopeDeclsNamed(scope_idx, .import_alias, import_ident);
    try std.testing.expectEqual(@as(usize, 1), import_alias_decls.count());
    try std.testing.expectEqual(import_decl_idx, import_alias_decls.first.?);
}

test "type dependency spans preserve parser-recorded type references" {
    const gpa = std.testing.allocator;

    var index = DeclIndex.init(gpa);
    defer index.deinit();

    const attrs = Ident.Attributes.fromString("TypeName");
    const first_ident = Ident.Idx{ .attributes = attrs, .idx = 1 };
    const second_ident = Ident.Idx{ .attributes = attrs, .idx = 2 };

    const start = index.typeDependencyTop();
    try index.addTypeDependencySegments(&.{first_ident});
    try index.addTypeDependencySegments(&.{second_ident});
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
    try std.testing.expectEqual(first, index.typeDeclsForPath(path).at(0).?);
}

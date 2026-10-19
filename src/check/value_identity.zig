//! Value identities of a checked module's compile-time roots (design.md
//! "Compile-Time Value Reuse Across Edits").
//!
//! A root's value identity is a digest of every input its evaluation reads,
//! computed from explicit checked data when the module is published. Two
//! roots with equal value identities compute equal values, so a later
//! version of the module reuses a stored value whose identity is unchanged.
//! A root's site names its position within the module independently of node
//! ids, so the roots a reused value read can be found in the later version.
//!
//! A definition identity is the per-definition analogue of a module's content
//! identity: a Merkle digest over the module's name-reference graph, one
//! component of mutually referencing definitions at a time.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");

const checked_artifact = @import("checked_artifact.zig");
const canonical = @import("canonical_names.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const DependencyGraph = can.DependencyGraph;
const Sha256 = base.Sha256;

const CheckedTypeId = checked_artifact.CheckedTypeId;
const CheckedTypeStoreView = checked_artifact.CheckedTypeStoreView;
const CompileTimeRoot = checked_artifact.CompileTimeRoot;

/// The value identity and site of one compile-time root.
pub const CompileTimeRootIdentity = extern struct {
    /// Digest of everything that decides the root's value.
    value: [32]u8,
    /// The root's position-stable name within its module.
    site: [32]u8,
};

/// What the identities of one module's roots are computed from.
pub const Inputs = struct {
    env: *const ModuleEnv,
    names: *const canonical.CanonicalNameStore,
    types: CheckedTypeStoreView,
    roots: []const CompileTimeRoot,
    top_level_values: *const checked_artifact.TopLevelValueTable,
    key: checked_artifact.CheckedModuleArtifactKey,
    /// The module's own content identity, which names its own declarations.
    own_identity: [32]u8,
};

const domain = "roc.compile_time_value_identity.v1";

/// Compute every root's identity, in root order.
pub fn compute(allocator: Allocator, inputs: Inputs) Allocator.Error![]CompileTimeRootIdentity {
    const identities = try allocator.alloc(CompileTimeRootIdentity, inputs.roots.len);
    errdefer allocator.free(identities);
    if (inputs.roots.len == 0) return identities;

    var state = try State.init(allocator, inputs);
    defer state.deinit();
    try state.computeDefinitionIdentities();
    try state.mapExpressionOwners();
    const context = state.moduleContext();
    const type_world = try state.typeWorld();

    var ordinals = std.AutoHashMapUnmanaged([32]u8, u32).empty;
    defer ordinals.deinit(allocator);
    for (inputs.roots, identities) |root, *identity| {
        identity.* = try state.rootIdentity(root, context, type_world, &ordinals);
    }
    return identities;
}

const Owner = union(enum) {
    def: CIR.Def.Idx,
    expect: CIR.Statement.Idx,
};

const State = struct {
    allocator: Allocator,
    inputs: Inputs,
    order: DependencyGraph.CheckOrder,
    pattern_to_def: collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx),
    definition: collections.DenseMap(CIR.Def.Idx, [32]u8),
    owners: collections.DenseMap(CIR.Expr.Idx, Owner),
    refs: std.AutoHashMapUnmanaged(CIR.Def.Idx, void) = .empty,
    expr_stack: std.ArrayList(CIR.Expr.Idx) = .empty,
    seen_defaults: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void) = .empty,
    digests: std.ArrayList([32]u8) = .empty,
    type_walk: TypeDigest,

    fn init(allocator: Allocator, inputs: Inputs) Allocator.Error!State {
        const env = inputs.env;
        var order = try DependencyGraph.computeCheckOrder(env, env.all_defs, allocator);
        errdefer order.deinit();
        var state = State{
            .allocator = allocator,
            .inputs = inputs,
            .order = order,
            .pattern_to_def = collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx).init(allocator),
            .definition = collections.DenseMap(CIR.Def.Idx, [32]u8).init(allocator),
            .owners = collections.DenseMap(CIR.Expr.Idx, Owner).init(allocator),
            .type_walk = TypeDigest.init(allocator, inputs),
        };
        errdefer state.deinitMaps();
        var binders = std.ArrayList(CIR.Pattern.Idx).empty;
        defer binders.deinit(allocator);
        var scratch = std.ArrayList(CIR.Pattern.Idx).empty;
        defer scratch.deinit(allocator);
        for (env.store.sliceDefs(env.all_defs)) |def_idx| {
            binders.clearRetainingCapacity();
            try DependencyGraph.appendPatternBinders(env, env.store.getDef(def_idx).pattern, &binders, &scratch, allocator);
            for (binders.items) |binder| try state.pattern_to_def.put(binder, def_idx);
        }
        for (env.store.sliceStatements(env.all_statements)) |statement| try state.type_walk.top_level.put(statement, {});
        return state;
    }

    fn deinitMaps(self: *State) void {
        self.pattern_to_def.deinit();
        self.definition.deinit();
        self.owners.deinit();
        self.refs.deinit(self.allocator);
        self.expr_stack.deinit(self.allocator);
        self.seen_defaults.deinit(self.allocator);
        self.digests.deinit(self.allocator);
        self.type_walk.deinit();
    }

    fn deinit(self: *State) void {
        self.deinitMaps();
        self.order.deinit();
    }

    fn source(self: *const State) []const u8 {
        return self.inputs.env.getSourceAll();
    }

    fn regionText(self: *const State, region: base.Region) []const u8 {
        return self.source()[region.start.offset..region.end.offset];
    }

    /// One definition's own contribution: its source bytes and the type of
    /// its published scheme.
    fn localDigest(self: *State, def_idx: CIR.Def.Idx) Allocator.Error![32]u8 {
        const env = self.inputs.env;
        const def = env.store.getDef(def_idx);
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "def");
        hashBytes(&hasher, self.regionText(env.store.getPatternRegion(def.pattern)));
        if (def.annotation) |annotation| {
            hashTag(&hasher, "annotation");
            hashBytes(&hasher, self.regionText(env.store.getAnnotationRegion(annotation)));
        } else hashTag(&hasher, "no-annotation");
        hashBytes(&hasher, self.regionText(env.store.getExprRegion(def.expr)));
        const scheme_root: ?CheckedTypeId = if (self.inputs.top_level_values.lookupByDef(def_idx)) |entry|
            if (self.inputs.types.schemeForKey(entry.source_scheme)) |scheme| scheme.root else null
        else
            null;
        if (scheme_root) |root| {
            hashTag(&hasher, "scheme");
            hasher.update(&try self.type_walk.digest(root));
        } else hashTag(&hasher, "no-scheme");
        return hasher.finalResult();
    }

    /// Definition identities, one strongly connected component at a time in
    /// dependency order: each folds its members' own contributions and the
    /// identities of the components they name.
    fn computeDefinitionIdentities(self: *State) Allocator.Error!void {
        const group_identities = try self.allocator.alloc([32]u8, self.order.sccs.len);
        defer self.allocator.free(group_identities);
        var locals = std.ArrayList([32]u8).empty;
        defer locals.deinit(self.allocator);
        var deps = std.ArrayList([32]u8).empty;
        defer deps.deinit(self.allocator);
        for (self.order.sccs, 0..) |scc, group_index| {
            locals.clearRetainingCapacity();
            for (scc.defs) |def_idx| try locals.append(self.allocator, try self.localDigest(def_idx));
            const member_locals = try self.allocator.dupe([32]u8, locals.items);
            defer self.allocator.free(member_locals);
            sortDigests(locals.items);
            deps.clearRetainingCapacity();
            for (self.order.dependenciesOf(@intCast(group_index))) |dependency| try deps.append(self.allocator, group_identities[dependency]);
            sortDigests(deps.items);
            var hasher = Sha256.init(.{});
            hashTag(&hasher, "component");
            hashDigests(&hasher, locals.items);
            hashDigests(&hasher, deps.items);
            group_identities[group_index] = hasher.finalResult();
            for (scc.defs, member_locals) |def_idx, local| {
                var member = Sha256.init(.{});
                hashTag(&member, "definition");
                member.update(&local);
                member.update(&group_identities[group_index]);
                try self.definition.put(def_idx, member.finalResult());
            }
        }
    }

    fn definitionIdentity(self: *const State, def_idx: CIR.Def.Idx) [32]u8 {
        return self.definition.get(def_idx) orelse
            base.invariant("compile-time value identity asked for a definition outside the module's definitions", .{});
    }

    /// A method's identity. A method the module's definition graph does not
    /// contain (a compiler-created method binding) has no component; its own
    /// contribution and the definitions its body names identify it.
    fn methodIdentity(self: *State, def_idx: CIR.Def.Idx) Allocator.Error![32]u8 {
        if (self.definition.get(def_idx)) |identity| return identity;
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "method-outside-definitions");
        hasher.update(&try self.localDigest(def_idx));
        hashDigests(&hasher, try self.referencedIdentities(self.inputs.env.store.getDef(def_idx).expr));
        return hasher.finalResult();
    }

    /// The sorted identities of the definitions `expr` names.
    fn referencedIdentities(self: *State, expr: CIR.Expr.Idx) Allocator.Error![]const [32]u8 {
        self.refs.clearRetainingCapacity();
        try DependencyGraph.collectNameReferences(self.inputs.env, &self.pattern_to_def, expr, &self.refs, &self.expr_stack, &self.seen_defaults, self.allocator);
        self.digests.clearRetainingCapacity();
        var it = self.refs.keyIterator();
        while (it.next()) |def_idx| try self.digests.append(self.allocator, self.definitionIdentity(def_idx.*));
        sortDigests(self.digests.items);
        return self.digests.items;
    }

    /// Map every expression of every definition and top-level `expect` to
    /// the declaration that contains it.
    fn mapExpressionOwners(self: *State) Allocator.Error!void {
        const env = self.inputs.env;
        for (env.store.sliceDefs(env.all_defs)) |def_idx| {
            try self.mapOwner(env.store.getDef(def_idx).expr, .{ .def = def_idx });
        }
        for (env.store.sliceStatements(env.all_statements)) |statement_idx| {
            switch (env.store.getStatement(statement_idx)) {
                .s_expect => |expect| try self.mapOwner(expect.body, .{ .expect = statement_idx }),
                .s_decl,
                .s_var,
                .s_var_uninitialized,
                .s_reassign,
                .s_crash,
                .s_dbg,
                .s_expr,
                .s_for,
                .s_while,
                .s_infinite_loop,
                .s_breakable_loop,
                .s_break,
                .s_return,
                .s_import,
                .s_alias_decl,
                .s_nominal_decl,
                .s_where_alias_decl,
                .s_type_anno,
                .s_type_var_alias,
                .s_runtime_error,
                => {},
            }
        }
    }

    fn mapOwner(self: *State, root: CIR.Expr.Idx, owner: Owner) Allocator.Error!void {
        var stack = std.ArrayList(CIR.Expr.Idx).empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, root);
        while (stack.pop()) |expr| {
            const entry = try self.owners.getOrPut(expr);
            if (entry.found_existing) continue;
            entry.value_ptr.* = owner;
            try appendExprChildren(self.inputs.env, expr, &stack, self.allocator);
        }
    }

    /// Everything outside any one root that its value can depend on: the
    /// compiler, the module's checking context and imports, its name and
    /// kind, and the files it imports.
    fn moduleContext(self: *const State) [32]u8 {
        const env = self.inputs.env;
        const key = self.inputs.key;
        var hasher = Sha256.init(.{});
        hashTag(&hasher, domain);
        hashTag(&hasher, "context");
        hasher.update(&key.compiler_artifact_hash);
        hasher.update(&key.checking_context_identity_hash);
        hasher.update(&key.direct_import_artifact_keys_hash);
        hashBytes(&hasher, env.module_name);
        hashBytes(&hasher, @tagName(env.module_kind));
        switch (env.module_kind) {
            .type_module => |ident| hashBytes(&hasher, env.getIdent(ident)),
            .default_app, .app, .package, .platform, .hosted, .module, .malformed => {},
        }
        const deps = env.file_dependencies.items.items;
        hashU32(&hasher, @intCast(deps.len));
        for (deps) |dep| {
            hashBytes(&hasher, env.fileDependencyRelativePath(dep));
            hashBytes(&hasher, @tagName(dep.state));
            switch (dep.state) {
                .present => hasher.update(&dep.content_hash),
                .missing, .unreadable => {},
                .pending => base.invariant("compile-time value identity read an unsettled file import", .{}),
            }
        }
        return hasher.finalResult();
    }

    /// The module's type world: the source of every top-level statement that
    /// is not a value or `expect`, every type declaration, and the identity
    /// of every method the module registers.
    fn typeWorld(self: *State) Allocator.Error![32]u8 {
        const env = self.inputs.env;
        var texts = std.ArrayList([32]u8).empty;
        defer texts.deinit(self.allocator);
        var seen = collections.DenseMap(CIR.Statement.Idx, void).init(self.allocator);
        defer seen.deinit();
        for ([_][]const CIR.Statement.Idx{ env.store.sliceStatements(env.all_statements), env.store.sliceStatements(env.type_decls) }) |statements| {
            for (statements) |statement_idx| {
                const entry = try seen.getOrPut(statement_idx);
                if (entry.found_existing) continue;
                const include = switch (env.store.getStatement(statement_idx)) {
                    .s_import, .s_alias_decl, .s_nominal_decl, .s_where_alias_decl, .s_type_var_alias, .s_runtime_error => true,
                    .s_decl,
                    .s_var,
                    .s_var_uninitialized,
                    .s_reassign,
                    .s_crash,
                    .s_dbg,
                    .s_expr,
                    .s_expect,
                    .s_for,
                    .s_while,
                    .s_infinite_loop,
                    .s_breakable_loop,
                    .s_break,
                    .s_return,
                    .s_type_anno,
                    => false,
                };
                if (!include) continue;
                try texts.append(self.allocator, digestBytes("statement", self.regionText(env.store.getStatementRegion(statement_idx))));
            }
        }
        sortDigests(texts.items);
        var methods = std.ArrayList([32]u8).empty;
        defer methods.deinit(self.allocator);
        for (env.method_defs.entries.items) |entry| try methods.append(self.allocator, try self.methodIdentity(entry.value.def_idx));
        sortDigests(methods.items);
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "type-world");
        hashDigests(&hasher, texts.items);
        hashDigests(&hasher, methods.items);
        return hasher.finalResult();
    }

    fn defName(self: *const State, def_idx: CIR.Def.Idx) []const u8 {
        const env = self.inputs.env;
        const pattern = env.store.getDef(def_idx).pattern;
        const data = env.store.getPattern(pattern);
        if (data == .assign) return env.getIdent(data.assign.ident);
        return self.regionText(env.store.getPatternRegion(pattern));
    }

    fn ownerKey(self: *const State, owner: ?Owner) [32]u8 {
        const env = self.inputs.env;
        const resolved = owner orelse return digestBytes("owner.none", "");
        return switch (resolved) {
            .def => |def_idx| digestBytes("owner.def", self.defName(def_idx)),
            .expect => |statement| digestBytes("owner.expect", self.regionText(env.store.getStatementRegion(statement))),
        };
    }

    fn nextOrdinal(self: *State, ordinals: *std.AutoHashMapUnmanaged([32]u8, u32), class: []const u8, owner: [32]u8) Allocator.Error!u32 {
        var hasher = Sha256.init(.{});
        hashTag(&hasher, class);
        hasher.update(&owner);
        const entry = try ordinals.getOrPut(self.allocator, hasher.finalResult());
        if (!entry.found_existing) entry.value_ptr.* = 0;
        const ordinal = entry.value_ptr.*;
        entry.value_ptr.* += 1;
        return ordinal;
    }

    fn rootIdentity(
        self: *State,
        root: CompileTimeRoot,
        context: [32]u8,
        type_world: [32]u8,
        ordinals: *std.AutoHashMapUnmanaged([32]u8, u32),
    ) Allocator.Error!CompileTimeRootIdentity {
        const env = self.inputs.env;
        var value = Sha256.init(.{});
        hashTag(&value, domain);
        value.update(&context);
        value.update(&type_world);
        hashBytes(&value, @tagName(root.kind));
        var site = Sha256.init(.{});
        hashTag(&site, "roc.compile_time_root_site.v1");
        hashBytes(&site, @tagName(root.kind));
        switch (root.source) {
            .def => |def_idx| {
                value.update(&self.definitionIdentity(def_idx));
                hashBytes(&site, self.defName(def_idx));
            },
            .statement => |statement| {
                const text = self.regionText(env.store.getStatementRegion(statement));
                hashBytes(&value, text);
                hashDigests(&value, try self.referencedIdentities(expectBody(env, statement)));
                hashBytes(&site, text);
            },
            .hoisted => |hoisted| {
                const owner = self.owners.get(hoisted.expr);
                const owner_key = self.ownerKey(owner);
                const ordinal = try self.nextOrdinal(ordinals, "hoisted", owner_key);
                if (owner) |resolved| switch (resolved) {
                    .def => |def_idx| value.update(&self.definitionIdentity(def_idx)),
                    .expect => |statement| {
                        hashBytes(&value, self.regionText(env.store.getStatementRegion(statement)));
                        hashDigests(&value, try self.referencedIdentities(expectBody(env, statement)));
                    },
                } else hashTag(&value, "unowned");
                hashU32(&value, ordinal);
                hashBytes(&value, if (root.hoisted_body) |body| @tagName(body) else "no-body");
                hashBytes(&value, self.regionText(env.store.getExprRegion(hoisted.expr)));
                hashDigests(&value, try self.referencedIdentities(hoisted.expr));
                site.update(&owner_key);
                hashU32(&site, ordinal);
            },
            .expr => |expr| switch (root.kind) {
                .numeral_conversion, .quote_conversion => {
                    const owner_key = self.ownerKey(self.owners.get(expr));
                    const ordinal = try self.nextOrdinal(ordinals, "literal", owner_key);
                    hashBytes(&value, self.regionText(env.store.getExprRegion(expr)));
                    site.update(&owner_key);
                    hashU32(&site, ordinal);
                },
                .repl_expr => {
                    hashBytes(&value, self.regionText(env.store.getExprRegion(expr)));
                    hashU32(&site, @backingInt(expr));
                },
                .constant, .hoisted_constant, .hoisted_validation, .callable_binding, .expect => base.invariant("compile-time root kind does not take an expression source", .{}),
            },
            .required_binding => |binding| {
                hashU32(&value, binding);
                hashU32(&site, binding);
            },
        }
        hashTag(&value, "type");
        value.update(&try self.type_walk.digest(root.checked_type));
        return .{ .value = value.finalResult(), .site = site.finalResult() };
    }
};

/// Digests a checked type graph in one canonical pre-order: each node is
/// written on first visit with its child count, and a revisit writes the
/// node's first-visit position, so cyclic and shared graphs digest exactly.
/// The module's own nominal types are named by declaration rather than by
/// the module's content identity.
const TypeDigest = struct {
    allocator: Allocator,
    inputs: Inputs,
    visited: collections.DenseMap(CheckedTypeId, u32),
    /// The module's top-level statements, whose declarations the type world
    /// covers by source.
    top_level: collections.DenseMap(CIR.Statement.Idx, void),
    stack: std.ArrayList(CheckedTypeId) = .empty,

    fn init(allocator: Allocator, inputs: Inputs) TypeDigest {
        return .{
            .allocator = allocator,
            .inputs = inputs,
            .visited = collections.DenseMap(CheckedTypeId, u32).init(allocator),
            .top_level = collections.DenseMap(CIR.Statement.Idx, void).init(allocator),
        };
    }

    fn deinit(self: *TypeDigest) void {
        self.visited.deinit();
        self.top_level.deinit();
        self.stack.deinit(self.allocator);
    }

    fn digest(self: *TypeDigest, root: CheckedTypeId) Allocator.Error![32]u8 {
        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "checked-type");
        try self.stack.append(self.allocator, root);
        var next_position: u32 = 0;
        // Children are pushed in reverse so they pop, and are written, in order.
        var children = std.ArrayList(CheckedTypeId).empty;
        defer children.deinit(self.allocator);
        while (self.stack.pop()) |ty| {
            if (self.visited.get(ty)) |position| {
                hashTag(&hasher, "seen");
                hashU32(&hasher, position);
                continue;
            }
            try self.visited.put(ty, next_position);
            next_position += 1;
            children.clearRetainingCapacity();
            try self.writeNode(&hasher, ty, &children);
            hashU32(&hasher, @intCast(children.items.len));
            var index = children.items.len;
            while (index > 0) {
                index -= 1;
                try self.stack.append(self.allocator, children.items[index]);
            }
        }
        return hasher.finalResult();
    }

    fn isOwn(self: *const TypeDigest, origin: canonical.ModuleIdentityId) bool {
        return std.mem.eql(u8, self.inputs.names.moduleIdentityBytes(origin), &self.inputs.own_identity);
    }

    /// Names a declaration of the module: a top-level one by its name and
    /// source, any other by version-specific data.
    fn writeOwnDeclaration(self: *const TypeDigest, hasher: *Sha256, name: []const u8, source_decl: ?u32) void {
        const env = self.inputs.env;
        if (source_decl) |raw| {
            const statement: CIR.Statement.Idx = @fromBackingInt(raw);
            if (self.top_level.contains(statement)) {
                hashTag(hasher, "own-top-level");
                hashBytes(hasher, name);
                const region = env.store.getStatementRegion(statement);
                hashBytes(hasher, env.getSourceAll()[region.start.offset..region.end.offset]);
                return;
            }
        }
        hashTag(hasher, "own-version");
        hasher.update(&self.inputs.own_identity);
        hashBytes(hasher, name);
        hashU32(hasher, source_decl orelse std.math.maxInt(u32));
    }

    fn writeNode(self: *TypeDigest, hasher: *Sha256, ty: CheckedTypeId, children: *std.ArrayList(CheckedTypeId)) Allocator.Error!void {
        const names = self.inputs.names;
        switch (self.inputs.types.payload(ty)) {
            .pending => hashTag(hasher, "pending"),
            .err => hashTag(hasher, "err"),
            .empty_record => hashTag(hasher, "empty-record"),
            .empty_tag_union => hashTag(hasher, "empty-tag-union"),
            inline .flex, .rigid => |variable, tag| {
                hashTag(hasher, @tagName(tag));
                hashBytes(hasher, variable.name orelse "");
                hashValue(hasher, variable.numeric_default_phase);
                hashValue(hasher, variable.row_default);
                hashValue(hasher, variable.row_default_discharges_constraints);
                hashU32(hasher, @intCast(variable.constraints.len));
                for (variable.constraints) |constraint| {
                    hashBytes(hasher, names.methodNameText(constraint.fn_name));
                    hashValue(hasher, constraint.origin);
                    try children.append(self.allocator, constraint.fn_ty);
                }
            },
            .alias => |alias| {
                hashTag(hasher, "alias");
                const name = names.typeNameText(alias.name);
                if (self.isOwn(alias.origin_module)) {
                    self.writeOwnDeclaration(hasher, name, alias.source_decl);
                } else {
                    hasher.update(names.moduleIdentityBytes(alias.origin_module));
                    hashBytes(hasher, name);
                    hashU32(hasher, alias.source_decl orelse std.math.maxInt(u32));
                }
                hashValue(hasher, alias.builtin_origin);
                try children.appendSlice(self.allocator, alias.args);
                try children.append(self.allocator, alias.backing);
            },
            .nominal => |nominal| {
                hashTag(hasher, "nominal");
                const name = names.typeNameText(nominal.name);
                if (self.isOwn(nominal.origin_module)) {
                    self.writeOwnDeclaration(hasher, name, nominal.source_decl);
                } else {
                    hasher.update(names.moduleIdentityBytes(nominal.origin_module));
                    hashBytes(hasher, name);
                    hashU32(hasher, nominal.source_decl);
                }
                hashValue(hasher, nominal.builtin);
                hashValue(hasher, nominal.is_opaque);
                hashBytes(hasher, @tagName(nominal.representation));
                switch (nominal.representation) {
                    .builtin => |builtin_nominal| hashBytes(hasher, @tagName(builtin_nominal)),
                    .imported_declaration => |ref| {
                        hasher.update(&ref.artifact.bytes);
                        hashU32(hasher, @backingInt(ref.declaration));
                    },
                    .imported_box_payload_capability => |ref| {
                        hasher.update(&ref.artifact.bytes);
                        hashU32(hasher, @backingInt(ref.capability));
                    },
                    // Own declarations are named above by their source.
                    .local_declaration, .local_box_payload_capability, .opaque_without_backing => {},
                }
                hashU32(hasher, @intCast(nominal.declared_fields.len));
                for (nominal.declared_fields) |field| switch (field) {
                    .named => |label| {
                        hashTag(hasher, "named");
                        hashBytes(hasher, names.recordFieldLabelText(label));
                    },
                    .padding => |slot| {
                        hashTag(hasher, "padding");
                        hashU32(hasher, slot);
                    },
                };
                hashU32(hasher, @intCast(nominal.args.len));
                try children.appendSlice(self.allocator, nominal.args);
                try children.appendSlice(self.allocator, nominal.padding_field_types);
            },
            .record => |record| {
                hashTag(hasher, "record");
                hashU32(hasher, @intCast(record.fields.len));
                for (record.fields) |field| {
                    hashBytes(hasher, names.recordFieldLabelText(field.name));
                    hashBytes(hasher, @tagName(field.kind.tag));
                    if (field.kind.defaultIdentity()) |default| {
                        if (default.origin()) |origin| {
                            if (self.isOwn(origin)) {
                                hashTag(hasher, "own-default");
                                hasher.update(&self.inputs.own_identity);
                            } else {
                                hasher.update(names.moduleIdentityBytes(origin));
                            }
                        }
                        hashU32(hasher, default.expr_node);
                    }
                    try children.append(self.allocator, field.ty);
                    if (field.kind.undeterminedVariable()) |variable| try children.append(self.allocator, variable);
                }
                try children.append(self.allocator, record.ext);
            },
            .tuple => |items| {
                hashTag(hasher, "tuple");
                try children.appendSlice(self.allocator, items);
            },
            .function => |function| {
                hashTag(hasher, "function");
                hashBytes(hasher, @tagName(function.kind));
                hashU32(hasher, @intCast(function.args.len));
                try children.appendSlice(self.allocator, function.args);
                try children.append(self.allocator, function.ret);
            },
            .tag_union => |tag_union| {
                hashTag(hasher, "tag-union");
                hashU32(hasher, @intCast(tag_union.tags.len));
                for (tag_union.tags) |tag| {
                    const args = tag.argsSlice(self.inputs.types);
                    hashBytes(hasher, names.tagLabelText(tag.name));
                    hashU32(hasher, @intCast(args.len));
                    try children.appendSlice(self.allocator, args);
                }
                try children.append(self.allocator, tag_union.ext);
            },
        }
    }
};

fn expectBody(env: *const ModuleEnv, statement: CIR.Statement.Idx) CIR.Expr.Idx {
    const data = env.store.getStatement(statement);
    if (data != .s_expect) base.invariant("compile-time statement root was not an expect", .{});
    return data.s_expect.body;
}

/// Push every direct subexpression of `expr`.
fn appendExprChildren(env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, stack: *std.ArrayList(CIR.Expr.Idx), allocator: Allocator) Allocator.Error!void {
    const store = &env.store;
    switch (store.getExpr(expr_idx)) {
        .e_type_method_call => |call| try stack.appendSlice(allocator, store.sliceExpr(call.args)),
        .e_type_dispatch_call => |call| try stack.appendSlice(allocator, store.sliceExpr(call.args)),
        .e_lambda => |lambda| try stack.append(allocator, lambda.body),
        .e_closure => |closure| try stack.append(allocator, closure.lambda_idx),
        .e_call => |call| {
            try stack.append(allocator, call.func);
            try stack.appendSlice(allocator, store.sliceExpr(call.args));
        },
        .e_if => |if_expr| {
            try stack.append(allocator, if_expr.final_else);
            for (store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                const branch = store.getIfBranch(branch_idx);
                try stack.append(allocator, branch.cond);
                try stack.append(allocator, branch.body);
            }
        },
        .e_match => |match_expr| {
            try stack.append(allocator, match_expr.cond);
            for (store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                const branch = store.getMatchBranch(branch_idx);
                if (branch.guard) |guard_idx| try stack.append(allocator, guard_idx);
                try stack.append(allocator, branch.value);
            }
        },
        .e_list => |list| try stack.appendSlice(allocator, store.sliceExpr(list.elems)),
        .e_record => |record| {
            if (record.ext) |ext_idx| try stack.append(allocator, ext_idx);
            for (store.sliceRecordFields(record.fields)) |field_idx| try stack.append(allocator, store.getRecordField(field_idx).value);
        },
        .e_field_access => |access| try stack.append(allocator, access.receiver),
        inline .e_method_call, .e_dispatch_call => |call| {
            try stack.append(allocator, call.receiver);
            try stack.appendSlice(allocator, store.sliceExpr(call.args));
        },
        .e_interpolation => |interpolation| {
            try stack.append(allocator, interpolation.first);
            try stack.appendSlice(allocator, store.sliceExpr(interpolation.parts));
        },
        .e_structural_eq => |eq| {
            try stack.append(allocator, eq.lhs);
            try stack.append(allocator, eq.rhs);
        },
        .e_structural_hash => |h| {
            try stack.append(allocator, h.hasher);
            try stack.append(allocator, h.value);
        },
        .e_method_eq => |eq| {
            try stack.append(allocator, eq.lhs);
            try stack.append(allocator, eq.rhs);
        },
        .e_tuple_access => |tuple_access| try stack.append(allocator, tuple_access.tuple),
        .e_tuple => |tuple| try stack.appendSlice(allocator, store.sliceExpr(tuple.elems)),
        .e_binop => |binop| {
            try stack.append(allocator, binop.lhs);
            try stack.append(allocator, binop.rhs);
        },
        .e_unary_minus => |unop| try stack.append(allocator, unop.expr),
        .e_block => |block| {
            for (store.sliceStatements(block.stmts)) |stmt_idx| {
                switch (store.getStatement(stmt_idx)) {
                    .s_decl => |decl| try stack.append(allocator, decl.expr),
                    .s_var => |var_stmt| try stack.append(allocator, var_stmt.expr),
                    .s_var_uninitialized => {},
                    inline .s_reassign, .s_dbg, .s_expr => |statement| try stack.append(allocator, statement.expr),
                    .s_expect => |expect| try stack.append(allocator, expect.body),
                    .s_for => |for_stmt| {
                        try stack.append(allocator, for_stmt.expr);
                        try stack.append(allocator, for_stmt.body);
                    },
                    .s_while => |while_stmt| {
                        try stack.append(allocator, while_stmt.cond);
                        try stack.append(allocator, while_stmt.body);
                    },
                    .s_infinite_loop => |loop_stmt| {
                        try stack.append(allocator, loop_stmt.cond);
                        try stack.append(allocator, loop_stmt.body);
                    },
                    .s_breakable_loop => |loop_stmt| {
                        try stack.append(allocator, loop_stmt.cond);
                        try stack.append(allocator, loop_stmt.body);
                    },
                    .s_return => |ret| try stack.append(allocator, ret.expr),
                    .s_runtime_error => |runtime_error| try stack.appendSlice(allocator, store.sliceExpr(runtime_error.evaluated)),
                    .s_import, .s_alias_decl, .s_nominal_decl, .s_where_alias_decl, .s_type_anno, .s_type_var_alias, .s_crash, .s_break => {},
                }
            }
            try stack.append(allocator, block.final_expr);
        },
        .e_tag => |tag| try stack.appendSlice(allocator, store.sliceExpr(tag.args)),
        .e_nominal => |nominal| try stack.append(allocator, nominal.backing_expr),
        .e_nominal_external => |nominal| try stack.append(allocator, nominal.backing_expr),
        .e_deferred_import_ref => |deferred| {
            if (deferred.backing) |backing| try stack.append(allocator, backing.expr);
        },
        .e_run_low_level => |run_ll| try stack.appendSlice(allocator, store.sliceExpr(run_ll.args)),
        .e_dbg => |dbg| try stack.append(allocator, dbg.expr),
        .e_expect_err => |expect_err| try stack.append(allocator, expect_err.expr),
        .e_expect => |expect| try stack.append(allocator, expect.body),
        // `ret.lambda` refers back to the enclosing lambda; it is no child.
        .e_return => |ret| try stack.append(allocator, ret.expr),
        .e_for => |for_expr| {
            try stack.append(allocator, for_expr.expr);
            try stack.append(allocator, for_expr.body);
        },
        .e_runtime_error => |runtime_error| try stack.appendSlice(allocator, store.sliceExpr(runtime_error.evaluated)),
        .e_lookup_local,
        .e_break,
        .e_num,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_num_from_numeral,
        .e_typed_int,
        .e_typed_frac,
        .e_typed_num_from_numeral,
        .e_str,
        .e_str_segment,
        .e_bytes_literal,
        .e_empty_list,
        .e_empty_record,
        .e_zero_argument_tag,
        .e_ellipsis,
        .e_anno_only,
        .e_derived_method,
        .e_hosted_lambda,
        .e_lookup_external,
        .e_lookup_associated_local,
        .e_lookup_associated,
        .e_lookup_associated_resolved,
        .e_lookup_required,
        .e_crash,
        => {},
    }
}

fn hashTag(hasher: *Sha256, tag: []const u8) void {
    hashBytes(hasher, tag);
}

fn hashU32(hasher: *Sha256, value: u32) void {
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .little);
    hasher.update(&bytes);
}

fn hashBytes(hasher: *Sha256, bytes: []const u8) void {
    hashU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn hashDigests(hasher: *Sha256, digests: []const [32]u8) void {
    hashU32(hasher, @intCast(digests.len));
    for (digests) |digest| hasher.update(&digest);
}

fn digestBytes(tag: []const u8, bytes: []const u8) [32]u8 {
    var hasher = Sha256.init(.{});
    hashTag(&hasher, tag);
    hashBytes(&hasher, bytes);
    return hasher.finalResult();
}

fn sortDigests(digests: [][32]u8) void {
    std.mem.sort([32]u8, digests, {}, struct {
        fn lessThan(_: void, a: [32]u8, b: [32]u8) bool {
            return std.mem.order(u8, &a, &b) == .lt;
        }
    }.lessThan);
}

/// Hash a plain value by its logical contents, field by field, so padding
/// never reaches the digest. Source regions are positions, not identity.
fn hashValue(hasher: *Sha256, value: anytype) void {
    const T = @TypeOf(value);
    if (T == base.Region) return;
    switch (@typeInfo(T)) {
        .bool => hasher.update(&.{@intFromBool(value)}),
        .int => |info| {
            const Wide = @Int(.unsigned, ((info.bits + 7) / 8) * 8);
            var bytes: [@sizeOf(Wide)]u8 = undefined;
            std.mem.writeInt(Wide, &bytes, @bitCast(@as(@Int(info.signedness, @bitSizeOf(Wide)), value)), .little);
            hasher.update(&bytes);
        },
        .@"enum" => hashBytes(hasher, @tagName(value)),
        .array => for (value) |item| hashValue(hasher, item),
        .optional => if (value) |inner| {
            hasher.update(&.{1});
            hashValue(hasher, inner);
        } else hasher.update(&.{0}),
        .@"struct" => |info| {
            if (info.backing_integer) |Backing| {
                hashValue(hasher, @as(Backing, @bitCast(value)));
                return;
            }
            inline for (info.field_names) |name| hashValue(hasher, @field(value, name));
        },
        .@"union" => |info| {
            hashBytes(hasher, @tagName(value));
            inline for (info.field_names) |name| {
                if (std.mem.eql(u8, @tagName(value), name)) hashValue(hasher, @field(value, name));
            }
        },
        .void => {},
        .type,
        .noreturn,
        .float,
        .pointer,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        .spirv,
        => @compileError("value identity cannot hash " ++ @typeName(T)),
    }
}

//! Resolution of a module's deferred references into its imported modules.
//!
//! Canonicalization is a pure function of the module's own source, so it never
//! reads another user module's environment. Every reference through an import
//! is emitted as a placeholder node plus an entry on the env's deferred import
//! worklist (`ModuleEnv.deferred_import_refs`). This module drains that
//! worklist once, after the module's direct imports have completed and before
//! type checking, rewriting each recorded node in place into its resolved form
//! or into a node carrying the diagnostic the reference earned.
//!
//! The worklist is the only record of this work. Nothing here walks the CIR to
//! discover what needs resolving, and no separate pipeline pass exists for it.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const CIR = @import("CIR.zig");
const ModuleEnv = @import("ModuleEnv.zig");
const CoreCtx = @import("ctx").CoreCtx;

const Ident = base.Ident;
const Region = base.Region;
const Statement = CIR.Statement;
const Diagnostic = CIR.Diagnostic;
const DeferredImportRef = ModuleEnv.DeferredImportRef;

/// One of the draining module's imports, named by the exact spelling the
/// source wrote (`Resource`, `pf.Stdout`), with the outcome import resolution
/// selected for it.
pub const ResolvedImport = struct {
    /// The exact source spelling of the import.
    import_name: []const u8,
    /// Exactly one resolution outcome for this import identity.
    resolution: Resolution,

    /// The outcome import resolution selected for one import identity.
    pub const Resolution = union(enum) {
        /// The import names a module, whose environment is complete.
        available: Available,
        /// Import resolution rejected this import: it names no module at all.
        /// Every use of it is checked-error data.
        rejected,
    };

    /// An accepted import target.
    pub const Available = struct {
        module_env: *const ModuleEnv,
        /// The declaration this import makes public, when the target package
        /// exposes a nested type rather than the whole module. Null means the
        /// import names the module itself.
        selected_type_decl: ?Statement.Idx = null,
    };
};

/// How the drain learns each import's outcome.
pub const Imports = union(enum) {
    /// Each import's outcome exactly as import resolution selected it.
    explicit: []const ResolvedImport,
    /// Take each import's outcome from the import store's own resolution
    /// against these environments, which
    /// `CIR.Import.Store.resolveImportsByExactModuleName` has already
    /// computed. An import the store left unresolved names no module.
    resolved_store: []const *const ModuleEnv,
};

/// How `import "path" as name` file imports are read.
pub const FileImports = union(enum) {
    /// Read each imported file, resolving its path against `source_dir`.
    read: struct {
        ctx: CoreCtx,
        source_dir: ?[]const u8 = null,
    },
    /// Do not read file contents. Tools that canonicalize for inspection use
    /// this, and each file import records as unreadable.
    skip,
};

/// Everything the drain reads besides the module's own environment.
pub const Inputs = struct {
    /// Each of the module's imports with the outcome resolution selected.
    imports: Imports,
    /// Every module environment reachable through the imports' own imports.
    /// A path through an exposed alias lands in the module the alias names,
    /// which the draining module need not import itself; the checker receives
    /// those same modules as owner modules.
    reachable_envs: []const *const ModuleEnv = &.{},
};

/// Read the `import "path" as name` file imports on the module's deferred
/// import worklist.
///
/// A file import depends on no other module: it is filesystem input named by
/// the module's own source. It is therefore settled in the canonicalize task,
/// before the module's source-input identity is used to key the checked-module
/// cache. This drains exactly the worklist's `.file_import` entries and leaves
/// every other entry to `resolveDeferredImports`; the worklist stays the one
/// record of deferred work, and nothing here walks the CIR.
pub fn resolveDeferredFileImports(
    env: *ModuleEnv,
    file_imports: FileImports,
) std.mem.Allocator.Error!void {
    if (env.deferred_import_refs.len() == 0) return;

    // Diagnostics canonicalization deliberately left unpublished stay that
    // way; only the ones this drain records are published below.
    const unpublished_before = env.unpublishedDiagnosticCount();

    var resolver = try Resolver.init(env, .{ .imports = .{ .explicit = &.{} } });
    defer resolver.deinit();

    var index: u32 = 0;
    while (index < env.deferred_import_refs.len()) : (index += 1) {
        const entry = env.deferred_import_refs.items.items[index];
        if (entry.kind != .file_import) continue;
        try resolver.resolveFileImport(entry, file_imports);
    }

    // Diagnostics the drain recorded belong to the module's reported set, so
    // they reach reporting exactly like the ones canonicalization recorded.
    try env.publishScratchDiagnosticsFrom(unpublished_before);
}

/// Drain a module's deferred import worklist.
///
/// This is the single entry point every checking entry point calls, with the
/// same inputs, after the module's direct imports have completed and before
/// `Check.init`. File imports have already been read by
/// `resolveDeferredFileImports`, so their entries are settled here.
pub fn resolveDeferredImports(
    env: *ModuleEnv,
    inputs: Inputs,
) std.mem.Allocator.Error!void {
    if (env.deferred_import_refs.len() == 0 and env.imports.imports.len() == 0) return;

    // Diagnostics canonicalization deliberately left unpublished stay that
    // way; only the ones this drain records are published below.
    const unpublished_before = env.unpublishedDiagnosticCount();

    var resolver = try Resolver.init(env, inputs);
    defer resolver.deinit();

    try resolver.recordImportIdentities();

    var index: u32 = 0;
    while (index < env.deferred_import_refs.len()) : (index += 1) {
        const entry = env.deferred_import_refs.items.items[index];
        if (entry.kind == .file_import) {
            std.debug.assert(env.fileDependencySettled(@enumFromInt(entry.file_dependency_idx)));
            continue;
        }
        try resolver.resolveEntry(entry);
    }

    // Method registrations the drain appended take part in sorted lookup.
    if (resolver.registered_methods) env.finalizeMethodTables();

    // Diagnostics the drain recorded belong to the module's reported set, so
    // they reach reporting exactly like the ones canonicalization recorded.
    try env.publishScratchDiagnosticsFrom(unpublished_before);
}

fn sha256Bytes(bytes: []const u8) [32]u8 {
    var digest: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytes, &digest, .{});
    return digest;
}

/// One receiver-extension registration this drain made.
const ReceiverExtension = struct {
    table_index: ModuleEnv.MethodTableIndex,
    region: Region,
};

/// The name an import's exposed declarations carry as their prefix.
const Prefix = struct {
    /// The prefix itself, or null when the import reaches the module's own
    /// exposed names directly.
    text: ?[]const u8 = null,
    /// Whether the prefix is the whole name the import reaches. A package
    /// header that projects one declaration, and an alias that names one,
    /// reach that declaration and nothing else in its module.
    exclusive: bool = false,
};

/// A module environment plus the declaration node selected inside it.
const DeclRef = struct {
    env: *const ModuleEnv,
    node_idx: u32,
};

/// What a resolved value path denotes.
const ValueTarget = union(enum) {
    /// An exposed value definition in the imported module.
    value: u32,
    /// An associated item of a type declaration exposed by the imported
    /// module. Checking resolves the declaration's transparent aliases.
    associated: struct {
        type_node_idx: u32,
        type_ident: Ident.Idx,
        item_ident: Ident.Idx,
    },
    /// An associated item of a declaration in a module reached through an
    /// exposed alias, already resolved to its exact definition.
    resolved_associated: struct {
        owner_env: *const ModuleEnv,
        type_node_idx: u32,
        def_idx: CIR.Def.Idx,
        source_ident: Ident.Idx,
    },
};

/// The import a deferred reference whose kind always names one goes through.
fn entryImport(entry: DeferredImportRef) CIR.Import.Idx {
    return entry.importIdx() orelse std.debug.panic(
        "compiler invariant violated: a deferred reference of kind {s} names a module import",
        .{@tagName(entry.kind)},
    );
}

const Resolver = struct {
    env: *ModuleEnv,
    inputs: Inputs,
    /// Import outcomes by the exact import spelling.
    outcomes: std.StringHashMap(ResolvedImport.Resolution),
    /// Every environment the drain may reach, by content identity.
    by_identity: std.AutoHashMap(base.ModuleIdentity.Hash, *const ModuleEnv),
    /// Scratch buffer for assembling qualified lookup names.
    scratch: std.ArrayList(u8),
    /// Receiver-extension registrations this drain has made, so two of them
    /// competing for one receiver call are reported like any other shadowing.
    receiver_extensions: std.AutoHashMap(ModuleEnv.MethodKey, ReceiverExtension),
    /// Whether any method registration was appended, which the method tables
    /// must be re-finalized for.
    registered_methods: bool,
    /// Owned copy of the entry's path text. Resolution interns identifiers in
    /// this module's store, which can move the interner's byte buffer, so the
    /// path it is walking cannot be a slice of that buffer.
    path_buf: std.ArrayList(u8),

    fn init(env: *ModuleEnv, inputs: Inputs) std.mem.Allocator.Error!Resolver {
        var self = Resolver{
            .env = env,
            .inputs = inputs,
            .outcomes = std.StringHashMap(ResolvedImport.Resolution).init(env.gpa),
            .by_identity = std.AutoHashMap(base.ModuleIdentity.Hash, *const ModuleEnv).init(env.gpa),
            .scratch = std.ArrayList(u8).empty,
            .receiver_extensions = std.AutoHashMap(ModuleEnv.MethodKey, ReceiverExtension).init(env.gpa),
            .registered_methods = false,
            .path_buf = std.ArrayList(u8).empty,
        };
        errdefer self.deinit();

        switch (inputs.imports) {
            .explicit => |imports| for (imports) |import| {
                try self.outcomes.put(import.import_name, import.resolution);
                switch (import.resolution) {
                    .available => |available| try self.registerEnv(available.module_env),
                    .rejected => {},
                }
            },
            .resolved_store => |envs| {
                const count: u32 = @intCast(env.imports.imports.len());
                var idx: u32 = 0;
                while (idx < count) : (idx += 1) {
                    const name = env.common.strings.get(env.imports.imports.items.items[idx]);
                    // An import the store left unresolved names no module at
                    // all, which is exactly the absence the drain reads below.
                    const resolved = env.imports.getResolvedModule(@enumFromInt(idx)) orelse continue;
                    if (resolved >= envs.len) continue;
                    try self.outcomes.put(name, .{ .available = .{ .module_env = envs[resolved] } });
                    try self.registerEnv(envs[resolved]);
                }
            },
        }
        for (inputs.reachable_envs) |reachable| try self.registerEnv(reachable);
        return self;
    }

    fn deinit(self: *Resolver) void {
        self.outcomes.deinit();
        self.by_identity.deinit();
        self.scratch.deinit(self.env.gpa);
        self.receiver_extensions.deinit();
        self.path_buf.deinit(self.env.gpa);
    }

    fn registerEnv(self: *Resolver, other: *const ModuleEnv) std.mem.Allocator.Error!void {
        const hash = other.contentIdentityHash() orelse return;
        const gop = try self.by_identity.getOrPut(hash.*);
        if (!gop.found_existing) gop.value_ptr.* = other;
    }

    fn envForIdentity(
        self: *const Resolver,
        owner: *const ModuleEnv,
        identity: base.ModuleIdentity.Idx,
    ) ?*const ModuleEnv {
        const hash = owner.moduleIdentityHash(identity);
        return self.by_identity.get(hash.*);
    }

    /// The outcome recorded for one of this module's imports.
    fn outcomeFor(self: *const Resolver, import_idx: CIR.Import.Idx) ?ResolvedImport.Resolution {
        const idx: usize = @intFromEnum(import_idx);
        if (idx >= self.env.imports.imports.len()) return null;
        const name = self.env.common.strings.get(self.env.imports.imports.items.items[idx]);
        return self.outcomes.get(name);
    }

    /// Record the content identity each import resolved to, so that a module
    /// importing this one can follow an alias declared here without spelling
    /// any module's name.
    fn recordImportIdentities(self: *Resolver) std.mem.Allocator.Error!void {
        const count: u32 = @intCast(self.env.imports.imports.len());
        var idx: u32 = 0;
        while (idx < count) : (idx += 1) {
            const import_idx: CIR.Import.Idx = @enumFromInt(idx);
            // The compiler's own baked `Builtin` module is part of the
            // compiler, not a module this one depends on, so it takes no
            // entry here and owns no receiver extension.
            const import_name = self.env.common.strings.get(self.env.imports.imports.items.items[idx]);
            if (CIR.Import.isCompilerBuiltinImportName(import_name)) continue;
            const outcome = self.outcomeFor(import_idx) orelse continue;
            const available = switch (outcome) {
                .available => |available| available,
                .rejected => continue,
            };
            const hash = available.module_env.contentIdentityHash() orelse continue;
            const display = try self.env.insertIdent(Ident.for_text(available.module_env.module_name));
            const identity = try self.env.internModuleIdentity(hash, display);
            try self.env.setImportIdentity(import_idx, identity);
        }
    }

    // Name assembly.

    fn scratchQualified(
        self: *Resolver,
        prefix: []const u8,
        suffix: []const u8,
    ) std.mem.Allocator.Error![]const u8 {
        self.scratch.clearRetainingCapacity();
        try self.scratch.appendSlice(self.env.gpa, prefix);
        try self.scratch.append(self.env.gpa, '.');
        try self.scratch.appendSlice(self.env.gpa, suffix);
        return self.scratch.items;
    }

    // Lookups inside one module.

    fn exposedTarget(
        self: *Resolver,
        other: *const ModuleEnv,
        prefix: Prefix,
        path: []const u8,
    ) std.mem.Allocator.Error!?collections.ExposedItemTarget {
        if (prefix.text) |text| {
            const qualified = try self.scratchQualified(text, path);
            if (other.common.findIdent(qualified)) |ident| {
                if (other.getExposedTargetById(ident)) |target| return target;
            }
            if (prefix.exclusive) return null;
        }
        {
            const qualified = try self.scratchQualified(other.module_name, path);
            if (other.common.findIdent(qualified)) |ident| {
                if (other.getExposedTargetById(ident)) |target| return target;
            }
        }
        if (other.common.findIdent(path)) |ident| {
            if (other.getExposedTargetById(ident)) |target| return target;
        }
        return null;
    }

    fn exposedTypeNode(
        self: *Resolver,
        other: *const ModuleEnv,
        prefix: Prefix,
        path: []const u8,
    ) std.mem.Allocator.Error!?u32 {
        const target = (try self.exposedTarget(other, prefix, path)) orelse return null;
        return target.typeDeclNode();
    }

    fn exposedValueNode(
        self: *Resolver,
        other: *const ModuleEnv,
        prefix: Prefix,
        path: []const u8,
    ) std.mem.Allocator.Error!?u32 {
        const target = (try self.exposedTarget(other, prefix, path)) orelse return null;
        return target.valueDefNode();
    }

    /// Find a type declaration by the header name it carries in its own
    /// module, which covers declarations a module does not expose by name but
    /// which a public selection still reaches.
    fn typeDeclNodeByHeader(
        self: *Resolver,
        other: *const ModuleEnv,
        prefix: Prefix,
        path: []const u8,
    ) std.mem.Allocator.Error!?u32 {
        const qualified_ident = blk: {
            if (prefix.text) |text| {
                const qualified = try self.scratchQualified(text, path);
                if (other.common.findIdent(qualified)) |ident| break :blk ident;
                if (prefix.exclusive) return null;
            }
            {
                const qualified = try self.scratchQualified(other.module_name, path);
                if (other.common.findIdent(qualified)) |ident| break :blk ident;
            }
            break :blk other.common.findIdent(path) orelse return null;
        };

        for (other.store.sliceStatements(other.all_statements)) |stmt_idx| {
            const header_idx = switch (other.store.getStatement(stmt_idx)) {
                .s_nominal_decl => |decl| decl.header,
                .s_alias_decl => |decl| decl.header,
                .s_where_alias_decl => |decl| decl.header,
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
                .s_import,
                .s_type_anno,
                .s_type_var_alias,
                .s_runtime_error,
                => continue,
            };
            if (other.store.getTypeHeader(header_idx).name.eql(qualified_ident)) {
                return @intFromEnum(stmt_idx);
            }
        }
        return null;
    }

    /// The declaration an exposed alias names, when the alias's annotation is
    /// a plain type reference. This is how a re-exported type
    /// (`Files : Resource.Files`) reaches the module it was declared in.
    fn followAlias(self: *const Resolver, other: *const ModuleEnv, node_idx: u32) ?DeclRef {
        const stmt_idx: Statement.Idx = @enumFromInt(node_idx);
        const alias = switch (other.store.getStatement(stmt_idx)) {
            .s_alias_decl => |decl| decl,
            .s_nominal_decl,
            .s_where_alias_decl,
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
            .s_import,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => return null,
        };
        const anno_base = switch (other.store.getTypeAnno(alias.anno)) {
            .lookup => |lookup| lookup.base,
            .apply => |apply| apply.base,
            .rigid_var,
            .rigid_var_lookup,
            .underscore,
            .tag_union,
            .tag,
            .tuple,
            .record,
            .@"fn",
            .parens,
            .malformed,
            => return null,
        };
        return switch (anno_base) {
            .local => |local| DeclRef{ .env = other, .node_idx = @intFromEnum(local.decl_idx) },
            .external => |external| blk: {
                const identity = other.importIdentity(external.module_idx) orelse break :blk null;
                const target_env = self.envForIdentity(other, identity) orelse break :blk null;
                break :blk DeclRef{ .env = target_env, .node_idx = external.target_node_idx };
            },
            .external_identity => |external| blk: {
                const target_env = self.envForIdentity(other, external.module_identity) orelse break :blk null;
                break :blk DeclRef{ .env = target_env, .node_idx = external.target_node_idx };
            },
            .builtin, .pending => null,
        };
    }

    // Path resolution.

    /// Resolve a dotted type path inside `other`, following exposed aliases
    /// out of it when a prefix of the path names one.
    fn resolveTypePath(
        self: *Resolver,
        other: *const ModuleEnv,
        prefix: Prefix,
        path: []const u8,
        depth: u8,
    ) std.mem.Allocator.Error!?DeclRef {
        if (try self.exposedTypeNode(other, prefix, path)) |node_idx| {
            return DeclRef{ .env = other, .node_idx = node_idx };
        }
        if (try self.typeDeclNodeByHeader(other, prefix, path)) |node_idx| {
            return DeclRef{ .env = other, .node_idx = node_idx };
        }
        if (depth == 0) return null;

        // Longest exposed prefix first: `A.B.C` prefers an `A.B` that exists
        // over an `A` that would have to carry `B.C` onward.
        var split = lastDot(path);
        while (split) |dot| : (split = lastDot(path[0..dot])) {
            const head = path[0..dot];
            const rest = path[dot + 1 ..];
            const head_node = (try self.exposedTypeNode(other, prefix, head)) orelse
                (try self.typeDeclNodeByHeader(other, prefix, head)) orelse continue;
            const alias_target = self.followAlias(other, head_node) orelse continue;
            // The alias names one declaration, so the walk continues inside
            // that declaration and nothing else in its module.
            const next_prefix = Prefix{
                .text = declPrefix(alias_target.env, alias_target.node_idx),
                .exclusive = true,
            };
            if (try self.resolveTypePath(alias_target.env, next_prefix, rest, depth - 1)) |found| {
                return found;
            }
        }
        return null;
    }

    fn lastDot(text: []const u8) ?usize {
        var i = text.len;
        while (i > 0) {
            i -= 1;
            if (text[i] == '.') return i;
        }
        return null;
    }

    /// Resolve a dotted value path inside `other`.
    fn resolveValuePath(
        self: *Resolver,
        other: *const ModuleEnv,
        prefix: Prefix,
        path: []const u8,
        module_ident: Ident.Idx,
    ) std.mem.Allocator.Error!?ValueTarget {
        // A path through a type declaration exposed by this module selects an
        // associated item of that declaration; checking resolves the
        // declaration's transparent aliases to the definition.
        if (lastDot(path)) |dot| {
            const head = path[0..dot];
            const item_text = path[dot + 1 ..];
            if (try self.exposedTypeNode(other, prefix, head)) |type_node_idx| {
                if (other.store.getStatement(@enumFromInt(type_node_idx)) == .s_alias_decl) {
                    return ValueTarget{ .associated = .{
                        .type_node_idx = type_node_idx,
                        .type_ident = try self.env.insertIdent(Ident.for_text(head)),
                        .item_ident = try self.env.insertIdent(Ident.for_text(item_text)),
                    } };
                }
            }
        }

        if (try self.exposedValueNode(other, prefix, path)) |node_idx| {
            return ValueTarget{ .value = node_idx };
        }

        // A path whose leading segments name a type reached through an
        // exposed alias selects an associated item of that declaration in the
        // module it was declared in.
        if (lastDot(path)) |dot| {
            const head = path[0..dot];
            const item_text = path[dot + 1 ..];
            if (try self.resolveTypePath(other, prefix, head, max_alias_depth)) |decl| {
                if (decl.env != other) {
                    const item_ident = decl.env.common.findIdent(item_text) orelse return null;
                    const owner = ModuleEnv.MethodOwner.initSelf(@enumFromInt(decl.node_idx));
                    if (decl.env.lookupMethodBindingForMethodOwnerConst(owner, item_ident)) |binding| {
                        return ValueTarget{ .resolved_associated = .{
                            .owner_env = decl.env,
                            .type_node_idx = @intFromEnum(binding.type_node_idx),
                            .def_idx = binding.def_idx,
                            .source_ident = try self.env.insertQualifiedIdent(
                                self.env.getIdent(module_ident),
                                path,
                            ),
                        } };
                    }
                }
            }
        }

        return null;
    }

    const max_alias_depth: u8 = 16;

    // Entry resolution.

    fn resolveEntry(self: *Resolver, entry: DeferredImportRef) std.mem.Allocator.Error!void {
        if (entry.kind == .receiver_method_owner) {
            return self.resolveReceiverMethodOwner(entry);
        }

        const outcome = self.outcomeFor(entryImport(entry));

        if (entry.kind == .import_statement) {
            // Import resolution reports its own rejections, and only the
            // workspace resolver can judge a package-qualified name, so this
            // statement reports only a plain name that denotes no module.
            if (outcome != null) return;
            if (entry.has(DeferredImportRef.Flags.is_package_qualified)) return;
            try self.env.pushDiagnostic(.{ .module_not_found = .{
                .module_name = entry.moduleName(),
                .region = self.regionOf(entry),
            } });
            return;
        }

        const available = switch (outcome orelse return self.failMissingModule(entry)) {
            .available => |available| available,
            .rejected => return self.failMissingModule(entry),
        };

        const prefix = importPrefix(available);

        self.path_buf.clearRetainingCapacity();
        try self.path_buf.appendSlice(self.env.gpa, self.env.getIdent(entry.path()));
        const path_text = self.path_buf.items;

        if (entry.has(DeferredImportRef.Flags.names_import_main_type)) {
            const node_idx = (try self.importMainTypeNode(available)) orelse
                return self.failEntry(entry, entry.not_found_failure);
            return self.rewriteResolvedType(entry, available.module_env, node_idx, true);
        }

        if (entry.has(DeferredImportRef.Flags.tag_after_import_alias)) {
            // `Alias.Name(...)` with an import's alias as the qualifier: when
            // the import selects a public declaration, the qualifier denotes
            // that declaration and `Name` is one of its tags; when the import
            // selects none, the qualifier denotes the module and `Name`
            // denotes one of its exposed types, which `entry.path()` spells.
            if (try self.importMainTypeNode(available)) |node_idx| {
                return self.rewriteResolvedType(entry, available.module_env, node_idx, true);
            }
        }

        switch (entry.kind) {
            .expr_value => try self.resolveValueEntry(entry, available, prefix, path_text),
            .receiver_method_owner, .file_import => unreachable, // handled above
            .numeric_suffix,
            .type_anno_lookup,
            .type_anno_apply,
            .expr_nominal,
            .pattern_nominal,
            => try self.resolveTypeEntry(entry, available, prefix, path_text),
            .exposed_item => try self.checkExposedItem(entry, available, prefix, path_text),
            .hosted_entry => try self.resolveHostedEntry(entry, available, prefix, path_text),
            .import_statement => unreachable, // handled above
        }
    }

    fn resolveValueEntry(
        self: *Resolver,
        entry: DeferredImportRef,
        available: ResolvedImport.Available,
        prefix: Prefix,
        path_text: []const u8,
    ) std.mem.Allocator.Error!void {
        const expr_idx: CIR.Expr.Idx = @enumFromInt(entry.node_idx);
        const import_idx = entryImport(entry);

        if (try self.resolveValuePath(available.module_env, prefix, path_text, entry.moduleName())) |found| {
            switch (found) {
                .value => |node_idx| self.env.store.resolveDeferredExprToExternalLookup(
                    expr_idx,
                    import_idx,
                    node_idx,
                    entry.itemName(),
                ),
                .associated => |assoc| self.env.store.resolveDeferredExprToAssociatedLookup(
                    expr_idx,
                    import_idx,
                    assoc.type_node_idx,
                    assoc.type_ident,
                    assoc.item_ident,
                ),
                .resolved_associated => |assoc| {
                    const hash = assoc.owner_env.contentIdentityHash() orelse
                        return self.failEntry(entry, entry.not_found_failure);
                    const display = try self.env.insertIdent(Ident.for_text(assoc.owner_env.module_name));
                    const identity = try self.env.internModuleIdentity(hash, display);
                    self.env.store.replaceExprWithResolvedAssociatedLookup(
                        expr_idx,
                        identity,
                        @enumFromInt(assoc.type_node_idx),
                        assoc.def_idx,
                        assoc.source_ident,
                    );
                },
            }
            return;
        }

        // A tag of the imported module's main nominal type is the remaining
        // meaning a single upper-case segment can have.
        if (entry.has(DeferredImportRef.Flags.allows_nominal_tag)) {
            if (try self.importMainTypeNode(available)) |node_idx| {
                {
                    const tag_expr = try self.env.addExpr(CIR.Expr{ .e_tag = .{
                        .name = entry.itemName(),
                        .args = .{ .span = base.DataSpan.empty() },
                    } }, self.env.store.getExprRegion(expr_idx));
                    try self.env.store.resolveDeferredExprToNominalExternal(
                        expr_idx,
                        import_idx,
                        node_idx,
                        tag_expr,
                        .tag,
                    );
                    return;
                }
            }
        }

        try self.failEntry(entry, entry.not_found_failure);
    }

    fn resolveTypeEntry(
        self: *Resolver,
        entry: DeferredImportRef,
        available: ResolvedImport.Available,
        prefix: Prefix,
        path_text: []const u8,
    ) std.mem.Allocator.Error!void {
        const decl = (try self.resolveTypePath(available.module_env, prefix, path_text, max_alias_depth)) orelse
            return self.failEntry(entry, entry.not_found_failure);

        try self.rewriteResolvedType(entry, decl.env, decl.node_idx, decl.env == available.module_env);
    }

    fn rewriteResolvedType(
        self: *Resolver,
        entry: DeferredImportRef,
        decl_env: *const ModuleEnv,
        decl_node_idx: u32,
        in_import: bool,
    ) std.mem.Allocator.Error!void {
        const import_idx = entryImport(entry);

        switch (entry.kind) {
            .numeric_suffix => {
                const target: ModuleEnv.NumericSuffixTarget.Target = if (in_import)
                    .{ .external = .{ .import_idx = import_idx, .target_node_idx = decl_node_idx } }
                else
                    .{ .external_identity = .{
                        .module_identity = (try self.identityFor(decl_env)) orelse
                            return self.failEntry(entry, entry.not_found_failure),
                        .target_node_idx = decl_node_idx,
                    } };
                try self.env.recordNumericSuffixTarget(@enumFromInt(entry.node_idx), target);
            },
            .type_anno_lookup => {
                const anno_idx: CIR.TypeAnno.Idx = @enumFromInt(entry.node_idx);
                if (in_import) {
                    self.env.store.resolveDeferredTypeAnnoLookupBase(anno_idx, import_idx, decl_node_idx);
                } else {
                    const identity = (try self.identityFor(decl_env)) orelse
                        return self.failEntry(entry, entry.not_found_failure);
                    self.env.store.resolveDeferredTypeAnnoLookupBaseIdentity(anno_idx, identity, decl_node_idx);
                }
            },
            .type_anno_apply => {
                const anno_idx: CIR.TypeAnno.Idx = @enumFromInt(entry.node_idx);
                if (in_import) {
                    self.env.store.resolveDeferredTypeAnnoApplyBase(anno_idx, import_idx, decl_node_idx);
                } else {
                    const identity = (try self.identityFor(decl_env)) orelse
                        return self.failEntry(entry, entry.not_found_failure);
                    self.env.store.resolveDeferredTypeAnnoApplyBaseIdentity(anno_idx, identity, decl_node_idx);
                }
            },
            .expr_nominal, .pattern_nominal => {
                // A nominal construction names the declaration through the
                // import it was written with, so a declaration reached only by
                // following an alias out of that module is not one this
                // position can name.
                if (!in_import) return self.failEntry(entry, entry.not_found_failure);
                switch (decl_env.store.getStatement(@enumFromInt(decl_node_idx))) {
                    .s_nominal_decl => try self.rewriteNominal(entry, import_idx, decl_node_idx),
                    // A construction names a nominal type; an alias is a
                    // different kind of declaration, not a missing one.
                    .s_alias_decl, .s_where_alias_decl => try self.failEntry(entry, .type_alias_but_needed_nominal),
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
                    .s_import,
                    .s_type_anno,
                    .s_type_var_alias,
                    .s_runtime_error,
                    => try self.failEntry(entry, .type_not_exposed),
                }
            },
            .expr_value,
            .receiver_method_owner,
            .exposed_item,
            .import_statement,
            .hosted_entry,
            .file_import,
            => unreachable, // handled by the caller
        }
    }

    fn rewriteNominal(
        self: *Resolver,
        entry: DeferredImportRef,
        import_idx: CIR.Import.Idx,
        node_idx: u32,
    ) std.mem.Allocator.Error!void {
        switch (entry.kind) {
            .expr_nominal => {
                const expr_idx: CIR.Expr.Idx = @enumFromInt(entry.node_idx);
                const deferred = self.env.store.getExpr(expr_idx).e_deferred_import_ref;
                const backing = deferred.backing orelse
                    return self.failEntry(entry, entry.not_found_failure);
                try self.env.store.resolveDeferredExprToNominalExternal(
                    expr_idx,
                    import_idx,
                    node_idx,
                    backing.expr,
                    backing.ty,
                );
            },
            .pattern_nominal => {
                const pattern_idx: CIR.Pattern.Idx = @enumFromInt(entry.node_idx);
                const deferred = self.env.store.getPattern(pattern_idx).deferred_import_ref;
                try self.env.store.resolveDeferredPatternToNominalExternal(
                    pattern_idx,
                    import_idx,
                    node_idx,
                    deferred.backing_pattern,
                    deferred.backing_type,
                );
            },
            .expr_value,
            .numeric_suffix,
            .type_anno_lookup,
            .type_anno_apply,
            .receiver_method_owner,
            .exposed_item,
            .import_statement,
            .hosted_entry,
            .file_import,
            => unreachable, // only nominal kinds reach here
        }
    }

    /// The declaration node an import's own name denotes: the declaration a
    /// package header makes public, or a type module's main type.
    fn importMainTypeNode(
        self: *Resolver,
        available: ResolvedImport.Available,
    ) std.mem.Allocator.Error!?u32 {
        if (available.selected_type_decl) |decl| {
            return available.module_env.getExposedNodeIndexByStatementIdx(decl);
        }
        const main_text = importPrefix(available).text orelse return null;
        return try self.exposedTypeNode(available.module_env, .{}, main_text);
    }

    /// Read an `import "path" as name` file and bind its contents, recording
    /// the read state the build's watch mode and checked-cache identity use.
    fn resolveFileImport(
        self: *Resolver,
        entry: DeferredImportRef,
        file_imports: FileImports,
    ) std.mem.Allocator.Error!void {
        const expr_idx: CIR.Expr.Idx = @enumFromInt(entry.node_idx);
        const region = self.regionOf(entry);
        const dependency_idx: ModuleEnv.FileDependency.SafeList.Idx = @enumFromInt(entry.file_dependency_idx);

        self.path_buf.clearRetainingCapacity();
        try self.path_buf.appendSlice(self.env.gpa, self.env.getIdent(entry.path()));
        const relative_path = self.path_buf.items;

        const read = switch (file_imports) {
            .read => |read| read,
            .skip => {
                self.env.setFileDependencyUnreadable(dependency_idx);
                return self.failFileImport(expr_idx, relative_path, region, .file_import_io_error);
            },
        };

        const full_path = if (read.source_dir) |dir|
            std.fs.path.join(self.env.gpa, &.{ dir, relative_path }) catch return error.OutOfMemory
        else
            self.env.gpa.dupe(u8, relative_path) catch return error.OutOfMemory;
        defer self.env.gpa.free(full_path);

        const file_contents: []u8 = read.ctx.readFile(full_path, self.env.gpa) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FileNotFound => {
                self.env.setFileDependencyMissing(dependency_idx);
                return self.failFileImport(expr_idx, relative_path, region, .file_import_not_found);
            },
            error.AccessDenied, error.StreamTooLong, error.IoError => {
                self.env.setFileDependencyUnreadable(dependency_idx);
                return self.failFileImport(expr_idx, relative_path, region, .file_import_io_error);
            },
        };
        defer self.env.gpa.free(file_contents);
        self.env.setFileDependencyContentHash(dependency_idx, sha256Bytes(file_contents));

        if (entry.has(DeferredImportRef.Flags.file_import_is_bytes)) {
            const literal = try self.env.insertString(file_contents);
            self.env.store.resolveDeferredExprToBytesLiteral(expr_idx, literal);
            return;
        }

        if (!std.unicode.utf8ValidateSlice(file_contents)) {
            return self.failFileImport(expr_idx, relative_path, region, .file_import_not_utf8);
        }
        const literal = try self.env.insertString(file_contents);
        self.env.store.resolveDeferredExprToStringSegment(expr_idx, literal);
    }

    const FileImportFailure = enum { file_import_not_found, file_import_io_error, file_import_not_utf8 };

    fn failFileImport(
        self: *Resolver,
        expr_idx: CIR.Expr.Idx,
        relative_path: []const u8,
        region: Region,
        failure: FileImportFailure,
    ) std.mem.Allocator.Error!void {
        const path = try self.env.insertString(relative_path);
        const diagnostic: Diagnostic = switch (failure) {
            .file_import_not_found => .{ .file_import_not_found = .{ .path = path, .region = region } },
            .file_import_io_error => .{ .file_import_io_error = .{ .path = path, .region = region } },
            .file_import_not_utf8 => .{ .file_import_not_utf8 = .{ .path = path, .region = region } },
        };
        try self.env.settleDeferredExprAsRuntimeError(expr_idx, diagnostic);
    }

    /// Settle a platform `hosted` entry against the module its mapping names.
    /// The recorded target is the same external-definition identity an
    /// ordinary qualified value reference resolves to.
    fn resolveHostedEntry(
        self: *Resolver,
        entry: DeferredImportRef,
        available: ResolvedImport.Available,
        prefix: Prefix,
        path_text: []const u8,
    ) std.mem.Allocator.Error!void {
        const hosted = &self.env.hosted_entries.items.items[entry.node_idx];
        const target_node_idx = (try self.exposedValueNode(available.module_env, prefix, path_text)) orelse {
            hosted.target_status = .missing_value;
            return;
        };
        hosted.target_import = entryImport(entry);
        hosted.target_def = @enumFromInt(target_node_idx);
        hosted.target_status = .resolved;
    }

    /// Report an `import ... exposing [...]` item the module does not expose,
    /// or exposes as the other sort of thing than the item's spelling names.
    fn checkExposedItem(
        self: *Resolver,
        entry: DeferredImportRef,
        available: ResolvedImport.Available,
        prefix: Prefix,
        path_text: []const u8,
    ) std.mem.Allocator.Error!void {
        const selects_type = entry.has(DeferredImportRef.Flags.selects_type);
        if (try self.exposedTarget(available.module_env, prefix, path_text)) |target| {
            const node = if (selects_type) target.typeDeclNode() else target.valueDefNode();
            if (node != null) return;
        }
        const region = self.regionOf(entry);
        try self.env.pushDiagnostic(if (selects_type)
            .{ .type_not_exposed = .{
                .module_name = entry.moduleName(),
                .type_name = entry.itemName(),
                .region = region,
            } }
        else
            .{ .value_not_exposed = .{
                .module_name = entry.moduleName(),
                .value_name = entry.itemName(),
                .region = region,
            } });
    }

    /// Register a receiver-extension method on an imported receiver type. The
    /// owner's identity is only known once the receiver type reference has
    /// resolved, which is why the registration waits here for it: the
    /// annotation this entry names has already been rewritten above.
    fn resolveReceiverMethodOwner(self: *Resolver, entry: DeferredImportRef) std.mem.Allocator.Error!void {
        const anno_idx: CIR.TypeAnno.Idx = @enumFromInt(entry.node_idx);
        const anno_base = switch (self.env.store.getTypeAnno(anno_idx)) {
            .lookup => |lookup| lookup.base,
            .apply => |apply| apply.base,
            .rigid_var,
            .rigid_var_lookup,
            .underscore,
            .tag_union,
            .tag,
            .tuple,
            .record,
            .@"fn",
            .parens,
            // The reference did not resolve, and its own diagnostic says so.
            .malformed,
            => return,
        };
        const owner = switch (anno_base) {
            .external => |external| blk: {
                // The compiler's own baked Builtin module records no import
                // identity here, and owns no receiver extension.
                const identity = self.env.importIdentity(external.module_idx) orelse return;
                break :blk ModuleEnv.MethodOwner.initImported(identity, @enumFromInt(external.target_node_idx));
            },
            .external_identity => |external| ModuleEnv.MethodOwner.initImported(
                external.module_identity,
                @enumFromInt(external.target_node_idx),
            ),
            .local => |local| ModuleEnv.MethodOwner.initSelf(local.decl_idx),
            .builtin, .pending => return,
        };

        const binding = entry.methodBinding() orelse std.debug.panic(
            "compiler invariant violated: a receiver method owner reference carries its method binding",
            .{},
        );
        const key = ModuleEnv.MethodKey.init(owner, entry.methodIdent());
        const region = self.env.store.getNodeRegion(ModuleEnv.nodeIdxFrom(binding.def_idx));
        const gop = try self.receiver_extensions.getOrPut(key);
        if (gop.found_existing) {
            // Two extensions in this module compete for one receiver call, so
            // the later declaration wins and shadows the earlier one, exactly
            // as two extensions on a locally declared receiver do.
            try self.env.pushDiagnostic(.{ .shadowing_warning = .{
                .ident = entry.methodIdent(),
                .region = region,
                .original_region = gop.value_ptr.region,
            } });
            self.env.replaceMethodAt(gop.value_ptr.table_index, owner, entry.methodIdent(), entry.qualifiedName(), binding);
            gop.value_ptr.region = region;
            return;
        }
        gop.value_ptr.* = .{
            .table_index = try self.env.appendMethodForMethodOwner(owner, entry.methodIdent(), entry.qualifiedName(), binding),
            .region = region,
        };
        self.registered_methods = true;
    }

    fn identityFor(
        self: *Resolver,
        other: *const ModuleEnv,
    ) std.mem.Allocator.Error!?base.ModuleIdentity.Idx {
        const hash = other.contentIdentityHash() orelse return null;
        const display = try self.env.insertIdent(Ident.for_text(other.module_name));
        return try self.env.internModuleIdentity(hash, display);
    }

    // Failure.

    /// The import names no module, so every reference through it is
    /// checked-error data.
    fn failMissingModule(self: *Resolver, entry: DeferredImportRef) std.mem.Allocator.Error!void {
        switch (entry.kind) {
            .hosted_entry => {
                self.env.hosted_entries.items.items[entry.node_idx].target_status = .missing_module;
            },
            // The import statement owns the diagnostic for a name that denotes
            // no module, so its exposed items do not restate it.
            .exposed_item, .import_statement => {},
            .expr_value,
            .expr_nominal,
            .pattern_nominal,
            .numeric_suffix,
            .type_anno_lookup,
            .type_anno_apply,
            .receiver_method_owner,
            .file_import,
            => try self.failEntry(entry, entry.missing_module_failure),
        }
    }

    fn failEntry(
        self: *Resolver,
        entry: DeferredImportRef,
        failure: ModuleEnv.DeferredRefFailure,
    ) std.mem.Allocator.Error!void {
        const region = self.regionOf(entry);
        const diagnostic: Diagnostic = switch (failure) {
            .type_not_exposed => .{ .type_not_exposed = .{
                .module_name = entry.moduleName(),
                .type_name = entry.itemName(),
                .region = region,
            } },
            .type_from_missing_module => .{ .type_from_missing_module = .{
                .module_name = entry.moduleName(),
                .type_name = entry.itemName(),
                .region = region,
            } },
            .nested_type_not_found => .{ .nested_type_not_found = .{
                .parent_name = entry.parentName(),
                .nested_name = entry.itemName(),
                .region = region,
            } },
            .nested_value_not_found => .{ .nested_value_not_found = .{
                .parent_name = entry.parentName(),
                .nested_name = entry.itemName(),
                .region = region,
            } },
            .qualified_ident_does_not_exist => .{ .qualified_ident_does_not_exist = .{
                .ident = entry.qualifiedName(),
                .region = region,
            } },
            .value_not_exposed => .{ .value_not_exposed = .{
                .module_name = entry.moduleName(),
                .value_name = entry.itemName(),
                .region = region,
            } },
            .module_not_imported => .{ .module_not_imported = .{
                .module_name = entry.moduleName(),
                .region = region,
            } },
            .undeclared_type => .{ .undeclared_type = .{
                .name = entry.itemName(),
                .region = region,
            } },
            .type_alias_but_needed_nominal => .{ .type_alias_but_needed_nominal = .{
                .name = entry.itemName(),
                .region = region,
            } },
            .record_builder_map2_not_found => .{ .record_builder_map2_not_found = .{
                .type_name = entry.parentName(),
                .region = region,
            } },
        };

        switch (entry.kind) {
            .expr_value, .expr_nominal => {
                try self.env.settleDeferredExprAsRuntimeError(@enumFromInt(entry.node_idx), diagnostic);
            },
            .pattern_nominal => {
                const diagnostic_idx = try self.env.addDiagnostic(diagnostic);
                self.env.store.settleDeferredPatternAsRuntimeError(@enumFromInt(entry.node_idx), diagnostic_idx);
            },
            .numeric_suffix => {
                _ = try self.env.addDiagnostic(diagnostic);
                try self.env.recordNumericSuffixTarget(@enumFromInt(entry.node_idx), .invalid);
            },
            .type_anno_lookup, .type_anno_apply => {
                const diagnostic_idx = try self.env.addDiagnostic(diagnostic);
                self.env.store.replaceTypeAnnoWithRuntimeError(@enumFromInt(entry.node_idx), diagnostic_idx);
            },
            // A method registration has no node of its own: the receiver type
            // reference it waits on carries the diagnostic. An import
            // statement's own diagnostic is pushed directly.
            .receiver_method_owner, .import_statement, .exposed_item, .hosted_entry, .file_import => {},
        }
    }

    fn regionOf(self: *const Resolver, entry: DeferredImportRef) Region {
        if (entry.has(DeferredImportRef.Flags.has_diagnostic_region)) {
            return .{
                .start = .{ .offset = entry.diagnostic_region_start },
                .end = .{ .offset = entry.diagnostic_region_end },
            };
        }
        return self.env.store.getRegionAt(@enumFromInt(entry.node_idx));
    }
};

/// The declaration prefix a module's exposed names carry for a selected
/// public declaration, or null when the import names the whole module.
fn selectedPrefix(other: *const ModuleEnv, selected: ?Statement.Idx) ?[]const u8 {
    const stmt_idx = selected orelse return null;
    const header_idx = switch (other.store.getStatement(stmt_idx)) {
        .s_alias_decl => |decl| decl.header,
        .s_nominal_decl => |decl| decl.header,
        .s_where_alias_decl => |decl| decl.header,
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
        .s_import,
        .s_type_anno,
        .s_type_var_alias,
        .s_runtime_error,
        => return null,
    };
    return other.getIdent(other.store.getTypeHeader(header_idx).name);
}

/// The prefix a declaration's own nested names carry inside its module.
fn declPrefix(other: *const ModuleEnv, node_idx: u32) ?[]const u8 {
    return selectedPrefix(other, @enumFromInt(node_idx));
}

/// The name an import's exposed declarations carry as their prefix: the
/// declaration a package header makes public, or a type module's main type.
fn importPrefix(available: ResolvedImport.Available) Prefix {
    if (selectedPrefix(available.module_env, available.selected_type_decl)) |prefix| {
        // A header that made a declaration nested inside another one public
        // reaches that declaration and nothing else in its module. A header
        // that made the module's own main type public reaches the module's
        // exposed names as an ordinary import of it does.
        return .{
            .text = prefix,
            .exclusive = !Ident.textEql(prefix, available.module_env.module_name),
        };
    }
    if (available.module_env.module_kind != .type_module) return .{};
    // A type module's main type carries the module's basename, which is the
    // prefix its exposed declarations are named under.
    return .{ .text = available.module_env.module_name };
}

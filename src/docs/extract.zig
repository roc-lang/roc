//! Extraction of documentation data from compiled Roc modules.
//!
//! This module provides functions to extract doc comments from source text,
//! build structured type representations, and build `DocModel` structs from a `ModuleEnv`.

const std = @import("std");
const base = @import("base");
const CIR = @import("can").CIR;
const ModuleEnv = @import("can").ModuleEnv;
const types_mod = @import("types").types;
const CheckedArtifact = @import("check").CheckedArtifact;

const DocModel = @import("DocModel.zig");
const render_type = @import("render_type.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;

const TypeStore = @import("types").Store;
const Var = types_mod.Var;
const FlatType = types_mod.FlatType;
const NominalType = types_mod.NominalType;

const DocType = DocModel.DocType;
const TypeAnno = CIR.TypeAnno;

/// A doc comment along with the 1-based source line where its first `##`
/// line begins. Used so downstream consumers (e.g. the docs renderer's
/// broken-link reporter) can tie each character of `text` back to a source
/// position.
pub const DocCommentExtract = struct {
    text: []const u8,
    start_line: u32,
};

/// Precomputed index of newline byte positions for O(log n) byte-to-line lookup.
pub const LineIndex = struct {
    /// Sorted array of byte offsets where each `\n` occurs.
    newline_offsets: []const u32,

    pub fn build(allocator: Allocator, source: []const u8) Allocator.Error!LineIndex {
        var list = std.ArrayList(u32).empty;
        errdefer list.deinit(allocator);
        for (source, 0..) |byte, i| {
            if (byte == '\n') {
                try list.append(allocator, @intCast(i));
            }
        }
        return .{ .newline_offsets = try list.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: LineIndex, allocator: Allocator) void {
        allocator.free(self.newline_offsets);
    }

    /// Convert a byte offset to a 1-based line number using binary search.
    pub fn lineOf(self: LineIndex, offset: u32) u32 {
        return @intCast(std.sort.lowerBound(u32, self.newline_offsets, offset, struct {
            fn compare(context: u32, item: u32) std.math.Order {
                return std.math.order(context, item);
            }
        }.compare) + 1);
    }
};

/// Extract the module-level doc comment from the top of a source file.
///
/// Module doc comments are consecutive `##` lines at the very beginning of the
/// file, before any non-comment content. Returns null if none found.
pub fn extractModuleDocComment(gpa: Allocator, source: []const u8, line_index: LineIndex) Allocator.Error!?DocCommentExtract {
    var lines = std.ArrayList([]const u8).empty;
    defer lines.deinit(gpa);

    var first_line_byte: u32 = 0;

    var pos: usize = 0;
    while (pos < source.len) {
        const line_start = pos;
        // Skip leading whitespace on the line (spaces/tabs only)
        while (pos < source.len and (source[pos] == ' ' or source[pos] == '\t')) {
            pos += 1;
        }

        // Check for ## doc comment
        if (base.doc_comment.startsWithHashHash(source[pos..])) {
            if (lines.items.len == 0) {
                first_line_byte = @intCast(line_start);
            }
            pos += 2;
            // Skip optional leading space after ##
            if (pos < source.len and source[pos] == ' ') {
                pos += 1;
            }
            const content_start = pos;
            // Find end of line
            while (pos < source.len and source[pos] != '\n') {
                pos += 1;
            }
            try lines.append(gpa, source[content_start..pos]);
            // Skip newline
            if (pos < source.len and source[pos] == '\n') {
                pos += 1;
            }
        } else if (pos == source.len or source[pos] == '\n') {
            // Empty line—skip but keep looking
            if (pos < source.len) pos += 1;
            // If we already collected some doc comment lines, an empty line
            // that's purely whitespace can be part of the gap before the header.
            // But a blank line before any ## lines means no module doc.
            if (lines.items.len == 0) {
                continue;
            } else {
                // End of module doc comment block
                break;
            }
        } else {
            // Non-comment content reached
            break;
        }
    }

    if (lines.items.len == 0) return null;

    return .{
        .text = try joinLines(gpa, lines.items),
        .start_line = line_index.lineOf(first_line_byte),
    };
}

/// Extract the doc comment immediately preceding a definition at the given byte offset.
///
/// Scans backwards from `def_start_offset` to find consecutive `##` lines.
/// Returns null if no doc comment is found.
pub fn extractDocComment(gpa: Allocator, source: []const u8, def_start_offset: u32, line_index: LineIndex) Allocator.Error!?DocCommentExtract {
    if (def_start_offset == 0 or def_start_offset > source.len) return null;

    var lines = std.ArrayList([]const u8).empty;
    defer lines.deinit(gpa);

    var first_line_byte: u32 = 0;

    var pos: usize = def_start_offset;

    // Skip backwards over whitespace to find the end of the previous line
    while (pos > 0 and (source[pos - 1] == ' ' or source[pos - 1] == '\t' or source[pos - 1] == '\r')) {
        pos -= 1;
    }
    // Skip the newline
    if (pos > 0 and source[pos - 1] == '\n') {
        pos -= 1;
    }

    // Now scan backwards collecting ## lines
    while (pos > 0) {
        // Find the start of the current line
        var line_start = pos;
        while (line_start > 0 and source[line_start - 1] != '\n') {
            line_start -= 1;
        }

        // Check if this line is a ## doc comment
        const line = source[line_start..pos];
        const trimmed = trimLeft(line);

        if (base.doc_comment.startsWithHashHash(trimmed)) {
            // Track the earliest doc-comment line we've seen so far. Since we
            // scan bottom-up and lines are added in reverse, the most recent
            // assignment to this is the topmost ## line of the block.
            first_line_byte = @intCast(line_start);
            // It's a doc comment line
            const content = base.doc_comment.stripPrefix(trimmed);
            try lines.append(gpa, content);
        } else if (trimmed.len == 0) {
            // Empty/whitespace line—stop looking if we already have doc lines
            if (lines.items.len > 0) break;
            // Skip empty lines between def and potential doc comment
        } else {
            // Non-comment content—stop
            break;
        }

        // Move to previous line
        if (line_start == 0) break;
        pos = line_start - 1;
        // Skip the newline we backed over
        while (pos > 0 and source[pos - 1] == '\r') {
            pos -= 1;
        }
    }

    if (lines.items.len == 0) return null;

    // Reverse the lines (we collected them bottom-up)
    std.mem.reverse([]const u8, lines.items);

    return .{
        .text = try joinLines(gpa, lines.items),
        .start_line = line_index.lineOf(first_line_byte),
    };
}

/// Extract documentation for all exported definitions in a module.
///
/// For type modules, only the main type entry (the one matching the module name)
/// and its nested children are included. All other top-level definitions are
/// considered private to the module and are excluded from documentation.
/// For example, in a `Color.roc` type module, only `Color := ...` and its
/// methods are documented; any helper functions defined outside the `Color`
/// type declaration are not visible.
///
/// The Builtin module receives additional special handling: the `Builtin` opaque
/// entry itself is removed, and its children are re-distributed under the
/// proper parent types (Str, List, Bool, Num, etc.).
pub fn extractModuleDocs(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    package_name: []const u8,
    source_path: ?[]const u8,
) Allocator.Error!DocModel.ModuleDocs {
    return extractModuleDocsWithOptions(gpa, module_env, package_name, source_path, .{});
}

/// Controls which source-level entries are included in one module's docs.
pub const ExtractOptions = struct {
    /// When set, retain only these top-level entries and their children.
    exposed_names: ?[]const []const u8 = null,
    /// Exact source type represented by a public package/platform module name.
    public_type: ?PublicTypeProjection = null,
    /// Every exact public type projection in the generated package docs,
    /// sorted once with `sortPublicTypeProjections` before extraction.
    public_types: []const PublicTypeProjection = &.{},
    /// Checked import identities for resolving external annotation references.
    checked_artifact: ?*const CheckedArtifact.CheckedModuleArtifact = null,
};

/// A borrowed mapping from one exact source type to its public docs namespace.
pub const PublicTypeProjection = struct {
    public_name: []const u8,
    package_name: []const u8,
    source_env: *const ModuleEnv,
    source_identity: *const [32]u8,
    source_decl: CIR.Statement.Idx,
    /// Declaration order in the owning package's public surface.
    public_order: u32,
};

/// Sort projections for binary-search routing by exact source identity.
pub fn sortPublicTypeProjections(projections: []PublicTypeProjection) void {
    std.mem.sort(PublicTypeProjection, projections, {}, publicTypeProjectionLessThan);
}

fn publicTypeProjectionLessThan(_: void, a: PublicTypeProjection, b: PublicTypeProjection) bool {
    const identity_order = std.mem.order(u8, a.source_identity, b.source_identity);
    if (identity_order != .eq) return identity_order == .lt;
    if (a.public_order != b.public_order) return a.public_order < b.public_order;
    return std.mem.order(u8, a.public_name, b.public_name) == .lt;
}

const PublicReferenceRouting = struct {
    current: ?PublicTypeProjection,
    all: []const PublicTypeProjection,
    checked_artifact: ?*const CheckedArtifact.CheckedModuleArtifact,
};

/// Extract documentation, optionally restricted to explicit top-level names.
pub fn extractModuleDocsWithOptions(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    package_name: []const u8,
    source_path: ?[]const u8,
    options: ExtractOptions,
) Allocator.Error!DocModel.ModuleDocs {
    std.debug.assert(options.exposed_names == null or options.public_type == null);
    std.debug.assert(std.sort.isSorted(
        PublicTypeProjection,
        options.public_types,
        {},
        publicTypeProjectionLessThan,
    ));

    const source = module_env.getSourceAll();
    const line_index = try LineIndex.build(gpa, source);
    defer line_index.deinit(gpa);

    // A projected public type owns its public page, so its declaration comment
    // is the page comment. The source module's comment belongs to the private
    // parent namespace and must not leak into the projection.
    const module_doc_extract = if (options.public_type) |projection| blk: {
        std.debug.assert(projection.source_env == module_env);
        const source_root = projectionRootName(module_env, projection);
        if (std.mem.eql(u8, projection.public_name, module_env.module_name) and
            std.mem.eql(u8, source_root, module_env.module_name))
        {
            break :blk try extractModuleDocComment(gpa, source, line_index);
        }
        const region = module_env.store.getStatementRegion(projection.source_decl);
        break :blk try extractDocComment(gpa, source, region.start.offset, line_index);
    } else try extractModuleDocComment(gpa, source, line_index);
    errdefer if (module_doc_extract) |d| gpa.free(d.text);
    const module_doc: ?[]const u8 = if (module_doc_extract) |d| d.text else null;
    const module_doc_start_line: u32 = if (module_doc_extract) |d| d.start_line else 0;

    // Determine module kind
    const kind = convertModuleKind(module_env.module_kind);

    // Get module name
    const name = try gpa.dupe(u8, if (options.public_type) |projection|
        projection.public_name
    else
        module_env.module_name);
    errdefer gpa.free(name);

    // Dupe package name
    const pkg_name = try gpa.dupe(u8, package_name);
    errdefer gpa.free(pkg_name);

    // Display-qualified path for local type refs ("app.Geometry"): docs must
    // never render identity keys (URLs, canonical paths) as module paths.
    const local_module_name = if (options.public_type) |projection| projection.public_name else module_env.module_name;
    const local_module_path = try std.fmt.allocPrint(gpa, "{s}.{s}", .{ package_name, local_module_name });
    defer gpa.free(local_module_path);
    const reference_routing = PublicReferenceRouting{
        .current = options.public_type,
        .all = options.public_types,
        .checked_artifact = options.checked_artifact,
    };

    // Collect entries from exported defs
    var entries_list = std.ArrayList(DocModel.DocEntry).empty;
    defer {
        for (entries_list.items) |*e| e.deinit(gpa);
        entries_list.deinit(gpa);
    }

    var exposed_names: std.StringHashMapUnmanaged(void) = .empty;
    defer exposed_names.deinit(gpa);
    if (options.exposed_names) |names_to_expose| {
        try exposed_names.ensureTotalCapacity(gpa, @intCast(names_to_expose.len));
        for (names_to_expose) |exposed_name| exposed_names.putAssumeCapacity(exposed_name, {});
    }

    // For documentation purposes, show all accessible definitions, not just
    // what's explicitly exported. Exports control compilation/linking (what
    // other modules can import), but docs should be comprehensive.
    const defs_slice = if (options.exposed_names != null)
        module_env.store.sliceDefs(module_env.all_defs)
    else switch (module_env.module_kind) {
        .platform, .hosted => blk: {
            // Platforms and hosted modules: only document explicitly provided items
            const exports_slice = module_env.store.sliceDefs(module_env.exports);
            break :blk if (exports_slice.len > 0)
                exports_slice
            else
                module_env.store.sliceDefs(module_env.all_defs);
        },
        .type_module, .default_app, .app, .package, .module, .malformed => module_env.store.sliceDefs(module_env.all_defs),
    };

    for (defs_slice) |def_idx| {
        if (options.exposed_names != null) {
            const entry_name = defEntryName(module_env, def_idx) orelse continue;
            if (!isUnderExposedName(&exposed_names, entry_name)) continue;
        }
        if (options.public_type) |projection| {
            const entry_name = defEntryName(module_env, def_idx) orelse continue;
            if (!nameBelongsToProjection(module_env, projection, entry_name)) continue;
        }
        if (try extractDefEntry(gpa, module_env, local_module_path, reference_routing, def_idx, source, line_index)) |entry| {
            var public_entry = entry;
            var public_entry_moved = false;
            errdefer if (!public_entry_moved) public_entry.deinit(gpa);
            if (options.public_type) |projection| {
                try rebaseEntryForProjection(gpa, module_env, projection, &public_entry);
            }
            // Skip internal Builtin functions
            if (entryIsUndocumented(package_name, module_env.module_name, public_entry.name)) {
                public_entry.deinit(gpa);
                public_entry_moved = true;
            } else {
                try entries_list.append(gpa, public_entry);
                public_entry_moved = true;
            }
        }
    }

    // Also scan all_statements for type declarations (alias, nominal)
    // that may not appear in the defs list
    const stmts_slice = module_env.store.sliceStatements(module_env.all_statements);
    for (stmts_slice) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_alias_decl => |decl| {
                const header = module_env.store.getTypeHeader(decl.header);
                const entry_name = module_env.getIdentText(header.relative_name);
                if (options.exposed_names != null and !isUnderExposedName(&exposed_names, entry_name)) continue;
                if (options.public_type) |projection| {
                    if (!statementBelongsToProjection(module_env, projection, stmt_idx, entry_name)) continue;
                }
                // Skip if already in entries
                if (findProjectedEntryByName(entries_list.items, module_env, options.public_type, entry_name)) continue;
                // Skip internal Builtin types
                if (entryIsUndocumented(package_name, module_env.module_name, entry_name)) continue;

                const region = module_env.store.getStatementRegion(stmt_idx);
                const doc_extract = try extractDocComment(gpa, source, region.start.offset, line_index);
                errdefer if (doc_extract) |d| gpa.free(d.text);

                const type_sig = try extractDeclTypeSig(gpa, module_env, local_module_path, reference_routing, decl.anno);
                errdefer if (type_sig) |s| {
                    s.deinit(gpa);
                    gpa.destroy(s);
                };

                const duped_name = try projectedEntryName(gpa, module_env, options.public_type, entry_name);
                errdefer gpa.free(duped_name);

                const empty_children = try gpa.alloc(DocModel.DocEntry, 0);
                errdefer gpa.free(empty_children);

                try entries_list.append(gpa, DocModel.DocEntry{
                    .name = duped_name,
                    .kind = .alias,
                    .type_signature = type_sig,
                    .doc_comment = if (doc_extract) |d| d.text else null,
                    .children = empty_children,
                    .doc_comment_start_line = if (doc_extract) |d| d.start_line else 0,
                });
            },
            .s_nominal_decl => |decl| {
                const header = module_env.store.getTypeHeader(decl.header);
                const entry_name = module_env.getIdentText(header.relative_name);
                if (options.exposed_names != null and !isUnderExposedName(&exposed_names, entry_name)) continue;
                if (options.public_type) |projection| {
                    if (!statementBelongsToProjection(module_env, projection, stmt_idx, entry_name)) continue;
                }
                if (findProjectedEntryByName(entries_list.items, module_env, options.public_type, entry_name)) continue;
                // Skip internal Builtin types
                if (entryIsUndocumented(package_name, module_env.module_name, entry_name)) continue;

                const region = module_env.store.getStatementRegion(stmt_idx);
                const doc_extract = try extractDocComment(gpa, source, region.start.offset, line_index);
                errdefer if (doc_extract) |d| gpa.free(d.text);

                const type_sig = try extractDeclTypeSig(gpa, module_env, local_module_path, reference_routing, decl.anno);
                errdefer if (type_sig) |s| {
                    s.deinit(gpa);
                    gpa.destroy(s);
                };

                const duped_name = try projectedEntryName(gpa, module_env, options.public_type, entry_name);
                errdefer gpa.free(duped_name);

                const type_header = try render_type.renderTypeHeaderToString(gpa, module_env, decl.header);
                errdefer gpa.free(type_header);

                const empty_children = try gpa.alloc(DocModel.DocEntry, 0);
                errdefer gpa.free(empty_children);

                try entries_list.append(gpa, DocModel.DocEntry{
                    .name = duped_name,
                    .type_header = type_header,
                    .kind = if (decl.is_opaque) .@"opaque" else .nominal,
                    .type_signature = type_sig,
                    .doc_comment = if (doc_extract) |d| d.text else null,
                    .children = empty_children,
                    .doc_comment_start_line = if (doc_extract) |d| d.start_line else 0,
                });
            },
            .s_where_alias_decl => |decl| {
                const header = module_env.store.getTypeHeader(decl.header);
                const entry_name = module_env.getIdentText(header.relative_name);
                if (options.exposed_names != null and !isUnderExposedName(&exposed_names, entry_name)) continue;
                if (options.public_type) |projection| {
                    if (!statementBelongsToProjection(module_env, projection, stmt_idx, entry_name)) continue;
                }
                if (findProjectedEntryByName(entries_list.items, module_env, options.public_type, entry_name)) continue;
                if (entryIsUndocumented(package_name, module_env.module_name, entry_name)) continue;

                const region = module_env.store.getStatementRegion(stmt_idx);
                const doc_extract = try extractDocComment(gpa, source, region.start.offset, line_index);
                errdefer if (doc_extract) |d| gpa.free(d.text);

                // The signature is the receiver together with every constraint
                // the alias names, so the docs list what an implementor must
                // provide.
                const receiver = try extractTypeAnnoAsDocType(gpa, module_env, local_module_path, reference_routing, decl.receiver);
                var receiver_moved = false;
                errdefer if (receiver) |r| if (!receiver_moved) {
                    r.deinit(gpa);
                    gpa.destroy(r);
                };
                const type_sig: ?*const DocType = if (receiver) |r| wrap: {
                    // A where alias with no renderable constraints still has a
                    // receiver to show, and `wrapInWhereClause` leaves it owned
                    // by this caller.
                    const wrapped = try wrapInWhereClause(gpa, module_env, local_module_path, reference_routing, r, decl.where);
                    receiver_moved = true;
                    break :wrap wrapped orelse r;
                } else null;
                errdefer if (type_sig) |sig| {
                    sig.deinit(gpa);
                    gpa.destroy(sig);
                };

                const duped_name = try projectedEntryName(gpa, module_env, options.public_type, entry_name);
                errdefer gpa.free(duped_name);

                const type_header = try render_type.renderTypeHeaderToString(gpa, module_env, decl.header);
                errdefer gpa.free(type_header);

                const empty_children = try gpa.alloc(DocModel.DocEntry, 0);
                errdefer gpa.free(empty_children);

                try entries_list.append(gpa, DocModel.DocEntry{
                    .name = duped_name,
                    .type_header = type_header,
                    .kind = .where_alias,
                    .type_signature = type_sig,
                    .doc_comment = if (doc_extract) |d| d.text else null,
                    .children = empty_children,
                    .doc_comment_start_line = if (doc_extract) |d| d.start_line else 0,
                });
            },
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
            .s_break,
            .s_return,
            .s_import,
            .s_infinite_loop,
            .s_breakable_loop,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => {},
        }
    }

    // Build hierarchical structure: move methods under their parent types
    var i: usize = 0;
    while (i < entries_list.items.len) {
        const entry = &entries_list.items[i];

        // Check if this is a method (name contains ".")
        if (std.mem.findScalarLast(u8, entry.name, '.')) |dot_idx| {
            const parent_name = entry.name[0..dot_idx];
            const method_short_name = entry.name[dot_idx + 1 ..];

            // Find parent type in entries_list
            var parent_idx_opt: ?usize = null;
            for (entries_list.items, 0..) |*potential_parent, idx| {
                if (std.mem.eql(u8, potential_parent.name, parent_name)) {
                    parent_idx_opt = idx;
                    break;
                }
            }

            if (parent_idx_opt) |parent_idx| {
                const parent = &entries_list.items[parent_idx];

                const method_entry = try moveEntryForReparenting(gpa, entry, method_short_name);

                // Add to parent's children
                try appendChildEntry(gpa, parent, method_entry);

                // Remove from top-level list (preserving source order)
                var removed = entries_list.orderedRemove(i);
                removed.deinit(gpa);
                continue; // Don't increment i, check same position again
            }
        }

        i += 1;
    }

    // Type module visibility: only the main type entry and its children are public.
    // Any other top-level definitions are private helpers and should be excluded.
    // For example, in Color.roc only `Color := ...` and its methods are visible;
    // helper functions or types defined outside `Color` are not documented.
    //
    // The Builtin module is a special case—it contains many top-level types
    // (Str, List, Bool, etc.) that need complex re-parenting rather than simple
    // filtering, so it is handled separately below.
    const is_builtin = std.mem.eql(u8, module_env.module_name, "Builtin");
    if (module_env.module_kind == .type_module and !is_builtin) {
        try filterTypeModuleEntries(gpa, &entries_list, local_module_name);
    }

    // Re-parent Builtin opaque type's children to their proper parent types.
    // The Builtin module has a single opaque "Builtin" entry with hundreds of
    // children like "Bool.not", "List.append", "Num.Dec.abs". We split those
    // dotted names and move each child under the matching top-level type.
    if (is_builtin) {
        try reparentBuiltinChildren(gpa, &entries_list);
    }

    const entries = try entries_list.toOwnedSlice(gpa);

    const duped_source_path: ?[]const u8 = if (source_path) |p| try gpa.dupe(u8, p) else null;
    errdefer if (duped_source_path) |p| gpa.free(p);

    return DocModel.ModuleDocs{
        .name = name,
        .package_name = pkg_name,
        .kind = kind,
        .module_doc = module_doc,
        .entries = entries,
        .source_path = duped_source_path,
        .module_doc_start_line = module_doc_start_line,
    };
}

fn projectionRootName(module_env: *const ModuleEnv, projection: PublicTypeProjection) []const u8 {
    std.debug.assert(module_env == projection.source_env);
    return typeDeclName(projection.source_env, projection.source_decl) orelse unreachable;
}

fn typeDeclName(module_env: *const ModuleEnv, statement_idx: CIR.Statement.Idx) ?[]const u8 {
    const statement = module_env.store.getStatement(statement_idx);
    const header_idx = switch (statement) {
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
        .s_break,
        .s_return,
        .s_import,
        .s_infinite_loop,
        .s_breakable_loop,
        .s_type_anno,
        .s_type_var_alias,
        .s_runtime_error,
        => return null,
    };
    return module_env.getIdentText(module_env.store.getTypeHeader(header_idx).relative_name);
}

fn nameIsAtOrUnder(root: []const u8, name: []const u8) bool {
    return std.mem.eql(u8, name, root) or
        (std.mem.startsWith(u8, name, root) and name.len > root.len and name[root.len] == '.');
}

fn nameBelongsToProjection(
    module_env: *const ModuleEnv,
    projection: PublicTypeProjection,
    name: []const u8,
) bool {
    return nameIsAtOrUnder(projectionRootName(module_env, projection), name);
}

fn statementBelongsToProjection(
    module_env: *const ModuleEnv,
    projection: PublicTypeProjection,
    statement: CIR.Statement.Idx,
    name: []const u8,
) bool {
    return statement == projection.source_decl or nameBelongsToProjection(module_env, projection, name);
}

fn projectedEntryName(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    projection: ?PublicTypeProjection,
    source_name: []const u8,
) Allocator.Error![]const u8 {
    const selected = projection orelse return try gpa.dupe(u8, source_name);
    const root = projectionRootName(module_env, selected);
    std.debug.assert(nameIsAtOrUnder(root, source_name));
    return if (source_name.len == root.len)
        try gpa.dupe(u8, selected.public_name)
    else
        try std.fmt.allocPrint(gpa, "{s}{s}", .{ selected.public_name, source_name[root.len..] });
}

fn rebaseEntryForProjection(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    projection: PublicTypeProjection,
    entry: *DocModel.DocEntry,
) Allocator.Error!void {
    const rebased = try projectedEntryName(gpa, module_env, projection, entry.name);
    gpa.free(entry.name);
    entry.name = rebased;
}

fn findProjectedEntryByName(
    entries: []const DocModel.DocEntry,
    module_env: *const ModuleEnv,
    projection: ?PublicTypeProjection,
    source_name: []const u8,
) bool {
    const selected = projection orelse return findEntryByName(entries, source_name);
    const root = projectionRootName(module_env, selected);
    if (!nameIsAtOrUnder(root, source_name)) return false;
    const suffix = source_name[root.len..];
    for (entries) |entry| {
        if (entry.name.len != selected.public_name.len + suffix.len) continue;
        if (!std.mem.startsWith(u8, entry.name, selected.public_name)) continue;
        if (std.mem.eql(u8, entry.name[selected.public_name.len..], suffix)) return true;
    }
    return false;
}

fn projectedTypeReference(
    module_env: *const ModuleEnv,
    current_projection: ?PublicTypeProjection,
    public_types: []const PublicTypeProjection,
    checked_artifact: ?*const CheckedArtifact.CheckedModuleArtifact,
    base_ref: TypeAnno.LocalOrExternal,
) ?PublicTypeProjection {
    if (current_projection == null and public_types.len == 0) return null;

    const identity, const target_statement = switch (base_ref) {
        .local => |local| .{
            (module_env.contentIdentityHash() orelse unreachable).*,
            local.decl_idx,
        },
        .external => |external| external_blk: {
            const artifact = checked_artifact orelse unreachable;
            const import_index = @intFromEnum(external.module_idx);
            if (import_index >= artifact.checking_context_identity.imports.len) unreachable;
            const import_key = artifact.checking_context_identity.imports[import_index].artifact_key orelse unreachable;
            break :external_blk .{ import_key.module_identity_hash, @as(CIR.Statement.Idx, @enumFromInt(external.target_node_idx)) };
        },
        .builtin => return null,
        .pending => unreachable,
    };

    return selectPublicProjection(current_projection, public_types, &identity, target_statement);
}

fn selectPublicProjection(
    current_projection: ?PublicTypeProjection,
    public_types: []const PublicTypeProjection,
    identity: *const [32]u8,
    target_statement: CIR.Statement.Idx,
) ?PublicTypeProjection {
    if (current_projection) |current| {
        if (projectionContainsIdentityAndStatement(current, identity, target_statement)) return current;
    }

    var low: usize = 0;
    var high = public_types.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        if (std.mem.order(u8, public_types[mid].source_identity, identity) == .lt)
            low = mid + 1
        else
            high = mid;
    }

    var selected: ?PublicTypeProjection = null;
    var selected_root_len: usize = 0;
    for (public_types[low..]) |candidate| {
        if (!std.mem.eql(u8, candidate.source_identity, identity)) break;
        if (!projectionContainsIdentityAndStatement(candidate, identity, target_statement)) continue;
        const root_len = projectionRootName(candidate.source_env, candidate).len;
        // The nearest public root is the exact namespace owner. Equal roots
        // retain source public-surface order, already established by sorting.
        if (selected == null or root_len > selected_root_len) {
            selected = candidate;
            selected_root_len = root_len;
        }
    }
    return selected;
}

fn typeReferenceStatement(base_ref: TypeAnno.LocalOrExternal) ?CIR.Statement.Idx {
    return switch (base_ref) {
        .local => |local| local.decl_idx,
        .external => |external| @enumFromInt(external.target_node_idx),
        .builtin, .pending => null,
    };
}

fn annotatedTypeReferenceDisplay(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    routing: PublicReferenceRouting,
    base_ref: TypeAnno.LocalOrExternal,
    source_name: []const u8,
) Allocator.Error!TypeReferenceDisplay {
    if (projectedTypeReference(
        module_env,
        routing.current,
        routing.all,
        routing.checked_artifact,
        base_ref,
    )) |projection| {
        const statement = typeReferenceStatement(base_ref) orelse unreachable;
        const target_name = typeDeclName(projection.source_env, statement) orelse unreachable;
        const module_path = try std.fmt.allocPrint(gpa, "{s}.{s}", .{ projection.package_name, projection.public_name });
        errdefer gpa.free(module_path);
        return .{
            .module_path = module_path,
            .type_name = try projectedEntryName(gpa, projection.source_env, projection, target_name),
        };
    }

    const module_path = try gpa.dupe(u8, resolveModulePathFromBase(module_env, local_module_path, base_ref));
    errdefer gpa.free(module_path);
    return .{
        .module_path = module_path,
        .type_name = try gpa.dupe(u8, source_name),
    };
}

fn projectionContainsIdentityAndStatement(
    projection: PublicTypeProjection,
    identity: *const [32]u8,
    statement: CIR.Statement.Idx,
) bool {
    if (!std.mem.eql(u8, projection.source_identity, identity)) return false;
    const type_name = typeDeclName(projection.source_env, statement) orelse return false;
    return statementBelongsToProjection(projection.source_env, projection, statement, type_name);
}

fn isUnderExposedName(exposed_names: *const std.StringHashMapUnmanaged(void), name: []const u8) bool {
    const root_name = if (std.mem.findScalar(u8, name, '.')) |dot| name[0..dot] else name;
    return exposed_names.contains(root_name);
}

/// Filter entries in a type module to only include the main type and its children.
///
/// In a type module (e.g. `Color.roc`), only the entry whose name matches the
/// module name is public. All other top-level entries—helper functions, internal
/// types, etc.—are private to the module and excluded from documentation.
/// The main type entry's children (methods defined inside `Color := [...].{...}`)
/// are preserved as nested entries.
fn filterTypeModuleEntries(
    gpa: Allocator,
    entries_list: *std.ArrayList(DocModel.DocEntry),
    module_name: []const u8,
) Allocator.Error!void {
    var idx: usize = 0;
    while (idx < entries_list.items.len) {
        const entry = &entries_list.items[idx];
        // At this point the hierarchical pass has already moved associated
        // entries under their parent. Anything still at top level is a sibling
        // declaration, so a type module keeps only its main type entry.
        if (std.mem.eql(u8, entry.name, module_name)) {
            idx += 1;
        } else {
            var removed = entries_list.orderedRemove(idx);
            removed.deinit(gpa);
        }
    }
}

fn reparentBuiltinChildren(gpa: Allocator, entries_list: *std.ArrayList(DocModel.DocEntry)) Allocator.Error!void {
    // Find the Builtin opaque entry
    var builtin_idx: ?usize = null;
    for (entries_list.items, 0..) |*entry, idx| {
        if (entry.kind == .@"opaque" and std.mem.eql(u8, entry.name, "Builtin")) {
            builtin_idx = idx;
            break;
        }
    }
    const bi = builtin_idx orelse return;
    const builtin_children = entries_list.items[bi].children;

    // Process each child—move it under its proper parent
    for (builtin_children) |child| {
        try reparentDottedChild(gpa, entries_list, child);
    }

    // Free the Builtin entry's children array (entries were moved out)
    gpa.free(builtin_children);
    entries_list.items[bi].children = try gpa.alloc(DocModel.DocEntry, 0);

    // Remove the Builtin entry itself (preserving source order)
    var builtin_entry = entries_list.orderedRemove(bi);
    builtin_entry.deinit(gpa);

    // Also strip "Builtin." prefix from top-level entries and re-parent them.
    const prefix = "Builtin.";
    for (entries_list.items) |*entry| {
        if (std.mem.startsWith(u8, entry.name, prefix)) {
            const old_name = entry.name;
            const stripped = old_name[prefix.len..];
            const new_name = try gpa.dupe(u8, stripped);
            gpa.free(old_name);
            entry.name = new_name;
        }
    }

    // Second pass: re-parent dotted entries under their parent types
    var j: usize = 0;
    while (j < entries_list.items.len) {
        const entry = &entries_list.items[j];
        if (std.mem.findScalar(u8, entry.name, '.')) |dot_idx| {
            const parent_name = entry.name[0..dot_idx];
            const method_short_name = entry.name[dot_idx + 1 ..];

            var parent_idx_opt: ?usize = null;
            for (entries_list.items, 0..) |*potential_parent, idx| {
                if (idx != j and std.mem.eql(u8, potential_parent.name, parent_name)) {
                    parent_idx_opt = idx;
                    break;
                }
            }

            if (parent_idx_opt) |parent_idx| {
                const parent_ptr = &entries_list.items[parent_idx];
                const method_entry = try moveEntryForReparenting(gpa, entry, method_short_name);

                if (std.mem.findScalar(u8, method_short_name, '.')) |_| {
                    var children_list = std.ArrayList(DocModel.DocEntry).empty;
                    for (parent_ptr.children) |c| {
                        try children_list.append(gpa, c);
                    }
                    gpa.free(parent_ptr.children);
                    try reparentDottedChildInto(gpa, &children_list, method_entry);
                    parent_ptr.children = try children_list.toOwnedSlice(gpa);
                } else {
                    try appendChildEntry(gpa, parent_ptr, method_entry);
                }

                var removed = entries_list.orderedRemove(j);
                removed.deinit(gpa);
                continue;
            }
        }
        j += 1;
    }

    // Remove top-level value entries that are not part of the public API.
    var k: usize = 0;
    while (k < entries_list.items.len) {
        const entry = &entries_list.items[k];
        if (entry.kind == .value and entry.children.len == 0) {
            var removed = entries_list.orderedRemove(k);
            removed.deinit(gpa);
            continue;
        }
        k += 1;
    }
}

/// Recursively re-parent a child with a dotted name into the correct position in entries_list.
fn reparentDottedChild(
    gpa: Allocator,
    entries_list: *std.ArrayList(DocModel.DocEntry),
    child: DocModel.DocEntry,
) Allocator.Error!void {
    const dot_idx = std.mem.findScalar(u8, child.name, '.') orelse {
        try entries_list.append(gpa, child);
        return;
    };

    const parent_name = child.name[0..dot_idx];
    const remainder = child.name[dot_idx + 1 ..];

    var parent: ?*DocModel.DocEntry = null;
    for (entries_list.items) |*entry| {
        if (std.mem.eql(u8, entry.name, parent_name)) {
            parent = entry;
            break;
        }
    }

    if (parent == null) {
        const group_name = try gpa.dupe(u8, parent_name);
        errdefer gpa.free(group_name);
        const empty = try gpa.alloc(DocModel.DocEntry, 0);
        errdefer gpa.free(empty);

        try entries_list.append(gpa, DocModel.DocEntry{
            .name = group_name,
            .kind = .nominal,
            .type_signature = null,
            .doc_comment = null,
            .children = empty,
        });
        parent = &entries_list.items[entries_list.items.len - 1];
    }

    const p = parent.?;

    var new_child = child;
    const short_name = try gpa.dupe(u8, remainder);
    gpa.free(child.name);
    new_child.name = short_name;

    if (std.mem.findScalar(u8, remainder, '.')) |_| {
        var children_list = std.ArrayList(DocModel.DocEntry).empty;
        for (p.children) |c| {
            try children_list.append(gpa, c);
        }
        gpa.free(p.children);
        try reparentDottedChildInto(gpa, &children_list, new_child);
        p.children = try children_list.toOwnedSlice(gpa);
    } else {
        try appendChildEntry(gpa, p, new_child);
    }
}

/// Like reparentDottedChild but operates on a children ArrayList (for nested levels).
fn reparentDottedChildInto(
    gpa: Allocator,
    children_list: *std.ArrayList(DocModel.DocEntry),
    child: DocModel.DocEntry,
) Allocator.Error!void {
    const dot_idx = std.mem.findScalar(u8, child.name, '.') orelse {
        try children_list.append(gpa, child);
        return;
    };

    const parent_name = child.name[0..dot_idx];
    const remainder = child.name[dot_idx + 1 ..];

    var parent: ?*DocModel.DocEntry = null;
    for (children_list.items) |*entry| {
        if (std.mem.eql(u8, entry.name, parent_name)) {
            parent = entry;
            break;
        }
    }

    if (parent == null) {
        const group_name = try gpa.dupe(u8, parent_name);
        errdefer gpa.free(group_name);
        const empty = try gpa.alloc(DocModel.DocEntry, 0);
        errdefer gpa.free(empty);

        try children_list.append(gpa, DocModel.DocEntry{
            .name = group_name,
            .kind = .nominal,
            .type_signature = null,
            .doc_comment = null,
            .children = empty,
        });
        parent = &children_list.items[children_list.items.len - 1];
    }

    const p = parent.?;

    var new_child = child;
    const short_name = try gpa.dupe(u8, remainder);
    gpa.free(child.name);
    new_child.name = short_name;

    if (std.mem.findScalar(u8, remainder, '.')) |_| {
        var sub_children = std.ArrayList(DocModel.DocEntry).empty;
        for (p.children) |c| {
            try sub_children.append(gpa, c);
        }
        gpa.free(p.children);
        try reparentDottedChildInto(gpa, &sub_children, new_child);
        p.children = try sub_children.toOwnedSlice(gpa);
    } else {
        try appendChildEntry(gpa, p, new_child);
    }
}

// --- Internal helpers ---

fn defEntryName(module_env: *const ModuleEnv, def_idx: CIR.Def.Idx) ?[]const u8 {
    const def = module_env.store.getDef(def_idx);
    return switch (module_env.store.getPattern(def.pattern)) {
        .assign => |assign| module_env.getIdentText(assign.ident),
        .nominal => |nominal| switch (module_env.store.getStatement(nominal.nominal_type_decl)) {
            .s_nominal_decl => |decl| module_env.getIdentText(module_env.store.getTypeHeader(decl.header).relative_name),
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
            .s_break,
            .s_return,
            .s_import,
            .s_infinite_loop,
            .s_breakable_loop,
            .s_alias_decl,
            .s_where_alias_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => null,
        },
        .as,
        .applied_tag,
        .nominal_external,
        .record_destructure,
        .list,
        .tuple,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .num_from_numeral_literal,
        .str_literal,
        .str_interpolation,
        .underscore,
        .runtime_error,
        => null,
    };
}

fn extractDefEntry(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    def_idx: CIR.Def.Idx,
    source: []const u8,
    line_index: LineIndex,
) Allocator.Error!?DocModel.DocEntry {
    const def = module_env.store.getDef(def_idx);
    const pattern = module_env.store.getPattern(def.pattern);

    switch (pattern) {
        .assign => |a| {
            const ident_name = module_env.getIdentText(a.ident);
            const duped_name = try gpa.dupe(u8, ident_name);
            errdefer gpa.free(duped_name);

            // Get the byte offset for doc comment scanning
            const offset = getDefSourceOffset(module_env, def);
            const doc_extract = try extractDocComment(gpa, source, offset, line_index);
            errdefer if (doc_extract) |d| gpa.free(d.text);

            // For annotated definitions, render the checked source annotation.
            // That is the signature the user wrote, and it avoids re-walking
            // recursive inferred type graphs when explicit documentation data
            // is already available in CIR.
            const type_sig: ?*const DocType = blk: {
                if (def.annotation) |anno_idx| {
                    break :blk try extractAnnotationAsDocType(gpa, module_env, local_module_path, reference_routing, anno_idx);
                }

                const def_var = ModuleEnv.varFrom(def_idx);
                if (@intFromEnum(def_var) >= module_env.types.len()) break :blk null;
                break :blk try extractDocType(
                    gpa,
                    &module_env.types,
                    module_env,
                    local_module_path,
                    reference_routing,
                    def_var,
                );
            };
            errdefer if (type_sig) |s| {
                s.deinit(gpa);
                gpa.destroy(s);
            };

            const empty_children = try gpa.alloc(DocModel.DocEntry, 0);
            errdefer gpa.free(empty_children);

            return DocModel.DocEntry{
                .name = duped_name,
                .kind = .value,
                .type_signature = type_sig,
                .doc_comment = if (doc_extract) |d| d.text else null,
                .children = empty_children,
                .doc_comment_start_line = if (doc_extract) |d| d.start_line else 0,
            };
        },
        .nominal => |n| {
            const stmt = module_env.store.getStatement(n.nominal_type_decl);
            switch (stmt) {
                .s_nominal_decl => |decl| {
                    const header = module_env.store.getTypeHeader(decl.header);
                    const entry_name = module_env.getIdentText(header.relative_name);
                    const duped_name = try gpa.dupe(u8, entry_name);
                    errdefer gpa.free(duped_name);

                    const type_header = try render_type.renderTypeHeaderToString(gpa, module_env, decl.header);
                    errdefer gpa.free(type_header);

                    // Use the statement region for doc comment scanning
                    const region = module_env.store.getStatementRegion(n.nominal_type_decl);
                    const doc_extract = try extractDocComment(gpa, source, region.start.offset, line_index);
                    errdefer if (doc_extract) |d| gpa.free(d.text);

                    const type_sig = try extractDeclTypeSig(gpa, module_env, local_module_path, reference_routing, decl.anno);
                    errdefer if (type_sig) |s| {
                        s.deinit(gpa);
                        gpa.destroy(s);
                    };

                    // Extract children for nominal types with record backing
                    const children = try extractNominalChildren(gpa, module_env, def);
                    errdefer {
                        for (children) |*c| @constCast(c).deinit(gpa);
                        gpa.free(children);
                    }

                    return DocModel.DocEntry{
                        .name = duped_name,
                        .type_header = type_header,
                        .kind = if (decl.is_opaque) .@"opaque" else .nominal,
                        .type_signature = type_sig,
                        .doc_comment = if (doc_extract) |d| d.text else null,
                        .children = children,
                        .doc_comment_start_line = if (doc_extract) |d| d.start_line else 0,
                    };
                },
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
                .s_break,
                .s_return,
                .s_import,
                .s_infinite_loop,
                .s_breakable_loop,
                .s_alias_decl,
                .s_where_alias_decl,
                .s_type_anno,
                .s_type_var_alias,
                .s_runtime_error,
                => return null,
            }
        },
        .as,
        .applied_tag,
        .nominal_external,
        .record_destructure,
        .list,
        .tuple,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .num_from_numeral_literal,
        .str_literal,
        .str_interpolation,
        .underscore,
        .runtime_error,
        => return null,
    }
}

fn getDefSourceOffset(module_env: *const ModuleEnv, def: CIR.Def) u32 {
    // If there's an annotation, find the start of the line containing the type
    // annotation. The TypeAnno region points into the middle of the annotation line
    // (e.g. at `Str -> Str` in `greet : Str -> Str`), so we scan backwards to the
    // line start. This ensures extractDocComment sees the line boundary correctly.
    if (def.annotation) |anno_idx| {
        const source = module_env.getSourceAll();
        const annotation = module_env.store.getAnnotation(anno_idx);
        const type_anno_region = module_env.store.getTypeAnnoRegion(annotation.anno);
        var pos: usize = type_anno_region.start.offset;
        // Scan backwards to the start of this line
        while (pos > 0 and source[pos - 1] != '\n') {
            pos -= 1;
        }
        return @intCast(pos);
    }
    // Otherwise use the pattern's region
    const region = module_env.store.getPatternRegion(def.pattern);
    return region.start.offset;
}

fn extractNominalChildren(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    def: CIR.Def,
) Allocator.Error![]DocModel.DocEntry {
    const expr = module_env.store.getExpr(def.expr);
    if (std.meta.activeTag(expr) == .e_nominal) {
        const backing = module_env.store.getExpr(expr.e_nominal.backing_expr);
        if (std.meta.activeTag(backing) == .e_record) {
            return try extractRecordChildren(gpa, module_env, backing.e_record.fields);
        }
    }
    return try gpa.alloc(DocModel.DocEntry, 0);
}

fn extractRecordChildren(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    fields: CIR.RecordField.Span,
) Allocator.Error![]DocModel.DocEntry {
    const fields_slice = module_env.store.sliceRecordFields(fields);
    var children = std.ArrayList(DocModel.DocEntry).empty;
    errdefer {
        for (children.items) |*c| c.deinit(gpa);
        children.deinit(gpa);
    }

    for (fields_slice) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        const field_name = try gpa.dupe(u8, module_env.getIdentText(field.name));
        errdefer gpa.free(field_name);

        const empty_children = try gpa.alloc(DocModel.DocEntry, 0);
        errdefer gpa.free(empty_children);

        try children.append(gpa, DocModel.DocEntry{
            .name = field_name,
            .kind = .value,
            .type_signature = null,
            .doc_comment = null,
            .children = empty_children,
        });
    }

    return children.toOwnedSlice(gpa);
}

/// Build a structured DocType for a type declaration (alias/nominal/opaque).
///
/// Extracts the backing type from a declaration's type annotation.
fn extractDeclTypeSig(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    anno_idx: CIR.TypeAnno.Idx,
) Allocator.Error!?*const DocType {
    // Extract the backing type from the CIR annotation. The inferred type for a
    // nominal resolves to the nominal itself, so we use the annotation instead.
    // DocEntry.writeToSExpr generates the declaration prefix from kind + name.
    return try extractTypeAnnoAsDocType(gpa, module_env, local_module_path, reference_routing, anno_idx);
}

fn extractAnnotationAsDocType(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    annotation_idx: CIR.Annotation.Idx,
) Allocator.Error!?*const DocType {
    const annotation = module_env.store.getAnnotation(annotation_idx);
    const base_type = try extractTypeAnnoAsDocType(gpa, module_env, local_module_path, reference_routing, annotation.anno) orelse return null;
    var base_type_moved = false;
    errdefer if (!base_type_moved) {
        base_type.deinit(gpa);
        gpa.destroy(base_type);
    };

    const where_span = annotation.where orelse return base_type;
    const wrapped = try wrapInWhereClause(gpa, module_env, local_module_path, reference_routing, base_type, where_span) orelse return base_type;
    base_type_moved = true;
    return wrapped;
}

/// Wrap a type in the constraints of a where clause. Returns null when the
/// clause contributes nothing renderable, leaving ownership of `base_type` with
/// the caller.
fn wrapInWhereClause(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    base_type: *const DocType,
    where_span: CIR.WhereClause.Span,
) Allocator.Error!?*const DocType {
    var constraints = std.ArrayList(DocType.Constraint).empty;
    defer {
        for (constraints.items) |constraint| constraint.deinit(gpa);
        constraints.deinit(gpa);
    }

    for (module_env.store.sliceWhereClauses(where_span)) |where_idx| {
        const where_clause = module_env.store.getWhereClause(where_idx);
        switch (where_clause) {
            .w_method => |method| {
                const constraint = try extractWhereMethodConstraint(gpa, module_env, local_module_path, reference_routing, method);
                errdefer constraint.deinit(gpa);
                try constraints.append(gpa, constraint);
            },
            .w_alias => |alias| {
                const constraint = try extractWhereAliasConstraint(gpa, module_env, local_module_path, reference_routing, alias) orelse continue;
                errdefer constraint.deinit(gpa);
                try constraints.append(gpa, constraint);
            },
            .w_malformed => {},
        }
    }

    if (constraints.items.len == 0) return null;

    const owned_constraints = try gpa.alloc(DocType.Constraint, constraints.items.len);
    var constraints_moved = false;
    errdefer if (!constraints_moved) {
        for (owned_constraints) |constraint| constraint.deinit(gpa);
        gpa.free(owned_constraints);
    };
    @memcpy(owned_constraints, constraints.items);
    constraints.clearRetainingCapacity();

    const wrapped = try allocDocType(gpa, .{ .where_clause = .{
        .type = base_type,
        .constraints = owned_constraints,
        .layout = sourceWhereClauseLayout(module_env, where_span),
    } });
    constraints_moved = true;
    return wrapped;
}

fn extractWhereAliasConstraint(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    alias: @TypeOf(@as(CIR.WhereClause, undefined).w_alias),
) Allocator.Error!?DocType.Constraint {
    const type_var = try extractWhereTypeVarName(gpa, module_env, alias.var_);
    errdefer gpa.free(type_var);

    const reference = try extractTypeAnnoAsDocType(gpa, module_env, local_module_path, reference_routing, alias.alias) orelse {
        gpa.free(type_var);
        return null;
    };

    return .{ .where_alias = .{ .type_var = type_var, .alias = reference } };
}

fn extractWhereMethodConstraint(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    method: @TypeOf(@as(CIR.WhereClause, undefined).w_method),
) Allocator.Error!DocType.Constraint {
    const type_var = try extractWhereTypeVarName(gpa, module_env, method.var_);
    errdefer gpa.free(type_var);

    const method_name = try gpa.dupe(u8, module_env.getIdentText(method.method_name));
    errdefer gpa.free(method_name);

    const signature = try extractWhereMethodSignature(gpa, module_env, local_module_path, reference_routing, method);
    errdefer {
        signature.deinit(gpa);
        gpa.destroy(signature);
    }

    return .{ .method = .{
        .type_var = type_var,
        .method_name = method_name,
        .signature = signature,
    } };
}

fn extractWhereMethodSignature(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    method: @TypeOf(@as(CIR.WhereClause, undefined).w_method),
) Allocator.Error!*const DocType {
    const args_slice = module_env.store.sliceTypeAnnos(method.args);
    const args = try gpa.alloc(*const DocType, args_slice.len);
    var args_len: usize = 0;
    var args_moved = false;
    errdefer if (!args_moved) {
        for (args[0..args_len]) |arg| {
            arg.deinit(gpa);
            gpa.destroy(arg);
        }
        gpa.free(args);
    };

    for (args_slice) |arg_idx| {
        args[args_len] = try extractTypeAnnoAsDocType(gpa, module_env, local_module_path, reference_routing, arg_idx) orelse
            try allocDocType(gpa, .@"error");
        args_len += 1;
    }

    const ret = try extractTypeAnnoAsDocType(gpa, module_env, local_module_path, reference_routing, method.ret) orelse
        try allocDocType(gpa, .@"error");
    var ret_moved = false;
    errdefer if (!ret_moved) {
        ret.deinit(gpa);
        gpa.destroy(ret);
    };

    const signature = try allocDocType(gpa, .{ .function = .{
        .args = args,
        .ret = ret,
        .effectful = method.effectful,
    } });
    args_moved = true;
    ret_moved = true;
    return signature;
}

fn extractWhereTypeVarName(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    var_idx: CIR.TypeAnno.Idx,
) Allocator.Error![]const u8 {
    var current = var_idx;
    while (true) {
        switch (module_env.store.getTypeAnno(current)) {
            .rigid_var => |rv| return try gpa.dupe(u8, module_env.getIdentText(rv.name)),
            .rigid_var_lookup => |rv_lookup| current = rv_lookup.ref,
            .apply,
            .underscore,
            .lookup,
            .tag_union,
            .tag,
            .@"fn",
            .tuple,
            .record,
            .parens,
            .malformed,
            => return try gpa.dupe(u8, "?"),
        }
    }
}

/// Resolve the module path from a CIR TypeAnno's LocalOrExternal base.
fn resolveModulePathFromBase(
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    local_or_ext: TypeAnno.LocalOrExternal,
) []const u8 {
    return switch (local_or_ext) {
        .builtin => "", // Don't expose "Builtin" module as it's an implementation detail
        // Local refs render the display-qualified path ("app.Geometry"), not
        // the identity-qualified ident (which may embed a URL or canonical
        // filesystem path); identity strings are cache keys, not presentation.
        .local => if (!module_env.qualified_module_ident.isNone())
            local_module_path
        else
            module_env.module_name,
        .external => |ext| blk: {
            const idx = @intFromEnum(ext.module_idx);
            if (idx >= module_env.imports.imports.items.items.len) break :blk "";
            const str_idx = module_env.imports.imports.items.items[idx];
            break :blk getModulePath(module_env.common.getString(str_idx));
        },
        .pending => |pend| blk: {
            const idx = @intFromEnum(pend.module_idx);
            if (idx >= module_env.imports.imports.items.items.len) break :blk "";
            const str_idx = module_env.imports.imports.items.items[idx];
            break :blk getModulePath(module_env.common.getString(str_idx));
        },
    };
}

/// Returns true when the annotation is the synthetic `#others` rigid used for
/// anonymous open extensions (`..`). Such extensions should be rendered as
/// `..` with no trailing name, so the caller should skip extracting the ext.
fn isAnonymousOpenExt(module_env: *const ModuleEnv, ext_idx: CIR.TypeAnno.Idx) bool {
    const anno = module_env.store.getTypeAnno(ext_idx);
    if (anno != .rigid_var) return false;
    const name = module_env.getIdentText(anno.rigid_var.name);
    return name.len > 0 and name[0] == '#';
}

/// Recover the parser's explicit collection-layout choice from a CIR source
/// region. A trailing comma requests expanded formatting; line breaks alone do
/// not. Comments after the comma are ignored, matching the parser's token-based
/// decision.
fn sourceCollectionLayout(module_env: *const ModuleEnv, region: base.Region) DocType.Layout {
    return collectionLayoutFromSource(module_env.getSourceAll(), region);
}

fn collectionLayoutFromSource(source: []const u8, region: base.Region) DocType.Layout {
    const start: usize = @min(region.start.offset, source.len);
    const end: usize = @min(region.end.offset, source.len);
    if (start >= end) return .compact;

    var last_significant: ?u8 = null;
    var in_comment = false;
    for (source[start..end]) |byte| {
        if (in_comment) {
            if (byte == '\n' or byte == '\r') in_comment = false;
            continue;
        }
        if (byte == '#') {
            in_comment = true;
            continue;
        }
        if (!std.ascii.isWhitespace(byte) and byte != ')' and byte != ']' and byte != '}') {
            last_significant = byte;
        }
    }

    return if (last_significant == ',') .multiline else .compact;
}

fn sourceWhereClauseLayout(module_env: *const ModuleEnv, where_span: CIR.WhereClause.Span) DocType.Layout {
    const clauses = module_env.store.sliceWhereClauses(where_span);
    if (clauses.len == 0) return .compact;

    const last_node: CIR.Node.Idx = @enumFromInt(@intFromEnum(clauses[clauses.len - 1]));
    const last_region = module_env.store.getNodeRegion(last_node);
    const source = module_env.getSourceAll();
    const start: usize = @min(last_region.end.offset, source.len);

    var in_comment = false;
    for (source[start..]) |byte| {
        if (in_comment) {
            if (byte == '\n' or byte == '\r') in_comment = false;
            continue;
        }
        if (byte == '#') {
            in_comment = true;
            continue;
        }
        if (std.ascii.isWhitespace(byte)) continue;
        return if (byte == ',') .multiline else .compact;
    }
    return .compact;
}

test "collection layout follows trailing commas rather than source newlines" {
    const compact_source = "List(\n    U8\n)";
    try std.testing.expectEqual(
        DocType.Layout.compact,
        collectionLayoutFromSource(compact_source, base.Region.from_raw_offsets(0, compact_source.len)),
    );

    const multiline_source = "List(U8, # preserve expanded layout\n)";
    try std.testing.expectEqual(
        DocType.Layout.multiline,
        collectionLayoutFromSource(multiline_source, base.Region.from_raw_offsets(0, multiline_source.len)),
    );
}

fn sourceTypeLayout(module_env: *const ModuleEnv, anno_idx: CIR.TypeAnno.Idx) DocType.Layout {
    return sourceCollectionLayout(module_env, module_env.store.getTypeAnnoRegion(anno_idx));
}

/// The trimmed source snippet of a defaulted field's default expression
/// (`?? <here>`), duped into an owned slice, or null when the region yields no
/// text—in which case docs render `?? …` (mirroring TypeWriter's fallback).
fn defaultSourceSnippet(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    default_idx: CIR.Expr.Idx,
) Allocator.Error!?[]const u8 {
    const source = module_env.getSourceAll();
    const region = module_env.store.getExprRegion(default_idx);
    const start: usize = @min(region.start.offset, source.len);
    const end: usize = @min(region.end.offset, source.len);
    if (start >= end) return null;
    const snippet = std.mem.trim(u8, source[start..end], &std.ascii.whitespace);
    if (snippet.len == 0) return null;
    return try gpa.dupe(u8, snippet);
}

/// Extract a CIR TypeAnno as a structured DocType.
fn extractTypeAnnoAsDocType(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    type_anno_idx: CIR.TypeAnno.Idx,
) Allocator.Error!?*const DocType {
    const BuildFrame = union(enum) {
        visit: CIR.TypeAnno.Idx,
        malformed_tag,
        finish_apply: struct {
            /// Owned until transferred to the resulting DocType.
            name: []const u8,
            /// Owned until transferred to the resulting DocType.
            module_path: []const u8,
            arg_count: usize,
            layout: DocType.Layout,
        },
        finish_tag: struct {
            name: []const u8,
            arg_count: usize,
            layout: DocType.Layout,
        },
        finish_tag_union: struct {
            tag_count: usize,
            has_ext: bool,
            is_open: bool,
            layout: DocType.Layout,
        },
        finish_tuple: struct {
            elem_count: usize,
            layout: DocType.Layout,
        },
        finish_record: struct {
            fields: []CIR.TypeAnno.RecordField.Idx,
            has_ext: bool,
            is_open: bool,
            layout: DocType.Layout,
        },
        finish_fn: struct {
            arg_count: usize,
            effectful: bool,
        },
    };

    const Builder = struct {
        fn pushResult(results: *std.ArrayList(*const DocType), allocator: Allocator, value: *const DocType) ExtractError!void {
            errdefer {
                value.deinit(allocator);
                allocator.destroy(value);
            }
            try results.append(allocator, value);
        }

        fn cleanupDocTypes(allocator: Allocator, values: []const *const DocType) void {
            for (values) |value| {
                value.deinit(allocator);
                allocator.destroy(value);
            }
        }

        fn cleanupTag(allocator: Allocator, tag: DocType.Tag) void {
            allocator.free(tag.name);
            cleanupDocTypes(allocator, tag.args);
            allocator.free(tag.args);
        }

        fn cleanupFields(allocator: Allocator, fields: []const DocType.Field) void {
            for (fields) |field| {
                allocator.free(field.name);
                if (field.kind == .defaulted) {
                    if (field.kind.defaulted) |snippet| allocator.free(snippet);
                }
                field.type.deinit(allocator);
                allocator.destroy(field.type);
            }
        }

        fn pushVisitsReversed(
            frames: *std.ArrayList(BuildFrame),
            allocator: Allocator,
            children: []const CIR.TypeAnno.Idx,
        ) ExtractError!void {
            var i = children.len;
            while (i > 0) {
                i -= 1;
                try frames.append(allocator, .{ .visit = children[i] });
            }
        }
    };

    var frames = std.ArrayList(BuildFrame).empty;
    defer frames.deinit(gpa);
    errdefer for (frames.items) |pending| switch (pending) {
        .finish_apply => |finish| {
            gpa.free(finish.name);
            gpa.free(finish.module_path);
        },
        .visit,
        .malformed_tag,
        .finish_tag,
        .finish_tag_union,
        .finish_tuple,
        .finish_record,
        .finish_fn,
        => {},
    };

    var results = std.ArrayList(*const DocType).empty;
    defer results.deinit(gpa);
    errdefer Builder.cleanupDocTypes(gpa, results.items);

    try frames.append(gpa, .{ .visit = type_anno_idx });
    while (frames.pop()) |frame| {
        switch (frame) {
            .visit => |idx| {
                const anno = module_env.store.getTypeAnno(idx);
                switch (anno) {
                    .apply => |a| {
                        const args_slice = module_env.store.sliceTypeAnnos(a.args);
                        const display = try annotatedTypeReferenceDisplay(
                            gpa,
                            module_env,
                            local_module_path,
                            reference_routing,
                            a.base,
                            module_env.getIdentText(a.name),
                        );
                        var display_moved = false;
                        errdefer if (!display_moved) {
                            gpa.free(display.module_path);
                            gpa.free(display.type_name);
                        };
                        if (args_slice.len == 0) {
                            const reference = try allocDocType(gpa, .{ .type_ref = .{
                                .module_path = display.module_path,
                                .type_name = display.type_name,
                            } });
                            display_moved = true;
                            try Builder.pushResult(&results, gpa, reference);
                        } else {
                            try frames.append(gpa, .{ .finish_apply = .{
                                .name = display.type_name,
                                .module_path = display.module_path,
                                .arg_count = args_slice.len,
                                .layout = sourceTypeLayout(module_env, idx),
                            } });
                            display_moved = true;
                            try Builder.pushVisitsReversed(&frames, gpa, args_slice);
                        }
                    },
                    .rigid_var => |tv| {
                        try Builder.pushResult(&results, gpa, try allocDocType(gpa, .{
                            .type_var = try gpa.dupe(u8, module_env.getIdentText(tv.name)),
                        }));
                    },
                    .rigid_var_lookup => |rv| {
                        try frames.append(gpa, .{ .visit = rv.ref });
                    },
                    .underscore => {
                        try Builder.pushResult(&results, gpa, try allocDocType(gpa, .wildcard));
                    },
                    .lookup => |t| {
                        const display = try annotatedTypeReferenceDisplay(
                            gpa,
                            module_env,
                            local_module_path,
                            reference_routing,
                            t.base,
                            module_env.getIdentText(t.name),
                        );
                        var display_moved = false;
                        errdefer if (!display_moved) {
                            gpa.free(display.module_path);
                            gpa.free(display.type_name);
                        };
                        const reference = try allocDocType(gpa, .{ .type_ref = .{
                            .module_path = display.module_path,
                            .type_name = display.type_name,
                        } });
                        display_moved = true;
                        try Builder.pushResult(&results, gpa, reference);
                    },
                    .tag_union => |tu| {
                        const tags_slice = module_env.store.sliceTypeAnnos(tu.tags);
                        const has_ext = if (tu.ext) |ext_idx| !isAnonymousOpenExt(module_env, ext_idx) else false;
                        try frames.append(gpa, .{ .finish_tag_union = .{
                            .tag_count = tags_slice.len,
                            .has_ext = has_ext,
                            .is_open = tu.ext != null,
                            .layout = sourceTypeLayout(module_env, idx),
                        } });
                        if (has_ext) {
                            try frames.append(gpa, .{ .visit = tu.ext.? });
                        }
                        var i = tags_slice.len;
                        while (i > 0) {
                            i -= 1;
                            const tag_anno = module_env.store.getTypeAnno(tags_slice[i]);
                            switch (tag_anno) {
                                .tag => |t| {
                                    const tag_args_slice = module_env.store.sliceTypeAnnos(t.args);
                                    try frames.append(gpa, .{ .finish_tag = .{
                                        .name = module_env.getIdentText(t.name),
                                        .arg_count = tag_args_slice.len,
                                        .layout = sourceTypeLayout(module_env, tags_slice[i]),
                                    } });
                                    try Builder.pushVisitsReversed(&frames, gpa, tag_args_slice);
                                },
                                .apply,
                                .rigid_var,
                                .rigid_var_lookup,
                                .underscore,
                                .lookup,
                                .tag_union,
                                .@"fn",
                                .tuple,
                                .record,
                                .parens,
                                .malformed,
                                => {
                                    try frames.append(gpa, .malformed_tag);
                                },
                            }
                        }
                    },
                    .tag => |t| {
                        const tag_args_slice = module_env.store.sliceTypeAnnos(t.args);
                        try frames.append(gpa, .{ .finish_tag = .{
                            .name = module_env.getIdentText(t.name),
                            .arg_count = tag_args_slice.len,
                            .layout = sourceTypeLayout(module_env, idx),
                        } });
                        try Builder.pushVisitsReversed(&frames, gpa, tag_args_slice);
                    },
                    .tuple => |t| {
                        const elems_slice = module_env.store.sliceTypeAnnos(t.elems);
                        try frames.append(gpa, .{ .finish_tuple = .{
                            .elem_count = elems_slice.len,
                            .layout = sourceTypeLayout(module_env, idx),
                        } });
                        try Builder.pushVisitsReversed(&frames, gpa, elems_slice);
                    },
                    .record => |r| {
                        const fields_slice = module_env.store.sliceAnnoRecordFields(r.fields);
                        const has_ext = if (r.ext) |ext_idx| !isAnonymousOpenExt(module_env, ext_idx) else false;
                        try frames.append(gpa, .{ .finish_record = .{
                            .fields = fields_slice,
                            .has_ext = has_ext,
                            .is_open = r.ext != null,
                            .layout = sourceTypeLayout(module_env, idx),
                        } });
                        if (has_ext) {
                            try frames.append(gpa, .{ .visit = r.ext.? });
                        }
                        var i = fields_slice.len;
                        while (i > 0) {
                            i -= 1;
                            const field = module_env.store.getAnnoRecordField(fields_slice[i]);
                            // Optional (`name ?: Type`) and defaulted
                            // (`name : Type ?? default`) fields both carry a
                            // value type; the declared kind is threaded through
                            // `finish_record` below. Every field contributes
                            // exactly one type visit so results stay aligned.
                            try frames.append(gpa, .{ .visit = field.ty });
                        }
                    },
                    .@"fn" => |f| {
                        const args_slice = module_env.store.sliceTypeAnnos(f.args);
                        try frames.append(gpa, .{ .finish_fn = .{
                            .arg_count = args_slice.len,
                            .effectful = f.effectful,
                        } });
                        try frames.append(gpa, .{ .visit = f.ret });
                        try Builder.pushVisitsReversed(&frames, gpa, args_slice);
                    },
                    .parens => |p| {
                        try frames.append(gpa, .{ .visit = p.anno });
                    },
                    .malformed => {
                        try Builder.pushResult(&results, gpa, try allocDocType(gpa, .@"error"));
                    },
                }
            },
            .malformed_tag => {
                const tag_args = try gpa.alloc(*const DocType, 0);
                var tag_args_moved = false;
                errdefer if (!tag_args_moved) gpa.free(tag_args);

                var tags = try gpa.alloc(DocType.Tag, 1);
                var tags_len: usize = 0;
                var tags_moved = false;
                errdefer if (!tags_moved) {
                    for (tags[0..tags_len]) |tag| {
                        Builder.cleanupTag(gpa, tag);
                    }
                    gpa.free(tags);
                };

                tags[0] = .{
                    .name = try gpa.dupe(u8, "?"),
                    .args = tag_args,
                    .layout = .compact,
                };
                tags_len = 1;
                tag_args_moved = true;

                const single_tag = try allocDocType(gpa, .{ .tag_union = .{
                    .tags = tags,
                    .ext = null,
                    .is_open = false,
                } });
                tags_moved = true;
                try Builder.pushResult(&results, gpa, single_tag);
            },
            .finish_apply => |finish| {
                var display_moved = false;
                errdefer if (!display_moved) {
                    gpa.free(finish.name);
                    gpa.free(finish.module_path);
                };
                std.debug.assert(results.items.len >= finish.arg_count);
                const start = results.items.len - finish.arg_count;
                const args = try gpa.alloc(*const DocType, finish.arg_count);
                var args_moved = false;
                errdefer if (!args_moved) {
                    Builder.cleanupDocTypes(gpa, args);
                    gpa.free(args);
                };
                @memcpy(args, results.items[start..]);
                results.shrinkRetainingCapacity(start);

                const constructor = try allocDocType(gpa, .{ .type_ref = .{
                    .module_path = finish.module_path,
                    .type_name = finish.name,
                } });
                display_moved = true;
                var constructor_moved = false;
                errdefer if (!constructor_moved) {
                    constructor.deinit(gpa);
                    gpa.destroy(constructor);
                };

                const app = try allocDocType(gpa, .{ .apply = .{
                    .constructor = constructor,
                    .args = args,
                    .layout = finish.layout,
                } });
                args_moved = true;
                constructor_moved = true;
                try Builder.pushResult(&results, gpa, app);
            },
            .finish_tag => |finish| {
                std.debug.assert(results.items.len >= finish.arg_count);
                const start = results.items.len - finish.arg_count;
                const tag_args = try gpa.alloc(*const DocType, finish.arg_count);
                var tag_args_moved = false;
                errdefer if (!tag_args_moved) {
                    Builder.cleanupDocTypes(gpa, tag_args);
                    gpa.free(tag_args);
                };
                @memcpy(tag_args, results.items[start..]);
                results.shrinkRetainingCapacity(start);

                var tags = try gpa.alloc(DocType.Tag, 1);
                var tags_len: usize = 0;
                var tags_moved = false;
                errdefer if (!tags_moved) {
                    for (tags[0..tags_len]) |tag| {
                        Builder.cleanupTag(gpa, tag);
                    }
                    gpa.free(tags);
                };
                tags[0] = .{
                    .name = try gpa.dupe(u8, finish.name),
                    .args = tag_args,
                    .layout = finish.layout,
                };
                tags_len = 1;
                tag_args_moved = true;

                const single_tag = try allocDocType(gpa, .{ .tag_union = .{
                    .tags = tags,
                    .ext = null,
                    .is_open = false,
                } });
                tags_moved = true;
                try Builder.pushResult(&results, gpa, single_tag);
            },
            .finish_tag_union => |finish| {
                var ext: ?*const DocType = null;
                var ext_moved = false;
                errdefer if (!ext_moved) {
                    if (ext) |ext_type| {
                        ext_type.deinit(gpa);
                        gpa.destroy(ext_type);
                    }
                };
                if (finish.has_ext) {
                    ext = results.pop().?;
                }

                std.debug.assert(results.items.len >= finish.tag_count);
                const start = results.items.len - finish.tag_count;
                var tags = try gpa.alloc(DocType.Tag, finish.tag_count);
                var tags_len: usize = 0;
                var tags_moved = false;
                errdefer if (!tags_moved) {
                    for (tags[0..tags_len]) |tag| {
                        Builder.cleanupTag(gpa, tag);
                    }
                    gpa.free(tags);
                };

                for (results.items[start..], 0..) |single_tag, i| {
                    switch (single_tag.*) {
                        .tag_union => |tu| {
                            std.debug.assert(tu.tags.len == 1);
                            std.debug.assert(tu.ext == null);
                            tags[i] = tu.tags[0];
                            tags_len += 1;
                            gpa.free(tu.tags);
                        },
                        .type_ref,
                        .type_var,
                        .function,
                        .record,
                        .tuple,
                        .apply,
                        .where_clause,
                        .wildcard,
                        .@"error",
                        => unreachable,
                    }
                    gpa.destroy(single_tag);
                }
                results.shrinkRetainingCapacity(start);

                const tag_union = try allocDocType(gpa, .{ .tag_union = .{
                    .tags = tags,
                    .ext = ext,
                    .is_open = finish.is_open,
                    .layout = finish.layout,
                } });
                tags_moved = true;
                ext_moved = true;
                try Builder.pushResult(&results, gpa, tag_union);
            },
            .finish_tuple => |finish| {
                std.debug.assert(results.items.len >= finish.elem_count);
                const start = results.items.len - finish.elem_count;
                const elems = try gpa.alloc(*const DocType, finish.elem_count);
                var elems_moved = false;
                errdefer if (!elems_moved) {
                    Builder.cleanupDocTypes(gpa, elems);
                    gpa.free(elems);
                };
                @memcpy(elems, results.items[start..]);
                results.shrinkRetainingCapacity(start);

                const tuple = try allocDocType(gpa, .{ .tuple = .{
                    .elems = elems,
                    .layout = finish.layout,
                } });
                elems_moved = true;
                try Builder.pushResult(&results, gpa, tuple);
            },
            .finish_record => |finish| {
                var ext: ?*const DocType = null;
                var ext_moved = false;
                errdefer if (!ext_moved) {
                    if (ext) |ext_type| {
                        ext_type.deinit(gpa);
                        gpa.destroy(ext_type);
                    }
                };
                if (finish.has_ext) {
                    ext = results.pop().?;
                }

                std.debug.assert(results.items.len >= finish.fields.len);
                const start = results.items.len - finish.fields.len;
                var field_names = try gpa.alloc([]const u8, finish.fields.len);
                defer gpa.free(field_names);
                var field_kinds = try gpa.alloc(DocType.Field.Kind, finish.fields.len);
                defer gpa.free(field_kinds);
                var field_names_len: usize = 0;
                var field_names_moved = false;
                errdefer if (!field_names_moved) {
                    for (field_names[0..field_names_len]) |name| {
                        gpa.free(name);
                    }
                    for (field_kinds[0..field_names_len]) |kind| {
                        if (kind == .defaulted) {
                            if (kind.defaulted) |snippet| gpa.free(snippet);
                        }
                    }
                };
                for (finish.fields) |field_idx| {
                    const field = module_env.store.getAnnoRecordField(field_idx);
                    // A field is optional (`?:`), defaulted (`?? default`), or a
                    // plain required field (mutually exclusive at can time).
                    const kind: DocType.Field.Kind = if (field.is_optional)
                        .optional
                    else if (field.default_value) |default_idx|
                        .{ .defaulted = try defaultSourceSnippet(gpa, module_env, default_idx) }
                    else
                        .required;
                    errdefer if (kind == .defaulted) {
                        if (kind.defaulted) |snippet| gpa.free(snippet);
                    };
                    field_names[field_names_len] = try gpa.dupe(u8, module_env.getIdentText(field.name));
                    field_kinds[field_names_len] = kind;
                    field_names_len += 1;
                }

                var fields = try gpa.alloc(DocType.Field, finish.fields.len);
                var fields_len: usize = 0;
                var fields_moved = false;
                errdefer if (!fields_moved) {
                    Builder.cleanupFields(gpa, fields[0..fields_len]);
                    gpa.free(fields);
                };

                for (field_names, field_kinds, 0..) |field_name, field_kind, i| {
                    fields[i] = .{
                        .name = field_name,
                        .type = results.items[start + i],
                        .kind = field_kind,
                    };
                    fields_len += 1;
                }
                field_names_moved = true;
                results.shrinkRetainingCapacity(start);

                const record = try allocDocType(gpa, .{ .record = .{
                    .fields = fields,
                    .ext = ext,
                    .is_open = finish.is_open,
                    .layout = finish.layout,
                } });
                fields_moved = true;
                ext_moved = true;
                try Builder.pushResult(&results, gpa, record);
            },
            .finish_fn => |finish| {
                const needed = finish.arg_count + 1;
                std.debug.assert(results.items.len >= needed);
                const ret = results.pop().?;
                var ret_moved = false;
                errdefer if (!ret_moved) {
                    ret.deinit(gpa);
                    gpa.destroy(ret);
                };

                const start = results.items.len - finish.arg_count;
                const args = try gpa.alloc(*const DocType, finish.arg_count);
                var args_moved = false;
                errdefer if (!args_moved) {
                    Builder.cleanupDocTypes(gpa, args);
                    gpa.free(args);
                };
                @memcpy(args, results.items[start..]);
                results.shrinkRetainingCapacity(start);

                const func = try allocDocType(gpa, .{ .function = .{
                    .args = args,
                    .ret = ret,
                    .effectful = finish.effectful,
                } });
                args_moved = true;
                ret_moved = true;
                try Builder.pushResult(&results, gpa, func);
            },
        }
    }

    std.debug.assert(results.items.len == 1);
    return results.pop().?;
}

// --- Type extraction from inferred types ---

const ExtractError = std.mem.Allocator.Error;

/// Extract a structured DocType from a type variable in the type store.
///
/// This walks the type store following the same resolution/dispatch pattern
/// as TypeWriter.writeVarWithContext, but builds a DocType tree instead of
/// a string.
/// Context for type extraction, holding shared mutable state.
const ExtractContext = struct {
    gpa: Allocator,
    types: *const TypeStore,
    env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    idents: *const Ident.Store,
    seen: std.ArrayList(Var),
    constraints_list: std.ArrayList(ConstraintInfo),
    flex_names: std.AutoHashMap(Var, []const u8),
    next_name_idx: u32,

    fn init(
        gpa: Allocator,
        types: *const TypeStore,
        env: *const ModuleEnv,
        local_module_path: []const u8,
        reference_routing: PublicReferenceRouting,
    ) ExtractContext {
        return .{
            .gpa = gpa,
            .types = types,
            .env = env,
            .local_module_path = local_module_path,
            .reference_routing = reference_routing,
            .idents = env.getIdentStoreConst(),
            .seen = std.ArrayList(Var).empty,
            .constraints_list = std.ArrayList(ConstraintInfo).empty,
            .flex_names = std.AutoHashMap(Var, []const u8).init(gpa),
            .next_name_idx = 0,
        };
    }

    fn deinit(self: *ExtractContext) void {
        self.seen.deinit(self.gpa);
        self.constraints_list.deinit(self.gpa);
        var it = self.flex_names.valueIterator();
        while (it.next()) |value| {
            self.gpa.free(value.*);
        }
        self.flex_names.deinit();
    }

    /// Generate the next flex var name: a, b, c, ..., z, aa, ab, ...
    fn nextFlexName(self: *ExtractContext) ExtractError![]const u8 {
        var n = self.next_name_idx;
        self.next_name_idx += 1;

        var name_buf: [8]u8 = undefined;
        var name_len: usize = 0;

        // Generate name in base-26: a, b, ..., z, aa, ab, ...
        while (name_len < name_buf.len) {
            name_buf[name_len] = @intCast('a' + (n % 26));
            name_len += 1;
            n = n / 26;
            if (n == 0) break;
            n -= 1;
        }

        // Names are generated in reverse order, so reverse
        std.mem.reverse(u8, name_buf[0..name_len]);

        return try self.gpa.dupe(u8, name_buf[0..name_len]);
    }

    /// Get or create a name for a flex var
    fn getFlexVarName(self: *ExtractContext, resolved_var: Var) ExtractError![]const u8 {
        if (self.flex_names.get(resolved_var)) |name| {
            return try self.gpa.dupe(u8, name);
        }
        const name = try self.nextFlexName();
        try self.flex_names.put(resolved_var, name);
        return try self.gpa.dupe(u8, name);
    }
};

fn extractDocType(
    gpa: Allocator,
    types: *const TypeStore,
    env: *const ModuleEnv,
    local_module_path: []const u8,
    reference_routing: PublicReferenceRouting,
    var_: Var,
) ExtractError!?*const DocType {
    var ctx = ExtractContext.init(gpa, types, env, local_module_path, reference_routing);
    defer ctx.deinit();

    const base_type = try extractDocTypeInner(&ctx, var_);
    if (base_type == null) return null;

    // If there are constraints, wrap in a where clause
    if (ctx.constraints_list.items.len > 0) {
        // Deduplicate constraints by (dispatcher_var_name, fn_name)
        // A solved type's constraints are always method constraints: where
        // aliases are expanded during checking.
        var unique_constraints = std.ArrayList(DocType.Constraint).empty;
        defer {
            for (unique_constraints.items) |c| c.deinit(gpa);
            unique_constraints.deinit(gpa);
        }

        for (ctx.constraints_list.items) |info| {
            // Check for duplicate
            var is_dup = false;
            for (unique_constraints.items) |existing| {
                if (std.mem.eql(u8, existing.method.type_var, info.dispatcher_name) and
                    std.mem.eql(u8, existing.method.method_name, info.fn_name_text))
                {
                    is_dup = true;
                    break;
                }
            }
            if (is_dup) continue;

            // Extract the constraint function's type using a fresh context
            // to avoid cycles with the main type's seen list.
            var fn_ctx = ExtractContext.init(gpa, types, env, local_module_path, reference_routing);
            defer fn_ctx.deinit();

            const fn_type = try extractDocTypeInner(&fn_ctx, info.fn_var) orelse
                try allocDocType(gpa, .@"error");

            try unique_constraints.append(gpa, .{ .method = .{
                .type_var = try gpa.dupe(u8, info.dispatcher_name),
                .method_name = try gpa.dupe(u8, info.fn_name_text),
                .signature = fn_type,
            } });
        }

        // Sort constraints alphabetically by (type_var, method_name)
        std.mem.sort(DocType.Constraint, unique_constraints.items, {}, struct {
            fn lessThan(_: void, a: DocType.Constraint, b: DocType.Constraint) bool {
                const type_cmp = std.mem.order(u8, a.method.type_var, b.method.type_var);
                if (type_cmp != .eq) return type_cmp == .lt;
                return std.mem.order(u8, a.method.method_name, b.method.method_name) == .lt;
            }
        }.lessThan);

        // Move constraints to owned slice
        const owned_constraints = try gpa.alloc(DocType.Constraint, unique_constraints.items.len);
        @memcpy(owned_constraints, unique_constraints.items);

        // Clear the unique_constraints so the deferred free doesn't double-free
        unique_constraints.clearRetainingCapacity();

        return try allocDocType(gpa, .{ .where_clause = .{
            .type = base_type.?,
            .constraints = owned_constraints,
            .layout = .multiline,
        } });
    }

    return base_type;
}

const ConstraintInfo = struct {
    dispatcher_name: []const u8, // borrowed from idents store
    fn_name_text: []const u8, // borrowed from idents store
    fn_var: Var,
};

const TypeReferenceDisplay = struct {
    /// Owned by the resulting DocType.
    module_path: []const u8,
    /// Owned by the resulting DocType.
    type_name: []const u8,
};

fn inferredTypeReferenceDisplay(
    ctx: *const ExtractContext,
    origin_module: base.ModuleIdentity.Idx,
    source_decl: ?u32,
    origin_text: []const u8,
    ident_text: []const u8,
) ExtractError!TypeReferenceDisplay {
    if (source_decl) |raw_statement| {
        const origin_identity = ctx.env.moduleIdentityHash(origin_module);
        const statement: CIR.Statement.Idx = @enumFromInt(raw_statement);
        if (selectPublicProjection(
            ctx.reference_routing.current,
            ctx.reference_routing.all,
            origin_identity,
            statement,
        )) |projection| {
            const source_name = typeDeclName(projection.source_env, statement) orelse unreachable;
            const module_path = try std.fmt.allocPrint(
                ctx.gpa,
                "{s}.{s}",
                .{ projection.package_name, projection.public_name },
            );
            errdefer ctx.gpa.free(module_path);
            return .{
                .module_path = module_path,
                .type_name = try projectedEntryName(ctx.gpa, projection.source_env, projection, source_name),
            };
        }
    }

    const module_path = try ctx.gpa.dupe(u8, getModulePath(origin_text));
    errdefer ctx.gpa.free(module_path);
    return .{
        .module_path = module_path,
        .type_name = try ctx.gpa.dupe(u8, getDisplayName(origin_text, ident_text)),
    };
}

fn extractDocTypeInner(
    ctx: *ExtractContext,
    var_: Var,
) ExtractError!?*const DocType {
    const gpa = ctx.gpa;
    const types = ctx.types;
    const idents = ctx.idents;

    if (@intFromEnum(var_) >= types.len()) {
        return try allocDocType(gpa, .@"error");
    }

    const resolved = types.resolveVar(var_);

    if (@intFromEnum(resolved.var_) >= types.len()) {
        return try allocDocType(gpa, .@"error");
    }

    if (resolved.desc.content == .err) {
        return try allocDocType(gpa, .@"error");
    }

    // Cycle detection
    for (ctx.seen.items) |seen_var| {
        if (seen_var == resolved.var_) {
            return try allocDocType(gpa, .@"error");
        }
    }
    try ctx.seen.append(gpa, resolved.var_);
    defer _ = ctx.seen.pop();

    switch (resolved.desc.content) {
        .flex => |flex| {
            // Check for a literal-conversion constraint and default to the
            // literal kind's default type (numeral -> Dec, quote -> Str).
            const constraints = types.sliceStaticDispatchConstraints(flex.constraints);
            var literal_kind: ?types_mod.StaticDispatchConstraint.LiteralKind = null;
            for (constraints) |constraint| {
                if (constraint.origin.literalKind()) |kind| {
                    literal_kind = kind;
                    break;
                }
            }

            if (literal_kind) |kind| {
                // Default open literal types for display
                return switch (kind) {
                    .numeral => try allocDocType(gpa, .{ .type_ref = .{
                        .module_path = try gpa.dupe(u8, "Num"),
                        .type_name = try gpa.dupe(u8, "Dec"),
                    } }),
                    .quote, .interpolation => try allocDocType(gpa, .{ .type_ref = .{
                        .module_path = try gpa.dupe(u8, ""),
                        .type_name = try gpa.dupe(u8, "Str"),
                    } }),
                };
            }

            // Get the variable name
            const var_name = if (flex.name) |ident_idx|
                try gpa.dupe(u8, idents.getText(ident_idx))
            else
                try ctx.getFlexVarName(resolved.var_);

            // Collect non-numeral constraints for where clause
            for (constraints) |constraint| {
                if (constraint.origin != .from_literal) {
                    const dispatcher_name = if (flex.name) |ident_idx| idents.getText(ident_idx) else var_name;
                    try ctx.constraints_list.append(gpa, .{
                        .dispatcher_name = dispatcher_name,
                        .fn_name_text = idents.getText(constraint.fn_name),
                        .fn_var = constraint.fn_var,
                    });
                }
            }

            return try allocDocType(gpa, .{ .type_var = var_name });
        },
        .rigid => |rigid| {
            const var_name = idents.getText(rigid.name);

            // Collect constraints for where clause
            const constraints = types.sliceStaticDispatchConstraints(rigid.constraints);
            for (constraints) |constraint| {
                try ctx.constraints_list.append(gpa, .{
                    .dispatcher_name = var_name,
                    .fn_name_text = idents.getText(constraint.fn_name),
                    .fn_var = constraint.fn_var,
                });
            }

            return try allocDocType(gpa, .{ .type_var = try gpa.dupe(u8, var_name) });
        },
        .alias => |alias| {
            const origin_text = ctx.env.moduleIdentityDisplayText(alias.origin_module);
            const ident_text = idents.getText(alias.ident.ident_idx);
            const display = try inferredTypeReferenceDisplay(
                ctx,
                alias.origin_module,
                alias.source_decl.toOptional(),
                origin_text,
                ident_text,
            );
            var display_moved = false;
            errdefer if (!display_moved) {
                gpa.free(display.module_path);
                gpa.free(display.type_name);
            };

            var args_iter = types.iterAliasArgs(alias);
            if (args_iter.count() > 0) {
                // Type application
                const constructor = try allocDocType(gpa, .{ .type_ref = .{
                    .module_path = display.module_path,
                    .type_name = display.type_name,
                } });
                display_moved = true;
                errdefer {
                    constructor.deinit(gpa);
                    gpa.destroy(constructor);
                }

                var args = std.ArrayList(*const DocType).empty;
                defer args.deinit(gpa);

                while (args_iter.next()) |arg_var| {
                    const arg_type = try extractDocTypeInner(ctx, arg_var) orelse
                        try allocDocType(gpa, .@"error");
                    try args.append(gpa, arg_type);
                }

                const args_slice = try args.toOwnedSlice(gpa);
                return try allocDocType(gpa, .{ .apply = .{
                    .constructor = constructor,
                    .args = args_slice,
                    .layout = .multiline,
                } });
            } else {
                // Simple type reference
                return try allocDocType(gpa, .{ .type_ref = .{
                    .module_path = display.module_path,
                    .type_name = display.type_name,
                } });
            }
        },
        .structure => |flat_type| {
            return try extractFlatType(ctx, flat_type);
        },
        .field_presence => {
            // A presence variable is never a documentable type in its own
            // right; it only appears on a record field's presence axis. Render
            // as the error type, consistent with `.err`.
            return try allocDocType(gpa, .@"error");
        },
        .err => {
            return try allocDocType(gpa, .@"error");
        },
    }
}

fn extractFlatType(
    ctx: *ExtractContext,
    flat_type: FlatType,
) ExtractError!*const DocType {
    const gpa = ctx.gpa;
    switch (flat_type) {
        .fn_pure => |func| {
            return try extractFunction(ctx, func, false);
        },
        .fn_effectful => |func| {
            return try extractFunction(ctx, func, true);
        },
        .fn_unbound => |func| {
            return try extractFunction(ctx, func, false);
        },
        .nominal_type => |nominal| {
            return try extractNominalType(ctx, nominal);
        },
        .record => |record| {
            return try extractRecord(ctx, record);
        },
        .record_unbound => |fields| {
            return try extractRecordUnbound(ctx, fields);
        },
        .tuple => |tuple| {
            return try extractTuple(ctx, tuple);
        },
        .tag_union => |tag_union| {
            return try extractTagUnion(ctx, tag_union);
        },
        .empty_record => {
            return try allocDocType(gpa, .{ .record = .{
                .fields = try gpa.alloc(DocType.Field, 0),
                .ext = null,
                .is_open = false,
                .layout = .multiline,
            } });
        },
        .empty_tag_union => {
            return try allocDocType(gpa, .{ .tag_union = .{
                .tags = try gpa.alloc(DocType.Tag, 0),
                .ext = null,
                .is_open = false,
                .layout = .multiline,
            } });
        },
    }
}

fn extractFunction(
    ctx: *ExtractContext,
    func: types_mod.Func,
    effectful: bool,
) ExtractError!*const DocType {
    const gpa = ctx.gpa;
    const arg_vars = ctx.types.sliceVars(func.args);

    var args = try gpa.alloc(*const DocType, arg_vars.len);
    errdefer gpa.free(args);

    for (arg_vars, 0..) |arg_var, i| {
        args[i] = try extractDocTypeInner(ctx, arg_var) orelse
            try allocDocType(gpa, .@"error");
    }

    const ret = try extractDocTypeInner(ctx, func.ret) orelse
        try allocDocType(gpa, .@"error");

    return try allocDocType(gpa, .{ .function = .{
        .args = args,
        .ret = ret,
        .effectful = effectful,
    } });
}

fn extractNominalType(
    ctx: *ExtractContext,
    nominal: NominalType,
) ExtractError!*const DocType {
    const gpa = ctx.gpa;
    const idents = ctx.idents;
    const origin_text = ctx.env.moduleIdentityDisplayText(nominal.origin_module);
    const ident_text = idents.getText(nominal.ident.ident_idx);
    const display = try inferredTypeReferenceDisplay(
        ctx,
        nominal.origin_module,
        nominal.sourceDeclOptional(),
        origin_text,
        ident_text,
    );
    var display_moved = false;
    errdefer if (!display_moved) {
        gpa.free(display.module_path);
        gpa.free(display.type_name);
    };

    var args_iter = ctx.types.iterNominalArgs(nominal);
    if (args_iter.count() > 0) {
        const constructor = try allocDocType(gpa, .{ .type_ref = .{
            .module_path = display.module_path,
            .type_name = display.type_name,
        } });
        display_moved = true;
        errdefer {
            constructor.deinit(gpa);
            gpa.destroy(constructor);
        }

        var args = std.ArrayList(*const DocType).empty;
        defer args.deinit(gpa);

        while (args_iter.next()) |arg_var| {
            const arg_type = try extractDocTypeInner(ctx, arg_var) orelse
                try allocDocType(gpa, .@"error");
            try args.append(gpa, arg_type);
        }

        const args_slice = try args.toOwnedSlice(gpa);
        return try allocDocType(gpa, .{ .apply = .{
            .constructor = constructor,
            .args = args_slice,
            .layout = .multiline,
        } });
    } else {
        return try allocDocType(gpa, .{ .type_ref = .{
            .module_path = display.module_path,
            .type_name = display.type_name,
        } });
    }
}

/// The documentation kind of a solved record field. A field whose kind solved
/// `optional` documents as `name ?: Type`. A required field, a defaulted field
/// (a required slot at runtime—rendering its default value from the solved
/// type is deferred, design.md "Defaulted Fields"), or a still-flex kind (flex
/// defaults to required, design.md "Field Kinds (All-Dynamic Optional Fields)")
/// all document as plain required fields. Mirrors TypeWriter's
/// `writeRecordFieldSeparator`.
fn docFieldKind(
    types: *const TypeStore,
    presence: types_mod.RecordField.Presence,
) DocType.Field.Kind {
    return switch (presence.decode()) {
        .required => .required,
        .unknown => |unknown| switch (types.resolveVar(unknown.presence).desc.content) {
            .field_presence => |fp| switch (fp) {
                .required, .defaulted => .required,
                .optional => .optional,
            },
            .flex, .rigid, .alias, .structure, .err => .required,
        },
    };
}

fn extractRecord(
    ctx: *ExtractContext,
    record: types_mod.Record,
) ExtractError!*const DocType {
    const gpa = ctx.gpa;
    const types = ctx.types;
    const idents = ctx.idents;

    // Gather record fields by following the extension chain
    var all_fields = std.ArrayList(types_mod.RecordField).empty;
    defer all_fields.deinit(gpa);

    // Get fields from the initial record
    const initial_slice = types.getRecordFieldsSlice(record.fields);
    for (initial_slice.items(.name), initial_slice.items(.presence)) |name, presence| {
        try all_fields.append(gpa, .{ .name = name, .presence = presence });
    }

    // Follow the extension chain
    var ext = record.ext;
    var ext_doc_type: ?*const DocType = null;
    var is_open = false;
    var guard_count: usize = 0;
    while (guard_count < 100) : (guard_count += 1) {
        const ext_resolved = types.resolveVar(ext);
        switch (ext_resolved.desc.content) {
            .flex => |flex| {
                const ident_text: ?[]const u8 = if (flex.name) |ident_idx|
                    idents.getText(ident_idx)
                else
                    null;

                // Collect constraints from the extension variable
                const constraints = types.sliceStaticDispatchConstraints(flex.constraints);
                if (constraints.len > 0) {
                    const dispatcher_name = if (ident_text) |t| t else try ctx.getFlexVarName(ext_resolved.var_);
                    for (constraints) |constraint| {
                        if (constraint.origin != .from_literal) {
                            try ctx.constraints_list.append(gpa, .{
                                .dispatcher_name = dispatcher_name,
                                .fn_name_text = idents.getText(constraint.fn_name),
                                .fn_var = constraint.fn_var,
                            });
                        }
                    }
                }

                is_open = true;
                if (ident_text) |t| {
                    if (t.len == 0 or t[0] == '#') {
                        // Synthetic anonymous-open name—render as `..` with no name.
                    } else {
                        ext_doc_type = try allocDocType(gpa, .{ .type_var = try gpa.dupe(u8, t) });
                    }
                } else {
                    ext_doc_type = try allocDocType(gpa, .{ .type_var = try ctx.getFlexVarName(ext_resolved.var_) });
                }
                break;
            },
            .rigid => |rigid| {
                const var_name = idents.getText(rigid.name);

                const constraints = types.sliceStaticDispatchConstraints(rigid.constraints);
                for (constraints) |constraint| {
                    try ctx.constraints_list.append(gpa, .{
                        .dispatcher_name = var_name,
                        .fn_name_text = idents.getText(constraint.fn_name),
                        .fn_var = constraint.fn_var,
                    });
                }

                is_open = true;
                if (var_name.len == 0 or var_name[0] != '#') {
                    ext_doc_type = try allocDocType(gpa, .{ .type_var = try gpa.dupe(u8, var_name) });
                }
                break;
            },
            .alias => |alias| {
                ext = types.getAliasBackingVar(alias);
            },
            .structure => |ft| {
                switch (ft) {
                    .record => |ext_record| {
                        const ext_slice = types.getRecordFieldsSlice(ext_record.fields);
                        for (ext_slice.items(.name), ext_slice.items(.presence)) |name, presence| {
                            try all_fields.append(gpa, .{ .name = name, .presence = presence });
                        }
                        ext = ext_record.ext;
                    },
                    .record_unbound => |ext_fields| {
                        const ext_slice = types.getRecordFieldsSlice(ext_fields);
                        for (ext_slice.items(.name), ext_slice.items(.presence)) |name, presence| {
                            try all_fields.append(gpa, .{ .name = name, .presence = presence });
                        }
                        break;
                    },
                    .empty_record => break,
                    .tuple,
                    .nominal_type,
                    .fn_pure,
                    .fn_effectful,
                    .fn_unbound,
                    .tag_union,
                    .empty_tag_union,
                    => break,
                }
            },
            // A presence variable can never be a record extension tail.
            .field_presence => break,
            .err => break,
        }
    }

    // Sort fields alphabetically
    std.mem.sort(types_mod.RecordField, all_fields.items, idents, comptime types_mod.RecordField.sortByNameAsc);

    // Build DocType.Field array from gathered fields
    var doc_fields = try gpa.alloc(DocType.Field, all_fields.items.len);
    for (all_fields.items, 0..) |field, i| {
        doc_fields[i] = .{
            .name = try gpa.dupe(u8, idents.getText(field.name)),
            // Every field carries a value type on the type axis, independent of
            // the solved kind (design.md "Field Kinds"); the kind only selects
            // the `:` / `?:` separator.
            .type = try extractDocTypeInner(ctx, field.presence.typeVar()) orelse
                try allocDocType(gpa, .@"error"),
            .kind = docFieldKind(types, field.presence),
        };
    }

    return try allocDocType(gpa, .{ .record = .{
        .fields = doc_fields,
        .ext = ext_doc_type,
        .is_open = is_open,
        .layout = .multiline,
    } });
}

fn extractRecordUnbound(
    ctx: *ExtractContext,
    fields_range: types_mod.RecordField.SafeMultiList.Range,
) ExtractError!*const DocType {
    const gpa = ctx.gpa;

    if (fields_range.isEmpty()) {
        return try allocDocType(gpa, .{ .record = .{
            .fields = try gpa.alloc(DocType.Field, 0),
            .ext = null,
            .is_open = false,
            .layout = .multiline,
        } });
    }

    const slice = ctx.types.getRecordFieldsSlice(fields_range);
    const names = slice.items(.name);
    const presences = slice.items(.presence);
    var fields = try gpa.alloc(DocType.Field, names.len);
    for (names, presences, 0..) |name, presence, i| {
        fields[i] = .{
            .name = try gpa.dupe(u8, ctx.idents.getText(name)),
            // Every field carries a value type on the type axis; the solved
            // kind only selects the `:` / `?:` separator (design.md
            // "Field Kinds").
            .type = try extractDocTypeInner(ctx, presence.typeVar()) orelse
                try allocDocType(gpa, .@"error"),
            .kind = docFieldKind(ctx.types, presence),
        };
    }

    // Sort fields alphabetically
    std.mem.sort(DocType.Field, fields, {}, struct {
        fn lessThan(_: void, a: DocType.Field, b: DocType.Field) bool {
            return std.mem.order(u8, a.name, b.name) == .lt;
        }
    }.lessThan);

    return try allocDocType(gpa, .{ .record = .{
        .fields = fields,
        .ext = null,
        .is_open = false,
        .layout = .multiline,
    } });
}

fn extractTuple(
    ctx: *ExtractContext,
    tuple: types_mod.Tuple,
) ExtractError!*const DocType {
    const gpa = ctx.gpa;
    const elem_vars = ctx.types.sliceVars(tuple.elems);
    var elems = try gpa.alloc(*const DocType, elem_vars.len);
    for (elem_vars, 0..) |elem_var, i| {
        elems[i] = try extractDocTypeInner(ctx, elem_var) orelse
            try allocDocType(gpa, .@"error");
    }
    return try allocDocType(gpa, .{ .tuple = .{
        .elems = elems,
        .layout = .multiline,
    } });
}

fn extractTagUnion(
    ctx: *ExtractContext,
    tag_union: types_mod.TagUnion,
) ExtractError!*const DocType {
    const gpa = ctx.gpa;
    const types = ctx.types;
    const idents = ctx.idents;

    // Bounds check the tags range
    const tags_start_idx = @intFromEnum(tag_union.tags.start);
    const tags_len = types.tags.len();
    if (tags_start_idx >= tags_len or tags_start_idx + tag_union.tags.count > tags_len) {
        return try allocDocType(gpa, .@"error");
    }

    var tags = std.ArrayList(DocType.Tag).empty;
    defer tags.deinit(gpa);

    var iter = tag_union.tags.iterIndices();
    while (iter.next()) |tag_idx| {
        const tag = types.tags.get(tag_idx);
        const tag_name = try gpa.dupe(u8, idents.getText(tag.name));

        const tag_arg_vars = types.sliceVars(tag.args);
        var tag_args = try gpa.alloc(*const DocType, tag_arg_vars.len);
        for (tag_arg_vars, 0..) |arg_var, i| {
            tag_args[i] = try extractDocTypeInner(ctx, arg_var) orelse
                try allocDocType(gpa, .@"error");
        }

        try tags.append(gpa, .{
            .name = tag_name,
            .args = tag_args,
            .layout = .multiline,
        });
    }

    // Handle extension variable
    var ext_type: ?*const DocType = null;
    var is_open = false;
    const ext_resolved = types.resolveVar(tag_union.ext);
    switch (ext_resolved.desc.content) {
        .flex => |flex| {
            if (flex.name) |ident_idx| {
                const name = idents.getText(ident_idx);
                is_open = true;
                if (name.len > 0 and name[0] != '#') {
                    ext_type = try allocDocType(gpa, .{ .type_var = try gpa.dupe(u8, name) });
                }
            }
            // unnamed flex with no constraints = closed union (no extension)

            const constraints = types.sliceStaticDispatchConstraints(flex.constraints);
            for (constraints) |constraint| {
                const var_name = if (flex.name) |ident_idx| idents.getText(ident_idx) else "_";
                try ctx.constraints_list.append(gpa, .{
                    .dispatcher_name = var_name,
                    .fn_name_text = idents.getText(constraint.fn_name),
                    .fn_var = constraint.fn_var,
                });
            }
        },
        .rigid => |rigid| {
            const name = idents.getText(rigid.name);
            is_open = true;
            if (name.len > 0 and name[0] != '#') {
                ext_type = try allocDocType(gpa, .{ .type_var = try gpa.dupe(u8, name) });
            }

            const constraints = types.sliceStaticDispatchConstraints(rigid.constraints);
            for (constraints) |constraint| {
                try ctx.constraints_list.append(gpa, .{
                    .dispatcher_name = name,
                    .fn_name_text = idents.getText(constraint.fn_name),
                    .fn_var = constraint.fn_var,
                });
            }
        },
        .structure => |ft| switch (ft) {
            .empty_tag_union => {}, // closed union
            .record,
            .record_unbound,
            .tuple,
            .nominal_type,
            .fn_pure,
            .fn_effectful,
            .fn_unbound,
            .empty_record,
            .tag_union,
            => {
                is_open = true;
                ext_type = try extractDocTypeInner(ctx, tag_union.ext);
            },
        },
        .alias => {
            is_open = true;
            ext_type = try extractDocTypeInner(ctx, tag_union.ext);
        },
        // A presence variable is never a tag-union tail; treat it as a closed
        // union like `.err`.
        .field_presence => {},
        .err => {},
    }

    const tags_slice = try tags.toOwnedSlice(gpa);
    return try allocDocType(gpa, .{ .tag_union = .{
        .tags = tags_slice,
        .ext = ext_type,
        .is_open = is_open,
        .layout = .multiline,
    } });
}

// --- Helpers ---

/// Allocate a DocType on the heap.
fn allocDocType(gpa: Allocator, value: DocType) ExtractError!*const DocType {
    const ptr = try gpa.create(DocType);
    ptr.* = value;
    return ptr;
}

/// Parse a full type name (like "Builtin.Str", "Num.U64") into module_path and type_name.
/// For the structured output, we want to preserve the full module path as-is from the compiler.
fn getDisplayName(origin_text: []const u8, ident_text: []const u8) []const u8 {
    // Strip module prefix from the ident text if it matches the origin
    // e.g., origin="Builtin", ident="Builtin.Str" -> display="Str"
    // e.g., origin="Num", ident="Num.U64" -> display="U64"
    if (origin_text.len > 0) {
        // Check if ident starts with origin + "."
        if (std.mem.startsWith(u8, ident_text, origin_text)) {
            if (ident_text.len > origin_text.len and ident_text[origin_text.len] == '.') {
                const after_prefix = ident_text[origin_text.len + 1 ..];
                // Also strip "Num." if present after "Builtin."
                if (std.mem.eql(u8, origin_text, "Builtin") and std.mem.startsWith(u8, after_prefix, "Num.")) {
                    return after_prefix[4..];
                }
                return after_prefix;
            }
        }

        // Check if ident starts with "Builtin." even when origin is something else
        if (std.mem.startsWith(u8, ident_text, "Builtin.")) {
            const after_builtin = ident_text[8..];
            if (std.mem.startsWith(u8, after_builtin, "Num.")) {
                return after_builtin[4..];
            }
            return after_builtin;
        }

        // Check if ident starts with "Num."
        if (std.mem.startsWith(u8, ident_text, "Num.")) {
            return ident_text[4..];
        }
    }

    return ident_text;
}

/// Get the module path from the origin text.
/// The origin_module text is the raw module path from the compiler.
/// Returns empty string for compiler-owned builtin types since Builtin is an implementation detail.
fn getModulePath(origin_text: []const u8) []const u8 {
    if (std.mem.eql(u8, origin_text, "Builtin") or CIR.Import.isCompilerBuiltinImportName(origin_text)) {
        return "";
    }
    return origin_text;
}

fn convertModuleKind(kind: ModuleEnv.ModuleKind) DocModel.ModuleKind {
    return switch (kind) {
        .app, .default_app => .app,
        .module => .module,
        .package => .package,
        .platform => .platform,
        .type_module => .type_module,
        .hosted, .malformed => .app, // hosted and malformed modules are not documented as package modules
    };
}

fn isInternalBuiltin(name: []const u8) bool {
    // Filter out unsafe/internal builtin functions
    return std.mem.endsWith(u8, name, "_unsafe") or
        std.mem.endsWith(u8, name, "_lossy");
}

/// Whether an entry of the module being documented must be left out of the docs.
///
/// Internal builtin types are keyed off the module name rather than the package
/// name because they must stay undocumented however docs were invoked:
/// publishing a type Roc code cannot name would hand readers a signature they
/// have no way to write down.
fn entryIsUndocumented(
    package_name: []const u8,
    module_name: []const u8,
    entry_name: []const u8,
) bool {
    if (std.mem.eql(u8, package_name, "Builtin") and isInternalBuiltin(entry_name)) return true;
    return std.mem.eql(u8, module_name, "Builtin") and CIR.builtinTypeIsInternal(entry_name);
}

fn findEntryByName(entries: []const DocModel.DocEntry, name: []const u8) bool {
    for (entries) |entry| {
        if (std.mem.eql(u8, entry.name, name)) return true;
    }
    return false;
}

fn joinLines(gpa: Allocator, lines: []const []const u8) Allocator.Error![]u8 {
    // Calculate total length
    var total_len: usize = 0;
    for (lines, 0..) |line, i| {
        if (i > 0) total_len += 1; // newline
        total_len += line.len;
    }

    const result = try gpa.alloc(u8, total_len);
    var pos: usize = 0;
    for (lines, 0..) |line, i| {
        if (i > 0) {
            result[pos] = '\n';
            pos += 1;
        }
        @memcpy(result[pos..][0..line.len], line);
        pos += line.len;
    }
    return result;
}

/// Append a child entry to a parent's children slice, reallocating in place.
/// This replaces the repeated pattern of: create ArrayList, copy all old children,
/// append new child, free old slice, toOwnedSlice.
fn appendChildEntry(gpa: Allocator, parent: *DocModel.DocEntry, child: DocModel.DocEntry) Allocator.Error!void {
    const old = parent.children;
    const new_children = try gpa.alloc(DocModel.DocEntry, old.len + 1);
    @memcpy(new_children[0..old.len], old);
    new_children[old.len] = child;
    gpa.free(old);
    parent.children = new_children;
}

fn moveEntryForReparenting(
    gpa: Allocator,
    entry: *DocModel.DocEntry,
    short_name: []const u8,
) Allocator.Error!DocModel.DocEntry {
    const new_name = try gpa.dupe(u8, short_name);
    errdefer gpa.free(new_name);

    const empty_children = try gpa.alloc(DocModel.DocEntry, 0);
    errdefer gpa.free(empty_children);

    var moved = entry.*;
    moved.name = new_name;

    entry.children = empty_children;
    entry.type_header = null;
    entry.type_signature = null;
    entry.doc_comment = null;
    entry.doc_refs = &.{};

    return moved;
}

fn trimLeft(s: []const u8) []const u8 {
    var i: usize = 0;
    while (i < s.len and (s[i] == ' ' or s[i] == '\t')) {
        i += 1;
    }
    return s[i..];
}

/// Reference implementation of the old byteOffsetToLine for test comparison.
fn oldByteOffsetToLine(source: []const u8, offset: u32) u32 {
    var line: u32 = 1;
    const end = @min(@as(usize, offset), source.len);
    var i: usize = 0;
    while (i < end) : (i += 1) {
        if (source[i] == '\n') line += 1;
    }
    return line;
}

fn expectLineIndexMatches(source: []const u8) Allocator.Error!void {
    const gpa = std.testing.allocator;
    const index = try LineIndex.build(gpa, source);
    defer index.deinit(gpa);

    const end: u32 = @intCast(source.len);
    var offset: u32 = 0;
    while (offset <= end) : (offset += 1) {
        const expected = oldByteOffsetToLine(source, offset);
        const actual = index.lineOf(offset);
        if (expected != actual) {
            std.debug.panic("lineOf({d}): expected {d}, got {d}", .{ offset, expected, actual });
        }
    }
}

test "LineIndex: empty source" {
    try expectLineIndexMatches("");
}

test "LineIndex: single line, no newline" {
    try expectLineIndexMatches("hello world");
}

test "LineIndex: single trailing newline" {
    try expectLineIndexMatches("hello\n");
}

test "LineIndex: two lines" {
    try expectLineIndexMatches("hello\nworld");
}

test "LineIndex: two lines with trailing newline" {
    try expectLineIndexMatches("hello\nworld\n");
}

test "LineIndex: multiple lines" {
    try expectLineIndexMatches("line1\nline2\nline3\nline4");
}

test "LineIndex: consecutive newlines" {
    try expectLineIndexMatches("a\n\n\nb\n");
}

test "LineIndex: offset at newline byte" {
    try expectLineIndexMatches("abc\ndef\nghi");
}

test "LineIndex: offset beyond source length clamps to line count" {
    const gpa = std.testing.allocator;
    const source = "one\ntwo\nthree";
    const index = try LineIndex.build(gpa, source);
    defer index.deinit(gpa);

    // Offset past end of source should still return the last line
    const beyond: u32 = @intCast(source.len + 100);
    const expected = oldByteOffsetToLine(source, beyond);
    const actual = index.lineOf(beyond);
    try std.testing.expectEqual(expected, actual);
}

test "LineIndex: offset zero" {
    const gpa = std.testing.allocator;
    const source = "first\nsecond\nthird";
    const index = try LineIndex.build(gpa, source);
    defer index.deinit(gpa);

    try std.testing.expectEqual(@as(u32, 1), index.lineOf(0));
}

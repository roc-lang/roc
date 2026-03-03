//! Extraction of documentation data from compiled Roc modules.
//!
//! This module provides functions to extract doc comments from source text,
//! build structured type representations, and build `DocModel` structs from a `ModuleEnv`.

const std = @import("std");
const base = @import("base");
const CIR = @import("can").CIR;
const ModuleEnv = @import("can").ModuleEnv;
const types_mod = @import("types").types;

const DocModel = @import("DocModel.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;

const TypeStore = @import("types").Store;
const Var = types_mod.Var;
const FlatType = types_mod.FlatType;
const NominalType = types_mod.NominalType;

const DocType = DocModel.DocType;
const TypeAnno = CIR.TypeAnno;

/// Extract the module-level doc comment from the top of a source file.
///
/// Module doc comments are consecutive `##` lines at the very beginning of the
/// file, before any non-comment content. Returns null if none found.
pub fn extractModuleDocComment(gpa: Allocator, source: []const u8) !?[]const u8 {
    var lines = std.ArrayList([]const u8).empty;
    defer lines.deinit(gpa);

    var pos: usize = 0;
    while (pos < source.len) {
        // Skip leading whitespace on the line (spaces/tabs only)
        while (pos < source.len and (source[pos] == ' ' or source[pos] == '\t')) {
            pos += 1;
        }

        // Check for ## doc comment
        if (pos + 2 <= source.len and source[pos] == '#' and source[pos + 1] == '#') {
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
            // Empty line — skip but keep looking
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

    // Join lines with newlines
    return try joinLines(gpa, lines.items);
}

/// Extract the doc comment immediately preceding a definition at the given byte offset.
///
/// Scans backwards from `def_start_offset` to find consecutive `##` lines.
/// Returns null if no doc comment is found.
pub fn extractDocComment(gpa: Allocator, source: []const u8, def_start_offset: u32) !?[]const u8 {
    if (def_start_offset == 0 or def_start_offset > source.len) return null;

    var lines = std.ArrayList([]const u8).empty;
    defer lines.deinit(gpa);

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

        if (trimmed.len >= 2 and trimmed[0] == '#' and trimmed[1] == '#') {
            // It's a doc comment line
            var content = trimmed[2..];
            // Skip optional leading space after ##
            if (content.len > 0 and content[0] == ' ') {
                content = content[1..];
            }
            try lines.append(gpa, content);
        } else if (trimmed.len == 0) {
            // Empty/whitespace line — stop looking if we already have doc lines
            if (lines.items.len > 0) break;
            // Skip empty lines between def and potential doc comment
        } else {
            // Non-comment content — stop
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

    return try joinLines(gpa, lines.items);
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
pub fn extractModuleDocs(gpa: Allocator, module_env: *const ModuleEnv, package_name: []const u8) !DocModel.ModuleDocs {
    const source = module_env.getSourceAll();

    // Extract module-level doc comment
    const module_doc = try extractModuleDocComment(gpa, source);
    errdefer if (module_doc) |d| gpa.free(d);

    // Determine module kind
    const kind = convertModuleKind(module_env.module_kind);

    // Get module name
    const name = try gpa.dupe(u8, module_env.module_name);
    errdefer gpa.free(name);

    // Dupe package name
    const pkg_name = try gpa.dupe(u8, package_name);
    errdefer gpa.free(pkg_name);

    // Collect entries from exported defs
    var entries_list = std.ArrayList(DocModel.DocEntry).empty;
    defer {
        for (entries_list.items) |*e| e.deinit(gpa);
        entries_list.deinit(gpa);
    }

    // For documentation purposes, show all accessible definitions, not just
    // what's explicitly exported. Exports control compilation/linking (what
    // other modules can import), but docs should be comprehensive.
    const defs_slice = switch (module_env.module_kind) {
        .platform, .hosted => blk: {
            // Platforms and hosted modules: only document explicitly provided items
            const exports_slice = module_env.store.sliceDefs(module_env.exports);
            break :blk if (exports_slice.len > 0)
                exports_slice
            else
                module_env.store.sliceDefs(module_env.all_defs);
        },
        else => module_env.store.sliceDefs(module_env.all_defs),
    };

    for (defs_slice) |def_idx| {
        if (try extractDefEntry(gpa, module_env, def_idx, source)) |entry| {
            // Skip internal Builtin functions
            if (std.mem.eql(u8, package_name, "Builtin") and isInternalBuiltin(entry.name)) {
                var mutable_entry = entry;
                mutable_entry.deinit(gpa);
            } else {
                try entries_list.append(gpa, entry);
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
                // Skip if already in entries
                if (findEntryByName(entries_list.items, entry_name)) continue;
                // Skip internal Builtin types
                if (std.mem.eql(u8, package_name, "Builtin") and isInternalBuiltin(entry_name)) continue;

                const region = module_env.store.getStatementRegion(stmt_idx);
                const doc_comment = try extractDocComment(gpa, source, region.start.offset);
                errdefer if (doc_comment) |d| gpa.free(d);

                const type_sig = try extractDeclTypeSig(gpa, module_env, decl.anno);
                errdefer if (type_sig) |s| {
                    s.deinit(gpa);
                    gpa.destroy(s);
                };

                const duped_name = try gpa.dupe(u8, entry_name);
                errdefer gpa.free(duped_name);

                const empty_children = try gpa.alloc(DocModel.DocEntry, 0);
                errdefer gpa.free(empty_children);

                try entries_list.append(gpa, DocModel.DocEntry{
                    .name = duped_name,
                    .kind = .alias,
                    .type_signature = type_sig,
                    .doc_comment = doc_comment,
                    .children = empty_children,
                });
            },
            .s_nominal_decl => |decl| {
                const header = module_env.store.getTypeHeader(decl.header);
                const entry_name = module_env.getIdentText(header.relative_name);
                if (findEntryByName(entries_list.items, entry_name)) continue;
                // Skip internal Builtin types
                if (std.mem.eql(u8, package_name, "Builtin") and isInternalBuiltin(entry_name)) continue;

                const region = module_env.store.getStatementRegion(stmt_idx);
                const doc_comment = try extractDocComment(gpa, source, region.start.offset);
                errdefer if (doc_comment) |d| gpa.free(d);

                const type_sig = try extractDeclTypeSig(gpa, module_env, decl.anno);
                errdefer if (type_sig) |s| {
                    s.deinit(gpa);
                    gpa.destroy(s);
                };

                const duped_name = try gpa.dupe(u8, entry_name);
                errdefer gpa.free(duped_name);

                const empty_children = try gpa.alloc(DocModel.DocEntry, 0);
                errdefer gpa.free(empty_children);

                try entries_list.append(gpa, DocModel.DocEntry{
                    .name = duped_name,
                    .kind = if (decl.is_opaque) .@"opaque" else .nominal,
                    .type_signature = type_sig,
                    .doc_comment = doc_comment,
                    .children = empty_children,
                });
            },
            else => {},
        }
    }

    // Build hierarchical structure: move methods under their parent types
    var i: usize = 0;
    while (i < entries_list.items.len) {
        const entry = &entries_list.items[i];

        // Check if this is a method (name contains ".")
        if (std.mem.lastIndexOfScalar(u8, entry.name, '.')) |dot_idx| {
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

                // Duplicate the method entry with short name
                const short_name = try gpa.dupe(u8, method_short_name);
                errdefer gpa.free(short_name);

                var method_entry = entry.*; // Copy entry
                gpa.free(method_entry.name); // Free old qualified name
                method_entry.name = short_name; // Use short name

                // Add to parent's children
                try appendChildEntry(gpa, parent, method_entry);

                // Remove from top-level list (preserving source order)
                gpa.free(entry.children); // Free empty children array
                _ = entries_list.orderedRemove(i);
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
    // The Builtin module is a special case — it contains many top-level types
    // (Str, List, Bool, etc.) that need complex re-parenting rather than simple
    // filtering, so it is handled separately below.
    const is_builtin = std.mem.eql(u8, module_env.module_name, "Builtin");
    if (module_env.module_kind == .type_module and !is_builtin) {
        try filterTypeModuleEntries(gpa, &entries_list, module_env.module_name);
    }

    // Re-parent Builtin opaque type's children to their proper parent types.
    // The Builtin module has a single opaque "Builtin" entry with hundreds of
    // children like "Bool.not", "List.append", "Num.Dec.abs". We split those
    // dotted names and move each child under the matching top-level type.
    if (is_builtin) {
        try reparentBuiltinChildren(gpa, &entries_list);
    }

    const entries = try entries_list.toOwnedSlice(gpa);

    return DocModel.ModuleDocs{
        .name = name,
        .package_name = pkg_name,
        .kind = kind,
        .module_doc = module_doc,
        .entries = entries,
    };
}

/// Filter entries in a type module to only include the main type and its children.
///
/// In a type module (e.g. `Color.roc`), only the entry whose name matches the
/// module name is public. All other top-level entries — helper functions, internal
/// types, etc. — are private to the module and excluded from documentation.
/// The main type entry's children (methods defined inside `Color := [...].{...}`)
/// are preserved as nested entries.
fn filterTypeModuleEntries(
    gpa: Allocator,
    entries_list: *std.ArrayList(DocModel.DocEntry),
    module_name: []const u8,
) !void {
    var idx: usize = 0;
    while (idx < entries_list.items.len) {
        const entry = &entries_list.items[idx];
        // Keep the main type entry (name matches the module name) and any entries
        // that are prefixed with the module name (e.g. "Color.helper" which would
        // have been a method moved under Color by the hierarchical pass, but if not
        // yet moved, we still keep it).
        if (std.mem.eql(u8, entry.name, module_name) or
            (std.mem.startsWith(u8, entry.name, module_name) and
                entry.name.len > module_name.len and
                entry.name[module_name.len] == '.'))
        {
            idx += 1;
        } else {
            var removed = entries_list.orderedRemove(idx);
            removed.deinit(gpa);
        }
    }
}

/// Find the `Builtin` opaque entry, re-parent its dotted children under the
/// matching top-level types, and remove the `Builtin` entry itself.
/// Also strip "Builtin." prefix from top-level entries and re-parent them.
fn reparentBuiltinChildren(gpa: Allocator, entries_list: *std.ArrayList(DocModel.DocEntry)) !void {
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

    // Process each child — move it under its proper parent
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
    // Entries like "Builtin.Bool.decode" survived the earlier hierarchical pass
    // because lastIndexOf('.') gave parent "Builtin.Bool" which didn't exist.
    // First pass: strip "Builtin." prefix from all matching entries
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
    // (same logic as the original hierarchical pass)
    var j: usize = 0;
    while (j < entries_list.items.len) {
        const entry = &entries_list.items[j];
        if (std.mem.indexOfScalar(u8, entry.name, '.')) |dot_idx| {
            const parent_name = entry.name[0..dot_idx];
            const method_short_name = entry.name[dot_idx + 1 ..];

            // Find parent in entries_list
            var parent_idx_opt: ?usize = null;
            for (entries_list.items, 0..) |*potential_parent, idx| {
                if (idx != j and std.mem.eql(u8, potential_parent.name, parent_name)) {
                    parent_idx_opt = idx;
                    break;
                }
            }

            if (parent_idx_opt) |parent_idx| {
                const parent_ptr = &entries_list.items[parent_idx];

                // Duplicate the method entry with short name
                const short_name = try gpa.dupe(u8, method_short_name);

                var method_entry = entry.*;
                gpa.free(method_entry.name);
                method_entry.name = short_name;

                // Check if remainder has more dots — if so, use reparentDottedChildInto
                if (std.mem.indexOfScalar(u8, method_short_name, '.')) |_| {
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

                // Remove from top-level list (preserving source order)
                gpa.free(entry.children);
                _ = entries_list.orderedRemove(j);
                continue;
            }
        }
        j += 1;
    }

    // Remove top-level value entries that are NOT part of the Builtin opaque's
    // public API. In Builtin.roc, all public items are type declarations inside
    // `Builtin :: [].{...}`. Standalone value entries like range_to, range_until
    // are module-private helpers and should not appear in documentation.
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

/// Recursively re-parent a child with a dotted name (e.g. "Bool.not" or "Dec.abs")
/// into the correct position in entries_list. If the target parent doesn't exist
/// as a top-level entry, create a group entry for it.
fn reparentDottedChild(
    gpa: Allocator,
    entries_list: *std.ArrayList(DocModel.DocEntry),
    child: DocModel.DocEntry,
) !void {
    // Split on first dot
    const dot_idx = std.mem.indexOfScalar(u8, child.name, '.') orelse {
        // No dot — this is a direct child. Nothing to re-parent into a subgroup;
        // it stays at top level as-is (shouldn't normally happen for Builtin children).
        try entries_list.append(gpa, child);
        return;
    };

    const parent_name = child.name[0..dot_idx];
    const remainder = child.name[dot_idx + 1 ..];

    // Find the matching top-level entry
    var parent: ?*DocModel.DocEntry = null;
    for (entries_list.items) |*entry| {
        if (std.mem.eql(u8, entry.name, parent_name)) {
            parent = entry;
            break;
        }
    }

    // If no parent exists, create a group entry
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

    // Create the child entry with shortened name (remainder)
    var new_child = child;
    // We need to allocate a new name for the remainder
    const short_name = try gpa.dupe(u8, remainder);
    gpa.free(child.name); // free old dotted name
    new_child.name = short_name;

    // Check if remainder still has dots (multi-level, e.g. "Dec.abs")
    if (std.mem.indexOfScalar(u8, remainder, '.')) |_| {
        // Recursively place into sub-children
        // Convert parent's children to an ArrayList temporarily
        var children_list = std.ArrayList(DocModel.DocEntry).empty;
        for (p.children) |c| {
            try children_list.append(gpa, c);
        }
        gpa.free(p.children);

        try reparentDottedChildInto(gpa, &children_list, new_child);

        p.children = try children_list.toOwnedSlice(gpa);
    } else {
        // Simple case — just append to parent's children
        try appendChildEntry(gpa, p, new_child);
    }
}

/// Like reparentDottedChild but operates on a children ArrayList (for nested levels).
fn reparentDottedChildInto(
    gpa: Allocator,
    children_list: *std.ArrayList(DocModel.DocEntry),
    child: DocModel.DocEntry,
) !void {
    const dot_idx = std.mem.indexOfScalar(u8, child.name, '.') orelse {
        // Leaf — just append
        try children_list.append(gpa, child);
        return;
    };

    const parent_name = child.name[0..dot_idx];
    const remainder = child.name[dot_idx + 1 ..];

    // Find or create intermediate group
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

    // Shorten the child name
    var new_child = child;
    const short_name = try gpa.dupe(u8, remainder);
    gpa.free(child.name);
    new_child.name = short_name;

    if (std.mem.indexOfScalar(u8, remainder, '.')) |_| {
        // Still has dots — recurse deeper
        var sub_children = std.ArrayList(DocModel.DocEntry).empty;
        for (p.children) |c| {
            try sub_children.append(gpa, c);
        }
        gpa.free(p.children);
        try reparentDottedChildInto(gpa, &sub_children, new_child);
        p.children = try sub_children.toOwnedSlice(gpa);
    } else {
        // Leaf — append to parent's children
        try appendChildEntry(gpa, p, new_child);
    }
}

// --- Internal helpers ---

fn extractDefEntry(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    def_idx: CIR.Def.Idx,
    source: []const u8,
) !?DocModel.DocEntry {
    const def = module_env.store.getDef(def_idx);
    const pattern = module_env.store.getPattern(def.pattern);

    switch (pattern) {
        .assign => |a| {
            const ident_name = module_env.getIdentText(a.ident);
            const duped_name = try gpa.dupe(u8, ident_name);
            errdefer gpa.free(duped_name);

            // Get the byte offset for doc comment scanning
            const offset = getDefSourceOffset(module_env, def);
            const doc_comment = try extractDocComment(gpa, source, offset);
            errdefer if (doc_comment) |d| gpa.free(d);

            // Extract structured type from inferred type
            const type_sig: ?*const DocType = blk: {
                const def_var = ModuleEnv.varFrom(def_idx);
                if (@intFromEnum(def_var) >= module_env.types.len()) break :blk null;
                break :blk extractDocType(
                    gpa,
                    &module_env.types,
                    module_env.getIdentStoreConst(),
                    def_var,
                ) catch break :blk null;
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
                .doc_comment = doc_comment,
                .children = empty_children,
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

                    // Use the statement region for doc comment scanning
                    const region = module_env.store.getStatementRegion(n.nominal_type_decl);
                    const doc_comment = try extractDocComment(gpa, source, region.start.offset);
                    errdefer if (doc_comment) |d| gpa.free(d);

                    const type_sig = try extractDeclTypeSig(gpa, module_env, decl.anno);
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
                        .kind = if (decl.is_opaque) .@"opaque" else .nominal,
                        .type_signature = type_sig,
                        .doc_comment = doc_comment,
                        .children = children,
                    };
                },
                else => return null,
            }
        },
        else => return null,
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
) ![]DocModel.DocEntry {
    const expr = module_env.store.getExpr(def.expr);
    switch (expr) {
        .e_nominal => |nom| {
            const backing = module_env.store.getExpr(nom.backing_expr);
            switch (backing) {
                .e_record => |rec| {
                    return try extractRecordChildren(gpa, module_env, rec.fields);
                },
                else => {},
            }
        },
        else => {},
    }
    return try gpa.alloc(DocModel.DocEntry, 0);
}

fn extractRecordChildren(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    fields: CIR.RecordField.Span,
) ![]DocModel.DocEntry {
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
    anno_idx: CIR.TypeAnno.Idx,
) !?*const DocType {
    // Extract the backing type from the CIR annotation. The inferred type for a
    // nominal resolves to the nominal itself, so we use the annotation instead.
    // DocEntry.writeToSExpr generates the declaration prefix from kind + name.
    return try extractTypeAnnoAsDocType(gpa, module_env, anno_idx);
}

/// Resolve the module path from a CIR TypeAnno's LocalOrExternal base.
fn resolveModulePathFromBase(
    module_env: *const ModuleEnv,
    local_or_ext: TypeAnno.LocalOrExternal,
) []const u8 {
    return switch (local_or_ext) {
        .builtin => "", // Don't expose "Builtin" module as it's an implementation detail
        .local => module_env.module_name,
        .external => |ext| blk: {
            const idx = @intFromEnum(ext.module_idx);
            if (idx >= module_env.imports.imports.items.items.len) break :blk "";
            const str_idx = module_env.imports.imports.items.items[idx];
            break :blk module_env.common.getString(str_idx);
        },
        .pending => |pend| blk: {
            const idx = @intFromEnum(pend.module_idx);
            if (idx >= module_env.imports.imports.items.items.len) break :blk "";
            const str_idx = module_env.imports.imports.items.items[idx];
            break :blk module_env.common.getString(str_idx);
        },
    };
}

/// Extract a CIR TypeAnno as a structured DocType.
fn extractTypeAnnoAsDocType(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    type_anno_idx: CIR.TypeAnno.Idx,
) !?*const DocType {
    const anno = module_env.store.getTypeAnno(type_anno_idx);
    switch (anno) {
        .apply => |a| {
            const name = module_env.getIdentText(a.name);
            const args_slice = module_env.store.sliceTypeAnnos(a.args);

            const module_path = resolveModulePathFromBase(module_env, a.base);

            if (args_slice.len > 0) {
                // Type application: List(Str), Result(ok, err)
                const constructor = try allocDocType(gpa, .{ .type_ref = .{
                    .module_path = try gpa.dupe(u8, module_path),
                    .type_name = try gpa.dupe(u8, name),
                } });
                errdefer {
                    constructor.deinit(gpa);
                    gpa.destroy(constructor);
                }

                var args = try gpa.alloc(*const DocType, args_slice.len);
                errdefer {
                    for (args) |arg| {
                        arg.deinit(gpa);
                        gpa.destroy(arg);
                    }
                    gpa.free(args);
                }

                for (args_slice, 0..) |arg_idx, i| {
                    args[i] = try extractTypeAnnoAsDocType(gpa, module_env, arg_idx) orelse
                        try allocDocType(gpa, .@"error");
                }

                return try allocDocType(gpa, .{ .apply = .{
                    .constructor = constructor,
                    .args = args,
                } });
            } else {
                // Simple type reference: Str, U64, etc.
                return try allocDocType(gpa, .{ .type_ref = .{
                    .module_path = try gpa.dupe(u8, module_path),
                    .type_name = try gpa.dupe(u8, name),
                } });
            }
        },
        .rigid_var => |tv| {
            return try allocDocType(gpa, .{ .type_var = try gpa.dupe(u8, module_env.getIdentText(tv.name)) });
        },
        .rigid_var_lookup => |rv| {
            return try extractTypeAnnoAsDocType(gpa, module_env, rv.ref);
        },
        .underscore => {
            return try allocDocType(gpa, .wildcard);
        },
        .lookup => |t| {
            const name = module_env.getIdentText(t.name);
            const module_path = resolveModulePathFromBase(module_env, t.base);
            return try allocDocType(gpa, .{ .type_ref = .{
                .module_path = try gpa.dupe(u8, module_path),
                .type_name = try gpa.dupe(u8, name),
            } });
        },
        .tag_union => |tu| {
            const tags_slice = module_env.store.sliceTypeAnnos(tu.tags);
            var tags = try gpa.alloc(DocType.Tag, tags_slice.len);
            errdefer gpa.free(tags);

            for (tags_slice, 0..) |tag_idx, i| {
                const tag_anno = module_env.store.getTypeAnno(tag_idx);
                switch (tag_anno) {
                    .tag => |t| {
                        const tag_name = try gpa.dupe(u8, module_env.getIdentText(t.name));
                        const tag_args_slice = module_env.store.sliceTypeAnnos(t.args);
                        var tag_args = try gpa.alloc(*const DocType, tag_args_slice.len);
                        for (tag_args_slice, 0..) |arg_idx, j| {
                            tag_args[j] = try extractTypeAnnoAsDocType(gpa, module_env, arg_idx) orelse
                                try allocDocType(gpa, .@"error");
                        }
                        tags[i] = .{ .name = tag_name, .args = tag_args };
                    },
                    else => {
                        tags[i] = .{ .name = try gpa.dupe(u8, "?"), .args = try gpa.alloc(*const DocType, 0) };
                    },
                }
            }

            var ext: ?*const DocType = null;
            if (tu.ext) |ext_idx| {
                ext = try extractTypeAnnoAsDocType(gpa, module_env, ext_idx);
            }

            return try allocDocType(gpa, .{ .tag_union = .{
                .tags = tags,
                .ext = ext,
            } });
        },
        .tag => |t| {
            // A bare tag (shouldn't normally appear at top level, but handle it)
            const tag_name = try gpa.dupe(u8, module_env.getIdentText(t.name));
            const tag_args_slice = module_env.store.sliceTypeAnnos(t.args);
            var tag_args = try gpa.alloc(*const DocType, tag_args_slice.len);
            for (tag_args_slice, 0..) |arg_idx, i| {
                tag_args[i] = try extractTypeAnnoAsDocType(gpa, module_env, arg_idx) orelse
                    try allocDocType(gpa, .@"error");
            }

            var tags = try gpa.alloc(DocType.Tag, 1);
            tags[0] = .{ .name = tag_name, .args = tag_args };
            return try allocDocType(gpa, .{ .tag_union = .{
                .tags = tags,
                .ext = null,
            } });
        },
        .tuple => |t| {
            const elems_slice = module_env.store.sliceTypeAnnos(t.elems);
            var elems = try gpa.alloc(*const DocType, elems_slice.len);
            for (elems_slice, 0..) |elem_idx, i| {
                elems[i] = try extractTypeAnnoAsDocType(gpa, module_env, elem_idx) orelse
                    try allocDocType(gpa, .@"error");
            }
            return try allocDocType(gpa, .{ .tuple = .{ .elems = elems } });
        },
        .record => |r| {
            const fields_slice = module_env.store.sliceAnnoRecordFields(r.fields);
            var fields = try gpa.alloc(DocType.Field, fields_slice.len);
            for (fields_slice, 0..) |field_idx, i| {
                const field = module_env.store.getAnnoRecordField(field_idx);
                fields[i] = .{
                    .name = try gpa.dupe(u8, module_env.getIdentText(field.name)),
                    .type = try extractTypeAnnoAsDocType(gpa, module_env, field.ty) orelse
                        try allocDocType(gpa, .@"error"),
                };
            }

            var ext: ?*const DocType = null;
            if (r.ext) |ext_idx| {
                ext = try extractTypeAnnoAsDocType(gpa, module_env, ext_idx);
            }

            return try allocDocType(gpa, .{ .record = .{
                .fields = fields,
                .ext = ext,
            } });
        },
        .@"fn" => |f| {
            const args_slice = module_env.store.sliceTypeAnnos(f.args);
            var args = try gpa.alloc(*const DocType, args_slice.len);
            for (args_slice, 0..) |arg_idx, i| {
                args[i] = try extractTypeAnnoAsDocType(gpa, module_env, arg_idx) orelse
                    try allocDocType(gpa, .@"error");
            }
            const ret = try extractTypeAnnoAsDocType(gpa, module_env, f.ret) orelse
                try allocDocType(gpa, .@"error");

            return try allocDocType(gpa, .{ .function = .{
                .args = args,
                .ret = ret,
                .effectful = f.effectful,
            } });
        },
        .parens => |p| {
            return try extractTypeAnnoAsDocType(gpa, module_env, p.anno);
        },
        .malformed => {
            return try allocDocType(gpa, .@"error");
        },
    }
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
    idents: *const Ident.Store,
    seen: std.ArrayList(Var),
    constraints_list: std.ArrayList(ConstraintInfo),
    flex_names: std.AutoHashMap(Var, []const u8),
    next_name_idx: u32,

    fn init(gpa: Allocator, types: *const TypeStore, idents_store: *const Ident.Store) ExtractContext {
        return .{
            .gpa = gpa,
            .types = types,
            .idents = idents_store,
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
    idents: *const Ident.Store,
    var_: Var,
) ExtractError!?*const DocType {
    var ctx = ExtractContext.init(gpa, types, idents);
    defer ctx.deinit();

    const base_type = try extractDocTypeInner(&ctx, var_);
    if (base_type == null) return null;

    // If there are constraints, wrap in a where clause
    if (ctx.constraints_list.items.len > 0) {
        // Deduplicate constraints by (dispatcher_var_name, fn_name)
        var unique_constraints = std.ArrayList(DocType.Constraint).empty;
        defer {
            for (unique_constraints.items) |*c| {
                gpa.free(c.type_var);
                gpa.free(c.method_name);
                c.signature.deinit(gpa);
                gpa.destroy(c.signature);
            }
            unique_constraints.deinit(gpa);
        }

        for (ctx.constraints_list.items) |info| {
            // Check for duplicate
            var is_dup = false;
            for (unique_constraints.items) |existing| {
                if (std.mem.eql(u8, existing.type_var, info.dispatcher_name) and
                    std.mem.eql(u8, existing.method_name, info.fn_name_text))
                {
                    is_dup = true;
                    break;
                }
            }
            if (is_dup) continue;

            // Extract the constraint function's type using a fresh context
            // to avoid cycles with the main type's seen list.
            var fn_ctx = ExtractContext.init(gpa, types, idents);
            defer fn_ctx.deinit();

            const fn_type = try extractDocTypeInner(&fn_ctx, info.fn_var) orelse
                try allocDocType(gpa, .@"error");

            try unique_constraints.append(gpa, .{
                .type_var = try gpa.dupe(u8, info.dispatcher_name),
                .method_name = try gpa.dupe(u8, info.fn_name_text),
                .signature = fn_type,
            });
        }

        // Sort constraints alphabetically by (type_var, method_name)
        std.mem.sort(DocType.Constraint, unique_constraints.items, {}, struct {
            fn lessThan(_: void, a: DocType.Constraint, b: DocType.Constraint) bool {
                const type_cmp = std.mem.order(u8, a.type_var, b.type_var);
                if (type_cmp != .eq) return type_cmp == .lt;
                return std.mem.order(u8, a.method_name, b.method_name) == .lt;
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
        } });
    }

    return base_type;
}

const ConstraintInfo = struct {
    dispatcher_name: []const u8, // borrowed from idents store
    fn_name_text: []const u8, // borrowed from idents store
    fn_var: Var,
};

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
            // Check for from_numeral constraint -> default to Dec
            const constraints = types.sliceStaticDispatchConstraints(flex.constraints);
            var has_numeral = false;
            for (constraints) |constraint| {
                if (constraint.origin == .from_numeral) {
                    has_numeral = true;
                    break;
                }
            }

            if (has_numeral) {
                // Default numeral types to Dec for display
                return try allocDocType(gpa, .{ .type_ref = .{
                    .module_path = try gpa.dupe(u8, "Num"),
                    .type_name = try gpa.dupe(u8, "Dec"),
                } });
            }

            // Get the variable name
            const var_name = if (flex.name) |ident_idx|
                try gpa.dupe(u8, idents.getText(ident_idx))
            else
                try ctx.getFlexVarName(resolved.var_);

            // Collect non-numeral constraints for where clause
            for (constraints) |constraint| {
                if (constraint.origin != .from_numeral) {
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
            const origin_text = idents.getText(alias.origin_module);
            const ident_text = idents.getText(alias.ident.ident_idx);
            const display_name = getDisplayName(origin_text, ident_text);

            // Get module path for the type reference
            const module_path = getModulePath(origin_text);

            var args_iter = types.iterAliasArgs(alias);
            if (args_iter.count() > 0) {
                // Type application
                const constructor = try allocDocType(gpa, .{ .type_ref = .{
                    .module_path = try gpa.dupe(u8, module_path),
                    .type_name = try gpa.dupe(u8, display_name),
                } });
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
                } });
            } else {
                // Simple type reference
                return try allocDocType(gpa, .{ .type_ref = .{
                    .module_path = try gpa.dupe(u8, module_path),
                    .type_name = try gpa.dupe(u8, display_name),
                } });
            }
        },
        .structure => |flat_type| {
            return try extractFlatType(ctx, flat_type);
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
            } });
        },
        .empty_tag_union => {
            return try allocDocType(gpa, .{ .tag_union = .{
                .tags = try gpa.alloc(DocType.Tag, 0),
                .ext = null,
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
    const origin_text = idents.getText(nominal.origin_module);
    const ident_text = idents.getText(nominal.ident.ident_idx);
    const display_name = getDisplayName(origin_text, ident_text);
    const module_path = getModulePath(origin_text);

    var args_iter = ctx.types.iterNominalArgs(nominal);
    if (args_iter.count() > 0) {
        const constructor = try allocDocType(gpa, .{ .type_ref = .{
            .module_path = try gpa.dupe(u8, module_path),
            .type_name = try gpa.dupe(u8, display_name),
        } });
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
        } });
    } else {
        return try allocDocType(gpa, .{ .type_ref = .{
            .module_path = try gpa.dupe(u8, module_path),
            .type_name = try gpa.dupe(u8, display_name),
        } });
    }
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
    for (initial_slice.items(.name), initial_slice.items(.var_)) |name, field_var| {
        try all_fields.append(gpa, .{ .name = name, .var_ = field_var });
    }

    // Follow the extension chain
    var ext = record.ext;
    var ext_doc_type: ?*const DocType = null;
    var guard_count: usize = 0;
    while (guard_count < 100) : (guard_count += 1) {
        const ext_resolved = types.resolveVar(ext);
        switch (ext_resolved.desc.content) {
            .flex => |flex| {
                const var_name = if (flex.name) |ident_idx|
                    try gpa.dupe(u8, idents.getText(ident_idx))
                else
                    try ctx.getFlexVarName(ext_resolved.var_);

                // Collect constraints from the extension variable
                const constraints = types.sliceStaticDispatchConstraints(flex.constraints);
                for (constraints) |constraint| {
                    if (constraint.origin != .from_numeral) {
                        const dispatcher_name = if (flex.name) |ident_idx| idents.getText(ident_idx) else var_name;
                        try ctx.constraints_list.append(gpa, .{
                            .dispatcher_name = dispatcher_name,
                            .fn_name_text = idents.getText(constraint.fn_name),
                            .fn_var = constraint.fn_var,
                        });
                    }
                }

                ext_doc_type = try allocDocType(gpa, .{ .type_var = var_name });
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

                ext_doc_type = try allocDocType(gpa, .{ .type_var = try gpa.dupe(u8, var_name) });
                break;
            },
            .alias => |alias| {
                ext = types.getAliasBackingVar(alias);
            },
            .structure => |ft| {
                switch (ft) {
                    .record => |ext_record| {
                        const ext_slice = types.getRecordFieldsSlice(ext_record.fields);
                        for (ext_slice.items(.name), ext_slice.items(.var_)) |name, field_var| {
                            try all_fields.append(gpa, .{ .name = name, .var_ = field_var });
                        }
                        ext = ext_record.ext;
                    },
                    .record_unbound => |ext_fields| {
                        const ext_slice = types.getRecordFieldsSlice(ext_fields);
                        for (ext_slice.items(.name), ext_slice.items(.var_)) |name, field_var| {
                            try all_fields.append(gpa, .{ .name = name, .var_ = field_var });
                        }
                        break;
                    },
                    .empty_record => break,
                    else => break,
                }
            },
            else => break,
        }
    }

    // Sort fields alphabetically
    std.mem.sort(types_mod.RecordField, all_fields.items, idents, comptime types_mod.RecordField.sortByNameAsc);

    // Build DocType.Field array from gathered fields
    var doc_fields = try gpa.alloc(DocType.Field, all_fields.items.len);
    for (all_fields.items, 0..) |field, i| {
        doc_fields[i] = .{
            .name = try gpa.dupe(u8, idents.getText(field.name)),
            .type = try extractDocTypeInner(ctx, field.var_) orelse
                try allocDocType(gpa, .@"error"),
        };
    }

    return try allocDocType(gpa, .{ .record = .{
        .fields = doc_fields,
        .ext = ext_doc_type,
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
        } });
    }

    const slice = ctx.types.getRecordFieldsSlice(fields_range);
    const names = slice.items(.name);
    const vars = slice.items(.var_);
    var fields = try gpa.alloc(DocType.Field, names.len);
    for (names, vars, 0..) |name, field_var, i| {
        fields[i] = .{
            .name = try gpa.dupe(u8, ctx.idents.getText(name)),
            .type = try extractDocTypeInner(ctx, field_var) orelse
                try allocDocType(gpa, .@"error"),
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
    return try allocDocType(gpa, .{ .tuple = .{ .elems = elems } });
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

        try tags.append(gpa, .{ .name = tag_name, .args = tag_args });
    }

    // Handle extension variable
    var ext_type: ?*const DocType = null;
    const ext_resolved = types.resolveVar(tag_union.ext);
    switch (ext_resolved.desc.content) {
        .flex => |flex| {
            if (flex.name) |ident_idx| {
                ext_type = try allocDocType(gpa, .{ .type_var = try gpa.dupe(u8, idents.getText(ident_idx)) });
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
            ext_type = try allocDocType(gpa, .{ .type_var = try gpa.dupe(u8, idents.getText(rigid.name)) });

            const constraints = types.sliceStaticDispatchConstraints(rigid.constraints);
            for (constraints) |constraint| {
                try ctx.constraints_list.append(gpa, .{
                    .dispatcher_name = idents.getText(rigid.name),
                    .fn_name_text = idents.getText(constraint.fn_name),
                    .fn_var = constraint.fn_var,
                });
            }
        },
        .structure => |ft| switch (ft) {
            .empty_tag_union => {}, // closed union
            else => {
                ext_type = try extractDocTypeInner(ctx, tag_union.ext);
            },
        },
        .alias => {
            ext_type = try extractDocTypeInner(ctx, tag_union.ext);
        },
        .err => {},
    }

    const tags_slice = try tags.toOwnedSlice(gpa);
    return try allocDocType(gpa, .{ .tag_union = .{
        .tags = tags_slice,
        .ext = ext_type,
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
/// Returns empty string for "Builtin" since it's an implementation detail.
fn getModulePath(origin_text: []const u8) []const u8 {
    if (std.mem.eql(u8, origin_text, "Builtin")) {
        return ""; // Don't expose "Builtin" module as it's an implementation detail
    }
    return origin_text;
}

fn convertModuleKind(kind: ModuleEnv.ModuleKind) DocModel.ModuleKind {
    return switch (kind) {
        .app, .default_app => .app,
        .package => .package,
        .platform => .platform,
        .type_module => .type_module,
        else => .app, // deprecated_module, hosted, malformed → treat as app
    };
}

fn isInternalBuiltin(name: []const u8) bool {
    // Filter out unsafe/internal builtin functions
    return std.mem.endsWith(u8, name, "_unsafe") or
        std.mem.endsWith(u8, name, "_lossy");
}

fn findEntryByName(entries: []const DocModel.DocEntry, name: []const u8) bool {
    for (entries) |entry| {
        if (std.mem.eql(u8, entry.name, name)) return true;
    }
    return false;
}

fn joinLines(gpa: Allocator, lines: []const []const u8) ![]u8 {
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
fn appendChildEntry(gpa: Allocator, parent: *DocModel.DocEntry, child: DocModel.DocEntry) !void {
    const old = parent.children;
    const new_children = try gpa.alloc(DocModel.DocEntry, old.len + 1);
    @memcpy(new_children[0..old.len], old);
    new_children[old.len] = child;
    gpa.free(old);
    parent.children = new_children;
}

fn trimLeft(s: []const u8) []const u8 {
    var i: usize = 0;
    while (i < s.len and (s[i] == ' ' or s[i] == '\t')) {
        i += 1;
    }
    return s[i..];
}

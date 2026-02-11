//! Extraction of documentation data from compiled Roc modules.
//!
//! This module provides functions to extract doc comments from source text,
//! render type signatures, and build `DocModel` structs from a `ModuleEnv`.

const std = @import("std");
const base = @import("base");
const CIR = @import("can").CIR;
const ModuleEnv = @import("can").ModuleEnv;

const DocModel = @import("DocModel.zig");
const render_type = @import("render_type.zig");

const Allocator = std.mem.Allocator;
const Region = base.Region;

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
        const line_start = pos;
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
        _ = line_start;
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
pub fn extractModuleDocs(gpa: Allocator, module_env: *const ModuleEnv) !DocModel.ModuleDocs {
    const source = module_env.getSourceAll();

    // Extract module-level doc comment
    const module_doc = try extractModuleDocComment(gpa, source);
    errdefer if (module_doc) |d| gpa.free(d);

    // Determine module kind
    const kind = convertModuleKind(module_env.module_kind);

    // Get module name
    const name = try gpa.dupe(u8, module_env.module_name);
    errdefer gpa.free(name);

    // Collect entries from exported defs
    var entries_list = std.ArrayList(DocModel.DocEntry).empty;
    defer {
        for (entries_list.items) |*e| e.deinit(gpa);
        entries_list.deinit(gpa);
    }

    const exports_slice = module_env.store.sliceDefs(module_env.exports);
    const defs_slice = if (exports_slice.len > 0)
        exports_slice
    else
        module_env.store.sliceDefs(module_env.all_defs);

    for (defs_slice) |def_idx| {
        if (try extractDefEntry(gpa, module_env, def_idx, source)) |entry| {
            try entries_list.append(gpa, entry);
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

                const region = module_env.store.getStatementRegion(stmt_idx);
                const doc_comment = try extractDocComment(gpa, source, region.start.offset);
                errdefer if (doc_comment) |d| gpa.free(d);

                const type_sig = try renderTypeDeclSig(gpa, module_env, decl.header, decl.anno, " : ");
                errdefer if (type_sig) |s| gpa.free(s);

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

                const region = module_env.store.getStatementRegion(stmt_idx);
                const doc_comment = try extractDocComment(gpa, source, region.start.offset);
                errdefer if (doc_comment) |d| gpa.free(d);

                const operator: []const u8 = if (decl.is_opaque) " :: " else " := ";
                const type_sig = try renderTypeDeclSig(gpa, module_env, decl.header, decl.anno, operator);
                errdefer if (type_sig) |s| gpa.free(s);

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

    const entries = try entries_list.toOwnedSlice(gpa);

    return DocModel.ModuleDocs{
        .name = name,
        .kind = kind,
        .module_doc = module_doc,
        .entries = entries,
    };
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

            // Render type signature
            const type_sig = if (def.annotation) |anno_idx| blk: {
                const annotation = module_env.store.getAnnotation(anno_idx);
                break :blk try render_type.renderTypeAnnoToString(gpa, module_env, annotation.anno);
            } else null;
            errdefer if (type_sig) |s| gpa.free(s);

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

                    const operator: []const u8 = if (decl.is_opaque) " :: " else " := ";
                    const type_sig = try renderTypeDeclSig(gpa, module_env, decl.header, decl.anno, operator);
                    errdefer if (type_sig) |s| gpa.free(s);

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

fn renderTypeDeclSig(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    header_idx: CIR.TypeHeader.Idx,
    anno_idx: CIR.TypeAnno.Idx,
    operator: []const u8,
) !?[]u8 {
    var buf = render_type.RenderBuffer.init();
    errdefer buf.deinit(gpa);
    render_type.renderTypeHeader(&buf, gpa, module_env, header_idx) catch return null;
    buf.list.appendSlice(gpa, operator) catch return null;
    render_type.renderTypeAnno(&buf, gpa, module_env, anno_idx, false) catch return null;
    const result: []u8 = buf.toOwnedSlice(gpa) catch return null;
    return result;
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

fn trimLeft(s: []const u8) []const u8 {
    var i: usize = 0;
    while (i < s.len and (s[i] == ' ' or s[i] == '\t')) {
        i += 1;
    }
    return s[i..];
}

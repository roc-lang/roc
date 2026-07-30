//! Data model for Roc documentation extraction.
//!
//! These structs represent the documentation data extracted from compiled modules.
//! They are serializable to a deterministic S-expression format for golden/snapshot testing.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Documentation for an entire package (or app/platform).
pub const PackageDocs = struct {
    name: []const u8,
    modules: []ModuleDocs,

    pub fn deinit(self: *PackageDocs, gpa: Allocator) void {
        for (self.modules) |*mod| {
            mod.deinit(gpa);
        }
        gpa.free(self.modules);
        gpa.free(self.name);
    }

    pub fn writeToSExpr(self: *const PackageDocs, writer: anytype) (Allocator.Error || error{WriteFailed})!void {
        try self.writeToSExprIndented(writer, 0);
    }

    pub fn writeToSExprIndented(self: *const PackageDocs, writer: anytype, depth: usize) (Allocator.Error || error{WriteFailed})!void {
        try writeIndent(writer, depth);
        try writer.writeAll("(package-docs\n");
        try writeIndent(writer, depth + 1);
        try writer.writeAll("(name \"");
        try writeEscaped(writer, self.name);
        try writer.writeAll("\")\n");
        for (self.modules) |*mod| {
            try mod.writeToSExpr(writer, depth + 1);
        }
        try writeIndent(writer, depth);
        try writer.writeAll(")\n");
    }

    /// Resolve every shorthand `[Name]` reference in doc comments against the
    /// final package docs layout.
    pub fn resolveDocRefs(self: *PackageDocs, gpa: Allocator) Allocator.Error!void {
        clearDocRefs(self, gpa);
        errdefer clearDocRefs(self, gpa);

        var resolver = try PackageDocRefResolver.init(gpa, self);
        defer resolver.deinit();

        for (self.modules) |*mod| {
            try resolver.resolveModule(gpa, mod);
        }
    }

    /// Promote the builtin types to top-level modules.
    ///
    /// The compiler models every builtin type (`Str`, `List`, `Num`, `Hasher`, …)
    /// as a nested type inside one big `Builtin` type, purely so they can refer to
    /// each other. Users never import `Builtin` and shouldn't have to know it
    /// exists, so in the docs we splice it out: each top-level type under `Builtin`
    /// becomes its own module (its members and nested types come along as that
    /// module's entries). Every `Builtin`-relative type reference in a signature
    /// then has its (empty) module path rewritten to the type's new owning module,
    /// so cross-references resolve to the promoted pages instead of `Builtin`.
    ///
    /// No-op unless a module literally named `Builtin` is present, so other
    /// packages are unaffected.
    pub fn reshapeBuiltin(self: *PackageDocs, gpa: Allocator) Allocator.Error!void {
        const bi = blk: {
            for (self.modules, 0..) |*mod, i| {
                if (std.mem.eql(u8, mod.name, "Builtin")) break :blk i;
            }
            return; // No Builtin module — nothing to reshape.
        };

        const builtin = self.modules[bi];

        // type short-name -> owning top-level type name (e.g. "U8" -> "Num").
        // Keys/values are slices into the (still-live) entry names.
        var type_to_module = std.StringHashMapUnmanaged([]const u8).empty;
        defer type_to_module.deinit(gpa);
        for (builtin.entries) |*entry| {
            try registerBuiltinTypes(gpa, &type_to_module, entry, entry.name);
        }

        // One module per top-level entry; the entry (with its children) moves in.
        var promoted = std.ArrayList(ModuleDocs).empty;
        errdefer promoted.deinit(gpa);
        for (builtin.entries) |entry| {
            const entries = try gpa.alloc(DocEntry, 1);
            entries[0] = entry; // move (shares inner allocations)
            try promoted.append(gpa, .{
                .name = try gpa.dupe(u8, entry.name),
                .package_name = try gpa.dupe(u8, builtin.package_name),
                .kind = .type_module,
                .module_doc = null,
                .entries = entries,
                .source_path = if (builtin.source_path) |p| try gpa.dupe(u8, p) else null,
                .builtin_derived = true,
            });
        }

        // Free the old Builtin shell — its entries were moved out, so free only
        // the backing slice, not the elements.
        gpa.free(builtin.entries);
        if (builtin.module_doc) |doc| gpa.free(doc);
        deinitDocRefs(gpa, builtin.module_doc_refs);
        if (builtin.source_path) |p| gpa.free(p);
        gpa.free(builtin.name);
        gpa.free(builtin.package_name);

        // Rebuild the module list: everything except Builtin, plus the promoted
        // type modules, sorted for deterministic output.
        var rebuilt = std.ArrayList(ModuleDocs).empty;
        errdefer rebuilt.deinit(gpa);
        for (self.modules, 0..) |mod, i| {
            if (i != bi) try rebuilt.append(gpa, mod);
        }
        try rebuilt.appendSlice(gpa, promoted.items);
        promoted.deinit(gpa);
        gpa.free(self.modules);
        self.modules = try rebuilt.toOwnedSlice(gpa);
        std.mem.sort(ModuleDocs, self.modules, {}, moduleDocsLessThan);

        // Now that every type knows its owning module, rewrite references.
        for (self.modules) |*mod| {
            for (mod.entries) |*entry| {
                rewriteBuiltinTypeRefs(gpa, entry, &type_to_module);
            }
        }
    }
};

fn clearDocRefs(package_docs: *PackageDocs, gpa: Allocator) void {
    for (package_docs.modules) |*mod| {
        deinitDocRefs(gpa, mod.module_doc_refs);
        mod.module_doc_refs = &.{};
        for (mod.entries) |*entry| {
            clearEntryDocRefs(entry, gpa);
        }
    }
}

fn clearEntryDocRefs(entry: *DocEntry, gpa: Allocator) void {
    deinitDocRefs(gpa, entry.doc_refs);
    entry.doc_refs = &.{};
    for (entry.children) |*child| {
        clearEntryDocRefs(child, gpa);
    }
}

const PackageDocRefResolver = struct {
    gpa: Allocator,
    package_docs: *const PackageDocs,
    known_modules: std.StringHashMapUnmanaged(*const ModuleDocs),
    builtin_type_owners: std.StringHashMapUnmanaged([]const u8),
    documenting_builtin: bool,

    fn init(gpa: Allocator, package_docs: *const PackageDocs) Allocator.Error!PackageDocRefResolver {
        var known_modules = std.StringHashMapUnmanaged(*const ModuleDocs).empty;
        errdefer known_modules.deinit(gpa);

        var builtin_type_owners = std.StringHashMapUnmanaged([]const u8).empty;
        errdefer builtin_type_owners.deinit(gpa);

        var documenting_builtin = false;
        for (package_docs.modules) |*mod| {
            try known_modules.put(gpa, mod.name, mod);
            if (std.mem.eql(u8, mod.name, "Builtin") or mod.builtin_derived) {
                documenting_builtin = true;
            }
            if (mod.builtin_derived) {
                for (mod.entries) |*entry| {
                    try collectBuiltinDocTypeOwners(&builtin_type_owners, gpa, entry, mod.name);
                }
            }
        }

        return .{
            .gpa = gpa,
            .package_docs = package_docs,
            .known_modules = known_modules,
            .builtin_type_owners = builtin_type_owners,
            .documenting_builtin = documenting_builtin,
        };
    }

    fn deinit(self: *PackageDocRefResolver) void {
        self.known_modules.deinit(self.gpa);
        self.builtin_type_owners.deinit(self.gpa);
    }

    fn resolveModule(
        self: *const PackageDocRefResolver,
        gpa: Allocator,
        mod: *ModuleDocs,
    ) Allocator.Error!void {
        var module_resolver = try ModuleDocRefResolver.init(gpa, self, mod);
        defer module_resolver.deinit();

        if (mod.module_doc) |doc| {
            mod.module_doc_refs = try module_resolver.resolveDocComment(gpa, doc);
        }
        for (mod.entries) |*entry| {
            try module_resolver.resolveEntry(gpa, entry);
        }
    }
};

const ModuleDocRefResolver = struct {
    package: *const PackageDocRefResolver,
    module: *const ModuleDocs,
    local_anchors: std.StringHashMapUnmanaged([]const u8),
    local_arena: std.heap.ArenaAllocator,

    fn init(
        gpa: Allocator,
        package: *const PackageDocRefResolver,
        mod: *const ModuleDocs,
    ) Allocator.Error!ModuleDocRefResolver {
        var resolver = ModuleDocRefResolver{
            .package = package,
            .module = mod,
            .local_anchors = .empty,
            .local_arena = std.heap.ArenaAllocator.init(gpa),
        };
        errdefer resolver.deinit();

        for (mod.entries) |*entry| {
            try resolver.registerEntry(gpa, entry, "");
        }

        return resolver;
    }

    fn deinit(self: *ModuleDocRefResolver) void {
        self.local_anchors.deinit(self.package.gpa);
        self.local_arena.deinit();
    }

    fn resolveEntry(self: *ModuleDocRefResolver, gpa: Allocator, entry: *DocEntry) Allocator.Error!void {
        if (entry.doc_comment) |doc| {
            entry.doc_refs = try self.resolveDocComment(gpa, doc);
        }
        for (entry.children) |*child| {
            try self.resolveEntry(gpa, child);
        }
    }

    fn resolveDocComment(
        self: *const ModuleDocRefResolver,
        gpa: Allocator,
        doc: []const u8,
    ) Allocator.Error![]const DocRef {
        var refs = std.ArrayList(DocRef).empty;
        errdefer {
            for (refs.items) |*ref| {
                ref.deinit(gpa);
            }
            refs.deinit(gpa);
        }

        var i: usize = 0;
        var line_start: usize = 0;
        var in_fence = false;
        while (i < doc.len) {
            if (i == line_start and std.mem.startsWith(u8, doc[i..], "```")) {
                in_fence = !in_fence;
                i = skipLine(doc, i);
                line_start = i;
                continue;
            }

            if (in_fence) {
                i = skipLine(doc, i);
                line_start = i;
                continue;
            }

            if (doc[i] == '\n') {
                i += 1;
                line_start = i;
                continue;
            }

            if (doc[i] == '`') {
                i = skipInlineCode(doc, i);
                continue;
            }

            if (doc[i] == '[') {
                if (parseMarkdownLink(doc, i)) |link| {
                    i = link.end;
                    continue;
                }
                if (parseDocRef(doc, i)) |parsed| {
                    const label = try gpa.dupe(u8, parsed.label);
                    var label_moved = false;
                    errdefer if (!label_moved) gpa.free(label);

                    var target = try self.resolveLabel(gpa, parsed.label);
                    var target_moved = false;
                    errdefer if (!target_moved) target.deinit(gpa);

                    try refs.append(gpa, .{
                        .byte_offset = @intCast(i),
                        .label = label,
                        .target = target,
                    });
                    label_moved = true;
                    target_moved = true;

                    i = parsed.end;
                    continue;
                }
            }

            i += 1;
        }

        if (refs.items.len == 0) {
            refs.deinit(gpa);
            return &.{};
        }

        return refs.toOwnedSlice(gpa);
    }

    fn resolveLabel(
        self: *const ModuleDocRefResolver,
        gpa: Allocator,
        label: []const u8,
    ) Allocator.Error!DocRefTarget {
        if (self.local_anchors.get(label)) |anchor| {
            return .{ .local_anchor = try gpa.dupe(u8, anchor) };
        }

        const first_dot = std.mem.findScalar(u8, label, '.');
        const head = if (first_dot) |d| label[0..d] else label;
        const tail: []const u8 = if (first_dot) |d| label[d..] else "";

        if (self.local_anchors.get(head)) |anchor_head| {
            const anchor = if (tail.len == 0)
                try gpa.dupe(u8, anchor_head)
            else
                try std.fmt.allocPrint(gpa, "{s}{s}", .{ anchor_head, tail });
            return .{ .local_anchor = anchor };
        }

        if (self.package.known_modules.get(head)) |target_module| {
            if (tail.len == 0) {
                return .{ .module_page = try gpa.dupe(u8, head) };
            }

            const anchor = if (target_module.builtin_derived)
                try gpa.dupe(u8, tail[1..])
            else
                try gpa.dupe(u8, label);
            errdefer gpa.free(anchor);

            return .{ .module_anchor = .{
                .module = try gpa.dupe(u8, head),
                .anchor = anchor,
            } };
        }

        if (self.package.documenting_builtin) {
            if (self.package.builtin_type_owners.get(head)) |owner| {
                const anchor = if (tail.len == 0)
                    try gpa.dupe(u8, head)
                else
                    try std.fmt.allocPrint(gpa, "{s}{s}", .{ head, tail });
                errdefer gpa.free(anchor);

                if (std.mem.eql(u8, owner, self.module.name)) {
                    return .{ .local_anchor = anchor };
                }

                return .{ .module_anchor = .{
                    .module = try gpa.dupe(u8, owner),
                    .anchor = anchor,
                } };
            }
        } else if (isBuiltinDocTypeName(head)) {
            return .{ .builtin_type = try gpa.dupe(u8, label) };
        }

        // A handful of builtin tag names have no doc entry of their own, so
        // resolve them via their owning nominal type's page instead.
        if (builtinTagOwner(label)) |owner| {
            return self.resolveLabel(gpa, owner);
        }

        return .{ .unresolved_anchor = try unresolvedAnchor(gpa, self.module.name, label) };
    }

    fn registerEntry(
        self: *ModuleDocRefResolver,
        gpa: Allocator,
        entry: *const DocEntry,
        parent_path: []const u8,
    ) Allocator.Error!void {
        const arena = self.local_arena.allocator();
        const full_path = if (parent_path.len == 0) blk: {
            if (entryNameHasModulePrefix(self.module.name, entry.name)) {
                break :blk try arena.dupe(u8, entry.name);
            }
            break :blk try std.fmt.allocPrint(arena, "{s}.{s}", .{ self.module.name, entry.name });
        } else try std.fmt.allocPrint(arena, "{s}.{s}", .{ parent_path, entry.name });

        const anchor_path = if (self.module.builtin_derived)
            moduleRelativeEntryName(self.module.name, full_path)
        else
            full_path;

        try self.putLocalAnchor(gpa, anchor_path, anchor_path);
        if (!std.mem.eql(u8, full_path, anchor_path)) {
            try self.putLocalAnchor(gpa, full_path, anchor_path);
        }

        var prefix_end: usize = anchor_path.len;
        if (entry.kind == .value) {
            if (std.mem.findScalarLast(u8, anchor_path, '.')) |last_dot| {
                prefix_end = last_dot;
            } else {
                prefix_end = 0;
            }
        }

        var seg_start: usize = 0;
        while (seg_start < prefix_end) {
            const next_dot = std.mem.findScalarPos(u8, anchor_path[0..prefix_end], seg_start, '.');
            const seg_end = next_dot orelse prefix_end;
            const short_name = anchor_path[seg_start..seg_end];
            const prefix_path = anchor_path[0..seg_end];
            try self.putLocalAnchor(gpa, short_name, prefix_path);
            try self.putLocalAnchor(gpa, prefix_path, prefix_path);
            seg_start = if (next_dot) |d| d + 1 else prefix_end;
        }

        for (entry.children) |*child| {
            try self.registerEntry(gpa, child, full_path);
        }
    }

    fn putLocalAnchor(
        self: *ModuleDocRefResolver,
        gpa: Allocator,
        key: []const u8,
        anchor: []const u8,
    ) Allocator.Error!void {
        const result = try self.local_anchors.getOrPut(gpa, key);
        if (!result.found_existing) {
            const arena = self.local_arena.allocator();
            result.key_ptr.* = try arena.dupe(u8, key);
            result.value_ptr.* = try arena.dupe(u8, anchor);
        }
    }
};

fn collectBuiltinDocTypeOwners(
    map: *std.StringHashMapUnmanaged([]const u8),
    gpa: Allocator,
    entry: *const DocEntry,
    module_name: []const u8,
) Allocator.Error!void {
    if (entry.kind != .value) {
        const short = if (std.mem.findScalarLast(u8, entry.name, '.')) |d| entry.name[d + 1 ..] else entry.name;
        const result = try map.getOrPut(gpa, short);
        if (!result.found_existing) {
            result.value_ptr.* = module_name;
        }
    }
    for (entry.children) |*child| {
        try collectBuiltinDocTypeOwners(map, gpa, child, module_name);
    }
}

fn unresolvedAnchor(gpa: Allocator, module_name: []const u8, label: []const u8) Allocator.Error![]const u8 {
    if (std.mem.eql(u8, label, module_name) or
        (std.mem.startsWith(u8, label, module_name) and
            label.len > module_name.len and
            label[module_name.len] == '.'))
    {
        return try gpa.dupe(u8, label);
    }
    return try std.fmt.allocPrint(gpa, "{s}.{s}", .{ module_name, label });
}

fn skipLine(text: []const u8, start: usize) usize {
    var i = start;
    while (i < text.len and text[i] != '\n') {
        i += 1;
    }
    return if (i < text.len) i + 1 else i;
}

fn skipInlineCode(text: []const u8, start: usize) usize {
    var i = start + 1;
    while (i < text.len and text[i] != '`' and text[i] != '\n') {
        i += 1;
    }
    return if (i < text.len and text[i] == '`') i + 1 else start + 1;
}

fn entryNameHasModulePrefix(module_name: []const u8, entry_name: []const u8) bool {
    return std.mem.eql(u8, entry_name, module_name) or
        (std.mem.startsWith(u8, entry_name, module_name) and
            entry_name.len > module_name.len and
            entry_name[module_name.len] == '.');
}

fn moduleRelativeEntryName(module_name: []const u8, entry_name: []const u8) []const u8 {
    if (std.mem.startsWith(u8, entry_name, module_name) and
        entry_name.len > module_name.len and
        entry_name[module_name.len] == '.')
    {
        return entry_name[module_name.len + 1 ..];
    }
    return entry_name;
}

fn isBuiltinDocTypeName(name: []const u8) bool {
    inline for (.{
        "Str",
        "List",
        "Bool",
        "Num",
        "U8",
        "U16",
        "U32",
        "U64",
        "U128",
        "I8",
        "I16",
        "I32",
        "I64",
        "I128",
        "F32",
        "F64",
        "Dec",
        "Box",
        "Dict",
        "Set",
        "Iter",
        "Try",
    }) |builtin| {
        if (std.mem.eql(u8, name, builtin)) return true;
    }
    return false;
}

/// Maps the well-known builtin tag names that lack their own doc entry to the
/// nominal type that owns them, so a bare `[True]`/`[Ok]`/etc. reference can be
/// resolved through that type's docs. Returns null for any other label.
fn builtinTagOwner(label: []const u8) ?[]const u8 {
    if (std.mem.eql(u8, label, "True") or std.mem.eql(u8, label, "False")) {
        return "Bool";
    }
    if (std.mem.eql(u8, label, "Ok") or std.mem.eql(u8, label, "Err")) {
        return "Try";
    }
    return null;
}

/// Short (final dotted segment) of a possibly-qualified name.
fn shortTypeName(name: []const u8) []const u8 {
    return if (std.mem.findScalarLast(u8, name, '.')) |d| name[d + 1 ..] else name;
}

/// Record every type (not value) reachable from `entry` as belonging to
/// `owner`, so a later reference by short name resolves to the owner module.
fn registerBuiltinTypes(
    gpa: Allocator,
    map: *std.StringHashMapUnmanaged([]const u8),
    entry: *const DocEntry,
    owner: []const u8,
) Allocator.Error!void {
    if (entry.kind != .value) {
        try map.put(gpa, shortTypeName(entry.name), owner);
    }
    for (entry.children) |*child| {
        try registerBuiltinTypes(gpa, map, child, owner);
    }
}

/// Rewrite the (empty) module path of every `Builtin`-relative type reference in
/// `entry`'s signature, and its children's, to the type's new owning module.
fn rewriteBuiltinTypeRefs(
    gpa: Allocator,
    entry: *DocEntry,
    map: *const std.StringHashMapUnmanaged([]const u8),
) void {
    if (entry.type_signature) |sig| rewriteDocTypeRefs(gpa, sig, map);
    for (entry.children) |*child| {
        rewriteBuiltinTypeRefs(gpa, child, map);
    }
}

/// Walk a `DocType` tree (iteratively, to tolerate deeply nested types) and
/// repoint each builtin type reference at its owning module.
fn rewriteDocTypeRefs(
    gpa: Allocator,
    root: *const DocType,
    map: *const std.StringHashMapUnmanaged([]const u8),
) void {
    var stack = std.ArrayList(*const DocType).empty;
    defer stack.deinit(gpa);
    // If we can't even push the root, there is nothing safe to do.
    stack.append(gpa, root) catch return;
    while (stack.pop()) |node| {
        switch (node.*) {
            .type_ref => |ref| {
                if (ref.module_path.len == 0) {
                    if (map.get(shortTypeName(ref.type_name))) |owner| {
                        const mutable = @constCast(node);
                        const new_path = gpa.dupe(u8, owner) catch continue;
                        gpa.free(mutable.type_ref.module_path);
                        mutable.type_ref.module_path = new_path;
                    }
                }
            },
            .function => |func| {
                for (func.args) |arg| stack.append(gpa, arg) catch {};
                stack.append(gpa, func.ret) catch {};
            },
            .record => |rec| {
                if (rec.ext) |ext| stack.append(gpa, ext) catch {};
                for (rec.fields) |field| stack.append(gpa, field.type) catch {};
            },
            .tag_union => |tu| {
                if (tu.ext) |ext| stack.append(gpa, ext) catch {};
                for (tu.tags) |tag| {
                    for (tag.args) |arg| stack.append(gpa, arg) catch {};
                }
            },
            .tuple => |tup| {
                for (tup.elems) |elem| stack.append(gpa, elem) catch {};
            },
            .apply => |app| {
                stack.append(gpa, app.constructor) catch {};
                for (app.args) |arg| stack.append(gpa, arg) catch {};
            },
            .where_clause => |wc| {
                stack.append(gpa, wc.type) catch {};
                for (wc.constraints) |c| stack.append(gpa, c.signature) catch {};
            },
            .type_var, .wildcard, .@"error" => {},
        }
    }
}

/// Documentation for a Roc application — the app's own modules,
/// its platform, and all dependency packages (recursively).
pub const AppDocs = struct {
    name: []const u8,
    /// The app's own modules (from the app package).
    modules: []ModuleDocs,
    /// The platform this app uses.
    platform: ?*PackageDocs,
    /// Dependency packages (direct and transitive).
    packages: []PackageDocs,

    pub fn deinit(self: *AppDocs, gpa: Allocator) void {
        for (self.modules) |*mod| {
            mod.deinit(gpa);
        }
        gpa.free(self.modules);
        if (self.platform) |plat| {
            plat.deinit(gpa);
            gpa.destroy(plat);
        }
        for (self.packages) |*pkg| {
            pkg.deinit(gpa);
        }
        gpa.free(self.packages);
        gpa.free(self.name);
    }

    pub fn writeToSExpr(self: *const AppDocs, writer: anytype) Allocator.Error!void {
        try writer.writeAll("(app-docs\n");
        try writer.writeAll("  (name \"");
        try writeEscaped(writer, self.name);
        try writer.writeAll("\")\n");
        for (self.modules) |*mod| {
            try mod.writeToSExpr(writer, 1);
        }
        if (self.platform) |plat| {
            try writer.writeAll("  (platform\n");
            try plat.writeToSExprIndented(writer, 2);
            try writer.writeAll("  )\n");
        }
        for (self.packages) |*pkg| {
            try pkg.writeToSExprIndented(writer, 1);
        }
        try writer.writeAll(")\n");
    }
};

/// Documentation for a Roc platform — the platform's own modules
/// and all dependency packages.
pub const PlatformDocs = struct {
    name: []const u8,
    modules: []ModuleDocs,
    packages: []PackageDocs,

    pub fn deinit(self: *PlatformDocs, gpa: Allocator) void {
        for (self.modules) |*mod| {
            mod.deinit(gpa);
        }
        gpa.free(self.modules);
        for (self.packages) |*pkg| {
            pkg.deinit(gpa);
        }
        gpa.free(self.packages);
        gpa.free(self.name);
    }

    pub fn writeToSExpr(self: *const PlatformDocs, writer: anytype) Allocator.Error!void {
        try writer.writeAll("(platform-docs\n");
        try writer.writeAll("  (name \"");
        try writeEscaped(writer, self.name);
        try writer.writeAll("\")\n");
        for (self.modules) |*mod| {
            try mod.writeToSExpr(writer, 1);
        }
        for (self.packages) |*pkg| {
            try pkg.writeToSExprIndented(writer, 1);
        }
        try writer.writeAll(")\n");
    }
};

/// The kind of module.
pub const ModuleKind = enum {
    app,
    module,
    package,
    platform,
    type_module,

    pub fn toStr(self: ModuleKind) []const u8 {
        return switch (self) {
            .app => "app",
            .module => "module",
            .package => "package",
            .platform => "platform",
            .type_module => "type_module",
        };
    }
};

/// A resolved shorthand reference from doc-comment text, such as `[Str]` or
/// `[Utf8.default]`.
pub const DocRef = struct {
    /// Byte offset of the opening `[` within the owning doc-comment string.
    byte_offset: u32,
    /// Label as written by the user, without brackets.
    label: []const u8,
    target: DocRefTarget,

    pub fn deinit(self: *const DocRef, gpa: Allocator) void {
        gpa.free(self.label);
        self.target.deinit(gpa);
    }
};

/// Destination for a resolved shorthand doc reference.
pub const DocRefTarget = union(enum) {
    /// Exact HTML fragment id in the current page, without the leading `#`.
    local_anchor: []const u8,
    /// Another module's index page.
    module_page: []const u8,
    /// Exact HTML fragment id in another module's page.
    module_anchor: ModuleAnchor,
    /// Builtin type path in the published builtin docs, e.g. `Str` or `U8`.
    builtin_type: []const u8,
    /// Explicitly unresolved target used only for diagnostics.
    unresolved_anchor: []const u8,

    pub const ModuleAnchor = struct {
        module: []const u8,
        anchor: []const u8,
    };

    pub fn deinit(self: *const DocRefTarget, gpa: Allocator) void {
        switch (self.*) {
            .local_anchor, .module_page, .builtin_type, .unresolved_anchor => |s| gpa.free(s),
            .module_anchor => |ma| {
                gpa.free(ma.module);
                gpa.free(ma.anchor);
            },
        }
    }
};

pub fn deinitDocRefs(gpa: Allocator, refs: []const DocRef) void {
    for (refs) |*ref| {
        ref.deinit(gpa);
    }
    if (refs.len > 0) {
        gpa.free(refs);
    }
}

/// Parsed Markdown inline link, `[label](url)`.
pub const MarkdownLink = struct {
    label: []const u8,
    url: []const u8,
    end: usize,
};

/// Parses a `[label](url)` markdown link starting at `start`, which must point
/// to `[`. Returns null if the pattern doesn't match.
pub fn parseMarkdownLink(text: []const u8, start: usize) ?MarkdownLink {
    std.debug.assert(text[start] == '[');
    var j = start + 1;
    while (j < text.len and text[j] != ']') {
        if (text[j] == '\n') return null;
        j += 1;
    }
    if (j >= text.len) return null;
    const label_end = j;
    if (label_end + 1 >= text.len or text[label_end + 1] != '(') return null;
    // Allow balanced parentheses inside the URL (e.g. a trailing ')' in a
    // Wikipedia link like `Union_(set_theory)`): only a `)` at depth 0
    // terminates the destination.
    var k = label_end + 2;
    var depth: usize = 0;
    while (k < text.len) : (k += 1) {
        const c = text[k];
        if (c == '\n') return null;
        if (c == '(') {
            depth += 1;
        } else if (c == ')') {
            if (depth == 0) break;
            depth -= 1;
        }
    }
    if (k >= text.len) return null;
    return .{
        .label = text[start + 1 .. label_end],
        .url = text[label_end + 2 .. k],
        .end = k + 1,
    };
}

/// Parsed shorthand doc reference, `[Name]` or `[Name.member]`.
pub const ParsedDocRef = struct {
    label: []const u8,
    end: usize,
};

/// Parses a shorthand `[Name]` or `[Name.member]` reference to another doc
/// entry. The label must be a (possibly dotted) identifier, and the closing
/// bracket must not be followed by `(`.
pub fn parseDocRef(text: []const u8, start: usize) ?ParsedDocRef {
    std.debug.assert(text[start] == '[');
    const label_start = start + 1;
    if (label_start >= text.len) return null;
    if (!isDocIdentStart(text[label_start])) return null;
    var j = label_start + 1;
    while (j < text.len and text[j] != ']') {
        const c = text[j];
        if (c == '.') {
            if (j + 1 >= text.len or !isDocIdentStart(text[j + 1])) return null;
        } else if (!isDocIdentCont(c)) {
            return null;
        }
        j += 1;
    }
    if (j >= text.len) return null;
    if (j + 1 < text.len and text[j + 1] == '(') return null;
    return .{
        .label = text[label_start..j],
        .end = j + 1,
    };
}

fn isDocIdentStart(c: u8) bool {
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z') or c == '_';
}

fn isDocIdentCont(c: u8) bool {
    return isDocIdentStart(c) or (c >= '0' and c <= '9');
}

/// Orders modules by (package name, module name) so docs output is
/// deterministic regardless of the hash-map order modules are collected in.
pub fn moduleDocsLessThan(_: void, a: ModuleDocs, b: ModuleDocs) bool {
    const package_order = std.mem.order(u8, a.package_name, b.package_name);
    if (package_order != .eq) return package_order == .lt;
    return std.mem.order(u8, a.name, b.name) == .lt;
}

/// Documentation for a single module.
pub const ModuleDocs = struct {
    name: []const u8,
    package_name: []const u8,
    kind: ModuleKind,
    module_doc: ?[]const u8,
    module_doc_refs: []const DocRef = &.{},
    entries: []DocEntry,
    /// Filesystem path to the module's source `.roc` file. Used by the
    /// renderer when reporting source-level diagnostics (e.g. broken
    /// `[ref]` links). Owned by `gpa`.
    source_path: ?[]const u8 = null,
    /// 1-based source line where `module_doc`'s first `##` line begins.
    /// Zero when there is no module doc or the line is unknown.
    module_doc_start_line: u32 = 0,
    /// True for modules synthesized by `PackageDocs.reshapeBuiltin` — each is one
    /// of the builtin types (`Str`, `Num`, …) promoted to a top-level module so
    /// the `Builtin` container never appears in the docs. The renderer uses this
    /// to strip the module-name prefix from anchors, giving bare ids like
    /// `write_u8` instead of `Hasher.write_u8`.
    builtin_derived: bool = false,

    pub fn deinit(self: *ModuleDocs, gpa: Allocator) void {
        for (self.entries) |*entry| {
            entry.deinit(gpa);
        }
        gpa.free(self.entries);
        if (self.module_doc) |doc| gpa.free(doc);
        deinitDocRefs(gpa, self.module_doc_refs);
        if (self.source_path) |p| gpa.free(p);
        gpa.free(self.name);
        gpa.free(self.package_name);
    }

    pub fn writeToSExpr(self: *const ModuleDocs, writer: anytype, depth: usize) (Allocator.Error || error{WriteFailed})!void {
        try writeIndent(writer, depth);
        try writer.writeAll("(module\n");
        try writeIndent(writer, depth + 1);
        try writer.writeAll("(name \"");
        try writeEscaped(writer, self.name);
        try writer.writeAll("\")\n");
        try writeIndent(writer, depth + 1);
        try writer.writeAll("(package \"");
        try writeEscaped(writer, self.package_name);
        try writer.writeAll("\")\n");
        try writeIndent(writer, depth + 1);
        try writer.writeAll("(kind ");
        try writer.writeAll(self.kind.toStr());
        try writer.writeAll(")\n");
        if (self.module_doc) |doc| {
            try writeIndent(writer, depth + 1);
            try writer.writeAll("(doc \"");
            try writeEscaped(writer, doc);
            try writer.writeAll("\")\n");
        }
        for (self.entries) |*entry| {
            try entry.writeToSExpr(writer, depth + 1);
        }
        try writeIndent(writer, depth);
        try writer.writeAll(")\n");
    }
};

/// The kind of a documentation entry.
pub const DocEntryKind = enum {
    value,
    alias,
    nominal,
    @"opaque",

    pub fn toStr(self: DocEntryKind) []const u8 {
        return switch (self) {
            .value => "value",
            .alias => "alias",
            .nominal => "nominal",
            .@"opaque" => "opaque",
        };
    }
};

/// Structured representation of a Roc type for documentation.
///
/// This recursive tagged union preserves the identity and origin of each type
/// reference, enabling HTML documentation with syntax highlighting and links
/// to type definitions. All child pointers and slices are heap-allocated.
pub const DocType = union(enum) {
    /// Named type reference with module origin: Str, List, Counter, etc.
    type_ref: TypeRef,
    /// Type variable: a, b, item, etc.
    type_var: []const u8,
    /// Function type: args -> ret
    function: Function,
    /// Record type: { field: Type, ... }
    record: Record,
    /// Tag union: [Ok(a), Err(e)]
    tag_union: TagUnion,
    /// Tuple: (a, b)
    tuple: Tuple,
    /// Type application: List(Str), Result(ok, err)
    apply: Apply,
    /// Where clause wrapping a type
    where_clause: WhereClause,
    /// Wildcard _
    wildcard,
    /// Error/unknown type
    @"error",

    pub const Layout = enum {
        compact,
        multiline,
    };

    pub const TypeRef = struct {
        /// Module path where this type is defined, as provided by the compiler.
        /// Currently basenames like "Builtin", "Counter", "Num".
        /// Will include dot-separated paths as the compiler evolves
        /// (e.g., "Json.Decode", "pkg.Data.Person").
        module_path: []const u8,
        /// Type name within the module (e.g., "Str", "Counter", "U64").
        /// May contain dots for nested types (e.g., "Num.U8").
        type_name: []const u8,
    };

    pub const Function = struct {
        args: []const *const DocType,
        ret: *const DocType,
        effectful: bool,
    };

    pub const Record = struct {
        fields: []const Field,
        /// Named extension variable (e.g. `..a` produces `type_var("a")`).
        /// Null when the record is closed or when it is anonymously open (`..`).
        ext: ?*const DocType,
        /// True when the record is open (`..` or `..name`).
        is_open: bool,
        layout: Layout = .compact,
    };

    pub const Field = struct {
        name: []const u8,
        type: *const DocType,
    };

    pub const TagUnion = struct {
        tags: []const Tag,
        /// Named extension variable (e.g. `..a` produces `type_var("a")`).
        /// Null when the union is closed or when it is anonymously open (`..`).
        ext: ?*const DocType,
        /// True when the union is open (`..` or `..name`).
        is_open: bool,
        layout: Layout = .compact,
    };

    pub const Tag = struct {
        name: []const u8,
        args: []const *const DocType,
        layout: Layout = .compact,
    };

    pub const Tuple = struct {
        elems: []const *const DocType,
        layout: Layout = .compact,
    };

    pub const Apply = struct {
        constructor: *const DocType, // the type being applied (e.g., List)
        args: []const *const DocType,
        layout: Layout = .compact,
    };

    pub const WhereClause = struct {
        type: *const DocType,
        constraints: []const Constraint,
        layout: Layout = .compact,
    };

    pub const Constraint = struct {
        type_var: []const u8,
        method_name: []const u8,
        signature: *const DocType, // the method's type signature
    };

    pub fn writeToSExpr(self: *const DocType, writer: anytype, depth: usize) (Allocator.Error || error{WriteFailed})!void {
        switch (self.*) {
            .type_ref => |ref| {
                try writer.writeAll("(type-ref");
                // Only include module path if it's not empty
                if (ref.module_path.len > 0) {
                    try writer.writeAll(" (module \"");
                    try writeEscaped(writer, ref.module_path);
                    try writer.writeAll("\")");
                }
                try writer.writeAll(" (name \"");
                try writeEscaped(writer, ref.type_name);
                try writer.writeAll("\"))");
            },
            .type_var => |name| {
                try writer.writeAll("(var \"");
                try writeEscaped(writer, name);
                try writer.writeAll("\")");
            },
            .function => |func| {
                if (func.effectful) {
                    try writer.writeAll("(fn!");
                } else {
                    try writer.writeAll("(fn");
                }
                for (func.args) |arg| {
                    try writer.writeAll(" ");
                    try arg.writeToSExpr(writer, depth);
                }
                try writer.writeAll(" ");
                try func.ret.writeToSExpr(writer, depth);
                try writer.writeAll(")");
            },
            .record => |rec| {
                try writer.writeAll("(record");
                if (rec.is_open) try writer.writeAll(" (open)");
                if (rec.ext) |ext| {
                    try writer.writeAll(" (ext ");
                    try ext.writeToSExpr(writer, depth);
                    try writer.writeAll(")");
                }
                for (rec.fields) |field| {
                    try writer.writeAll(" (field \"");
                    try writeEscaped(writer, field.name);
                    try writer.writeAll("\" ");
                    try field.type.writeToSExpr(writer, depth);
                    try writer.writeAll(")");
                }
                try writer.writeAll(")");
            },
            .tag_union => |tu| {
                try writer.writeAll("(tag-union");
                if (tu.is_open) try writer.writeAll(" (open)");
                for (tu.tags) |tag| {
                    try writer.writeAll(" (tag \"");
                    try writeEscaped(writer, tag.name);
                    try writer.writeAll("\"");
                    for (tag.args) |arg| {
                        try writer.writeAll(" ");
                        try arg.writeToSExpr(writer, depth);
                    }
                    try writer.writeAll(")");
                }
                if (tu.ext) |ext| {
                    try writer.writeAll(" (ext ");
                    try ext.writeToSExpr(writer, depth);
                    try writer.writeAll(")");
                }
                try writer.writeAll(")");
            },
            .tuple => |tup| {
                try writer.writeAll("(tuple");
                for (tup.elems) |elem| {
                    try writer.writeAll(" ");
                    try elem.writeToSExpr(writer, depth);
                }
                try writer.writeAll(")");
            },
            .apply => |app| {
                try writer.writeAll("(apply ");
                try app.constructor.writeToSExpr(writer, depth);
                for (app.args) |arg| {
                    try writer.writeAll(" ");
                    try arg.writeToSExpr(writer, depth);
                }
                try writer.writeAll(")");
            },
            .where_clause => |wc| {
                try writer.writeAll("(where ");
                try wc.type.writeToSExpr(writer, depth);
                for (wc.constraints) |constraint| {
                    try writer.writeAll(" (constraint \"");
                    try writeEscaped(writer, constraint.type_var);
                    try writer.writeAll("\" \"");
                    try writeEscaped(writer, constraint.method_name);
                    try writer.writeAll("\" ");
                    try constraint.signature.writeToSExpr(writer, depth);
                    try writer.writeAll(")");
                }
                try writer.writeAll(")");
            },
            .wildcard => {
                try writer.writeAll("(wildcard)");
            },
            .@"error" => {
                try writer.writeAll("(error)");
            },
        }
    }

    pub fn deinit(self: *const DocType, gpa: Allocator) void {
        const Frame = struct {
            node: *const DocType,
            children_done: bool,
        };

        const Stack = struct {
            fn append(stack: *std.ArrayList(Frame), allocator: Allocator, frame: Frame) void {
                stack.append(allocator, frame) catch @panic("out of memory while deinitializing DocType");
            }
        };

        var stack = std.ArrayList(Frame).empty;
        defer stack.deinit(gpa);

        Stack.append(&stack, gpa, .{ .node = self, .children_done = false });
        while (stack.pop()) |frame| {
            const node = frame.node;
            if (!frame.children_done) {
                Stack.append(&stack, gpa, .{ .node = node, .children_done = true });
                switch (node.*) {
                    .function => |func| {
                        Stack.append(&stack, gpa, .{ .node = func.ret, .children_done = false });
                        for (func.args) |arg| {
                            Stack.append(&stack, gpa, .{ .node = arg, .children_done = false });
                        }
                    },
                    .record => |rec| {
                        if (rec.ext) |ext| {
                            Stack.append(&stack, gpa, .{ .node = ext, .children_done = false });
                        }
                        for (rec.fields) |field| {
                            Stack.append(&stack, gpa, .{ .node = field.type, .children_done = false });
                        }
                    },
                    .tag_union => |tu| {
                        if (tu.ext) |ext| {
                            Stack.append(&stack, gpa, .{ .node = ext, .children_done = false });
                        }
                        for (tu.tags) |tag| {
                            for (tag.args) |arg| {
                                Stack.append(&stack, gpa, .{ .node = arg, .children_done = false });
                            }
                        }
                    },
                    .tuple => |tup| {
                        for (tup.elems) |elem| {
                            Stack.append(&stack, gpa, .{ .node = elem, .children_done = false });
                        }
                    },
                    .apply => |app| {
                        Stack.append(&stack, gpa, .{ .node = app.constructor, .children_done = false });
                        for (app.args) |arg| {
                            Stack.append(&stack, gpa, .{ .node = arg, .children_done = false });
                        }
                    },
                    .where_clause => |wc| {
                        Stack.append(&stack, gpa, .{ .node = wc.type, .children_done = false });
                        for (wc.constraints) |constraint| {
                            Stack.append(&stack, gpa, .{ .node = constraint.signature, .children_done = false });
                        }
                    },
                    .type_ref, .type_var, .wildcard, .@"error" => {},
                }
                continue;
            }

            switch (node.*) {
                .type_ref => |ref| {
                    gpa.free(ref.module_path);
                    gpa.free(ref.type_name);
                },
                .type_var => |name| {
                    gpa.free(name);
                },
                .function => |func| {
                    for (func.args) |arg| {
                        gpa.destroy(arg);
                    }
                    gpa.free(func.args);
                    gpa.destroy(func.ret);
                },
                .record => |rec| {
                    if (rec.ext) |ext| {
                        gpa.destroy(ext);
                    }
                    for (rec.fields) |field| {
                        gpa.free(field.name);
                        gpa.destroy(field.type);
                    }
                    gpa.free(rec.fields);
                },
                .tag_union => |tu| {
                    for (tu.tags) |tag| {
                        gpa.free(tag.name);
                        for (tag.args) |arg| {
                            gpa.destroy(arg);
                        }
                        gpa.free(tag.args);
                    }
                    gpa.free(tu.tags);
                    if (tu.ext) |ext| {
                        gpa.destroy(ext);
                    }
                },
                .tuple => |tup| {
                    for (tup.elems) |elem| {
                        gpa.destroy(elem);
                    }
                    gpa.free(tup.elems);
                },
                .apply => |app| {
                    gpa.destroy(app.constructor);
                    for (app.args) |arg| {
                        gpa.destroy(arg);
                    }
                    gpa.free(app.args);
                },
                .where_clause => |wc| {
                    gpa.destroy(wc.type);
                    for (wc.constraints) |constraint| {
                        gpa.free(constraint.type_var);
                        gpa.free(constraint.method_name);
                        gpa.destroy(constraint.signature);
                    }
                    gpa.free(wc.constraints);
                },
                .wildcard, .@"error" => {},
            }
        }
    }
};

/// Documentation for a single exported definition.
pub const DocEntry = struct {
    name: []const u8,
    /// The declaration header, including any type parameters (e.g. `List(a)`).
    /// This is distinct from `name` so opaque declarations can expose their
    /// public shape without exposing their backing type.
    type_header: ?[]const u8 = null,
    kind: DocEntryKind,
    type_signature: ?*const DocType,
    doc_comment: ?[]const u8,
    doc_refs: []const DocRef = &.{},
    children: []DocEntry,
    /// 1-based source line where `doc_comment`'s first `##` line begins.
    /// Zero when there is no doc comment or the line is unknown.
    doc_comment_start_line: u32 = 0,

    pub fn deinit(self: *DocEntry, gpa: Allocator) void {
        for (self.children) |*child| {
            child.deinit(gpa);
        }
        gpa.free(self.children);
        if (self.type_header) |header| gpa.free(header);
        if (self.type_signature) |sig| {
            sig.deinit(gpa);
            gpa.destroy(sig);
        }
        if (self.doc_comment) |doc| gpa.free(doc);
        deinitDocRefs(gpa, self.doc_refs);
        gpa.free(self.name);
    }

    pub fn writeToSExpr(self: *const DocEntry, writer: anytype, depth: usize) (Allocator.Error || error{WriteFailed})!void {
        try writeIndent(writer, depth);
        try writer.writeAll("(entry\n");
        try writeIndent(writer, depth + 1);
        try writer.writeAll("(name \"");
        try writeEscaped(writer, self.name);
        try writer.writeAll("\")\n");
        try writeIndent(writer, depth + 1);
        try writer.writeAll("(kind ");
        try writer.writeAll(self.kind.toStr());
        try writer.writeAll(")\n");
        if (self.type_signature) |sig| {
            try writeIndent(writer, depth + 1);
            // For declaration kinds, emit prefix with name + operator
            switch (self.kind) {
                .nominal => {
                    try writer.writeAll("(type \"");
                    try writeEscaped(writer, self.name);
                    try writer.writeAll(" := \" ");
                    try sig.writeToSExpr(writer, depth + 1);
                    try writer.writeAll(")\n");
                },
                .@"opaque" => {
                    try writer.writeAll("(type \"");
                    try writeEscaped(writer, self.name);
                    try writer.writeAll(" :: \" ");
                    try sig.writeToSExpr(writer, depth + 1);
                    try writer.writeAll(")\n");
                },
                .alias => {
                    try writer.writeAll("(type \"");
                    try writeEscaped(writer, self.name);
                    try writer.writeAll(" : \" ");
                    try sig.writeToSExpr(writer, depth + 1);
                    try writer.writeAll(")\n");
                },
                .value => {
                    try writer.writeAll("(type ");
                    try sig.writeToSExpr(writer, depth + 1);
                    try writer.writeAll(")\n");
                },
            }
        }
        if (self.doc_comment) |doc| {
            try writeIndent(writer, depth + 1);
            try writer.writeAll("(doc \"");
            try writeEscaped(writer, doc);
            try writer.writeAll("\")\n");
        }
        for (self.children) |*child| {
            try child.writeToSExpr(writer, depth + 1);
        }
        try writeIndent(writer, depth);
        try writer.writeAll(")\n");
    }
};

// --- Helpers ---

fn writeIndent(writer: anytype, depth: usize) error{WriteFailed}!void {
    for (0..depth) |_| {
        try writer.writeAll("  ");
    }
}

fn writeEscaped(writer: anytype, s: []const u8) (Allocator.Error || error{WriteFailed})!void {
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => try writer.writeAll(&[_]u8{c}),
        }
    }
}

test "doc shorthand tag refs resolve to owning builtin types in package docs" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // A third-party package with no `Bool`/`Try` modules of its own: `[True]`
    // and `[Ok]` should resolve through their owning nominal type to the
    // published builtin docs (`Bool` / `Try`) rather than staying unresolved.
    const entry = DocEntry{
        .name = try gpa.dupe(u8, "any_thing"),
        .kind = .value,
        .type_signature = null,
        .doc_comment = try gpa.dupe(u8, "Returns [True] on success and [Ok] otherwise."),
        .children = try gpa.alloc(DocEntry, 0),
        .doc_comment_start_line = 1,
    };

    const entries = try gpa.alloc(DocEntry, 1);
    entries[0] = entry;

    var module = ModuleDocs{
        .name = try gpa.dupe(u8, "Thing"),
        .package_name = try gpa.dupe(u8, "roc-thing"),
        .kind = .type_module,
        .module_doc = null,
        .entries = entries,
        .source_path = try gpa.dupe(u8, "/fake/roc-thing/package/Thing.roc"),
    };
    defer module.deinit(gpa);

    const modules = try gpa.alloc(ModuleDocs, 1);
    modules[0] = module;
    defer gpa.free(modules);

    var package_docs = PackageDocs{
        .name = try gpa.dupe(u8, "roc-thing"),
        .modules = modules,
    };
    defer gpa.free(package_docs.name);

    try package_docs.resolveDocRefs(gpa);

    const refs = package_docs.modules[0].entries[0].doc_refs;
    try testing.expectEqual(@as(usize, 2), refs.len);

    try testing.expectEqualStrings("True", refs[0].label);
    try testing.expect(refs[0].target == .builtin_type);
    try testing.expectEqualStrings("Bool", refs[0].target.builtin_type);

    try testing.expectEqualStrings("Ok", refs[1].label);
    try testing.expect(refs[1].target == .builtin_type);
    try testing.expectEqualStrings("Try", refs[1].target.builtin_type);
}

test "doc shorthand tag refs resolve to owning type module pages when present" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // When the package itself publishes the owning type as a module (as the
    // builtin docs do after `Bool`/`Try` are promoted), `[True]`/`[Ok]` should
    // resolve to those module pages.
    const referring_entry = DocEntry{
        .name = try gpa.dupe(u8, "helper"),
        .kind = .value,
        .type_signature = null,
        .doc_comment = try gpa.dupe(u8, "Uses [True] and [Ok]."),
        .children = try gpa.alloc(DocEntry, 0),
        .doc_comment_start_line = 1,
    };

    const thing_entries = try gpa.alloc(DocEntry, 1);
    thing_entries[0] = referring_entry;

    var thing_module = ModuleDocs{
        .name = try gpa.dupe(u8, "Thing"),
        .package_name = try gpa.dupe(u8, "roc-thing"),
        .kind = .type_module,
        .module_doc = null,
        .entries = thing_entries,
        .source_path = try gpa.dupe(u8, "/fake/roc-thing/package/Thing.roc"),
    };

    var bool_module = ModuleDocs{
        .name = try gpa.dupe(u8, "Bool"),
        .package_name = try gpa.dupe(u8, "roc-thing"),
        .kind = .type_module,
        .module_doc = null,
        .entries = try gpa.alloc(DocEntry, 0),
        .source_path = try gpa.dupe(u8, "/fake/roc-thing/package/Bool.roc"),
    };

    var try_module = ModuleDocs{
        .name = try gpa.dupe(u8, "Try"),
        .package_name = try gpa.dupe(u8, "roc-thing"),
        .kind = .type_module,
        .module_doc = null,
        .entries = try gpa.alloc(DocEntry, 0),
        .source_path = try gpa.dupe(u8, "/fake/roc-thing/package/Try.roc"),
    };

    const modules = try gpa.alloc(ModuleDocs, 3);
    modules[0] = thing_module;
    modules[1] = bool_module;
    modules[2] = try_module;
    defer {
        thing_module.deinit(gpa);
        bool_module.deinit(gpa);
        try_module.deinit(gpa);
        gpa.free(modules);
    }

    var package_docs = PackageDocs{
        .name = try gpa.dupe(u8, "roc-thing"),
        .modules = modules,
    };
    defer gpa.free(package_docs.name);

    try package_docs.resolveDocRefs(gpa);

    const refs = package_docs.modules[0].entries[0].doc_refs;
    try testing.expectEqual(@as(usize, 2), refs.len);

    try testing.expect(refs[0].target == .module_page);
    try testing.expectEqualStrings("Bool", refs[0].target.module_page);

    try testing.expect(refs[1].target == .module_page);
    try testing.expectEqualStrings("Try", refs[1].target.module_page);
}

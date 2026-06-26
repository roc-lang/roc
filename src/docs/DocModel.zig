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
    };

    pub const Tag = struct {
        name: []const u8,
        args: []const *const DocType,
    };

    pub const Tuple = struct {
        elems: []const *const DocType,
    };

    pub const Apply = struct {
        constructor: *const DocType, // the type being applied (e.g., List)
        args: []const *const DocType,
    };

    pub const WhereClause = struct {
        type: *const DocType,
        constraints: []const Constraint,
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
    kind: DocEntryKind,
    type_signature: ?*const DocType,
    doc_comment: ?[]const u8,
    children: []DocEntry,
    /// 1-based source line where `doc_comment`'s first `##` line begins.
    /// Zero when there is no doc comment or the line is unknown.
    doc_comment_start_line: u32 = 0,

    pub fn deinit(self: *DocEntry, gpa: Allocator) void {
        for (self.children) |*child| {
            child.deinit(gpa);
        }
        gpa.free(self.children);
        if (self.type_signature) |sig| {
            sig.deinit(gpa);
            gpa.destroy(sig);
        }
        if (self.doc_comment) |doc| gpa.free(doc);
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

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

    pub fn writeToSExpr(self: *const PackageDocs, writer: anytype) !void {
        try self.writeToSExprIndented(writer, 0);
    }

    pub fn writeToSExprIndented(self: *const PackageDocs, writer: anytype, depth: usize) !void {
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
};

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

    pub fn writeToSExpr(self: *const AppDocs, writer: anytype) !void {
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

    pub fn writeToSExpr(self: *const PlatformDocs, writer: anytype) !void {
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
    package,
    platform,
    type_module,

    pub fn toStr(self: ModuleKind) []const u8 {
        return switch (self) {
            .app => "app",
            .package => "package",
            .platform => "platform",
            .type_module => "type_module",
        };
    }
};

/// Documentation for a single module.
pub const ModuleDocs = struct {
    name: []const u8,
    package_name: []const u8,
    kind: ModuleKind,
    module_doc: ?[]const u8,
    entries: []DocEntry,

    pub fn deinit(self: *ModuleDocs, gpa: Allocator) void {
        for (self.entries) |*entry| {
            entry.deinit(gpa);
        }
        gpa.free(self.entries);
        if (self.module_doc) |doc| gpa.free(doc);
        gpa.free(self.name);
        gpa.free(self.package_name);
    }

    pub fn writeToSExpr(self: *const ModuleDocs, writer: anytype, depth: usize) !void {
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
        ext: ?*const DocType, // extension var for open records { .., name: Str }
    };

    pub const Field = struct {
        name: []const u8,
        type: *const DocType,
    };

    pub const TagUnion = struct {
        tags: []const Tag,
        ext: ?*const DocType,
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

    pub fn writeToSExpr(self: *const DocType, writer: anytype, depth: usize) !void {
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
        switch (self.*) {
            .type_ref => |ref| {
                gpa.free(ref.module_path);
                gpa.free(ref.type_name);
            },
            .type_var => |name| {
                gpa.free(name);
            },
            .function => |func| {
                for (func.args) |arg| {
                    arg.deinit(gpa);
                    gpa.destroy(arg);
                }
                gpa.free(func.args);
                func.ret.deinit(gpa);
                gpa.destroy(func.ret);
            },
            .record => |rec| {
                if (rec.ext) |ext| {
                    ext.deinit(gpa);
                    gpa.destroy(ext);
                }
                for (rec.fields) |field| {
                    gpa.free(field.name);
                    field.type.deinit(gpa);
                    gpa.destroy(field.type);
                }
                gpa.free(rec.fields);
            },
            .tag_union => |tu| {
                for (tu.tags) |tag| {
                    gpa.free(tag.name);
                    for (tag.args) |arg| {
                        arg.deinit(gpa);
                        gpa.destroy(arg);
                    }
                    gpa.free(tag.args);
                }
                gpa.free(tu.tags);
                if (tu.ext) |ext| {
                    ext.deinit(gpa);
                    gpa.destroy(ext);
                }
            },
            .tuple => |tup| {
                for (tup.elems) |elem| {
                    elem.deinit(gpa);
                    gpa.destroy(elem);
                }
                gpa.free(tup.elems);
            },
            .apply => |app| {
                app.constructor.deinit(gpa);
                gpa.destroy(app.constructor);
                for (app.args) |arg| {
                    arg.deinit(gpa);
                    gpa.destroy(arg);
                }
                gpa.free(app.args);
            },
            .where_clause => |wc| {
                wc.type.deinit(gpa);
                gpa.destroy(wc.type);
                for (wc.constraints) |constraint| {
                    gpa.free(constraint.type_var);
                    gpa.free(constraint.method_name);
                    constraint.signature.deinit(gpa);
                    gpa.destroy(constraint.signature);
                }
                gpa.free(wc.constraints);
            },
            .wildcard, .@"error" => {},
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

    pub fn writeToSExpr(self: *const DocEntry, writer: anytype, depth: usize) !void {
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

fn writeIndent(writer: anytype, depth: usize) !void {
    for (0..depth) |_| {
        try writer.writeAll("  ");
    }
}

fn writeEscaped(writer: anytype, s: []const u8) !void {
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

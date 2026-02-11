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
        try writer.writeAll("(package-docs\n");
        try writer.writeAll("  (name \"");
        try writeEscaped(writer, self.name);
        try writer.writeAll("\")\n");
        for (self.modules) |*mod| {
            try mod.writeToSExpr(writer, 1);
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
    }

    pub fn writeToSExpr(self: *const ModuleDocs, writer: anytype, depth: usize) !void {
        try writeIndent(writer, depth);
        try writer.writeAll("(module\n");
        try writeIndent(writer, depth + 1);
        try writer.writeAll("(name \"");
        try writeEscaped(writer, self.name);
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

/// Documentation for a single exported definition.
pub const DocEntry = struct {
    name: []const u8,
    kind: DocEntryKind,
    type_signature: ?[]const u8,
    doc_comment: ?[]const u8,
    children: []DocEntry,

    pub fn deinit(self: *DocEntry, gpa: Allocator) void {
        for (self.children) |*child| {
            child.deinit(gpa);
        }
        gpa.free(self.children);
        if (self.type_signature) |sig| gpa.free(sig);
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
            try writer.writeAll("(type \"");
            try writeEscaped(writer, sig);
            try writer.writeAll("\")\n");
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

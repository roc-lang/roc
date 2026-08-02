//! Shared utilities for module discovery during compilation.
//!
//! These functions are used by both the IPC path (default `roc` command) and the BuildEnv path (`roc check`/`roc build`)
//! to ensure consistent behavior when discovering and loading sibling modules.

const std = @import("std");
const parse = @import("parse");

const Allocator = std.mem.Allocator;
const AST = parse.AST;

/// Extract unqualified sibling module imports from parser-recorded import inventory.
/// Returns module names that:
/// 1. Have no qualifier (not like "pf.Stdout")
/// 2. Are uppercase identifiers (module names start with uppercase)
/// 3. Are not compiler-owned package-header Builtin auto-imports
///
/// In addition to explicit `import` statements, the upper-cased entries in a
/// `package [Mod1, Mod2, ...] {}` header and module targets in a platform
/// `hosted` section are treated as auto-imports.
///
/// This is used to identify which sibling modules need to be compiled
/// before canonicalizing the current module.
///
/// Parameters:
///   parse_ast: The parsed AST whose declaration index contains import facts
///   gpa: Allocator for the returned strings
///
/// Returns: Slice of imported module names (caller owns memory)
pub fn extractImportsFromDeclIndex(
    parse_ast: *const AST,
    gpa: Allocator,
) Allocator.Error![][]const u8 {
    var result = std.ArrayList([]const u8).empty;
    errdefer {
        for (result.items) |item| gpa.free(item);
        result.deinit(gpa);
    }

    // Modules listed in a `package [...]` header are auto-imported.
    for (parse_ast.decl_index.package_header_modules.items) |package_module| {
        if (parse_ast.decl_index.hasExplicitUnqualifiedImport(package_module.module_name)) continue;
        try appendModuleName(gpa, &result, parse_ast.env.getIdent(package_module.module_name), false);
    }

    for (parse_ast.decl_index.imports.items) |import| {
        if (import.qualifier != null) continue;
        try appendModuleName(gpa, &result, stripLeadingDot(parse_ast.env.getIdent(import.module_name)), true);
    }

    const file = parse_ast.store.getFile();
    switch (parse_ast.store.getHeader(file.header)) {
        .platform => |platform| {
            for (parse_ast.store.symbolMapEntrySlice(platform.hosted)) |entry_idx| {
                const entry = parse_ast.store.getSymbolMapEntry(entry_idx);
                const module_token = entry.module orelse continue;
                const module_ident = parse_ast.tokens.resolveIdentifier(module_token) orelse continue;
                try appendModuleName(gpa, &result, parse_ast.env.getIdent(module_ident), false);
            }
        },
        else => {},
    }

    return result.toOwnedSlice(gpa);
}

fn appendModuleName(
    gpa: Allocator,
    result: *std.ArrayList([]const u8),
    module_name: []const u8,
    allow_builtin_source_import: bool,
) Allocator.Error!void {
    if (!allow_builtin_source_import and std.mem.eql(u8, module_name, "Builtin")) return;

    // Check if it looks like a module name (starts with uppercase)
    if (module_name.len == 0) return;
    if (module_name[0] < 'A' or module_name[0] > 'Z') return;

    // Check for duplicates using linear scan (typically few imports)
    for (result.items) |existing| {
        if (std.mem.eql(u8, existing, module_name)) return;
    }
    try result.append(gpa, try gpa.dupe(u8, module_name));
}

fn stripLeadingDot(text: []const u8) []const u8 {
    return if (text.len > 0 and text[0] == '.') text[1..] else text;
}

/// Extract qualified/external imports from parser-recorded import inventory.
/// These are imports like "import pf.Stdout" where qualifier_tok is set.
///
/// Returns: Slice of qualified import names (e.g., "pf.Stdout") (caller owns memory)
pub fn extractQualifiedImportsFromDeclIndex(
    parse_ast: *const AST,
    gpa: Allocator,
) Allocator.Error![][]const u8 {
    var result = std.ArrayList([]const u8).empty;
    errdefer {
        for (result.items) |item| gpa.free(item);
        result.deinit(gpa);
    }

    for (parse_ast.decl_index.imports.items) |import| {
        const qualifier_ident = import.qualifier orelse continue;
        const qualifier = parse_ast.env.getIdent(qualifier_ident);
        const module_name = stripLeadingDot(parse_ast.env.getIdent(import.module_name));

        // Build qualified name like "pf.Stdout"
        const qualified_name = try std.fmt.allocPrint(gpa, "{s}.{s}", .{ qualifier, module_name });
        errdefer gpa.free(qualified_name);

        // Check for duplicates
        var found = false;
        for (result.items) |existing| {
            if (std.mem.eql(u8, existing, qualified_name)) {
                found = true;
                gpa.free(qualified_name);
                break;
            }
        }
        if (!found) {
            try result.append(gpa, qualified_name);
        }
    }

    return result.toOwnedSlice(gpa);
}

test "module discovery consumes parser import inventory" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\package [Auto, Builtin] {}
        \\import Foo
        \\import Foo
        \\import Builtin
        \\import pf.Stdout
        \\import Src.Widget as Widget
        \\import Data.Codec exposing [decode]
        \\import Nested.Type
        \\import pf.IO.Stream as Stream
        \\import Layout .Path as LayoutPath
        \\import lower
        \\
        \\main = {}
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();

    const local_imports = try extractImportsFromDeclIndex(ast, gpa);
    defer {
        for (local_imports) |item| gpa.free(item);
        gpa.free(local_imports);
    }
    try std.testing.expectEqual(@as(usize, 7), local_imports.len);
    try std.testing.expectEqualStrings("Auto", local_imports[0]);
    try std.testing.expectEqualStrings("Foo", local_imports[1]);
    try std.testing.expectEqualStrings("Builtin", local_imports[2]);
    try std.testing.expectEqualStrings("Src.Widget", local_imports[3]);
    try std.testing.expectEqualStrings("Data.Codec", local_imports[4]);
    try std.testing.expectEqualStrings("Nested", local_imports[5]);
    try std.testing.expectEqualStrings("Layout.Path", local_imports[6]);

    const qualified_imports = try extractQualifiedImportsFromDeclIndex(ast, gpa);
    defer {
        for (qualified_imports) |item| gpa.free(item);
        gpa.free(qualified_imports);
    }
    try std.testing.expectEqual(@as(usize, 2), qualified_imports.len);
    try std.testing.expectEqualStrings("pf.Stdout", qualified_imports[0]);
    try std.testing.expectEqualStrings("pf.IO.Stream", qualified_imports[1]);
}

test "explicit directory import supplies a package-header module alias" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\package [Widget, Auto] {}
        \\import Src.Widget as Widget
        \\
        \\main = {}
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();

    const local_imports = try extractImportsFromDeclIndex(ast, gpa);
    defer {
        for (local_imports) |item| gpa.free(item);
        gpa.free(local_imports);
    }
    try std.testing.expectEqual(@as(usize, 2), local_imports.len);
    try std.testing.expectEqualStrings("Auto", local_imports[0]);
    try std.testing.expectEqualStrings("Src.Widget", local_imports[1]);
}

test "platform hosted targets are module dependencies" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\platform ""
        \\    requires {}
        \\    exposes [Effect]
        \\    packages {}
        \\    provides {}
        \\    hosted {
        \\        "roc_bar_get": Bar.Idx.get!,
        \\        "roc_effect_run": Effect.run!,
        \\    }
        \\    targets: {}
        \\
        \\import Effect
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();

    const local_imports = try extractImportsFromDeclIndex(ast, gpa);
    defer {
        for (local_imports) |item| gpa.free(item);
        gpa.free(local_imports);
    }

    try std.testing.expectEqual(@as(usize, 2), local_imports.len);
    try std.testing.expectEqualStrings("Effect", local_imports[0]);
    try std.testing.expectEqualStrings("Bar", local_imports[1]);
}

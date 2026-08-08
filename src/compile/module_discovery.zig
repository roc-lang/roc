//! Shared utilities for module discovery during compilation.
//!
//! These functions are used by both the IPC path (default `roc` command) and the BuildEnv path (`roc check`/`roc build`)
//! to ensure consistent behavior when discovering and loading sibling modules.

const std = @import("std");
const parse = @import("parse");

const Allocator = std.mem.Allocator;
const AST = parse.AST;

/// A parser-produced local source target awaiting importer-relative
/// normalization.
pub const LocalImport = struct {
    import_name: []const u8,
    base: AST.DeclIndex.Import.LocalBase,
    parent_count: u16,
};

/// A package's public module name and package-root-relative source target.
pub const PublicModule = struct {
    name: []const u8,
    target: []const u8,
    /// Type selected from `target`, or null when the public entry exposes the
    /// source module's main type.
    nested_type: ?[]const u8,
};

/// Errors produced while deriving public source targets from a package header.
pub const PublicModuleError = Allocator.Error || error{ImportEscapesPackageRoot};

/// Resolve a package header's public names to package-root-relative logical
/// module paths. An explicit local module import whose binding matches a public
/// name supplies its internal target; otherwise the public name is the target.
pub fn extractPublicModules(
    ast: *const AST,
    exposes: AST.Collection.Idx,
    gpa: Allocator,
) PublicModuleError![]PublicModule {
    var result = std.ArrayList(PublicModule).empty;
    errdefer {
        for (result.items) |module| {
            gpa.free(module.name);
            gpa.free(module.target);
            if (module.nested_type) |nested_type| gpa.free(nested_type);
        }
        result.deinit(gpa);
    }

    const collection = ast.store.getCollection(exposes);
    for (ast.store.exposedItemSlice(.{ .span = collection.span })) |item_idx| {
        const item = ast.store.getExposedItem(item_idx);
        const token_idx, const qualifiers, const alias_tok = switch (item) {
            .upper_ident => |upper| .{ upper.ident, upper.qualifiers, upper.as },
            .upper_ident_star => |upper| .{ upper.ident, upper.qualifiers, null },
            .lower_ident, .malformed => continue,
        };

        const qualifier_tokens = ast.store.tokenSlice(qualifiers);
        const source_name_tok: AST.Token.Idx = if (qualifier_tokens.len > 0)
            @intCast(qualifier_tokens[0])
        else
            token_idx;
        const source_name = identifierTokenText(ast, source_name_tok);
        const exposed_name = if (alias_tok) |alias|
            identifierTokenText(ast, alias)
        else
            identifierTokenText(ast, token_idx);

        var target_text = source_name;
        for (ast.decl_index.imports.items) |import| {
            if (import.origin != .local) continue;
            if (import.nested_type_path != null) continue;
            const binding = import.module_binding orelse continue;
            if (!std.mem.eql(u8, ast.env.getIdent(binding), source_name)) continue;
            target_text = ast.env.getIdent(import.module_name);
            target_text = switch (import.base) {
                .importer => if (std.mem.startsWith(u8, target_text, "./")) target_text[2..] else target_text,
                .package_root => target_text[1..],
                .parent => return error.ImportEscapesPackageRoot,
            };
            break;
        }

        const name = try gpa.dupe(u8, exposed_name);
        errdefer gpa.free(name);
        const target = try gpa.dupe(u8, target_text);
        errdefer gpa.free(target);
        const nested_type = if (qualifier_tokens.len > 0)
            try qualifiedNestedTypeText(ast, qualifier_tokens[1..], token_idx, gpa)
        else
            null;
        errdefer if (nested_type) |nested| gpa.free(nested);
        try result.append(gpa, .{ .name = name, .target = target, .nested_type = nested_type });
    }
    return result.toOwnedSlice(gpa);
}

fn identifierTokenText(ast: *const AST, token_idx: AST.Token.Idx) []const u8 {
    const raw = ast.resolve(token_idx);
    return if (raw.len > 0 and raw[0] == '.') raw[1..] else raw;
}

fn qualifiedNestedTypeText(
    ast: *const AST,
    intermediate_tokens: []const u32,
    final_token: AST.Token.Idx,
    gpa: Allocator,
) Allocator.Error![]const u8 {
    var result = std.ArrayList(u8).empty;
    errdefer result.deinit(gpa);

    for (intermediate_tokens) |raw_token| {
        try result.appendSlice(gpa, identifierTokenText(ast, @intCast(raw_token)));
        try result.append(gpa, '.');
    }
    try result.appendSlice(gpa, identifierTokenText(ast, final_token));

    return result.toOwnedSlice(gpa);
}

/// Free a public-module mapping returned by `extractPublicModules`.
pub fn freePublicModules(gpa: Allocator, modules: []PublicModule) void {
    for (modules) |module| {
        gpa.free(module.name);
        gpa.free(module.target);
        if (module.nested_type) |nested_type| gpa.free(nested_type);
    }
    gpa.free(modules);
}

/// Normalize one local source target into its package-root-relative logical
/// module path. Returning null means parent traversal crossed the package root.
pub fn resolveLocalImportLogicalPath(
    gpa: Allocator,
    importer_name: []const u8,
    parsed: LocalImport,
) Allocator.Error!?[]const u8 {
    var segments = std.ArrayList([]const u8).empty;
    defer segments.deinit(gpa);

    if (parsed.base != .package_root) {
        var importer_segments = std.mem.splitScalar(u8, importer_name, '/');
        while (importer_segments.next()) |segment| try segments.append(gpa, segment);
        if (segments.items.len > 0) _ = segments.pop();
    }

    if (parsed.base == .parent) {
        if (parsed.parent_count > segments.items.len) return null;
        segments.shrinkRetainingCapacity(segments.items.len - parsed.parent_count);
    }

    var module_path = parsed.import_name;
    switch (parsed.base) {
        .importer => {
            if (std.mem.startsWith(u8, module_path, "./")) module_path = module_path[2..];
        },
        .package_root => module_path = module_path[1..],
        .parent => {
            for (0..parsed.parent_count) |_| module_path = module_path[3..];
        },
    }
    var path_segments = std.mem.splitScalar(u8, module_path, '/');
    while (path_segments.next()) |segment| {
        if (segment.len == 0 or std.mem.eql(u8, segment, ".") or std.mem.eql(u8, segment, "..")) return null;
        try segments.append(gpa, segment);
    }
    if (segments.items.len == 0) return null;
    return try std.mem.join(gpa, "/", segments.items);
}

/// Extract local module imports from parser-recorded import inventory.
/// Each result retains the parsed base so the coordinator can normalize it
/// against the importing module's logical path without inspecting source text.
/// Uppercase entries in a package header are also importer-relative
/// auto-imports unless an explicit import supplies that public binding.
/// Platform hosted targets are importer-relative auto-imports as well.
///
/// This is used to identify which sibling modules need to be compiled
/// before canonicalizing the current module.
///
/// Parameters:
///   parse_ast: The parsed AST whose declaration index contains import facts
///   gpa: Allocator for the returned strings
///
/// Returns: Slice of parsed local import targets (caller owns memory)
pub fn extractImportsFromDeclIndex(
    parse_ast: *const AST,
    gpa: Allocator,
) Allocator.Error![]LocalImport {
    var result = std.ArrayList(LocalImport).empty;
    errdefer {
        for (result.items) |item| gpa.free(item.import_name);
        result.deinit(gpa);
    }

    // Modules listed in a `package [...]` header are auto-imported.
    for (parse_ast.decl_index.package_header_modules.items) |package_module| {
        if (parse_ast.decl_index.hasExplicitUnqualifiedImport(package_module.module_name)) continue;
        try appendModuleName(gpa, &result, parse_ast.env.getIdent(package_module.module_name), .importer, 0, false);
    }

    for (parse_ast.decl_index.imports.items) |import| {
        if (import.origin != .local) continue;
        try appendModuleName(gpa, &result, parse_ast.env.getIdent(import.module_name), import.base, import.parent_count, true);
    }

    const file = parse_ast.store.getFile();
    switch (parse_ast.store.getHeader(file.header)) {
        .platform => |platform| {
            for (parse_ast.store.symbolMapEntrySlice(platform.hosted)) |entry_idx| {
                const entry = parse_ast.store.getSymbolMapEntry(entry_idx);
                const module_token = entry.module orelse continue;
                const module_ident = parse_ast.tokens.resolveIdentifier(module_token) orelse continue;
                try appendModuleName(gpa, &result, parse_ast.env.getIdent(module_ident), .importer, 0, false);
            }
        },
        .app, .module, .package, .hosted, .type_module, .default_app, .malformed => {},
    }

    return result.toOwnedSlice(gpa);
}

fn appendModuleName(
    gpa: Allocator,
    result: *std.ArrayList(LocalImport),
    module_name: []const u8,
    base: AST.DeclIndex.Import.LocalBase,
    parent_count: u16,
    allow_builtin_source_import: bool,
) Allocator.Error!void {
    if (!allow_builtin_source_import and std.mem.eql(u8, module_name, "Builtin")) return;

    if (module_name.len == 0) return;

    // Check for duplicates using linear scan (typically few imports)
    for (result.items) |existing| {
        if (std.mem.eql(u8, existing.import_name, module_name)) return;
    }
    try result.append(gpa, .{
        .import_name = try gpa.dupe(u8, module_name),
        .base = base,
        .parent_count = parent_count,
    });
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
        if (import.origin != .package) continue;
        const qualified_name = try gpa.dupe(u8, parse_ast.env.getIdent(import.module_name));
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
        \\import Src/Widget as Widget
        \\import Data/Codec exposing [decode]
        \\import Nested.Type
        \\import pf.IO.Stream as Stream
        \\import Layout/Path as LayoutPath
        \\import lower
        \\
        \\main = {}
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();

    const local_imports = try extractImportsFromDeclIndex(ast, gpa);
    defer {
        for (local_imports) |item| gpa.free(item.import_name);
        gpa.free(local_imports);
    }
    try std.testing.expectEqual(@as(usize, 7), local_imports.len);
    try std.testing.expectEqualStrings("Auto", local_imports[0].import_name);
    try std.testing.expectEqualStrings("Foo", local_imports[1].import_name);
    try std.testing.expectEqualStrings("Builtin", local_imports[2].import_name);
    try std.testing.expectEqualStrings("Src/Widget", local_imports[3].import_name);
    try std.testing.expectEqualStrings("Data/Codec", local_imports[4].import_name);
    try std.testing.expectEqualStrings("Nested", local_imports[5].import_name);
    try std.testing.expectEqualStrings("Layout/Path", local_imports[6].import_name);

    const qualified_imports = try extractQualifiedImportsFromDeclIndex(ast, gpa);
    defer {
        for (qualified_imports) |item| gpa.free(item);
        gpa.free(qualified_imports);
    }
    try std.testing.expectEqual(@as(usize, 2), qualified_imports.len);
    try std.testing.expectEqualStrings("pf.Stdout", qualified_imports[0]);
    try std.testing.expectEqualStrings("pf.IO", qualified_imports[1]);
}

test "explicit directory import supplies a package-header module alias" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\package [Widget, Auto] {}
        \\import Src/Widget as Widget
        \\
        \\main = {}
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();

    const local_imports = try extractImportsFromDeclIndex(ast, gpa);
    defer {
        for (local_imports) |item| gpa.free(item.import_name);
        gpa.free(local_imports);
    }
    try std.testing.expectEqual(@as(usize, 2), local_imports.len);
    try std.testing.expectEqualStrings("Auto", local_imports[0].import_name);
    try std.testing.expectEqualStrings("Src/Widget", local_imports[1].import_name);
}

test "package public modules map explicit aliases to internal logical paths" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\package [Parser, Direct] {}
        \\import /Internal/Parsing/Parser as Parser
        \\
        \\main = {}
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();
    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const header = ast.store.getHeader(ast.store.getFile().header);
    try std.testing.expect(header == .package);
    const exposes = header.package.exposes;
    const modules = try extractPublicModules(ast, exposes, gpa);
    defer freePublicModules(gpa, modules);

    try std.testing.expectEqual(@as(usize, 2), modules.len);
    try std.testing.expectEqualStrings("Parser", modules[0].name);
    try std.testing.expectEqualStrings("Internal/Parsing/Parser", modules[0].target);
    try std.testing.expectEqualStrings("Direct", modules[1].name);
    try std.testing.expectEqualStrings("Direct", modules[1].target);
}

test "qualified public type keeps its source module and nested selection" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\package [Container.Blub] {}
        \\import Container
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();
    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const header = ast.store.getHeader(ast.store.getFile().header);
    try std.testing.expect(header == .package);
    const modules = try extractPublicModules(ast, header.package.exposes, gpa);
    defer freePublicModules(gpa, modules);

    try std.testing.expectEqual(@as(usize, 1), modules.len);
    try std.testing.expectEqualStrings("Blub", modules[0].name);
    try std.testing.expectEqualStrings("Container", modules[0].target);
    try std.testing.expectEqualStrings("Blub", modules[0].nested_type.?);
}

test "package public module aliases cannot traverse above package root" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\package [Parser] {}
        \\import ../Parser as Parser
        \\
        \\main = {}
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();
    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const header = ast.store.getHeader(ast.store.getFile().header);
    try std.testing.expect(header == .package);
    const exposes = header.package.exposes;
    try std.testing.expectError(error.ImportEscapesPackageRoot, extractPublicModules(ast, exposes, gpa));
}

test "explicit import targets separate module paths from nested types" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\import Hello.ParseErr as PE
        \\import Dir/Hello.SubType as ST
        \\import ./Dir/Hello.SubType exposing [decode]
        \\import ../Shared/Hello
        \\import /Shared/Hello.ParseErr
        \\import json.Parser.ParseErr
        \\import json/Parser
        \\
        \\main = {}
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();

    const imports = ast.decl_index.imports.items;
    try std.testing.expectEqual(@as(usize, 7), imports.len);
    try std.testing.expectEqualStrings("Hello", ast.env.getIdent(imports[0].module_name));
    try std.testing.expectEqualStrings("ParseErr", ast.env.getIdent(imports[0].nested_type_path.?));
    try std.testing.expectEqualStrings("Dir/Hello", ast.env.getIdent(imports[1].module_name));
    try std.testing.expectEqualStrings("SubType", ast.env.getIdent(imports[1].nested_type_path.?));
    try std.testing.expectEqualStrings("./Dir/Hello", ast.env.getIdent(imports[2].module_name));
    try std.testing.expectEqualStrings("../Shared/Hello", ast.env.getIdent(imports[3].module_name));
    try std.testing.expectEqualStrings("/Shared/Hello", ast.env.getIdent(imports[4].module_name));
    try std.testing.expectEqualStrings("ParseErr", ast.env.getIdent(imports[4].nested_type_path.?));
    try std.testing.expectEqualStrings("json.Parser", ast.env.getIdent(imports[5].module_name));
    try std.testing.expectEqualStrings("ParseErr", ast.env.getIdent(imports[5].nested_type_path.?));
    try std.testing.expectEqualStrings("json/Parser", ast.env.getIdent(imports[6].module_name));
}

test "binding clauses never change the selected source module" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\import Dir/Hello
        \\import Dir/Hello as Greeting
        \\import Dir/Hello exposing [hello]
        \\import Dir/Hello.SubType
        \\import Dir/Hello.SubType as ST
        \\import Dir/Hello.SubType exposing [decode]
        \\
        \\main = {}
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();
    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 6), ast.decl_index.imports.items.len);

    for (ast.decl_index.imports.items[0..3]) |import| {
        try std.testing.expectEqualStrings("Dir/Hello", ast.env.getIdent(import.module_name));
        try std.testing.expect(import.nested_type_path == null);
    }
    for (ast.decl_index.imports.items[3..]) |import| {
        try std.testing.expectEqualStrings("Dir/Hello", ast.env.getIdent(import.module_name));
        try std.testing.expectEqualStrings("SubType", ast.env.getIdent(import.nested_type_path.?));
    }
}

test "path identifiers and package boundaries are parsed explicitly" {
    const gpa = std.testing.allocator;
    var env = try @import("base").CommonEnv.init(gpa,
        \\import lowerDir/mixedCase/Hello.Outer.Inner
        \\import ../../../Shared/Hello
        \\import json.Parser.Outer.Inner
        \\import json/Parser
        \\
        \\main = {}
    );
    defer env.deinit(gpa);

    const ast = try parse.file(gpa, &env);
    defer ast.deinit();
    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
    const imports = ast.decl_index.imports.items;
    try std.testing.expectEqual(@as(usize, 4), imports.len);

    try std.testing.expectEqual(.local, imports[0].origin);
    try std.testing.expectEqualStrings("lowerDir/mixedCase/Hello", ast.env.getIdent(imports[0].module_name));
    try std.testing.expectEqualStrings("Outer.Inner", ast.env.getIdent(imports[0].nested_type_path.?));
    try std.testing.expectEqual(.parent, imports[1].base);
    try std.testing.expectEqual(@as(u16, 3), imports[1].parent_count);
    try std.testing.expectEqual(.package, imports[2].origin);
    try std.testing.expectEqualStrings("json.Parser", ast.env.getIdent(imports[2].module_name));
    try std.testing.expectEqualStrings("Outer.Inner", ast.env.getIdent(imports[2].nested_type_path.?));
    try std.testing.expectEqual(.local, imports[3].origin);
    try std.testing.expectEqualStrings("json/Parser", ast.env.getIdent(imports[3].module_name));
}

test "invalid module path spellings are rejected by grammar" {
    const gpa = std.testing.allocator;
    const invalid_targets = [_][]const u8{
        "json.Parser/Internal",
        "Dir/../Hello",
        "Dir//Hello",
        "Dir/",
        "Dir/hello",
        "Dir/Hello.roc",
        "Dir\\Hello",
        "C:/Hello",
        "json.parser",
        "./",
        "../",
    };

    for (invalid_targets) |target| {
        const source = try std.fmt.allocPrint(gpa, "import {s}\n\nmain = {{}}", .{target});
        defer gpa.free(source);
        var env = try @import("base").CommonEnv.init(gpa, source);
        defer env.deinit(gpa);
        const ast = try parse.file(gpa, &env);
        defer ast.deinit();
        try std.testing.expect(ast.tokenize_diagnostics.items.len != 0 or ast.parse_diagnostics.items.len != 0);
    }
}

test "local import normalization is package-root relative and rejects escape" {
    const gpa = std.testing.allocator;
    const importer = "Pages/Admin/View";
    const cases = [_]struct {
        parsed: LocalImport,
        expected: ?[]const u8,
    }{
        .{ .parsed = .{ .import_name = "Hello", .base = .importer, .parent_count = 0 }, .expected = "Pages/Admin/Hello" },
        .{ .parsed = .{ .import_name = "./Hello", .base = .importer, .parent_count = 0 }, .expected = "Pages/Admin/Hello" },
        .{ .parsed = .{ .import_name = "../../Tools/Foo", .base = .parent, .parent_count = 2 }, .expected = "Tools/Foo" },
        .{ .parsed = .{ .import_name = "/Tools/Foo", .base = .package_root, .parent_count = 0 }, .expected = "Tools/Foo" },
        .{ .parsed = .{ .import_name = "/lowerDir/Tools/Foo", .base = .package_root, .parent_count = 0 }, .expected = "lowerDir/Tools/Foo" },
        .{ .parsed = .{ .import_name = "../../../Escape", .base = .parent, .parent_count = 3 }, .expected = null },
        .{ .parsed = .{ .import_name = "../../../../Escape", .base = .parent, .parent_count = 4 }, .expected = null },
    };
    for (cases) |case| {
        const actual = try resolveLocalImportLogicalPath(gpa, importer, case.parsed);
        defer if (actual) |path| gpa.free(path);
        if (case.expected) |expected| {
            try std.testing.expectEqualStrings(expected, actual.?);
        } else {
            try std.testing.expect(actual == null);
        }
    }
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
        for (local_imports) |item| gpa.free(item.import_name);
        gpa.free(local_imports);
    }

    try std.testing.expectEqual(@as(usize, 2), local_imports.len);
    try std.testing.expectEqualStrings("Effect", local_imports[0].import_name);
    try std.testing.expectEqualStrings("Bar", local_imports[1].import_name);
    try std.testing.expectEqual(.importer, local_imports[1].base);
    try std.testing.expectEqual(@as(u16, 0), local_imports[1].parent_count);
}

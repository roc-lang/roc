//! Single-module compilation interface.
//!
//! This module provides composable functions for compiling individual modules
//! through each compilation phase (parse, canonicalize, type-check).
//!
//! This is the foundation for all Roc compilation tools:
//! - Coordinator (multi-threaded CLI builds)
//! - Snapshot tool (compiler testing)
//! - REPL (interactive evaluation)
//! - Playground (web-based compilation)

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");

pub const Allocators = base.Allocators;
pub const AST = parse.AST;
pub const ModuleEnv = can.ModuleEnv;

/// Parsing modes for different compilation contexts.
pub const ParseMode = enum {
    /// Full module file (Coordinator, Snapshot, Playground).
    file,
    /// Single expression (REPL, Snapshot expr tests).
    expr,
    /// Single statement (REPL, Snapshot statement tests).
    statement,
    /// Module header only (Snapshot header tests).
    header,
};

/// Compilation options.
///
/// Note: parsing always continues even after errors to provide
/// maximum diagnostic information.
pub const CompileOptions = struct {
    /// Module name for CIR initialization.
    module_name: []const u8 = "Main",
    /// Whether to initialize CIR fields (module_name, imports store, etc).
    /// Set to false if the caller will call initCIRFields separately.
    /// Default: true
    init_cir_fields: bool = true,
};

/// Parse source code into an AST.
///
/// This function:
/// 1. Calculates line starts for source location tracking (if not already done)
/// 2. Initializes CIR fields with module name (unless init_cir_fields=false)
/// 3. Parses the source based on the specified mode
///
/// Always continues even after parse errors. Check `ast.hasErrors()` and
/// use `ast.parse_diagnostics`/`ast.tokenize_diagnostics` to handle errors.
///
/// Memory ownership:
/// - allocators: Caller provides and manages
/// - module_env: Caller provides and manages
/// - Returned *AST: Heap-allocated; caller must call `ast.deinit()` when done
///
/// Example:
/// ```zig
/// var allocators: Allocators = undefined;
/// allocators.initInPlace(gpa);
/// defer allocators.deinit();
///
/// var module_env = try ModuleEnv.init(allocators.gpa, source);
/// defer module_env.deinit();
///
/// const ast = try parseSingleModule(&allocators, &module_env, .file, .{});
/// defer ast.deinit();
///
/// if (ast.hasErrors()) {
///     // Handle diagnostics via ast.parse_diagnostics, ast.tokenize_diagnostics
/// }
/// ```
pub fn parseSingleModule(
    allocators: *Allocators,
    module_env: *ModuleEnv,
    mode: ParseMode,
    options: CompileOptions,
) !*AST {
    const gpa = allocators.gpa;

    // Calculate line starts for source location tracking (idempotent if already done)
    try module_env.common.calcLineStarts(gpa);

    // Initialize CIR fields with module name (unless caller will do it)
    if (options.init_cir_fields) {
        try module_env.initCIRFields(options.module_name);
    }

    // Parse based on mode - parse functions now return *AST directly
    const ast = switch (mode) {
        .file => try parse.parse(allocators, &module_env.common),
        .expr => try parse.parseExpr(allocators, &module_env.common),
        .statement => try parse.parseStatement(allocators, &module_env.common),
        .header => try parse.parseHeader(allocators, &module_env.common),
    };
    errdefer ast.deinit();

    // Clear scratch space after parsing
    ast.store.emptyScratch();

    return ast;
}

// Tests
test "parseSingleModule - simple expression" {
    const allocator = std.testing.allocator;

    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    defer allocators.deinit();

    var module_env = try ModuleEnv.init(allocator, "1 + 2");
    defer module_env.deinit();

    const ast = try parseSingleModule(&allocators, &module_env, .expr, .{});
    defer ast.deinit();

    // Verify we got valid result
    try std.testing.expect(ast.root_node_idx != std.math.maxInt(u32));
    try std.testing.expect(module_env.common.source.len > 0);
}

test "parseSingleModule - simple file" {
    const allocator = std.testing.allocator;
    const source =
        \\module [main]
        \\
        \\main = "Hello"
    ;

    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    defer allocators.deinit();

    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    const ast = try parseSingleModule(&allocators, &module_env, .file, .{ .module_name = "Test" });
    defer ast.deinit();

    // Verify we got a valid result
    try std.testing.expect(module_env.common.source.len > 0);
}

test "parseSingleModule - collects diagnostics" {
    const allocator = std.testing.allocator;

    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    defer allocators.deinit();

    var module_env = try ModuleEnv.init(allocator, "x = ");
    defer module_env.deinit();

    const ast = try parseSingleModule(&allocators, &module_env, .statement, .{});
    defer ast.deinit();

    // Parsing incomplete input - should have diagnostics in the AST
    try std.testing.expect(ast.parse_diagnostics.items.len > 0);
}

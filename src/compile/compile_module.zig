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
const Allocator = std.mem.Allocator;
const parse = @import("parse");
const can = @import("can");
const CoreCtx = @import("ctx").CoreCtx;

pub const AST = parse.AST;
pub const ModuleEnv = can.ModuleEnv;
pub const AutoImportedType = can.AutoImportedType;

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
/// - gpa: Caller provides the general-purpose allocator
/// - module_env: Caller provides and manages
/// - Returned *AST: Heap-allocated; caller must call `ast.deinit()` when done
///
/// Example:
/// ```zig
/// var module_env = try ModuleEnv.init(gpa, source);
/// defer module_env.deinit();
///
/// const ast = try parseSingleModule(gpa, &module_env, .file, .{});
/// defer ast.deinit();
///
/// if (ast.hasErrors()) {
///     // Handle diagnostics via ast.parse_diagnostics, ast.tokenize_diagnostics
/// }
/// ```
pub fn parseSingleModule(
    gpa: Allocator,
    module_env: *ModuleEnv,
    mode: ParseMode,
    options: CompileOptions,
) !*AST {
    // Calculate line starts for source location tracking (idempotent if already done)
    try module_env.common.calcLineStarts(gpa);

    // Initialize CIR fields with module name (unless caller will do it)
    if (options.init_cir_fields) {
        try module_env.initCIRFields(options.module_name);
    }

    // Parse based on mode - parse functions now return *AST directly
    const ast = switch (mode) {
        .file => try parse.parse(gpa, &module_env.common),
        .expr => try parse.parseExpr(gpa, &module_env.common),
        .statement => try parse.parseStatement(gpa, &module_env.common),
        .header => try parse.parseHeader(gpa, &module_env.common),
    };
    errdefer ast.deinit();

    // Clear scratch space after parsing
    ast.store.emptyScratch();

    return ast;
}

/// Canonicalize a parsed module.
///
/// This function canonicalizes the AST into Canonical IR (CIR), performing:
/// 1. Scope resolution
/// 2. Desugaring
/// 3. Semantic analysis
/// 4. Validation for type checking
///
/// Results are stored in module_env (all_defs, all_statements, diagnostics, etc).
///
/// Memory ownership:
/// - roc_ctx: Caller provides the Roc compiler context (allocators + I/O)
/// - module_env: Caller provides; results stored here
/// - parse_ast: Caller provides and manages
/// - context: Builtin type context plus optional explicit imported module environments
///
/// Example:
/// ```zig
/// var module_env = try ModuleEnv.init(gpa, source);
/// defer module_env.deinit();
///
/// const ast = try parseSingleModule(gpa, &module_env, .file, .{});
/// defer ast.deinit();
///
/// try canonicalizeSingleModule(roc_ctx, &module_env, ast, context);
///
/// // Results are now in module_env
/// ```
pub fn canonicalizeSingleModule(
    roc_ctx: CoreCtx,
    module_env: *ModuleEnv,
    parse_ast: *AST,
    context: can.Can.ModuleInitContext,
) !void {
    try can.canonicalizeModule(roc_ctx, module_env, parse_ast, context);
}

// Tests
test "parseSingleModule - simple expression" {
    const allocator = std.testing.allocator;

    var module_env = try ModuleEnv.init(allocator, "1 + 2");
    defer module_env.deinit();

    const ast = try parseSingleModule(allocator, &module_env, .expr, .{});
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

    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    const ast = try parseSingleModule(allocator, &module_env, .file, .{ .module_name = "Test" });
    defer ast.deinit();

    // Verify we got a valid result
    try std.testing.expect(module_env.common.source.len > 0);
}

test "parseSingleModule - collects diagnostics" {
    const allocator = std.testing.allocator;

    var module_env = try ModuleEnv.init(allocator, "x = ");
    defer module_env.deinit();

    const ast = try parseSingleModule(allocator, &module_env, .statement, .{});
    defer ast.deinit();

    // Parsing incomplete input - should have diagnostics in the AST
    try std.testing.expect(ast.parse_diagnostics.items.len > 0);
}

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
const check = @import("check");

pub const Allocators = base.Allocators;
pub const AST = parse.AST;
pub const CIR = can.CIR;
pub const Can = can.Can;
pub const ModuleEnv = can.ModuleEnv;
pub const AutoImportedType = can.AutoImportedType;
pub const Check = check.Check;
pub const BuiltinContext = Check.BuiltinContext;

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

/// Canonicalization modes for different compilation contexts.
pub const CanonicalizeMode = union(enum) {
    /// Full module file.
    file,
    /// Single expression (input: AST expression index from parse_ast.root_node_idx).
    expr: AST.Expr.Idx,
    /// Single statement (input: AST statement index from parse_ast.root_node_idx).
    statement: AST.Statement.Idx,
};

/// Result of canonicalization, varies by mode.
pub const CanonicalizeResult = union(enum) {
    /// File mode - results stored in module_env.
    file,
    /// Expression mode - canonical expression (null if canonicalization failed).
    expr: ?Can.CanonicalizedExpr,
    /// Statement mode - canonical statement (null if canonicalization failed).
    statement: ?CIR.Statement.Idx,
};

/// Type checking modes for different compilation contexts.
pub const TypeCheckMode = union(enum) {
    /// Full module file.
    file,
    /// Single expression (input: CIR expression index from canonicalization).
    expr: CIR.Expr.Idx,
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

/// Canonicalize a parsed module.
///
/// This function canonicalizes the AST into Canonical IR (CIR), performing:
/// 1. Scope resolution
/// 2. Desugaring
/// 3. Semantic analysis
///
/// Note: This does NOT call validateForChecking(). For full module validation
/// (checking main function, type module requirements, etc.), either:
/// - Use can.canonicalizeModule() which includes validation
/// - Call validateForChecking() separately after canonicalization
///
/// The mode determines what to canonicalize:
/// - `.file`: Full module file (results in module_env)
/// - `.expr`: Single expression (returns canonical expression index)
/// - `.statement`: Single statement (returns canonical statement index)
///
/// Results are stored in module_env (all_defs, all_statements, diagnostics, etc).
///
/// Memory ownership:
/// - allocators: Caller provides and manages
/// - module_env: Caller provides; results stored here
/// - parse_ast: Caller provides and manages
/// - module_envs: Optional map of imported module environments
///
/// Example (file mode):
/// ```zig
/// const result = try canonicalizeSingleModule(&allocators, &module_env, ast, null, .file);
/// // Results are now in module_env
/// ```
///
/// Example (expr mode):
/// ```zig
/// const ast_expr_idx: AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
/// const result = try canonicalizeSingleModule(&allocators, &module_env, ast, null, .{ .expr = ast_expr_idx });
/// if (result.expr) |can_expr| {
///     // Use can_expr.idx for type checking
/// }
/// ```
pub fn canonicalizeSingleModule(
    allocators: *Allocators,
    module_env: *ModuleEnv,
    parse_ast: *AST,
    module_envs: ?*const std.AutoHashMap(base.Ident.Idx, AutoImportedType),
    mode: CanonicalizeMode,
) !CanonicalizeResult {
    var czer = try Can.init(allocators, module_env, parse_ast, module_envs);
    defer czer.deinit();

    switch (mode) {
        .file => {
            try czer.canonicalizeFile();
            // Note: callers who need validation should call czer.validateForChecking()
            // separately or use canonicalizeModule() from can/mod.zig which includes it.
            return .file;
        },
        .expr => |ast_expr_idx| {
            const result = try czer.canonicalizeExpr(ast_expr_idx);
            return .{ .expr = result };
        },
        .statement => |ast_stmt_idx| {
            const ast_stmt = parse_ast.store.getStatement(ast_stmt_idx);
            const stmt_result = try czer.canonicalizeBlockStatement(ast_stmt, &.{}, 0);
            if (stmt_result.canonicalized_stmt) |can_stmt| {
                // Track scratch statements for statement mode
                const scratch_statements_start = module_env.store.scratch.?.statements.top();
                try module_env.store.addScratchStatement(can_stmt.idx);
                module_env.all_statements = try module_env.store.statementSpanFrom(scratch_statements_start);
                return .{ .statement = can_stmt.idx };
            }
            return .{ .statement = null };
        },
    }
}

/// Type check a canonicalized module.
///
/// This function performs Hindley-Milner type inference on the CIR:
/// 1. Constraint generation
/// 2. Unification
/// 3. Type error detection
///
/// The mode determines what to type check:
/// - `.file`: Full module file (calls checkFile)
/// - `.expr`: Single expression (calls checkExprRepl)
///
/// Returns a heap-allocated `*Check` that provides access to solver state,
/// import mappings, and type checking results. Results are also stored in
/// module_env.types.
///
/// Memory ownership:
/// - allocators: Caller provides and manages
/// - module_env: Caller provides; type information stored here
/// - imported_modules: Slice of imported module environments
/// - auto_imported_types: Optional map of auto-imported type environments
/// - builtin_ctx: Builtin type context for Bool, Try, Str resolution
/// - Returned *Check: Heap-allocated; caller must call `checker.deinit()` and
///   `allocators.gpa.destroy(checker)` when done
///
/// Example (file mode):
/// ```zig
/// const checker = try typeCheckSingleModule(&allocators, &module_env, &.{builtin_env}, &module_envs, builtin_ctx, .file);
/// defer {
///     checker.deinit();
///     allocators.gpa.destroy(checker);
/// }
/// ```
///
/// Example (expr mode):
/// ```zig
/// const checker = try typeCheckSingleModule(&allocators, &module_env, &.{builtin_env}, &module_envs, builtin_ctx, .{ .expr = can_expr_idx });
/// defer {
///     checker.deinit();
///     allocators.gpa.destroy(checker);
/// }
/// ```
pub fn typeCheckSingleModule(
    allocators: *Allocators,
    module_env: *ModuleEnv,
    imported_modules: []const *const ModuleEnv,
    auto_imported_types: ?*const std.AutoHashMap(base.Ident.Idx, AutoImportedType),
    builtin_ctx: BuiltinContext,
    mode: TypeCheckMode,
) !*Check {
    const gpa = allocators.gpa;

    const checker = try gpa.create(Check);
    errdefer gpa.destroy(checker);

    checker.* = try Check.init(
        allocators,
        &module_env.types,
        module_env,
        imported_modules,
        auto_imported_types,
        &module_env.store.regions,
        builtin_ctx,
    );
    errdefer checker.deinit();

    checker.fixupTypeWriter();

    switch (mode) {
        .file => try checker.checkFile(),
        .expr => |expr_idx| _ = try checker.checkExprRepl(expr_idx),
    }

    return checker;
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

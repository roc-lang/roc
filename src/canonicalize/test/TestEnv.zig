//! Test environment for canonicalization testing, providing utilities to parse, canonicalize, and inspect Roc expressions.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");

const CommonEnv = base.CommonEnv;

gpa: std.mem.Allocator,
module_env: *ModuleEnv,
parse_ast: *parse.AST,
can: *Can,

/// Test environment for canonicalization testing, providing a convenient wrapper around ModuleEnv, AST, and Can.
pub const TestEnv = @This();

pub fn init(source: []const u8) !TestEnv {
    const gpa = std.testing.allocator;

    // Allocate our ModuleEnv, AST, and Can on the heap
    // so we can keep them around for testing purposes...
    // this is an unusual setup, but helps us with testing
    const module_env: *ModuleEnv = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(module_env);

    const parse_ast = try gpa.create(parse.AST);
    errdefer gpa.destroy(parse_ast);

    const can = try gpa.create(Can);
    errdefer gpa.destroy(can);

    // Initialize the ModuleEnv with the CommonEnv
    module_env.* = try ModuleEnv.init(gpa, source);
    errdefer module_env.deinit();

    parse_ast.* = try parse.parseExpr(&module_env.common, gpa);
    errdefer parse_ast.deinit(gpa);

    // Phase 4: AST Structure Validation
    if (parse_ast.root_node_idx >= 0) {
        const root_expr = parse_ast.store.getExpr(@enumFromInt(parse_ast.root_node_idx));
        if (root_expr == .tag) {}
    }

    parse_ast.store.emptyScratch();

    try module_env.initCIRFields(gpa, "test");

    can.* = try Can.init(module_env, parse_ast, null);

    return TestEnv{
        .gpa = gpa,
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
    };
}

pub fn deinit(self: *TestEnv) void {
    self.can.deinit();
    self.gpa.destroy(self.can);
    self.parse_ast.deinit(self.gpa);
    self.gpa.destroy(self.parse_ast);

    // ModuleEnv.deinit calls self.common.deinit() to clean up CommonEnv's internals
    // Since common is now a value field, we don't need to free it separately
    self.module_env.deinit();
    self.gpa.destroy(self.module_env);
}

/// Canonicalizes the root expression from the parsed AST, returning null if there are parse errors.
pub fn canonicalizeExpr(self: *TestEnv) !?Can.CanonicalizedExpr {
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(self.parse_ast.root_node_idx);

    if (self.parse_ast.parse_diagnostics.items.len > 0 or
        self.parse_ast.tokenize_diagnostics.items.len > 0)
    {
        return null;
    }

    return try self.can.canonicalizeExpr(expr_idx);
}

/// Retrieves a canonical expression from the module store by its index.
pub fn getCanonicalExpr(self: *TestEnv, idx: CIR.Expr.Idx) CIR.Expr {
    return self.module_env.store.getExpr(idx);
}

/// Gets the string representation of an identifier by its index.
pub fn getIdent(self: *TestEnv, idx: base.Ident.Idx) []const u8 {
    return self.module_env.common.getIdent(idx);
}

/// Checks if there are any parse or tokenization errors in the AST.
pub fn hasParseErrors(self: *TestEnv) bool {
    return self.parse_ast.parse_diagnostics.items.len > 0 or
        self.parse_ast.tokenize_diagnostics.items.len > 0;
}

/// Returns all diagnostics from the module environment.
pub fn getDiagnostics(self: *TestEnv) ![]CIR.Diagnostic {
    return self.module_env.getDiagnostics();
}

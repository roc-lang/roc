//! Test environment for canonicalization testing, providing utilities to parse, canonicalize, and inspect Roc expressions.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can_mod = @import("../mod.zig");
const CIR = can_mod.CIR;
const Can = can_mod.Can;
const ModuleEnv = can_mod.ModuleEnv;

const CommonEnv = base.CommonEnv;

gpa: std.mem.Allocator,
module_env: *ModuleEnv,
parse_ast: *parse.AST,
can: *Can,
src_testing: base.SrcBytes.Testing,

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

    // Create SrcBytes from source
    var src_testing = try base.SrcBytes.Testing.initFromSlice(gpa, source);
    errdefer src_testing.deinit(gpa);

    // Initialize the ModuleEnv with the CommonEnv
    module_env.* = try ModuleEnv.init(gpa, src_testing.src);
    errdefer module_env.deinit();

    parse_ast.* = try parse.parseExpr(&module_env.common, gpa);
    errdefer parse_ast.deinit(gpa);

    // Phase 4: AST Structure Validation - removed since store interface no longer exists

    try module_env.initCIRFields(gpa, "test");

    can.* = Can.init(parse_ast, &module_env.types);

    return TestEnv{
        .gpa = gpa,
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
        .src_testing = src_testing,
    };
}

pub fn deinit(self: *TestEnv) void {
    self.can.deinit(self.gpa);
    self.gpa.destroy(self.can);
    self.parse_ast.deinit(self.gpa);
    self.gpa.destroy(self.parse_ast);

    // ModuleEnv.deinit calls self.common.deinit() to clean up CommonEnv's internals
    // Since common is now a value field, we don't need to free it separately
    self.module_env.deinit();
    self.gpa.destroy(self.module_env);
    
    // Clean up SrcBytes.Testing
    self.src_testing.deinit(self.gpa);
}

/// Canonicalizes the root expression from the parsed AST, returning null if there are parse errors.
pub fn canonicalizeExpr(self: *TestEnv) !?Can.Expr.Idx {
    const expr_idx: parse.AST.Node.Idx = @enumFromInt(self.parse_ast.root_node_idx);

    if (self.parse_ast.parse_diagnostics.items.len > 0 or
        self.parse_ast.tokenize_diagnostics.items.len > 0)
    {
        return null;
    }

    return try self.can.canonicalizeExpr(self.gpa, expr_idx, self.module_env.common.source.bytes(), &self.module_env.common.idents);
}

/// Retrieves a canonical expression from the module store by its index.
pub fn getCanonicalExpr(self: *TestEnv, idx: CIR.Expr.Idx) CIR.ExprView {
    return self.can.getExpr(idx);
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

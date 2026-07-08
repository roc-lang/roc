//! Tests for local type declarations in block contexts.
//!
//! Local type declarations allow defining type aliases, nominal types, and opaque types
//! within function bodies and blocks, scoped to that block.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const TestEnv = @import("TestEnv.zig").TestEnv;
const BuiltinTestContext = @import("./BuiltinTestContext.zig").BuiltinTestContext;
const ModuleEnv = @import("../ModuleEnv.zig");
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");

const CoreCtx = @import("ctx").CoreCtx;
const testing = std.testing;
const Ident = base.Ident;
const Statement = CIR.Statement;

const SourceAuditError = error{
    TestUnexpectedResult,
    MissingCanonicalizeBlock,
    MissingCanonicalizeBlockWalk,
    MissingCanonicalizationSourceStart,
    MissingCanonicalizationSourceEnd,
};

const TypeDeclTestError = std.mem.Allocator.Error || SourceAuditError || error{
    TestExpectedEqual,
    MissingQualifiedOuterOkIdent,
};

fn canonicalizeModuleAndCheck(source: []const u8, check: anytype) TypeDeclTestError!void {
    const allocator = testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    const roc_ctx = CoreCtx.testing(allocator, allocator);
    var czer = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer czer.deinit();

    try czer.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);

    try check(&env, diagnostics);
}

fn countRedeclarationDiagnostics(diagnostics: []const CIR.Diagnostic) usize {
    var count: usize = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .type_redeclared,
            .type_alias_redeclared,
            .nominal_type_redeclared,
            => count += 1,
            else => {},
        }
    }
    return count;
}

fn countMutualAliasDiagnostics(diagnostics: []const CIR.Diagnostic) usize {
    var count: usize = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .mutually_recursive_type_aliases => count += 1,
            else => {},
        }
    }
    return count;
}

fn countUndeclaredTypeDiagnostics(diagnostics: []const CIR.Diagnostic) usize {
    var count: usize = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .undeclared_type => count += 1,
            else => {},
        }
    }
    return count;
}

fn countIdentNotInScopeDiagnostics(env: *ModuleEnv, diagnostics: []const CIR.Diagnostic, name: []const u8) usize {
    var count: usize = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .ident_not_in_scope => |d| {
                if (std.mem.eql(u8, env.getIdent(d.ident), name)) count += 1;
            },
            else => {},
        }
    }
    return count;
}

fn countLocalReferenceBeforeDefinitionDiagnostics(env: *ModuleEnv, diagnostics: []const CIR.Diagnostic, name: []const u8) usize {
    var count: usize = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .local_reference_before_definition => |d| {
                if (std.mem.eql(u8, env.getIdent(d.ident), name)) count += 1;
            },
            else => {},
        }
    }
    return count;
}

fn countMutuallyRecursiveLocalDefinitionDiagnostics(diagnostics: []const CIR.Diagnostic) usize {
    var count: usize = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .mutually_recursive_local_definitions => count += 1,
            else => {},
        }
    }
    return count;
}

fn countAssociatedLookupDiagnostics(env: *ModuleEnv, diagnostics: []const CIR.Diagnostic, name: []const u8) usize {
    var count: usize = 0;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .qualified_ident_does_not_exist => |d| {
                if (std.mem.eql(u8, env.getIdent(d.ident), name)) count += 1;
            },
            .nested_value_not_found => |d| {
                const parent = env.getIdent(d.parent_name);
                const nested = env.getIdent(d.nested_name);
                if (std.mem.eql(u8, name, parent) or std.mem.eql(u8, name, nested)) count += 1;
            },
            else => {},
        }
    }
    return count;
}

fn isQualifiedName(name: []const u8, parent_name: []const u8, child_name: []const u8) bool {
    const suffix_len = parent_name.len + 1 + child_name.len;
    if (name.len < suffix_len) return false;

    const suffix_start = name.len - suffix_len;
    if (suffix_start != 0 and name[suffix_start - 1] != '.') return false;

    const suffix = name[suffix_start..];
    if (!std.mem.eql(u8, suffix[0..parent_name.len], parent_name)) return false;
    if (suffix[parent_name.len] != '.') return false;
    return std.mem.eql(u8, suffix[parent_name.len + 1 ..], child_name);
}

fn isNameOrQualifiedSuffix(name: []const u8, bare_name: []const u8) bool {
    if (std.mem.eql(u8, name, bare_name)) return true;
    if (name.len <= bare_name.len) return false;
    const suffix_start = name.len - bare_name.len;
    return name[suffix_start - 1] == '.' and std.mem.eql(u8, name[suffix_start..], bare_name);
}

fn countValueLookupDiagnostics(env: *ModuleEnv, diagnostics: []const CIR.Diagnostic, parent_name: []const u8, child_name: []const u8) usize {
    var count: usize = 0;

    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .ident_not_in_scope => |d| {
                const ident = env.getIdent(d.ident);
                if (isNameOrQualifiedSuffix(ident, parent_name) or isQualifiedName(ident, parent_name, child_name)) count += 1;
            },
            .qualified_ident_does_not_exist => |d| {
                if (isQualifiedName(env.getIdent(d.ident), parent_name, child_name)) count += 1;
            },
            .nested_value_not_found => |d| {
                const parent = env.getIdent(d.parent_name);
                const nested = env.getIdent(d.nested_name);
                if (std.mem.eql(u8, parent, parent_name) and std.mem.eql(u8, nested, child_name)) count += 1;
            },
            else => {},
        }
    }
    return count;
}

fn expectSourceDoesNotContain(source: []const u8, needle: []const u8) error{TestUnexpectedResult}!void {
    if (std.mem.find(u8, source, needle)) |_| {
        std.debug.print("Source still contains forbidden canonicalization structure: {s}\n", .{needle});
        return error.TestUnexpectedResult;
    }
}

fn expectBlockSetupDoesNotScanStatements(can_source: []const u8) SourceAuditError!void {
    const expr_driver_start = std.mem.find(u8, can_source, "fn runExprKernel(") orelse return error.MissingCanonicalizeBlock;
    const block_start_rel = std.mem.find(u8, can_source[expr_driver_start..], ".block => |e| {") orelse return error.MissingCanonicalizeBlock;
    const block_start = expr_driver_start + block_start_rel;
    const walk_start_rel = std.mem.find(u8, can_source[block_start..], "stacks.pushBlockNext(") orelse return error.MissingCanonicalizeBlockWalk;
    const block_setup = can_source[block_start .. block_start + walk_start_rel];

    try expectSourceDoesNotContain(block_setup, "while (");
    try expectSourceDoesNotContain(block_setup, "for (");
    try expectSourceDoesNotContain(block_setup, "getStatement(");
}

fn expectSourceSliceBetweenDoesNotContain(
    source: []const u8,
    start_marker: []const u8,
    end_marker: []const u8,
    needle: []const u8,
) SourceAuditError!void {
    const start = std.mem.find(u8, source, start_marker) orelse return error.MissingCanonicalizationSourceStart;
    const end_rel = std.mem.find(u8, source[start..], end_marker) orelse return error.MissingCanonicalizationSourceEnd;
    const source_slice = source[start .. start + end_rel];
    try expectSourceDoesNotContain(source_slice, needle);
}

fn expectSourceFunctionBodyDoesNotContain(
    source: []const u8,
    start_marker: []const u8,
    next_fn_marker: []const u8,
    needle: []const u8,
) SourceAuditError!void {
    const fn_start = std.mem.find(u8, source, start_marker) orelse return error.MissingCanonicalizationSourceStart;
    const body_start_rel = std.mem.find(u8, source[fn_start..], "{") orelse return error.MissingCanonicalizationSourceStart;
    const body_start = fn_start + body_start_rel + 1;
    const body_end_rel = std.mem.find(u8, source[body_start..], next_fn_marker) orelse return error.MissingCanonicalizationSourceEnd;
    try expectSourceDoesNotContain(source[body_start .. body_start + body_end_rel], needle);
}

fn expectOldCanonicalizationTraversalDeleted(can_source: []const u8) SourceAuditError!void {
    try expectSourceDoesNotContain(can_source, "canonicalizeExpr" ++ "StackSafe");
    try expectSourceDoesNotContain(can_source, "Expr" ++ "Frame");
    try expectSourceDoesNotContain(can_source, "const Block" ++ "Work = struct");
    try expectSourceDoesNotContain(can_source, "Pattern" ++ "Frame");
    try expectSourceDoesNotContain(can_source, "TypeAnno" ++ "Frame");
    try expectSourceDoesNotContain(can_source, "Expr" ++ "KernelStep");
    try expectSourceDoesNotContain(can_source, "Pattern" ++ "KernelStep");
    try expectSourceDoesNotContain(can_source, "TypeAnno" ++ "KernelStep");
    try expectSourceDoesNotContain(can_source, "Associated" ++ "Step");
    try expectSourceDoesNotContain(can_source, "Associated" ++ "Frame");
    try expectSourceDoesNotContain(can_source, "while (frames.pop())");
    try expectSourceDoesNotContain(can_source, "ArrayList(Expr" ++ "KernelResult)");
    try expectSourceDoesNotContain(can_source, "const Expr" ++ "KernelResult");
    try expectSourceDoesNotContain(can_source, "Expr" ++ "ResultSlots");
    try expectSourceDoesNotContain(can_source, "ArrayList(?CanonicalizedExpr)");
    try expectSourceDoesNotContain(can_source, "const Block" ++ "State = struct");
    try expectSourceDoesNotContain(can_source, "const Block" ++ "Work = struct");
    try expectSourceDoesNotContain(can_source, "Associated" ++ "BlockWork");
    try expectSourceDoesNotContain(can_source, "Kernel" ++ "Step");
}

fn expectCanonicalizationKernelsUseExplicitPayloadStacks(can_source: []const u8) SourceAuditError!void {
    try expectSourceDoesNotContain(can_source, "coerceKernelStruct");
    try expectSourceDoesNotContain(can_source, "coerceKernelListItem");
    try expectSourceDoesNotContain(can_source, "arrayListItemType");
    try expectSourceDoesNotContain(can_source, "canonicalizedAssociatedLookupMaybeUsed");

    try expectSourceSliceBetweenDoesNotContain(can_source, "const PatternKernelWork = struct", "const ExprKernelLabel = enum", "fn append(self: *PatternKernelWork");
    try expectSourceSliceBetweenDoesNotContain(can_source, "const ExprKernelWork = struct", "const ExprRecordFieldWork = struct", "fn append(self: *ExprKernelWork");
    try expectSourceSliceBetweenDoesNotContain(can_source, "const TypeAnnoKernelWork = struct", "fn runTypeAnnoKernel(", "fn append(self: *TypeAnnoKernelWork");

    try expectSourceSliceBetweenDoesNotContain(can_source, "const PatternKernelWork = struct", "const ExprKernelLabel = enum", "std.ArrayList(struct");
    try expectSourceSliceBetweenDoesNotContain(can_source, "const ExprKernelWork = struct", "const ExprRecordFieldWork = struct", "std.ArrayList(struct");
    try expectSourceSliceBetweenDoesNotContain(can_source, "const TypeAnnoKernelWork = struct", "fn runTypeAnnoKernel(", "std.ArrayList(struct");
    try expectSourceSliceBetweenDoesNotContain(can_source, "fn runExprKernel(", "fn addBoolTagExpr", "frames.");
    try expectSourceSliceBetweenDoesNotContain(can_source, "pub fn canonicalizePattern(", "fn enterFunction", "frames.");
    try expectSourceSliceBetweenDoesNotContain(can_source, "fn runTypeAnnoKernel(", "/// Handle basic type lookup", "frames.");
}

fn expectCanonicalizationTypePathLookupIsNonRecursive(can_source: []const u8) SourceAuditError!void {
    try expectSourceFunctionBodyDoesNotContain(
        can_source,
        "fn parserTypePathFromRoot(",
        "fn moduleParserTypePathForSegments(",
        "parserTypePathFromRoot(",
    );
}

fn expectCanonicalizationKernelsDoNotCallRecursiveWrappers(can_source: []const u8) SourceAuditError!void {
    try expectSourceSliceBetweenDoesNotContain(can_source, "fn runExprKernel(", "fn addBoolTagExpr", "canonicalizeExpr(");
    try expectSourceSliceBetweenDoesNotContain(can_source, "fn runExprKernel(", "fn addBoolTagExpr", "canonicalizeExprOrMalformed(");
    try expectSourceSliceBetweenDoesNotContain(can_source, "fn runExprKernel(", "fn addBoolTagExpr", "canonicalizeStatement");
    try expectSourceSliceBetweenDoesNotContain(can_source, "fn runExprKernel(", "fn addBoolTagExpr", "processAssociatedBlock(");
    try expectSourceSliceBetweenDoesNotContain(can_source, "pub fn canonicalizePattern(", "fn enterFunction", "self.canonicalizePattern(");
    try expectSourceSliceBetweenDoesNotContain(can_source, "pub fn canonicalizePattern(", "fn enterFunction", "canonicalizePatternOrMalformed(");
    try expectSourceSliceBetweenDoesNotContain(can_source, "fn runTypeAnnoKernel(", "/// Handle basic type lookup", "canonicalizeTypeAnno(");
    try expectSourceSliceBetweenDoesNotContain(can_source, "fn runTypeAnnoKernel(", "/// Handle basic type lookup", "self.runTypeAnnoKernel(");
}

test "canonicalization records explicit type declaration tables" {
    const allocator = testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    const source =
        \\A : B
        \\B : U64
        \\C := [C]
    ;

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    const roc_ctx = CoreCtx.testing(allocator, allocator);
    var czer = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer czer.deinit();

    try czer.canonicalizeFile();

    try testing.expectEqual(@as(u32, 3), env.type_decls.span.len);
    try testing.expectEqual(@as(u32, 1), env.forward_type_decls.span.len);

    const forward_stmt = env.store.statementAt(env.forward_type_decls, 0);
    const header = switch (env.store.getStatement(forward_stmt)) {
        .s_alias_decl => |alias| env.store.getTypeHeader(alias.header),
        else => return error.ExpectedForwardAlias,
    };
    try testing.expectEqualStrings("B", env.getIdent(header.relative_name));
}

test "nested type redeclarations are detected after previous associated scope exits" {
    const allocator = testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    const source =
        \\T := [A].{
        \\    L2 := [B].{
        \\        L3 := [C]
        \\    }
        \\
        \\    L2 := [D].{
        \\        L3 := [E]
        \\    }
        \\}
    ;

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    const roc_ctx = CoreCtx.testing(allocator, allocator);
    var czer = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer czer.deinit();

    try czer.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);

    var saw_l2_redeclared = false;
    var saw_l3_redeclared = false;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .type_redeclared => |data| {
                const name = env.getIdent(data.name);
                if (std.mem.eql(u8, name, "Test.T.L2")) saw_l2_redeclared = true;
                if (std.mem.eql(u8, name, "Test.T.L2.L3")) saw_l3_redeclared = true;
            },
            else => {},
        }
    }

    try testing.expect(saw_l2_redeclared);
    try testing.expect(saw_l3_redeclared);
}

test "block-local type paths with the same text do not redeclare each other" {
    const source =
        \\first = {
        \\    T := [First].{
        \\        Inner := [FirstInner]
        \\    }
        \\    1
        \\}
        \\
        \\second = {
        \\    T := [Second].{
        \\        Inner := [SecondInner]
        \\    }
        \\    2
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(_: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 0), countRedeclarationDiagnostics(diagnostics));
        }
    }.check);
}

test "block-local associated value lookup resolves through the visible local owner" {
    const source =
        \\first = {
        \\    T := [First].{
        \\        marker = 1
        \\    }
        \\    T.marker
        \\}
        \\
        \\second = {
        \\    T := [Second].{
        \\        marker = 2
        \\    }
        \\    T.marker
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(_: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 0), countRedeclarationDiagnostics(diagnostics));
            try testing.expectEqual(@as(usize, 0), countUndeclaredTypeDiagnostics(diagnostics));
        }
    }.check);
}

test "same-named aliases in separate block scopes are not mutually recursive" {
    const source =
        \\first = {
        \\    A : B
        \\    B : U64
        \\    1
        \\}
        \\
        \\second = {
        \\    B : A
        \\    A : U64
        \\    2
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(_: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 0), countMutualAliasDiagnostics(diagnostics));
        }
    }.check);
}

test "module-qualified type lookup ignores same-named block-local type roots" {
    const source =
        \\T := [ModuleT].{
        \\    marker = 1
        \\}
        \\
        \\value = {
        \\    T := [LocalT].{
        \\        marker = 2
        \\    }
        \\    Test.T.marker
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(_: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 0), countRedeclarationDiagnostics(diagnostics));
            try testing.expectEqual(@as(usize, 0), countUndeclaredTypeDiagnostics(diagnostics));
        }
    }.check);
}

test "block-local type use before declaration does not forward resolve" {
    const source =
        \\value = {
        \\    x : T
        \\    x = 1
        \\    T := [LocalT]
        \\    x
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(_: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 1), countUndeclaredTypeDiagnostics(diagnostics));
        }
    }.check);
}

test "block-local lambda use before declaration does not forward resolve" {
    const source =
        \\value = {
        \\    before = later(1)
        \\    later = |n| n
        \\    before
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(env: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 1), countLocalReferenceBeforeDefinitionDiagnostics(env, diagnostics, "later"));
        }
    }.check);
}

test "block-local lambdas cannot be mutually recursive through forward declaration" {
    const source =
        \\value = {
        \\    first = |n| second(n)
        \\    second = |n| first(n)
        \\    first(0)
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(_: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 1), countMutuallyRecursiveLocalDefinitionDiagnostics(diagnostics));
        }
    }.check);
}

test "block-local lambda declaration does not capture earlier use of same-named outer lambda" {
    const source =
        \\later = |n| n + 10
        \\
        \\value = {
        \\    before = later(1)
        \\    later = |n| n + 100
        \\    before
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(env: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 0), countIdentNotInScopeDiagnostics(env, diagnostics, "later"));
            try testing.expectEqual(@as(usize, 0), countLocalReferenceBeforeDefinitionDiagnostics(env, diagnostics, "later"));
        }
    }.check);
}

test "malformed associated type header does not suppress later associated value" {
    const source =
        \\Outer := [Outer].{
        \\    Broken(a := [Broken]
        \\    ok = 1
        \\}
        \\
        \\use = Outer.ok
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(env: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            const qualified_outer_ok = env.common.findIdent("Test.Outer.ok") orelse return error.MissingQualifiedOuterOkIdent;
            try testing.expect(env.getExposedValueNodeIndexById(qualified_outer_ok) != null);
            try testing.expectEqual(@as(usize, 0), countAssociatedLookupDiagnostics(env, diagnostics, "Outer.ok"));
        }
    }.check);
}

test "block-local associated value does not leak after owner scope exits" {
    const source =
        \\Test := [].{
        \\    first = {
        \\        T := [First].{
        \\            marker = 1
        \\        }
        \\        1
        \\    }
        \\
        \\    leaked = T.marker
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(env: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 1), countValueLookupDiagnostics(env, diagnostics, "T", "marker"));

            if (env.common.findIdent("T.marker")) |ident| {
                try testing.expect(env.getExposedValueNodeIndexById(ident) == null);
            }
            if (env.common.findIdent("Test.T.marker")) |ident| {
                try testing.expect(env.getExposedValueNodeIndexById(ident) == null);
            }
        }
    }.check);
}

test "canonicalization has no separate nested associated item alias traversal" {
    const can_source = @embedFile("../Can.zig");

    try expectSourceDoesNotContain(can_source, "fn introduceNestedItemAliases");
    try expectSourceDoesNotContain(can_source, "statementSlice(assoc_statements)");
}

test "canonicalization block setup does not pre-scan block statements" {
    const can_source = @embedFile("../Can.zig");

    try expectOldCanonicalizationTraversalDeleted(can_source);
    try expectBlockSetupDoesNotScanStatements(can_source);
    try expectCanonicalizationKernelsUseExplicitPayloadStacks(can_source);
    try expectCanonicalizationKernelsDoNotCallRecursiveWrappers(can_source);
    try expectCanonicalizationTypePathLookupIsNonRecursive(can_source);
}

test "package header auto imports consume parser inventory instead of scanning statements" {
    const can_source = @embedFile("../Can.zig");

    try expectSourceDoesNotContain(can_source, "fn collectPackageHeaderAutoImports");
    try expectSourceDoesNotContain(can_source, "collectPackageHeaderAutoImports(header.package.exposes, file.statements");
}

test "block-local recursive nominal type can reference itself in its declaration" {
    const source =
        \\Test := [].{
        \\    value = {
        \\        Tree := [Leaf, Node(Tree)]
        \\        1
        \\    }
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(_: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            try testing.expectEqual(@as(usize, 0), countUndeclaredTypeDiagnostics(diagnostics));
        }
    }.check);
}

test "local type alias redeclaration diagnostic renders" {
    const source =
        \\Test := [].{
        \\    value = {
        \\        T : U64
        \\        T : U8
        \\        1
        \\    }
        \\}
    ;

    try canonicalizeModuleAndCheck(source, struct {
        fn check(env: *ModuleEnv, diagnostics: []const CIR.Diagnostic) TypeDeclTestError!void {
            var count: usize = 0;
            for (diagnostics) |diagnostic| {
                switch (diagnostic) {
                    .type_alias_redeclared => {
                        count += 1;
                        var report = try env.diagnosticToReport(diagnostic, testing.allocator, env.module_name);
                        defer report.deinit();
                        try testing.expect(report.title.len > 0);
                    },
                    else => {},
                }
            }
            try testing.expectEqual(@as(usize, 1), count);
        }
    }.check);
}

test "local type alias is parsed and canonicalized" {
    const source =
        \\|_| {
        \\    MyNum : U64
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    // Check diagnostics - should have no errors
    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "local nominal type is parsed and canonicalized" {
    const source =
        \\|_| {
        \\    Counter := U64
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "local opaque type is parsed and canonicalized" {
    // Use U8 instead of Str since Str is an auto-imported type, not a builtin
    const source =
        \\|_| {
        \\    Secret :: U8
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "nested blocks with local types" {
    // Use builtin types (U64, U8) instead of Str
    const source =
        \\|_| {
        \\    OuterType : U64
        \\    inner = {
        \\        InnerType : U8
        \\        42
        \\    }
        \\    inner
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "multiple local types in same block" {
    // Use builtin types (U64, U8) instead of Str
    const source =
        \\|_| {
        \\    First : U64
        \\    Second : U8
        \\    Third : { a: U64, b: U8 }
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "local type with type parameters" {
    // Type parameters use parentheses syntax: MyList(a)
    const source =
        \\|_| {
        \\    MyWrapper(a) : List(a)
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "expression that looks like type decl but isn't - record field" {
    // Record fields use lowercase names with colons: { name: "value" }
    // This should NOT be parsed as a type declaration
    const source =
        \\|_| {
        \\    x = { name: 42, count: 10 }
        \\    x
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    // This should parse as a record, not as a type declaration
    // No type-related errors expected
    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var type_decl_errors: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .undeclared_type => type_decl_errors += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), type_decl_errors);
}

test "local type alias can be used in annotation" {
    // Test that a locally defined type alias can be used in a type annotation
    const source =
        \\|_| {
        \\    MyNum : U64
        \\    x : MyNum
        \\    x = 42
        \\    x
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const result = try test_env.canonicalizeExpr();
    try testing.expect(result != null);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .not_implemented => error_count += 1,
            .undeclared_type => error_count += 1,
            .ident_not_in_scope => error_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 0), error_count);
}

test "scopeLookupTypeDecl API is accessible" {
    const gpa = testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();
    const source = "";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    const roc_ctx = CoreCtx.testing(gpa, gpa);

    const ast = try parse.expr(gpa, &env.common);
    defer ast.deinit();

    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();

    // Enter a scope
    try can.scopeEnter(gpa, true);

    // Look up a type that doesn't exist - should return null
    const my_type_ident = try env.insertIdent(Ident.for_text("MyType"));
    const type_lookup = try can.scopeLookupTypeDecl(my_type_ident);

    try testing.expect(type_lookup == null);
}

test "introduceType API is accessible" {
    const gpa = testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();
    const source = "";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    const roc_ctx = CoreCtx.testing(gpa, gpa);

    const ast = try parse.expr(gpa, &env.common);
    defer ast.deinit();

    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();

    // Enter a scope for local type declarations
    try can.scopeEnter(gpa, true);

    // Create a type header manually
    const type_name = try env.insertIdent(Ident.for_text("LocalType"));
    const type_header = CIR.TypeHeader{
        .name = type_name,
        .relative_name = type_name,
        .args = CIR.TypeAnno.Span{ .span = base.DataSpan.empty() },
    };
    const header_idx = try env.addTypeHeader(type_header, base.Region.zero());

    // Create a type alias statement
    const alias_stmt = Statement{
        .s_alias_decl = .{
            .header = header_idx,
            .anno = .placeholder,
        },
    };
    const stmt_idx = try env.addStatement(alias_stmt, base.Region.zero());

    // Introduce the type into scope
    try can.introduceType(type_name, stmt_idx, base.Region.zero());

    // Verify the type is now in scope
    const type_lookup = try can.scopeLookupTypeDecl(type_name);
    try testing.expect(type_lookup != null);
    try testing.expect(type_lookup.? == stmt_idx);
}

test "open ext not allowed in type decl" {
    const source =
        \\|_| {
        \\    Counter := [A, B, ..]
        \\    42
        \\}
    ;

    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    _ = try test_env.canonicalizeExpr();

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var diag_count: usize = 0;
    for (diagnostics) |diag| {
        switch (diag) {
            .open_ext_not_allowed_in_type_decl => diag_count += 1,
            else => {},
        }
    }
    try testing.expectEqual(@as(usize, 1), diag_count);
}

test "local type scoping - not visible after exiting block" {
    const gpa = testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();
    const source = "";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    const roc_ctx = CoreCtx.testing(gpa, gpa);

    const ast = try parse.expr(gpa, &env.common);
    defer ast.deinit();

    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();

    // Enter outer scope
    try can.scopeEnter(gpa, true);

    // Enter inner scope
    try can.scopeEnter(gpa, false);

    // Create and introduce a local type in the inner scope
    const type_name = try env.insertIdent(Ident.for_text("InnerType"));
    const type_header = CIR.TypeHeader{
        .name = type_name,
        .relative_name = type_name,
        .args = CIR.TypeAnno.Span{ .span = base.DataSpan.empty() },
    };
    const header_idx = try env.addTypeHeader(type_header, base.Region.zero());
    const alias_stmt = Statement{
        .s_alias_decl = .{
            .header = header_idx,
            .anno = .placeholder,
        },
    };
    const stmt_idx = try env.addStatement(alias_stmt, base.Region.zero());
    try can.introduceType(type_name, stmt_idx, base.Region.zero());

    // Type should be visible in inner scope
    const lookup_in_inner = try can.scopeLookupTypeDecl(type_name);
    try testing.expect(lookup_in_inner != null);

    // Exit inner scope
    try can.scopeExit(gpa);

    // Type should NOT be visible in outer scope anymore
    const lookup_in_outer = try can.scopeLookupTypeDecl(type_name);
    try testing.expect(lookup_in_outer == null);
}

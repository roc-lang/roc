//! Tests for the typed CIR module and view APIs.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");
const TypedCIR = @import("../typed_cir.zig");

test "typed CIR exposes solved vars on defs exprs and patterns" {
    var test_env = try TestEnv.init("Test",
        \\id = \x -> x
        \\answer = id(42)
    );
    defer test_env.deinit();

    const source_modules = [_]TypedCIR.Modules.SourceModule{
        test_env.takePublishedSourceModule(),
        .{ .precompiled = test_env.builtin_module.env },
    };
    var modules = try TypedCIR.Modules.init(std.testing.allocator, &source_modules);
    defer modules.deinit();
    const module = modules.module(0);
    const defs = test_env.module_env.store.sliceDefs(test_env.module_env.all_defs);

    try std.testing.expect(defs.len >= 2);

    for (defs) |def_idx| {
        const typed_cir_def = module.def(def_idx);
        try std.testing.expectEqual(def_idx, typed_cir_def.idx);
        try std.testing.expectEqual(module.exprType(typed_cir_def.data.expr), typed_cir_def.expr.ty());
        try std.testing.expectEqual(module.patternType(typed_cir_def.data.pattern), typed_cir_def.pattern.ty());

        const expr = typed_cir_def.expr.data;
        if (expr == .e_lambda) {
            const arg_patterns = test_env.module_env.store.slicePatterns(expr.e_lambda.args);
            try std.testing.expect(arg_patterns.len > 0);
            const typed_cir_arg = module.pattern(arg_patterns[0]);
            try std.testing.expectEqual(module.patternType(arg_patterns[0]), typed_cir_arg.ty());
        }
    }
}

test "published typed CIR survives checker teardown" {
    var test_env = try TestEnv.init("Test",
        \\a = 1
        \\b = a
    );

    const source_modules = [_]TypedCIR.Modules.SourceModule{
        test_env.takePublishedSourceModule(),
        .{ .precompiled = test_env.builtin_module.env },
    };
    var modules = try TypedCIR.Modules.init(std.testing.allocator, &source_modules);
    defer modules.deinit();

    const expected_name = try std.testing.allocator.dupe(u8, modules.module(0).name());
    defer std.testing.allocator.free(expected_name);
    const expected_def_count = modules.module(0).allDefs().len;
    const expected_scc_count = modules.module(0).evaluationOrder().?.sccs.len;

    test_env.deinit();

    const module = modules.module(0);
    try std.testing.expectEqualStrings(expected_name, module.name());
    try std.testing.expectEqual(expected_def_count, module.allDefs().len);
    try std.testing.expect(module.evaluationOrder() != null);
    try std.testing.expectEqual(expected_scc_count, module.evaluationOrder().?.sccs.len);
}

test "a type declared in a function body takes the function's variables its backing names as implicit formals" {
    var test_env = try TestEnv.init("Test",
        \\f : a, b -> a
        \\f = |x, _y| {
        \\    Plain := { n : U8 }
        \\    Inner(c) := { v : a, c : c }
        \\    Outer := { inner : Inner(U8), more : List(b) }
        \\    _p = Plain.{ n: 1 }
        \\    _o = Outer.{ inner: Inner.{ v: x, c: 2 }, more: [] }
        \\    x
        \\}
    );
    defer test_env.deinit();
    try test_env.assertNoErrors();
    const env = test_env.module_env;

    const source_modules = [_]TypedCIR.Modules.SourceModule{
        test_env.takePublishedSourceModule(),
        .{ .precompiled = test_env.builtin_module.env },
    };
    var modules = try TypedCIR.Modules.init(std.testing.allocator, &source_modules);
    defer modules.deinit();
    const module = modules.module(0);

    var seen: usize = 0;
    for (env.store.sliceStatements(env.all_statements)) |statement_idx| {
        const statement = env.store.getStatement(statement_idx);
        if (statement != .s_nominal_decl) continue;
        const header = env.store.getTypeHeader(statement.s_nominal_decl.header);
        const name = env.getIdentStoreConst().getText(header.relative_name);
        const implicit = try module.nominalDeclarationImplicitFormals(std.testing.allocator, statement_idx);
        defer std.testing.allocator.free(implicit);
        var names: [2][]const u8 = undefined;
        try std.testing.expect(implicit.len <= names.len);
        for (implicit, 0..) |var_, index| {
            const content = env.types.resolveVar(var_).desc.content;
            try std.testing.expect(content == .rigid);
            names[index] = env.getIdentStoreConst().getText(content.rigid.name);
        }
        if (std.mem.endsWith(u8, name, "Plain")) {
            try std.testing.expectEqual(@as(usize, 0), implicit.len);
        } else if (std.mem.endsWith(u8, name, "Inner")) {
            // Its own formal `c` is not implicit.
            try std.testing.expectEqual(@as(usize, 1), implicit.len);
            try std.testing.expectEqualStrings("a", names[0]);
        } else if (std.mem.endsWith(u8, name, "Outer")) {
            // `a` through `Inner`, and `b`.
            try std.testing.expectEqual(@as(usize, 2), implicit.len);
            try std.testing.expect(!std.mem.eql(u8, names[0], names[1]));
            for (names) |implicit_name| {
                try std.testing.expect(std.mem.eql(u8, implicit_name, "a") or std.mem.eql(u8, implicit_name, "b"));
            }
        } else continue;
        seen += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), seen);
}

//! Tests for Records

const std = @import("std");
const parse = @import("parse");
const base = @import("base");
const ModuleEnv = @import("../ModuleEnv.zig");
const Can = @import("../Can.zig");

const Ident = base.Ident;

test "record literal uses record_unbound" {
    const gpa = std.testing.allocator;

    // Test a simple record literal
    {
        const source = "{ x: 42, y: \"hello\" }";

        var env = try ModuleEnv.init(gpa, source);
        defer env.deinit();

        try env.initCIRFields("test");

        var ast = try parse.parseExpr(&env.common, gpa);
        defer ast.deinit(gpa);

        var can = try Can.init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            return error.CanonicalizeError;
        };

        const canonical_expr = env.store.getExpr(canonical_expr_idx.idx);
        // Check that it's a record
        switch (canonical_expr) {
            .e_record => |record| {
                // Success! The record literal created a record
                try std.testing.expect(record.fields.span.len == 2);
            },
            else => return error.ExpectedRecord,
        }
    }

    // Test an empty record literal
    {
        const source2 = "{}";

        var env = try ModuleEnv.init(gpa, source2);
        defer env.deinit();

        try env.initCIRFields("test");

        var ast = try parse.parseExpr(&env.common, gpa);
        defer ast.deinit(gpa);

        var can = try Can.init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            return error.CanonicalizeError;
        };

        const canonical_expr = env.store.getExpr(canonical_expr_idx.idx);
        // Check that it's an empty_record
        switch (canonical_expr) {
            .e_empty_record => {
                // Success! Empty record literal created empty_record
            },
            else => return error.ExpectedEmptyRecord,
        }
    }

    // Test a record with a single field
    // Test a nested record literal
    {
        const source3 = "{ value: 123 }";

        var env = try ModuleEnv.init(gpa, source3);
        defer env.deinit();

        try env.initCIRFields("test");

        var ast = try parse.parseExpr(&env.common, gpa);
        defer ast.deinit(gpa);

        var can = try Can.init(&env, &ast, null);
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            return error.CanonicalizeError;
        };

        const canonical_expr = env.store.getExpr(canonical_expr_idx.idx);
        // Check that it's a record
        switch (canonical_expr) {
            .e_record => |record| {
                // Success! The record literal created a record
                try std.testing.expect(record.fields.span.len == 1);

                const cir_fields = env.store.sliceRecordFields(record.fields);

                const cir_field = env.store.getRecordField(cir_fields[0]);

                const field_name = env.getIdent(cir_field.name);
                try std.testing.expectEqualStrings("value", field_name);
            },
            else => return error.ExpectedRecord,
        }
    }
}

test "record_unbound basic functionality" {
    const gpa = std.testing.allocator;

    const source = "{ x: 42, y: 99 }";

    // Test that record literals create record_unbound types
    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var ast = try parse.parseExpr(&env.common, gpa);
    defer ast.deinit(gpa);

    var can = try Can.init(&env, &ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    const canonical_expr = env.store.getExpr(canonical_expr_idx.idx);
    // Check that it's a record
    switch (canonical_expr) {
        .e_record => |record| {
            // Success! The record literal created a record
            try std.testing.expect(record.fields.span.len == 2);

            const cir_fields = env.store.sliceRecordFields(record.fields);

            const cir_field_0 = env.store.getRecordField(cir_fields[0]);
            const cir_field_1 = env.store.getRecordField(cir_fields[1]);

            // Check field names
            try std.testing.expectEqualStrings("x", env.getIdent(cir_field_0.name));
            try std.testing.expectEqualStrings("y", env.getIdent(cir_field_1.name));
        },
        else => return error.ExpectedRecord,
    }
}

test "record_unbound with multiple fields" {
    const gpa = std.testing.allocator;

    const source = "{ a: 123, b: 456, c: 789 }";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    // Create record_unbound with multiple fields
    var ast = try parse.parseExpr(&env.common, gpa);
    defer ast.deinit(gpa);

    var can = try Can.init(&env, &ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    const canonical_expr = env.store.getExpr(canonical_expr_idx.idx);
    // Check that it's a record
    switch (canonical_expr) {
        .e_record => |record| {
            // Success! The record literal created a record
            try std.testing.expect(record.fields.span.len == 3);

            const cir_fields = env.store.sliceRecordFields(record.fields);

            const cir_field_0 = env.store.getRecordField(cir_fields[0]);
            const cir_field_1 = env.store.getRecordField(cir_fields[1]);
            const cir_field_2 = env.store.getRecordField(cir_fields[2]);

            // Check field names
            try std.testing.expectEqualStrings("a", env.getIdent(cir_field_0.name));
            try std.testing.expectEqualStrings("b", env.getIdent(cir_field_1.name));
            try std.testing.expectEqualStrings("c", env.getIdent(cir_field_2.name));
        },
        else => return error.ExpectedRecord,
    }
}

test "record pattern destructuring" {
    const gpa = std.testing.allocator;

    // Test simple record destructuring: { x, y } = { x: 1, y: 2 }
    const source = "{ x, y } = { x: 1, y: 2 }";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var ast = try parse.parseStatement(&env.common, gpa);
    defer ast.deinit(gpa);

    var can = try Can.init(&env, &ast, null);
    defer can.deinit();

    // Enter a function scope so we can have local bindings
    try can.scopeEnter(gpa, true);

    const stmt_idx: parse.AST.Statement.Idx = @enumFromInt(ast.root_node_idx);
    const stmt = ast.store.getStatement(stmt_idx);

    // The statement should be a declaration
    switch (stmt) {
        .decl => |decl| {
            // Get the pattern from the declaration
            const pattern_idx = decl.pattern;
            const canonical_pattern_idx = try can.canonicalizePattern(pattern_idx) orelse {
                return error.CanonicalizePatternError;
            };

            const canonical_pattern = env.store.getPattern(canonical_pattern_idx);

            // Check that it's a record_destructure pattern
            switch (canonical_pattern) {
                .record_destructure => |rd| {
                    // Get the destructs
                    const destructs = env.store.sliceRecordDestructs(rd.destructs);
                    try std.testing.expect(destructs.len == 2);

                    // Check the first destruct (x)
                    const destruct_x = env.store.getRecordDestruct(destructs[0]);
                    try std.testing.expectEqualStrings("x", env.getIdent(destruct_x.label));

                    // Check the second destruct (y)
                    const destruct_y = env.store.getRecordDestruct(destructs[1]);
                    try std.testing.expectEqualStrings("y", env.getIdent(destruct_y.label));

                    // Verify that x and y are now in scope
                    const x_ident = try env.insertIdent(Ident.for_text("x"));
                    const y_ident = try env.insertIdent(Ident.for_text("y"));

                    const x_lookup = can.scopeLookup(.ident, x_ident);
                    const y_lookup = can.scopeLookup(.ident, y_ident);

                    // Both should be found in scope
                    switch (x_lookup) {
                        .found => {},
                        else => return error.XNotInScope,
                    }
                    switch (y_lookup) {
                        .found => {},
                        else => return error.YNotInScope,
                    }
                },
                else => return error.ExpectedRecordDestructure,
            }
        },
        else => return error.ExpectedDecl,
    }
}

test "record pattern with sub-patterns" {
    const gpa = std.testing.allocator;

    // Test record destructuring with sub-patterns: { name: n, age: a } = person
    const source = "{ name: n, age: a } = person";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var ast = try parse.parseStatement(&env.common, gpa);
    defer ast.deinit(gpa);

    var can = try Can.init(&env, &ast, null);
    defer can.deinit();

    // Enter a function scope so we can have local bindings
    try can.scopeEnter(gpa, true);

    const stmt_idx: parse.AST.Statement.Idx = @enumFromInt(ast.root_node_idx);
    const stmt = ast.store.getStatement(stmt_idx);

    // The statement should be a declaration
    switch (stmt) {
        .decl => |decl| {
            // Get the pattern from the declaration
            const pattern_idx = decl.pattern;
            const canonical_pattern_idx = try can.canonicalizePattern(pattern_idx) orelse {
                return error.CanonicalizePatternError;
            };

            const canonical_pattern = env.store.getPattern(canonical_pattern_idx);

            // Check that it's a record_destructure pattern
            switch (canonical_pattern) {
                .record_destructure => |rd| {
                    // Get the destructs
                    const destructs = env.store.sliceRecordDestructs(rd.destructs);
                    try std.testing.expect(destructs.len == 2);

                    // Check the first destruct (name: n)
                    const destruct_name = env.store.getRecordDestruct(destructs[0]);
                    try std.testing.expectEqualStrings("name", env.getIdent(destruct_name.label));
                    // The ident should be the sub-pattern variable name
                    try std.testing.expectEqualStrings("name", env.getIdent(destruct_name.ident));
                    // Should have a SubPattern kind
                    switch (destruct_name.kind) {
                        .SubPattern => {},
                        else => return error.ExpectedSubPattern,
                    }

                    // Check the second destruct (age: a)
                    const destruct_age = env.store.getRecordDestruct(destructs[1]);
                    try std.testing.expectEqualStrings("age", env.getIdent(destruct_age.label));

                    // Verify that n and a are now in scope (the sub-pattern bindings)
                    const n_ident = try env.insertIdent(Ident.for_text("n"));
                    const a_ident = try env.insertIdent(Ident.for_text("a"));

                    const n_lookup = can.scopeLookup(.ident, n_ident);
                    const a_lookup = can.scopeLookup(.ident, a_ident);

                    // Both should be found in scope
                    switch (n_lookup) {
                        .found => {},
                        else => return error.NNotInScope,
                    }
                    switch (a_lookup) {
                        .found => {},
                        else => return error.ANotInScope,
                    }
                },
                else => return error.ExpectedRecordDestructure,
            }
        },
        else => return error.ExpectedDecl,
    }
}

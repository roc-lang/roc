//! Tests for Records

const std = @import("std");
const parse = @import("parse");
const base = @import("base");
const types = @import("types");
const ModuleEnv = @import("../ModuleEnv.zig");
const Can = @import("../Can.zig");

const Ident = base.Ident;
const TypeVar = types.Var;
const Content = types.Content;

test "record literal uses record_unbound" {
    const gpa = std.testing.allocator;

    // Test a simple record literal
    {
        const source = "{ x: 42, y: \"hello\" }";

        var env = try ModuleEnv.init(gpa, source);
        defer env.deinit();

        try env.initCIRFields(gpa, "test");

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

        try env.initCIRFields(gpa, "test");

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

        try env.initCIRFields(gpa, "test");

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

    try env.initCIRFields(gpa, "test");

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

    try env.initCIRFields(gpa, "test");

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

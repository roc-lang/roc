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

        var can = try Can.init(&env, &ast, null, .{});
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

        var can = try Can.init(&env, &ast, null, .{});
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

        var can = try Can.init(&env, &ast, null, .{});
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

    var can = try Can.init(&env, &ast, null, .{});
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

    var can = try Can.init(&env, &ast, null, .{});
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

test "record with extension variable" {
    const gpa = std.testing.allocator;

    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();

    try env.initCIRFields(gpa, "test");

    // Test that regular records have extension variables
    // Create { x: 42, y: "hi" }* (open record)
    const num_var = try env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_precision = .i32 } } });
    const str_var = try env.types.freshFromContent(Content{ .structure = .str });

    const fields = [_]types.RecordField{
        .{ .name = try env.insertIdent(Ident.for_text("x")), .var_ = num_var },
        .{ .name = try env.insertIdent(Ident.for_text("y")), .var_ = str_var },
    };
    const fields_range = try env.types.appendRecordFields(&fields);
    const ext_var = try env.types.fresh(); // Open extension
    const record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = ext_var } } };
    const record_var = try env.types.freshFromContent(record_content);

    // Verify the record has an extension variable
    const resolved = env.types.resolveVar(record_var);
    switch (resolved.desc.content) {
        .structure => |structure| switch (structure) {
            .record => |record| {
                try std.testing.expect(record.fields.len() == 2);

                // Check that extension is a flex var (open record)
                const ext_resolved = env.types.resolveVar(record.ext);
                switch (ext_resolved.desc.content) {
                    .flex => {
                        // Success! The record has an open extension
                    },
                    else => return error.ExpectedFlexVar,
                }
            },
            else => return error.ExpectedRecord,
        },
        else => return error.ExpectedStructure,
    }

    // Now test a closed record
    const closed_ext_var = try env.types.freshFromContent(Content{ .structure = .empty_record });
    const closed_record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = closed_ext_var } } };
    const closed_record_var = try env.types.freshFromContent(closed_record_content);

    // Verify the closed record has empty_record as extension
    const closed_resolved = env.types.resolveVar(closed_record_var);
    switch (closed_resolved.desc.content) {
        .structure => |structure| switch (structure) {
            .record => |record| {
                try std.testing.expect(record.fields.len() == 2);

                // Check that extension is empty_record (closed record)
                const ext_resolved = env.types.resolveVar(record.ext);
                switch (ext_resolved.desc.content) {
                    .structure => |ext_structure| switch (ext_structure) {
                        .empty_record => {
                            // Success! The record is closed
                        },
                        else => return error.ExpectedEmptyRecord,
                    },
                    else => return error.ExpectedStructure,
                }
            },
            else => return error.ExpectedRecord,
        },
        else => return error.ExpectedStructure,
    }
}

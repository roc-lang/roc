//! Tests for tag union canonicalization during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of tag unions and tag applications from parsed AST into the compiler's
//! canonical internal representation (CIR).

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const builtins = @import("builtins");
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;
const ModuleEnv = @import("../ModuleEnv.zig");

test "canonicalize tag with numeric payload should be e_tag not e_num" {
    // This is the bug: Some(10) gets canonicalized to e_num instead of e_tag
    const source = "Some(10)";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.module_env.store.getExpr(canonical_expr.get_idx());

    // BUG: Currently this is e_num, but it should be e_tag!
    // When we write Some(10), we're applying a tag constructor, not just writing a number
    switch (expr) {
        .e_tag => |tag_expr| {
            // This is what we WANT to happen
            const tag_name = test_env.module_env.getIdent(tag_expr.name);
            try testing.expectEqualStrings("Some", tag_name);
        },
        .e_num => {
            // This is what CURRENTLY happens (the bug!)
            return error.TagWithPayloadCanonicalizedAsNumber;
        },
        else => return error.UnexpectedExpressionType,
    }
}

test "canonicalize list of tags with numeric payloads" {
    // This test reproduces the full bug from BUG_LIST_OF_TAGS.md
    const source = "[Some(10), Some(20)]";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.module_env.store.getExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_list => |list_expr| {
            const elem_indices = test_env.module_env.store.sliceExpr(list_expr.elems);
            try testing.expectEqual(@as(usize, 2), elem_indices.len);

            // Check first element
            const first_elem = test_env.module_env.store.getExpr(elem_indices[0]);
            switch (first_elem) {
                .e_tag => |tag_expr| {
                    const tag_name = test_env.module_env.getIdent(tag_expr.name);
                    try testing.expectEqualStrings("Some", tag_name);

                    // Should have one argument (the number 10)
                    const args = test_env.module_env.store.sliceExpr(tag_expr.args);
                    try testing.expectEqual(@as(usize, 1), args.len);
                },
                .e_num => {
                    // BUG: Elements are e_num instead of e_tag
                    return error.ListElementsShouldBeTagsNotNumbers;
                },
                else => return error.UnexpectedListElementType,
            }
        },
        else => return error.ExpectedListExpression,
    }
}

test "canonicalize tag without payload" {
    // Control test: tags without payloads should work fine
    const source = "None";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.module_env.store.getExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_tag => |tag_expr| {
            const tag_name = test_env.module_env.getIdent(tag_expr.name);
            try testing.expectEqualStrings("None", tag_name);

            const args = test_env.module_env.store.sliceExpr(tag_expr.args);
            try testing.expectEqual(@as(usize, 0), args.len);
        },
        else => return error.ExpectedTagExpression,
    }
}

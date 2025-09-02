const std = @import("std");
const testing = std.testing;
const base = @import("base");
const CIR2 = @import("canonicalize/CIR.zig");
const AST = @import("parse/AST.zig");
const ByteSlices = @import("collections").ByteSlices;
const NodeSlices = @import("collections").NodeSlices;

const Allocator = std.mem.Allocator;
const Position = base.Position;
const Ident = base.Ident;
const Region = base.Region;

test "sign bit encoding: basic mutability encoding" {
    _ = testing.allocator; // Available if needed

    // Test that we can encode and decode mutability correctly
    const node_idx = AST.Node.Idx{ .idx = 42 };

    // Create immutable pattern index
    const immutable_idx = CIR2.Patt.Idx.withMutability(node_idx, false);
    try testing.expect(!immutable_idx.isMutable());
    try testing.expectEqual(node_idx, immutable_idx.toNodeIdx());

    // Create mutable pattern index
    const mutable_idx = CIR2.Patt.Idx.withMutability(node_idx, true);
    try testing.expect(mutable_idx.isMutable());
    try testing.expectEqual(node_idx, mutable_idx.toNodeIdx());

    // Test setMutability
    const changed_idx = immutable_idx.setMutability(true);
    try testing.expect(changed_idx.isMutable());
    try testing.expectEqual(node_idx, changed_idx.toNodeIdx());
}

test "sign bit encoding: edge cases" {
    _ = testing.allocator; // Available if needed

    // Test with index 0
    const node_idx_0 = AST.Node.Idx{ .idx = 0 };
    const mutable_0 = CIR2.Patt.Idx.withMutability(node_idx_0, true);
    try testing.expect(mutable_0.isMutable());
    try testing.expectEqual(node_idx_0, mutable_0.toNodeIdx());

    // Test with large index
    const node_idx_large = AST.Node.Idx{ .idx = 1000000 };
    const mutable_large = CIR2.Patt.Idx.withMutability(node_idx_large, true);
    try testing.expect(mutable_large.isMutable());
    try testing.expectEqual(node_idx_large, mutable_large.toNodeIdx());

    // Test that mutability is preserved through multiple conversions
    const idx1 = CIR2.Patt.Idx.withMutability(node_idx_large, true);
    const node_idx_extracted = idx1.toNodeIdx();
    const idx2 = CIR2.Patt.Idx.withMutability(node_idx_extracted, idx1.isMutable());
    try testing.expectEqual(idx1, idx2);
}

test "sign bit encoding: mutable vs immutable pattern canonicalization" {
    const allocator = testing.allocator;

    var ast = try AST.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create CIR with shared AST storage
    var cir = CIR2.init(&ast);
    defer cir.deinit(allocator);

    // Create two identifier nodes
    const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };

    const node1_idx = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = ident_idx },
    });

    const node2_idx = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 10 },
        .payload = .{ .ident = ident_idx },
    });

    // Canonicalize one as immutable, one as mutable
    const immutable_patt = try cir.canonicalizePatt(allocator, &ast, node1_idx);
    const mutable_patt = try cir.canonicalizePattMutable(allocator, &ast, node2_idx);

    // Verify mutability encoding
    try testing.expect(!immutable_patt.isMutable());
    try testing.expect(mutable_patt.isMutable());

    // Verify the underlying nodes are correct
    try testing.expectEqual(node1_idx, immutable_patt.toNodeIdx());
    try testing.expectEqual(node2_idx, mutable_patt.toNodeIdx());

    // Verify getPatt returns correct mutability info
    const patt1_view = cir.getPatt(immutable_patt);
    const patt2_view = cir.getPatt(mutable_patt);

    try testing.expect(!patt1_view.is_mutable);
    try testing.expect(patt2_view.is_mutable);
}

test "sign bit encoding: nested scopes with mixed mutability" {
    const allocator = testing.allocator;

    var ast = try AST.initCapacity(allocator, 30);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var cir = CIR2.init(&ast);
    defer cir.deinit(allocator);

    var scope_state = CIR2.ScopeState{};
    defer scope_state.deinit(allocator);

    // Create identifiers for testing
    const x_ident = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const y_ident = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const z_ident = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 3 };

    // Push outer scope
    try scope_state.pushScope(allocator, false);

    // Add immutable x to outer scope
    const x_node = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = x_ident },
    });
    const x_patt = try cir.canonicalizePatt(allocator, &ast, x_node);
    try scope_state.addIdent(allocator, x_ident, x_patt);

    // Add mutable y to outer scope
    const y_node = try ast.nodes.append(allocator, .{
        .tag = .var_lc,
        .start = Position{ .offset = 10 },
        .payload = .{ .ident = y_ident },
    });
    const y_patt = try cir.canonicalizePattMutable(allocator, &ast, y_node);
    try scope_state.addIdent(allocator, y_ident, y_patt);
    try scope_state.recordVarPattern(allocator, y_patt);

    // Verify outer scope state
    try testing.expect(!scope_state.isVarPattern(x_patt));
    try testing.expect(scope_state.isVarPattern(y_patt));

    // Push inner scope
    try scope_state.pushScope(allocator, false);

    // Shadow x with mutable var in inner scope
    const x_inner_node = try ast.nodes.append(allocator, .{
        .tag = .var_lc,
        .start = Position{ .offset = 20 },
        .payload = .{ .ident = x_ident },
    });
    const x_inner_patt = try cir.canonicalizePattMutable(allocator, &ast, x_inner_node);
    try scope_state.addIdent(allocator, x_ident, x_inner_patt);
    try scope_state.recordVarPattern(allocator, x_inner_patt);

    // Add immutable z to inner scope
    const z_node = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 30 },
        .payload = .{ .ident = z_ident },
    });
    const z_patt = try cir.canonicalizePatt(allocator, &ast, z_node);
    try scope_state.addIdent(allocator, z_ident, z_patt);

    // Verify inner scope lookups
    const x_lookup = scope_state.lookupIdent(x_ident);
    const y_lookup = scope_state.lookupIdent(y_ident);
    const z_lookup = scope_state.lookupIdent(z_ident);

    try testing.expect(x_lookup != null);
    try testing.expect(y_lookup != null);
    try testing.expect(z_lookup != null);

    // x should be the mutable inner version
    try testing.expect(scope_state.isVarPattern(x_lookup.?));
    try testing.expectEqual(x_inner_patt, x_lookup.?);

    // y should still be mutable from outer scope
    try testing.expect(scope_state.isVarPattern(y_lookup.?));
    try testing.expectEqual(y_patt, y_lookup.?);

    // z should be immutable
    try testing.expect(!scope_state.isVarPattern(z_lookup.?));

    // Pop inner scope
    scope_state.popScope(allocator);

    // Verify outer scope is restored
    const x_outer_lookup = scope_state.lookupIdent(x_ident);
    try testing.expect(x_outer_lookup != null);
    try testing.expect(!scope_state.isVarPattern(x_outer_lookup.?)); // Back to immutable
    try testing.expectEqual(x_patt, x_outer_lookup.?);

    // z should no longer be in scope
    try testing.expect(scope_state.lookupIdent(z_ident) == null);
}

test "sign bit encoding: function boundaries and var reassignment" {
    const allocator = testing.allocator;

    var ast = try AST.initCapacity(allocator, 20);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var cir = CIR2.init(&ast);
    defer cir.deinit(allocator);

    var scope_state = CIR2.ScopeState{};
    defer scope_state.deinit(allocator);

    const x_ident = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const region1 = Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 100 } };
    const region2 = Region{ .start = Position{ .offset = 200 }, .end = Position{ .offset = 300 } };

    // Enter first function
    try scope_state.enterFunction(allocator, region1);
    try scope_state.pushScope(allocator, true); // Function boundary

    // Create mutable var in first function
    const x_node = try ast.nodes.append(allocator, .{
        .tag = .var_lc,
        .start = Position{ .offset = 10 },
        .payload = .{ .ident = x_ident },
    });
    const x_patt = try cir.canonicalizePattMutable(allocator, &ast, x_node);
    try scope_state.addIdent(allocator, x_ident, x_patt);
    try scope_state.recordVarPattern(allocator, x_patt);

    // Verify it's tracked as mutable with function region
    try testing.expect(scope_state.isVarPattern(x_patt));
    try testing.expect(!scope_state.isVarReassignmentAcrossFunctionBoundary(x_patt));

    // Enter nested function
    try scope_state.enterFunction(allocator, region2);
    try scope_state.pushScope(allocator, true); // Another function boundary

    // Try to reassign var from outer function - should detect boundary crossing
    try testing.expect(scope_state.isVarReassignmentAcrossFunctionBoundary(x_patt));

    // Exit nested function
    scope_state.popScope(allocator);
    scope_state.exitFunction();

    // Back in original function - should be OK to reassign
    try testing.expect(!scope_state.isVarReassignmentAcrossFunctionBoundary(x_patt));
}

test "sign bit encoding: var_lc pattern always mutable" {
    const allocator = testing.allocator;

    var ast = try AST.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var cir = CIR2.init(&ast);
    defer cir.deinit(allocator);

    // Create a var_lc node (explicit var pattern)
    const node_idx = try ast.nodes.append(allocator, .{
        .tag = .var_lc,
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = Ident.Idx{ .attributes = .{}, .idx = 1 } },
    });

    // Canonicalize with either method - var_lc should always be mutable
    const patt1 = try cir.canonicalizePatt(allocator, &ast, node_idx);
    try testing.expect(patt1.isMutable()); // var_lc is always mutable

    // The pattern view should also indicate mutability
    const patt_view = cir.getPatt(patt1);
    try testing.expect(patt_view.is_mutable);
    try testing.expect(patt_view.tag == .var_ident);
}

test "sign bit encoding: literals and underscore never mutable" {
    const allocator = testing.allocator;

    var ast = try AST.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var cir = CIR2.init(&ast);
    defer cir.deinit(allocator);

    // Create underscore pattern
    const underscore_node = try ast.nodes.append(allocator, .{
        .tag = .underscore,
        .start = Position{ .offset = 0 },
        .payload = .{ .src_bytes_end = Position{ .offset = 1 } },
    });

    // Create number literal pattern
    const num_node = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 10 },
        .payload = .{ .num_literal_i32 = 42 },
    });

    // Try to canonicalize as mutable (should still be immutable)
    const underscore_patt = try cir.canonicalizePattMutable(allocator, &ast, underscore_node);
    const num_patt = try cir.canonicalizePattMutable(allocator, &ast, num_node);

    // These patterns can never be mutable
    try testing.expect(!underscore_patt.isMutable());
    try testing.expect(!num_patt.isMutable());

    // Verify in pattern views
    const underscore_view = cir.getPatt(underscore_patt);
    const num_view = cir.getPatt(num_patt);

    try testing.expect(!underscore_view.is_mutable);
    try testing.expect(!num_view.is_mutable);
    try testing.expect(underscore_view.tag == .underscore);
    try testing.expect(num_view.tag == .num_literal_i32);
}

test "sign bit encoding: comprehensive statement canonicalization" {
    const allocator = testing.allocator;

    var ast = try AST.initCapacity(allocator, 50);
    defer ast.deinit(allocator);

    var node_slices = try NodeSlices(AST.Node.Idx).initCapacity(allocator, 20);
    defer node_slices.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Update AST to use the node_slices
    ast.node_slices = node_slices;

    var cir = CIR2.init(&ast);
    defer cir.deinit(allocator);

    var scope_state = CIR2.ScopeState{};
    defer scope_state.deinit(allocator);

    try scope_state.pushScope(allocator, false);

    // Test case 1: Immutable assignment
    const x_ident = Ident.Idx{ .attributes = .{}, .idx = 1 };
    const x_node = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = x_ident },
    });
    const num1_node = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 10 },
        .payload = .{ .num_literal_i32 = 10 },
    });

    const binop1_idx = try ast.node_slices.appendBinOp(allocator, .{
        .lhs = x_node,
        .rhs = num1_node,
    });

    const assign1_node = try ast.nodes.append(allocator, .{
        .tag = .binop_equals,
        .start = Position{ .offset = 0 },
        .payload = .{ .binop = binop1_idx },
    });

    const stmt1_idx = try cir.canonicalizeStmt(allocator, assign1_node, &scope_state);

    // Verify immutable assignment was created
    const stmt1 = cir.getStmt(stmt1_idx);
    try testing.expect(stmt1.tag == .assign);

    // Get the pattern from the statement
    const assign1_payload = stmt1.payload.assignment;
    try testing.expect(!assign1_payload.pattern_idx.isMutable());

    // Test case 2: Mutable var initialization
    const y_ident = Ident.Idx{ .attributes = .{}, .idx = 2 };
    const y_node = try ast.nodes.append(allocator, .{
        .tag = .var_lc,
        .start = Position{ .offset = 20 },
        .payload = .{ .ident = y_ident },
    });
    const num2_node = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 30 },
        .payload = .{ .num_literal_i32 = 20 },
    });

    const binop2_idx = try ast.node_slices.appendBinOp(allocator, .{
        .lhs = y_node,
        .rhs = num2_node,
    });

    const assign2_node = try ast.nodes.append(allocator, .{
        .tag = .binop_equals,
        .start = Position{ .offset = 20 },
        .payload = .{ .binop = binop2_idx },
    });

    const stmt2_idx = try cir.canonicalizeStmt(allocator, assign2_node, &scope_state);

    // Verify mutable var initialization was created
    const stmt2 = cir.getStmt(stmt2_idx);
    try testing.expect(stmt2.tag == .init_var);

    // Get the pattern from the statement and verify it's mutable
    const assign2_payload = stmt2.payload.assignment;
    try testing.expect(assign2_payload.pattern_idx.isMutable());

    // Test case 3: Reassignment of mutable var
    const y_reassign_node = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 40 },
        .payload = .{ .ident = y_ident },
    });
    const num3_node = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 50 },
        .payload = .{ .num_literal_i32 = 30 },
    });

    const binop3_idx = try ast.node_slices.appendBinOp(allocator, .{
        .lhs = y_reassign_node,
        .rhs = num3_node,
    });

    const assign3_node = try ast.nodes.append(allocator, .{
        .tag = .binop_equals,
        .start = Position{ .offset = 40 },
        .payload = .{ .binop = binop3_idx },
    });

    const stmt3_idx = try cir.canonicalizeStmt(allocator, assign3_node, &scope_state);

    // Verify reassignment was created
    const stmt3 = cir.getStmt(stmt3_idx);
    try testing.expect(stmt3.tag == .reassign);

    // The pattern being reassigned should still be mutable
    const assign3_payload = stmt3.payload.assignment;
    try testing.expect(assign3_payload.pattern_idx.isMutable());
}

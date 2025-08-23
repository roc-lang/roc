// Tests for sign bit encoding of pattern mutability
// These tests are designed to be added to CIR2.zig

test "sign bit encoding: basic mutability encoding" {
    const testing = std.testing;

    // Test that we can encode and decode mutability correctly
    const node_idx = @as(AST2.Node.Idx, @enumFromInt(42));

    // Create immutable pattern index
    const immutable_idx = Patt.Idx.withMutability(node_idx, false);
    try testing.expect(!immutable_idx.isMutable());
    try testing.expectEqual(node_idx, immutable_idx.toNodeIdx());

    // Create mutable pattern index
    const mutable_idx = Patt.Idx.withMutability(node_idx, true);
    try testing.expect(mutable_idx.isMutable());
    try testing.expectEqual(node_idx, mutable_idx.toNodeIdx());

    // Test setMutability
    const changed_idx = immutable_idx.setMutability(true);
    try testing.expect(changed_idx.isMutable());
    try testing.expectEqual(node_idx, changed_idx.toNodeIdx());
}

test "sign bit encoding: edge cases" {
    const testing = std.testing;

    // Test with index 0
    const node_idx_0 = @as(AST2.Node.Idx, @enumFromInt(0));
    const mutable_0 = Patt.Idx.withMutability(node_idx_0, true);
    try testing.expect(mutable_0.isMutable());
    try testing.expectEqual(node_idx_0, mutable_0.toNodeIdx());

    // Test with large index
    const node_idx_large = @as(AST2.Node.Idx, @enumFromInt(1000000));
    const mutable_large = Patt.Idx.withMutability(node_idx_large, true);
    try testing.expect(mutable_large.isMutable());
    try testing.expectEqual(node_idx_large, mutable_large.toNodeIdx());

    // Test that mutability is preserved through multiple conversions
    const idx1 = Patt.Idx.withMutability(node_idx_large, true);
    const node_idx_extracted = idx1.toNodeIdx();
    const idx2 = Patt.Idx.withMutability(node_idx_extracted, idx1.isMutable());
    try testing.expectEqual(idx1, idx2);
}

test "sign bit encoding: mutable vs immutable pattern canonicalization" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var ast = try AST2.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    // Create CIR with shared AST storage
    var cir = CIR.init(&ast);
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
    const testing = std.testing;
    const allocator = testing.allocator;

    var ast = try AST2.initCapacity(allocator, 30);
    defer ast.deinit(allocator);

    var cir = CIR.init(&ast);
    defer cir.deinit(allocator);

    var scope_state = ScopeState{};
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

test "sign bit encoding: var_lc always creates mutable pattern" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var ast = try AST2.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    var cir = CIR.init(&ast);
    defer cir.deinit(allocator);

    // Create a var_lc node (explicit var pattern)
    const node_idx = try ast.nodes.append(allocator, .{
        .tag = .var_lc,
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 } },
    });

    // Canonicalize - var_lc should always be mutable regardless of which method we use
    const patt1 = try cir.canonicalizePattWithMutability(allocator, &ast, node_idx, false);
    try testing.expect(patt1.isMutable()); // var_lc is always mutable even if we pass false

    // The pattern view should also indicate mutability
    const patt_view = cir.getPatt(patt1);
    try testing.expect(patt_view.is_mutable);
    try testing.expect(patt_view.tag == .var_ident);
}

test "sign bit encoding: function boundaries and var reassignment" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var ast = try AST2.initCapacity(allocator, 20);
    defer ast.deinit(allocator);

    var cir = CIR.init(&ast);
    defer cir.deinit(allocator);

    var scope_state = ScopeState{};
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

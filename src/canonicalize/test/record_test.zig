// test "record literal uses record_unbound" {
//     const gpa = std.testing.allocator;

//     // Test a simple record literal
//     {
//         const source1 = "{ x: 42, y: \"hello\" }";

//         var common_env = try base.CommonEnv.init(gpa, source1);
//         // Module env takes ownership of Common env -- no need to deinit here

//         var env = try ModuleEnv.init(gpa, &common_env);
//         defer env.deinit();

//         try env.initCIRFields(gpa, "test");

//         var ast = try parse.parseExpr(&env, gpa);
//         defer ast.deinit(gpa);

//         var can = try Self.init(&env, &ast, null);
//         defer can.deinit();

//         const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
//         const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
//             return error.CanonicalizeError;
//         };

//         // Get the type of the expression
//         const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx())));
//         const resolved = env.types.resolveVar(expr_var);

//         // Check that it's a record_unbound
//         switch (resolved.desc.content) {
//             .structure => |structure| switch (structure) {
//                 .record_unbound => |fields| {
//                     // Success! The record literal created a record_unbound type
//                     try testing.expect(fields.len() == 2);
//                 },
//                 else => return error.ExpectedRecordUnbound,
//             },
//             else => return error.ExpectedStructure,
//         }
//     }

//     // Test an empty record literal
//     {
//         const source2 = "{}";

//         var env = try ModuleEnv.init(gpa, source2);
//         defer env.deinit();

//         try env.initCIRFields(gpa, "test");

//         var ast = try parse.parseExpr(&env, gpa);
//         defer ast.deinit(gpa);

//         var can = try Self.init(&env, &ast, null);
//         defer can.deinit();

//         const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
//         const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
//             return error.CanonicalizeError;
//         };

//         // Get the type of the expression
//         const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx())));
//         const resolved = env.types.resolveVar(expr_var);

//         // Check that it's an empty_record
//         switch (resolved.desc.content) {
//             .structure => |structure| switch (structure) {
//                 .empty_record => {
//                     // Success! Empty record literal created empty_record type
//                 },
//                 else => return error.ExpectedEmptyRecord,
//             },
//             else => return error.ExpectedStructure,
//         }
//     }

//     // Test a record with a single field
//     // Test a nested record literal
//     {
//         const source3 = "{ value: 123 }";

//         var env = try ModuleEnv.init(gpa, source3);
//         defer env.deinit();

//         try env.initCIRFields(gpa, "test");

//         var ast = try parse.parseExpr(&env, gpa, gpa);
//         defer ast.deinit(gpa);

//         var can = try Self.init(&env, &ast, null);
//         defer can.deinit();

//         const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
//         const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
//             return error.CanonicalizeError;
//         };

//         // Get the type of the expression
//         const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx())));
//         const resolved = env.types.resolveVar(expr_var);

//         // Check that it's a record_unbound
//         switch (resolved.desc.content) {
//             .structure => |structure| switch (structure) {
//                 .record_unbound => |fields| {
//                     // Success! The record literal created a record_unbound type
//                     try testing.expect(fields.len() == 1);

//                     // Check the field
//                     const fields_slice = env.types.getRecordFieldsSlice(fields);
//                     const field_name = env.getIdent(fields_slice.get(0).name);
//                     try testing.expectEqualStrings("value", field_name);
//                 },
//                 else => return error.ExpectedRecordUnbound,
//             },
//             else => return error.ExpectedStructure,
//         }
//     }
// }

// test "record_unbound basic functionality" {
//     const gpa = std.testing.allocator;
//     const source = "{ x: 42, y: 99 }";

//     var common_env = try base.CommonEnv.init(gpa, source);
//     // Module env takes ownership of Common env -- no need to deinit here

//     // Test that record literals create record_unbound types
//     var env = try ModuleEnv.init(gpa, &common_env);
//     defer env.deinit();

//     try env.initCIRFields(gpa, "test");

//     var ast = try parse.parseExpr(&common_env, gpa);
//     defer ast.deinit(gpa);

//     var can = try Self.init(&env, &ast, null);
//     defer can.deinit();

//     const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
//     const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
//         return error.CanonicalizeError;
//     };

//     // Get the type of the expression
//     const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx())));
//     const resolved = env.types.resolveVar(expr_var);

//     // Verify it starts as record_unbound
//     switch (resolved.desc.content) {
//         .structure => |structure| switch (structure) {
//             .record_unbound => |fields| {
//                 // Success! Record literal created record_unbound type
//                 try testing.expect(fields.len() == 2);

//                 // Check field names
//                 const field_slice = env.types.getRecordFieldsSlice(fields);
//                 try testing.expectEqualStrings("x", env.getIdent(field_slice.get(0).name));
//                 try testing.expectEqualStrings("y", env.getIdent(field_slice.get(1).name));
//             },
//             else => return error.ExpectedRecordUnbound,
//         },
//         else => return error.ExpectedStructure,
//     }
// }

// test "record_unbound with multiple fields" {
//     const gpa = std.testing.allocator;
//     const source = "{ a: 123, b: 456, c: 789 }";

//     var common_env = try base.CommonEnv.init(gpa, source);
//     // Module env takes ownership of Common env -- no need to deinit here

//     var env = try ModuleEnv.init(gpa, &common_env);
//     defer env.deinit();

//     try env.initCIRFields(gpa, "test");

//     // Create record_unbound with multiple fields
//     var ast = try parse.parseExpr(&common_env, gpa);
//     defer ast.deinit(gpa);

//     var can = try Self.init(&env, &ast, null);
//     defer can.deinit();

//     const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
//     const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
//         return error.CanonicalizeError;
//     };

//     const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx.get_idx())));
//     const resolved = env.types.resolveVar(expr_var);

//     // Should be record_unbound
//     switch (resolved.desc.content) {
//         .structure => |s| switch (s) {
//             .record_unbound => |fields| {
//                 try testing.expect(fields.len() == 3);

//                 // Check field names
//                 const field_slice = env.types.getRecordFieldsSlice(fields);
//                 try testing.expectEqualStrings("a", env.getIdent(field_slice.get(0).name));
//                 try testing.expectEqualStrings("b", env.getIdent(field_slice.get(1).name));
//                 try testing.expectEqualStrings("c", env.getIdent(field_slice.get(2).name));
//             },
//             else => return error.ExpectedRecordUnbound,
//         },
//         else => return error.ExpectedStructure,
//     }
// }

// test "record with extension variable" {
//     const gpa = std.testing.allocator;

//     var common_env = try base.CommonEnv.init(gpa, "");
//     // Module env takes ownership of Common env -- no need to deinit here

//     var env = try ModuleEnv.init(gpa, &common_env);
//     defer env.deinit();

//     try env.initCIRFields(gpa, "test");

//     // Test that regular records have extension variables
//     // Create { x: 42, y: "hi" }* (open record)
//     const num_var = try env.types.freshFromContent(Content{ .structure = .{ .num = .{ .int_precision = .i32 } } });
//     const str_var = try env.types.freshFromContent(Content{ .structure = .str });

//     const fields = [_]types.RecordField{
//         .{ .name = try env.insertIdent( base.Ident.for_text("x")), .var_ = num_var },
//         .{ .name = try env.insertIdent( base.Ident.for_text("y")), .var_ = str_var },
//     };
//     const fields_range = try env.types.appendRecordFields(&fields);
//     const ext_var = try env.types.fresh(); // Open extension
//     const record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = ext_var } } };
//     const record_var = try env.types.freshFromContent(record_content);

//     // Verify the record has an extension variable
//     const resolved = env.types.resolveVar(record_var);
//     switch (resolved.desc.content) {
//         .structure => |structure| switch (structure) {
//             .record => |record| {
//                 try testing.expect(record.fields.len() == 2);

//                 // Check that extension is a flex var (open record)
//                 const ext_resolved = env.types.resolveVar(record.ext);
//                 switch (ext_resolved.desc.content) {
//                     .flex_var => {
//                         // Success! The record has an open extension
//                     },
//                     else => return error.ExpectedFlexVar,
//                 }
//             },
//             else => return error.ExpectedRecord,
//         },
//         else => return error.ExpectedStructure,
//     }

//     // Now test a closed record
//     const closed_ext_var = try env.types.freshFromContent(Content{ .structure = .empty_record });
//     const closed_record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = closed_ext_var } } };
//     const closed_record_var = try env.types.freshFromContent(closed_record_content);

//     // Verify the closed record has empty_record as extension
//     const closed_resolved = env.types.resolveVar(closed_record_var);
//     switch (closed_resolved.desc.content) {
//         .structure => |structure| switch (structure) {
//             .record => |record| {
//                 try testing.expect(record.fields.len() == 2);

//                 // Check that extension is empty_record (closed record)
//                 const ext_resolved = env.types.resolveVar(record.ext);
//                 switch (ext_resolved.desc.content) {
//                     .structure => |ext_structure| switch (ext_structure) {
//                         .empty_record => {
//                             // Success! The record is closed
//                         },
//                         else => return error.ExpectedEmptyRecord,
//                     },
//                     else => return error.ExpectedStructure,
//                 }
//             },
//             else => return error.ExpectedRecord,
//         },
//         else => return error.ExpectedStructure,
//     }
// }

const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const types = @import("types.zig");
const writers = @import("writers.zig");
const store_mod = @import("store.zig");

const ModuleEnv = base.ModuleEnv;
const Ident = base.Ident;
const Region = base.Region;
const TypeWriter = writers.TypeWriter;
const Store = store_mod.Store;
const Content = types.Content;
const Var = types.Var;

// TODO: Fix bus error in these tests and re-enable them
// test "type variable naming avoids existing identifiers" {
//     const allocator = testing.allocator;

//     // Create a module environment
//     var module_env = ModuleEnv.init(allocator, "TestModule");
//     defer module_env.deinit();

//     // Insert identifiers that should be avoided
//     const a_ident = module_env.idents.insert(allocator, Ident.for_text("a"), Region.zero());
//     const b_ident = module_env.idents.insert(allocator, Ident.for_text("b"), Region.zero());
//     const c_ident = module_env.idents.insert(allocator, Ident.for_text("c"), Region.zero());
//     _ = a_ident;
//     _ = b_ident;
//     _ = c_ident;

//     // Create some unnamed type variables
//     const var1 = module_env.types.fresh();
//     const var2 = module_env.types.fresh();
//     const var3 = module_env.types.fresh();

//     // Set them as unnamed flex vars
//     try module_env.types.setVarContent(var1, Content{ .flex_var = null });
//     try module_env.types.setVarContent(var2, Content{ .flex_var = null });
//     try module_env.types.setVarContent(var3, Content{ .flex_var = null });

//     // Create a function type: var1 -> var1 (identity function)
//     const func_content = module_env.types.mkFuncPure(&[_]Var{var1}, var1);
//     const func_var = module_env.types.freshFromContent(func_content);

//     // Write the type using TypeWriter
//     var type_writer = try TypeWriter.init(allocator, &module_env);
//     defer type_writer.deinit();

//     try type_writer.write(func_var);
//     const result = type_writer.get();

//     // The first unnamed variable should skip 'a', 'b', 'c' and use 'd'
//     // Since it's the identity function, it should be "d -> d"
//     try testing.expectEqualStrings("d -> d", result);

//     // Now test with another function using different variables
//     type_writer.buf.clearRetainingCapacity();
//     type_writer.next_name_index = 0;

//     // Create a function type: var2 -> var3
//     const func_content2 = module_env.types.mkFuncPure(&[_]Var{var2}, var3);
//     const func_var2 = module_env.types.freshFromContent(func_content2);

//     try type_writer.write(func_var2);
//     const result2 = type_writer.get();

//     // This should use 'e' and 'f' since 'a', 'b', 'c' are taken and 'd' was already generated
//     try testing.expectEqualStrings("e -> f", result2);
// }

// test "type variable naming wraps around after 'z'" {
//     const allocator = testing.allocator;

//     // Create a module environment
//     var module_env = ModuleEnv.init(allocator, "TestModule");
//     defer module_env.deinit();

//     // Insert identifiers from 'a' to 'z'
//     var i: u8 = 0;
//     while (i < 26) : (i += 1) {
//         var name = [_]u8{@as(u8, 'a' + i)};
//         _ = module_env.idents.insert(allocator, Ident.for_text(&name), Region.zero());
//     }

//     // Create an unnamed type variable
//     const var1 = module_env.types.fresh();
//     try module_env.types.setVarContent(var1, Content{ .flex_var = null });

//     // Write the type using TypeWriter
//     var type_writer = try TypeWriter.init(allocator, &module_env);
//     defer type_writer.deinit();

//     try type_writer.write(var1);
//     const result = type_writer.get();

//     // Should use 'aa' since 'a' through 'z' are taken
//     try testing.expectEqualStrings("aa", result);
// }

// test "type variable naming avoids multi-letter collisions" {
//     const allocator = testing.allocator;

//     // Create a module environment
//     var module_env = ModuleEnv.init(allocator, "TestModule");
//     defer module_env.deinit();

//     // Insert identifiers from 'a' to 'z' and 'aa', 'ab'
//     var i: u8 = 0;
//     while (i < 26) : (i += 1) {
//         var name = [_]u8{@as(u8, 'a' + i)};
//         _ = module_env.idents.insert(allocator, Ident.for_text(&name), Region.zero());
//     }

//     _ = module_env.idents.insert(allocator, Ident.for_text("aa"), Region.zero());
//     _ = module_env.idents.insert(allocator, Ident.for_text("ab"), Region.zero());

//     // Create two unnamed type variables
//     const var1 = module_env.types.fresh();
//     const var2 = module_env.types.fresh();
//     try module_env.types.setVarContent(var1, Content{ .flex_var = null });
//     try module_env.types.setVarContent(var2, Content{ .flex_var = null });

//     // Create a tuple type: (var1, var2)
//     const tuple_elems = module_env.types.appendTupleElems(&[_]Var{ var1, var2 });
//     const tuple_content = Content{ .structure = .{ .tuple = types.Tuple{ .elems = tuple_elems } } };
//     const tuple_var = module_env.types.freshFromContent(tuple_content);

//     // Write the type using TypeWriter
//     var type_writer = try TypeWriter.init(allocator, &module_env);
//     defer type_writer.deinit();

//     try type_writer.write(tuple_var);
//     const result = type_writer.get();

//     // Should use 'ac' and 'ad' since 'a'-'z', 'aa', 'ab' are taken
//     try testing.expectEqualStrings("(ac, ad)", result);
// }

//! Test to reproduce the bug where Try types are printed as Error
const std = @import("std");
const base = @import("base");
const can = @import("can");
const parse = @import("parse");
const types = @import("types");
const eval = @import("eval");
const Check = @import("check").Check;
const compile_package = @import("../compile_package.zig");
const BuiltinModules = eval.BuiltinModules;

const ModuleEnv = can.ModuleEnv;
const Ident = base.Ident;
const AST = parse.AST;

test "canonicalizeAndTypeCheckModule preserves Try types in type printing" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const source =
        \\app [main] { pf: platform "platform.roc" }
        \\
        \\# Map over Try type
        \\map_result : Try(a, e), (a -> b) -> Try(b, e)
        \\map_result = |result, transform| {
        \\    match result {
        \\        Ok(value) => Ok(transform(value))
        \\        Err(error) => Err(error)
        \\    }
        \\}
        \\
        \\main = |_| "done"
    ;

    // Create ModuleEnv
    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    // Parse
    var parse_ast = try parse.parse(&env.common, gpa);
    defer parse_ast.deinit(gpa);

    // Load builtin modules
    var builtin_modules = try BuiltinModules.init(gpa);
    defer builtin_modules.deinit();

    const builtin_env = builtin_modules.builtin_module.env;

    // Call canonicalizeAndTypeCheckModule (this is the function under test)
    // Create module_envs and keep it alive - the checker needs it during type checking
    var module_envs = std.AutoHashMap(base.Ident.Idx, can.Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    const imported_envs: []const *ModuleEnv = &[_]*ModuleEnv{builtin_env};
    var result = try compile_package.PackageEnv.canonicalizeAndTypeCheckModule(
        gpa,
        &env,
        &parse_ast,
        builtin_env,
        builtin_modules.builtin_indices,
        imported_envs,
        &module_envs,
    );
    defer result.deinit();

    // Now get the type of map_result and convert it to a string
    // Find the map_result definition and get its type var from the expression
    const defs_slice = env.store.sliceDefs(env.all_defs);
    var map_result_var: ?types.Var = null;
    for (defs_slice) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_idx = pattern.assign.ident;
            const ident_text = env.getIdent(ident_idx);
            if (std.mem.eql(u8, ident_text, "map_result")) {
                // Get the type variable from the definition's expression
                map_result_var = ModuleEnv.varFrom(def.expr);
                break;
            }
        }
    }

    try testing.expect(map_result_var != null);

    // Create a TypeWriter and write the type
    var type_writer = try env.initTypeWriter();
    defer type_writer.deinit();

    try type_writer.write(map_result_var.?);
    const type_str = type_writer.get();

    // Check that the type contains "Try" and not "Error"
    try testing.expect(std.mem.indexOf(u8, type_str, "Try") != null);
    try testing.expect(std.mem.indexOf(u8, type_str, "Error") == null);
}

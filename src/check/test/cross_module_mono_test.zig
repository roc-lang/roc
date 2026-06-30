//! Tests for cross-module static dispatch and type checking.
//!
//! These tests verify that:
//! 1. Static dispatch to methods defined in other modules works correctly
//! 2. Cross-module type resolution for static dispatch is accurate

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");

const CoreCtx = @import("can").CoreCtx;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Check = @import("../Check.zig");

const testing = std.testing;
const compiled_builtins = @import("compiled_builtins");
const builtin_static = can.BuiltinStatic;

fn findTypeDeclByName(env: *const ModuleEnv, name: base.Ident.Idx) ?CIR.Statement.Idx {
    if (findTypeDeclByNameInSpan(env, env.type_decls, name)) |stmt_idx| return stmt_idx;
    if (findTypeDeclByNameInSpan(env, env.forward_type_decls, name)) |stmt_idx| return stmt_idx;
    if (findTypeDeclByNameInSpan(env, env.all_statements, name)) |stmt_idx| return stmt_idx;
    if (findTypeDeclByNameInSpan(env, env.builtin_statements, name)) |stmt_idx| return stmt_idx;
    return null;
}

fn findTypeDeclByNameInSpan(env: *const ModuleEnv, span: CIR.Statement.Span, name: base.Ident.Idx) ?CIR.Statement.Idx {
    for (0..span.span.len) |offset| {
        const stmt_idx = env.store.statementAt(span, offset);
        const header_idx = switch (env.store.getStatement(stmt_idx)) {
            .s_nominal_decl => |nominal| nominal.header,
            .s_alias_decl => |alias| alias.header,
            else => continue,
        };
        if (env.store.getTypeHeader(header_idx).name.eql(name)) return stmt_idx;
    }
    return null;
}

/// Test environment for cross-module monomorphization testing
const MonoTestEnv = struct {
    gpa: std.mem.Allocator,
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can_instance: *Can,
    checker: Check,
    module_envs: std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),
    builtin_module: builtin_static.BuiltinModuleView,
    owns_builtin_module: bool,
    imported_envs_list: std.ArrayList(*const ModuleEnv),

    const Self = @This();

    /// Initialize a single module test environment
    pub fn init(module_name: []const u8, source: []const u8) (Allocator.Error || error{CorruptEmbeddedBuiltins})!Self {
        const gpa = testing.allocator;
        const roc_ctx = CoreCtx.testing(gpa, gpa);

        const module_env = try gpa.create(ModuleEnv);
        errdefer gpa.destroy(module_env);

        const can_instance = try gpa.create(Can);
        errdefer gpa.destroy(can_instance);

        var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);

        const builtin_indices = compiled_builtins.builtinIndices(CIR);
        var builtin_module = try builtin_static.moduleView(gpa, compiled_builtins.builtin_bin[0..], "Builtin", compiled_builtins.builtin_source);
        errdefer builtin_module.deinit();

        module_env.* = try ModuleEnv.init(gpa, source);
        errdefer module_env.deinit();

        module_env.common.source = source;
        module_env.module_name = module_name;
        module_env.display_module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
        module_env.qualified_module_ident = module_env.display_module_name_idx;
        try module_env.common.calcLineStarts(gpa);

        const parse_ast = try parse.file(gpa, &module_env.common);
        errdefer parse_ast.deinit();
        parse_ast.store.emptyScratch();

        try module_env.initCIRFields(module_name);

        can_instance.* = try Can.initModule(roc_ctx, module_env, parse_ast, .{
            .builtin_types = .{
                .builtin_module_env = builtin_module.env,
                .builtin_indices = builtin_indices,
            },
            .imported_modules = &module_envs,
        });
        errdefer can_instance.deinit();

        try can_instance.canonicalizeFile();
        try can_instance.validateForChecking();

        const module_builtin_ctx: Check.BuiltinContext = .{
            .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
            .bool_stmt = builtin_indices.bool_type,
            .try_stmt = builtin_indices.try_type,
            .str_stmt = builtin_indices.str_type,
            .builtin_module = builtin_module.env,
            .builtin_indices = builtin_indices,
        };

        var imported_envs_list = std.ArrayList(*const ModuleEnv).empty;
        try imported_envs_list.append(gpa, builtin_module.env);

        module_env.imports.clearResolvedModules();
        try module_env.imports.resolveImportsByExactModuleName(module_env, imported_envs_list.items);

        var checker = try Check.init(
            gpa,
            &module_env.types,
            module_env,
            imported_envs_list.items,
            &module_envs,
            &module_env.store.regions,
            module_builtin_ctx,
        );
        checker.fixupTypeWriter();
        errdefer checker.deinit();

        try checker.checkFile();

        return Self{
            .gpa = gpa,
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can_instance = can_instance,
            .checker = checker,
            .module_envs = module_envs,
            .builtin_module = builtin_module,
            .owns_builtin_module = true,
            .imported_envs_list = imported_envs_list,
        };
    }

    /// Initialize with an imported module
    pub fn initWithImport(module_name: []const u8, source: []const u8, other_module_name: []const u8, other_env: *const Self) Allocator.Error!Self {
        const gpa = testing.allocator;
        const roc_ctx = CoreCtx.testing(gpa, gpa);

        const module_env = try gpa.create(ModuleEnv);
        errdefer gpa.destroy(module_env);

        const can_instance = try gpa.create(Can);
        errdefer gpa.destroy(can_instance);

        var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);

        const builtin_indices = compiled_builtins.builtinIndices(CIR);
        const builtin_env = other_env.builtin_module.env;

        module_env.* = try ModuleEnv.init(gpa, source);
        errdefer module_env.deinit();

        module_env.common.source = source;
        module_env.module_name = module_name;
        module_env.display_module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
        module_env.qualified_module_ident = module_env.display_module_name_idx;
        try module_env.common.calcLineStarts(gpa);

        const other_module_ident = try module_env.insertIdent(base.Ident.for_text(other_module_name));

        const statement_idx = blk: {
            if (other_env.module_env.module_kind == .type_module) {
                const type_ident = other_env.module_env.common.findIdent(other_module_name);
                if (type_ident) |ident| {
                    if (other_env.module_env.getExposedTypeNodeIndexById(ident)) |node_idx| {
                        break :blk @as(CIR.Statement.Idx, @enumFromInt(node_idx));
                    }
                }
            }
            break :blk null;
        };

        const other_qualified_ident = try module_env.insertIdent(base.Ident.for_text(other_module_name));
        try module_envs.put(other_module_ident, .{
            .env = other_env.module_env,
            .statement_idx = statement_idx,
            .qualified_type_ident = other_qualified_ident,
        });

        const parse_ast = try parse.file(gpa, &module_env.common);
        errdefer parse_ast.deinit();
        parse_ast.store.emptyScratch();

        try module_env.initCIRFields(module_name);

        can_instance.* = try Can.initModule(roc_ctx, module_env, parse_ast, .{
            .builtin_types = .{
                .builtin_module_env = builtin_env,
                .builtin_indices = builtin_indices,
            },
            .imported_modules = &module_envs,
        });
        errdefer can_instance.deinit();

        try can_instance.canonicalizeFile();
        try can_instance.validateForChecking();

        const module_builtin_ctx: Check.BuiltinContext = .{
            .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
            .bool_stmt = builtin_indices.bool_type,
            .try_stmt = builtin_indices.try_type,
            .str_stmt = builtin_indices.str_type,
            .builtin_module = other_env.builtin_module.env,
            .builtin_indices = builtin_indices,
        };

        var imported_envs_list = std.ArrayList(*const ModuleEnv).empty;
        try imported_envs_list.append(gpa, other_env.builtin_module.env);

        const import_count = module_env.imports.imports.items.items.len;
        for (module_env.imports.imports.items.items[0..import_count]) |str_idx| {
            const import_name = module_env.getString(str_idx);
            if (std.mem.eql(u8, import_name, other_module_name)) {
                try imported_envs_list.append(gpa, other_env.module_env);
            }
        }

        module_env.imports.clearResolvedModules();
        try module_env.imports.resolveImportsByExactModuleName(module_env, imported_envs_list.items);

        var checker = try Check.init(
            gpa,
            &module_env.types,
            module_env,
            imported_envs_list.items,
            &module_envs,
            &module_env.store.regions,
            module_builtin_ctx,
        );
        checker.fixupTypeWriter();
        errdefer checker.deinit();

        try checker.checkFile();

        return Self{
            .gpa = gpa,
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can_instance = can_instance,
            .checker = checker,
            .module_envs = module_envs,
            .builtin_module = other_env.builtin_module,
            .owns_builtin_module = false,
            .imported_envs_list = imported_envs_list,
        };
    }

    const ImportedModule = struct { name: []const u8, env: *const MonoTestEnv };

    /// Initialize with multiple imported modules
    pub fn initWithImports(module_name: []const u8, source: []const u8, imports: []const ImportedModule) Allocator.Error!Self {
        const gpa = testing.allocator;
        const roc_ctx = CoreCtx.testing(gpa, gpa);

        const module_env = try gpa.create(ModuleEnv);
        errdefer gpa.destroy(module_env);

        const can_instance = try gpa.create(Can);
        errdefer gpa.destroy(can_instance);

        var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);

        const builtin_indices = compiled_builtins.builtinIndices(CIR);
        const builtin_env = imports[0].env.builtin_module.env;

        module_env.* = try ModuleEnv.init(gpa, source);
        errdefer module_env.deinit();

        module_env.common.source = source;
        module_env.module_name = module_name;
        module_env.display_module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
        module_env.qualified_module_ident = module_env.display_module_name_idx;
        try module_env.common.calcLineStarts(gpa);

        for (imports) |imp| {
            const other_module_ident = try module_env.insertIdent(base.Ident.for_text(imp.name));

            const statement_idx = blk: {
                if (imp.env.module_env.module_kind == .type_module) {
                    const type_ident = imp.env.module_env.common.findIdent(imp.name);
                    if (type_ident) |ident| {
                        if (imp.env.module_env.getExposedTypeNodeIndexById(ident)) |node_idx| {
                            break :blk @as(CIR.Statement.Idx, @enumFromInt(node_idx));
                        }
                    }
                }
                break :blk null;
            };

            const other_qualified_ident = try module_env.insertIdent(base.Ident.for_text(imp.name));
            try module_envs.put(other_module_ident, .{
                .env = imp.env.module_env,
                .statement_idx = statement_idx,
                .qualified_type_ident = other_qualified_ident,
            });
        }

        const parse_ast = try parse.file(gpa, &module_env.common);
        errdefer parse_ast.deinit();
        parse_ast.store.emptyScratch();

        try module_env.initCIRFields(module_name);

        can_instance.* = try Can.initModule(roc_ctx, module_env, parse_ast, .{
            .builtin_types = .{
                .builtin_module_env = builtin_env,
                .builtin_indices = builtin_indices,
            },
            .imported_modules = &module_envs,
        });
        errdefer can_instance.deinit();

        try can_instance.canonicalizeFile();
        try can_instance.validateForChecking();

        const module_builtin_ctx: Check.BuiltinContext = .{
            .module_name = try module_env.insertIdent(base.Ident.for_text(module_name)),
            .bool_stmt = builtin_indices.bool_type,
            .try_stmt = builtin_indices.try_type,
            .str_stmt = builtin_indices.str_type,
            .builtin_module = builtin_env,
            .builtin_indices = builtin_indices,
        };

        var imported_envs_list = std.ArrayList(*const ModuleEnv).empty;
        try imported_envs_list.append(gpa, builtin_env);

        const import_count = module_env.imports.imports.items.items.len;
        for (module_env.imports.imports.items.items[0..import_count]) |str_idx| {
            const import_name = module_env.getString(str_idx);
            for (imports) |imp| {
                if (std.mem.eql(u8, import_name, imp.name)) {
                    try imported_envs_list.append(gpa, imp.env.module_env);
                }
            }
        }

        module_env.imports.clearResolvedModules();
        try module_env.imports.resolveImportsByExactModuleName(module_env, imported_envs_list.items);

        var checker = try Check.init(
            gpa,
            &module_env.types,
            module_env,
            imported_envs_list.items,
            &module_envs,
            &module_env.store.regions,
            module_builtin_ctx,
        );
        checker.fixupTypeWriter();
        errdefer checker.deinit();

        try checker.checkFile();

        return Self{
            .gpa = gpa,
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can_instance = can_instance,
            .checker = checker,
            .module_envs = module_envs,
            .builtin_module = imports[0].env.builtin_module,
            .owns_builtin_module = false,
            .imported_envs_list = imported_envs_list,
        };
    }

    pub fn deinit(self: *Self) void {
        self.imported_envs_list.deinit(self.gpa);
        self.checker.deinit();
        self.module_envs.deinit();
        self.can_instance.deinit();
        self.gpa.destroy(self.can_instance);
        self.parse_ast.deinit();
        self.module_env.deinit();
        self.gpa.destroy(self.module_env);
        if (self.owns_builtin_module) {
            self.builtin_module.deinit();
        }
    }
};

test "cross-module mono: static dispatch lookup finds method in imported module" {
    // Module A defines a type with a method
    const source_a =
        \\A := [A(U64)].{
        \\  get_value : A -> U64
        \\  get_value = |A.A(val)| val
        \\}
    ;
    var env_a = try MonoTestEnv.init("A", source_a);
    defer env_a.deinit();

    // Verify module A has the method registered
    const a_type_ident = env_a.module_env.common.findIdent("A");
    try testing.expect(a_type_ident != null);

    const get_value_ident = env_a.module_env.common.findIdent("get_value");
    try testing.expect(get_value_ident != null);

    // Check that the method is registered under the source declaration owner.
    const a_owner = findTypeDeclByName(env_a.module_env, a_type_ident.?) orelse return error.MissingATypeOwner;
    const method_lookup = env_a.module_env.lookupMethodIdentForOwner(a_owner, get_value_ident.?);
    try testing.expect(method_lookup != null);
}

test "cross-module mono: importing module can reference methods from imported type" {
    // Module A defines a type with a method
    const source_a =
        \\A := [A(U64)].{
        \\  get_value : A -> U64
        \\  get_value = |A.A(val)| val
        \\}
    ;
    var env_a = try MonoTestEnv.init("A", source_a);
    defer env_a.deinit();

    // Module B imports A and uses the method
    const source_b =
        \\import A
        \\
        \\main : U64
        \\main = {
        \\    val = A.A(42)
        \\    val.get_value()
        \\}
    ;
    var env_b = try MonoTestEnv.initWithImport("B", source_b, "A", &env_a);
    defer env_b.deinit();

    // Module B should have parsed and type-checked successfully with the import
    const a_ident_in_b = env_b.module_env.common.findIdent("A");
    try testing.expect(a_ident_in_b != null);
}

test "cross-module mono: static dispatch method registration in type module" {
    // This tests that when a type module defines methods, they are properly
    // registered so that other modules can look them up for static dispatch

    const source_a =
        \\Counter := [Counter(U64)].{
        \\  new : U64 -> Counter
        \\  new = |n| Counter.Counter(n)
        \\
        \\  increment : Counter -> Counter
        \\  increment = |Counter.Counter(n)| Counter.Counter(n + 1)
        \\
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
    ;
    var env_a = try MonoTestEnv.init("Counter", source_a);
    defer env_a.deinit();

    // Verify methods are registered
    const counter_ident = env_a.module_env.common.findIdent("Counter");
    try testing.expect(counter_ident != null);

    const new_ident = env_a.module_env.common.findIdent("new");
    const increment_ident = env_a.module_env.common.findIdent("increment");
    const get_ident = env_a.module_env.common.findIdent("get");

    try testing.expect(new_ident != null);
    try testing.expect(increment_ident != null);
    try testing.expect(get_ident != null);

    // Check method lookups work under the source declaration owner.
    const counter_owner = findTypeDeclByName(env_a.module_env, counter_ident.?) orelse return error.MissingCounterTypeOwner;
    try testing.expect(env_a.module_env.lookupMethodIdentForOwner(counter_owner, new_ident.?) != null);
    try testing.expect(env_a.module_env.lookupMethodIdentForOwner(counter_owner, increment_ident.?) != null);
    try testing.expect(env_a.module_env.lookupMethodIdentForOwner(counter_owner, get_ident.?) != null);
}

test "cross-module mono: static dispatch with chained method calls" {
    const source_a =
        \\Counter := [Counter(U64)].{
        \\  new : U64 -> Counter
        \\  new = |n| Counter.Counter(n)
        \\
        \\  increment : Counter -> Counter
        \\  increment = |Counter.Counter(n)| Counter.Counter(n + 1)
        \\
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
    ;
    var env_a = try MonoTestEnv.init("Counter", source_a);
    defer env_a.deinit();

    // Module B chains method calls
    const source_b =
        \\import Counter
        \\
        \\main : U64
        \\main = Counter.new(0).increment().increment().get()
    ;
    var env_b = try MonoTestEnv.initWithImport("B", source_b, "Counter", &env_a);
    defer env_b.deinit();

    // Module B should have parsed and type-checked successfully
    // This means the cross-module method resolution worked
    const main_ident = env_b.module_env.common.findIdent("main");
    try testing.expect(main_ident != null);
}

test "cross-module mono: recursive nominal type with self-referencing children" {
    // Module A defines a recursive nominal type where children reference
    // the type itself (Elem contains List(Elem)). This pattern is the key
    // scenario where cross-module nominal remapping does
    // meaningful work: the TypeId for List(Elem) may contain a `.rec`
    // placeholder indirection internally, which must be resolved when used
    // from a different module for TypeId comparisons to succeed.
    const source_a =
        \\Elem := [Div(List(Elem)), Text(Str)].{
        \\  div : List(Elem) -> Elem
        \\  div = |children| Div(children)
        \\
        \\  text : Str -> Elem
        \\  text = |content| Text(content)
        \\}
    ;
    var env_a = try MonoTestEnv.init("Elem", source_a);
    defer env_a.deinit();

    // Module B imports Elem and uses both constructors
    const source_b =
        \\import Elem
        \\
        \\main : Elem
        \\main = Elem.div([Elem.text("hello")])
    ;
    var env_b = try MonoTestEnv.initWithImport("B", source_b, "Elem", &env_a);
    defer env_b.deinit();

    // Type-check should succeed — the recursive nominal type is usable cross-module
    const main_ident = env_b.module_env.common.findIdent("main");
    try testing.expect(main_ident != null);
}

test "cross-module mono: recursive nominal through 3-module chain" {
    // Recursive nominal type used transitively: A defines the type,
    // B wraps it, C uses B's wrapper. This exercises cross-module
    // TypeId canonicalization across multiple module boundaries.
    const source_a =
        \\Tree := [Leaf(U64), Branch(List(Tree))].{
        \\  leaf : U64 -> Tree
        \\  leaf = |n| Leaf(n)
        \\
        \\  branch : List(Tree) -> Tree
        \\  branch = |children| Branch(children)
        \\}
    ;
    var env_a = try MonoTestEnv.init("Tree", source_a);
    defer env_a.deinit();

    const source_b =
        \\import Tree
        \\
        \\make_pair : U64, U64 -> Tree
        \\make_pair = |a, b| Tree.branch([Tree.leaf(a), Tree.leaf(b)])
    ;
    var env_b = try MonoTestEnv.initWithImport("B", source_b, "Tree", &env_a);
    defer env_b.deinit();

    const source_c =
        \\import B
        \\import Tree
        \\
        \\main : Tree
        \\main = B.make_pair(1, 2)
    ;
    var env_c = try MonoTestEnv.initWithImports("C", source_c, &.{
        .{ .name = "B", .env = &env_b },
        .{ .name = "Tree", .env = &env_a },
    });
    defer env_c.deinit();

    const main_ident = env_c.module_env.common.findIdent("main");
    try testing.expect(main_ident != null);
}

test "type checker catches polymorphic recursion (infinite type)" {
    // This test verifies that polymorphic recursion (f = |x| f([x])) is caught
    // during type checking as a circular/infinite type.
    //
    // The pattern `f = |x| f([x])` would require:
    //   f : a -> b
    //   But the recursive call passes [x] (List a), so we'd need:
    //   f : List a -> b
    //   This means a = List a, which is an infinite type.
    //
    // With the proper occurs check in unification, this is caught during type checking,
    // not during monomorphization.

    const source =
        \\f = |x| f([x])
    ;

    // Initialize test environment
    const gpa = testing.allocator;

    const roc_ctx = CoreCtx.testing(gpa, gpa);

    const module_env = try gpa.create(ModuleEnv);
    defer gpa.destroy(module_env);

    const can_instance = try gpa.create(Can);
    defer gpa.destroy(can_instance);

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    const builtin_indices = compiled_builtins.builtinIndices(CIR);
    var builtin_module = try builtin_static.moduleView(gpa, compiled_builtins.builtin_bin[0..], "Builtin", compiled_builtins.builtin_source);
    defer builtin_module.deinit();

    module_env.* = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = "Test";
    module_env.display_module_name_idx = try module_env.insertIdent(base.Ident.for_text("Test"));
    module_env.qualified_module_ident = module_env.display_module_name_idx;
    try module_env.common.calcLineStarts(gpa);

    const parse_ast = try parse.file(gpa, &module_env.common);
    defer parse_ast.deinit();
    parse_ast.store.emptyScratch();

    try module_env.initCIRFields("Test");

    can_instance.* = try Can.initModule(roc_ctx, module_env, parse_ast, .{
        .builtin_types = .{
            .builtin_module_env = builtin_module.env,
            .builtin_indices = builtin_indices,
        },
        .imported_modules = &module_envs,
    });
    defer can_instance.deinit();

    try can_instance.canonicalizeFile();
    try can_instance.validateForChecking();

    const module_builtin_ctx: Check.BuiltinContext = .{
        .module_name = try module_env.insertIdent(base.Ident.for_text("Test")),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    var imported_envs_list = std.ArrayList(*const ModuleEnv).empty;
    defer imported_envs_list.deinit(gpa);
    try imported_envs_list.append(gpa, builtin_module.env);

    module_env.imports.clearResolvedModules();
    try module_env.imports.resolveImportsByExactModuleName(module_env, imported_envs_list.items);

    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs_list.items,
        &module_envs,
        &module_env.store.regions,
        module_builtin_ctx,
    );
    checker.fixupTypeWriter();
    defer checker.deinit();

    try checker.checkFile();

    // The key assertion: type checking should catch the infinite type error.
    // This proves we don't need the polymorphic recursion detection in the monomorphizer.
    try testing.expect(checker.problems.len() > 0);
}

test "cross-module mono: 3-module linear chain (A -> B -> C)" {
    // Module A: type module with Counter type + new, get methods
    const source_a =
        \\Counter := [Counter(U64)].{
        \\  new : U64 -> Counter
        \\  new = |n| Counter.Counter(n)
        \\
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
    ;
    var env_a = try MonoTestEnv.init("Counter", source_a);
    defer env_a.deinit();

    // Module B: imports A, defines create_default helper
    const source_b =
        \\import Counter
        \\
        \\create_default : Counter
        \\create_default = Counter.new(0)
    ;
    var env_b = try MonoTestEnv.initWithImport("B", source_b, "Counter", &env_a);
    defer env_b.deinit();

    // Module C: imports B, calls B.create_default() then chains .get()
    const source_c =
        \\import B
        \\import Counter
        \\
        \\main : U64
        \\main = B.create_default.get()
    ;
    var env_c = try MonoTestEnv.initWithImports("C", source_c, &.{
        .{ .name = "B", .env = &env_b },
        .{ .name = "Counter", .env = &env_a },
    });
    defer env_c.deinit();

    // C should type-check successfully — transitive type visibility works
    const main_ident = env_c.module_env.common.findIdent("main");
    try testing.expect(main_ident != null);
}

test "cross-module mono: 3-module diamond (A <- B, A <- C, B+C <- D)" {
    // Module A: type module with Value type
    const source_a =
        \\Value := [Value(U64)].{
        \\  new : U64 -> Value
        \\  new = |n| Value.Value(n)
        \\
        \\  get : Value -> U64
        \\  get = |Value.Value(n)| n
        \\}
    ;
    var env_a = try MonoTestEnv.init("Value", source_a);
    defer env_a.deinit();

    // Module B: imports A, defines double
    const source_b =
        \\import Value
        \\
        \\double : Value -> Value
        \\double = |v| Value.new(Value.get(v) * 2)
    ;
    var env_b = try MonoTestEnv.initWithImport("B", source_b, "Value", &env_a);
    defer env_b.deinit();

    // Module C: imports A, defines add_one
    const source_c =
        \\import Value
        \\
        \\add_one : Value -> Value
        \\add_one = |v| Value.new(Value.get(v) + 1)
    ;
    var env_c = try MonoTestEnv.initWithImport("C", source_c, "Value", &env_a);
    defer env_c.deinit();

    // Module D: imports B and C, uses both
    const source_d =
        \\import B
        \\import C
        \\import Value
        \\
        \\main : U64
        \\main = Value.new(5) |> B.double |> C.add_one |> Value.get
    ;
    var env_d = try MonoTestEnv.initWithImports("D", source_d, &.{
        .{ .name = "B", .env = &env_b },
        .{ .name = "C", .env = &env_c },
        .{ .name = "Value", .env = &env_a },
    });
    defer env_d.deinit();

    // D should type-check successfully — diamond dependency works
    const main_ident = env_d.module_env.common.findIdent("main");
    try testing.expect(main_ident != null);
}

test "cross-module mono: 4-module chain with wrapper types (A -> B -> C -> D)" {
    // Module A: type module Base with new method
    const source_a =
        \\Base := [Base(U64)].{
        \\  new : U64 -> Base
        \\  new = |n| Base.Base(n)
        \\
        \\  get : Base -> U64
        \\  get = |Base.Base(n)| n
        \\}
    ;
    var env_a = try MonoTestEnv.init("Base", source_a);
    defer env_a.deinit();

    // Module B: imports A, defines wrap that takes Base and returns a record
    const source_b =
        \\import Base
        \\
        \\wrap : Base, U64 -> { base : Base, label : U64 }
        \\wrap = |b, l| { base: b, label: l }
    ;
    var env_b = try MonoTestEnv.initWithImport("B", source_b, "Base", &env_a);
    defer env_b.deinit();

    // Module C: imports B and Base, defines process
    const source_c =
        \\import B
        \\import Base
        \\
        \\process : { base : Base, label : U64 } -> U64
        \\process = |r| Base.get(r.base) + r.label
    ;
    var env_c = try MonoTestEnv.initWithImports("C", source_c, &.{
        .{ .name = "B", .env = &env_b },
        .{ .name = "Base", .env = &env_a },
    });
    defer env_c.deinit();

    // Module D: imports C, B, and Base — calls the full chain
    const source_d =
        \\import C
        \\import B
        \\import Base
        \\
        \\main : U64
        \\main = C.process(B.wrap(Base.new(10), 5))
    ;
    var env_d = try MonoTestEnv.initWithImports("D", source_d, &.{
        .{ .name = "C", .env = &env_c },
        .{ .name = "B", .env = &env_b },
        .{ .name = "Base", .env = &env_a },
    });
    defer env_d.deinit();

    // D should type-check successfully — deep transitive chain works
    const main_ident = env_d.module_env.common.findIdent("main");
    try testing.expect(main_ident != null);
}

test "cross-module mono: 5-module fan-in (A, B, C independent; D combines; E uses D)" {
    // Module A: type module Width
    const source_a =
        \\Width := [Width(U64)].{
        \\  new : U64 -> Width
        \\  new = |n| Width.Width(n)
        \\
        \\  get : Width -> U64
        \\  get = |Width.Width(n)| n
        \\}
    ;
    var env_a = try MonoTestEnv.init("Width", source_a);
    defer env_a.deinit();

    // Module B: type module Height
    const source_b =
        \\Height := [Height(U64)].{
        \\  new : U64 -> Height
        \\  new = |n| Height.Height(n)
        \\
        \\  get : Height -> U64
        \\  get = |Height.Height(n)| n
        \\}
    ;
    var env_b = try MonoTestEnv.init("Height", source_b);
    defer env_b.deinit();

    // Module C: type module Depth
    const source_c =
        \\Depth := [Depth(U64)].{
        \\  new : U64 -> Depth
        \\  new = |n| Depth.Depth(n)
        \\
        \\  get : Depth -> U64
        \\  get = |Depth.Depth(n)| n
        \\}
    ;
    var env_c = try MonoTestEnv.init("Depth", source_c);
    defer env_c.deinit();

    // Module D: imports A, B, C — defines volume
    const source_d =
        \\import Width
        \\import Height
        \\import Depth
        \\
        \\volume : Width, Height, Depth -> U64
        \\volume = |w, h, d| Width.get(w) * Height.get(h) * Depth.get(d)
    ;
    var env_d = try MonoTestEnv.initWithImports("D", source_d, &.{
        .{ .name = "Width", .env = &env_a },
        .{ .name = "Height", .env = &env_b },
        .{ .name = "Depth", .env = &env_c },
    });
    defer env_d.deinit();

    // Module E: imports A, B, C, D — creates all three types and calls D.volume
    const source_e =
        \\import Width
        \\import Height
        \\import Depth
        \\import D
        \\
        \\main : U64
        \\main = D.volume(Width.new(3), Height.new(4), Depth.new(5))
    ;
    var env_e = try MonoTestEnv.initWithImports("E", source_e, &.{
        .{ .name = "Width", .env = &env_a },
        .{ .name = "Height", .env = &env_b },
        .{ .name = "Depth", .env = &env_c },
        .{ .name = "D", .env = &env_d },
    });
    defer env_e.deinit();

    // E should type-check successfully — wide fan-in works
    const main_ident = env_e.module_env.common.findIdent("main");
    try testing.expect(main_ident != null);
}

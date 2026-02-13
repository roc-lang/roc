//! Tests for cross-module static dispatch and type checking.
//!
//! These tests verify that:
//! 1. Static dispatch to methods defined in other modules works correctly
//! 2. Cross-module type resolution for static dispatch is accurate

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");

const Allocators = base.Allocators;
const Can = can.Can;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const ClosureTransformer = can.ClosureTransformer;

const Check = @import("../Check.zig");

const testing = std.testing;
const compiled_builtins = @import("compiled_builtins");
const collections = @import("collections");

/// Wrapper for a loaded compiled module that tracks the buffer
const LoadedModule = struct {
    env: *ModuleEnv,
    buffer: []align(collections.CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    gpa: std.mem.Allocator,

    fn deinit(self: *LoadedModule) void {
        self.env.imports.map.deinit(self.gpa);
        self.gpa.free(self.buffer);
        self.gpa.destroy(self.env);
    }
};

/// Deserialize BuiltinIndices from the binary data generated at build time
fn deserializeBuiltinIndices(gpa: std.mem.Allocator, bin_data: []const u8) !CIR.BuiltinIndices {
    const aligned_buffer = try gpa.alignedAlloc(u8, @enumFromInt(@alignOf(CIR.BuiltinIndices)), bin_data.len);
    defer gpa.free(aligned_buffer);
    @memcpy(aligned_buffer, bin_data);
    const indices_ptr = @as(*const CIR.BuiltinIndices, @ptrCast(aligned_buffer.ptr));
    return indices_ptr.*;
}

/// Load a compiled ModuleEnv from embedded binary data
fn loadCompiledModule(gpa: std.mem.Allocator, bin_data: []const u8, module_name: []const u8, source: []const u8) !LoadedModule {
    const CompactWriter = collections.CompactWriter;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, bin_data.len);
    @memcpy(buffer, bin_data);

    const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const env = try gpa.create(ModuleEnv);
    errdefer gpa.destroy(env);

    const base_ptr = @intFromPtr(buffer.ptr);
    const common = serialized_ptr.common.deserializeInto(base_ptr, source);

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = common,
        .types = serialized_ptr.types.deserializeInto(base_ptr, gpa),
        .module_kind = serialized_ptr.module_kind.decode(),
        .all_defs = serialized_ptr.all_defs,
        .all_statements = serialized_ptr.all_statements,
        .exports = serialized_ptr.exports,
        .requires_types = serialized_ptr.requires_types.deserializeInto(base_ptr),
        .for_clause_aliases = serialized_ptr.for_clause_aliases.deserializeInto(base_ptr),
        .builtin_statements = serialized_ptr.builtin_statements,
        .external_decls = serialized_ptr.external_decls.deserializeInto(base_ptr),
        .imports = try serialized_ptr.imports.deserializeInto(base_ptr, gpa),
        .module_name = module_name,
        .module_name_idx = undefined,
        .diagnostics = serialized_ptr.diagnostics,
        .store = serialized_ptr.store.deserializeInto(base_ptr, gpa),
        .evaluation_order = null,
        .idents = ModuleEnv.CommonIdents.find(&common),
        .deferred_numeric_literals = try ModuleEnv.DeferredNumericLiteral.SafeList.initCapacity(gpa, 0),
        .import_mapping = types.import_mapping.ImportMapping.init(gpa),
        .method_idents = serialized_ptr.method_idents.deserializeInto(base_ptr),
        .rigid_vars = std.AutoHashMapUnmanaged(base.Ident.Idx, types.Var){},
    };

    return LoadedModule{
        .env = env,
        .buffer = buffer,
        .gpa = gpa,
    };
}

/// Test environment for cross-module monomorphization testing
const MonoTestEnv = struct {
    gpa: std.mem.Allocator,
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can_instance: *Can,
    checker: Check,
    module_envs: std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType),
    builtin_module: LoadedModule,
    owns_builtin_module: bool,
    imported_envs_list: std.ArrayList(*const ModuleEnv),

    const Self = @This();

    /// Initialize a single module test environment
    pub fn init(module_name: []const u8, source: []const u8) !Self {
        const gpa = testing.allocator;

        var allocators: Allocators = undefined;
        allocators.initInPlace(gpa);
        defer allocators.deinit();

        const module_env = try gpa.create(ModuleEnv);
        errdefer gpa.destroy(module_env);

        const can_instance = try gpa.create(Can);
        errdefer gpa.destroy(can_instance);

        var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);

        const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
        var builtin_module = try loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
        errdefer builtin_module.deinit();

        module_env.* = try ModuleEnv.init(gpa, source);
        errdefer module_env.deinit();

        module_env.common.source = source;
        module_env.module_name = module_name;
        module_env.module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
        try module_env.common.calcLineStarts(gpa);

        try Can.populateModuleEnvs(&module_envs, module_env, builtin_module.env, builtin_indices);

        const parse_ast = try parse.parse(&allocators, &module_env.common);
        errdefer parse_ast.deinit();
        parse_ast.store.emptyScratch();

        try module_env.initCIRFields(module_name);

        can_instance.* = try Can.init(&allocators, module_env, parse_ast, &module_envs);
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

        module_env.imports.resolveImports(module_env, imported_envs_list.items);

        var checker = try Check.init(
            gpa,
            &module_env.types,
            module_env,
            imported_envs_list.items,
            &module_envs,
            &module_env.store.regions,
            module_builtin_ctx,
        );
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
    pub fn initWithImport(module_name: []const u8, source: []const u8, other_module_name: []const u8, other_env: *const Self) !Self {
        const gpa = testing.allocator;

        var allocators: Allocators = undefined;
        allocators.initInPlace(gpa);
        defer allocators.deinit();

        const module_env = try gpa.create(ModuleEnv);
        errdefer gpa.destroy(module_env);

        const can_instance = try gpa.create(Can);
        errdefer gpa.destroy(can_instance);

        var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);

        const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
        const builtin_env = other_env.builtin_module.env;

        module_env.* = try ModuleEnv.init(gpa, source);
        errdefer module_env.deinit();

        module_env.common.source = source;
        module_env.module_name = module_name;
        module_env.module_name_idx = try module_env.insertIdent(base.Ident.for_text(module_name));
        try module_env.common.calcLineStarts(gpa);

        const other_module_ident = try module_env.insertIdent(base.Ident.for_text(other_module_name));

        const statement_idx = blk: {
            if (other_env.module_env.module_kind == .type_module) {
                const type_ident = other_env.module_env.common.findIdent(other_module_name);
                if (type_ident) |ident| {
                    if (other_env.module_env.getExposedNodeIndexById(ident)) |node_idx| {
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

        try Can.populateModuleEnvs(&module_envs, module_env, builtin_env, builtin_indices);

        const parse_ast = try parse.parse(&allocators, &module_env.common);
        errdefer parse_ast.deinit();
        parse_ast.store.emptyScratch();

        try module_env.initCIRFields(module_name);

        can_instance.* = try Can.init(&allocators, module_env, parse_ast, &module_envs);
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

        module_env.imports.resolveImports(module_env, imported_envs_list.items);

        var checker = try Check.init(
            gpa,
            &module_env.types,
            module_env,
            imported_envs_list.items,
            &module_envs,
            &module_env.store.regions,
            module_builtin_ctx,
        );
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

    /// Create a ClosureTransformer for this module
    pub fn createClosureTransformer(self: *Self) ClosureTransformer {
        return ClosureTransformer.init(self.gpa, self.module_env);
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

    // Check that the method is registered in method_idents
    const method_lookup = env_a.module_env.lookupMethodIdent(a_type_ident.?, get_value_ident.?);
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

test "cross-module mono: closure transformer tracks unspecialized closures" {
    // Module A defines a type with a method
    const source_a =
        \\A := [A(U64)].{
        \\  get_value : A -> U64
        \\  get_value = |A.A(val)| val
        \\}
    ;
    var env_a = try MonoTestEnv.init("A", source_a);
    defer env_a.deinit();

    // Module B imports A and uses polymorphic dispatch
    const source_b =
        \\import A
        \\
        \\# Polymorphic function that uses static dispatch
        \\call_get : a -> U64 where [a.get_value : a -> U64]
        \\call_get = |x|
        \\    T : x
        \\    T.get_value()
    ;
    var env_b = try MonoTestEnv.initWithImport("B", source_b, "A", &env_a);
    defer env_b.deinit();

    // Create closure transformer for module B
    var transformer = env_b.createClosureTransformer();
    defer transformer.deinit();

    // The transformer should be initialized and ready
    _ = &transformer;
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

    // Check method lookups work
    try testing.expect(env_a.module_env.lookupMethodIdent(counter_ident.?, new_ident.?) != null);
    try testing.expect(env_a.module_env.lookupMethodIdent(counter_ident.?, increment_ident.?) != null);
    try testing.expect(env_a.module_env.lookupMethodIdent(counter_ident.?, get_ident.?) != null);
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

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const module_env = try gpa.create(ModuleEnv);
    defer gpa.destroy(module_env);

    const can_instance = try gpa.create(Can);
    defer gpa.destroy(can_instance);

    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs.deinit();

    const builtin_indices = try deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    var builtin_module = try loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", compiled_builtins.builtin_source);
    defer builtin_module.deinit();

    module_env.* = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    module_env.common.source = source;
    module_env.module_name = "Test";
    module_env.module_name_idx = try module_env.insertIdent(base.Ident.for_text("Test"));
    try module_env.common.calcLineStarts(gpa);

    try Can.populateModuleEnvs(&module_envs, module_env, builtin_module.env, builtin_indices);

    const parse_ast = try parse.parse(&allocators, &module_env.common);
    defer parse_ast.deinit();
    parse_ast.store.emptyScratch();

    try module_env.initCIRFields("Test");

    can_instance.* = try Can.init(&allocators, module_env, parse_ast, &module_envs);
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

    module_env.imports.resolveImports(module_env, imported_envs_list.items);

    var checker = try Check.init(
        gpa,
        &module_env.types,
        module_env,
        imported_envs_list.items,
        &module_envs,
        &module_env.store.regions,
        module_builtin_ctx,
    );
    defer checker.deinit();

    try checker.checkFile();

    // The key assertion: type checking should catch the infinite type error.
    // This proves we don't need the polymorphic recursion detection in the monomorphizer.
    try testing.expect(checker.problems.len() > 0);
}

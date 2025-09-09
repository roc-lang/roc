//! These tests verify the core type instantiation logic for polymorphic values.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const can = @import("can");

const TypesStore = types.Store;
const ModuleEnv = can.ModuleEnv;
const testing = std.testing;
const test_allocator = testing.allocator;
const Instantiate = types.instantiate.Instantiate;

// test env //

const TestEnv = struct {
    module_env: *ModuleEnv,
    store: *TypesStore,
    var_subs: *Instantiate.SeenVars,
    rigid_var_subs: *Instantiate.RigidToFlexSubs,

    fn init(allocator: std.mem.Allocator) !TestEnv {
        var src_testing = try base.SrcBytes.Testing.initFromSlice(allocator, "");
        defer src_testing.deinit(allocator);
        const module_env = try allocator.create(ModuleEnv);
        module_env.* = try ModuleEnv.init(allocator, src_testing.src);

        const store = try allocator.create(TypesStore);
        store.* = try TypesStore.init(allocator);

        const var_subs = try allocator.create(Instantiate.SeenVars);
        var_subs.* = Instantiate.SeenVars.init(allocator);

        const rigid_var_subs = try allocator.create(Instantiate.RigidToFlexSubs);
        rigid_var_subs.* = try Instantiate.RigidToFlexSubs.init(allocator);

        return .{
            .module_env = module_env,
            .store = store,
            .var_subs = var_subs,
            .rigid_var_subs = rigid_var_subs,
        };
    }

    fn deinit(self: *TestEnv, allocator: std.mem.Allocator) void {
        self.store.deinit();
        allocator.destroy(self.store);
        self.module_env.deinit();
        allocator.destroy(self.module_env);
        self.var_subs.deinit();
        allocator.destroy(self.var_subs);
        self.rigid_var_subs.deinit(allocator);
        allocator.destroy(self.rigid_var_subs);
    }

    fn instantiate(self: *TestEnv, var_to_inst: types.Var, rigid_subs: []const struct { ident: []const u8, var_: types.Var }) !types.Var {
        self.var_subs.clearRetainingCapacity();
        self.rigid_var_subs.clearFrom(0);

        for (rigid_subs) |sub| {
            _ = try self.module_env.insertIdent(base.Ident.for_text(sub.ident));
            try self.rigid_var_subs.append(self.module_env.gpa, .{ .ident = sub.ident, .var_ = sub.var_ });
        }

        var inst = Instantiate.init(self.store, self.module_env.getIdentStore(), self.var_subs);
        var instantiate_ctx = Instantiate.Ctx{
            .rigid_var_subs = self.rigid_var_subs,
        };
        return inst.instantiateVar(var_to_inst, &instantiate_ctx);
    }
};

test "let-polymorphism with empty list" {
    var env = try TestEnv.init(test_allocator);
    defer env.deinit(test_allocator);

    // forall a. List a
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const list_elem_var = try env.store.freshFromContent(.{ .rigid_var = a_ident });
    const poly_list_var = try env.store.freshFromContent(.{ .structure = .{ .list = list_elem_var } });

    try testing.expect(env.store.needsInstantiation(poly_list_var));

    const int_var = try env.store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
    const int_list = try env.instantiate(poly_list_var, &.{.{ .ident = "a", .var_ = int_var }});

    const str_var = try env.store.freshFromContent(.{ .structure = .str });
    const str_list = try env.instantiate(poly_list_var, &.{.{ .ident = "a", .var_ = str_var }});

    try testing.expect(int_list != str_list);
    try testing.expect(int_list != poly_list_var);
}

test "let-polymorphism with polymorphic function" {
    var env = try TestEnv.init(test_allocator);
    defer env.deinit(test_allocator);

    // forall a. a -> a
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const type_param = try env.store.freshFromContent(.{ .rigid_var = a_ident });
    const func_content = try env.store.mkFuncPure(&.{type_param}, type_param);
    const func_var = try env.store.freshFromContent(func_content);

    try testing.expect(env.store.needsInstantiation(func_var));

    const str_var = try env.store.freshFromContent(.{ .structure = .str });
    const str_func = try env.instantiate(func_var, &.{.{ .ident = "a", .var_ = str_var }});

    const num_var = try env.store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .u32 } } } });
    const num_func = try env.instantiate(func_var, &.{.{ .ident = "a", .var_ = num_var }});

    try testing.expect(str_func != num_func);
}

test "let-polymorphism with multiple type parameters" {
    var env = try TestEnv.init(test_allocator);
    defer env.deinit(test_allocator);

    // forall a b. (a, b) -> (b, a)
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const b_ident = try env.module_env.insertIdent(base.Ident.for_text("b"));
    const type_a = try env.store.freshFromContent(.{ .rigid_var = a_ident });
    const type_b = try env.store.freshFromContent(.{ .rigid_var = b_ident });

    const func_content = try env.store.mkFuncPure(&.{ type_a, type_b }, type_b); // Simplified for test
    const func_var = try env.store.freshFromContent(func_content);

    const str_var = try env.store.freshFromContent(.{ .structure = .str });
    const int_var = try env.store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });

    const inst1 = try env.instantiate(func_var, &.{
        .{ .ident = "a", .var_ = str_var },
        .{ .ident = "b", .var_ = int_var },
    });

    const inst2 = try env.instantiate(func_var, &.{
        .{ .ident = "a", .var_ = int_var },
        .{ .ident = "b", .var_ = str_var },
    });

    try testing.expect(inst1 != inst2);
}

test "let-polymorphism preserves sharing within single instantiation" {
    var env = try TestEnv.init(test_allocator);
    defer env.deinit(test_allocator);

    // forall a. { first: a, second: a }
    const a_ident = try env.module_env.insertIdent(base.Ident.for_text("a"));
    const type_param = try env.store.freshFromContent(.{ .rigid_var = a_ident });

    const fields_range = try env.store.record_fields.appendSlice(env.module_env.gpa, &[_]types.RecordField{
        .{ .name = try env.module_env.insertIdent(base.Ident.for_text("first")), .var_ = type_param },
        .{ .name = try env.module_env.insertIdent(base.Ident.for_text("second")), .var_ = type_param },
    });
    const empty_ext = try env.store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try env.store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields_range, .ext = empty_ext } } });

    const int_var = try env.store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
    const instantiated_rec = try env.instantiate(record_var, &.{.{ .ident = "a", .var_ = int_var }});

    // Verify that both fields now point to the same, new concrete type.
    const content = env.store.resolveVar(instantiated_rec).desc.content;
    const rec = content.structure.record;
    const fields = env.store.record_fields.sliceRange(rec.fields);

    try testing.expectEqual(fields.get(0).var_, fields.get(1).var_);
    try testing.expect(env.store.resolveVar(fields.get(0).var_).desc.content.structure.num.num_compact.int == .i32);
}

test "let-polymorphism prevents over-generalization of concrete types" {
    var env = try TestEnv.init(test_allocator);
    defer env.deinit(test_allocator);

    const i32_var = try env.store.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } });
    const list_i32_var = try env.store.freshFromContent(.{ .structure = .{ .list = i32_var } });

    // This should NOT need instantiation because it's already concrete.
    try testing.expect(!env.store.needsInstantiation(list_i32_var));
}

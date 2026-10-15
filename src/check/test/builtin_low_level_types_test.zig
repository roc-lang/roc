//! Tests that every builtin the compiler implements builds no tag union of its
//! own (design.md "Builtins Build Their Own Tag Unions").

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");
const ModuleEnv = @import("can").ModuleEnv;
const CIR = @import("can").CIR;
const types = @import("types");

const Var = types.Var;

test "no compiler-implemented builtin has a structural tag union in its type" {
    var test_env = try TestEnv.init("Test", "x = 1");
    defer test_env.deinit();
    const env = test_env.builtin_module.env;
    const gpa = std.testing.allocator;

    var stack: std.ArrayListUnmanaged(Var) = .empty;
    defer stack.deinit(gpa);
    var seen: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer seen.deinit(gpa);

    try std.testing.expect(env.provided_low_level_defs.items.items.len > 0);
    for (env.provided_low_level_defs.items.items) |entry| {
        const def_idx: CIR.Def.Idx = @fromBackingInt(entry.def_idx);
        const def = env.store.getDef(def_idx);
        stack.clearRetainingCapacity();
        seen.clearRetainingCapacity();
        try stack.append(gpa, ModuleEnv.varFrom(def.pattern));
        while (stack.pop()) |var_| {
            const resolved = env.types.resolveVar(var_);
            if ((try seen.getOrPut(gpa, resolved.var_)).found_existing) continue;
            switch (resolved.desc.content) {
                .alias => |alias| try stack.append(gpa, env.types.getAliasBackingVar(alias)),
                .structure => |flat| switch (flat) {
                    .tag_union, .empty_tag_union => {
                        std.debug.print("compiler-implemented builtin `{s}` ({s}) has a structural tag union in its type\n", .{
                            env.getIdentText(env.store.getPattern(def.pattern).assign.ident),
                            @tagName(entry.op),
                        });
                        return error.TestUnexpectedResult;
                    },
                    // A nominal type's backing is its own declaration; only its
                    // arguments are part of this type.
                    .nominal_type => |nominal| try stack.appendSlice(gpa, env.types.sliceNominalArgs(nominal)),
                    .fn_pure, .fn_effectful, .fn_unbound => |func| {
                        try stack.appendSlice(gpa, env.types.sliceVars(func.args));
                        try stack.append(gpa, func.ret);
                    },
                    .tuple => |tuple| try stack.appendSlice(gpa, env.types.sliceVars(tuple.elems)),
                    .record => |record| {
                        for (env.types.getRecordFieldsSlice(record.fields).items(.presence)) |presence| {
                            try stack.append(gpa, presence.typeVar());
                        }
                        try stack.append(gpa, record.ext);
                    },
                    .empty_record => {},
                },
                .flex, .rigid, .err, .field_presence => {},
            }
        }
    }
}

//! Value-reference resolution across a module boundary: an importer's
//! recorded resolution reads the imported module's recorded binding
//! resolutions (design.md "Value-Reference Resolution").

const std = @import("std");
const can = @import("can");
const TestEnv = @import("TestEnv.zig");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const testing = std.testing;

fn topLevelDef(env: *const ModuleEnv, name: []const u8) CIR.Def.Idx {
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const pattern = env.store.getPattern(env.store.getDef(def_idx).pattern);
        if (pattern != .assign) continue;
        if (std.mem.eql(u8, env.getIdent(pattern.assign.ident), name)) return def_idx;
    }
    std.debug.panic("no top-level definition named {s}", .{name});
}

fn bindingBody(env: *const ModuleEnv, name: []const u8) std.meta.Tag(ModuleEnv.BindingResolutionBody) {
    return std.meta.activeTag(env.bindingResolution(env.store.getDef(topLevelDef(env, name)).pattern).?.body);
}

/// The call in the body of the one-parameter lambda `name` is bound to.
fn lambdaBodyCall(env: *const ModuleEnv, name: []const u8) CIR.Expr.Idx {
    const lambda = env.store.getExpr(env.store.getDef(topLevelDef(env, name)).expr).e_lambda;
    std.debug.assert(env.store.getExpr(lambda.body) == .e_call);
    return lambda.body;
}

fn contains(items: []const CIR.Expr.Idx, expr: CIR.Expr.Idx) bool {
    for (items) |item| {
        if (item == expr) return true;
    }
    return false;
}

test "an importer forwards through an imported alias and retires uses of an imported checked error" {
    const imported_src =
        \\Lib := [Lib]
        \\
        \\scale : U64 -> U64
        \\scale = |value| value * 3
        \\
        \\triple : U64 -> U64
        \\triple = scale
        \\
        \\broken : U64 -> U64
        \\broken = missing_function
    ;
    var imported = try TestEnv.init("Lib", imported_src);
    defer imported.deinit();
    const lib = imported.module_env;
    try testing.expectEqual(.procedure, bindingBody(lib, "scale"));
    try testing.expectEqual(.evaluated, bindingBody(lib, "triple"));
    try testing.expect(lib.bindingResolution(lib.store.getDef(topLevelDef(lib, "triple")).pattern).?.forwarded_lookup != null);
    try testing.expectEqual(.checked_error, bindingBody(lib, "broken"));

    const main_src =
        \\Main := [Main]
        \\
        \\import Lib
        \\
        \\tripled : U64 -> U64
        \\tripled = Lib.triple
        \\
        \\use_triple : U64 -> U64
        \\use_triple = |value| Lib.triple(value)
        \\
        \\use_broken : U64 -> U64
        \\use_broken = |value| Lib.broken(value)
    ;
    var main_module = try TestEnv.initWithImport("Main", main_src, "Lib", &imported);
    defer main_module.deinit();
    const main = main_module.module_env;

    // An alias of an imported forwarding alias forwards too.
    const tripled = main.bindingResolution(main.store.getDef(topLevelDef(main, "tripled")).pattern).?;
    try testing.expect(tripled.body == .evaluated);
    try testing.expect(tripled.forwarded_lookup != null);

    // A call through the imported alias is a direct call.
    const triple_call = lambdaBodyCall(main, "use_triple");
    try testing.expect(contains(main.direct_procedure_calls.items.items, triple_call));
    try testing.expect(!contains(main.retired_exprs.items.items, triple_call));

    // The use of the imported checked error, and its call, are retired.
    const broken_call = lambdaBodyCall(main, "use_broken");
    const broken_lookup = main.store.getExpr(broken_call).e_call.func;
    try testing.expect(contains(main.retired_exprs.items.items, broken_lookup));
    try testing.expect(contains(main.retired_exprs.items.items, broken_call));
    try testing.expect(!contains(main.direct_procedure_calls.items.items, broken_call));
    try testing.expect(main.valueReference(broken_lookup).? == .imported);
}

const std = @import("std");
const testing = std.testing;
const ModuleEnv = @import("can").ModuleEnv;
const Can = @import("can").Can;
const Check = @import("check").Check;
const parse = @import("parse");

test "simple lambda parsing" {
    const allocator = testing.allocator;

    const source =
        \\module [double]
        \\
        \\double = \x -> x * 2
    ;

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();

    env.module_name = "Test";
    env.common.source = source;
    try env.common.calcLineStarts(allocator);

    var parser = try parse.parse(&env.common, allocator);
    defer parser.deinit(allocator);

    try env.initCIRFields(allocator, "Test");

    var czer = try Can.init(&env, &parser, null);
    defer czer.deinit();
    try czer.canonicalizeFile();

    // Check what we got
    const defs = env.store.sliceDefs(env.all_defs);
    for (defs) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = env.getIdent(pattern.assign.ident);
            const expr = env.store.getExpr(def.expr);
        }
    }
}

//! Regression test for https://github.com/roc-lang/roc/issues/10151.

const std = @import("std");
const parse = @import("parse");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const BuiltinTestContext = @import("BuiltinTestContext.zig").BuiltinTestContext;
const CoreCtx = @import("ctx").CoreCtx;

test "issue 10151: malformed nested calls produce diagnostics without panicking" {
    const source =
        \\C(a):=[E e()].{p=|c|{{()(f(v))y}}()t=|t|{r{}}a=|c|{r{e(v)f(v)}}}c=|g f x|e
        \\c=|f x|{d n(e a s)m(a b)(())m|0||x|{p f0}p|e|{d(0)d|x|p(|0|v)t}g
        \\a{p|f||c|{r(t)}r("")d()r d("")s
        \\e s o d n
        \\n|x|s m}p(|x|0)|x|l t{s:(i i i)e,d,l}}
    ;

    const allocator = std.testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    const roc_ctx = CoreCtx.testing(allocator, allocator);
    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();
    try can.canonicalizeFile();
    try can.validateForChecking();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try std.testing.expect(diagnostics.len > 0);
}

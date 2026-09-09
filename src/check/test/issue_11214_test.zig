//! Regression test for https://github.com/roc-lang/roc/issues/11214
//!
//! A `base.ModuleIdentity.Idx` is only meaningful in the identity table of the
//! module that recorded it. An `e_lookup_associated_resolved` owned by an
//! imported module must therefore be resolved through that module's table, not
//! through the root module's: pruning hoisted roots after solving walks
//! imported bodies, where the same slot number names a different module.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "issue 11214 - pruning hoisted roots resolves an imported associated lookup through its own module" {
    // Naming an imported receiver interns Shape's identity before Helper
    // interns its own identity and Builtin's. Main has only its own identity
    // and Builtin's when pruning starts, so Helper's Builtin slot is out of
    // bounds in Main's table.
    const shape_source =
        \\Shape := [Circle, Square]
    ;
    var shape_env = try TestEnv.init("Shape", shape_source);
    defer shape_env.deinit();
    try shape_env.assertNoErrors();

    const helper_source =
        \\import Shape
        \\
        \\Ext := [].{
        \\    describe : Shape.Shape -> Str
        \\    describe = |_| "shape"
        \\}
        \\
        \\negate! = |b| !b
    ;
    var helper_env = try TestEnv.initWithImport("Helper", helper_source, "Shape", &shape_env);
    defer helper_env.deinit();
    try helper_env.assertNoErrors();

    // Pruning walks Helper.negate!'s body, including its resolved lookup of
    // Builtin.Bool.not, to decide whether the selected call can be stored.
    const main_source =
        \\import Helper
        \\
        \\main = |_| Helper.negate!(True)
    ;
    var main_env = try TestEnv.initWithImport("Main", main_source, "Helper", &helper_env);
    defer main_env.deinit();
    try main_env.assertNoErrors();

    const roots = main_env.checker.selectedHoistedRoots();
    try std.testing.expectEqual(@as(usize, 1), roots.len);
    try std.testing.expect(main_env.module_env.store.getExpr(roots[0].expr) == .e_call);
}

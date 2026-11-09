//! Regression tests for https://github.com/roc-lang/roc/issues/12190.

const expectAppPathLowersToMonotype = @import("lower_to_lir_harness.zig").expectAppPathLowersToMonotype;

test "issue 12190: imported nominal lambda field default followed by a lambda reaches Monotype" {
    // `Config.{}` fills in the imported nominal's `filter` default, a lambda
    // defined in `Config.roc`. The app then writes its own `|x| x` lambda in
    // the same body. The app checks cleanly and must lower through Monotype.
    try expectAppPathLowersToMonotype(
        "test/postcheck/issue_12190_imported_lambda_field_default/app.roc",
    );
}

test "issue 12190: lambda field default of a type nested in an imported type module reaches Monotype" {
    try expectAppPathLowersToMonotype(
        "test/postcheck/issue_12190_imported_lambda_field_default/app_nested_type_module.roc",
    );
}

test "issue 12190: imported lambda field default that binds a value reaches Monotype" {
    // The default's lambda is lowered with a binder of `Suffixed.roc` in
    // scope; the app's later lambda must not see that module's binders.
    try expectAppPathLowersToMonotype(
        "test/postcheck/issue_12190_imported_lambda_field_default/app_default_binding.roc",
    );
}

//! Complete imported schemes must survive both call syntaxes and preserve
//! independent codec validation at every use.

const TestEnv = @import("TestEnv.zig");

pub const codec_source =
    \\Codec := {}.{
    \\    encode = |_self, a, b| Json.to_str({ a, b })
    \\
    \\    encode_tuple = |_self, a, b| Json.to_str((a, b))
    \\}
;

test "issue 11444: imported codec methods independently check mixed call syntaxes" {
    var source = try TestEnv.init("Codec", codec_source);
    defer source.deinit();
    try source.assertNoErrors();

    const accepted_source =
        \\import Codec
        \\first = Codec.encode(Codec.{}, "ok", {})
        \\second = Codec.{}.encode(True, "other")
        \\third = Codec.encode(Codec.{}, {}, False)
    ;
    var accepted = try TestEnv.initWithImport("Accepted", accepted_source, "Codec", &source);
    defer accepted.deinit();
    try accepted.assertNoErrors();
    try accepted.assertDefType("first", "Str");
    try accepted.assertDefType("second", "Str");
    try accepted.assertDefType("third", "Str");

    // Neither syntax can reuse an earlier successful codec validation for a
    // different substitution, or contaminate the next independent use.
    inline for (.{
        "Codec.{}.encode(Opaque.O(\"missing codec\"), {})",
        "Codec.encode(Codec.{}, Opaque.O(\"missing codec\"), {})",
    }) |bad_call| {
        const rejected_source =
            "import Codec\n" ++
            "Opaque := [O(Str)]\n" ++
            "first = Codec.encode(Codec.{}, \"ok\", {})\n" ++
            "bad = " ++ bad_call ++ "\n" ++
            "last = Codec.{}.encode(True, \"still ok\")\n";
        var rejected = try TestEnv.initWithImport("Rejected", rejected_source, "Codec", &source);
        defer rejected.deinit();
        try rejected.assertOneTypeError("Missing Method");
        try rejected.assertDefTypeOptions("first", "Str", .{ .allow_type_errors = true });
        try rejected.assertDefTypeOptions("last", "Str", .{ .allow_type_errors = true });
    }
}

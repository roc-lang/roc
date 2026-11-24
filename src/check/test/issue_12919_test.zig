//! Composite dictionary key gates have one shape-independent role per derivation.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

const format =
    \\Format := [Default].{
    \\    encode_u64 : U64, U64 -> Try(U64, [])
    \\    encode_u64 = |value, state| Ok(state + value)
    \\    encode_str : Str, U64 -> Try(U64, [])
    \\    encode_str = |value, state| Ok(state + value.count_utf8_bytes())
    \\    encode_tuple : U64, U64, (U64, (U64, (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
    \\    encode_tuple = |state, _count, write| write(state, |items, write_item| write_item(items))
    \\    encode_dict : U64, U64, (U64, (U64, (U64 -> Try(U64, [])), (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
    \\    encode_dict = |state, _count, write| write(state, |items, write_key, write_value| write_value(write_key(items)?))
    \\
;

const encode =
    \\}
    \\encode : a -> Try(U64, []) where [a.encoder_for : Format -> (a, U64 -> Try(U64, []))]
    \\encode = |value| {
    \\    Shape : a
    \\    Shape.encoder_for(Format.Default)(value, 0)
    \\}
    \\expect encode((
    \\    Dict.from_list([((1.U64, "a"), "one")]),
    \\    Dict.from_list([((2.U64, 3.U64), "two")]),
    \\)) == Ok(13)
;

test "issue 12919 - distinct composite key shapes share one subject-free gate" {
    var env = try TestEnv.init("Test", format ++
        \\    encode_key_start : Format, U64 -> Try(U64, [])
        \\    encode_key_start = |_, state| Ok(state)
        \\
    ++ encode);
    defer env.deinit();
    try env.assertNoErrors();

    var gate_count: usize = 0;
    const calls = env.module_env.generated_codec_calls.items();
    for (env.module_env.generated_codec_derivations.items()) |derivation| {
        var boundary_gates: usize = 0;
        for (calls[derivation.calls_start..][0..derivation.calls_len]) |call| {
            if (!std.mem.eql(u8, env.module_env.getIdentText(@bitCast(call.method_ident)), "encode_key_start")) continue;
            try std.testing.expect(call.subject_var == .none);
            boundary_gates += 1;
        }
        try std.testing.expect(boundary_gates <= 1);
        gate_count += boundary_gates;
    }
    try std.testing.expect(gate_count > 0);
}

test "issue 12919 - composite keys still require the format's gate" {
    var env = try TestEnv.init("Test", format ++ encode);
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
    try env.assertOneTypeProblemMsgContains("Missing Method", "encode_key_start");
}

test "issue 12919 - a validated gate does not admit another format without one" {
    var env = try TestEnv.init("Test", format ++
        \\    encode_key_start : Format, U64 -> Try(U64, [])
        \\    encode_key_start = |_, state| Ok(state)
        \\
    ++ encode ++
        \\
        \\Other := [Default].{
        \\    encode_u64 = Format.encode_u64
        \\    encode_str = Format.encode_str
        \\    encode_tuple = Format.encode_tuple
        \\    encode_dict = Format.encode_dict
        \\}
        \\expect {
        \\    Shape : Dict((U64, Str), Str)
        \\    Shape.encoder_for(Other.Default)(Dict.from_list([((1.U64, "a"), "one")]), 0.U64) == Ok(5)
        \\}
    );
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
    try env.assertOneTypeProblemMsgContains("Missing Method", "encode_key_start");
}

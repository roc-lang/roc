//! Deferred codec constraints must reach a function's type before it generalizes.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

const decoder =
    \\decode = |body| {
    \\    decoded : Try({ a : Str }, _)
    \\    decoded = Json.parse(body)
    \\    decoded.map_ok(|r| r.a)
    \\}
    \\send = |decoder, body| decoder(body)
    \\fetch = |body| send(decode, body)
    \\forward = |body| fetch(body)
;

test "issue 11730: fixed decoder errors survive generalized helper layers" {
    var env = try TestEnv.init("Test", decoder);
    defer env.deinit();
    try env.assertNoErrors();
    for ([_][]const u8{ "decode", "fetch", "forward" }) |name| {
        const ty = try env.allocDefType(std.testing.allocator, name);
        defer std.testing.allocator.free(ty);
        try std.testing.expect(std.mem.find(u8, ty, "MissingRequiredField(Str)") != null);
        try std.testing.expect(std.mem.find(u8, ty, "InvalidJson(Str)") != null);
    }
    // A fixed local shape contributes its constraints once. Its helpers do not
    // need per-use codec evidence just to rediscover the same required field.
    try std.testing.expectEqual(0, env.module_env.binding_scheme_codec_requirements.items.items.len);
}

test "issue 11730: forwarding depth does not multiply fixed codec derivations" {
    var direct = try TestEnv.init("Direct", decoder);
    defer direct.deinit();
    try direct.assertNoErrors();

    var source = std.ArrayList(u8).empty;
    defer source.deinit(std.testing.allocator);
    try source.appendSlice(std.testing.allocator, decoder ++ "\nhop0 = |body| forward(body)\n");
    for (1..33) |depth| {
        const line = try std.fmt.allocPrint(std.testing.allocator, "hop{d} = |body| hop{d}(body)\n", .{ depth, depth - 1 });
        defer std.testing.allocator.free(line);
        try source.appendSlice(std.testing.allocator, line);
    }
    var forwarded = try TestEnv.init("Forwarded", source.items);
    defer forwarded.deinit();
    try forwarded.assertNoErrors();
    const ty = try forwarded.allocDefType(std.testing.allocator, "hop32");
    defer std.testing.allocator.free(ty);
    try std.testing.expect(std.mem.find(u8, ty, "MissingRequiredField(Str)") != null);
    try std.testing.expectEqual(0, forwarded.module_env.binding_scheme_codec_requirements.items.items.len);
    try std.testing.expectEqual(
        direct.module_env.generated_codec_derivations.items.items.len,
        forwarded.module_env.generated_codec_derivations.items.items.len,
    );
    try std.testing.expectEqual(
        direct.module_env.generated_codec_calls.items.items.len,
        forwarded.module_env.generated_codec_calls.items.items.len,
    );
}

test "issue 11730: decoder errors survive an imported helper" {
    var imported = try TestEnv.init("Decoder", "module [decode, fetch, forward]\n" ++ decoder);
    defer imported.deinit();
    var env = try TestEnv.initWithImport("Test",
        \\import Decoder
        \\send = |decoder, body| decoder(body)
        \\fetch = |body| send(Decoder.forward, body)
    , "Decoder", &imported);
    defer env.deinit();
    try env.assertNoErrors();
    const ty = try env.allocDefType(std.testing.allocator, "fetch");
    defer std.testing.allocator.free(ty);
    try std.testing.expect(std.mem.find(u8, ty, "MissingRequiredField(Str)") != null);
}

test "issue 11730: consuming helper cannot exclude a required parser error" {
    var env = try TestEnv.init("Test", decoder ++
        \\
        \\consume : Str -> Str
        \\consume = |body| match forward(body) {
        \\    Ok(value) => value
        \\    Err(InvalidJson(_)) => "invalid"
        \\}
    );
    defer env.deinit();
    try std.testing.expect(try env.typeProblemCount() > 0);
}

test "issue 11730: decoder annotation cannot exclude its required-field error" {
    var env = try TestEnv.init("Test",
        \\decode : Str -> Try(Str, [InvalidJson(Str)])
        \\
    ++ decoder);
    defer env.deinit();
    try std.testing.expect(try env.typeProblemCount() > 0);
}

test "issue 11730: nested local record demands survive success projection" {
    var env = try TestEnv.init("Test",
        \\decode = |body| {
        \\    decoded : Try({ a : { value : Str } }, _)
        \\    decoded = Json.parse(body)
        \\    decoded.map_ok(|r| r.a.value)
        \\}
        \\send = |decoder, body| decoder(body)
        \\fetch = |body| send(decode, body)
    );
    defer env.deinit();
    try env.assertNoErrors();
    const ty = try env.allocDefType(std.testing.allocator, "fetch");
    defer std.testing.allocator.free(ty);
    try std.testing.expect(std.mem.find(u8, ty, "MissingRequiredField(Str)") != null);
    try std.testing.expectEqual(0, env.module_env.binding_scheme_codec_requirements.items.items.len);
}

test "issue 11730: local decoder permits wider independent caller rows" {
    var env = try TestEnv.init("Test", decoder ++
        \\
        \\first = |body, fail| if fail { Err(First) } else { forward(body) }
        \\second = |body, fail| if fail { Err(Second) } else { forward(body) }
    );
    defer env.deinit();
    try env.assertNoErrors();
    const first = try env.allocDefType(std.testing.allocator, "first");
    defer std.testing.allocator.free(first);
    const second = try env.allocDefType(std.testing.allocator, "second");
    defer std.testing.allocator.free(second);
    try std.testing.expect(std.mem.find(u8, first, "MissingRequiredField(Str)") != null);
    try std.testing.expect(std.mem.find(u8, second, "MissingRequiredField(Str)") != null);
    try std.testing.expect(std.mem.find(u8, first, "Second") == null);
    try std.testing.expect(std.mem.find(u8, second, "First") == null);
}

test "issue 11730: generic decoder retains independent per-use codec requirements" {
    var env = try TestEnv.init("Test",
        \\decode = |fallback, body| {
        \\    decoded : Try({ a : _ }, _)
        \\    decoded = Json.parse(body)
        \\    decoded.map_ok(|r| {
        \\        _ = [r.a, fallback]
        \\        "ok"
        \\    })
        \\}
        \\send = |decoder, fallback, body| decoder(fallback, body)
        \\fetch = |fallback, body| send(decode, fallback, body)
        \\first = fetch("", "{}")
        \\zero : U64
        \\zero = 0
        \\second = fetch(zero, "{}")
    );
    defer env.deinit();
    try env.assertNoErrors();
    for ([_][]const u8{ "first", "second" }) |name| {
        const ty = try env.allocDefType(std.testing.allocator, name);
        defer std.testing.allocator.free(ty);
        try std.testing.expect(std.mem.find(u8, ty, "MissingRequiredField(Str)") != null);
    }
}

test "issue 11730: optional-only local records do not gain required-field errors" {
    var env = try TestEnv.init("Test",
        \\decode = |body| {
        \\    decoded : Try({ a : Try(Str, [Missing]) }, _)
        \\    decoded = Json.parse(body)
        \\    decoded.map_ok(|_| "ok")
        \\}
        \\send = |decoder, body| decoder(body)
        \\fetch = |body| send(decode, body)
    );
    defer env.deinit();
    try env.assertNoErrors();
    const ty = try env.allocDefType(std.testing.allocator, "fetch");
    defer std.testing.allocator.free(ty);
    try std.testing.expect(std.mem.find(u8, ty, "MissingRequiredField") == null);
}

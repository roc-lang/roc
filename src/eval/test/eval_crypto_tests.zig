//! Eval coverage for the public typed crypto builtins.

const std = @import("std");
const TestCase = @import("parallel_runner.zig").TestCase;

const Sha256 = std.crypto.hash.sha2.Sha256;
const Blake3 = std.crypto.hash.Blake3;

fn digestBytes(comptime Hasher: type, comptime input: []const u8) [32]u8 {
    @setEvalBranchQuota(10_000);

    comptime var digest: [32]u8 = undefined;
    Hasher.hash(input, &digest, .{});
    return digest;
}

fn digestHex(comptime Hasher: type, comptime input: []const u8) [64]u8 {
    return std.fmt.bytesToHex(digestBytes(Hasher, input), .lower);
}

fn quotedHexInspect(comptime Hasher: type, comptime input: []const u8) []const u8 {
    const hex = digestHex(Hasher, input);
    return std.fmt.comptimePrint("\"{s}\"", .{&hex});
}

fn digestBytesInspect(comptime Hasher: type, comptime input: []const u8) []const u8 {
    @setEvalBranchQuota(100_000);

    const digest = digestBytes(Hasher, input);

    comptime var text: []const u8 = "[";
    inline for (digest, 0..) |byte, index| {
        if (index != 0) {
            text = text ++ ", ";
        }
        text = text ++ std.fmt.comptimePrint("{d}", .{byte});
    }
    return text ++ "]";
}

fn hexLiteral(comptime Hasher: type, comptime input: []const u8) []const u8 {
    const hex = digestHex(Hasher, input);
    return std.fmt.comptimePrint("{s}", .{&hex});
}

fn repeatByte(comptime byte: u8, comptime count: usize) []const u8 {
    comptime var text: []const u8 = "";
    comptime var index: usize = 0;
    inline while (index < count) : (index += 1) {
        text = text ++ .{byte};
    }
    return text;
}

const sha256_abc_hex = hexLiteral(Sha256, "abc");
const blake3_abc_hex = hexLiteral(Blake3, "abc");
const sha256_abcdef_hex = hexLiteral(Sha256, "abcdef");
const blake3_abcdef_hex = hexLiteral(Blake3, "abcdef");
const invalid_hex_last = repeatByte('0', 63) ++ "z";

/// Public crypto eval cases consumed by the parallel eval runner.
pub const tests = [_]TestCase{
    .{
        .name = "Crypto SHA256 hash to_hex",
        .source = "Crypto.SHA256.hash(\"abc\".to_utf8()).to_hex()",
        .expected = .{ .inspect_str = quotedHexInspect(Sha256, "abc") },
    },
    .{
        .name = "Crypto BLAKE3 hash to_hex",
        .source = "Crypto.BLAKE3.hash(\"abc\".to_utf8()).to_hex()",
        .expected = .{ .inspect_str = quotedHexInspect(Blake3, "abc") },
    },
    .{
        .name = "Crypto SHA256 hash to_bytes",
        .source = "Crypto.SHA256.hash(\"abc\".to_utf8()).to_bytes()",
        .expected = .{ .inspect_str = digestBytesInspect(Sha256, "abc") },
    },
    .{
        .name = "Crypto BLAKE3 hash to_bytes",
        .source = "Crypto.BLAKE3.hash(\"abc\".to_utf8()).to_bytes()",
        .expected = .{ .inspect_str = digestBytesInspect(Blake3, "abc") },
    },
    .{
        .name = "Crypto SHA256 Hasher write finish",
        .source =
        \\{
        \\    hasher0 = Crypto.SHA256.Hasher.empty()
        \\    hasher1 = Crypto.SHA256.Hasher.write(hasher0, "ab".to_utf8())
        \\    hasher2 = Crypto.SHA256.Hasher.write(hasher1, "cdef".to_utf8())
        \\    Crypto.SHA256.Digest.to_hex(Crypto.SHA256.Hasher.finish(hasher2))
        \\}
        ,
        .expected = .{ .inspect_str = quotedHexInspect(Sha256, "abcdef") },
    },
    .{
        .name = "Crypto BLAKE3 Hasher write finish",
        .source =
        \\{
        \\    hasher0 = Crypto.BLAKE3.Hasher.empty()
        \\    hasher1 = Crypto.BLAKE3.Hasher.write(hasher0, "ab".to_utf8())
        \\    hasher2 = Crypto.BLAKE3.Hasher.write(hasher1, "cdef".to_utf8())
        \\    Crypto.BLAKE3.Digest.to_hex(Crypto.BLAKE3.Hasher.finish(hasher2))
        \\}
        ,
        .expected = .{ .inspect_str = quotedHexInspect(Blake3, "abcdef") },
    },
    .{
        .name = "Crypto SHA256 hash_chunks",
        .source = "Crypto.SHA256.hash_chunks([\"ab\".to_utf8(), [], \"cdef\".to_utf8()].iter()).to_hex()",
        .expected = .{ .inspect_str = quotedHexInspect(Sha256, "abcdef") },
    },
    .{
        .name = "Crypto BLAKE3 hash_chunks",
        .source = "Crypto.BLAKE3.hash_chunks([\"ab\".to_utf8(), [], \"cdef\".to_utf8()].iter()).to_hex()",
        .expected = .{ .inspect_str = quotedHexInspect(Blake3, "abcdef") },
    },
    .{
        .name = "Crypto SHA256 from_bytes accepts digest bytes",
        .source =
        \\{
        \\    digest = Crypto.SHA256.hash("abc".to_utf8())
        \\    match Crypto.SHA256.Digest.from_bytes(digest.to_bytes()) {
        \\        Ok(parsed) => Crypto.SHA256.Digest.is_eq(parsed, digest)
        \\        Err(_) => False
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Crypto BLAKE3 from_bytes accepts digest bytes",
        .source =
        \\{
        \\    digest = Crypto.BLAKE3.hash("abc".to_utf8())
        \\    match Crypto.BLAKE3.Digest.from_bytes(digest.to_bytes()) {
        \\        Ok(parsed) => Crypto.BLAKE3.Digest.is_eq(parsed, digest)
        \\        Err(_) => False
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Crypto SHA256 from_hex accepts hex",
        .source =
        \\match Crypto.SHA256.Digest.from_hex("
        ++ sha256_abc_hex ++
            \\") {
            \\    Ok(digest) => Crypto.SHA256.Digest.to_hex(digest)
            \\    Err(_) => "bad"
            \\}
        ,
        .expected = .{ .inspect_str = quotedHexInspect(Sha256, "abc") },
    },
    .{
        .name = "Crypto BLAKE3 from_hex accepts hex",
        .source =
        \\match Crypto.BLAKE3.Digest.from_hex("
        ++ blake3_abc_hex ++
            \\") {
            \\    Ok(digest) => Crypto.BLAKE3.Digest.to_hex(digest)
            \\    Err(_) => "bad"
            \\}
        ,
        .expected = .{ .inspect_str = quotedHexInspect(Blake3, "abc") },
    },
    .{
        .name = "Crypto SHA256 from_hex accepts uppercase",
        .source =
        \\match Crypto.SHA256.Digest.from_hex("BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD") {
        \\    Ok(digest) => Crypto.SHA256.Digest.to_hex(digest)
        \\    Err(_) => "bad"
        \\}
        ,
        .expected = .{ .inspect_str = quotedHexInspect(Sha256, "abc") },
    },
    .{
        .name = "Crypto SHA256 from_bytes rejects wrong length",
        .source =
        \\match Crypto.SHA256.Digest.from_bytes([1, 2, 3]) {
        \\    Err(WrongLength({ expected, actual })) => expected == 32 and actual == 3
        \\    Ok(_) => False
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Crypto SHA256 from_hex rejects wrong length",
        .source =
        \\match Crypto.SHA256.Digest.from_hex("00") {
        \\    Err(WrongLength({ expected, actual })) => expected == 64 and actual == 2
        \\    _ => False
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Crypto SHA256 from_hex rejects invalid hex",
        .source =
        \\match Crypto.SHA256.Digest.from_hex("
        ++ invalid_hex_last ++
            \\") {
            \\    Err(InvalidHex({ index, byte })) => index == 63 and byte == 122
            \\    _ => False
            \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Crypto SHA256 finish does not consume prior state",
        .source =
        \\{
        \\    hasher = Crypto.SHA256.Hasher.write(Crypto.SHA256.Hasher.empty(), "abc".to_utf8())
        \\    first = Crypto.SHA256.Hasher.finish(hasher)
        \\    second = Crypto.SHA256.Hasher.finish(Crypto.SHA256.Hasher.write(hasher, "def".to_utf8()))
        \\    Crypto.SHA256.Digest.to_hex(first) == "
        ++ sha256_abc_hex ++
            \\" and Crypto.SHA256.Digest.to_hex(second) == "
        ++ sha256_abcdef_hex ++
            \\"
            \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "Crypto BLAKE3 finish does not consume prior state",
        .source =
        \\{
        \\    hasher = Crypto.BLAKE3.Hasher.write(Crypto.BLAKE3.Hasher.empty(), "abc".to_utf8())
        \\    first = Crypto.BLAKE3.Hasher.finish(hasher)
        \\    second = Crypto.BLAKE3.Hasher.finish(Crypto.BLAKE3.Hasher.write(hasher, "def".to_utf8()))
        \\    Crypto.BLAKE3.Digest.to_hex(first) == "
        ++ blake3_abc_hex ++
            \\" and Crypto.BLAKE3.Digest.to_hex(second) == "
        ++ blake3_abcdef_hex ++
            \\"
            \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
};

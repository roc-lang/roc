//! Cryptographic digest helpers for compiler-owned crypto builtins.

const std = @import("std");
const list = @import("list.zig");
const utils = @import("utils.zig");

const RocList = list.RocList;
const RocOps = utils.RocOps;
const Sha256 = std.crypto.hash.sha2.Sha256;
const Blake3 = std.crypto.hash.Blake3;

const digest_len = 32;

const sha256_state_version: u8 = 1;
const sha256_state_len = 1 + 8 * 4 + Sha256.block_length + 1 + 8;

const blake3_state_version: u8 = 1;
const blake3_cv_stack_max = 55;
const blake3_state_base_len = 1 + 8 * 4 + 8 * 4 + 8 + Blake3.block_length + 1 + 1 + 1 + 1;

fn invariant(roc_ops: *RocOps, comptime message: []const u8) noreturn {
    roc_ops.crash("crypto builtin invariant violated: " ++ message);
    unreachable;
}

fn readInput(bytes: ?[*]const u8, len: usize, roc_ops: *RocOps) []const u8 {
    if (bytes) |ptr| return ptr[0..len];
    if (len == 0) return &.{};
    invariant(roc_ops, "null pointer with nonzero byte length");
}

fn listFromBytes(bytes: []const u8, roc_ops: *RocOps) RocList {
    return RocList.fromSlice(u8, bytes, false, roc_ops);
}

fn writeU32(out: []u8, value: u32) void {
    std.mem.writeInt(u32, out[0..4], value, .little);
}

fn readU32(bytes: []const u8) u32 {
    return std.mem.readInt(u32, bytes[0..4], .little);
}

fn writeU64(out: []u8, value: u64) void {
    std.mem.writeInt(u64, out[0..8], value, .little);
}

fn readU64(bytes: []const u8) u64 {
    return std.mem.readInt(u64, bytes[0..8], .little);
}

/// Hash a byte slice with SHA-256 and return the 32 digest bytes as a Roc list.
pub fn sha256HashBytes(input_bytes: ?[*]const u8, input_len: usize, roc_ops: *RocOps) RocList {
    const input = readInput(input_bytes, input_len, roc_ops);
    var digest: [digest_len]u8 = undefined;
    Sha256.hash(input, &digest, .{});
    return listFromBytes(&digest, roc_ops);
}

/// Hash a byte slice with BLAKE3 and return the 32 digest bytes as a Roc list.
pub fn blake3HashBytes(input_bytes: ?[*]const u8, input_len: usize, roc_ops: *RocOps) RocList {
    const input = readInput(input_bytes, input_len, roc_ops);
    var digest: [digest_len]u8 = undefined;
    Blake3.hash(input, &digest, .{});
    return listFromBytes(&digest, roc_ops);
}

fn serializeSha256(hasher: *const Sha256, roc_ops: *RocOps) RocList {
    var state: [sha256_state_len]u8 = undefined;
    state[0] = sha256_state_version;

    var offset: usize = 1;
    for (hasher.s) |word| {
        writeU32(state[offset..][0..4], word);
        offset += 4;
    }

    @memset(state[offset..][0..Sha256.block_length], 0);
    @memcpy(state[offset..][0..hasher.buf_len], hasher.buf[0..hasher.buf_len]);
    offset += Sha256.block_length;

    state[offset] = hasher.buf_len;
    offset += 1;
    writeU64(state[offset..][0..8], hasher.total_len);

    return listFromBytes(&state, roc_ops);
}

fn deserializeSha256(state: []const u8, roc_ops: *RocOps) Sha256 {
    if (state.len != sha256_state_len) invariant(roc_ops, "bad SHA256 state length");
    if (state[0] != sha256_state_version) invariant(roc_ops, "bad SHA256 state version");

    var hasher = Sha256.init(.{});
    var offset: usize = 1;
    for (&hasher.s) |*word| {
        word.* = readU32(state[offset..][0..4]);
        offset += 4;
    }

    hasher.buf = @splat(0);
    @memcpy(hasher.buf[0..Sha256.block_length], state[offset..][0..Sha256.block_length]);
    offset += Sha256.block_length;

    hasher.buf_len = state[offset];
    if (hasher.buf_len > Sha256.block_length) invariant(roc_ops, "bad SHA256 buffer length");
    offset += 1;
    hasher.total_len = readU64(state[offset..][0..8]);
    if (hasher.total_len % Sha256.block_length != hasher.buf_len) invariant(roc_ops, "bad SHA256 total length");

    return hasher;
}

/// Return a serialized empty SHA-256 hasher state.
pub fn sha256HasherEmpty(roc_ops: *RocOps) RocList {
    var hasher = Sha256.init(.{});
    return serializeSha256(&hasher, roc_ops);
}

/// Deserialize SHA-256 state, update it with bytes, and return serialized state.
pub fn sha256HasherWrite(state_bytes: ?[*]const u8, state_len: usize, input_bytes: ?[*]const u8, input_len: usize, roc_ops: *RocOps) RocList {
    const state = readInput(state_bytes, state_len, roc_ops);
    const input = readInput(input_bytes, input_len, roc_ops);
    var hasher = deserializeSha256(state, roc_ops);
    hasher.update(input);
    return serializeSha256(&hasher, roc_ops);
}

/// Deserialize SHA-256 state and return its digest without mutating the input state bytes.
pub fn sha256HasherFinish(state_bytes: ?[*]const u8, state_len: usize, roc_ops: *RocOps) RocList {
    const state = readInput(state_bytes, state_len, roc_ops);
    var hasher = deserializeSha256(state, roc_ops);
    var digest: [digest_len]u8 = undefined;
    hasher.final(&digest);
    return listFromBytes(&digest, roc_ops);
}

fn serializeBlake3Words(out: []u8, words: [8]u32) void {
    var offset: usize = 0;
    for (words) |word| {
        writeU32(out[offset..][0..4], word);
        offset += 4;
    }
}

fn deserializeBlake3Words(bytes: []const u8) [8]u32 {
    var words: [8]u32 = undefined;
    var offset: usize = 0;
    for (&words) |*word| {
        word.* = readU32(bytes[offset..][0..4]);
        offset += 4;
    }
    return words;
}

fn serializeBlake3(hasher: *const Blake3, roc_ops: *RocOps) RocList {
    if (hasher.cv_stack_len > blake3_cv_stack_max) invariant(roc_ops, "bad BLAKE3 stack length");

    const state_len = blake3_state_base_len + @as(usize, hasher.cv_stack_len) * digest_len;
    var state: [blake3_state_base_len + blake3_cv_stack_max * digest_len]u8 = undefined;
    const bytes = state[0..state_len];
    var offset: usize = 0;
    bytes[offset] = blake3_state_version;
    offset += 1;

    serializeBlake3Words(bytes[offset..][0..32], hasher.key);
    offset += 32;
    serializeBlake3Words(bytes[offset..][0..32], hasher.chunk.cv);
    offset += 32;
    writeU64(bytes[offset..][0..8], hasher.chunk.chunk_counter);
    offset += 8;

    @memset(bytes[offset..][0..Blake3.block_length], 0);
    @memcpy(bytes[offset..][0..hasher.chunk.buf_len], hasher.chunk.buf[0..hasher.chunk.buf_len]);
    offset += Blake3.block_length;

    bytes[offset] = hasher.chunk.buf_len;
    offset += 1;
    bytes[offset] = hasher.chunk.blocks_compressed;
    offset += 1;
    bytes[offset] = @bitCast(hasher.chunk.flags);
    offset += 1;
    bytes[offset] = hasher.cv_stack_len;
    offset += 1;

    var i: usize = 0;
    while (i < hasher.cv_stack_len) : (i += 1) {
        serializeBlake3Words(bytes[offset..][0..32], hasher.cv_stack[i]);
        offset += 32;
    }

    return listFromBytes(bytes, roc_ops);
}

fn deserializeBlake3(state: []const u8, roc_ops: *RocOps) Blake3 {
    if (state.len < blake3_state_base_len) invariant(roc_ops, "bad BLAKE3 state length");
    if (state[0] != blake3_state_version) invariant(roc_ops, "bad BLAKE3 state version");

    var offset: usize = 1;
    var hasher = Blake3.init(.{});

    hasher.key = deserializeBlake3Words(state[offset..][0..32]);
    offset += 32;
    hasher.chunk.cv = deserializeBlake3Words(state[offset..][0..32]);
    offset += 32;
    hasher.chunk.chunk_counter = readU64(state[offset..][0..8]);
    offset += 8;

    hasher.chunk.buf = @splat(0);
    @memcpy(hasher.chunk.buf[0..Blake3.block_length], state[offset..][0..Blake3.block_length]);
    offset += Blake3.block_length;

    hasher.chunk.buf_len = state[offset];
    if (hasher.chunk.buf_len > Blake3.block_length) invariant(roc_ops, "bad BLAKE3 buffer length");
    offset += 1;
    hasher.chunk.blocks_compressed = state[offset];
    if (hasher.chunk.blocks_compressed > 15) invariant(roc_ops, "bad BLAKE3 block count");
    offset += 1;
    hasher.chunk.flags = @bitCast(state[offset]);
    offset += 1;
    hasher.cv_stack_len = state[offset];
    if (hasher.cv_stack_len > blake3_cv_stack_max) invariant(roc_ops, "bad BLAKE3 stack length");
    offset += 1;

    const expected_len = blake3_state_base_len + @as(usize, hasher.cv_stack_len) * digest_len;
    if (state.len != expected_len) invariant(roc_ops, "bad BLAKE3 active stack byte length");

    hasher.cv_stack = undefined;
    var i: usize = 0;
    while (i < hasher.cv_stack_len) : (i += 1) {
        hasher.cv_stack[i] = deserializeBlake3Words(state[offset..][0..32]);
        offset += 32;
    }

    return hasher;
}

/// Return a serialized empty BLAKE3 hasher state.
pub fn blake3HasherEmpty(roc_ops: *RocOps) RocList {
    var hasher = Blake3.init(.{});
    return serializeBlake3(&hasher, roc_ops);
}

/// Deserialize BLAKE3 state, update it with bytes, and return serialized state.
pub fn blake3HasherWrite(state_bytes: ?[*]const u8, state_len: usize, input_bytes: ?[*]const u8, input_len: usize, roc_ops: *RocOps) RocList {
    const state = readInput(state_bytes, state_len, roc_ops);
    const input = readInput(input_bytes, input_len, roc_ops);
    var hasher = deserializeBlake3(state, roc_ops);
    hasher.update(input);
    return serializeBlake3(&hasher, roc_ops);
}

/// Deserialize BLAKE3 state and return its digest without mutating the input state bytes.
pub fn blake3HasherFinish(state_bytes: ?[*]const u8, state_len: usize, roc_ops: *RocOps) RocList {
    const state = readInput(state_bytes, state_len, roc_ops);
    var hasher = deserializeBlake3(state, roc_ops);
    var digest: [digest_len]u8 = undefined;
    hasher.final(&digest);
    return listFromBytes(&digest, roc_ops);
}

fn listBytes(roc_list: RocList) []const u8 {
    if (roc_list.bytes) |ptr| return ptr[0..roc_list.length];
    if (roc_list.length == 0) return &.{};
    unreachable;
}

fn expectDigestEqual(actual: RocList, expected: []const u8) error{TestExpectedEqual}!void {
    try std.testing.expectEqualSlices(u8, expected, listBytes(actual));
}

fn sha256Digest(input: []const u8) [digest_len]u8 {
    var digest: [digest_len]u8 = undefined;
    Sha256.hash(input, &digest, .{});
    return digest;
}

fn blake3Digest(input: []const u8) [digest_len]u8 {
    var digest: [digest_len]u8 = undefined;
    Blake3.hash(input, &digest, .{});
    return digest;
}

test "SHA256 one-shot and streaming match stdlib" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const ops = env.getOps();

    const cases = [_][]const u8{
        "",
        "abc",
        "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
    };

    for (cases) |input| {
        const expected = sha256Digest(input);
        try expectDigestEqual(sha256HashBytes(input.ptr, input.len, ops), &expected);

        var state = sha256HasherEmpty(ops);
        var offset: usize = 0;
        while (offset < input.len) {
            const next = @min(input.len, offset + 7);
            state = sha256HasherWrite(state.bytes, state.length, input.ptr + offset, next - offset, ops);
            offset = next;
        }
        try expectDigestEqual(sha256HasherFinish(state.bytes, state.length, ops), &expected);
    }
}

test "BLAKE3 one-shot and streaming match stdlib" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const ops = env.getOps();

    var boundary: [1024]u8 = undefined;
    for (&boundary, 0..) |*byte, i| byte.* = @truncate(i);

    var larger: [4097]u8 = undefined;
    for (&larger, 0..) |*byte, i| byte.* = @truncate(i * 31);

    const cases = [_][]const u8{
        "",
        "abc",
        &boundary,
        &larger,
    };

    for (cases) |input| {
        const expected = blake3Digest(input);
        try expectDigestEqual(blake3HashBytes(input.ptr, input.len, ops), &expected);

        var state = blake3HasherEmpty(ops);
        var offset: usize = 0;
        while (offset < input.len) {
            const step = if (offset % 3 == 0) @as(usize, 1) else @as(usize, 37);
            const next = @min(input.len, offset + step);
            state = blake3HasherWrite(state.bytes, state.length, input.ptr + offset, next - offset, ops);
            offset = next;
        }
        try expectDigestEqual(blake3HasherFinish(state.bytes, state.length, ops), &expected);
    }
}

test "crypto finish does not mutate serialized state" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const ops = env.getOps();

    const input = "abc";
    const more = "def";

    const sha_state = sha256HasherWrite(sha256HasherEmpty(ops).bytes, sha256_state_len, input.ptr, input.len, ops);
    const sha_before = listBytes(sha_state);
    _ = sha256HasherFinish(sha_state.bytes, sha_state.length, ops);
    try std.testing.expectEqualSlices(u8, sha_before, listBytes(sha_state));
    const sha_expected = sha256Digest(input ++ more);
    try expectDigestEqual(sha256HasherFinish(sha256HasherWrite(sha_state.bytes, sha_state.length, more.ptr, more.len, ops).bytes, sha256_state_len, ops), &sha_expected);

    const blake_state = blake3HasherWrite(blake3HasherEmpty(ops).bytes, blake3_state_base_len, input.ptr, input.len, ops);
    const blake_before = listBytes(blake_state);
    _ = blake3HasherFinish(blake_state.bytes, blake_state.length, ops);
    try std.testing.expectEqualSlices(u8, blake_before, listBytes(blake_state));
    const blake_expected = blake3Digest(input ++ more);
    const blake_after = blake3HasherWrite(blake_state.bytes, blake_state.length, more.ptr, more.len, ops);
    try expectDigestEqual(blake3HasherFinish(blake_after.bytes, blake_after.length, ops), &blake_expected);
}

test "crypto null pointer with zero length hashes empty input" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const ops = env.getOps();

    const sha_expected = sha256Digest("");
    const blake_expected = blake3Digest("");
    try expectDigestEqual(sha256HashBytes(null, 0, ops), &sha_expected);
    try expectDigestEqual(blake3HashBytes(null, 0, ops), &blake_expected);
}

//! BLAKE3 `update` without recursion, over std's hasher state.
//!
//! std's `Blake3.update` hashes each whole subtree with a recursive helper.
//! Builtins compiled for a wasm platform that reserves low memory must not
//! recurse (see `stack_guard.zig`), so those builtins update through this
//! instead. It hashes a subtree's two halves one chunk at a time and leaves
//! exactly the state std's `update` leaves, so the serialized hasher state and
//! every digest are the same whichever builtins a program uses.

const std = @import("std");

const Blake3 = std.crypto.hash.Blake3;
const chunk_length = 1024;
const block_length = Blake3.block_length;

const flag_chunk_start: u8 = 1;
const flag_chunk_end: u8 = 2;
const flag_parent: u8 = 4;

const iv: [8]u32 = .{
    0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
    0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19,
};

const msg_schedule: [7][16]u8 = .{
    .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 },
    .{ 2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8 },
    .{ 3, 4, 10, 12, 13, 2, 7, 14, 6, 5, 9, 0, 11, 15, 8, 1 },
    .{ 10, 7, 12, 9, 14, 3, 13, 15, 4, 0, 11, 2, 5, 8, 1, 6 },
    .{ 12, 13, 9, 11, 15, 10, 14, 8, 7, 2, 5, 3, 0, 1, 6, 4 },
    .{ 9, 14, 11, 5, 8, 12, 15, 1, 13, 3, 0, 10, 2, 6, 4, 7 },
    .{ 11, 15, 5, 0, 1, 9, 8, 6, 14, 10, 2, 12, 3, 4, 7, 13 },
};

/// The deepest a BLAKE3 tree over less than 2^64 bytes goes.
const max_depth = 54;

/// Add `input` to `hasher`, exactly as `Blake3.update` does.
pub fn update(hasher: *Blake3, input: []const u8) void {
    if (input.len == 0) return;
    var inp = input;

    if (chunkLen(hasher) > 0) {
        const take = @min(chunk_length - chunkLen(hasher), inp.len);
        chunkUpdate(hasher, inp[0..take]);
        inp = inp[take..];
        if (inp.len == 0) return;
        pushCv(hasher, chunkOutputCv(hasher), hasher.chunk.chunk_counter);
        resetChunk(hasher, hasher.chunk.chunk_counter + 1);
    }

    while (inp.len > chunk_length) {
        var subtree_len = roundDownToPowerOf2(inp.len);
        const count_so_far = hasher.chunk.chunk_counter * chunk_length;
        while ((subtree_len - 1) & count_so_far != 0) subtree_len /= 2;

        const subtree_chunks = subtree_len / chunk_length;
        const subtree = inp[0..@intCast(subtree_len)];
        if (subtree_len <= chunk_length) {
            pushCv(hasher, chunkCv(subtree, hasher.key, hasher.chunk.chunk_counter, flagsOf(hasher)), hasher.chunk.chunk_counter);
        } else {
            const half = subtree.len / 2;
            const half_chunks = subtree_chunks / 2;
            const left = subtreeCv(subtree[0..half], hasher.key, hasher.chunk.chunk_counter, flagsOf(hasher));
            const right = subtreeCv(subtree[half..], hasher.key, hasher.chunk.chunk_counter + half_chunks, flagsOf(hasher));
            pushCv(hasher, left, hasher.chunk.chunk_counter);
            pushCv(hasher, right, hasher.chunk.chunk_counter + half_chunks);
        }
        hasher.chunk.chunk_counter += subtree_chunks;
        inp = inp[@intCast(subtree_len)..];
    }

    if (inp.len > 0) {
        chunkUpdate(hasher, inp);
        mergeCvStack(hasher, hasher.chunk.chunk_counter);
    }
}

fn flagsOf(hasher: *const Blake3) u8 {
    return @bitCast(hasher.chunk.flags);
}

fn roundDownToPowerOf2(x: u64) u64 {
    return @as(u64, 1) << @intCast(63 - @clz(x));
}

fn chunkLen(hasher: *const Blake3) usize {
    return block_length * @as(usize, hasher.chunk.blocks_compressed) + @as(usize, hasher.chunk.buf_len);
}

fn resetChunk(hasher: *Blake3, chunk_counter: u64) void {
    hasher.chunk.cv = hasher.key;
    hasher.chunk.chunk_counter = chunk_counter;
    hasher.chunk.blocks_compressed = 0;
    hasher.chunk.buf = @splat(0);
    hasher.chunk.buf_len = 0;
}

fn startFlag(blocks_compressed: u8) u8 {
    return if (blocks_compressed == 0) flag_chunk_start else 0;
}

/// Feed `input` to the hasher's current chunk, compressing a full buffered
/// block only once more input follows it, as std's `ChunkState.update` does.
fn chunkUpdate(hasher: *Blake3, input: []const u8) void {
    var inp = input;
    while (inp.len > 0) {
        if (hasher.chunk.buf_len == block_length) {
            compressInPlace(&hasher.chunk.cv, &hasher.chunk.buf, block_length, hasher.chunk.chunk_counter, flagsOf(hasher) | startFlag(hasher.chunk.blocks_compressed));
            hasher.chunk.blocks_compressed += 1;
            hasher.chunk.buf = @splat(0);
            hasher.chunk.buf_len = 0;
        }
        const take = @min(block_length - @as(usize, hasher.chunk.buf_len), inp.len);
        @memcpy(hasher.chunk.buf[hasher.chunk.buf_len..][0..take], inp[0..take]);
        hasher.chunk.buf_len += @intCast(take);
        inp = inp[take..];
    }
}

fn chunkOutputCv(hasher: *const Blake3) [8]u32 {
    var cv = hasher.chunk.cv;
    compressInPlace(&cv, &hasher.chunk.buf, hasher.chunk.buf_len, hasher.chunk.chunk_counter, flagsOf(hasher) | startFlag(hasher.chunk.blocks_compressed) | flag_chunk_end);
    return cv;
}

/// The chaining value of one chunk of at most `chunk_length` bytes.
fn chunkCv(chunk: []const u8, key: [8]u32, chunk_counter: u64, flags: u8) [8]u32 {
    var cv = key;
    var blocks_compressed: u8 = 0;
    var rest = chunk;
    while (rest.len > block_length) {
        compressInPlace(&cv, rest[0..block_length], block_length, chunk_counter, flags | startFlag(blocks_compressed));
        blocks_compressed += 1;
        rest = rest[block_length..];
    }
    var last: [block_length]u8 = @splat(0);
    @memcpy(last[0..rest.len], rest);
    compressInPlace(&cv, &last, @intCast(rest.len), chunk_counter, flags | startFlag(blocks_compressed) | flag_chunk_end);
    return cv;
}

/// The chaining value of a subtree of a power-of-two number of whole chunks,
/// merging its chunks' chaining values on an explicit stack.
fn subtreeCv(subtree: []const u8, key: [8]u32, first_chunk: u64, flags: u8) [8]u32 {
    const chunk_count = subtree.len / chunk_length;
    if (chunk_count == 1) return chunkCv(subtree, key, first_chunk, flags);
    var stack: [max_depth + 1][8]u32 = undefined;
    var stack_len: usize = 0;
    for (0..chunk_count) |index| {
        stack[stack_len] = chunkCv(subtree[index * chunk_length ..][0..chunk_length], key, first_chunk + index, flags);
        stack_len += 1;
        var merged = index + 1;
        while (merged % 2 == 0) : (merged /= 2) {
            stack[stack_len - 2] = parentCv(stack[stack_len - 2], stack[stack_len - 1], key, flags);
            stack_len -= 1;
        }
    }
    return stack[0];
}

fn parentCv(left: [8]u32, right: [8]u32, key: [8]u32, flags: u8) [8]u32 {
    var block: [block_length]u8 = undefined;
    for (0..8) |i| {
        std.mem.writeInt(u32, block[i * 4 ..][0..4], left[i], .little);
        std.mem.writeInt(u32, block[(i + 8) * 4 ..][0..4], right[i], .little);
    }
    var cv = key;
    compressInPlace(&cv, &block, block_length, 0, flags | flag_parent);
    return cv;
}

fn mergeCvStack(hasher: *Blake3, total_len: u64) void {
    const post_merge_stack_len: u8 = @intCast(@popCount(total_len));
    while (hasher.cv_stack_len > post_merge_stack_len) {
        const left = hasher.cv_stack[hasher.cv_stack_len - 2];
        const right = hasher.cv_stack[hasher.cv_stack_len - 1];
        hasher.cv_stack[hasher.cv_stack_len - 2] = parentCv(left, right, hasher.key, flagsOf(hasher));
        hasher.cv_stack_len -= 1;
    }
}

fn pushCv(hasher: *Blake3, cv: [8]u32, chunk_counter: u64) void {
    mergeCvStack(hasher, chunk_counter);
    hasher.cv_stack[hasher.cv_stack_len] = cv;
    hasher.cv_stack_len += 1;
}

fn g(state: *[16]u32, a: usize, b: usize, c: usize, d: usize, x: u32, y: u32) void {
    state[a] +%= state[b] +% x;
    state[d] = std.math.rotr(u32, state[d] ^ state[a], 16);
    state[c] +%= state[d];
    state[b] = std.math.rotr(u32, state[b] ^ state[c], 12);
    state[a] +%= state[b] +% y;
    state[d] = std.math.rotr(u32, state[d] ^ state[a], 8);
    state[c] +%= state[d];
    state[b] = std.math.rotr(u32, state[b] ^ state[c], 7);
}

fn compressInPlace(cv: *[8]u32, block: *const [block_length]u8, block_len: u8, counter: u64, flags: u8) void {
    var msg: [16]u32 = undefined;
    for (&msg, 0..) |*word, i| word.* = std.mem.readInt(u32, block[i * 4 ..][0..4], .little);
    var state: [16]u32 = undefined;
    @memcpy(state[0..8], cv);
    @memcpy(state[8..12], iv[0..4]);
    state[12] = @truncate(counter);
    state[13] = @truncate(counter >> 32);
    state[14] = block_len;
    state[15] = flags;
    for (msg_schedule) |schedule| {
        g(&state, 0, 4, 8, 12, msg[schedule[0]], msg[schedule[1]]);
        g(&state, 1, 5, 9, 13, msg[schedule[2]], msg[schedule[3]]);
        g(&state, 2, 6, 10, 14, msg[schedule[4]], msg[schedule[5]]);
        g(&state, 3, 7, 11, 15, msg[schedule[6]], msg[schedule[7]]);
        g(&state, 0, 5, 10, 15, msg[schedule[8]], msg[schedule[9]]);
        g(&state, 1, 6, 11, 12, msg[schedule[10]], msg[schedule[11]]);
        g(&state, 2, 7, 8, 13, msg[schedule[12]], msg[schedule[13]]);
        g(&state, 3, 4, 9, 14, msg[schedule[14]], msg[schedule[15]]);
    }
    for (cv, 0..) |*word, i| word.* = state[i] ^ state[i + 8];
}

test "iterative update leaves std's state and digest for every split of the input" {
    var input: [9 * chunk_length + 517]u8 = undefined;
    for (&input, 0..) |*byte, i| byte.* = @truncate(i *% 31 +% 7);

    const lengths = [_]usize{ 0, 1, 63, 64, 65, 1023, 1024, 1025, 2048, 2049, 3 * chunk_length + 5, 4 * chunk_length, 8 * chunk_length + 1, input.len };
    for (lengths) |total| {
        const splits = [_]usize{ 0, 1, 64, 1000, 1024, 1500, 4096 };
        for (splits) |first| {
            if (first > total) continue;
            var expected = Blake3.init(.{});
            expected.update(input[0..first]);
            expected.update(input[first..total]);
            var actual = Blake3.init(.{});
            update(&actual, input[0..first]);
            update(&actual, input[first..total]);

            try std.testing.expectEqual(expected.cv_stack_len, actual.cv_stack_len);
            try std.testing.expectEqualSlices([8]u32, expected.cv_stack[0..expected.cv_stack_len], actual.cv_stack[0..actual.cv_stack_len]);
            try std.testing.expectEqual(expected.chunk.cv, actual.chunk.cv);
            try std.testing.expectEqual(expected.chunk.chunk_counter, actual.chunk.chunk_counter);
            try std.testing.expectEqual(expected.chunk.buf_len, actual.chunk.buf_len);
            try std.testing.expectEqual(expected.chunk.blocks_compressed, actual.chunk.blocks_compressed);
            try std.testing.expectEqualSlices(u8, &expected.chunk.buf, &actual.chunk.buf);

            var expected_digest: [32]u8 = undefined;
            var actual_digest: [32]u8 = undefined;
            expected.final(&expected_digest);
            actual.final(&actual_digest);
            try std.testing.expectEqualSlices(u8, &expected_digest, &actual_digest);
        }
    }
}

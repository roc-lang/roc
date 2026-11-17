//! UTF-8 validation for the `Str` builtins.
//!
//! `validate` reports the first ill-formed sequence of a byte slice, or null for
//! well-formed UTF-8 (Unicode Table 3-7). Two kernels implement it with identical
//! results. CPUs with a 16-lane byte table lookup (x86 SSSE3 `pshufb`, AArch64
//! NEON `tbl`, Wasm simd128 `i8x16.swizzle`) run the "lookup" algorithm of John
//! Keiser and Daniel Lemire, "Validating UTF-8 In Less Than One Instruction Per
//! Byte" (Software: Practice and Experience, 2021), with the three nibble tables
//! of simdutf (https://github.com/simdutf/simdutf, MIT license; see
//! legal_details). Every other CPU runs a shift-based DFA. The kernel is fixed
//! by the CPU features the code is compiled for (`Kernel.forCpu`); nothing
//! detects CPU features at runtime.

const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;

/// Why a UTF-8 sequence is ill-formed.
/// Values must be in alphabetical order. That is, lowest values are the first alphabetically.
pub const Utf8ByteProblem = enum(u8) {
    CodepointTooLarge = 0,
    EncodesSurrogateHalf = 1,
    ExpectedContinuation = 2,
    InvalidStartByte = 3,
    OverlongEncoding = 4,
    UnexpectedEndOfSequence = 5,
};

/// The first ill-formed sequence in a byte slice: where it starts, and why it
/// is ill-formed.
pub const Utf8Error = struct {
    index: usize,
    problem: Utf8ByteProblem,
};

/// The validation kernels. Each one gives the same result for every input.
pub const Kernel = enum {
    /// The shift-based DFA, for every CPU.
    dfa,
    /// The lookup algorithm with SSSE3 `pshufb`.
    x86_ssse3,
    /// The lookup algorithm with NEON `tbl`.
    aarch64_neon,
    /// The lookup algorithm with simd128 `i8x16.swizzle`.
    wasm_simd128,

    /// The fastest kernel code compiled for `cpu` may use: a lookup kernel
    /// exactly when its feature set carries a 16-lane byte table lookup.
    pub fn forCpu(cpu: std.Target.Cpu) Kernel {
        if (cpu.arch == .x86_64) return if (cpu.has(.x86, .ssse3)) .x86_ssse3 else .dfa;
        if (cpu.arch == .aarch64) return if (cpu.has(.aarch64, .neon)) .aarch64_neon else .dfa;
        if (cpu.arch == .wasm32 or cpu.arch == .wasm64) return if (cpu.has(.wasm, .simd128)) .wasm_simd128 else .dfa;
        return .dfa;
    }
};

/// Whether this code is compiled for the generic x86-64 CPU model with SSSE3
/// added: the floor build.zig gives x86_64 Debug builds for the SHA-256
/// extension (see `sha256.x86_floor`). The generic model names no CPU, so the
/// floor lets Zig's self-hosted backend encode the instructions without
/// promising that the machine running the code has them.
const x86_floor = builtin.cpu.arch == .x86_64 and builtin.cpu.model == &std.Target.x86.cpu.x86_64 and builtin.cpu.has(.x86, .ssse3);

/// The kernel `validate` runs in code compiled for its own target: the one
/// `Kernel.forCpu` selects, except under the x86 floor, and except for the C
/// backend, which cannot emit the table lookup.
pub const target_kernel: Kernel = if (x86_floor or builtin.zig_backend == .stage2_c) .dfa else Kernel.forCpu(builtin.cpu);

/// Returns null when `bytes` is well-formed UTF-8 (Unicode Table 3-7), and
/// otherwise the start index and problem of its first ill-formed sequence.
///
/// The problem is decided in this order. A lead byte that is neither ASCII nor
/// C0-F7 is `InvalidStartByte`. The lead byte announces the sequence length
/// (C0-DF: 2, E0-EF: 3, F0-F7: 4); a sequence running past the end of the input
/// is `UnexpectedEndOfSequence`, whatever its available bytes are. A byte
/// outside 80-BF after the lead byte is `ExpectedContinuation`. Otherwise the
/// lead and second byte decide `OverlongEncoding` (C0, C1, E0 below A0, F0 below
/// 90), `EncodesSurrogateHalf` (ED from A0), or `CodepointTooLarge` (F4 from 90,
/// F5-F7).
///
/// The LLVM builtins bitcode is compiled for wasm64 or wasm32 and retargeted,
/// so it cannot select a kernel when it is compiled: it calls
/// `validate_symbol`, which the LLVM backend defines by linking
/// `utf8_kernel_lib.zig` compiled for the kernel the target's CPU features
/// select.
pub fn validate(bytes: []const u8) ?Utf8Error {
    if (comptime llvm_payload) return validateLinked(bytes);
    return validateWith(target_kernel, bytes);
}

/// Validates `bytes` with `kernel`, which the CPU this code runs on must support.
pub fn validateWith(comptime kernel: Kernel, bytes: []const u8) ?Utf8Error {
    return switch (kernel) {
        .dfa => validateDfa(bytes),
        .x86_ssse3 => validateLookup(lookupX86Ssse3, bytes),
        .aarch64_neon => validateLookup(lookupAarch64Neon, bytes),
        .wasm_simd128 => validateLookup(lookupWasmSimd128, bytes),
    };
}

/// Whether this code is the LLVM builtins bitcode: the 64-bit payload, the only
/// code compiled for wasm64, or a 32-bit payload, whose root (an `llvm32_*`
/// wrapper) declares `roc_llvm_payload`.
const llvm_payload = builtin.cpu.arch == .wasm64 or
    (@hasDecl(@import("root"), "roc_llvm_payload") and @import("root").roc_llvm_payload);

/// The C ABI of the validation function the LLVM builtins bitcode links
/// in for its target: validates `len` bytes at `bytes`, returns `valid_code`
/// for well-formed input, and otherwise stores the error's index in `index` and
/// returns its `Utf8ByteProblem`.
///
/// The caller is compiled for WebAssembly and the definition for the target's
/// own architecture, and the two disagree on who extends an integer narrower
/// than 32 bits: wasm marks a `u8` result `zeroext`, which the caller then
/// trusts, while AArch64 outside Darwin leaves its upper bits unspecified. So
/// the result is a `u32`, which no target extends.
pub const ValidateAbi = fn (bytes: [*]const u8, len: usize, index: *usize) callconv(.c) u32;

/// The symbol the LLVM builtins bitcode references for its validation
/// function, defined by `utf8_kernel_lib.zig` compiled for the target's `Kernel`.
pub const validate_symbol = "roc_builtins_utf8_validate";

/// What `ValidateAbi` returns for well-formed input; no `Utf8ByteProblem` has
/// this value.
pub const valid_code: u32 = 0xFF;

/// `ValidateAbi` implemented by `kernel`.
pub fn validateAbi(comptime kernel: Kernel, bytes: [*]const u8, len: usize, index: *usize) u32 {
    const err = validateWith(kernel, bytes[0..len]) orelse return valid_code;
    index.* = err.index;
    return @backingInt(err.problem);
}

fn validateLinked(bytes: []const u8) ?Utf8Error {
    const linked = @extern(*const ValidateAbi, .{ .name = validate_symbol });
    var index: usize = 0;
    const code = linked(bytes.ptr, bytes.len, &index);
    if (code == valid_code) return null;
    return .{ .index = index, .problem = @fromBackingInt(@intCast(code)) };
}

/// Valid input is checked in one pass without decoding: ASCII runs in 32- and
/// 16-byte blocks, everything else through a shift-based DFA that costs one
/// table load and one shift per byte. Only an error reruns the DFA, from the
/// start of the sequence containing it, to find where that sequence begins.
fn validateDfa(bytes: []const u8) ?Utf8Error {
    const n = bytes.len;
    var i: usize = 0;
    var state: u64 = dfa_accept;
    while (n - i >= 16) {
        if (state & dfa_state_mask == dfa_accept) {
            i = skipAsciiBlocks(bytes, i);
            if (n - i < 16) break;
            if (isAsciiBlock(16, bytes[i..][0..16])) {
                i += 16;
                continue;
            }
        }
        const chunk_state = state;
        inline for (0..16) |k| state = dfaStep(state, bytes[i + k]);
        if (state & dfa_state_mask == dfa_reject) return errorAfter(bytes, i, chunk_state & dfa_state_mask != dfa_accept);
        i += 16;
    }
    if (state & dfa_state_mask == dfa_accept and isAsciiTail(bytes, i)) return null;
    const tail_state = state;
    const tail_start = i;
    while (i < n) : (i += 1) state = dfaStep(state, bytes[i]);
    if (state & dfa_state_mask != dfa_accept) return errorAfter(bytes, tail_start, tail_state & dfa_state_mask != dfa_accept);
    return null;
}

/// The end of the run of all-ASCII 32-byte blocks starting at `start`.
inline fn skipAsciiBlocks(bytes: []const u8, start: usize) usize {
    var i = start;
    while (bytes.len - i >= 32 and isAsciiBlock(32, bytes[i..][0..32])) i += 32;
    return i;
}

/// Whether a block of `len` bytes is all ASCII, using generic vector operations
/// so the builtin bitcode stays target-independent.
inline fn isAsciiBlock(comptime len: comptime_int, block: *const [len]u8) bool {
    var any: V = block[0..16].*;
    inline for (1..len / 16) |k| any |= block[k * 16 ..][0..16].*;
    return isAsciiVector(any);
}

inline fn isAsciiVector(v: V) bool {
    const signed: @Vector(16, i8) = @bitCast(v);
    return !@reduce(.Or, signed < @as(@Vector(16, i8), @splat(0)));
}

/// Whether `bytes[start..]`, fewer than 16 bytes, is all ASCII. Reads may
/// overlap bytes before `start`, which are already known to be valid.
inline fn isAsciiTail(bytes: []const u8, start: usize) bool {
    const n = bytes.len;
    const rem = n - start;
    std.debug.assert(rem < 16);
    if (rem == 0) return true;
    if (n >= 16) return isAsciiBlock(16, bytes[n - 16 ..][0..16]);
    if (n >= 8) {
        var word = mem.readInt(u64, bytes[n - 8 ..][0..8], .little);
        if (rem > 8) word |= mem.readInt(u64, bytes[start..][0..8], .little);
        return word & 0x80808080_80808080 == 0;
    }
    if (n >= 4) {
        var word = mem.readInt(u32, bytes[n - 4 ..][0..4], .little);
        if (rem > 4) word |= mem.readInt(u32, bytes[start..][0..4], .little);
        return word & 0x80808080 == 0;
    }
    for (bytes[start..]) |byte| {
        if (byte >= 0x80) return false;
    }
    return true;
}

// The DFA's state `s` is stored as the bit offset `s * 6`, and the row for a
// byte packs, in bits `s * 6 .. s * 6 + 5`, the next state's offset. One step
// is `row[byte] >> state`; only the low 6 bits of a state are meaningful.
// States follow Unicode Table 3-7:
//   0 accept, 1 reject (absorbing), 2 one continuation byte left,
//   3 two continuation bytes left, 4 after E0 (needs A0-BF), 5 after ED
//   (needs 80-9F), 6 after F0 (needs 90-BF), 7 after F1-F3 (needs 80-BF, then
//   two more), 8 after F4 (needs 80-8F).
const dfa_state_mask: u64 = 63;
const dfa_accept: u64 = 0 * 6;
const dfa_reject: u64 = 1 * 6;

/// Packs the next state for each current state 0-8.
fn dfaRow(comptime next: [9]u64) u64 {
    var row: u64 = 0;
    for (next, 0..) |next_state, state| row |= (next_state * 6) << (state * 6);
    return row;
}

const dfa_row_ascii = dfaRow(.{ 0, 1, 1, 1, 1, 1, 1, 1, 1 });
const dfa_row_cont_80_8f = dfaRow(.{ 1, 1, 0, 2, 1, 2, 1, 3, 3 });
const dfa_row_cont_90_9f = dfaRow(.{ 1, 1, 0, 2, 1, 2, 3, 3, 1 });
const dfa_row_cont_a0_bf = dfaRow(.{ 1, 1, 0, 2, 2, 1, 3, 3, 1 });
const dfa_row_invalid = dfaRow(.{ 1, 1, 1, 1, 1, 1, 1, 1, 1 });
const dfa_row_lead_2 = dfaRow(.{ 2, 1, 1, 1, 1, 1, 1, 1, 1 });
const dfa_row_lead_e0 = dfaRow(.{ 4, 1, 1, 1, 1, 1, 1, 1, 1 });
const dfa_row_lead_3 = dfaRow(.{ 3, 1, 1, 1, 1, 1, 1, 1, 1 });
const dfa_row_lead_ed = dfaRow(.{ 5, 1, 1, 1, 1, 1, 1, 1, 1 });
const dfa_row_lead_f0 = dfaRow(.{ 6, 1, 1, 1, 1, 1, 1, 1, 1 });
const dfa_row_lead_4 = dfaRow(.{ 7, 1, 1, 1, 1, 1, 1, 1, 1 });
const dfa_row_lead_f4 = dfaRow(.{ 8, 1, 1, 1, 1, 1, 1, 1, 1 });

const dfa_rows: [256]u64 =
    @as([0x80]u64, @splat(dfa_row_ascii)) ++ // 00-7F
    @as([0x10]u64, @splat(dfa_row_cont_80_8f)) ++ // 80-8F
    @as([0x10]u64, @splat(dfa_row_cont_90_9f)) ++ // 90-9F
    @as([0x20]u64, @splat(dfa_row_cont_a0_bf)) ++ // A0-BF
    @as([0x02]u64, @splat(dfa_row_invalid)) ++ // C0-C1
    @as([0x1E]u64, @splat(dfa_row_lead_2)) ++ // C2-DF
    @as([0x01]u64, @splat(dfa_row_lead_e0)) ++ // E0
    @as([0x0C]u64, @splat(dfa_row_lead_3)) ++ // E1-EC
    @as([0x01]u64, @splat(dfa_row_lead_ed)) ++ // ED
    @as([0x02]u64, @splat(dfa_row_lead_3)) ++ // EE-EF
    @as([0x01]u64, @splat(dfa_row_lead_f0)) ++ // F0
    @as([0x03]u64, @splat(dfa_row_lead_4)) ++ // F1-F3
    @as([0x01]u64, @splat(dfa_row_lead_f4)) ++ // F4
    @as([0x0B]u64, @splat(dfa_row_invalid)); // F5-FF

inline fn dfaStep(state: u64, byte: u8) u64 {
    return dfa_rows[byte] >> @as(u6, @truncate(state));
}

fn isContinuation(byte: u8) bool {
    return byte & 0xC0 == 0x80;
}

/// Finds the first ill-formed sequence at or after `start`, given that
/// `bytes[0..start]` is well-formed except for one sequence still in progress
/// at `start` when `mid_sequence` holds, and that an ill-formed sequence
/// exists.
noinline fn errorAfter(bytes: []const u8, start: usize, mid_sequence: bool) Utf8Error {
    var i = start;
    if (mid_sequence) {
        // Back up over the in-progress sequence's continuation bytes to its lead byte.
        while (isContinuation(bytes[i - 1])) i -= 1;
        i -= 1;
    }
    var state: u64 = dfa_accept;
    var sequence_start = i;
    while (i < bytes.len) : (i += 1) {
        if (state & dfa_state_mask == dfa_accept) sequence_start = i;
        state = dfaStep(state, bytes[i]);
        if (state & dfa_state_mask == dfa_reject) break;
    }
    std.debug.assert(state & dfa_state_mask != dfa_accept);
    return .{ .index = sequence_start, .problem = problemAt(bytes, sequence_start) };
}

/// Finds the first ill-formed sequence, given that `bytes[0..start]` is
/// well-formed except possibly for one sequence still in progress at `start`,
/// and that an ill-formed sequence exists.
noinline fn errorAfterBlock(bytes: []const u8, start: usize) Utf8Error {
    // An in-progress sequence ends in its lead byte or in at most three
    // continuation bytes after it.
    var i = start;
    while (i > 0 and isContinuation(bytes[i - 1])) i -= 1;
    return errorAfter(bytes, i, i > 0 and bytes[i - 1] >= 0xC0);
}

/// The problem of the ill-formed sequence starting at `start`, in the order
/// `validate` documents.
fn problemAt(bytes: []const u8, start: usize) Utf8ByteProblem {
    const lead = bytes[start];
    const len: usize = switch (lead) {
        0xC0...0xDF => 2,
        0xE0...0xEF => 3,
        0xF0...0xF7 => 4,
        else => return .InvalidStartByte,
    };
    if (bytes.len - start < len) return .UnexpectedEndOfSequence;
    for (bytes[start + 1 .. start + len]) |byte| {
        if (!isContinuation(byte)) return .ExpectedContinuation;
    }
    return switch (lead) {
        0xC0, 0xC1, 0xE0, 0xF0 => .OverlongEncoding,
        0xED => .EncodesSurrogateHalf,
        0xF4...0xF7 => .CodepointTooLarge,
        else => unreachable, // only lead bytes C0, C1, E0, ED and F0-F7 start complete all-continuation sequences that are ill-formed
    };
}

const V = @Vector(16, u8);

/// Inputs shorter than one 16-byte block go to the DFA.
const lookup_min_len = 16;

/// Each byte is checked against the three bytes before it. `byte_1_*` classify
/// the previous byte by its high and low nibble, `byte_2_high` the current byte
/// by its high nibble; a bit set in all three marks one kind of error. Bit
/// meanings, with byte 1 the previous byte and byte 2 the current one:
const too_short: u8 = 1 << 0; // lead byte followed by ASCII or a lead byte
const too_long: u8 = 1 << 1; // ASCII followed by a continuation byte
const overlong_3: u8 = 1 << 2; // E0 followed by 80-9F
const too_large: u8 = 1 << 3; // F4 followed by 90-BF, or F5-FF followed by 90-BF
const surrogate: u8 = 1 << 4; // ED followed by A0-BF
const overlong_2: u8 = 1 << 5; // C0 or C1 followed by a continuation byte
const too_large_1000: u8 = 1 << 6; // F5-FF followed by 80-8F
const overlong_4: u8 = 1 << 6; // F0 followed by 80-8F
const two_continuations: u8 = 1 << 7; // continuation byte followed by a continuation byte
const carry: u8 = too_short | too_long | two_continuations;

const table_byte_1_high: V = .{
    too_long, too_long, too_long, too_long, too_long, too_long, too_long, too_long, // 0_______
    two_continuations, two_continuations, two_continuations, two_continuations, // 10______
    too_short | overlong_2, // 1100____
    too_short, // 1101____
    too_short | overlong_3 | surrogate, // 1110____
    too_short | too_large | too_large_1000 | overlong_4, // 1111____
};

const table_byte_1_low: V = .{
    carry | overlong_3 | overlong_2 | overlong_4, // ____0000
    carry | overlong_2, // ____0001
    carry, carry, // ____001_
    carry | too_large, // ____0100
    carry | too_large | too_large_1000, // ____0101
    carry | too_large | too_large_1000, carry | too_large | too_large_1000, // ____011_
    carry | too_large | too_large_1000, carry | too_large | too_large_1000, carry | too_large | too_large_1000, carry | too_large | too_large_1000, carry | too_large | too_large_1000, // ____1000-____1100
    carry | too_large | too_large_1000 | surrogate, // ____1101
    carry | too_large | too_large_1000, carry | too_large | too_large_1000, // ____111_
};

const table_byte_2_high: V = .{
    too_short, too_short, too_short, too_short, too_short, too_short, too_short, too_short, // 0_______
    too_long | overlong_2 | two_continuations | overlong_3 | too_large_1000 | overlong_4, // 1000____
    too_long | overlong_2 | two_continuations | overlong_3 | too_large, // 1001____
    too_long | overlong_2 | two_continuations | surrogate | too_large, too_long | overlong_2 | two_continuations | surrogate | too_large, // 101_____
    too_short, too_short, too_short, too_short, // 11______
};

/// A block whose last three bytes are above these ends inside a sequence:
/// a 4-byte lead in the third-to-last byte, a 3- or 4-byte lead in the
/// second-to-last, any lead in the last.
const incomplete_max: V = @as([13]u8, @splat(0xFF)) ++ [3]u8{ 0xF0 - 1, 0xE0 - 1, 0xC0 - 1 };

/// `input` shifted `k` lanes later, with the last `k` lanes of `prev` in front.
inline fn prevBytes(comptime k: comptime_int, input: V, prev: V) V {
    const mask = comptime blk: {
        var m: @Vector(16, i32) = undefined;
        for (0..16) |lane| m[lane] = if (lane >= k) lane - k else ~@as(i32, 16 - k + lane);
        break :blk m;
    };
    return @shuffle(u8, input, prev, mask);
}

/// Nonzero lanes mark errors in `input`, which follows `prev` in the input.
inline fn checkBlock(comptime lookup: fn (V, V) V, input: V, prev: V) V {
    const nibble_shift: @Vector(16, u3) = @splat(4);
    const prev1 = prevBytes(1, input, prev);
    const special = lookup(table_byte_1_high, prev1 >> nibble_shift) &
        lookup(table_byte_1_low, prev1 & @as(V, @splat(0x0F))) &
        lookup(table_byte_2_high, input >> nibble_shift);
    // The third and fourth bytes of 3- and 4-byte sequences must be
    // continuation bytes; only 111_____ two bytes back and 1111____ three
    // bytes back reach 0x80 after the saturating subtractions.
    const prev2 = prevBytes(2, input, prev);
    const prev3 = prevBytes(3, input, prev);
    const must_be_continuation = ((prev2 -| @as(V, @splat(0xE0 - 0x80))) | (prev3 -| @as(V, @splat(0xF0 - 0x80)))) & @as(V, @splat(0x80));
    return must_be_continuation ^ special;
}

inline fn isZeroVector(v: V) bool {
    return @as(u128, @bitCast(v)) == 0;
}

/// The lookup algorithm over 32-byte steps, then at most one 16-byte step, then
/// a final 16-byte window ending at the last byte. A step whose bytes are all
/// ASCII only checks that the previous step did not end inside a sequence. The
/// final window may overlap bytes already checked; it is checked against the
/// bytes before it, so the overlap gives the same result again, and it also
/// checks that the input does not end inside a sequence. An error reruns the
/// DFA from the start of the sequence in progress at the failing step.
inline fn validateLookup(comptime lookup: fn (V, V) V, bytes: []const u8) ?Utf8Error {
    const n = bytes.len;
    if (n < lookup_min_len) return validateDfa(bytes);
    var i: usize = skipAsciiBlocks(bytes, 0);
    var prev: V = @splat(0);
    var incomplete: V = @splat(0);
    while (n - i >= 32) {
        if (isZeroVector(incomplete)) {
            const ascii_end = skipAsciiBlocks(bytes, i);
            if (ascii_end != i) {
                i = ascii_end;
                // Every ASCII byte before a block checks like a zero byte.
                prev = @splat(0);
                continue;
            }
        }
        const a: V = bytes[i..][0..16].*;
        const b: V = bytes[i + 16 ..][0..16].*;
        const errors = checkBlock(lookup, a, prev) | checkBlock(lookup, b, a);
        if (!isZeroVector(errors)) return errorAfterBlock(bytes, i);
        incomplete = b -| incomplete_max;
        prev = b;
        i += 32;
    }
    if (n - i >= 16) {
        const a: V = bytes[i..][0..16].*;
        if (isAsciiVector(a)) {
            if (!isZeroVector(incomplete)) return errorAfterBlock(bytes, i);
        } else {
            if (!isZeroVector(checkBlock(lookup, a, prev))) return errorAfterBlock(bytes, i);
            incomplete = a -| incomplete_max;
        }
        i += 16;
    }
    if (i == n) {
        if (!isZeroVector(incomplete)) return errorAfterBlock(bytes, i);
        return null;
    }
    const window_start = n - 16;
    const window: V = bytes[window_start..][0..16].*;
    if (isAsciiVector(window)) {
        if (!isZeroVector(incomplete)) return errorAfterBlock(bytes, i);
        return null;
    }
    const errors = checkBlock(lookup, window, bytesBefore(bytes, window_start)) | (window -| incomplete_max);
    if (!isZeroVector(errors)) return errorAfterBlock(bytes, i);
    return null;
}

/// The 16 bytes before `pos`, or, when fewer than 16 precede it, a vector
/// whose last three lanes hold the bytes `checkBlock` reads (zero before the
/// start of the input, which acts as ASCII).
inline fn bytesBefore(bytes: []const u8, pos: usize) V {
    if (pos >= 16) return bytes[pos - 16 ..][0..16].*;
    var before: V = @splat(0);
    inline for (0..3) |k| {
        if (pos + k >= 3) before[13 + k] = bytes[pos + k - 3];
    }
    return before;
}

// 16-lane byte table lookups. Every index is below 16. x86 uses inline
// assembly: an LLVM intrinsic declared as an extern function takes its vector
// arguments by reference under the Windows x64 calling convention, which does
// not match the intrinsic's signature.

extern fn @"llvm.aarch64.neon.tbl1.v16i8"(V, V) V;
extern fn @"llvm.wasm.swizzle"(V, V) V;

fn lookupX86Ssse3(table: V, index: V) V {
    return asm ("pshufb %[index], %[table]"
        : [ret] "=x" (-> V),
        : [table] "0" (table),
          [index] "x" (index),
    );
}

fn lookupAarch64Neon(table: V, index: V) V {
    if (builtin.zig_backend == .stage2_llvm) return @"llvm.aarch64.neon.tbl1.v16i8"(table, index);
    return asm ("tbl %[ret].16b, {%[table].16b}, %[index].16b"
        : [ret] "=w" (-> V),
        : [table] "w" (table),
          [index] "w" (index),
    );
}

fn lookupWasmSimd128(table: V, index: V) V {
    return @"llvm.wasm.swizzle"(table, index);
}

const testing = std.testing;

/// The table lookup the hardware kernels perform, one lane at a time, so the
/// lookup algorithm runs on every machine the tests run on.
fn lookupPortable(table: V, index: V) V {
    const table_bytes: [16]u8 = table;
    const index_bytes: [16]u8 = index;
    var out: [16]u8 = undefined;
    for (&out, index_bytes) |*byte, i| byte.* = table_bytes[i];
    return out;
}

fn validatePortableLookup(bytes: []const u8) ?Utf8Error {
    return validateLookup(lookupPortable, bytes);
}

/// The hardware lookup kernel this test binary is compiled with, if any.
const test_hardware_kernel: ?Kernel = if (builtin.zig_backend == .stage2_c)
    null
else if (builtin.cpu.arch == .x86_64 and builtin.cpu.has(.x86, .ssse3))
    .x86_ssse3
else if (builtin.cpu.arch == .aarch64 and builtin.cpu.has(.aarch64, .neon))
    .aarch64_neon
else
    null;

/// Whether this machine runs `test_hardware_kernel`. Under the x86 floor the
/// instruction is encodable, but only this machine's CPUID says whether it
/// runs here.
fn hardwareKernelRunsHere() bool {
    if (comptime builtin.cpu.arch == .x86_64 and x86_floor) return hostHasSsse3();
    return true;
}

fn hostHasSsse3() bool {
    var eax: u32 = undefined;
    var ebx: u32 = undefined;
    var ecx: u32 = undefined;
    var edx: u32 = undefined;
    asm volatile ("cpuid"
        : [_] "={eax}" (eax),
          [_] "={ebx}" (ebx),
          [_] "={ecx}" (ecx),
          [_] "={edx}" (edx),
        : [_] "{eax}" (@as(u32, 1)),
          [_] "{ecx}" (@as(u32, 0)),
    );
    return ecx & (1 << 9) != 0;
}

fn sameResult(a: ?Utf8Error, b: ?Utf8Error) bool {
    if (a == null or b == null) return a == null and b == null;
    return a.?.index == b.?.index and a.?.problem == b.?.problem;
}

/// `validate`'s result specified independently of every kernel, by the byte
/// ranges of Unicode Table 3-7 and the problem order `validate` documents.
fn referenceResult(bytes: []const u8) ?Utf8Error {
    var i: usize = 0;
    while (i < bytes.len) {
        const lead = bytes[i];
        const len: usize = switch (lead) {
            0x00...0x7F => 1,
            0xC0...0xDF => 2,
            0xE0...0xEF => 3,
            0xF0...0xF7 => 4,
            else => return .{ .index = i, .problem = .InvalidStartByte },
        };
        if (i + len > bytes.len) return .{ .index = i, .problem = .UnexpectedEndOfSequence };
        for (bytes[i + 1 .. i + len]) |byte| {
            if (byte & 0xC0 != 0x80) return .{ .index = i, .problem = .ExpectedContinuation };
        }
        if (len > 1) {
            const second = bytes[i + 1];
            const problem: ?Utf8ByteProblem = switch (lead) {
                0xC0, 0xC1 => .OverlongEncoding,
                0xE0 => if (second < 0xA0) .OverlongEncoding else null,
                0xED => if (second >= 0xA0) .EncodesSurrogateHalf else null,
                0xF0 => if (second < 0x90) .OverlongEncoding else null,
                0xF4 => if (second >= 0x90) .CodepointTooLarge else null,
                0xF5...0xF7 => .CodepointTooLarge,
                else => null,
            };
            if (problem) |p| return .{ .index = i, .problem = p };
        }
        i += len;
    }
    return null;
}

/// Checks, separately, that every kernel matches the independent reference
/// (and agrees with `std.unicode.utf8ValidateSlice` on whether `bytes` is
/// well-formed), and that the vector kernels match the DFA exactly.
fn expectKernelsAgree(bytes: []const u8) error{TestUnexpectedResult}!void {
    const reference = referenceResult(bytes);
    if ((reference == null) != std.unicode.utf8ValidateSlice(bytes)) {
        std.debug.print("the reference disagrees with std.unicode on {x}\n", .{bytes});
        return error.TestUnexpectedResult;
    }
    const dfa = validateDfa(bytes);
    if (!sameResult(reference, dfa)) {
        std.debug.print("the DFA disagrees with the reference on {x}\n", .{bytes});
        return error.TestUnexpectedResult;
    }
    const portable = validatePortableLookup(bytes);
    if (!sameResult(dfa, portable)) {
        std.debug.print("portable lookup disagrees with the DFA on {x}\n", .{bytes});
        return error.TestUnexpectedResult;
    }
    if (!sameResult(reference, portable)) {
        std.debug.print("portable lookup disagrees with the reference on {x}\n", .{bytes});
        return error.TestUnexpectedResult;
    }
    if (comptime test_hardware_kernel) |kernel| {
        if (hardwareKernelRunsHere()) {
            const hardware = validateWith(kernel, bytes);
            if (!sameResult(dfa, hardware) or !sameResult(reference, hardware)) {
                std.debug.print("{s} disagrees with the DFA or the reference on {x}\n", .{ @tagName(kernel), bytes });
                return error.TestUnexpectedResult;
            }
        }
    }
}

/// Unicode Table 3-7 as transitions between the states `dfa_rows` uses.
fn table37NextState(state: u64, byte: u8) u64 {
    return switch (state) {
        1 => 1,
        0 => switch (byte) {
            0x00...0x7F => 0,
            0xC2...0xDF => 2,
            0xE0 => 4,
            0xE1...0xEC, 0xEE...0xEF => 3,
            0xED => 5,
            0xF0 => 6,
            0xF1...0xF3 => 7,
            0xF4 => 8,
            else => 1,
        },
        2 => if (byte >= 0x80 and byte <= 0xBF) 0 else 1,
        3 => if (byte >= 0x80 and byte <= 0xBF) 2 else 1,
        4 => if (byte >= 0xA0 and byte <= 0xBF) 2 else 1,
        5 => if (byte >= 0x80 and byte <= 0x9F) 2 else 1,
        6 => if (byte >= 0x90 and byte <= 0xBF) 3 else 1,
        7 => if (byte >= 0x80 and byte <= 0xBF) 3 else 1,
        8 => if (byte >= 0x80 and byte <= 0x8F) 3 else 1,
        else => unreachable, // the DFA has states 0 through 8
    };
}

test "utf8: DFA rows encode Unicode Table 3-7 for every byte and state" {
    for (0..256) |byte| {
        for (0..9) |state| {
            const next = (dfa_rows[byte] >> @intCast(state * 6)) & dfa_state_mask;
            try testing.expectEqual(table37NextState(state, @intCast(byte)) * 6, next);
            try testing.expectEqual(next, dfaStep(state * 6, @intCast(byte)) & dfa_state_mask);
        }
    }
}

test "utf8: kernels are selected from the CPU features of the target" {
    const x86_64 = std.Target.Cpu.Arch.x86_64;
    try testing.expectEqual(Kernel.dfa, Kernel.forCpu(std.Target.x86.cpu.x86_64.toCpu(x86_64)));
    try testing.expectEqual(Kernel.x86_ssse3, Kernel.forCpu(std.Target.x86.cpu.x86_64_v3.toCpu(x86_64)));
    try testing.expectEqual(Kernel.aarch64_neon, Kernel.forCpu(std.Target.aarch64.cpu.generic.toCpu(.aarch64)));
    try testing.expectEqual(Kernel.dfa, Kernel.forCpu(std.Target.wasm.cpu.generic.toCpu(.wasm32)));
    var simd128 = std.Target.wasm.cpu.generic.toCpu(.wasm32);
    simd128.features.addFeature(@backingInt(std.Target.wasm.Feature.simd128));
    try testing.expectEqual(Kernel.wasm_simd128, Kernel.forCpu(simd128));
    try testing.expectEqual(Kernel.dfa, Kernel.forCpu(std.Target.arm.cpu.generic.toCpu(.arm)));
}

/// Writes well-formed text to `out`, cycling through `pattern` and padding with
/// ASCII where the next sequence does not fit.
fn fillValid(out: []u8, pattern: []const u8) void {
    var i: usize = 0;
    var p: usize = 0;
    while (i < out.len) {
        const width: usize = switch (pattern[p]) {
            0x00...0x7F => 1,
            0xC0...0xDF => 2,
            0xE0...0xEF => 3,
            else => 4,
        };
        if (width <= out.len - i) {
            @memcpy(out[i..][0..width], pattern[p..][0..width]);
            i += width;
            p = (p + width) % pattern.len;
        } else {
            out[i] = 'a';
            i += 1;
        }
    }
}

const fill_patterns = [_][]const u8{
    "abcdefgh",
    "ab\u{e9}cd\u{fc}ef",
    "\u{e9}\u{fc}\u{3b1}\u{434}",
    "\u{6f22}\u{5b57}\u{20ac}",
    "\u{1f389}\u{1f600}",
    "\u{e9}\u{6f22}\u{1f389}x",
};

const error_templates = [_][]const u8{
    "\x80",             "\xbf",             "\xf8",             "\xff",
    "\xc0\x80",         "\xc1\xbf",         "\xc2\x41",         "\xc2",
    "\xe0\x80\x80",     "\xe0\x9f\xbf",     "\xed\xa0\x80",     "\xe2\x82\x41",
    "\xe2\x41\x41",     "\xe2\x82",         "\xe2\x41",         "\xf0\x8f\xbf\xbf",
    "\xf4\x90\x80\x80", "\xf5\x80\x80\x80", "\xf1\x80\x80\x41", "\xf0\x9f\x98",
    "\xf4\x8f\xbf",     "\xc2\xc2",         "\xe2\xe2",         "\x41\x80\x80",
};

test "utf8: kernels agree with each other and the reference on every 1- and 2-byte input" {
    var buf: [40]u8 = undefined;
    for (0..256) |b0| {
        try expectKernelsAgree(&.{@intCast(b0)});
        for (0..256) |b1| {
            const pair = [2]u8{ @intCast(b0), @intCast(b1) };
            try expectKernelsAgree(&pair);
            // The same bytes after valid text that fills a lookup step.
            fillValid(buf[0..38], fill_patterns[(b0 + b1) % fill_patterns.len]);
            buf[38] = pair[0];
            buf[39] = pair[1];
            try expectKernelsAgree(&buf);
        }
    }
}

test "utf8: kernels agree with each other and the reference on every 3-byte input led by E0-F7" {
    // The bytes as a whole input, and after text that fills one 16-byte block
    // but ends one byte short of it, so the sequence crosses into the final
    // window.
    var buf: [18]u8 = undefined;
    fillValid(buf[0..15], fill_patterns[1]);
    for (0xE0..0xF8) |b0| {
        for (0..256) |b1| {
            for (0..256) |b2| {
                const triple = [3]u8{ @intCast(b0), @intCast(b1), @intCast(b2) };
                try expectKernelsAgree(&triple);
                @memcpy(buf[15..18], &triple);
                try expectKernelsAgree(&buf);
            }
        }
    }
}

test "utf8: kernels agree with each other and the reference with an error at every offset of every length up to 100" {
    var buf: [100]u8 = undefined;
    for (fill_patterns) |pattern| {
        for (0..buf.len + 1) |len| {
            fillValid(buf[0..len], pattern);
            try testing.expectEqual(@as(?Utf8Error, null), validateDfa(buf[0..len]));
            try expectKernelsAgree(buf[0..len]);
            for (error_templates) |template| {
                if (template.len > len) continue;
                for (0..len - template.len + 1) |pos| {
                    fillValid(buf[0..pos], pattern);
                    @memcpy(buf[pos..][0..template.len], template);
                    fillValid(buf[pos + template.len .. len], pattern);
                    try expectKernelsAgree(buf[0..len]);
                }
            }
        }
    }
}

test "utf8: kernels agree with each other and the reference on random long inputs with errors at every offset modulo 16" {
    var prng = std.Random.DefaultPrng.init(0x13025);
    const random = prng.random();
    const alphabet = [_]u8{ 0x00, 0x41, 0x7F, 0x80, 0x8F, 0x90, 0x9F, 0xA0, 0xBF, 0xC0, 0xC1, 0xC2, 0xDF, 0xE0, 0xE1, 0xEC, 0xED, 0xEE, 0xEF, 0xF0, 0xF1, 0xF3, 0xF4, 0xF5, 0xF7, 0xF8, 0xFF };
    var buf: [600]u8 = undefined;
    for (0..3000) |round| {
        const len = 32 + random.uintLessThan(usize, buf.len - 32 + 1);
        fillValid(buf[0..len], fill_patterns[round % fill_patterns.len]);
        try expectKernelsAgree(buf[0..len]);
        // One injected byte, at an offset chosen so every residue modulo 16
        // and both sides of each block boundary occur.
        const pos = (round * 7 + round / 16) % len;
        const saved = buf[pos];
        buf[pos] = alphabet[random.uintLessThan(usize, alphabet.len)];
        try expectKernelsAgree(buf[0..len]);
        // A truncated final sequence.
        buf[pos] = saved;
        const leads = [_]u8{ 0xC3, 0xE6, 0xF0 };
        const lead = leads[round % 3];
        buf[len - 1] = lead;
        try expectKernelsAgree(buf[0..len]);
        // Random bytes from the boundary alphabet, mostly ASCII.
        for (buf[0..len]) |*byte| {
            byte.* = if (random.uintLessThan(u32, 24) != 0) 'a' else alphabet[random.uintLessThan(usize, alphabet.len)];
        }
        try expectKernelsAgree(buf[0..len]);
    }
}

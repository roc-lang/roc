//! Tests for the range-prove LIR pass: checks that a dominating margin guard
//! proves away are rewritten to proven forms, and checks the prover cannot
//! justify stay fully checked.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");

/// A miniature decode fastloop: a while loop whose margin guard dominates an
/// eight-byte read and cursor arithmetic, mirroring the shape the pass exists
/// to prove. The 16-byte margin makes the read's bounds test, the `?? 0`
/// fallback, and the advance's underflow check all provably dead.
fn fastloopApp(comptime margin: []const u8) []const u8 {
    return "decode : List(U8), U64 -> U64\n" ++
        "decode = |input, start| {\n" ++
        "    in_len = List.len(input)\n" ++
        "    var $in_next = start\n" ++
        "    var $acc = 0.U64\n" ++
        "    while $in_next + " ++ margin ++ " <= in_len {\n" ++
        "        word = U64.from_le_bytes(input, $in_next) ?? 0\n" ++
        "        $acc = $acc.bitwise_or(word)\n" ++
        "        $in_next = $in_next + 7 - $acc.shr_zf_wrap(3).bitwise_and(7)\n" ++
        "    }\n" ++
        "    $acc\n" ++
        "}\n" ++
        "\n" ++
        "main! : List(Str) => Try({}, [Exit(I8), ..])\n" ++
        "main! = |args| {\n" ++
        "    n = decode(Str.to_utf8(Str.join_with(args, \",\")), 0)\n" ++
        "    echo!(Str.inspect(n))\n" ++
        "    Ok({})\n" ++
        "}\n";
}

const Shape = struct {
    found_decode_proc: bool = false,
    plus_checked: usize = 0,
    minus_checked: usize = 0,
    is_lt: usize = 0,
    is_gt: usize = 0,
    switches: usize = 0,
};

var counted: Shape = .{};

fn countDecodeShape(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    counted = .{};
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 22);
    defer gpa.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), &writer);
        const text = writer.buffered();
        if (std.mem.count(u8, text, "num_from_le_bytes_unchecked") == 0) continue;
        if (std.c.getenv("RANGE_PROVE_DUMP") != null) {
            std.debug.print("\n===== decode proc =====\n{s}\n", .{text});
        }
        counted = .{
            .found_decode_proc = true,
            .plus_checked = std.mem.count(u8, text, "num_int_add_crash_on_overflow"),
            .minus_checked = std.mem.count(u8, text, "num_int_sub_crash_on_overflow"),
            .is_lt = std.mem.count(u8, text, "num_is_lt("),
            .is_gt = std.mem.count(u8, text, "num_is_gt("),
            .switches = std.mem.count(u8, text, "switch "),
        };
        return;
    }
}

test "a 16-byte margin guard proves away the read's bounds test and the advance's checks" {
    try harness.expectLirInspectionWithOptions(
        fastloopApp("16"),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countDecodeShape,
    );
    try std.testing.expect(counted.found_decode_proc);
    // The margin test itself keeps its overflow check: the loop entry cursor
    // is caller-controlled, so `start + 16` may genuinely overflow.
    try std.testing.expectEqual(@as(usize, 1), counted.plus_checked);
    // The advance's `+ 7` was proven (cursor is at most len - 16) and its
    // subtraction was proven (the subtrahend is masked to at most 7).
    try std.testing.expectEqual(@as(usize, 0), counted.minus_checked);
    // Both halves of the read's bounds test folded to constants.
    try std.testing.expectEqual(@as(usize, 0), counted.is_lt);
    try std.testing.expectEqual(@as(usize, 0), counted.is_gt);
    // Only the loop's own margin switch survives; the bounds-test switch and
    // the `?? 0` fallback switch folded to their safe arms.
    try std.testing.expectEqual(@as(usize, 1), counted.switches);
}

test "a margin too small for the read keeps every bounds check" {
    try harness.expectLirInspectionWithOptions(
        fastloopApp("4"),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countDecodeShape,
    );
    try std.testing.expect(counted.found_decode_proc);
    // A four-byte margin cannot justify an eight-byte read: the wrapper's
    // length test and bound test both survive, as do their switches.
    try std.testing.expect(counted.is_lt >= 1);
    try std.testing.expect(counted.is_gt >= 1);
    try std.testing.expectEqual(@as(usize, 3), counted.switches);
    // The margin add still cannot be proven overflow-free: the entry cursor
    // is caller-controlled. The advance's `+ 7` is provable even here: the
    // margin fact survives the fallback merge (both edges carry it), and a
    // cursor bounded by a list length cannot exceed a signed 64-bit count,
    // so adding seven cannot wrap. The advance's subtraction is provable
    // too: control past the surviving checked add means its result is exact
    // and at least seven, which covers the masked subtrahend.
    try std.testing.expectEqual(@as(usize, 1), counted.plus_checked);
    try std.testing.expectEqual(@as(usize, 0), counted.minus_checked);
}

test "the pass leaves the shape untouched when disabled" {
    try harness.expectLirInspectionWithOptions(
        fastloopApp("16"),
        .{ .inline_mode = .wrappers },
        countDecodeShape,
    );
    try std.testing.expect(counted.found_decode_proc);
    try std.testing.expectEqual(@as(usize, 2), counted.plus_checked);
    try std.testing.expectEqual(@as(usize, 1), counted.minus_checked);
    try std.testing.expectEqual(@as(usize, 1), counted.is_lt);
    try std.testing.expectEqual(@as(usize, 1), counted.is_gt);
    try std.testing.expectEqual(@as(usize, 3), counted.switches);
}

const ArithmeticShape = struct {
    found: bool = false,
    add_wrap: usize = 0,
    add_crash: usize = 0,
    add_overflows: usize = 0,
    add_proven: usize = 0,
    sub_crash: usize = 0,
    sub_proven: usize = 0,
    mul_crash: usize = 0,
    mul_proven: usize = 0,
    switches: usize = 0,
    crashes: usize = 0,
    literal_three: usize = 0,
};

const ArithmeticSelection = enum {
    masked_mul,
    two_masked_add,
    overflow_predicate,
    folded_try,
    same_sign_chain,
    mixed_sign_chain,
    overflow_crash,
};

var arithmetic_shape: ArithmeticShape = .{};
var arithmetic_selection: ArithmeticSelection = .masked_mul;

fn countArithmeticShape(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    arithmetic_shape = .{};
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 20);
    defer gpa.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), &writer);
        const text = writer.buffered();
        if (std.c.getenv("RANGE_PROVE_DUMP_ALL") != null) std.debug.print("\n===== arithmetic candidate =====\n{s}\n", .{text});
        const selected = switch (arithmetic_selection) {
            .masked_mul => std.mem.count(u8, text, "num_int_mul_") > 0 and std.mem.count(u8, text, "num_bitwise_and") == 1,
            .two_masked_add => std.mem.count(u8, text, "num_int_add_") > 0 and std.mem.count(u8, text, "num_bitwise_and") == 2,
            .overflow_predicate => std.mem.count(u8, text, "num_int_add_overflows") > 0,
            .folded_try => std.mem.count(u8, text, "num_bitwise_and") == 1 and std.mem.count(u8, text, "num_int_add_proven_cannot_overflow") > 0,
            .same_sign_chain => std.mem.count(u8, text, "num_int_add_wrap") > 0 and std.mem.count(u8, text, "literal 3") > 0,
            .mixed_sign_chain => std.mem.count(u8, text, "num_int_add_crash_on_overflow") > 0 and std.mem.count(u8, text, "num_int_sub_") > 0,
            .overflow_crash => std.mem.count(u8, text, "num_bitwise_and") > 0 and std.mem.count(u8, text, "crash") > 0,
        };
        if (!selected) continue;
        arithmetic_shape = .{
            .found = true,
            .add_wrap = std.mem.count(u8, text, "num_int_add_wrap"),
            .add_crash = std.mem.count(u8, text, "num_int_add_crash_on_overflow"),
            .add_overflows = std.mem.count(u8, text, "num_int_add_overflows"),
            .add_proven = std.mem.count(u8, text, "num_int_add_proven_cannot_overflow"),
            .sub_crash = std.mem.count(u8, text, "num_int_sub_crash_on_overflow"),
            .sub_proven = std.mem.count(u8, text, "num_int_sub_proven_cannot_overflow"),
            .mul_crash = std.mem.count(u8, text, "num_int_mul_crash_on_overflow"),
            .mul_proven = std.mem.count(u8, text, "num_int_mul_proven_cannot_overflow"),
            .switches = std.mem.count(u8, text, "switch "),
            .crashes = std.mem.count(u8, text, "crash"),
            .literal_three = std.mem.count(u8, text, "literal 3"),
        };
        if (std.c.getenv("RANGE_PROVE_DUMP") != null) std.debug.print("\n===== arithmetic proc =====\n{s}\n", .{text});
        return;
    }
}

fn arithmeticApp(comptime body: []const u8, comptime call: []const u8) []const u8 {
    return body ++
        "\nmain! : List(Str) => Try({}, [Exit(I8), ..])\n" ++
        "main! = |_args| {\n" ++
        "    echo!(Str.inspect(" ++ call ++ "))\n" ++
        "    Ok({})\n" ++
        "}\n";
}

const MeetShape = struct {
    found: bool = false,
    is_lt: usize = 0,
    get_unsafe: usize = 0,
    mul_crash: usize = 0,
    mul_proven: usize = 0,
};

const MeetSelection = enum {
    hashed_read,
    scaled_byte,
};

var meet_shape: MeetShape = .{};
var meet_selection: MeetSelection = .hashed_read;

fn countMeetShape(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    meet_shape = .{};
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 20);
    defer gpa.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), &writer);
        const text = writer.buffered();
        if (std.mem.count(u8, text, "list_get_unsafe") == 0) continue;
        const selected = switch (meet_selection) {
            .hashed_read => std.mem.count(u8, text, "num_shift_right_zf_by") > 0,
            .scaled_byte => std.mem.count(u8, text, "num_int_mul_") > 0,
        };
        if (!selected) continue;
        meet_shape = .{
            .found = true,
            .is_lt = std.mem.count(u8, text, "num_is_lt("),
            .get_unsafe = std.mem.count(u8, text, "list_get_unsafe"),
            .mul_crash = std.mem.count(u8, text, "num_int_mul_crash_on_overflow"),
            .mul_proven = std.mem.count(u8, text, "num_int_mul_proven_cannot_overflow"),
        };
        if (std.c.getenv("RANGE_PROVE_DUMP") != null) std.debug.print("\n===== meet proc =====\n{s}\n", .{text});
        return;
    }
}

test "a hash bounded by a guard on entry and a shift on the back edge proves its table read" {
    meet_selection = .hashed_read;
    // The loop parameter meets two different derivations of the same
    // constant bound: the entry guard and the shift's range, whose amount
    // is a narrowing of a literal difference.
    try harness.expectLirInspectionWithOptions(
        arithmeticApp(
            "walk : List(U16), U64, List(U8), U64 -> U64\n" ++
                "walk = |tab, h0, input, n| {\n" ++
                "    if List.len(tab) < 256 or h0 >= 256 {\n" ++
                "        return 0\n" ++
                "    } else {\n" ++
                "    }\n" ++
                "    var $h = h0\n" ++
                "    var $i = 0\n" ++
                "    var $acc = 0.U64\n" ++
                "    while $i < n {\n" ++
                "        $acc = $acc.plus_wrap((List.get(tab, $h) ?? 0).to_u64())\n" ++
                "        seq = U32.from_le_bytes(input, $i) ?? 0\n" ++
                "        $h = seq.times_wrap(0x1E35A7BD).shr_zf_wrap((32.U64 - 8).to_u8_wrap()).to_u64()\n" ++
                "        $i = $i + 1\n" ++
                "    }\n" ++
                "    $acc\n" ++
                "}\n",
            "walk([], 0, Str.to_utf8(Str.join_with(_args, \",\")), List.len(_args))",
        ),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countMeetShape,
    );
    try std.testing.expect(meet_shape.found);
    try std.testing.expectEqual(@as(usize, 1), meet_shape.get_unsafe);
    // The entry guard's length test, the loop condition, and the word read's
    // short-input test remain; the table read's bounds test is proven away.
    try std.testing.expectEqual(@as(usize, 3), meet_shape.is_lt);
}

test "a byte merged from a read and a literal keeps its byte range for the overflow proof" {
    meet_selection = .scaled_byte;
    try harness.expectLirInspectionWithOptions(
        arithmeticApp(
            "cost : List(U8), U64, U64 -> U64\ncost = |bits, slot, base| base + (List.get(bits, slot) ?? 0).to_u64() * 16\n",
            "cost(Str.to_utf8(Str.join_with(_args, \",\")), List.len(_args), 3)",
        ),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countMeetShape,
    );
    try std.testing.expect(meet_shape.found);
    try std.testing.expectEqual(@as(usize, 0), meet_shape.mul_crash);
    try std.testing.expectEqual(@as(usize, 1), meet_shape.mul_proven);
}

const SearchShape = struct {
    found: bool = false,
    is_lt: usize = 0,
    get_unsafe: usize = 0,
    folded: usize = 0,
};

var search_shape: SearchShape = .{};

fn countSearchShape(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    search_shape = .{};
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 20);
    defer gpa.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), &writer);
        const text = writer.buffered();
        if (std.mem.count(u8, text, "list_get_unsafe") == 0 or std.mem.count(u8, text, "i16_to_u64_wrap") == 0) continue;
        search_shape = .{
            .found = true,
            .is_lt = std.mem.count(u8, text, "num_is_lt("),
            .get_unsafe = std.mem.count(u8, text, "list_get_unsafe"),
            .folded = std.mem.count(u8, text, "tag v1 d1"),
        };
        if (std.c.getenv("RANGE_PROVE_DUMP") != null) std.debug.print("\n===== search proc =====\n{s}\n", .{text});
        return;
    }
}

const real_search =
    "Match : { length : U64, offset : U64 }\n" ++
    "ext : List(U8), U64, U64, U64, U64 -> U64\n" ++
    "ext = |input, a, b, start_len, max_len| {\n" ++
    "    var $len = start_len\n" ++
    "    while $len < max_len and (List.get(input, a.plus_wrap($len)) ?? 0) == (List.get(input, b.plus_wrap($len)) ?? 1) {\n" ++
    "        $len = $len + 1\n" ++
    "    }\n" ++
    "    $len\n" ++
    "}\n" ++
    "mi : U64, I16 -> U64\n" ++
    "mi = |in_base, node| in_base.to_i64_wrap().plus_wrap(node.to_i64()).to_u64_wrap()\n" ++
    "longest_match : List(I16), I16, I16, U64, List(U8), U64, U64, U64, U64, U64 -> Try(Match, [CompressBug])\n" ++
    "longest_match = |next_tab, cur_node3, cur_node4, in_base, input, in_next, best_len_in, max_len, nice_len, max_search_depth| {\n" ++
    "    if List.len(input) < 4 {\n" ++
    "        return Err(CompressBug)\n" ++
    "    } else {\n" ++
    "    }\n" ++
    "    # The chain table is one window long, so every masked chain index is in\n" ++
    "    # range. Establishing that once here lets the bounds test on each chain\n" ++
    "    # read fold away instead of running per candidate. The node is masked\n" ++
    "    # before it is widened so the next read's address needs only the mask\n" ++
    "    # on top of the load, which is the loop's critical path.\n" ++
    "    if List.len(next_tab) < 32768 {\n" ++
    "        return Err(CompressBug)\n" ++
    "    } else {\n" ++
    "    }\n" ++
    "    cur_pos = in_next - in_base\n" ++
    "    cutoff = cur_pos.to_i32_wrap() - 32768\n" ++
    "\n" ++
    "    var $best_len = best_len_in\n" ++
    "    var $best_match_at = in_next\n" ++
    "\n" ++
    "    seq4 = U32.from_le_bytes(input, in_next) ?? 0\n" ++
    "    var $node4 = cur_node4\n" ++
    "    var $depth = max_search_depth\n" ++
    "    var $done = 0.U64\n" ++
    "\n" ++
    "    if $best_len < 4 {\n" ++
    "        if cur_node3.to_i32() <= cutoff {\n" ++
    "            $done = 1\n" ++
    "        } else {\n" ++
    "            if $best_len < 3 {\n" ++
    "                match_at = mi(in_base, cur_node3)\n" ++
    "                if (U32.from_le_bytes(input, match_at) ?? 0).bitwise_and(0xFFFFFF)\n" ++
    "                    == seq4.bitwise_and(0xFFFFFF) {\n" ++
    "                    $best_len = 3\n" ++
    "                    $best_match_at = match_at\n" ++
    "                } else {\n" ++
    "                }\n" ++
    "            } else {\n" ++
    "            }\n" ++
    "\n" ++
    "            if $node4.to_i32() <= cutoff {\n" ++
    "                $done = 1\n" ++
    "            } else {\n" ++
    "                # Walk the chain until four bytes agree.\n" ++
    "                var $found_at = 0.U64\n" ++
    "                while True {\n" ++
    "                    match_at = mi(in_base, $node4)\n" ++
    "                    if (U32.from_le_bytes(input, match_at) ?? 0) == seq4 {\n" ++
    "                        $found_at = match_at\n" ++
    "                        break\n" ++
    "                    } else {\n" ++
    "                    }\n" ++
    "                    $node4 = List.get(next_tab, $node4.bitwise_and(32767).to_u64_wrap()) ?? 0\n" ++
    "                    $depth = $depth.minus_wrap(1)\n" ++
    "                    if $node4.to_i32() <= cutoff or $depth == 0 {\n" ++
    "                        $done = 1\n" ++
    "                        break\n" ++
    "                    } else {\n" ++
    "                    }\n" ++
    "                }\n" ++
    "\n" ++
    "                if $done == 0 {\n" ++
    "                    $best_match_at = $found_at\n" ++
    "                    $best_len = ext(input, in_next, $found_at, 4, max_len)\n" ++
    "                    if $best_len >= nice_len {\n" ++
    "                        $done = 1\n" ++
    "                    } else {\n" ++
    "                        $node4 = List.get(next_tab, $node4.bitwise_and(32767).to_u64_wrap()) ?? 0\n" ++
    "                        $depth = $depth.minus_wrap(1)\n" ++
    "                        if $node4.to_i32() <= cutoff or $depth == 0 {\n" ++
    "                            $done = 1\n" ++
    "                        } else {\n" ++
    "                        }\n" ++
    "                    }\n" ++
    "                } else {\n" ++
    "                }\n" ++
    "            }\n" ++
    "        }\n" ++
    "    } else {\n" ++
    "        if $node4.to_i32() <= cutoff or $best_len >= nice_len {\n" ++
    "            $done = 1\n" ++
    "        } else {\n" ++
    "        }\n" ++
    "    }\n" ++
    "\n" ++
    "    # Now look only for matches longer than the one in hand.\n" ++
    "    while $done == 0 {\n" ++
    "        var $cand_at = 0.U64\n" ++
    "        while True {\n" ++
    "            match_at = mi(in_base, $node4)\n" ++
    "            # The four bytes ending just past the current best length\n" ++
    "            # are what a longer match must agree on, so check them\n" ++
    "            # before anything else.\n" ++
    "            # Wrapping arithmetic: positions are far below 2^63, and a checked\n" ++
    "            # add or subtract would put an overflow branch on every candidate.\n" ++
    "            if (U32.from_le_bytes(input, match_at.plus_wrap($best_len).minus_wrap(3)) ?? 0)\n" ++
    "                == (U32.from_le_bytes(input, in_next.plus_wrap($best_len).minus_wrap(3)) ?? 0)\n" ++
    "                and (U32.from_le_bytes(input, match_at) ?? 0)\n" ++
    "                    == (U32.from_le_bytes(input, in_next) ?? 0) {\n" ++
    "                $cand_at = match_at\n" ++
    "                break\n" ++
    "            } else {\n" ++
    "            }\n" ++
    "            $node4 = List.get(next_tab, $node4.bitwise_and(32767).to_u64_wrap()) ?? 0\n" ++
    "            $depth = $depth.minus_wrap(1)\n" ++
    "            if $node4.to_i32() <= cutoff or $depth == 0 {\n" ++
    "                $done = 1\n" ++
    "                break\n" ++
    "            } else {\n" ++
    "            }\n" ++
    "        }\n" ++
    "\n" ++
    "        if $done == 0 {\n" ++
    "            len = ext(input, in_next, $cand_at, 4, max_len)\n" ++
    "            if len > $best_len {\n" ++
    "                $best_len = len\n" ++
    "                $best_match_at = $cand_at\n" ++
    "                if $best_len >= nice_len {\n" ++
    "                    $done = 1\n" ++
    "                } else {\n" ++
    "                }\n" ++
    "            } else {\n" ++
    "            }\n" ++
    "            if $done == 0 {\n" ++
    "                $node4 = List.get(next_tab, $node4.bitwise_and(32767).to_u64_wrap()) ?? 0\n" ++
    "                $depth = $depth.minus_wrap(1)\n" ++
    "                if $node4.to_i32() <= cutoff or $depth == 0 {\n" ++
    "                    $done = 1\n" ++
    "                } else {\n" ++
    "                }\n" ++
    "            } else {\n" ++
    "            }\n" ++
    "        } else {\n" ++
    "        }\n" ++
    "    }\n" ++
    "\n" ++
    "    Ok({ length: $best_len, offset: in_next - $best_match_at })\n" ++
    "}\n" ++
    "\n" ++
    "## Insert `count` positions into the tables without searching them.\n" ++
    "\n" ++
    "main! : List(Str) => Try({}, [Exit(I8), ..])\n" ++
    "main! = |_args| {\n" ++
    "    r = longest_match([], 1, 2, 0, Str.to_utf8(Str.join_with(_args, \",\")), 9, 3, 20, 16, 8) ?? { length: 0, offset: 0 }\n" ++
    "    echo!(Str.inspect(r.length))\n" ++
    "    Ok({})\n" ++
    "}\n";

// The hash-chain search of a lazy compressor: a first walk for a four-byte
// match, then a restart loop that walks the chain for longer matches. The
// loops lower to joins that jump among one another, so the chain table's
// length guard reaches the later walks only through several persisted meets
// across walk regions.
test "the lazy matchfinder's search proves every chain read against its entry guard" {
    try harness.expectLirInspectionWithOptions(
        real_search,
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countSearchShape,
    );
    try std.testing.expect(search_shape.found);
    try std.testing.expectEqual(@as(usize, 4), search_shape.get_unsafe);
    // Only the two entry guards and the two loop conditions compare with
    // `<`; every chain read's bounds test is proven away.
    try std.testing.expectEqual(@as(usize, 4), search_shape.is_lt);
}

const real_skip =
    "State : { hash_tab : List(I16), in_cur_base : U64, next_hash : U64 }\n" ++
    "required_nbytes : U64\n" ++
    "required_nbytes = 5\n" ++
    "table_size : U64\n" ++
    "table_size = 65536\n" ++
    "hash_order : U64\n" ++
    "hash_order = 15\n" ++
    "lz_hash : U32, U64 -> U64\n" ++
    "lz_hash = |seq, num_bits|\n" ++
    "    seq.times_wrap(0x1E35A7BD).shr_zf_wrap((32 - num_bits).to_u8_wrap()).to_u64()\n" ++
    "rebase : List(I16) -> Try(List(I16), [CompressBug])\n" ++
    "rebase = |table0| {\n" ++
    "    var $table = table0\n" ++
    "    n = List.len($table)\n" ++
    "    var $i = 0.U64\n" ++
    "    while $i < n {\n" ++
    "        v = List.get($table, $i) ?? 0\n" ++
    "        slid = (-32768).bitwise_or(v.bitwise_and(v.shr_wrap(15).bitwise_not()))\n" ++
    "        $table = match List.set($table, $i, slid) {\n" ++
    "            Ok(next) => next\n" ++
    "            Err(_) => return Err(CompressBug)\n" ++
    "        }\n" ++
    "        $i = $i + 1\n" ++
    "    }\n" ++
    "    Ok($table)\n" ++
    "}\n" ++
    "skip_bytes : List(I16), U64, U64, List(U8), U64, U64, U64 -> Try(State, [CompressBug])\n" ++
    "skip_bytes = |tab_0, base_0, hash_0, input, in_next0, in_end, count| {\n" ++
    "    # The run ends at `end`, and every hash read the loop makes lies within\n" ++
    "    # `required_nbytes` of it. Bounding the run against the input's length\n" ++
    "    # here, and the bucket table's size once after any slide, lets the\n" ++
    "    # range prover discharge the bounds test on every read and bucket\n" ++
    "    # access in the loop, so each byte pays only for the work itself.\n" ++
    "    end = in_next0 + count\n" ++
    "    if end + required_nbytes > in_end {\n" ++
    "        Ok({ hash_tab: tab_0, in_cur_base: base_0, next_hash: hash_0 })\n" ++
    "    } else if in_end > List.len(input) {\n" ++
    "        Err(CompressBug)\n" ++
    "    } else {\n" ++
    "        var $tab = tab_0\n" ++
    "        var $base = base_0\n" ++
    "        var $in_next = in_next0\n" ++
    "        var $cur_pos = ($in_next - base_0).to_i64_wrap()\n" ++
    "        # One slide covers the whole run, since it is bounded by a window.\n" ++
    "        if $cur_pos + count.to_i64_wrap() - 1 >= 32768.to_i64_wrap() {\n" ++
    "            $tab = rebase($tab)?\n" ++
    "            $base = $base + 32768\n" ++
    "            $cur_pos = $cur_pos - 32768.to_i64_wrap()\n" ++
    "        } else {\n" ++
    "        }\n" ++
    "        if List.len($tab) < table_size {\n" ++
    "            return Err(CompressBug)\n" ++
    "        } else {\n" ++
    "        }\n" ++
    "\n" ++
    "        var $hash = hash_0\n" ++
    "        while $in_next < end {\n" ++
    "            slot0 = $hash.bitwise_and(0x7FFF) * 2\n" ++
    "            first = List.get($tab, slot0) ?? 0\n" ++
    "            tab1 = match List.set($tab, slot0 + 1, first) {\n" ++
    "                Ok(next) => next\n" ++
    "                Err(_) => return Err(CompressBug)\n" ++
    "            }\n" ++
    "            $tab = match List.set(tab1, slot0, $cur_pos.to_i16_wrap()) {\n" ++
    "                Ok(next) => next\n" ++
    "                Err(_) => return Err(CompressBug)\n" ++
    "            }\n" ++
    "\n" ++
    "            $in_next = $in_next + 1\n" ++
    "            $hash = lz_hash(U32.from_le_bytes(input, $in_next) ?? 0, hash_order)\n" ++
    "            $cur_pos = $cur_pos.plus_wrap(1)\n" ++
    "        }\n" ++
    "        Ok({ hash_tab: $tab, in_cur_base: $base, next_hash: $hash })\n" ++
    "    }\n" ++
    "}\n" ++
    "\n" ++
    "main! : List(Str) => Try({}, [Exit(I8), ..])\n" ++
    "main! = |_args| {\n" ++
    "    r = skip_bytes([], 0, 3, Str.to_utf8(Str.join_with(_args, \",\")), 1, 90, 8) ?? { hash_tab: [], in_cur_base: 0, next_hash: 0 }\n" ++
    "    echo!(Str.inspect(r.next_hash))\n" ++
    "    Ok({})\n" ++
    "}\n";

const SkipShape = struct { found: bool = false, is_gt: usize = 0, is_lt: usize = 0, folded: usize = 0 };
var skip_shape: SkipShape = .{};

fn countSkipShape(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    skip_shape = .{};
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 20);
    defer gpa.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), &writer);
        const text = writer.buffered();
        if (std.mem.count(u8, text, "num_from_le_bytes_unchecked") == 0 or std.mem.count(u8, text, "list_set") == 0) continue;
        skip_shape = .{
            .found = true,
            .is_gt = std.mem.count(u8, text, "num_is_gt("),
            .is_lt = std.mem.count(u8, text, "num_is_lt("),
            .folded = std.mem.count(u8, text, "tag v1 d1"),
        };
        if (std.c.getenv("RANGE_PROVE_DUMP") != null) std.debug.print("\n===== skip proc =====\n{s}\n", .{text});
        return;
    }
}

// The bucket matchfinder's insert run: a cursor loop whose entry guards bound
// the run against the input and the bucket table against its size. Its
// hash read and three bucket accesses per byte all prove from those guards.
test "the bucket insert run proves every table access and hash read against its entry guards" {
    try harness.expectLirInspectionWithOptions(
        real_skip,
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countSkipShape,
    );
    try std.testing.expect(skip_shape.found);
    // The two entry guards are the only `>` compares left; the loop
    // conditions (the run's and the slide's) are the only `<` compares.
    try std.testing.expectEqual(@as(usize, 2), skip_shape.is_gt);
    try std.testing.expectEqual(@as(usize, 3), skip_shape.is_lt);
}

test "checked multiply is discharged from a masked range" {
    arithmetic_selection = .masked_mul;
    try harness.expectLirInspectionWithOptions(
        arithmeticApp("calc : U8 -> U8\ncalc = |a| a.bitwise_and(15) * 4\n", "calc(List.len(_args).to_u8_wrap())"),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countArithmeticShape,
    );
    try std.testing.expect(arithmetic_shape.found);
    try std.testing.expectEqual(@as(usize, 0), arithmetic_shape.mul_crash);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.mul_proven);
}

test "two bounded variables discharge checked add" {
    arithmetic_selection = .two_masked_add;
    try harness.expectLirInspectionWithOptions(
        arithmeticApp(
            "calc : U8, U8 -> U8\ncalc = |a, b| a.bitwise_and(15) + b.bitwise_and(31)\n",
            "calc(List.len(_args).to_u8_wrap(), List.len(_args).to_u8_wrap())",
        ),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countArithmeticShape,
    );
    try std.testing.expect(arithmetic_shape.found);
    try std.testing.expectEqual(@as(usize, 0), arithmetic_shape.add_crash);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.add_proven);
}

test "a false overflow-predicate edge proves the matching wrap exact" {
    arithmetic_selection = .overflow_predicate;
    try harness.expectLirInspectionWithOptions(
        arithmeticApp(
            "calc : U8, U8 -> U8\ncalc = |a, b| match a.plus_try(b) { Ok(value) => value, Err(Overflow) => 0 }\n",
            "calc(List.len(_args).to_u8_wrap(), List.len(_args).to_u8_wrap())",
        ),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countArithmeticShape,
    );
    try std.testing.expect(arithmetic_shape.found);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.add_overflows);
    try std.testing.expectEqual(@as(usize, 0), arithmetic_shape.add_wrap);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.add_proven);
}

test "a provably false overflow predicate folds its branch" {
    arithmetic_selection = .folded_try;
    try harness.expectLirInspectionWithOptions(
        arithmeticApp(
            "calc : U8 -> U8\ncalc = |a| {\n    value = a.bitwise_and(15)\n    if value.plus_overflows(1) { 0 } else { value.plus_wrap(1) }\n}\n",
            "calc(List.len(_args).to_u8_wrap())",
        ),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countArithmeticShape,
    );
    try std.testing.expect(arithmetic_shape.found);
    try std.testing.expectEqual(@as(usize, 0), arithmetic_shape.add_overflows);
    try std.testing.expectEqual(@as(usize, 0), arithmetic_shape.switches);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.add_proven);
}

test "same-sign checked constants combine but mixed-sign constants do not" {
    arithmetic_selection = .same_sign_chain;
    try harness.expectLirInspectionWithOptions(
        arithmeticApp("calc : U8 -> U8\ncalc = |a| (a + 1) + 2\n", "calc(List.len(_args).to_u8_wrap())"),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countArithmeticShape,
    );
    try std.testing.expect(arithmetic_shape.found);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.add_crash);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.literal_three);

    arithmetic_selection = .mixed_sign_chain;
    try harness.expectLirInspectionWithOptions(
        arithmeticApp("calc : U8 -> U8\ncalc = |a| (a + 100) - 100\n", "calc(List.len(_args).to_u8_wrap())"),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countArithmeticShape,
    );
    try std.testing.expect(arithmetic_shape.found);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.add_crash);
    try std.testing.expectEqual(@as(usize, 0), arithmetic_shape.add_wrap);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.sub_proven);
}

test "a constant overflowing plain add becomes an unconditional crash" {
    arithmetic_selection = .overflow_crash;
    try harness.expectLirInspectionWithOptions(
        arithmeticApp("calc : U8 -> U8\ncalc = |a| (a.bitwise_and(0) + 255) + 1\n", "calc(List.len(_args).to_u8_wrap())"),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countArithmeticShape,
    );
    try std.testing.expect(arithmetic_shape.found);
    try std.testing.expectEqual(@as(usize, 0), arithmetic_shape.add_crash);
    try std.testing.expectEqual(@as(usize, 1), arithmetic_shape.crashes);
}

// Mirrors the real decode loop's structure: a conjunction in the loop
// condition, a statement-if that conditionally advances the cursor, and a
// refill after the merge whose proof needs the met cursor window.
const merge_fastloop_app =
    \\decode : List(U8), U64 -> U64
    \\decode = |input, start| {
    \\    in_len = List.len(input)
    \\    var $in_next = start
    \\    var $acc = 0.U64
    \\    var $done = 0.U64
    \\    while $done == 0 and $in_next + 24 <= in_len {
    \\        e0 = U64.from_le_bytes(input, $in_next) ?? 0
    \\        if e0.bitwise_and(1) != 0 {
    \\            $acc = $acc.bitwise_or(e0)
    \\            $in_next = $in_next + 7 - e0.shr_zf_wrap(3).bitwise_and(7)
    \\        } else {}
    \\        w2 = U64.from_le_bytes(input, $in_next) ?? 0
    \\        $acc = $acc.bitwise_or(w2)
    \\        $in_next = $in_next + 7 - w2.shr_zf_wrap(3).bitwise_and(7)
    \\        if $acc.bitwise_and(64) != 0 {
    \\            $done = 1
    \\        } else {}
    \\    }
    \\    $acc
    \\}
    \\
    \\main! : List(Str) => Try({}, [Exit(I8), ..])
    \\main! = |args| {
    \\    n = decode(Str.to_utf8(Str.join_with(args, ",")), 0)
    \\    echo!(Str.inspect(n))
    \\    Ok({})
    \\}
;

test "scratch: conjunction and post-merge refill both prove" {
    try harness.expectLirInspectionWithOptions(
        merge_fastloop_app,
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countDecodeShape,
    );
    try std.testing.expect(counted.found_decode_proc);
    try std.testing.expectEqual(@as(usize, 0), counted.is_lt);
    try std.testing.expectEqual(@as(usize, 0), counted.is_gt);
    try std.testing.expectEqual(@as(usize, 0), counted.minus_checked);
}

// The real decode fastloop from roc-deflate's Inflate.roc, structurally
// identical (constants inlined, plain arguments), so the pass is exercised
// against the exact shape it exists to optimize.
const real_fastloop_app =
    \\inflate_block : List(U8), U64, U64, U64, List(U8), List(U32), U64, List(U32) -> Try({ out : List(U8), in_next : U64 }, [CorruptData, ..])
    \\inflate_block = |input, in_next0, bitbuf0, bitsleft0, out0, litlen_table, litlen_mask, offset_table| {
    \\    in_len = List.len(input)
    \\
    \\    var $in_next = in_next0
    \\    var $bitbuf = bitbuf0
    \\    var $bitsleft = bitsleft0
    \\    var $out = out0
    \\    var $done = 0.U64
    \\
    \\    var $entry = 0.U32
    \\    if $in_next + 24 <= in_len {
    \\        word0 = U64.from_le_bytes(input, $in_next) ?? 0
    \\        $bitbuf = $bitbuf.bitwise_or(word0.shl_wrap($bitsleft.to_u8_wrap()))
    \\        $in_next = $in_next + 7 - $bitsleft.shr_zf_wrap(3).bitwise_and(7)
    \\        $bitsleft = $bitsleft.bitwise_or(56)
    \\        $entry = (List.get(litlen_table, $bitbuf.bitwise_and(litlen_mask)) ?? 0)
    \\    } else {}
    \\    while $done == 0 and $in_next + 24 <= in_len {
    \\        var $saved_bitbuf = $bitbuf
    \\        var $consumed = $entry.bitwise_and(255).to_u64()
    \\        $bitbuf = $bitbuf.shr_zf_wrap($consumed.to_u8_wrap())
    \\        $bitsleft = $bitsleft - $consumed
    \\        if $entry.bitwise_and(0x4000) != 0 {
    \\            sub_mask = 1.U64.shl_wrap($entry.shr_zf_wrap(8).bitwise_and(63).to_u8_wrap()) - 1
    \\            sub_index = $entry.shr_zf_wrap(16).to_u64() + $bitbuf.bitwise_and(sub_mask)
    \\            $entry = (List.get(litlen_table, sub_index) ?? 0)
    \\            $saved_bitbuf = $bitbuf
    \\            $consumed = $entry.bitwise_and(255).to_u64()
    \\            $bitbuf = $bitbuf.shr_zf_wrap($consumed.to_u8_wrap())
    \\            $bitsleft = $bitsleft - $consumed
    \\        } else {}
    \\
    \\        var $pending = 1.U64
    \\        if $entry.bitwise_and(0x80000000) != 0 {
    \\            $out = List.append($out, $entry.shr_zf_wrap(16).to_u8_wrap())
    \\            $entry = (List.get(litlen_table, $bitbuf.bitwise_and(litlen_mask)) ?? 0)
    \\            $saved_bitbuf = $bitbuf
    \\            $consumed = $entry.bitwise_and(255).to_u64()
    \\            $bitbuf = $bitbuf.shr_zf_wrap($consumed.to_u8_wrap())
    \\            $bitsleft = $bitsleft - $consumed
    \\            if $entry.bitwise_and(0x80000000) != 0 {
    \\                $out = List.append($out, $entry.shr_zf_wrap(16).to_u8_wrap())
    \\                word3 = U64.from_le_bytes(input, $in_next) ?? 0
    \\                $bitbuf = $bitbuf.bitwise_or(word3.shl_wrap($bitsleft.to_u8_wrap()))
    \\                $in_next = $in_next + 7 - $bitsleft.shr_zf_wrap(3).bitwise_and(7)
    \\                $bitsleft = $bitsleft.bitwise_or(56)
    \\                $entry = (List.get(litlen_table, $bitbuf.bitwise_and(litlen_mask)) ?? 0)
    \\                $pending = 0
    \\            } else {}
    \\        } else {}
    \\
    \\        if $pending == 1 {
    \\            if $entry.bitwise_and(0x2000) != 0 {
    \\                $done = 1
    \\            } else {
    \\                len_codeword_bits = $entry.shr_zf_wrap(8).bitwise_and(255).to_u8_wrap()
    \\                len_mask = 1.U64.shl_wrap($consumed.to_u8_wrap()) - 1
    \\                length = $entry.shr_zf_wrap(16).to_u64()
    \\                    + $saved_bitbuf.bitwise_and(len_mask).shr_zf_wrap(len_codeword_bits)
    \\
    \\                if $bitsleft < 28 {
    \\                    word_r = U64.from_le_bytes(input, $in_next) ?? 0
    \\                    $bitbuf = $bitbuf.bitwise_or(word_r.shl_wrap($bitsleft.to_u8_wrap()))
    \\                    $in_next = $in_next + 7 - $bitsleft.shr_zf_wrap(3).bitwise_and(7)
    \\                    $bitsleft = $bitsleft.bitwise_or(56)
    \\                } else {}
    \\
    \\                var $off_entry = (List.get(offset_table, $bitbuf.bitwise_and(255)) ?? 0)
    \\                if $off_entry.bitwise_and(0x8000) != 0 {
    \\                    $bitbuf = $bitbuf.shr_zf_wrap(8)
    \\                    $bitsleft = $bitsleft - 8
    \\                    osm = 1.U64.shl_wrap($off_entry.shr_zf_wrap(8).bitwise_and(63).to_u8_wrap()) - 1
    \\                    osi = $off_entry.shr_zf_wrap(16).to_u64() + $bitbuf.bitwise_and(osm)
    \\                    $off_entry = (List.get(offset_table, osi) ?? 0)
    \\                } else {}
    \\                off_consumed = $off_entry.bitwise_and(255).to_u64()
    \\                off_codeword_bits = $off_entry.shr_zf_wrap(8).bitwise_and(255).to_u8_wrap()
    \\                off_mask = 1.U64.shl_wrap(off_consumed.to_u8_wrap()) - 1
    \\                offset = $off_entry.shr_zf_wrap(16).to_u64()
    \\                    + $bitbuf.bitwise_and(off_mask).shr_zf_wrap(off_codeword_bits)
    \\                $bitbuf = $bitbuf.shr_zf_wrap(off_consumed.to_u8_wrap())
    \\                $bitsleft = $bitsleft - off_consumed
    \\
    \\                out_len = List.len($out)
    \\                if offset > out_len or offset == 0 {
    \\                    return Err(CorruptData)
    \\                } else {}
    \\
    \\                word2 = U64.from_le_bytes(input, $in_next) ?? 0
    \\                $bitbuf = $bitbuf.bitwise_or(word2.shl_wrap($bitsleft.to_u8_wrap()))
    \\                $in_next = $in_next + 7 - $bitsleft.shr_zf_wrap(3).bitwise_and(7)
    \\                $bitsleft = $bitsleft.bitwise_or(56)
    \\                $entry = (List.get(litlen_table, $bitbuf.bitwise_and(litlen_mask)) ?? 0)
    \\
    \\                $out = match List.append_range_within($out, out_len - offset, length) {
    \\                    Ok(new_out) => new_out
    \\                    Err(_) => return Err(CorruptData)
    \\                }
    \\            }
    \\        } else {}
    \\    }
    \\    Ok({ out: $out, in_next: $in_next })
    \\}
    \\
    \\main! : List(Str) => Try({}, [Exit(I8), ..])
    \\main! = |args| {
    \\    bytes = Str.to_utf8(Str.join_with(args, ","))
    \\    tables = List.map(bytes, |b| b.to_u32())
    \\    r = inflate_block(bytes, 0, 0, 0, [], tables, 255, tables) ?? { out: [], in_next: 0 }
    \\    echo!(Str.inspect(List.len(r.out) + r.in_next))
    \\    Ok({})
    \\}
;

test "the deflate fastloop proves every refill guard" {
    try harness.expectLirInspectionWithOptions(
        real_fastloop_app,
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countRealShape,
    );
    try std.testing.expect(counted.found_decode_proc);
    // The one surviving greater-than comparison is the match-offset
    // validation, which tests decoded data and must stay. Every refill
    // bounds test proved against the 24-byte margin.
    try std.testing.expectEqual(@as(usize, 1), counted.is_gt);
}

fn countRealShape(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    counted = .{};
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 22);
    defer gpa.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), &writer);
        const text = writer.buffered();
        if (std.mem.count(u8, text, "num_from_le_bytes_unchecked") < 3) continue;
        if (std.c.getenv("RANGE_PROVE_DUMP") != null) {
            std.debug.print("\n===== real fastloop proc =====\n{s}\n", .{text});
        }
        counted = .{
            .found_decode_proc = true,
            .plus_checked = std.mem.count(u8, text, "num_int_add_crash_on_overflow"),
            .minus_checked = std.mem.count(u8, text, "num_int_sub_crash_on_overflow"),
            .is_lt = std.mem.count(u8, text, "num_is_lt("),
            .is_gt = std.mem.count(u8, text, "num_is_gt("),
            .switches = std.mem.count(u8, text, "switch "),
        };
        return;
    }
}

var simd_concat_ops: usize = 0;
var simd_constant_concats: usize = 0;

fn countSimdConcatCounts(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    simd_concat_ops = 0;
    simd_constant_concats = 0;
    const buf = try std.testing.allocator.alloc(u8, 1 << 22);
    defer std.testing.allocator.free(buf);
    for (0..store.getProcSpecs().len) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(std.testing.allocator, store, layouts, @enumFromInt(index), &writer);
        simd_concat_ops += std.mem.count(u8, writer.buffered(), "low_level simd_concat_shift_bytes(");
        simd_constant_concats += std.mem.count(u8, writer.buffered(), " concat_count=");
    }
}

fn simdConcatApp(comptime count: []const u8) []const u8 {
    return
    \\main! : List(Str) => Try({}, [Exit(I8), ..])
    \\main! = |args| {
    \\    v = U8x16.splat(args.len().to_u8_wrap())
    ++ "\n    result = v.concat_shift_bytes(v.bitwise_not(), " ++ count ++ ")\n" ++
        \\    echo!(Str.inspect(result.to_u128_bits()))
        \\    Ok({})
        \\}
    ;
}

test "SIMD alignment records exact constant counts including endpoints" {
    inline for (.{ "0", "14", "16", "7 + 8" }) |count| {
        try harness.expectLirInspectionWithOptions(
            simdConcatApp(count),
            .{ .inline_mode = .wrappers, .prove_ranges = true },
            countSimdConcatCounts,
        );
        try std.testing.expect(simd_concat_ops > 0);
        try std.testing.expect(simd_constant_concats > 0);
    }
}

test "SIMD alignment keeps caller-dependent counts dynamic" {
    try harness.expectLirInspectionWithOptions(
        simdConcatApp("args.len().to_u8_wrap() % 17"),
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countSimdConcatCounts,
    );
    try std.testing.expect(simd_concat_ops > 0);
    try std.testing.expectEqual(@as(usize, 0), simd_constant_concats);
}

test "SIMD alignment does not specialize a changing loop count from its initial value" {
    try harness.expectLirInspectionWithOptions(
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    v = U8x16.splat(args.len().to_u8_wrap())
        \\    var $count = 0.U8
        \\    var $bits = 0.U128
        \\    while $count <= 16 {
        \\        $bits = $bits.bitwise_xor(v.concat_shift_bytes(v.bitwise_not(), $count).to_u128_bits())
        \\        $count = $count + 1
        \\    }
        \\    echo!(Str.inspect($bits))
        \\    Ok({})
        \\}
    ,
        .{ .inline_mode = .wrappers, .prove_ranges = true },
        countSimdConcatCounts,
    );
    try std.testing.expect(simd_concat_ops > 0);
    try std.testing.expectEqual(@as(usize, 0), simd_constant_concats);
}

var guarded_proc_survives: bool = false;

fn inspectGuardedWrapper(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    guarded_proc_survives = false;
    for (0..store.procSpecCount()) |index| {
        if (store.procDebugName(@enumFromInt(index))) |name| {
            if (std.mem.eql(u8, name, "guarded")) guarded_proc_survives = true;
        }
    }
}

fn guardedWrapperApp(comptime body: []const u8) []const u8 {
    return "guarded : U64, U8 -> U64\nguarded = |x, n| " ++ body ++ "\n" ++
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    x = args.len().to_u64()
        \\    n = args.len().to_u8_wrap()
        \\    echo!(Str.inspect(guarded(x, n).bitwise_or(guarded(x, n + 1))))
        \\    Ok({})
        \\}
    ;
}

test "SIMD alignment prerequisite inlines checked wrappers with either crash arm" {
    inline for (.{
        "if n > 16 { crash \"count\" } else { x.shl_wrap(n) }",
        "if n <= 16 { x.shl_wrap(n) } else { crash \"count\" }",
    }) |body| {
        try harness.expectLirInspectionWithOptions(
            guardedWrapperApp(body),
            .{ .inline_mode = .wrappers, .proc_debug_names = true },
            inspectGuardedWrapper,
        );
        try std.testing.expect(!guarded_proc_survives);
    }
}

test "SIMD alignment prerequisite keeps two-continuation functions out of wrapper inlining" {
    inline for (.{
        "if n > 16 { x.shr_zf_wrap(n) } else { x.shl_wrap(n) }",
        "x.bitwise_or(if n > 16 { x.shr_zf_wrap(n) } else { x.shl_wrap(n) })",
    }) |body| {
        try harness.expectLirInspectionWithOptions(
            guardedWrapperApp(body),
            .{ .inline_mode = .wrappers, .proc_debug_names = true },
            inspectGuardedWrapper,
        );
        try std.testing.expect(guarded_proc_survives);
    }
}

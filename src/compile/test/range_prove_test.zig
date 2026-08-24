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

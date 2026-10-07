//! A record whose observable field values are written out of layout order
//! evaluates them in source order. That ordering must not cost the record
//! anything: its LIR performs exactly the work the same record written in
//! layout order performs, with the same inlined producer calls, allocations,
//! and in-place list updates, differing only in the order of that work.

const std = @import("std");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");

const model =
    \\Model : { points : List(F32), cursor : U64 }
    \\
;

// `cursor` precedes `points` in layout order, and both values are observable:
// the producer calls can run arbitrary code and `+` can overflow.
const in_layout_order =
    \\init : U64 -> Model
    \\init = |n| { cursor: n + 1, points: List.repeat(0.0.F32, n) }
    \\
    \\step : Model -> Model
    \\step = |m| { ..m, cursor: m.cursor + 1, points: List.append(m.points, m.cursor.to_f32()) }
    \\
;

const out_of_layout_order =
    \\init : U64 -> Model
    \\init = |n| { points: List.repeat(0.0.F32, n), cursor: n + 1 }
    \\
    \\step : Model -> Model
    \\step = |m| { ..m, points: List.append(m.points, m.cursor.to_f32()), cursor: m.cursor + 1 }
    \\
;

const main =
    \\main! = |args| {
    \\    n = List.len(args)
    \\    echo!(Str.inspect(step(step(init(n)))))
    \\    Ok({})
    \\}
;

fn lowerToLir(app_body: []const u8, inline_mode: lir.CheckedPipeline.InlineMode, buffer: []u8) harness.LowerToLirHarnessError![]const u8 {
    var writer = std.Io.Writer.fixed(buffer);
    try harness.writeLirWithOptions(app_body, .{ .inline_mode = inline_mode }, &writer);
    return writer.buffered();
}

/// The work a LIR dump performs, independent of its order: every procedure's
/// statements with local, procedure, and join identities erased, sorted
/// within the procedure, and the procedures sorted.
fn workOf(gpa: std.mem.Allocator, dump: []const u8) std.mem.Allocator.Error![]const u8 {
    var procs: std.ArrayList([]const u8) = .empty;
    defer {
        for (procs.items) |proc| gpa.free(proc);
        procs.deinit(gpa);
    }
    var lines: std.ArrayList([]const u8) = .empty;
    defer {
        for (lines.items) |line| gpa.free(line);
        lines.deinit(gpa);
    }

    var rest = dump;
    while (rest.len != 0) {
        const end = std.mem.findScalar(u8, rest, '\n') orelse rest.len;
        const line = std.mem.trim(u8, rest[0..end], " ");
        rest = if (end < rest.len) rest[end + 1 ..] else rest[rest.len..];
        if (std.mem.startsWith(u8, line, "proc ") and lines.items.len != 0) {
            try procs.append(gpa, try sortedJoin(gpa, &lines));
        }
        try lines.append(gpa, try eraseIdentities(gpa, line));
    }
    if (lines.items.len != 0) try procs.append(gpa, try sortedJoin(gpa, &lines));
    return try sortedJoin(gpa, &procs);
}

/// Drop the digits of every `l`, `p`, and `j` identity that starts a word.
fn eraseIdentities(gpa: std.mem.Allocator, line: []const u8) std.mem.Allocator.Error![]const u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);
    var index: usize = 0;
    while (index < line.len) {
        const c = line[index];
        try out.append(gpa, c);
        index += 1;
        const starts_word = index == 1 or !std.ascii.isAlphanumeric(line[index - 2]);
        if ((c == 'l' or c == 'p' or c == 'j') and starts_word and index < line.len and std.ascii.isDigit(line[index])) {
            while (index < line.len and std.ascii.isDigit(line[index])) index += 1;
        }
    }
    return try out.toOwnedSlice(gpa);
}

fn lessThan(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

/// Sort `items`, join them with newlines, and free and clear them.
fn sortedJoin(gpa: std.mem.Allocator, items: *std.ArrayList([]const u8)) std.mem.Allocator.Error![]const u8 {
    std.mem.sort([]const u8, items.items, {}, lessThan);
    const joined = try std.mem.join(gpa, "\n", items.items);
    for (items.items) |item| gpa.free(item);
    items.clearRetainingCapacity();
    return joined;
}

test "a record written out of layout order lowers to the same LIR work as in layout order" {
    const gpa = std.testing.allocator;
    const capacity = 1 << 22;
    const in_order_buffer = try gpa.alloc(u8, capacity);
    defer gpa.free(in_order_buffer);
    const out_of_order_buffer = try gpa.alloc(u8, capacity);
    defer gpa.free(out_of_order_buffer);

    for ([_]lir.CheckedPipeline.InlineMode{ .wrappers, .wrappers_and_source_single_use }) |inline_mode| {
        const in_order = try lowerToLir(model ++ in_layout_order ++ main, inline_mode, in_order_buffer);
        const out_of_order = try lowerToLir(model ++ out_of_layout_order ++ main, inline_mode, out_of_order_buffer);

        // The comparison is meaningful only while the layout-order record
        // inlines its producers: `List.repeat` becomes a loop over a list
        // allocated with its final capacity, and `List.append` reserves and
        // appends in place.
        try std.testing.expect(std.mem.find(u8, in_order, "list_with_capacity") != null);
        try std.testing.expect(std.mem.find(u8, in_order, "list_reserve_for_append(") != null);
        try std.testing.expect(std.mem.find(u8, in_order, ") unique=") != null);

        const in_order_work = try workOf(gpa, in_order);
        defer gpa.free(in_order_work);
        const out_of_order_work = try workOf(gpa, out_of_order);
        defer gpa.free(out_of_order_work);
        try std.testing.expectEqualStrings(in_order_work, out_of_order_work);
    }
}

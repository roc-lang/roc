//! Generated-corpus tests for the decision-tree match compiler
//! (src/postcheck/match_tree.zig). Each generated program runs through
//! compilation (compile-time finalization executes tree-lowered LIR on the
//! dev backend) and the LIR interpreter, catching lowering panics,
//! statement-count lint violations, ARC issues, and evaluation crashes.
//! Cross-executor agreement is covered by `zig build run-test-eval`.
//!
//! Programs are generated from fixed seeds (never wall-clock), covering
//! random nesting of tags, records, tuples, lists (with rests), strings
//! (literal and interpolation patterns), int literals, and guards, plus a
//! small-universe family where every value of a tiny type is enumerated
//! against random pattern lists.

const std = @import("std");
const eval = @import("eval");

const helpers = eval.test_helpers;

const Buf = std.ArrayList(u8);

/// Error set for corpus generation and execution helpers.
const CorpusError = std.mem.Allocator.Error || error{TestUnexpectedResult};

fn appendf(buf: *Buf, alloc: std.mem.Allocator, comptime fmt: []const u8, args: anytype) std.mem.Allocator.Error!void {
    const piece = try std.fmt.allocPrint(alloc, fmt, args);
    defer alloc.free(piece);
    try buf.appendSlice(alloc, piece);
}

/// Compile `source` (a module whose `main` is inspect-wrapped) and run it
/// through the LIR interpreter, capturing compile problems and crashes as
/// strings. A rare COMPILE_ERROR (e.g. a generated redundant branch the
/// checker rejects) is an acceptable outcome; a RUN_ERROR is not — every
/// generated match ends in a wildcard branch and its bodies cannot crash.
fn runProgram(alloc: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error![]u8 {
    var compiled = helpers.compileInspectedProgramForTarget(alloc, std.testing.io, .module, source, &.{}, .native) catch |err| {
        return try std.fmt.allocPrint(alloc, "COMPILE_ERROR:{s}", .{@errorName(err)});
    };
    defer compiled.deinit(alloc);
    return helpers.lirInterpreterInspectedStr(alloc, &compiled.lowered) catch |err| {
        return try std.fmt.allocPrint(alloc, "RUN_ERROR:{s}", .{@errorName(err)});
    };
}

fn expectRunsCleanly(alloc: std.mem.Allocator, source: []const u8) CorpusError!void {
    const out = try runProgram(alloc, source);
    defer alloc.free(out);
    std.testing.expect(!std.mem.startsWith(u8, out, "RUN_ERROR")) catch |err| {
        std.debug.print("\n=== generated match program failed ===\n{s}\n=== result: {s}\n", .{ source, out });
        return err;
    };
}

const int_pool = [_]i64{ -3, 0, 1, 2, 7, 42 };
const str_pool = [_][]const u8{ "", "a", "ab", "pre", "prefix", "hi!", "x.txt" };

const Gen = struct {
    alloc: std.mem.Allocator,
    random: std.Random,
    binds: usize = 0,
    /// Name of one bound variable in the current branch (if any) and whether
    /// it is an integer (usable in guards and inspected bodies).
    last_int_bind: ?[]const u8 = null,
    last_any_bind: ?[]const u8 = null,

    fn freshBind(self: *Gen, buf: *Buf, is_int: bool) std.mem.Allocator.Error![]const u8 {
        const name = try std.fmt.allocPrint(self.alloc, "v{d}", .{self.binds});
        self.binds += 1;
        try buf.appendSlice(self.alloc, name);
        self.last_any_bind = name;
        if (is_int) self.last_int_bind = name;
        return name;
    }

    fn intValue(self: *Gen) i64 {
        return int_pool[self.random.uintLessThan(usize, int_pool.len)];
    }

    fn strValue(self: *Gen) []const u8 {
        return str_pool[self.random.uintLessThan(usize, str_pool.len)];
    }

    /// A pattern for an I64 position: literal, bind, or wildcard.
    fn intPattern(self: *Gen, buf: *Buf) std.mem.Allocator.Error!void {
        switch (self.random.uintLessThan(u8, 4)) {
            0, 1 => try appendf(buf, self.alloc, "{d}", .{self.intValue()}),
            2 => _ = try self.freshBind(buf, true),
            else => try buf.appendSlice(self.alloc, "_"),
        }
    }

    /// A pattern for a Bool position.
    fn boolPattern(self: *Gen, buf: *Buf) std.mem.Allocator.Error!void {
        switch (self.random.uintLessThan(u8, 3)) {
            0 => try buf.appendSlice(self.alloc, "True"),
            1 => try buf.appendSlice(self.alloc, "False"),
            else => try buf.appendSlice(self.alloc, "_"),
        }
    }

    /// A pattern for a Str position: literal, interpolation, bind, wildcard.
    fn strPattern(self: *Gen, buf: *Buf) std.mem.Allocator.Error!void {
        switch (self.random.uintLessThan(u8, 6)) {
            0 => try appendf(buf, self.alloc, "\"{s}\"", .{self.strValue()}),
            1 => try buf.appendSlice(self.alloc, "\"pre${r}\""),
            2 => try buf.appendSlice(self.alloc, "\"${a}!\""),
            3 => try buf.appendSlice(self.alloc, "\"${_}.txt\""),
            4 => _ = try self.freshBind(buf, false),
            else => try buf.appendSlice(self.alloc, "_"),
        }
    }

    /// Body observing the branch index and (sometimes) a bound variable.
    fn body(self: *Gen, buf: *Buf, index: usize) std.mem.Allocator.Error!void {
        if (self.last_int_bind) |name| {
            if (self.random.boolean()) {
                try appendf(buf, self.alloc, "Str.concat(\"b{d}:\", Str.inspect({s}))", .{ index, name });
                return;
            }
        }
        try appendf(buf, self.alloc, "\"b{d}\"", .{index});
    }

    /// Optional guard using a bound int or the salt parameter.
    fn guard(self: *Gen, buf: *Buf) std.mem.Allocator.Error!void {
        if (self.random.uintLessThan(u8, 4) != 0) return; // 25% guarded
        const rhs = self.intValue();
        if (self.last_int_bind) |name| {
            try appendf(buf, self.alloc, " if {s} > {d}", .{ name, rhs });
        } else {
            try appendf(buf, self.alloc, " if salt > {d}", .{rhs});
        }
    }

    fn resetBranch(self: *Gen) void {
        self.last_int_bind = null;
        self.last_any_bind = null;
    }
};

const Family = enum {
    tags,
    lists,
    strings,
    record_nested,
    tuple_nested,
};

fn familyHeader(family: Family) []const u8 {
    return switch (family) {
        .tags =>
        \\T := [A, B(I64), C(Str), D(I64, Bool)]
        \\
        \\f : T, I64 -> Str
        ,
        .lists =>
        \\f : List(I64), I64 -> Str
        ,
        .strings =>
        \\f : Str, I64 -> Str
        ,
        .record_nested =>
        \\f : { x : I64, y : [P, Q(I64)] }, I64 -> Str
        ,
        .tuple_nested =>
        \\f : (I64, [P, Q(I64)]), I64 -> Str
        ,
    };
}

fn genScrutineePattern(gen: *Gen, buf: *Buf, family: Family) std.mem.Allocator.Error!void {
    const alloc = gen.alloc;
    switch (family) {
        .tags => switch (gen.random.uintLessThan(u8, 5)) {
            0 => try buf.appendSlice(alloc, "A"),
            1 => {
                try buf.appendSlice(alloc, "B(");
                try gen.intPattern(buf);
                try buf.appendSlice(alloc, ")");
            },
            2 => {
                try buf.appendSlice(alloc, "C(");
                try gen.strPattern(buf);
                try buf.appendSlice(alloc, ")");
            },
            3 => {
                try buf.appendSlice(alloc, "D(");
                try gen.intPattern(buf);
                try buf.appendSlice(alloc, ", ");
                try gen.boolPattern(buf);
                try buf.appendSlice(alloc, ")");
            },
            else => _ = try gen.freshBind(buf, false),
        },
        .lists => {
            const fixed = gen.random.uintLessThan(usize, 4);
            const with_rest = gen.random.boolean();
            const rest_at = if (with_rest) gen.random.uintLessThan(usize, fixed + 1) else fixed + 1;
            try buf.appendSlice(alloc, "[");
            var wrote: bool = false;
            for (0..fixed + 1) |i| {
                if (i == rest_at) {
                    if (wrote) try buf.appendSlice(alloc, ", ");
                    if (gen.random.boolean()) {
                        try buf.appendSlice(alloc, ".. as ");
                        _ = try gen.freshBind(buf, false);
                    } else {
                        try buf.appendSlice(alloc, "..");
                    }
                    wrote = true;
                }
                if (i == fixed) break;
                if (wrote) try buf.appendSlice(alloc, ", ");
                try gen.intPattern(buf);
                wrote = true;
            }
            try buf.appendSlice(alloc, "]");
        },
        .strings => try gen.strPattern(buf),
        .record_nested => {
            // { x: <intpat>, y: <unionpat> } with occasional partial destructs.
            switch (gen.random.uintLessThan(u8, 4)) {
                0 => {
                    try buf.appendSlice(alloc, "{ x: ");
                    try gen.intPattern(buf);
                    try buf.appendSlice(alloc, " }");
                },
                1 => {
                    try buf.appendSlice(alloc, "{ y: ");
                    try genUnionPQPattern(gen, buf);
                    try buf.appendSlice(alloc, " }");
                },
                else => {
                    try buf.appendSlice(alloc, "{ x: ");
                    try gen.intPattern(buf);
                    try buf.appendSlice(alloc, ", y: ");
                    try genUnionPQPattern(gen, buf);
                    try buf.appendSlice(alloc, " }");
                },
            }
        },
        .tuple_nested => {
            try buf.appendSlice(alloc, "(");
            try gen.intPattern(buf);
            try buf.appendSlice(alloc, ", ");
            try genUnionPQPattern(gen, buf);
            try buf.appendSlice(alloc, ")");
        },
    }
}

fn genUnionPQPattern(gen: *Gen, buf: *Buf) std.mem.Allocator.Error!void {
    switch (gen.random.uintLessThan(u8, 4)) {
        0 => try buf.appendSlice(gen.alloc, "P"),
        1, 2 => {
            try buf.appendSlice(gen.alloc, "Q(");
            try gen.intPattern(buf);
            try buf.appendSlice(gen.alloc, ")");
        },
        else => try buf.appendSlice(gen.alloc, "_"),
    }
}

fn genScrutineeValue(gen: *Gen, buf: *Buf, family: Family) std.mem.Allocator.Error!void {
    const alloc = gen.alloc;
    switch (family) {
        .tags => switch (gen.random.uintLessThan(u8, 4)) {
            0 => try buf.appendSlice(alloc, "A"),
            1 => try appendf(buf, alloc, "B({d})", .{gen.intValue()}),
            2 => try appendf(buf, alloc, "C(\"{s}\")", .{gen.strValue()}),
            else => try appendf(buf, alloc, "D({d}, {s})", .{ gen.intValue(), if (gen.random.boolean()) "True" else "False" }),
        },
        .lists => {
            const len = gen.random.uintLessThan(usize, 5);
            try buf.appendSlice(alloc, "[");
            for (0..len) |i| {
                if (i > 0) try buf.appendSlice(alloc, ", ");
                try appendf(buf, alloc, "{d}", .{gen.intValue()});
            }
            try buf.appendSlice(alloc, "]");
        },
        .strings => try appendf(buf, alloc, "\"{s}\"", .{gen.strValue()}),
        .record_nested => {
            if (gen.random.boolean()) {
                try appendf(buf, alloc, "{{ x: {d}, y: P }}", .{gen.intValue()});
            } else {
                try appendf(buf, alloc, "{{ x: {d}, y: Q({d}) }}", .{ gen.intValue(), gen.intValue() });
            }
        },
        .tuple_nested => {
            if (gen.random.boolean()) {
                try appendf(buf, alloc, "({d}, P)", .{gen.intValue()});
            } else {
                try appendf(buf, alloc, "({d}, Q({d}))", .{ gen.intValue(), gen.intValue() });
            }
        },
    }
}

fn genProgram(alloc: std.mem.Allocator, family: Family, seed: u64) std.mem.Allocator.Error![]u8 {
    var prng = std.Random.DefaultPrng.init(seed);
    var gen = Gen{ .alloc = alloc, .random = prng.random() };
    var buf: Buf = .empty;

    try buf.appendSlice(alloc, familyHeader(family));
    try buf.appendSlice(alloc, "\nf = |v, salt| match v {\n");

    const branch_count = 2 + gen.random.uintLessThan(usize, 6);
    for (0..branch_count) |i| {
        gen.resetBranch();
        try buf.appendSlice(alloc, "    ");
        try genScrutineePattern(&gen, &buf, family);
        try gen.guard(&buf);
        try buf.appendSlice(alloc, " => ");
        try gen.body(&buf, i);
        try buf.appendSlice(alloc, "\n");
    }
    try buf.appendSlice(alloc, "    _ => \"end\"\n}\n\nmain = (");

    const value_count = 3 + gen.random.uintLessThan(usize, 3);
    for (0..value_count) |i| {
        if (i > 0) try buf.appendSlice(alloc, ", ");
        try buf.appendSlice(alloc, "f(");
        try genScrutineeValue(&gen, &buf, family);
        try appendf(&buf, alloc, ", {d})", .{gen.intValue()});
    }
    try buf.appendSlice(alloc, ")\n");
    return buf.items;
}

fn runFamily(family: Family, program_count: usize, seed_base: u64) CorpusError!void {
    const alloc = std.testing.allocator;
    for (0..program_count) |i| {
        var arena_state = std.heap.ArenaAllocator.init(alloc);
        defer arena_state.deinit();
        const arena = arena_state.allocator();
        const source = try genProgram(arena, family, seed_base + i);
        try expectRunsCleanly(alloc, source);
    }
}

test "corpus: random tag matches compile and run" {
    try runFamily(.tags, 10, 0x7a95_0001);
}

test "corpus: random list matches compile and run" {
    try runFamily(.lists, 10, 0x7a95_0002);
}

test "corpus: random string matches compile and run" {
    try runFamily(.strings, 10, 0x7a95_0003);
}

test "corpus: random record matches compile and run" {
    try runFamily(.record_nested, 8, 0x7a95_0004);
}

test "corpus: random tuple matches compile and run" {
    try runFamily(.tuple_nested, 8, 0x7a95_0005);
}

test "corpus: small-universe enumeration compiles and runs" {
    // Every value of E := [A, B(Bool)] against random pattern lists; all
    // three values are enumerated in every program.
    const alloc = std.testing.allocator;
    for (0..12) |i| {
        var arena_state = std.heap.ArenaAllocator.init(alloc);
        defer arena_state.deinit();
        const arena = arena_state.allocator();
        var prng = std.Random.DefaultPrng.init(0x7a95_1000 + i);
        var gen = Gen{ .alloc = arena, .random = prng.random() };
        var buf: Buf = .empty;

        try buf.appendSlice(arena,
            \\E := [A, B(Bool)]
            \\
            \\f : E, I64 -> Str
            \\f = |v, salt| match v {
            \\
        );
        const branch_count = 1 + gen.random.uintLessThan(usize, 4);
        for (0..branch_count) |b| {
            gen.resetBranch();
            try buf.appendSlice(arena, "    ");
            switch (gen.random.uintLessThan(u8, 4)) {
                0 => try buf.appendSlice(arena, "A"),
                1 => {
                    try buf.appendSlice(arena, "B(");
                    try gen.boolPattern(&buf);
                    try buf.appendSlice(arena, ")");
                },
                2 => try buf.appendSlice(arena, "B(_)"),
                else => _ = try gen.freshBind(&buf, false),
            }
            try gen.guard(&buf);
            try appendf(&buf, arena, " => \"b{d}\"\n", .{b});
        }
        try buf.appendSlice(arena,
            \\    _ => "end"
            \\}
            \\
            \\main = (f(A, 1), f(B(True), 2), f(B(False), 3))
            \\
        );
        try expectRunsCleanly(alloc, buf.items);
    }
}

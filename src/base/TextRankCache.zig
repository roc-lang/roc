//! Transient lexicographic ranks for append-only name stores. Rank generations
//! are keyed by the exact index limit; inserts require exclusive store access.
//! Each cache belongs to one interner; equal lengths from different interners
//! are not interchangeable. Concurrent readers may prepare the same generation
//! without racing.
const std = @import("std");
const Allocator = std.mem.Allocator;
const Self = @This();

allocator: Allocator,
ranks: std.ArrayList(u32) = .empty,
generation: std.atomic.Value(usize) = .init(std.math.maxInt(usize)),
mutex: std.atomic.Mutex = .unlocked,

pub fn init(allocator: Allocator) Self {
    return .{ .allocator = allocator };
}

/// Allocate an empty cache owned by its name store.
pub fn create(allocator: Allocator) Allocator.Error!*Self {
    const cache = try allocator.create(Self);
    cache.* = init(allocator);
    return cache;
}

pub fn deinit(self: *Self) void {
    self.ranks.deinit(self.allocator);
}

/// Release both the rank buffer and its owning cache allocation.
pub fn destroy(self: *Self) void {
    const allocator = self.allocator;
    self.deinit();
    allocator.destroy(self);
}

/// Borrow the ranks only when they cover the current index domain.
pub fn current(self: *const Self, limit: usize) ?[]const u32 {
    if (self.generation.load(.acquire) != limit) return null;
    return self.ranks.items;
}

/// Context supplies the exact text and successor for each valid index. This
/// supports both dense serial names and byte-offset identifier indices.
pub fn ensure(self: *Self, first: u32, limit: usize, count: usize, context: anytype) Allocator.Error![]const u32 {
    if (self.current(limit)) |ranks| return ranks;
    while (!self.mutex.tryLock()) std.atomic.spinLoopHint();
    defer self.mutex.unlock();
    if (self.current(limit)) |ranks| return ranks;

    const Entry = struct {
        index: u32,
        text: []const u8,

        fn less(_: void, a: @This(), b: @This()) bool {
            return std.mem.lessThan(u8, a.text, b.text);
        }
    };
    const entries = try self.allocator.alloc(Entry, count);
    defer self.allocator.free(entries);
    var index = first;
    for (entries) |*entry| {
        // Resolve each string once. Byte-offset interners otherwise scan for
        // its terminator again on every comparison in the rank sort.
        entry.* = .{ .index = index, .text = context.text(index) };
        index = context.next(index, entry.text);
    }
    std.debug.assert(index == limit);
    // Radix distribution avoids repeatedly comparing common prefixes on large
    // name sets. The fixed cutoffs bound comparison work in small partitions.
    // Bucket zero ends the name; every byte, including zero, has its own bucket.
    if (count <= 256) {
        std.sort.pdq(Entry, entries, {}, Entry.less);
    } else {
        const scratch = try self.allocator.alloc(Entry, count);
        defer self.allocator.free(scratch);
        const Run = struct { start: usize, end: usize, depth: usize };
        var runs: std.ArrayList(Run) = .empty;
        defer runs.deinit(self.allocator);
        try runs.append(self.allocator, .{ .start = 0, .end = count, .depth = 0 });
        while (runs.pop()) |initial| {
            var run = initial;
            while (run.end - run.start > 16) {
                var counts: [257]usize = @splat(0);
                for (entries[run.start..run.end]) |entry| {
                    const bucket: usize = if (run.depth == entry.text.len) 0 else @as(usize, entry.text[run.depth]) + 1;
                    counts[bucket] += 1;
                }
                var nonempty: usize = 0;
                for (counts) |n| {
                    nonempty += @intFromBool(n != 0);
                }
                if (nonempty == 1) {
                    if (counts[0] != 0) break;
                    run.depth += 1;
                    continue;
                }
                var offsets: [257]usize = undefined;
                var position = run.start;
                for (counts, &offsets) |n, *offset| {
                    offset.* = position;
                    position += n;
                }
                for (entries[run.start..run.end]) |entry| {
                    const bucket: usize = if (run.depth == entry.text.len) 0 else @as(usize, entry.text[run.depth]) + 1;
                    scratch[offsets[bucket]] = entry;
                    offsets[bucket] += 1;
                }
                @memcpy(entries[run.start..run.end], scratch[run.start..run.end]);
                for (1..257) |bucket| {
                    if (counts[bucket] > 1) try runs.append(self.allocator, .{ .start = offsets[bucket] - counts[bucket], .end = offsets[bucket], .depth = run.depth + 1 });
                }
                break;
            }
            if (run.end - run.start <= 16) std.sort.insertion(Entry, entries[run.start..run.end], {}, Entry.less);
        }
    }
    // Reserve before publishing anything, so allocation failure leaves the
    // previous generation intact and a later request can retry.
    try self.ranks.resize(self.allocator, limit);
    for (entries, 0..) |entry, rank| self.ranks.items[entry.index] = @intCast(rank);
    self.generation.store(limit, .release);
    return self.ranks.items;
}

/// Normalize rows by exact u32 lexicographic ranks. Already ordered and
/// reversed runs take linear time without scratch. Large unordered runs use
/// at most four byte-distribution passes; small rows use comparison sorting.
pub fn sortByRank(comptime T: type, items: []T, scratch: *std.ArrayList(T), allocator: Allocator, context: anytype, comptime rank: fn (@TypeOf(context), T) u32) Allocator.Error!void {
    const Order = struct {
        fn less(ctx: @TypeOf(context), a: T, b: T) bool {
            return rank(ctx, a) < rank(ctx, b);
        }
        fn greater(ctx: @TypeOf(context), a: T, b: T) bool {
            return rank(ctx, a) > rank(ctx, b);
        }
    };
    if (std.sort.isSorted(T, items, context, Order.less)) return;
    if (items.len < 256) {
        if (items.len <= 16) std.sort.insertion(T, items, context, Order.less) else std.sort.pdq(T, items, context, Order.less);
        return;
    }
    if (std.sort.isSorted(T, items, context, Order.greater)) {
        std.mem.reverse(T, items);
        return;
    }
    // Allocate before moving rows. Failure leaves the input intact.
    try scratch.resize(allocator, items.len);
    var source = items;
    var destination = scratch.items;
    var remaining: u32 = 0;
    for (items) |item| remaining |= rank(context, item);
    var shift: u5 = 0;
    while (true) {
        var counts: [256]usize = @splat(0);
        for (source) |item| counts[@as(u8, @truncate(rank(context, item) >> shift))] += 1;
        var offsets: [256]usize = undefined;
        var position: usize = 0;
        for (counts, &offsets) |count, *offset| {
            offset.* = position;
            position += count;
        }
        for (source) |item| {
            const byte: u8 = @truncate(rank(context, item) >> shift);
            destination[offsets[byte]] = item;
            offsets[byte] += 1;
        }
        std.mem.swap([]T, &source, &destination);
        remaining >>= 8;
        if (remaining == 0) break;
        shift += 8;
    }
    if (source.ptr != items.ptr) @memcpy(items, source);
}

test "text rank generation survives every allocation failure" {
    const Context = struct {
        pub fn text(_: @This(), index: u32) []const u8 {
            return (&[_][]const u8{ "z", "a", "aa" })[index];
        }
        pub fn next(_: @This(), index: u32, _: []const u8) u32 {
            return index + 1;
        }
    };
    for (0..2) |failure| {
        var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = failure });
        var cache = Self.init(failing.allocator());
        defer cache.deinit();
        try std.testing.expectError(error.OutOfMemory, cache.ensure(0, 3, 3, Context{}));
        try std.testing.expect(cache.current(3) == null);
        failing.fail_index = std.math.maxInt(usize);
        const ranks = try cache.ensure(0, 3, 3, Context{});
        try std.testing.expectEqualSlices(u32, &.{ 2, 0, 1 }, ranks);
        // Reuse requires no allocation, including when allocation is disabled.
        failing.fail_index = failing.alloc_index;
        try std.testing.expectEqualSlices(u32, ranks, try cache.ensure(0, 3, 3, Context{}));
    }
}

test "concurrent readers share one text rank generation" {
    if (@import("builtin").single_threaded) return error.SkipZigTest;
    const Worker = struct {
        pub fn text(_: @This(), index: u32) []const u8 {
            return (&[_][]const u8{ "z", "a", "aa" })[index];
        }
        pub fn next(_: @This(), index: u32, _: []const u8) u32 {
            return index + 1;
        }
        fn run(cache: *Self) void {
            for (0..100) |_| {
                const ranks = cache.ensure(0, 3, 3, @This(){}) catch @panic("allocation failed");
                std.debug.assert(ranks[0] == 2 and ranks[1] == 0 and ranks[2] == 1);
            }
        }
    };
    var cache = Self.init(std.testing.allocator);
    defer cache.deinit();
    var threads: [4]std.Thread = undefined;
    var spawned: usize = 0;
    defer for (threads[0..spawned]) |thread| thread.join();
    for (&threads) |*thread| {
        thread.* = try std.Thread.spawn(.{}, Worker.run, .{&cache});
        spawned += 1;
    }
}

test "text ranks agree with byte ordering through radix partitions and long prefixes" {
    const gpa = std.testing.allocator;
    var rng = std.Random.DefaultPrng.init(11363);
    var bytes: [1024][600]u8 = undefined;
    var names: [1024][]const u8 = undefined;
    for (&bytes, &names, 0..) |*buffer, *name, i| {
        // Include zero bytes, non-ASCII bytes, prefix pairs, and names sharing
        // a long prefix. IDs make the long names unique without assumptions
        // about character encoding.
        rng.random().bytes(buffer);
        if (i < 256) {
            buffer[0] = @intCast(i);
            name.* = buffer[0..1];
        } else {
            const prefix: usize = if (i % 2 == 0) 512 else 0;
            @memset(buffer[0..prefix], 'a');
            std.mem.writeInt(u32, buffer[prefix..][0..4], @intCast(i), .big);
            name.* = buffer[0 .. prefix + 4 + i % 32];
        }
    }
    const Context = struct {
        names: []const []const u8,
        pub fn text(ctx: @This(), id: u32) []const u8 {
            return ctx.names[id];
        }
        pub fn next(_: @This(), id: u32, _: []const u8) u32 {
            return id + 1;
        }
        fn less(ctx: @This(), a: u32, b: u32) bool {
            return std.mem.lessThan(u8, ctx.names[a], ctx.names[b]);
        }
    };
    var cache = Self.init(gpa);
    defer cache.deinit();
    for ([_]usize{ 0, 1, 16, 256, 257, 1024 }) |count| {
        const context = Context{ .names = names[0..count] };
        const ranks = try cache.ensure(0, count, count, context);
        const expected = try gpa.alloc(u32, count);
        defer gpa.free(expected);
        for (expected, 0..) |*id, i| id.* = @intCast(i);
        std.sort.pdq(u32, expected, context, Context.less);
        for (expected, 0..) |id, rank| try std.testing.expectEqual(rank, ranks[id]);
    }
}

test "text rank radix allocation failures preserve the previous generation" {
    const Run = struct {
        fn run(allocator: Allocator) Allocator.Error!void {
            var bytes: [300][4]u8 = undefined;
            for (&bytes, 0..) |*value, i| std.mem.writeInt(u32, value, @intCast(i), .big);
            const Context = struct {
                bytes: []const [4]u8,
                pub fn text(ctx: @This(), id: u32) []const u8 {
                    return &ctx.bytes[id];
                }
                pub fn next(_: @This(), id: u32, _: []const u8) u32 {
                    return id + 1;
                }
            };
            var cache = Self.init(allocator);
            defer cache.deinit();
            const context = Context{ .bytes = &bytes };
            _ = try cache.ensure(0, 4, 4, context);
            errdefer {
                const ranks = cache.current(4).?;
                for (0..4) |i| std.debug.assert(ranks[i] == i);
            }
            _ = try cache.ensure(0, 300, 300, context);
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Run.run, .{});
}

test "row rank sorting agrees with comparison ordering across widths and rank domains" {
    const Row = struct {
        key: u32,
        payload: usize,
        fn rank(_: void, row: @This()) u32 {
            return row.key;
        }
    };
    var rng = std.Random.DefaultPrng.init(11363);
    var input: [1024]Row = undefined;
    var rows: [1024]Row = undefined;
    var expected: [1024]u32 = undefined;
    var scratch: std.ArrayList(Row) = .empty;
    defer scratch.deinit(std.testing.allocator);
    for ([_]usize{ 0, 1, 16, 17, 255, 256, 257, 1024 }) |len| {
        for ([_]u32{ 7, 255, 65535, 16777215, std.math.maxInt(u32) }) |mask| {
            for (input[0..len], expected[0..len], 0..) |*row, *key, index| {
                key.* = rng.random().int(u32) & mask;
                row.* = .{ .key = key.*, .payload = index };
            }
            @memcpy(rows[0..len], input[0..len]);
            std.sort.pdq(u32, expected[0..len], {}, std.sort.asc(u32));
            try sortByRank(Row, rows[0..len], &scratch, std.testing.allocator, {}, Row.rank);
            var seen: [1024]bool = @splat(false);
            for (rows[0..len], expected[0..len]) |row, key| {
                try std.testing.expectEqual(key, row.key);
                try std.testing.expectEqualDeep(input[row.payload], row);
                try std.testing.expect(!seen[row.payload]);
                seen[row.payload] = true;
            }
            // Ordered and reversed runs also retain every payload.
            try sortByRank(Row, rows[0..len], &scratch, std.testing.allocator, {}, Row.rank);
            std.mem.reverse(Row, rows[0..len]);
            try sortByRank(Row, rows[0..len], &scratch, std.testing.allocator, {}, Row.rank);
            for (rows[0..len], expected[0..len]) |row, key| try std.testing.expectEqual(key, row.key);
        }
    }
}

test "row sort allocation failure preserves input and permits retry" {
    const Rank = struct {
        fn get(_: void, value: u32) u32 {
            return value;
        }
    };
    var rows: [512]u32 = undefined;
    for (&rows, 0..) |*value, i| value.* = @intCast(i);
    var rng = std.Random.DefaultPrng.init(11363);
    rng.random().shuffle(u32, &rows);
    const original = rows;
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 0 });
    var scratch: std.ArrayList(u32) = .empty;
    defer scratch.deinit(failing.allocator());
    try std.testing.expectError(error.OutOfMemory, sortByRank(u32, &rows, &scratch, failing.allocator(), {}, Rank.get));
    try std.testing.expectEqual(original, rows);
    failing.fail_index = std.math.maxInt(usize);
    try sortByRank(u32, &rows, &scratch, failing.allocator(), {}, Rank.get);
    for (rows, 0..) |value, i| try std.testing.expectEqual(i, value);
}

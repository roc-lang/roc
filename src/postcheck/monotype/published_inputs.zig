//! Publication storage for specialization inputs. The coordinator is the sole
//! writer; workers borrow captured prefixes until their task completes.
const std = @import("std");
const Ast = @import("ast.zig");
const Type = @import("type.zig");
const checked = @import("check");
const names = checked.CheckedNames;
const Allocator = std.mem.Allocator;

/// Contiguous append-only publication. Growth retains the old backing, so a
/// reader never follows a mutable list header. Geometric capacities bound all
/// retained allocations by a constant multiple of the largest published prefix.
/// Source prefixes are immutable; only the newly committed suffix is copied.
fn Prefix(comptime T: type) type {
    return struct {
        allocations: std.ArrayList([]T) = .empty,
        items: []T = &.{},
        capacity: usize = 0,

        fn publish(self: *@This(), allocator: Allocator, source: []const T) Allocator.Error![]T {
            std.debug.assert(source.len >= self.items.len);
            if (source.len > self.capacity) {
                const capacity = std.math.ceilPowerOfTwo(usize, source.len) catch return error.OutOfMemory;
                try self.allocations.ensureUnusedCapacity(allocator, 1);
                const backing = try allocator.alloc(T, capacity);
                @memcpy(backing[0..self.items.len], self.items);
                self.allocations.appendAssumeCapacity(backing);
                self.items = backing[0..self.items.len];
                self.capacity = capacity;
            }
            const old_len = self.items.len;
            self.items = self.items.ptr[0..source.len];
            @memcpy(self.items[old_len..], source[old_len..]);
            return self.items;
        }

        fn deinit(self: *@This(), allocator: Allocator) void {
            for (self.allocations.items) |allocation| allocator.free(allocation);
            self.allocations.deinit(allocator);
        }
    };
}

const NamePrefix = struct {
    bytes: Prefix(u8) = .{},
    ranges: Prefix(names.NameInterner.Range) = .{},

    fn publish(self: *NamePrefix, allocator: Allocator, source: *const names.NameInterner) Allocator.Error!names.NameInterner {
        return .{
            .bytes = .{ .items = std.ArrayList(u8).fromOwnedSlice(try self.bytes.publish(allocator, source.bytes.items.items)) },
            .ranges = .{ .items = std.ArrayList(names.NameInterner.Range).fromOwnedSlice(try self.ranges.publish(allocator, source.ranges.items.items)) },
            .supports_inserts = false,
        };
    }

    fn deinit(self: *NamePrefix, allocator: Allocator) void {
        self.bytes.deinit(allocator);
        self.ranges.deinit(allocator);
    }
};

/// Captured worker inputs. This boundary contains no mutable reservation rows
/// or final syntax. Its backing belongs to `ProgramInputs`, not to the reader.
pub const Snapshot = struct {
    types: Type.Store,
    names: names.NameStore,
    imported_fns: []const Ast.ImportedFn,
    const_fn_evidence: []const checked.ConstStore.ConstFnEvidence,
    const_fn_evidence_frames: []const checked.ConstStore.ConstFnEvidenceFrame,
    current_loc: @import("base").SourceLoc,
    current_region: @import("base").Region,

    /// Install the read set in the lane's stable-address body-lowering context.
    /// Only indexed reads and type import may consume these borrowed stores;
    /// caches and all output tables belong to the lane's private workspace.
    pub fn workerProgram(self: *const Snapshot, allocator: Allocator) Ast.Program {
        var program = Ast.Program.init(allocator);
        inline for (.{ "types", "constructing", "spans", "fields", "tags", "declared_fields" }) |field| {
            @field(program.types, field) = @field(self.types, field);
        }
        program.types.freeze();
        program.names = self.names;
        program.names.allocator = allocator;
        program.names.proc_base_by_key = std.StringHashMap(names.ProcBaseKeyRef).init(allocator);
        inline for (.{ "imported_fns", "const_fn_evidence", "const_fn_evidence_frames" }) |field| {
            @field(program, field) = @TypeOf(@field(program, field)).fromOwnedSlice(@constCast(@field(self, field)));
        }
        program.current_loc = self.current_loc;
        program.current_region = self.current_region;
        return program;
    }
};

/// The complete published read set. Final syntax and mutable reservation rows
/// never enter a worker snapshot. Snapshots borrow this owner's allocations.
pub const ProgramInputs = struct {
    types: Prefix(Type.Content) = .{},
    constructing: Prefix(bool) = .{},
    spans: Prefix(Type.TypeId) = .{},
    fields: Prefix(Type.Field) = .{},
    tags: Prefix(Type.Tag) = .{},
    declared_fields: Prefix(Type.DeclaredField) = .{},
    module_names: NamePrefix = .{},
    module_identities: NamePrefix = .{},
    type_names: NamePrefix = .{},
    method_names: NamePrefix = .{},
    record_field_labels: NamePrefix = .{},
    tag_labels: NamePrefix = .{},
    export_names: NamePrefix = .{},
    external_symbol_names: NamePrefix = .{},
    proc_bases: Prefix(names.ProcBaseKey) = .{},
    imported_fns: Prefix(Ast.ImportedFn) = .{},
    const_fn_evidence: Prefix(checked.ConstStore.ConstFnEvidence) = .{},
    const_fn_evidence_frames: Prefix(checked.ConstStore.ConstFnEvidenceFrame) = .{},

    pub fn publish(self: *ProgramInputs, allocator: Allocator, source: *const Ast.Program) Allocator.Error!Snapshot {
        std.debug.assert(!source.types.hasSpeculativeConstruction());
        var view = Snapshot{
            .types = Type.Store.init(allocator),
            .names = names.NameStore.init(allocator),
            .imported_fns = &.{},
            .const_fn_evidence = &.{},
            .const_fn_evidence_frames = &.{},
            .current_loc = source.current_loc,
            .current_region = source.current_region,
        };
        inline for (.{ "types", "constructing", "spans", "fields", "tags", "declared_fields" }) |field| {
            const values = try @field(self, field).publish(allocator, @field(source.types, field).unsafeRawItemsForView());
            @field(view.types, field) = @TypeOf(@field(view.types, field)).fromOwnedSlice(values);
        }
        view.types.freeze();
        inline for (.{ "module_names", "module_identities", "type_names", "method_names", "record_field_labels", "tag_labels", "export_names", "external_symbol_names" }) |field| {
            @field(view.names, field) = try @field(self, field).publish(allocator, &@field(source.names, field));
        }
        view.names.proc_bases.items = std.ArrayList(names.ProcBaseKey).fromOwnedSlice(try self.proc_bases.publish(allocator, source.names.proc_bases.items.items));
        view.names.serialized = true;
        inline for (.{ "imported_fns", "const_fn_evidence", "const_fn_evidence_frames" }) |field| {
            @field(view, field) = try @field(self, field).publish(allocator, @field(source, field).unsafeRawItemsForView());
        }
        return view;
    }

    pub fn deinit(self: *ProgramInputs, allocator: Allocator) void {
        inline for (std.meta.fields(ProgramInputs)) |field| @field(self, field.name).deinit(allocator);
    }
};

/// Single-writer, concurrent-reader append-only hash index. Patricia branches
/// distinguish exact hash bits; collisions retain full keys and values. Inserts
/// publish initialized nodes with release stores and never remove old entries.
/// Readers capture an entry boundary, so later inserts cannot expose identities
/// outside the associated program snapshot. Space is linear in entry count.
pub fn AppendIndex(comptime K: type, comptime V: type) type {
    const Hash = struct {
        fn hashKey(key: K) u64 {
            var hash = std.hash.Wyhash.init(0);
            std.hash.autoHash(&hash, key);
            return hash.final();
        }
    };
    return AppendIndexHashed(K, V, Hash.hashKey);
}

fn AppendIndexHashed(comptime K: type, comptime V: type, comptime hashKey: fn (K) u64) type {
    return struct {
        const Self = @This();
        const Link = std.atomic.Value(?*Node);
        const Entry = struct { key: K, value: V, index: usize, next: ?*const Entry };
        const Node = union(enum) {
            branch: struct { bit: u6, children: [2]Link },
            leaf: struct { hash: u64, entries: std.atomic.Value(?*const Entry) },
        };
        arena: std.heap.ArenaAllocator,
        root: Link = .init(null),
        count: usize = 0,

        pub fn init(allocator: Allocator) Self {
            return .{ .arena = std.heap.ArenaAllocator.init(allocator) };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        pub fn insert(self: *Self, key: K, value: V) Allocator.Error!void {
            const allocator = self.arena.allocator();
            const hash = hashKey(key);
            var found = self.root.load(.acquire);
            while (found) |node| switch (node.*) {
                .branch => |*branch| found = branch.children[(hash >> branch.bit) & 1].load(.acquire),
                .leaf => break,
            };
            const entry = try allocator.create(Entry);
            entry.* = .{ .key = key, .value = value, .index = self.count, .next = null };
            if (found) |node| if (node.leaf.hash == hash) {
                entry.next = node.leaf.entries.load(.acquire);
                node.leaf.entries.store(entry, .release);
                self.count += 1;
                return;
            };
            const leaf = try allocator.create(Node);
            leaf.* = .{ .leaf = .{ .hash = hash, .entries = .init(entry) } };
            if (found == null) {
                self.root.store(leaf, .release);
            } else {
                const bit: u6 = @intCast(63 - @clz(hash ^ found.?.leaf.hash));
                var link = &self.root;
                while (link.load(.acquire)) |node| {
                    if (node.* != .branch or node.branch.bit < bit) break;
                    link = &node.branch.children[(hash >> node.branch.bit) & 1];
                }
                const branch = try allocator.create(Node);
                const side = (hash >> bit) & 1;
                branch.* = .{ .branch = .{ .bit = bit, .children = .{ .init(null), .init(null) } } };
                branch.branch.children[side] = .init(leaf);
                branch.branch.children[1 - side] = .init(link.load(.acquire));
                link.store(branch, .release);
            }
            self.count += 1;
        }

        pub const Iterator = struct {
            entry: ?*const Entry,
            key: K,
            end: usize,

            pub fn next(self: *Iterator) ?V {
                while (self.entry) |entry| {
                    self.entry = entry.next;
                    if (entry.index < self.end and std.meta.eql(entry.key, self.key)) return entry.value;
                }
                return null;
            }
        };

        pub fn get(self: *const Self, key: K, end: usize) Iterator {
            const hash = hashKey(key);
            var node = self.root.load(.acquire);
            while (node) |current| switch (current.*) {
                .branch => |*branch| node = branch.children[(hash >> branch.bit) & 1].load(.acquire),
                .leaf => |*leaf| return .{ .entry = if (leaf.hash == hash) leaf.entries.load(.acquire) else null, .key = key, .end = end },
            };
            return .{ .entry = null, .key = key, .end = end };
        }
    };
}

test "published prefixes survive growth and retain captured lengths" {
    var prefix: Prefix(u32) = .{};
    defer prefix.deinit(std.testing.allocator);
    const first = try prefix.publish(std.testing.allocator, &.{1});
    const second = try prefix.publish(std.testing.allocator, &.{ 1, 2, 3, 4 });
    _ = try prefix.publish(std.testing.allocator, &.{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });
    try std.testing.expectEqualSlices(u32, &.{1}, first);
    try std.testing.expectEqualSlices(u32, &.{ 1, 2, 3, 4 }, second);
}

test "published index preserves exact keys and snapshot visibility through splits" {
    var index = AppendIndex(u32, u32).init(std.testing.allocator);
    defer index.deinit();
    for (0..1000) |key| try index.insert(@intCast(key), @intCast(key * 2));
    const end = index.count;
    for (0..2000) |key| try index.insert(@intCast(key), @intCast(key * 3));
    for (0..2000) |key| {
        var before = index.get(@intCast(key), end);
        try std.testing.expectEqual(if (key < 1000) @as(?u32, @intCast(key * 2)) else null, before.next());
        try std.testing.expectEqual(@as(?u32, null), before.next());
        var after = index.get(@intCast(key), index.count);
        try std.testing.expectEqual(@as(?u32, @intCast(key * 3)), after.next());
    }
}

test "published program inputs stay sealed while the coordinator constructs more types" {
    const allocator = std.testing.allocator;
    var source = Ast.Program.init(allocator);
    defer source.deinit();
    var publication: ProgramInputs = .{};
    defer publication.deinit(allocator);
    const unit = try source.types.internZst(&source.names);
    const label = try source.names.internTagLabel("Before");
    var snapshot = try publication.publish(allocator, &source);
    const old_count = snapshot.types.types.len();
    var latest = unit;
    for (0..256) |_| latest = try source.types.internList(&source.names, latest);
    _ = try source.names.internTagLabel("After");
    const newer = try publication.publish(allocator, &source);
    try std.testing.expect(newer.types.types.len() > old_count);
    try std.testing.expectEqual(old_count, snapshot.types.types.len());
    try std.testing.expectEqualStrings("Before", snapshot.names.tagLabelText(label));
    try std.testing.expectEqual(@as(u32, 1), snapshot.names.tagLabelCount());

    const transaction = source.types.beginTransaction();
    defer transaction.abort(&source.types);
    _ = try transaction.reserve(&source.types);
    try std.testing.expect(source.types.hasSpeculativeConstruction());
    try std.testing.expect(!snapshot.types.hasSpeculativeConstruction());
    var destination = Type.Store.init(allocator);
    defer destination.deinit();
    var destination_names = names.NameStore.init(allocator);
    defer destination_names.deinit();
    var relocation = Type.Store.TypeRelocation.init(allocator, &snapshot.types, &snapshot.names, &destination, &destination_names);
    defer relocation.deinit();
    var imported = try destination.importTypes(&destination_names, &snapshot.types, &snapshot.names, &relocation, &.{unit});
    defer imported.deinit();
    try std.testing.expectEqual(Type.Content.zst, destination.get(imported.roots[0]));
}

test "published index resolves hash collisions by full keys" {
    const Collision = struct {
        fn hash(_: u32) u64 {
            return 0;
        }
    };
    var index = AppendIndexHashed(u32, u32, Collision.hash).init(std.testing.allocator);
    defer index.deinit();
    try index.insert(1, 10);
    try index.insert(2, 20);
    const end = index.count;
    try index.insert(1, 11);
    var first = index.get(1, end);
    var second = index.get(2, index.count);
    var absent = index.get(3, index.count);
    try std.testing.expectEqual(@as(?u32, 10), first.next());
    try std.testing.expectEqual(@as(?u32, null), first.next());
    try std.testing.expectEqual(@as(?u32, 20), second.next());
    try std.testing.expectEqual(@as(?u32, null), second.next());
    try std.testing.expectEqual(@as(?u32, null), absent.next());
}

test "published index readers retain a fixed view during concurrent inserts" {
    if (@import("builtin").single_threaded) return;
    const Index = AppendIndex(u32, u32);
    const Reader = struct {
        fn run(index: *const Index, done: *std.atomic.Value(bool), reads: *std.atomic.Value(usize)) void {
            while (!done.load(.acquire)) {
                for (0..128) |key| {
                    var iterator = index.get(@intCast(key), 128);
                    std.debug.assert(iterator.next().? == key * 2);
                    std.debug.assert(iterator.next() == null);
                }
                _ = reads.fetchAdd(1, .release);
            }
        }
    };
    var index = Index.init(std.testing.allocator);
    defer index.deinit();
    for (0..128) |key| try index.insert(@intCast(key), @intCast(key * 2));
    var done = std.atomic.Value(bool).init(false);
    var reads = std.atomic.Value(usize).init(0);
    const reader = try std.Thread.spawn(.{}, Reader.run, .{ &index, &done, &reads });
    defer reader.join();
    defer done.store(true, .release);
    while (reads.load(.acquire) == 0) std.atomic.spinLoopHint();
    for (0..4096) |key| try index.insert(@intCast(key), @intCast(key * 3));
}

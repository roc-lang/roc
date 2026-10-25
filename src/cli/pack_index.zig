//! Persistent keyed dev offers. Metadata is a payload-free pack of one
//! producer-declared dependency closure; each artifact's bytes are addressed
//! separately by SHA-256. No directory of historical packs is consulted.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const lir = @import("lir");
const PA = backend.dev.ProcArtifact;
const PF = backend.dev.PackFile;
const Allocator = std.mem.Allocator;

/// Magic and schema version of an offer metadata record.
pub const magic = "ROFFER01";
/// Magic and schema version of an independently addressed payload record.
pub const payload_magic = "RPAYLD01";
/// Optional cache records can fail validation or allocation.
pub const Error = PF.ReadError;

/// Identity and integrity digest of immutable record bytes.
pub fn digest(bytes: []const u8) [32]u8 {
    var result: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytes, &result, .{});
    return result;
}

fn appendSlice(allocator: Allocator, bytes: *std.ArrayList(u8), value: []const u8) Allocator.Error!void {
    var count: [8]u8 = undefined;
    std.mem.writeInt(u64, &count, value.len, .little);
    try bytes.appendSlice(allocator, &count);
    try bytes.appendSlice(allocator, value);
}

/// Encode one artifact's code and static-data bytes, without metadata.
pub fn payload(allocator: Allocator, artifact: PA.Artifact) Allocator.Error![]u8 {
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    try bytes.appendSlice(allocator, payload_magic);
    try appendSlice(allocator, &bytes, artifact.code);
    for (artifact.data) |item| try appendSlice(allocator, &bytes, item.bytes);
    return bytes.toOwnedSlice(allocator);
}

/// The producer supplies the exact splice closure, in original set order.
/// Content naming uses the original bytes, never the stripped metadata.
pub fn encode(allocator: Allocator, set: *const PA.Set, spec: PF.SpecEntry, closure: []const u32, hashes: []const [32]u8, names: *const PA.ContentNames) Allocator.Error![]u8 {
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const scratch = arena.allocator();
    const map = try scratch.alloc(u32, set.artifacts.len);
    @memset(map, std.math.maxInt(u32));
    for (closure, 0..) |old, new| map[old] = std.math.cast(u32, new) orelse return error.OutOfMemory;
    const artifacts = try scratch.alloc(PA.Artifact, closure.len);
    for (closure, artifacts) |old, *artifact| {
        artifact.* = set.artifacts[old];
        artifact.code = &.{};
        const refs = try scratch.dupe(PA.Reference, artifact.refs);
        for (refs) |*ref| {
            std.debug.assert(map[ref.target] != std.math.maxInt(u32));
            ref.target = map[ref.target];
        }
        artifact.refs = refs;
        const data = try scratch.dupe(PA.DataItem, artifact.data);
        for (data) |*item| item.bytes = &.{};
        artifact.data = data;
    }
    var entry = spec;
    entry.artifact = map[spec.artifact];
    std.debug.assert(entry.artifact != std.math.maxInt(u32));
    const metadata = try PF.writeWithNames(scratch, &.{ .arena = undefined, .artifacts = artifacts, .source_files = set.source_files }, &.{entry}, names);
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    try bytes.appendSlice(allocator, magic);
    var count: [4]u8 = undefined;
    std.mem.writeInt(u32, &count, std.math.cast(u32, closure.len) orelse return error.OutOfMemory, .little);
    try bytes.appendSlice(allocator, &count);
    for (closure) |old| try bytes.appendSlice(allocator, &hashes[old]);
    try bytes.appendSlice(allocator, metadata);
    return bytes.toOwnedSlice(allocator);
}

/// An owned decoded offer and the explicit digests of its required payloads.
pub const Offer = struct {
    pack: PF.Pack,
    hashes: []const [32]u8,
};

/// Own both the metadata and its decoding; reject foreign/corrupt optional data.
pub fn decode(allocator: Allocator, bytes: []const u8, key: [32]u8) Error!Offer {
    if (bytes.len < 12 or !std.mem.eql(u8, bytes[0..8], magic)) return error.MalformedPack;
    const count = std.mem.readInt(u32, bytes[8..12], .little);
    if (count > (bytes.len - 12) / 32) return error.MalformedPack;
    var pack = try PF.read(allocator, bytes[12 + @as(usize, count) * 32 ..]);
    errdefer pack.deinit();
    if (pack.set.artifacts.len != count or pack.specs.len != 1 or !std.mem.eql(u8, &pack.specs[0].key, &key)) return error.MalformedPack;
    if (pack.set.artifacts[pack.specs[0].artifact].kind != .proc) return error.MalformedPack;
    for (pack.set.artifacts) |artifact| {
        if (artifact.code.len != 0 or artifact.runtime_refs.len != 0) return error.MalformedPack;
        for (artifact.data) |item| if (item.bytes.len != 0) return error.MalformedPack;
    }
    const hashes = try pack.set.arena.allocator().alloc([32]u8, count);
    for (hashes, 0..) |*hash, i| hash.* = bytes[12 + i * 32 ..][0..32].*;
    return .{ .pack = pack, .hashes = hashes };
}

/// Bytes are owned by the loader, and remain live until all its consumers end.
pub fn materialize(artifact: *PA.Artifact, bytes: []const u8) Error!void {
    if (!std.mem.startsWith(u8, bytes, payload_magic)) return error.MalformedPack;
    var offset: usize = payload_magic.len;
    artifact.code = try take(bytes, &offset);
    if (artifact.entry > artifact.code.len) return error.MalformedPack;
    for (@constCast(artifact.data)) |*item| item.bytes = try take(bytes, &offset);
    if (offset != bytes.len) return error.MalformedPack;
}

fn take(bytes: []const u8, offset: *usize) Error![]const u8 {
    if (bytes.len - offset.* < 8) return error.MalformedPack;
    const len = std.math.cast(usize, std.mem.readInt(u64, bytes[offset.*..][0..8], .little)) orelse return error.MalformedPack;
    offset.* += 8;
    if (len > bytes.len - offset.*) return error.MalformedPack;
    defer offset.* += len;
    return bytes[offset.*..][0..len];
}

/// The exact set-local and symbolic dependency closure followed by a splice.
pub const SpliceClosure = struct {
    allocator: Allocator,
    set: *const backend.dev.ProcArtifact.Set,
    procs: std.AutoHashMap(lir.ProcIdentity, ProcArtifacts),
    thunks: std.AutoHashMap(lir.ProcIdentity, u32),
    helpers: std.StringHashMap(u32),
    /// Artifacts the current query reached; exactly the indices in `order`.
    seen: std.DynamicBitSetUnmanaged,
    order: std.ArrayList(u32) = .empty,

    /// The artifacts of one procedure identity: a splice reaches the first,
    /// and a pack offers the last.
    const ProcArtifacts = struct { first: u32, last: u32 };

    pub fn init(allocator: Allocator, set: *const backend.dev.ProcArtifact.Set) Allocator.Error!SpliceClosure {
        var self = SpliceClosure{
            .allocator = allocator,
            .set = set,
            .procs = std.AutoHashMap(lir.ProcIdentity, ProcArtifacts).init(allocator),
            .thunks = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator),
            .helpers = std.StringHashMap(u32).init(allocator),
            .seen = .{},
        };
        errdefer self.deinit();
        self.seen = try std.DynamicBitSetUnmanaged.initEmpty(allocator, set.artifacts.len);
        for (set.artifacts, 0..) |artifact, index| {
            switch (artifact.kind) {
                .proc => |identity| {
                    const entry = try self.procs.getOrPut(identity);
                    if (!entry.found_existing) entry.value_ptr.first = @intCast(index);
                    entry.value_ptr.last = @intCast(index);
                },
                .boxy_thunk => |identity| _ = try self.thunks.getOrPutValue(identity, @intCast(index)),
                .rc_helper => |name| _ = try self.helpers.getOrPutValue(name, @intCast(index)),
                .entrypoint, .message_pool_run, .branch_island => {},
            }
        }
        return self;
    }

    /// The artifact a pack offers for the procedure `identity`.
    pub fn offeredProcArtifact(self: *const SpliceClosure, identity: lir.ProcIdentity) ?u32 {
        const artifacts = self.procs.get(identity) orelse return null;
        return artifacts.last;
    }

    pub fn deinit(self: *SpliceClosure) void {
        self.order.deinit(self.allocator);
        self.seen.deinit(self.allocator);
        self.helpers.deinit();
        self.thunks.deinit();
        self.procs.deinit();
    }

    /// Indices of the artifacts a splice of `root` places, valid until the
    /// next query.
    pub fn of(self: *SpliceClosure, root: u32) Allocator.Error![]const u32 {
        for (self.order.items) |index| self.seen.unset(index);
        self.order.clearRetainingCapacity();
        try self.visit(root);
        var cursor: usize = 0;
        while (cursor < self.order.items.len) : (cursor += 1) {
            const artifact = self.set.artifacts[self.order.items[cursor]];
            for (artifact.refs) |ref| try self.visit(ref.target);
            for (artifact.symbolic_refs) |ref| {
                const target = switch (ref.target) {
                    .proc => |identity| if (self.procs.get(identity)) |artifacts| artifacts.first else null,
                    .boxy_thunk => |identity| self.thunks.get(identity),
                    .rc_helper => |name| self.helpers.get(name),
                };
                if (target) |index| try self.visit(index);
            }
        }
        return self.order.items;
    }

    pub fn visit(self: *SpliceClosure, index: u32) Allocator.Error!void {
        if (self.seen.isSet(index)) return;
        try self.order.append(self.allocator, index);
        self.seen.set(index);
    }
};

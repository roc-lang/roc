//! Translate explicit finalized read provenance into portable pack certificates.
//!
//! Eligibility belongs to the actual emitting artifact closure, not a shared
//! root's arbitrary owner. Local references are resolved while the program
//! session is alive; persisted facts retain only complete stable identities.

const std = @import("std");
const lir = @import("lir");
const eval = @import("eval");
const backend = @import("backend");

const Allocator = std.mem.Allocator;
const Facts = lir.LIR.FinalizedLiteralOutcomes;
const Outcomes = eval.CompileTimeFinalization.FinalizedLiteralOutcomes.Store;

pub const Decision = union(enum) {
    no_literals,
    withhold,
    certified: *const Facts.Certificate,
};

pub const Index = struct {
    allocator: Allocator,
    certificates: std.heap.ArenaAllocator,
    emitters: std.AutoHashMap(lir.ProcIdentity, State),

    const State = struct {
        eligible: bool = true,
        roots: std.ArrayList(Facts.PortableSuccess) = .empty,
    };

    pub fn init(
        allocator: Allocator,
        outcomes: ?*const Outcomes,
        uses: []const Facts.RuntimeUse,
        external: []const Facts.ExternalCertificate,
    ) Allocator.Error!Index {
        var index: Index = .{
            .allocator = allocator,
            .certificates = std.heap.ArenaAllocator.init(allocator),
            .emitters = std.AutoHashMap(lir.ProcIdentity, State).init(allocator),
        };
        errdefer index.deinit();
        var owners = std.AutoHashMap(Facts.OwnerId, usize).init(allocator);
        defer owners.deinit();
        if (outcomes) |completed| {
            if (completed.records.items.len != completed.record_roots.items.len or
                completed.records.items.len != completed.record_owners.items.len)
                invariant("completion facts lost their local provenance");
            for (completed.record_owners.items, 0..) |owner, ordinal| {
                if (owner) |id| {
                    const entry = try owners.getOrPut(id);
                    if (entry.found_existing) invariant("one read owner had multiple completion records");
                    entry.value_ptr.* = ordinal;
                }
            }
        }
        for (uses) |use| {
            const state = try index.emitter(use.emitter);
            const owner = use.owner orelse {
                state.eligible = false;
                continue;
            };
            const ordinal = owners.get(owner) orelse invariant("runtime read owner had no completion producer");
            const completed = outcomes orelse invariant("runtime literal use had no completed session");
            if (completed.record_roots.items[ordinal] != use.root)
                invariant("runtime read owner resolved to another literal");
            const success = completed.records.items[ordinal].portableSuccess() orelse {
                state.eligible = false;
                continue;
            };
            try appendUnique(allocator, &state.roots, success);
        }
        for (external) |entry| {
            if (!std.mem.eql(u8, &entry.emitter.bytes, &entry.certificate.artifact_identity))
                invariant("external certificate did not belong to its emitting artifact");
            const state = try index.emitter(entry.emitter);
            for (entry.certificate.roots) |root| try appendUnique(allocator, &state.roots, root);
        }
        return index;
    }

    pub fn deinit(self: *Index) void {
        var values = self.emitters.valueIterator();
        while (values.next()) |state| state.roots.deinit(self.allocator);
        self.emitters.deinit();
        self.certificates.deinit();
        self.* = undefined;
    }

    fn emitter(self: *Index, identity: lir.ProcIdentity) Allocator.Error!*State {
        const entry = try self.emitters.getOrPut(identity);
        if (!entry.found_existing) entry.value_ptr.* = .{};
        return entry.value_ptr;
    }

    pub fn forClosure(
        self: *Index,
        key: [32]u8,
        identity: lir.ProcIdentity,
        set: *const backend.dev.ProcArtifact.Set,
        placed: []const u32,
        early: bool,
    ) Allocator.Error!Decision {
        if (self.emitters.count() == 0) return .no_literals;
        var roots = std.ArrayList(Facts.PortableSuccess).empty;
        defer roots.deinit(self.allocator);
        for (placed) |artifact| {
            switch (set.artifacts[artifact].kind) {
                .proc => |proc| {
                    if (self.emitters.get(proc)) |state| {
                        if (!state.eligible) return .withhold;
                        for (state.roots.items) |root| try appendUnique(self.allocator, &roots, root);
                    }
                },
                else => {},
            }
        }
        if (roots.items.len == 0) return .no_literals;
        if (!early) return .withhold;
        std.mem.sort(Facts.PortableSuccess, roots.items, {}, stableRootOrder);
        const arena = self.certificates.allocator();
        const certificate = try arena.create(Facts.Certificate);
        certificate.* = .{
            .specialization_key = key,
            .artifact_identity = identity.bytes,
            .roots = try arena.dupe(Facts.PortableSuccess, roots.items),
        };
        return .{ .certified = certificate };
    }
};

fn stableRootOrder(_: void, left: Facts.PortableSuccess, right: Facts.PortableSuccess) bool {
    const owner = std.mem.order(u8, &left.owner_specialization_key, &right.owner_specialization_key);
    if (owner != .eq) return owner == .lt;
    const module = std.mem.order(u8, &left.root.source.module.bytes, &right.root.source.module.bytes);
    if (module != .eq) return module == .lt;
    if (left.root.source.expr != right.root.source.expr)
        return @intFromEnum(left.root.source.expr) < @intFromEnum(right.root.source.expr);
    return std.mem.order(u8, &left.root.procedure, &right.root.procedure) == .lt;
}

fn appendUnique(allocator: Allocator, roots: *std.ArrayList(Facts.PortableSuccess), root: Facts.PortableSuccess) Allocator.Error!void {
    for (roots.items) |existing| if (existing.eql(root)) return;
    try roots.append(allocator, root);
}

fn invariant(comptime message: []const u8) noreturn {
    std.debug.panic("Finalized literal publication invariant violated: " ++ message, .{});
}

test "finalized literal closure certificates keep shared supported and null owners separate" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testMixedOwners, .{});
}

fn testMixedOwners(allocator: Allocator) !void {
    var outcomes = Outcomes{};
    defer outcomes.deinit(allocator);
    const root: Facts.RootIdentity = .{
        .source = .{ .module = .{ .bytes = [_]u8{1} ** 32 }, .expr = @enumFromInt(7) },
        .procedure = [_]u8{2} ** 32,
    };
    const key = [_]u8{3} ** 32;
    const first: Facts.OwnerId = @enumFromInt(0);
    const second: Facts.OwnerId = @enumFromInt(1);
    const id: lir.LIR.LiteralRootId = @enumFromInt(0);
    try outcomes.appendForRoot(allocator, id, first, .{ .root = root, .specialization_key = key, .outcome = .success, .has_callable_result = false });
    try outcomes.appendForRoot(allocator, id, second, .{ .root = root, .outcome = .success, .has_callable_result = false });
    const good = lir.ProcIdentity.forTest(1);
    const bad = lir.ProcIdentity.forTest(2);
    var index = try Index.init(allocator, &outcomes, &.{
        .{ .root = id, .emitter = good, .owner = first },
        .{ .root = id, .emitter = bad, .owner = second },
    }, &.{});
    defer index.deinit();
    const set = backend.dev.ProcArtifact.Set{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .artifacts = &.{
            .{ .kind = .{ .proc = good }, .code = "", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
            .{ .kind = .{ .proc = bad }, .code = "", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
        },
    };
    const accepted = try index.forClosure(key, good, &set, &.{0}, true);
    try std.testing.expect(accepted == .certified);
    try std.testing.expectEqualDeep(root, accepted.certified.roots[0].root);
    try std.testing.expect((try index.forClosure(key, bad, &set, &.{1}, true)) == .withhold);
    try std.testing.expect((try index.forClosure(key, good, &set, &.{ 0, 1 }, true)) == .withhold);
    try std.testing.expect((try index.forClosure(key, good, &set, &.{0}, false)) == .withhold);
}

test "finalized literal closure certificates deduplicate live and decoded checked identities" {
    if (!@import("base").CompilerFeatures.finalized_literal_cache) return;
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testDecodedIdentityUnion, .{});
}

fn testDecodedIdentityUnion(allocator: Allocator) !void {
    const proc = lir.ProcIdentity.forTest(18);
    const key = [_]u8{17} ** 32;
    const full: Facts.PortableSuccess = .{
        .root = .{
            .source = .{
                .module = .{ .bytes = [_]u8{15} ** 32, .source_hash = [_]u8{13} ** 32, .compiler_artifact_hash = [_]u8{14} ** 32 },
                .expr = @enumFromInt(3),
            },
            .procedure = [_]u8{16} ** 32,
        },
        .owner_specialization_key = key,
    };
    const set = backend.dev.ProcArtifact.Set{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .artifacts = &.{.{ .kind = .{ .proc = proc }, .code = "", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} }},
    };
    const certificate: Facts.Certificate = .{ .specialization_key = key, .artifact_identity = proc.bytes, .roots = &.{full} };
    var spec: backend.dev.PackFile.SpecEntry = .{
        .key = key,
        .artifact = 0,
        .rc_borrowed_params = 0,
        .rc_ret_borrowed = false,
        .rc_ret_lenders = 0,
        .finalized_literals = &certificate,
    };
    const bytes = try backend.dev.PackFile.write(allocator, &set, &.{spec});
    defer allocator.free(bytes);
    var decoded = try backend.dev.PackFile.read(allocator, bytes);
    defer decoded.deinit();
    const decoded_certificate = decoded.specs[0].finalized_literals.?;
    const decoded_root = decoded_certificate.roots[0];
    try std.testing.expect(!std.meta.eql(full, decoded_root));
    try std.testing.expect(full.eql(decoded_root));
    var different_owner = decoded_root;
    different_owner.owner_specialization_key[0] ^= 1;
    try std.testing.expect(!full.eql(different_owner));
    var outcomes = Outcomes{};
    defer outcomes.deinit(allocator);
    const root: lir.LIR.LiteralRootId = @enumFromInt(0);
    const owner: Facts.OwnerId = @enumFromInt(0);
    try outcomes.appendForRoot(allocator, root, owner, .{
        .root = full.root,
        .specialization_key = key,
        .outcome = .success,
        .has_callable_result = false,
    });
    var index = try Index.init(allocator, &outcomes, &.{.{ .root = root, .emitter = proc, .owner = owner }}, &.{.{ .emitter = proc, .certificate = decoded_certificate }});
    defer index.deinit();
    const decision = try index.forClosure(key, proc, &set, &.{0}, true);
    try std.testing.expect(decision == .certified);
    try std.testing.expectEqual(@as(usize, 1), decision.certified.roots.len);
    spec.finalized_literals = decision.certified;
    const reencoded = try backend.dev.PackFile.write(allocator, &set, &.{spec});
    defer allocator.free(reencoded);
    try std.testing.expectEqualSlices(u8, bytes, reencoded);
    var validated = try backend.dev.PackFile.read(allocator, reencoded);
    defer validated.deinit();
    try std.testing.expectEqual(@as(usize, 1), validated.specs[0].finalized_literals.?.roots.len);
}

test "finalized literal external certificates retain deterministic owned pack bytes" {
    if (!@import("base").CompilerFeatures.finalized_literal_cache) return;
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testExternalOrder, .{});
}

fn testExternalOrder(allocator: Allocator) !void {
    const proc = lir.ProcIdentity.forTest(9);
    const key = [_]u8{10} ** 32;
    const first: Facts.PortableSuccess = .{
        .root = .{ .source = .{ .module = .{ .bytes = [_]u8{11} ** 32 }, .expr = @enumFromInt(1) }, .procedure = [_]u8{12} ** 32 },
        .owner_specialization_key = [_]u8{1} ** 32,
    };
    var second = first;
    second.owner_specialization_key = [_]u8{2} ** 32;
    const orders = [_][2]Facts.PortableSuccess{ .{ first, second }, .{ second, first } };
    const set = backend.dev.ProcArtifact.Set{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .artifacts = &.{.{ .kind = .{ .proc = proc }, .code = "", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} }},
    };
    var expected: ?[]u8 = null;
    defer if (expected) |bytes| allocator.free(bytes);
    for (orders) |order| {
        const inherited: Facts.Certificate = .{ .specialization_key = key, .artifact_identity = proc.bytes, .roots = &order };
        var index = try Index.init(allocator, null, &.{}, &.{.{ .emitter = proc, .certificate = &inherited }});
        defer index.deinit();
        const decision = try index.forClosure(key, proc, &set, &.{0}, true);
        try std.testing.expect(decision == .certified);
        const bytes = try backend.dev.PackFile.write(allocator, &set, &.{.{
            .key = key,
            .artifact = 0,
            .rc_borrowed_params = 0,
            .rc_ret_borrowed = false,
            .rc_ret_lenders = 0,
            .finalized_literals = decision.certified,
        }});
        defer allocator.free(bytes);
        if (expected) |previous| {
            try std.testing.expectEqualSlices(u8, previous, bytes);
        } else {
            expected = try allocator.dupe(u8, bytes);
        }
        var decoded = try backend.dev.PackFile.read(allocator, bytes);
        defer decoded.deinit();
        try std.testing.expectEqualDeep(first, decoded.specs[0].finalized_literals.?.roots[0]);
        try std.testing.expectEqualDeep(second, decoded.specs[0].finalized_literals.?.roots[1]);
    }
}

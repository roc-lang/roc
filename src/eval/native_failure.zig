//! Semantic native CTFE failure observations, independent of procedure bodies.
//!
//! Cached code binds these records before execution. Diagnostics must not read
//! a producer's statement ordinal from the consumer's LIR store.

const lir = @import("lir");
const check = @import("check");
const std = @import("std");
const base = @import("base");

pub const GuardProducer = struct {
    module: check.CheckedArtifact.ModuleId,
    root: lir.LIR.ComptimeProducer,
};

pub const Observation = struct {
    checked_error: bool,
    literal_rejection: ?lir.LIR.LiteralRejectionSite = null,
    guard_producer: ?GuardProducer = null,
    /// Current guard statement whose published origin can override the hook's
    /// source location. This is bound consumer metadata, never persisted.
    origin_statement: ?lir.LIR.CFStmtId = null,
    /// Cached guards observe the current producer's published origin through
    /// its slot, even when no source guard statement survived body elision.
    origin_slot: ?lir.LIR.StaticDataId = null,
};

pub const Site = struct {
    owner: lir.LIR.LoweringModuleId,
    checked_site: ?check.CheckedArtifact.CheckedExhaustivenessSiteId,
    procedure_identity: lir.ProcIdentity,
    kind: lir.LIR.ComptimeSiteKind,
    region: base.Region,
    branch_regions: []const base.Region,
};

pub fn site(result: *const lir.Program.Result, id: lir.LIR.ComptimeSiteId) Site {
    const original = result.comptime_sites.items[@intFromEnum(id)];
    return .{
        .owner = original.owner,
        .checked_site = original.checked_site,
        .procedure_identity = result.store.getProcSpec(original.proc).identity,
        .kind = original.kind,
        .region = original.region,
        .branch_regions = original.branch_regions,
    };
}

pub fn captureSites(allocator: std.mem.Allocator, result: *const lir.Program.Result) std.mem.Allocator.Error![]Site {
    const sites = try allocator.alloc(Site, result.comptime_sites.items.len);
    for (sites, 0..) |*entry, index| entry.* = site(result, @enumFromInt(index));
    return sites;
}

/// Raw emitted statement IDs reserve the prefix. Cached failures append explicit
/// semantic rows after it; ordinary statements need no duplicate descriptor.
pub const Registry = struct {
    allocator: std.mem.Allocator,
    result: *const lir.Program.Result,
    prefix: u32,
    guards: std.AutoHashMapUnmanaged(lir.LIR.CFStmtId, lir.LIR.StaticDataId) = .{},
    bound: std.ArrayList(Observation) = .empty,

    pub fn create(allocator: std.mem.Allocator, result: *const lir.Program.Result) std.mem.Allocator.Error!*Registry {
        const registry = try allocator.create(Registry);
        errdefer allocator.destroy(registry);
        registry.* = .{
            .allocator = allocator,
            .result = result,
            .prefix = std.math.cast(u32, result.store.getCFStmts().len) orelse return error.OutOfMemory,
        };
        errdefer registry.guards.deinit(allocator);
        for (result.comptime_value_guards.items) |guard| {
            try registry.guards.put(allocator, guard.crash, guard.value_slot);
            try registry.guards.put(allocator, guard.checked_crash, guard.value_slot);
        }
        return registry;
    }

    pub fn deinit(self: *Registry) void {
        const allocator = self.allocator;
        self.guards.deinit(allocator);
        self.bound.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn get(self: *const Registry, id: u32) Observation {
        if (id >= self.prefix) return self.bound.items[id - self.prefix];
        const statement: lir.LIR.CFStmtId = @enumFromInt(id);
        const data = self.result.store.getCFStmt(statement);
        var observation = Observation{
            .checked_error = data == .crash and data.crash.checked_error,
            .literal_rejection = if (data == .crash) data.crash.literal_rejection else null,
        };
        if (self.guards.get(statement)) |slot| {
            const root = self.result.static_data_values.items[@intFromEnum(slot)].compile_time_root orelse unreachable;
            observation.guard_producer = .{ .module = root.module, .root = root.root };
            observation.origin_statement = statement;
        }
        return observation;
    }

    pub fn append(self: *Registry, observation: Observation) std.mem.Allocator.Error!u32 {
        const count = std.math.cast(u32, self.bound.items.len) orelse return error.OutOfMemory;
        const id = std.math.add(u32, self.prefix, count) catch return error.OutOfMemory;
        try self.bound.append(self.allocator, observation);
        return id;
    }
};

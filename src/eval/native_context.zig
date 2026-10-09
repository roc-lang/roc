//! Producer-exported CTFE context and semantic failure registries.
//!
//! Cached hooks bind immutable checked provenance to consumer-local IDs before
//! execution; neither emission nor diagnostics reconstruct erased bodies.

const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const lir = @import("lir");
const layout = @import("layout");
const Context = backend.dev.CtfeContext;
const Failure = @import("native_failure.zig");
pub const FailureRegistry = Failure.Registry;

pub const Catalog = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    result: *const lir.Program.Result,
    registry: *const Failure.Registry,
    roots: []const ?Context.RootDescriptor,
    view: Context.Catalog,

    pub fn init(allocator: std.mem.Allocator, result: *const lir.Program.Result, registry: *const Failure.Registry) std.mem.Allocator.Error!*Catalog {
        const catalog = try allocator.create(Catalog);
        errdefer allocator.destroy(catalog);
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();
        const a = arena.allocator();
        const roots = try a.alloc(?Context.RootDescriptor, result.static_data_values.items.len);
        var digests = try layout.Digests.init(allocator, &result.layouts);
        defer digests.deinit();
        for (result.static_data_values.items, roots) |value, *descriptor| {
            const root = value.compile_time_root orelse {
                descriptor.* = null;
                continue;
            };
            descriptor.* = .{
                .module = root.module.bytes,
                .producer = switch (root.root) {
                    .checked => |checked_root| .{ .checked = @intFromEnum(checked_root) },
                    .literal => |literal| blk: {
                        const plan = result.literal_roots.items[@intFromEnum(literal)];
                        break :blk .{ .literal = .{
                            .literal = literalDescriptor(result, plan.site),
                            .procedure_identity = result.store.getProcSpec(plan.proc).identity.bytes,
                        } };
                    },
                },
                .layout_digest = try digests.get(value.layout_idx),
                .role = switch (root.role) {
                    .value => .value,
                    .failure_message => |message| .{ .failure_message = .{
                        .failed_field = message.failed_field,
                        .message_field = message.message_field,
                        .failed_offset = message.failed_offset,
                        .message_offset = message.message_offset,
                    } },
                },
            };
        }
        catalog.* = .{
            .allocator = allocator,
            .arena = arena,
            .result = result,
            .registry = registry,
            .roots = roots,
            .view = .{ .provider = .{
                .context = catalog,
                .source = provideSource,
                .failure = provideFailure,
                .site = provideSite,
                .root = provideRoot,
            } },
        };
        return catalog;
    }

    pub fn deinit(self: *Catalog) void {
        const allocator = self.allocator;
        self.arena.deinit();
        allocator.destroy(self);
    }

    fn provideSource(context: *const anyopaque, statement_raw: u32) ?Context.SourceDescriptor {
        const self: *const Catalog = @ptrCast(@alignCast(context));
        if (statement_raw >= self.registry.prefix) return null;
        const statement: lir.LIR.CFStmtId = @enumFromInt(statement_raw);
        return sourceDescriptor(&self.result.store, self.result.store.stmtRegion(statement), self.result.store.stmtLoc(statement));
    }

    fn provideFailure(context: *const anyopaque, statement: u32) ?Context.FailureDescriptor {
        const self: *const Catalog = @ptrCast(@alignCast(context));
        const provenance = provideSource(context, statement) orelse return null;
        const observation = self.registry.get(statement);
        const guard = if (self.registry.guards.get(@enumFromInt(statement))) |slot|
            self.roots[@intFromEnum(slot)] orelse return null
        else
            null;
        return .{
            .source = provenance,
            .checked_error = observation.checked_error,
            .literal_rejection = if (observation.literal_rejection) |literal| literalDescriptor(self.result, literal) else null,
            .guard_root = guard,
        };
    }

    fn provideSite(context: *const anyopaque, index: u32) ?Context.SiteDescriptor {
        const self: *const Catalog = @ptrCast(@alignCast(context));
        if (index >= self.result.comptime_sites.items.len) return null;
        const original = self.result.comptime_sites.items[index];
        return .{
            .checked_module = self.result.loweringModuleKey(original.owner).bytes,
            .checked_site = if (original.checked_site) |checked_site| @intFromEnum(checked_site) else null,
            .procedure_identity = self.result.store.getProcSpec(original.proc).identity.bytes,
            .kind = switch (original.kind) {
                .match => .match,
                .destructure => .destructure,
                .if_ => .if_,
            },
            .region = original.region,
            .branch_regions = original.branch_regions,
        };
    }

    fn provideRoot(context: *const anyopaque, index: u32) ?Context.RootDescriptor {
        const self: *const Catalog = @ptrCast(@alignCast(context));
        if (index >= self.roots.len) return null;
        return self.roots[index];
    }
};

pub fn sourceDescriptor(store: *const lir.LirStore, region: base.Region, loc: base.SourceLoc) ?Context.SourceDescriptor {
    if (!loc.hasLocation()) return null;
    const owner = store.sourceFileCheckedModule(loc.file) orelse return null;
    return .{
        .checked_module = owner,
        .source_identity = store.sourceFileModuleIdentity(loc.file),
        .region = region,
        .line = loc.line,
        .column = loc.column,
        .has_location = true,
    };
}

fn literalDescriptor(result: *const lir.Program.Result, literal: lir.LIR.LiteralRejectionSite) Context.LiteralDescriptor {
    return .{
        .checked_module = result.loweringModuleKey(literal.owner).bytes,
        .checked_expr = literal.checked_expr,
        .kind = switch (literal.kind) {
            .numeral => .numeral,
            .quote => .quote,
        },
    };
}

const BindingHash = struct {
    pub fn hash(_: BindingHash, value: Context.Binding) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHashStrat(&hasher, value, .Deep);
        return hasher.final();
    }

    pub fn eql(_: BindingHash, left: Context.Binding, right: Context.Binding) bool {
        if (std.meta.activeTag(left) != std.meta.activeTag(right)) return false;
        return switch (left) {
            .source_file => |source| std.meta.eql(source, right.source_file),
            .failure => |failure| std.meta.eql(failure, right.failure),
            .static_root => |root| std.meta.eql(root, right.static_root),
            .site => |site| blk: {
                const other = right.site;
                if (!std.meta.eql(site.checked_module, other.checked_module) or
                    site.checked_site != other.checked_site or
                    !std.meta.eql(site.procedure_identity, other.procedure_identity) or
                    site.kind != other.kind or !std.meta.eql(site.region, other.region) or
                    site.branch_regions.len != other.branch_regions.len) break :blk false;
                for (site.branch_regions, other.branch_regions) |region, peer| {
                    if (!std.meta.eql(region, peer)) break :blk false;
                }
                break :blk true;
            },
        };
    }
};

/// Exact descriptor index, built once from current producer declarations.
/// Duplicate compatible rows are still ambiguous: binding never picks a slot.
pub const RootIndex = struct {
    entries: std.HashMap(Context.Binding, ?u32, BindingHash, 80),

    pub fn init(allocator: std.mem.Allocator, roots: []const ?Context.RootDescriptor) std.mem.Allocator.Error!RootIndex {
        var self = RootIndex{ .entries = std.HashMap(Context.Binding, ?u32, BindingHash, 80).init(allocator) };
        errdefer self.deinit();
        for (roots, 0..) |descriptor, index| {
            const root = descriptor orelse continue;
            const entry = try self.entries.getOrPut(.{ .static_root = root });
            entry.value_ptr.* = if (entry.found_existing) null else @intCast(index);
        }
        return self;
    }

    pub fn deinit(self: *RootIndex) void {
        self.entries.deinit();
    }

    pub fn resolve(self: *const RootIndex, root: Context.RootDescriptor) ?u32 {
        return self.entries.get(.{ .static_root = root }) orelse null;
    }
};

test "CTFE root index selects unique current slots and rejects incompatible descriptors" {
    const value: Context.RootDescriptor = .{
        .module = [_]u8{11} ** 32,
        .producer = .{ .checked = 4 },
        .layout_digest = [_]u8{12} ** 32,
        .role = .value,
    };
    var failure = value;
    failure.layout_digest = [_]u8{13} ** 32;
    failure.role = .{ .failure_message = .{ .failed_field = 0, .message_field = 1, .failed_offset = 24, .message_offset = 0 } };
    var index = try RootIndex.init(std.testing.allocator, &.{ null, failure, null, value });
    defer index.deinit();
    try std.testing.expectEqual(@as(?u32, 3), index.resolve(value));
    try std.testing.expectEqual(@as(?u32, 1), index.resolve(failure));
    var wrong = value;
    wrong.module[0] += 1;
    try std.testing.expect(index.resolve(wrong) == null);
    wrong = value;
    wrong.producer.checked += 1;
    try std.testing.expect(index.resolve(wrong) == null);
    wrong = value;
    wrong.layout_digest[0] += 1;
    try std.testing.expect(index.resolve(wrong) == null);
    wrong = failure;
    wrong.role.failure_message.message_offset += 1;
    try std.testing.expect(index.resolve(wrong) == null);
    var ambiguous = try RootIndex.init(std.testing.allocator, &.{ value, failure, value });
    defer ambiguous.deinit();
    try std.testing.expect(ambiguous.resolve(value) == null);
    try std.testing.expectEqual(@as(?u32, 1), ambiguous.resolve(failure));
    var literal = value;
    literal.producer = .{ .literal = .{
        .literal = .{ .checked_module = value.module, .checked_expr = 9, .kind = .numeral },
        .procedure_identity = [_]u8{21} ** 32,
    } };
    var instances = try RootIndex.init(std.testing.allocator, &.{literal});
    defer instances.deinit();
    try std.testing.expectEqual(@as(?u32, 0), instances.resolve(literal));
    literal.producer.literal.procedure_identity[0] += 1;
    try std.testing.expect(instances.resolve(literal) == null);
}

test "CTFE root index owns allocation failure paths" {
    const root: Context.RootDescriptor = .{
        .module = [_]u8{1} ** 32,
        .producer = .{ .checked = 0 },
        .layout_digest = [_]u8{2} ** 32,
        .role = .value,
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, struct {
        fn run(allocator: std.mem.Allocator, descriptor: Context.RootDescriptor) std.mem.Allocator.Error!void {
            var index = try RootIndex.init(allocator, &.{descriptor});
            defer index.deinit();
        }
    }.run, .{root});
}

/// Image-construction owner. All borrowed registry pointers are scoped to native
/// initialization; bound artifact/data storage remains alive through image link.
pub const BindingPlan = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    catalog: *Catalog,
    failures: *Failure.Registry,
    sites: *std.ArrayList(Failure.Site),
    result: *const lir.Program.Result,
    files: std.AutoHashMap(SourceKey, u32),
    modules: std.AutoHashMap([32]u8, lir.LIR.LoweringModuleId),
    values: std.HashMap(Context.Binding, u64, BindingHash, 80),
    roots: RootIndex,
    owned_sets: std.ArrayList(*backend.dev.ProcArtifact.Set) = .empty,
    cached_roots: []const backend.dev.LocatedArtifact = &.{},

    const SourceKey = struct { checked: [32]u8, content: [32]u8 };

    pub fn init(allocator: std.mem.Allocator, result: *const lir.Program.Result, catalog: *Catalog, failures: *Failure.Registry, sites: *std.ArrayList(Failure.Site)) std.mem.Allocator.Error!BindingPlan {
        var plan = BindingPlan{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .catalog = catalog,
            .failures = failures,
            .sites = sites,
            .result = result,
            .files = std.AutoHashMap(SourceKey, u32).init(allocator),
            .modules = std.AutoHashMap([32]u8, lir.LIR.LoweringModuleId).init(allocator),
            .values = std.HashMap(Context.Binding, u64, BindingHash, 80).init(allocator),
            .roots = try RootIndex.init(allocator, catalog.roots),
        };
        errdefer plan.deinit();
        for (0..result.store.sourceFileCount()) |index| {
            const file: u32 = @intCast(index);
            const owner = result.store.sourceFileCheckedModule(file) orelse continue;
            try plan.files.put(.{ .checked = owner, .content = result.store.sourceFileModuleIdentity(file) }, file);
        }
        for (result.lowering_modules.items, 0..) |module, index| {
            try plan.modules.put(module.bytes, @enumFromInt(index));
        }
        for (0..result.comptime_sites.items.len) |index| {
            if (catalog.view.site(@intCast(index))) |descriptor| try plan.values.put(.{ .site = descriptor }, index);
        }
        return plan;
    }

    pub fn deinit(self: *BindingPlan) void {
        for (self.owned_sets.items) |set| set.deinit();
        self.owned_sets.deinit(self.allocator);
        self.values.deinit();
        self.roots.deinit();
        self.modules.deinit();
        self.files.deinit();
        self.arena.deinit();
    }

    fn bind(self: *BindingPlan, binding: Context.Binding) std.mem.Allocator.Error!void {
        if (self.values.contains(binding)) return;
        const value: u64 = switch (binding) {
            .source_file => |source| self.files.get(.{ .checked = source.checked_module, .content = source.source_identity }) orelse unreachable,
            .failure => |failure| blk: {
                const guard_slot = if (failure.guard_root) |root| self.roots.resolve(root) orelse unreachable else null;
                const producer = if (guard_slot) |slot| self.result.static_data_values.items[slot].compile_time_root.? else null;
                const observation = Failure.Observation{
                    .checked_error = failure.checked_error,
                    .guard_producer = if (producer) |root| .{ .module = root.module, .root = root.root } else null,
                    .origin_slot = if (guard_slot) |slot| @enumFromInt(slot) else null,
                    .literal_rejection = if (failure.literal_rejection) |literal| .{
                        .owner = self.modules.get(literal.checked_module) orelse unreachable,
                        .checked_expr = literal.checked_expr,
                        .kind = switch (literal.kind) {
                            .numeral => .numeral,
                            .quote => .quote,
                        },
                    } else null,
                };
                break :blk try self.failures.append(observation);
            },
            .site => |site| blk: {
                const index = self.sites.items.len;
                _ = std.math.cast(u32, index) orelse return error.OutOfMemory;
                try self.sites.append(self.allocator, .{
                    .owner = self.modules.get(site.checked_module) orelse unreachable,
                    .checked_site = if (site.checked_site) |checked_site| @enumFromInt(checked_site) else null,
                    .procedure_identity = .{ .bytes = site.procedure_identity },
                    .kind = switch (site.kind) {
                        .match => .match,
                        .destructure => .destructure,
                        .if_ => .if_,
                    },
                    .region = site.region,
                    .branch_regions = try self.catalog.arena.allocator().dupe(base.Region, site.branch_regions),
                });
                break :blk index;
            },
            .static_root => |root| self.roots.resolve(root) orelse unreachable,
        };
        try self.values.put(binding, value);
    }

    fn resolve(context: *anyopaque, binding: Context.Binding) ?u64 {
        const self: *BindingPlan = @ptrCast(@alignCast(context));
        return self.values.get(binding);
    }

    /// Groups all demanded roots by immutable source set and walks each union
    /// once. Only that serving closure is cloned and patched, not unrelated packs.
    pub fn prepare(context: *anyopaque, located: []const backend.dev.LocatedArtifact) std.mem.Allocator.Error![]const backend.dev.LocatedArtifact {
        const self: *BindingPlan = @ptrCast(@alignCast(context));
        const a = self.arena.allocator();
        self.cached_roots = try a.dupe(backend.dev.LocatedArtifact, located);
        const Group = struct {
            source: *const backend.dev.ProcArtifact.Set,
            roots: std.ArrayList(u32) = .empty,
            bound: *const backend.dev.ProcArtifact.Set,
            remap: ?[]const ?u32 = null,
        };
        var groups = std.ArrayList(Group).empty;
        var group_index = std.AutoHashMap(*const backend.dev.ProcArtifact.Set, usize).init(self.allocator);
        defer group_index.deinit();
        for (located) |artifact| {
            const entry = try group_index.getOrPut(artifact.set);
            if (!entry.found_existing) {
                entry.value_ptr.* = groups.items.len;
                try groups.append(a, .{ .source = artifact.set, .bound = artifact.set });
            }
            try groups.items[entry.value_ptr.*].roots.append(a, artifact.index);
        }
        for (groups.items) |*group| {
            var contextual = false;
            for (group.source.artifacts) |artifact| contextual = contextual or artifact.domain == .ctfe;
            if (!contextual) continue;
            var closure = try backend.dev.ArtifactClosure.init(self.allocator, group.source);
            defer closure.deinit();
            const placed = try closure.ofMany(group.roots.items);
            std.debug.assert(closure.complete);
            const marked = try a.alloc(bool, group.source.artifacts.len);
            @memset(marked, false);
            for (placed) |index| {
                marked[index] = true;
                for (group.source.artifacts[index].context_bindings) |binding| try self.bind(binding);
            }
            var selected = try backend.dev.ArtifactClosure.selectMarked(self.allocator, group.source, marked);
            defer selected.deinit();
            const set = try a.create(backend.dev.ProcArtifact.Set);
            backend.dev.ProcArtifact.bindContextOwned(&selected.set, .{
                .context = self,
                .resolve = resolve,
            }) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.IncompleteContext, error.InvalidContextDomain, error.MissingContextBinding, error.InvalidContextRelocation => unreachable,
            };
            group.remap = try a.dupe(?u32, selected.old_to_new);
            try self.owned_sets.append(self.allocator, set);
            set.* = selected.set;
            selected.set.arena = std.heap.ArenaAllocator.init(self.allocator);
            group.bound = set;
        }
        const prepared = try a.alloc(backend.dev.LocatedArtifact, located.len);
        for (located, prepared) |artifact, *bound| {
            const group = groups.items[group_index.get(artifact.set).?];
            bound.* = .{ .set = group.bound, .index = if (group.remap) |remap| remap[artifact.index].? else artifact.index };
        }
        return prepared;
    }
};

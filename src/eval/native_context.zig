//! Producer-exported CTFE context and semantic failure registries.
//!
//! Cached hooks bind immutable checked provenance to consumer-local IDs before
//! execution; neither emission nor diagnostics derive erased bodies again.

const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const lir = @import("lir");
const layout = @import("layout");
const Events = @import("builtins").source_events;
const Context = backend.dev.CtfeContext;
const Failure = @import("native_failure.zig");
/// Semantic failure observations, keyed by failure statement ID.
pub const FailureRegistry = Failure.Registry;

/// Provides the code generator with source, failure, site, and root
/// descriptors for one lowered program.
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
                    .checked => |checked_root| .{ .checked = @backingInt(checked_root) },
                    .literal => |literal| blk: {
                        const plan = result.literal_roots.items[@backingInt(literal)];
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
        // Materializers append producer statements after consumer failure IDs
        // reserve their prefix. That prefix does not bound the producer domain.
        if (statement_raw >= self.result.store.cfStmtCount()) return null;
        const statement: lir.LIR.CFStmtId = @fromBackingInt(@intCast(statement_raw));
        return sourceDescriptor(&self.result.store, self.result.store.stmtRegion(statement), self.result.store.stmtLoc(statement));
    }

    fn provideFailure(context: *const anyopaque, statement: u32) ?Context.FailureDescriptor {
        const self: *const Catalog = @ptrCast(@alignCast(context));
        const provenance = provideSource(context, statement) orelse return null;
        const observation = self.registry.sourceStatement(@fromBackingInt(@intCast(statement)));
        const guard = if (self.registry.guards.get(@fromBackingInt(@intCast(statement)))) |slot|
            self.roots[@backingInt(slot)] orelse return null
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
            .checked_site = if (original.checked_site) |checked_site| @backingInt(checked_site) else null,
            .procedure_identity = self.result.store.getProcSpec(original.proc).identity.bytes,
            .kind = switch (original.kind) {
                .match => .match,
                .destructure => .destructure,
                .if_ => .if_,
            },
            .region = original.region,
            .branch_regions = original.branch_regions,
            .checked_origin = original.checked_origin,
        };
    }

    fn provideRoot(context: *const anyopaque, index: u32) ?Context.RootDescriptor {
        const self: *const Catalog = @ptrCast(@alignCast(context));
        if (index >= self.roots.len) return null;
        return self.roots[index];
    }
};

test "native producer catalog owns appended materializer call and failure records for object emission" {
    const allocator = std.testing.allocator;
    var result = try lir.Program.Result.init(allocator, base.target.TargetUsize.native);
    defer result.deinit();
    try result.store.setSourceFiles(&.{.{
        .name = "Clean",
        .qualified_name = "app.Clean",
        .module_identity = @splat(41),
        .checked_module_key = @splat(42),
    }});
    const registry = try Failure.Registry.create(allocator, &result);
    defer registry.deinit();
    const catalog = try Catalog.init(allocator, &result, registry);
    defer catalog.deinit();

    // Consumer failure IDs have already been allocated when reconstruction
    // appends producer LIR. Equal integers must not alias those two domains.
    const bound = try registry.append(.{ .checked_error = false });
    const origin: lir.LIR.StmtOrigin = .{
        .loc = .{ .file = 0, .line = 2, .column = 3 },
        .region = .from_raw_offsets(10, 20),
        .inline_scope = .none,
        .kind = .lowering_glue,
    };
    const crash = try result.store.addCFStmt(.{ .crash = .{
        .msg = .{ .literal = try result.store.insertString("materializer failed") },
        .checked_error = true,
    } }, origin);
    try std.testing.expectEqual(bound, @backingInt(crash));
    try std.testing.expect(!registry.get(bound).checked_error);
    const callee = try result.store.addProcSpec(.{
        .name = result.store.freshSyntheticSymbol(),
        .identity = lir.ProcIdentity.forTest(41),
        .args = .empty(),
        .body = crash,
        .ret_layout = .i64,
    }, .none);
    const local = try result.store.addLocal(.{ .layout_idx = .i64 });
    const ret = try result.store.addCFStmt(.{ .ret = .{ .value = local } }, origin);
    const call = try result.store.addCFStmt(.{ .assign_call = .{
        .target = local,
        .proc = callee,
        .args = .empty(),
        .next = ret,
    } }, origin);
    _ = try result.store.addProcSpec(.{
        .name = result.store.freshSyntheticSymbol(),
        .identity = lir.ProcIdentity.forTest(42),
        .args = .empty(),
        .body = call,
        .ret_layout = .i64,
    }, .none);

    const source = catalog.view.source(@backingInt(call)).?;
    try std.testing.expectEqual(@as([32]u8, @splat(42)), source.checked_module);
    try std.testing.expectEqual(@as([32]u8, @splat(41)), source.source_identity);
    try std.testing.expectEqualDeep(origin.region, source.region);
    const failure = catalog.view.failure(@backingInt(crash)).?;
    try std.testing.expectEqualDeep(source, failure.source);
    try std.testing.expect(failure.checked_error);
    const absent: u32 = @intCast(result.store.cfStmtCount());
    try std.testing.expect(catalog.view.source(absent) == null);
    try std.testing.expect(catalog.view.failure(absent) == null);

    // Exercise the complete object consumer, including its native worker lanes,
    // with the real producer catalog rather than a synthetic test provider.
    var compiler = backend.ObjectFileCompiler.initForPack(allocator);
    compiler.source_catalog = catalog.view;
    var object = try compiler.compileToObjectFile(
        &result.store,
        &result.layouts,
        &.{},
        &.{},
        result.store.getProcSpecs(),
        &.{},
        &.{},
        &.{},
        .x64linux,
    );
    defer object.deinit();
    var calls: usize = 0;
    var failures: usize = 0;
    for (object.artifacts.?.artifacts) |artifact| {
        try backend.dev.ProcArtifact.validateSourceEventCertificates(allocator, artifact, .little);
        for (artifact.source_events) |certificate| {
            try std.testing.expectEqual(source.checked_module, certificate.descriptor.source.checked_module);
            try std.testing.expectEqual(source.source_identity, certificate.descriptor.source.source_identity);
            switch (@as(Events.Kind, @fromBackingInt(@intCast(certificate.descriptor.header.kind)))) {
                .call_enter, .call_exit => calls += 1,
                .failure => {
                    failures += 1;
                    try std.testing.expectEqual(@as(u32, 1), certificate.descriptor.failure.checked_error);
                },
                .branch_taken, .exhaustiveness_failed => return error.TestUnexpectedResult,
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 2), calls);
    try std.testing.expectEqual(@as(usize, 1), failures);
}

/// The source descriptor for `region` at `loc`, or null when `loc`'s file has no checked module.
pub fn sourceDescriptor(store: *const lir.LirStore, region: base.Region, loc: base.SourceLoc) ?Context.SourceDescriptor {
    if (!loc.hasLocation()) return .{
        .checked_module = @splat(0),
        .source_identity = @splat(0),
        .region = region,
        .line = 0,
        .column = 0,
        .has_location = false,
    };
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
            .interpolation => .interpolation,
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
        .module = @as([32]u8, @splat(11)),
        .producer = .{ .checked = 4 },
        .layout_digest = @as([32]u8, @splat(12)),
        .role = .value,
    };
    var failure = value;
    failure.layout_digest = @as([32]u8, @splat(13));
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
        .procedure_identity = @as([32]u8, @splat(21)),
    } };
    var instances = try RootIndex.init(std.testing.allocator, &.{literal});
    defer instances.deinit();
    try std.testing.expectEqual(@as(?u32, 0), instances.resolve(literal));
    literal.producer.literal.procedure_identity[0] += 1;
    try std.testing.expect(instances.resolve(literal) == null);
}

test "CTFE root index owns allocation failure paths" {
    const root: Context.RootDescriptor = .{
        .module = @as([32]u8, @splat(1)),
        .producer = .{ .checked = 0 },
        .layout_digest = @as([32]u8, @splat(2)),
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
/// Source event resolutions remembered by descriptor address. The memo is
/// its own allocation so a plan borrowed as a constant resolver can still
/// remember what it resolved; a resolution that cannot be remembered for lack
/// of memory is simply resolved again next time.
const EventMemo = struct {
    allocator: std.mem.Allocator,
    sites: std.AutoHashMapUnmanaged(usize, lir.LIR.ComptimeSiteId) = .empty,
    files: std.AutoHashMapUnmanaged(usize, u32) = .empty,

    fn create(allocator: std.mem.Allocator) std.mem.Allocator.Error!*EventMemo {
        const memo = try allocator.create(EventMemo);
        memo.* = .{ .allocator = allocator };
        return memo;
    }

    fn destroy(self: *EventMemo) void {
        self.sites.deinit(self.allocator);
        self.files.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    fn rememberSite(self: *EventMemo, site: *const Events.Site, resolved: lir.LIR.ComptimeSiteId) void {
        self.sites.put(self.allocator, @intFromPtr(site), resolved) catch {};
    }

    fn rememberFile(self: *EventMemo, source: *const Events.Source, file: u32) void {
        self.files.put(self.allocator, @intFromPtr(source), file) catch {};
    }
};

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
    event_sites: std.AutoHashMap([32]u8, lir.LIR.ComptimeSiteId),
    /// Resolutions of source event descriptors by their address. A
    /// descriptor is data of the compiled image, fixed for the plan's life,
    /// and compile-time code reports the same descriptor many times (every
    /// taken branch of a parse), so each is resolved once.
    event_memo: *EventMemo,
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
            .event_sites = std.AutoHashMap([32]u8, lir.LIR.ComptimeSiteId).init(allocator),
            .event_memo = try EventMemo.create(allocator),
            .roots = try RootIndex.init(allocator, catalog.roots),
        };
        errdefer plan.deinit();
        for (0..result.store.sourceFileCount()) |index| {
            const file: u32 = @intCast(index);
            const owner = result.store.sourceFileCheckedModule(file) orelse continue;
            try plan.files.put(.{ .checked = owner, .content = result.store.sourceFileModuleIdentity(file) }, file);
        }
        for (result.lowering_modules.items, 0..) |module, index| {
            try plan.modules.put(module.bytes, @fromBackingInt(@intCast(index)));
        }
        for (0..result.comptime_sites.items.len) |index| {
            if (catalog.view.site(@intCast(index))) |descriptor| {
                try plan.values.put(.{ .site = descriptor }, index);
                try plan.event_sites.put(siteKey(base.Region, descriptor, descriptor.branch_regions), @fromBackingInt(@intCast(index)));
            }
        }
        for (0..failures.prefix) |statement| {
            if (catalog.view.failure(@intCast(statement))) |descriptor| try plan.bind(.{ .failure = descriptor });
        }
        return plan;
    }

    pub fn deinit(self: *BindingPlan) void {
        for (self.owned_sets.items) |set| set.deinit();
        self.owned_sets.deinit(self.allocator);
        self.values.deinit();
        self.event_sites.deinit();
        self.event_memo.destroy();
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
                    .origin_slot = if (guard_slot) |slot| @fromBackingInt(@intCast(slot)) else null,
                    .literal_rejection = if (failure.literal_rejection) |literal| .{
                        .owner = self.modules.get(literal.checked_module) orelse unreachable,
                        .checked_expr = literal.checked_expr,
                        .kind = switch (literal.kind) {
                            .numeral => .numeral,
                            .quote => .quote,
                            .interpolation => .interpolation,
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
                    .checked_site = if (site.checked_site) |checked_site| @fromBackingInt(@intCast(checked_site)) else null,
                    .procedure_identity = .{ .bytes = site.procedure_identity },
                    .kind = switch (site.kind) {
                        .match => .match,
                        .destructure => .destructure,
                        .if_ => .if_,
                    },
                    .region = site.region,
                    .branch_regions = try self.catalog.arena.allocator().dupe(base.Region, site.branch_regions),
                });
                try self.event_sites.put(siteKey(base.Region, site, site.branch_regions), @fromBackingInt(@intCast(index)));
                break :blk index;
            },
            .static_root => |root| self.roots.resolve(root) orelse unreachable,
        };
        try self.values.put(binding, value);
    }

    pub fn resolveEventSourceFile(raw: *const anyopaque, source: *const Events.Source) u32 {
        const self: *const BindingPlan = @ptrCast(@alignCast(raw));
        std.debug.assert(source.has_location == 1);
        if (self.event_memo.files.get(@intFromPtr(source))) |file| return file;
        const file = self.files.get(.{ .checked = source.checked_module, .content = source.source_identity }) orelse
            base.invariant("source event location has no exact declared source binding", .{});
        self.event_memo.rememberFile(source, file);
        return file;
    }

    pub fn resolveEventSite(raw: *const anyopaque, site: *const Events.Site, branches: []align(1) const Events.Region) lir.LIR.ComptimeSiteId {
        const self: *const BindingPlan = @ptrCast(@alignCast(raw));
        if (self.event_memo.sites.get(@intFromPtr(site))) |resolved| return resolved;
        const resolved = self.resolveEventSiteUncached(site, branches);
        self.event_memo.rememberSite(site, resolved);
        return resolved;
    }

    fn resolveEventSiteUncached(self: *const BindingPlan, site: *const Events.Site, branches: []align(1) const Events.Region) lir.LIR.ComptimeSiteId {
        const descriptor: Context.SiteDescriptor = .{
            .checked_module = site.checked_module,
            .checked_site = if (site.has_checked_site == 1) site.checked_site else null,
            .procedure_identity = site.procedure_identity,
            .kind = switch (@as(Events.SiteKind, @fromBackingInt(@intCast(site.kind)))) {
                .match => .match,
                .destructure => .destructure,
                .if_ => .if_,
            },
            .region = eventRegion(site.region),
            .branch_regions = &.{},
        };
        return self.event_sites.get(siteKey(Events.Region, descriptor, branches)) orelse
            base.invariant("source event site has no exact prebound producer certificate", .{});
    }

    pub fn resolveEventFailure(raw: *const anyopaque, descriptor: *const Events.Descriptor) Failure.Observation {
        const self: *const BindingPlan = @ptrCast(@alignCast(raw));
        const id = self.values.get(.{ .failure = eventFailureDescriptor(descriptor) }) orelse
            base.invariant("source event failure has no exact prebound producer certificate", .{});
        return self.failures.get(@intCast(id));
    }

    fn eventFailureDescriptor(descriptor: *const Events.Descriptor) Context.FailureDescriptor {
        const source = descriptor.source;
        const failure = descriptor.failure;
        return .{
            .source = .{
                .checked_module = source.checked_module,
                .source_identity = source.source_identity,
                .region = eventRegion(source.region),
                .line = source.line,
                .column = source.column,
                .has_location = source.has_location == 1,
            },
            .checked_error = failure.checked_error == 1,
            .literal_rejection = if (failure.has_literal_rejection == 1) eventLiteral(failure.literal_rejection) else null,
            .guard_root = if (failure.has_guard_root == 1) eventRoot(failure.guard_root) else null,
        };
    }

    pub fn bindSourceEvent(self: *BindingPlan, certificate: backend.dev.ProcArtifact.SourceEventCertificate) std.mem.Allocator.Error!void {
        const descriptor = certificate.descriptor;
        if (descriptor.source.has_location == 1) {
            _ = resolveEventSourceFile(self, &descriptor.source);
        }
        switch (@as(Events.Kind, @fromBackingInt(@intCast(descriptor.header.kind)))) {
            .call_enter, .call_exit => {},
            .failure => try self.bind(.{ .failure = eventFailureDescriptor(&descriptor) }),
            .branch_taken, .exhaustiveness_failed => {
                const site = descriptor.site;
                try self.bind(.{ .site = .{
                    .checked_module = site.checked_module,
                    .checked_site = if (site.has_checked_site == 1) site.checked_site else null,
                    .procedure_identity = site.procedure_identity,
                    .kind = switch (@as(Events.SiteKind, @fromBackingInt(@intCast(site.kind)))) {
                        .match => .match,
                        .destructure => .destructure,
                        .if_ => .if_,
                    },
                    .region = eventRegion(site.region),
                    .branch_regions = certificate.branch_regions,
                } });
            },
        }
    }

    fn eventRegion(region: Events.Region) base.Region {
        return base.Region.from_raw_offsets(region.start, region.end);
    }

    fn eventLiteral(literal: Events.Literal) Context.LiteralDescriptor {
        return .{
            .checked_module = literal.checked_module,
            .checked_expr = literal.checked_expr,
            .kind = switch (@as(Events.LiteralKind, @fromBackingInt(@intCast(literal.kind)))) {
                .numeral => .numeral,
                .quote => .quote,
                .interpolation => .interpolation,
            },
        };
    }

    fn eventRoot(root: Events.Root) Context.RootDescriptor {
        return .{
            .module = root.module,
            .producer = switch (@as(Events.ProducerKind, @fromBackingInt(@intCast(root.producer_kind)))) {
                .checked => .{ .checked = root.checked_expr },
                .literal => .{ .literal = .{ .literal = eventLiteral(root.literal), .procedure_identity = root.procedure_identity } },
            },
            .layout_digest = root.layout_digest,
            .role = switch (@as(Events.RootRole, @fromBackingInt(@intCast(root.role)))) {
                .value => .value,
                .failure_message => .{ .failure_message = .{
                    .failed_field = root.failed_field,
                    .message_field = root.message_field,
                    .failed_offset = root.failed_offset,
                    .message_offset = root.message_offset,
                } },
            },
        };
    }

    fn siteWord(hasher: *base.Sha256, value: u32) void {
        var bytes: [4]u8 = undefined;
        std.mem.writeInt(u32, &bytes, value, .little);
        hasher.update(&bytes);
    }

    fn siteKey(comptime Region: type, site: Context.SiteDescriptor, branches: []align(1) const Region) [32]u8 {
        var hasher = base.Sha256.init(.{});
        hasher.update("roc.source-event.site.v1");
        hasher.update(&site.checked_module);
        hasher.update(&site.procedure_identity);
        siteWord(&hasher, @backingInt(site.kind));
        siteWord(&hasher, @intFromBool(site.checked_site != null));
        siteWord(&hasher, site.checked_site orelse 0);
        siteWord(&hasher, site.region.start.offset);
        siteWord(&hasher, site.region.end.offset);
        siteWord(&hasher, @intCast(branches.len));
        for (branches) |branch| {
            siteWord(&hasher, if (Region == base.Region) branch.start.offset else branch.start);
            siteWord(&hasher, if (Region == base.Region) branch.end.offset else branch.end);
        }
        var digest: [32]u8 = undefined;
        hasher.final(&digest);
        return digest;
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
            var closure = try backend.dev.ArtifactClosure.init(self.allocator, group.source);
            defer closure.deinit();
            const placed = try closure.ofMany(group.roots.items);
            std.debug.assert(closure.complete);
            const marked = try a.alloc(bool, group.source.artifacts.len);
            @memset(marked, false);
            for (placed) |index| {
                marked[index] = true;
                for (group.source.artifacts[index].source_events) |certificate| try self.bindSourceEvent(certificate);
            }
        }
        const prepared = try a.alloc(backend.dev.LocatedArtifact, located.len);
        for (located, prepared) |artifact, *bound| {
            const group = groups.items[group_index.get(artifact.set).?];
            bound.* = .{ .set = group.bound, .index = if (group.remap) |remap| remap[artifact.index].? else artifact.index };
        }
        return prepared;
    }
};

//! Closed native artifact graphs shared by pack publication and native placement.
//!
//! A serving entry owns its transitive code/data closure; unrelated offers must
//! not add source-context requirements or claim complete-module availability.

const std = @import("std");
const builtin = @import("builtin");
const lir = @import("lir");
const collections = @import("collections");
const ProcArtifact = @import("ProcArtifact.zig");
const LirCodeGenMod = @import("LirCodeGen.zig");
const Allocator = std.mem.Allocator;
const Self = @This();

allocator: Allocator,
set: *const ProcArtifact.Set,
procs: std.AutoHashMap(lir.ProcIdentity, u32),
thunks: std.AutoHashMap(lir.ProcIdentity, u32),
helpers: std.StringHashMap(u32),
/// One resolution per symbolic edge, shared by admission and placement.
symbolic_offsets: []usize = &.{},
symbolic_targets: []?u32 = &.{},
data_offsets: []usize = &.{},
data_targets: []?u32 = &.{},
symbolic_lookups: usize = 0,
/// Same-kind definitions whose symbolic name cannot prove one callable contract.
ambiguous_definitions: std.AutoHashMap(u32, void),
seen: std.AutoHashMap(u32, void),
order: std.ArrayList(u32) = .empty,
complete: bool = true,
/// Content identity of this exact producer graph, excluding execution-phase
/// labels, session LIR ordinals, and source-file debug numbering, when the
/// set's producer recorded one. A set without one (a program's own
/// artifacts) derives it only where a provision names it
/// (`physicalNamespace`), not for every walk over the set.
physical_namespace: ?[32]u8 = null,

pub fn init(allocator: Allocator, set: *const ProcArtifact.Set) Allocator.Error!Self {
    var self = Self{
        .allocator = allocator,
        .set = set,
        .procs = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator),
        .thunks = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator),
        .helpers = std.StringHashMap(u32).init(allocator),
        .ambiguous_definitions = std.AutoHashMap(u32, void).init(allocator),
        .seen = std.AutoHashMap(u32, void).init(allocator),
    };
    errdefer self.deinit();
    self.physical_namespace = set.physical_namespace;
    for (set.artifacts, 0..) |artifact, index| {
        switch (artifact.kind) {
            .proc => |identity| try self.indexDefinition(&self.procs, identity, @intCast(index)),
            .boxy_thunk => |identity| try self.indexDefinition(&self.thunks, identity, @intCast(index)),
            .rc_helper => |name| try self.indexDefinition(&self.helpers, name, @intCast(index)),
            .entrypoint, .message_pool_run, .branch_island => {},
        }
    }
    self.symbolic_offsets = try allocator.alloc(usize, set.artifacts.len + 1);
    self.symbolic_offsets[0] = 0;
    for (set.artifacts, 0..) |artifact, index| {
        self.symbolic_offsets[index + 1] = self.symbolic_offsets[index] + artifact.symbolic_refs.len;
    }
    self.symbolic_targets = try allocator.alloc(?u32, self.symbolic_offsets[set.artifacts.len]);
    for (set.artifacts, 0..) |artifact, index| {
        for (artifact.symbolic_refs, self.symbolic_targets[self.symbolic_offsets[index]..self.symbolic_offsets[index + 1]]) |ref, *target| {
            self.symbolic_lookups += 1;
            target.* = self.resolveSymbolic(ref.target);
        }
    }
    self.data_offsets = try allocator.alloc(usize, set.artifacts.len + 1);
    self.data_offsets[0] = 0;
    for (set.artifacts, 0..) |artifact, index| {
        var count: usize = 0;
        for (artifact.data) |item| for (item.relocations) |relocation| {
            if (relocation.code_target != null) count += 1;
        };
        self.data_offsets[index + 1] = self.data_offsets[index] + count;
    }
    self.data_targets = try allocator.alloc(?u32, self.data_offsets[set.artifacts.len]);
    for (set.artifacts, 0..) |artifact, index| {
        var cursor = self.data_offsets[index];
        for (artifact.data) |item| for (item.relocations) |relocation| {
            const target = relocation.code_target orelse continue;
            self.data_targets[cursor] = switch (target) {
                .local => |node| node,
                .symbolic => |symbol| self.resolveSymbolic(symbol),
                .image_offset => unreachable,
            };
            cursor += 1;
        };
    }
    return self;
}

/// The physical identity of the set: the producer's, or derived from the
/// artifacts when no producer recorded one.
pub fn physicalNamespace(self: *const Self) [32]u8 {
    return self.physical_namespace orelse derivedPhysicalNamespace(self.set);
}

/// The physical identity of a set no producer recorded one for.
pub fn derivedPhysicalNamespace(set: *const ProcArtifact.Set) [32]u8 {
    var physical = std.crypto.hash.Blake3.init(.{});
    physical.update("roc.native.physical.provision.v1");
    for (set.artifacts) |artifact| {
        std.hash.autoHashStrat(&physical, .{
            artifact.kind,          artifact.code,              artifact.entry,       artifact.frame,
            artifact.refs,          artifact.symbolic_refs,     artifact.relocations, artifact.data,
            artifact.source_events, artifact.callable_contract,
        }, .Deep);
        if (artifact.context_contract) |contract| {
            std.hash.autoHash(&physical, .{ contract.target, contract.cpu_level, contract.hot_reload, contract.default_platform_runtime });
        }
    }
    var namespace: [32]u8 = undefined;
    physical.final(&namespace);
    return namespace;
}

/// The physical provision of artifact `index`: its content address when the
/// set is addressed (admitted from the object store), otherwise its
/// definition in the set's physical namespace.
pub fn provision(self: *const Self, index: u32) LirCodeGenMod.PhysicalProvision {
    if (self.set.addresses) |addresses| return .{ .graph = addresses[index], .definition = 0 };
    return .{ .graph = self.physicalNamespace(), .definition = self.set.physicalDefinition(index) };
}

/// The edges of artifact `index` in the order the content address renders
/// them: its set-local references, its symbolic references, and the code
/// targets of its data; null for a reference that resolves to nothing in
/// the set.
fn contentEdges(self: *const Self, arena: Allocator, index: u32) Allocator.Error![]const ?u32 {
    const artifact = self.set.artifacts[index];
    const symbolic = self.symbolicTargets(index);
    const data = self.dataTargets(index);
    const edges = try arena.alloc(?u32, artifact.refs.len + symbolic.len + data.len);
    for (artifact.refs, edges[0..artifact.refs.len]) |ref, *edge| edge.* = ref.target;
    @memcpy(edges[artifact.refs.len..][0..symbolic.len], symbolic);
    @memcpy(edges[artifact.refs.len + symbolic.len ..], data);
    return edges;
}

/// The content address of every artifact of the set: a digest of what
/// determines the artifact's placed bytes and everything it reaches. An
/// artifact's own rendering is its kind, code, entry, frame, references,
/// relocations, carried data, source-event certificates, materializer
/// dependencies and contracts, with carried data named by content
/// (`ContentNames`); its debug lines are not part of it, as they are not of
/// `derivedPhysicalNamespace`, since their file ordinals belong to the
/// program that emitted them. The address then takes in the addresses of
/// the artifacts its edges reach, so two artifacts with one address place
/// the same bytes and call the same code however many packs carry them.
/// Recursive groups are digested as a group: members are partitioned by
/// repeated refinement of their renderings over their in-group edges, and
/// each member's address is the group's digest with its class, so a group
/// drawn from two programs' graphs gives every member one address.
///
/// An artifact whose `content_address` is known (one served from the store)
/// keeps it, and its edges are not followed: nothing it reaches is new.
pub fn contentAddresses(self: *const Self, names: *const ProcArtifact.ContentNames) Allocator.Error![]const [32]u8 {
    const addresses = try self.computeContentAddresses(names, true);
    if (builtin.mode == .debug) {
        // A known address is a function of the artifact's content alone, so
        // computing it again from the content gives the same address.
        var any_known = false;
        for (self.set.artifacts) |artifact| any_known = any_known or artifact.content_address != null;
        if (any_known) {
            const recomputed = try self.computeContentAddresses(names, false);
            defer self.allocator.free(recomputed);
            for (addresses, recomputed, self.set.artifacts) |known, fresh, artifact| {
                if (artifact.content_address != null and !std.mem.eql(u8, &known, &fresh)) {
                    std.debug.panic("an artifact's recorded content address differs from the address of its content", .{});
                }
            }
        }
    }
    return addresses;
}

fn computeContentAddresses(self: *const Self, names: *const ProcArtifact.ContentNames, trust_known: bool) Allocator.Error![]const [32]u8 {
    const allocator = self.allocator;
    const set = self.set;
    const count = set.artifacts.len;
    const addresses = try allocator.alloc([32]u8, count);
    errdefer allocator.free(addresses);
    var scratch = collections.SingleThreadArena.init(allocator);
    defer scratch.deinit();
    const a = scratch.allocator();

    const known = try a.alloc(bool, count);
    const edges = try a.alloc([]const ?u32, count);
    for (set.artifacts, 0..) |artifact, index| {
        known[index] = trust_known and artifact.content_address != null;
        if (known[index]) addresses[index] = artifact.content_address.?;
        edges[index] = try self.contentEdges(a, @intCast(index));
    }

    // The condensation of the graph over resolved edges out of unknown
    // artifacts; a known artifact is a leaf.
    const succ_starts = try a.alloc(u32, count + 1);
    const pred_starts = try a.alloc(u32, count + 1);
    @memset(pred_starts, 0);
    succ_starts[0] = 0;
    for (edges, 0..) |targets, node| {
        var kept: u32 = 0;
        if (!known[node]) for (targets) |target| if (target) |callee| {
            kept += 1;
            pred_starts[callee + 1] += 1;
        };
        succ_starts[node + 1] = succ_starts[node] + kept;
    }
    for (1..count + 1) |node| pred_starts[node] += pred_starts[node - 1];
    const succs = try a.alloc(u32, succ_starts[count]);
    const preds = try a.alloc(u32, pred_starts[count]);
    const cursors = try a.dupe(u32, pred_starts[0..count]);
    var next: usize = 0;
    for (edges, 0..) |targets, node| {
        if (known[node]) continue;
        for (targets) |target| if (target) |callee| {
            succs[next] = callee;
            next += 1;
            preds[cursors[callee]] = @intCast(node);
            cursors[callee] += 1;
        };
    }
    const condensation = try collections.scc.compute(a, succ_starts, succs, pred_starts, preds);

    // Callees first: no edge leads into a lower-numbered component.
    var component = condensation.count();
    while (component != 0) {
        component -= 1;
        const members = condensation.membersOf(component);
        if (members.len == 1 and known[members[0]]) continue;
        var cyclic = members.len > 1;
        for (edges[members[0]]) |target| if (target == members[0]) {
            cyclic = true;
        };
        if (!cyclic) {
            const member = members[0];
            var hasher = std.crypto.hash.Blake3.init(.{});
            hasher.update("roc.artifact.address.v1");
            hasher.update("acyclic");
            try hashArtifactContent(a, &hasher, set.artifacts[member], names);
            hashEdges(&hasher, edges[member], condensation.component_of, component, addresses);
            hasher.final(&addresses[member]);
            continue;
        }
        // Members start in classes by their own rendering, with in-group
        // edges as such, and are refined over those edges until the number
        // of classes is stable: then two members in one class are bisimilar
        // and place the same bytes.
        const ids = try a.alloc([32]u8, members.len);
        const refined = try a.alloc([32]u8, members.len);
        const position = try a.alloc(u32, count);
        for (members, 0..) |member, rank| position[member] = @intCast(rank);
        for (members, ids) |member, *id| {
            var hasher = std.crypto.hash.Blake3.init(.{});
            hasher.update("roc.artifact.local.v1");
            try hashArtifactContent(a, &hasher, set.artifacts[member], names);
            hashEdges(&hasher, edges[member], condensation.component_of, component, addresses);
            hasher.final(id);
        }
        var classes = try distinctCount(a, ids);
        while (true) {
            for (members, refined) |member, *id| {
                var hasher = std.crypto.hash.Blake3.init(.{});
                hasher.update("roc.artifact.refine.v1");
                hasher.update(&ids[position[member]]);
                for (edges[member]) |target| {
                    const callee = target orelse continue;
                    if (condensation.component_of[callee] != component) continue;
                    hasher.update(&ids[position[callee]]);
                }
                hasher.final(id);
            }
            const refined_classes = try distinctCount(a, refined);
            @memcpy(ids, refined);
            if (refined_classes == classes) break;
            classes = refined_classes;
        }
        const sorted = try a.dupe([32]u8, ids);
        std.mem.sort([32]u8, sorted, {}, addressLessThan);
        var group_hasher = std.crypto.hash.Blake3.init(.{});
        group_hasher.update("roc.artifact.group.v1");
        for (sorted) |id| group_hasher.update(&id);
        var group: [32]u8 = undefined;
        group_hasher.final(&group);
        for (members, ids) |member, id| {
            var hasher = std.crypto.hash.Blake3.init(.{});
            hasher.update("roc.artifact.address.v1");
            hasher.update("cyclic");
            hasher.update(&group);
            hasher.update(&id);
            hasher.final(&addresses[member]);
        }
    }
    return addresses;
}

fn addressLessThan(_: void, left: [32]u8, right: [32]u8) bool {
    return std.mem.order(u8, &left, &right) == .lt;
}

/// How many distinct digests `ids` holds.
fn distinctCount(arena: Allocator, ids: []const [32]u8) Allocator.Error!usize {
    var seen = std.AutoHashMap([32]u8, void).init(arena);
    for (ids) |id| try seen.put(id, {});
    return seen.count();
}

/// Hash an artifact's edges: each resolved edge out of the component by the
/// address of the artifact it reaches, each edge within the component as
/// such (the component's refinement names it), and each unresolved one as
/// unresolved.
fn hashEdges(hasher: *std.crypto.hash.Blake3, edges: []const ?u32, component_of: []const u32, component: usize, addresses: []const [32]u8) void {
    hashWord(hasher, @intCast(edges.len));
    for (edges) |target| {
        const callee = target orelse {
            hasher.update("unresolved");
            continue;
        };
        if (component_of[callee] == component) {
            hasher.update("intra");
        } else {
            hasher.update("address");
            hasher.update(&addresses[callee]);
        }
    }
}

fn hashWord(hasher: *std.crypto.hash.Blake3, value: u32) void {
    var buffer: [4]u8 = undefined;
    std.mem.writeInt(u32, &buffer, value, .little);
    hasher.update(&buffer);
}

fn hashWide(hasher: *std.crypto.hash.Blake3, value: u64) void {
    var buffer: [8]u8 = undefined;
    std.mem.writeInt(u64, &buffer, value, .little);
    hasher.update(&buffer);
}

fn hashBytes(hasher: *std.crypto.hash.Blake3, bytes: []const u8) void {
    hashWord(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn hashSymbolicTarget(hasher: *std.crypto.hash.Blake3, target: @FieldType(ProcArtifact.SymbolicReference, "target")) void {
    switch (target) {
        .proc => |identity| {
            hasher.update("proc");
            hasher.update(&identity.bytes);
        },
        .rc_helper => |name| {
            hasher.update("helper");
            hashBytes(hasher, name);
        },
        .boxy_thunk => |identity| {
            hasher.update("thunk");
            hasher.update(&identity.bytes);
        },
    }
}

fn hashMaterialization(hasher: *std.crypto.hash.Blake3, declaration: lir.Program.RootMaterialization) void {
    hasher.update(&declaration.module);
    switch (declaration.producer) {
        .checked => |root| {
            hasher.update("checked");
            hashWord(hasher, root);
        },
        .literal => |literal| {
            hasher.update("literal");
            hashWord(hasher, literal.checked_expr);
            hashWord(hasher, @backingInt(literal.kind));
            hasher.update(&literal.conversion_identity);
        },
    }
    hasher.update(&declaration.representation_digest);
    hasher.update(&declaration.layout_digest);
    hasher.update(&declaration.wrapper_identity);
    hashWord(hasher, @backingInt(declaration.abi));
}

/// Hash what an artifact's own rows record, in the order a pack writes them,
/// with its edges left to `hashEdges`: a reference's target and a data
/// relocation's local code target are edges, not content. Carried data is
/// rendered by structure, and a name that means a carried item (one a pack
/// names by content) by the item's position in the artifact's data: a
/// content name is derived from the item and the set it was encoded in, so
/// two packs may carry one artifact under different data names, and the
/// address must not see that.
fn hashArtifactContent(arena: Allocator, hasher: *std.crypto.hash.Blake3, artifact: ProcArtifact.Artifact, names: *const ProcArtifact.ContentNames) Allocator.Error!void {
    var items = std.StringHashMap(u32).init(arena);
    for (artifact.data, 0..) |item, index| try items.put(item.name, @intCast(index));
    const Named = struct {
        fn hash(h: *std.crypto.hash.Blake3, carried: *const std.StringHashMap(u32), content_names: *const ProcArtifact.ContentNames, name: []const u8) void {
            if (content_names.renames(name)) if (carried.get(name)) |index| {
                h.update("carried");
                hashWord(h, index);
                return;
            };
            h.update("named");
            hashBytes(h, content_names.of(name));
        }
    };
    switch (artifact.kind) {
        .proc => |identity| {
            hasher.update("proc");
            hasher.update(&identity.bytes);
        },
        .rc_helper => |name| {
            hasher.update("helper");
            hashBytes(hasher, name);
        },
        .boxy_thunk => |identity| {
            hasher.update("thunk");
            hasher.update(&identity.bytes);
        },
        .entrypoint => hasher.update("entrypoint"),
        .message_pool_run => hasher.update("message-pool-run"),
        .branch_island => hasher.update("branch-island"),
    }
    hashBytes(hasher, artifact.code);
    hashWord(hasher, artifact.entry);
    if (artifact.frame) |frame| {
        hasher.update("frame");
        hashWord(hasher, frame.prologue_size);
        hashWord(hasher, frame.stack_alloc);
        hashWord(hasher, frame.frame_size);
        hashWord(hasher, frame.callee_saved_mask);
        hashWord(hasher, frame.epilogue_offset);
        hasher.update(&.{@intFromBool(frame.uses_frame_pointer)});
    } else hasher.update("no-frame");
    hashWord(hasher, @intCast(artifact.refs.len));
    for (artifact.refs) |ref| {
        hashWord(hasher, ref.site);
        hashWord(hasher, @backingInt(ref.form));
        hashWord(hasher, ref.delta);
        hashWord(hasher, ref.veneer orelse std.math.maxInt(u32));
    }
    hashWord(hasher, @intCast(artifact.symbolic_refs.len));
    for (artifact.symbolic_refs) |ref| {
        hashWord(hasher, ref.site);
        hashWord(hasher, @backingInt(ref.form));
        hashWord(hasher, ref.veneer orelse std.math.maxInt(u32));
        hashSymbolicTarget(hasher, ref.target);
    }
    hashWord(hasher, @intCast(artifact.relocations.len));
    for (artifact.relocations) |relocation| {
        hashWord(hasher, relocation.offset);
        // A pack names carried program data by content and in shared scope.
        const scope: ProcArtifact.SymbolScope = if (names.renames(relocation.name)) .shared else relocation.scope;
        hashWord(hasher, @backingInt(scope));
        switch (relocation.kind) {
            .function => hasher.update("function"),
            .data => |kind| {
                hasher.update("data");
                hashWord(hasher, @backingInt(kind));
            },
        }
        Named.hash(hasher, &items, names, relocation.name);
    }
    hashWord(hasher, @intCast(artifact.data.len));
    for (artifact.data) |item| {
        hashBytes(hasher, item.bytes);
        hashWord(hasher, item.alignment);
        hashWord(hasher, item.symbol_offset);
        hasher.update(&.{@intFromBool(item.program_local_name)});
        hashWord(hasher, @intCast(item.relocations.len));
        for (item.relocations) |relocation| {
            hashWord(hasher, relocation.offset);
            hashWide(hasher, @bitCast(relocation.addend));
            hasher.update(&.{ @intFromBool(relocation.function), @intFromBool(relocation.external) });
            // An external binding names the linking image's symbol.
            if (relocation.external) {
                hasher.update("external");
                hashBytes(hasher, relocation.name);
            } else Named.hash(hasher, &items, names, relocation.name);
            if (relocation.code_target) |target| switch (target) {
                .local => hasher.update("local"),
                .symbolic => |symbol| {
                    hasher.update("symbolic");
                    hashSymbolicTarget(hasher, symbol);
                },
                // Native-image offsets have no persistent representation.
                .image_offset => unreachable,
            } else hasher.update("no-code");
        }
    }
    hashWord(hasher, @intCast(artifact.source_events.len));
    for (artifact.source_events) |certificate| {
        Named.hash(hasher, &items, names, certificate.data_name);
        hasher.update(std.mem.asBytes(&certificate.descriptor));
        hashWord(hasher, @intCast(certificate.branch_regions.len));
        for (certificate.branch_regions) |region| {
            hashWord(hasher, region.start.offset);
            hashWord(hasher, region.end.offset);
        }
        if (certificate.site_origin) |origin| switch (origin) {
            .checked_expr => |id| {
                hasher.update("expr");
                hashWord(hasher, id);
            },
            .checked_pattern => |id| {
                hasher.update("pattern");
                hashWord(hasher, id);
            },
        } else hasher.update("no-site");
    }
    hashWord(hasher, @intCast(artifact.materializations.len));
    for (artifact.materializations) |dependency| {
        Named.hash(hasher, &items, names, dependency.binding_name);
        Named.hash(hasher, &items, names, dependency.header_name);
        hashMaterialization(hasher, dependency.materialization);
    }
    if (artifact.root_materialization) |declaration| {
        hasher.update("materializer");
        hashMaterialization(hasher, declaration);
    } else hasher.update("no-materializer");
    if (artifact.context_contract) |contract| {
        hasher.update("contract");
        hashWord(hasher, @backingInt(contract.target));
        hashWord(hasher, @backingInt(contract.cpu_level));
        hasher.update(&.{ @intFromBool(contract.hot_reload), @intFromBool(contract.default_platform_runtime) });
    } else hasher.update("no-contract");
    if (artifact.context_dependencies) |dependencies| {
        hasher.update("dependencies");
        hasher.update(&.{ @intFromBool(dependencies.static_data), @intFromBool(dependencies.boxy_runtime_entry), @intFromBool(dependencies.boxy_runtime), @intFromBool(dependencies.dict_seed) });
    } else hasher.update("no-dependencies");
    hasher.update(&.{@intFromBool(artifact.context_complete)});
    if (artifact.callable_contract) |contract| {
        hasher.update("callable");
        hasher.update(&contract);
    } else hasher.update("no-callable");
}

fn resolveSymbolic(self: *const Self, target: @FieldType(ProcArtifact.SymbolicReference, "target")) ?u32 {
    const definition = switch (target) {
        .proc => |identity| self.procs.get(identity),
        .boxy_thunk => |identity| self.thunks.get(identity),
        .rc_helper => |name| self.helpers.get(name),
    } orelse return null;
    return if (self.ambiguous_definitions.contains(definition)) null else definition;
}

fn indexDefinition(self: *Self, map: anytype, key: anytype, index: u32) Allocator.Error!void {
    const entry = try map.getOrPut(key);
    if (!entry.found_existing) {
        entry.value_ptr.* = index;
        return;
    }
    const previous = entry.value_ptr.*;
    const artifact = self.set.artifacts[index];
    const supported = artifact.context_complete and artifact.context_contract != null and artifact.context_dependencies != null and
        (switch (artifact.kind) {
            .proc, .boxy_thunk => artifact.callable_contract != null,
            .rc_helper, .entrypoint, .message_pool_run, .branch_island => true,
        });
    if (!supported or self.ambiguous_definitions.contains(previous) or
        !sameValue(ProcArtifact.Artifact, artifact, self.set.artifacts[previous]))
    {
        try self.ambiguous_definitions.put(previous, {});
    }
}

pub fn deinit(self: *Self) void {
    self.ambiguous_definitions.deinit();
    self.allocator.free(self.symbolic_targets);
    self.allocator.free(self.symbolic_offsets);
    self.allocator.free(self.data_targets);
    self.allocator.free(self.data_offsets);
    self.order.deinit(self.allocator);
    self.seen.deinit();
    self.helpers.deinit();
    self.thunks.deinit();
    self.procs.deinit();
}

/// Null preserves an external symbolic reference; it is never guessed to name
/// a local definition. Serving-closure admission rejects unresolved dependencies.
pub fn symbolicTargets(self: *const Self, artifact: u32) []const ?u32 {
    return self.symbolic_targets[self.symbolic_offsets[artifact]..self.symbolic_offsets[artifact + 1]];
}

/// Resolved code targets of `artifact`'s data relocations that name code, in
/// relocation order. Null marks an unresolved symbolic target, as in `symbolicTargets`.
pub fn dataTargets(self: *const Self, artifact: u32) []const ?u32 {
    return self.data_targets[self.data_offsets[artifact]..self.data_offsets[artifact + 1]];
}

/// Artifact indices valid until the next query. Missing symbolic definitions
/// make this an explicitly incomplete closure, not an external-code guess.
pub fn of(self: *Self, root: u32) Allocator.Error![]const u32 {
    return self.ofMany(&.{root});
}

/// Like `of`, for the union of the closures of every root.
pub fn ofMany(self: *Self, roots: []const u32) Allocator.Error![]const u32 {
    self.seen.clearRetainingCapacity();
    self.order.clearRetainingCapacity();
    self.complete = true;
    for (roots) |root| try self.visit(root);
    var cursor: usize = 0;
    while (cursor < self.order.items.len) : (cursor += 1) {
        const node = self.order.items[cursor];
        const artifact = self.set.artifacts[node];
        for (artifact.refs) |ref| try self.visit(ref.target);
        for (self.symbolicTargets(node)) |target| {
            if (target) |index| {
                try self.visit(index);
            } else {
                self.complete = false;
            }
        }
        for (self.dataTargets(node)) |target| {
            if (target) |index| {
                try self.visit(index);
            } else {
                self.complete = false;
            }
        }
    }
    return self.order.items;
}

fn visit(self: *Self, index: u32) Allocator.Error!void {
    if ((try self.seen.getOrPut(index)).found_existing) return;
    try self.order.append(self.allocator, index);
}

/// One immutable condensation of the exact serving graph. Components are
/// numbered caller-before-callee, so consumers union callee facts once in
/// reverse order. Recursive procedures share a component, not provisional facts.
pub const Components = struct {
    arena: collections.SingleThreadArena,
    nodes: []const u32,
    callees: []const []const u32,

    pub fn deinit(self: *Components) void {
        self.arena.deinit();
    }
};

/// Condense the serving graph into its strongly connected components.
pub fn components(self: *const Self) Allocator.Error!Components {
    var arena = collections.SingleThreadArena.init(self.allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    const count = self.set.artifacts.len;
    const callers = try a.alloc(std.ArrayList(u32), count);
    @memset(callers, .empty);
    const callees = try a.alloc(std.ArrayList(u32), count);
    @memset(callees, .empty);
    for (self.set.artifacts, 0..) |artifact, index| {
        for (artifact.refs) |ref| {
            try callers[ref.target].append(a, @intCast(index));
            try callees[index].append(a, ref.target);
        }
        for (self.symbolicTargets(@intCast(index))) |target| {
            if (target) |callee| {
                try callers[callee].append(a, @intCast(index));
                try callees[index].append(a, callee);
            }
        }
        for (self.dataTargets(@intCast(index))) |target| {
            if (target) |callee| {
                try callers[callee].append(a, @intCast(index));
                try callees[index].append(a, callee);
            }
        }
    }
    const visited = try a.alloc(bool, count);
    @memset(visited, false);
    const Frame = struct { node: u32, next: usize = 0 };
    var frames = std.ArrayList(Frame).empty;
    var order = std.ArrayList(u32).empty;
    for (0..count) |start| {
        if (visited[start]) continue;
        visited[start] = true;
        try frames.append(a, .{ .node = @intCast(start) });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            if (frame.next == callees[frame.node].items.len) {
                try order.append(a, frame.node);
                _ = frames.pop();
                continue;
            }
            const callee = callees[frame.node].items[frame.next];
            frame.next += 1;
            if (visited[callee]) continue;
            visited[callee] = true;
            try frames.append(a, .{ .node = callee });
        }
    }
    const nodes = try a.alloc(u32, count);
    @memset(nodes, std.math.maxInt(u32));
    var component_count: u32 = 0;
    var pending = std.ArrayList(u32).empty;
    while (order.pop()) |start| {
        if (nodes[start] != std.math.maxInt(u32)) continue;
        nodes[start] = component_count;
        try pending.append(a, start);
        while (pending.pop()) |node| for (callers[node].items) |caller| {
            if (nodes[caller] != std.math.maxInt(u32)) continue;
            nodes[caller] = component_count;
            try pending.append(a, caller);
        };
        component_count += 1;
    }
    const edges = try a.alloc(std.AutoHashMap(u32, void), component_count);
    for (edges) |*set| set.* = std.AutoHashMap(u32, void).init(a);
    for (callees, 0..) |targets, node| {
        for (targets.items) |callee| {
            if (nodes[callee] != nodes[node]) try edges[nodes[node]].put(nodes[callee], {});
        }
    }
    const output = try a.alloc([]const u32, component_count);
    for (edges, output, 0..) |*set, *owned, component| {
        const targets = try a.alloc(u32, set.count());
        var iterator = set.keyIterator();
        var index: usize = 0;
        while (iterator.next()) |callee| : (index += 1) {
            std.debug.assert(callee.* > component);
            targets[index] = callee.*;
        }
        owned.* = targets;
    }
    return .{ .arena = arena, .nodes = nodes, .callees = output };
}

/// Admit all roots in graph-proportional work. Invalid node facts propagate
/// backwards through exact dependencies; recursive closed graphs need no
/// provisional "hit" or repeated transitive walk.
pub fn admit(self: *const Self, context: *anyopaque, accepts: *const fn (*anyopaque, ProcArtifact.Artifact) bool) Allocator.Error![]bool {
    const allocator = self.allocator;
    const valid = try allocator.alloc(bool, self.set.artifacts.len);
    errdefer allocator.free(valid);
    for (self.set.artifacts, 0..) |artifact, index| valid[index] = accepts(context, artifact);
    try self.propagateInvalid(valid);
    return valid;
}

/// Propagate caller-supplied node facts over the same resolved edges used by
/// closure selection and placement.
pub fn propagateInvalid(self: *const Self, valid: []bool) Allocator.Error!void {
    const allocator = self.allocator;
    std.debug.assert(valid.len == self.set.artifacts.len);
    const Edge = struct { from: u32, to: u32 };
    var edges = std.ArrayList(Edge).empty;
    defer edges.deinit(allocator);
    for (self.set.artifacts, 0..) |artifact, index| {
        for (artifact.refs) |ref| try edges.append(allocator, .{ .from = @intCast(index), .to = ref.target });
        for (self.symbolicTargets(@intCast(index))) |target| {
            if (target) |to| {
                try edges.append(allocator, .{ .from = @intCast(index), .to = to });
            } else {
                valid[index] = false;
            }
        }
        for (self.dataTargets(@intCast(index))) |target| {
            if (target) |to| {
                try edges.append(allocator, .{ .from = @intCast(index), .to = to });
            } else {
                valid[index] = false;
            }
        }
        for (artifact.data) |item| for (item.relocations) |relocation| {
            if (relocation.function and !relocation.external and relocation.code_target == null) valid[index] = false;
        };
    }
    const offsets = try allocator.alloc(usize, valid.len + 1);
    defer allocator.free(offsets);
    @memset(offsets, 0);
    for (edges.items) |edge| offsets[edge.to + 1] += 1;
    for (1..offsets.len) |index| offsets[index] += offsets[index - 1];
    const cursors = try allocator.dupe(usize, offsets[0..valid.len]);
    defer allocator.free(cursors);
    const predecessors = try allocator.alloc(u32, edges.items.len);
    defer allocator.free(predecessors);
    for (edges.items) |edge| {
        predecessors[cursors[edge.to]] = edge.from;
        cursors[edge.to] += 1;
    }
    var invalid = std.ArrayList(u32).empty;
    defer invalid.deinit(allocator);
    for (valid, 0..) |accept, index| if (!accept) {
        try invalid.append(allocator, @intCast(index));
    };
    var cursor: usize = 0;
    while (cursor < invalid.items.len) : (cursor += 1) {
        const node = invalid.items[cursor];
        for (predecessors[offsets[node]..offsets[node + 1]]) |predecessor| {
            if (!valid[predecessor]) continue;
            valid[predecessor] = false;
            try invalid.append(allocator, predecessor);
        }
    }
}

/// A deep-owned subset of an artifact set, with each original index's new index (null when not selected).
pub const Selection = struct {
    set: ProcArtifact.Set,
    /// Borrowed from `set`'s arena.
    old_to_new: []const ?u32,

    pub fn deinit(self: *Selection) void {
        self.set.deinit();
    }
};

fn sameValue(comptime T: type, left: T, right: T) bool {
    return switch (@typeInfo(T)) {
        .pointer => |info| blk: {
            if (info.size != .slice) @compileError("persistent contracts cannot contain pointers");
            if (left.len != right.len) break :blk false;
            for (left, right) |a, b| if (!sameValue(info.child, a, b)) break :blk false;
            break :blk true;
        },
        .@"struct" => |info| blk: {
            inline for (info.field_names, info.field_types) |field_name, field_type| {
                if (!sameValue(field_type, @field(left, field_name), @field(right, field_name))) break :blk false;
            }
            break :blk true;
        },
        .@"union" => |info| blk: {
            if (std.meta.activeTag(left) != std.meta.activeTag(right)) break :blk false;
            inline for (info.field_names, info.field_types) |field_name, field_type| {
                if (std.mem.eql(u8, @tagName(left), field_name)) {
                    break :blk sameValue(field_type, @field(left, field_name), @field(right, field_name));
                }
            }
            unreachable;
        },
        .optional => |info| if (left) |a| if (right) |b| sameValue(info.child, a, b) else false else right == null,
        .array => |info| blk: {
            for (left, right) |a, b| if (!sameValue(info.child, a, b)) break :blk false;
            break :blk true;
        },
        .type,
        .void,
        .bool,
        .noreturn,
        .int,
        .float,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"enum",
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        .spirv,
        => std.meta.eql(left, right),
    };
}

/// Selection preserves the exact callable contract and real native codegen
/// policy. Execution consumers bind the same symbolic body to their own data
/// images and optional source observers; execution phase is not a variant.
pub fn compatibleDefinition(original: ProcArtifact.Artifact, selected: ProcArtifact.Artifact) bool {
    const left = original.callable_contract orelse return false;
    const right = selected.callable_contract orelse return false;
    if (!std.mem.eql(u8, &left, &right)) return false;
    const a = original.context_contract orelse return false;
    const b = selected.context_contract orelse return false;
    if (a.target != b.target or a.cpu_level != b.cpu_level or a.hot_reload != b.hot_reload) return false;
    if (a.default_platform_runtime != b.default_platform_runtime) return false;
    return true;
}

/// An immutable physical-variant image plan. Missing mappings are rejected offers,
/// established before a cache reader can authorize source-body elision.
pub const Variants = struct {
    set: ProcArtifact.Set,
    source_offsets: []const u32,
    old_to_new: []const ?u32,
    /// Physical source-set provenance of each actually selected definition.
    new_to_source: []const u32,

    pub fn deinit(self: *Variants) void {
        self.set.deinit();
    }
};

/// Combine physical artifact sets into one owned set of the definitions whose
/// closures resolve within their own source and are accepted by `accepts`.
pub fn prepareVariants(
    allocator: Allocator,
    sources: []const *const ProcArtifact.Set,
    context: *anyopaque,
    accepts: *const fn (*anyopaque, ProcArtifact.Artifact) bool,
) Allocator.Error!Variants {
    // Preparation borrows payloads; only the accepted physical graph is
    // deep-owned. Avoid cloning whole packs twice on every warm preparation.
    var combined = ProcArtifact.Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{},
    };
    defer combined.deinit();
    const a = combined.arena.allocator();
    var joined = std.ArrayList(ProcArtifact.Artifact).empty;
    // Each source's lines name its own files; the combined set names the
    // files of every source once, in first-source order.
    var source_files = std.ArrayList([32]u8).empty;
    for (sources) |source| {
        const base: u32 = @intCast(joined.items.len);
        const file_map = try a.alloc(u32, source.source_files.len);
        var identity_map = true;
        for (source.source_files, file_map, 0..) |identity, *mapped, file| {
            mapped.* = for (source_files.items, 0..) |existing, index| {
                if (std.mem.eql(u8, &existing, &identity)) break @intCast(index);
            } else index: {
                try source_files.append(a, identity);
                break :index @intCast(source_files.items.len - 1);
            };
            if (mapped.* != file) identity_map = false;
        }
        for (source.artifacts) |original| {
            var artifact = original;
            const refs = try a.dupe(ProcArtifact.Reference, original.refs);
            for (refs) |*ref| ref.target += base;
            artifact.refs = refs;
            if (!identity_map) {
                const lines = try a.dupe(ProcArtifact.LineEntry, original.lines);
                for (lines) |*line| line.loc = ProcArtifact.rebindLoc(line.loc, file_map);
                artifact.lines = lines;
            }
            try joined.append(a, artifact);
        }
    }
    combined.artifacts = joined.items;
    combined.source_files = source_files.items;
    const artifacts = @constCast(combined.artifacts);
    const valid = try a.alloc(bool, artifacts.len);
    @memset(valid, true);
    const offsets = try a.alloc(u32, sources.len + 1);
    offsets[0] = 0;
    // Resolve within each original namespace before sources are combined.
    // Otherwise an edge could acquire another physical source's ABI.
    for (sources, 0..) |source, source_index| {
        offsets[source_index + 1] = offsets[source_index] + @as(u32, @intCast(source.artifacts.len));
        var local = try Self.init(allocator, source);
        defer local.deinit();
        for (source.artifacts, 0..) |original, index| {
            const artifact = &artifacts[offsets[source_index] + index];
            var refs = std.ArrayList(ProcArtifact.Reference).empty;
            try refs.appendSlice(a, artifact.refs);
            var external = std.ArrayList(ProcArtifact.SymbolicReference).empty;
            for (original.symbolic_refs, local.symbolicTargets(@intCast(index)), 0..) |ref, target, ref_index| {
                if (target) |node| {
                    try refs.append(a, .{
                        .site = ref.site,
                        .form = ref.form,
                        .target = offsets[source_index] + node,
                        .delta = source.artifacts[node].entry,
                        .veneer = ref.veneer,
                    });
                } else {
                    // Preparation borrows this name; selection deep-owns it.
                    try external.append(a, artifact.symbolic_refs[ref_index]);
                    valid[offsets[source_index] + index] = false;
                }
            }
            artifact.refs = try refs.toOwnedSlice(a);
            artifact.symbolic_refs = try external.toOwnedSlice(a);
            const data = try a.dupe(ProcArtifact.DataItem, original.data);
            var data_cursor: usize = 0;
            for (data) |*item| {
                const relocations = try a.dupe(ProcArtifact.DataRelocation, item.relocations);
                for (relocations) |*relocation| {
                    if (relocation.code_target == null) continue;
                    const target = local.dataTargets(@intCast(index))[data_cursor];
                    data_cursor += 1;
                    if (target) |node| {
                        relocation.code_target = .{ .local = offsets[source_index] + node };
                    } else {
                        valid[offsets[source_index] + index] = false;
                    }
                }
                item.relocations = relocations;
            }
            artifact.data = data;
        }
    }
    var graph = try Self.init(allocator, &combined);
    defer graph.deinit();
    for (artifacts, 0..) |artifact, index| valid[index] = valid[index] and accepts(context, artifact);
    // Admission belongs to each physical closure independently. A logical
    // identity does not authorize substituting a sibling definition.
    try graph.propagateInvalid(valid);
    var selected = try selectMarked(allocator, &combined, valid);
    errdefer selected.deinit();
    const owned = selected.set.arena.allocator();
    const mapping = try owned.alloc(?u32, artifacts.len);
    for (mapping, 0..) |*mapped, index| mapped.* = selected.old_to_new[index];
    const owned_offsets = try owned.dupe(u32, offsets);
    const provenance = try owned.alloc(u32, selected.set.artifacts.len);
    for (sources, 0..) |_, source| {
        for (offsets[source]..offsets[source + 1]) |original| {
            if (selected.old_to_new[original]) |index| provenance[index] = @intCast(source);
        }
    }
    return .{
        .set = selected.set,
        .source_offsets = owned_offsets,
        .old_to_new = mapping,
        .new_to_source = provenance,
    };
}

test "physical closure preserves distinct bodies in both source and splice orders" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testCanonicalMixed, .{false});
}

test "physical closure owns an empty cold provider plan" {
    const Attempt = struct {
        fn accepts(_: *anyopaque, _: ProcArtifact.Artifact) bool {
            unreachable;
        }
        fn run(allocator: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
            var context: u8 = 0;
            var canonical = try prepareVariants(allocator, &.{}, &context, accepts);
            defer canonical.deinit();
            try std.testing.expectEqual(@as(usize, 0), canonical.set.artifacts.len);
            try std.testing.expectEqualSlices(u32, &.{0}, canonical.source_offsets);
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Attempt.run, .{});
}

test "physical selection preserves static and seed serving closures" {
    const allocator = std.testing.allocator;
    for ([_]bool{ false, true }) |symbolic| {
        for ([_]bool{ false, true }) |reverse| {
            var runtime: [3]ProcArtifact.Artifact = undefined;
            for (&runtime, 0..) |*artifact, index| artifact.* = .{
                .kind = .{ .proc = lir.ProcIdentity.forTest(@intCast(index + 1)) },
                .code = "\xe8\x00\x00\x00\x00\xc3",
                .entry = 0,
                .frame = null,
                .refs = &.{},
                .relocations = &.{},
                .data = &.{},
                .callable_contract = @as([32]u8, @splat(11)),
                .context_contract = .{
                    .target = .x64linux,
                    .cpu_level = .default,
                    .hot_reload = false,
                    .default_platform_runtime = false,
                },
                .context_dependencies = .{},
            };
            runtime[1].context_dependencies = .{ .dict_seed = true };
            runtime[2].context_dependencies = .{ .static_data = true };
            if (symbolic) {
                runtime[0].symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) } }};
                runtime[1].symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(3) } }};
                runtime[2].symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) } }};
            } else {
                runtime[0].refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
                runtime[1].refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }};
                runtime[2].refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
            }
            var ctfe = runtime;
            for (&ctfe) |*artifact| {
                artifact.context_contract.?.default_platform_runtime = true;
            }
            // Exact physical edges retain their original codegen policy.
            try std.testing.expect(!compatibleDefinition(runtime[2], ctfe[2]));
            try std.testing.expect(!compatibleDefinition(ctfe[1], runtime[1]));
            const rt = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &runtime };
            const cf = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &ctfe };
            const sources = if (reverse) [_]*const ProcArtifact.Set{ &cf, &rt } else [_]*const ProcArtifact.Set{ &rt, &cf };
            const accepts = struct {
                fn node(_: *anyopaque, _: ProcArtifact.Artifact) bool {
                    return true;
                }
            }.node;
            var context: u8 = 0;
            for (0..sources.len) |wanted_source| {
                var image = try prepareVariants(allocator, &sources, &context, accepts);
                defer image.deinit();
                const root = image.old_to_new[image.source_offsets[wanted_source]].?;
                var graph = try Self.init(allocator, &image.set);
                defer graph.deinit();
                const placed = try graph.of(root);
                try std.testing.expect(graph.complete);
                try std.testing.expectEqual(@as(usize, 3), placed.len);
                for (placed) |index| try std.testing.expectEqual(@as(u32, @intCast(wanted_source)), image.new_to_source[index]);
            }
        }
    }
}

test "variant graph preserves independently valid conflicting ARC and emitter closures" {
    try testCanonicalMixed(std.testing.allocator, true);
}

fn testCanonicalMixed(allocator: Allocator, conflict: bool) (ProcArtifact.ExtractError || error{ TestUnexpectedResult, TestExpectedEqual })!void {
    const CG = @import("LirCodeGen.zig").LirCodeGen(@import("roc_target").RocTarget.x64linux);
    const b_identity = lir.ProcIdentity.forTest(2);
    var runtime = [_]ProcArtifact.Artifact{
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
            .code = "\xe8\x00\x00\x00\x00\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }},
            .relocations = &.{},
            .data = &.{},
        },
        .{
            .kind = .{ .proc = b_identity },
            .code = "\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        },
    };
    for (&runtime) |*artifact| {
        artifact.callable_contract = @as([32]u8, @splat(1));
        artifact.context_contract = .{
            .target = .x64linux,
            .cpu_level = .default,
            .hot_reload = false,
            .default_platform_runtime = false,
        };
        artifact.context_dependencies = .{};
    }
    var ctfe = runtime;
    ctfe[0].kind = .{ .proc = lir.ProcIdentity.forTest(3) };
    ctfe[1].code = "\x48\xb8" ++ "\x00\x00\x00\x00\x00\x00\x00\x00" ++ "\x48\xb8" ++ "\x00\x00\x00\x00\x00\x00\x00\x00" ++ "\xc3";
    if (conflict) runtime[1].callable_contract = @as([32]u8, @splat(9));
    var runtime_set = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &runtime };
    defer runtime_set.deinit();
    var ctfe_set = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &ctfe };
    defer ctfe_set.deinit();
    const Accept = struct {
        fn node(_: *anyopaque, _: ProcArtifact.Artifact) bool {
            return true;
        }
    };
    var context: u8 = 0;
    for ([_]bool{ false, true }) |reverse_sources| {
        const sources = if (reverse_sources) [_]*const ProcArtifact.Set{ &ctfe_set, &runtime_set } else [_]*const ProcArtifact.Set{ &runtime_set, &ctfe_set };
        var canonical = try prepareVariants(allocator, &sources, &context, Accept.node);
        defer canonical.deinit();
        const runtime_source: usize = if (reverse_sources) 1 else 0;
        const ctfe_source: usize = if (reverse_sources) 0 else 1;
        const a = canonical.old_to_new[canonical.source_offsets[runtime_source]];
        const c = canonical.old_to_new[canonical.source_offsets[ctfe_source]].?;
        try std.testing.expect(a != null);
        const b = canonical.set.artifacts[c].refs[0].target;
        const runtime_b = canonical.set.artifacts[a.?].refs[0].target;
        try std.testing.expect(b != runtime_b);
        try std.testing.expectEqualDeep(runtime[1], canonical.set.artifacts[runtime_b]);
        try std.testing.expectEqualDeep(ctfe[1], canonical.set.artifacts[b]);
        try std.testing.expectEqual(@as(u32, 1), runtime[0].refs[0].target);
        try std.testing.expectEqual(@as(u32, 1), ctfe[0].refs[0].target);
        inline for (.{ false, true }) |reverse_roots| {
            var store = lir.LirStore.init(allocator);
            defer store.deinit();
            var layouts = try @import("layout").Store.init(allocator, .u64);
            defer layouts.deinit();
            var receiver = try CG.init(allocator, &store, &layouts, .{}, &.{}, .default);
            defer receiver.deinit();
            var graph = try Self.init(allocator, &canonical.set);
            defer graph.deinit();
            var procs = std.AutoHashMap(u32, lir.LIR.LirProcSpecId).init(allocator);
            defer procs.deinit();
            var placed = std.AutoHashMap(u32, usize).init(allocator);
            defer placed.deinit();
            var data = std.ArrayList(ProcArtifact.DataItem).empty;
            defer data.deinit(allocator);
            const roots = if (reverse_roots) [_]u32{ c, a.? } else [_]u32{ a.?, c };
            for (roots) |root| try ProcArtifact.spliceIndexed(CG, allocator, &receiver, &graph, &.{root}, &procs, &placed, &data);
            try std.testing.expectEqual(@as(usize, 4), placed.count());
            try receiver.finishImage();
            var recaptured = try ProcArtifact.extract(CG, allocator, &receiver, store.getProcSpecs(), &layouts, &.{}, &.{}, &.{});
            defer recaptured.deinit();
            var found_physical_definition = false;
            for (recaptured.artifacts) |artifact| {
                if (artifact.kind != .proc or !std.meta.eql(artifact.kind.proc, b_identity) or
                    !std.mem.eql(u8, artifact.code, ctfe[1].code)) continue;
                try std.testing.expectEqualDeep(canonical.set.artifacts[b], artifact);
                found_physical_definition = true;
            }
            try std.testing.expect(found_physical_definition);
        }
    }
    // A real codegen policy mismatch remains incompatible, even with the
    // same ARC/ABI stamp. Execution phase and seed binding are not policies.
    runtime[1].callable_contract = ctfe[1].callable_contract;
    runtime[1].context_dependencies.?.dict_seed = true;
    try std.testing.expect(compatibleDefinition(runtime[1], ctfe[1]));
    runtime[1].context_contract.?.default_platform_runtime = true;
    try std.testing.expect(!compatibleDefinition(runtime[1], ctfe[1]));
    runtime[1].context_contract.?.default_platform_runtime = false;
    // Both consumers retain their own seed contracts in either physical load
    // order. No callee substitution is needed to serve either root.
    ctfe[1].code = "\xc3";
    ctfe[1].context_dependencies = .{ .dict_seed = true };
    for ([_]bool{ false, true }) |seed_dependent| {
        runtime[1].context_dependencies.?.dict_seed = seed_dependent;
        ctfe[1].context_dependencies.?.dict_seed = seed_dependent;
        for ([_]bool{ false, true }) |reverse_sources| {
            const sources = if (reverse_sources) [_]*const ProcArtifact.Set{ &ctfe_set, &runtime_set } else [_]*const ProcArtifact.Set{ &runtime_set, &ctfe_set };
            var canonical = try prepareVariants(allocator, &sources, &context, Accept.node);
            defer canonical.deinit();
            const runtime_source: usize = if (reverse_sources) 1 else 0;
            const ctfe_source: usize = if (reverse_sources) 0 else 1;
            const root = canonical.old_to_new[canonical.source_offsets[runtime_source]].?;
            const b = canonical.set.artifacts[root].refs[0].target;
            const neutral_root = canonical.old_to_new[canonical.source_offsets[ctfe_source]];
            try std.testing.expect(neutral_root != null);
            const other = canonical.set.artifacts[neutral_root.?].refs[0].target;
            try std.testing.expect(b != other);
            try std.testing.expectEqual(@as(u32, @intCast(ctfe_source)), canonical.new_to_source[other]);
            try std.testing.expectEqual(@as(u32, @intCast(runtime_source)), canonical.new_to_source[b]);
        }
    }
}

test "artifact admission evaluates each chain node once and propagates invalid ancestors" {
    const count = 128;
    var artifacts: [count]ProcArtifact.Artifact = undefined;
    var refs: [count - 1]ProcArtifact.Reference = undefined;
    for (&artifacts, 0..) |*artifact, index| {
        artifact.* = .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(@intCast(index)) },
            .code = "",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        };
        if (index + 1 < count) {
            refs[index] = .{ .site = 0, .form = .call, .target = @intCast(index + 1), .delta = 0 };
            artifact.refs = refs[index..][0..1];
        }
    }
    var source = ProcArtifact.Set{
        .arena = collections.SingleThreadArena.init(std.testing.allocator),
        .artifacts = &artifacts,
    };
    defer source.deinit();
    var graph = try Self.init(std.testing.allocator, &source);
    defer graph.deinit();
    const Probe = struct {
        calls: usize = 0,
        fn accepts(context: *anyopaque, artifact: ProcArtifact.Artifact) bool {
            const self: *@This() = @ptrCast(@alignCast(context));
            self.calls += 1;
            return !std.meta.eql(artifact.kind.proc, lir.ProcIdentity.forTest(count - 1));
        }
    };
    var probe = Probe{};
    const eligible = try graph.admit(&probe, Probe.accepts);
    defer std.testing.allocator.free(eligible);
    try std.testing.expectEqual(count, probe.calls);
    for (eligible) |valid| try std.testing.expect(!valid);
}

test "symbolic index resolves a chain once and preserves cycles external names and kind namespaces" {
    const count = 128;
    var artifacts: [count + 2]ProcArtifact.Artifact = undefined;
    var refs: [count]ProcArtifact.SymbolicReference = undefined;
    for (&artifacts, 0..) |*artifact, index| {
        artifact.* = .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(@intCast(index)) },
            .code = "",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        };
        if (index < count) {
            refs[index] = .{
                .site = 0,
                .form = .call,
                .target = .{ .proc = lir.ProcIdentity.forTest(@intCast((index + 1) % count)) },
            };
            artifact.symbolic_refs = refs[index..][0..1];
        }
    }
    artifacts[count].kind = .{ .boxy_thunk = lir.ProcIdentity.forTest(0) };
    const external = ProcArtifact.SymbolicReference{
        .site = 0,
        .form = .call,
        .target = .{ .rc_helper = "external_helper" },
    };
    artifacts[count + 1].symbolic_refs = &.{external};
    var source = ProcArtifact.Set{
        .arena = collections.SingleThreadArena.init(std.testing.allocator),
        .artifacts = &artifacts,
    };
    defer source.deinit();
    var graph = try Self.init(std.testing.allocator, &source);
    defer graph.deinit();
    try std.testing.expectEqual(count + 1, graph.symbolic_lookups);
    try std.testing.expectEqual(@as(?u32, 0), graph.procs.get(lir.ProcIdentity.forTest(0)));
    try std.testing.expectEqual(@as(?u32, count), graph.thunks.get(lir.ProcIdentity.forTest(0)));
    for (0..count) |root| {
        const closure = try graph.of(@intCast(root));
        try std.testing.expectEqual(count, closure.len);
        try std.testing.expect(graph.complete);
    }
    try std.testing.expectEqual(count + 1, graph.symbolic_lookups);
    try std.testing.expectEqual(@as(?u32, null), graph.symbolicTargets(count + 1)[0]);
    _ = try graph.of(count + 1);
    try std.testing.expect(!graph.complete);
}

test "symbolic index declines ambiguous same-identity contracts in either definition order" {
    const identity = lir.ProcIdentity.forTest(2);
    const reference = ProcArtifact.SymbolicReference{
        .site = 0,
        .form = .call,
        .target = .{ .proc = identity },
    };
    const runtime = ProcArtifact.Artifact{
        .kind = .{ .proc = identity },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .callable_contract = @as([32]u8, @splat(1)),
        .context_dependencies = .{},
        .context_contract = .{
            .target = .x64linux,
            .cpu_level = .default,
            .hot_reload = false,
            .default_platform_runtime = false,
        },
    };
    const Variant = enum { identical, different_code, conflicting, unsupported };
    for ([_]Variant{ .identical, .different_code, .conflicting, .unsupported }) |variant| {
        const conflict = variant != .identical;
        var ctfe = runtime;
        if (variant == .different_code) ctfe.code = "\x90\xc3";
        if (variant == .conflicting) ctfe.callable_contract = @as([32]u8, @splat(9));
        if (variant == .unsupported) ctfe.callable_contract = null;
        for ([_]bool{ false, true }) |reverse| {
            var artifacts = if (reverse) [_]ProcArtifact.Artifact{ runtime, ctfe, runtime, runtime } else [_]ProcArtifact.Artifact{ runtime, runtime, ctfe, runtime };
            artifacts[0].kind = .{ .proc = lir.ProcIdentity.forTest(1) };
            artifacts[0].symbolic_refs = &.{reference};
            const runtime_index: u32 = if (reverse) 2 else 1;
            const exact = ProcArtifact.Reference{ .site = 0, .form = .call, .target = runtime_index, .delta = 0 };
            artifacts[3].kind = .{ .proc = lir.ProcIdentity.forTest(3) };
            artifacts[3].refs = &.{exact};
            var source = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(std.testing.allocator), .artifacts = &artifacts };
            defer source.deinit();
            var graph = try Self.init(std.testing.allocator, &source);
            defer graph.deinit();
            try std.testing.expectEqual(conflict, graph.symbolicTargets(0)[0] == null);
            _ = try graph.of(0);
            try std.testing.expectEqual(!conflict, graph.complete);
            if (!conflict) {
                const target = graph.symbolicTargets(0)[0].?;
                try std.testing.expectEqualDeep(runtime, artifacts[target]);
            }
            _ = try graph.of(3);
            try std.testing.expect(graph.complete);
            const Accept = struct {
                fn node(_: *anyopaque, _: ProcArtifact.Artifact) bool {
                    return true;
                }
            };
            var context: u8 = 0;
            const admitted = try graph.admit(&context, Accept.node);
            defer std.testing.allocator.free(admitted);
            try std.testing.expectEqual(!conflict, admitted[0]);
            try std.testing.expect(admitted[3]);
            try std.testing.expect(admitted[runtime_index]);
        }
    }
}

/// Deep-own an explicitly selected closed graph, preserving stable identities
/// and rebasing only set-local references. Order is the producer's set order.
pub fn selectMarked(allocator: Allocator, source: *const ProcArtifact.Set, marked: []const bool) Allocator.Error!Selection {
    std.debug.assert(marked.len == source.artifacts.len);
    var temporary = collections.SingleThreadArena.init(allocator);
    defer temporary.deinit();
    const a = temporary.allocator();
    const remap = try a.alloc(?u32, marked.len);
    @memset(remap, null);
    var artifacts = std.ArrayList(ProcArtifact.Artifact).empty;
    for (source.artifacts, marked, 0..) |artifact, keep, index| {
        if (!keep) continue;
        remap[index] = @intCast(artifacts.items.len);
        try artifacts.append(a, artifact);
    }
    for (artifacts.items) |*artifact| {
        const refs = try a.dupe(ProcArtifact.Reference, artifact.refs);
        for (refs) |*ref| ref.target = remap[ref.target].?;
        artifact.refs = refs;
        const data = try a.dupe(ProcArtifact.DataItem, artifact.data);
        for (data) |*item| {
            const relocations = try a.dupe(ProcArtifact.DataRelocation, item.relocations);
            for (relocations) |*relocation| {
                if (relocation.code_target) |*target| {
                    if (target.* == .local) target.local = remap[target.local].?;
                }
            }
            item.relocations = relocations;
        }
        artifact.data = data;
    }
    const view = ProcArtifact.Set{ .arena = temporary, .artifacts = artifacts.items, .source_files = source.source_files };
    var set = try ProcArtifact.combine(allocator, &.{&view});
    errdefer set.deinit();
    const owned_remap = try set.arena.allocator().dupe(?u32, remap);
    return .{ .set = set, .old_to_new = owned_remap };
}

test "artifact closure selection owns context graphs and rebases cyclic references" {
    // Whether the testing allocator grows a block in place depends on its
    // neighbours, so the check runs over an allocator that never does.
    var deterministic = collections.DeterministicAllocator.init(std.testing.allocator);
    try std.testing.checkAllAllocationFailures(deterministic.allocator(), testSelection, .{});
}

fn testSelection(allocator: Allocator) (@import("PackFile.zig").WriteError || @import("PackFile.zig").ReadError || error{ TestUnexpectedResult, TestExpectedEqual })!void {
    const helper = lir.ProcIdentity.forTest(3);
    const artifacts = [_]ProcArtifact.Artifact{
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(1) },
            .code = "\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        },
        .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(2) },
            .code = "\xe8\x00\x00\x00\x00\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }},
            .relocations = &.{},
            .data = &.{},
            .context_contract = .{
                .target = .x64musl,
                .cpu_level = .default,
                .hot_reload = false,
                .default_platform_runtime = false,
            },
            .context_dependencies = .{},
        },
        .{
            .kind = .{ .proc = helper },
            .code = "\x48\xb8" ++ "\x00\x00\x00\x00\x00\x00\x00\x00" ++ "\xe8\x00\x00\x00\x00\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{.{ .site = 10, .form = .call, .target = 2, .delta = 0 }},
            .relocations = &.{},
            .data = &.{},
            .context_contract = .{
                .target = .x64musl,
                .cpu_level = .default,
                .hot_reload = false,
                .default_platform_runtime = false,
            },
            .context_dependencies = .{},
        },
    };
    const source = ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
    var closure = try init(allocator, &source);
    defer closure.deinit();
    const placed = try closure.of(1);
    try std.testing.expect(closure.complete);
    try std.testing.expectEqualSlices(u32, &.{ 1, 2 }, placed);
    const Accept = struct {
        fn node(_: *anyopaque, artifact: ProcArtifact.Artifact) bool {
            return switch (artifact.kind) {
                .proc => |identity| !std.meta.eql(identity, lir.ProcIdentity.forTest(1)),
                .boxy_thunk, .rc_helper, .entrypoint, .message_pool_run, .branch_island => true,
            };
        }
    };
    var context: u8 = 0;
    const valid = try closure.admit(&context, Accept.node);
    defer allocator.free(valid);
    try std.testing.expectEqualSlices(bool, &.{ false, true, true }, valid);
    var selected = try selectMarked(allocator, &source, &.{ false, true, true });
    defer selected.deinit();
    try std.testing.expectEqual(@as(u32, 1), selected.set.artifacts[0].refs[0].target);
    try std.testing.expectEqual(@as(u32, 1), selected.set.artifacts[1].refs[0].target);
    const PackFile = @import("PackFile.zig");
    const bytes = try PackFile.write(allocator, &selected.set, &.{}, PackFile.test_version_hash);
    defer allocator.free(bytes);
    var decoded = try PackFile.read(allocator, bytes, PackFile.test_version_hash);
    defer decoded.deinit();
    try std.testing.expectEqual(selected.set.artifacts.len, decoded.set.artifacts.len);
    for (selected.set.artifacts, decoded.set.artifacts, decoded.set.addresses.?) |original, read_back, address| {
        var expected = original;
        expected.content_address = address;
        try std.testing.expectEqualDeep(expected, read_back);
    }
}

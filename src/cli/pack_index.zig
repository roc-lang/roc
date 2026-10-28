//! Persistent keyed dev offers over immutable dependency components. Payloads,
//! component metadata and selection manifests have independent content identities.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");
const lir = @import("lir");
const PA = backend.dev.CodeExcerpt;
const PF = backend.dev.PackFile;
const Allocator = std.mem.Allocator;

/// Magic and schema version of an independently addressed payload record.
pub const payload_magic = "RPAYLD01";
/// Optional cache records can fail validation or allocation.
pub const Error = PF.ReadError;

/// Identity and integrity digest of immutable record bytes.
pub fn digest(bytes: []const u8) [32]u8 {
    var result: [32]u8 = undefined;
    base.Sha256.hash(bytes, &result, .{});
    return result;
}

fn appendSlice(allocator: Allocator, bytes: *std.ArrayList(u8), value: []const u8) Allocator.Error!void {
    var count: [8]u8 = undefined;
    std.mem.writeInt(u64, &count, value.len, .little);
    try bytes.appendSlice(allocator, &count);
    try bytes.appendSlice(allocator, value);
}

/// Encode one artifact's code and static-data bytes, without metadata.
pub fn payload(allocator: Allocator, artifact: PA.Excerpt) Allocator.Error![]u8 {
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    try bytes.appendSlice(allocator, payload_magic);
    try appendSlice(allocator, &bytes, artifact.code);
    for (artifact.data) |item| try appendSlice(allocator, &bytes, item.bytes);
    return bytes.toOwnedSlice(allocator);
}

/// Bytes are owned by the loader, and remain live until all its consumers end.
pub fn materialize(artifact: *PA.Excerpt, bytes: []const u8) Error!void {
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
    set: *const backend.dev.CodeExcerpt.Set,
    procs: std.AutoHashMap(lir.ProcIdentity, CodeExcerpts),
    thunks: std.AutoHashMap(lir.ProcIdentity, u32),
    helpers: std.StringHashMap(u32),
    /// Artifacts the current query reached; exactly the indices in `order`.
    seen: std.DynamicBitSetUnmanaged,
    order: std.ArrayList(u32) = .empty,
    nodes_visited: usize = 0,
    edges_visited: usize = 0,
    queries: usize = 0,

    /// The artifacts of one procedure identity: a splice reaches the first,
    /// and a pack offers the last.
    const CodeExcerpts = struct { first: u32, last: u32 };

    pub fn init(allocator: Allocator, set: *const backend.dev.CodeExcerpt.Set) Allocator.Error!SpliceClosure {
        var self = SpliceClosure{
            .allocator = allocator,
            .set = set,
            .procs = std.AutoHashMap(lir.ProcIdentity, CodeExcerpts).init(allocator),
            .thunks = std.AutoHashMap(lir.ProcIdentity, u32).init(allocator),
            .helpers = std.StringHashMap(u32).init(allocator),
            .seen = .{},
        };
        errdefer self.deinit();
        self.seen = try std.DynamicBitSetUnmanaged.initEmpty(allocator, set.excerpts.len);
        for (set.excerpts, 0..) |artifact, index| {
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
    pub fn offeredProcExcerpt(self: *const SpliceClosure, identity: lir.ProcIdentity) ?u32 {
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
        self.clear();
        try self.visit(root);
        return self.expand();
    }

    /// Gather the union without visiting a shared dependency once per root.
    pub fn ofEntries(self: *SpliceClosure, entries: []const PF.SpecEntry) Allocator.Error![]const u32 {
        self.clear();
        for (entries) |entry| try self.visit(entry.excerpt);
        return self.expand();
    }

    fn clear(self: *SpliceClosure) void {
        for (self.order.items) |index| self.seen.unset(index);
        self.order.clearRetainingCapacity();
        self.queries += 1;
    }

    fn expand(self: *SpliceClosure) Allocator.Error![]const u32 {
        var cursor: usize = 0;
        while (cursor < self.order.items.len) : (cursor += 1) {
            self.nodes_visited += 1;
            const artifact = self.set.excerpts[self.order.items[cursor]];
            for (artifact.refs) |ref| {
                self.edges_visited += 1;
                try self.visit(ref.target);
            }
            for (artifact.symbolic_refs) |ref| {
                self.edges_visited += 1;
                const target = self.symbolicTarget(ref);
                if (target) |index| try self.visit(index);
            }
        }
        return self.order.items;
    }

    pub fn symbolicTarget(self: *const SpliceClosure, ref: PA.SymbolicReference) ?u32 {
        return switch (ref.binding) {
            .local => |index| index,
            .absent => null,
            .unresolved => switch (ref.target) {
                .proc => |identity| if (self.procs.get(identity)) |artifacts| artifacts.first else null,
                .boxy_thunk => |identity| self.thunks.get(identity),
                .rc_helper => |name| self.helpers.get(name),
            },
        };
    }

    pub fn visit(self: *SpliceClosure, index: u32) Allocator.Error!void {
        if (self.seen.isSet(index)) return;
        try self.order.append(self.allocator, index);
        self.seen.set(index);
    }
};

const collections = @import("collections");

/// SCCs of one frozen native graph. Outgoing component edges always lead to
/// higher component numbers; reverse order therefore names dependencies first.
pub const ComponentGraph = struct {
    arena: base.SingleThreadArena,
    components: collections.scc.Components,
    ordinal: []u32,
    edge_count: usize,

    pub fn init(allocator: Allocator, closure: *const SpliceClosure) Allocator.Error!ComponentGraph {
        var arena = base.SingleThreadArena.init(allocator);
        errdefer arena.deinit();
        const a = arena.allocator();
        const set = closure.set;
        const starts = try a.alloc(u32, set.excerpts.len + 1);
        var targets = std.ArrayList(u32).empty;
        for (set.excerpts, 0..) |artifact, i| {
            starts[i] = std.math.cast(u32, targets.items.len) orelse return error.OutOfMemory;
            for (artifact.refs) |ref| try targets.append(a, ref.target);
            for (artifact.symbolic_refs) |ref| if (closure.symbolicTarget(ref)) |target_index| try targets.append(a, target_index);
        }
        starts[set.excerpts.len] = std.math.cast(u32, targets.items.len) orelse return error.OutOfMemory;
        const reverse = try collections.csr.transpose(a, starts, targets.items);
        const components = try collections.scc.compute(a, starts, targets.items, reverse.starts, reverse.targets);
        const ordinal = try a.alloc(u32, set.excerpts.len);
        for (0..components.count()) |c| {
            const members = components.membersOf(c);
            // A retained component keeps its producer's ordinal mapping even
            // when assembly changes placement order. Fresh components use the
            // producer's deterministic emission order within that component.
            const origin = commonOrigin(set, members);
            if (origin != null) {
                std.mem.sort(u32, members, set, struct {
                    fn less(owner: *const PA.Set, lhs: u32, rhs: u32) bool {
                        return owner.excerpts[lhs].metadata_origin.?.index < owner.excerpts[rhs].metadata_origin.?.index;
                    }
                }.less);
            } else std.mem.sort(u32, members, {}, std.sort.asc(u32));
            for (members, 0..) |index, member| ordinal[index] = @intCast(member);
        }
        return .{ .arena = arena, .components = components, .ordinal = ordinal, .edge_count = targets.items.len };
    }

    pub fn deinit(self: *ComponentGraph) void {
        self.arena.deinit();
    }

    pub fn commonOrigin(set: *const PA.Set, members: []const u32) ?*const PA.MetadataOrigin {
        if (members.len == 0) return null;
        const first = set.excerpts[members[0]].metadata_origin orelse return null;
        if (first.source.excerpts.len != members.len) return null;
        for (members) |index| {
            const origin = set.excerpts[index].metadata_origin orelse return null;
            if (origin.source != first.source) return null;
        }
        return first;
    }

    pub fn target(self: *const ComponentGraph, component: u32, index: u32, hashes: []const [32]u8) Target {
        const other = self.components.component_of[index];
        return .{ .component = if (other == component) null else hashes[other], .index = self.ordinal[index] };
    }
};

/// Internal edges use a component-local ordinal, so cycles do not require a
/// recursive hash. External edges name a previously committed component.
pub const Target = struct { component: ?[32]u8, index: u32 };
/// One persisted dependency endpoint, attached to an exact reference slot.
pub const Edge = struct { excerpt: u32, reference: u32, symbolic: bool, target: Target };
/// Decoded component metadata; all slices belong to pack.set.arena.
pub const Component = struct { pack: PF.Pack, hashes: []const [32]u8, edges: []const Edge };

fn word(a: Allocator, out: *std.ArrayList(u8), value: u32) Allocator.Error!void {
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .little);
    try out.appendSlice(a, &bytes);
}

/// One component's stable metadata, independent of offered specializations.
pub fn encodeComponent(a: Allocator, set: *const PA.Set, graph: *const ComponentGraph, closure: *const SpliceClosure, component: u32, component_hashes: []const [32]u8, payload_hashes: []const [32]u8, names: *const PA.ContentNames) Allocator.Error![]u8 {
    var arena = base.SingleThreadArena.init(a);
    defer arena.deinit();
    const scratch = arena.allocator();
    const members = graph.components.membersOf(component);
    const artifacts = try scratch.alloc(PA.Excerpt, members.len);
    var edges = std.ArrayList(Edge).empty;
    for (members, artifacts, 0..) |old, *artifact, local| {
        artifact.* = set.excerpts[old];
        artifact.code = &.{};
        const refs = try scratch.dupe(PA.Reference, artifact.refs);
        for (refs, 0..) |*ref, r| {
            const target = graph.target(component, ref.target, component_hashes);
            try edges.append(scratch, .{ .excerpt = @intCast(local), .reference = @intCast(r), .symbolic = false, .target = target });
            ref.target = if (target.component == null) target.index else 0;
        }
        artifact.refs = refs;
        const symbolic = try scratch.dupe(PA.SymbolicReference, artifact.symbolic_refs);
        for (symbolic, 0..) |*ref, r| {
            if (closure.symbolicTarget(ref.*)) |target| try edges.append(scratch, .{ .excerpt = @intCast(local), .reference = @intCast(r), .symbolic = true, .target = graph.target(component, target, component_hashes) });
            ref.binding = .absent;
        }
        artifact.symbolic_refs = symbolic;
        const data = try scratch.dupe(PA.DataItem, artifact.data);
        for (data) |*item| item.bytes = &.{};
        artifact.data = data;
    }
    const encoded_pack = try PF.writeWithNames(scratch, &.{ .arena = undefined, .excerpts = artifacts, .source_files = set.source_files }, &.{}, names);
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(a);
    try out.appendSlice(a, "ROFCOMP1");
    try word(a, &out, std.math.cast(u32, members.len) orelse return error.OutOfMemory);
    for (members) |index| try out.appendSlice(a, &payload_hashes[index]);
    try word(a, &out, std.math.cast(u32, edges.items.len) orelse return error.OutOfMemory);
    for (edges.items) |edge| {
        try word(a, &out, edge.excerpt);
        try word(a, &out, edge.reference);
        try out.append(a, @intFromBool(edge.symbolic));
        try out.append(a, @intFromBool(edge.target.component != null));
        if (edge.target.component) |hash| try out.appendSlice(a, &hash);
        try word(a, &out, edge.target.index);
    }
    try out.appendSlice(a, encoded_pack);
    return out.toOwnedSlice(a);
}

const Cursor = struct {
    bytes: []const u8,
    offset: usize = 0,
    fn raw(self: *Cursor, n: usize) Error![]const u8 {
        if (n > self.bytes.len - self.offset) return error.MalformedPack;
        defer self.offset += n;
        return self.bytes[self.offset..][0..n];
    }
    fn word(self: *Cursor) Error!u32 {
        return std.mem.readInt(u32, (try self.raw(4))[0..4], .little);
    }
    fn flag(self: *Cursor) Error!bool {
        return switch ((try self.raw(1))[0]) {
            0 => false,
            1 => true,
            else => error.MalformedPack,
        };
    }
};

/// Validate a component envelope and its complete ordered dependency table.
pub fn decodeComponent(a: Allocator, bytes: []const u8) Error!Component {
    var cursor = Cursor{ .bytes = bytes };
    if (!std.mem.eql(u8, try cursor.raw(8), "ROFCOMP1")) return error.MalformedPack;
    const count = try cursor.word();
    const raw_hashes = try cursor.raw(@as(usize, count) * 32);
    const edge_count = try cursor.word();
    if (edge_count > (bytes.len - cursor.offset) / 14) return error.MalformedPack;
    const edges = try a.alloc(Edge, edge_count);
    defer a.free(edges);
    for (edges) |*edge| {
        edge.* = .{ .excerpt = try cursor.word(), .reference = try cursor.word(), .symbolic = try cursor.flag(), .target = .{ .component = if (try cursor.flag()) (try cursor.raw(32))[0..32].* else null, .index = try cursor.word() } };
    }
    var pack = try PF.read(a, bytes[cursor.offset..]);
    errdefer pack.deinit();
    if (pack.set.excerpts.len != count or pack.specs.len != 0) return error.MalformedPack;
    const scratch = pack.set.arena.allocator();
    const hashes = try scratch.alloc([32]u8, count);
    for (hashes, 0..) |*hash, i| hash.* = raw_hashes[i * 32 ..][0..32].*;
    // Edge order is canonical and every positional reference has one endpoint.
    var edge_cursor: usize = 0;
    for (pack.set.excerpts, 0..) |artifact, i| {
        if (artifact.code.len != 0 or artifact.runtime_numbers.len != 0) return error.MalformedPack;
        for (artifact.data) |item| if (item.bytes.len != 0) return error.MalformedPack;
        for (artifact.symbolic_refs) |ref| if (ref.binding != .absent) return error.MalformedPack;
        for (artifact.refs, 0..) |_, r| {
            if (edge_cursor == edges.len) return error.MalformedPack;
            const edge = edges[edge_cursor];
            if (edge.excerpt != i or edge.reference != r or edge.symbolic) return error.MalformedPack;
            edge_cursor += 1;
        }
        var previous: ?u32 = null;
        while (edge_cursor < edges.len and edges[edge_cursor].excerpt == i) : (edge_cursor += 1) {
            const edge = edges[edge_cursor];
            if (!edge.symbolic or edge.reference >= artifact.symbolic_refs.len) return error.MalformedPack;
            if (previous) |last| if (edge.reference <= last) return error.MalformedPack;
            previous = edge.reference;
        }
    }
    if (edge_cursor != edges.len) return error.MalformedPack;
    for (edges) |edge| if (edge.target.component == null and edge.target.index >= count) return error.MalformedPack;
    const owned_edges = try scratch.dupe(Edge, edges);
    return .{ .pack = pack, .hashes = hashes, .edges = owned_edges };
}

/// Small selection manifest. The dummy artifact carries only the root's exact
/// procedure identity; its spec codec is the ordinary PackFile spec codec.
pub fn encodeOffer(a: Allocator, component: [32]u8, index: u32, identity: lir.ProcIdentity, spec: PF.SpecEntry) Allocator.Error![]u8 {
    var local_spec = spec;
    local_spec.excerpt = 0;
    const encoded_pack = try PF.write(a, &.{ .arena = undefined, .excerpts = &.{.{ .kind = .{ .proc = identity }, .code = &.{}, .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} }} }, &.{local_spec});
    defer a.free(encoded_pack);
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(a);
    try out.appendSlice(a, "ROFFER03");
    try out.appendSlice(a, &component);
    try word(a, &out, index);
    try out.appendSlice(a, encoded_pack);
    return out.toOwnedSlice(a);
}

/// A decoded root selection and its owned specialization interface.
pub const Selection = struct { component: [32]u8, index: u32, pack: PF.Pack };
/// Validate a selection manifest without acquiring its dependencies.
pub fn decodeOffer(a: Allocator, bytes: []const u8) Error!Selection {
    var cursor = Cursor{ .bytes = bytes };
    if (!std.mem.eql(u8, try cursor.raw(8), "ROFFER03")) return error.MalformedPack;
    const component = (try cursor.raw(32))[0..32].*;
    const index = try cursor.word();
    var pack = try PF.read(a, bytes[cursor.offset..]);
    errdefer pack.deinit();
    if (pack.set.excerpts.len != 1 or pack.specs.len != 1 or pack.set.excerpts[0].kind != .proc) return error.MalformedPack;
    return .{ .component = component, .index = index, .pack = pack };
}

/// Structural equality of persisted metadata, with slices compared by value.
/// Code/data payloads never pass through this walk: their owned references prove
/// their identity. Only producer-stamped metadata and dependency identities do.
fn equal(comptime T: type, a: T, b: T) bool {
    return switch (@typeInfo(T)) {
        .pointer => |p| if (p.size == .slice) blk: {
            if (a.len != b.len) break :blk false;
            for (a, b) |x, y| if (!equal(p.child, x, y)) break :blk false;
            break :blk true;
        } else a == b,
        .array => |array| blk: {
            for (a, b) |x, y| if (!equal(array.child, x, y)) break :blk false;
            break :blk true;
        },
        .@"struct" => |info| blk: {
            inline for (info.field_names) |name| if (!equal(@FieldType(T, name), @field(a, name), @field(b, name))) break :blk false;
            break :blk true;
        },
        .@"union" => |info| blk: {
            const tag = std.meta.activeTag(a);
            if (tag != std.meta.activeTag(b)) break :blk false;
            inline for (info.field_names) |name| {
                if (tag == @field(std.meta.Tag(T), name)) break :blk equal(@FieldType(T, name), @field(a, name), @field(b, name));
            }
            unreachable; // the loop above matches the active tag, which is one of the union's fields
        },
        .optional => |opt| if (a) |value| if (b) |other| equal(opt.child, value, other) else false else b == null,
        .void => true,
        .bool, .int, .@"enum" => a == b,
        .type, .noreturn, .float, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("unsupported persisted native metadata type: " ++ @typeName(T)),
    };
}

fn sameLoc(a: base.SourceLoc, files_a: []const [32]u8, b: base.SourceLoc, files_b: []const [32]u8) bool {
    if (a.hasLocation() != b.hasLocation()) return false;
    if (!a.hasLocation()) return true;
    return a.line == b.line and a.column == b.column and std.mem.eql(u8, &files_a[a.file], &files_b[b.file]);
}

fn binding(target: Target, own: [32]u8) PA.Binding {
    return .{ .component = target.component orelse own, .index = target.index };
}

/// Reuse the exact immutable component only when all current metadata and every
/// dependency endpoint still match its producing component. This is one direct
/// edge walk per component, including cycles, never a closure walk per offer.
pub fn reusableComponent(set: *const PA.Set, graph: *const ComponentGraph, closure: *const SpliceClosure, component: u32, component_hashes: []const [32]u8, payload_hashes: []const [32]u8) ?*const base.retained_cache.Record {
    const members = graph.components.membersOf(component);
    const first = ComponentGraph.commonOrigin(set, members) orelse return null;
    const own_hash = first.record.digest;
    for (members, 0..) |index, ordinal| {
        const current = set.excerpts[index];
        const origin = current.metadata_origin.?;
        if (origin.index != ordinal) return null;
        const old = origin.source.excerpts[origin.index];
        const old_payload = old.payload_record orelse return null;
        if (!std.mem.eql(u8, &payload_hashes[index], &old_payload.digest)) return null;
        if (!equal(PA.Kind, current.kind, old.kind) or current.entry != old.entry or !equal(?PA.Frame, current.frame, old.frame) or
            !equal([]const PA.FailureSite, current.failure_sites, old.failure_sites) or
            !equal([]const PA.NamedRelocation, current.relocations, old.relocations) or
            !equal([]const PA.RuntimeNumberLoad, current.runtime_numbers, old.runtime_numbers)) return null;
        if (!sameLoc(current.loc, set.source_files, old.loc, origin.source.source_files) or current.lines.len != old.lines.len) return null;
        for (current.lines, old.lines) |x, y| {
            if (x.offset != y.offset or !equal(base.Region, x.region, y.region) or !sameLoc(x.loc, set.source_files, y.loc, origin.source.source_files)) return null;
        }
        if (current.data.len != old.data.len) return null;
        for (current.data, old.data) |x, y| {
            if (!std.mem.eql(u8, x.name, y.name) or x.alignment != y.alignment or x.symbol_offset != y.symbol_offset or
                x.program_local_name != y.program_local_name or !equal([]const PA.DataRelocation, x.relocations, y.relocations)) return null;
        }
        if (current.refs.len != old.refs.len or current.symbolic_refs.len != old.symbolic_refs.len) return null;
        for (current.refs, old.refs, origin.refs) |x, y, expected| {
            if (x.site != y.site or x.form != y.form or x.delta != y.delta or x.veneer != y.veneer) return null;
            if (!equal(PA.Binding, binding(graph.target(component, x.target, component_hashes), own_hash), expected)) return null;
        }
        for (current.symbolic_refs, old.symbolic_refs, origin.symbolic) |x, y, expected| {
            if (x.site != y.site or x.form != y.form or x.veneer != y.veneer or !equal(@FieldType(PA.SymbolicReference, "target"), x.target, y.target)) return null;
            const actual: ?PA.Binding = if (closure.symbolicTarget(x)) |target| binding(graph.target(component, target, component_hashes), own_hash) else null;
            if (!equal(?PA.Binding, actual, expected)) return null;
        }
    }
    return first.record;
}

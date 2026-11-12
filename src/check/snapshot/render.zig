//! On-demand adapter from an immutable diagnostic capture to TypeWriter.
//! The snapshot remains the authoritative data. Only a capture whose text is
//! requested gets a private rendering graph; no live solver types are consulted.
const std = @import("std");
const base = @import("base");
const types = @import("types");
const snapshot = @import("../snapshot.zig");
const Self = @This();
const Allocator = std.mem.Allocator;
const Var = types.Var;
const Idx = snapshot.SnapshotContentIdx;

gpa: Allocator,
store: types.Store,
writer: types.TypeWriter,
import_mapping: ?types.import_mapping.ImportMapping,
start: u32,
nodes: []Var,
sources: std.AutoHashMapUnmanaged(Var, Var) = .empty,
raw_callables: std.AutoHashMapUnmanaged(Var, Var) = .empty,

pub fn init(snapshots: *const snapshot.Store, capture_idx: u32, context: snapshot.FormatContext) Allocator.Error!*Self {
    const gpa = snapshots.gpa;
    const capture = snapshots.captures.items[capture_idx];
    const self = try gpa.create(Self);
    errdefer gpa.destroy(self);
    self.* = .{
        .gpa = gpa,
        .store = try types.Store.initCapacity(gpa, capture.len, 16),
        .writer = undefined,
        .import_mapping = context.import_mapping,
        .start = capture.start,
        .nodes = undefined,
    };
    errdefer self.store.deinit();
    self.nodes = try gpa.alloc(Var, capture.len);
    errdefer gpa.free(self.nodes);
    errdefer self.sources.deinit(gpa);
    errdefer self.raw_callables.deinit(gpa);
    self.writer = try types.TypeWriter.initFromParts(gpa, &self.store, context.idents, if (self.import_mapping) |*mapping| mapping else null);
    errdefer self.writer.deinit();
    self.writer.mb_default_source = context.default_source;
    self.writer.default_source_ctx = context.default_source_ctx;

    // Allocate identities before their contents, preserving shared variables
    // and recursive edges without recursing on the native stack.
    const infos = snapshots.node_formats.items[capture.start..][0..capture.len];
    for (infos, self.nodes) |info, *node| {
        if (info.source_var) |source| {
            const entry = try self.sources.getOrPut(gpa, source);
            if (!entry.found_existing) entry.value_ptr.* = try self.store.freshFromContent(.err);
            node.* = entry.value_ptr.*;
        } else {
            node.* = try self.store.freshFromContent(.err);
        }
    }
    self.writer.omit_literal_conversion_constraints = capture.omit_literal_conversion_constraints;
    self.writer.omitted_dispatcher = if (capture.omitted_dispatcher) |source| self.sources.get(source) else null;
    self.writer.visible_identifier_count = capture.identifier_count;
    var filled: std.AutoHashMapUnmanaged(Var, void) = .empty;
    defer filled.deinit(gpa);
    for (self.nodes, 0..) |node, offset| {
        const captured = snapshots.getContent(@fromBackingInt(capture.start + @as(u32, @intCast(offset))));
        if (captured == .recursive) continue;
        if ((try filled.getOrPut(gpa, node)).found_existing) continue;
        // These are newly allocated presentation nodes, never solved types.
        try self.store.setVarContent(node, try self.content(snapshots, captured));
    }
    return self;
}

pub fn deinit(self: *Self) void {
    const gpa = self.gpa;
    self.writer.deinit();
    self.store.deinit();
    self.sources.deinit(gpa);
    self.raw_callables.deinit(gpa);
    gpa.free(self.nodes);
    gpa.destroy(self);
}

/// Maps a captured node to its private presentation variable.
pub fn nodeVar(self: *const Self, idx: Idx) Var {
    return self.nodes[@backingInt(idx) - self.start];
}

fn vars(self: *Self, snapshots: *const snapshot.Store, range: anytype) Allocator.Error![]Var {
    const nodes = snapshots.sliceVars(range);
    const result = try self.gpa.alloc(Var, nodes.len);
    for (nodes, result) |node, *var_| var_.* = self.nodeVar(node);
    return result;
}

fn constraints(self: *Self, snapshots: *const snapshot.Store, range: anytype) Allocator.Error!types.StaticDispatchConstraint.SafeList.Range {
    const captured = snapshots.sliceStaticDispatchConstraints(range);
    const result = try self.gpa.alloc(types.StaticDispatchConstraint, captured.len);
    defer self.gpa.free(result);
    for (captured, result) |constraint, *out| {
        // Raw callable identities, unlike their resolved types, distinguish
        // independent requirements. Redirects preserve both facts.
        const entry = try self.raw_callables.getOrPut(self.gpa, constraint.source_fn_var);
        if (!entry.found_existing) entry.value_ptr.* = try self.store.freshRedirect(self.nodeVar(constraint.fn_content));
        out.* = .{ .fn_name = constraint.fn_name, .fn_var = entry.value_ptr.*, .origin = constraint.origin };
    }
    return self.store.appendStaticDispatchConstraints(result);
}

fn content(self: *Self, snapshots: *const snapshot.Store, captured: snapshot.SnapshotContent) Allocator.Error!types.Content {
    return switch (captured) {
        .err => .err,
        .recursive => unreachable, // identities were allocated before contents
        .flex => |flex| .{ .flex = .{ .name = flex.name, .constraints = try self.constraints(snapshots, flex.constraints), .literal = flex.literal } },
        .rigid => |rigid| .{ .rigid = .{ .name = rigid.name, .constraints = try self.constraints(snapshots, rigid.constraints) } },
        .alias => |alias| blk: {
            const args = try self.vars(snapshots, alias.vars);
            defer self.gpa.free(args);
            break :blk try self.store.mkAlias(alias.ident, self.nodeVar(alias.backing), args, alias.origin_module);
        },
        .structure => |flat| switch (flat) {
            .empty_record => .{ .structure = .empty_record },
            .empty_tag_union => .{ .structure = .empty_tag_union },
            .box => base.invariant("legacy box content reached snapshot rendering", .{}),
            .tuple => |tuple| blk: {
                const elems = try self.vars(snapshots, tuple.elems);
                defer self.gpa.free(elems);
                break :blk .{ .structure = .{ .tuple = .{ .elems = try self.store.appendVars(elems) } } };
            },
            .nominal_type => |nominal| blk: {
                const args = try self.vars(snapshots, nominal.vars);
                defer self.gpa.free(args);
                // Snapshot nominal spans start with diagnostic backing data.
                // TypeWriter only displays the nominal's name and arguments.
                break :blk try self.store.mkNominal(nominal.ident, args[1..], nominal.origin_module, false);
            },
            inline .fn_pure, .fn_effectful, .fn_unbound => |func, kind| blk: {
                const args = try self.vars(snapshots, func.args);
                defer self.gpa.free(args);
                break :blk .{ .structure = @unionInit(types.FlatType, @tagName(kind), .{
                    .args = try self.store.appendVars(args),
                    .ret = self.nodeVar(func.ret),
                }) };
            },
            .record => |record| blk: {
                const fields = snapshots.sliceRecordFields(record.fields);
                const result = try self.gpa.alloc(types.RecordField, fields.len);
                defer self.gpa.free(result);
                for (result, 0..) |*out, i| {
                    const field = fields.get(i);
                    const value = self.nodeVar(field.content);
                    out.* = .{ .name = field.name, .presence = switch (field.presence) {
                        .required => .required(value),
                        .unknown => .unknown(try self.store.fresh(), value),
                        .optional => .unknown(try self.store.freshFromContent(.{ .field_presence = .optional }), value),
                        .defaulted => |id| .unknown(try self.store.freshFromContent(.{ .field_presence = .{ .defaulted = id } }), value),
                    } };
                }
                break :blk .{ .structure = .{ .record = .{
                    .fields = try self.store.appendRecordFields(result),
                    .ext = self.nodeVar(record.ext),
                } } };
            },
            .tag_union => |union_| blk: {
                const tags = snapshots.sliceTags(union_.tags);
                const result = try self.gpa.alloc(types.Tag, tags.len);
                defer self.gpa.free(result);
                for (result, 0..) |*out, i| out.* = try self.tag(snapshots, tags.get(i));
                break :blk .{ .structure = .{ .tag_union = .{
                    .tags = try self.store.appendTags(result),
                    .ext = self.nodeVar(union_.ext),
                } } };
            },
        },
    };
}

fn tag(self: *Self, snapshots: *const snapshot.Store, captured: snapshot.SnapshotTag) Allocator.Error!types.Tag {
    const args = try self.vars(snapshots, captured.args);
    defer self.gpa.free(args);
    return .{ .name = captured.name, .args = try self.store.appendVars(args) };
}

/// Renders a tag using its capture's naming scope and returns owned text.
pub fn formatTag(self: *Self, snapshots: *const snapshot.Store, captured: snapshot.SnapshotTag, root: Idx) Allocator.Error![]const u8 {
    const native = try self.tag(snapshots, captured);
    const text = self.writer.writeTagGet(native, self.nodeVar(root)) catch return error.OutOfMemory;
    return self.gpa.dupe(u8, text);
}

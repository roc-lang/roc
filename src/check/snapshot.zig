//! Capture the state of an type at a point in time for the purpose of error reporting.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const tracy = @import("tracy");
const types = @import("types");

const Allocator = std.mem.Allocator;
const TypesStore = types.Store;
const Ident = base.Ident;

/// Index enum for SnapshotContentList
pub const SnapshotContentIdx = SnapshotContentList.Idx;

const SnapshotContentList = collections.SafeList(SnapshotContent);
const SnapshotContentIdxSafeList = collections.SafeList(SnapshotContentIdx);
const SnapshotStaticDispatchConstraintSafeList = collections.SafeList(SnapshotStaticDispatchConstraint);

/// A safe list of record fields
pub const SnapshotRecordFieldSafeList = collections.SafeMultiList(SnapshotRecordField);

/// A safe list of tags
pub const SnapshotTagSafeList = collections.SafeMultiList(SnapshotTag);

/// The content of a type snapshot, mirroring types.Content for error reporting.
pub const SnapshotContent = union(enum) {
    flex: SnapshotFlex,
    rigid: SnapshotRigid,
    alias: SnapshotAlias,
    structure: SnapshotFlatType,
    /// A recursive type reference. Stores the name of the type variable if available.
    recursive: ?Ident.Idx,
    err,
};

/// A snapshotted flex (unbound) type variable with optional name and constraints.
pub const SnapshotFlex = struct {
    name: ?Ident.Idx,
    var_: Var,
    constraints: SnapshotStaticDispatchConstraintSafeList.Range,
};

/// A snapshotted rigid (bound) type variable with name and constraints.
pub const SnapshotRigid = struct {
    name: Ident.Idx,
    constraints: SnapshotStaticDispatchConstraintSafeList.Range,
};

/// A snapshotted type alias with its backing type and type variables.
pub const SnapshotAlias = struct {
    ident: types.TypeIdent,
    origin_module: base.ModuleIdentity.Idx,
    backing: SnapshotContentIdx,
    vars: SnapshotContentIdxSafeList.Range,
};

/// A snapshotted flat type structure (non-variable types like records, functions, etc).
pub const SnapshotFlatType = union(enum) {
    box: SnapshotContentIdx,
    tuple: SnapshotTuple,
    nominal_type: SnapshotNominalType,
    fn_pure: SnapshotFunc,
    fn_effectful: SnapshotFunc,
    fn_unbound: SnapshotFunc,
    record: SnapshotRecord,
    empty_record,
    tag_union: SnapshotTagUnion,
    empty_tag_union,
};

/// A snapshotted tuple type with its element types.
pub const SnapshotTuple = struct {
    elems: SnapshotContentIdxSafeList.Range,
};

/// A snapshotted nominal (named) type with its type parameters and origin module.
pub const SnapshotNominalType = struct {
    ident: types.TypeIdent,
    vars: SnapshotContentIdxSafeList.Range,
    origin_module: base.ModuleIdentity.Idx,
    /// Whether the Builtin module declares the type, which decides the name
    /// a builtin declaration published through an alias is shown under.
    builtin_origin: bool,
};

/// A snapshotted function type with argument types and return type.
pub const SnapshotFunc = struct {
    args: SnapshotContentIdxSafeList.Range,
    ret: SnapshotContentIdx,
};

/// A snapshotted record type with fields and extension variable.
pub const SnapshotRecord = struct {
    fields: SnapshotRecordFieldSafeList.Range,
    ext: SnapshotContentIdx,
};

/// The resolved kind of a snapshotted record field. The field's value type is
/// stored separately in `SnapshotRecordField.content`.
pub const SnapshotFieldPresence = union(enum) {
    required,
    optional,
    defaulted: types.DefaultId,
    unknown,
};

/// A single field in a snapshotted record type.
pub const SnapshotRecordField = struct {
    name: Ident.Idx,
    content: SnapshotContentIdx,
    presence: SnapshotFieldPresence,
};

/// A snapshotted tag union type with its tags and extension variable.
pub const SnapshotTagUnion = struct {
    tags: SnapshotTagSafeList.Range,
    ext: SnapshotContentIdx,
};

/// Append one row part's entry to the labels gathered into `list` from
/// `base_idx`, where `outer_end` ends the outer parts' labels. A label an outer
/// part already holds names the same field or tag (design.md "Row Union
/// Normalization"), and the row means what it would mean with only the
/// innermost occurrence, so that occurrence replaces the outer one.
pub fn appendRowLabel(comptime T: type, list: *collections.SafeMultiList(T), gpa: std.mem.Allocator, base_idx: u32, outer_end: u32, entry: T) std.mem.Allocator.Error!void {
    const names = list.field(.name);
    for (names[base_idx..outer_end], base_idx..) |name, index| {
        if (name.eql(entry.name)) {
            list.set(@fromBackingInt(@intCast(index)), entry);
            return;
        }
    }
    _ = try list.append(gpa, entry);
}

/// A single tag in a snapshotted tag union with its name and argument types.
pub const SnapshotTag = struct {
    name: Ident.Idx,
    args: SnapshotContentIdxSafeList.Range,
    /// Pre-formatted string representation of this tag (e.g., "TagName(a, b)")
    formatted: []const u8,
    render_root: ?SnapshotContentIdx = null,
};

/// A snapshotted static dispatch constraint for method resolution.
pub const SnapshotStaticDispatchConstraint = struct {
    fn_name: Ident.Idx,
    fn_content: SnapshotContentIdx,
    source_fn_var: Var,
    origin: @FieldType(types.StaticDispatchConstraint, "origin"),
};

const Var = types.Var;

/// Self-contained snapshot store with frozen content. Source variables retained
/// as identity keys are never read from the live type store during rendering.
///
/// Whenever a type error occurs, we update the `Var` in the type store to
/// have `.err` content. This is necessary to continue type-checking but
/// looses essential error information. So before doing this, we create a fully
/// resolved snapshot of the type that we can use in reporting
///
/// Entry point is `snapshotVarForError`
const TypeWriter = types.TypeWriter;
const RenderGraph = @import("snapshot/render.zig");

/// Frozen source identity and polarity for a snapshot node. A recursion marker
/// retains the same identity as the node it refers back to.
pub const NodeFormat = struct {
    source_var: ?Var,
    polarity: types.Polarity,
    capture: u32,
};

/// One immutable capture, independent of every later solver mutation.
pub const Capture = struct {
    start: u32,
    len: u32,
    omit_literal_conversion_constraints: bool,
    omitted_dispatcher: ?Var,
    identifier_count: u32,
};

/// The rendering context contains no solver types. Names and default source
/// expressions belong to the module, including when diagnostics are restored.
pub const FormatContext = struct {
    idents: *const Ident.Store,
    /// Borrow the immutable table, not the address of its movable owner.
    import_mapping: ?types.import_mapping.ImportMapping,
    default_source: @FieldType(TypeWriter, "mb_default_source"),
    default_source_ctx: *const anyopaque,
    display_name: @FieldType(TypeWriter, "mb_display_name"),

    /// The context `writer` renders with.
    pub fn of(writer: *const TypeWriter) FormatContext {
        return .{
            .idents = writer.idents,
            .import_mapping = if (writer.import_mapping) |mapping| mapping.* else null,
            .default_source = writer.mb_default_source,
            .default_source_ctx = writer.default_source_ctx,
            .display_name = writer.mb_display_name,
        };
    }
};

/// A node's identity and presentation context at capture time.
const SnapshotFill = struct {
    resolved_var: types.Var,
    /// The polarity of the position this var was requested from: `.pos` at
    /// the snapshot root, flipped through function argument positions,
    /// preserved everywhere else. Deferred rendering starts its polarity
    /// walk here, so error messages hide
    /// implicit output-position openness exactly where the full type's
    /// rendering would.
    polarity: types.Polarity,
};

const SnapshotIdentityResult = enum { flex, rigid };

const SnapshotFuncKind = enum { pure, effectful, unbound };

/// One suspended step of the snapshot walk. A frame is pushed only after its
/// var is on the visit path, and it takes the var back off that path when it
/// records its content, so the recursion markers the snapshot embeds are
/// exactly the ones the recursive walk produced.
///
/// Source runs are held as slices into the type store, which snapshotting
/// never writes to, so a run stays valid across the children that suspend the
/// frame holding it. Collected results live in the store's own scratch stacks
/// at a base captured per frame.
const SnapshotFrame = union(enum) {
    identity: IdentityFrame,
    alias: AliasFrame,
    tuple: TupleFrame,
    nominal: NominalFrame,
    func: FuncFrame,
    record: RecordFrame,
    tag_union: TagUnionFrame,
};

/// A flex or rigid var, snapshotting its static-dispatch constraint list one
/// constraint at a time.
const IdentityFrame = struct {
    fill: SnapshotFill,
    result: SnapshotIdentityResult,
    name: ?Ident.Idx,
    constraints: []const types.StaticDispatchConstraint,
    idx: u32 = 0,
    scratch_top: u32,
    stage: enum { head, await_fn } = .head,
};

const AliasFrame = struct {
    fill: SnapshotFill,
    ident: types.TypeIdent,
    origin_module: base.ModuleIdentity.Idx,
    backing: types.Var,
    args: []const types.Var,
    idx: u32 = 0,
    backing_content: SnapshotContentIdx = undefined,
    scratch_top: u32 = 0,
    stage: enum { backing, await_backing, args, await_arg } = .backing,
};

const TupleFrame = struct {
    fill: SnapshotFill,
    elems: []const types.Var,
    idx: u32 = 0,
    scratch_top: u32,
    stage: enum { elems, await_elem } = .elems,
};

const NominalFrame = struct {
    fill: SnapshotFill,
    ident: types.TypeIdent,
    origin_module: base.ModuleIdentity.Idx,
    builtin_origin: bool,
    args: []const types.Var,
    idx: u32 = 0,
    backing: types.Var = undefined,
    scratch_top: u32,
    stage: enum { await_backing, args, await_arg } = .args,
};

const FuncFrame = struct {
    fill: SnapshotFill,
    kind: SnapshotFuncKind,
    args: []const types.Var,
    ret: types.Var,
    idx: u32 = 0,
    scratch_top: u32,
    args_range: SnapshotContentIdxSafeList.Range = undefined,
    stage: enum { args, await_arg, await_ret } = .args,
};

const RecordFrame = struct {
    fill: SnapshotFill,
    source_fields: types.RecordField.SafeMultiList.Range,
    ext: types.Var,
    idx: u32 = 0,
    scratch_top: u32,
    fields_range: SnapshotRecordFieldSafeList.Range = undefined,
    stage: enum { fields, await_field, await_ext } = .fields,
};

const TagUnionFrame = struct {
    fill: SnapshotFill,
    source_tags: types.Tag.SafeMultiList.Range,
    ext: types.Var,
    tag_idx: u32 = 0,
    arg_idx: u32 = 0,
    /// Base of the current tag's payload results in `scratch_content`.
    content_scratch_top: u32 = 0,
    /// Base of this frame's collected tags in `scratch_tags`.
    tags_scratch_top: u32,
    tags_range: SnapshotTagSafeList.Range = undefined,
    stage: enum { tag_head, tag_args, await_tag_arg, await_ext } = .tag_head,
};

/// Stores snapshots of types captured before unification errors overwrite them with `.err`.
/// This allows error messages to display the original conflicting types rather than the
/// error state. Rendering is demand-driven; captured identities preserve sharing
/// and cycles without retaining any live solver descriptors.
pub const Store = struct {
    const Self = @This();

    gpa: Allocator,

    // Content storage
    contents: SnapshotContentList,

    /// Vars on the current visit path, so a re-entry becomes a recursion
    /// marker instead of an unbounded descent. Entries are unique: a var
    /// already on the path is never added again.
    seen_vars: std.AutoHashMapUnmanaged(Var, void),
    /// A completed node is shared throughout this capture. Back edges in its
    /// structural view are recursion markers with preserved graph identity.
    captured_nodes: std.AutoHashMapUnmanaged(CaptureKey, SnapshotContentIdx) = .empty,

    /// Suspended steps of the snapshot walk, innermost last. The walk descends
    /// on this heap stack rather than the native one, so snapshot depth is
    /// bounded only by available memory.
    frames: std.ArrayList(SnapshotFrame),
    /// Finished child results, consumed by the frame that requested them.
    pending_values: base.Scratch(SnapshotContentIdx),

    /// Storage for compound type parts
    content_indexes: SnapshotContentIdxSafeList,
    record_fields: SnapshotRecordFieldSafeList,
    tags: SnapshotTagSafeList,
    static_dispatch_constraints: SnapshotStaticDispatchConstraintSafeList,

    // Scratch
    scratch_content: base.Scratch(SnapshotContentIdx),
    scratch_tags: base.Scratch(SnapshotTag),
    scratch_record_fields: base.Scratch(SnapshotRecordField),
    scratch_static_dispatch_constraints: base.Scratch(SnapshotStaticDispatchConstraint),

    /// Formatted type strings, indexed by SnapshotContentIdx
    formatted_strings: std.AutoHashMapUnmanaged(SnapshotContentIdx, []const u8),
    formatted_log: std.ArrayList(SnapshotContentIdx) = .empty,
    node_formats: std.ArrayList(NodeFormat) = .empty,
    captures: std.ArrayList(Capture) = .empty,
    render_graphs: std.ArrayList(?*RenderGraph) = .empty,
    active_capture: u32 = 0,
    format_context: ?FormatContext = null,
    test_capture_steps: if (@import("builtin").is_test) usize else void = if (@import("builtin").is_test) 0 else {},
    test_format_work: if (@import("builtin").is_test) usize else void = if (@import("builtin").is_test) 0 else {},

    const CaptureKey = struct { var_: Var, polarity: types.Polarity };

    pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .gpa = gpa,
            .contents = try SnapshotContentList.initCapacity(gpa, capacity),
            .seen_vars = .empty,
            .frames = .empty,
            .pending_values = try base.Scratch(SnapshotContentIdx).init(gpa),
            .content_indexes = try SnapshotContentIdxSafeList.initCapacity(gpa, capacity),
            .record_fields = try SnapshotRecordFieldSafeList.initCapacity(gpa, 256),
            .tags = try SnapshotTagSafeList.initCapacity(gpa, 256),
            .static_dispatch_constraints = try SnapshotStaticDispatchConstraintSafeList.initCapacity(gpa, 64),
            .scratch_content = try base.Scratch(SnapshotContentIdx).init(gpa),
            .scratch_tags = try base.Scratch(SnapshotTag).init(gpa),
            .scratch_record_fields = try base.Scratch(SnapshotRecordField).init(gpa),
            .scratch_static_dispatch_constraints = try base.Scratch(SnapshotStaticDispatchConstraint).init(gpa),
            .formatted_strings = blk: {
                var map = std.AutoHashMapUnmanaged(SnapshotContentIdx, []const u8){};
                try map.ensureTotalCapacity(gpa, 32);
                break :blk map;
            },
        };
    }

    pub fn deinit(self: *Self) void {
        // Free all stored formatted strings
        var strings = self.formatted_strings.valueIterator();
        while (strings.next()) |text| self.gpa.free(text.*);
        self.formatted_strings.deinit(self.gpa);
        self.formatted_log.deinit(self.gpa);
        self.node_formats.deinit(self.gpa);
        for (self.render_graphs.items) |graph| if (graph) |owned| owned.deinit();
        self.render_graphs.deinit(self.gpa);
        self.captures.deinit(self.gpa);

        // Free all formatted tag strings
        const tags_len = self.tags.len();
        if (tags_len > 0) {
            const all_tags_range = SnapshotTagSafeList.Range{ .start = .first, .count = tags_len };
            const tags_slice = self.tags.sliceRange(all_tags_range);
            for (tags_slice.items(.formatted)) |formatted| {
                self.gpa.free(formatted);
            }
        }

        self.contents.deinit(self.gpa);
        self.seen_vars.deinit(self.gpa);
        self.captured_nodes.deinit(self.gpa);
        self.frames.deinit(self.gpa);
        self.pending_values.deinit();
        self.content_indexes.deinit(self.gpa);
        self.record_fields.deinit(self.gpa);
        self.tags.deinit(self.gpa);
        self.static_dispatch_constraints.deinit(self.gpa);
        self.scratch_content.deinit();
        self.scratch_tags.deinit();
        self.scratch_record_fields.deinit();
        self.scratch_static_dispatch_constraints.deinit();
    }

    /// Returns null on allocation failure for diagnostic clients that allow a placeholder.
    /// Clients which propagate allocation failure use `formatType`.
    pub fn getFormattedString(self: *const Self, idx: SnapshotContentIdx) ?[]const u8 {
        return @constCast(self).formatType(idx) catch null;
    }

    pub fn setFormatContext(self: *Self, writer: *const TypeWriter) void {
        self.format_context = FormatContext.of(writer);
    }

    fn renderGraph(self: *Self, capture: u32) Allocator.Error!*RenderGraph {
        const slot = &self.render_graphs.items[capture];
        if (slot.* == null) {
            slot.* = try RenderGraph.init(self, capture, self.format_context.?);
            if (@import("builtin").is_test) self.test_format_work += self.captures.items[capture].len;
        }
        return slot.*.?;
    }

    /// Render only a requested node, from its immutable capture.
    pub fn formatType(self: *Self, idx: SnapshotContentIdx) Allocator.Error![]const u8 {
        if (self.formatted_strings.get(idx)) |text| return text;
        const info = self.node_formats.items[@backingInt(idx)];
        const graph = try self.renderGraph(info.capture);
        const before = if (@import("builtin").is_test) graph.writer.test_work + graph.writer.test_constraint_comparisons else {};
        graph.writer.writeIntoAtPolarity(&graph.writer.buf, graph.nodeVar(idx), .wrap, info.polarity) catch return error.OutOfMemory;
        if (@import("builtin").is_test) self.test_format_work += graph.writer.test_work + graph.writer.test_constraint_comparisons - before;
        const text = try self.gpa.dupe(u8, graph.writer.get());
        errdefer self.gpa.free(text);
        try self.formatted_log.ensureUnusedCapacity(self.gpa, 1);
        try self.formatted_strings.put(self.gpa, idx, text);
        self.formatted_log.appendAssumeCapacity(idx);
        return text;
    }

    pub fn formatTag(self: *Self, idx: SnapshotTagSafeList.Idx) Allocator.Error![]const u8 {
        var tag = self.tags.get(idx);
        if (tag.formatted.len != 0) return tag.formatted;
        const root = tag.render_root.?;
        const graph = try self.renderGraph(self.node_formats.items[@backingInt(root)].capture);
        const before = if (@import("builtin").is_test) graph.writer.test_work + graph.writer.test_constraint_comparisons else {};
        tag.formatted = try graph.formatTag(self, tag, root);
        if (@import("builtin").is_test) self.test_format_work += graph.writer.test_work + graph.writer.test_constraint_comparisons - before;
        self.tags.set(idx, tag);
        return tag.formatted;
    }

    /// Lengths of every persistent append-only sequence in this store,
    /// captured by `mark` so a speculative probe can `truncateToMark` away
    /// everything snapshotted since (the scratch fields are transient within a
    /// single `snapshotVarForError` call and need no capture).
    pub const Mark = struct {
        contents_len: usize,
        content_indexes_len: usize,
        record_fields_len: usize,
        tags_len: usize,
        static_dispatch_constraints_len: usize,
        formatted_log_len: usize,
        captures_len: usize,
    };

    /// Capture the current lengths for a later `truncateToMark`.
    pub fn mark(self: *const Self) Mark {
        return .{
            .contents_len = @intCast(self.contents.len()),
            .content_indexes_len = @intCast(self.content_indexes.len()),
            .record_fields_len = self.record_fields.len(),
            .tags_len = self.tags.len(),
            .static_dispatch_constraints_len = @intCast(self.static_dispatch_constraints.len()),
            .formatted_log_len = self.formatted_log.items.len,
            .captures_len = self.captures.items.len,
        };
    }

    /// Discard every snapshot recorded after `mark` was captured—the
    /// rollback of a speculative probe that recorded against this store.
    /// Discard rendered entries created after the mark even when they describe
    /// an older capture. Pure adapter graphs of retained captures remain valid.
    pub fn truncateToMark(self: *Self, m: Mark) void {
        const tags_len = self.tags.len();
        if (tags_len > m.tags_len) {
            const removed_range = SnapshotTagSafeList.Range{
                .start = @fromBackingInt(@intCast(m.tags_len)),
                .count = @intCast(tags_len - m.tags_len),
            };
            for (self.tags.sliceRange(removed_range).items(.formatted)) |formatted| {
                self.gpa.free(formatted);
            }
        }

        for (self.formatted_log.items[m.formatted_log_len..]) |idx| {
            self.gpa.free(self.formatted_strings.fetchRemove(idx).?.value);
        }
        self.formatted_log.shrinkRetainingCapacity(m.formatted_log_len);
        for (self.render_graphs.items[m.captures_len..]) |graph| if (graph) |owned| owned.deinit();
        self.render_graphs.shrinkRetainingCapacity(m.captures_len);
        self.captures.shrinkRetainingCapacity(m.captures_len);
        self.node_formats.shrinkRetainingCapacity(m.contents_len);

        self.contents.items.shrinkRetainingCapacity(m.contents_len);
        self.content_indexes.items.shrinkRetainingCapacity(m.content_indexes_len);
        self.record_fields.items.shrinkRetainingCapacity(m.record_fields_len);
        self.tags.items.shrinkRetainingCapacity(m.tags_len);
        self.static_dispatch_constraints.items.shrinkRetainingCapacity(m.static_dispatch_constraints_len);
    }

    /// Freeze a type graph before poisoning. No nested type or tag is formatted
    /// during capture; reports request their text from the immutable snapshot.
    /// ONLY use this in error paths.
    ///
    /// The graph walk runs on an explicit heap worklist, so snapshot depth is
    /// bounded only by available memory, never by the native stack.
    pub fn snapshotVarForError(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, var_: types.Var) std.mem.Allocator.Error!SnapshotContentIdx {
        const trace = tracy.trace(@src());
        defer trace.end();

        const frames_base = self.frames.items.len;
        const values_base = self.pending_values.top();
        self.captured_nodes.clearRetainingCapacity();
        const capture_mark = self.mark();
        errdefer self.truncateToMark(capture_mark);
        self.setFormatContext(type_writer);
        const previous_capture = self.active_capture;
        defer self.active_capture = previous_capture;
        self.active_capture = @intCast(self.captures.items.len);
        try self.captures.ensureUnusedCapacity(self.gpa, 1);
        try self.render_graphs.ensureUnusedCapacity(self.gpa, 1);
        self.captures.appendAssumeCapacity(.{
            .start = std.math.cast(u32, self.contents.len()) orelse return error.OutOfMemory,
            .len = 0,
            .omit_literal_conversion_constraints = type_writer.omit_literal_conversion_constraints,
            .omitted_dispatcher = type_writer.omitted_dispatcher,
            .identifier_count = type_writer.idents.interner.entry_count,
        });
        self.render_graphs.appendAssumeCapacity(null);
        // A completed walk drains the worklist and leaves `seen_vars` empty.
        // An allocation failure mid-walk can leave entries behind on buffers
        // this store keeps for the next snapshot, so unwind them here and
        // preserve that invariant on both exit paths.
        errdefer {
            self.frames.items.len = frames_base;
            self.pending_values.clearFrom(values_base);
            self.seen_vars.clearRetainingCapacity();
        }

        if (!try self.requestVar(store, var_, .pos)) {
            while (self.frames.items.len > frames_base) {
                const top = &self.frames.items[self.frames.items.len - 1];
                // A step either suspends after requesting exactly one child
                // (having already written its own resume state), or finishes
                // without requesting anything—so popping on finish always
                // removes the frame the step ran for.
                const finished = switch (top.*) {
                    .identity => |*frame| try self.stepIdentity(store, frame),
                    .alias => |*frame| try self.stepAlias(store, frame),
                    .tuple => |*frame| try self.stepTuple(store, frame),
                    .nominal => |*frame| try self.stepNominal(store, frame),
                    .func => |*frame| try self.stepFunc(store, frame),
                    .record => |*frame| try self.stepRecord(store, frame),
                    .tag_union => |*frame| try self.stepTagUnion(store, frame),
                };
                if (finished) {
                    self.frames.items.len -= 1;
                }
            }
        }

        std.debug.assert(self.seen_vars.count() == 0);
        std.debug.assert(self.pending_values.top() == values_base + 1);
        self.captures.items[self.active_capture].len = std.math.cast(u32, self.contents.len() - self.captures.items[self.active_capture].start) orelse return error.OutOfMemory;
        return self.pending_values.pop().?;
    }

    /// Snapshot one var's head: hand back a recursion marker for a var already
    /// on the path, and otherwise mark it as being visited and either record
    /// its content immediately (contents with no children) or push the frame
    /// that will record it. Returns true when the result index is already on
    /// the value stack; false when a frame was pushed.
    fn requestVar(self: *Self, store: *const TypesStore, var_: types.Var, polarity: types.Polarity) std.mem.Allocator.Error!bool {
        if (@import("builtin").is_test) self.test_capture_steps += 1;
        const resolved = store.resolveVar(var_);
        if (self.captured_nodes.get(.{ .var_ = resolved.var_, .polarity = polarity })) |idx| {
            try self.pending_values.append(idx);
            return true;
        }

        // If we've already reached this variable on the current path, then
        // return it as a recursive type. Try to extract the name from the
        // content for better error messages.
        if (self.seen_vars.contains(resolved.var_)) {
            const recursive_name: ?Ident.Idx = switch (resolved.desc.content) {
                .flex => |flex| flex.name,
                .rigid => |rigid| rigid.name,
                .alias => |alias| alias.ident.ident_idx,
                .field_presence => null,
                .structure => |flat_type| switch (flat_type) {
                    .nominal_type => |nominal| nominal.ident.ident_idx,
                    // Other structures can appear as backing vars for nominal types.
                    // E.g., List(a) := [Nil, Cons(a, List(a))] has a tag union as backing.
                    // These don't have a direct name, so contextual naming names them.
                    .record, .tuple, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .tag_union, .empty_tag_union => null,
                },
                .err => unreachable, // .err content is recorded and leaves seen_vars at once, so it is never on the path
            };
            const idx = try self.appendContent(.{ .recursive = recursive_name }, resolved.var_, polarity);
            try self.pending_values.append(idx);
            return true;
        }

        // If not, mark it as being visited
        try self.seen_vars.put(self.gpa, resolved.var_, {});

        const fill = SnapshotFill{
            .resolved_var = resolved.var_,
            .polarity = polarity,
        };
        switch (resolved.desc.content) {
            .err => {
                try self.finishFrame(fill, SnapshotContent.err);
                return true;
            },
            .flex => |flex| {
                if (flex.constraints.len() == 0) {
                    try self.finishFrame(fill, SnapshotContent{ .flex = SnapshotFlex{
                        .name = flex.name,
                        .var_ = resolved.var_,
                        .constraints = try self.static_dispatch_constraints.appendSlice(self.gpa, &.{}),
                    } });
                    return true;
                }
                try self.frames.append(self.gpa, .{ .identity = .{
                    .fill = fill,
                    .result = .flex,
                    .name = flex.name,
                    .constraints = store.sliceStaticDispatchConstraints(flex.constraints),
                    .scratch_top = self.scratch_static_dispatch_constraints.top(),
                } });
                return false;
            },
            .rigid => |rigid| {
                if (rigid.constraints.len() == 0) {
                    try self.finishFrame(fill, SnapshotContent{ .rigid = SnapshotRigid{
                        .name = rigid.name,
                        .constraints = try self.static_dispatch_constraints.appendSlice(self.gpa, &.{}),
                    } });
                    return true;
                }
                try self.frames.append(self.gpa, .{ .identity = .{
                    .fill = fill,
                    .result = .rigid,
                    .name = rigid.name,
                    .constraints = store.sliceStaticDispatchConstraints(rigid.constraints),
                    .scratch_top = self.scratch_static_dispatch_constraints.top(),
                } });
                return false;
            },
            .alias => |alias| {
                try self.frames.append(self.gpa, .{ .alias = .{
                    .fill = fill,
                    .ident = alias.ident,
                    .origin_module = alias.origin_module,
                    .backing = store.getAliasBackingVar(alias),
                    .args = store.sliceAliasArgs(alias),
                } });
                return false;
            },
            // Presence facts are represented on their owning record field in
            // snapshots, never as standalone reportable types.
            .field_presence => {
                try self.finishFrame(fill, SnapshotContent.err);
                return true;
            },
            .structure => |flat_type| switch (flat_type) {
                .empty_record => {
                    try self.finishFrame(fill, SnapshotContent{ .structure = SnapshotFlatType.empty_record });
                    return true;
                },
                .empty_tag_union => {
                    try self.finishFrame(fill, SnapshotContent{ .structure = SnapshotFlatType.empty_tag_union });
                    return true;
                },
                .tuple => |tuple| {
                    try self.frames.append(self.gpa, .{ .tuple = .{
                        .fill = fill,
                        .elems = store.sliceVars(tuple.elems),
                        .scratch_top = self.scratch_content.top(),
                    } });
                    return false;
                },
                .nominal_type => |nominal_type| {
                    // vars[0] is the backing, kept for report traversals (e.g. equality
                    // explanations descend into it). The application itself carries no
                    // backing, so snapshot the DECLARATION's backing template—formals
                    // read as rigids there, which the traversals treat optimistically.
                    var frame = NominalFrame{
                        .fill = fill,
                        .ident = nominal_type.ident,
                        .origin_module = nominal_type.origin_module,
                        .builtin_origin = nominal_type.originIsBuiltin(),
                        // A declaration in a function body's trailing
                        // context arguments are never shown.
                        .args = store.nominalDeclaredArgs(nominal_type),
                        .scratch_top = self.scratch_content.top(),
                    };
                    if (store.lookupNominalDecl(nominal_type)) |decl_idx| {
                        const decl = store.getNominalDecl(decl_idx);
                        if (decl.isValid()) {
                            frame.backing = decl.backing;
                            frame.stage = .await_backing;
                            try self.frames.append(self.gpa, .{ .nominal = frame });
                            _ = try self.requestVar(store, decl.backing, polarity);
                            return false;
                        }
                    }
                    try self.scratch_content.append(try self.appendContent(.err, null, polarity));
                    try self.frames.append(self.gpa, .{ .nominal = frame });
                    return false;
                },
                .fn_pure => |func| return try self.pushFunc(store, fill, .pure, func),
                .fn_effectful => |func| return try self.pushFunc(store, fill, .effectful, func),
                .fn_unbound => |func| return try self.pushFunc(store, fill, .unbound, func),
                .record => |record| {
                    try self.frames.append(self.gpa, .{ .record = .{
                        .fill = fill,
                        .source_fields = record.fields,
                        .ext = record.ext,
                        .scratch_top = self.scratch_record_fields.top(),
                    } });
                    return false;
                },
                .tag_union => |tag_union| {
                    try self.frames.append(self.gpa, .{ .tag_union = .{
                        .fill = fill,
                        .source_tags = tag_union.tags,
                        .ext = tag_union.ext,
                        .tags_scratch_top = self.scratch_tags.top(),
                    } });
                    return false;
                },
            },
        }
    }

    fn pushFunc(
        self: *Self,
        store: *const TypesStore,
        fill: SnapshotFill,
        kind: SnapshotFuncKind,
        func: types.Func,
    ) std.mem.Allocator.Error!bool {
        try self.frames.append(self.gpa, .{ .func = .{
            .fill = fill,
            .kind = kind,
            .args = store.sliceVars(func.args),
            .ret = func.ret,
            .scratch_top = self.scratch_content.top(),
        } });
        return false;
    }

    fn appendContent(self: *Self, content: SnapshotContent, source: ?Var, polarity: types.Polarity) Allocator.Error!SnapshotContentIdx {
        try self.node_formats.ensureUnusedCapacity(self.gpa, 1);
        const idx = try self.contents.append(self.gpa, content);
        self.node_formats.appendAssumeCapacity(.{ .source_var = source, .polarity = polarity, .capture = self.active_capture });
        return idx;
    }

    /// Freeze one node's content and take it off the visit path. No rendering
    /// happens while traversing the solver graph.
    fn finishFrame(
        self: *Self,
        fill: SnapshotFill,
        content: SnapshotContent,
    ) std.mem.Allocator.Error!void {
        const snapshot_idx = try self.appendContent(content, fill.resolved_var, fill.polarity);
        if (content == .structure and content.structure == .tag_union) {
            var tags = content.structure.tag_union.tags.iterIndices();
            while (tags.next()) |idx| {
                var tag = self.tags.get(idx);
                tag.render_root = snapshot_idx;
                self.tags.set(idx, tag);
            }
        }
        try self.captured_nodes.put(self.gpa, .{ .var_ = fill.resolved_var, .polarity = fill.polarity }, snapshot_idx);

        _ = self.seen_vars.remove(fill.resolved_var);
        try self.pending_values.append(snapshot_idx);
    }

    fn stepIdentity(self: *Self, store: *const TypesStore, frame: *IdentityFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .head => {
                    if (frame.idx < frame.constraints.len) {
                        frame.stage = .await_fn;
                        // A method signature is its own annotation root, as
                        // `TypeWriter` writes it in a where clause.
                        if (!try self.requestVar(store, frame.constraints[frame.idx].fn_var, .pos)) return false;
                        continue;
                    }
                    const range = try self.static_dispatch_constraints.appendSlice(
                        self.gpa,
                        self.scratch_static_dispatch_constraints.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_static_dispatch_constraints.clearFrom(frame.scratch_top);
                    const content: SnapshotContent = switch (frame.result) {
                        .flex => SnapshotContent{ .flex = SnapshotFlex{
                            .name = frame.name,
                            .var_ = frame.fill.resolved_var,
                            .constraints = range,
                        } },
                        .rigid => SnapshotContent{ .rigid = SnapshotRigid{
                            .name = frame.name.?,
                            .constraints = range,
                        } },
                    };
                    try self.finishFrame(frame.fill, content);
                    return true;
                },
                .await_fn => {
                    try self.scratch_static_dispatch_constraints.append(.{
                        .fn_name = frame.constraints[frame.idx].fn_name,
                        .fn_content = self.pending_values.pop().?,
                        .source_fn_var = frame.constraints[frame.idx].fn_var,
                        .origin = frame.constraints[frame.idx].origin,
                    });
                    frame.idx += 1;
                    frame.stage = .head;
                },
            }
        }
    }

    fn stepAlias(self: *Self, store: *const TypesStore, frame: *AliasFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .backing => {
                    frame.stage = .await_backing;
                    if (!try self.requestVar(store, frame.backing, frame.fill.polarity)) return false;
                    continue;
                },
                .await_backing => {
                    frame.backing_content = self.pending_values.pop().?;
                    // The scratch run holding the alias arguments starts only
                    // after the backing copy is done, matching the layout of
                    // the snapshot alias, whose backing is a separate field.
                    frame.scratch_top = self.scratch_content.top();
                    frame.stage = .args;
                },
                .args => {
                    if (frame.idx < frame.args.len) {
                        frame.stage = .await_arg;
                        if (!try self.requestVar(store, frame.args[frame.idx], frame.fill.polarity)) return false;
                        continue;
                    }
                    const args_range = try self.content_indexes.appendSlice(
                        self.gpa,
                        self.scratch_content.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_content.clearFrom(frame.scratch_top);
                    try self.finishFrame(frame.fill, SnapshotContent{ .alias = SnapshotAlias{
                        .ident = frame.ident,
                        .origin_module = frame.origin_module,
                        .backing = frame.backing_content,
                        .vars = args_range,
                    } });
                    return true;
                },
                .await_arg => {
                    try self.scratch_content.append(self.pending_values.pop().?);
                    frame.idx += 1;
                    frame.stage = .args;
                },
            }
        }
    }

    fn stepTuple(self: *Self, store: *const TypesStore, frame: *TupleFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .elems => {
                    if (frame.idx < frame.elems.len) {
                        frame.stage = .await_elem;
                        if (!try self.requestVar(store, frame.elems[frame.idx], frame.fill.polarity)) return false;
                        continue;
                    }
                    const elems_range = try self.content_indexes.appendSlice(
                        self.gpa,
                        self.scratch_content.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_content.clearFrom(frame.scratch_top);
                    try self.finishFrame(frame.fill, SnapshotContent{ .structure = SnapshotFlatType{
                        .tuple = SnapshotTuple{ .elems = elems_range },
                    } });
                    return true;
                },
                .await_elem => {
                    try self.scratch_content.append(self.pending_values.pop().?);
                    frame.idx += 1;
                    frame.stage = .elems;
                },
            }
        }
    }

    fn stepNominal(self: *Self, store: *const TypesStore, frame: *NominalFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .await_backing => {
                    try self.scratch_content.append(self.pending_values.pop().?);
                    frame.stage = .args;
                },
                .args => {
                    if (frame.idx < frame.args.len) {
                        frame.stage = .await_arg;
                        if (!try self.requestVar(store, frame.args[frame.idx], frame.fill.polarity)) return false;
                        continue;
                    }
                    const args_range = try self.content_indexes.appendSlice(
                        self.gpa,
                        self.scratch_content.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_content.clearFrom(frame.scratch_top);
                    try self.finishFrame(frame.fill, SnapshotContent{ .structure = SnapshotFlatType{
                        .nominal_type = SnapshotNominalType{
                            .ident = frame.ident,
                            .vars = args_range,
                            .origin_module = frame.origin_module,
                            .builtin_origin = frame.builtin_origin,
                        },
                    } });
                    return true;
                },
                .await_arg => {
                    try self.scratch_content.append(self.pending_values.pop().?);
                    frame.idx += 1;
                    frame.stage = .args;
                },
            }
        }
    }

    fn stepFunc(self: *Self, store: *const TypesStore, frame: *FuncFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .args => {
                    if (frame.idx < frame.args.len) {
                        frame.stage = .await_arg;
                        if (!try self.requestVar(store, frame.args[frame.idx], frame.fill.polarity.flip())) return false;
                        continue;
                    }
                    // The argument run is committed before the return type is
                    // walked, so the return type's own runs land after it.
                    frame.args_range = try self.content_indexes.appendSlice(
                        self.gpa,
                        self.scratch_content.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_content.clearFrom(frame.scratch_top);
                    frame.stage = .await_ret;
                    if (!try self.requestVar(store, frame.ret, frame.fill.polarity)) return false;
                    continue;
                },
                .await_arg => {
                    try self.scratch_content.append(self.pending_values.pop().?);
                    frame.idx += 1;
                    frame.stage = .args;
                },
                .await_ret => {
                    const deep_ret = self.pending_values.pop().?;
                    const snapshot_func = SnapshotFunc{ .args = frame.args_range, .ret = deep_ret };
                    const content: SnapshotContent = switch (frame.kind) {
                        .pure => SnapshotContent{ .structure = SnapshotFlatType{ .fn_pure = snapshot_func } },
                        .effectful => SnapshotContent{ .structure = SnapshotFlatType{ .fn_effectful = snapshot_func } },
                        .unbound => SnapshotContent{ .structure = SnapshotFlatType{ .fn_unbound = snapshot_func } },
                    };
                    try self.finishFrame(frame.fill, content);
                    return true;
                },
            }
        }
    }

    /// Read the source field at `idx` within `range`. Indexing through the
    /// run's start only happens when the record has fields; start may be
    /// undefined when count is 0.
    fn sourceRecordField(
        store: *const TypesStore,
        range: types.RecordField.SafeMultiList.Range,
        idx: u32,
    ) types.RecordField {
        return store.record_fields.get(@fromBackingInt(@intCast(@backingInt(range.start) + idx)));
    }

    fn snapshotFieldPresence(store: *const TypesStore, presence: types.RecordField.Presence) SnapshotFieldPresence {
        return switch (presence.decode()) {
            .required => .required,
            .unknown => |unknown| switch (store.resolveVar(unknown.presence).desc.content) {
                .field_presence => |resolved| switch (resolved) {
                    .required => .required,
                    .optional => .optional,
                    .defaulted => |id| .{ .defaulted = id },
                },
                .flex, .rigid, .alias, .structure, .err => .unknown,
            },
        };
    }

    fn stepRecord(self: *Self, store: *const TypesStore, frame: *RecordFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .fields => {
                    if (frame.idx < frame.source_fields.count) {
                        frame.stage = .await_field;
                        const field = sourceRecordField(store, frame.source_fields, frame.idx);
                        if (!try self.requestVar(store, field.presence.typeVar(), frame.fill.polarity)) return false;
                        continue;
                    }
                    // The field run is committed before the extension is
                    // walked, so the extension's own runs land after it.
                    frame.fields_range = try self.record_fields.appendSlice(
                        self.gpa,
                        self.scratch_record_fields.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_record_fields.clearFrom(frame.scratch_top);
                    frame.stage = .await_ext;
                    if (!try self.requestVar(store, frame.ext, frame.fill.polarity)) return false;
                    continue;
                },
                .await_field => {
                    const field = sourceRecordField(store, frame.source_fields, frame.idx);
                    try self.scratch_record_fields.append(.{
                        .name = field.name,
                        .content = self.pending_values.pop().?,
                        .presence = snapshotFieldPresence(store, field.presence),
                    });
                    frame.idx += 1;
                    frame.stage = .fields;
                },
                .await_ext => {
                    const deep_ext = self.pending_values.pop().?;
                    try self.finishFrame(frame.fill, SnapshotContent{ .structure = SnapshotFlatType{
                        .record = SnapshotRecord{ .fields = frame.fields_range, .ext = deep_ext },
                    } });
                    return true;
                },
            }
        }
    }

    fn stepTagUnion(self: *Self, store: *const TypesStore, frame: *TagUnionFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .tag_head => {
                    if (frame.tag_idx == frame.source_tags.count) {
                        const tags_range = try self.tags.appendSlice(
                            self.gpa,
                            self.scratch_tags.sliceFromStart(frame.tags_scratch_top),
                        );
                        self.scratch_tags.clearFrom(frame.tags_scratch_top);
                        frame.tags_range = tags_range;
                        frame.stage = .await_ext;
                        if (!try self.requestVar(store, frame.ext, frame.fill.polarity)) return false;
                        continue;
                    }
                    frame.content_scratch_top = self.scratch_content.top();
                    frame.arg_idx = 0;
                    frame.stage = .tag_args;
                },
                .tag_args => {
                    // Indexing through the run's start only happens when the
                    // tag union has tags; an empty run's start addresses no row.
                    const tag = store.tags.get(@fromBackingInt(@intCast(@backingInt(frame.source_tags.start) + frame.tag_idx)));
                    const tag_args_slice = store.sliceVars(tag.args);
                    if (frame.arg_idx < tag_args_slice.len) {
                        frame.stage = .await_tag_arg;
                        if (!try self.requestVar(store, tag_args_slice[frame.arg_idx], frame.fill.polarity)) return false;
                        continue;
                    }
                    const tag_args_range = try self.content_indexes.appendSlice(
                        self.gpa,
                        self.scratch_content.sliceFromStart(frame.content_scratch_top),
                    );
                    self.scratch_content.clearFrom(frame.content_scratch_top);

                    try self.scratch_tags.append(.{
                        .name = tag.name,
                        .args = tag_args_range,
                        .formatted = &.{},
                    });
                    frame.tag_idx += 1;
                    frame.stage = .tag_head;
                },
                .await_tag_arg => {
                    try self.scratch_content.append(self.pending_values.pop().?);
                    frame.arg_idx += 1;
                    frame.stage = .tag_args;
                },
                .await_ext => {
                    const deep_ext = self.pending_values.pop().?;
                    try self.finishFrame(frame.fill, SnapshotContent{ .structure = SnapshotFlatType{
                        .tag_union = SnapshotTagUnion{ .tags = frame.tags_range, .ext = deep_ext },
                    } });
                    return true;
                },
            }
        }
    }

    pub fn sliceVars(self: *const Self, range: SnapshotContentIdxSafeList.Range) []const SnapshotContentIdx {
        return self.content_indexes.sliceRange(range);
    }

    pub fn sliceRecordFields(self: *const Self, range: SnapshotRecordFieldSafeList.Range) SnapshotRecordFieldSafeList.Slice {
        return self.record_fields.sliceRange(range);
    }

    pub fn sliceStaticDispatchConstraints(self: *const Self, range: SnapshotStaticDispatchConstraintSafeList.Range) SnapshotStaticDispatchConstraintSafeList.Slice {
        return self.static_dispatch_constraints.sliceRange(range);
    }

    pub fn sliceTags(self: *const Self, range: SnapshotTagSafeList.Range) SnapshotTagSafeList.Slice {
        return self.tags.sliceRange(range);
    }

    pub fn getContent(self: *const Self, idx: SnapshotContentIdx) SnapshotContent {
        return self.contents.get(idx).*;
    }

    pub fn getContentUnwrapAlias(self: *const Self, initial_idx: SnapshotContentIdx) SnapshotContent {
        var idx = initial_idx;
        while (true) {
            const content = self.contents.get(idx).*;
            if (std.meta.activeTag(content) != .alias) return content;
            idx = content.alias.backing;
        }
    }

    /// Whether `idx` is a closed record: one whose extension chain terminates in
    /// `empty_record`. A too-narrow record-destructure pattern is always closed,
    /// so this distinguishes it from an open (`..`) pattern.
    pub fn isClosedRecord(self: *const Self, idx: SnapshotContentIdx) bool {
        var cur = idx;
        while (true) {
            const content = self.getContentUnwrapAlias(cur);
            if (std.meta.activeTag(content) != .structure) return false;
            const structure = content.structure;
            const tag = std.meta.activeTag(structure);
            if (tag == .empty_record) return true;
            if (tag != .record) return false;
            cur = structure.record.ext;
        }
    }

    const RecordFieldSnapshot = union(enum) {
        not_a_record,
        empty_record,
        record: SnapshotRecordFieldSafeList.Range,
    };

    pub fn gatherRecordFields(
        self: *const Self,
        idx: SnapshotContentIdx,
        gpa: std.mem.Allocator,
        fields_out: *SnapshotRecordFieldSafeList,
    ) std.mem.Allocator.Error!RecordFieldSnapshot {
        const trace = tracy.trace(@src());
        defer trace.end();

        const unwrapped = self.getContentUnwrapAlias(idx);
        if (std.meta.activeTag(unwrapped) == .structure) {
            switch (unwrapped.structure) {
                .record => |record| {
                    // Gather all fields into fields_out
                    const fields_out_top: u32 = @intCast(fields_out.items.len);
                    try self.gatherRecordFieldsHelp(record, gpa, fields_out);
                    const fields_out_range = fields_out.rangeToEnd(fields_out_top);

                    // Return empty record on base-case
                    if (fields_out_range.count == 0) {
                        return .empty_record;
                    }

                    return RecordFieldSnapshot{ .record = fields_out_range };
                },
                .empty_record => return .empty_record,
                .box,
                .tuple,
                .nominal_type,
                .fn_pure,
                .fn_effectful,
                .fn_unbound,
                .tag_union,
                .empty_tag_union,
                => return .not_a_record,
            }
        }
        return .not_a_record;
    }

    /// Gather all fields from a record, following extension chain.
    /// Returns a Range into fields buffer.
    pub fn gatherRecordFieldsHelp(
        self: *const Store,
        record: SnapshotRecord,
        gpa: std.mem.Allocator,
        fields_out: *SnapshotRecordFieldSafeList,
    ) std.mem.Allocator.Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // Add immediate fields
        const base_idx = fields_out.len();
        const record_fields = self.sliceRecordFields(record.fields);
        for (record_fields.items(.name), record_fields.items(.content), record_fields.items(.presence)) |name, content, presence| {
            _ = try fields_out.append(gpa, .{ .name = name, .content = content, .presence = presence });
        }

        // Follow extension chain
        var ext_idx = record.ext;
        while (true) {
            const content = self.getContent(ext_idx);
            switch (content) {
                .structure => |flat| switch (flat) {
                    .record => |rec| {
                        const ext_fields = self.sliceRecordFields(rec.fields);
                        const outer_end = fields_out.len();
                        for (ext_fields.items(.name), ext_fields.items(.content), ext_fields.items(.presence)) |name, field_content, presence| {
                            try appendRowLabel(SnapshotRecordField, fields_out, gpa, base_idx, outer_end, .{ .name = name, .content = field_content, .presence = presence });
                        }
                        ext_idx = rec.ext;
                    },
                    .empty_record => break,
                    .box,
                    .tuple,
                    .nominal_type,
                    .fn_pure,
                    .fn_effectful,
                    .fn_unbound,
                    .tag_union,
                    .empty_tag_union,
                    => break,
                },
                .alias => |alias| {
                    ext_idx = alias.backing;
                },
                .flex, .rigid, .err, .recursive => break,
            }
        }
    }

    /// Read a tag's display after the diff gatherer requested its formatting.
    pub fn getFormattedTagString(tag: SnapshotTag) []const u8 {
        return tag.formatted;
    }
};

test "snapshot record field presence survives deep copy and gather" {
    const gpa = std.testing.allocator;

    var type_store = try TypesStore.initCapacity(gpa, 16, 8);
    defer type_store.deinit();
    var idents = try Ident.Store.initCapacity(gpa, 4);
    defer idents.deinit(gpa);

    const top_name = try idents.insert(gpa, Ident.for_text("top"));
    const middle_name = try idents.insert(gpa, Ident.for_text("middle"));
    const tail_name = try idents.insert(gpa, Ident.for_text("tail"));
    const field_var = try type_store.freshFromContent(.{ .structure = .empty_record });
    const presence_var = try type_store.fresh();

    const tail_fields = try type_store.appendRecordFields(&.{.{
        .name = tail_name,
        .presence = .unknown(presence_var, field_var),
    }});
    const tail_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = tail_fields,
        .ext = try type_store.fresh(),
    } } });

    const middle_fields = try type_store.appendRecordFields(&.{.{
        .name = middle_name,
        .presence = .unknown(presence_var, field_var),
    }});
    const middle_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = middle_fields,
        .ext = tail_var,
    } } });

    const top_fields = try type_store.appendRecordFields(&.{.{
        .name = top_name,
        .presence = .required(field_var),
    }});
    const top_var = try type_store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = top_fields,
        .ext = middle_var,
    } } });

    var type_writer = try TypeWriter.initFromParts(gpa, &type_store, &idents, null);
    defer type_writer.deinit();
    var snapshots = try Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    const top_snapshot_idx = try snapshots.snapshotVarForError(&type_store, &type_writer, top_var);
    const top_content = snapshots.getContent(top_snapshot_idx);
    if (top_content != .structure or top_content.structure != .record) return error.TestUnexpectedResult;
    const top_snapshot = top_content.structure.record;
    try std.testing.expectEqual(
        SnapshotFieldPresence.required,
        snapshots.sliceRecordFields(top_snapshot.fields).items(.presence)[0],
    );

    const middle_content = snapshots.getContent(top_snapshot.ext);
    if (middle_content != .structure or middle_content.structure != .record) return error.TestUnexpectedResult;
    const middle_snapshot = middle_content.structure.record;
    try std.testing.expectEqual(
        SnapshotFieldPresence.unknown,
        snapshots.sliceRecordFields(middle_snapshot.fields).items(.presence)[0],
    );

    const tail_content = snapshots.getContent(middle_snapshot.ext);
    if (tail_content != .structure or tail_content.structure != .record) return error.TestUnexpectedResult;
    const tail_snapshot_fields = tail_content.structure.record.fields;
    try std.testing.expectEqual(
        SnapshotFieldPresence.unknown,
        snapshots.sliceRecordFields(tail_snapshot_fields).items(.presence)[0],
    );

    var gathered_fields = try SnapshotRecordFieldSafeList.initCapacity(gpa, 4);
    defer gathered_fields.deinit(gpa);
    const gathered = try snapshots.gatherRecordFields(top_snapshot_idx, gpa, &gathered_fields);
    if (gathered != .record) return error.TestUnexpectedResult;
    try std.testing.expectEqualSlices(
        SnapshotFieldPresence,
        &.{ .required, .unknown, .unknown },
        gathered_fields.sliceRange(gathered.record).items(.presence),
    );
}

// Depth pin for the snapshot walk. Any of the ~29 `snapshotVarForError` call
// sites can be handed a type as deep as the instantiator can build, so this
// walk must survive whatever the copier produces. The chain is built from
// alias backing vars: the snapshot walk descends every one of them, while the
// per-node TypeWriter render stays constant-size (an alias renders as its
// name), which keeps the pin about walk depth rather than about how much text
// a deep type formats to. The recursive walk this replaced segfaulted on
// exactly this chain.
test "snapshotting a spine deeper than any native-stack budget" {
    const allocator = std.testing.allocator;
    const depth: u32 = 40000;

    var idents = try Ident.Store.initCapacity(allocator, 16);
    defer idents.deinit(allocator);
    const alias_ident = try idents.insert(allocator, Ident.for_text("Chain"));

    var store = try TypesStore.initCapacity(allocator, depth + 8, 8);
    defer store.deinit();

    var current = try store.freshFromContent(.{ .structure = .empty_record });
    for (0..depth) |_| {
        current = try store.freshFromContent(try store.mkAlias(
            .{ .ident_idx = alias_ident },
            current,
            &.{},
            base.ModuleIdentity.Idx.NONE,
        ));
    }

    var type_writer = try TypeWriter.initFromParts(allocator, &store, &idents, null);
    defer type_writer.deinit();

    var snapshots = try Store.initCapacity(allocator, depth + 8);
    defer snapshots.deinit();

    const idx = try snapshots.snapshotVarForError(&store, &type_writer, current);
    try std.testing.expect(snapshots.getFormattedString(idx) != null);
}

test "diagnostic snapshots preserve acyclic sharing without rendering unused nodes" {
    const gpa = std.testing.allocator;
    var store = try TypesStore.initCapacity(gpa, 64, 8);
    defer store.deinit();
    var idents = try Ident.Store.initCapacity(gpa, 4);
    defer idents.deinit(gpa);
    var writer = try TypeWriter.initFromParts(gpa, &store, &idents, null);
    defer writer.deinit();
    var snapshots = try Store.initCapacity(gpa, 32);
    defer snapshots.deinit();

    const depth = 24;
    var root = try store.freshFromContent(.{ .structure = .empty_record });
    for (0..depth) |_| {
        root = try store.freshFromContent(.{ .structure = .{ .tuple = .{
            .elems = try store.appendVars(&.{ root, root }),
        } } });
    }
    const captured = try snapshots.snapshotVarForError(&store, &writer, root);
    try std.testing.expectEqual(@as(usize, depth + 1), snapshots.contents.len());
    try std.testing.expect(snapshots.test_capture_steps <= 2 * depth + 1);
    try std.testing.expectEqual(@as(usize, 0), snapshots.formatted_strings.count());
    try std.testing.expectEqual(null, snapshots.render_graphs.items[0]);
    const children = snapshots.sliceVars(snapshots.getContent(captured).structure.tuple.elems);
    try std.testing.expectEqual(children[0], children[1]);
}

test "diagnostic snapshots preserve sharing through recursive subgraphs" {
    const gpa = std.testing.allocator;
    var store = try TypesStore.initCapacity(gpa, 32, 8);
    defer store.deinit();
    var idents = try Ident.Store.initCapacity(gpa, 4);
    defer idents.deinit(gpa);
    var writer = try TypeWriter.initFromParts(gpa, &store, &idents, null);
    defer writer.deinit();
    var snapshots = try Store.initCapacity(gpa, 32);
    defer snapshots.deinit();

    const depth = 12;
    const root = try store.fresh();
    var current = root;
    for (0..depth) |_| {
        current = try store.freshFromContent(.{ .structure = .{ .tuple = .{
            .elems = try store.appendVars(&.{ current, current }),
        } } });
    }
    try store.setVarContent(root, store.resolveVar(current).desc.content);
    _ = try snapshots.snapshotVarForError(&store, &writer, root);
    try std.testing.expect(snapshots.contents.len() <= depth + 3);
    try std.testing.expect(snapshots.test_capture_steps <= 2 * depth + 3);
    try std.testing.expectEqual(null, snapshots.render_graphs.items[0]);
}

test "diagnostic snapshots render frozen identities after poisoning and naming scope changes" {
    const gpa = std.testing.allocator;
    var store = try TypesStore.initCapacity(gpa, 16, 8);
    defer store.deinit();
    var idents = try Ident.Store.initCapacity(gpa, 4);
    defer idents.deinit(gpa);
    const method = try idents.insert(gpa, Ident.for_text("echo"));
    const shared = try store.fresh();
    const root = try store.freshFromContent(try store.mkFuncPure(&.{shared}, shared));
    const constraints = try store.appendStaticDispatchConstraints(&.{
        .{ .fn_name = method, .fn_var = root, .origin = .{ .where_clause = .{} } },
    });
    try store.setVarContent(shared, .{ .flex = .{ .name = null, .constraints = constraints } });
    var writer = try TypeWriter.initFromParts(gpa, &store, &idents, null);
    defer writer.deinit();
    const expected = try gpa.dupe(u8, try writer.writeGet(root, .wrap));
    defer gpa.free(expected);
    var snapshots = try Store.initCapacity(gpa, 16);
    defer snapshots.deinit();
    const captured = try snapshots.snapshotVarForError(&store, &writer, root);
    try store.setVarContent(root, .err);
    try store.setVarContent(shared, .err);
    _ = try idents.insert(gpa, Ident.for_text("arg"));
    _ = try idents.insert(gpa, Ident.for_text("a"));

    const before_format = snapshots.mark();
    try std.testing.expectEqualStrings(expected, try snapshots.formatType(captured));
    snapshots.truncateToMark(before_format);
    try std.testing.expectEqual(@as(usize, 0), snapshots.formatted_strings.count());
    const first_text = try snapshots.formatType(captured);
    try std.testing.expectEqualStrings(expected, first_text);

    // Formatting a later, large capture cannot relocate an earlier string or
    // change which version of the solver graph that string describes.
    var long_name: [4096]u8 = undefined;
    @memset(&long_name, 'L');
    const alias_name = try idents.insert(gpa, Ident.for_text(&long_name));
    const alias = try store.freshFromContent(try store.mkAlias(
        .{ .ident_idx = alias_name },
        root,
        &.{},
        base.ModuleIdentity.Idx.NONE,
    ));
    const later = try snapshots.snapshotVarForError(&store, &writer, alias);
    try std.testing.expectEqualStrings(&long_name, try snapshots.formatType(later));
    try std.testing.expectEqualStrings(expected, first_text);
}

test "diagnostic snapshot rendering options belong to each capture" {
    const gpa = std.testing.allocator;
    var store = try TypesStore.initCapacity(gpa, 16, 8);
    defer store.deinit();
    var idents = try Ident.Store.initCapacity(gpa, 4);
    defer idents.deinit(gpa);
    const method = try idents.insert(gpa, Ident.for_text("from_quote"));
    const receiver = try store.fresh();
    const callable = try store.freshFromContent(try store.mkFuncPure(&.{receiver}, receiver));
    const constraints = try store.appendStaticDispatchConstraints(&.{
        .{ .fn_name = method, .fn_var = callable, .origin = .{ .from_literal = .quote } },
    });
    try store.setVarContent(receiver, .{ .flex = .{ .name = null, .constraints = constraints } });
    var writer = try TypeWriter.initFromParts(gpa, &store, &idents, null);
    defer writer.deinit();
    var snapshots = try Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    writer.omit_literal_conversion_constraints = true;
    const expected_omitted = try gpa.dupe(u8, try writer.writeGet(receiver, .wrap));
    defer gpa.free(expected_omitted);
    const omitted = try snapshots.snapshotVarForError(&store, &writer, receiver);
    writer.omit_literal_conversion_constraints = false;
    const expected_shown = try gpa.dupe(u8, try writer.writeGet(receiver, .wrap));
    defer gpa.free(expected_shown);
    const shown = try snapshots.snapshotVarForError(&store, &writer, receiver);
    try std.testing.expect(expected_shown.len > expected_omitted.len);
    try std.testing.expectEqualStrings(expected_omitted, try snapshots.formatType(omitted));
    try std.testing.expectEqualStrings(expected_shown, try snapshots.formatType(shown));
}

fn diagnosticAllocationBytes(requirement_count: usize) (Allocator.Error || error{ WriteFailed, TestExpectedEqual })!usize {
    const gpa = std.testing.allocator;
    var store = try TypesStore.initCapacity(gpa, 128, 8);
    defer store.deinit();
    var idents = try Ident.Store.initCapacity(gpa, 4);
    defer idents.deinit(gpa);
    const method = try idents.insert(gpa, Ident.for_text("get"));
    const receiver = try store.fresh();
    const result = try store.freshFromContent(.{ .structure = .empty_record });
    const constraints = try gpa.alloc(types.StaticDispatchConstraint, requirement_count);
    defer gpa.free(constraints);
    for (constraints) |*constraint| constraint.* = .{
        .fn_name = method,
        .fn_var = try store.freshFromContent(try store.mkFuncPure(&.{receiver}, result)),
        .origin = .{ .where_clause = .{} },
    };
    try store.setVarContent(receiver, .{ .flex = .{
        .name = null,
        .constraints = try store.appendStaticDispatchConstraints(constraints),
    } });
    var writer = try TypeWriter.initFromParts(gpa, &store, &idents, null);
    defer writer.deinit();
    const expected = try gpa.dupe(u8, try writer.writeGet(receiver, .wrap));
    defer gpa.free(expected);
    var measured = std.testing.FailingAllocator.init(gpa, .{});
    var snapshots = try Store.initCapacity(measured.allocator(), 16);
    defer snapshots.deinit();
    const before = measured.allocated_bytes;
    const captured = try snapshots.snapshotVarForError(&store, &writer, receiver);
    const text = try snapshots.formatType(captured);
    try std.testing.expectEqualStrings(expected, text);
    return measured.allocated_bytes - before;
}

test "diagnostic capture and requested rendering allocate in proportion to requirements" {
    const small = try diagnosticAllocationBytes(32);
    const large = try diagnosticAllocationBytes(64);
    if (large > 3 * small) std.debug.print("diagnostic allocation bytes: 32 requirements {d}; 64 requirements {d}\n", .{ small, large });
    try std.testing.expect(large <= 3 * small);
}

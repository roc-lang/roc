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
    record_unbound: SnapshotRecordFieldSafeList.Range,
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

    const Self = @This();

    /// Returns true if field `a` should sort before field `b` by name.
    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByName(ident_store, a, b) == .lt;
    }

    /// Compares two record fields by their name for ordering.
    pub fn orderByName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.name);
        const b_text = store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }
};

/// A snapshotted tag union type with its tags and extension variable.
pub const SnapshotTagUnion = struct {
    tags: SnapshotTagSafeList.Range,
    ext: SnapshotContentIdx,
};

/// A single tag in a snapshotted tag union with its name and argument types.
pub const SnapshotTag = struct {
    name: Ident.Idx,
    args: SnapshotContentIdxSafeList.Range,
    /// Pre-formatted string representation of this tag (e.g., "TagName(a, b)")
    formatted: []const u8,
};

/// A snapshotted static dispatch constraint for method resolution.
pub const SnapshotStaticDispatchConstraint = struct {
    fn_name: Ident.Idx,
    fn_content: SnapshotContentIdx,
    /// The type variable that has this constraint (the dispatcher).
    /// This is the type that the method is called on.
    dispatcher: SnapshotContentIdx,
};

const Var = types.Var;

/// Self-contained snapshot store with fully resolved content (ie no Vars)
///
/// Whenever a type error occurs, we update the `Var` in the type store to
/// have `.err` content. This is necessary to continue type-checking but
/// looses essential error information. So before doing this, we create a fully
/// resolved snapshot of the type that we can use in reporting
///
/// Entry point is `snapshotVarForError`
const TypeWriter = types.TypeWriter;

const ByteList = std.array_list.Managed(u8);
const ByteListRange = struct { start: usize, count: usize };

/// Which var a snapshot frame records: `original_var` is the var the caller
/// asked for—what the TypeWriter renders and what tag formatting uses as its
/// root—while `resolved_var` is its union-find root, which is what the visit
/// path and the flex identity record.
const SnapshotFill = struct {
    original_var: types.Var,
    resolved_var: types.Var,
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
    record_unbound: RecordUnboundFrame,
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

const RecordUnboundFrame = struct {
    fill: SnapshotFill,
    source_fields: types.RecordField.SafeMultiList.Range,
    idx: u32 = 0,
    scratch_top: u32,
    stage: enum { fields, await_field } = .fields,
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
/// error state. Also stores pre-formatted type strings for efficient error reporting.
pub const Store = struct {
    const Self = @This();

    gpa: Allocator,

    // Content storage
    contents: SnapshotContentList,

    /// Vars on the current visit path, so a re-entry becomes a recursion
    /// marker instead of an unbounded descent. Entries are unique: a var
    /// already on the path is never added again.
    seen_vars: std.AutoHashMapUnmanaged(Var, void),

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
    formatted_strings: std.AutoHashMapUnmanaged(SnapshotContentIdx, ByteListRange),
    formatted_strings_backing: ByteList,

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
                var map = std.AutoHashMapUnmanaged(SnapshotContentIdx, ByteListRange){};
                try map.ensureTotalCapacity(gpa, 32);
                break :blk map;
            },
            .formatted_strings_backing = try ByteList.initCapacity(gpa, 512),
        };
    }

    pub fn deinit(self: *Self) void {
        // Free all stored formatted strings
        self.formatted_strings.deinit(self.gpa);
        self.formatted_strings_backing.deinit();

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

    /// Get the pre-formatted string for a snapshot.
    pub fn getFormattedString(self: *const Self, idx: SnapshotContentIdx) ?[]const u8 {
        const mb_range = self.formatted_strings.get(idx);

        if (mb_range == null) return null;
        const range = mb_range.?;

        return self.formatted_strings_backing.items[range.start..][0..range.count];
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
        formatted_strings_backing_len: usize,
    };

    /// Capture the current lengths for a later `truncateToMark`.
    pub fn mark(self: *const Self) Mark {
        return .{
            .contents_len = @intCast(self.contents.len()),
            .content_indexes_len = @intCast(self.content_indexes.len()),
            .record_fields_len = self.record_fields.len(),
            .tags_len = self.tags.len(),
            .static_dispatch_constraints_len = @intCast(self.static_dispatch_constraints.len()),
            .formatted_strings_backing_len = self.formatted_strings_backing.items.len,
        };
    }

    /// Discard every snapshot recorded after `mark` was captured—the
    /// rollback of a speculative probe that recorded against this store.
    /// Owned memory is released exactly as `deinit` would: the `formatted`
    /// string of each truncated tag is freed, and the `formatted_strings`
    /// entries keyed by truncated content indexes (every content appended
    /// after the mark gets its formatted entry when it is created, so the
    /// keys to drop are exactly the truncated index range) are removed before
    /// their backing bytes are truncated.
    pub fn truncateToMark(self: *Self, m: Mark) void {
        const tags_len = self.tags.len();
        if (tags_len > m.tags_len) {
            const removed_range = SnapshotTagSafeList.Range{
                .start = @enumFromInt(m.tags_len),
                .count = @intCast(tags_len - m.tags_len),
            };
            for (self.tags.sliceRange(removed_range).items(.formatted)) |formatted| {
                self.gpa.free(formatted);
            }
        }

        var content_idx = m.contents_len;
        const contents_len: usize = @intCast(self.contents.len());
        while (content_idx < contents_len) : (content_idx += 1) {
            _ = self.formatted_strings.remove(@enumFromInt(content_idx));
        }

        self.contents.items.shrinkRetainingCapacity(m.contents_len);
        self.content_indexes.items.shrinkRetainingCapacity(m.content_indexes_len);
        self.record_fields.items.shrinkRetainingCapacity(m.record_fields_len);
        self.tags.items.shrinkRetainingCapacity(m.tags_len);
        self.static_dispatch_constraints.items.shrinkRetainingCapacity(m.static_dispatch_constraints_len);
        self.formatted_strings_backing.shrinkRetainingCapacity(m.formatted_strings_backing_len);
    }

    /// Deep copy a type variable for error reporting. This snapshots the type structure
    /// AND formats each nested type using TypeWriter before the types get overwritten with .err.
    /// ONLY use this in error paths - it allocates formatted strings for all nested types.
    ///
    /// The graph walk runs on an explicit heap worklist, so snapshot depth is
    /// bounded only by available memory, never by the native stack.
    pub fn snapshotVarForError(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, var_: types.Var) std.mem.Allocator.Error!SnapshotContentIdx {
        const trace = tracy.trace(@src());
        defer trace.end();

        const frames_base = self.frames.items.len;
        const values_base = self.pending_values.top();
        // A completed walk drains the worklist and leaves `seen_vars` empty.
        // An allocation failure mid-walk can leave entries behind on buffers
        // this store keeps for the next snapshot, so unwind them here and
        // preserve that invariant on both exit paths.
        errdefer {
            self.frames.items.len = frames_base;
            self.pending_values.clearFrom(values_base);
            self.seen_vars.clearRetainingCapacity();
        }

        if (!try self.requestVar(store, type_writer, var_)) {
            while (self.frames.items.len > frames_base) {
                const top = &self.frames.items[self.frames.items.len - 1];
                // A step either suspends after requesting exactly one child
                // (having already written its own resume state), or finishes
                // without requesting anything—so popping on finish always
                // removes the frame the step ran for.
                const finished = switch (top.*) {
                    .identity => |*frame| try self.stepIdentity(store, type_writer, frame),
                    .alias => |*frame| try self.stepAlias(store, type_writer, frame),
                    .tuple => |*frame| try self.stepTuple(store, type_writer, frame),
                    .nominal => |*frame| try self.stepNominal(store, type_writer, frame),
                    .func => |*frame| try self.stepFunc(store, type_writer, frame),
                    .record => |*frame| try self.stepRecord(store, type_writer, frame),
                    .record_unbound => |*frame| try self.stepRecordUnbound(store, type_writer, frame),
                    .tag_union => |*frame| try self.stepTagUnion(store, type_writer, frame),
                };
                if (finished) {
                    self.frames.items.len -= 1;
                }
            }
        }

        std.debug.assert(self.seen_vars.count() == 0);
        std.debug.assert(self.pending_values.top() == values_base + 1);
        return self.pending_values.pop().?;
    }

    /// Snapshot one var's head: hand back a recursion marker for a var already
    /// on the path, and otherwise mark it as being visited and either record
    /// its content immediately (contents with no children) or push the frame
    /// that will record it. Returns true when the result index is already on
    /// the value stack; false when a frame was pushed.
    fn requestVar(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, var_: types.Var) std.mem.Allocator.Error!bool {
        const resolved = store.resolveVar(var_);

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
                    .record, .record_unbound, .tuple, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .tag_union, .empty_tag_union => null,
                },
                // Error types shouldn't create cycles
                .err => unreachable,
            };
            const idx = try self.contents.append(self.gpa, .{ .recursive = recursive_name });
            try self.pending_values.append(idx);
            return true;
        }

        // If not, mark it as being visited
        try self.seen_vars.put(self.gpa, resolved.var_, {});

        const fill = SnapshotFill{ .original_var = var_, .resolved_var = resolved.var_ };
        switch (resolved.desc.content) {
            .err => {
                try self.finishFrame(type_writer, fill, SnapshotContent.err);
                return true;
            },
            .flex => |flex| {
                if (flex.constraints.len() == 0) {
                    try self.finishFrame(type_writer, fill, SnapshotContent{ .flex = SnapshotFlex{
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
                    try self.finishFrame(type_writer, fill, SnapshotContent{ .rigid = SnapshotRigid{
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
                    .backing = store.getAliasBackingVar(alias),
                    .args = store.sliceAliasArgs(alias),
                } });
                return false;
            },
            // Presence facts are represented on their owning record field in
            // snapshots, never as standalone reportable types.
            .field_presence => {
                try self.finishFrame(type_writer, fill, SnapshotContent.err);
                return true;
            },
            .structure => |flat_type| switch (flat_type) {
                .empty_record => {
                    try self.finishFrame(type_writer, fill, SnapshotContent{ .structure = SnapshotFlatType.empty_record });
                    return true;
                },
                .empty_tag_union => {
                    try self.finishFrame(type_writer, fill, SnapshotContent{ .structure = SnapshotFlatType.empty_tag_union });
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
                        .args = store.sliceNominalArgs(nominal_type),
                        .scratch_top = self.scratch_content.top(),
                    };
                    if (store.lookupNominalDecl(nominal_type)) |decl_idx| {
                        const decl = store.getNominalDecl(decl_idx);
                        if (decl.isValid()) {
                            frame.backing = decl.backing;
                            frame.stage = .await_backing;
                            try self.frames.append(self.gpa, .{ .nominal = frame });
                            _ = try self.requestVar(store, type_writer, decl.backing);
                            return false;
                        }
                    }
                    try self.scratch_content.append(try self.contents.append(self.gpa, .err));
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
                .record_unbound => |fields| {
                    try self.frames.append(self.gpa, .{ .record_unbound = .{
                        .fill = fill,
                        .source_fields = fields,
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

    /// Record one node's finished content, format it with the TypeWriter, and
    /// take it off the visit path. The formatted string is keyed by the
    /// content index this call assigns, exactly as the recursion did.
    fn finishFrame(
        self: *Self,
        type_writer: *TypeWriter,
        fill: SnapshotFill,
        content: SnapshotContent,
    ) std.mem.Allocator.Error!void {
        const snapshot_idx = try self.contents.append(self.gpa, content);

        // Format this type and store the formatted string
        // Here, we run the TypeWriter, writing directly into our backing
        {
            const formatted_strings_start = self.formatted_strings_backing.items.len;
            type_writer.writeInto(&self.formatted_strings_backing, fill.original_var, .wrap) catch return error.OutOfMemory;
            const formatted_strings_end = self.formatted_strings_backing.items.len;

            const formatted_range = ByteListRange{
                .start = formatted_strings_start,
                .count = formatted_strings_end - formatted_strings_start,
            };

            try self.formatted_strings.put(self.gpa, snapshot_idx, formatted_range);
        }

        _ = self.seen_vars.remove(fill.resolved_var);
        try self.pending_values.append(snapshot_idx);
    }

    fn stepIdentity(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, frame: *IdentityFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .head => {
                    if (frame.idx < frame.constraints.len) {
                        frame.stage = .await_fn;
                        if (!try self.requestVar(store, type_writer, frame.constraints[frame.idx].fn_var)) return false;
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
                    try self.finishFrame(type_writer, frame.fill, content);
                    return true;
                },
                .await_fn => {
                    try self.scratch_static_dispatch_constraints.append(.{
                        .fn_name = frame.constraints[frame.idx].fn_name,
                        .fn_content = self.pending_values.pop().?,
                        // Dispatcher is set when collecting constraints during write
                        .dispatcher = undefined,
                    });
                    frame.idx += 1;
                    frame.stage = .head;
                },
            }
        }
    }

    fn stepAlias(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, frame: *AliasFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .backing => {
                    frame.stage = .await_backing;
                    if (!try self.requestVar(store, type_writer, frame.backing)) return false;
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
                        if (!try self.requestVar(store, type_writer, frame.args[frame.idx])) return false;
                        continue;
                    }
                    const args_range = try self.content_indexes.appendSlice(
                        self.gpa,
                        self.scratch_content.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_content.clearFrom(frame.scratch_top);
                    try self.finishFrame(type_writer, frame.fill, SnapshotContent{ .alias = SnapshotAlias{
                        .ident = frame.ident,
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

    fn stepTuple(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, frame: *TupleFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .elems => {
                    if (frame.idx < frame.elems.len) {
                        frame.stage = .await_elem;
                        if (!try self.requestVar(store, type_writer, frame.elems[frame.idx])) return false;
                        continue;
                    }
                    const elems_range = try self.content_indexes.appendSlice(
                        self.gpa,
                        self.scratch_content.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_content.clearFrom(frame.scratch_top);
                    try self.finishFrame(type_writer, frame.fill, SnapshotContent{ .structure = SnapshotFlatType{
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

    fn stepNominal(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, frame: *NominalFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .await_backing => {
                    try self.scratch_content.append(self.pending_values.pop().?);
                    frame.stage = .args;
                },
                .args => {
                    if (frame.idx < frame.args.len) {
                        frame.stage = .await_arg;
                        if (!try self.requestVar(store, type_writer, frame.args[frame.idx])) return false;
                        continue;
                    }
                    const args_range = try self.content_indexes.appendSlice(
                        self.gpa,
                        self.scratch_content.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_content.clearFrom(frame.scratch_top);
                    try self.finishFrame(type_writer, frame.fill, SnapshotContent{ .structure = SnapshotFlatType{
                        .nominal_type = SnapshotNominalType{
                            .ident = frame.ident,
                            .vars = args_range,
                            .origin_module = frame.origin_module,
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

    fn stepFunc(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, frame: *FuncFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .args => {
                    if (frame.idx < frame.args.len) {
                        frame.stage = .await_arg;
                        if (!try self.requestVar(store, type_writer, frame.args[frame.idx])) return false;
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
                    if (!try self.requestVar(store, type_writer, frame.ret)) return false;
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
                    try self.finishFrame(type_writer, frame.fill, content);
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
        return store.record_fields.get(@enumFromInt(@intFromEnum(range.start) + idx));
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

    fn stepRecord(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, frame: *RecordFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .fields => {
                    if (frame.idx < frame.source_fields.count) {
                        frame.stage = .await_field;
                        const field = sourceRecordField(store, frame.source_fields, frame.idx);
                        if (!try self.requestVar(store, type_writer, field.presence.typeVar())) return false;
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
                    if (!try self.requestVar(store, type_writer, frame.ext)) return false;
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
                    try self.finishFrame(type_writer, frame.fill, SnapshotContent{ .structure = SnapshotFlatType{
                        .record = SnapshotRecord{ .fields = frame.fields_range, .ext = deep_ext },
                    } });
                    return true;
                },
            }
        }
    }

    fn stepRecordUnbound(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, frame: *RecordUnboundFrame) std.mem.Allocator.Error!bool {
        while (true) {
            switch (frame.stage) {
                .fields => {
                    if (frame.idx < frame.source_fields.count) {
                        frame.stage = .await_field;
                        const field = sourceRecordField(store, frame.source_fields, frame.idx);
                        if (!try self.requestVar(store, type_writer, field.presence.typeVar())) return false;
                        continue;
                    }
                    const fields_range = try self.record_fields.appendSlice(
                        self.gpa,
                        self.scratch_record_fields.sliceFromStart(frame.scratch_top),
                    );
                    self.scratch_record_fields.clearFrom(frame.scratch_top);
                    try self.finishFrame(type_writer, frame.fill, SnapshotContent{ .structure = SnapshotFlatType{
                        .record_unbound = fields_range,
                    } });
                    return true;
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
            }
        }
    }

    fn stepTagUnion(self: *Self, store: *const TypesStore, type_writer: *TypeWriter, frame: *TagUnionFrame) std.mem.Allocator.Error!bool {
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
                        if (!try self.requestVar(store, type_writer, frame.ext)) return false;
                        continue;
                    }
                    frame.content_scratch_top = self.scratch_content.top();
                    frame.arg_idx = 0;
                    frame.stage = .tag_args;
                },
                .tag_args => {
                    // Indexing through the run's start only happens when the
                    // tag union has tags; start may be undefined when count is 0.
                    const tag = store.tags.get(@enumFromInt(@intFromEnum(frame.source_tags.start) + frame.tag_idx));
                    const tag_args_slice = store.sliceVars(tag.args);
                    if (frame.arg_idx < tag_args_slice.len) {
                        frame.stage = .await_tag_arg;
                        if (!try self.requestVar(store, type_writer, tag_args_slice[frame.arg_idx])) return false;
                        continue;
                    }
                    const tag_args_range = try self.content_indexes.appendSlice(
                        self.gpa,
                        self.scratch_content.sliceFromStart(frame.content_scratch_top),
                    );
                    self.scratch_content.clearFrom(frame.content_scratch_top);

                    // Format the tag using TypeWriter (uses correct Roc syntax like "TagName(a, b)")
                    const formatted_tag = type_writer.writeTagGet(tag, frame.fill.original_var) catch return error.OutOfMemory;
                    const formatted_owned = try self.gpa.dupe(u8, formatted_tag);

                    try self.scratch_tags.append(.{
                        .name = tag.name,
                        .args = tag_args_range,
                        .formatted = formatted_owned,
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
                    try self.finishFrame(type_writer, frame.fill, SnapshotContent{ .structure = SnapshotFlatType{
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
    /// so this distinguishes it from an open (`..`) pattern or an unbound record.
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
                .record_unbound => |fields| {
                    if (fields.count == 0) {
                        return .empty_record;
                    }

                    const fields_out_top: u32 = @intCast(fields_out.items.len);
                    const slice = self.sliceRecordFields(fields);
                    for (slice.items(.name), slice.items(.content), slice.items(.presence)) |name, content, presence| {
                        _ = try fields_out.append(gpa, .{ .name = name, .content = content, .presence = presence });
                    }
                    const fields_out_range = fields_out.rangeToEnd(fields_out_top);
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
                        for (ext_fields.items(.name), ext_fields.items(.content), ext_fields.items(.presence)) |name, field_content, presence| {
                            _ = try fields_out.append(gpa, .{ .name = name, .content = field_content, .presence = presence });
                        }
                        ext_idx = rec.ext;
                    },
                    .record_unbound => |fields_range| {
                        const ext_fields = self.sliceRecordFields(fields_range);
                        for (ext_fields.items(.name), ext_fields.items(.content), ext_fields.items(.presence)) |name, field_content, presence| {
                            _ = try fields_out.append(gpa, .{ .name = name, .content = field_content, .presence = presence });
                        }
                        break;
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

    /// Get the pre-formatted string representation of a tag (e.g., "TagName(a, b)").
    /// The tag was formatted using TypeWriter during snapshotting.
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
    const tail_var = try type_store.freshFromContent(.{ .structure = .{ .record_unbound = tail_fields } });

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
    if (top_content != .structure or top_content.structure != .record) unreachable;
    const top_snapshot = top_content.structure.record;
    try std.testing.expectEqual(
        SnapshotFieldPresence.required,
        snapshots.sliceRecordFields(top_snapshot.fields).items(.presence)[0],
    );

    const middle_content = snapshots.getContent(top_snapshot.ext);
    if (middle_content != .structure or middle_content.structure != .record) unreachable;
    const middle_snapshot = middle_content.structure.record;
    try std.testing.expectEqual(
        SnapshotFieldPresence.unknown,
        snapshots.sliceRecordFields(middle_snapshot.fields).items(.presence)[0],
    );

    const tail_content = snapshots.getContent(middle_snapshot.ext);
    if (tail_content != .structure or tail_content.structure != .record_unbound) unreachable;
    const tail_snapshot_fields = tail_content.structure.record_unbound;
    try std.testing.expectEqual(
        SnapshotFieldPresence.unknown,
        snapshots.sliceRecordFields(tail_snapshot_fields).items(.presence)[0],
    );

    var gathered_fields = try SnapshotRecordFieldSafeList.initCapacity(gpa, 4);
    defer gathered_fields.deinit(gpa);
    const gathered = try snapshots.gatherRecordFields(top_snapshot_idx, gpa, &gathered_fields);
    if (gathered != .record) unreachable;
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

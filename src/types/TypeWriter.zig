//! Type serialization utilities for writing type information as S-expressions.
//!
//! This module provides functionality to serialize type store contents and
//! individual types into S-expression format for debugging, inspection, and
//! external tool integration. The serialized output helps visualize the
//! compiler's internal type representations.

const std = @import("std");
const base = @import("base");
const collections_mod = @import("collections");
const types_mod = @import("types.zig");
const import_mapping_mod = @import("import_mapping.zig");

const TypesStore = @import("store.zig").Store;
const Allocator = std.mem.Allocator;
const Var = types_mod.Var;
const Content = types_mod.Content;
const RecordField = types_mod.RecordField;
const TagUnion = types_mod.TagUnion;
const Tag = types_mod.Tag;
const Alias = types_mod.Alias;
const FlatType = types_mod.FlatType;
const NominalType = types_mod.NominalType;
const Record = types_mod.Record;
const Tuple = types_mod.Tuple;
const Func = types_mod.Func;

// const SExpr = base.SExpr;
const Ident = base.Ident;

const TypeContext = enum {
    General,
    RecordExtension,
    TagUnionExtension,
    RecordFieldContent,
    TupleFieldContent,
    FunctionArgument,
    FunctionReturn,
};

/// Helper that accepts a `Var` and write it as a nice string.
/// Entry point is `writeVar`
const TypeWriter = @This();

types: *const TypesStore,
idents: *const Ident.Store,
buf: std.array_list.Managed(u8),
/// The vars enclosing the node being rendered, innermost last. A var already
/// on this stack renders as `<RecursiveType>` instead of descending again.
seen: std.array_list.Managed(Var),
/// Membership half of `seen`. The stack keeps the order pops need; this
/// answers "is it enclosing?" without a scan, which a spine deep enough to
/// need frames also needs.
seen_set: std.AutoHashMap(Var, void),
/// Suspended steps of the rendering walk, innermost last. The walk descends
/// on this heap stack rather than the native one, so render depth is bounded
/// only by available memory.
frames: std.array_list.Managed(Frame),
seen_count_var_occurrences: std.array_list.Managed(Var),
/// Membership half of `seen_count_var_occurrences`.
seen_count_set: std.AutoHashMap(Var, void),
/// Suspended steps of the occurrence-counting walk, innermost last.
count_frames: std.array_list.Managed(CountFrame),
/// Children collected for the occurrence-counting frames in flight, one
/// contiguous run per frame.
count_pending: std.array_list.Managed(Var),
/// Extension vars already reached by the row collection currently running.
/// Row collection runs to completion without rendering anything, so one
/// buffer serves every row node.
ext_seen: std.AutoHashMap(Var, void),
next_name_index: u32,
name_counters: std.EnumMap(TypeContext, u32),
flex_var_names_map: std.AutoHashMap(Var, FlexVarNameRange),
flex_var_names: std.array_list.Managed(u8),
static_dispatch_constraints: std.array_list.Managed(ConstraintWithDispatcher),
static_dispatch_constraints_tmp: std.array_list.Managed(StaticDispatchTmp),
buf_tmp: std.array_list.Managed(u8),
name_tmp: std.array_list.Managed(u8),
scratch_record_fields: std.array_list.Managed(types_mod.RecordField),
scratch_tags: std.array_list.Managed(types_mod.Tag),
/// Mapping from fully-qualified type identifiers to their display names based on top-level imports.
/// This allows error messages to show "Str" instead of "Builtin.Str" for auto-imported types,
/// "Bar" instead of "Foo.Bar" for nested imports, and aliases like "Baz" instead of "Foo".
import_mapping: ?*const import_mapping_mod.ImportMapping,
/// Optional resolver for rendering a defaulted field's source expression.
mb_default_source: ?DefaultSourceFn = null,
default_source_ctx: *const anyopaque = undefined,
/// The allocator used to create owned fields
gpa: std.mem.Allocator,

/// Resolves a defaulted field identity to source text for display.
pub const DefaultSourceFn = *const fn (ctx: *const anyopaque, id: types_mod.DefaultId) ?[]const u8;

/// Install the source resolver used when rendering defaulted record fields.
pub fn setDefaultSourceResolver(self: *TypeWriter, ctx: *const anyopaque, resolver: DefaultSourceFn) void {
    self.default_source_ctx = ctx;
    self.mb_default_source = resolver;
}

const ByteWrite = std.Io.Writer;

const FlexVarNameRange = struct { start: usize, end: usize };

const StaticDispatchTmp = struct {
    fn_name: Ident.Idx,

    /// Start of the type name in buf_tmp
    type_name_start: usize,
    /// End of the type name in buf_tmp
    type_name_end: usize,

    start: usize,
    len: usize,

    /// Ctx to be provided to sort function
    const SortCtx = struct {
        buf_tmp: *const std.array_list.Managed(u8),
        idents: *const Ident.Store,
    };

    /// A function to be passed into std.mem.sort to sort fields by name
    fn sort(ctx: SortCtx, a: StaticDispatchTmp, b: StaticDispatchTmp) bool {
        const a_type_name = ctx.buf_tmp.items[a.type_name_start..a.type_name_end];
        const b_type_name = ctx.buf_tmp.items[b.type_name_start..b.type_name_end];
        const type_ord = std.mem.order(u8, a_type_name, b_type_name);
        if (type_ord == .eq) {
            return std.mem.order(
                u8,
                ctx.idents.getText(a.fn_name),
                ctx.idents.getText(b.fn_name),
            ) == .lt;
        }
        return type_ord == .lt;
    }
};

/// A constraint paired with its dispatcher variable (the type that has the constraint)
const ConstraintWithDispatcher = struct {
    dispatcher_var: Var,
    constraint: types_mod.StaticDispatchConstraint,
};

/// The tail a record row ends on, once every field its extension chain
/// contributes has been collected.
const RecordExt = union(enum) {
    flex: struct { var_: Var, payload: types_mod.Flex },
    rigid: types_mod.Rigid,
    empty_record,
    unbound: Var,
    invalid,
};

/// The tail a tag-union row ends on, once every tag its extension chain
/// contributes has been collected.
const TagUnionExt = union(enum) {
    flex: struct { var_: Var, payload: types_mod.Flex },
    rigid: types_mod.Rigid,
    empty_tag_union,
    err,
    alias: Var,
    invalid,
};

/// One suspended step of the rendering walk. A frame is created only after
/// its node's leading bytes are already in the output, so byte order is the
/// recursion's: the frame then renders children one at a time and emits
/// whatever trailing bytes follow them. Child runs are held as store ranges
/// rather than slices, and every child is fetched through the store by index,
/// so a run parked across an unbounded number of nested renders cannot be
/// invalidated by growth of the store it points into.
const Frame = union(enum) {
    args: ArgsFrame,
    func: FuncFrame,
    record: RecordFrame,
    record_unbound: RecordUnboundFrame,
    tag_union: TagUnionFrame,
    tag: TagFrame,
};

/// A parenthesised, comma-separated run of child vars: alias arguments,
/// nominal arguments, and tuple elements. Every var in the run renders in the
/// same context, and the frame owns the `seen` entry its node pushed.
const ArgsFrame = struct {
    vars: Var.SafeList.Range,
    context: TypeContext,
    idx: u32 = 0,
};

/// A function type: its arguments, then its arrow, then its return type. The
/// parentheses a function wears in argument or return position close after
/// the return type, so the frame carries that decision rather than the caller.
const FuncFrame = struct {
    args: Var.SafeList.Range,
    ret: Var,
    arrow: []const u8,
    wrap_in_parens: bool,
    idx: u32 = 0,
    stage: enum { args, ret, done } = .args,
};

/// A normalized record row. The row's fields are collected and sorted into
/// `scratch_record_fields` before the frame exists, so the frame carries the
/// base of its own run and re-reads entries by index: a nested row appends
/// above this run and truncates back to its own base when it finishes. Both
/// extension occurrence counts are taken before the row's first byte is
/// written, which is where the naming they drive expects them.
const RecordFrame = struct {
    fields_base: u32,
    fields_count: u32,
    ext: RecordExt,
    /// The row's own extension var, which a rigid tail uses as the dispatcher
    /// its constraints are recorded against.
    ext_var: Var,
    flex_ext_occurrences: usize,
    unbound_ext_occurrences: usize,
    idx: u32 = 0,
    stage: enum { fields, after_field, ext } = .fields,
};

/// An unbound record: its own fields, then the `..` tail, which names the
/// record itself when it appears more than once in the type being rendered.
const RecordUnboundFrame = struct {
    fields: RecordField.SafeMultiList.Range,
    record_unbound_var: Var,
    unbound_ext_occurrences: usize,
    idx: u32 = 0,
    stage: enum { fields, after_field, ext } = .fields,
};

/// A normalized tag-union row, holding the position within the row. Its tags
/// are collected and sorted into `scratch_tags` on the same base-relative
/// terms as a record's fields.
const TagUnionFrame = struct {
    tags_base: u32,
    tags_count: u32,
    ext: TagUnionExt,
    /// The row's own extension var, which a rigid tail uses as the dispatcher
    /// its constraints are recorded against.
    ext_var: Var,
    idx: u32 = 0,
    stage: enum { tags, ext, done } = .tags,
};

/// One tag's payload run. A tag is not a var of its own, so this frame owns
/// no `seen` entry.
const TagFrame = struct {
    args: Var.SafeList.Range,
    idx: u32 = 0,
};

/// One suspended step of the occurrence-counting walk. Counting visits vars
/// only, so a frame is just the run of children its node contributes, held as
/// a base-relative range of `count_pending`.
const CountFrame = struct {
    base: u32,
    count: u32,
    idx: u32 = 0,
};

pub fn initFromParts(
    gpa: std.mem.Allocator,
    types_store: *const TypesStore,
    idents: *const Ident.Store,
    import_mapping: ?*const import_mapping_mod.ImportMapping,
) std.mem.Allocator.Error!TypeWriter {
    return .{
        .types = types_store,
        .idents = idents,
        .buf = try std.array_list.Managed(u8).initCapacity(gpa, 32),
        .seen = try std.array_list.Managed(Var).initCapacity(gpa, 16),
        .seen_set = std.AutoHashMap(Var, void).init(gpa),
        .frames = try std.array_list.Managed(Frame).initCapacity(gpa, 16),
        .seen_count_var_occurrences = try std.array_list.Managed(Var).initCapacity(gpa, 16),
        .seen_count_set = std.AutoHashMap(Var, void).init(gpa),
        .count_frames = try std.array_list.Managed(CountFrame).initCapacity(gpa, 16),
        .count_pending = try std.array_list.Managed(Var).initCapacity(gpa, 16),
        .ext_seen = std.AutoHashMap(Var, void).init(gpa),
        .next_name_index = 0,
        .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
        .flex_var_names_map = std.AutoHashMap(Var, FlexVarNameRange).init(gpa),
        .flex_var_names = try std.array_list.Managed(u8).initCapacity(gpa, 32),
        .static_dispatch_constraints = try std.array_list.Managed(ConstraintWithDispatcher).initCapacity(gpa, 32),
        .static_dispatch_constraints_tmp = try std.array_list.Managed(StaticDispatchTmp).initCapacity(gpa, 32),
        .buf_tmp = try std.array_list.Managed(u8).initCapacity(gpa, 32),
        .name_tmp = try std.array_list.Managed(u8).initCapacity(gpa, 32),
        .scratch_record_fields = try std.array_list.Managed(types_mod.RecordField).initCapacity(gpa, 32),
        .scratch_tags = try std.array_list.Managed(types_mod.Tag).initCapacity(gpa, 32),
        .import_mapping = import_mapping,
        .mb_default_source = null,
        .default_source_ctx = undefined,
        .gpa = gpa,
    };
}

/// Deinit type writer
pub fn deinit(self: *TypeWriter) void {
    self.buf.deinit();
    self.seen.deinit();
    self.seen_set.deinit();
    self.frames.deinit();
    self.seen_count_var_occurrences.deinit();
    self.seen_count_set.deinit();
    self.count_frames.deinit();
    self.count_pending.deinit();
    self.ext_seen.deinit();
    self.flex_var_names_map.deinit();
    self.flex_var_names.deinit();
    self.static_dispatch_constraints.deinit();
    self.static_dispatch_constraints_tmp.deinit();
    self.buf_tmp.deinit();
    self.name_tmp.deinit();
    self.scratch_record_fields.deinit();
    self.scratch_tags.deinit();
    // import_mapping is borrowed, not owned, so don't deinit it
}

/// Update the import_mapping pointer. This is needed when the owning struct
/// is returned by value, which invalidates the original pointer.
pub fn setImportMapping(self: *TypeWriter, import_mapping: ?*const import_mapping_mod.ImportMapping) void {
    self.import_mapping = import_mapping;
}

/// Reset type writer state
pub fn reset(self: *TypeWriter) void {
    self.buf.clearRetainingCapacity();
    self.seen.clearRetainingCapacity();
    self.frames.clearRetainingCapacity();
    self.seen_count_var_occurrences.clearRetainingCapacity();
    self.count_frames.clearRetainingCapacity();
    self.count_pending.clearRetainingCapacity();
    clearMapIfUsed(Var, void, &self.seen_set);
    clearMapIfUsed(Var, void, &self.seen_count_set);
    clearMapIfUsed(Var, void, &self.ext_seen);
    clearMapIfUsed(Var, FlexVarNameRange, &self.flex_var_names_map);
    self.flex_var_names.clearRetainingCapacity();
    self.static_dispatch_constraints.clearRetainingCapacity();
    self.static_dispatch_constraints_tmp.clearRetainingCapacity();
    self.buf_tmp.clearRetainingCapacity();
    self.name_tmp.clearRetainingCapacity();
    self.scratch_record_fields.clearRetainingCapacity();
    self.scratch_tags.clearRetainingCapacity();

    self.next_name_index = 0;
    self.name_counters = std.EnumMap(TypeContext, u32).init(.{});
}

/// The var at `idx` within `range`, fetched through the store on every
/// access. Frames park ranges rather than slices of them, so a run held
/// across an unbounded number of nested renders never carries a pointer that
/// growth of the store could invalidate.
fn varAt(self: *const TypeWriter, range: Var.SafeList.Range, idx: u32) Var {
    return self.types.vars.get(@enumFromInt(@intFromEnum(range.start) + idx)).*;
}

/// Empty a map without paying for its capacity when it is already empty.
/// One writer serves a whole module—the snapshotter renders a string per node
/// and reporting renders a type per diagnostic—so a single deep render leaves
/// these maps holding tens of thousands of slots, and `clearRetainingCapacity`
/// walks all of that metadata every time. A walk that runs to completion
/// drains its own sets, so the usual entry finds nothing to drop.
fn clearMapIfUsed(comptime K: type, comptime V: type, map: *std.AutoHashMap(K, V)) void {
    if (map.count() != 0) map.clearRetainingCapacity();
}

fn hasSeenVar(self: *const TypeWriter, var_: Var) bool {
    return self.seen_set.contains(var_);
}

/// Mark `var_` as enclosing the node about to render. `seen` never holds a
/// var twice, because a var already on it renders as `<RecursiveType>`
/// instead of being pushed.
fn pushSeen(self: *TypeWriter, var_: Var) std.mem.Allocator.Error!void {
    try self.seen.ensureUnusedCapacity(1);
    try self.seen_set.put(var_, {});
    self.seen.appendAssumeCapacity(var_);
}

fn popSeen(self: *TypeWriter) void {
    const var_ = self.seen.pop().?;
    _ = self.seen_set.remove(var_);
}

const Format = enum { one_line, wrap };

/// Writes the current var into the the writers buffer and returns a bytes slice
pub fn writeGet(self: *TypeWriter, var_: Var, format: Format) error{ OutOfMemory, WriteFailed }![]const u8 {
    try self.write(var_, format);
    return self.get();
}

/// Returns the current contents of the type writer's buffer as a slice.
/// This contains the formatted type representation built up by write operations.
pub fn get(self: *const TypeWriter) []const u8 {
    return self.buf.items;
}

/// Writes a type variable to the buffer, formatting it as a human-readable string.
/// This clears any existing content in the buffer before writing.
pub fn write(self: *TypeWriter, var_: Var, format: Format) error{ OutOfMemory, WriteFailed }!void {
    self.reset();

    var aw = collections_mod.managedWriter(&self.buf);
    try self.writeVar(&aw.writer, var_, var_);

    if (self.static_dispatch_constraints.items.len > 0) {
        collections_mod.managedWriterFinish(&aw, &self.buf);
        aw = collections_mod.managedWriter(&self.buf);
        try self.writeWhereClause(&aw.writer, var_, self.buf.items.len, format);
    }
    collections_mod.managedWriterFinish(&aw, &self.buf);
}

/// Writes a type variable to the provided buffer, formatting it as a human-readable string.
/// This APPENDS to the provided buffer
/// Internal TypeWriter state will be reset before processing
pub fn writeInto(self: *TypeWriter, into: *std.array_list.Managed(u8), var_: Var, format: Format) error{ OutOfMemory, WriteFailed }!void {
    self.reset();

    var aw = collections_mod.managedWriter(into);

    const into_start = into.items.len;
    try self.writeVar(&aw.writer, var_, var_);
    collections_mod.managedWriterFinish(&aw, into);
    const into_end = into.items.len;

    if (self.static_dispatch_constraints.items.len > 0) {
        aw = collections_mod.managedWriter(into);
        try self.writeWhereClause(&aw.writer, var_, into_end - into_start, format);
        collections_mod.managedWriterFinish(&aw, into);
    }
}

/// Writes a type variable to the buffer WITHOUT the where clause.
/// Use this for nested types (function arguments, record fields, etc.) where the
/// where clause should only appear at the top level of the complete type.
pub fn writeWithoutConstraints(self: *TypeWriter, var_: Var) error{ OutOfMemory, WriteFailed }!void {
    self.reset();

    var aw = collections_mod.managedWriter(&self.buf);
    try self.writeVar(&aw.writer, var_, var_);
    collections_mod.managedWriterFinish(&aw, &self.buf);
    // Don't write where clause - constraints will be collected and written at the top level
}

/// Writes the where clause containing static dispatch constraints to the buffer.
/// Formats constraints in one of three styles based on line length:
/// 1. All on same line: "where [a.plus : a -> a, b.minus : b -> b]"
/// 2. All on next line: "\n  where [a.plus : a -> a, b.minus : b -> b]"
/// 3. One per line: "\n  where [\n    a.plus : a -> a,\n    b.minus : b -> b,\n  ]"
fn writeWhereClause(self: *TypeWriter, writer: *ByteWrite, root_var: Var, var_len: usize, format: Format) error{ OutOfMemory, WriteFailed }!void {
    // Ensure we have enough temp storage to collect dispatch constraints
    try self.static_dispatch_constraints_tmp.ensureUnusedCapacity(
        self.static_dispatch_constraints.items.len + 2,
    );

    // Pre-allocate buffer space for constraint strings BEFORE creating the
    // managedWriter, because ensureUnusedCapacity on buf_tmp can reallocate,
    // invalidating the writer's buffer pointer.
    try self.buf_tmp.ensureUnusedCapacity(
        60 + (self.static_dispatch_constraints.items.len - 1) * 30,
    );

    // Iterate over static dispatch constraints, generating their string representations
    // into a tmp buffer. We don't write directly to the main buffer because we need to
    // sort them alphabetically first and decide on formatting.
    //
    // Use a while loop with index instead of for loop over slice, because
    // writeVar may collect additional constraints into this local display list
    // while printing existing ones. This is NOT unification - we're just reading
    // existing constraint data from nested types and gathering them for display.
    // (e.g., `!=` desugars to `is_eq().not()` - when printing the `is_eq` constraint's
    // return type `f`, we find that `f` has a `not` constraint which we also need to display)
    var total_constraint_len: usize = 0;
    {
        // Use a block scope so the defer syncs buf_tmp before the sort/formatting below.
        // While the writer is active, use tmp_aw.writer.end (not self.buf_tmp.items.len)
        // to track positions, since writes go through the writer's internal state.
        var tmp_aw = collections_mod.managedWriter(&self.buf_tmp);
        defer collections_mod.managedWriterFinish(&tmp_aw, &self.buf_tmp);
        var tmp_writer: *ByteWrite = &tmp_aw.writer;

        var i: usize = 0;
        while (i < self.static_dispatch_constraints.items.len) : (i += 1) {
            const item = self.static_dispatch_constraints.items[i];

            const start = tmp_aw.writer.end;
            try self.writeVar(tmp_writer, item.dispatcher_var, root_var);
            const type_name_end = tmp_aw.writer.end;

            try tmp_writer.writeAll(".");
            try tmp_writer.writeAll(self.idents.getText(item.constraint.fn_name));
            try tmp_writer.writeAll(" : ");

            try self.writeVar(tmp_writer, item.constraint.fn_var, root_var);

            const constraint_len = tmp_aw.writer.end - start;
            total_constraint_len += constraint_len;

            try self.static_dispatch_constraints_tmp.append(.{
                .fn_name = item.constraint.fn_name,
                .type_name_start = start,
                .type_name_end = type_name_end,
                .start = start,
                .len = constraint_len,
            });
        }
    }

    // Sort constraints alphabetically by type name first, then by function name
    std.mem.sort(
        StaticDispatchTmp,
        self.static_dispatch_constraints_tmp.items,
        StaticDispatchTmp.SortCtx{
            .buf_tmp = &self.buf_tmp,
            .idents = self.idents,
        },
        StaticDispatchTmp.sort,
    );

    // Calculate line lengths for different formatting options

    // Length of all ", " between constraints
    const separator_len = (self.static_dispatch_constraints.items.len - 1) * 2; // ", " between each

    // Length of all constraints, separators, plus open/closing brackets
    const constraints_len_if_on_same_line = total_constraint_len + separator_len + 2; // extra two the open/closing []

    const line_len_if_all_on_same_line = var_len + 7 + constraints_len_if_on_same_line; // " where " = 7 charts
    const line_len_if_all_on_next_line = 8 + constraints_len_if_on_same_line; // "  where " = 8 chars

    // Choose formatting style based on line length
    if (line_len_if_all_on_same_line <= 80 or format == .one_line) {
        // All constraints fit on the same line as the type
        // Example: MyType where [plus : a, a -> a, minus : a, a -> a]
        try writer.writeAll(" where [");
        for (self.static_dispatch_constraints_tmp.items, 0..) |constraint, j| {
            if (j > 0) try writer.writeAll(", ");
            try writer.writeAll(self.buf_tmp.items[constraint.start..][0..constraint.len]);
        }
        try writer.writeAll("]");
    } else if (line_len_if_all_on_next_line <= 80) {
        // All constraints fit on the next line
        // Example:
        //   where [plus : a, a -> a, minus : a, a -> a]
        try writer.writeAll("\n  where [");
        for (self.static_dispatch_constraints_tmp.items, 0..) |constraint, j| {
            if (j > 0) try writer.writeAll("\n     , ");
            try writer.writeAll(self.buf_tmp.items[constraint.start..][0..constraint.len]);
        }
        try writer.writeAll("]");
    } else {
        // Each constraint on its own line
        // Example:
        //   where [
        //     plus : a, a -> a,
        //     minus : a, a -> a,
        //   ]
        try writer.writeAll("\n  where [\n    ");
        for (self.static_dispatch_constraints_tmp.items, 0..) |constraint, j| {
            if (j > 0) try writer.writeAll(",\n    ");
            try writer.writeAll(self.buf_tmp.items[constraint.start..][0..constraint.len]);
        }
        try writer.writeAll(",\n  ]");
    }
}

/// Convert a var to a type string, driving the walk to completion on the
/// frame stack.
fn writeVarWithContext(self: *TypeWriter, writer: *ByteWrite, var_: Var, context: TypeContext, root_var: Var) error{ OutOfMemory, WriteFailed }!void {
    const frames_base = self.frames.items.len;
    const seen_base = self.seen.items.len;
    const fields_base = self.scratch_record_fields.items.len;
    const tags_base = self.scratch_tags.items.len;
    // A completed walk drains every buffer back to its entry length. A write
    // or allocation failure mid-walk can leave entries behind, so unwind them
    // here and keep the writer's buffers consistent on both exit paths.
    errdefer {
        while (self.seen.items.len > seen_base) self.popSeen();
        self.frames.items.len = frames_base;
        self.scratch_record_fields.shrinkRetainingCapacity(fields_base);
        self.scratch_tags.shrinkRetainingCapacity(tags_base);
    }

    if (!try self.requestVar(writer, var_, context, root_var)) {
        try self.driveFrames(writer, frames_base, root_var);
    }

    std.debug.assert(self.seen.items.len == seen_base);
}

/// Run suspended steps until the frame stack is back down to `frames_base`.
/// A step either suspends after requesting exactly one child (having already
/// written its own resume state), or finishes without requesting anything—so
/// popping on finish always removes the frame the step ran for.
fn driveFrames(self: *TypeWriter, writer: *ByteWrite, frames_base: usize, root_var: Var) error{ OutOfMemory, WriteFailed }!void {
    while (self.frames.items.len > frames_base) {
        const top = &self.frames.items[self.frames.items.len - 1];
        const finished = switch (top.*) {
            .args => |*frame| try self.stepArgs(writer, frame, root_var),
            .func => |*frame| try self.stepFunc(writer, frame, root_var),
            .record => |*frame| try self.stepRecord(writer, frame, root_var),
            .record_unbound => |*frame| try self.stepRecordUnbound(writer, frame, root_var),
            .tag_union => |*frame| try self.stepTagUnion(writer, frame, root_var),
            .tag => |*frame| try self.stepTag(writer, frame, root_var),
        };
        if (finished) {
            self.frames.items.len -= 1;
        }
    }
}

/// Render one var's head: write every byte that precedes its children and
/// either finish it outright (returning true) or push the frame that will
/// render its children (returning false).
fn requestVar(self: *TypeWriter, writer: *ByteWrite, var_: Var, context: TypeContext, root_var: Var) error{ OutOfMemory, WriteFailed }!bool {
    if (@intFromEnum(var_) >= self.types.slots.backing.len()) {
        // Variable is out of bounds - this can happen with corrupted type data
        try writer.writeAll("Error");
        return true;
    }

    const resolved = self.types.resolveVar(var_);

    if (@intFromEnum(resolved.var_) >= self.types.slots.backing.len()) {
        // Variable is out of bounds - this can happen with corrupted type data
        try writer.writeAll("Error");
        return true;
    }

    // Check if resolution returned an error descriptor - bail immediately
    if (resolved.desc.content == .err) {
        try writer.writeAll("Error");
        return true;
    }

    if (self.hasSeenVar(resolved.var_)) {
        try writer.writeAll("<RecursiveType>");
        return true;
    }

    try self.pushSeen(resolved.var_);
    if (try self.writeContentHead(writer, resolved.desc.content, var_, resolved.var_, context, root_var)) {
        return false;
    }
    self.popSeen();
    return true;
}

/// Write `content`'s leading bytes with its var already on `seen`. Returns
/// true when a frame was pushed to render children, false when the content
/// had none and the caller must pop `seen` itself.
fn writeContentHead(
    self: *TypeWriter,
    writer: *ByteWrite,
    content: Content,
    var_: Var,
    resolved_var: Var,
    context: TypeContext,
    root_var: Var,
) error{ OutOfMemory, WriteFailed }!bool {
    switch (content) {
        .flex => |flex| {
            const constraints = self.types.sliceStaticDispatchConstraints(flex.constraints);

            if (flex.name) |ident_idx| {
                try writer.writeAll(self.getIdent(ident_idx));
            } else {
                try self.writeFlexVarName(writer, var_, context, root_var);
            }

            for (constraints) |constraint| {
                try self.appendStaticDispatchConstraint(var_, constraint);
            }
            return false;
        },
        .rigid => |rigid| {
            try writer.writeAll(self.getIdent(rigid.name));

            // Useful in debugging to see if a var is rigid or not
            // _ = try writer.print("[r-{}]", .{var_});

            for (self.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                try self.appendStaticDispatchConstraint(var_, constraint);
            }
            return false;
        },
        .alias => |alias| return try self.startAlias(writer, alias),
        .field_presence => |field_presence| {
            try writer.writeAll(switch (field_presence) {
                .required => "present",
                .optional => "optional",
                .defaulted => "defaulted",
            });
            return false;
        },
        .structure => |flat_type| {
            const should_wrap_in_parens = ((context == .FunctionArgument or context == .FunctionReturn) and (flat_type == .fn_effectful or flat_type == .fn_pure or flat_type == .fn_unbound));
            if (should_wrap_in_parens) {
                try writer.writeAll("(");
            }

            const pushed = try self.startFlatType(writer, flat_type, resolved_var, root_var, should_wrap_in_parens);
            // Only function content ever wraps, and a function always suspends
            // on the frame that closes the paren after its return type.
            std.debug.assert(pushed or !should_wrap_in_parens);
            return pushed;
        },
        .err => {
            try writer.writeAll("Error");
            return false;
        },
    }
}

fn writeVar(self: *TypeWriter, writer: *ByteWrite, var_: Var, root_var: Var) error{ OutOfMemory, WriteFailed }!void {
    try self.writeVarWithContext(writer, var_, .General, root_var);
}

/// Write an alias type's name, and push the frame for its arguments when it
/// has any.
fn startAlias(self: *TypeWriter, writer: *ByteWrite, alias: Alias) error{ OutOfMemory, WriteFailed }!bool {
    try writer.writeAll(self.getDisplayName(alias.ident.ident_idx));
    // An alias stores its backing var as the first element of its span, so
    // its arguments are the span with that element dropped.
    var args = alias.vars.nonempty;
    args.dropFirstElem();
    if (args.len() == 0) return false;
    try writer.writeAll("(");
    try self.frames.append(.{ .args = .{ .vars = args, .context = .General } });
    return true;
}

/// Write a flat type's leading bytes, returning true when a frame was pushed.
fn startFlatType(
    self: *TypeWriter,
    writer: *ByteWrite,
    flat_type: FlatType,
    flat_type_var: Var,
    root_var: Var,
    wrap_in_parens: bool,
) error{ OutOfMemory, WriteFailed }!bool {
    switch (flat_type) {
        .tuple => |tuple| return try self.startTuple(writer, tuple),
        .nominal_type => |nominal_type| return try self.startNominalType(writer, nominal_type),
        .fn_pure => |func| return try self.startFunc(writer, func, " -> ", wrap_in_parens),
        .fn_effectful => |func| return try self.startFunc(writer, func, " => ", wrap_in_parens),
        .fn_unbound => |func| return try self.startFunc(writer, func, " -> ", wrap_in_parens),
        .record => |record| return try self.startRecord(writer, record, flat_type_var, root_var),
        .record_unbound => |fields| return try self.startRecordUnbound(writer, fields, flat_type_var, root_var),
        .empty_record => {
            try writer.writeAll("{}");
            return false;
        },
        .tag_union => |tag_union| return try self.startTagUnion(writer, tag_union, flat_type_var),
        .empty_tag_union => {
            try writer.writeAll("[]");
            return false;
        },
    }
}

/// Write a tuple type's opening paren, and push the frame for its elements.
fn startTuple(self: *TypeWriter, writer: *ByteWrite, tuple: Tuple) error{ OutOfMemory, WriteFailed }!bool {
    try writer.writeAll("(");
    try self.frames.append(.{ .args = .{ .vars = tuple.elems, .context = .TupleFieldContent } });
    return true;
}

/// Write a nominal type's name, and push the frame for its arguments when it
/// has any.
fn startNominalType(self: *TypeWriter, writer: *ByteWrite, nominal_type: NominalType) error{ OutOfMemory, WriteFailed }!bool {
    try writer.writeAll(self.getDisplayName(nominal_type.ident.ident_idx));
    const args = nominal_type.args;
    if (args.len() == 0) return false;
    try writer.writeAll("(");
    try self.frames.append(.{ .args = .{ .vars = args, .context = .General } });
    return true;
}

/// Write a function type's leading bytes and push the frame that renders its
/// arguments, arrow, and return type.
fn startFunc(
    self: *TypeWriter,
    writer: *ByteWrite,
    func: Func,
    arrow: []const u8,
    wrap_in_parens: bool,
) error{ OutOfMemory, WriteFailed }!bool {
    if (func.args.len() == 0) {
        try writer.writeAll("({})");
    }
    try self.frames.append(.{ .func = .{
        .args = func.args,
        .ret = func.ret,
        .arrow = arrow,
        .wrap_in_parens = wrap_in_parens,
    } });
    return true;
}

/// Collect and sort a record row, write its opening bytes, and push the frame
/// that renders its fields and extension. A row that turns out to be closed
/// and empty renders as `{}` with no frame at all.
fn startRecord(
    self: *TypeWriter,
    writer: *ByteWrite,
    record: Record,
    row_var: Var,
    root_var: Var,
) error{ OutOfMemory, WriteFailed }!bool {
    const fields_base: u32 = @intCast(self.scratch_record_fields.items.len);

    const ext = try self.gatherRecordFields(record.fields, record.ext, row_var);
    const gathered_fields = self.scratch_record_fields.items[fields_base..];
    const num_fields = gathered_fields.len;

    std.mem.sort(types_mod.RecordField, gathered_fields, self.idents, comptime types_mod.RecordField.sortByNameAsc);

    var flex_ext_occurrences: usize = 0;
    var unbound_ext_occurrences: usize = 0;

    if (num_fields == 0) {
        const has_ext = switch (ext) {
            .flex => |flex| blk: {
                if (flex.payload.name) |_| {
                    break :blk true;
                } else {
                    flex_ext_occurrences = try self.countVarOccurrences(record.ext, root_var);
                    break :blk flex_ext_occurrences > 1;
                }
            },
            .rigid => true,
            .unbound => |unbound_var| blk: {
                unbound_ext_occurrences = try self.countVarOccurrences(unbound_var, root_var);
                break :blk unbound_ext_occurrences > 1;
            },
            .invalid, .empty_record => false,
        };
        if (!has_ext) {
            self.scratch_record_fields.shrinkRetainingCapacity(fields_base);
            try writer.writeAll("{}");
            return false;
        }
    }

    try writer.writeAll("{ ");
    try self.frames.append(.{ .record = .{
        .fields_base = fields_base,
        .fields_count = @intCast(num_fields),
        .ext = ext,
        .ext_var = record.ext,
        .flex_ext_occurrences = flex_ext_occurrences,
        .unbound_ext_occurrences = unbound_ext_occurrences,
    } });
    return true;
}

/// Write an unbound record's opening bytes and push its frame.
///
/// Note that an unbound record is semantically the same as a record with a
/// `flex` extension var. Because of this, we have to count the occurrences of
/// this unbound  record appearing in this type, to properly display the ext
/// type.
fn startRecordUnbound(
    self: *TypeWriter,
    writer: *ByteWrite,
    fields: RecordField.SafeMultiList.Range,
    record_unbound_var: Var,
    root_var: Var,
) error{ OutOfMemory, WriteFailed }!bool {
    var unbound_ext_occurrences: usize = 0;
    if (record_unbound_var != root_var) {
        unbound_ext_occurrences = try self.countVarOccurrences(record_unbound_var, root_var);
    }

    try writer.writeAll("{ ");
    try self.frames.append(.{ .record_unbound = .{
        .fields = fields,
        .record_unbound_var = record_unbound_var,
        .unbound_ext_occurrences = unbound_ext_occurrences,
    } });
    return true;
}

/// Collect and sort a tag-union row, write its opening bracket, and push the
/// frame that renders its tags and extension.
fn startTagUnion(
    self: *TypeWriter,
    writer: *ByteWrite,
    tag_union: TagUnion,
    row_var: Var,
) error{ OutOfMemory, WriteFailed }!bool {
    // Bounds check the tags range before iterating
    const tags_start_idx = @intFromEnum(tag_union.tags.start);
    const tags_len = self.types.tags.len();
    if (tags_start_idx >= tags_len or tags_start_idx + tag_union.tags.count > tags_len) {
        try writer.writeAll("[Error]");
        return false;
    }

    const tags_base: u32 = @intCast(self.scratch_tags.items.len);

    const ext = try self.gatherTags(tag_union.tags, tag_union.ext, row_var);
    const gathered_tags = self.scratch_tags.items[tags_base..];
    const num_tags = gathered_tags.len;

    std.mem.sort(types_mod.Tag, gathered_tags, self.idents, comptime types_mod.Tag.sortByNameAsc);

    try writer.writeAll("[");
    try self.frames.append(.{ .tag_union = .{
        .tags_base = tags_base,
        .tags_count = @intCast(num_tags),
        .ext = ext,
        .ext_var = tag_union.ext,
    } });
    return true;
}

/// Write a single tag's name, and push the frame for its payload when it has
/// one.
fn requestTag(self: *TypeWriter, writer: *ByteWrite, tag: Tag) error{ OutOfMemory, WriteFailed }!bool {
    try writer.writeAll(self.getIdent(tag.name));
    if (tag.args.len() == 0) return true;
    try writer.writeAll("(");
    try self.frames.append(.{ .tag = .{ .args = tag.args } });
    return false;
}

fn stepArgs(self: *TypeWriter, writer: *ByteWrite, frame: *ArgsFrame, root_var: Var) error{ OutOfMemory, WriteFailed }!bool {
    while (true) {
        if (frame.idx < frame.vars.len()) {
            if (frame.idx > 0) try writer.writeAll(", ");
            const child = self.varAt(frame.vars, frame.idx);
            const context = frame.context;
            frame.idx += 1;
            if (!try self.requestVar(writer, child, context, root_var)) return false;
            continue;
        }
        try writer.writeAll(")");
        self.popSeen();
        return true;
    }
}

fn stepFunc(self: *TypeWriter, writer: *ByteWrite, frame: *FuncFrame, root_var: Var) error{ OutOfMemory, WriteFailed }!bool {
    while (true) {
        switch (frame.stage) {
            .args => {
                if (frame.idx < frame.args.len()) {
                    if (frame.idx > 0) try writer.writeAll(", ");
                    const arg = self.varAt(frame.args, frame.idx);
                    frame.idx += 1;
                    if (!try self.requestVar(writer, arg, .FunctionArgument, root_var)) return false;
                    continue;
                }
                try writer.writeAll(frame.arrow);
                frame.stage = .ret;
            },
            .ret => {
                const ret = frame.ret;
                frame.stage = .done;
                if (!try self.requestVar(writer, ret, .FunctionReturn, root_var)) return false;
            },
            .done => {
                if (frame.wrap_in_parens) try writer.writeAll(")");
                self.popSeen();
                return true;
            },
        }
    }
}

fn writeRecordFieldSeparator(self: *TypeWriter, writer: *ByteWrite, presence: RecordField.Presence) error{WriteFailed}!void {
    try writer.writeAll(switch (presence.decode()) {
        .required => ": ",
        .unknown => |unknown| switch (self.types.resolveVar(unknown.presence).desc.content) {
            .field_presence => |resolved| switch (resolved) {
                .required, .defaulted => ": ",
                .optional => " ?: ",
            },
            .flex, .rigid, .alias, .structure, .err => ": ",
        },
    });
}

fn writeFieldDefaultSuffix(self: *TypeWriter, writer: *ByteWrite, presence: RecordField.Presence) error{WriteFailed}!void {
    const unknown = switch (presence.decode()) {
        .required => return,
        .unknown => |value| value,
    };
    const id = switch (self.types.resolveVar(unknown.presence).desc.content) {
        .field_presence => |resolved| switch (resolved) {
            .defaulted => |id| id,
            .required, .optional => return,
        },
        .flex, .rigid, .alias, .structure, .err => return,
    };
    try writer.writeAll(" ?? ");
    if (self.mb_default_source) |resolve| {
        if (resolve(self.default_source_ctx, id)) |snippet| {
            try writer.writeAll(snippet);
            return;
        }
    }
    try writer.writeAll("…");
}

fn stepRecord(self: *TypeWriter, writer: *ByteWrite, frame: *RecordFrame, root_var: Var) error{ OutOfMemory, WriteFailed }!bool {
    while (true) {
        switch (frame.stage) {
            .fields => {
                if (frame.idx < frame.fields_count) {
                    if (frame.idx > 0) try writer.writeAll(", ");
                    // Re-read from the scratch list every iteration: rendering
                    // a field's type can append to (and reallocate)
                    // scratch_record_fields, which invalidates any slice held
                    // across iterations.
                    const field = self.scratch_record_fields.items[frame.fields_base + frame.idx];
                    try writer.writeAll(self.getIdent(field.name));
                    try self.writeRecordFieldSeparator(writer, field.presence);
                    frame.stage = .after_field;
                    if (!try self.requestVar(writer, field.presence.typeVar(), .RecordFieldContent, root_var)) return false;
                    continue;
                }
                self.scratch_record_fields.shrinkRetainingCapacity(frame.fields_base);
                frame.stage = .ext;
            },
            .after_field => {
                const field = self.scratch_record_fields.items[frame.fields_base + frame.idx];
                try self.writeFieldDefaultSuffix(writer, field.presence);
                frame.idx += 1;
                frame.stage = .fields;
            },
            .ext => {
                switch (frame.ext) {
                    .flex => |flex| {
                        if (frame.fields_count > 0) try writer.writeAll(", ");
                        try writer.writeAll("..");

                        if (flex.payload.name) |ident_idx| {
                            const name = self.getIdent(ident_idx);
                            // Suppress internal names (e.g. #open_ext_0 from anonymous `..`)
                            if (name.len > 0 and name[0] != '#') {
                                try writer.writeAll(name);
                            }
                        } else {
                            if (frame.flex_ext_occurrences > 1) {
                                try self.writeFlexVarName(writer, flex.var_, .RecordExtension, root_var);
                            }
                        }

                        // Since don't recurse above, we must capture the static dispatch
                        // constraints directly
                        for (self.types.sliceStaticDispatchConstraints(flex.payload.constraints)) |constraint| {
                            try self.appendStaticDispatchConstraint(flex.var_, constraint);
                        }
                    },
                    .rigid => |rigid| {
                        if (frame.fields_count > 0) try writer.writeAll(", ");
                        try writer.writeAll("..");
                        const name = self.getIdent(rigid.name);
                        // Suppress internal names (e.g. #open_ext_0 from anonymous `..`)
                        if (name.len == 0 or name[0] != '#') {
                            try writer.writeAll(name);
                        }

                        // Since don't recurse above, we must capture the static dispatch
                        // constraints directly
                        for (self.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                            try self.appendStaticDispatchConstraint(frame.ext_var, constraint);
                        }
                    },
                    .unbound => |unbound_var| {
                        if (frame.fields_count > 0) try writer.writeAll(", ");
                        try writer.writeAll("..");

                        if (frame.unbound_ext_occurrences > 1) {
                            try self.writeFlexVarName(writer, unbound_var, .RecordExtension, root_var);
                        }
                    },
                    .invalid, .empty_record => {},
                }

                try writer.writeAll(" }");
                self.popSeen();
                return true;
            },
        }
    }
}

fn stepRecordUnbound(self: *TypeWriter, writer: *ByteWrite, frame: *RecordUnboundFrame, root_var: Var) error{ OutOfMemory, WriteFailed }!bool {
    const fields_slice = self.types.getRecordFieldsSlice(frame.fields);
    const num_fields = fields_slice.len;
    while (true) {
        switch (frame.stage) {
            .fields => {
                if (frame.idx < num_fields) {
                    if (frame.idx > 0) try writer.writeAll(", ");
                    const name = fields_slice.items(.name)[frame.idx];
                    const presence = fields_slice.items(.presence)[frame.idx];
                    try writer.writeAll(self.getIdent(name));
                    try self.writeRecordFieldSeparator(writer, presence);
                    frame.stage = .after_field;
                    if (!try self.requestVar(writer, presence.typeVar(), .RecordFieldContent, root_var)) return false;
                    continue;
                }
                frame.stage = .ext;
            },
            .after_field => {
                const presence = fields_slice.items(.presence)[frame.idx];
                try self.writeFieldDefaultSuffix(writer, presence);
                frame.idx += 1;
                frame.stage = .fields;
            },
            .ext => {
                if (num_fields > 0) try writer.writeAll(", ");
                try writer.writeAll("..");
                if (frame.unbound_ext_occurrences > 1) {
                    try self.writeFlexVarName(writer, frame.record_unbound_var, .RecordExtension, root_var);
                }
                try writer.writeAll(" }");
                self.popSeen();
                return true;
            },
        }
    }
}

fn stepTagUnion(self: *TypeWriter, writer: *ByteWrite, frame: *TagUnionFrame, root_var: Var) error{ OutOfMemory, WriteFailed }!bool {
    while (true) {
        switch (frame.stage) {
            .tags => {
                if (frame.idx < frame.tags_count) {
                    if (frame.idx > 0) try writer.writeAll(", ");
                    // Re-read from the scratch list every iteration: rendering
                    // a tag's payload can append to (and reallocate)
                    // scratch_tags, which invalidates any slice held across
                    // iterations.
                    const tag = self.scratch_tags.items[frame.tags_base + frame.idx];
                    frame.idx += 1;
                    if (!try self.requestTag(writer, tag)) return false;
                    continue;
                }
                self.scratch_tags.shrinkRetainingCapacity(frame.tags_base);
                frame.stage = .ext;
            },
            .ext => {
                frame.stage = .done;
                switch (frame.ext) {
                    .flex => |flex| {
                        if (frame.tags_count > 0) try writer.writeAll(", ");
                        try writer.writeAll("..");

                        if (flex.payload.name) |ident_idx| {
                            const name = self.getIdent(ident_idx);
                            // Suppress internal names (e.g. #open_ext_0 from anonymous `..`)
                            if (name.len > 0 and name[0] != '#') {
                                try writer.writeAll(name);
                            }
                        } else if (true) {
                            // TODO: ^ here, we should consider polarity
                            const occurrences = try self.countVarOccurrences(flex.var_, root_var);
                            if (occurrences > 1) {
                                try self.writeFlexVarName(writer, flex.var_, .TagUnionExtension, root_var);
                            }
                        }

                        for (self.types.sliceStaticDispatchConstraints(flex.payload.constraints)) |constraint| {
                            try self.appendStaticDispatchConstraint(flex.var_, constraint);
                        }
                    },
                    .rigid => |rigid| {
                        if (frame.tags_count > 0) try writer.writeAll(", ");
                        try writer.writeAll("..");
                        const name = self.getIdent(rigid.name);
                        // Suppress internal names (e.g. #open_ext_0 from anonymous `..`)
                        if (name.len == 0 or name[0] != '#') {
                            try writer.writeAll(name);
                        }

                        for (self.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                            try self.appendStaticDispatchConstraint(frame.ext_var, constraint);
                        }
                    },
                    .empty_tag_union, .err, .invalid => {},
                    .alias => |alias_var| {
                        if (frame.tags_count > 0) try writer.writeAll(", ");
                        try writer.writeAll("..");
                        if (!try self.requestVar(writer, alias_var, .TagUnionExtension, root_var)) return false;
                    },
                }
            },
            .done => {
                try writer.writeAll("]");
                self.popSeen();
                return true;
            },
        }
    }
}

fn stepTag(self: *TypeWriter, writer: *ByteWrite, frame: *TagFrame, root_var: Var) error{ OutOfMemory, WriteFailed }!bool {
    while (true) {
        if (frame.idx < frame.args.len()) {
            if (frame.idx > 0) try writer.writeAll(", ");
            const arg = self.varAt(frame.args, frame.idx);
            frame.idx += 1;
            if (!try self.requestVar(writer, arg, .General, root_var)) return false;
            continue;
        }
        try writer.writeAll(")");
        return true;
    }
}

/// Recursively unwrap all record fields
fn gatherRecordFields(
    self: *TypeWriter,
    fields: RecordField.SafeMultiList.Range,
    initial_ext: Var,
    row_var: Var,
) std.mem.Allocator.Error!RecordExt {
    const slice = self.types.getRecordFieldsSlice(fields);
    try self.scratch_record_fields.ensureUnusedCapacity(fields.len());
    for (slice.items(.name), slice.items(.presence)) |name, presence| {
        self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .presence = presence });
    }

    var ext = initial_ext;
    self.ext_seen.clearRetainingCapacity();
    // The row whose fields were just collected counts as reached, so a chain
    // that loops back through the starting row stops there instead of
    // collecting that row's own fields a second time.
    try self.ext_seen.put(row_var, {});
    while (true) {
        const resolved = self.types.resolveVar(ext);
        // An extension chain that returns to a row it already collected has
        // contributed every field it can, and would otherwise contribute them
        // forever. Terminating on the revisit is what bounds this collection
        // for a cyclic row; a row that terminates on its own never revisits.
        if ((try self.ext_seen.getOrPut(resolved.var_)).found_existing) return .invalid;
        switch (resolved.desc.content) {
            .flex => |flex| {
                return .{ .flex = .{ .var_ = resolved.var_, .payload = flex } };
            },
            .rigid => |rigid| {
                return .{ .rigid = rigid };
            },
            .alias => |alias| {
                ext = self.types.getAliasBackingVar(alias);
            },
            .structure => |flat_type| {
                switch (flat_type) {
                    .record => |ext_record| {
                        const ext_slice = self.types.getRecordFieldsSlice(ext_record.fields);
                        try self.scratch_record_fields.ensureUnusedCapacity(ext_record.fields.len());
                        for (ext_slice.items(.name), ext_slice.items(.presence)) |name, presence| {
                            self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .presence = presence });
                        }
                        ext = ext_record.ext;
                    },
                    .record_unbound => |ext_fields| {
                        const ext_slice = self.types.getRecordFieldsSlice(ext_fields);
                        try self.scratch_record_fields.ensureUnusedCapacity(ext_fields.len());
                        for (ext_slice.items(.name), ext_slice.items(.presence)) |name, presence| {
                            self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .presence = presence });
                        }
                        return .{ .unbound = resolved.var_ };
                    },
                    .empty_record => return .empty_record,
                    .tuple,
                    .nominal_type,
                    .fn_pure,
                    .fn_effectful,
                    .fn_unbound,
                    .tag_union,
                    .empty_tag_union,
                    => return .invalid,
                }
            },
            .field_presence => return .invalid,
            .err => return .invalid,
        }
    }
}

/// Recursively unwrap all tag union tags, following ext var chains
fn gatherTags(
    self: *TypeWriter,
    tags: Tag.SafeMultiList.Range,
    initial_ext: Var,
    row_var: Var,
) std.mem.Allocator.Error!TagUnionExt {
    const slice = self.types.getTagsSlice(tags);
    try self.scratch_tags.ensureUnusedCapacity(tags.len());
    for (slice.items(.name), slice.items(.args)) |name, args| {
        self.scratch_tags.appendAssumeCapacity(.{ .name = name, .args = args });
    }

    var ext = initial_ext;
    self.ext_seen.clearRetainingCapacity();
    // The row whose tags were just collected counts as reached, so a chain
    // that loops back through the starting row stops there instead of
    // collecting that row's own tags a second time.
    try self.ext_seen.put(row_var, {});
    while (true) {
        const resolved = self.types.resolveVar(ext);
        // An extension chain that returns to a row it already collected has
        // contributed every tag it can, and would otherwise contribute them
        // forever. Terminating on the revisit is what bounds this collection
        // for a cyclic row; a row that terminates on its own never revisits.
        if ((try self.ext_seen.getOrPut(resolved.var_)).found_existing) return .invalid;
        switch (resolved.desc.content) {
            .flex => |flex| {
                return .{ .flex = .{ .var_ = resolved.var_, .payload = flex } };
            },
            .rigid => |rigid| {
                return .{ .rigid = rigid };
            },
            .alias => |alias| {
                ext = self.types.getAliasBackingVar(alias);
            },
            .structure => |flat_type| {
                switch (flat_type) {
                    .tag_union => |ext_tu| {
                        const ext_slice = self.types.getTagsSlice(ext_tu.tags);
                        try self.scratch_tags.ensureUnusedCapacity(ext_tu.tags.len());
                        for (ext_slice.items(.name), ext_slice.items(.args)) |name, args| {
                            self.scratch_tags.appendAssumeCapacity(.{ .name = name, .args = args });
                        }
                        ext = ext_tu.ext;
                    },
                    .empty_tag_union => return .empty_tag_union,
                    .record,
                    .record_unbound,
                    .tuple,
                    .nominal_type,
                    .fn_pure,
                    .fn_effectful,
                    .fn_unbound,
                    .empty_record,
                    => return .invalid,
                }
            },
            .field_presence => return .invalid,
            .err => return .err,
        }
    }
}

/// Write a single tag, driving the walk to completion on the frame stack.
fn writeTag(self: *TypeWriter, writer: *ByteWrite, tag: Tag, root_var: Var) error{ OutOfMemory, WriteFailed }!void {
    const frames_base = self.frames.items.len;
    const seen_base = self.seen.items.len;
    const fields_base = self.scratch_record_fields.items.len;
    const tags_base = self.scratch_tags.items.len;
    errdefer {
        while (self.seen.items.len > seen_base) self.popSeen();
        self.frames.items.len = frames_base;
        self.scratch_record_fields.shrinkRetainingCapacity(fields_base);
        self.scratch_tags.shrinkRetainingCapacity(tags_base);
    }

    if (!try self.requestTag(writer, tag)) {
        try self.driveFrames(writer, frames_base, root_var);
    }

    std.debug.assert(self.seen.items.len == seen_base);
}

/// Format a single tag and return the result as a string slice.
/// The returned slice is only valid until the next call to any write method.
pub fn writeTagGet(self: *TypeWriter, tag: Tag, root_var: Var) error{ OutOfMemory, WriteFailed }![]const u8 {
    self.reset();
    var aw = collections_mod.managedWriter(&self.buf);
    try self.writeTag(&aw.writer, tag, root_var);
    collections_mod.managedWriterFinish(&aw, &self.buf);
    return self.get();
}

/// Append a constraint with its dispatcher var to the list, if it doesn't already exist
fn appendStaticDispatchConstraint(self: *TypeWriter, dispatcher_var: Var, constraint_to_add: types_mod.StaticDispatchConstraint) error{ OutOfMemory, WriteFailed }!void {
    for (self.static_dispatch_constraints.items) |item| {
        if (item.constraint.fn_name == constraint_to_add.fn_name and item.constraint.fn_var == constraint_to_add.fn_var) {
            return;
        }
    }
    try self.static_dispatch_constraints.append(.{
        .dispatcher_var = dispatcher_var,
        .constraint = constraint_to_add,
    });
}

/// Generate a name for a flex var that may appear multiple times in the type
pub fn writeFlexVarName(self: *TypeWriter, writer: *ByteWrite, var_: Var, context: TypeContext, root_var: Var) error{ OutOfMemory, WriteFailed }!void {
    const resolved_var = self.types.resolveVar(var_).var_;

    // If resolved var is out of bounds, it's corrupted - just write a simple name
    if (@intFromEnum(resolved_var) >= self.types.slots.backing.len()) {
        try writer.writeAll("_");
        try self.generateContextualName(writer, context);
        return;
    }
    // Check if we've seen this flex var before.
    if (self.flex_var_names_map.get(resolved_var)) |range| {
        // If so, then use that name
        try writer.writeAll(
            self.flex_var_names.items[range.start..range.end],
        );
    } else {
        // Check if this variable appears multiple times
        // Note: counting can fail with corrupted data, so we treat it as appearing once
        const occurrences = try self.countVarOccurrences(resolved_var, root_var);
        if (occurrences <= 1) {
            // If it appears once, then generate and write the name
            try writer.writeAll("_");
            try self.generateContextualName(writer, context);
        } else {
            // If it appears more than once, then we have to track the name we
            // assign it so it appears consistently across the type str

            // Generate a new general var name directly to the output writer.
            // We do not use the context here because that may be the current
            // context the var appears in, but the var may later appear in a
            // different context
            const name_start = self.flex_var_names.items.len;
            var flex_aw = collections_mod.managedWriter(&self.flex_var_names);
            try self.generateContextualName(&flex_aw.writer, .General);
            collections_mod.managedWriterFinish(&flex_aw, &self.flex_var_names);
            const name_end = self.flex_var_names.items.len;

            const contextual_name = self.flex_var_names.items[name_start..name_end];

            // Write the name to the output
            try writer.writeAll(contextual_name);

            // Record the name range for this var
            try self.flex_var_names_map.put(resolved_var, .{ .start = name_start, .end = name_end });
        }
    }
}

/// Count how many times a variable appears in a type, driving the count on
/// the frame stack rather than the native one.
///
/// The seen set is a PATH set, not a global visited set: a var is recorded
/// while its own subtree is being counted and released when that subtree
/// finishes, so a node reachable by several distinct paths is counted once
/// per path. That is what makes the count an occurrence count rather than a
/// node count, and it is what the naming decisions here read.
fn countVarOccurrences(self: *TypeWriter, search_var: Var, root_var: Var) std.mem.Allocator.Error!usize {
    self.seen_count_var_occurrences.clearRetainingCapacity();
    self.count_frames.clearRetainingCapacity();
    self.count_pending.clearRetainingCapacity();
    clearMapIfUsed(Var, void, &self.seen_count_set);

    // An aborted count leaves its own sets behind; draining them here keeps
    // the entry above free for every later naming decision.
    errdefer {
        self.seen_count_var_occurrences.clearRetainingCapacity();
        self.count_frames.clearRetainingCapacity();
        self.count_pending.clearRetainingCapacity();
        clearMapIfUsed(Var, void, &self.seen_count_set);
    }

    var count: usize = 0;
    if (!try self.countRequest(search_var, root_var, &count)) {
        while (self.count_frames.items.len > 0) {
            const frame = &self.count_frames.items[self.count_frames.items.len - 1];
            if (frame.idx < frame.count) {
                const child = self.count_pending.items[frame.base + frame.idx];
                frame.idx += 1;
                _ = try self.countRequest(search_var, child, &count);
                continue;
            }
            self.count_pending.shrinkRetainingCapacity(frame.base);
            self.popSeenCount();
            self.count_frames.items.len -= 1;
        }
    }
    return count;
}

/// Visit one var of the occurrence count: tally it, then either finish it
/// outright (returning true) or push the frame that will visit its children
/// (returning false).
fn countRequest(self: *TypeWriter, search_var: Var, current_var: Var, count: *usize) std.mem.Allocator.Error!bool {
    if (@intFromEnum(current_var) >= self.types.slots.backing.len()) return true;

    const resolved = self.types.resolveVar(current_var);

    // If resolution returned an error descriptor, stop traversing
    if (resolved.desc.content == .err) {
        return true;
    }

    // Count if this is the search var

    // First, check if this is the var we are counting
    if (resolved.var_ == search_var) {
        count.* += 1;
    }

    // Check if we've already seen this var on the path we are on
    // This avoids infinite recursion
    if (self.seen_count_set.contains(resolved.var_)) return true;

    // Record that we've seen this var
    try self.pushSeenCount(resolved.var_);

    const children_base: u32 = @intCast(self.count_pending.items.len);
    try self.collectCountChildren(resolved.desc.content);
    const child_count = self.count_pending.items.len - children_base;
    if (child_count == 0) {
        self.popSeenCount();
        return true;
    }
    try self.count_frames.append(.{ .base = children_base, .count = @intCast(child_count) });
    return false;
}

fn pushSeenCount(self: *TypeWriter, var_: Var) std.mem.Allocator.Error!void {
    try self.seen_count_var_occurrences.ensureUnusedCapacity(1);
    try self.seen_count_set.put(var_, {});
    self.seen_count_var_occurrences.appendAssumeCapacity(var_);
}

fn popSeenCount(self: *TypeWriter) void {
    const var_ = self.seen_count_var_occurrences.pop().?;
    _ = self.seen_count_set.remove(var_);
}

/// Append every child the occurrence count visits from `content`, in the
/// order the count visits them, onto the pending run.
fn collectCountChildren(self: *TypeWriter, content: Content) std.mem.Allocator.Error!void {
    switch (content) {
        .flex => |flex| {
            const constraints = self.types.sliceStaticDispatchConstraints(flex.constraints);
            for (constraints) |constraint| {
                try self.count_pending.append(constraint.fn_var);
            }
        },
        .rigid => |rigid| {
            const constraints = self.types.sliceStaticDispatchConstraints(rigid.constraints);
            for (constraints) |constraint| {
                try self.count_pending.append(constraint.fn_var);
            }
        },
        .alias => |alias| {
            // For aliases, we only count occurrences in the type arguments
            var args_iter = self.types.iterAliasArgs(alias);
            while (args_iter.next()) |arg_var| {
                try self.count_pending.append(arg_var);
            }
        },
        .structure => |flat_type| {
            try self.collectCountChildrenInFlatType(flat_type);
        },
        .field_presence => {},
        .err => {},
    }
}

fn collectCountChildrenInFlatType(self: *TypeWriter, flat_type: FlatType) std.mem.Allocator.Error!void {
    switch (flat_type) {
        .empty_record, .empty_tag_union => {},
        .tuple => |tuple| {
            try self.count_pending.appendSlice(self.types.sliceVars(tuple.elems));
        },
        .nominal_type => |nominal_type| {
            var args_iter = self.types.iterNominalArgs(nominal_type);
            while (args_iter.next()) |arg_var| {
                try self.count_pending.append(arg_var);
            }
        },
        .fn_pure, .fn_effectful, .fn_unbound => |func| {
            try self.count_pending.appendSlice(self.types.sliceVars(func.args));
            try self.count_pending.append(func.ret);
        },
        .record => |record| {
            const fields = self.types.getRecordFieldsSlice(record.fields);
            for (fields.items(.presence)) |presence| {
                try self.count_pending.append(presence.typeVar());
                if (presence.presenceVar()) |presence_var| try self.count_pending.append(presence_var);
            }
            try self.count_pending.append(record.ext);
        },
        .record_unbound => |fields| {
            const fields_slice = self.types.getRecordFieldsSlice(fields);
            for (fields_slice.items(.presence)) |presence| {
                try self.count_pending.append(presence.typeVar());
                if (presence.presenceVar()) |presence_var| try self.count_pending.append(presence_var);
            }
        },
        .tag_union => |tag_union| {
            // Bounds check the tags range before iterating
            const tags_start_idx = @intFromEnum(tag_union.tags.start);
            const tags_len = self.types.tags.len();
            if (tags_start_idx >= tags_len or tags_start_idx + tag_union.tags.count > tags_len) {
                // Tags range is out of bounds - skip counting in corrupted data
                return;
            }

            var iter = tag_union.tags.iterIndices();
            while (iter.next()) |tag_idx| {
                const tag = self.types.tags.get(tag_idx);
                try self.count_pending.appendSlice(self.types.sliceVars(tag.args));
            }
            try self.count_pending.append(tag_union.ext);
        },
    }
}

/// Retrieves the text representation of an identifier by its index.
/// This is used when formatting types that reference named identifiers.
pub fn getIdent(self: *const TypeWriter, idx: Ident.Idx) []const u8 {
    return self.idents.getText(idx);
}

/// Gets the display name for a type identifier, accounting for import mappings.
/// If the identifier is in the import_mapping, returns the mapped name.
/// Otherwise, returns the original identifier text.
fn getDisplayName(self: *const TypeWriter, idx: Ident.Idx) []const u8 {
    if (self.import_mapping) |mapping| {
        if (mapping.get(idx)) |display_idx| {
            return self.idents.getText(display_idx);
        }
    }

    return stripBuiltinQualification(self.idents.getText(idx));
}

/// Strip the implementation-detail `Builtin.` / `Num.` qualification from a
/// type name for user-facing display: "Builtin.Try" -> "Try",
/// "Builtin.Num.Dec" -> "Dec", "Num.U8" -> "U8".
pub fn stripBuiltinQualification(name: []const u8) []const u8 {
    if (std.mem.startsWith(u8, name, "Builtin.")) {
        const without_builtin = name[8..];
        if (std.mem.startsWith(u8, without_builtin, "Num.")) {
            return without_builtin[4..];
        }
        return without_builtin;
    }
    if (std.mem.startsWith(u8, name, "Num.")) {
        return name[4..];
    }
    return name;
}

fn generateContextualName(self: *TypeWriter, writer: *ByteWrite, context: TypeContext) error{ OutOfMemory, WriteFailed }!void {
    const base_name = switch (context) {
        .RecordExtension => "others",
        .TagUnionExtension => "others",
        .RecordFieldContent => "field",
        .TupleFieldContent => "field",
        .FunctionArgument => "arg",
        .FunctionReturn => "ret",
        .General => {
            // Fall back to generic name generation
            try self.generateNextName(writer);
            return;
        },
    };

    // Try to generate a name with increasing counters until we find one that doesn't collide
    var counter = self.name_counters.get(context) orelse 0;
    var found = false;

    // We need at most as many attempts as there are existing identifiers
    const max_attempts = self.idents.interner.entry_count;
    var attempts: usize = 0;
    while (!found and attempts < max_attempts) : (attempts += 1) {
        const candidate_name = if (counter == 0)
            base_name
        else blk: {
            self.name_tmp.clearRetainingCapacity();
            const len = std.fmt.count("{s}{}", .{ base_name, counter + 1 });
            try self.name_tmp.resize(len);
            _ = std.fmt.bufPrint(self.name_tmp.items, "{s}{}", .{ base_name, counter + 1 }) catch unreachable;
            break :blk self.name_tmp.items;
        };

        // Check if this name already exists in the identifier store
        const exists = self.idents.interner.contains(candidate_name);

        if (!exists) {
            // This name is available, write it to the buffer
            try writer.writeAll(candidate_name);
            found = true;
        } else {
            // Try next counter
            counter += 1;
        }
    }

    // If we couldn't find a unique contextual name, fall back to generic names
    if (!found) {
        try self.generateNextName(writer);
        return;
    }

    self.name_counters.put(context, counter + 1);
}

fn generateNextName(self: *TypeWriter, writer: *ByteWrite) error{ OutOfMemory, WriteFailed }!void {
    // Generate name: a, b, ..., z, aa, ab, ..., az, ba, ...
    // Skip any names that already exist in the identifier store
    // We need at most one more name than the number of existing identifiers
    const max_attempts = self.idents.interner.entry_count + 1;
    var attempts: usize = 0;
    while (attempts < max_attempts) : (attempts += 1) {
        var n = self.next_name_index;
        self.next_name_index += 1;

        self.name_tmp.clearRetainingCapacity();

        // Generate name in base-26: a, b, ..., z, aa, ab, ..., az, ba, ...
        while (true) {
            try self.name_tmp.append(@intCast('a' + (n % 26)));
            n = n / 26;
            if (n == 0) break;
            n -= 1;
        }

        // Names are generated in reverse order, so reverse the buffer
        std.mem.reverse(u8, self.name_tmp.items);

        // Check if this name already exists in the identifier store
        const candidate_name = self.name_tmp.items;
        const exists = self.idents.interner.contains(candidate_name);

        if (!exists) {
            // This name is available, use it
            try writer.writeAll(candidate_name);
            break;
        }
        // Name already exists, try the next one
    }

    // This should never happen in practice, but let's handle it gracefully
    if (attempts >= max_attempts) {
        try writer.writeAll("var");
        try writer.print("{}", .{self.next_name_index});
    }
}

const testing = std.testing;

/// A hand-built type store plus the writer that renders it.
const TestEnv = struct {
    gpa: std.mem.Allocator,
    idents: Ident.Store,
    types: TypesStore,
    writer: TypeWriter,

    fn init(gpa: std.mem.Allocator, root_capacity: usize) std.mem.Allocator.Error!*TestEnv {
        const self = try gpa.create(TestEnv);
        self.* = .{
            .gpa = gpa,
            .idents = try Ident.Store.initCapacity(gpa, 16),
            .types = try TypesStore.initCapacity(gpa, root_capacity, 16),
            .writer = undefined,
        };
        self.writer = try TypeWriter.initFromParts(gpa, &self.types, &self.idents, null);
        return self;
    }

    fn deinit(self: *TestEnv) void {
        const gpa = self.gpa;
        self.writer.deinit();
        self.types.deinit();
        self.idents.deinit(gpa);
        gpa.destroy(self);
    }

    fn ident(self: *TestEnv, text: []const u8) std.mem.Allocator.Error!Ident.Idx {
        return try self.idents.insert(self.gpa, Ident.for_text(text));
    }

    fn tuple(self: *TestEnv, elems: []const Var) std.mem.Allocator.Error!Var {
        const range = try self.types.appendVars(elems);
        return try self.types.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = range } } });
    }

    fn record(self: *TestEnv, fields: []const RecordField, ext: Var) std.mem.Allocator.Error!Var {
        const range = try self.types.appendRecordFields(fields);
        return try self.types.freshFromContent(.{ .structure = .{ .record = .{ .fields = range, .ext = ext } } });
    }

    fn expectRender(self: *TestEnv, var_: Var, expected: []const u8) TestEnvError!void {
        try testing.expectEqualStrings(expected, try self.writer.writeGet(var_, .wrap));
    }
};

const TestEnvError = Allocator.Error || error{ WriteFailed, TestExpectedEqual };

fn testRecordFields(
    gpa: Allocator,
    store: *TypesStore,
    idents: *Ident.Store,
) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
    const required_name = try idents.insert(gpa, Ident.for_text("a_required"));
    const optional_name = try idents.insert(gpa, Ident.for_text("b_optional"));
    const value_var = try store.freshFromContent(.{ .structure = .empty_record });
    const optional_presence = try store.freshFromContent(.{ .field_presence = .optional });
    return store.appendRecordFields(&.{
        .{ .name = required_name, .presence = .required(value_var) },
        .{ .name = optional_name, .presence = .unknown(optional_presence, value_var) },
    });
}

test "TypeWriter renders required and optional fields in closed records" {
    const gpa = std.testing.allocator;
    var store = try TypesStore.initCapacity(gpa, 8, 4);
    defer store.deinit();
    var idents = try Ident.Store.initCapacity(gpa, 4);
    defer idents.deinit(gpa);

    const fields = try testRecordFields(gpa, &store, &idents);
    const ext = try store.freshFromContent(.{ .structure = .empty_record });
    const record_var = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = fields,
        .ext = ext,
    } } });

    var type_writer = try TypeWriter.initFromParts(gpa, &store, &idents, null);
    defer type_writer.deinit();
    try std.testing.expectEqualStrings(
        "{ a_required: {}, b_optional ?: {} }",
        try type_writer.writeGet(record_var, .one_line),
    );
}

test "TypeWriter renders required and optional fields in unbound records" {
    const gpa = std.testing.allocator;
    var store = try TypesStore.initCapacity(gpa, 8, 4);
    defer store.deinit();
    var idents = try Ident.Store.initCapacity(gpa, 4);
    defer idents.deinit(gpa);

    const fields = try testRecordFields(gpa, &store, &idents);
    const record_var = try store.freshFromContent(.{ .structure = .{ .record_unbound = fields } });

    var type_writer = try TypeWriter.initFromParts(gpa, &store, &idents, null);
    defer type_writer.deinit();
    try std.testing.expectEqualStrings(
        "{ a_required: {}, b_optional ?: {}, .. }",
        try type_writer.writeGet(record_var, .one_line),
    );
}

test "TypeWriter renders every shape the walk descends through" {
    var env = try TestEnv.init(testing.allocator, 128);
    defer env.deinit();

    const empty_record = try env.types.freshFromContent(.{ .structure = .empty_record });
    const empty_tag_union = try env.types.freshFromContent(.{ .structure = .empty_tag_union });

    try env.expectRender(empty_record, "{}");
    try env.expectRender(empty_tag_union, "[]");
    try env.expectRender(try env.types.freshFromContent(.err), "Error");

    // Tuples keep their element order; nesting parenthesises.
    const pair = try env.tuple(&.{ empty_record, empty_tag_union });
    try env.expectRender(pair, "({}, [])");
    try env.expectRender(try env.tuple(&.{try env.tuple(&.{pair})}), "((({}, [])))");

    // Records sort their fields by name and flatten their extension chain.
    const b_field = try env.ident("b");
    const a_field = try env.ident("a");
    const closed = try env.record(&.{
        .{ .name = b_field, .presence = .required(empty_tag_union) },
        .{ .name = a_field, .presence = .required(empty_record) },
    }, empty_record);
    try env.expectRender(closed, "{ a: {}, b: [] }");

    const tail = try env.record(&.{.{ .name = a_field, .presence = .required(empty_record) }}, empty_record);
    const chained = try env.record(&.{.{ .name = b_field, .presence = .required(empty_tag_union) }}, tail);
    try env.expectRender(chained, "{ a: {}, b: [] }");

    const rigid_name = try env.ident("row");
    const rigid_ext = try env.types.freshFromContent(.{ .rigid = types_mod.Rigid.init(rigid_name) });
    const open = try env.record(&.{.{ .name = a_field, .presence = .required(empty_record) }}, rigid_ext);
    try env.expectRender(open, "{ a: {}, ..row }");

    const unbound_fields = try env.types.appendRecordFields(&.{.{ .name = a_field, .presence = .required(empty_record) }});
    const unbound = try env.types.freshFromContent(.{ .structure = .{ .record_unbound = unbound_fields } });
    try env.expectRender(unbound, "{ a: {}, .. }");

    // Tag unions sort their tags and parenthesise payloads.
    const tag_z = try env.ident("Z");
    const tag_a = try env.ident("A");
    const tags = try env.types.appendTags(&.{
        .{ .name = tag_z, .args = try env.types.appendVars(&.{ empty_record, empty_tag_union }) },
        .{ .name = tag_a, .args = try env.types.appendVars(&.{}) },
    });
    const tag_union = try env.types.freshFromContent(.{ .structure = .{ .tag_union = .{ .tags = tags, .ext = empty_tag_union } } });
    try env.expectRender(tag_union, "[A, Z({}, [])]");

    // Functions wrap in parens exactly in argument and return position.
    const inner_fn = try env.types.freshFromContent(try env.types.mkFuncPure(&.{empty_record}, empty_tag_union));
    try env.expectRender(inner_fn, "{} -> []");
    try env.expectRender(
        try env.types.freshFromContent(try env.types.mkFuncPure(&.{inner_fn}, inner_fn)),
        "({} -> []) -> ({} -> [])",
    );
    try env.expectRender(
        try env.types.freshFromContent(try env.types.mkFuncEffectful(&.{}, empty_record)),
        "({}) => {}",
    );
    try env.expectRender(try env.tuple(&.{inner_fn}), "({} -> [])");

    // Aliases and nominals render their name plus their arguments.
    const alias_name = try env.ident("Al");
    const alias = try env.types.freshFromContent(try env.types.mkAlias(
        .{ .ident_idx = alias_name },
        closed,
        &.{ empty_record, empty_tag_union },
        base.ModuleIdentity.Idx.NONE,
    ));
    try env.expectRender(alias, "Al({}, [])");
    const nominal_name = try env.ident("Builtin.Str");
    try env.expectRender(
        try env.types.freshFromContent(try env.types.mkNominal(
            .{ .ident_idx = nominal_name },
            &.{},
            base.ModuleIdentity.Idx.NONE,
            false,
        )),
        "Str",
    );

    // A var that encloses itself stops at the recursion marker.
    const cyclic = try env.types.fresh();
    const cyclic_elems = try env.types.appendVars(&.{cyclic});
    try env.types.setVarContent(cyclic, .{ .structure = .{ .tuple = .{ .elems = cyclic_elems } } });
    try env.expectRender(cyclic, "(<RecursiveType>)");
}

test "TypeWriter names a flex var consistently once it occurs more than once" {
    var env = try TestEnv.init(testing.allocator, 32);
    defer env.deinit();

    // Positional names are drawn from the identifier store's own vocabulary,
    // so an empty store leaves only the base-26 generator to name with.
    _ = try env.ident("x");

    const shared = try env.types.freshFromContent(.{ .flex = types_mod.Flex.init() });
    const solo = try env.types.freshFromContent(.{ .flex = types_mod.Flex.init() });

    // A var that occurs once takes a positional name; one that occurs twice
    // takes a name it keeps at every occurrence.
    try env.expectRender(try env.tuple(&.{ shared, shared, solo }), "(a, a, _field)");
}

test "TypeWriter renders a where clause for the constraints it collects" {
    var env = try TestEnv.init(testing.allocator, 32);
    defer env.deinit();

    const empty_record = try env.types.freshFromContent(.{ .structure = .empty_record });
    const method = try env.ident("plus");
    const fn_var = try env.types.freshFromContent(try env.types.mkFuncPure(&.{empty_record}, empty_record));
    const constraints = try env.types.appendStaticDispatchConstraints(&.{.{
        .fn_name = method,
        .fn_var = fn_var,
        .origin = .{ .from_literal = .quote },
    }});
    const constrained = try env.types.freshFromContent(.{
        .flex = types_mod.Flex.init().withConstraints(constraints),
    });

    try env.expectRender(constrained, "_a where [_b.plus : {} -> {}]");
}

// A row whose extension chain returns to a row it already collected has
// contributed every field it can. Collection terminates on the revisit, so a
// cyclic row renders rather than collecting fields forever. The starting row
// carries fields of its own here: a chain that loops back through it must not
// collect those fields a second time, and the exact bytes are asserted so a
// duplicate cannot pass.
test "TypeWriter terminates on a record row whose extension chain cycles" {
    var env = try TestEnv.init(testing.allocator, 32);
    defer env.deinit();

    const f = try env.ident("f");
    const g = try env.ident("g");
    const empty_record = try env.types.freshFromContent(.{ .structure = .empty_record });

    // outer = { g: {} } ..inner, inner = { f: {} } ..outer
    const outer = try env.types.fresh();
    const inner_fields = try env.types.appendRecordFields(&.{.{ .name = f, .presence = .required(empty_record) }});
    const inner = try env.types.freshFromContent(.{ .structure = .{ .record = .{
        .fields = inner_fields,
        .ext = outer,
    } } });
    const outer_fields = try env.types.appendRecordFields(&.{.{ .name = g, .presence = .required(empty_record) }});
    try env.types.setVarContent(outer, .{ .structure = .{ .record = .{
        .fields = outer_fields,
        .ext = inner,
    } } });

    try env.expectRender(outer, "{ f: {}, g: {} }");
    try env.expectRender(inner, "{ f: {}, g: {} }");
}

// The tag-union row collector has the same shape as the record one, and the
// same duplicate is possible when a chain loops back through the starting row.
test "TypeWriter terminates on a tag union row whose extension chain cycles" {
    var env = try TestEnv.init(testing.allocator, 32);
    defer env.deinit();

    const tag_a = try env.ident("A");
    const tag_b = try env.ident("B");

    // outer = [B, ..inner], inner = [A, ..outer]
    const outer = try env.types.fresh();
    const inner_tags = try env.types.appendTags(&.{
        .{ .name = tag_a, .args = try env.types.appendVars(&.{}) },
    });
    const inner = try env.types.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = inner_tags,
        .ext = outer,
    } } });
    const outer_tags = try env.types.appendTags(&.{
        .{ .name = tag_b, .args = try env.types.appendVars(&.{}) },
    });
    try env.types.setVarContent(outer, .{ .structure = .{ .tag_union = .{
        .tags = outer_tags,
        .ext = inner,
    } } });

    try env.expectRender(outer, "[A, B]");
    try env.expectRender(inner, "[A, B]");
}

// A row whose extension is the row itself is the shortest cycle there is, and
// must contribute its fields exactly once.
test "TypeWriter terminates on a row whose extension is the row itself" {
    var env = try TestEnv.init(testing.allocator, 32);
    defer env.deinit();

    const f = try env.ident("f");
    const tag_a = try env.ident("A");
    const empty_record = try env.types.freshFromContent(.{ .structure = .empty_record });

    const self_record = try env.types.fresh();
    const self_fields = try env.types.appendRecordFields(&.{.{ .name = f, .presence = .required(empty_record) }});
    try env.types.setVarContent(self_record, .{ .structure = .{ .record = .{
        .fields = self_fields,
        .ext = self_record,
    } } });
    try env.expectRender(self_record, "{ f: {} }");

    const self_tags_union = try env.types.fresh();
    const self_tags = try env.types.appendTags(&.{
        .{ .name = tag_a, .args = try env.types.appendVars(&.{}) },
    });
    try env.types.setVarContent(self_tags_union, .{ .structure = .{ .tag_union = .{
        .tags = self_tags,
        .ext = self_tags_union,
    } } });
    try env.expectRender(self_tags_union, "[A]");
}

// Depth pins for the rendering walk. The type instantiator builds graphs whose
// depth is bounded only by heap, the error snapshotter renders one string per
// node it snapshots, and `report.zig` renders a type for every diagnostic it
// formats—so any type deep enough to build is deep enough to reach here. The
// recursive walk these replaced segfaulted on a 40,000-node tuple spine while
// it survived 20,000, and on a 10,000-node record spine while it survived
// 5,000: a record level costs more native frames than a tuple level, and a
// flex leaf adds the occurrence count's own descent on top.

test "TypeWriter renders a tuple spine deeper than any native-stack budget" {
    const depth: u32 = 40000;
    var env = try TestEnv.init(testing.allocator, depth + 16);
    defer env.deinit();

    var current = try env.types.freshFromContent(.{ .structure = .empty_record });
    for (0..depth) |_| {
        current = try env.tuple(&.{current});
    }

    var into = std.array_list.Managed(u8).init(testing.allocator);
    defer into.deinit();
    try env.writer.writeInto(&into, current, .wrap);

    // `depth` open parens, `{}`, then `depth` close parens.
    try testing.expectEqual(@as(usize, depth * 2 + 2), into.items.len);
}

test "TypeWriter renders a record spine deeper than any native-stack budget" {
    const depth: u32 = 20000;
    var env = try TestEnv.init(testing.allocator, depth * 2 + 16);
    defer env.deinit();

    const field = try env.ident("f");
    // A flex leaf makes the walk drive the occurrence count as well: naming it
    // counts its occurrences from the root, so that walk sees the whole spine.
    var current = try env.types.freshFromContent(.{ .flex = types_mod.Flex.init() });
    for (0..depth) |_| {
        const ext = try env.types.freshFromContent(.{ .structure = .empty_record });
        current = try env.record(&.{.{ .name = field, .presence = .required(current) }}, ext);
    }

    const rendered = try env.writer.writeGet(current, .wrap);
    // `{ f: ` and ` }` per level, then the leaf's positional name.
    try testing.expectEqual(@as(usize, depth * 7 + "_field".len), rendered.len);
}

test "TypeWriter renders a function spine deeper than any native-stack budget" {
    const depth: u32 = 20000;
    var env = try TestEnv.init(testing.allocator, depth * 2 + 16);
    defer env.deinit();

    const empty_record = try env.types.freshFromContent(.{ .structure = .empty_record });
    var current = empty_record;
    for (0..depth) |_| {
        current = try env.types.freshFromContent(try env.types.mkFuncPure(&.{empty_record}, current));
    }

    const rendered = try env.writer.writeGet(current, .wrap);
    // Every level but the outermost sits in return position, so it wears a
    // pair of parens on top of its `{} -> `.
    try testing.expectEqual(@as(usize, depth * 6 + (depth - 1) * 2 + 2), rendered.len);
}

test "TypeWriter counts occurrences across a spine deeper than any native-stack budget" {
    const depth: u32 = 40000;
    var env = try TestEnv.init(testing.allocator, depth + 16);
    defer env.deinit();

    // The leaf occurs twice, so naming it runs the occurrence count over the
    // whole spine before the first name is written.
    const leaf = try env.types.freshFromContent(.{ .flex = types_mod.Flex.init() });
    var current = try env.tuple(&.{ leaf, leaf });
    for (0..depth) |_| {
        current = try env.tuple(&.{current});
    }

    const rendered = try env.writer.writeGet(current, .wrap);
    try testing.expectEqual(@as(usize, depth * 2 + "(a, a)".len), rendered.len);
}

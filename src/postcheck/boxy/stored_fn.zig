//! Stored function values of `.boxy` compile-time evaluation (design.md
//! "Stored Function Values").
//!
//! A function value under `.boxy` is an erased callable: a worker's code and
//! what the worker captured, which is values, the descriptors of its scope's
//! type variables, and dictionaries, themselves erased callables. A stored
//! function records exactly that: a recipe naming the worker, the types the
//! descriptors describe, and the captured values and dictionaries as nodes
//! of the constant store. Restoring it rebuilds the worker from the recipe
//! and packs the same captures, so nothing is inferred about the value.
//!
//! The recipe is bytes the constant store keeps opaque: a table of the types
//! it names, generic where the worker's own types are, then the worker, then
//! the closed type each captured descriptor describes.

const std = @import("std");
const collections = @import("collections");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");
const boxy_runtime = @import("boxy_runtime");

const types = @import("types.zig");
const lower = @import("lower.zig");
const const_schema = @import("const_schema.zig");
const Codec = @import("codec.zig");
const body = @import("body.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const const_store = check.ConstStore;
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const TypeDesc = boxy_runtime.desc.TypeDesc;
const InProcessState = boxy_runtime.compiled.InProcessState;
const TypeId = types.TypeId;
const Program = lower.Program;
const WorkerId = lower.WorkerId;
const invariant = types.invariant;

/// What a worker's erased callable captures, in the order a body packs it:
/// `values`, then `descs` descriptors, then `dicts` erased callables.
pub const CaptureShape = struct {
    values: []const TypeId,
    descs: u32,
    dicts: u32,
    /// The callable is an immortal dictionary (`Builder.composeDictionary`).
    composed: bool,
};

/// The capture shape of `id`, which is a worker some body packs as a
/// function value.
pub fn captureShape(program: *Program, id: WorkerId) CaptureShape {
    const worker = program.worker(id).*;
    return switch (worker.key) {
        .lambda => .{
            .values = worker.capture_types,
            .descs = @intCast(worker.scheme_vars.len - worker.own_descs),
            .dicts = worker.capturedDictionaryCount() + worker.capturedLiteralCount(),
            .composed = false,
        },
        .adapter => .{ .values = worker.capture_types, .descs = @intCast(worker.scheme_vars.len), .dicts = 0, .composed = false },
        .method_entry => |target| blk: {
            const target_worker = program.worker(target).*;
            break :blk .{
                .values = &.{},
                .descs = @intCast(target_worker.scheme_vars.len),
                .dicts = @intCast(target_worker.dictionary_params.len + target_worker.literal_params),
                .composed = true,
            };
        },
        .structural, .structural_map => .{ .values = &.{}, .descs = worker.structural_descs, .dicts = 0, .composed = true },
        .crash_dictionary, .literal_thunk, .root_slot => .{ .values = &.{}, .descs = 0, .dicts = 0, .composed = true },
        .codec => |index| .{
            .values = worker.capture_types,
            .descs = @intCast(worker.scheme_vars.len),
            .dicts = 0,
            .composed = program.codec.pieces.items[index].kind == .constructor,
        },
        .template, .default, .root, .static_data, .convert => invariant("a worker that is no function value reached a stored function"),
    };
}

/// One captured value of a stored function: read it at `ptr`, by `plan`,
/// at `layout_idx`.
pub const Capture = struct {
    plan: LirProgram.ConstPlanId,
    layout_idx: layout.Idx,
    ptr: [*]u8,
};

/// A function value read from a compile-time program.
pub const Described = struct {
    /// Owned by the `allocator` `describe` was given.
    recipe: []const u8,
    /// Its values, then its dictionaries. Owned by the same allocator.
    captures: []const Capture,
};

/// A lowered compile-time program's types and workers, kept for reading the
/// function values it evaluates to. `lir_result` of each call is where the
/// program's result lives after lowering moved it out of the `Program`.
pub const Retained = struct {
    allocator: Allocator,
    program: *Program,
    workers: std.AutoHashMapUnmanaged(LIR.ProcIdentity, WorkerId) = .empty,
    /// The closed type of each descriptor resolved so far.
    desc_types: std.AutoHashMapUnmanaged(*const TypeDesc, TypeId) = .empty,

    pub fn deinit(self: *Retained) void {
        self.workers.deinit(self.allocator);
        self.desc_types.deinit(self.allocator);
        self.program.release();
        self.allocator.destroy(self.program);
        self.allocator.destroy(self);
    }

    /// Read the function value of the proc `proc` whose capture record is at
    /// `capture`, as `composition` composed its descriptors.
    pub fn describe(
        self: *Retained,
        allocator: Allocator,
        result: *LirProgram.Result,
        proc: LIR.LirProcSpecId,
        capture: [*]u8,
        composition: *InProcessState,
    ) Allocator.Error!Described {
        const program = self.program;
        program.result = result.*;
        defer result.* = program.result;
        const worker_id = self.workers.get(result.store.getProcSpec(proc).identity) orelse
            invariant("a stored function value's code is no worker of its program");
        const shape = captureShape(program, worker_id);
        const open = !try allClosed(program, shape.values);
        const word_count = shape.descs + shape.dicts;

        // Where each capture is: a closed record's order is values, then
        // words; a dynamic record's is its own descriptor, words, then values.
        const offsets = try allocator.alloc(u32, shape.values.len + word_count);
        defer allocator.free(offsets);
        if (open) {
            const record_desc: *const TypeDesc = @ptrCast(@alignCast(readWord(capture)));
            for (0..word_count) |index| offsets[shape.values.len + index] = record_desc.fieldBySemanticIndex(@intCast(1 + index)).offset;
            for (0..shape.values.len) |index| offsets[index] = record_desc.fieldBySemanticIndex(@intCast(1 + word_count + index)).offset;
        } else if (offsets.len != 0) {
            const record = try closedRecordLayout(program, shape);
            if (record != .zst) {
                const layouts = &program.result.layouts;
                const struct_idx = layouts.getLayout(record).getStruct().idx;
                for (offsets, 0..) |*offset, index| offset.* = layouts.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(index));
            } else {
                @memset(offsets, 0);
            }
        }

        const bound = try allocator.alloc(TypeId, shape.descs);
        defer allocator.free(bound);
        for (bound, offsets[shape.values.len..][0..shape.descs]) |*ty, offset| {
            const desc: *const TypeDesc = @ptrCast(@alignCast(readWord(capture + offset)));
            ty.* = try self.descType(desc, composition);
        }

        var recipe = Encoder{ .allocator = allocator, .program = program };
        defer recipe.deinit();
        try recipe.worker(worker_id);
        try recipe.typeSpan(bound);

        const captures = try allocator.alloc(Capture, shape.values.len + shape.dicts);
        errdefer allocator.free(captures);
        for (shape.values, captures[0..shape.values.len], offsets[0..shape.values.len]) |generic, *slot, offset| {
            const concrete = try program.substitute(allocator, generic, bound);
            slot.* = .{
                .plan = try const_schema.constPlanOf(program, concrete),
                .layout_idx = try program.layouts.layoutOf(concrete),
                .ptr = capture + offset,
            };
        }
        const dictionary_ty = try program.anyFunction();
        for (captures[shape.values.len..], offsets[shape.values.len + shape.descs..]) |*slot, offset| {
            slot.* = .{
                .plan = try const_schema.constPlanOf(program, dictionary_ty),
                .layout_idx = try program.result.layouts.insertErasedCallable(),
                .ptr = capture + offset,
            };
        }
        return .{ .recipe = try recipe.finish(), .captures = captures };
    }

    /// A closed type `desc` describes (`LIR.DynOp.desc_note`): the noted type
    /// with its variables at the types their descriptors describe. Function
    /// types share a descriptor, so a function in it stands for any function.
    fn descType(self: *Retained, desc: *const TypeDesc, composition: *InProcessState) Allocator.Error!TypeId {
        if (self.desc_types.get(desc)) |known| return known;
        const origin = composition.origin(desc) orelse invariant("a stored function value captured a descriptor its program never noted");
        const noted: TypeId = @fromBackingInt(origin.ty);
        const program = self.program;
        const bound = try self.allocator.alloc(TypeId, origin.vars.len);
        defer self.allocator.free(bound);
        for (bound, origin.vars) |*ty, variable| ty.* = try self.descType(variable, composition);
        const resolved = try program.substituteRepresentation(self.allocator, noted, bound);
        try self.desc_types.put(self.allocator, desc, resolved);
        return resolved;
    }
};

/// Keep `program` for reading the function values its program evaluates to.
pub fn retain(allocator: Allocator, program: *Program) Allocator.Error!*Retained {
    const retained = try allocator.create(Retained);
    errdefer allocator.destroy(retained);
    retained.* = .{ .allocator = allocator, .program = program };
    errdefer retained.workers.deinit(allocator);
    for (program.workers.items, 0..) |worker, index| {
        try retained.workers.put(allocator, program.result.store.getProcSpec(worker.proc).identity, @fromBackingInt(@intCast(index)));
    }
    return retained;
}

fn readWord(ptr: [*]const u8) *const anyopaque {
    return @ptrFromInt(std.mem.readInt(usize, ptr[0..@sizeOf(usize)], @import("builtin").cpu.arch.endian()));
}

fn allClosed(program: *Program, tys: []const TypeId) Allocator.Error!bool {
    var converter = program.converter();
    for (tys) |ty| {
        if (!try converter.representationClosed(ty)) return false;
    }
    return true;
}

/// A closed capture record's layout (`Builder.captureLayout`).
fn closedRecordLayout(program: *Program, shape: CaptureShape) Allocator.Error!layout.Idx {
    const allocator = program.allocator;
    const fields = try allocator.alloc(layout.StructField, shape.values.len + shape.descs + shape.dicts);
    defer allocator.free(fields);
    for (fields, 0..) |*field, index| {
        field.* = .{
            .index = @intCast(index),
            .layout = if (index < shape.values.len)
                try program.layouts.layoutOf(shape.values[index])
            else if (index < shape.values.len + shape.descs)
                .opaque_ptr
            else
                try program.result.layouts.insertErasedCallable(),
        };
    }
    return try program.result.layouts.putStructFields(fields);
}

const Tag = enum(u8) {
    template,
    lambda,
    adapter,
    default,
    method_entry,
    literal_thunk,
    root_slot,
    structural,
    structural_map,
    crash_dictionary,
    codec,
};

fn appendWord(allocator: Allocator, list: *std.ArrayList(u8), value: u32) Allocator.Error!void {
    var buffer: [4]u8 = undefined;
    std.mem.writeInt(u32, &buffer, value, .little);
    try list.appendSlice(allocator, &buffer);
}

const no_type = std.math.maxInt(u32);

const Encoder = struct {
    allocator: Allocator,
    program: *Program,
    /// The table of types: each is a `types.Node` whose components are
    /// earlier entries, so a type shared across the recipe is written once.
    table: std.ArrayList(u8) = .empty,
    table_len: u32 = 0,
    table_index: collections.DenseMapUnmanaged(TypeId, u32) = .empty,
    bytes: std.ArrayList(u8) = .empty,

    fn deinit(self: *Encoder) void {
        self.table.deinit(self.allocator);
        self.table_index.deinit(self.allocator);
        self.bytes.deinit(self.allocator);
    }

    fn finish(self: *Encoder) Allocator.Error![]const u8 {
        var out: std.ArrayList(u8) = .empty;
        errdefer out.deinit(self.allocator);
        try appendWord(self.allocator, &out, self.table_len);
        try out.appendSlice(self.allocator, self.table.items);
        try out.appendSlice(self.allocator, self.bytes.items);
        return try out.toOwnedSlice(self.allocator);
    }

    fn word(self: *Encoder, value: u32) Allocator.Error!void {
        try appendWord(self.allocator, &self.bytes, value);
    }

    fn tag(self: *Encoder, value: Tag) Allocator.Error!void {
        try self.bytes.append(self.allocator, @intFromEnum(value));
    }

    fn module(self: *Encoder, index: types.ModuleIndex) Allocator.Error!void {
        try self.bytes.appendSlice(self.allocator, &self.program.module(index).key.bytes);
    }

    fn typeOf(self: *Encoder, ty: TypeId) Allocator.Error!void {
        try self.word(try self.tableIndex(ty));
    }

    fn typeSpan(self: *Encoder, tys: []const TypeId) Allocator.Error!void {
        try self.word(@intCast(tys.len));
        for (tys) |ty| try self.typeOf(ty);
    }

    fn name(self: *Encoder, id: types.NameId) Allocator.Error!void {
        const text = self.program.store.nameText(id);
        try self.word(@intCast(text.len));
        try self.bytes.appendSlice(self.allocator, text);
    }

    /// `root`'s entry of the table, written along with its components'.
    fn tableIndex(self: *Encoder, root: TypeId) Allocator.Error!u32 {
        const Frame = struct { ty: TypeId, expanded: bool };
        var stack: std.ArrayList(Frame) = .empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .ty = root, .expanded = false });
        const store = &self.program.store;
        while (stack.pop()) |frame| {
            if (self.table_index.contains(frame.ty)) continue;
            if (frame.expanded) {
                try self.tableEntry(frame.ty);
                try self.table_index.put(self.allocator, frame.ty, self.table_len);
                self.table_len += 1;
                continue;
            }
            try stack.append(self.allocator, .{ .ty = frame.ty, .expanded = true });
            const start = stack.items.len;
            switch (store.get(frame.ty)) {
                .record => |record| {
                    for (store.fieldSpan(record.fields)) |field| try stack.append(self.allocator, .{ .ty = field.ty, .expanded = false });
                    if (record.ext) |ext| try stack.append(self.allocator, .{ .ty = ext, .expanded = false });
                },
                .tuple => |items| for (store.span(items)) |item| try stack.append(self.allocator, .{ .ty = item, .expanded = false }),
                .tag_union => |tag_union| {
                    for (store.tagSpan(tag_union.tags)) |entry| for (store.span(entry.payloads)) |payload| try stack.append(self.allocator, .{ .ty = payload, .expanded = false });
                    if (tag_union.ext) |ext| try stack.append(self.allocator, .{ .ty = ext, .expanded = false });
                },
                .list, .box => |child| try stack.append(self.allocator, .{ .ty = child, .expanded = false }),
                .nominal => |nominal| for (store.nominalArgs(nominal)) |arg| try stack.append(self.allocator, .{ .ty = arg, .expanded = false }),
                .function => |function| {
                    for (store.span(function.args)) |arg| try stack.append(self.allocator, .{ .ty = arg, .expanded = false });
                    try stack.append(self.allocator, .{ .ty = function.ret, .expanded = false });
                },
                .primitive, .empty_record, .variable => {},
            }
            std.mem.reverse(Frame, stack.items[start..]);
        }
        return self.table_index.get(root).?;
    }

    fn tableEntry(self: *Encoder, ty: TypeId) Allocator.Error!void {
        const store = &self.program.store;
        const node = store.get(ty);
        try self.table.append(self.allocator, @intFromEnum(std.meta.activeTag(node)));
        switch (node) {
            .primitive => |primitive| try self.table.append(self.allocator, @intFromEnum(primitive)),
            .empty_record => {},
            .record => |record| {
                const fields = store.fieldSpan(record.fields);
                try self.tableWord(@intCast(fields.len));
                for (fields) |field| {
                    try self.tableName(field.name);
                    try self.tableType(field.ty);
                    try self.table.append(self.allocator, @intFromEnum(field.kind));
                }
                try self.tableExt(record.ext);
            },
            .tuple => |items| try self.tableTypes(store.span(items)),
            .tag_union => |tag_union| {
                const tags = store.tagSpan(tag_union.tags);
                try self.tableWord(@intCast(tags.len));
                for (tags) |entry| {
                    try self.tableName(entry.name);
                    try self.tableTypes(store.span(entry.payloads));
                }
                try self.tableExt(tag_union.ext);
            },
            .list, .box => |child| try self.tableType(child),
            .nominal => |nominal| {
                try self.table.appendSlice(self.allocator, &self.program.module(nominal.decl.module).key.bytes);
                try self.tableWord(@backingInt(nominal.decl.decl));
                try self.tableTypes(store.nominalArgs(nominal));
                try self.tableName(nominal.name);
                try self.table.append(self.allocator, @intFromBool(nominal.is_opaque));
                try self.table.append(self.allocator, if (nominal.builtin) |builtin| @intFromEnum(builtin) + 1 else 0);
                try self.table.append(self.allocator, @intFromBool(nominal.has_backing));
            },
            .function => |function| {
                try self.tableTypes(store.span(function.args));
                try self.tableType(function.ret);
                try self.table.append(self.allocator, @intFromBool(function.effectful));
            },
            .variable => |index| try self.tableWord(index),
        }
    }

    fn tableWord(self: *Encoder, value: u32) Allocator.Error!void {
        try appendWord(self.allocator, &self.table, value);
    }

    fn tableType(self: *Encoder, ty: TypeId) Allocator.Error!void {
        try self.tableWord(self.table_index.get(ty).?);
    }

    fn tableTypes(self: *Encoder, tys: []const TypeId) Allocator.Error!void {
        try self.tableWord(@intCast(tys.len));
        for (tys) |ty| try self.tableType(ty);
    }

    fn tableExt(self: *Encoder, ext: ?TypeId) Allocator.Error!void {
        try self.tableWord(if (ext) |ty| self.table_index.get(ty).? else no_type);
    }

    fn tableName(self: *Encoder, id: types.NameId) Allocator.Error!void {
        const text = self.program.store.nameText(id);
        try self.tableWord(@intCast(text.len));
        try self.table.appendSlice(self.allocator, text);
    }

    fn worker(self: *Encoder, id: WorkerId) Allocator.Error!void {
        const program = self.program;
        const record = program.worker(id).*;
        switch (record.key) {
            .template => |key| {
                try self.tag(.template);
                try self.module(key.module);
                try self.word(@backingInt(key.template));
            },
            .lambda => |key| {
                try self.tag(.lambda);
                try self.word(@backingInt(key.expr));
                try self.word(@intFromBool(key.captured_scheme));
                try self.worker(key.outer);
            },
            .adapter => |key| {
                try self.tag(.adapter);
                try self.typeOf(key.from);
                try self.typeOf(key.to);
                try self.worker(key.outer);
            },
            .default => |key| {
                try self.tag(.default);
                try self.module(key.module);
                try self.word(@backingInt(key.expr));
            },
            .method_entry => |target| {
                try self.tag(.method_entry);
                try self.worker(target);
            },
            .literal_thunk => |key| {
                try self.tag(.literal_thunk);
                try self.typeOf(key.bound);
                try self.word(key.literal_requirement);
                try self.worker(key.target);
            },
            .root_slot => |key| {
                try self.tag(.root_slot);
                try self.module(key.module);
                try self.word(@backingInt(key.root));
                try self.typeOf(record.ret);
            },
            .structural => |method| {
                try self.tag(.structural);
                try self.module(record.module);
                try self.bytes.append(self.allocator, @intFromEnum(method));
            },
            .structural_map => |map| {
                try self.tag(.structural_map);
                try self.module(record.module);
                try self.name(map.name);
                try self.word(map.payload_index);
                try self.word(map.arity);
                try self.word(@intFromBool(map.effectful));
            },
            .crash_dictionary => |kind| {
                try self.tag(.crash_dictionary);
                try self.module(record.module);
                try self.bytes.append(self.allocator, @intFromEnum(kind));
            },
            .codec => |index| {
                try self.tag(.codec);
                try self.module(record.module);
                const piece = program.codec.pieces.items[index];
                try self.bytes.append(self.allocator, @intFromEnum(piece.kind));
                try self.word(Codec.pieceFamilyRoot(program, piece));
                switch (Codec.pieceOperands(piece.kind)) {
                    .none => {},
                    .types => {
                        try self.typeOf(@fromBackingInt(piece.a));
                        try self.typeOf(@fromBackingInt(piece.b));
                    },
                    .type_and_name => {
                        try self.typeOf(@fromBackingInt(piece.a));
                        try self.name(@fromBackingInt(piece.b));
                    },
                    .type_and_word => {
                        try self.typeOf(@fromBackingInt(piece.a));
                        try self.word(piece.b);
                    },
                }
                try self.typeSpan(record.params);
                try self.typeOf(record.ret);
                try self.typeSpan(record.capture_types);
            },
            .root, .static_data, .convert => invariant("a worker that is no function value reached a stored function"),
        }
    }
};

const Decoder = struct {
    b: *body.Builder,
    bytes: []const u8,
    at: usize = 0,
    table: []TypeId = &.{},

    fn take(self: *Decoder, len: usize) []const u8 {
        defer self.at += len;
        return self.bytes[self.at..][0..len];
    }

    fn word(self: *Decoder) u32 {
        return std.mem.readInt(u32, self.take(4)[0..4], .little);
    }

    fn byte(self: *Decoder) u8 {
        return self.take(1)[0];
    }

    fn module(self: *Decoder) types.ModuleIndex {
        return self.b.program.moduleByKey(.{ .bytes = self.take(32)[0..32].* }).index;
    }

    fn typeOf(self: *Decoder) TypeId {
        return self.table[self.word()];
    }

    fn typeSpan(self: *Decoder) Allocator.Error![]const TypeId {
        const out = try self.b.program.arena.allocator().alloc(TypeId, self.word());
        for (out) |*ty| ty.* = self.typeOf();
        return out;
    }

    fn ext(self: *Decoder) ?TypeId {
        const index = self.word();
        return if (index == no_type) null else self.table[index];
    }

    fn readTable(self: *Decoder) Allocator.Error!void {
        self.table = try self.b.allocator.alloc(TypeId, self.word());
        for (self.table) |*ty| ty.* = try self.tableEntry();
    }

    /// One entry of the table, whose components are entries before it.
    fn tableEntry(self: *Decoder) Allocator.Error!TypeId {
        const program = self.b.program;
        const allocator = self.b.allocator;
        const store = &program.store;
        const node_tag: std.meta.Tag(types.Node) = @enumFromInt(self.byte());
        return switch (node_tag) {
            .primitive => try store.intern(.{ .primitive = @enumFromInt(self.byte()) }),
            .empty_record => try store.intern(.empty_record),
            .record => blk: {
                const fields = try allocator.alloc(types.Field, self.word());
                defer allocator.free(fields);
                for (fields) |*field| {
                    const field_name = try self.name();
                    const ty = self.typeOf();
                    field.* = .{ .name = field_name, .ty = ty, .kind = @enumFromInt(self.byte()) };
                }
                break :blk try store.intern(.{ .record = .{ .fields = fields, .ext = self.ext() } });
            },
            .tuple => blk: {
                const items = try self.typeSpan();
                break :blk try store.intern(.{ .tuple = items });
            },
            .tag_union => blk: {
                const tags = try allocator.alloc(types.TagInput, self.word());
                defer allocator.free(tags);
                for (tags) |*entry| {
                    const tag_name = try self.name();
                    entry.* = .{ .name = tag_name, .payloads = try self.typeSpan() };
                }
                break :blk try store.intern(.{ .tag_union = .{ .tags = tags, .ext = self.ext() } });
            },
            .list => try store.intern(.{ .list = self.typeOf() }),
            .box => try store.intern(.{ .box = self.typeOf() }),
            .nominal => blk: {
                const module_index = self.module();
                const decl: checked.CheckedNominalDeclarationId = @fromBackingInt(self.word());
                const args = try self.typeSpan();
                const nominal_name = try self.name();
                const is_opaque = self.byte() != 0;
                const builtin = self.byte();
                break :blk try store.intern(.{ .nominal = .{
                    .decl = .{ .module = module_index, .decl = decl },
                    .args = args,
                    .name = nominal_name,
                    .is_opaque = is_opaque,
                    .builtin = if (builtin == 0) null else @enumFromInt(builtin - 1),
                    .has_backing = self.byte() != 0,
                } });
            },
            .function => blk: {
                const args = try self.typeSpan();
                const ret = self.typeOf();
                break :blk try store.intern(.{ .function = .{ .args = args, .ret = ret, .effectful = self.byte() != 0 } });
            },
            .variable => try store.variable(self.word()),
        };
    }

    fn name(self: *Decoder) Allocator.Error!types.NameId {
        const len = self.word();
        return try self.b.program.store.internName(self.take(len));
    }

    fn worker(self: *Decoder) Allocator.Error!WorkerId {
        const b = self.b;
        const program = b.program;
        const tag: Tag = @enumFromInt(self.byte());
        switch (tag) {
            .template => {
                const module_index = self.module();
                return try program.templateWorker(module_index, @fromBackingInt(self.word()));
            },
            .lambda => {
                const expr: checked.CheckedExprId = @fromBackingInt(self.word());
                const captured_scheme = self.word() != 0;
                return try b.storedLambdaWorker(try self.worker(), expr, captured_scheme);
            },
            .adapter => {
                const from = self.typeOf();
                const to = self.typeOf();
                return try program.adapterWorker(try self.worker(), from, to);
            },
            .default => {
                const module_index = self.module();
                return try program.defaultWorker(module_index, @fromBackingInt(self.word()));
            },
            .method_entry => return try program.methodEntryWorker(try self.worker()),
            .literal_thunk => {
                const bound_ty = self.typeOf();
                const requirement = self.word();
                const bound = switch (program.store.get(bound_ty)) {
                    .tuple => |items| program.store.span(items),
                    .primitive, .empty_record, .record, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a stored literal thunk's types are not a tuple"),
                };
                return try program.literalThunkWorker(try self.worker(), try program.arena.allocator().dupe(TypeId, bound), requirement);
            },
            // The compile-time program read the root's slot; its value is
            // stored now, and the root's wrapper restores it.
            .root_slot => {
                const module_index = self.module();
                const root: checked.ComptimeRootId = @fromBackingInt(self.word());
                _ = self.typeOf();
                const wrapper = program.wrapper_by_root.get(.{ .module = module_index, .root = root }) orelse
                    invariant("a literal argument root has no entry wrapper");
                return try program.methodEntryWorker(try program.templateRefWorker(wrapper));
            },
            .structural => {
                const module_index = self.module();
                return try program.structuralWorker(module_index, @enumFromInt(self.byte()));
            },
            .structural_map => {
                const module_index = self.module();
                const name_id = try self.name();
                const payload_index = self.word();
                const arity = self.word();
                return try program.structuralMapWorker(module_index, .{ .name = name_id, .payload_index = payload_index, .arity = arity, .effectful = self.word() != 0 });
            },
            .crash_dictionary => {
                const module_index = self.module();
                return try program.crashDictionaryWorker(module_index, @enumFromInt(self.byte()));
            },
            .codec => {
                const module_index = self.module();
                const kind: Codec.PieceKind = @enumFromInt(self.byte());
                const root = self.word();
                var a: u32 = 0;
                var b_operand: u32 = 0;
                switch (Codec.pieceOperands(kind)) {
                    .none => {},
                    .types => {
                        a = @backingInt(self.typeOf());
                        b_operand = @backingInt(self.typeOf());
                    },
                    .type_and_name => {
                        a = @backingInt(self.typeOf());
                        b_operand = @backingInt(try self.name());
                    },
                    .type_and_word => {
                        a = @backingInt(self.typeOf());
                        b_operand = self.word();
                    },
                }
                const params = try self.typeSpan();
                const ret = self.typeOf();
                const captures = try self.typeSpan();
                return try Codec.restorePiece(program, module_index, root, kind, a, b_operand, params, ret, captures);
            },
        }
    }
};

/// A stored function value's worker and the types its descriptors describe.
pub const Decoded = struct { worker: WorkerId, bound: []const TypeId };

/// Read a stored function's recipe.
pub fn decode(b: *body.Builder, recipe: []const u8) Allocator.Error!Decoded {
    var decoder = Decoder{ .b = b, .bytes = recipe };
    try decoder.readTable();
    defer b.allocator.free(decoder.table);
    const worker = try decoder.worker();
    return .{ .worker = worker, .bound = try decoder.typeSpan() };
}

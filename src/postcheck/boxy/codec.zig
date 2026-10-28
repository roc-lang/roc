//! Generated structural codecs under `.boxy` (design.md "Dictionaries",
//! "Workers And Hidden Parameters", "Function Values"): the parser and
//! encoder workers of one checked generated-codec derivation, and the
//! compiler-owned serialization intrinsics they hand to format methods.
//!
//! A derivation is lowered once per content identity, as a *family* of
//! workers: an erased constructor (the dictionary entry of `parser_for` or
//! `encoder_for`), the runtime callable the constructor returns, one direct
//! helper per encoded or parsed type, and the erased callbacks the format's
//! container methods receive. Every format method call is the exact checked
//! call of the derivation's contract (or of a nested structural derivation's
//! contract the family reaches), selected by method name and subject type.
//!
//! The constructor evaluates `rename_field` for every record field the
//! family reaches, in the order the checked codec visits them, and the
//! runtime callable captures the encoding and those names. Every helper
//! takes the encoding and the names as trailing arguments.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");

const types = @import("types.zig");
const lower = @import("lower.zig");
const Body = @import("body.zig");
const layouts_mod = @import("layouts.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;
const LIR = lir_core.LIR;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const TypeId = types.TypeId;
const NameId = types.NameId;
const ModuleIndex = types.ModuleIndex;
const Program = lower.Program;
const WorkerId = lower.WorkerId;
const Builder = Body.Builder;
const invariant = types.invariant;
const invariantFmt = types.invariantFmt;

const DerivationId = static_dispatch.GeneratedCodecDerivationId;

pub const Kind = enum(u8) { parser, encoder };

/// The subject of a subject-free contract call.
const no_subject: TypeId = @fromBackingInt(std.math.maxInt(u32));

/// A contract call's selection key: its method and its subject type.
const CallKey = struct { method: NameId, subject: TypeId };

/// What a contract call resolved to.
const Resolved = union(enum) {
    /// A procedure, through its checked evidence edge.
    callable: struct { node: static_dispatch.EvidenceNodeId, fn_ty: TypeId, checked_fn: checked.CheckedTypeId },
    /// A nested generated derivation, walked structurally by this family.
    structural: DerivationId,
    /// A call checking rejected.
    checked_error: TypeId,
};

const FamilyKey = struct { kind: Kind, digest: [32]u8 };

/// One renamed record field the constructor computes.
const NameEntry = struct { record: TypeId, label: NameId };

pub const Family = struct {
    module: ModuleIndex,
    /// The family's type variables (the derivation's), which every family
    /// worker takes as hidden descriptors.
    env: types.Env,
    kind: Kind,
    root: DerivationId,
    /// The value type the codec reads or writes.
    shape: TypeId,
    encoding: TypeId,
    state: TypeId,
    /// `encoding -> runtime`.
    constructor_fn: TypeId,
    /// The runtime callable's function type.
    runtime_fn: TypeId,
    calls: std.AutoHashMapUnmanaged(CallKey, Resolved) = .empty,
    names: std.ArrayList(NameEntry) = .empty,
    /// The first name of each record's fields, by record type.
    record_names: std.AutoHashMapUnmanaged(TypeId, u32) = .empty,
    /// The tuple of every renamed name (the empty record when there are none).
    names_ty: TypeId,
    /// A `Try` type whose declaration builds the family's other `Try`s.
    try_template: TypeId,
};

pub const PieceKind = enum(u8) {
    /// The erased `parser_for`/`encoder_for` entry.
    constructor,
    /// The runtime callable the constructor returns.
    runtime,
    /// `a`: value type, `b`: result type.
    encode_helper,
    /// `a`: value type, `b`: result type.
    parse_helper,
    /// `a`: record type.
    record_body,
    /// `a`: tuple type.
    tuple_body,
    /// `a`: list type.
    list_body,
    /// `a`: dict type.
    dict_body,
    /// `a`: union type, `b`: tag name.
    tag_body,
    /// `a`: value type, `b`: writer function type.
    value_thunk,
    /// `a`: key type, `b`: writer function type.
    key_thunk,
    /// `a`: union type, `b`: the `parse_tag_union` function type.
    tag_spec,
    /// `a`: iterator type. Not part of a family.
    field_names_step,
};

pub const Piece = struct { family: u32, kind: PieceKind, a: u32 = 0, b: u32 = 0 };

const no_family = std.math.maxInt(u32);

pub const State = struct {
    families: std.ArrayList(Family) = .empty,
    family_ids: std.AutoHashMapUnmanaged(FamilyKey, u32) = .empty,
    pieces: std.ArrayList(Piece) = .empty,
    /// The captured value types of each piece's worker, by piece index.
    piece_captures: std.ArrayList([]const TypeId) = .empty,
    piece_ids: std.AutoHashMapUnmanaged(Piece, WorkerId) = .empty,

    pub fn deinit(self: *State, allocator: Allocator) void {
        for (self.families.items) |*family| {
            family.calls.deinit(allocator);
            family.names.deinit(allocator);
            family.record_names.deinit(allocator);
        }
        self.families.deinit(allocator);
        self.family_ids.deinit(allocator);
        self.pieces.deinit(allocator);
        self.piece_captures.deinit(allocator);
        self.piece_ids.deinit(allocator);
    }
};

// -- Families --

/// A contract type: its variables are the family's type variables, and its
/// undetermined field kinds commit to `required`, as no caller pins them.
fn convertIn(program: *Program, env: types.Env, ty: checked.CheckedTypeId) Allocator.Error!TypeId {
    var converter = program.converter();
    converter.seal_undetermined_kinds = true;
    return converter.convert(env.module, ty, env);
}

/// The type variables of a derivation: those its constructor type mentions
/// outside the value shape, in first-visit order, each bound to the family's
/// variable at its index. The shape's own variables (an uninhabited payload,
/// say) seal to their defaults: the shape is the contract's, and only the
/// results' rows vary with a use.
fn contractEnv(program: *Program, module: ModuleIndex, constructor_ty: checked.CheckedTypeId, shape_ty: checked.CheckedTypeId) Allocator.Error!types.Env {
    const allocator = program.allocator;
    const store = program.module(module).types();
    var vars: std.ArrayList(checked.CheckedTypeId) = .empty;
    defer vars.deinit(allocator);
    var seen: std.AutoHashMapUnmanaged(checked.CheckedTypeId, void) = .empty;
    defer seen.deinit(allocator);
    var pending: std.ArrayList(checked.CheckedTypeId) = .empty;
    defer pending.deinit(allocator);
    // The shape's types are visited first and contribute no variables.
    try pending.append(allocator, constructor_ty);
    try pending.append(allocator, shape_ty);
    var shape_done = false;
    while (pending.pop()) |ty| {
        if (!shape_done and pending.items.len == 0 and ty == constructor_ty) shape_done = true;
        if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
        const start = pending.items.len;
        switch (store.payload(ty)) {
            .flex, .rigid => if (shape_done) try vars.append(allocator, ty),
            .alias => |alias| try pending.append(allocator, alias.backing),
            .record => |record| {
                for (record.fields) |field| try pending.append(allocator, field.ty);
                try pending.append(allocator, record.ext);
            },
            .tuple => |items| try pending.appendSlice(allocator, items),
            .nominal => |nominal| try pending.appendSlice(allocator, nominal.args),
            .function => |function| {
                try pending.appendSlice(allocator, function.args);
                try pending.append(allocator, function.ret);
            },
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| try pending.appendSlice(allocator, tag.argsSlice(store));
                try pending.append(allocator, tag_union.ext);
            },
            .empty_record, .empty_tag_union => {},
            .pending, .err => invariant("a generated codec contract's type is not a checked type"),
        }
        // Components are visited in source order.
        std.mem.reverse(checked.CheckedTypeId, pending.items[start..]);
    }
    const arena = program.arena.allocator();
    const bound = try arena.alloc(TypeId, vars.items.len);
    for (bound, 0..) |*slot, index| slot.* = try program.store.variable(@intCast(index));
    return .{ .module = module, .vars = try arena.dupe(checked.CheckedTypeId, vars.items), .bound = bound };
}

fn functionOf(program: *Program, ty: TypeId) @FieldType(types.Node, "function") {
    return switch (program.store.get(ty)) {
        .function => |function| function,
        else => invariant("a generated codec's callable type is not a function type"),
    };
}

fn fnArgs(program: *Program, ty: TypeId) []const TypeId {
    return program.store.span(functionOf(program, ty).args);
}

fn fnRet(program: *Program, ty: TypeId) TypeId {
    return functionOf(program, ty).ret;
}

/// The family of derivation `id` of `module`'s plan table.
pub fn familyFor(program: *Program, module: ModuleIndex, id: DerivationId) Allocator.Error!u32 {
    const state = &program.codec;
    const allocator = program.allocator;
    const table = program.module(module).view.static_dispatch_plans;
    const derivation = table.generated_codec_derivations[@backingInt(id)];
    const kind: Kind = switch (derivation.kind) {
        .parser => .parser,
        .encoder => .encoder,
    };
    const key: FamilyKey = .{ .kind = kind, .digest = derivation.content.bytes };
    if (state.family_ids.get(key)) |existing| return existing;

    const env = try contractEnv(program, module, derivation.constructor_ty, derivation.shape_ty);
    const constructor_fn = try convertIn(program, env, derivation.constructor_ty);
    if (fnArgs(program, constructor_fn).len != 1) invariant("a generated codec constructor does not take one encoding");
    const encoding_ty = fnArgs(program, constructor_fn)[0];
    const runtime_fn = fnRet(program, constructor_fn);
    const runtime_args = fnArgs(program, runtime_fn);
    const state_ty = switch (kind) {
        .parser => if (runtime_args.len == 1) runtime_args[0] else invariant("a generated parser's runtime does not take one state"),
        .encoder => if (runtime_args.len == 2) runtime_args[1] else invariant("a generated encoder's runtime does not take a value and a state"),
    };
    const shape = try convertIn(program, env, derivation.shape_ty);
    // The codec walks a value whose shape its contract fixes; only the
    // results (their error rows) vary with the family's variables.
    const conv = program.converter();
    if (!try conv.representationClosed(shape) or !try conv.representationClosed(encoding_ty) or !try conv.representationClosed(state_ty)) {
        invariant("generated codecs over open value shapes are not yet lowered by Boxy");
    }

    const index: u32 = @intCast(state.families.items.len);
    try state.families.append(allocator, .{
        .module = module,
        .env = env,
        .kind = kind,
        .root = id,
        .shape = shape,
        .encoding = encoding_ty,
        .state = state_ty,
        .constructor_fn = constructor_fn,
        .runtime_fn = runtime_fn,
        .names_ty = undefined,
        .try_template = fnRet(program, runtime_fn),
    });
    try state.family_ids.put(allocator, key, index);
    try collectCalls(program, index);
    try planNames(program, index);
    return index;
}

/// The method name of a call's selection key.
fn methodName(program: *Program, text: []const u8) Allocator.Error!NameId {
    return program.store.internName(text);
}

/// The selection-key form of a subject type: a format method's subject is
/// structural, so nominal layers that the codec walks through (user
/// nominals and `Try`) are peeled. A codec boundary method keeps its exact
/// subject, whose nominal identity selects the codec.
fn subjectKey(program: *Program, method: NameId, ty: TypeId) Allocator.Error!TypeId {
    const text = program.store.nameText(method);
    if (std.mem.eql(u8, text, "encoder_for") or std.mem.eql(u8, text, "parser_for")) return ty;
    var current = ty;
    while (true) {
        switch (program.store.get(current)) {
            .nominal => |nominal| {
                if (!peelable(nominal)) return current;
                current = try program.converter().backing(current);
            },
            else => return current,
        }
    }
}

/// Whether a codec walks through a nominal to its backing: a user nominal
/// or `Try` (whose null and missing forms the codec reads first).
fn peelable(nominal: types.Nominal) bool {
    const builtin = nominal.builtin orelse return nominal.has_backing;
    return builtin == .try_;
}

/// Every call of the root derivation's contract and of each nested
/// structural derivation it reaches, by selection key.
fn collectCalls(program: *Program, family_index: u32) Allocator.Error!void {
    const allocator = program.allocator;
    const module = program.codec.families.items[family_index].module;
    const env = program.codec.families.items[family_index].env;
    const table = program.module(module).view.static_dispatch_plans;
    const names = program.module(module).nameStore();
    var visited: std.AutoHashMapUnmanaged(DerivationId, void) = .empty;
    defer visited.deinit(allocator);
    var pending: std.ArrayList(DerivationId) = .empty;
    defer pending.deinit(allocator);
    try pending.append(allocator, program.codec.families.items[family_index].root);
    try visited.put(allocator, program.codec.families.items[family_index].root, {});
    while (pending.pop()) |id| {
        const derivation = table.generated_codec_derivations[@backingInt(id)];
        for (derivation.callsSlice(table)) |call| {
            const method = try methodName(program, names.methodNameText(call.method));
            const subject = if (call.subject_ty) |subject_ty|
                try subjectKey(program, method, try convertIn(program, env, subject_ty))
            else
                no_subject;
            const resolved: Resolved = switch (call.resolution) {
                .pending => invariant("a generated codec contract retained an unlinked call"),
                .checked_error => .{ .checked_error = try convertIn(program, env, call.callable_ty) },
                .callable => |node| .{ .callable = .{ .node = node, .fn_ty = try convertIn(program, env, call.callable_ty), .checked_fn = call.callable_ty } },
                .structural => |nested| blk: {
                    if ((try visited.getOrPut(allocator, nested)).found_existing == false) try pending.append(allocator, nested);
                    break :blk .{ .structural = nested };
                },
            };
            const entry = try program.codec.families.items[family_index].calls.getOrPut(allocator, .{ .method = method, .subject = subject });
            if (!entry.found_existing) entry.value_ptr.* = resolved;
        }
    }
}

fn lookupCall(program: *Program, family_index: u32, method_text: []const u8, subject: ?TypeId) Allocator.Error!Resolved {
    const method = try methodName(program, method_text);
    const key: CallKey = .{ .method = method, .subject = if (subject) |ty| try subjectKey(program, method, ty) else no_subject };
    return program.codec.families.items[family_index].calls.get(key) orelse
        invariantFmt("a generated codec called {s}, which its checked contract does not record", .{method_text});
}

fn findCall(program: *Program, family_index: u32, method_text: []const u8, subject: TypeId) Allocator.Error!?Resolved {
    const method = try methodName(program, method_text);
    return program.codec.families.items[family_index].calls.get(.{ .method = method, .subject = try subjectKey(program, method, subject) });
}

// -- Shapes --

const Scalar = enum { bool, str, u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, dec, f32, f64 };

const Shape = union(enum) {
    /// `Try(ok, [Null])`.
    null_try: struct { ok: TypeId },
    dict: struct { key: TypeId, value: TypeId },
    set: TypeId,
    /// A nominal whose codec is a checked procedure.
    custom: Resolved,
    /// The structural list type.
    list: TypeId,
    tuple: TypeId,
    box: TypeId,
    scalar: Scalar,
    /// A record or the empty record.
    record: TypeId,
    tag_union: TypeId,
};

fn scalarOf(program: *Program, ty: TypeId) ?Scalar {
    return switch (program.store.get(ty)) {
        .primitive => |primitive| switch (primitive) {
            inline .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec, .f32, .f64 => |tag| @field(Scalar, @tagName(tag)),
            else => null,
        },
        .nominal => |nominal| if (nominal.builtin == .bool) .bool else null,
        else => null,
    };
}

fn builtinArgs(program: *Program, ty: TypeId, builtin: checked.CheckedBuiltinNominal) ?[]const TypeId {
    return switch (program.store.get(ty)) {
        .nominal => |nominal| if (nominal.builtin == builtin) program.store.nominalArgs(nominal) else null,
        else => null,
    };
}

fn tagByText(program: *Program, union_ty: TypeId, text: []const u8) ?types.Tag {
    const tag_union = switch (program.store.get(union_ty)) {
        .tag_union => |tag_union| tag_union,
        else => return null,
    };
    for (program.store.tagSpan(tag_union.tags)) |tag| {
        if (std.mem.eql(u8, program.store.nameText(tag.name), text)) return tag;
    }
    return null;
}

/// Whether `ty` is the closed union of the one payload-free tag `text`.
fn isUnitTagUnion(program: *Program, ty: TypeId, text: []const u8) bool {
    const tag_union = switch (program.store.get(ty)) {
        .tag_union => |tag_union| tag_union,
        else => return false,
    };
    if (tag_union.ext != null) return false;
    const tags = program.store.tagSpan(tag_union.tags);
    return tags.len == 1 and tags[0].payloads.len == 0 and std.mem.eql(u8, program.store.nameText(tags[0].name), text);
}

const TryParts = struct { union_ty: TypeId, ok: TypeId, err: TypeId };

fn tryParts(program: *Program, try_ty: TypeId) Allocator.Error!?TryParts {
    if (builtinArgs(program, try_ty, .try_) == null) return null;
    const backing = try program.converter().backing(try_ty);
    const ok = tagByText(program, backing, "Ok") orelse return null;
    const err = tagByText(program, backing, "Err") orelse return null;
    if (ok.payloads.len != 1 or err.payloads.len != 1) return null;
    return .{ .union_ty = backing, .ok = program.store.span(ok.payloads)[0], .err = program.store.span(err.payloads)[0] };
}

fn requireTry(program: *Program, try_ty: TypeId) Allocator.Error!TryParts {
    return (try tryParts(program, try_ty)) orelse invariant("a generated codec result is not a Try");
}

/// `Try(ok, [Missing])`, the optional record field form.
fn missingTryOk(program: *Program, ty: TypeId) Allocator.Error!?TypeId {
    const parts = (try tryParts(program, ty)) orelse return null;
    return if (isUnitTagUnion(program, parts.err, "Missing")) parts.ok else null;
}

/// How a family reads or writes values of `ty`.
fn classify(program: *Program, family_index: u32, ty: TypeId) Allocator.Error!Shape {
    const kind = program.codec.families.items[family_index].kind;
    const boundary = switch (kind) {
        .parser => "parser_for",
        .encoder => "encoder_for",
    };
    var current = ty;
    while (true) {
        if (try tryParts(program, current)) |parts| {
            if (isUnitTagUnion(program, parts.err, "Null")) return .{ .null_try = .{ .ok = parts.ok } };
        }
        if (builtinArgs(program, current, .dict)) |args| return .{ .dict = .{ .key = args[0], .value = args[1] } };
        if (builtinArgs(program, current, .set)) |args| return .{ .set = args[0] };
        if (try findCall(program, family_index, boundary, current)) |resolved| switch (resolved) {
            .callable, .checked_error => return .{ .custom = resolved },
            .structural => {},
        };
        if (scalarOf(program, current)) |scalar| return .{ .scalar = scalar };
        switch (program.store.get(current)) {
            .nominal => |nominal| {
                if (!peelable(nominal)) invariantFmt("a generated codec reached the nominal {s}, which it cannot walk", .{program.store.nameText(nominal.name)});
                current = try program.converter().backing(current);
            },
            .list => return .{ .list = current },
            .box => |payload| return .{ .box = payload },
            .tuple => return .{ .tuple = current },
            .record, .empty_record => return .{ .record = current },
            .tag_union => return .{ .tag_union = current },
            .primitive, .function, .variable => invariant("a generated codec reached a type no codec reads or writes"),
        }
    }
}

fn recordFields(program: *Program, record_ty: TypeId) []const types.Field {
    return switch (program.store.get(record_ty)) {
        .record => |record| program.store.fieldSpan(record.fields),
        .empty_record => &.{},
        else => invariant("a generated record codec's shape is not a record"),
    };
}

/// A dict key the format renders as a key string: a scalar, or a structural
/// union of payload-free tags.
fn keyIsUnitTags(program: *Program, ty: TypeId) bool {
    const tag_union = switch (program.store.get(ty)) {
        .tag_union => |tag_union| tag_union,
        else => return false,
    };
    const tags = program.store.tagSpan(tag_union.tags);
    if (tags.len == 0) return false;
    for (tags) |tag| {
        if (tag.payloads.len != 0) return false;
    }
    return true;
}

fn keyIsStringRendered(program: *Program, ty: TypeId) bool {
    return scalarOf(program, ty) != null or keyIsUnitTags(program, ty);
}

/// Record every record the family reaches, in the order the checked codec
/// visits them (`buildCodecPrecomputedPlan` order), with an explicit stack.
fn planNames(program: *Program, family_index: u32) Allocator.Error!void {
    const allocator = program.allocator;
    const kind = program.codec.families.items[family_index].kind;
    const Step = struct { ty: TypeId, field_payload: bool };
    var seen: std.AutoHashMapUnmanaged(TypeId, void) = .empty;
    defer seen.deinit(allocator);
    var pending: std.ArrayList(Step) = .empty;
    defer pending.deinit(allocator);
    try pending.append(allocator, .{ .ty = program.codec.families.items[family_index].shape, .field_payload = false });
    while (pending.pop()) |step| {
        var ty = step.ty;
        if (step.field_payload) {
            if (try missingTryOk(program, ty)) |ok| ty = ok;
        }
        if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
        if (kind == .parser) {
            if (try missingTryOk(program, ty)) |ok| {
                try pending.append(allocator, .{ .ty = ok, .field_payload = false });
                continue;
            }
        }
        const start = pending.items.len;
        switch (try classify(program, family_index, ty)) {
            .null_try => |null_try| try pending.append(allocator, .{ .ty = null_try.ok, .field_payload = false }),
            .custom, .scalar => {},
            .set => |elem| try pending.append(allocator, .{ .ty = elem, .field_payload = false }),
            .dict => |dict| {
                try pending.append(allocator, .{ .ty = dict.value, .field_payload = false });
                if (!keyIsStringRendered(program, dict.key)) try pending.append(allocator, .{ .ty = dict.key, .field_payload = false });
            },
            .list => |list_ty| try pending.append(allocator, .{ .ty = program.store.get(list_ty).list, .field_payload = false }),
            .box => |payload| try pending.append(allocator, .{ .ty = payload, .field_payload = false }),
            .tuple => |tuple_ty| for (program.store.span(program.store.get(tuple_ty).tuple)) |item| {
                try pending.append(allocator, .{ .ty = item, .field_payload = false });
            },
            .record => |record_ty| {
                const family = &program.codec.families.items[family_index];
                const names_entry = try family.record_names.getOrPut(allocator, record_ty);
                if (!names_entry.found_existing) {
                    names_entry.value_ptr.* = @intCast(family.names.items.len);
                    for (recordFields(program, record_ty)) |field| try family.names.append(allocator, .{ .record = record_ty, .label = field.name });
                }
                for (recordFields(program, record_ty)) |field| try pending.append(allocator, .{ .ty = field.ty, .field_payload = kind == .encoder });
            },
            .tag_union => |union_ty| for (program.store.tagSpan(program.store.get(union_ty).tag_union.tags)) |tag| {
                for (program.store.span(tag.payloads)) |payload| try pending.append(allocator, .{ .ty = payload, .field_payload = false });
            },
        }
        // Components pop in source order.
        std.mem.reverse(Step, pending.items[start..]);
    }
    const family = &program.codec.families.items[family_index];
    if (family.names.items.len == 0) {
        family.names_ty = try program.store.emptyRecord();
    } else {
        const str = try program.store.primitive(.str);
        const items = try allocator.alloc(TypeId, family.names.items.len);
        defer allocator.free(items);
        @memset(items, str);
        family.names_ty = try program.store.intern(.{ .tuple = items });
    }
}

// -- Pieces --

/// The worker of `piece`, created on first request.
fn pieceWorker(program: *Program, module: ModuleIndex, piece: Piece, params: []const TypeId, ret: TypeId, captures: []const TypeId, erased: bool) Allocator.Error!WorkerId {
    const scheme_vars: []const checked.CheckedTypeId = if (piece.family == no_family) &.{} else program.codec.families.items[piece.family].env.vars;
    const state = &program.codec;
    if (state.piece_ids.get(piece)) |existing| return existing;
    const arena = program.arena.allocator();
    const piece_index: u32 = @intCast(state.pieces.items.len);
    try state.pieces.append(program.allocator, piece);
    try state.piece_captures.append(program.allocator, try arena.dupe(TypeId, captures));
    const key: lower.WorkerKey = .{ .codec = piece_index };
    const id: WorkerId = @fromBackingInt(@intCast(program.workers.items.len));
    const proc = try program.result.store.addProcSpec(.{
        .name = program.result.store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.programLocal("boxy.codec", @backingInt(id)),
        .args = .empty(),
        .ret_layout = .zst,
        .abi = if (erased) .erased_record else .roc,
    }, base.SourceLoc.none);
    try program.workers.append(program.allocator, .{
        .key = key,
        .proc = proc,
        .module = module,
        .scheme_vars = scheme_vars,
        .dictionary_params = &.{},
        .params = try arena.dupe(TypeId, params),
        .ret = ret,
        .capture_types = try arena.dupe(TypeId, captures),
    });
    try program.worker_ids.put(program.allocator, key, id);
    try program.worklist.append(program.allocator, id);
    try state.piece_ids.put(program.allocator, piece, id);
    return id;
}

fn constructorWorker(program: *Program, family_index: u32) Allocator.Error!WorkerId {
    const family = program.codec.families.items[family_index];
    return pieceWorker(program, family.module, .{ .family = family_index, .kind = .constructor }, fnArgs(program, family.constructor_fn), family.runtime_fn, &.{}, true);
}

fn runtimeCaptures(program: *Program, family_index: u32) Allocator.Error![]const TypeId {
    const family = program.codec.families.items[family_index];
    const captures = try program.arena.allocator().alloc(TypeId, 1 + family.names.items.len);
    captures[0] = family.encoding;
    @memset(captures[1..], try program.store.primitive(.str));
    return captures;
}

fn runtimeWorker(program: *Program, family_index: u32) Allocator.Error!WorkerId {
    const family = program.codec.families.items[family_index];
    const runtime = functionOf(program, family.runtime_fn);
    return pieceWorker(program, family.module, .{ .family = family_index, .kind = .runtime }, program.store.span(runtime.args), runtime.ret, try runtimeCaptures(program, family_index), true);
}

fn helperWorker(program: *Program, family_index: u32, kind: Kind, value_ty: TypeId, ret: TypeId) Allocator.Error!WorkerId {
    const family = program.codec.families.items[family_index];
    const params: []const TypeId = switch (kind) {
        .encoder => &.{ value_ty, family.state, family.encoding, family.names_ty },
        .parser => &.{ family.state, family.encoding, family.names_ty },
    };
    const piece_kind: PieceKind = switch (kind) {
        .encoder => .encode_helper,
        .parser => .parse_helper,
    };
    return pieceWorker(program, family.module, .{ .family = family_index, .kind = piece_kind, .a = @backingInt(value_ty), .b = @backingInt(ret) }, params, ret, &.{}, false);
}

/// An erased callback of the family: `captures` are the captured value's
/// type (when it has one) followed by the encoding and the names.
fn callbackWorker(program: *Program, family_index: u32, piece: Piece, fn_ty: TypeId, values: []const TypeId) Allocator.Error!WorkerId {
    const family = program.codec.families.items[family_index];
    const captures = try std.mem.concat(program.allocator, TypeId, &.{ values, &.{ family.encoding, family.names_ty } });
    defer program.allocator.free(captures);
    const function = functionOf(program, fn_ty);
    return pieceWorker(program, family.module, piece, program.store.span(function.args), function.ret, captures, true);
}

// -- Hooks --

/// The dictionary of structural parser or encoder evidence: the erased
/// constructor of the evidence's derivation.
pub fn dictionary(b: *Builder, structural: static_dispatch.StructuralEvidence) Allocator.Error!LocalId {
    const id = structural.generated_codec_derivation orelse invariant("structural codec evidence names no generated derivation");
    const family = try familyFor(b.program, b.module, id);
    const descs = try familyDescs(b, family, b.program.codec.families.items[family].constructor_fn, try b.convert(structural.callable_ty));
    defer b.allocator.free(descs);
    return try b.composeDictionary(try constructorWorker(b.program, family), descs);
}

/// The dictionary of generated derivation `id` of `module_index` at the
/// method callable type `callable`.
pub fn dictionaryAt(b: *Builder, module_index: ModuleIndex, id: static_dispatch.GeneratedCodecDerivationId, callable: TypeId) Allocator.Error!LocalId {
    const family = try familyFor(b.program, module_index, id);
    const descs = try familyDescs(b, family, b.program.codec.families.items[family].constructor_fn, callable);
    defer b.allocator.free(descs);
    return try b.composeDictionary(try constructorWorker(b.program, family), descs);
}

/// The descriptors of a family's variables at a use whose type `actual`
/// instantiates the family's `formal` type.
fn familyDescs(b: *Builder, family: u32, formal: TypeId, actual: TypeId) Allocator.Error![]LocalId {
    const count = b.program.codec.families.items[family].env.vars.len;
    const bound = try b.relateVariables(formal, actual, count);
    defer b.allocator.free(bound);
    const descs = try b.allocator.alloc(LocalId, count);
    for (bound, descs) |ty, *desc| desc.* = try b.descOf(ty);
    return descs;
}

/// A structural `parser_for`/`encoder_for` dispatch: the derivation's
/// constructor applied to the encoding operand.
pub fn dispatch(b: *Builder, call_plan: static_dispatch.StaticDispatchCallPlan, operands: []const static_dispatch.StaticDispatchOperand, operand_types: []const TypeId, result_ty: TypeId) Allocator.Error!void {
    const id = call_plan.generated_codec_derivation orelse invariant("a structural codec dispatch names no generated derivation");
    if (operands.len != 1) invariant("a structural codec dispatch does not take one encoding");
    const family = try familyFor(b.program, b.module, id);
    const fn_ty = try b.convert(call_plan.callable_ty);
    const descs = try familyDescs(b, family, b.program.codec.families.items[family].constructor_fn, fn_ty);
    defer b.allocator.free(descs);
    const constructor = try b.composeDictionary(try constructorWorker(b.program, family), descs);
    try b.push(.{ .finish_erased_call = .{ .fn_ty = fn_ty, .arg_count = 1, .result = result_ty } });
    try b.pushOperand(operands[0], operand_types[0]);
    try b.pushValue(constructor);
}

/// Restore a stored generated runtime: its family's runtime callable over
/// the stored encoding and renamed names. Compile-time evaluation numbered
/// those captures in the family's name order after the encoding.
pub fn restoreRuntime(b: *Builder, source: lower.ConstSource, fn_id: check.ConstStore.ConstFnId, owner: check.CheckedNames.ProcTemplate, expr: checked.CheckedExprId, fn_ty: TypeId) Allocator.Error!void {
    const program = b.program;
    const stored = program.constStore(source).getFn(fn_id);
    const owner_view = program.moduleByKey(.{ .bytes = owner.artifact.bytes });
    const plan_id = switch (owner_view.bodies().expr(expr).data) {
        .dispatch_call, .type_dispatch_call => |maybe| maybe orelse invariant("a stored generated codec runtime's dispatch has no plan"),
        else => invariant("a stored generated codec runtime does not name a dispatch"),
    };
    const plan = owner_view.view.static_dispatch_plans.plans[@backingInt(plan_id)];
    // A dispatch on a constrained variable reaches its derivation through
    // the evidence the stored value's procedure was instantiated with.
    const derivation_module, const derivation = if (plan.generated_codec_derivation) |id| .{ owner_view.index, id } else switch (plan.resolution) {
        .evidence_dependent => |dependent| blk: {
            var frame_index = stored.evidence_frame_head orelse invariant("a stored generated codec runtime has no evidence");
            for (0..dependent.index.depth) |_| frame_index = stored.evidence_frames[frame_index].parent orelse
                invariant("a stored generated codec runtime's evidence has fewer frames than its dispatch names");
            const frame = stored.evidence_frames[frame_index];
            if (dependent.index.index >= frame.roots_len) invariant("a stored generated codec runtime's evidence frame lacks its dispatch's parameter");
            const roots = try b.storedEvidenceRoots(stored.evidence, frame.roots_start, frame.roots_len);
            defer b.allocator.free(roots);
            const structural = switch (stored.evidence.nodes[roots[dependent.index.index]]) {
                .structural => |index| stored.evidence.structural(index).*,
                else => invariant("a stored generated codec runtime's evidence is not a structural derivation"),
            };
            const recorded = structural.checked orelse invariant("stored structural codec evidence records no checked edge");
            break :blk .{ program.moduleByKey(.{ .bytes = recorded.view.bytes }).index, recorded.generated_codec_derivation orelse
                invariant("stored structural codec evidence names no generated derivation") };
        },
        else => invariant("a stored generated codec runtime's dispatch names no derivation"),
    };
    const family = try familyFor(program, derivation_module, derivation);
    const worker = try runtimeWorker(program, family);
    const captures = try runtimeCaptures(program, family);
    const descs = try familyDescs(b, family, program.codec.families.items[family].runtime_fn, fn_ty);
    try b.push(.{ .finish_restore_closure = .{ .worker = worker, .count = @intCast(captures.len), .descs = try program.arena.allocator().dupe(LocalId, descs), .dicts = &.{}, .capture_types = captures, .worker_ty = fn_ty, .ty = fn_ty } });
    b.allocator.free(descs);
    var index = captures.len;
    while (index > 0) {
        index -= 1;
        const needle = check.CheckedIds.CaptureId.generatedCheck(@intCast(index));
        const node = for (stored.captures) |capture| {
            if (capture.id == needle) break capture.value;
        } else null;
        if (node) |stored_node| {
            try b.push(.{ .restore = .{ .source = source, .node = stored_node, .ty = captures[index] } });
        } else if (index == 0 and try program.layouts.layoutOf(captures[0]) == .zst) {
            // Compile-time evaluation stores no zero-sized encoding.
            try b.push(.{ .push_value = try b.unitValue() });
        } else {
            invariant("a stored generated codec runtime lacks one of its captures");
        }
    }
}

/// The body of an intrinsic wrapper template's worker, whose explicit
/// parameters are `params`: the compiler-owned serialization values a
/// generated codec hands to format methods, and the descriptor-guided
/// inspection and equality.
pub fn lowerIntrinsic(b: *Builder, intrinsic: checked.IntrinsicId, params: []const LocalId) Allocator.Error!void {
    const worker = b.currentWorker().*;
    var gen: Gen = .{ .b = b, .family = no_family, .piece = no_family };
    const result = switch (intrinsic) {
        // A spec is the erased callable that parses its union's tags.
        .parse_tag_union => try b.emitErasedCall(params[0], try b.program.store.intern(.{ .function = .{ .args = worker.params[1..2], .ret = worker.ret, .effectful = false } }), params[1..2], worker.ret),
        .field_names_rename_fields => try gen.renameFields(params[0], params[1], worker.params[1], worker.ret),
        .field_names_shortest_name => try gen.nameBound(params[0], worker.params[0], .shortest),
        .field_names_longest_name => try gen.nameBound(params[0], worker.params[0], .longest),
        .field_names_iter => try gen.fieldNamesIter(params[0], null, worker.ret),
        .field_names_for_size => try gen.fieldNamesIter(params[0], params[1], worker.ret),
        .field_name => (try gen.field(params[0], worker.params[0], "name")).value,
        // The descriptor-guided walkers (design.md "Dynamic Values In LIR").
        .str_inspect => blk: {
            const text = try b.closedLocal(.str);
            try b.emitDyn(text, .{ .inspect = .{ .value = params[0], .desc = try b.descOf(worker.params[0]) } });
            break :blk text;
        },
        .structural_eq => blk: {
            const equal = try b.closedLocal(.bool);
            try b.emitDyn(equal, .{ .eq = .{ .lhs = params[0], .rhs = params[1], .desc = try b.descOf(worker.params[0]) } });
            break :blk equal;
        },
    };
    try b.push(.ret);
    try b.pushValue(result);
}

/// Lower piece `index` into the worker `b` builds.
pub fn lowerPiece(b: *Builder, index: u32) Allocator.Error!void {
    const piece = b.program.codec.pieces.items[index];
    var gen: Gen = .{ .b = b, .family = piece.family, .piece = index };
    switch (piece.kind) {
        .constructor => try gen.lowerConstructor(),
        .runtime => try gen.lowerRuntime(),
        .encode_helper => try gen.lowerEncodeHelper(@fromBackingInt(piece.a), @fromBackingInt(piece.b)),
        .parse_helper => try gen.lowerParseHelper(@fromBackingInt(piece.a), @fromBackingInt(piece.b)),
        .record_body => try gen.lowerRecordBody(@fromBackingInt(piece.a)),
        .tuple_body => try gen.lowerTupleBody(@fromBackingInt(piece.a)),
        .list_body => try gen.lowerListBody(.list),
        .dict_body => try gen.lowerListBody(.dict),
        .tag_body => try gen.lowerTagBody(@fromBackingInt(piece.a), @fromBackingInt(piece.b)),
        .value_thunk => try gen.lowerValueThunk(@fromBackingInt(piece.a)),
        .key_thunk => try gen.lowerKeyThunk(@fromBackingInt(piece.a)),
        .tag_spec => try gen.lowerTagSpec(@fromBackingInt(piece.a)),
        .field_names_step => try gen.lowerFieldNamesStep(@fromBackingInt(piece.a)),
    }
}

// -- Emission --

/// Where a failure goes: the procedure returns `Err` of its result type,
/// or a join takes the error payload.
const OnErr = union(enum) {
    ret: TypeId,
    join: struct { id: LIR.JoinPointId, param: LocalId, err_ty: TypeId },
};

const Typed = struct { value: LocalId, ty: TypeId };

const Gen = struct {
    b: *Builder,
    family: u32,
    piece: u32,
    /// The encoding and names of the worker being lowered.
    encoding: LocalId = undefined,
    names: LocalId = undefined,

    fn program(self: *Gen) *Program {
        return self.b.program;
    }

    fn store(self: *Gen) *types.Store {
        return &self.b.program.store;
    }

    fn fam(self: *Gen) *Family {
        return &self.b.program.codec.families.items[self.family];
    }

    fn name(self: *Gen, text: []const u8) Allocator.Error!NameId {
        return self.store().internName(text);
    }

    fn layoutOf(self: *Gen, ty: TypeId) Allocator.Error!layout.Idx {
        return self.b.program.layouts.layoutOf(ty);
    }

    fn local(self: *Gen, ty: TypeId) Allocator.Error!LocalId {
        return self.b.localOf(ty, .frame);
    }

    fn u64Ty(self: *Gen) Allocator.Error!TypeId {
        return self.store().primitive(.u64);
    }

    fn strTy(self: *Gen) Allocator.Error!TypeId {
        return self.store().primitive(.str);
    }

    fn boolTy(self: *Gen) Allocator.Error!TypeId {
        return self.store().primitive(.bool);
    }

    fn u64Value(self: *Gen, value: u64) Allocator.Error!LocalId {
        return self.b.intValue(value, try self.u64Ty());
    }

    fn lowLevel(self: *Gen, op: LIR.LowLevel, args: []const LocalId, result_ty: TypeId) Allocator.Error!LocalId {
        const target = try self.local(result_ty);
        try self.b.emitLowLevel(target, op, args);
        return target;
    }

    fn lowLevelLayout(self: *Gen, op: LIR.LowLevel, args: []const LocalId, result_layout: layout.Idx) Allocator.Error!LocalId {
        const target = try self.b.closedLocal(result_layout);
        try self.b.emitLowLevel(target, op, args);
        return target;
    }

    fn isEq(self: *Gen, a: LocalId, b_local: LocalId) Allocator.Error!LocalId {
        return self.lowLevelLayout(.num_is_eq, &.{ a, b_local }, .bool);
    }

    fn addOne(self: *Gen, value: LocalId) Allocator.Error!LocalId {
        return self.lowLevelLayout(.num_int_add_wrap, &.{ value, try self.u64Value(1) }, .u64);
    }

    fn subOne(self: *Gen, value: LocalId) Allocator.Error!LocalId {
        return self.lowLevelLayout(.num_int_sub_wrap, &.{ value, try self.u64Value(1) }, .u64);
    }

    fn ret(self: *Gen, value: LocalId) Allocator.Error!void {
        const b = self.b;
        if (b.result_local) |result| {
            try b.emitDyn(result, .{ .copy = .{ .src = value, .desc = b.store.localDyn(result).?.desc } });
            try b.emitTerminal(.{ .ret = .{ .value = result } });
        } else {
            try b.emitTerminal(.{ .ret = .{ .value = value } });
        }
    }

    // -- Control flow --

    /// Declare join `id` with `params` (and `retained` ownership units);
    /// continue in its remainder and return its body's hole.
    fn declareJoin(self: *Gen, id: LIR.JoinPointId, params: []const LocalId, retained: []const LocalId) Allocator.Error!CFStmtId {
        const b = self.b;
        const hole = try b.currentHole();
        const body = try b.newHole();
        const remainder = try b.newHole();
        const span = try b.store.addLocalSpan(params);
        const extra: LIR.StmtRowId(LIR.JoinExtra) = if (retained.len == 0)
            .none
        else
            try b.store.addStmtRow(LIR.JoinExtra, .{ .retained = try b.store.addLocalSpan(retained) });
        try b.fill(hole, .{ .join = .{ .id = id, .params = span, .extra = extra, .body = body, .remainder = remainder } }, .lowering_glue);
        try b.join_points.append(b.allocator, .{ .id = id, .params = span, .body = body });
        b.hole = remainder;
        return body;
    }

    /// Assign `values` to `params` and jump to `id`.
    fn jump(self: *Gen, id: LIR.JoinPointId, params: []const LocalId, values: []const LocalId) Allocator.Error!void {
        for (params, values) |param, value| try self.setParam(param, value);
        try self.b.emitTerminal(.{ .jump = .{ .target = id } });
    }

    fn setParam(self: *Gen, param: LocalId, value: LocalId) Allocator.Error!void {
        try self.b.emitLinear(.{ .set_local = .{ .target = param, .value = value, .mode = .initialize_join_param, .next = undefined } });
    }

    /// A merge point carrying one value of `ty`: returns the join, its
    /// parameter, and the hole that continues after it. The builder
    /// continues where the merging paths are emitted.
    const Merge = struct { id: LIR.JoinPointId, param: LocalId, after: CFStmtId };

    fn merge(self: *Gen, ty: TypeId) Allocator.Error!Merge {
        const id = self.b.freshJoin();
        const param = try self.local(ty);
        const after = try self.declareJoin(id, &.{param}, &.{});
        return .{ .id = id, .param = param, .after = after };
    }

    /// Continue when `cond` holds; return the hole of the other path.
    fn branchIf(self: *Gen, cond: LocalId) Allocator.Error!CFStmtId {
        const holes = try self.b.emitBoolSwitch(cond);
        self.b.hole = holes.then;
        return holes.otherwise;
    }

    /// Continue when `value` holds tag `text`; return the hole of the other
    /// tags.
    fn branchTag(self: *Gen, value: LocalId, ty: TypeId, text: []const u8) Allocator.Error!CFStmtId {
        const miss = try self.b.newHole();
        try self.b.branchOnTag(value, ty, try self.name(text), miss);
        return miss;
    }

    fn payload(self: *Gen, value: LocalId, ty: TypeId, text: []const u8, index: u32) Allocator.Error!Typed {
        const typed = try self.b.tagPayloadValue(value, ty, try self.name(text), index);
        return .{ .value = typed.value, .ty = typed.ty };
    }

    fn field(self: *Gen, value: LocalId, ty: TypeId, text: []const u8) Allocator.Error!Typed {
        const typed = try self.b.recordFieldValue(value, ty, try self.name(text));
        return .{ .value = typed.value, .ty = typed.ty };
    }

    fn tag(self: *Gen, ty: TypeId, text: []const u8, payloads: []const LocalId) Allocator.Error!LocalId {
        return self.b.buildTag(ty, try self.name(text), payloads);
    }

    /// The tags of a tag union, through nominal wrappers.
    fn structuralTags(self: *Gen, ty: TypeId) Allocator.Error![]const types.Tag {
        return unionTags(self.program(), try self.b.structuralType(ty));
    }

    // -- Try --

    /// `Try(ok, err)` at the declaration of `template`.
    fn tryType(self: *Gen, template: TypeId, ok: TypeId, err: TypeId) Allocator.Error!TypeId {
        const nominal = switch (self.store().get(template)) {
            .nominal => |nominal| nominal,
            else => invariant("a generated codec's Try template is not a nominal"),
        };
        const ty = try self.store().intern(.{ .nominal = .{
            .decl = nominal.decl,
            .args = &.{ ok, err },
            .name = nominal.name,
            .is_opaque = nominal.is_opaque,
            .builtin = nominal.builtin,
            .has_backing = nominal.has_backing,
        } });
        if (std.debug.runtime_safety) {
            const parts = try requireTry(self.program(), ty);
            std.debug.assert(parts.ok == ok and parts.err == err);
        }
        return ty;
    }

    fn tryOk(self: *Gen, try_ty: TypeId, value: LocalId) Allocator.Error!LocalId {
        return self.tag(try_ty, "Ok", &.{value});
    }

    fn tryErr(self: *Gen, try_ty: TypeId, err: LocalId) Allocator.Error!LocalId {
        return self.tag(try_ty, "Err", &.{err});
    }

    /// The `Ok` payload of `value`; on `Err`, its payload goes to `on_err`.
    fn unwrap(self: *Gen, value: LocalId, try_ty: TypeId, on_err: OnErr) Allocator.Error!LocalId {
        const parts = try requireTry(self.program(), try_ty);
        const err_hole = try self.branchTag(value, try_ty, "Ok");
        const ok = try self.payload(value, try_ty, "Ok", 0);
        const ok_hole = self.b.hole;
        self.b.hole = err_hole;
        try self.fail(value, try_ty, parts.err, on_err);
        self.b.hole = ok_hole;
        return ok.value;
    }

    /// The failure path of an `Err` value of `try_ty`.
    fn fail(self: *Gen, value: LocalId, try_ty: TypeId, err_ty: TypeId, on_err: OnErr) Allocator.Error!void {
        if ((try self.structuralTags(err_ty)).len == 0) {
            // A method whose error row is empty never returns `Err`.
            try self.b.emitTerminal(.runtime_error);
            return;
        }
        const err = try self.payload(value, try_ty, "Err", 0);
        try self.failWith(err.value, err_ty, on_err);
    }

    fn failWith(self: *Gen, err: LocalId, err_ty: TypeId, on_err: OnErr) Allocator.Error!void {
        switch (on_err) {
            .ret => |ret_ty| {
                const target = (try requireTry(self.program(), ret_ty)).err;
                const injected = try self.injectErr(err, err_ty, target);
                try self.ret(try self.tryErr(ret_ty, injected));
            },
            .join => |join| {
                const injected = try self.injectErr(err, err_ty, join.err_ty);
                try self.jump(join.id, &.{join.param}, &.{injected});
            },
        }
    }

    /// `err` (of union `from`) as a value of the union `to`, which includes
    /// every tag of `from` with the same payload types.
    fn injectErr(self: *Gen, err: LocalId, from: TypeId, to: TypeId) Allocator.Error!LocalId {
        if (from == to) return err;
        const from_structural = try self.b.structuralType(from);
        const tags = try self.b.allocator.dupe(types.Tag, try self.structuralTags(from_structural));
        defer self.b.allocator.free(tags);
        if (tags.len == 0) invariant("an empty error row was injected into another row");
        const merged = try self.merge(to);
        for (tags, 0..) |tag_info, index| {
            const miss: ?CFStmtId = if (index + 1 < tags.len) blk: {
                const hole = try self.b.newHole();
                try self.b.branchOnTag(err, from_structural, tag_info.name, hole);
                break :blk hole;
            } else null;
            const payload_count = tag_info.payloads.len;
            const payloads = try self.b.allocator.alloc(LocalId, payload_count);
            defer self.b.allocator.free(payloads);
            for (payloads, 0..) |*slot, payload_index| slot.* = (try self.b.tagPayloadValue(err, from_structural, tag_info.name, @intCast(payload_index))).value;
            const rebuilt = try self.b.buildTag(to, tag_info.name, payloads);
            try self.jump(merged.id, &.{merged.param}, &.{rebuilt});
            if (miss) |hole| self.b.hole = hole;
        }
        self.b.hole = merged.after;
        return merged.param;
    }

    /// `value` (a `Try` of `from`) as a `Try` of `to`, whose `Ok` payload
    /// type is the same and whose error row includes `from`'s.
    fn injectTry(self: *Gen, value: LocalId, from: TypeId, to: TypeId) Allocator.Error!LocalId {
        if (from == to) return value;
        const from_parts = try requireTry(self.program(), from);
        const merged = try self.merge(to);
        const err_hole = try self.branchTag(value, from, "Ok");
        const ok = try self.payload(value, from, "Ok", 0);
        try self.jump(merged.id, &.{merged.param}, &.{try self.tryOk(to, ok.value)});
        self.b.hole = err_hole;
        if ((try self.structuralTags(from_parts.err)).len == 0) {
            try self.b.emitTerminal(.runtime_error);
        } else {
            const err = try self.payload(value, from, "Err", 0);
            const injected = try self.injectErr(err.value, from_parts.err, (try requireTry(self.program(), to)).err);
            try self.jump(merged.id, &.{merged.param}, &.{try self.tryErr(to, injected)});
        }
        self.b.hole = merged.after;
        return merged.param;
    }

    // -- Calls --

    /// Call the contract's `method` for `subject` with `args`.
    fn callFormat(self: *Gen, method: []const u8, subject: ?TypeId, args: []const LocalId) Allocator.Error!Typed {
        const resolved = try lookupCall(self.program(), self.family, method, subject);
        return self.callResolved(resolved, args);
    }

    fn callResolved(self: *Gen, resolved: Resolved, args: []const LocalId) Allocator.Error!Typed {
        const b = self.b;
        const program_ = self.program();
        switch (resolved) {
            .structural => invariant("a generated codec called a nested structural derivation as a procedure"),
            .checked_error => |fn_ty| {
                const ret_ty = fnRet(program_, fn_ty);
                const literal = try b.store.insertString("runtime error");
                try b.emitTerminal(.{ .crash = .{ .msg = .{ .literal = literal }, .checked_error = true } });
                return .{ .value = try self.local(ret_ty), .ty = ret_ty };
            },
            .callable => |callable| {
                const table = program_.module(self.fam().module).view.static_dispatch_plans;
                const node = table.evidenceNode(callable.node);
                const template = switch (node.target.kind) {
                    .procedure => |procedure| procedure.template,
                    .local_proc => invariant("generated codec calls of local procedures are not yet lowered by Boxy"),
                    .structural => invariant("a generated codec call's callable evidence names a structural target"),
                };
                const callee = try program_.templateRefWorker(template);
                // The edge's substitution names each scheme variable's
                // instantiation, and the target's own evidence is the edge's
                // nested evidence (design.md "Workers And Hidden Parameters").
                const target_worker = program_.worker(callee).*;
                const nested: []const static_dispatch.CheckedEvidence = if (target_worker.dictionary_params.len == 0) &.{} else switch (node.nested) {
                    .resolved => table.nestedEvidence(node),
                    .from_callable, .through_alias => invariant("a generated codec call's evidence is not resolved"),
                };
                const subst = table.site_substitutions[node.subst.start..][0..node.subst.len];
                const hidden = switch (node.instantiation) {
                    .alias => invariant("generated codec calls of targets reached through a procedure alias are not yet lowered by Boxy"),
                    .derived_from_callable => |callable_ty| try b.relatedHiddenArgs(callee, callable_ty),
                    .callable, .monomorphic => try b.hiddenArgsAt(callee, subst, nested, callable.fn_ty),
                };
                defer b.allocator.free(hidden);
                const ret_ty = fnRet(program_, callable.fn_ty);
                // A target storing record fields of its signature in presence
                // slots is called at its own storage, adapting the arguments
                // and the result (design.md "Field Kinds").
                const bound: ?[]TypeId = if (!try program_.signatureHasSlots(callee)) null else switch (node.instantiation) {
                    .alias => unreachable, // rejected above
                    .derived_from_callable => |callable_ty| try b.relateVariables(
                        try program_.store.intern(.{ .function = .{ .args = target_worker.params, .ret = target_worker.ret, .effectful = false } }),
                        try b.convert(callable_ty),
                        target_worker.scheme_vars.len,
                    ),
                    .callable, .monomorphic => try b.boundTypesAt(callee, subst, callable.fn_ty),
                };
                defer if (bound) |types_| b.allocator.free(types_);
                const call_args = try b.allocator.dupe(LocalId, args);
                defer b.allocator.free(call_args);
                if (bound) |types_| {
                    for (call_args, fnArgs(program_, callable.fn_ty), target_worker.params) |*arg, arg_ty, param_ty| {
                        arg.* = try b.adaptValue(arg.*, arg_ty, try program_.substitute(b.allocator, param_ty, types_));
                    }
                }
                const own_ret = if (bound) |types_| try program_.substitute(b.allocator, target_worker.ret, types_) else ret_ty;
                const all = try std.mem.concat(b.allocator, LocalId, &.{ call_args, hidden });
                defer b.allocator.free(all);
                const target = try self.local(own_ret);
                try b.emitLinear(.{ .assign_call = .{ .target = target, .proc = target_worker.proc, .args = try b.store.addLocalSpan(all), .next = undefined } });
                return .{ .value = try b.adaptValue(target, own_ret, ret_ty), .ty = ret_ty };
            },
        }
    }

    /// Call the family helper for `value_ty` at result type `ret_ty`.
    fn callHelper(self: *Gen, kind: Kind, value_ty: TypeId, ret_ty: TypeId, explicit: []const LocalId) Allocator.Error!LocalId {
        const worker = try helperWorker(self.program(), self.family, kind, value_ty, ret_ty);
        const all = try std.mem.concat(self.b.allocator, LocalId, &.{ explicit, &.{ self.encoding, self.names }, self.b.var_descs });
        defer self.b.allocator.free(all);
        const target = try self.local(ret_ty);
        try self.b.emitLinear(.{ .assign_call = .{ .target = target, .proc = self.program().worker(worker).proc, .args = try self.b.store.addLocalSpan(all), .next = undefined } });
        return target;
    }

    /// Pack callback `worker` capturing `values` (of `value_types`), then
    /// the encoding and the names.
    fn packCallback(self: *Gen, worker: WorkerId, values: []const LocalId, value_types: []const TypeId) Allocator.Error!LocalId {
        const fam_ = self.fam().*;
        const captured = try std.mem.concat(self.b.allocator, LocalId, &.{ values, &.{ self.encoding, self.names } });
        defer self.b.allocator.free(captured);
        const capture_types = try std.mem.concat(self.b.allocator, TypeId, &.{ value_types, &.{ fam_.encoding, fam_.names_ty } });
        defer self.b.allocator.free(capture_types);
        return self.b.packClosure(worker, captured, capture_types, self.b.var_descs, &.{});
    }

    /// Renamed name `index` of the family's names.
    fn renamed(self: *Gen, index: u32) Allocator.Error!LocalId {
        return self.b.readField(self.fam().names_ty, self.names, index, try self.strTy());
    }

    fn recordNameBase(self: *Gen, record_ty: TypeId) u32 {
        return self.fam().record_names.get(record_ty) orelse invariant("a generated codec reached a record whose names it never renamed");
    }

    // -- Worker frames --

    const Direct = struct { entry: CFStmtId, body_start: CFStmtId, params: []LocalId, ret_layout: layout.Idx };

    /// A worker's result layout: its committed layout, or a dynamic result
    /// written through the result local.
    fn resultLayout(self: *Gen, ret_ty: TypeId) Allocator.Error!layout.Idx {
        const b = self.b;
        if (try b.closed(ret_ty)) return self.layoutOf(ret_ty);
        b.result_local = try b.localOf(ret_ty, .result);
        return b.program.result.layouts.dynamicLayout();
    }

    fn beginDirect(self: *Gen) Allocator.Error!Direct {
        const b = self.b;
        const worker = b.currentWorker().*;
        b.entry_hole = try b.newHole();
        const entry = b.entry_hole;
        // Explicit parameters, then the family's descriptors.
        const params = try b.allocator.alloc(LocalId, worker.params.len + worker.scheme_vars.len);
        b.var_descs = try b.program.arena.allocator().alloc(LocalId, worker.scheme_vars.len);
        for (b.var_descs, params[worker.params.len..]) |*desc, *param| {
            desc.* = try b.closedLocal(.opaque_ptr);
            param.* = desc.*;
        }
        for (params[0..worker.params.len], worker.params) |*param, ty| param.* = try b.localOf(ty, .param);
        const ret_layout = try self.resultLayout(worker.ret);
        const body_start = try b.newHole();
        b.hole = body_start;
        return .{ .entry = entry, .body_start = body_start, .params = params, .ret_layout = ret_layout };
    }

    fn finishDirect(self: *Gen, frame: Direct) Allocator.Error!void {
        defer self.b.allocator.free(frame.params);
        try self.b.finishProc(frame.entry, frame.body_start, frame.params, frame.ret_layout);
    }

    const Erased = struct {
        start: Builder.ErasedStart,
        record: ?LocalId,
        ret_layout: layout.Idx,
        body_start: CFStmtId,
        args: []LocalId,
        captures: []LocalId,
    };

    fn beginErased(self: *Gen) Allocator.Error!Erased {
        const b = self.b;
        const worker = b.currentWorker().*;
        // Captured values, then the family's descriptors.
        const capture_layouts = try b.allocator.alloc(layout.Idx, worker.capture_types.len + worker.scheme_vars.len);
        defer b.allocator.free(capture_layouts);
        var all_zero_sized = true;
        for (capture_layouts, 0..) |*slot, index| {
            slot.* = if (index < worker.capture_types.len) try self.layoutOf(worker.capture_types[index]) else .opaque_ptr;
            if (slot.* != .zst) all_zero_sized = false;
        }
        // A capture record of zero-sized values has no storage to read.
        var start = try b.beginErasedWorker(if (all_zero_sized) &.{} else capture_layouts);
        if (all_zero_sized and capture_layouts.len != 0) {
            b.allocator.free(start.captures);
            start.captures = try b.allocator.alloc(LocalId, capture_layouts.len);
            for (start.captures) |*capture| {
                capture.* = try b.closedLocal(.zst);
                try b.emitEntry(.{ .assign_struct = .{ .target = capture.*, .fields = LIR.LocalSpan.empty(), .next = undefined } });
            }
        }
        b.var_descs = try b.program.arena.allocator().dupe(LocalId, start.captures[worker.capture_types.len..]);
        var record: ?LocalId = null;
        if (worker.params.len != 0) record = try b.argsRecordLocal(worker.params, .param);
        const ret_layout = try self.resultLayout(worker.ret);
        const body_start = try b.newHole();
        b.hole = body_start;
        const args = try b.allocator.alloc(LocalId, worker.params.len);
        for (args, worker.params, 0..) |*arg, ty, index| arg.* = try b.argsRecordField(worker.params, record.?, @intCast(index), ty);
        return .{ .start = start, .record = record, .ret_layout = ret_layout, .body_start = body_start, .args = args, .captures = start.captures[0..worker.capture_types.len] };
    }

    fn finishErased(self: *Gen, frame: Erased) Allocator.Error!void {
        const b = self.b;
        defer b.allocator.free(frame.args);
        defer b.allocator.free(frame.start.captures);
        var proc_args: std.ArrayList(LocalId) = .empty;
        defer proc_args.deinit(b.allocator);
        if (frame.record) |record| try proc_args.append(b.allocator, record);
        try proc_args.append(b.allocator, frame.start.capture_ptr);
        try b.finishProc(frame.start.entry, frame.body_start, proc_args.items, frame.ret_layout);
        const proc = b.store.getProcSpecPtr(b.currentWorker().proc);
        proc.erased_capture_arg = frame.start.capture_ptr;
        proc.abi = .erased_record;
    }

    /// Bind a callback's trailing encoding and names captures.
    fn bindCallbackCaptures(self: *Gen, captures: []const LocalId) []const LocalId {
        self.encoding = captures[captures.len - 2];
        self.names = captures[captures.len - 1];
        return captures[0 .. captures.len - 2];
    }

    // -- Constructor and runtime --

    fn lowerConstructor(self: *Gen) Allocator.Error!void {
        const frame = try self.beginErased();
        const program_ = self.program();
        const encoding = frame.args[0];
        const entries = try self.b.allocator.dupe(NameEntry, self.fam().names.items);
        defer self.b.allocator.free(entries);
        const captured = try self.b.allocator.alloc(LocalId, 1 + entries.len);
        defer self.b.allocator.free(captured);
        captured[0] = encoding;
        for (entries, captured[1..]) |entry, *slot| {
            const label = try self.b.strLiteral(self.store().nameText(entry.label));
            slot.* = (try self.callFormat("rename_field", null, &.{ encoding, label })).value;
        }
        const runtime = try runtimeWorker(program_, self.family);
        const closure = try self.b.packClosure(runtime, captured, program_.worker(runtime).capture_types, self.b.var_descs, &.{});
        try self.ret(closure);
        try self.finishErased(frame);
    }

    fn lowerRuntime(self: *Gen) Allocator.Error!void {
        const frame = try self.beginErased();
        const fam_ = self.fam().*;
        self.encoding = frame.captures[0];
        self.names = try self.b.buildStruct(fam_.names_ty, frame.captures[1..]);
        const runtime = functionOf(self.program(), fam_.runtime_fn);
        const result = switch (fam_.kind) {
            .encoder => try self.callHelper(.encoder, fam_.shape, runtime.ret, frame.args),
            .parser => try self.callHelper(.parser, fam_.shape, runtime.ret, frame.args),
        };
        try self.ret(result);
        try self.finishErased(frame);
    }

    // -- Encoders --

    fn lowerEncodeHelper(self: *Gen, value_ty: TypeId, ret_ty: TypeId) Allocator.Error!void {
        const frame = try self.beginDirect();
        self.encoding = frame.params[2];
        self.names = frame.params[3];
        const result = try self.encodeShape(value_ty, frame.params[0], frame.params[1], ret_ty);
        try self.ret(result);
        try self.finishDirect(frame);
    }

    /// Write `value` of `value_ty` into `state`, producing `ret_ty`.
    fn encodeShape(self: *Gen, value_ty: TypeId, value: LocalId, state: LocalId, ret_ty: TypeId) Allocator.Error!LocalId {
        const program_ = self.program();
        switch (try classify(program_, self.family, value_ty)) {
            .null_try => |null_try| {
                const merged = try self.merge(ret_ty);
                const err_hole = try self.branchTag(value, value_ty, "Ok");
                const ok = try self.payload(value, value_ty, "Ok", 0);
                const written = try self.callHelper(.encoder, null_try.ok, ret_ty, &.{ ok.value, state });
                try self.jump(merged.id, &.{merged.param}, &.{written});
                self.b.hole = err_hole;
                const null_written = try self.callFormat("encode_null", null, &.{state});
                try self.jump(merged.id, &.{merged.param}, &.{try self.injectTry(null_written.value, null_written.ty, ret_ty)});
                self.b.hole = merged.after;
                return merged.param;
            },
            .dict => {
                const entries = try self.callFormat("to_list", value_ty, &.{value});
                const len = try self.lowLevelLayout(.list_len, &.{entries.value}, .u64);
                return try self.encodeContainer("encode_dict", value_ty, .dict_body, @backingInt(value_ty), 0, &.{entries.value}, &.{entries.ty}, &.{ state, len }, ret_ty);
            },
            .set => {
                const list = try self.callFormat("to_list", value_ty, &.{value});
                // A set is written as a list whose contract subject is the set.
                return try self.encodeList(list.ty, value_ty, list.value, state, ret_ty);
            },
            .custom => |resolved| {
                const encoder = try self.callResolved(resolved, &.{self.encoding});
                const runtime = functionOf(program_, encoder.ty);
                const written = try self.b.emitErasedCall(encoder.value, encoder.ty, &.{ value, state }, runtime.ret);
                return try self.injectTry(written, runtime.ret, ret_ty);
            },
            .list => |list_ty| return try self.encodeList(list_ty, list_ty, value, state, ret_ty),
            .tuple => |tuple_ty| {
                const count = try self.u64Value(program_.store.span(program_.store.get(tuple_ty).tuple).len);
                return try self.encodeContainer("encode_tuple", tuple_ty, .tuple_body, @backingInt(tuple_ty), 0, &.{value}, &.{tuple_ty}, &.{ state, count }, ret_ty);
            },
            .box => |payload_ty| {
                const unboxed = try self.local(payload_ty);
                try self.b.emitLowLevel(unboxed, .box_unbox, &.{value});
                return try self.callHelper(.encoder, payload_ty, ret_ty, &.{ unboxed, state });
            },
            .scalar => |scalar| {
                var buf: [32]u8 = undefined;
                const method = std.fmt.bufPrint(&buf, "encode_{s}", .{@tagName(scalar)}) catch unreachable; // scalar names are short
                const written = try self.callFormat(method, value_ty, &.{ value, state });
                return try self.injectTry(written.value, written.ty, ret_ty);
            },
            .record => |record_ty| {
                const count = try self.u64Value(recordFields(program_, record_ty).len);
                return try self.encodeContainer("encode_record", record_ty, .record_body, @backingInt(record_ty), 0, &.{value}, &.{record_ty}, &.{ state, count }, ret_ty);
            },
            .tag_union => |union_ty| return try self.encodeTagUnion(union_ty, value, state, ret_ty),
        }
    }

    fn encodeList(self: *Gen, list_ty: TypeId, subject: TypeId, list: LocalId, state: LocalId, ret_ty: TypeId) Allocator.Error!LocalId {
        const len = try self.lowLevelLayout(.list_len, &.{list}, .u64);
        return try self.encodeContainer("encode_list", subject, .list_body, @backingInt(list_ty), 0, &.{list}, &.{list_ty}, &.{ state, len }, ret_ty);
    }

    /// Call container method `method` (for `subject`) with `leading`
    /// arguments and the body callback `body_kind` capturing `captured`.
    fn encodeContainer(
        self: *Gen,
        method: []const u8,
        subject: TypeId,
        body_kind: PieceKind,
        a: u32,
        b_arg: u32,
        captured: []const LocalId,
        captured_types: []const TypeId,
        leading: []const LocalId,
        ret_ty: TypeId,
    ) Allocator.Error!LocalId {
        const program_ = self.program();
        const resolved = try lookupCall(program_, self.family, method, subject);
        const method_fn = switch (resolved) {
            .callable => |callable| callable.fn_ty,
            .checked_error => |fn_ty| fn_ty,
            .structural => invariant("a container encoder method resolved to a structural derivation"),
        };
        const method_args = fnArgs(program_, method_fn);
        const body_ty = method_args[method_args.len - 1];
        const body_worker = try callbackWorker(program_, self.family, .{ .family = self.family, .kind = body_kind, .a = a, .b = b_arg }, body_ty, captured_types);
        const body = try self.packCallback(body_worker, captured, captured_types);
        const all = try std.mem.concat(self.b.allocator, LocalId, &.{ leading, &.{body} });
        defer self.b.allocator.free(all);
        const written = try self.callResolved(resolved, all);
        return try self.injectTry(written.value, written.ty, ret_ty);
    }

    fn encodeTagUnion(self: *Gen, union_ty: TypeId, value: LocalId, state: LocalId, ret_ty: TypeId) Allocator.Error!LocalId {
        const program_ = self.program();
        const tags = try self.b.allocator.dupe(types.Tag, unionTags(program_, union_ty));
        defer self.b.allocator.free(tags);
        var inhabited: std.ArrayList(types.Tag) = .empty;
        defer inhabited.deinit(self.b.allocator);
        for (tags) |tag_info| {
            if (!try tagUninhabited(program_, tag_info)) try inhabited.append(self.b.allocator, tag_info);
        }
        if (inhabited.items.len == 0) {
            // No value has a tag of this union.
            try self.b.emitTerminal(.runtime_error);
            return try self.local(ret_ty);
        }
        const merged = try self.merge(ret_ty);
        for (inhabited.items, 0..) |tag_info, index| {
            const miss: ?CFStmtId = if (index + 1 < inhabited.items.len) blk: {
                const hole = try self.b.newHole();
                try self.b.branchOnTag(value, union_ty, tag_info.name, hole);
                break :blk hole;
            } else null;
            const payload_types = try self.b.allocator.dupe(TypeId, program_.store.span(tag_info.payloads));
            defer self.b.allocator.free(payload_types);
            const payloads = try self.b.allocator.alloc(LocalId, payload_types.len);
            defer self.b.allocator.free(payloads);
            for (payloads, 0..) |*slot, payload_index| slot.* = (try self.b.tagPayloadValue(value, union_ty, tag_info.name, @intCast(payload_index))).value;
            const tag_name = try self.b.strLiteral(program_.store.nameText(tag_info.name));
            const count = try self.u64Value(payload_types.len);
            const written = try self.encodeContainer("encode_tag", union_ty, .tag_body, @backingInt(union_ty), @backingInt(tag_info.name), payloads, payload_types, &.{ state, tag_name, count }, ret_ty);
            try self.jump(merged.id, &.{merged.param}, &.{written});
            if (miss) |hole| self.b.hole = hole;
        }
        self.b.hole = merged.after;
        return merged.param;
    }

    /// A container body callback: `(container_state, writer) -> Try`.
    const BodyFrame = struct { frame: Erased, values: []const LocalId, state: LocalId, writer: LocalId, writer_ty: TypeId, ret_ty: TypeId };

    fn beginBody(self: *Gen) Allocator.Error!BodyFrame {
        const frame = try self.beginErased();
        const values = self.bindCallbackCaptures(frame.captures);
        const worker = self.b.currentWorker().*;
        return .{ .frame = frame, .values = values, .state = frame.args[0], .writer = frame.args[1], .writer_ty = worker.params[1], .ret_ty = worker.ret };
    }

    /// Write one item through `writer`: its value-writing thunk, preceded by
    /// `leading` arguments. Returns the container state after it.
    fn writeItem(self: *Gen, body: BodyFrame, state: LocalId, leading: []const LocalId, value: LocalId, value_ty: TypeId) Allocator.Error!LocalId {
        const program_ = self.program();
        const writer_args = fnArgs(program_, body.writer_ty);
        const thunk_ty = writer_args[writer_args.len - 1];
        const thunk_worker = try callbackWorker(program_, self.family, .{ .family = self.family, .kind = .value_thunk, .a = @backingInt(value_ty), .b = @backingInt(thunk_ty) }, thunk_ty, &.{value_ty});
        const thunk = try self.packCallback(thunk_worker, &.{value}, &.{value_ty});
        const args = try std.mem.concat(self.b.allocator, LocalId, &.{ &.{state}, leading, &.{thunk} });
        defer self.b.allocator.free(args);
        const written = try self.b.emitErasedCall(body.writer, body.writer_ty, args, fnRet(program_, body.writer_ty));
        return try self.unwrap(written, fnRet(program_, body.writer_ty), .{ .ret = body.ret_ty });
    }

    fn finishBody(self: *Gen, body: BodyFrame, state: LocalId) Allocator.Error!void {
        try self.ret(try self.tryOk(body.ret_ty, state));
        try self.finishErased(body.frame);
    }

    fn lowerRecordBody(self: *Gen, record_ty: TypeId) Allocator.Error!void {
        const program_ = self.program();
        const body = try self.beginBody();
        const record = body.values[0];
        const fields = try self.b.allocator.dupe(types.Field, recordFields(program_, record_ty));
        defer self.b.allocator.free(fields);
        const base_index = self.recordNameBase(record_ty);
        var state = body.state;
        for (fields, 0..) |field_info, index| {
            const storage = try program_.layouts.fieldStorage(field_info);
            const stored = try self.b.readField(record_ty, record, @intCast(index), storage);
            const name_value = try self.renamed(base_index + @as(u32, @intCast(index)));
            const missing_ok = if (field_info.kind == .inline_slot) try missingTryOk(program_, field_info.ty) else null;
            if (field_info.kind == .inline_slot and missing_ok == null) {
                state = try self.writeItem(body, state, &.{name_value}, stored, field_info.ty);
                continue;
            }
            // A field that may be missing is written only when present.
            const state_ty = self.b.currentWorker().params[0];
            const merged = try self.merge(state_ty);
            const present_text = if (field_info.kind == .presence_slot) layouts_mod.present_label else "Ok";
            const missing_hole = try self.branchTag(stored, storage, present_text);
            const present = try self.payload(stored, storage, present_text, 0);
            const written = try self.writeItem(body, state, &.{name_value}, present.value, present.ty);
            try self.jump(merged.id, &.{merged.param}, &.{written});
            self.b.hole = missing_hole;
            try self.jump(merged.id, &.{merged.param}, &.{state});
            self.b.hole = merged.after;
            state = merged.param;
        }
        try self.finishBody(body, state);
    }

    fn lowerTupleBody(self: *Gen, tuple_ty: TypeId) Allocator.Error!void {
        const program_ = self.program();
        const body = try self.beginBody();
        const tuple = body.values[0];
        const items = try self.b.allocator.dupe(TypeId, program_.store.span(program_.store.get(tuple_ty).tuple));
        defer self.b.allocator.free(items);
        var state = body.state;
        for (items, 0..) |item_ty, index| {
            const item = try self.b.readField(tuple_ty, tuple, @intCast(index), item_ty);
            state = try self.writeItem(body, state, &.{}, item, item_ty);
        }
        try self.finishBody(body, state);
    }

    fn lowerTagBody(self: *Gen, union_ty: TypeId, tag_name: NameId) Allocator.Error!void {
        const body = try self.beginBody();
        const variant = try self.b.closedVariant(union_ty, tag_name);
        const payload_types = try self.b.allocator.dupe(TypeId, variant.payloads);
        defer self.b.allocator.free(payload_types);
        var state = body.state;
        for (payload_types, body.values) |payload_ty, payload_value| {
            state = try self.writeItem(body, state, &.{}, payload_value, payload_ty);
        }
        try self.finishBody(body, state);
    }

    const ListKind = enum { list, dict };

    /// A list's or dict's body: one writer call per element, in a loop.
    fn lowerListBody(self: *Gen, kind: ListKind) Allocator.Error!void {
        const program_ = self.program();
        const body = try self.beginBody();
        const list = body.values[0];
        const list_ty = self.b.currentWorker().capture_types[0];
        const elem_ty = switch (program_.store.get(list_ty)) {
            .list => |elem| elem,
            else => invariant("a generated list encoder's elements are not a list"),
        };
        const len = try self.lowLevelLayout(.list_len, &.{list}, .u64);
        const head = self.b.freshJoin();
        const index = try self.b.closedLocal(.u64);
        const state_ty = self.b.currentWorker().params[0];
        const state = try self.local(state_ty);
        const head_body = try self.declareJoin(head, &.{ index, state }, &.{});
        try self.jump(head, &.{ index, state }, &.{ try self.u64Value(0), body.state });
        self.b.store.shapes.loop = true;
        self.b.hole = head_body;
        const more = try self.branchIf(try self.lowLevelLayout(.bool_not, &.{try self.isEq(index, len)}, .bool));
        const elem = try self.local(elem_ty);
        try self.b.emitLowLevel(elem, .list_get_unsafe, &.{ list, index });
        const next_state = switch (kind) {
            .list => try self.writeItem(body, state, &.{}, elem, elem_ty),
            .dict => blk: {
                const items = program_.store.span(program_.store.get(elem_ty).tuple);
                const key_ty = items[0];
                const value_ty = items[1];
                const key = try self.b.readField(elem_ty, elem, 0, key_ty);
                const item_value = try self.b.readField(elem_ty, elem, 1, value_ty);
                const writer_args = fnArgs(program_, body.writer_ty);
                const key_writer_ty = writer_args[1];
                const key_worker = try callbackWorker(program_, self.family, .{ .family = self.family, .kind = .key_thunk, .a = @backingInt(key_ty), .b = @backingInt(key_writer_ty) }, key_writer_ty, &.{key_ty});
                const key_thunk = try self.packCallback(key_worker, &.{key}, &.{key_ty});
                break :blk try self.writeItem(body, state, &.{key_thunk}, item_value, value_ty);
            },
        };
        try self.jump(head, &.{ index, state }, &.{ try self.addOne(index), next_state });
        self.b.hole = more;
        try self.finishBody(body, state);
    }

    fn lowerValueThunk(self: *Gen, value_ty: TypeId) Allocator.Error!void {
        const frame = try self.beginErased();
        const values = self.bindCallbackCaptures(frame.captures);
        const worker = self.b.currentWorker().*;
        const result = try self.callHelper(.encoder, value_ty, worker.ret, &.{ values[0], frame.args[0] });
        try self.ret(result);
        try self.finishErased(frame);
    }

    fn lowerKeyThunk(self: *Gen, key_ty: TypeId) Allocator.Error!void {
        const program_ = self.program();
        const frame = try self.beginErased();
        const values = self.bindCallbackCaptures(frame.captures);
        const worker = self.b.currentWorker().*;
        const key = values[0];
        const state = frame.args[0];
        const result = if (scalarOf(program_, key_ty)) |scalar| blk: {
            var buf: [40]u8 = undefined;
            const method = std.fmt.bufPrint(&buf, "encode_key_{s}", .{@tagName(scalar)}) catch unreachable; // scalar names are short
            const written = try self.callFormat(method, key_ty, &.{ self.encoding, key, state });
            break :blk try self.injectTry(written.value, written.ty, worker.ret);
        } else if (keyIsUnitTags(program_, key_ty)) blk: {
            const text = try self.unitTagName(key, key_ty);
            const written = try self.callFormat("encode_key_str", try self.strTy(), &.{ self.encoding, text, state });
            break :blk try self.injectTry(written.value, written.ty, worker.ret);
        } else blk: {
            const started = try self.callFormat("encode_key_start", null, &.{ self.encoding, state });
            const opened = try self.unwrap(started.value, started.ty, .{ .ret = worker.ret });
            break :blk try self.callHelper(.encoder, key_ty, worker.ret, &.{ key, opened });
        };
        try self.ret(result);
        try self.finishErased(frame);
    }

    /// The name of the payload-free tag `value` holds.
    fn unitTagName(self: *Gen, value: LocalId, union_ty: TypeId) Allocator.Error!LocalId {
        const program_ = self.program();
        const tags = try self.b.allocator.dupe(types.Tag, unionTags(program_, union_ty));
        defer self.b.allocator.free(tags);
        const merged = try self.merge(try self.strTy());
        for (tags, 0..) |tag_info, index| {
            const miss: ?CFStmtId = if (index + 1 < tags.len) blk: {
                const hole = try self.b.newHole();
                try self.b.branchOnTag(value, union_ty, tag_info.name, hole);
                break :blk hole;
            } else null;
            try self.jump(merged.id, &.{merged.param}, &.{try self.b.strLiteral(program_.store.nameText(tag_info.name))});
            if (miss) |hole| self.b.hole = hole;
        }
        self.b.hole = merged.after;
        return merged.param;
    }

    // -- Parsers --

    fn lowerParseHelper(self: *Gen, value_ty: TypeId, ret_ty: TypeId) Allocator.Error!void {
        const frame = try self.beginDirect();
        self.encoding = frame.params[1];
        self.names = frame.params[2];
        try self.parseShape(value_ty, frame.params[0], ret_ty);
        try self.finishDirect(frame);
    }

    /// The `{ rest, value }` record a parser returns.
    fn parsedType(self: *Gen, value_ty: TypeId) Allocator.Error!TypeId {
        const state_ty = self.fam().state;
        var fields = [_]types.Field{
            .{ .name = try self.name("rest"), .ty = state_ty, .kind = .inline_slot },
            .{ .name = try self.name("value"), .ty = value_ty, .kind = .inline_slot },
        };
        std.mem.sort(types.Field, &fields, self.store(), struct {
            fn lessThan(s: *types.Store, a: types.Field, b: types.Field) bool {
                return s.nameLessThan(a.name, b.name);
            }
        }.lessThan);
        return self.store().intern(.{ .record = .{ .fields = &fields, .ext = null } });
    }

    /// The result type of parsing `value_ty` with the errors of `ret_ty`.
    fn parseResultType(self: *Gen, value_ty: TypeId, ret_ty: TypeId) Allocator.Error!TypeId {
        return self.tryType(ret_ty, try self.parsedType(value_ty), (try requireTry(self.program(), ret_ty)).err);
    }

    /// `Ok({ rest, value })` of `ret_ty`.
    fn parsedOk(self: *Gen, ret_ty: TypeId, value: LocalId, rest: LocalId) Allocator.Error!LocalId {
        const parts = try requireTry(self.program(), ret_ty);
        const record_ty = parts.ok;
        const fields = recordFields(self.program(), try self.b.structuralType(record_ty));
        var values: [2]LocalId = undefined;
        for (fields, 0..) |field_info, index| {
            values[index] = if (std.mem.eql(u8, self.store().nameText(field_info.name), "rest")) rest else value;
        }
        return self.tryOk(ret_ty, try self.b.buildStruct(record_ty, &values));
    }

    const Parsed = struct { value: LocalId, rest: LocalId };

    /// The value and rest of a successful parse result `result` of
    /// `result_ty`; failures go to `on_err`.
    fn unwrapParsed(self: *Gen, result: LocalId, result_ty: TypeId, on_err: OnErr) Allocator.Error!Parsed {
        const ok = try self.unwrap(result, result_ty, on_err);
        const ok_ty = (try requireTry(self.program(), result_ty)).ok;
        return .{
            .value = (try self.field(ok, ok_ty, "value")).value,
            .rest = (try self.field(ok, ok_ty, "rest")).value,
        };
    }

    /// Parse a value of `value_ty` from `state` with the family helper.
    fn parseWith(self: *Gen, value_ty: TypeId, state: LocalId, ret_ty: TypeId, on_err: OnErr) Allocator.Error!Parsed {
        const result_ty = try self.parseResultType(value_ty, ret_ty);
        const result = try self.callHelper(.parser, value_ty, result_ty, &.{state});
        return try self.unwrapParsed(result, result_ty, on_err);
    }

    /// Branch on the tags `texts` of `value`: returns the hole of each tag's
    /// path, testing every tag but the last.
    fn switchTags(self: *Gen, value: LocalId, ty: TypeId, texts: []const []const u8, holes: []CFStmtId) Allocator.Error!void {
        for (texts, holes, 0..) |text, *hole, index| {
            if (index + 1 == texts.len) {
                hole.* = try self.b.currentHole();
                break;
            }
            const miss = try self.branchTag(value, ty, text);
            hole.* = try self.b.currentHole();
            self.b.hole = miss;
        }
    }

    fn scalarMethod(buf: []u8, prefix: []const u8, scalar: Scalar) []const u8 {
        return std.fmt.bufPrint(buf, "{s}{s}", .{ prefix, @tagName(scalar) }) catch unreachable; // scalar method names are short
    }

    /// Parse `value_ty` from `state`; every path returns `ret_ty`.
    fn parseShape(self: *Gen, value_ty: TypeId, state: LocalId, ret_ty: TypeId) Allocator.Error!void {
        const program_ = self.program();
        const on_err: OnErr = .{ .ret = ret_ty };
        switch (try classify(program_, self.family, value_ty)) {
            .null_try => |null_try| {
                const parts = (try tryParts(program_, value_ty)).?;
                const nulled = try self.callFormat("parse_null", null, &.{ self.encoding, state });
                const err_hole = try self.branchTag(nulled.value, nulled.ty, "Ok");
                const rest = try self.payload(nulled.value, nulled.ty, "Ok", 0);
                const null_value = try self.tryErr(value_ty, try self.tag(parts.err, "Null", &.{}));
                try self.ret(try self.parsedOk(ret_ty, null_value, rest.value));
                self.b.hole = err_hole;
                const parsed = try self.parseWith(null_try.ok, state, ret_ty, on_err);
                try self.ret(try self.parsedOk(ret_ty, try self.tryOk(value_ty, parsed.value), parsed.rest));
            },
            .dict => |dict| try self.parseCounted(.{ .dict = .{ .key = dict.key, .value = dict.value } }, value_ty, state, ret_ty, null),
            .set => |elem| {
                const list_ty = try self.store().intern(.{ .list = elem });
                try self.parseCounted(.{ .list = elem }, list_ty, state, ret_ty, value_ty);
            },
            .custom => |resolved| {
                const parser = try self.callResolved(resolved, &.{self.encoding});
                const runtime = functionOf(program_, parser.ty);
                const result = try self.b.emitErasedCall(parser.value, parser.ty, &.{state}, runtime.ret);
                try self.ret(try self.injectTry(result, runtime.ret, ret_ty));
            },
            .list => |list_ty| try self.parseCounted(.{ .list = program_.store.get(list_ty).list }, list_ty, state, ret_ty, null),
            .tuple => |tuple_ty| try self.parseTuple(tuple_ty, value_ty, state, ret_ty),
            .box => |payload_ty| {
                const parsed = try self.parseWith(payload_ty, state, ret_ty, on_err);
                const boxed = try self.local(value_ty);
                try self.b.emitLowLevel(boxed, .box_box, &.{parsed.value});
                try self.ret(try self.parsedOk(ret_ty, boxed, parsed.rest));
            },
            .scalar => |scalar| {
                var buf: [40]u8 = undefined;
                const result = try self.callFormat(scalarMethod(&buf, "parse_", scalar), value_ty, &.{ self.encoding, state });
                try self.ret(try self.injectTry(result.value, result.ty, ret_ty));
            },
            .record => |record_ty| try self.parseRecord(record_ty, value_ty, state, ret_ty),
            .tag_union => |union_ty| {
                const resolved = try lookupCall(program_, self.family, "parse_tag_union", union_ty);
                const method_fn = switch (resolved) {
                    .callable => |callable| callable.fn_ty,
                    .checked_error => |fn_ty| fn_ty,
                    .structural => invariant("parse_tag_union resolved to a structural derivation"),
                };
                const worker = try callbackWorker(program_, self.family, .{ .family = self.family, .kind = .tag_spec, .a = @backingInt(union_ty), .b = @backingInt(method_fn) }, try self.specType(method_fn), &.{});
                const spec = try self.packCallback(worker, &.{}, &.{});
                const result = try self.callResolved(resolved, &.{ self.encoding, spec, state });
                try self.ret(try self.injectTry(result.value, result.ty, ret_ty));
            },
        }
    }

    const Counted = union(enum) {
        list: TypeId,
        dict: struct { key: TypeId, value: TypeId },
    };

    /// A list or dict: the format says up front whether it knows the entry
    /// count (`Counted`), or answers after each entry (`Uncounted`). With
    /// `set_ty`, the parsed list becomes that set.
    fn parseCounted(self: *Gen, container: Counted, container_ty: TypeId, state: LocalId, ret_ty: TypeId, set_ty: ?TypeId) Allocator.Error!void {
        const on_err: OnErr = .{ .ret = ret_ty };
        const prefix = switch (container) {
            .list => "parse_list_",
            .dict => "parse_dict_",
        };
        var buf: [40]u8 = undefined;
        // A set is read as a list whose contract subject is the set.
        const subject = set_ty orelse container_ty;
        const started = try self.callFormat(std.fmt.bufPrint(&buf, "{s}start", .{prefix}) catch unreachable, subject, &.{ self.encoding, state }); // short names
        const event = try self.unwrap(started.value, started.ty, on_err);
        const event_ty = (try requireTry(self.program(), started.ty)).ok;

        const head = self.b.freshJoin();
        const cursor = try self.local(self.fam().state);
        const acc = try self.local(container_ty);
        const counted = try self.b.closedLocal(.bool);
        const remaining = try self.b.closedLocal(.u64);
        const params = [_]LocalId{ cursor, acc, counted, remaining };
        const head_body = try self.declareJoin(head, &params, &.{});
        var holes: [2]CFStmtId = undefined;
        try self.switchTags(event, event_ty, &.{ "Counted", "Uncounted" }, &holes);
        self.b.hole = holes[0];
        {
            const payload_value = try self.payload(event, event_ty, "Counted", 0);
            const len = try self.field(payload_value.value, payload_value.ty, "len");
            const rest = try self.field(payload_value.value, payload_value.ty, "rest");
            try self.jump(head, &params, &.{ rest.value, try self.emptyContainer(container, container_ty, len.value), try self.b.boolValue(true), len.value });
        }
        self.b.hole = holes[1];
        {
            const rest = try self.payload(event, event_ty, "Uncounted", 0);
            const zero = try self.u64Value(0);
            try self.jump(head, &params, &.{ rest.value, try self.emptyContainer(container, container_ty, zero), try self.b.boolValue(false), zero });
        }
        self.b.store.shapes.loop = true;
        self.b.hole = head_body;
        const uncounted_hole = try self.branchIf(counted);
        // Counted: stop when no entry remains.
        const more_hole = try self.branchIf(try self.isEq(remaining, try self.u64Value(0)));
        try self.finishCounted(ret_ty, set_ty, acc, cursor);
        self.b.hole = more_hole;
        try self.parseEntry(container, container_ty, cursor, acc, head, &params, try self.subOne(remaining), true, ret_ty, set_ty);
        // Uncounted: ask the format whether another entry follows.
        self.b.hole = uncounted_hole;
        const next = try self.callFormat(std.fmt.bufPrint(&buf, "{s}next", .{prefix}) catch unreachable, subject, &.{ self.encoding, cursor });
        const next_event = try self.unwrap(next.value, next.ty, on_err);
        const next_ty = (try requireTry(self.program(), next.ty)).ok;
        const item_text = switch (container) {
            .list => "Item",
            .dict => "Entry",
        };
        try self.switchTags(next_event, next_ty, &.{ item_text, "Done" }, &holes);
        self.b.hole = holes[1];
        const done_rest = try self.payload(next_event, next_ty, "Done", 0);
        try self.finishCounted(ret_ty, set_ty, acc, done_rest.value);
        self.b.hole = holes[0];
        const item_state = try self.payload(next_event, next_ty, item_text, 0);
        try self.parseEntry(container, container_ty, item_state.value, acc, head, &params, remaining, false, ret_ty, set_ty);
    }

    fn emptyContainer(self: *Gen, container: Counted, container_ty: TypeId, capacity: LocalId) Allocator.Error!LocalId {
        return switch (container) {
            .list => blk: {
                const list = try self.local(container_ty);
                try self.b.emitLowLevel(list, .list_with_capacity, &.{capacity});
                break :blk list;
            },
            .dict => (try self.callFormat("with_capacity", container_ty, &.{capacity})).value,
        };
    }

    fn finishCounted(self: *Gen, ret_ty: TypeId, set_ty: ?TypeId, acc: LocalId, rest: LocalId) Allocator.Error!void {
        const value = if (set_ty) |set| (try self.callFormat("from_list", set, &.{acc})).value else acc;
        try self.ret(try self.parsedOk(ret_ty, value, rest));
    }

    /// One entry from `entry_state`, then the loop continues (or, uncounted,
    /// the format says it is done).
    fn parseEntry(
        self: *Gen,
        container: Counted,
        container_ty: TypeId,
        entry_state: LocalId,
        acc: LocalId,
        head: LIR.JoinPointId,
        params: []const LocalId,
        remaining: LocalId,
        counted: bool,
        ret_ty: TypeId,
        set_ty: ?TypeId,
    ) Allocator.Error!void {
        const on_err: OnErr = .{ .ret = ret_ty };
        var after_entry: LocalId = undefined;
        var next_acc: LocalId = undefined;
        switch (container) {
            .list => |elem_ty| {
                const parsed = try self.parseWith(elem_ty, entry_state, ret_ty, on_err);
                next_acc = try self.append(acc, container_ty, parsed.value);
                after_entry = parsed.rest;
            },
            .dict => |dict| {
                const key = try self.parseKey(dict.key, entry_state, ret_ty);
                const separated = try self.callFormat("parse_dict_after_key", container_ty, &.{ self.encoding, key.rest });
                const value_state = try self.unwrap(separated.value, separated.ty, on_err);
                const parsed = try self.parseWith(dict.value, value_state, ret_ty, on_err);
                next_acc = (try self.callFormat("insert", container_ty, &.{ acc, key.value, parsed.value })).value;
                after_entry = parsed.rest;
            },
        }
        if (counted) {
            try self.jump(head, params, &.{ after_entry, next_acc, params[2], remaining });
            return;
        }
        const after_method = switch (container) {
            .list => "parse_list_after_item",
            .dict => "parse_dict_after_entry",
        };
        const after = try self.callFormat(after_method, set_ty orelse container_ty, &.{ self.encoding, after_entry });
        const event = try self.unwrap(after.value, after.ty, on_err);
        const event_ty = (try requireTry(self.program(), after.ty)).ok;
        var holes: [2]CFStmtId = undefined;
        try self.switchTags(event, event_ty, &.{ "Continue", "Done" }, &holes);
        self.b.hole = holes[0];
        const continued = try self.payload(event, event_ty, "Continue", 0);
        try self.jump(head, params, &.{ continued.value, next_acc, params[2], remaining });
        self.b.hole = holes[1];
        const done = try self.payload(event, event_ty, "Done", 0);
        try self.finishCounted(ret_ty, set_ty, next_acc, done.value);
    }

    fn append(self: *Gen, list: LocalId, list_ty: TypeId, elem: LocalId) Allocator.Error!LocalId {
        const reserved = try self.local(list_ty);
        try self.b.emitLowLevel(reserved, .list_reserve_for_append, &.{ list, try self.u64Value(1) });
        const appended = try self.local(list_ty);
        try self.b.emitLowLevel(appended, .list_append_unsafe, &.{ reserved, elem });
        return appended;
    }

    /// A dict key: a key string for scalars and payload-free tag unions,
    /// otherwise the key's own parser after `parse_key_start`.
    fn parseKey(self: *Gen, key_ty: TypeId, state: LocalId, ret_ty: TypeId) Allocator.Error!Parsed {
        const program_ = self.program();
        const on_err: OnErr = .{ .ret = ret_ty };
        if (scalarOf(program_, key_ty)) |scalar| {
            var buf: [40]u8 = undefined;
            const result = try self.callFormat(scalarMethod(&buf, "parse_key_", scalar), key_ty, &.{ self.encoding, state });
            return try self.unwrapParsed(result.value, result.ty, on_err);
        }
        if (keyIsUnitTags(program_, key_ty)) {
            const result = try self.callFormat("parse_key_str", try self.strTy(), &.{ self.encoding, state });
            const parsed = try self.unwrapParsed(result.value, result.ty, on_err);
            const tags = try self.b.allocator.dupe(types.Tag, unionTags(program_, key_ty));
            defer self.b.allocator.free(tags);
            const merged = try self.merge(key_ty);
            for (tags) |tag_info| {
                const literal = try self.b.strLiteral(program_.store.nameText(tag_info.name));
                const matches = try self.lowLevelLayout(.str_is_eq, &.{ parsed.value, literal }, .bool);
                const miss = try self.branchIf(matches);
                try self.jump(merged.id, &.{merged.param}, &.{try self.b.buildTag(key_ty, tag_info.name, &.{})});
                self.b.hole = miss;
            }
            // A name no tag has is the one key failure the codec detects.
            const invalid = try self.callFormat("invalid_value", null, &.{ self.encoding, parsed.rest });
            try self.failWith(invalid.value, invalid.ty, on_err);
            self.b.hole = merged.after;
            return .{ .value = merged.param, .rest = parsed.rest };
        }
        const started = try self.callFormat("parse_key_start", key_ty, &.{ self.encoding, state });
        const opened = try self.unwrap(started.value, started.ty, on_err);
        return try self.parseWith(key_ty, opened, ret_ty, on_err);
    }

    /// A tuple has a static arity: the format is told the element count.
    fn parseTuple(self: *Gen, tuple_ty: TypeId, value_ty: TypeId, state: LocalId, ret_ty: TypeId) Allocator.Error!void {
        const program_ = self.program();
        const on_err: OnErr = .{ .ret = ret_ty };
        const items = try self.b.allocator.dupe(TypeId, program_.store.span(program_.store.get(tuple_ty).tuple));
        defer self.b.allocator.free(items);
        const len = try self.u64Value(items.len);
        const started = try self.callFormat("parse_tuple_start", tuple_ty, &.{ self.encoding, state, len });
        var cursor = try self.unwrap(started.value, started.ty, on_err);
        const values = try self.b.allocator.alloc(LocalId, items.len);
        defer self.b.allocator.free(values);
        for (items, values, 0..) |item_ty, *value, index| {
            if (index != 0) {
                const separated = try self.callFormat("parse_tuple_next", tuple_ty, &.{ self.encoding, cursor, try self.u64Value(index), len });
                cursor = try self.unwrap(separated.value, separated.ty, on_err);
            }
            const parsed = try self.parseWith(item_ty, cursor, ret_ty, on_err);
            value.* = parsed.value;
            cursor = parsed.rest;
        }
        const ended = try self.callFormat("parse_tuple_end", tuple_ty, &.{ self.encoding, cursor, len });
        const rest = try self.unwrap(ended.value, ended.ty, on_err);
        try self.ret(try self.parsedOk(ret_ty, try self.b.buildStruct(value_ty, values), rest));
    }

    /// A record: one loop over the format's field events. Each field's slot
    /// is `#Missing` until its value parses; each field's value parser is
    /// one join every matching event jumps to; every failure inside the loop
    /// reaches one error join that releases the slots and the field names.
    fn parseRecord(self: *Gen, record_ty: TypeId, value_ty: TypeId, state: LocalId, ret_ty: TypeId) Allocator.Error!void {
        const program_ = self.program();
        const b = self.b;
        const allocator = b.allocator;
        const err_ty = (try requireTry(program_, ret_ty)).err;
        const fields = try allocator.dupe(types.Field, recordFields(program_, record_ty));
        defer allocator.free(fields);
        const base_index = self.recordNameBase(record_ty);

        // The field set the format matches names against.
        const field_call = try lookupCall(program_, self.family, "parse_record_field", record_ty);
        const field_fn = switch (field_call) {
            .callable => |callable| callable.fn_ty,
            .checked_error => |fn_ty| fn_ty,
            .structural => invariant("parse_record_field resolved to a structural derivation"),
        };
        const field_set_ty = fnArgs(program_, field_fn)[1];
        const handle_ty = switch (program_.store.get(field_set_ty)) {
            .list => |elem| elem,
            else => invariant("a record parser's field set is not a list of field handles"),
        };
        const names = try allocator.alloc(LocalId, fields.len);
        defer allocator.free(names);
        const handles = try allocator.alloc(LocalId, fields.len);
        defer allocator.free(handles);
        for (names, handles, 0..) |*name_value, *handle, index| {
            name_value.* = try self.renamed(base_index + @as(u32, @intCast(index)));
            handle.* = try self.fieldHandle(handle_ty, @intCast(index), name_value.*);
        }
        const field_set = try self.local(field_set_ty);
        try b.emitLinear(.{ .assign_list = .{ .target = field_set, .elems = try b.store.addLocalSpan(handles), .next = undefined } });

        const started = try self.callFormat("parse_record_start", record_ty, &.{ self.encoding, state });
        const start_event = try self.unwrap(started.value, started.ty, .{ .ret = ret_ty });
        const start_ty = (try requireTry(program_, started.ty)).ok;

        // Loop state: cursor, done, counted, remaining, then one slot per field.
        const slot_types = try allocator.alloc(TypeId, fields.len);
        defer allocator.free(slot_types);
        for (fields, slot_types) |field_info, *slot_ty| slot_ty.* = try program_.layouts.presenceSlot(field_info.ty);
        const head = b.freshJoin();
        const params = try allocator.alloc(LocalId, 4 + fields.len);
        defer allocator.free(params);
        params[0] = try self.local(self.fam().state);
        params[1] = try b.closedLocal(.bool);
        params[2] = try b.closedLocal(.bool);
        params[3] = try b.closedLocal(.u64);
        for (params[4..], slot_types) |*param, slot_ty| param.* = try self.local(slot_ty);
        const cursor = params[0];
        const done = params[1];
        const counted = params[2];
        const remaining = params[3];
        const slots = params[4..];
        const head_body = try self.declareJoin(head, params, &.{});

        const initial = try allocator.alloc(LocalId, params.len);
        defer allocator.free(initial);
        var start_holes: [2]CFStmtId = undefined;
        try self.switchTags(start_event, start_ty, &.{ "Counted", "Uncounted" }, &start_holes);
        for (start_holes, 0..) |hole, which| {
            b.hole = hole;
            if (which == 0) {
                const counted_value = try self.payload(start_event, start_ty, "Counted", 0);
                initial[0] = (try self.field(counted_value.value, counted_value.ty, "rest")).value;
                initial[2] = try b.boolValue(true);
                initial[3] = (try self.field(counted_value.value, counted_value.ty, "len")).value;
            } else {
                initial[0] = (try self.payload(start_event, start_ty, "Uncounted", 0)).value;
                initial[2] = try b.boolValue(false);
                initial[3] = try self.u64Value(0);
            }
            initial[1] = try b.boolValue(false);
            for (initial[4..], slot_types) |*slot, slot_ty| slot.* = try self.tag(slot_ty, layouts_mod.missing_label, &.{});
            try self.jump(head, params, initial);
        }
        b.store.shapes.loop = true;
        b.hole = head_body;

        // The error join, which owns the field set and the slots.
        const err_join = b.freshJoin();
        const err_param = try self.local(err_ty);
        const retained = try std.mem.concat(allocator, LocalId, &.{ &.{field_set}, slots });
        defer allocator.free(retained);
        const err_body = try self.declareJoin(err_join, &.{err_param}, retained);
        const loop_hole = b.hole;
        b.hole = err_body;
        try self.ret(try self.tryErr(ret_ty, err_param));
        b.hole = loop_hole;
        const on_err: OnErr = .{ .join = .{ .id = err_join, .param = err_param, .err_ty = err_ty } };

        const next = try allocator.alloc(LocalId, params.len);
        defer allocator.free(next);

        // After an entry: counted records count it down; uncounted records
        // ask the format whether another follows.
        const entry_end = b.freshJoin();
        const entry_state = try self.local(self.fam().state);
        const entry_body = try self.declareJoin(entry_end, &.{entry_state}, &.{});
        const after_entries = b.hole;
        b.hole = entry_body;
        {
            const uncounted_hole = try self.branchIf(counted);
            @memcpy(next, params);
            next[0] = entry_state;
            next[3] = try self.subOne(remaining);
            try self.jump(head, params, next);
            b.hole = uncounted_hole;
            const after = try self.callFormat("parse_record_after_field", record_ty, &.{ self.encoding, entry_state });
            const event = try self.unwrap(after.value, after.ty, on_err);
            const event_ty = (try requireTry(program_, after.ty)).ok;
            var holes: [2]CFStmtId = undefined;
            try self.switchTags(event, event_ty, &.{ "Continue", "Done" }, &holes);
            for (holes, [_][]const u8{ "Continue", "Done" }, 0..) |hole, text, which| {
                b.hole = hole;
                @memcpy(next, params);
                next[0] = (try self.payload(event, event_ty, text, 0)).value;
                next[1] = try b.boolValue(which == 1);
                try self.jump(head, params, next);
            }
        }
        b.hole = after_entries;

        // An entry no field matches is skipped.
        const skip = b.freshJoin();
        const skip_state = try self.local(self.fam().state);
        const skip_body = try self.declareJoin(skip, &.{skip_state}, &.{});
        const after_skip = b.hole;
        b.hole = skip_body;
        {
            const skipped = try self.callFormat("skip_record_field", null, &.{ self.encoding, skip_state });
            const rest = try self.unwrap(skipped.value, skipped.ty, on_err);
            try self.jump(entry_end, &.{entry_state}, &.{rest});
        }
        b.hole = after_skip;

        // One value parser per field.
        const field_joins = try allocator.alloc(LIR.JoinPointId, fields.len);
        defer allocator.free(field_joins);
        const field_states = try allocator.alloc(LocalId, fields.len);
        defer allocator.free(field_states);
        for (fields, field_joins, field_states, slots, slot_types) |field_info, *join, *field_state, slot, slot_ty| {
            join.* = b.freshJoin();
            field_state.* = try self.local(self.fam().state);
            const body = try self.declareJoin(join.*, &.{field_state.*}, &.{});
            const after_field = b.hole;
            b.hole = body;
            const missing_ok = if (field_info.kind == .inline_slot) try missingTryOk(program_, field_info.ty) else null;
            const parsed = try self.parseWith(missing_ok orelse field_info.ty, field_state.*, ret_ty, on_err);
            const value = if (missing_ok != null) try self.tryOk(field_info.ty, parsed.value) else parsed.value;
            // The slot's old value is released before it is replaced.
            try self.setParam(slot, try self.tag(slot_ty, layouts_mod.present_label, &.{value}));
            try self.jump(entry_end, &.{entry_state}, &.{parsed.rest});
            b.hole = after_field;
        }

        // The loop head: finish, or read the next field event.
        const step_hole = try self.branchIf(try self.finished(done, counted, remaining));
        try self.finishRecord(value_ty, fields, slots, slot_types, names, cursor, ret_ty, on_err);
        b.hole = step_hole;
        const step = try self.callResolved(field_call, &.{ self.encoding, field_set, cursor });
        const event = try self.unwrap(step.value, step.ty, on_err);
        const event_ty = (try requireTry(program_, step.ty)).ok;
        var holes: [5]CFStmtId = undefined;
        const event_texts = [_][]const u8{ "Continue", "Done", "Field", "TryField", "TryFieldCaseless" };
        try self.switchTags(event, event_ty, &event_texts, &holes);
        // Continue: the format consumed a whole entry itself.
        b.hole = holes[0];
        {
            @memcpy(next, params);
            next[0] = (try self.payload(event, event_ty, "Continue", 0)).value;
            const uncounted_hole = try self.branchIf(counted);
            const continued = next[0];
            next[3] = try self.subOne(remaining);
            try self.jump(head, params, next);
            b.hole = uncounted_hole;
            @memcpy(next, params);
            next[0] = continued;
            try self.jump(head, params, next);
        }
        b.hole = holes[1];
        @memcpy(next, params);
        next[0] = (try self.payload(event, event_ty, "Done", 0)).value;
        next[1] = try b.boolValue(true);
        try self.jump(head, params, next);
        // Field: the format matched the name against the field set.
        b.hole = holes[2];
        {
            const payload_value = try self.payload(event, event_ty, "Field", 0);
            const rest = (try self.field(payload_value.value, payload_value.ty, "rest")).value;
            const handle = try self.field(payload_value.value, payload_value.ty, "field");
            if (fields.len == 0) {
                try self.jump(skip, &.{skip_state}, &.{rest});
            } else {
                const index = (try self.field(handle.value, handle.ty, "index")).value;
                for (field_joins, field_states, 0..) |join, field_state, field_index| {
                    if (field_index + 1 < fields.len) {
                        const miss = try self.branchIf(try self.isEq(index, try self.u64Value(field_index)));
                        try self.jump(join, &.{field_state}, &.{rest});
                        b.hole = miss;
                    } else {
                        try self.jump(join, &.{field_state}, &.{rest});
                    }
                }
            }
        }
        // TryField and TryFieldCaseless: match the name here.
        for ([_][]const u8{ "TryField", "TryFieldCaseless" }, [_]LIR.LowLevel{ .str_is_eq, .str_caseless_ascii_equals }, 3..) |text, compare, which| {
            b.hole = holes[which];
            const payload_value = try self.payload(event, event_ty, text, 0);
            const key = (try self.field(payload_value.value, payload_value.ty, "name")).value;
            const rest = (try self.field(payload_value.value, payload_value.ty, "rest")).value;
            for (field_joins, field_states, names) |join, field_state, name_value| {
                const miss = try self.branchIf(try self.lowLevelLayout(compare, &.{ key, name_value }, .bool));
                try self.jump(join, &.{field_state}, &.{rest});
                b.hole = miss;
            }
            try self.jump(skip, &.{skip_state}, &.{rest});
        }
    }

    /// `done or (counted and remaining == 0)`.
    fn finished(self: *Gen, done: LocalId, counted: LocalId, remaining: LocalId) Allocator.Error!LocalId {
        const merged = try self.merge(try self.boolTy());
        const not_done = try self.branchIf(done);
        try self.jump(merged.id, &.{merged.param}, &.{try self.b.boolValue(true)});
        self.b.hole = not_done;
        const uncounted = try self.branchIf(counted);
        try self.jump(merged.id, &.{merged.param}, &.{try self.isEq(remaining, try self.u64Value(0))});
        self.b.hole = uncounted;
        try self.jump(merged.id, &.{merged.param}, &.{try self.b.boolValue(false)});
        self.b.hole = merged.after;
        return merged.param;
    }

    /// Build the record from its slots: a required field never parsed fails
    /// with `MissingRequiredField`, checked for every field before any slot
    /// is consumed.
    fn finishRecord(
        self: *Gen,
        value_ty: TypeId,
        fields: []const types.Field,
        slots: []const LocalId,
        slot_types: []const TypeId,
        names: []const LocalId,
        rest: LocalId,
        ret_ty: TypeId,
        on_err: OnErr,
    ) Allocator.Error!void {
        const program_ = self.program();
        const err_ty = (try requireTry(program_, ret_ty)).err;
        const optional = try self.b.allocator.alloc(?TypeId, fields.len);
        defer self.b.allocator.free(optional);
        // A defaulted field never parsed takes its default (design.md
        // "Defaulted Fields").
        const record_ty = try self.b.structuralType(value_ty);
        const defaults = try self.b.allocator.alloc(?checked.CheckedFieldDefault, fields.len);
        defer self.b.allocator.free(defaults);
        for (fields, defaults) |field_info, *default| default.* = program_.store.field_defaults.get(.{ .record = record_ty, .name = field_info.name });
        for (fields, slots, slot_types, names, optional, defaults) |field_info, slot, slot_ty, name_value, *missing_ok, default| {
            missing_ok.* = if (field_info.kind == .inline_slot) try missingTryOk(program_, field_info.ty) else null;
            if (field_info.kind == .presence_slot or missing_ok.* != null or default != null) continue;
            const present_hole = try self.branchTag(slot, slot_ty, layouts_mod.missing_label);
            const missing = try self.tag(err_ty, "MissingRequiredField", &.{name_value});
            try self.failWith(missing, err_ty, on_err);
            self.b.hole = present_hole;
        }
        const values = try self.b.allocator.alloc(LocalId, fields.len);
        defer self.b.allocator.free(values);
        for (fields, slots, slot_types, optional, values, defaults) |field_info, slot, slot_ty, missing_ok, *value, default| {
            if (field_info.kind == .presence_slot) {
                // The slot is the field's own storage.
                value.* = slot;
                continue;
            }
            if (default) |field_default| {
                const merged = try self.merge(field_info.ty);
                const missing_hole = try self.branchTag(slot, slot_ty, layouts_mod.present_label);
                try self.jump(merged.id, &.{merged.param}, &.{(try self.payload(slot, slot_ty, layouts_mod.present_label, 0)).value});
                self.b.hole = missing_hole;
                try self.jump(merged.id, &.{merged.param}, &.{try self.b.defaultValue(field_default)});
                self.b.hole = merged.after;
                value.* = merged.param;
                continue;
            }
            if (missing_ok == null) {
                value.* = (try self.payload(slot, slot_ty, layouts_mod.present_label, 0)).value;
                continue;
            }
            const merged = try self.merge(field_info.ty);
            const missing_hole = try self.branchTag(slot, slot_ty, layouts_mod.present_label);
            try self.jump(merged.id, &.{merged.param}, &.{(try self.payload(slot, slot_ty, layouts_mod.present_label, 0)).value});
            self.b.hole = missing_hole;
            const err_union = (try requireTry(program_, field_info.ty)).err;
            try self.jump(merged.id, &.{merged.param}, &.{try self.tryErr(field_info.ty, try self.tag(err_union, "Missing", &.{}))});
            self.b.hole = merged.after;
            value.* = merged.param;
        }
        try self.ret(try self.parsedOk(ret_ty, try self.b.buildStruct(value_ty, values), rest));
    }

    /// `Encoding.FieldName` of field `index` named `name_value`.
    fn fieldHandle(self: *Gen, handle_ty: TypeId, index: u32, name_value: LocalId) Allocator.Error!LocalId {
        var fields_buf: [3]types.Field = undefined;
        const fields = fields_buf[0..recordFields(self.program(), handle_ty).len];
        @memcpy(fields, recordFields(self.program(), handle_ty));
        var values: [3]LocalId = undefined;
        for (fields, &values) |field_info, *value| {
            const text = self.store().nameText(field_info.name);
            value.* = if (std.mem.eql(u8, text, "index"))
                try self.u64Value(index)
            else if (std.mem.eql(u8, text, "name"))
                name_value
            else if (std.mem.eql(u8, text, "name_len"))
                try self.lowLevelLayout(.str_count_utf8_bytes, &.{name_value}, .u64)
            else
                invariant("a field handle has an unexpected field");
        }
        return self.b.buildStruct(handle_ty, &values);
    }

    /// The function type of a union's spec: the options record of
    /// `ParseTagUnionSpec.parse` at the format method's encoding, state,
    /// and error row, to the parse result.
    fn specType(self: *Gen, method_fn: TypeId) Allocator.Error!TypeId {
        const program_ = self.program();
        const encoding_ty = fnArgs(program_, method_fn)[0];
        const state_ty = fnArgs(program_, method_fn)[2];
        const result_ty = fnRet(program_, method_fn);
        const parts = try requireTry(program_, result_ty);
        const u64_ty = try self.u64Ty();
        const step_result = try self.tryType(result_ty, state_ty, parts.err);
        const start_fn = try self.store().intern(.{ .function = .{ .args = &.{ state_ty, u64_ty }, .ret = step_result, .effectful = false } });
        const next_fn = try self.store().intern(.{ .function = .{ .args = &.{ state_ty, u64_ty, u64_ty }, .ret = step_result, .effectful = false } });
        var fields = [_]types.Field{
            .{ .name = try self.name("encoding"), .ty = encoding_ty, .kind = .inline_slot },
            .{ .name = try self.name("finish_payloads"), .ty = start_fn, .kind = .inline_slot },
            .{ .name = try self.name("missing"), .ty = parts.err, .kind = .inline_slot },
            .{ .name = try self.name("next_payload"), .ty = next_fn, .kind = .inline_slot },
            .{ .name = try self.name("start_payloads"), .ty = start_fn, .kind = .inline_slot },
            .{ .name = try self.name("state"), .ty = state_ty, .kind = .inline_slot },
            .{ .name = try self.name("tag"), .ty = try self.strTy(), .kind = .inline_slot },
        };
        std.mem.sort(types.Field, &fields, self.store(), struct {
            fn lessThan(s: *types.Store, a: types.Field, b: types.Field) bool {
                return s.nameLessThan(a.name, b.name);
            }
        }.lessThan);
        const options = try self.store().intern(.{ .record = .{ .fields = &fields, .ext = null } });
        return self.store().intern(.{ .function = .{ .args = &.{options}, .ret = result_ty, .effectful = false } });
    }

    /// A union's spec: match the tag name the format read, then parse that
    /// tag's payloads between the format's payload callbacks.
    fn lowerTagSpec(self: *Gen, union_ty: TypeId) Allocator.Error!void {
        const program_ = self.program();
        const frame = try self.beginErased();
        _ = self.bindCallbackCaptures(frame.captures);
        const worker = self.b.currentWorker().*;
        const ret_ty = worker.ret;
        const options_ty = worker.params[0];
        const options = frame.args[0];
        const tag_name = (try self.field(options, options_ty, "tag")).value;
        self.encoding = (try self.field(options, options_ty, "encoding")).value;
        const state = (try self.field(options, options_ty, "state")).value;
        const start_fn = try self.field(options, options_ty, "start_payloads");
        const next_fn = try self.field(options, options_ty, "next_payload");
        const finish_fn = try self.field(options, options_ty, "finish_payloads");
        const missing = (try self.field(options, options_ty, "missing")).value;
        const on_err: OnErr = .{ .ret = ret_ty };
        const value_ty = for (recordFields(program_, try self.b.structuralType((try requireTry(program_, ret_ty)).ok))) |field_info| {
            if (std.mem.eql(u8, self.store().nameText(field_info.name), "value")) break field_info.ty;
        } else invariant("a tag-union spec's result has no value");
        const tags = try self.b.allocator.dupe(types.Tag, unionTags(program_, union_ty));
        defer self.b.allocator.free(tags);
        for (tags) |tag_info| {
            const literal = try self.b.strLiteral(program_.store.nameText(tag_info.name));
            const miss = try self.branchIf(try self.lowLevelLayout(.str_is_eq, &.{ tag_name, literal }, .bool));
            const payload_types = try self.b.allocator.dupe(TypeId, program_.store.span(tag_info.payloads));
            defer self.b.allocator.free(payload_types);
            const count = try self.u64Value(payload_types.len);
            const started = try self.b.emitErasedCall(start_fn.value, start_fn.ty, &.{ state, count }, fnRet(program_, start_fn.ty));
            var cursor = try self.unwrap(started, fnRet(program_, start_fn.ty), on_err);
            const payloads = try self.b.allocator.alloc(LocalId, payload_types.len);
            defer self.b.allocator.free(payloads);
            for (payload_types, payloads, 0..) |payload_ty, *payload_value, index| {
                const parsed = try self.parseWith(payload_ty, cursor, ret_ty, on_err);
                payload_value.* = parsed.value;
                cursor = parsed.rest;
                if (index + 1 < payload_types.len) {
                    const separated = try self.b.emitErasedCall(next_fn.value, next_fn.ty, &.{ cursor, try self.u64Value(index + 1), count }, fnRet(program_, next_fn.ty));
                    cursor = try self.unwrap(separated, fnRet(program_, next_fn.ty), on_err);
                }
            }
            const finished_try = try self.b.emitErasedCall(finish_fn.value, finish_fn.ty, &.{ cursor, count }, fnRet(program_, finish_fn.ty));
            const rest = try self.unwrap(finished_try, fnRet(program_, finish_fn.ty), on_err);
            try self.ret(try self.parsedOk(ret_ty, try self.b.buildTag(value_ty, tag_info.name, payloads), rest));
            self.b.hole = miss;
        }
        // A tag name the union lacks.
        try self.ret(try self.tryErr(ret_ty, missing));
        try self.finishErased(frame);
    }

    // -- Field sets --

    const Bound = enum { shortest, longest };

    /// Loop over the elements of `list` (of `list_ty`): `body` emits one
    /// element's work at `index` and returns the next `acc`. Returns the
    /// final `acc`.
    fn foldList(self: *Gen, list: LocalId, list_ty: TypeId, acc_ty: TypeId, init: LocalId, context: anytype, comptime body: fn (@TypeOf(context), *Gen, LocalId, LocalId, LocalId) Allocator.Error!LocalId) Allocator.Error!LocalId {
        const b = self.b;
        const elem_ty = self.store().get(list_ty).list;
        const len = try self.lowLevelLayout(.list_len, &.{list}, .u64);
        const head = b.freshJoin();
        const index = try b.closedLocal(.u64);
        const acc = try self.local(acc_ty);
        const exit = b.freshJoin();
        const result = try self.local(acc_ty);
        const after = try self.declareJoin(exit, &.{result}, &.{});
        const head_body = try self.declareJoin(head, &.{ index, acc }, &.{});
        try self.jump(head, &.{ index, acc }, &.{ try self.u64Value(0), init });
        b.store.shapes.loop = true;
        b.hole = head_body;
        const more = try self.branchIf(try self.isEq(index, len));
        try self.jump(exit, &.{result}, &.{acc});
        b.hole = more;
        const elem = try self.local(elem_ty);
        try b.emitLowLevel(elem, .list_get_unsafe, &.{ list, index });
        const next = try body(context, self, elem, acc, index);
        try self.jump(head, &.{ index, acc }, &.{ try self.addOne(index), next });
        b.hole = after;
        return result;
    }

    fn renameFields(self: *Gen, fields: LocalId, rename: LocalId, rename_ty: TypeId, ret_ty: TypeId) Allocator.Error!LocalId {
        const Context = struct { rename: LocalId, rename_ty: TypeId, list_ty: TypeId };
        const empty = try self.local(ret_ty);
        try self.b.emitLowLevel(empty, .list_with_capacity, &.{try self.lowLevelLayout(.list_len, &.{fields}, .u64)});
        return self.foldList(fields, ret_ty, ret_ty, empty, Context{ .rename = rename, .rename_ty = rename_ty, .list_ty = ret_ty }, struct {
            fn each(ctx: Context, gen: *Gen, handle: LocalId, acc: LocalId, _: LocalId) Allocator.Error!LocalId {
                const handle_ty = gen.store().get(ctx.list_ty).list;
                const old_name = (try gen.field(handle, handle_ty, "name")).value;
                const renamed_name = try gen.b.emitErasedCall(ctx.rename, ctx.rename_ty, &.{old_name}, try gen.strTy());
                const index = (try gen.field(handle, handle_ty, "index")).value;
                const renamed_handle = try gen.fieldHandleAt(handle_ty, index, renamed_name);
                return gen.append(acc, ctx.list_ty, renamed_handle);
            }
        }.each);
    }

    /// `Encoding.FieldName` at a runtime position.
    fn fieldHandleAt(self: *Gen, handle_ty: TypeId, index: LocalId, name_value: LocalId) Allocator.Error!LocalId {
        var fields_buf: [3]types.Field = undefined;
        const fields = fields_buf[0..recordFields(self.program(), handle_ty).len];
        @memcpy(fields, recordFields(self.program(), handle_ty));
        var values: [3]LocalId = undefined;
        for (fields, &values) |field_info, *value| {
            const text = self.store().nameText(field_info.name);
            value.* = if (std.mem.eql(u8, text, "index"))
                index
            else if (std.mem.eql(u8, text, "name"))
                name_value
            else if (std.mem.eql(u8, text, "name_len"))
                try self.lowLevelLayout(.str_count_utf8_bytes, &.{name_value}, .u64)
            else
                invariant("a field handle has an unexpected field");
        }
        return self.b.buildStruct(handle_ty, &values);
    }

    /// The shortest or longest name length of a field set; 0 when empty.
    fn nameBound(self: *Gen, fields: LocalId, fields_ty: TypeId, bound: Bound) Allocator.Error!LocalId {
        const u64_ty = try self.u64Ty();
        const Context = struct { bound: Bound, fields_ty: TypeId };
        // The accumulator is the bound so far, `none` before the first.
        const none = try self.u64Value(std.math.maxInt(u64));
        const folded = try self.foldList(fields, fields_ty, u64_ty, none, Context{ .bound = bound, .fields_ty = fields_ty }, struct {
            fn each(ctx: Context, gen: *Gen, handle: LocalId, acc: LocalId, _: LocalId) Allocator.Error!LocalId {
                const handle_ty = gen.store().get(ctx.fields_ty).list;
                const len = (try gen.field(handle, handle_ty, "name_len")).value;
                const merged = try gen.merge(try gen.u64Ty());
                const first = try gen.isEq(acc, try gen.u64Value(std.math.maxInt(u64)));
                const later = try gen.branchIf(first);
                try gen.jump(merged.id, &.{merged.param}, &.{len});
                gen.b.hole = later;
                const replaces = try gen.lowLevelLayout(switch (ctx.bound) {
                    .shortest => .num_is_lt,
                    .longest => .num_is_gt,
                }, &.{ len, acc }, .bool);
                const keep = try gen.branchIf(replaces);
                try gen.jump(merged.id, &.{merged.param}, &.{len});
                gen.b.hole = keep;
                try gen.jump(merged.id, &.{merged.param}, &.{acc});
                gen.b.hole = merged.after;
                return merged.param;
            }
        }.each);
        const merged = try self.merge(u64_ty);
        const nonempty = try self.branchIf(try self.isEq(folded, none));
        try self.jump(merged.id, &.{merged.param}, &.{try self.u64Value(0)});
        self.b.hole = nonempty;
        try self.jump(merged.id, &.{merged.param}, &.{folded});
        self.b.hole = merged.after;
        return merged.param;
    }

    /// The iterator over a field set's handles from `position`: all of them
    /// (with a known length), or those whose name has `size` bytes.
    fn fieldNamesIter(self: *Gen, fields: LocalId, size: ?LocalId, iter_ty: TypeId) Allocator.Error!LocalId {
        return self.iterFrom(fields, try self.u64Value(0), size orelse try self.u64Value(0), size != null, iter_ty);
    }

    fn iterFrom(self: *Gen, fields: LocalId, position: LocalId, size: LocalId, filtered: bool, iter_ty: TypeId) Allocator.Error!LocalId {
        const program_ = self.program();
        const backing = try program_.converter().backing(iter_ty);
        const iter_fields = try self.b.allocator.dupe(types.Field, recordFields(program_, backing));
        defer self.b.allocator.free(iter_fields);
        var step_fn: TypeId = undefined;
        var len_ty: TypeId = undefined;
        for (iter_fields) |field_info| {
            if (std.mem.eql(u8, self.store().nameText(field_info.name), "step")) step_fn = field_info.ty else len_ty = field_info.ty;
        }
        const fields_ty = try self.fieldsTypeOf(iter_ty);
        const u64_ty = try self.u64Ty();
        const worker = try pieceWorker(program_, self.b.module, .{ .family = no_family, .kind = .field_names_step, .a = @backingInt(iter_ty), .b = @intFromBool(filtered) }, &.{}, fnRet(program_, step_fn), &.{ fields_ty, u64_ty, u64_ty }, true);
        const step = try self.b.packClosure(worker, &.{ fields, position, size }, &.{ fields_ty, u64_ty, u64_ty }, &.{}, &.{});
        const len = if (filtered)
            try self.tag(len_ty, "Unknown", &.{})
        else
            try self.tag(len_ty, "Known", &.{try self.lowLevelLayout(.num_int_sub_wrap, &.{ try self.lowLevelLayout(.list_len, &.{fields}, .u64), position }, .u64)});
        var values: [2]LocalId = undefined;
        for (iter_fields, &values) |field_info, *value| {
            value.* = if (std.mem.eql(u8, self.store().nameText(field_info.name), "step")) step else len;
        }
        return self.b.buildStruct(iter_ty, &values);
    }

    /// The field set an iterator of field handles iterates.
    fn fieldsTypeOf(self: *Gen, iter_ty: TypeId) Allocator.Error!TypeId {
        const args = switch (self.store().get(iter_ty)) {
            .nominal => |nominal| self.store().nominalArgs(nominal),
            else => invariant("a field set iterator is not an Iter"),
        };
        return self.store().intern(.{ .list = args[0] });
    }

    fn lowerFieldNamesStep(self: *Gen, iter_ty: TypeId) Allocator.Error!void {
        const filtered = self.b.program.codec.pieces.items[self.piece].b != 0;
        const frame = try self.beginErased();
        const worker = self.b.currentWorker().*;
        const fields = frame.captures[0];
        const position = frame.captures[1];
        const size = frame.captures[2];
        const step_ty = worker.ret;
        const fields_ty = worker.capture_types[0];
        const handle_ty = self.store().get(fields_ty).list;
        const b = self.b;
        const len = try self.lowLevelLayout(.list_len, &.{fields}, .u64);
        const head = b.freshJoin();
        const index = try b.closedLocal(.u64);
        const head_body = try self.declareJoin(head, &.{index}, &.{});
        try self.jump(head, &.{index}, &.{position});
        b.store.shapes.loop = true;
        b.hole = head_body;
        const more = try self.branchIf(try self.isEq(index, len));
        try self.ret(try self.tag(step_ty, "Done", &.{}));
        b.hole = more;
        const handle = try self.local(handle_ty);
        try b.emitLowLevel(handle, .list_get_unsafe, &.{ fields, index });
        if (filtered) {
            const name_len = (try self.field(handle, handle_ty, "name_len")).value;
            const matches = try self.branchIf(try self.lowLevelLayout(.bool_not, &.{try self.isEq(name_len, size)}, .bool));
            try self.jump(head, &.{index}, &.{try self.addOne(index)});
            b.hole = matches;
        }
        const rest = try self.iterFrom(fields, try self.addOne(index), size, filtered, iter_ty);
        const one = tagByText(self.program(), try b.structuralType(step_ty), "One") orelse invariant("an iterator step has no One tag");
        const one_ty = self.store().span(one.payloads)[0];
        var values: [2]LocalId = undefined;
        for (recordFields(self.program(), one_ty), &values) |field_info, *value| {
            value.* = if (std.mem.eql(u8, self.store().nameText(field_info.name), "item")) handle else rest;
        }
        try self.ret(try self.tag(step_ty, "One", &.{try b.buildStruct(one_ty, &values)}));
        try self.finishErased(frame);
    }
};

fn unionTags(program: *Program, ty: TypeId) []const types.Tag {
    return switch (program.store.get(ty)) {
        .tag_union => |tag_union| program.store.tagSpan(tag_union.tags),
        else => invariant("a generated codec expected a tag union"),
    };
}

/// Whether a tag has a payload no value inhabits: the empty tag union.
fn tagUninhabited(program: *Program, tag_info: types.Tag) Allocator.Error!bool {
    for (program.store.span(tag_info.payloads)) |payload_ty| {
        switch (program.store.get(payload_ty)) {
            .tag_union => |tag_union| if (tag_union.ext == null and tag_union.tags.len == 0) return true,
            else => {},
        }
    }
    return false;
}

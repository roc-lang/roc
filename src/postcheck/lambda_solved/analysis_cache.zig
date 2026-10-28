//! Completed lambda analysis over an exactly identified lifted input.
const std = @import("std");
const base = @import("base");
const check = @import("check");
const Lifted = @import("../monotype_lifted/ast.zig");
const Mono = @import("../monotype/type.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");
const Codec = @import("../monotype/interface_summary.zig");
const names = check.CheckedNames;
const Allocator = std.mem.Allocator;

/// All these columns are read by lambda solving. Source locations, literal
/// byte backings and Fn.source metadata are carried by the current program,
/// and are never part of the analysis result. Numeric literal payloads remain
/// in the key conservatively; this format makes no value-equivalence claims.
const columns = .{
    "exprs",    "pats",        "stmts",                 "locals",                  "expr_ids",         "pat_ids",          "typed_locals",
    "stmt_ids", "field_exprs", "field_access_segments", "fn_def_captures",         "capture_operands", "record_destructs", "str_pattern_steps",
    "branches", "if_branches", "layout_requests",       "runtime_schema_requests",
};
const type_columns = .{ "types", "named_nodes", "erased_digests", "spans", "fields", "tags", "declared_fields" };

/// Exact consumed input modulo renaming of Monotype table ids. Expression,
/// function and binding tables keep their current identities. Named occurrence
/// references are rebound through the matching current input, never an old
/// checked module. Their equality partition remains part of the key.
pub const Identity = struct {
    key: [32]u8,
    occurrences: []Mono.NamedType,
    pub fn deinit(self: Identity, allocator: Allocator) void {
        allocator.free(self.occurrences);
    }
};

/// The identity of a lifted program as lambda analysis reads it: its types,
/// named type occurrences, and functions.
pub fn identity(allocator: Allocator, input: Lifted.ProgramView) Allocator.Error!Identity {
    var scratch = std.heap.ArenaAllocator.init(allocator);
    defer scratch.deinit();
    const gpa = scratch.allocator();
    const ids = try gpa.alloc(u32, input.types.types.len);
    @memset(ids, std.math.maxInt(u32));
    var occurrences: std.AutoHashMapUnmanaged(Mono.NamedType, u32) = .empty;
    try occurrences.ensureTotalCapacity(gpa, @intCast(input.types.named_nodes.len));
    var out = Fingerprint{
        .names = input.names,
        .types = input.types,
        .type_ids = ids,
        .pending_types = try gpa.alloc(Mono.TypeId, ids.len),
        .occurrence_ids = &occurrences,
        .pending_occurrences = try gpa.alloc(Mono.NamedType, input.types.named_nodes.len),
    };
    out.bytes("roc.lambda-analysis-input.v3");
    inline for (columns) |field| {
        out.value(@TypeOf(@field(input, field)), @field(input, field));
    }
    out.value(usize, input.fns.len);
    for (input.fns) |f| {
        out.value(@TypeOf(f.symbol), f.symbol);
        out.value(@TypeOf(f.signature), f.signature);
        out.value(@TypeOf(f.args), f.args);
        out.value(@TypeOf(f.captures), f.captures);
        out.value(@TypeOf(f.body), f.body);
        out.value(@TypeOf(f.ret), f.ret);
    }
    out.value(usize, input.roots.len);
    for (input.roots) |root| {
        out.value(@TypeOf(root.fn_id), root.fn_id);
        out.value(@TypeOf(root.request.abi), root.request.abi);
    }
    out.bytes("type-graph");
    var index: usize = 0;
    while (index < out.type_count) : (index += 1) out.typeContent(input.types.get(out.pending_types[index]));
    out.value(usize, out.type_count);
    return .{ .key = out.finish(), .occurrences = try allocator.dupe(Mono.NamedType, out.pending_occurrences[0..out.occurrence_count]) };
}

/// Developer trace of the exact input columns, for explaining invalidation.
pub fn traceIdentity(input: Lifted.ProgramView) void {
    inline for (columns) |field| {
        var out = Fingerprint{ .names = input.names };
        out.value(@TypeOf(@field(input, field)), @field(input, field));
        std.debug.print("lambda-input {s}={x}\n", .{ field, out.finish() });
    }
    inline for (type_columns) |field| {
        var out = Fingerprint{ .names = input.names };
        out.value(@TypeOf(@field(input.types, field)), @field(input.types, field));
        std.debug.print("lambda-input types.{s}={x}\n", .{ field, out.finish() });
    }
    inline for (@typeInfo(Lifted.Local).@"struct".field_names) |field| {
        var out = Fingerprint{ .names = input.names };
        for (input.locals) |local| out.value(@TypeOf(@field(local, field)), @field(local, field));
        std.debug.print("lambda-input locals.{s}={x}\n", .{ field, out.finish() });
    }
    var fns = Fingerprint{ .names = input.names };
    for (input.fns) |f| {
        fns.value(@TypeOf(f.symbol), f.symbol);
        fns.value(@TypeOf(f.signature), f.signature);
        fns.value(@TypeOf(f.args), f.args);
        fns.value(@TypeOf(f.captures), f.captures);
        fns.value(@TypeOf(f.body), f.body);
        fns.value(@TypeOf(f.ret), f.ret);
    }
    std.debug.print("lambda-input fns={x}\n", .{fns.finish()});
}

const Fingerprint = struct {
    names: *const names.NameStore,
    types: ?Mono.Store.View = null,
    type_ids: []u32 = &.{},
    pending_types: []Mono.TypeId = &.{},
    type_count: u32 = 0,
    occurrence_ids: ?*std.AutoHashMapUnmanaged(Mono.NamedType, u32) = null,
    pending_occurrences: []Mono.NamedType = &.{},
    occurrence_count: u32 = 0,
    hash: base.Sha256 = .init(.{}),
    buffer: [4096]u8 = undefined,
    used: usize = 0,
    fn bytes(self: *Fingerprint, data: []const u8) void {
        if (data.len > self.buffer.len - self.used) {
            self.hash.update(self.buffer[0..self.used]);
            self.used = 0;
            if (data.len >= self.buffer.len) {
                self.hash.update(data);
                return;
            }
        }
        @memcpy(self.buffer[self.used..][0..data.len], data);
        self.used += data.len;
    }
    fn finish(self: *Fingerprint) [32]u8 {
        self.hash.update(self.buffer[0..self.used]);
        return self.hash.finalResult();
    }
    fn typeSpan(self: *Fingerprint, comptime field: []const u8, span: Mono.Span) void {
        const values = @field(self.types.?, field)[span.start..][0..span.len];
        self.value(@TypeOf(values), values);
    }
    fn typeContent(self: *Fingerprint, content: Mono.Content) void {
        self.value(std.meta.Tag(Mono.Content), std.meta.activeTag(content));
        switch (content) {
            .primitive => |v| self.value(@TypeOf(v), v),
            .erased => |v| self.value(@TypeOf(v), v),
            .zst, .hole => {},
            .list, .box => |v| self.value(Mono.TypeId, v),
            .tuple => |v| self.typeSpan("spans", v),
            .record => |v| self.typeSpan("fields", v),
            .func => |v| {
                self.typeSpan("spans", v.args);
                self.value(Mono.TypeId, v.ret);
            },
            .tag_union => |v| {
                self.value(u32, v.len);
                for (self.types.?.tags[v.start..][0..v.len]) |tag| {
                    self.value(names.TagNameId, tag.name);
                    self.value(names.TagNameId, tag.checked_name);
                    self.typeSpan("spans", tag.payloads);
                }
            },
            .named => |v| {
                self.value(Mono.NamedType, v.named_type);
                self.value(@TypeOf(v.def), v.def);
                self.value(@TypeOf(v.kind), v.kind);
                self.value(@TypeOf(v.builtin_owner), v.builtin_owner);
                self.typeSpan("spans", v.args);
                self.value(@TypeOf(v.backing), v.backing);
                self.typeSpan("declared_fields", v.declared_order);
            },
        }
    }
    fn value(self: *Fingerprint, comptime T: type, v: T) void {
        if (T == Lifted.Local) {
            // Checked capture provenance is copied to output, not consumed by
            // solving. Restore obtains it from the matching current local.
            self.value(@TypeOf(v.id), v.id);
            self.value(@TypeOf(v.symbol), v.symbol);
            self.value(@TypeOf(v.ty), v.ty);
            return;
        }
        if (T == Mono.TypeId) if (self.types != null) {
            const slot = &self.type_ids[@backingInt(v)];
            if (slot.* == std.math.maxInt(u32)) {
                slot.* = self.type_count;
                self.pending_types[self.type_count] = v;
                self.type_count += 1;
            }
            return self.value(u32, slot.*);
        };
        if (T == Mono.MaybeTypeId) if (self.types != null) return self.value(?Mono.TypeId, v.get());
        if (T == Mono.NamedType) if (self.occurrence_ids) |ids| {
            const entry = ids.getOrPutAssumeCapacity(v);
            if (!entry.found_existing) {
                entry.value_ptr.* = self.occurrence_count;
                self.pending_occurrences[self.occurrence_count] = v;
                self.occurrence_count += 1;
            }
            // Nominal owner is part of the meaning. The checked occurrence's raw address
            // is only used for equality by this solver; preserve that relation
            // and bind the annotation to its current occurrence on restore.
            self.bytes(&v.module.bytes);
            return self.value(u32, entry.value_ptr.*);
        };
        if (T == names.ModuleIdentityId) {
            if (self.types == null) self.value(u32, @backingInt(v));
            return self.bytes(self.names.moduleIdentityBytes(v));
        }
        if (T == names.RecordFieldNameId or T == names.TagNameId or T == names.TypeNameId or T == names.ExportNameId or T == names.ExternalSymbolNameId) {
            if (self.types == null) self.value(u32, @backingInt(v));
            const text = if (T == names.RecordFieldNameId) self.names.recordFieldLabelText(v) else if (T == names.TagNameId) self.names.tagLabelText(v) else if (T == names.TypeNameId) self.names.typeNameText(v) else if (T == names.ExportNameId) self.names.exportNameText(v) else self.names.externalSymbolNameText(v);
            return self.value([]const u8, text);
        }
        switch (@typeInfo(T)) {
            .@"struct" => |info| inline for (info.field_names, info.field_types) |field, F| self.value(F, @field(v, field)),
            .@"union" => |info| {
                self.value(info.tag_type.?, std.meta.activeTag(v));
                inline for (info.field_names, info.field_types) |field, F| if (std.meta.activeTag(v) == @field(info.tag_type.?, field)) return self.value(F, @field(v, field));
                unreachable; // the loop above matches the active tag, which is one of the union's fields
            },
            .optional => |info| {
                self.value(bool, v != null);
                if (v) |present| self.value(info.child, present);
            },
            .pointer => |info| {
                if (info.size != .slice) @compileError("analysis identity contains an object pointer: " ++ @typeName(T));
                self.value(usize, v.len);
                if (info.child == u8) return self.bytes(v);
                for (v) |element| self.value(info.child, element);
            },
            .array => |info| {
                if (info.child == u8) return self.bytes(&v);
                for (v) |element| self.value(info.child, element);
            },
            .@"enum" => |info| self.value(info.tag_type, @backingInt(v)),
            .bool => self.value(u8, @intFromBool(v)),
            .float => self.value(@Int(.unsigned, @bitSizeOf(T)), @as(@Int(.unsigned, @bitSizeOf(T)), @bitCast(v))),
            .int => {
                const U = @Int(.unsigned, @max(8, @bitSizeOf(T)));
                var bits: U = @as(@Int(.unsigned, @bitSizeOf(T)), @bitCast(v));
                var bytes_: [(@bitSizeOf(T) + 7) / 8]u8 = undefined;
                for (&bytes_) |*byte| {
                    byte.* = @truncate(bits);
                    bits = std.math.shr(U, bits, 8);
                }
                self.bytes(&bytes_);
            },
            .void => {},
            .type,
            .noreturn,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .error_union,
            .error_set,
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .vector,
            .enum_literal,
            .spirv,
            => @compileError("unsupported analysis identity field: " ++ @typeName(T)),
        }
    }
};

/// Solver annotations only. The lowered IR always comes from this build.
const Payload = struct {
    input_identity: [32]u8,
    types: Type.Store.View,
    defs: []const Ast.Def,
    local_tys: []const Type.TypeVarId,
    expr_tys: []const Type.TypeVarId,
    pat_tys: []const Type.TypeVarId,
    fn_tys: []const Type.TypeVarId,
    layout_requests: []const Ast.LayoutRequest,
    runtime_schema_requests: []const Ast.RuntimeSchemaRequest,
};

fn payloadOf(program: *const Ast.Program, key: [32]u8) Payload {
    return .{
        .input_identity = key,
        .types = program.types.view(),
        .defs = program.defs.items,
        .local_tys = program.local_tys.items,
        .expr_tys = program.expr_tys.items,
        .pat_tys = program.pat_tys.items,
        .fn_tys = program.fn_tys.items,
        .layout_requests = program.layout_requests.items,
        .runtime_schema_requests = program.runtime_schema_requests.items,
    };
}

/// Copy only the completed graph reachable from output annotations. Solver
/// scratch rows (including abandoned lazy leaves) have no persistent meaning.
/// Reserve a destination before visiting its children, preserving cycles and
/// sharing without recursive traversal of user types.
const CompletedClosure = struct {
    allocator: Allocator,
    input: Type.Store.View,
    output: Type.Store,
    ids: []Type.MaybeTypeVarId,
    pending: std.ArrayList(Type.TypeVarId) = .empty,
    spans: std.AutoHashMapUnmanaged(SpanKey, Type.Span) = .empty,
    const Column = enum { spans, fields, tags, captures, fn_members, declared_fields };
    const SpanKey = struct { column: Column, span: Type.Span };

    fn copy(allocator: Allocator, original: Payload) Allocator.Error!Payload {
        const ids = try allocator.alloc(Type.MaybeTypeVarId, original.types.vars.len);
        @memset(ids, .none);
        var self = CompletedClosure{ .allocator = allocator, .input = original.types, .output = Type.Store.init(allocator), .ids = ids };
        var result: Payload = undefined;
        inline for (@typeInfo(Payload).@"struct".field_names) |field| {
            if (comptime !std.mem.eql(u8, field, "types")) @field(result, field) = try self.value(@TypeOf(@field(original, field)), @field(original, field));
        }
        var index: usize = 0;
        while (index < self.pending.items.len) : (index += 1) {
            const source = self.pending.items[index];
            const node = try self.content(self.input.get(source));
            try self.output.set(@fromBackingInt(@as(u32, @intCast(index))), node);
        }
        result.types = self.output.view();
        return result;
    }

    fn ref(self: *CompletedClosure, id: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const root = self.input.root(id);
        if (self.ids[@backingInt(root)].get()) |existing| return existing;
        const fresh = try self.output.add(.unbound);
        self.ids[@backingInt(root)] = .of(fresh);
        try self.pending.append(self.allocator, root);
        return fresh;
    }

    fn value(self: *CompletedClosure, comptime T: type, v: T) Allocator.Error!T {
        if (T == Type.TypeVarId) return self.ref(v);
        return switch (@typeInfo(T)) {
            .@"struct" => |info| blk: {
                var out: T = undefined;
                inline for (info.field_names, info.field_types) |field, F| @field(out, field) = try self.value(F, @field(v, field));
                break :blk out;
            },
            .@"union" => |info| blk: {
                inline for (info.field_names, info.field_types) |field, F| if (std.meta.activeTag(v) == @field(info.tag_type.?, field)) break :blk @unionInit(T, field, try self.value(F, @field(v, field)));
                unreachable; // the loop above matches the active tag, which is one of the union's fields
            },
            .optional => |info| if (v) |present| try self.value(info.child, present) else null,
            .pointer => |info| blk: {
                if (info.size != .slice) @compileError("completed annotations contain owned slices");
                const out = try self.allocator.alloc(info.child, v.len);
                for (v, out) |source, *dest| dest.* = try self.value(info.child, source);
                break :blk out;
            },
            .type,
            .void,
            .bool,
            .noreturn,
            .int,
            .float,
            .array,
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
            => v,
        };
    }

    fn span(self: *CompletedClosure, comptime column: Column, old: Type.Span) Allocator.Error!Type.Span {
        if (old.len == 0) return .empty();
        const key = SpanKey{ .column = column, .span = old };
        if (self.spans.get(key)) |existing| return existing;
        const input = @field(self.input, @tagName(column))[old.start..][0..old.len];
        const list = &@field(self.output, @tagName(column));
        const start = std.math.cast(u32, list.items.len) orelse return error.OutOfMemory;
        const result = Type.Span{ .start = start, .len = old.len };
        // Reserve the whole span first; processing member captures or tag
        // payloads only appends to different columns.
        const slots = try list.addManyAsSlice(self.allocator, old.len);
        for (input, slots) |item, *slot| {
            slot.* = try self.value(@TypeOf(item), item);
            if (comptime column == .tags) slot.payloads = try self.span(.spans, item.payloads);
            if (comptime column == .fn_members) slot.captures = try self.span(.captures, item.captures);
        }
        try self.spans.put(self.allocator, key, result);
        return result;
    }

    fn content(self: *CompletedClosure, old: Type.Content) Allocator.Error!Type.Content {
        return switch (old) {
            .link, .mono => @import("../common.zig").invariant("completed lambda output reached private solver state"),
            .unbound, .forall, .primitive, .zst, .hole => old,
            .list => |id| .{ .list = try self.ref(id) },
            .box => |id| .{ .box = try self.ref(id) },
            .tuple => |items| .{ .tuple = try self.span(.spans, items) },
            .record => |items| .{ .record = try self.span(.fields, items) },
            .tag_union => |items| .{ .tag_union = try self.span(.tags, items) },
            .lambda_set => |items| .{ .lambda_set = try self.span(.fn_members, items) },
            .func => |f| .{ .func = .{ .args = try self.span(.spans, f.args), .callable = try self.ref(f.callable), .ret = try self.ref(f.ret) } },
            .named => |n| blk: {
                var out = n;
                out.args = try self.span(.spans, n.args);
                out.declared_order = try self.span(.declared_fields, n.declared_order);
                if (out.backing) |*backing| backing.ty = try self.ref(backing.ty);
                break :blk .{ .named = out };
            },
            .erased => |e| blk: {
                var out = e;
                out.members = try self.span(.fn_members, e.members);
                if (out.abi_fn) |ty| out.abi_fn = try self.ref(ty);
                break :blk .{ .erased = out };
            },
        };
    }
};

const Context = struct {
    allocator: Allocator,
    input: Lifted.ProgramView,
    key: [32]u8,
    named: std.AutoHashMapUnmanaged(Mono.NamedType, u32) = .empty,
    occurrences: []const Mono.NamedType,
    modules: std.AutoHashMapUnmanaged([32]u8, void) = .empty,
    defaults: std.AutoHashMapUnmanaged([36]u8, void) = .empty,

    fn init(allocator: Allocator, input: Lifted.ProgramView, id: *const Identity) Allocator.Error!Context {
        var self = Context{ .allocator = allocator, .input = input, .key = id.key, .occurrences = id.occurrences };
        errdefer self.deinit();
        for (id.occurrences, 0..) |named, index| try self.named.put(allocator, named, @intCast(index));
        for (input.types.named_nodes) |named| {
            try self.modules.put(allocator, input.names.moduleIdentityBytes(named.def.module).*, {});
        }
        for (input.types.fields) |field| if (field.default) |default| {
            const module = input.names.moduleIdentityBytes(default.module).*;
            try self.modules.put(allocator, module, {});
            try self.defaults.put(allocator, defaultKey(module, default.expr_node), {});
        };
        for (input.runtime_schema_requests) |request| try self.modules.put(allocator, input.names.moduleIdentityBytes(request.def.module).*, {});
        return self;
    }
    fn deinit(self: *Context) void {
        self.defaults.deinit(self.allocator);
        self.modules.deinit(self.allocator);
        self.named.deinit(self.allocator);
    }
    fn defaultKey(module: [32]u8, expr: u32) [36]u8 {
        var key: [36]u8 = undefined;
        key[0..32].* = module;
        std.mem.writeInt(u32, key[32..36], expr, .little);
        return key;
    }
    pub fn defaultDeclaringModule(self: *const Context, module: [32]u8, expr: u32) ?[32]u8 {
        // Analysis only carries a current input default reference; it never
        // persists its lowered body. Its validation token is the complete
        // input identity, rather than a newly reconstructed checked module.
        return if (self.defaults.contains(defaultKey(module, expr))) self.key else null;
    }
    pub fn validModuleIdentity(self: *const Context, module: [32]u8) bool {
        return self.modules.contains(module);
    }
    pub fn describeNamedType(self: *const Context, named: Mono.NamedType) Codec.NamedOccurrence {
        return .{ .input = self.named.get(named) orelse @import("../common.zig").invariant("completed annotation named an occurrence outside its input") };
    }
    pub fn resolveNamedType(self: *const Context, stored: Codec.NamedOccurrence) Allocator.Error!?Mono.NamedType {
        if (stored != .input or stored.input >= self.occurrences.len) return null;
        return self.occurrences[stored.input];
    }
    pub fn validNamedType(self: *const Context, named: Mono.NamedType) bool {
        return self.named.contains(named);
    }
    pub fn validNamedDefinition(self: *const Context, _: *const names.NameStore, named: Mono.NamedType, _: Mono.TypeDef, _: Mono.NamedKind, _: ?check.StaticDispatchRegistry.BuiltinOwner) bool {
        return self.validNamedType(named);
    }
    pub fn validateFrozenPayload(self: *const Context, payload: Payload) Codec.DecodeError!void {
        if (!std.mem.eql(u8, &payload.input_identity, &self.key)) return error.InvalidSummary;
        try validate(self.allocator, self.input, payload);
    }
};

fn validVar(id: Type.TypeVarId, len: usize) Codec.DecodeError!void {
    if (@backingInt(id) >= len) return error.InvalidSummary;
}
fn validSpan(span: Type.Span, len: usize) Codec.DecodeError!void {
    if (span.start > len or span.len > len - span.start) return error.InvalidSummary;
}
fn validate(allocator: Allocator, input: Lifted.ProgramView, p: Payload) Codec.DecodeError!void {
    if (p.defs.len != input.fns.len or p.fn_tys.len != input.fns.len or p.local_tys.len != input.locals.len or p.expr_tys.len != input.exprs.len or p.pat_tys.len != input.pats.len or p.layout_requests.len != input.layout_requests.len or p.runtime_schema_requests.len != input.runtime_schema_requests.len) return error.InvalidSummary;
    const t = p.types;
    const count = t.vars.len;
    for (t.vars) |node| switch (node) {
        .link, .list, .box => |id| try validVar(id, count),
        .named => |id| if (id >= t.named_payloads.len) return error.InvalidSummary,
        .erased => |id| if (id >= t.erased_payloads.len) return error.InvalidSummary,
        .record => |span| try validSpan(span, t.fields.len),
        .tag_union => |span| try validSpan(span, t.tags.len),
        .tuple => |span| try validSpan(span, t.spans.len),
        .lambda_set => |span| try validSpan(span, t.fn_members.len),
        .func => |func| {
            try validSpan(func.args, t.spans.len);
            try validVar(func.callable, count);
            try validVar(func.ret, count);
        },
        .mono => return error.InvalidSummary,
        .primitive, .unbound, .forall, .zst, .hole => {},
    };
    for (t.named_payloads) |named| {
        try validSpan(named.args, t.spans.len);
        try validSpan(named.declared_order, t.declared_fields.len);
        if (named.backing) |backing| try validVar(backing.ty, count);
    }
    for (t.erased_payloads) |erased| {
        try validSpan(erased.members, t.fn_members.len);
        if (erased.abi_fn) |ty| try validVar(ty, count);
    }
    for (t.spans) |ty| try validVar(ty, count);
    for (t.fields) |field| {
        try validVar(field.ty, count);
        if (field.value_ty) |ty| try validVar(ty, count);
    }
    for (t.tags) |tag| try validSpan(tag.payloads, t.spans.len);
    for (t.declared_fields) |field| if (field == .padding) try validVar(field.padding, count);
    var symbols = std.AutoHashMapUnmanaged(@import("../common.zig").Symbol, void).empty;
    defer symbols.deinit(allocator);
    for (input.fns) |func| try symbols.put(allocator, func.symbol, {});
    for (t.fn_members) |member| {
        if (!symbols.contains(member.lambda)) return error.InvalidSummary;
        try validSpan(member.captures, t.captures.len);
    }
    for (t.captures) |capture| {
        if (@backingInt(capture.local) >= input.locals.len) return error.InvalidSummary;
        const local = input.locals[@backingInt(capture.local)];
        if (capture.symbol != local.symbol) return error.InvalidSummary;
        try validVar(capture.ty, count);
    }
    inline for (.{ "local_tys", "expr_tys", "pat_tys", "fn_tys" }) |field| for (@field(p, field)) |ty| try validVar(ty, count);
    for (p.defs, input.fns, p.fn_tys) |def, func, ty| {
        if (def.symbol != func.symbol or def.ty != ty) return error.InvalidSummary;
        switch (def.body) {
            .roc => |expr| if (func.body != .roc or func.body.roc != expr) return error.InvalidSummary,
            .hosted => if (func.body != .hosted) return error.InvalidSummary,
        }
    }
    for (p.layout_requests, input.layout_requests) |result, request| {
        if (result.checked_type != request.checked_type or result.fn_id != request.fn_id or !std.meta.eql(result.const_locator, request.const_locator)) return error.InvalidSummary;
        try validVar(result.ty, count);
    }
    for (p.runtime_schema_requests, input.runtime_schema_requests) |result, request| {
        if (!std.meta.eql(result.def, request.def)) return error.InvalidSummary;
        try validVar(result.ty, count);
    }
    // Redirect cycles would hang readers. Validate every chain once, including
    // unreachable rows: optional records never enter the compiler as invariants.
    const colors = try allocator.alloc(enum { unseen, active, done }, count);
    defer allocator.free(colors);
    @memset(colors, .unseen);
    for (0..count) |start| {
        var index = start;
        while (colors[index] == .unseen) {
            colors[index] = .active;
            if (t.vars[index] != .link) break;
            index = @backingInt(t.vars[index].link);
            if (colors[index] == .active) return error.InvalidSummary;
        }
        index = start;
        while (colors[index] == .active) {
            colors[index] = .done;
            if (t.vars[index] != .link) break;
            index = @backingInt(t.vars[index].link);
        }
    }
    for (p.fn_tys) |ty| if (t.rootContent(ty) != .func) return error.InvalidSummary;
}

/// Encode only annotations; no source or lowered payload is persisted.
pub fn encode(allocator: Allocator, program: *Ast.Program, id: *const Identity) Allocator.Error!?[]u8 {
    var context = try Context.init(allocator, program.lifted.view(), id);
    defer context.deinit();
    var scratch = std.heap.ArenaAllocator.init(allocator);
    defer scratch.deinit();
    const payload = try CompletedClosure.copy(scratch.allocator(), payloadOf(program, id.key));
    return Codec.encodeFrozen(Payload, "LSAN0002".*, allocator, &program.lifted.types, &program.lifted.names, payload, &context, null) catch |err| switch (err) {
        error.NotRepresentable => return null,
        error.OutOfMemory => return error.OutOfMemory,
    };
}

/// On success transfers the current lifted program into the restored result.
/// Invalid optional data leaves the current program available for fresh solving.
pub fn restore(allocator: Allocator, lifted: *Lifted.Program, id: *const Identity, bytes: []const u8) Allocator.Error!?Ast.Program {
    const arena = try allocator.create(std.heap.ArenaAllocator);
    arena.* = .init(allocator);
    var owned = true;
    defer if (owned) {
        arena.deinit();
        allocator.destroy(arena);
    };
    var context = try Context.init(allocator, lifted.view(), id);
    defer context.deinit();
    const payload = Codec.decodeFrozen(Payload, "LSAN0002".*, arena.allocator(), &lifted.types, &lifted.names, bytes, &context, null) catch |err| switch (err) {
        error.InvalidSummary => return null,
        error.OutOfMemory => return error.OutOfMemory,
    };
    // The validated input binds every capture to the same current local. Its
    // checked provenance belongs to this build, even when solving is reused.
    for (@constCast(payload.types.captures)) |*capture| {
        const local = lifted.getLocal(capture.local);
        capture.binder = local.identity.binder();
        capture.capture_id = local.identity.captureId();
        capture.checked_capture_id = local.identity.checkedCaptureId();
        capture.capture_kind = local.capture_kind;
    }
    // This producer's completed graph has no private solver state. Ownership
    // flags are meaningful only while solving and start empty on immutable reuse.
    const flags = try arena.allocator().alloc(bool, payload.types.vars.len);
    @memset(flags, false);
    var result = Ast.Program.init(arena.allocator(), lifted.*);
    result.analysis_backing = arena;
    inline for (.{ "defs", "local_tys", "expr_tys", "pat_tys", "fn_tys", "layout_requests", "runtime_schema_requests" }) |field| {
        @field(result, field) = .fromOwnedSlice(@constCast(@field(payload, field)));
    }
    inline for (@typeInfo(Type.Store.View).@"struct".field_names) |field| {
        @field(result.types, field) = .fromOwnedSlice(@constCast(@field(payload.types, field)));
    }
    result.types.owned_named_backings = .fromOwnedSlice(flags);
    owned = false;
    lifted.* = undefined;
    return result;
}

fn emptyInput(allocator: Allocator) Lifted.Program {
    return Lifted.Program.init(
        allocator,
        names.NameStore.init(allocator),
        Mono.Store.init(allocator),
        .empty, // const_fn_evidence
        .empty, // const_fn_evidence_targets
        .empty, // const_fn_evidence_structurals
        .empty, // const_fn_evidence_frames
        .empty, // exprs
        .empty, // pats
        .empty, // stmts
        .empty, // locals
        .empty, // expr_ids
        .empty, // pat_ids
        .empty, // typed_locals
        .empty, // stmt_ids
        .empty, // field_exprs
        .empty, // field_access_segments
        .empty, // fn_def_captures
        .empty, // capture_operands
        .empty, // record_destructs
        .empty, // str_pattern_steps
        .empty, // branches
        .empty, // if_branches
        .empty, // string_literals
        Lifted.ProcDebugNameMap.init(allocator),
        .empty, // source_files
        .empty, // expr_locs
        .empty, // expr_regions
        .empty, // stmt_locs
        .empty, // stmt_regions
        .empty, // inline_scopes
        .empty, // expr_inline_scopes
        .empty, // stmt_inline_scopes
        .empty, // local_names
        .empty, // static_data_values
        .empty, // comptime_sites
        0, // next_symbol
    );
}

fn literalInput(allocator: Allocator, text: []const u8) Allocator.Error!Lifted.Program {
    var input = emptyInput(allocator);
    errdefer input.deinit();
    const ty = try input.types.internPrimitive(&input.names, .str);
    const literal = try input.addStringLiteral(text);
    const body = try input.addExpr(.{ .ty = ty, .data = .{ .str_lit = literal } });
    _ = try input.addFn(.{ .symbol = @fromBackingInt(3), .args = .empty(), .captures = .empty(), .body = .{ .roc = body }, .ret = ty });
    return input;
}

test "lambda analysis reuse keeps the current literal and reporting metadata" {
    const gpa = std.testing.allocator;
    var first = try @import("solve.zig").run(gpa, try literalInput(gpa, "before"));
    defer first.deinit();
    const id = try identity(gpa, first.lifted.view());
    defer id.deinit(gpa);
    const key = id.key;
    const bytes = (try encode(gpa, &first, &id)).?;
    defer gpa.free(bytes);
    var current = try literalInput(gpa, "after with different length");
    var current_owned = true;
    defer if (current_owned) current.deinit();
    current.current_loc = .{ .file = 0, .line = 88, .column = 3 };
    current.types.freeze();
    const current_id = try identity(gpa, current.view());
    defer current_id.deinit(gpa);
    try std.testing.expectEqual(key, current_id.key);
    var restored = (try restore(gpa, &current, &id, bytes)).?;
    current_owned = false;
    defer restored.deinit();
    try std.testing.expectEqualStrings("after with different length", restored.lifted.view().string_literals[0].text());
    try std.testing.expectEqual(@as(u32, 88), restored.lifted.current_loc.line);
    const func = restored.types.rootContentCompressed(restored.fn_tys.items[0]).func;
    try std.testing.expectEqual(Mono.Primitive.str, restored.types.rootContentCompressed(func.ret).primitive);
    try std.testing.expectEqual(restored.types.rootCompressed(func.ret), restored.types.rootCompressed(restored.expr_tys.items[0]));
}

test "lambda analysis identity includes callable topology and rejects mismatched records" {
    const gpa = std.testing.allocator;
    var first = try @import("solve.zig").run(gpa, try literalInput(gpa, "before"));
    defer first.deinit();
    const id = try identity(gpa, first.lifted.view());
    defer id.deinit(gpa);
    const key = id.key;
    const bytes = (try encode(gpa, &first, &id)).?;
    defer gpa.free(bytes);
    var current = try literalInput(gpa, "after");
    defer current.deinit();
    var changed_fn = current.getFnAt(0);
    changed_fn.ret = try current.types.internPrimitive(&current.names, .u64);
    current.setFnAt(0, changed_fn);
    const changed = try identity(gpa, current.view());
    defer changed.deinit(gpa);
    try std.testing.expect(!std.mem.eql(u8, &key, &changed.key));
    try std.testing.expectEqual(null, try restore(gpa, &current, &changed, bytes));
    for (0..bytes.len) |end| try std.testing.expectEqual(null, try restore(gpa, &current, &id, bytes[0..end]));
}

fn analysisAllocationRoundTrip(gpa: Allocator) Allocator.Error!void {
    var first = try @import("solve.zig").run(gpa, try literalInput(gpa, "before"));
    defer first.deinit();
    const id = try identity(gpa, first.lifted.view());
    defer id.deinit(gpa);
    const bytes = (try encode(gpa, &first, &id)).?;
    defer gpa.free(bytes);
    var current = try literalInput(gpa, "after");
    var owned = true;
    defer if (owned) current.deinit();
    var restored = (try restore(gpa, &current, &id, bytes)).?;
    owned = false;
    defer restored.deinit();
}

test "lambda analysis allocation failures preserve input ownership" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, analysisAllocationRoundTrip, .{});
}

fn recursiveInput(allocator: Allocator) Allocator.Error!Lifted.Program {
    return recursiveInputWithPadding(allocator, false);
}
fn recursiveInputWithPadding(allocator: Allocator, padding: bool) Allocator.Error!Lifted.Program {
    var input = emptyInput(allocator);
    errdefer input.deinit();
    if (padding) _ = try input.types.internPrimitive(&input.names, .bool);
    const root = try input.types.beginRecursive();
    const callback = try input.types.add(.{ .func = .{ .args = .empty(), .ret = root.ty } });
    input.types.finishRecursive(root, .{ .record = try input.types.addFields(&.{.{ .name = try input.names.internRecordFieldLabel("next"), .ty = callback, .default = null }}) });
    const local = try input.addLocal(@fromBackingInt(2), root.ty);
    const args = try input.addTypedLocalSpan(&.{.{ .local = local, .ty = root.ty }});
    const body = try input.addExpr(.{ .ty = root.ty, .data = .{ .local = local } });
    _ = try input.addFn(.{ .symbol = @fromBackingInt(1), .args = args, .captures = .empty(), .body = .{ .roc = body }, .ret = root.ty });
    return input;
}

test "lambda analysis preserves recursive callable relations and rejects redirect cycles" {
    const gpa = std.testing.allocator;
    var first = try @import("solve.zig").run(gpa, try recursiveInput(gpa));
    defer first.deinit();
    const id = try identity(gpa, first.lifted.view());
    defer id.deinit(gpa);
    const key = id.key;
    const bytes = (try encode(gpa, &first, &id)).?;
    defer gpa.free(bytes);
    var current = try recursiveInput(gpa);
    var owned = true;
    defer if (owned) current.deinit();
    var restored = (try restore(gpa, &current, &id, bytes)).?;
    owned = false;
    defer restored.deinit();
    const func = restored.types.rootContentCompressed(restored.fn_tys.items[0]).func;
    const argument = restored.types.spanItem(func.args, 0);
    const row = restored.types.rootContentCompressed(argument).record;
    const next = restored.types.fieldItem(row, 0);
    const callback = restored.types.rootContentCompressed(next.ty).func;
    try std.testing.expectEqual(restored.types.rootCompressed(argument), restored.types.rootCompressed(callback.ret));
    try std.testing.expectEqual(restored.types.rootCompressed(argument), restored.types.rootCompressed(restored.local_tys.items[0]));
    var bad = payloadOf(&first, key);
    const vars = try gpa.dupe(Type.StoredNode, bad.types.vars);
    defer gpa.free(vars);
    // A variable linked to itself is a cycle no encoder produces.
    const self_linked = 0;
    vars[self_linked] = .{ .link = @fromBackingInt(self_linked) };
    bad.types.vars = vars;
    var context = try Context.init(gpa, first.lifted.view(), &id);
    defer context.deinit();
    const corrupt = try Codec.encodeFrozen(Payload, "LSAN0002".*, gpa, &first.lifted.types, &first.lifted.names, bad, &context, null);
    defer gpa.free(corrupt);
    var rejected_input = try recursiveInput(gpa);
    defer rejected_input.deinit();
    try std.testing.expectEqual(null, try restore(gpa, &rejected_input, &id, corrupt));
}

test "lambda analysis identity distinguishes callbacks with the same signature" {
    const gpa = std.testing.allocator;
    var input = try literalInput(gpa, "first callback");
    defer input.deinit();
    const first_index = 0;
    const first = input.view().fns[first_index];
    const literal = try input.addStringLiteral("second callback");
    const body = try input.addExpr(.{ .ty = first.ret, .data = .{ .str_lit = literal } });
    const second = try input.addFn(.{ .symbol = @fromBackingInt(1), .args = .empty(), .captures = .empty(), .body = .{ .roc = body }, .ret = first.ret });
    const signature = try input.types.add(.{ .func = .{ .args = .empty(), .ret = first.ret } });
    const closure = try input.addExpr(.{ .ty = signature, .data = .{ .fn_ref = .{ .fn_id = @fromBackingInt(first_index), .captures = .empty() } } });
    _ = try input.addFn(.{ .symbol = @fromBackingInt(2), .args = .empty(), .captures = .empty(), .body = .{ .roc = closure }, .ret = signature });
    const before = try identity(gpa, input.view());
    defer before.deinit(gpa);
    input.setExprData(closure, .{ .fn_ref = .{ .fn_id = second, .captures = .empty() } });
    const after = try identity(gpa, input.view());
    defer after.deinit(gpa);
    try std.testing.expect(!std.mem.eql(u8, &before.key, &after.key));
}

test "lambda analysis persistence excludes abandoned lazy solver rows" {
    const gpa = std.testing.allocator;
    var first = try @import("solve.zig").run(gpa, try literalInput(gpa, "before"));
    defer first.deinit();
    const id = try identity(gpa, first.lifted.view());
    defer id.deinit(gpa);
    const clean = (try encode(gpa, &first, &id)).?;
    defer gpa.free(clean);
    _ = try first.types.add(.{ .mono = .{ .id = first.lifted.view().fns[0].ret, .ctx = 1234 } });
    const with_scratch = (try encode(gpa, &first, &id)).?;
    defer gpa.free(with_scratch);
    try std.testing.expectEqualSlices(u8, clean, with_scratch);
    var current = try literalInput(gpa, "after");
    current.types.freeze();
    var owned = true;
    defer if (owned) current.deinit();
    var restored = (try restore(gpa, &current, &id, with_scratch)).?;
    owned = false;
    defer restored.deinit();
    for (restored.types.vars.items) |node| try std.testing.expect(node != .mono and node != .link);
}

test "lambda analysis identity ignores type numbering and unreachable types" {
    const gpa = std.testing.allocator;
    var first = try recursiveInputWithPadding(gpa, false);
    defer first.deinit();
    var second = try recursiveInputWithPadding(gpa, true);
    defer second.deinit();
    const a = try identity(gpa, first.view());
    defer a.deinit(gpa);
    const b = try identity(gpa, second.view());
    defer b.deinit(gpa);
    try std.testing.expectEqual(a.key, b.key);
    try std.testing.expect(first.getFnAt(0).ret != second.getFnAt(0).ret);
}

fn occurrenceInput(allocator: Allocator, version: u8, shared: bool, owner: u8) Allocator.Error!Lifted.Program {
    var input = emptyInput(allocator);
    errdefer input.deinit();
    if (version != 0) _ = try input.types.internPrimitive(&input.names, .bool);
    const number = try input.types.internPrimitive(&input.names, .u64);
    const module = try input.names.internModuleIdentity(&@as([32]u8, @splat(1)));
    const label = try input.names.internTypeName("Value");
    var named: @FieldType(Mono.Content, "named") = .{
        .named_type = .{ .module = .{ .bytes = @splat(owner) }, .ty_module = .{ .bytes = @splat(version) }, .ty = @fromBackingInt(@as(u32, version) + 7) },
        .def = .{ .module = module, .type_name = label },
        .kind = .nominal,
        .args = .empty(),
        .backing = .{ .ty = number, .use = .inspectable },
    };
    const first = try input.types.add(.{ .named = named });
    if (!shared) named.named_type.ty = @fromBackingInt(@backingInt(named.named_type.ty) + 1);
    const second = try input.types.add(.{ .named = named });
    const a = try input.addLocal(@fromBackingInt(3), first);
    const b = try input.addLocal(@fromBackingInt(1), second);
    const args = try input.addTypedLocalSpan(&.{ .{ .local = a, .ty = first }, .{ .local = b, .ty = second } });
    const body = try input.addExpr(.{ .ty = second, .data = .{ .local = b } });
    _ = try input.addFn(.{ .symbol = @fromBackingInt(2), .args = args, .captures = .empty(), .body = .{ .roc = body }, .ret = second });
    return input;
}

test "lambda analysis rebinds checked occurrences to the current matching input" {
    const gpa = std.testing.allocator;
    var first = try @import("solve.zig").run(gpa, try occurrenceInput(gpa, 0, true, 9));
    defer first.deinit();
    const a = try identity(gpa, first.lifted.view());
    defer a.deinit(gpa);
    const bytes = (try encode(gpa, &first, &a)).?;
    defer gpa.free(bytes);
    var current = try occurrenceInput(gpa, 2, true, 9);
    var owned = true;
    defer if (owned) current.deinit();
    const b = try identity(gpa, current.view());
    defer b.deinit(gpa);
    try std.testing.expectEqual(a.key, b.key);
    const expected = current.types.get(current.getFnAt(0).ret).named.named_type;
    current.types.freeze();
    var restored = (try restore(gpa, &current, &b, bytes)).?;
    owned = false;
    defer restored.deinit();
    const func = restored.types.rootContentCompressed(restored.fn_tys.items[0]).func;
    const actual = restored.types.rootContentCompressed(func.ret).named.named_type;
    try std.testing.expectEqualDeep(expected, actual);
}

test "lambda analysis preserves checked occurrence equality and nominal ownership" {
    const gpa = std.testing.allocator;
    var a_input = try occurrenceInput(gpa, 0, true, 9);
    defer a_input.deinit();
    const a = try identity(gpa, a_input.view());
    defer a.deinit(gpa);
    var b_input = try occurrenceInput(gpa, 0, false, 9);
    defer b_input.deinit();
    const b = try identity(gpa, b_input.view());
    defer b.deinit(gpa);
    var c_input = try occurrenceInput(gpa, 0, true, 10);
    defer c_input.deinit();
    const c = try identity(gpa, c_input.view());
    defer c.deinit(gpa);
    try std.testing.expect(!std.mem.eql(u8, &a.key, &b.key));
    try std.testing.expect(!std.mem.eql(u8, &a.key, &c.key));
}

fn capturedInput(allocator: Allocator, binder: ?u32) Allocator.Error!Lifted.Program {
    var input = emptyInput(allocator);
    errdefer input.deinit();
    const ty = try input.types.internPrimitive(&input.names, .str);
    const local = try input.addLocalWithBinder(@fromBackingInt(1), ty, if (binder) |id| @fromBackingInt(id) else null);
    var local_data = input.getLocal(local);
    local_data.capture_kind = if (binder) |id| .{ .recursive_binding = .{ .module = .{ .bytes = @splat(@intCast(id)) }, .root = @fromBackingInt(id) } } else .lexical;
    input.locals.set(@backingInt(local), local_data);
    const captures = try input.addTypedLocalSpan(&.{.{ .local = local, .ty = ty }});
    const body = try input.addExpr(.{ .ty = ty, .data = .{ .local = local } });
    _ = try input.addFn(.{ .symbol = @fromBackingInt(2), .args = .empty(), .captures = captures, .body = .{ .roc = body }, .ret = ty });
    return input;
}

test "lambda analysis restores current checked capture provenance" {
    const gpa = std.testing.allocator;
    var first = try @import("solve.zig").run(gpa, try capturedInput(gpa, 3));
    defer first.deinit();
    const old_id = try identity(gpa, first.lifted.view());
    defer old_id.deinit(gpa);
    const bytes = (try encode(gpa, &first, &old_id)).?;
    defer gpa.free(bytes);
    for ([_]?u32{ 9, null }) |binder| {
        var current = try capturedInput(gpa, binder);
        var owned = true;
        defer if (owned) current.deinit();
        const current_id = try identity(gpa, current.view());
        defer current_id.deinit(gpa);
        try std.testing.expectEqual(old_id.key, current_id.key);
        current.types.freeze();
        var result = (try restore(gpa, &current, &current_id, bytes)).?;
        owned = false;
        defer result.deinit();
        try std.testing.expect(result.types.captures.items.len != 0);
        for (result.types.captures.items) |capture| {
            const local = result.lifted.getLocal(capture.local);
            try std.testing.expectEqual(local.identity.binder(), capture.binder);
            try std.testing.expectEqual(local.identity.captureId(), capture.capture_id);
            try std.testing.expectEqual(local.identity.checkedCaptureId(), capture.checked_capture_id);
            try std.testing.expectEqual(local.capture_kind, capture.capture_kind);
        }
    }
}

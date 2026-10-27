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

/// Versioned identity of the complete solver input, including table order.
/// No source-position identity or inference from a prior result is involved.
pub fn identity(input: Lifted.ProgramView) [32]u8 {
    var out = Fingerprint{ .names = input.names };
    out.bytes("roc.lambda-analysis-input.v1");
    inline for (columns) |field| {
        out.value(@TypeOf(@field(input, field)), @field(input, field));
    }
    inline for (type_columns) |field| {
        out.value(@TypeOf(@field(input.types, field)), @field(input.types, field));
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
    return out.finish();
}

const Fingerprint = struct {
    names: *const names.NameStore,
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
    fn value(self: *Fingerprint, comptime T: type, v: T) void {
        // Keep both the table id and its meaning: output annotations retain
        // numeric references to this input, and name equality reads the text.
        if (T == names.ModuleIdentityId) {
            self.value(u32, @backingInt(v));
            return self.bytes(self.names.moduleIdentityBytes(v));
        }
        if (T == names.RecordFieldNameId or T == names.TagNameId or T == names.TypeNameId or T == names.ExportNameId or T == names.ExternalSymbolNameId) {
            self.value(u32, @backingInt(v));
            const text = if (T == names.RecordFieldNameId) self.names.recordFieldLabelText(v) else if (T == names.TagNameId) self.names.tagLabelText(v) else if (T == names.TypeNameId) self.names.typeNameText(v) else if (T == names.ExportNameId) self.names.exportNameText(v) else self.names.externalSymbolNameText(v);
            return self.value([]const u8, text);
        }
        switch (@typeInfo(T)) {
            .@"struct" => |info| inline for (info.field_names, info.field_types) |field, F| self.value(F, @field(v, field)),
            .@"union" => |info| {
                self.value(info.tag_type.?, std.meta.activeTag(v));
                inline for (info.field_names, info.field_types) |field, F| if (std.meta.activeTag(v) == @field(info.tag_type.?, field)) return self.value(F, @field(v, field));
                unreachable;
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
            else => @compileError("unsupported analysis identity field: " ++ @typeName(T)),
        }
    }
};

/// Solver annotations only. The executable IR always comes from this build.
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

const Context = struct {
    allocator: Allocator,
    input: Lifted.ProgramView,
    key: [32]u8,
    named: std.AutoHashMapUnmanaged(Mono.NamedType, void) = .empty,
    modules: std.AutoHashMapUnmanaged([32]u8, void) = .empty,
    defaults: std.AutoHashMapUnmanaged([36]u8, void) = .empty,

    fn init(allocator: Allocator, input: Lifted.ProgramView, key: [32]u8) Allocator.Error!Context {
        var self = Context{ .allocator = allocator, .input = input, .key = key };
        errdefer self.deinit();
        for (input.types.named_nodes) |named| {
            try self.named.put(allocator, named.named_type, {});
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
    pub fn defaultArtifact(self: *const Context, module: [32]u8, expr: u32) ?[32]u8 {
        // Analysis only carries a current input default reference; it never
        // persists its executable body. Its validation token is the complete
        // input identity, rather than a newly reconstructed checked artifact.
        return if (self.defaults.contains(defaultKey(module, expr))) self.key else null;
    }
    pub fn validModuleIdentity(self: *const Context, module: [32]u8) bool {
        return self.modules.contains(module);
    }
    pub fn describeNamedType(_: *const Context, named: Mono.NamedType) Codec.NamedOccurrence {
        return .{ .exact = .{ .owner = named.module.bytes, .occurrence = named.ty_module.bytes, .ty = @backingInt(named.ty) } };
    }
    pub fn resolveNamedType(self: *const Context, stored: Codec.NamedOccurrence) Allocator.Error!?Mono.NamedType {
        if (stored != .exact) return null;
        const named = Mono.NamedType{ .module = .{ .bytes = stored.exact.owner }, .ty_module = .{ .bytes = stored.exact.occurrence }, .ty = @fromBackingInt(stored.exact.ty) };
        return if (self.named.contains(named)) named else null;
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

/// Encode only annotations; no source or executable payload is persisted.
pub fn encode(allocator: Allocator, program: *Ast.Program, key: [32]u8) Allocator.Error!?[]u8 {
    var context = try Context.init(allocator, program.lifted.view(), key);
    defer context.deinit();
    return Codec.encodeFrozen(Payload, "LSAN0001".*, allocator, &program.lifted.types, &program.lifted.names, payloadOf(program, key), &context, null) catch |err| switch (err) {
        error.NotRepresentable => return null,
        error.OutOfMemory => return error.OutOfMemory,
    };
}

/// On success transfers the current lifted program into the restored result.
/// Invalid optional data leaves the current program available for fresh solving.
pub fn restore(allocator: Allocator, lifted: *Lifted.Program, key: [32]u8, bytes: []const u8) Allocator.Error!?Ast.Program {
    const arena = try allocator.create(std.heap.ArenaAllocator);
    arena.* = .init(allocator);
    var owned = true;
    defer if (owned) {
        arena.deinit();
        allocator.destroy(arena);
    };
    var context = try Context.init(allocator, lifted.view(), key);
    defer context.deinit();
    const payload = Codec.decodeFrozen(Payload, "LSAN0001".*, arena.allocator(), &lifted.types, &lifted.names, bytes, &context, null) catch |err| switch (err) {
        error.InvalidSummary => return null,
        error.OutOfMemory => return error.OutOfMemory,
    };
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

fn literalInput(allocator: Allocator, text: []const u8) !Lifted.Program {
    var input = emptyInput(allocator);
    errdefer input.deinit();
    const ty = try input.types.internPrimitive(&input.names, .str);
    const literal = try input.addStringLiteral(text);
    const body = try input.addExpr(.{ .ty = ty, .data = .{ .str_lit = literal } });
    _ = try input.addFn(.{ .symbol = @fromBackingInt(0), .args = .empty(), .captures = .empty(), .body = .{ .roc = body }, .ret = ty });
    return input;
}

test "lambda analysis reuse keeps the current literal and reporting metadata" {
    const gpa = std.testing.allocator;
    var first = try @import("solve.zig").run(gpa, try literalInput(gpa, "before"));
    defer first.deinit();
    const key = identity(first.lifted.view());
    const bytes = (try encode(gpa, &first, key)).?;
    defer gpa.free(bytes);
    var current = try literalInput(gpa, "after with different length");
    var current_owned = true;
    defer if (current_owned) current.deinit();
    current.current_loc = .{ .file = 0, .line = 88, .column = 3 };
    try std.testing.expectEqual(key, identity(current.view()));
    var restored = (try restore(gpa, &current, key, bytes)).?;
    current_owned = false;
    defer restored.deinit();
    try std.testing.expectEqualStrings("after with different length", restored.lifted.view().string_literals[0].text());
    try std.testing.expectEqual(@as(u32, 88), restored.lifted.current_loc.line);
    try std.testing.expectEqualDeep(first.types.view(), restored.types.view());
    try std.testing.expectEqualDeep(first.fn_tys.items, restored.fn_tys.items);
}

test "lambda analysis identity includes callable topology and rejects mismatched records" {
    const gpa = std.testing.allocator;
    var first = try @import("solve.zig").run(gpa, try literalInput(gpa, "before"));
    defer first.deinit();
    const key = identity(first.lifted.view());
    const bytes = (try encode(gpa, &first, key)).?;
    defer gpa.free(bytes);
    var current = try literalInput(gpa, "after");
    defer current.deinit();
    _ = try current.types.internPrimitive(&current.names, .u64);
    const changed = identity(current.view());
    try std.testing.expect(!std.mem.eql(u8, &key, &changed));
    try std.testing.expectEqual(null, try restore(gpa, &current, changed, bytes));
    for (0..bytes.len) |end| try std.testing.expectEqual(null, try restore(gpa, &current, key, bytes[0..end]));
}

fn analysisAllocationRoundTrip(gpa: Allocator) !void {
    var first = try @import("solve.zig").run(gpa, try literalInput(gpa, "before"));
    defer first.deinit();
    const key = identity(first.lifted.view());
    const bytes = (try encode(gpa, &first, key)).?;
    defer gpa.free(bytes);
    var current = try literalInput(gpa, "after");
    var owned = true;
    defer if (owned) current.deinit();
    var restored = (try restore(gpa, &current, key, bytes)).?;
    owned = false;
    defer restored.deinit();
}

test "lambda analysis allocation failures preserve input ownership" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, analysisAllocationRoundTrip, .{});
}

fn recursiveInput(allocator: Allocator) !Lifted.Program {
    var input = emptyInput(allocator);
    errdefer input.deinit();
    const root = try input.types.beginRecursive();
    const callback = try input.types.add(.{ .func = .{ .args = .empty(), .ret = root.ty } });
    input.types.finishRecursive(root, .{ .record = try input.types.addFields(&.{.{ .name = try input.names.internRecordFieldLabel("next"), .ty = callback, .default = null }}) });
    const local = try input.addLocal(@fromBackingInt(0), root.ty);
    const args = try input.addTypedLocalSpan(&.{.{ .local = local, .ty = root.ty }});
    const body = try input.addExpr(.{ .ty = root.ty, .data = .{ .local = local } });
    _ = try input.addFn(.{ .symbol = @fromBackingInt(1), .args = args, .captures = .empty(), .body = .{ .roc = body }, .ret = root.ty });
    return input;
}

test "lambda analysis preserves recursive callable relations and rejects redirect cycles" {
    const gpa = std.testing.allocator;
    var first = try @import("solve.zig").run(gpa, try recursiveInput(gpa));
    defer first.deinit();
    const key = identity(first.lifted.view());
    const bytes = (try encode(gpa, &first, key)).?;
    defer gpa.free(bytes);
    var current = try recursiveInput(gpa);
    var owned = true;
    defer if (owned) current.deinit();
    var restored = (try restore(gpa, &current, key, bytes)).?;
    owned = false;
    defer restored.deinit();
    try std.testing.expectEqualDeep(first.types.view(), restored.types.view());
    try std.testing.expectEqualDeep(first.local_tys.items, restored.local_tys.items);
    var bad = payloadOf(&first, key);
    const vars = try gpa.dupe(Type.StoredNode, bad.types.vars);
    defer gpa.free(vars);
    vars[0] = .{ .link = @fromBackingInt(0) };
    bad.types.vars = vars;
    var context = try Context.init(gpa, first.lifted.view(), key);
    defer context.deinit();
    const corrupt = try Codec.encodeFrozen(Payload, "LSAN0001".*, gpa, &first.lifted.types, &first.lifted.names, bad, &context, null);
    defer gpa.free(corrupt);
    var rejected_input = try recursiveInput(gpa);
    defer rejected_input.deinit();
    try std.testing.expectEqual(null, try restore(gpa, &rejected_input, key, corrupt));
}

test "lambda analysis identity distinguishes callbacks with the same signature" {
    const gpa = std.testing.allocator;
    var input = try literalInput(gpa, "first callback");
    defer input.deinit();
    const first = input.view().fns[0];
    const literal = try input.addStringLiteral("second callback");
    const body = try input.addExpr(.{ .ty = first.ret, .data = .{ .str_lit = literal } });
    const second = try input.addFn(.{ .symbol = @fromBackingInt(1), .args = .empty(), .captures = .empty(), .body = .{ .roc = body }, .ret = first.ret });
    const signature = try input.types.add(.{ .func = .{ .args = .empty(), .ret = first.ret } });
    const closure = try input.addExpr(.{ .ty = signature, .data = .{ .fn_ref = .{ .fn_id = @fromBackingInt(0), .captures = .empty() } } });
    _ = try input.addFn(.{ .symbol = @fromBackingInt(2), .args = .empty(), .captures = .empty(), .body = .{ .roc = closure }, .ret = signature });
    const before = identity(input.view());
    input.setExprData(closure, .{ .fn_ref = .{ .fn_id = second, .captures = .empty() } });
    try std.testing.expect(!std.mem.eql(u8, &before, &identity(input.view())));
}

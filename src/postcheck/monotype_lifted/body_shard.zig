//! Private SpecConstr body storage and ordered row commits.
//!
//! A worker's virtual IDs name a frozen prefix followed by owned suffixes.
//! Committing rows relocates only suffix identities, retaining checked evidence,
//! types, function identities, and capture/binder identities verbatim.

const std = @import("std");
const ast = @import("ast.zig");
const Mono = @import("../monotype/ast.zig");
const Common = @import("../common.zig");

/// Columns a body task may append, including parallel diagnostic metadata.
pub const body_fields = .{
    "exprs",            "pats",              "stmts",        "locals",                "expr_ids",           "pat_ids",
    "typed_locals",     "stmt_ids",          "field_exprs",  "field_access_segments", "fn_def_captures",    "capture_operands",
    "record_destructs", "str_pattern_steps", "branches",     "if_branches",           "string_literals",    "expr_locs",
    "expr_regions",     "stmt_locs",         "stmt_regions", "inline_scopes",         "expr_inline_scopes", "stmt_inline_scopes",
    "local_names",
};

const frozen_fields = .{
    "fns",             "const_fn_evidence",       "const_fn_evidence_frames", "roots",
    "layout_requests", "runtime_schema_requests", "static_data_values",       "comptime_sites",
    "source_files",    "comptime_value_roots",
};

/// Every virtual-ID pool whose frozen boundary a shard must retain.
pub const all_fields = body_fields ++ frozen_fields;

fn fieldIndex(comptime field: []const u8) usize {
    inline for (all_fields, 0..) |name, index| {
        if (comptime std.mem.eql(u8, name, field)) return index;
    }
    @compileError("unknown body shard pool: " ++ field);
}

/// Fixed-size boundary metadata; its size is independent of source program size.
pub const Prefix = struct {
    source: *const ast.Program,
    lengths: [all_fields.len]usize,
    source_fn: ast.FnId,
    patch: ast.Fn,

    pub fn init(source: *const ast.Program, source_fn: ast.FnId) Prefix {
        var result: Prefix = .{
            .source = source,
            .lengths = undefined,
            .source_fn = source_fn,
            .patch = source.getFn(source_fn),
        };
        inline for (all_fields, 0..) |field, index| result.lengths[index] = @field(source, field).len();
        return result;
    }

    pub fn len(self: Prefix, comptime field: []const u8) usize {
        return self.lengths[fieldIndex(field)];
    }
};

const Relocation = struct {
    prefix: Prefix,
    offsets: [all_fields.len]u32,
    symbol_start: u32,
    symbol_offset: u32,
    join_start: u32,
    join_offset: u32,

    fn index(self: *const Relocation, comptime field: []const u8, item: u32) u32 {
        return if (item < self.prefix.len(field)) item else item + self.offsets[fieldIndex(field)];
    }

    fn id(self: *const Relocation, comptime field: []const u8, item: anytype) @TypeOf(item) {
        return @enumFromInt(self.index(field, @intFromEnum(item)));
    }

    fn span(self: *const Relocation, comptime field: []const u8, item: anytype) @TypeOf(item) {
        if (item.len == 0) return .{ .start = 0, .len = 0 };
        if (item.start < self.prefix.len(field)) std.debug.assert(item.len <= self.prefix.len(field) - item.start);
        return .{ .start = self.index(field, item.start), .len = item.len };
    }

    /// Common.Span deliberately erases its element type. Its enclosing IR
    /// field, not its Zig representation, determines the addressed pool.
    fn spanPool(comptime Owner: type, comptime field: []const u8) []const u8 {
        inline for (.{
            .{ ast.ExprData, "list", "expr_ids" },
            .{ ast.ExprData, "tuple", "expr_ids" },
            .{ ast.ExprData, "record", "field_exprs" },
            .{ ast.PatData, "tuple", "pat_ids" },
            .{ ast.PatData, "record", "record_destructs" },
            .{ ast.Fn, "args", "typed_locals" },
            .{ ast.Fn, "captures", "typed_locals" },
            .{ Mono.RecordUpdate, "fields", "field_exprs" },
            .{ Mono.TagExpr, "payloads", "expr_ids" },
            .{ Mono.LambdaExpr, "args", "typed_locals" },
            .{ Mono.CallValue, "args", "expr_ids" },
            .{ Mono.LowLevelCall, "args", "expr_ids" },
            .{ Mono.ContinueExpr, "values", "expr_ids" },
            .{ Mono.LiftedFunctionValue, "captures", "capture_operands" },
            .{ Mono.MonotypeFunctionValue, "captures", "fn_def_captures" },
            .{ Mono.CallProc, "captures", "capture_operands" },
            .{ Mono.CallProc, "args", "expr_ids" },
            .{ Mono.MatchExpr, "branches", "branches" },
            .{ Mono.IfExpr, "branches", "if_branches" },
            .{ Mono.BlockExpr, "statements", "stmt_ids" },
            .{ Mono.LoopExpr, "params", "typed_locals" },
            .{ Mono.LoopExpr, "initial_values", "expr_ids" },
            .{ Mono.JoinPointExpr, "params", "typed_locals" },
            .{ Mono.JoinPointExpr, "retained", "typed_locals" },
            .{ Mono.JumpExpr, "loop_params", "typed_locals" },
            .{ Mono.JumpExpr, "args", "expr_ids" },
            .{ Mono.JumpExpr, "loop_values", "expr_ids" },
            .{ @FieldType(ast.ExprData, "field_access"), "segments", "field_access_segments" },
            .{ @FieldType(ast.PatData, "tag"), "payloads", "pat_ids" },
            .{ Mono.StrPattern, "steps", "str_pattern_steps" },
            .{ Mono.ListPattern, "patterns", "pat_ids" },
            .{ Mono.Branch, "bindings", "stmt_ids" },
        }) |entry| {
            if (Owner == entry[0] and std.mem.eql(u8, field, entry[1])) return entry[2];
        }
        @compileError("unclassified body span: " ++ @typeName(Owner) ++ "." ++ field);
    }

    fn member(self: *const Relocation, comptime Owner: type, comptime field: []const u8, item: anytype) @TypeOf(item) {
        if (@TypeOf(item) == ast.Span(void)) return self.span(comptime spanPool(Owner, field), item);
        return self.value(item);
    }

    /// Only the explicitly listed arena identities move. Distinct Mono.FnId,
    /// LiftedFnId, TypeId, NameIds, CaptureIds and binder IDs never alias these.
    /// Structural traversal makes nested try/join/loop operands obey the same
    /// rule as top-level operands, including future wrappers around those IDs.
    fn value(self: *const Relocation, item: anytype) @TypeOf(item) {
        const T = @TypeOf(item);
        if (T == Mono.FnTemplate) return item;
        if (T == ast.ExprId) return self.id("exprs", item);
        if (T == ast.PatId) return self.id("pats", item);
        if (T == ast.StmtId) return self.id("stmts", item);
        if (T == ast.LocalId) return self.id("locals", item);
        if (T == ast.StringLiteralId) return self.id("string_literals", item);
        if (T == ast.InlineScopeId) return if (item == ast.InlineScopeId.none) item else self.id("inline_scopes", item);
        if (T == Common.Symbol) return @enumFromInt(@intFromEnum(item) + if (@intFromEnum(item) >= self.symbol_start) self.symbol_offset else @as(u32, 0));
        if (T == ast.JoinPointId) return @enumFromInt(@intFromEnum(item) + if (@intFromEnum(item) >= self.join_start) self.join_offset else @as(u32, 0));
        if (T == ast.Span(void)) @compileError("body spans must be relocated through their owning field");
        const info = @typeInfo(T);
        if (comptime std.meta.activeTag(info) == .@"struct") {
            var result = item;
            inline for (std.meta.fields(T)) |field| {
                @field(result, field.name) = self.member(T, field.name, @field(item, field.name));
            }
            return result;
        }
        if (comptime std.meta.activeTag(info) == .@"union") {
            const Tag = info.@"union".tag_type orelse return item;
            const active = std.meta.activeTag(item);
            inline for (info.@"union".fields) |field| {
                if (active == @field(Tag, field.name)) {
                    return @unionInit(T, field.name, self.member(T, field.name, @field(item, field.name)));
                }
            }
            unreachable;
        }
        if (comptime std.meta.activeTag(info) == .optional) {
            return if (item) |payload| self.value(payload) else null;
        }
        if (comptime std.meta.activeTag(info) == .array) {
            var result = item;
            for (&result) |*element| element.* = self.value(element.*);
            return result;
        }
        return item;
    }
};

/// All fallible preparation precedes committing rows. No source row is
/// borrowed here: a previous ordered append may already have reallocated it.
pub fn append(destination: *ast.Program, worker: *const ast.Program, symbol_start: u32, symbol_offset: u32, join_start: u32, join_offset: u32) std.mem.Allocator.Error!void {
    std.debug.assert(destination.body_prefix == null);
    const prefix = worker.body_prefix orelse Common.invariant("expected a private body shard");
    std.debug.assert(prefix.source == destination);
    inline for (frozen_fields) |field| std.debug.assert(@field(worker, field).len() == 0);
    std.debug.assert(worker.proc_debug_names.view().len == 0);
    std.debug.assert(worker.next_lift_capture_id == destination.next_lift_capture_id);
    assertMetadataAligned(worker);
    var relocation: Relocation = .{
        .prefix = prefix,
        .offsets = undefined,
        .symbol_start = symbol_start,
        .symbol_offset = symbol_offset,
        .join_start = join_start,
        .join_offset = join_offset,
    };
    inline for (all_fields, 0..) |field, index| {
        std.debug.assert(@field(destination, field).len() >= prefix.len(field));
        relocation.offsets[index] = @intCast(@field(destination, field).len() - prefix.len(field));
    }
    const patch = relocation.value(prefix.patch);
    const allocator = destination.allocator;
    var literals: std.ArrayList(Mono.StringLiteral) = .empty;
    defer literals.deinit(allocator);
    errdefer for (literals.items) |literal| literal.deinit(allocator);
    try literals.ensureTotalCapacity(allocator, worker.string_literals.len());
    for (worker.string_literals.unsafeRawItemsForView()) |literal| {
        literals.appendAssumeCapacity(try literal.clone(allocator));
    }
    var local_names: std.ArrayList([]const u8) = .empty;
    defer local_names.deinit(allocator);
    errdefer for (local_names.items) |name| {
        if (name.len > 0) allocator.free(name);
    };
    try local_names.ensureTotalCapacity(allocator, worker.local_names.len());
    for (worker.local_names.unsafeRawItemsForView()) |name| {
        local_names.appendAssumeCapacity(if (name.len == 0) "" else try allocator.dupe(u8, name));
    }
    inline for (body_fields) |field| {
        try @field(destination, field).ensureUnusedCapacity(allocator, @field(worker, field).len());
    }

    // From this point onward no allocation, source borrowing, or failure occurs.
    inline for (body_fields) |field| {
        if (comptime !std.mem.eql(u8, field, "string_literals") and !std.mem.eql(u8, field, "local_names")) {
            for (@field(worker, field).unsafeRawItemsForView()) |item| {
                @field(destination, field).appendAssumeCapacity(relocation.value(item));
            }
        }
    }
    for (literals.items) |literal| destination.string_literals.appendAssumeCapacity(literal);
    for (local_names.items) |name| destination.local_names.appendAssumeCapacity(name);
    destination.setFn(prefix.source_fn, patch);
    assertMetadataAligned(destination);
}

fn assertMetadataAligned(program: *const ast.Program) void {
    std.debug.assert(program.exprs.len() == program.expr_locs.len());
    std.debug.assert(program.exprs.len() == program.expr_regions.len());
    std.debug.assert(program.exprs.len() == program.expr_inline_scopes.len());
    std.debug.assert(program.stmts.len() == program.stmt_locs.len());
    std.debug.assert(program.stmts.len() == program.stmt_regions.len());
    std.debug.assert(program.stmts.len() == program.stmt_inline_scopes.len());
    std.debug.assert(program.locals.len() == program.local_names.len());
}

test "body shard relocation distinguishes typed pools and preserves frozen IDs" {
    var relocation: Relocation = .{
        .prefix = .{
            .source = undefined, // Relocation reads only the prefix lengths.
            .lengths = @splat(2),
            .source_fn = undefined, // Function publication is not exercised here.
            .patch = undefined, // Function publication is not exercised here.
        },
        .offsets = @splat(10),
        .symbol_start = 50,
        .symbol_offset = 20,
        .join_start = 80,
        .join_offset = 30,
    };
    relocation.offsets[fieldIndex("pat_ids")] = 40;
    const expr_span: ast.Span(ast.ExprId) = .{ .start = 2, .len = 1 };
    const pat_span: ast.Span(ast.PatId) = .{ .start = 2, .len = 1 };
    try std.testing.expectEqual(@as(u32, 12), relocation.span("expr_ids", expr_span).start);
    try std.testing.expectEqual(@as(u32, 42), relocation.span("pat_ids", pat_span).start);
    try std.testing.expectEqual(@as(ast.ExprId, @enumFromInt(1)), relocation.value(@as(ast.ExprId, @enumFromInt(1))));
    try std.testing.expectEqual(@as(Mono.FnId, @enumFromInt(2)), relocation.value(@as(Mono.FnId, @enumFromInt(2))));
    try std.testing.expectEqual(@as(ast.FnId, @enumFromInt(2)), relocation.value(@as(ast.FnId, @enumFromInt(2))));
    try std.testing.expectEqual(@as(Common.ComptimeValueRootId, @enumFromInt(2)), relocation.value(@as(Common.ComptimeValueRootId, @enumFromInt(2))));
    const expr: ast.Expr = .{ .ty = @enumFromInt(2), .data = .{ .jump = .{
        .target = @enumFromInt(80),
        .args = expr_span,
    } } };
    const relocated = relocation.value(expr);
    try std.testing.expectEqual(expr.ty, relocated.ty);
    try std.testing.expectEqual(@as(ast.JoinPointId, @enumFromInt(110)), relocated.data.jump.target);
    try std.testing.expectEqual(@as(u32, 12), relocated.data.jump.args.start);
    const pat: ast.Pat = .{ .ty = expr.ty, .data = .{ .tuple = pat_span } };
    try std.testing.expectEqual(@as(u32, 42), relocation.value(pat).data.tuple.start);
    const stmt: ast.Stmt = .{ .expr = @enumFromInt(2) };
    try std.testing.expectEqual(@as(ast.ExprId, @enumFromInt(12)), relocation.value(stmt).expr);
}

test "body shard relocation preserves metadata sentinels and donor values" {
    const relocation: Relocation = .{
        .prefix = .{
            .source = undefined, // Relocation reads only the prefix lengths.
            .lengths = @splat(2),
            .source_fn = undefined, // Function publication is not exercised here.
            .patch = undefined, // Function publication is not exercised here.
        },
        .offsets = @splat(10),
        .symbol_start = 50,
        .symbol_offset = 20,
        .join_start = 80,
        .join_offset = 30,
    };
    const original = [_]?ast.ExprId{ @enumFromInt(1), null, @enumFromInt(2), @enumFromInt(9) };
    const references = relocation.value(original);
    try std.testing.expectEqualDeep(
        [_]?ast.ExprId{ @enumFromInt(1), null, @enumFromInt(12), @enumFromInt(19) },
        references,
    );
    try std.testing.expectEqual(@as(ast.ExprId, @enumFromInt(2)), original[2].?);

    var scope: ast.InlineScope = .{
        .source_symbol = @enumFromInt(50),
        .source_loc = .{ .file = 70, .line = 80, .column = 90 },
        .call_site = .{ .file = 100, .line = 110, .column = 120 },
    };
    const source_loc = scope.source_loc;
    const call_site = scope.call_site;
    scope = relocation.value(scope);
    try std.testing.expectEqual(@as(Common.Symbol, @enumFromInt(70)), scope.source_symbol);
    try std.testing.expectEqual(ast.InlineScopeId.none, scope.parent);
    try std.testing.expectEqualDeep(source_loc, scope.source_loc);
    try std.testing.expectEqualDeep(call_site, scope.call_site);

    var empty: ast.Expr = .{ .ty = @enumFromInt(8), .data = .{ .tuple = .{ .start = 99, .len = 0 } } };
    empty = relocation.value(empty);
    try std.testing.expectEqual(@as(@TypeOf(empty.ty), @enumFromInt(8)), empty.ty);
    try std.testing.expectEqualDeep(ast.Span(ast.ExprId).empty(), empty.data.tuple);
}

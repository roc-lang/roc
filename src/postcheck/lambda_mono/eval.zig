//! Debug-only tree-walking evaluator over the materialized Lambda Mono program.
//!
//! This is an oracle executor for a differential harness: it derives runtime
//! behavior from the Lambda Mono tree (`ast.zig` + `type.zig`) alone, sharing
//! only runtime-behavior code (`src/builtins/`) with the production path. It
//! never consults LIR lowering. Numeric, string, list, and box behavior mirrors
//! the LIR interpreter (`src/eval/interpreter.zig`) exactly so that results can
//! be compared byte-for-byte against the direct solved-to-LIR lowering executed
//! by that interpreter.
//!
//! Constructs that this version does not model return `error.Unsupported` with
//! the `unsupported` field set to a static description, so the harness skips
//! those roots rather than comparing an approximation.
//!
//! Stated coverage gaps (each fails loudly, and the harness reports the
//! affected cases per reason): crypto ops (`crypto_*`), hasher ops
//! (`hasher_*`), `structural_hash`, Dec transcendentals
//! (pow/sqrt/trig/log) and Dec floor/ceiling, `str_from_utf8` invalid-UTF-8
//! error details, imported function calls, hosted function bodies, roots
//! with runtime arguments, and recursion beyond the depth cap. Everything
//! else in the Lambda Mono IR — every expression, statement, and pattern
//! form, callable and erased dispatch, capture records, try sequencing,
//! loops, the numeric op and conversion matrix, string and list op
//! families — executes and is compared.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const collections = @import("collections");
const lir_core = @import("lir_core");

const MonoType = @import("../monotype/type.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const LIR = lir_core.LIR;
const GuardedList = collections.GuardedList;
const RocDec = builtins.dec.RocDec;
const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const Primitive = MonoType.Primitive;

/// Why an execution terminated without producing a value.
pub const AbortKind = enum { crash, runtime_error, comptime_exhaustiveness, expect_err };

/// Result of running one root to completion.
pub const RunOutcome = union(enum) {
    value: Value,
    aborted: Abort,
};

/// Abort record: kind plus the message the host would have received.
pub const Abort = struct {
    kind: AbortKind,
    message: []const u8, // arena-owned
};

/// Errors surfaced to the harness.
pub const Error = error{ OutOfMemory, Unsupported };

/// Internal control-flow and error set. `Aborted` carries a stored `Abort`;
/// `Returned`/`Broke`/`Continued` carry values stashed on the evaluator.
const EvalError = error{ OutOfMemory, Unsupported, Aborted, Returned, Broke, Continued, Jumped };

/// A generated-callable value: a runtime variant selector plus optional payload.
pub const CallableValue = struct { variant: Type.FnVariantId, payload: ?*const Value };

/// An erased callable value: a target function plus an optional captured value.
pub const ErasedFnValue = struct { target: Type.FnId, capture: ?*const Value };

/// A tag-union value: discriminant index into the type's tag span plus payloads.
pub const TagValue = struct { discriminant: u32, payloads: []const Value };

/// An immutable Lambda Mono runtime value. Aggregates share arena storage; a
/// shallow copy of a `Value` is legal because values are never mutated in place.
pub const Value = union(enum) {
    unit,
    int: i128,
    float32: f32,
    float64: f64,
    dec: i128,
    bool_: bool,
    str: []const u8,
    list: []const Value,
    tuple: []const Value,
    record: []const Value,
    capture_record: []const Value,
    tag: TagValue,
    callable: CallableValue,
    erased_fn: ErasedFnValue,
    box: *const Value,
    uninitialized,
};

/// Assert the value is a string and return its bytes.
pub fn strBytes(value: Value) []const u8 {
    return value.str;
}

/// Interpret a boolean-typed value as true/false, tolerating the `bool_`, small
/// integer, and two-variant tag encodings a boolean may arrive in.
fn truthy(value: Value) bool {
    return switch (value) {
        .bool_ => |b| b,
        .int => |i| i != 0,
        .tag => |t| t.discriminant != 0,
        else => false,
    };
}

/// Interpret a boolean-typed value as a 0/1 byte for numeric comparison.
fn boolBit(value: Value) u8 {
    return @intFromBool(truthy(value));
}

/// View a value as a tag: a `.tag` directly, or a boolean (which a two-variant
/// tag union `[False, True]` encodes with False = 0, True = 1). Comparisons and
/// structural equality can materialize a boolean as `bool_` while the static
/// type is that tag union, so tag-directed consumers tolerate both encodings.
fn tagView(value: Value) ?TagValue {
    return switch (value) {
        .tag => |t| t,
        .bool_ => |b| .{ .discriminant = @intFromBool(b), .payloads = &.{} },
        else => null,
    };
}

/// Per-call binding scope from `LocalId` to `Value`.
const Frame = std.AutoHashMap(Ast.LocalId, Value);

const recursion_depth_cap = 4000;

/// Tree-walking evaluator over one Lambda Mono program.
pub const Evaluator = struct {
    gpa: std.mem.Allocator,
    program: *const Ast.Program,
    arena: std.heap.ArenaAllocator,

    /// Rendered dbg messages in execution order (arena-owned bytes).
    dbg_events: std.ArrayList([]const u8),
    /// Expect-failure messages in execution order (arena-owned bytes).
    expect_failures: std.ArrayList([]const u8),
    /// Set when `error.Unsupported` is returned: what was not supported.
    unsupported: ?[]const u8,

    /// Stored abort record raised by `error.Aborted`.
    abort_record: ?Abort,
    /// Value carried by `error.Returned`.
    return_value: Value,
    /// Value carried by `error.Broke`.
    break_value: Value,
    /// Values carried by `error.Continued`.
    continue_values: []const Value,
    /// Join-point transfer carried by `error.Jumped`.
    jump_target: Ast.JoinPointId,
    jump_values: []const Value,
    /// Live call depth, capped by `recursion_depth_cap`.
    depth: usize,

    /// Lazily constructed RocOps for builtins that require host callbacks
    /// (float and dec `to_str`). Its env pointer is patched on first use.
    roc_ops: ?RocOps,
    /// Sizes of live allocations handed out through `roc_ops`, for realloc.
    ops_alloc_sizes: std.AutoHashMap(usize, usize),

    pub fn init(gpa: std.mem.Allocator, program: *const Ast.Program) Evaluator {
        return .{
            .gpa = gpa,
            .program = program,
            .arena = std.heap.ArenaAllocator.init(gpa),
            .dbg_events = .empty,
            .expect_failures = .empty,
            .unsupported = null,
            .abort_record = null,
            .return_value = .unit,
            .break_value = .unit,
            .continue_values = &.{},
            // Written by every jump before the error.Jumped unwind that reads it.
            .jump_target = undefined,
            .jump_values = &.{},
            .depth = 0,
            .roc_ops = null,
            .ops_alloc_sizes = std.AutoHashMap(usize, usize).init(gpa),
        };
    }

    pub fn deinit(self: *Evaluator) void {
        self.dbg_events.deinit(self.gpa);
        self.expect_failures.deinit(self.gpa);
        self.ops_alloc_sizes.deinit();
        self.arena.deinit();
    }

    fn alloc(self: *Evaluator) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Execute the fn bound to `program.rootsView()[root_index]` with no
    /// arguments. Roots whose fn takes runtime arguments return
    /// `error.Unsupported`.
    pub fn runRoot(self: *Evaluator, root_index: usize) Error!RunOutcome {
        const roots = self.program.rootsView();
        if (root_index >= roots.len) return self.unsup("root index out of range");
        const root = roots[root_index];
        const fn_ = self.program.getFn(root.fn_id);
        if (self.program.typedLocalSpan(fn_.args).len != 0) {
            return self.unsup("root with runtime arguments");
        }

        const body = switch (fn_.body) {
            .roc => |expr_id| expr_id,
            .hosted => return self.unsup("hosted function body"),
        };

        var frame = Frame.init(self.gpa);
        defer frame.deinit();

        const value = self.evalExpr(&frame, body) catch |err| switch (err) {
            error.Returned => self.return_value,
            error.Aborted => return RunOutcome{ .aborted = self.abort_record.? },
            error.OutOfMemory => return error.OutOfMemory,
            error.Unsupported => return error.Unsupported,
            error.Broke, error.Continued => return self.unsup("break or continue escaped a loop"),
            error.Jumped => return self.unsup("jump escaped its join point"),
        };
        return RunOutcome{ .value = value };
    }

    // error and abort helpers

    fn unsup(self: *Evaluator, comptime message: []const u8) Error {
        self.unsupported = message;
        return error.Unsupported;
    }

    fn unsupported_(self: *Evaluator, comptime message: []const u8) EvalError {
        self.unsupported = message;
        return error.Unsupported;
    }

    fn raiseAbort(self: *Evaluator, kind: AbortKind, message: []const u8) EvalError {
        const owned = self.alloc().dupe(u8, message) catch return error.OutOfMemory;
        self.abort_record = .{ .kind = kind, .message = owned };
        return error.Aborted;
    }

    fn crashAbort(self: *Evaluator, message: []const u8) EvalError {
        return self.raiseAbort(.crash, message);
    }

    fn runtimeErrorAbort(self: *Evaluator) EvalError {
        return self.raiseAbort(.runtime_error, "RuntimeError");
    }

    fn comptimeExhaustAbort(self: *Evaluator) EvalError {
        return self.raiseAbort(.comptime_exhaustiveness, "comptime exhaustiveness failed");
    }

    // type resolution

    /// Resolve `ty` through named backings to its structural content.
    fn structural(self: *Evaluator, ty: Type.TypeId) Type.Content {
        var cur = ty;
        while (true) {
            const content = self.program.types.get(cur);
            switch (content) {
                .named => |named| {
                    if (named.backing) |backing| {
                        cur = backing.ty;
                        continue;
                    }
                    return content;
                },
                else => return content,
            }
        }
    }

    fn exprType(self: *Evaluator, id: Ast.ExprId) Type.TypeId {
        return self.program.getExpr(id).ty;
    }

    fn primitiveOf(self: *Evaluator, ty: Type.TypeId) ?Primitive {
        return switch (self.structural(ty)) {
            .primitive => |p| p,
            else => null,
        };
    }

    // value helpers

    fn boxValue(self: *Evaluator, value: Value) EvalError!Value {
        const cell = self.alloc().create(Value) catch return error.OutOfMemory;
        cell.* = value;
        return .{ .box = cell };
    }

    fn cellOf(self: *Evaluator, value: Value) EvalError!*const Value {
        const cell = self.alloc().create(Value) catch return error.OutOfMemory;
        cell.* = value;
        return cell;
    }

    // expression evaluation

    fn evalExpr(self: *Evaluator, frame: *Frame, id: Ast.ExprId) EvalError!Value {
        const expr = self.program.getExpr(id);
        switch (expr.data) {
            .local => |local_id| {
                return frame.get(local_id) orelse self.unsupported_("unbound local");
            },
            .unit => return .unit,
            .int_lit => |int_value| {
                const prim = self.primitiveOf(expr.ty) orelse return self.unsupported_("int literal without primitive type");
                const raw: i128 = switch (int_value.kind) {
                    .i128 => int_value.toI128(),
                    .u128 => @bitCast(@as(u128, @bitCast(int_value.bytes))),
                };
                return self.canonicalInt(prim, raw);
            },
            .frac_f32_lit => |f| return .{ .float32 = f },
            .frac_f64_lit => |f| return .{ .float64 = f },
            .dec_lit => |d| return .{ .dec = d.num },
            .str_lit => |sid| return .{ .str = self.program.stringLiteralText(sid) },
            .bytes_lit => |sid| {
                const bytes = self.program.stringLiteralText(sid);
                const elems = self.alloc().alloc(Value, bytes.len) catch return error.OutOfMemory;
                for (bytes, 0..) |byte, i| elems[i] = self.canonicalInt(.u8, byte);
                return .{ .list = elems };
            },
            .static_data_candidate => |cand| return self.evalExpr(frame, cand.runtime_expr),
            .list => |span| return .{ .list = try self.evalExprSpan(frame, span) },
            .tuple => |span| return .{ .tuple = try self.evalExprSpan(frame, span) },
            .record => |span| return try self.evalRecord(frame, expr.ty, span),
            .capture_record => |span| return .{ .capture_record = try self.evalExprSpan(frame, span) },
            .tag => |tag| return try self.evalTag(frame, expr.ty, tag.name, tag.payloads),
            .callable => |callable| {
                const payload: ?*const Value = if (callable.payload) |p|
                    try self.cellOf(try self.evalExpr(frame, p))
                else
                    null;
                return .{ .callable = .{ .variant = callable.variant, .payload = payload } };
            },
            .nominal => |backing_expr| {
                const value = try self.evalExpr(frame, backing_expr);
                return switch (self.structural(expr.ty)) {
                    .box => try self.boxValue(value),
                    else => value,
                };
            },
            .packed_erased_fn => |packed_fn| {
                const capture: ?*const Value = if (packed_fn.capture) |c|
                    try self.cellOf(try self.evalExpr(frame, c))
                else
                    null;
                return .{ .erased_fn = .{ .target = packed_fn.target, .capture = capture } };
            },
            .direct_call => |call| return try self.evalDirectCall(frame, call),
            .indirect_erased_call => |call| return try self.evalErasedCall(frame, call),
            .low_level => |low| return try self.evalLowLevel(frame, expr.ty, low.op, low.args),
            .field_access => |access| return try self.evalFieldAccess(frame, access.receiver, access.field),
            .capture_access => |slot| return try self.evalCaptureAccess(frame, slot),
            .tuple_access => |access| {
                const receiver = try self.evalExpr(frame, access.tuple);
                return switch (receiver) {
                    .tuple => |elems| elems[access.elem_index],
                    else => self.unsupported_("tuple access on non-tuple"),
                };
            },
            .structural_eq => |eq| return try self.evalStructuralEq(frame, eq.lhs, eq.rhs, eq.negated),
            .structural_hash => return self.unsupported_("structural hash"),
            .let_ => |let| {
                const value = try self.evalExpr(frame, let.value);
                if (!try self.bindPattern(frame, let.bind, value)) return self.runtimeErrorAbort();
                return self.evalExpr(frame, let.rest);
            },
            .match_ => |match| return try self.evalMatch(frame, match.scrutinee, match.branches, match.comptime_site),
            .if_ => |if_expr| return try self.evalIf(frame, if_expr.branches, if_expr.final_else),
            .block => |block| return try self.evalBlock(frame, block.statements, block.final_expr),
            .loop_ => |loop| return try self.evalLoop(frame, loop.params, loop.initial_values, loop.body),
            .break_ => |maybe_value| {
                self.break_value = if (maybe_value) |v| try self.evalExpr(frame, v) else .unit;
                return error.Broke;
            },
            .continue_ => |cont| {
                self.continue_values = try self.evalExprSpan(frame, cont.values);
                return error.Continued;
            },
            .join_point => |join_point| return try self.evalJoinPoint(frame, join_point),
            .jump => |jump| {
                self.jump_target = jump.target;
                self.jump_values = try self.evalExprSpan(frame, jump.args);
                return error.Jumped;
            },
            .return_ => |value_expr| {
                self.return_value = try self.evalExpr(frame, value_expr);
                return error.Returned;
            },
            .uninitialized => return .uninitialized,
            .uninitialized_payload => return .uninitialized,
            .if_initialized_payload => |switch_| return try self.evalInitializedPayload(frame, switch_),
            .try_sequence => |seq| return try self.evalTrySequence(frame, expr.ty, seq),
            .try_record_sequence => |seq| return try self.evalTryRecordSequence(frame, expr.ty, seq),
            .comptime_branch_taken => |taken| return self.evalExpr(frame, taken.body),
            .comptime_exhaustiveness_failed => return self.comptimeExhaustAbort(),
            .crash => |sid| return self.crashAbort(self.program.stringLiteralText(sid)),
            .dbg => |child| {
                const value = try self.evalExpr(frame, child);
                const bytes = self.alloc().dupe(u8, value.str) catch return error.OutOfMemory;
                self.dbg_events.append(self.gpa, bytes) catch return error.OutOfMemory;
                return .unit;
            },
            .expect => |child| {
                const value = try self.evalExpr(frame, child);
                if (!truthy(value)) {
                    const bytes = self.alloc().dupe(u8, "expect failed") catch return error.OutOfMemory;
                    self.expect_failures.append(self.gpa, bytes) catch return error.OutOfMemory;
                }
                return .unit;
            },
            .expect_err => |expect_err| {
                const msg = try self.evalExpr(frame, expect_err.msg);
                return self.raiseAbort(.expect_err, msg.str);
            },
        }
    }

    fn evalExprSpan(self: *Evaluator, frame: *Frame, span: Ast.Span(Ast.ExprId)) EvalError![]const Value {
        const slice = self.program.exprSpan(span);
        const out = self.alloc().alloc(Value, slice.len) catch return error.OutOfMemory;
        for (0..slice.len) |i| {
            out[i] = try self.evalExpr(frame, GuardedList.at(slice, i));
        }
        return out;
    }

    fn evalRecord(self: *Evaluator, frame: *Frame, ty: Type.TypeId, span: Ast.Span(Ast.FieldExpr)) EvalError!Value {
        const field_types = switch (self.structural(ty)) {
            .record => |fields| fields,
            else => return self.unsupported_("record expression without record type"),
        };
        const type_fields = self.program.types.fieldSpan(field_types);
        const out = self.alloc().alloc(Value, type_fields.len) catch return error.OutOfMemory;
        const field_exprs = self.program.fieldExprSpan(span);
        // Evaluate in expression span order (observable through dbg), then place
        // each value into the record type's field-span storage slot.
        for (0..field_exprs.len) |i| {
            const field_expr = GuardedList.at(field_exprs, i);
            const value = try self.evalExpr(frame, field_expr.value);
            const index = self.recordFieldIndex(type_fields, field_expr.name) orelse
                return self.unsupported_("record field not found in record type");
            out[index] = value;
        }
        return .{ .record = out };
    }

    fn recordFieldIndex(
        self: *Evaluator,
        type_fields: Type.StoreSpanBorrow(Type.Field, "fields"),
        name: Type.names.RecordFieldNameId,
    ) ?usize {
        for (0..type_fields.len) |i| {
            const field = GuardedList.at(type_fields, i);
            if (self.program.names.recordFieldLabelTextEql(field.name, name)) return i;
        }
        return null;
    }

    fn evalTag(
        self: *Evaluator,
        frame: *Frame,
        ty: Type.TypeId,
        name: Type.names.TagNameId,
        payloads: Ast.Span(Ast.ExprId),
    ) EvalError!Value {
        const discriminant = self.tagIndex(ty, name) orelse
            return self.unsupported_("tag not found in tag-union type");
        return .{ .tag = .{
            .discriminant = @intCast(discriminant),
            .payloads = try self.evalExprSpan(frame, payloads),
        } };
    }

    fn tagIndex(self: *Evaluator, ty: Type.TypeId, name: Type.names.TagNameId) ?usize {
        const tags = switch (self.structural(ty)) {
            .tag_union => |span| span,
            else => return null,
        };
        const tag_slice = self.program.types.tagSpan(tags);
        for (0..tag_slice.len) |i| {
            const tag = GuardedList.at(tag_slice, i);
            if (self.program.names.tagLabelTextEql(tag.name, name)) return i;
        }
        return null;
    }

    fn tagIndexByText(self: *Evaluator, ty: Type.TypeId, text: []const u8) ?usize {
        const tags = switch (self.structural(ty)) {
            .tag_union => |span| span,
            else => return null,
        };
        const tag_slice = self.program.types.tagSpan(tags);
        for (0..tag_slice.len) |i| {
            const tag = GuardedList.at(tag_slice, i);
            if (std.mem.eql(u8, self.program.names.tagLabelText(tag.name), text)) return i;
        }
        return null;
    }

    /// A callable value carries a lambda-set variant id, but the same lambda-set
    /// member can be assigned different `FnVariantId`s across variant spans (a
    /// narrowed match re-indexes them). The stable identity is the variant's
    /// source symbol, so callable matching compares that.
    fn sameVariant(self: *Evaluator, a: Type.FnVariantId, b: Type.FnVariantId) bool {
        if (a == b) return true;
        const variants = self.program.types.view().fn_variants;
        return std.meta.eql(variants[@intFromEnum(a)].source, variants[@intFromEnum(b)].source);
    }

    fn evalFieldAccess(self: *Evaluator, frame: *Frame, receiver: Ast.ExprId, field: Type.names.RecordFieldNameId) EvalError!Value {
        const value = try self.evalExpr(frame, receiver);
        const type_fields = switch (self.structural(self.exprType(receiver))) {
            .record => |fields| self.program.types.fieldSpan(fields),
            else => return self.unsupported_("field access on non-record"),
        };
        const index = self.recordFieldIndex(type_fields, field) orelse
            return self.unsupported_("field access field not found");
        return switch (value) {
            .record => |elems| elems[index],
            else => self.unsupported_("field access on non-record value"),
        };
    }

    fn evalCaptureAccess(self: *Evaluator, frame: *Frame, slot: Ast.CaptureSlot) EvalError!Value {
        const value = try self.evalExpr(frame, slot.record);
        const capture_fields = switch (self.structural(self.exprType(slot.record))) {
            .capture_record => |span| self.program.types.captureFieldSpan(span),
            else => return self.unsupported_("capture access on non-capture-record"),
        };
        var index: ?usize = null;
        for (0..capture_fields.len) |i| {
            if (std.meta.eql(GuardedList.at(capture_fields, i).symbol, slot.symbol)) {
                index = i;
                break;
            }
        }
        const resolved = index orelse return self.unsupported_("capture symbol not found");
        return switch (value) {
            .capture_record => |elems| elems[resolved],
            else => self.unsupported_("capture access on non-capture-record value"),
        };
    }

    // calls

    fn evalDirectCall(self: *Evaluator, frame: *Frame, call: Ast.DirectCall) EvalError!Value {
        const fn_id = switch (call.target) {
            .local => |id| id,
            .imported => return self.unsupported_("imported function call"),
        };
        const args = try self.evalExprSpan(frame, call.args);
        return self.callFn(fn_id, args);
    }

    fn evalErasedCall(self: *Evaluator, frame: *Frame, call: Ast.ErasedCall) EvalError!Value {
        const callee = try self.evalExpr(frame, call.callee);
        const erased = switch (callee) {
            .erased_fn => |e| e,
            else => return self.unsupported_("indirect call on non-erased value"),
        };
        const args = try self.evalExprSpan(frame, call.args);

        const target_fn = self.program.getFn(erased.target);
        const target_arity = self.program.typedLocalSpan(target_fn.args).len;
        if (target_arity == args.len + 1) {
            const with_capture = self.alloc().alloc(Value, args.len + 1) catch return error.OutOfMemory;
            @memcpy(with_capture[0..args.len], args);
            with_capture[args.len] = if (erased.capture) |cap| cap.* else .unit;
            return self.callFn(erased.target, with_capture);
        }
        return self.callFn(erased.target, args);
    }

    fn callFn(self: *Evaluator, fn_id: Type.FnId, args: []const Value) EvalError!Value {
        if (self.depth >= recursion_depth_cap) return self.unsupported_("recursion depth cap");
        self.depth += 1;
        defer self.depth -= 1;

        const fn_ = self.program.getFn(fn_id);
        const body = switch (fn_.body) {
            .roc => |expr_id| expr_id,
            .hosted => return self.unsupported_("hosted function body"),
        };

        var frame = Frame.init(self.gpa);
        defer frame.deinit();

        const arg_locals = self.program.typedLocalSpan(fn_.args);
        if (arg_locals.len != args.len) return self.unsupported_("function arity mismatch");
        for (0..arg_locals.len) |i| {
            frame.put(GuardedList.at(arg_locals, i).local, args[i]) catch return error.OutOfMemory;
        }

        return self.evalExpr(&frame, body) catch |err| switch (err) {
            error.Returned => self.return_value,
            else => err,
        };
    }

    // control flow

    fn evalMatch(
        self: *Evaluator,
        frame: *Frame,
        scrutinee_expr: Ast.ExprId,
        branches: Ast.Span(Ast.Branch),
        comptime_site: ?Ast.ComptimeSiteId,
    ) EvalError!Value {
        const scrutinee = try self.evalExpr(frame, scrutinee_expr);
        const branch_slice = self.program.branchSpan(branches);
        for (0..branch_slice.len) |i| {
            const branch = GuardedList.at(branch_slice, i);
            if (!try self.bindPattern(frame, branch.pat, scrutinee)) continue;
            if (branch.guard) |guard_expr| {
                const guard = try self.evalExpr(frame, guard_expr);
                if (!truthy(guard)) continue;
            }
            return self.evalExpr(frame, branch.body);
        }
        if (comptime_site != null) return self.comptimeExhaustAbort();
        return self.runtimeErrorAbort();
    }

    fn evalIf(self: *Evaluator, frame: *Frame, branches: Ast.Span(Ast.IfBranch), final_else: Ast.ExprId) EvalError!Value {
        const branch_slice = self.program.ifBranchSpan(branches);
        for (0..branch_slice.len) |i| {
            const branch = GuardedList.at(branch_slice, i);
            const cond = try self.evalExpr(frame, branch.cond);
            if (truthy(cond)) return self.evalExpr(frame, branch.body);
        }
        return self.evalExpr(frame, final_else);
    }

    fn evalBlock(self: *Evaluator, frame: *Frame, statements: Ast.Span(Ast.StmtId), final_expr: Ast.ExprId) EvalError!Value {
        const stmt_slice = self.program.stmtSpan(statements);
        for (0..stmt_slice.len) |i| {
            try self.evalStmt(frame, GuardedList.at(stmt_slice, i));
        }
        return self.evalExpr(frame, final_expr);
    }

    fn evalStmt(self: *Evaluator, frame: *Frame, id: Ast.StmtId) EvalError!void {
        switch (self.program.getStmt(id)) {
            .uninitialized => |pat| try self.bindUninitialized(frame, pat),
            .let_ => |let| {
                if (let.recursive) {
                    try self.preBindUninitialized(frame, let.pat);
                    const value = try self.evalExpr(frame, let.value);
                    if (!try self.bindPattern(frame, let.pat, value)) return self.runtimeErrorAbort();
                } else {
                    const value = try self.evalExpr(frame, let.value);
                    if (!try self.bindPattern(frame, let.pat, value)) return self.runtimeErrorAbort();
                }
            },
            .expr => |expr_id| _ = try self.evalExpr(frame, expr_id),
            .expect => |expr_id| {
                const value = try self.evalExpr(frame, expr_id);
                if (!truthy(value)) {
                    const bytes = self.alloc().dupe(u8, "expect failed") catch return error.OutOfMemory;
                    self.expect_failures.append(self.gpa, bytes) catch return error.OutOfMemory;
                }
            },
            .dbg => |expr_id| {
                const value = try self.evalExpr(frame, expr_id);
                const bytes = self.alloc().dupe(u8, value.str) catch return error.OutOfMemory;
                self.dbg_events.append(self.gpa, bytes) catch return error.OutOfMemory;
            },
            .return_ => |expr_id| {
                self.return_value = try self.evalExpr(frame, expr_id);
                return error.Returned;
            },
            .crash => |sid| return self.crashAbort(self.program.stringLiteralText(sid)),
        }
    }

    fn evalLoop(
        self: *Evaluator,
        frame: *Frame,
        params: Ast.Span(Ast.TypedLocal),
        initial_values: Ast.Span(Ast.ExprId),
        body: Ast.ExprId,
    ) EvalError!Value {
        const param_slice = self.program.typedLocalSpan(params);
        const init_slice = self.program.exprSpan(initial_values);
        if (param_slice.len != init_slice.len) return self.unsupported_("loop param and initial-value arity mismatch");
        for (0..param_slice.len) |i| {
            const value = try self.evalExpr(frame, GuardedList.at(init_slice, i));
            frame.put(GuardedList.at(param_slice, i).local, value) catch return error.OutOfMemory;
        }

        while (true) {
            const result = self.evalExpr(frame, body) catch |err| switch (err) {
                error.Broke => return self.break_value,
                error.Continued => {
                    const next = self.continue_values;
                    if (next.len != param_slice.len) return self.unsupported_("loop continue arity mismatch");
                    for (0..param_slice.len) |i| {
                        frame.put(GuardedList.at(param_slice, i).local, next[i]) catch return error.OutOfMemory;
                    }
                    continue;
                },
                else => return err,
            };
            return result;
        }
    }

    fn evalJoinPoint(self: *Evaluator, frame: *Frame, join_point: Ast.JoinPointExpr) EvalError!Value {
        const params = self.program.typedLocalSpan(join_point.params);
        var next_expr = join_point.remainder;
        while (true) {
            const result = self.evalExpr(frame, next_expr) catch |err| switch (err) {
                error.Jumped => {
                    if (self.jump_target != join_point.id) return error.Jumped;
                    if (self.jump_values.len != params.len) return self.unsupported_("join-point argument arity mismatch");
                    for (0..params.len) |index| {
                        frame.put(GuardedList.at(params, index).local, self.jump_values[index]) catch return error.OutOfMemory;
                    }
                    next_expr = join_point.body;
                    continue;
                },
                else => return err,
            };
            return result;
        }
    }

    fn evalInitializedPayload(self: *Evaluator, frame: *Frame, switch_: Ast.InitializedPayloadSwitch) EvalError!Value {
        const cond = try self.evalExpr(frame, switch_.cond);
        const cond_bits: u64 = switch (cond) {
            .bool_ => |b| @intFromBool(b),
            .tag => |t| t.discriminant,
            .int => |i| @truncate(@as(u128, @bitCast(i))),
            else => return self.unsupported_("if-initialized-payload condition is not int or bool"),
        };
        if ((cond_bits & switch_.cond_mask) != 0) {
            return self.evalExpr(frame, switch_.initialized);
        }
        return self.evalExpr(frame, switch_.uninitialized);
    }

    fn evalTrySequence(self: *Evaluator, frame: *Frame, result_ty: Type.TypeId, seq: Ast.TrySequence) EvalError!Value {
        const scrutinee = try self.evalExpr(frame, seq.try_expr);
        const tag = switch (scrutinee) {
            .tag => |t| t,
            else => return self.unsupported_("try sequence on non-tag value"),
        };
        const try_ty = self.exprType(seq.try_expr);
        const ok_index = self.tagIndexByText(try_ty, "Ok") orelse return self.unsupported_("try sequence Ok tag not found");
        const err_index = self.tagIndexByText(try_ty, "Err") orelse return self.unsupported_("try sequence Err tag not found");

        if (tag.discriminant == ok_index) {
            if (tag.payloads.len != 1) return self.unsupported_("try sequence Ok payload arity");
            if (!try self.bindLocal(frame, seq.ok_local, tag.payloads[0])) return self.runtimeErrorAbort();
            return self.evalExpr(frame, seq.ok_body);
        }
        if (tag.discriminant == err_index) {
            // Build the Err in the enclosing expression's result type.
            return self.rebuildErr(result_ty, tag.payloads);
        }
        return self.unsupported_("try sequence scrutinee tag neither Ok nor Err");
    }

    fn rebuildErr(self: *Evaluator, result_ty: Type.TypeId, payloads: []const Value) EvalError!Value {
        const err_index = self.tagIndexByText(result_ty, "Err") orelse return self.unsupported_("enclosing Err tag not found");
        return .{ .tag = .{ .discriminant = @intCast(err_index), .payloads = payloads } };
    }

    fn evalTryRecordSequence(self: *Evaluator, frame: *Frame, result_ty: Type.TypeId, seq: Ast.TryRecordSequence) EvalError!Value {
        const scrutinee = try self.evalExpr(frame, seq.try_expr);
        const tag = switch (scrutinee) {
            .tag => |t| t,
            else => return self.unsupported_("try record sequence on non-tag value"),
        };
        const try_ty = self.exprType(seq.try_expr);
        const ok_index = self.tagIndexByText(try_ty, "Ok") orelse return self.unsupported_("try record Ok tag not found");
        const err_index = self.tagIndexByText(try_ty, "Err") orelse return self.unsupported_("try record Err tag not found");

        if (tag.discriminant == ok_index) {
            if (tag.payloads.len != 1) return self.unsupported_("try record Ok payload arity");
            const record = switch (tag.payloads[0]) {
                .record => |elems| elems,
                else => return self.unsupported_("try record Ok payload not a record"),
            };
            const payload_ty = self.okPayloadType(try_ty, ok_index) orelse return self.unsupported_("try record Ok payload type");
            const record_fields = switch (self.structural(payload_ty)) {
                .record => |fields| self.program.types.fieldSpan(fields),
                else => return self.unsupported_("try record Ok payload type not a record"),
            };
            const value_idx = self.recordFieldIndex(record_fields, seq.value_field) orelse return self.unsupported_("try record value field");
            const rest_idx = self.recordFieldIndex(record_fields, seq.rest_field) orelse return self.unsupported_("try record rest field");
            if (!try self.bindLocal(frame, seq.value_local, record[value_idx])) return self.runtimeErrorAbort();
            if (!try self.bindLocal(frame, seq.rest_local, record[rest_idx])) return self.runtimeErrorAbort();
            return self.evalExpr(frame, seq.ok_body);
        }
        if (tag.discriminant == err_index) {
            return self.rebuildErr(result_ty, tag.payloads);
        }
        return self.unsupported_("try record scrutinee tag neither Ok nor Err");
    }

    fn okPayloadType(self: *Evaluator, tag_ty: Type.TypeId, ok_index: usize) ?Type.TypeId {
        const tags = switch (self.structural(tag_ty)) {
            .tag_union => |span| span,
            else => return null,
        };
        const tag_slice = self.program.types.tagSpan(tags);
        const payloads = GuardedList.at(tag_slice, ok_index).payloads;
        if (payloads.len != 1) return null;
        const payload_span = self.program.types.span(payloads);
        return GuardedList.at(payload_span, 0);
    }

    // pattern matching

    fn bindLocal(_: *Evaluator, frame: *Frame, local: Ast.LocalId, value: Value) EvalError!bool {
        frame.put(local, value) catch return error.OutOfMemory;
        return true;
    }

    fn bindPattern(self: *Evaluator, frame: *Frame, pat_id: Ast.PatId, value: Value) EvalError!bool {
        const pat = self.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| return self.bindLocal(frame, local, value),
            .wildcard => return true,
            .as => |as_pat| {
                if (!try self.bindPattern(frame, as_pat.pattern, value)) return false;
                return self.bindLocal(frame, as_pat.local, value);
            },
            .record => |span| return try self.matchRecordPattern(frame, pat.ty, span, value),
            .tuple => |span| {
                const elems = switch (value) {
                    .tuple => |e| e,
                    else => return self.unsupported_("tuple pattern on non-tuple value"),
                };
                const pats = self.program.patSpan(span);
                if (pats.len != elems.len) return false;
                for (0..pats.len) |i| {
                    if (!try self.bindPattern(frame, GuardedList.at(pats, i), elems[i])) return false;
                }
                return true;
            },
            .list => |list_pat| return try self.matchListPattern(frame, list_pat, value),
            .tag => |tag_pat| {
                const tag = tagView(value) orelse return self.unsupported_("tag pattern on non-tag value");
                const index = self.tagIndex(pat.ty, tag_pat.name) orelse return self.unsupported_("tag pattern tag not found");
                if (tag.discriminant != index) return false;
                const pats = self.program.patSpan(tag_pat.payloads);
                if (pats.len != tag.payloads.len) return false;
                for (0..pats.len) |i| {
                    if (!try self.bindPattern(frame, GuardedList.at(pats, i), tag.payloads[i])) return false;
                }
                return true;
            },
            .callable => |callable_pat| {
                const callable = switch (value) {
                    .callable => |c| c,
                    else => return self.unsupported_("callable pattern on non-callable value"),
                };
                if (!self.sameVariant(callable.variant, callable_pat.variant)) return false;
                if (callable_pat.payload) |payload_pat| {
                    const payload = callable.payload orelse return false;
                    return self.bindPattern(frame, payload_pat, payload.*);
                }
                return true;
            },
            .nominal => |inner| {
                const inner_value = switch (self.structural(pat.ty)) {
                    .box => switch (value) {
                        .box => |cell| cell.*,
                        else => return self.unsupported_("nominal box pattern on non-box value"),
                    },
                    else => value,
                };
                return self.bindPattern(frame, inner, inner_value);
            },
            .int_lit => |int_value| {
                const prim = self.primitiveOf(pat.ty) orelse return self.unsupported_("int pattern without primitive type");
                const raw: i128 = switch (int_value.kind) {
                    .i128 => int_value.toI128(),
                    .u128 => @bitCast(@as(u128, @bitCast(int_value.bytes))),
                };
                const expected = self.canonicalInt(prim, raw);
                return switch (expected) {
                    .bool_ => |b| truthy(value) == b,
                    else => value == .int and value.int == expected.int,
                };
            },
            .dec_lit => |d| return value == .dec and value.dec == d.num,
            .frac_f32_lit => |f| return value == .float32 and value.float32 == f,
            .frac_f64_lit => |f| return value == .float64 and value.float64 == f,
            .str_lit => |sid| return value == .str and std.mem.eql(u8, value.str, self.program.stringLiteralText(sid)),
            .str_pattern => |str_pat| return try self.matchStrPattern(frame, str_pat, value),
        }
    }

    fn matchRecordPattern(self: *Evaluator, frame: *Frame, ty: Type.TypeId, span: Ast.Span(Ast.RecordDestruct), value: Value) EvalError!bool {
        const elems = switch (value) {
            .record => |e| e,
            else => return self.unsupported_("record pattern on non-record value"),
        };
        const type_fields = switch (self.structural(ty)) {
            .record => |fields| self.program.types.fieldSpan(fields),
            else => return self.unsupported_("record pattern without record type"),
        };
        const destructs = self.program.recordDestructSpan(span);
        for (0..destructs.len) |i| {
            const destruct = GuardedList.at(destructs, i);
            const index = self.recordFieldIndex(type_fields, destruct.name) orelse return self.unsupported_("record pattern field not found");
            if (!try self.bindPattern(frame, destruct.pattern, elems[index])) return false;
        }
        return true;
    }

    fn matchListPattern(self: *Evaluator, frame: *Frame, list_pat: Ast.ListPattern, value: Value) EvalError!bool {
        const elems = switch (value) {
            .list => |e| e,
            else => return self.unsupported_("list pattern on non-list value"),
        };
        const pats = self.program.patSpan(list_pat.patterns);
        const fixed = pats.len;
        const n = elems.len;

        if (list_pat.rest == null) {
            if (n != fixed) return false;
        } else {
            if (n < fixed) return false;
        }

        for (0..fixed) |i| {
            const rest_index: usize = if (list_pat.rest) |rest| rest.index else fixed;
            const elem = if (i < rest_index) elems[i] else elems[n - (fixed - i)];
            if (!try self.bindPattern(frame, GuardedList.at(pats, i), elem)) return false;
        }

        if (list_pat.rest) |rest| {
            if (rest.pattern) |rest_pat| {
                const slice = elems[rest.index .. n - (fixed - rest.index)];
                if (!try self.bindPattern(frame, rest_pat, .{ .list = slice })) return false;
            }
        }
        return true;
    }

    fn matchStrPattern(self: *Evaluator, frame: *Frame, str_pat: Ast.StrPattern, value: Value) EvalError!bool {
        const source = switch (value) {
            .str => |s| s,
            else => return self.unsupported_("string pattern on non-string value"),
        };
        const prefix = self.program.stringLiteralText(str_pat.prefix);
        if (!LIR.strMatchPrefixMatches(source, prefix)) return false;

        const end: LIR.StrPatternEnd = switch (str_pat.end) {
            .exact => .exact,
            .tail => .tail,
        };

        var cursor: usize = prefix.len;
        const steps = self.program.strPatternStepSpan(str_pat.steps);
        for (0..steps.len) |step_i| {
            const step = GuardedList.at(steps, step_i);
            const delimiter = self.program.stringLiteralText(step.delimiter);
            const is_final_tail_capture = end == .tail and step_i + 1 == steps.len and delimiter.len == 0;
            const result = LIR.strMatchStep(source, cursor, delimiter, is_final_tail_capture) orelse return false;
            cursor = result.next_cursor;
            if (step.capture) |capture_pat| {
                const captured = source[result.capture_start..result.capture_end];
                if (!try self.bindPattern(frame, capture_pat, .{ .str = captured })) return false;
            }
        }
        return LIR.strMatchEndMatches(source.len, cursor, end);
    }

    fn bindUninitialized(self: *Evaluator, frame: *Frame, pat_id: Ast.PatId) EvalError!void {
        try self.walkPatternBinders(frame, pat_id, .uninitialized);
    }

    fn preBindUninitialized(self: *Evaluator, frame: *Frame, pat_id: Ast.PatId) EvalError!void {
        try self.walkPatternBinders(frame, pat_id, .uninitialized);
    }

    /// Bind every `bind`/`as` local reachable in the pattern to `value`.
    fn walkPatternBinders(self: *Evaluator, frame: *Frame, pat_id: Ast.PatId, value: Value) EvalError!void {
        const pat = self.program.getPat(pat_id);
        switch (pat.data) {
            .bind => |local| _ = try self.bindLocal(frame, local, value),
            .wildcard => {},
            .as => |as_pat| {
                try self.walkPatternBinders(frame, as_pat.pattern, value);
                _ = try self.bindLocal(frame, as_pat.local, value);
            },
            .record => |span| {
                const destructs = self.program.recordDestructSpan(span);
                for (0..destructs.len) |i| {
                    try self.walkPatternBinders(frame, GuardedList.at(destructs, i).pattern, value);
                }
            },
            .tuple => |span| {
                const pats = self.program.patSpan(span);
                for (0..pats.len) |i| try self.walkPatternBinders(frame, GuardedList.at(pats, i), value);
            },
            .list => |list_pat| {
                const pats = self.program.patSpan(list_pat.patterns);
                for (0..pats.len) |i| try self.walkPatternBinders(frame, GuardedList.at(pats, i), value);
                if (list_pat.rest) |rest| {
                    if (rest.pattern) |rest_pat| try self.walkPatternBinders(frame, rest_pat, value);
                }
            },
            .tag => |tag_pat| {
                const pats = self.program.patSpan(tag_pat.payloads);
                for (0..pats.len) |i| try self.walkPatternBinders(frame, GuardedList.at(pats, i), value);
            },
            .callable => |callable_pat| {
                if (callable_pat.payload) |payload_pat| try self.walkPatternBinders(frame, payload_pat, value);
            },
            .nominal => |inner| try self.walkPatternBinders(frame, inner, value),
            .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => {},
        }
    }

    // structural equality

    fn evalStructuralEq(self: *Evaluator, frame: *Frame, lhs_expr: Ast.ExprId, rhs_expr: Ast.ExprId, negated: bool) EvalError!Value {
        const lhs = try self.evalExpr(frame, lhs_expr);
        const rhs = try self.evalExpr(frame, rhs_expr);
        const equal = try self.structuralEqual(self.exprType(lhs_expr), lhs, rhs);
        return .{ .bool_ = if (negated) !equal else equal };
    }

    fn structuralEqual(self: *Evaluator, ty: Type.TypeId, lhs: Value, rhs: Value) EvalError!bool {
        switch (self.structural(ty)) {
            .primitive => |prim| return self.primitiveEqual(prim, lhs, rhs),
            .zst => return true,
            .record => |fields| {
                const lhs_elems = lhs.record;
                const rhs_elems = rhs.record;
                const field_slice = self.program.types.fieldSpan(fields);
                for (0..field_slice.len) |i| {
                    if (!try self.structuralEqual(GuardedList.at(field_slice, i).ty, lhs_elems[i], rhs_elems[i])) return false;
                }
                return true;
            },
            .capture_record => |fields| {
                const lhs_elems = lhs.capture_record;
                const rhs_elems = rhs.capture_record;
                const field_slice = self.program.types.captureFieldSpan(fields);
                for (0..field_slice.len) |i| {
                    if (!try self.structuralEqual(GuardedList.at(field_slice, i).ty, lhs_elems[i], rhs_elems[i])) return false;
                }
                return true;
            },
            .tuple => |items| {
                const lhs_elems = lhs.tuple;
                const rhs_elems = rhs.tuple;
                const item_slice = self.program.types.span(items);
                for (0..item_slice.len) |i| {
                    if (!try self.structuralEqual(GuardedList.at(item_slice, i), lhs_elems[i], rhs_elems[i])) return false;
                }
                return true;
            },
            .tag_union => |tags| {
                const lhs_tag = tagView(lhs) orelse return self.unsupported_("structural eq on non-tag value");
                const rhs_tag = tagView(rhs) orelse return self.unsupported_("structural eq on non-tag value");
                if (lhs_tag.discriminant != rhs_tag.discriminant) return false;
                const tag_slice = self.program.types.tagSpan(tags);
                const payload_tys = self.program.types.span(GuardedList.at(tag_slice, lhs_tag.discriminant).payloads);
                if (lhs_tag.payloads.len != rhs_tag.payloads.len) return false;
                for (0..lhs_tag.payloads.len) |i| {
                    if (!try self.structuralEqual(GuardedList.at(payload_tys, i), lhs_tag.payloads[i], rhs_tag.payloads[i])) return false;
                }
                return true;
            },
            .box => |inner| return self.structuralEqual(inner, lhs.box.*, rhs.box.*),
            .list => return self.unsupported_("structural eq on list"),
            .callable => return self.unsupported_("structural eq on callable"),
            .erased_fn => return self.unsupported_("structural eq on erased_fn"),
            .erased_capture_ptr => return self.unsupported_("structural eq on erased_capture_ptr"),
            .named => return self.unsupported_("structural eq on unresolved named type"),
        }
    }

    fn primitiveEqual(_: *Evaluator, prim: Primitive, lhs: Value, rhs: Value) bool {
        return switch (prim) {
            .bool => boolBit(lhs) == boolBit(rhs),
            .str => std.mem.eql(u8, lhs.str, rhs.str),
            .f32 => lhs.float32 == rhs.float32,
            .f64 => lhs.float64 == rhs.float64,
            .dec => lhs.dec == rhs.dec,
            else => lhs.int == rhs.int,
        };
    }

    // integer canonicalization

    /// Re-canonicalize `bits` for `prim`: sign-extend for signed primitives,
    /// zero-extend for unsigned primitives, so raw `i128` equality is exact.
    fn canonicalInt(_: *Evaluator, prim: Primitive, bits: i128) Value {
        return switch (prim) {
            inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| blk: {
                const T = intType(p);
                const U = std.meta.Int(.unsigned, @typeInfo(T).int.bits);
                const narrowed: U = @truncate(@as(u128, @bitCast(bits)));
                const typed: T = @bitCast(narrowed);
                break :blk makeInt(T, typed);
            },
            .bool => .{ .bool_ = bits != 0 },
            else => .{ .int = bits },
        };
    }

    // low level ops

    fn evalLowLevel(
        self: *Evaluator,
        frame: *Frame,
        result_ty: Type.TypeId,
        op: base.LowLevel,
        args_span: Ast.Span(Ast.ExprId),
    ) EvalError!Value {
        const arg_ids = self.program.exprSpan(args_span);
        const args = self.alloc().alloc(Value, arg_ids.len) catch return error.OutOfMemory;
        const arg_types = self.alloc().alloc(Type.TypeId, arg_ids.len) catch return error.OutOfMemory;
        for (0..arg_ids.len) |i| {
            const arg_id = GuardedList.at(arg_ids, i);
            arg_types[i] = self.exprType(arg_id);
            args[i] = try self.evalExpr(frame, arg_id);
        }

        return switch (op) {
            .num_is_eq => self.numCompare(args, arg_types, .eq),
            .num_is_lt => self.numCompare(args, arg_types, .lt),
            .num_is_lte => self.numCompare(args, arg_types, .lte),
            .num_is_gt => self.numCompare(args, arg_types, .gt),
            .num_is_gte => self.numCompare(args, arg_types, .gte),

            .num_plus, .num_plus_checked => self.numArith(args, arg_types, result_ty, .add),
            .num_minus, .num_minus_checked => self.numArith(args, arg_types, result_ty, .sub),
            .num_times, .num_times_checked => self.numArith(args, arg_types, result_ty, .mul),
            .num_div_by, .num_div_by_checked => self.numArith(args, arg_types, result_ty, .div),
            .num_div_trunc_by, .num_div_trunc_by_checked => self.numArith(args, arg_types, result_ty, .div_trunc),
            .num_rem_by, .num_rem_by_checked => self.numArith(args, arg_types, result_ty, .rem),
            .num_mod_by, .num_mod_by_checked => self.numArith(args, arg_types, result_ty, .mod),
            .num_negate, .num_negate_checked => self.numArith(args, arg_types, result_ty, .negate),
            .num_abs, .num_abs_checked => self.numArith(args, arg_types, result_ty, .abs),
            .num_abs_diff => self.numArith(args, arg_types, result_ty, .abs_diff),

            .num_pow => self.numFloatMath2(args, arg_types, .pow),
            .num_sqrt => self.numFloatMath1(args, arg_types, .sqrt),
            .num_sin => self.numFloatMath1(args, arg_types, .sin),
            .num_cos => self.numFloatMath1(args, arg_types, .cos),
            .num_tan => self.numFloatMath1(args, arg_types, .tan),
            .num_asin => self.numFloatMath1(args, arg_types, .asin),
            .num_acos => self.numFloatMath1(args, arg_types, .acos),
            .num_atan => self.numFloatMath1(args, arg_types, .atan),
            .num_log => self.numFloatMath1(args, arg_types, .log),
            .num_round => self.numRoundLike(args, arg_types, .round),
            .num_floor => self.numRoundLike(args, arg_types, .floor),
            .num_ceiling => self.numRoundLike(args, arg_types, .ceiling),

            .num_bitwise_and => self.numBitwise(args, arg_types, .@"and"),
            .num_bitwise_or => self.numBitwise(args, arg_types, .@"or"),
            .num_bitwise_xor => self.numBitwise(args, arg_types, .xor),
            .num_bitwise_not => self.numBitwise(args, arg_types, .not),
            .num_shift_left_by => self.numShift(args, arg_types, .shl),
            .num_shift_right_by => self.numShift(args, arg_types, .shr),
            .num_shift_right_zf_by => self.numShift(args, arg_types, .shr_zf),
            .num_count_one_bits => self.numBitCount(args, arg_types, .count_ones),
            .num_count_leading_zero_bits => self.numBitCount(args, arg_types, .count_leading_zeros),
            .num_count_trailing_zero_bits => self.numBitCount(args, arg_types, .count_trailing_zeros),

            .bool_not => .{ .bool_ = !truthy(args[0]) },

            .f32_to_bits => self.canonicalInt(.u32, builtins.float_bits.normalizeF32NanBits(@bitCast(args[0].float32))),
            .f32_from_bits => .{ .float32 = @bitCast(readInt(u32, args[0])) },
            .f64_to_bits => self.canonicalInt(.u64, @as(i128, builtins.float_bits.normalizeF64NanBits(@bitCast(args[0].float64)))),
            .f64_from_bits => .{ .float64 = @bitCast(readInt(u64, args[0])) },

            .u8_to_str,
            .i8_to_str,
            .u16_to_str,
            .i16_to_str,
            .u32_to_str,
            .i32_to_str,
            .u64_to_str,
            .i64_to_str,
            .u128_to_str,
            .i128_to_str,
            .dec_to_str,
            .f32_to_str,
            .f64_to_str,
            .num_to_str,
            => self.evalToStr(args, arg_types),

            .str_is_eq,
            .str_is_eq_static_small,
            .str_static_small_word_eq,
            .str_static_small_word_caseless_eq,
            .str_concat,
            .str_contains,
            .str_trim,
            .str_trim_start,
            .str_trim_end,
            .str_caseless_ascii_equals,
            .str_with_ascii_lowercased,
            .str_with_ascii_uppercased,
            .str_starts_with,
            .str_ends_with,
            .str_repeat,
            .str_drop_prefix,
            .str_drop_prefix_caseless_ascii,
            .str_drop_suffix,
            .str_split_first,
            .str_count_utf8_bytes,
            .str_with_capacity,
            .str_reserve,
            .str_release_excess_capacity,
            .str_to_utf8,
            .str_from_utf8_lossy,
            .str_from_utf8,
            .str_split_on,
            .str_join_with,
            .str_inspect,
            => self.evalStrOp(op, args, result_ty),

            .list_len,
            .list_get_unsafe,
            .list_append_unsafe,
            .list_concat,
            .list_with_capacity,
            .list_drop_at,
            .list_sublist,
            .list_set,
            .list_replace_unsafe,
            .list_swap,
            .list_prepend,
            .list_first,
            .list_last,
            .list_drop_first,
            .list_drop_last,
            .list_take_first,
            .list_take_last,
            .list_reverse,
            .list_reserve,
            .list_release_excess_capacity,
            .list_split_first,
            .list_split_last,
            .list_map_can_reuse,
            => self.evalListOp(op, args, arg_types, result_ty),

            .box_box => self.boxValue(args[0]),
            .box_unbox => switch (args[0]) {
                .box => |cell| cell.*,
                else => self.unsupported_("box_unbox on non-box value"),
            },
            .box_prepare_update => switch (args[0]) {
                .box => |cell| self.boxValue(cell.*),
                else => self.unsupported_("box_prepare_update on non-box value"),
            },
            .erased_capture_load => switch (args[0]) {
                .box => |cell| cell.*,
                else => args[0],
            },

            .u8_from_str,
            .i8_from_str,
            .u16_from_str,
            .i16_from_str,
            .u32_from_str,
            .i32_from_str,
            .u64_from_str,
            .i64_from_str,
            .u128_from_str,
            .i128_from_str,
            .dec_from_str,
            .f32_from_str,
            .f64_from_str,
            => self.evalFromStr(op, args, result_ty),

            .compare => self.evalCompareOp(args, arg_types, result_ty),
            .dict_pseudo_seed => self.canonicalInt(self.primitiveOf(result_ty) orelse .u64, 0),
            .crash => self.crashAbort(args[0].str),

            inline else => |op_tag| self.evalConversionOrUnsupported(op_tag, args, arg_types, result_ty),
        };
    }

    // numeric comparisons

    const CmpOp = enum { eq, lt, lte, gt, gte };

    fn numCompare(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId, op: CmpOp) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("comparison operand without primitive type");
        const a = args[0];
        const b = args[1];
        const result = switch (prim) {
            .f32 => cmpValues(f32, a.float32, b.float32, op),
            .f64 => cmpValues(f64, a.float64, b.float64, op),
            .dec => cmpValues(i128, a.dec, b.dec, op),
            .bool => cmpValues(u8, boolBit(a), boolBit(b), op),
            .i8, .i16, .i32, .i64, .i128 => cmpValues(i128, a.int, b.int, op),
            .u8, .u16, .u32, .u64, .u128 => cmpValues(u128, @bitCast(a.int), @bitCast(b.int), op),
            .str => return self.unsupported_("numeric comparison on string"),
        };
        return .{ .bool_ = result };
    }

    fn cmpValues(comptime T: type, a: T, b: T, op: CmpOp) bool {
        return switch (op) {
            .eq => a == b,
            .lt => a < b,
            .lte => a <= b,
            .gt => a > b,
            .gte => a >= b,
        };
    }

    fn evalCompareOp(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId, _: Type.TypeId) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("compare operand without primitive type");
        const a = args[0];
        const b = args[1];
        const order: u8 = switch (prim) {
            .f32 => cmpOrder(f32, a.float32, b.float32),
            .f64 => cmpOrder(f64, a.float64, b.float64),
            .dec => cmpOrder(i128, a.dec, b.dec),
            .bool => cmpOrder(u8, boolBit(a), boolBit(b)),
            .i8, .i16, .i32, .i64, .i128 => cmpOrder(i128, a.int, b.int),
            .u8, .u16, .u32, .u64, .u128 => cmpOrder(u128, @bitCast(a.int), @bitCast(b.int)),
            .str => return self.unsupported_("compare on string"),
        };
        // Result ordering tag union sorts as EQ, GT, LT (alphabetical), matching
        // the interpreter's runtime discriminants EQ=0, GT=1, LT=2.
        return .{ .tag = .{ .discriminant = order, .payloads = &.{} } };
    }

    fn cmpOrder(comptime T: type, a: T, b: T) u8 {
        if (a == b) return 0; // EQ
        if (a > b) return 1; // GT
        return 2; // LT
    }

    // numeric arithmetic

    const ArithOp = enum { add, sub, mul, div, div_trunc, rem, mod, negate, abs, abs_diff };

    fn numArith(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId, result_ty: Type.TypeId, op: ArithOp) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("arithmetic operand without primitive type");
        const result_prim = self.primitiveOf(result_ty) orelse prim;
        // Negate and abs are unary; every other arithmetic op is binary.
        const rhs = if (op == .negate or op == .abs) args[0] else args[1];
        return switch (prim) {
            inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| self.intArith(intType(p), p, result_prim, op, args[0], rhs),
            .f32 => self.floatArith(f32, op, args[0], rhs),
            .f64 => self.floatArith(f64, op, args[0], rhs),
            .dec => self.decArith(op, args[0], rhs),
            else => self.unsupported_("arithmetic on non-numeric type"),
        };
    }

    fn intArith(self: *Evaluator, comptime T: type, comptime prim: Primitive, result_prim: Primitive, op: ArithOp, va: Value, vb: Value) EvalError!Value {
        const signed = @typeInfo(T).int.signedness == .signed;
        const a = readAs(T, va);
        const b = readAs(T, vb);
        // The bit pattern is computed in the operand type and then re-canonicalized
        // for the result primitive, which differs in signedness for `abs_diff`
        // (signed operands, unsigned result).
        switch (op) {
            .add => {
                const r = @addWithOverflow(a, b);
                if (r[1] == 1) return self.crashAbort("Integer addition overflowed");
                return self.canonicalInt(result_prim, bitsOf(T, r[0]));
            },
            .sub => {
                const r = @subWithOverflow(a, b);
                if (r[1] == 1) return self.crashAbort("Integer subtraction overflowed");
                return self.canonicalInt(result_prim, bitsOf(T, r[0]));
            },
            .mul => {
                const r = @mulWithOverflow(a, b);
                if (r[1] == 1) return self.crashAbort("Integer multiplication overflowed");
                return self.canonicalInt(result_prim, bitsOf(T, r[0]));
            },
            .negate => {
                if (!signed) return self.canonicalInt(result_prim, bitsOf(T, -%a));
                if (a == std.math.minInt(T)) return self.crashAbort("Integer negation overflowed");
                return self.canonicalInt(result_prim, bitsOf(T, -a));
            },
            .abs => {
                if (!signed) return self.canonicalInt(result_prim, bitsOf(T, a));
                if (a >= 0) return self.canonicalInt(result_prim, bitsOf(T, a));
                if (a == std.math.minInt(T)) return self.crashAbort("Integer absolute value overflowed");
                return self.canonicalInt(result_prim, bitsOf(T, -a));
            },
            .div, .div_trunc => {
                if (b == 0) return self.crashAbort(comptime divByZeroMessage(prim));
                if (signed and a == std.math.minInt(T) and b == -1) return self.crashAbort("Integer division overflowed");
                return self.canonicalInt(result_prim, bitsOf(T, @divTrunc(a, b)));
            },
            .rem => {
                if (b == 0) return self.crashAbort(comptime remByZeroMessage(prim));
                if (signed and a == std.math.minInt(T) and b == -1) return self.canonicalInt(result_prim, 0);
                return self.canonicalInt(result_prim, bitsOf(T, @rem(a, b)));
            },
            .mod => {
                if (b == 0) return self.crashAbort(comptime modByZeroMessage(prim));
                if (signed and a == std.math.minInt(T) and b == -1) return self.canonicalInt(result_prim, 0);
                return self.canonicalInt(result_prim, bitsOf(T, @mod(a, b)));
            },
            .abs_diff => {
                if (signed) return self.canonicalInt(result_prim, bitsOf(T, if (a > b) a -% b else b -% a));
                return self.canonicalInt(result_prim, bitsOf(T, if (a > b) a - b else b - a));
            },
        }
    }

    fn floatArith(_: *Evaluator, comptime F: type, op: ArithOp, va: Value, vb: Value) EvalError!Value {
        const a = if (F == f32) va.float32 else va.float64;
        const b = if (F == f32) vb.float32 else vb.float64;
        const res: F = switch (op) {
            .add => a + b,
            .sub => a - b,
            .mul => a * b,
            .div => a / b,
            .div_trunc => @trunc(a / b),
            .rem, .mod => @rem(a, b),
            .negate => -a,
            .abs => @abs(a),
            .abs_diff => @abs(a - b),
        };
        return if (F == f32) .{ .float32 = res } else .{ .float64 = res };
    }

    fn decArith(self: *Evaluator, op: ArithOp, va: Value, vb: Value) EvalError!Value {
        const a = va.dec;
        const b = vb.dec;
        switch (op) {
            .add => return .{ .dec = a +% b },
            .sub => return .{ .dec = a -% b },
            .mul => {
                const result = RocDec.mulWithOverflow(.{ .num = a }, .{ .num = b });
                if (result.has_overflowed) return self.crashAbort("Decimal multiplication overflowed!");
                return .{ .dec = result.value.num };
            },
            .negate => return .{ .dec = -%a },
            .abs => return .{ .dec = if (a < 0) -%a else a },
            .abs_diff => return .{ .dec = if (a > b) a -% b else b -% a },
            .div => return .{ .dec = try self.decDiv(a, b) },
            .div_trunc => return .{ .dec = decTrunc(try self.decDiv(a, b)) },
            .rem => {
                if (b == 0) return self.crashAbort("Decimal remainder by 0!");
                return .{ .dec = @rem(a, b) };
            },
            .mod => {
                if (b == 0) return self.crashAbort("Decimal modulo by 0!");
                const remainder = @rem(a, b);
                if (remainder == 0) return .{ .dec = 0 };
                if ((remainder > 0) != (b > 0)) return .{ .dec = remainder +% b };
                return .{ .dec = remainder };
            },
        }
    }

    fn decDiv(self: *Evaluator, a: i128, b: i128) EvalError!i128 {
        if (b == 0) return self.crashAbort("Decimal division by 0!");
        if (a == 0) return 0;

        const one = RocDec.one_point_zero_i128;
        const max_i128: u128 = @intCast(std.math.maxInt(i128));
        const is_negative = (a < 0) != (b < 0);
        const numerator = absU128(a);
        if (numerator > max_i128) {
            if (b == one) return a;
            return self.crashAbort("Decimal division overflow in numerator!");
        }

        const denominator = absU128(b);
        if (denominator > max_i128) {
            if (a == one) return b;
            return self.crashAbort("Decimal division overflow in denominator!");
        }

        const scaled: u256 = @as(u256, numerator) * @as(u256, @intCast(one));
        const quotient: u256 = scaled / @as(u256, denominator);
        if (quotient > @as(u256, max_i128)) return self.crashAbort("Decimal division overflow!");

        const magnitude: i128 = @intCast(quotient);
        return if (is_negative) -magnitude else magnitude;
    }

    // transcendental / rounding

    const FloatMath1 = enum { sqrt, sin, cos, tan, asin, acos, atan, log };

    fn numFloatMath1(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId, op: FloatMath1) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("float math operand without primitive type");
        switch (prim) {
            .f32 => return .{ .float32 = floatMath1(f32, args[0].float32, op) },
            .f64 => return .{ .float64 = floatMath1(f64, args[0].float64, op) },
            .dec => return self.unsupported_("dec transcendental op"),
            else => return self.unsupported_("integer transcendental op"),
        }
    }

    fn floatMath1(comptime F: type, x: F, op: FloatMath1) F {
        if (F == f32) {
            return switch (op) {
                .sqrt => @sqrt(x),
                .sin => builtins.float_math_f32.sin(x),
                .cos => builtins.float_math_f32.cos(x),
                .tan => builtins.float_math_f32.tan(x),
                .asin => builtins.float_math_f32.asin(x),
                .acos => builtins.float_math_f32.acos(x),
                .atan => builtins.float_math_f32.atan(x),
                .log => builtins.float_math_f32.log(x),
            };
        }
        return switch (op) {
            .sqrt => @sqrt(x),
            .sin => std.math.sin(x),
            .cos => std.math.cos(x),
            .tan => std.math.tan(x),
            .asin => std.math.asin(x),
            .acos => std.math.acos(x),
            .atan => std.math.atan(x),
            .log => @log(x),
        };
    }

    fn numFloatMath2(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId, _: enum { pow }) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("pow operand without primitive type");
        switch (prim) {
            .f32 => return .{ .float32 = builtins.float_math_f32.pow(args[0].float32, args[1].float32) },
            .f64 => return .{ .float64 = std.math.pow(f64, args[0].float64, args[1].float64) },
            .dec => return self.unsupported_("dec transcendental op"),
            else => return self.unsupported_("integer pow op"),
        }
    }

    const RoundOp = enum { round, floor, ceiling };

    fn numRoundLike(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId, op: RoundOp) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("round operand without primitive type");
        switch (prim) {
            .f32 => return .{ .float32 = switch (op) {
                .round => @round(args[0].float32),
                .floor => @floor(args[0].float32),
                .ceiling => @ceil(args[0].float32),
            } },
            .f64 => return .{ .float64 = switch (op) {
                .round => @round(args[0].float64),
                .floor => @floor(args[0].float64),
                .ceiling => @ceil(args[0].float64),
            } },
            .dec => switch (op) {
                .round => return .{ .dec = decRound(args[0].dec) },
                .floor, .ceiling => return self.unsupported_("dec floor or ceiling op"),
            },
            else => return self.unsupported_("integer round op"),
        }
    }

    // bitwise / shift

    const BitwiseOp = enum { @"and", @"or", xor, not };

    fn numBitwise(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId, op: BitwiseOp) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("bitwise operand without primitive type");
        // Bitwise not is unary; and/or/xor are binary.
        const rhs = if (op == .not) args[0] else args[1];
        return switch (prim) {
            inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| blk: {
                const T = intType(p);
                const a = readAs(T, args[0]);
                const b = readAs(T, rhs);
                const res: T = switch (op) {
                    .@"and" => a & b,
                    .@"or" => a | b,
                    .xor => a ^ b,
                    .not => ~a,
                };
                break :blk makeInt(T, res);
            },
            else => self.unsupported_("bitwise on non-integer type"),
        };
    }

    const ShiftOp = enum { shl, shr, shr_zf };

    fn numShift(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId, op: ShiftOp) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("shift operand without primitive type");
        const amount: u8 = @truncate(@as(u128, @bitCast(args[1].int)));
        return switch (prim) {
            inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| makeInt(intType(p), shiftOp(intType(p), readAs(intType(p), args[0]), amount, op)),
            else => self.unsupported_("shift on non-integer type"),
        };
    }

    fn shiftOp(comptime T: type, av: T, amount: u8, op: ShiftOp) T {
        const Bits = std.math.Log2Int(T);
        const max_bits = @typeInfo(T).int.bits;
        // The shift count is taken modulo the bit width, matching every backend.
        const shift: Bits = @intCast(amount % max_bits);
        return switch (op) {
            .shl => av << shift,
            .shr => av >> shift,
            .shr_zf => blk: {
                const U = std.meta.Int(.unsigned, max_bits);
                break :blk @bitCast(@as(U, @bitCast(av)) >> shift);
            },
        };
    }

    const BitCountOp = enum { count_ones, count_leading_zeros, count_trailing_zeros };

    /// Count one/leading-zero/trailing-zero bits. The result is always a U8,
    /// independent of the operand width. `@clz`/`@ctz` of 0 return the operand's
    /// bit width, matching the spec.
    fn numBitCount(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId, op: BitCountOp) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("bit-count operand without primitive type");
        return switch (prim) {
            inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| blk: {
                const T = intType(p);
                const a = readAs(T, args[0]);
                const count: u8 = switch (op) {
                    .count_ones => @popCount(a),
                    .count_leading_zeros => @clz(a),
                    .count_trailing_zeros => @ctz(a),
                };
                break :blk makeInt(u8, count);
            },
            else => self.unsupported_("bit-count on non-integer type"),
        };
    }

    // to_str

    fn evalToStr(self: *Evaluator, args: []const Value, arg_types: []const Type.TypeId) EvalError!Value {
        const prim = self.primitiveOf(arg_types[0]) orelse return self.unsupported_("to_str operand without primitive type");
        switch (prim) {
            .dec => {
                var rs = builtins.dec.to_str(.{ .num = args[0].dec }, self.getOps());
                return .{ .str = self.alloc().dupe(u8, rs.asSlice()) catch return error.OutOfMemory };
            },
            .f32 => {
                const bits: u64 = @as(u64, @as(u32, @bitCast(args[0].float32)));
                var rs = builtins.str.floatToStrFromBits(bits, true, self.getOps());
                return .{ .str = self.alloc().dupe(u8, rs.asSlice()) catch return error.OutOfMemory };
            },
            .f64 => {
                const bits: u64 = @bitCast(args[0].float64);
                var rs = builtins.str.floatToStrFromBits(bits, false, self.getOps());
                return .{ .str = self.alloc().dupe(u8, rs.asSlice()) catch return error.OutOfMemory };
            },
            inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |p| {
                const T = intType(p);
                const formatted = std.fmt.allocPrint(self.alloc(), "{d}", .{readAs(T, args[0])}) catch return error.OutOfMemory;
                return .{ .str = formatted };
            },
            .bool, .str => return self.unsupported_("to_str on bool or str"),
        }
    }

    // from_str parse ops

    /// Parse a numeric value from a string. The result is a `Result value [..]`
    /// tag union: on success `Ok value`, otherwise the `Err` variant. Parsing is
    /// delegated to the same builtins the LIR interpreter uses so the accepted
    /// syntax (radix prefixes, leading sign, overflow, empty string) matches.
    fn evalFromStr(self: *Evaluator, op: base.LowLevel, args: []const Value, result_ty: Type.TypeId) EvalError!Value {
        const spec = op.numericParseSpec() orelse return self.unsupported_("numeric parse spec missing");
        const source = args[0].str;
        const outcome: ParseOutcome = switch (spec) {
            .int => |int| switch (int.width_bytes) {
                1 => if (int.signed) parseIntResult(i8, source) else parseIntResult(u8, source),
                2 => if (int.signed) parseIntResult(i16, source) else parseIntResult(u16, source),
                4 => if (int.signed) parseIntResult(i32, source) else parseIntResult(u32, source),
                8 => if (int.signed) parseIntResult(i64, source) else parseIntResult(u64, source),
                16 => if (int.signed) parseIntResult(i128, source) else parseIntResult(u128, source),
                else => return self.unsupported_("unexpected integer parse width"),
            },
            .float => |float| switch (float.width_bytes) {
                4 => parseFloatResult(f32, source),
                8 => parseFloatResult(f64, source),
                else => return self.unsupported_("unexpected float parse width"),
            },
            .dec => parseDecResult(source),
        };
        return self.buildResultTag(result_ty, outcome.ok, outcome.payload);
    }

    /// Build a `Result` tag union: `Ok payload` when `ok`, otherwise the `Err`
    /// variant filled with unit placeholders for its payload slots.
    fn buildResultTag(self: *Evaluator, result_ty: Type.TypeId, ok: bool, payload: Value) EvalError!Value {
        const ok_index = self.tagIndexByText(result_ty, "Ok") orelse return self.unsupported_("from_str Ok tag not found");
        const err_index = self.tagIndexByText(result_ty, "Err") orelse return self.unsupported_("from_str Err tag not found");
        if (ok) {
            return .{ .tag = .{ .discriminant = @intCast(ok_index), .payloads = try self.dupeValues(&.{payload}) } };
        }
        return .{ .tag = .{ .discriminant = @intCast(err_index), .payloads = try self.zeroPayloadsForTag(result_ty, err_index) } };
    }

    // string ops

    fn evalStrOp(self: *Evaluator, op: base.LowLevel, args: []const Value, result_ty: Type.TypeId) EvalError!Value {
        const arena = self.alloc();
        switch (op) {
            .str_is_eq => return .{ .bool_ = std.mem.eql(u8, args[0].str, args[1].str) },
            .str_concat => {
                const out = arena.alloc(u8, args[0].str.len + args[1].str.len) catch return error.OutOfMemory;
                @memcpy(out[0..args[0].str.len], args[0].str);
                @memcpy(out[args[0].str.len..], args[1].str);
                return .{ .str = out };
            },
            .str_contains => return .{ .bool_ = std.mem.find(u8, args[0].str, args[1].str) != null },
            .str_starts_with => return .{ .bool_ = std.mem.startsWith(u8, args[0].str, args[1].str) },
            .str_ends_with => return .{ .bool_ = std.mem.endsWith(u8, args[0].str, args[1].str) },
            .str_caseless_ascii_equals => return .{ .bool_ = caselessAsciiEqual(args[0].str, args[1].str) },
            .str_count_utf8_bytes => return self.canonicalInt(.u64, @intCast(args[0].str.len)),
            .str_with_capacity => return .{ .str = "" },
            .str_reserve, .str_release_excess_capacity => return .{ .str = args[0].str },
            .str_repeat => {
                const source = args[0].str;
                const count = readInt(u64, args[1]);
                if (count == 0 or source.len == 0) return .{ .str = "" };
                const out = arena.alloc(u8, source.len * @as(usize, @intCast(count))) catch return error.OutOfMemory;
                var offset: usize = 0;
                var i: u64 = 0;
                while (i < count) : (i += 1) {
                    @memcpy(out[offset .. offset + source.len], source);
                    offset += source.len;
                }
                return .{ .str = out };
            },
            .str_drop_prefix => {
                if (std.mem.startsWith(u8, args[0].str, args[1].str)) return .{ .str = args[0].str[args[1].str.len..] };
                return .{ .str = args[0].str };
            },
            .str_drop_suffix => {
                if (std.mem.endsWith(u8, args[0].str, args[1].str)) return .{ .str = args[0].str[0 .. args[0].str.len - args[1].str.len] };
                return .{ .str = args[0].str };
            },
            .str_with_ascii_lowercased => return .{ .str = try self.mapAscii(args[0].str, std.ascii.toLower) },
            .str_with_ascii_uppercased => return .{ .str = try self.mapAscii(args[0].str, std.ascii.toUpper) },
            .str_trim => return .{ .str = trimUnicode(args[0].str, true, true) },
            .str_trim_start => return .{ .str = trimUnicode(args[0].str, true, false) },
            .str_trim_end => return .{ .str = trimUnicode(args[0].str, false, true) },
            .str_to_utf8 => {
                const source = args[0].str;
                const elems = arena.alloc(Value, source.len) catch return error.OutOfMemory;
                for (source, 0..) |byte, i| elems[i] = self.canonicalInt(.u8, byte);
                return .{ .list = elems };
            },
            .str_split_on => return try self.strSplitOn(args[0].str, args[1].str),
            .str_join_with => return try self.strJoinWith(args[0], args[1].str),
            .str_inspect => return .{ .str = try self.strInspect(args[0].str) },
            .str_drop_prefix_caseless_ascii => return try self.strDropPrefixCaseless(result_ty, args[0].str, args[1].str),
            .str_split_first => return try self.strSplitFirst(result_ty, args[0].str, args[1].str),
            .str_from_utf8 => return try self.strFromUtf8(result_ty, args[0]),
            .str_from_utf8_lossy => {
                const elems = args[0].list;
                const buf = arena.alloc(u8, elems.len) catch return error.OutOfMemory;
                for (elems, 0..) |e, i| buf[i] = @truncate(@as(u128, @bitCast(e.int)));
                const list = builtins.list.RocList{ .bytes = buf.ptr, .length = elems.len, .capacity_or_alloc_ptr = elems.len << 1 };
                var rs = builtins.str.fromUtf8Lossy(list, self.getOps());
                return .{ .str = arena.dupe(u8, rs.asSlice()) catch return error.OutOfMemory };
            },
            .str_is_eq_static_small,
            .str_static_small_word_eq,
            .str_static_small_word_caseless_eq,
            => return self.unsupported_("static small string dispatch op"),
            else => return self.unsupported_("string op"),
        }
    }

    fn mapAscii(self: *Evaluator, source: []const u8, comptime f: fn (u8) u8) EvalError![]const u8 {
        const out = self.alloc().alloc(u8, source.len) catch return error.OutOfMemory;
        for (source, 0..) |c, i| out[i] = f(c);
        return out;
    }

    fn strSplitOn(self: *Evaluator, source: []const u8, delimiter: []const u8) EvalError!Value {
        var pieces: std.ArrayList(Value) = .empty;
        defer pieces.deinit(self.gpa);
        if (delimiter.len == 0) {
            pieces.append(self.gpa, .{ .str = source }) catch return error.OutOfMemory;
        } else {
            var it = std.mem.splitSequence(u8, source, delimiter);
            while (it.next()) |piece| {
                pieces.append(self.gpa, .{ .str = piece }) catch return error.OutOfMemory;
            }
        }
        return .{ .list = self.alloc().dupe(Value, pieces.items) catch return error.OutOfMemory };
    }

    fn strJoinWith(self: *Evaluator, list: Value, separator: []const u8) EvalError!Value {
        const elems = list.list;
        if (elems.len == 0) return .{ .str = "" };
        var total: usize = separator.len * (elems.len - 1);
        for (elems) |e| total += e.str.len;
        const out = self.alloc().alloc(u8, total) catch return error.OutOfMemory;
        var offset: usize = 0;
        for (elems, 0..) |e, i| {
            if (i != 0) {
                @memcpy(out[offset .. offset + separator.len], separator);
                offset += separator.len;
            }
            @memcpy(out[offset .. offset + e.str.len], e.str);
            offset += e.str.len;
        }
        return .{ .str = out };
    }

    fn strInspect(self: *Evaluator, source: []const u8) EvalError![]const u8 {
        var extra: usize = 0;
        for (source) |c| {
            if (c == '\\' or c == '"') extra += 1;
        }
        const out = self.alloc().alloc(u8, source.len + extra + 2) catch return error.OutOfMemory;
        var i: usize = 0;
        out[i] = '"';
        i += 1;
        for (source) |c| {
            if (c == '\\' or c == '"') {
                out[i] = '\\';
                i += 1;
            }
            out[i] = c;
            i += 1;
        }
        out[i] = '"';
        return out;
    }

    fn strDropPrefixCaseless(self: *Evaluator, result_ty: Type.TypeId, source: []const u8, prefix: []const u8) EvalError!Value {
        var after: []const u8 = "";
        var found = false;
        if (prefix.len <= source.len and caselessAsciiEqual(source[0..prefix.len], prefix)) {
            after = source[prefix.len..];
            found = true;
        }
        return self.buildNamedRecord(result_ty, &.{
            .{ .name = "after", .value = .{ .str = after } },
            .{ .name = "found", .value = .{ .bool_ = found } },
        });
    }

    fn strSplitFirst(self: *Evaluator, result_ty: Type.TypeId, source: []const u8, delimiter: []const u8) EvalError!Value {
        var before: []const u8 = "";
        var after: []const u8 = "";
        var found = false;
        if (delimiter.len == 0) {
            found = true;
            after = source;
        } else if (std.mem.find(u8, source, delimiter)) |index| {
            found = true;
            before = source[0..index];
            after = source[index + delimiter.len ..];
        }
        return self.buildNamedRecord(result_ty, &.{
            .{ .name = "after", .value = .{ .str = after } },
            .{ .name = "before", .value = .{ .str = before } },
            .{ .name = "found", .value = .{ .bool_ = found } },
        });
    }

    fn strFromUtf8(self: *Evaluator, result_ty: Type.TypeId, list: Value) EvalError!Value {
        const elems = list.list;
        const bytes = self.alloc().alloc(u8, elems.len) catch return error.OutOfMemory;
        for (elems, 0..) |e, i| bytes[i] = @truncate(@as(u128, @bitCast(e.int)));
        if (!std.unicode.utf8ValidateSlice(bytes)) return self.unsupported_("str_from_utf8 invalid utf8 detail");
        const ok_index = self.tagIndexByText(result_ty, "Ok") orelse return self.unsupported_("str_from_utf8 Ok tag not found");
        return .{ .tag = .{ .discriminant = @intCast(ok_index), .payloads = try self.dupeValues(&.{.{ .str = bytes }}) } };
    }

    const NamedField = struct { name: []const u8, value: Value };

    /// Build a record value placing each `(name, value)` into the storage slot
    /// its name occupies in `result_ty`'s field span.
    fn buildNamedRecord(self: *Evaluator, result_ty: Type.TypeId, fields: []const NamedField) EvalError!Value {
        const type_fields = switch (self.structural(result_ty)) {
            .record => |span| self.program.types.fieldSpan(span),
            else => return self.unsupported_("expected record result type"),
        };
        if (type_fields.len != fields.len) return self.unsupported_("record result field count mismatch");
        const out = self.alloc().alloc(Value, type_fields.len) catch return error.OutOfMemory;
        for (0..type_fields.len) |i| {
            const label = self.program.names.recordFieldLabelText(GuardedList.at(type_fields, i).name);
            var placed = false;
            for (fields) |field| {
                if (std.mem.eql(u8, field.name, label)) {
                    out[i] = field.value;
                    placed = true;
                    break;
                }
            }
            if (!placed) return self.unsupported_("record result field name not found");
        }
        return .{ .record = out };
    }

    // list ops

    fn evalListOp(self: *Evaluator, op: base.LowLevel, args: []const Value, arg_types: []const Type.TypeId, result_ty: Type.TypeId) EvalError!Value {
        const arena = self.alloc();
        switch (op) {
            .list_len => return self.canonicalInt(.u64, @intCast(args[0].list.len)),
            .list_get_unsafe => {
                const index = readInt(u64, args[1]);
                const list = args[0].list;
                if (index >= list.len) return self.unsupported_("list_get_unsafe out of range");
                return list[@intCast(index)];
            },
            .list_append_unsafe => {
                const list = args[0].list;
                const out = arena.alloc(Value, list.len + 1) catch return error.OutOfMemory;
                @memcpy(out[0..list.len], list);
                out[list.len] = args[1];
                return .{ .list = out };
            },
            .list_prepend => {
                const list = args[0].list;
                const out = arena.alloc(Value, list.len + 1) catch return error.OutOfMemory;
                out[0] = args[1];
                @memcpy(out[1..], list);
                return .{ .list = out };
            },
            .list_concat => {
                const a = args[0].list;
                const b = args[1].list;
                const out = arena.alloc(Value, a.len + b.len) catch return error.OutOfMemory;
                @memcpy(out[0..a.len], a);
                @memcpy(out[a.len..], b);
                return .{ .list = out };
            },
            .list_with_capacity => return .{ .list = &.{} },
            .list_reserve, .list_release_excess_capacity => return .{ .list = args[0].list },
            .list_reverse => {
                const list = args[0].list;
                const out = arena.alloc(Value, list.len) catch return error.OutOfMemory;
                for (list, 0..) |elem, i| out[list.len - 1 - i] = elem;
                return .{ .list = out };
            },
            .list_drop_first => return self.sublist(args[0].list, 1, std.math.maxInt(usize)),
            .list_drop_last => {
                const list = args[0].list;
                if (list.len == 0) return .{ .list = list };
                return self.sublist(list, 0, list.len - 1);
            },
            .list_take_first => return self.sublist(args[0].list, 0, @intCast(readInt(u64, args[1]))),
            .list_take_last => {
                const list = args[0].list;
                const take: usize = @intCast(readInt(u64, args[1]));
                const start: usize = if (take >= list.len) 0 else list.len - take;
                return self.sublist(list, start, take);
            },
            .list_drop_at => {
                const list = args[0].list;
                const drop_index = readInt(u64, args[1]);
                if (drop_index >= list.len) return .{ .list = list };
                const out = arena.alloc(Value, list.len - 1) catch return error.OutOfMemory;
                const di: usize = @intCast(drop_index);
                @memcpy(out[0..di], list[0..di]);
                @memcpy(out[di..], list[di + 1 ..]);
                return .{ .list = out };
            },
            .list_sublist => {
                const list = args[0].list;
                const record = args[1].record;
                // The sublist parameter record carries `start` and `len` fields;
                // resolve each by name within the record argument's type.
                const record_fields = switch (self.structural(arg_types[1])) {
                    .record => |span| self.program.types.fieldSpan(span),
                    else => return self.unsupported_("list_sublist parameter is not a record"),
                };
                const start_idx = self.recordFieldIndexByText(record_fields, "start") orelse return self.unsupported_("list_sublist start field");
                const len_idx = self.recordFieldIndexByText(record_fields, "len") orelse return self.unsupported_("list_sublist len field");
                return self.sublist(list, @intCast(readInt(u64, record[start_idx])), @intCast(readInt(u64, record[len_idx])));
            },
            .list_set => {
                const list = args[0].list;
                const index = readInt(u64, args[1]);
                if (index >= list.len) return .{ .list = list };
                const out = arena.dupe(Value, list) catch return error.OutOfMemory;
                out[@intCast(index)] = args[2];
                return .{ .list = out };
            },
            .list_swap => {
                const list = args[0].list;
                const i = readInt(u64, args[1]);
                const j = readInt(u64, args[2]);
                if (i == j or i >= list.len or j >= list.len) return .{ .list = list };
                const out = arena.dupe(Value, list) catch return error.OutOfMemory;
                const tmp = out[@intCast(i)];
                out[@intCast(i)] = out[@intCast(j)];
                out[@intCast(j)] = tmp;
                return .{ .list = out };
            },
            .list_replace_unsafe => return try self.listReplaceUnsafe(result_ty, args),
            .list_first => return try self.listFirstLast(result_ty, args[0].list, true),
            .list_last => return try self.listFirstLast(result_ty, args[0].list, false),
            .list_split_first => return try self.listSplit(result_ty, args[0].list, true),
            .list_split_last => return try self.listSplit(result_ty, args[0].list, false),
            .list_map_can_reuse => {
                // In-place map reuse is disabled on the compared compile.
                const prim = self.primitiveOf(result_ty) orelse .u8;
                return switch (prim) {
                    .bool => .{ .bool_ = false },
                    else => self.canonicalInt(prim, 0),
                };
            },
            else => return self.unsupported_("list op"),
        }
    }

    fn recordFieldIndexByText(
        self: *Evaluator,
        type_fields: Type.StoreSpanBorrow(Type.Field, "fields"),
        text: []const u8,
    ) ?usize {
        for (0..type_fields.len) |i| {
            if (std.mem.eql(u8, self.program.names.recordFieldLabelText(GuardedList.at(type_fields, i).name), text)) return i;
        }
        return null;
    }

    fn sublist(self: *Evaluator, list: []const Value, start: usize, len: usize) EvalError!Value {
        if (list.len == 0 or len == 0 or start >= list.len) return .{ .list = &.{} };
        const keep = @min(len, list.len - start);
        return .{ .list = self.alloc().dupe(Value, list[start .. start + keep]) catch return error.OutOfMemory };
    }

    fn listReplaceUnsafe(self: *Evaluator, result_ty: Type.TypeId, args: []const Value) EvalError!Value {
        const list = args[0].list;
        const index = readInt(u64, args[1]);
        if (index >= list.len) return self.unsupported_("list_replace_unsafe out of range");
        const displaced = list[@intCast(index)];
        const new_list = self.alloc().dupe(Value, list) catch return error.OutOfMemory;
        new_list[@intCast(index)] = args[2];
        return self.buildListElemRecord(result_ty, .{ .list = new_list }, displaced);
    }

    /// Build a two-field record whose list-typed field gets `list_value` and
    /// whose other field gets `elem_value`, identified by field type.
    fn buildListElemRecord(self: *Evaluator, record_ty: Type.TypeId, list_value: Value, elem_value: Value) EvalError!Value {
        const type_fields = switch (self.structural(record_ty)) {
            .record => |span| self.program.types.fieldSpan(span),
            else => return self.unsupported_("expected record result type"),
        };
        if (type_fields.len != 2) return self.unsupported_("expected two-field record result");
        const out = self.alloc().alloc(Value, 2) catch return error.OutOfMemory;
        for (0..2) |i| {
            const field_ty = GuardedList.at(type_fields, i).ty;
            out[i] = switch (self.structural(field_ty)) {
                .list => list_value,
                else => elem_value,
            };
        }
        return .{ .record = out };
    }

    fn listFirstLast(self: *Evaluator, result_ty: Type.TypeId, list: []const Value, first: bool) EvalError!Value {
        const ok_index = self.tagIndexByText(result_ty, "Ok") orelse return self.unsupported_("list first/last Ok tag not found");
        const err_index = self.tagIndexByText(result_ty, "Err") orelse return self.unsupported_("list first/last Err tag not found");
        if (list.len == 0) {
            return .{ .tag = .{ .discriminant = @intCast(err_index), .payloads = try self.zeroPayloadsForTag(result_ty, err_index) } };
        }
        const elem = if (first) list[0] else list[list.len - 1];
        return .{ .tag = .{ .discriminant = @intCast(ok_index), .payloads = try self.dupeValues(&.{elem}) } };
    }

    fn listSplit(self: *Evaluator, result_ty: Type.TypeId, list: []const Value, first: bool) EvalError!Value {
        const ok_index = self.tagIndexByText(result_ty, "Ok") orelse return self.unsupported_("list split Ok tag not found");
        const err_index = self.tagIndexByText(result_ty, "Err") orelse return self.unsupported_("list split Err tag not found");
        if (list.len == 0) {
            return .{ .tag = .{ .discriminant = @intCast(err_index), .payloads = try self.zeroPayloadsForTag(result_ty, err_index) } };
        }
        const payload_ty = self.okPayloadType(result_ty, ok_index) orelse return self.unsupported_("list split Ok payload type");
        const elem = if (first) list[0] else list[list.len - 1];
        const rest = if (first)
            try self.sublist(list, 1, std.math.maxInt(usize))
        else
            try self.sublist(list, 0, list.len - 1);
        const record = try self.buildListElemRecord(payload_ty, rest, elem);
        return .{ .tag = .{ .discriminant = @intCast(ok_index), .payloads = try self.dupeValues(&.{record}) } };
    }

    /// Build the payload values for tag `index` of `tag_ty` as type-directed
    /// zero values. A failing `from_str` or an empty-list `Err` carries a real
    /// payload (typically a zero-arg error tag), not a bare unit, so downstream
    /// pattern matching on that payload succeeds.
    fn zeroPayloadsForTag(self: *Evaluator, tag_ty: Type.TypeId, index: usize) EvalError![]const Value {
        const tags = switch (self.structural(tag_ty)) {
            .tag_union => |span| span,
            else => return &.{},
        };
        const tag_slice = self.program.types.tagSpan(tags);
        const payload_tys = self.program.types.span(GuardedList.at(tag_slice, index).payloads);
        const out = self.alloc().alloc(Value, payload_tys.len) catch return error.OutOfMemory;
        for (0..payload_tys.len) |i| {
            out[i] = try self.zeroValueOfType(GuardedList.at(payload_tys, i));
        }
        return out;
    }

    /// Construct the zero value for `ty`, chasing named backings. Tag
    /// unions take their first (discriminant-0) variant with zeroed payloads.
    fn zeroValueOfType(self: *Evaluator, ty: Type.TypeId) EvalError!Value {
        switch (self.structural(ty)) {
            .primitive => |p| return switch (p) {
                .bool => .{ .bool_ = false },
                .str => .{ .str = "" },
                .f32 => .{ .float32 = 0 },
                .f64 => .{ .float64 = 0 },
                .dec => .{ .dec = 0 },
                else => self.canonicalInt(p, 0),
            },
            .zst, .erased_capture_ptr => return .unit,
            .list => return .{ .list = &.{} },
            .box => |inner| return self.boxValue(try self.zeroValueOfType(inner)),
            .record => |fields| {
                const field_slice = self.program.types.fieldSpan(fields);
                const out = self.alloc().alloc(Value, field_slice.len) catch return error.OutOfMemory;
                for (0..field_slice.len) |i| out[i] = try self.zeroValueOfType(GuardedList.at(field_slice, i).ty);
                return .{ .record = out };
            },
            .capture_record => |fields| {
                const field_slice = self.program.types.captureFieldSpan(fields);
                const out = self.alloc().alloc(Value, field_slice.len) catch return error.OutOfMemory;
                for (0..field_slice.len) |i| out[i] = try self.zeroValueOfType(GuardedList.at(field_slice, i).ty);
                return .{ .capture_record = out };
            },
            .tuple => |items| {
                const item_slice = self.program.types.span(items);
                const out = self.alloc().alloc(Value, item_slice.len) catch return error.OutOfMemory;
                for (0..item_slice.len) |i| out[i] = try self.zeroValueOfType(GuardedList.at(item_slice, i));
                return .{ .tuple = out };
            },
            .tag_union => |tags| {
                const tag_slice = self.program.types.tagSpan(tags);
                if (tag_slice.len == 0) return .unit;
                const payload_tys = self.program.types.span(GuardedList.at(tag_slice, 0).payloads);
                const out = self.alloc().alloc(Value, payload_tys.len) catch return error.OutOfMemory;
                for (0..payload_tys.len) |i| out[i] = try self.zeroValueOfType(GuardedList.at(payload_tys, i));
                return .{ .tag = .{ .discriminant = 0, .payloads = out } };
            },
            .callable, .erased_fn, .named => return .unit,
        }
    }

    fn dupeValues(self: *Evaluator, values: []const Value) EvalError![]const Value {
        return self.alloc().dupe(Value, values) catch return error.OutOfMemory;
    }

    // conversions

    fn evalConversionOrUnsupported(
        self: *Evaluator,
        comptime op: base.LowLevel,
        args: []const Value,
        _: []const Type.TypeId,
        result_ty: Type.TypeId,
    ) EvalError!Value {
        const spec = comptime parseConversion(@tagName(op));
        if (comptime spec) |s| {
            return self.convert(s.src, s.dst, s.kind, args[0], result_ty);
        } else {
            self.unsupported = @tagName(op);
            return error.Unsupported;
        }
    }

    const ConvKind = enum { plain, wrap, try_, trunc, try_unsafe };
    const ConvSpec = struct { src: Primitive, dst: Primitive, kind: ConvKind };

    fn convert(
        self: *Evaluator,
        comptime src: Primitive,
        comptime dst: Primitive,
        comptime kind: ConvKind,
        value: Value,
        result_ty: Type.TypeId,
    ) EvalError!Value {
        return switch (comptime primCategory(src)) {
            .int => switch (comptime primCategory(dst)) {
                .int => switch (kind) {
                    .plain, .wrap => self.canonicalInt(dst, value.int),
                    .try_ => blk: {
                        const fits = intFitsPrim(dst, value.int);
                        break :blk self.buildTryRecord(result_ty, fits, if (fits) self.canonicalInt(dst, value.int) else self.zeroValue(dst));
                    },
                    else => self.unsupported_("unexpected int-to-int conversion kind"),
                },
                .float => floatValueOf(dst, intToFloat(if (dst == .f32) f32 else f64, src, value)),
                .dec => switch (kind) {
                    .plain => .{ .dec = @as(i128, @intCast(intWhole(src, value))) *% RocDec.one_point_zero_i128 },
                    .try_unsafe => blk: {
                        const maybe = intToDecTry(src, value);
                        break :blk self.buildTryRecord(result_ty, maybe != null, .{ .dec = maybe orelse 0 });
                    },
                    else => self.unsupported_("unexpected int-to-dec conversion kind"),
                },
            },
            .float => switch (comptime primCategory(dst)) {
                .int => blk: {
                    const F = if (src == .f32) f32 else f64;
                    const fv = if (src == .f32) value.float32 else value.float64;
                    break :blk switch (kind) {
                        .trunc => self.canonicalInt(dst, floatToIntWrapPrim(F, dst, fv)),
                        .try_unsafe => inner: {
                            const maybe = floatToIntTryPrim(F, dst, fv);
                            break :inner self.buildTryRecord(result_ty, maybe != null, if (maybe) |m| self.canonicalInt(dst, m) else self.zeroValue(dst));
                        },
                        else => self.unsupported_("unexpected float-to-int conversion kind"),
                    };
                },
                .float => blk: {
                    const sv = if (src == .f32) value.float32 else value.float64;
                    break :blk switch (kind) {
                        .plain, .wrap => floatValueOf(dst, sv),
                        .try_unsafe => inner: {
                            const in_range = builtins.numeric_conversions.f64FitsF32(sv);
                            const casted: f64 = if (in_range) @floatCast(sv) else 0;
                            break :inner self.buildTryRecord(result_ty, in_range, floatValueOf(dst, casted));
                        },
                        else => self.unsupported_("unexpected float-to-float conversion kind"),
                    };
                },
                .dec => self.unsupported_("unexpected float-to-dec conversion"),
            },
            .dec => switch (comptime primCategory(dst)) {
                .int => switch (kind) {
                    .trunc => self.decToIntWrap(dst, value.dec),
                    .try_unsafe => blk: {
                        const maybe = decToIntTry(dst, value.dec);
                        break :blk self.buildTryRecord(result_ty, maybe != null, if (maybe) |m| self.canonicalInt(dst, m) else self.zeroValue(dst));
                    },
                    else => self.unsupported_("unexpected dec-to-int conversion kind"),
                },
                .float => switch (kind) {
                    .plain, .wrap => if (dst == .f32)
                        .{ .float32 = builtins.dec.toF32(.{ .num = value.dec }) }
                    else
                        .{ .float64 = RocDec.toF64(.{ .num = value.dec }) },
                    .try_unsafe => blk: {
                        const maybe = builtins.dec.toF32Try(.{ .num = value.dec });
                        break :blk self.buildTryRecord(result_ty, maybe != null, .{ .float32 = maybe orelse 0 });
                    },
                    else => self.unsupported_("unexpected dec-to-float conversion kind"),
                },
                .dec => self.unsupported_("unexpected dec-to-dec conversion"),
            },
        };
    }

    fn decToIntWrap(_: *Evaluator, comptime dst: Primitive, dec_num: i128) EvalError!Value {
        const T = intType(dst);
        return makeInt(T, builtins.dec.toIntWrap(T, .{ .num = dec_num }));
    }

    fn zeroValue(self: *Evaluator, comptime prim: Primitive) Value {
        return self.canonicalInt(prim, 0);
    }

    /// Build the explicitly named unsafe-conversion record, or the public
    /// `Try` tag union used by an integer conversion.
    fn buildTryRecord(self: *Evaluator, result_ty: Type.TypeId, ok: bool, value: Value) EvalError!Value {
        // A checked conversion result is bit-identical whether the front end
        // typed it as a `{ value, is_ok }` record or a `Result` tag union: the
        // `is_ok` flag is the Ok/Err discriminant. Build whichever the type says.
        const type_fields = switch (self.structural(result_ty)) {
            .record => |span| self.program.types.fieldSpan(span),
            .tag_union => return self.buildResultTag(result_ty, ok, value),
            else => return self.unsupported_("expected try-record result type"),
        };
        if (type_fields.len != 2) return self.unsupported_("expected two-field try record");
        const success_index = self.recordFieldIndexByText(type_fields, "success") orelse
            return self.unsupported_("unsafe conversion record omitted success field");
        const value_index = self.recordFieldIndexByText(type_fields, "val_or_memory_garbage") orelse
            return self.unsupported_("unsafe conversion record omitted value field");
        const out = self.alloc().alloc(Value, 2) catch return error.OutOfMemory;
        out[success_index] = self.canonicalInt(.u8, @intFromBool(ok));
        out[value_index] = value;
        return .{ .record = out };
    }

    // host ops for builtins that require RocOps

    fn getOps(self: *Evaluator) *RocOps {
        if (self.roc_ops == null) {
            self.roc_ops = .{
                .env = @ptrCast(self),
                .roc_alloc = rocAllocFn,
                .roc_dealloc = rocDeallocFn,
                .roc_realloc = rocReallocFn,
                .roc_dbg = rocNoopBytesFn,
                .roc_expect_failed = rocNoopBytesFn,
                .roc_crashed = rocNoopBytesFn,
                .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
            };
        }
        return &self.roc_ops.?;
    }

    fn rocAllocFn(ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
        const self: *Evaluator = @ptrCast(@alignCast(ops.env));
        const ptr = allocAligned(self.alloc(), length, alignment) orelse return null;
        self.ops_alloc_sizes.put(@intFromPtr(ptr), length) catch return null;
        return @ptrCast(ptr);
    }

    fn rocDeallocFn(ops: *RocOps, ptr: *anyopaque, _: usize) callconv(.c) void {
        const self: *Evaluator = @ptrCast(@alignCast(ops.env));
        _ = self.ops_alloc_sizes.remove(@intFromPtr(ptr));
    }

    fn rocReallocFn(ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
        const self: *Evaluator = @ptrCast(@alignCast(ops.env));
        const new_ptr = allocAligned(self.alloc(), new_length, alignment) orelse return null;
        const old_size = self.ops_alloc_sizes.get(@intFromPtr(ptr)) orelse 0;
        const copy = @min(old_size, new_length);
        if (copy > 0) {
            const src: [*]const u8 = @ptrCast(ptr);
            @memcpy(new_ptr[0..copy], src[0..copy]);
        }
        _ = self.ops_alloc_sizes.remove(@intFromPtr(ptr));
        self.ops_alloc_sizes.put(@intFromPtr(new_ptr), new_length) catch return null;
        return @ptrCast(new_ptr);
    }

    fn rocNoopBytesFn(_: *RocOps, _: [*]const u8, _: usize) callconv(.c) void {}
};

// free helpers

/// Success flag plus the parsed value for a numeric `from_str` op.
const ParseOutcome = struct { ok: bool, payload: Value };

/// View `source` bytes as a big (non-small) RocStr so the parse builtins read
/// them directly. `length` keeps its high bit clear, marking a non-small str.
fn rocStrOf(source: []const u8) RocStr {
    return .{ .bytes = @constCast(source.ptr), .capacity_or_alloc_ptr = source.len, .length = source.len };
}

fn parseIntResult(comptime T: type, source: []const u8) ParseOutcome {
    const r = builtins.num.parseIntFromStr(T, rocStrOf(source));
    return .{ .ok = r.errorcode == 0, .payload = makeInt(T, r.value) };
}

fn parseFloatResult(comptime F: type, source: []const u8) ParseOutcome {
    const r = builtins.num.parseFloatFromStr(F, rocStrOf(source));
    return .{ .ok = r.errorcode == 0, .payload = if (F == f32) .{ .float32 = r.value } else .{ .float64 = r.value } };
}

fn parseDecResult(source: []const u8) ParseOutcome {
    const r = builtins.dec.fromStr(rocStrOf(source));
    return .{ .ok = r.errorcode == 0, .payload = .{ .dec = r.value } };
}

fn allocAligned(allocator: std.mem.Allocator, len: usize, alignment: usize) ?[*]u8 {
    const effective = if (len == 0) 1 else len;
    return switch (alignment) {
        0, 1 => (allocator.alignedAlloc(u8, .@"1", effective) catch return null).ptr,
        2 => @ptrCast((allocator.alignedAlloc(u8, .@"2", effective) catch return null).ptr),
        4 => @ptrCast((allocator.alignedAlloc(u8, .@"4", effective) catch return null).ptr),
        8 => @ptrCast((allocator.alignedAlloc(u8, .@"8", effective) catch return null).ptr),
        16 => @ptrCast((allocator.alignedAlloc(u8, .@"16", effective) catch return null).ptr),
        else => (allocator.alignedAlloc(u8, .@"16", effective) catch return null).ptr,
    };
}

fn intType(comptime prim: Primitive) type {
    return switch (prim) {
        .u8 => u8,
        .i8 => i8,
        .u16 => u16,
        .i16 => i16,
        .u32 => u32,
        .i32 => i32,
        .u64 => u64,
        .i64 => i64,
        .u128 => u128,
        .i128 => i128,
        else => @compileError("intType requires an integer primitive"),
    };
}

fn absU128(x: i128) u128 {
    return if (x < 0) @bitCast(-%x) else @intCast(x);
}

/// Truncate a Dec fixed-point value toward zero, mirroring `RocDec.trunc`.
fn decTrunc(x: i128) i128 {
    const one: u128 = @intCast(RocDec.one_point_zero_i128);
    const frac_magnitude: i128 = @intCast(absU128(x) % one);
    const fract = if (x < 0) -frac_magnitude else frac_magnitude;
    return x - fract;
}

/// Zero-extend the operand-width bit pattern of `x` into an i128 so a result
/// primitive of a different signedness can re-canonicalize it.
fn bitsOf(comptime T: type, x: T) i128 {
    const U = std.meta.Int(.unsigned, @typeInfo(T).int.bits);
    return @bitCast(@as(u128, @as(U, @bitCast(x))));
}

fn makeInt(comptime T: type, x: T) Value {
    return switch (@typeInfo(T).int.signedness) {
        .signed => .{ .int = @intCast(x) },
        .unsigned => .{ .int = @bitCast(@as(u128, @intCast(x))) },
    };
}

fn readAs(comptime T: type, v: Value) T {
    return switch (@typeInfo(T).int.signedness) {
        .signed => @intCast(v.int),
        .unsigned => @intCast(@as(u128, @bitCast(v.int))),
    };
}

/// Read a value's int payload as a specific unsigned/signed width.
fn readInt(comptime T: type, v: Value) T {
    return readAs(T, v);
}

const PrimCategory = enum { int, float, dec };

fn primCategory(comptime prim: Primitive) PrimCategory {
    return switch (prim) {
        .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => .int,
        .f32, .f64 => .float,
        .dec => .dec,
        else => @compileError("primCategory requires a numeric primitive"),
    };
}

fn floatValueOf(comptime prim: Primitive, x: anytype) Value {
    return switch (prim) {
        .f32 => .{ .float32 = @floatCast(x) },
        .f64 => .{ .float64 = @floatCast(x) },
        else => @compileError("floatValueOf requires a float primitive"),
    };
}

fn intToFloat(comptime F: type, comptime src: Primitive, value: Value) F {
    const T = intType(src);
    return @floatFromInt(readAs(T, value));
}

fn intWhole(comptime src: Primitive, value: Value) i128 {
    const T = intType(src);
    return @intCast(readAs(T, value));
}

fn intToDecTry(comptime src: Primitive, value: Value) ?i128 {
    const T = intType(src);
    const sv = readAs(T, value);
    return switch (@typeInfo(T).int.signedness) {
        .signed => blk: {
            const d = RocDec.fromWholeInt(@intCast(sv)) orelse break :blk null;
            break :blk d.num;
        },
        .unsigned => blk: {
            if (@as(u128, @intCast(sv)) > @as(u128, @intCast(std.math.maxInt(i128)))) break :blk null;
            const d = RocDec.fromWholeInt(@intCast(sv)) orelse break :blk null;
            break :blk d.num;
        },
    };
}

fn intFitsPrim(comptime dst: Primitive, bits: i128) bool {
    const T = intType(dst);
    return bits >= std.math.minInt(T) and bits <= std.math.maxInt(T);
}

fn decToIntTry(comptime dst: Primitive, dec_num: i128) ?i128 {
    const T = intType(dst);
    const v = builtins.dec.toIntTry(T, .{ .num = dec_num }) orelse return null;
    return @intCast(v);
}

fn floatToIntTryPrim(comptime F: type, comptime dst: Primitive, value: F) ?i128 {
    const T = intType(dst);
    const v = builtins.numeric_conversions.floatToIntTry(F, T, value) orelse return null;
    return switch (@typeInfo(T).int.signedness) {
        .signed => @intCast(v),
        .unsigned => @bitCast(@as(u128, @intCast(v))),
    };
}

/// Mirror the interpreter's target-independent wrapping float conversion.
fn floatToIntWrapPrim(comptime F: type, comptime dst: Primitive, value: F) i128 {
    const T = intType(dst);
    const info = @typeInfo(T).int;
    const U = std.meta.Int(.unsigned, info.bits);
    const raw_bits = builtins.numeric_conversions.floatToIntWrapBits(
        F,
        value,
        info.bits,
    );
    const typed: T = @bitCast(@as(U, @truncate(raw_bits)));
    return signedI128(T, typed);
}

fn signedI128(comptime T: type, x: T) i128 {
    return switch (@typeInfo(T).int.signedness) {
        .signed => @intCast(x),
        .unsigned => @bitCast(@as(u128, @intCast(x))),
    };
}

/// Round a Dec fixed-point value half-away-from-zero, matching `RocDec.round`.
fn decRound(num: i128) i128 {
    const one = RocDec.one_point_zero_i128;
    const whole = @divTrunc(num, one);
    const truncated = whole *% one;
    const fract = num - truncated;
    const abs_fract = if (fract < 0) -fract else fract;
    if (abs_fract >= @divTrunc(one, 2)) {
        return truncated + (if (num < 0) -one else one);
    }
    return truncated;
}

fn caselessAsciiEqual(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (x == y) continue;
        if ((x ^ y) == 0x20 and std.ascii.toLower(x) >= 'a' and std.ascii.toLower(x) <= 'z') continue;
        return false;
    }
    return true;
}

fn isUnicodeWhitespace(codepoint: u21) bool {
    return switch (codepoint) {
        0x0009...0x000D => true,
        0x0020 => true,
        0x0085 => true,
        0x00A0 => true,
        0x1680 => true,
        0x2000...0x200A => true,
        0x200E...0x200F => true,
        0x2028 => true,
        0x2029 => true,
        0x202F => true,
        0x205F => true,
        0x3000 => true,
        else => false,
    };
}

/// Trim leading/trailing Unicode whitespace codepoints from `source`.
fn trimUnicode(source: []const u8, trim_start: bool, trim_end: bool) []const u8 {
    var start: usize = 0;
    var end: usize = source.len;

    if (trim_start) {
        var view = std.unicode.Utf8View.initUnchecked(source);
        var it = view.iterator();
        while (it.nextCodepointSlice()) |slice| {
            const cp = std.unicode.utf8Decode(slice) catch break;
            if (!isUnicodeWhitespace(cp)) break;
            start += slice.len;
        }
    }

    if (trim_end) {
        while (end > start) {
            const slice_start = utf8LastCodepointStart(source, end);
            const slice = source[slice_start..end];
            const cp = std.unicode.utf8Decode(slice) catch break;
            if (!isUnicodeWhitespace(cp)) break;
            end = slice_start;
        }
    }

    return source[start..end];
}

fn utf8LastCodepointStart(source: []const u8, end: usize) usize {
    var i = end;
    while (i > 0) {
        i -= 1;
        if ((source[i] & 0xC0) != 0x80) return i;
    }
    return 0;
}

fn divByZeroMessage(comptime prim: Primitive) []const u8 {
    return primTypeName(prim) ++ " division by zero";
}

fn remByZeroMessage(comptime prim: Primitive) []const u8 {
    return primTypeName(prim) ++ " remainder by zero";
}

fn modByZeroMessage(comptime prim: Primitive) []const u8 {
    return primTypeName(prim) ++ " modulo by zero";
}

fn primTypeName(comptime prim: Primitive) []const u8 {
    return switch (prim) {
        .u8 => "U8",
        .i8 => "I8",
        .u16 => "U16",
        .i16 => "I16",
        .u32 => "U32",
        .i32 => "I32",
        .u64 => "U64",
        .i64 => "I64",
        .u128 => "U128",
        .i128 => "I128",
        else => @compileError("primTypeName requires an integer primitive"),
    };
}

/// Parse a conversion op name of the form `<src>_to_<dst>[suffix]` into its
/// source/destination primitives and conversion kind, or null if it is not a
/// numeric conversion op.
fn parseConversion(comptime name: []const u8) ?Evaluator.ConvSpec {
    const marker = "_to_";
    const idx = std.mem.find(u8, name, marker) orelse return null;
    const left: []const u8 = name[0..idx];
    var rest: []const u8 = name[idx + marker.len ..];

    var kind: Evaluator.ConvKind = .plain;
    if (std.mem.endsWith(u8, rest, "_try_unsafe")) {
        kind = .try_unsafe;
        rest = rest[0 .. rest.len - "_try_unsafe".len];
    } else if (std.mem.endsWith(u8, rest, "_try")) {
        kind = .try_;
        rest = rest[0 .. rest.len - "_try".len];
    } else if (std.mem.endsWith(u8, rest, "_wrap")) {
        kind = .wrap;
        rest = rest[0 .. rest.len - "_wrap".len];
    } else if (std.mem.endsWith(u8, rest, "_trunc")) {
        kind = .trunc;
        rest = rest[0 .. rest.len - "_trunc".len];
    }

    const src = std.meta.stringToEnum(Primitive, left) orelse return null;
    const dst = std.meta.stringToEnum(Primitive, rest) orelse return null;
    if (!isNumericPrimitive(src) or !isNumericPrimitive(dst)) return null;
    return .{ .src = src, .dst = dst, .kind = kind };
}

fn isNumericPrimitive(prim: Primitive) bool {
    return switch (prim) {
        .bool, .str => false,
        else => true,
    };
}

test "lambda mono eval declarations are referenced" {
    std.testing.refAllDecls(@This());
}

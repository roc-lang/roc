//! Debug-only exporter of Monotype Lifted IR in a line-oriented text format,
//! for the models that read Lifted programs: `models/rows-evidence` checks
//! their typing, and another model runs them before and after SpecConstr.
//!
//! It runs only when `ROC_LIFTED_IR_EXPORT` names a directory
//! (`base.dev_diagnostics.Var.lifted_ir_export`), after lifting and after
//! SpecConstr, and writes one file per program and stage there:
//! `<n>-<stage>.lifted`. The after-lift export takes `<n>`, the lowest number
//! not yet taken, and the after-SpecConstr export of the same program reuses
//! it, so the two files of one program share their number.
//!
//! The format, and exactly what it leaves out, is specified in the `FORMAT.md`
//! of the model that runs them. In brief: one record per line,
//! fields separated by single spaces, every reference an integer id into the
//! program's own tables, `-` for an absent optional, names percent-escaped.
//! Only the expressions, patterns and statements reachable from a function
//! body are written; types and locals are written in full.
//!
//! Files are created through libc's `open`, which Zig's standard library does
//! not provide on Windows, so a Windows compiler exports nothing.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");

const Mono = @import("../monotype/ast.zig");
const Type = @import("../monotype/type.zig");
const Lifted = @import("ast.zig");
const can = @import("can");

const Allocator = std.mem.Allocator;

/// Version of the line format; bumped whenever a record changes shape.
pub const format_version = 3;

/// The pipeline boundary an export was written at.
pub const Stage = enum {
    after_lift,
    after_spec_constr,
    /// The program Lambda Solved consumed, with the callable variants direct
    /// LIR lowering gave each value call (`LambdaSetExport`).
    lambda_solved,
};

/// The export number to try first; a file another process already wrote
/// under that number is skipped over.
var next_number: std.atomic.Value(u32) = .init(0);

/// Whether this host can export: it needs libc's `open`.
const supported = builtin.os.tag != .windows;

/// Write `program` to the export directory when exporting is switched on.
/// `number` is the number an earlier export of the same program returned, or
/// null to take a fresh one. Returns the number written under, or null when
/// nothing was written.
pub fn exportLifted(allocator: Allocator, program: *const Lifted.Program, stage: Stage, number: ?u32) Allocator.Error!?u32 {
    if (comptime !supported) return null;
    const dir = base.dev_diagnostics.get(.lifted_ir_export) orelse return null;
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);
    try write(allocator, program.view(), stage, "", &out);
    return writeFile(allocator, dir, stage, number, out.items);
}

/// Write `bytes` to `<dir>/<n>-<stage>.lifted`, with `n` as for `exportLifted`.
fn writeFile(allocator: Allocator, dir: []const u8, stage: Stage, number: ?u32, bytes: []const u8) Allocator.Error!?u32 {
    while (true) {
        const claimed = number orelse next_number.fetchAdd(1, .monotonic);
        const path = try std.fmt.allocPrintSentinel(allocator, "{s}/{d}-{s}.lifted", .{
            dir,
            claimed,
            @tagName(stage),
        }, 0);
        defer allocator.free(path);
        // Creating exclusively claims the name, even against another `roc`
        // process writing to the same directory.
        const fd = std.c.open(path, .{ .ACCMODE = .WRONLY, .CREAT = true, .EXCL = true, .CLOEXEC = true }, @as(std.c.mode_t, 0o644));
        if (fd < 0) {
            if (number == null and std.c.errno(fd) == .EXIST) continue;
            std.debug.print("lifted-ir-export: cannot create {s}\n", .{path});
            return null;
        }
        defer _ = std.c.close(fd);
        var written: usize = 0;
        while (written < bytes.len) {
            const result = std.c.write(fd, bytes[written..].ptr, bytes.len - written);
            if (result <= 0) {
                std.debug.print("lifted-ir-export: short write to {s}\n", .{path});
                return null;
            }
            written += @intCast(result);
        }
        return claimed;
    }
}

/// Write the program in the export format. `extra` is a run of complete
/// records written just before the closing `end`.
pub fn write(allocator: Allocator, program: Lifted.ProgramView, stage: Stage, extra: []const u8, out: *std.ArrayList(u8)) Allocator.Error!void {
    var writer: Writer = .{ .allocator = allocator, .source = program, .stage = stage, .extra = extra, .out = out };
    defer writer.deinit();
    try writer.run();
}

const Item = union(enum) {
    expr: Mono.ExprId,
    pat: Mono.PatId,
    stmt: Mono.StmtId,
};

const Writer = struct {
    allocator: Allocator,
    source: Lifted.ProgramView,
    stage: Stage,
    extra: []const u8,
    out: *std.ArrayList(u8),
    stack: std.ArrayList(Item) = .empty,
    expr_seen: std.DynamicBitSetUnmanaged = .{},
    pat_seen: std.DynamicBitSetUnmanaged = .{},
    stmt_seen: std.DynamicBitSetUnmanaged = .{},

    fn deinit(self: *Writer) void {
        self.stack.deinit(self.allocator);
        self.expr_seen.deinit(self.allocator);
        self.pat_seen.deinit(self.allocator);
        self.stmt_seen.deinit(self.allocator);
    }

    fn print(self: *Writer, comptime fmt: []const u8, args: anytype) Allocator.Error!void {
        try self.out.print(self.allocator, fmt, args);
    }

    /// A name, percent-escaped so it is one space-free token.
    fn name(self: *Writer, text: []const u8) Allocator.Error!void {
        try self.out.append(self.allocator, ' ');
        if (text.len == 0) return self.out.appendSlice(self.allocator, "%");
        for (text) |byte| {
            if (byte > ' ' and byte < 0x7f and byte != '%') {
                try self.out.append(self.allocator, byte);
            } else try self.print("%{X:0>2}", .{byte});
        }
    }

    fn id(self: *Writer, value: anytype) Allocator.Error!void {
        try self.print(" {d}", .{@backingInt(value)});
    }

    fn optId(self: *Writer, value: anytype) Allocator.Error!void {
        if (value) |present| try self.id(present) else try self.out.appendSlice(self.allocator, " -");
    }

    fn typeSpan(self: *Writer, items: []const Type.TypeId) Allocator.Error!void {
        try self.print(" {d}", .{items.len});
        for (items) |item| try self.id(item);
    }

    fn exprs(self: *Writer, items: []const Mono.ExprId) Allocator.Error!void {
        try self.print(" {d}", .{items.len});
        for (items) |item| {
            try self.id(item);
            try self.stack.append(self.allocator, .{ .expr = item });
        }
    }

    fn typedLocals(self: *Writer, items: []const Mono.TypedLocal) Allocator.Error!void {
        try self.print(" {d}", .{items.len});
        for (items) |item| {
            try self.id(item.local);
            try self.id(item.ty);
        }
    }

    fn captures(self: *Writer, items: []const Mono.CaptureOperand) Allocator.Error!void {
        try self.print(" {d}", .{items.len});
        for (items) |item| {
            try self.id(item.id);
            try self.id(item.value);
            try self.stack.append(self.allocator, .{ .expr = item.value });
        }
    }

    fn fields(self: *Writer, items: []const Mono.FieldExpr) Allocator.Error!void {
        try self.print(" {d}", .{items.len});
        for (items) |item| {
            try self.name(self.source.names.recordFieldLabelText(item.name));
            try self.id(item.value);
            try self.stack.append(self.allocator, .{ .expr = item.value });
        }
    }

    fn child(self: *Writer, item: Mono.ExprId) Allocator.Error!void {
        try self.id(item);
        try self.stack.append(self.allocator, .{ .expr = item });
    }

    fn childPat(self: *Writer, item: Mono.PatId) Allocator.Error!void {
        try self.id(item);
        try self.stack.append(self.allocator, .{ .pat = item });
    }

    fn childStmt(self: *Writer, item: Mono.StmtId) Allocator.Error!void {
        try self.id(item);
        try self.stack.append(self.allocator, .{ .stmt = item });
    }

    fn run(self: *Writer) Allocator.Error!void {
        const source = self.source;
        self.expr_seen = try .initEmpty(self.allocator, source.exprs.len);
        self.pat_seen = try .initEmpty(self.allocator, source.pats.len);
        self.stmt_seen = try .initEmpty(self.allocator, source.stmts.len);

        try self.print("roc-lifted-ir {d}\nstage {s}\n", .{ format_version, @tagName(self.stage) });
        try self.writeTypes();

        try self.print("locals {d}\n", .{source.locals.len});
        for (source.locals) |local| {
            try self.print("l {d} {d}", .{ @backingInt(local.id), @backingInt(local.ty) });
            try self.optId(local.identity.captureId());
            try self.out.append(self.allocator, '\n');
        }

        try self.print("fns {d}\n", .{source.fns.len});
        for (source.fns, 0..) |function, index| {
            try self.print("f {d}", .{index});
            try self.typedLocals(source.typedLocalSpan(function.args));
            try self.typedLocals(source.typedLocalSpan(function.captures));
            try self.id(function.ret);
            switch (function.body) {
                .roc => |body| try self.child(body),
                .hosted => try self.out.appendSlice(self.allocator, " hosted"),
            }
            try self.out.append(self.allocator, '\n');
            while (self.stack.pop()) |item| switch (item) {
                .expr => |expr| try self.writeExpr(expr),
                .pat => |pat| try self.writePat(pat),
                .stmt => |stmt| try self.writeStmt(stmt),
            };
        }
        try self.print("roots {d}\n", .{source.roots.len});
        for (source.roots) |root| {
            try self.print("r {d} {s} {d}\n", .{ @backingInt(root.fn_id), @tagName(root.request.kind), root.request.order });
        }
        try self.out.appendSlice(self.allocator, self.extra);
        try self.out.appendSlice(self.allocator, "end\n");
    }

    fn writeTypes(self: *Writer) Allocator.Error!void {
        const types = self.source.types;
        try self.print("types {d}\n", .{types.types.len});
        for (0..types.types.len) |index| {
            const ty: Type.TypeId = @fromBackingInt(@intCast(index));
            try self.print("t {d}", .{index});
            switch (types.get(ty)) {
                .primitive => |prim| try self.print(" prim {s}", .{@tagName(prim)}),
                .zst => try self.out.appendSlice(self.allocator, " zst"),
                .hole => try self.out.appendSlice(self.allocator, " hole"),
                .erased => try self.out.appendSlice(self.allocator, " erased"),
                .list => |elem| try self.print(" list {d}", .{@backingInt(elem)}),
                .box => |elem| try self.print(" box {d}", .{@backingInt(elem)}),
                .tuple => |span| {
                    try self.out.appendSlice(self.allocator, " tuple");
                    try self.typeSpan(types.span(span));
                },
                .func => |func| {
                    try self.out.appendSlice(self.allocator, " func");
                    try self.typeSpan(types.span(func.args));
                    try self.id(func.ret);
                },
                .record => |span| {
                    const items = types.fieldSpan(span);
                    try self.print(" record {d}", .{items.len});
                    for (items) |field| {
                        try self.name(self.source.names.recordFieldLabelText(field.name));
                        try self.id(field.ty);
                    }
                },
                .tag_union => |span| {
                    const items = types.tagSpan(span);
                    try self.print(" tags {d}", .{items.len});
                    for (items) |tag| {
                        try self.name(self.source.names.tagLabelText(tag.name));
                        try self.typeSpan(types.span(tag.payloads));
                    }
                },
                .named => |named| {
                    try self.print(" named {s}", .{@tagName(named.kind)});
                    const module = self.source.names.moduleIdentityBytes(named.def.module);
                    try self.print(" {x}", .{module[0..8]});
                    if (named.def.source_decl.get()) |decl| try self.print(":{d}", .{decl}) else try self.out.appendSlice(self.allocator, ":-");
                    try self.name(self.source.names.typeNameText(named.def.type_name));
                    try self.typeSpan(types.span(named.args));
                    try self.optId(if (named.backing.get()) |backing| backing.ty else null);
                },
            }
            try self.out.append(self.allocator, '\n');
        }
    }

    fn writeExpr(self: *Writer, expr_id: Mono.ExprId) Allocator.Error!void {
        const index = @backingInt(expr_id);
        if (self.expr_seen.isSet(index)) return;
        self.expr_seen.set(index);
        const source = self.source;
        const expr = source.exprs[index];
        try self.print("e {d} {d} {s}", .{ index, @backingInt(expr.ty), @tagName(expr.data) });
        switch (expr.data) {
            .unit, .@"unreachable", .uninitialized, .comptime_exhaustiveness_failed => {},
            .crash, .checked_error, .str_lit => |literal| try self.stringLiteral(literal),
            .int_lit => |value| try self.intValue(value),
            .frac_f32_lit => |value| try self.print(" {x}", .{@as(u32, @bitCast(value))}),
            .frac_f64_lit => |value| try self.print(" {x}", .{@as(u64, @bitCast(value))}),
            .dec_lit => |value| try self.print(" {d}", .{value.num}),
            .bytes_lit => |packed_list| {
                try self.stringLiteral(packed_list.literal);
                try self.print(" {d} {s} {d}", .{
                    packed_list.len,
                    if (packed_list.element) |element| @tagName(element) else "-",
                    packed_list.product_width,
                });
            },
            .local => |local| try self.id(local),
            .uninitialized_payload => |payload| {
                try self.id(payload.condition);
                try self.print(" {d}", .{payload.mask});
            },
            .static_data_candidate => |candidate| if (candidate.value.subexpression()) |value| try self.child(value) else try self.out.appendSlice(self.allocator, " -"),
            .comptime_value => |value| try self.child(value.initializer),
            .typed_boundary => |boundary| try self.child(boundary.value),
            .row_widen => |widen| try self.child(widen.value),
            .nominal, .dbg, .expect => |value| try self.child(value),
            inline .expect_err, .literal_rejected => |failure| try self.child(failure.msg),
            .list, .tuple => |items| try self.exprs(source.exprSpan(items)),
            .record => |span| try self.fields(source.fieldExprSpan(span)),
            .record_update => |update| {
                try self.child(update.base);
                try self.fields(source.fieldExprSpan(update.fields));
            },
            .tag => |tag| {
                try self.name(source.names.tagLabelText(tag.name));
                try self.exprs(source.exprSpan(tag.payloads));
            },
            .let_ => |binding| {
                try self.childPat(binding.bind);
                try self.child(binding.value);
                try self.child(binding.rest);
            },
            .lambda, .def_ref, .fn_def => {},
            .fn_ref => |reference| {
                try self.id(reference.fn_id);
                try self.captures(source.captureOperandSpan(reference.captures));
            },
            .call_value => |call| {
                try self.child(call.callee);
                try self.exprs(source.exprSpan(call.args));
            },
            .call_proc => |call| {
                switch (call.callee) {
                    .lifted => |fn_id| try self.id(fn_id),
                    .func => try self.out.appendSlice(self.allocator, " -"),
                }
                try self.print(" {d}", .{@intFromBool(call.demand_shaped)});
                try self.exprs(source.exprSpan(call.args));
                try self.captures(source.captureOperandSpan(call.captures));
            },
            .low_level => |call| {
                try self.print(" {s}", .{@tagName(call.op)});
                try self.exprs(source.exprSpan(call.args));
            },
            .field_access => |access| {
                try self.child(access.receiver);
                const segments = source.fieldAccessSegmentSpan(access.segments);
                try self.print(" {d}", .{segments.len});
                for (segments) |segment| try self.name(source.names.recordFieldLabelText(segment.field));
            },
            .tuple_access => |access| {
                try self.child(access.tuple);
                try self.print(" {d}", .{access.elem_index});
            },
            .structural_eq => |equal| {
                try self.child(equal.lhs);
                try self.child(equal.rhs);
                try self.print(" {d}", .{@intFromBool(equal.negated)});
            },
            .structural_hash => |hash| {
                try self.child(hash.value);
                try self.child(hash.hasher);
            },
            .match_ => |match| {
                try self.child(match.scrutinee);
                const branches = source.branchSpan(match.branches);
                try self.print(" {d}", .{branches.len});
                for (branches) |branch| {
                    try self.childPat(branch.pat);
                    const bindings = source.stmtSpan(branch.bindings);
                    try self.print(" {d}", .{bindings.len});
                    for (bindings) |stmt| try self.childStmt(stmt);
                    if (branch.guard.get()) |guard| try self.child(guard) else try self.out.appendSlice(self.allocator, " -");
                    try self.child(branch.body);
                }
            },
            .if_ => |conditional| {
                const branches = source.ifBranchSpan(conditional.branches);
                try self.print(" {d}", .{branches.len});
                for (branches) |branch| {
                    try self.child(branch.cond);
                    try self.child(branch.body);
                }
                try self.child(conditional.final_else);
            },
            .if_initialized_payload => |payload| {
                try self.child(payload.cond);
                try self.print(" {d}", .{payload.cond_mask});
                try self.id(payload.payload);
                try self.child(payload.initialized);
                try self.child(payload.uninitialized);
            },
            .try_sequence => |sequence_| {
                try self.child(sequence_.try_expr);
                try self.id(sequence_.ok_local);
                try self.optId(sequence_.err_target);
                try self.child(sequence_.ok_body);
            },
            .try_record_sequence => |sequence_| {
                try self.child(sequence_.try_expr);
                try self.id(sequence_.value_local);
                try self.name(source.names.recordFieldLabelText(sequence_.value_field));
                try self.id(sequence_.rest_local);
                try self.name(source.names.recordFieldLabelText(sequence_.rest_field));
                try self.optId(sequence_.err_target);
                try self.child(sequence_.ok_body);
            },
            .block => |block| {
                const statements = source.stmtSpan(block.statements);
                try self.print(" {d}", .{statements.len});
                for (statements) |stmt| try self.childStmt(stmt);
                try self.child(block.final_expr);
            },
            .loop_ => |loop| {
                try self.typedLocals(source.typedLocalSpan(loop.params));
                try self.exprs(source.exprSpan(loop.initial_values));
                try self.child(loop.body);
            },
            .break_ => |maybe_value| if (maybe_value) |value| try self.child(value) else try self.out.appendSlice(self.allocator, " -"),
            .continue_ => |transfer| try self.exprs(source.exprSpan(transfer.values)),
            .join_point => |join| {
                try self.id(join.id);
                try self.typedLocals(source.typedLocalSpan(join.params));
                try self.child(join.body);
                try self.child(join.remainder);
            },
            .jump => |jump| {
                try self.id(jump.target);
                try self.exprs(source.exprSpan(jump.args));
                try self.typedLocals(source.typedLocalSpan(jump.loop_params));
                try self.exprs(source.exprSpan(jump.loop_values));
            },
            .return_ => |ret| {
                try self.child(ret.value);
                try self.id(ret.target);
            },
        }
        try self.out.append(self.allocator, '\n');
    }

    fn writePat(self: *Writer, pat_id: Mono.PatId) Allocator.Error!void {
        const index = @backingInt(pat_id);
        if (self.pat_seen.isSet(index)) return;
        self.pat_seen.set(index);
        const source = self.source;
        const pat = source.pats[index];
        try self.print("p {d} {d} {s}", .{ index, @backingInt(pat.ty), @tagName(pat.data) });
        switch (pat.data) {
            .wildcard => {},
            .int_lit => |value| try self.intValue(value),
            .dec_lit => |value| try self.print(" {d}", .{value.num}),
            .frac_f32_lit => |value| try self.print(" {x}", .{@as(u32, @bitCast(value))}),
            .frac_f64_lit => |value| try self.print(" {x}", .{@as(u64, @bitCast(value))}),
            .str_lit => |literal| try self.stringLiteral(literal),
            .bind => |local| try self.id(local),
            .as => |as| {
                try self.childPat(as.pattern);
                try self.id(as.local);
            },
            .record => |span| {
                const destructs = source.recordDestructSpan(span);
                try self.print(" {d}", .{destructs.len});
                for (destructs) |destruct| {
                    try self.name(source.names.recordFieldLabelText(destruct.name));
                    try self.childPat(destruct.pattern);
                }
            },
            .tuple => |span| {
                const items = source.patSpan(span);
                try self.print(" {d}", .{items.len});
                for (items) |item| try self.childPat(item);
            },
            .list => |list| {
                const items = source.patSpan(list.patterns);
                try self.print(" {d}", .{items.len});
                for (items) |item| try self.childPat(item);
                if (list.rest.get()) |rest| {
                    try self.print(" {d}", .{rest.index});
                    if (rest.pattern.get()) |rest_pat| try self.childPat(rest_pat) else try self.out.appendSlice(self.allocator, " -");
                } else try self.out.appendSlice(self.allocator, " - -");
            },
            .tag => |tag| {
                try self.name(source.names.tagLabelText(tag.name));
                const items = source.patSpan(tag.payloads);
                try self.print(" {d}", .{items.len});
                for (items) |item| try self.childPat(item);
            },
            .nominal => |inner| try self.childPat(inner),
            .str_pattern => |str| {
                try self.stringLiteral(str.prefix);
                const steps = source.strPatternStepSpan(str.steps);
                try self.print(" {d}", .{steps.len});
                for (steps) |step| {
                    if (step.capture.get()) |capture| try self.childPat(capture) else try self.out.appendSlice(self.allocator, " -");
                    try self.stringLiteral(step.delimiter);
                }
                try self.print(" {s}", .{@tagName(str.end)});
            },
        }
        try self.out.append(self.allocator, '\n');
    }

    fn writeStmt(self: *Writer, stmt_id: Mono.StmtId) Allocator.Error!void {
        const index = @backingInt(stmt_id);
        if (self.stmt_seen.isSet(index)) return;
        self.stmt_seen.set(index);
        const statement = self.source.stmts[index];
        try self.print("s {d} {s}", .{ index, @tagName(statement) });
        switch (statement) {
            .uninitialized => |pat| try self.childPat(pat),
            .let_ => |binding| {
                try self.childPat(binding.pat);
                try self.child(binding.value);
                try self.print(" {d}", .{@intFromBool(binding.recursive)});
            },
            .expr, .expect, .dbg => |value| try self.child(value),
            .return_ => |ret| {
                try self.child(ret.value);
                try self.id(ret.target);
            },
            .crash, .checked_error => |literal| try self.stringLiteral(literal),
        }
        try self.out.append(self.allocator, '\n');
    }

    /// A string literal's bytes, as one name token.
    fn stringLiteral(self: *Writer, literal: Mono.StringLiteralId) Allocator.Error!void {
        try self.name(self.source.string_literals[@backingInt(literal)].text());
    }

    /// An integer literal: `i` or `u` for its kind, then its decimal value.
    fn intValue(self: *Writer, value: can.CIR.IntValue) Allocator.Error!void {
        switch (value.kind) {
            .i128 => try self.print(" i{d}", .{@as(i128, @bitCast(value.bytes))}),
            .u128 => try self.print(" u{d}", .{@as(u128, @bitCast(value.bytes))}),
        }
    }
};

/// The callable variants direct LIR lowering gives each value call, for
/// `ROC_LAMBDA_SET_EXPORT`. Lowering workers share one export, so recording
/// takes a lock and allocates from the page allocator.
pub const LambdaSetExport = struct {
    mutex: std.atomic.Mutex = .unlocked,
    records: std.ArrayList(u8) = .empty,

    /// Whether this compiler writes the export.
    pub fn enabled() bool {
        return supported and base.dev_diagnostics.get(.lambda_set_export) != null;
    }

    /// Release the recorded call sites.
    pub fn deinit(self: *LambdaSetExport) void {
        self.records.deinit(std.heap.page_allocator);
    }

    /// Record that the value call whose callee is `callee` dispatches over
    /// `members`, the lifted functions of its callable variants, in variant
    /// order: `c <callee> finite <n> <fn>...`.
    pub fn recordFinite(self: *LambdaSetExport, callee: Mono.ExprId, members: []const Lifted.FnId) Allocator.Error!void {
        while (!self.mutex.tryLock()) std.atomic.spinLoopHint();
        defer self.mutex.unlock();
        const gpa = std.heap.page_allocator;
        try self.records.print(gpa, "c {d} finite {d}", .{ @backingInt(callee), members.len });
        for (members) |member| try self.records.print(gpa, " {d}", .{@backingInt(member)});
        try self.records.append(gpa, '\n');
    }

    /// Record that the value call whose callee is `callee` is an erased call:
    /// `c <callee> erased`.
    pub fn recordErased(self: *LambdaSetExport, callee: Mono.ExprId) Allocator.Error!void {
        while (!self.mutex.tryLock()) std.atomic.spinLoopHint();
        defer self.mutex.unlock();
        try self.records.print(std.heap.page_allocator, "c {d} erased\n", .{@backingInt(callee)});
    }

    /// Write `program` with the recorded call sites to the export directory.
    pub fn flush(self: *LambdaSetExport, allocator: Allocator, program: *const Lifted.Program) Allocator.Error!void {
        if (comptime !supported) return;
        const dir = base.dev_diagnostics.get(.lambda_set_export) orelse return;
        var out: std.ArrayList(u8) = .empty;
        defer out.deinit(allocator);
        try write(allocator, program.view(), .lambda_solved, self.records.items, &out);
        _ = try writeFile(allocator, dir, .lambda_solved, null, out.items);
    }
};

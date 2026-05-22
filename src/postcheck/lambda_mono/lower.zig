//! Lambda Solved IR to Lambda Mono IR.

const std = @import("std");

const Common = @import("../common.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Solved = @import("../lambda_solved/ast.zig");
const SolvedType = @import("../lambda_solved/type.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;

pub fn run(
    allocator: Allocator,
    solved: Solved.Program,
) Common.LowerError!Ast.Program {
    var owned = solved;
    errdefer owned.deinit();

    var string_literals = owned.lifted.string_literals;
    owned.lifted.string_literals = .empty;
    var name_store = owned.lifted.names;
    owned.lifted.names = @import("check").CheckedNames.NameStore.init(allocator);
    var program = Ast.Program.init(allocator, name_store, string_literals);
    name_store = undefined;
    string_literals = undefined;
    errdefer program.deinit();

    var lowerer = try Lowerer.init(allocator, &owned, &program);
    defer lowerer.deinit();
    try lowerer.lower();
    program.next_symbol = lowerer.symbols.next;

    owned.deinit();
    return program;
}

const CaptureBinding = struct {
    record: Ast.ExprId,
    symbol: Common.Symbol,
    ty: Type.TypeId,
};

const Lowerer = struct {
    allocator: Allocator,
    solved: *const Solved.Program,
    program: *Ast.Program,
    type_map: std.AutoHashMap(SolvedType.TypeVarId, Type.TypeId),
    local_map: []?Ast.LocalId,
    expr_map: []?Ast.ExprId,
    pat_map: []?Ast.PatId,
    stmt_map: []?Ast.StmtId,
    fn_map: []Ast.FnId,
    fn_written: []bool,
    captures: std.AutoHashMap(Lifted.LocalId, CaptureBinding),
    symbols: Common.SymbolGen,

    fn init(allocator: Allocator, solved: *const Solved.Program, program: *Ast.Program) Allocator.Error!Lowerer {
        const local_map = try allocator.alloc(?Ast.LocalId, solved.lifted.locals.items.len);
        errdefer allocator.free(local_map);
        @memset(local_map, null);

        const expr_map = try allocator.alloc(?Ast.ExprId, solved.lifted.exprs.items.len);
        errdefer allocator.free(expr_map);
        @memset(expr_map, null);

        const pat_map = try allocator.alloc(?Ast.PatId, solved.lifted.pats.items.len);
        errdefer allocator.free(pat_map);
        @memset(pat_map, null);

        const stmt_map = try allocator.alloc(?Ast.StmtId, solved.lifted.stmts.items.len);
        errdefer allocator.free(stmt_map);
        @memset(stmt_map, null);

        const fn_map = try allocator.alloc(Ast.FnId, solved.lifted.fns.items.len);
        errdefer allocator.free(fn_map);

        const fn_written = try allocator.alloc(bool, solved.lifted.fns.items.len);
        errdefer allocator.free(fn_written);
        @memset(fn_written, false);

        return .{
            .allocator = allocator,
            .solved = solved,
            .program = program,
            .type_map = std.AutoHashMap(SolvedType.TypeVarId, Type.TypeId).init(allocator),
            .local_map = local_map,
            .expr_map = expr_map,
            .pat_map = pat_map,
            .stmt_map = stmt_map,
            .fn_map = fn_map,
            .fn_written = fn_written,
            .captures = std.AutoHashMap(Lifted.LocalId, CaptureBinding).init(allocator),
            .symbols = .{ .next = solved.lifted.next_symbol },
        };
    }

    fn deinit(self: *Lowerer) void {
        self.captures.deinit();
        self.type_map.deinit();
        self.allocator.free(self.fn_written);
        self.allocator.free(self.fn_map);
        self.allocator.free(self.stmt_map);
        self.allocator.free(self.pat_map);
        self.allocator.free(self.expr_map);
        self.allocator.free(self.local_map);
    }

    fn lower(self: *Lowerer) Allocator.Error!void {
        try self.reserveFns();
        for (self.solved.lifted.fns.items, 0..) |fn_, index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            try self.lowerFn(fn_id, fn_);
        }

        for (self.fn_written) |written| {
            if (!written) Common.invariant("Lambda Mono function reservation was not filled");
        }

        for (self.solved.lifted.roots.items) |root| {
            try self.program.roots.append(self.allocator, .{
                .fn_id = self.fn_map[@intFromEnum(root.fn_id)],
                .request = root.request,
            });
        }
    }

    fn reserveFns(self: *Lowerer) Allocator.Error!void {
        try self.program.fns.ensureTotalCapacity(self.allocator, self.solved.lifted.fns.items.len);
        for (self.solved.lifted.fns.items, 0..) |_, index| {
            const id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.program.fns.items.len)));
            self.fn_map[index] = id;
            try self.program.fns.append(self.allocator, undefined);
        }
    }

    fn lowerFn(self: *Lowerer, fn_id: Lifted.FnId, fn_: Lifted.Fn) Allocator.Error!void {
        self.captures.clearRetainingCapacity();

        const solved_fn_ty = self.solved.fn_tys.items[@intFromEnum(fn_id)];
        const func = switch (self.solved.types.rootContent(solved_fn_ty)) {
            .func => |func| func,
            else => Common.invariant("Lambda Mono function table contains a non-function type"),
        };

        const solved_args = self.solved.types.span(func.args);
        const lifted_args = self.solved.lifted.typedLocalSpan(fn_.args);
        if (solved_args.len != lifted_args.len) Common.invariant("Lambda Mono function arity changed after Lambda Solved");

        var args = std.ArrayList(Ast.TypedLocal).empty;
        defer args.deinit(self.allocator);

        for (lifted_args, 0..) |arg, i| {
            const arg_ty = try self.lowerType(solved_args[i]);
            const local = try self.localFor(arg.local, arg_ty);
            try args.append(self.allocator, .{ .local = local, .ty = arg_ty });
        }

        const maybe_capture_record = try self.captureRecordForFn(fn_id);
        if (maybe_capture_record) |capture_record| {
            const capture_local = try self.program.addLocal(self.symbols.fresh(), capture_record.ty);
            const capture_expr = try self.program.addExpr(.{
                .ty = capture_record.ty,
                .data = .{ .local = capture_local },
            });
            try args.append(self.allocator, .{ .local = capture_local, .ty = capture_record.ty });
            for (capture_record.captures) |capture| {
                try self.captures.put(capture.local, .{
                    .record = capture_expr,
                    .symbol = capture.symbol,
                    .ty = try self.lowerType(capture.ty),
                });
            }
        }

        const body = try self.lowerExpr(fn_.body);
        const ret = try self.lowerType(func.ret);

        const out_id = self.fn_map[@intFromEnum(fn_id)];
        self.program.fns.items[@intFromEnum(out_id)] = .{
            .symbol = fn_.symbol,
            .source = fn_.source,
            .args = try self.program.addTypedLocalSpan(args.items),
            .body = body,
            .ret = ret,
        };
        self.fn_written[@intFromEnum(fn_id)] = true;
    }

    const CaptureRecord = struct {
        ty: Type.TypeId,
        captures: []const SolvedType.Capture,
    };

    fn captureRecordForFn(self: *Lowerer, fn_id: Lifted.FnId) Allocator.Error!?CaptureRecord {
        const fn_symbol = self.solved.lifted.fns.items[@intFromEnum(fn_id)].symbol;
        const func = switch (self.solved.types.rootContent(self.solved.fn_tys.items[@intFromEnum(fn_id)])) {
            .func => |func| func,
            else => Common.invariant("Lambda Mono function table contains a non-function type"),
        };
        return switch (self.solved.types.rootContent(func.callable)) {
            .lambda_set => |members| {
                for (self.solved.types.memberSpan(members)) |member| {
                    if (member.lambda != fn_symbol) continue;
                    const captures = self.solved.types.captureSpan(member.captures);
                    if (captures.len == 0) return null;
                    return .{
                        .ty = try self.captureRecordType(member.captures),
                        .captures = captures,
                    };
                }
                Common.invariant("function callable slot did not contain its own lambda member");
            },
            .erased => return null,
            else => Common.invariant("function callable slot was unresolved before Lambda Mono"),
        };
    }

    fn lowerExpr(self: *Lowerer, expr_id: Lifted.ExprId) Allocator.Error!Ast.ExprId {
        const index = @intFromEnum(expr_id);
        if (self.expr_map[index]) |cached| return cached;

        const expr = self.solved.lifted.exprs.items[index];
        const ty = try self.lowerExprTy(expr_id);
        const data: Ast.ExprData = switch (expr.data) {
            .local => |local| try self.lowerLocalExpr(local, ty),
            .unit => .unit,
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .list => |items| .{ .list = try self.lowerExprSpan(items) },
            .tuple => |items| .{ .tuple = try self.lowerExprSpan(items) },
            .record => |fields| .{ .record = try self.lowerFieldExprSpan(fields) },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.lowerExprSpan(tag.payloads),
            } },
            .nominal => |backing| .{ .nominal = try self.lowerExpr(backing) },
            .let_ => |let_| .{ .let_ = .{
                .bind = try self.lowerPat(let_.bind),
                .value = try self.lowerExpr(let_.value),
                .rest = try self.lowerExpr(let_.rest),
            } },
            .fn_ref => |target| try self.lowerCallableValue(target, ty),
            .fn_def => |fn_def| try self.lowerCallableValue(self.fnIdForSource(fn_def), ty),
            .call_value => |call| try self.lowerValueCall(ty, call),
            .call_proc => |call| .{ .direct_call = .{
                .target = self.fn_map[@intFromEnum(self.fnIdForSource(call.callee))],
                .args = try self.lowerExprSpan(call.args),
            } },
            .low_level => |call| .{ .low_level = .{
                .op = call.op,
                .args = try self.lowerExprSpan(call.args),
            } },
            .field_access => |field| .{ .field_access = .{
                .receiver = try self.lowerExpr(field.receiver),
                .field = field.field,
            } },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
                .negated = eq.negated,
            } },
            .match_ => |match| .{ .match_ = .{
                .scrutinee = try self.lowerExpr(match.scrutinee),
                .branches = try self.lowerBranchSpan(match.branches),
            } },
            .if_ => |if_| .{ .if_ = .{
                .branches = try self.lowerIfBranchSpan(if_.branches),
                .final_else = try self.lowerExpr(if_.final_else),
            } },
            .block => |block| .{ .block = .{
                .statements = try self.lowerStmtSpan(block.statements),
                .final_expr = try self.lowerExpr(block.final_expr),
            } },
            .loop_ => |loop| .{ .loop_ = .{
                .params = try self.lowerTypedLocalSpan(loop.params),
                .initial_values = try self.lowerExprSpan(loop.initial_values),
                .body = try self.lowerExpr(loop.body),
            } },
            .break_ => |maybe| .{ .break_ = if (maybe) |value| try self.lowerExpr(value) else null },
            .continue_ => |continue_| .{ .continue_ = .{ .values = try self.lowerExprSpan(continue_.values) } },
            .return_ => |value| .{ .return_ = try self.lowerExpr(value) },
            .crash => |msg| .{ .crash = msg },
            .dbg => |child| .{ .dbg = try self.lowerExpr(child) },
            .expect => |child| .{ .expect = try self.lowerExpr(child) },
        };

        const lowered = try self.program.addExpr(.{ .ty = ty, .data = data });
        self.expr_map[index] = lowered;
        return lowered;
    }

    fn lowerLocalExpr(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId) Allocator.Error!Ast.ExprData {
        if (self.captures.get(local)) |capture| {
            return .{ .capture_access = .{
                .record = capture.record,
                .symbol = capture.symbol,
            } };
        }
        return .{ .local = try self.localFor(local, ty) };
    }

    fn lowerCallableValue(self: *Lowerer, fn_id: Lifted.FnId, ty: Type.TypeId) Allocator.Error!Ast.ExprData {
        return switch (self.program.types.get(ty)) {
            .callable => |variants| blk: {
                const fn_symbol = self.solved.lifted.fns.items[@intFromEnum(fn_id)].symbol;
                for (self.program.types.fnVariantSpan(variants)) |variant| {
                    if (variant.lambda != fn_symbol) continue;
                    break :blk .{ .callable = .{
                        .ty = ty,
                        .variant = variant.id,
                        .payload = if (variant.capture_ty) |capture_ty| try self.buildCaptureRecord(fn_id, capture_ty) else null,
                    } };
                }
                Common.invariant("finite callable type did not contain referenced function");
            },
            .erased_fn => .{ .packed_erased_fn = .{
                .target = self.fn_map[@intFromEnum(fn_id)],
                .capture = null,
            } },
            else => Common.invariant("function value lowered to non-callable Lambda Mono type"),
        };
    }

    fn lowerValueCall(self: *Lowerer, ty: Type.TypeId, call: anytype) Allocator.Error!Ast.ExprData {
        const callee = try self.lowerExpr(call.callee);
        const callee_ty = self.program.exprs.items[@intFromEnum(callee)].ty;
        const args = try self.lowerExprSlice(self.solved.lifted.exprSpan(call.args));
        defer self.allocator.free(args);

        return switch (self.program.types.get(callee_ty)) {
            .callable => |variants| blk: {
                const branches = try self.allocator.alloc(Ast.Branch, variants.len);
                defer self.allocator.free(branches);
                for (self.program.types.fnVariantSpan(variants), 0..) |variant, i| {
                    const payload_pat = if (variant.capture_ty) |capture_ty| blk_payload: {
                        const local = try self.program.addLocal(self.symbols.fresh(), capture_ty);
                        break :blk_payload try self.program.addPat(.{ .ty = capture_ty, .data = .{ .bind = local } });
                    } else null;

                    const pat = try self.program.addPat(.{
                        .ty = callee_ty,
                        .data = .{ .callable = .{
                            .variant = variant.id,
                            .payload = payload_pat,
                        } },
                    });

                    const call_args = try self.allocator.alloc(Ast.ExprId, args.len + if (payload_pat != null) @as(usize, 1) else 0);
                    defer self.allocator.free(call_args);
                    @memcpy(call_args[0..args.len], args);
                    if (payload_pat) |pat_id| {
                        const bind_local = switch (self.program.pats.items[@intFromEnum(pat_id)].data) {
                            .bind => |local| local,
                            else => unreachable,
                        };
                        call_args[args.len] = try self.program.addExpr(.{
                            .ty = variant.capture_ty.?,
                            .data = .{ .local = bind_local },
                        });
                    }

                    branches[i] = .{
                        .pat = pat,
                        .body = try self.program.addExpr(.{
                            .ty = ty,
                            .data = .{ .direct_call = .{
                                .target = self.fnIdForSymbol(variant.lambda),
                                .args = try self.program.addExprSpan(call_args),
                            } },
                        }),
                    };
                }

                break :blk .{ .match_ = .{
                    .scrutinee = callee,
                    .branches = try self.program.addBranchSpan(branches),
                } };
            },
            .erased_fn => .{ .indirect_erased_call = .{
                .callee = callee,
                .args = try self.program.addExprSpan(args),
            } },
            else => Common.invariant("value call callee had no callable Lambda Mono representation"),
        };
    }

    fn buildCaptureRecord(self: *Lowerer, fn_id: Lifted.FnId, capture_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const func = switch (self.solved.types.rootContent(self.solved.fn_tys.items[@intFromEnum(fn_id)])) {
            .func => |func| func,
            else => Common.invariant("Lambda Mono function table contains a non-function type"),
        };
        const callable = switch (self.solved.types.rootContent(func.callable)) {
            .lambda_set => |members| members,
            else => Common.invariant("finite callable value did not have a finite callable slot"),
        };
        const fn_symbol = self.solved.lifted.fns.items[@intFromEnum(fn_id)].symbol;
        for (self.solved.types.memberSpan(callable)) |member| {
            if (member.lambda != fn_symbol) continue;
            const captures = self.solved.types.captureSpan(member.captures);
            const values = try self.allocator.alloc(Ast.ExprId, captures.len);
            defer self.allocator.free(values);
            for (captures, 0..) |capture, i| {
                values[i] = try self.lowerCapturedValue(capture);
            }
            return try self.program.addExpr(.{
                .ty = capture_ty,
                .data = .{ .capture_record = try self.program.addExprSpan(values) },
            });
        }
        Common.invariant("finite callable slot did not contain referenced function");
    }

    fn lowerCapturedValue(self: *Lowerer, capture: SolvedType.Capture) Allocator.Error!Ast.ExprId {
        const ty = try self.lowerType(capture.ty);
        const data = try self.lowerLocalExpr(capture.local, ty);
        return try self.program.addExpr(.{ .ty = ty, .data = data });
    }

    fn lowerPat(self: *Lowerer, pat_id: Lifted.PatId) Allocator.Error!Ast.PatId {
        const index = @intFromEnum(pat_id);
        if (self.pat_map[index]) |cached| return cached;
        const pat = self.solved.lifted.pats.items[index];
        const ty = try self.lowerTypeForSolvedPat(pat_id);
        const data: Ast.PatData = switch (pat.data) {
            .bind => |local| .{ .bind = try self.localFor(local, ty) },
            .wildcard => .wildcard,
            .as => |as| .{ .as = .{
                .pattern = try self.lowerPat(as.pattern),
                .local = try self.localFor(as.local, ty),
            } },
            .record => |fields| .{ .record = try self.lowerRecordDestructSpan(fields) },
            .tuple => |items| .{ .tuple = try self.lowerPatSpan(items) },
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .payloads = try self.lowerPatSpan(tag.payloads),
            } },
            .nominal => |backing| .{ .nominal = try self.lowerPat(backing) },
            .int_lit => |value| .{ .int_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .str_lit => |value| .{ .str_lit = value },
        };
        const lowered = try self.program.addPat(.{ .ty = ty, .data = data });
        self.pat_map[index] = lowered;
        return lowered;
    }

    fn lowerStmt(self: *Lowerer, stmt_id: Lifted.StmtId) Allocator.Error!Ast.StmtId {
        const index = @intFromEnum(stmt_id);
        if (self.stmt_map[index]) |cached| return cached;
        const lowered_stmt: Ast.Stmt = switch (self.solved.lifted.stmts.items[index]) {
            .let_ => |let_| .{ .let_ = .{
                .pat = try self.lowerPat(let_.pat),
                .value = try self.lowerExpr(let_.value),
            } },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .dbg => |expr| .{ .dbg = try self.lowerExpr(expr) },
            .return_ => |expr| .{ .return_ = try self.lowerExpr(expr) },
            .crash => |msg| .{ .crash = msg },
        };
        const lowered = try self.program.addStmt(lowered_stmt);
        self.stmt_map[index] = lowered;
        return lowered;
    }

    fn lowerExprTy(self: *Lowerer, expr_id: Lifted.ExprId) Allocator.Error!Type.TypeId {
        return try self.lowerType(self.solved.expr_tys.items[@intFromEnum(expr_id)]);
    }

    fn lowerTypeForSolvedPat(self: *Lowerer, pat_id: Lifted.PatId) Allocator.Error!Type.TypeId {
        return try self.lowerType(self.solved.pat_tys.items[@intFromEnum(pat_id)]);
    }

    fn lowerType(self: *Lowerer, solved_ty: SolvedType.TypeVarId) Allocator.Error!Type.TypeId {
        const root = self.solved.types.root(solved_ty);
        if (self.type_map.get(root)) |cached| return cached;

        switch (self.solved.types.get(root)) {
            .func => |func| return try self.lowerType(func.callable),
            else => {},
        }

        const reserved = try self.program.types.add(.zst);
        try self.type_map.put(root, reserved);
        self.program.types.set(reserved, try self.lowerTypeContent(self.solved.types.get(root)));
        return reserved;
    }

    fn lowerTypeContent(self: *Lowerer, content: SolvedType.Content) Allocator.Error!Type.Content {
        return switch (content) {
            .link => Common.invariant("Lambda Mono type lowering saw an unresolved Lambda Solved link"),
            .unbound, .forall => Common.invariant("Lambda Mono type lowering saw an unresolved Lambda Solved type"),
            .primitive => |primitive| .{ .primitive = primitive },
            .zst => .zst,
            .erased => |erased| .{ .erased_fn = .{ .source_fn_ty = erased.source_fn_ty } },
            .func => |func| return self.program.types.get(try self.lowerType(func.callable)),
            .list => |elem| .{ .list = try self.lowerType(elem) },
            .box => |elem| .{ .box = try self.lowerType(elem) },
            .tuple => |items| blk: {
                const lowered = try self.lowerTypeSpan(self.solved.types.span(items));
                defer self.allocator.free(lowered);
                break :blk .{ .tuple = try self.program.types.addSpan(lowered) };
            },
            .record => |fields| blk: {
                const lowered = try self.allocator.alloc(Type.Field, fields.len);
                defer self.allocator.free(lowered);
                for (self.solved.types.fieldSpan(fields), 0..) |field, i| {
                    lowered[i] = .{ .name = field.name, .ty = try self.lowerType(field.ty) };
                }
                break :blk .{ .record = try self.program.types.addFields(lowered) };
            },
            .tag_union => |tags| blk: {
                const lowered = try self.allocator.alloc(Type.Tag, tags.len);
                defer self.allocator.free(lowered);
                for (self.solved.types.tagSpan(tags), 0..) |tag, i| {
                    const payloads = try self.lowerTypeSpan(self.solved.types.span(tag.payloads));
                    defer self.allocator.free(payloads);
                    lowered[i] = .{ .name = tag.name, .payloads = try self.program.types.addSpan(payloads) };
                }
                break :blk .{ .tag_union = try self.program.types.addTags(lowered) };
            },
            .named => |named| blk: {
                const args = try self.lowerTypeSpan(self.solved.types.span(named.args));
                defer self.allocator.free(args);
                break :blk .{ .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .args = try self.program.types.addSpan(args),
                    .backing = if (named.backing) |backing| .{
                        .ty = try self.lowerType(backing.ty),
                        .use = backing.use,
                    } else null,
                } };
            },
            .lambda_set => |members| blk: {
                const solved_members = self.solved.types.memberSpan(members);
                const variants = try self.allocator.alloc(Type.FnVariant, solved_members.len);
                defer self.allocator.free(variants);
                for (solved_members, 0..) |member, i| {
                    const captures = self.solved.types.captureSpan(member.captures);
                    variants[i] = .{
                        .id = @enumFromInt(0),
                        .lambda = member.lambda,
                        .capture_ty = if (captures.len == 0) null else try self.captureRecordType(member.captures),
                    };
                }
                break :blk .{ .callable = try self.program.types.addFnVariants(variants) };
            },
        };
    }

    fn captureRecordType(self: *Lowerer, captures: SolvedType.Span) Allocator.Error!Type.TypeId {
        const capture_items = self.solved.types.captureSpan(captures);
        const fields = try self.allocator.alloc(Type.CaptureField, capture_items.len);
        defer self.allocator.free(fields);
        for (capture_items, 0..) |capture, i| {
            fields[i] = .{
                .symbol = capture.symbol,
                .binder = capture.binder,
                .ty = try self.lowerType(capture.ty),
            };
        }
        return try self.program.types.add(.{ .capture_record = try self.program.types.addCaptureFields(fields) });
    }

    fn lowerTypeSpan(self: *Lowerer, items: []const SolvedType.TypeVarId) Allocator.Error![]Type.TypeId {
        const lowered = try self.allocator.alloc(Type.TypeId, items.len);
        errdefer self.allocator.free(lowered);
        for (items, 0..) |item, i| lowered[i] = try self.lowerType(item);
        return lowered;
    }

    fn localFor(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId) Allocator.Error!Ast.LocalId {
        const index = @intFromEnum(local);
        if (self.local_map[index]) |existing| return existing;
        const lifted_local = self.solved.lifted.locals.items[index];
        const lowered = try self.program.addLocalWithBinder(lifted_local.symbol, ty, lifted_local.binder);
        self.local_map[index] = lowered;
        return lowered;
    }

    fn lowerExprSpan(self: *Lowerer, span: Lifted.Span(Lifted.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        const lowered = try self.lowerExprSlice(self.solved.lifted.exprSpan(span));
        defer self.allocator.free(lowered);
        return try self.program.addExprSpan(lowered);
    }

    fn lowerExprSlice(self: *Lowerer, exprs: []const Lifted.ExprId) Allocator.Error![]Ast.ExprId {
        const lowered = try self.allocator.alloc(Ast.ExprId, exprs.len);
        errdefer self.allocator.free(lowered);
        for (exprs, 0..) |expr, i| lowered[i] = try self.lowerExpr(expr);
        return lowered;
    }

    fn lowerPatSpan(self: *Lowerer, span: Lifted.Span(Lifted.PatId)) Allocator.Error!Ast.Span(Ast.PatId) {
        const input_items = self.solved.lifted.patSpan(span);
        const lowered = try self.allocator.alloc(Ast.PatId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerPat(item);
        return try self.program.addPatSpan(lowered);
    }

    fn lowerStmtSpan(self: *Lowerer, span: Lifted.Span(Lifted.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.solved.lifted.stmtSpan(span);
        const lowered = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerStmt(item);
        return try self.program.addStmtSpan(lowered);
    }

    fn lowerTypedLocalSpan(self: *Lowerer, span: Lifted.Span(Lifted.TypedLocal)) Allocator.Error!Ast.Span(Ast.TypedLocal) {
        const input_items = self.solved.lifted.typedLocalSpan(span);
        const lowered = try self.allocator.alloc(Ast.TypedLocal, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| {
            const lifted_local = self.solved.lifted.locals.items[@intFromEnum(item.local)];
            const ty = try self.lowerTypeByLiftedLocal(lifted_local.id);
            lowered[i] = .{ .local = try self.localFor(item.local, ty), .ty = ty };
        }
        return try self.program.addTypedLocalSpan(lowered);
    }

    fn lowerTypeByLiftedLocal(self: *Lowerer, local: Lifted.LocalId) Allocator.Error!Type.TypeId {
        const mapped = self.local_map[@intFromEnum(local)] orelse {
            return try self.lowerType(self.solved.local_tys.items[@intFromEnum(local)]);
        };
        return self.program.locals.items[@intFromEnum(mapped)].ty;
    }

    fn lowerFieldExprSpan(self: *Lowerer, span: Lifted.Span(Lifted.FieldExpr)) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const input_items = self.solved.lifted.fieldExprSpan(span);
        const lowered = try self.allocator.alloc(Ast.FieldExpr, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |field, i| {
            lowered[i] = .{
                .name = field.name,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.program.addFieldExprSpan(lowered);
    }

    fn lowerRecordDestructSpan(self: *Lowerer, span: Lifted.Span(Lifted.RecordDestruct)) Allocator.Error!Ast.Span(Ast.RecordDestruct) {
        const input_items = self.solved.lifted.recordDestructSpan(span);
        const lowered = try self.allocator.alloc(Ast.RecordDestruct, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |field, i| {
            lowered[i] = .{
                .name = field.name,
                .pattern = try self.lowerPat(field.pattern),
            };
        }
        return try self.program.addRecordDestructSpan(lowered);
    }

    fn lowerBranchSpan(self: *Lowerer, span: Lifted.Span(Lifted.Branch)) Allocator.Error!Ast.Span(Ast.Branch) {
        const input_items = self.solved.lifted.branchSpan(span);
        const lowered = try self.allocator.alloc(Ast.Branch, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |branch, i| {
            lowered[i] = .{
                .pat = try self.lowerPat(branch.pat),
                .guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null,
                .body = try self.lowerExpr(branch.body),
            };
        }
        return try self.program.addBranchSpan(lowered);
    }

    fn lowerIfBranchSpan(self: *Lowerer, span: Lifted.Span(Lifted.IfBranch)) Allocator.Error!Ast.Span(Ast.IfBranch) {
        const input_items = self.solved.lifted.ifBranchSpan(span);
        const lowered = try self.allocator.alloc(Ast.IfBranch, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |branch, i| {
            lowered[i] = .{
                .cond = try self.lowerExpr(branch.cond),
                .body = try self.lowerExpr(branch.body),
            };
        }
        return try self.program.addIfBranchSpan(lowered);
    }

    fn fnIdForSource(self: *Lowerer, fn_def: @import("../monotype/ast.zig").FnTemplate) Lifted.FnId {
        for (self.solved.lifted.fns.items, 0..) |fn_, index| {
            if (fn_.source) |source| {
                if (std.meta.eql(source, fn_def)) return @enumFromInt(@as(u32, @intCast(index)));
            }
        }
        Common.invariant("Lambda Mono referenced a function definition that was not lifted");
    }

    fn fnIdForSymbol(self: *Lowerer, symbol: Common.Symbol) Ast.FnId {
        for (self.solved.lifted.fns.items, 0..) |fn_, index| {
            if (fn_.symbol == symbol) return self.fn_map[index];
        }
        Common.invariant("Lambda Mono callable variant referenced a missing function symbol");
    }
};

test "lambda mono lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}

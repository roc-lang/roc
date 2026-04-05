//! Executable lowering from lambdasolved to lambdamono.

const std = @import("std");
const base = @import("base");
const solved = @import("lambdasolved");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");
const lower_type = @import("lower_type.zig");
const specializations = @import("specializations.zig");

const Symbol = symbol_mod.Symbol;
const TypeVarId = solved.Type.TypeVarId;

pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    strings: base.StringLiteral.Store,

    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.types.deinit();
        self.strings.deinit(self.store.allocator);
    }
};

pub fn run(allocator: std.mem.Allocator, input: solved.Lower.Result) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, input);
    defer lowerer.deinit();
    try lowerer.lowerProgram();
    return lowerer.finish();
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    input: solved.Lower.Result,
    output: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    types: type_mod.Store,
    queue: specializations.Queue,
    fenv: []specializations.FEnvEntry,
    pending_values: std.ArrayList(ast.Def),

    const EnvEntry = struct {
        symbol: Symbol,
        ty: TypeVarId,
    };

    const InstScope = struct {
        allocator: std.mem.Allocator,
        mapping: std.AutoHashMap(TypeVarId, TypeVarId),

        fn init(allocator: std.mem.Allocator) InstScope {
            return .{
                .allocator = allocator,
                .mapping = std.AutoHashMap(TypeVarId, TypeVarId).init(allocator),
            };
        }

        fn deinit(self: *InstScope) void {
            self.mapping.deinit();
        }
    };

    fn init(allocator: std.mem.Allocator, input: solved.Lower.Result) Lowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = ast.Store.init(allocator),
            .root_defs = .empty,
            .types = type_mod.Store.init(allocator),
            .queue = specializations.Queue.init(allocator),
            .fenv = &.{},
            .pending_values = .empty,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.pending_values.deinit(self.allocator);
        self.queue.deinit();
        if (self.fenv.len != 0) self.allocator.free(self.fenv);
        self.types.deinit();
        self.root_defs.deinit(self.allocator);
        self.output.deinit();
        self.input.deinit();
    }

    fn finish(self: *Lowerer) Result {
        const result = Result{
            .store = self.output,
            .root_defs = self.root_defs,
            .symbols = self.input.symbols,
            .types = self.types,
            .strings = self.input.strings,
        };

        self.output = ast.Store.init(self.allocator);
        self.root_defs = .empty;
        self.input.symbols = symbol_mod.Store.init(self.allocator);
        self.input.strings = .{};
        self.types = type_mod.Store.init(self.allocator);
        return result;
    }

    fn lowerProgram(self: *Lowerer) std.mem.Allocator.Error!void {
        self.fenv = try specializations.buildFEnv(self.allocator, &self.input);

        for (self.input.store.defsSlice()) |def| {
            switch (def.value) {
                .fn_ => {},
                .val => |expr_id| try self.pending_values.append(self.allocator, .{
                    .bind = def.bind.symbol,
                    .value = .{ .val = try self.specializeStandaloneExpr(expr_id) },
                }),
                .run => |run_def| try self.pending_values.append(self.allocator, .{
                    .bind = def.bind.symbol,
                    .value = .{ .run = .{
                        .body = try self.specializeStandaloneExpr(run_def.body),
                        .entry_ty = run_def.entry_ty,
                    } },
                }),
            }
        }

        while (self.queue.nextPending()) |pending| {
            pending.specialized = try self.specializeFn(pending.*);
        }

        const fn_defs = try self.queue.solvedDefs(self.allocator);
        defer self.allocator.free(fn_defs);

        for (fn_defs) |def| {
            const def_id = try self.output.addDef(def);
            try self.root_defs.append(self.allocator, def_id);
        }
        for (self.pending_values.items) |def| {
            const def_id = try self.output.addDef(def);
            try self.root_defs.append(self.allocator, def_id);
        }
    }

    fn specializeStandaloneExpr(self: *Lowerer, expr_id: solved.Ast.ExprId) std.mem.Allocator.Error!ast.ExprId {
        var inst = InstScope.init(self.allocator);
        defer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();
        return try self.specializeExpr(&inst, &mono_cache, &.{}, expr_id);
    }

    fn specializeFn(self: *Lowerer, pending: specializations.Pending) std.mem.Allocator.Error!ast.FnDef {
        var inst = InstScope.init(self.allocator);
        defer inst.deinit();
        var mono_cache = lower_type.MonoCache.init(self.allocator);
        defer mono_cache.deinit();

        const t = try self.cloneInstType(&inst, pending.fn_ty);
        try self.unify(t, pending.requested_ty);

        const arg_ty = try self.cloneInstType(&inst, pending.fn_def.arg.ty);
        const lowered_arg_ty = try lower_type.lowerType(
            &self.input.types,
            &self.types,
            &mono_cache,
            arg_ty,
            &self.input.symbols,
        );

        return switch (lower_type.lambdaRepr(&self.input.types, t)) {
            .lset => switch (try lower_type.extractLsetFn(
                &self.input.types,
                &self.types,
                &mono_cache,
                t,
                pending.name,
                &self.input.symbols,
            )) {
                .toplevel => .{
                    .args = try self.output.addTypedSymbolSpan(&.{.{
                        .ty = lowered_arg_ty,
                        .symbol = pending.fn_def.arg.symbol,
                    }}),
                    .body = try self.specializeExpr(
                        &inst,
                        &mono_cache,
                        &.{.{ .symbol = pending.fn_def.arg.symbol, .ty = arg_ty }},
                        pending.fn_def.body,
                    ),
                },
                .lset => |capture_info| blk: {
                    const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                    const capture_bindings = try self.captureBindingsFromSolved(capture_info.captures);
                    defer self.allocator.free(capture_bindings);
                    const body_env = try self.buildFnBodyEnv(arg_ty, pending.fn_def.arg.symbol, capture_bindings);
                    defer self.allocator.free(body_env);

                    var body = try self.specializeExpr(&inst, &mono_cache, body_env, pending.fn_def.body);
                    body = try self.bindCaptureLets(
                        &mono_cache,
                        captures_symbol,
                        capture_info.ty,
                        capture_bindings,
                        body,
                    );

                    break :blk .{
                        .args = try self.output.addTypedSymbolSpan(&.{
                            .{
                                .ty = lowered_arg_ty,
                                .symbol = pending.fn_def.arg.symbol,
                            },
                            .{
                                .ty = capture_info.ty,
                                .symbol = captures_symbol,
                            },
                        }),
                        .body = body,
                    };
                },
            },
            .erased => blk: {
                const captures_new = pending.captures_new orelse &.{};
                const captures = try self.captureBindingsFromTypedSymbols(&inst, pending.fn_def.captures);
                defer self.allocator.free(captures);
                self.sortCaptureBindings(captures);

                if (captures.len != captures_new.len) {
                    debugPanic("lambdamono.lower.specializeFn erased capture arity mismatch");
                }
                for (captures, captures_new) |capture, capture_new| {
                    if (capture.symbol != capture_new.symbol) {
                        debugPanic("lambdamono.lower.specializeFn erased capture symbol mismatch");
                    }
                    try self.unify(capture.ty, capture_new.ty);
                }

                const body_env = try self.buildFnBodyEnv(arg_ty, pending.fn_def.arg.symbol, captures);
                defer self.allocator.free(body_env);

                var body = try self.specializeExpr(&inst, &mono_cache, body_env, pending.fn_def.body);
                if (captures.len == 0) {
                    break :blk .{
                        .args = try self.output.addTypedSymbolSpan(&.{.{
                            .ty = lowered_arg_ty,
                            .symbol = pending.fn_def.arg.symbol,
                        }}),
                        .body = body,
                    };
                }

                const captures_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                const captures_ty = try lower_type.lowerCaptureBindings(
                    &self.input.types,
                    &self.types,
                    &mono_cache,
                    captures,
                    &self.input.symbols,
                );
                body = try self.bindCaptureLets(&mono_cache, captures_symbol, captures_ty, captures, body);

                break :blk .{
                    .args = try self.output.addTypedSymbolSpan(&.{
                        .{
                            .ty = lowered_arg_ty,
                            .symbol = pending.fn_def.arg.symbol,
                        },
                        .{
                            .ty = captures_ty,
                            .symbol = captures_symbol,
                        },
                    }),
                    .body = body,
                };
            },
        };
    }

    fn buildFnBodyEnv(
        self: *Lowerer,
        arg_ty: TypeVarId,
        arg_symbol: Symbol,
        captures: []const lower_type.CaptureBinding,
    ) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, captures.len + 1);
        out[0] = .{ .symbol = arg_symbol, .ty = arg_ty };
        for (captures, 0..) |capture, i| {
            out[i + 1] = .{
                .symbol = capture.symbol,
                .ty = capture.ty,
            };
        }
        return out;
    }

    fn bindCaptureLets(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        captures_symbol: Symbol,
        capture_record_ty: type_mod.TypeId,
        captures: []const lower_type.CaptureBinding,
        body: ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        var result = body;
        var idx = captures.len;
        while (idx > 0) {
            idx -= 1;
            const capture = captures[idx];
            const lowered_capture_ty = try lower_type.lowerType(
                &self.input.types,
                &self.types,
                mono_cache,
                capture.ty,
                &self.input.symbols,
            );
            const captures_var = try self.output.addExpr(.{
                .ty = capture_record_ty,
                .data = .{ .var_ = captures_symbol },
            });
            const access = try self.output.addExpr(.{
                .ty = lowered_capture_ty,
                .data = .{ .access = .{
                    .record = captures_var,
                    .field = self.input.symbols.get(capture.symbol).name,
                } },
            });
            result = try self.output.addExpr(.{
                .ty = self.output.getExpr(result).ty,
                .data = .{ .let_ = .{
                    .bind = .{
                        .ty = lowered_capture_ty,
                        .symbol = capture.symbol,
                    },
                    .body = access,
                    .rest = result,
                } },
            });
        }
        return result;
    }

    fn specializeExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        expr_id: solved.Ast.ExprId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const expr = self.input.store.getExpr(expr_id);
        const ty = try self.cloneInstType(inst, expr.ty);
        const lowered_ty = try lower_type.lowerType(&self.input.types, &self.types, mono_cache, ty, &self.input.symbols);

        const data: ast.Expr.Data = switch (expr.data) {
            .var_ => |symbol| try self.specializeVarExpr(inst, mono_cache, venv, symbol, ty, lowered_ty),
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .unit => .unit,
            .tag => |tag| .{ .tag = .{
                .name = tag.name,
                .args = try self.specializeExprSpan(inst, mono_cache, venv, tag.args),
            } },
            .record => |fields| .{ .record = try self.specializeFieldSpan(inst, mono_cache, venv, fields) },
            .access => |access| .{ .access = .{
                .record = try self.specializeExpr(inst, mono_cache, venv, access.record),
                .field = access.field,
            } },
            .let_ => |let_expr| .{ .let_ = .{
                .bind = .{
                    .ty = try lower_type.lowerType(
                        &self.input.types,
                        &self.types,
                        mono_cache,
                        try self.cloneInstType(inst, let_expr.bind.ty),
                        &self.input.symbols,
                    ),
                    .symbol = let_expr.bind.symbol,
                },
                .body = try self.specializeExpr(inst, mono_cache, venv, let_expr.body),
                .rest = blk: {
                    const bind_ty = try self.cloneInstType(inst, let_expr.bind.ty);
                    const rest_env = try self.extendEnv(venv, .{
                        .symbol = let_expr.bind.symbol,
                        .ty = bind_ty,
                    });
                    defer self.allocator.free(rest_env);
                    break :blk try self.specializeExpr(inst, mono_cache, rest_env, let_expr.rest);
                },
            } },
            .call => |call| try self.specializeCallExpr(inst, mono_cache, venv, call, lowered_ty),
            .low_level => |ll| .{ .low_level = .{
                .op = ll.op,
                .args = try self.specializeExprSpan(inst, mono_cache, venv, ll.args),
            } },
            .when => |when_expr| .{ .when = .{
                .cond = try self.specializeExpr(inst, mono_cache, venv, when_expr.cond),
                .branches = try self.specializeBranchSpan(inst, mono_cache, venv, when_expr.branches),
            } },
            .if_ => |if_expr| .{ .if_ = .{
                .cond = try self.specializeExpr(inst, mono_cache, venv, if_expr.cond),
                .then_body = try self.specializeExpr(inst, mono_cache, venv, if_expr.then_body),
                .else_body = try self.specializeExpr(inst, mono_cache, venv, if_expr.else_body),
            } },
            .block => |block| .{ .block = try self.specializeBlockExpr(inst, mono_cache, venv, block) },
            .tuple => |elems| .{ .tuple = try self.specializeExprSpan(inst, mono_cache, venv, elems) },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.specializeExpr(inst, mono_cache, venv, access.tuple),
                .elem_index = access.elem_index,
            } },
            .list => |items| .{ .list = try self.specializeExprSpan(inst, mono_cache, venv, items) },
            .return_ => |ret_expr| .{ .return_ = try self.specializeExpr(inst, mono_cache, venv, ret_expr) },
            .runtime_error => |msg| .{ .runtime_error = msg },
            .for_ => |for_expr| .{ .for_ = try self.specializeForExpr(inst, mono_cache, venv, for_expr) },
        };

        return try self.output.addExpr(.{ .ty = lowered_ty, .data = data });
    }

    fn specializeVarExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        symbol: Symbol,
        instantiated_ty: TypeVarId,
        lowered_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        if (specializations.lookupFn(self.fenv, symbol)) |fn_entry| {
            return switch (lower_type.lambdaRepr(&self.input.types, instantiated_ty)) {
                .lset => blk: {
                    _ = try specializations.specializeFnLset(
                        &self.queue,
                        self.fenv,
                        &self.input.symbols,
                        &self.input.types,
                        &self.types,
                        symbol,
                        instantiated_ty,
                    );

                    switch (try lower_type.extractLsetFn(
                        &self.input.types,
                        &self.types,
                        mono_cache,
                        instantiated_ty,
                        symbol,
                        &self.input.symbols,
                    )) {
                        .toplevel => break :blk .{ .tag = .{
                            .name = lower_type.lambdaTagName(&self.input.symbols, symbol),
                            .args = ast.Span(ast.ExprId).empty(),
                        } },
                        .lset => |capture_info| {
                            const capture_bindings = try self.captureBindingsFromSolved(capture_info.captures);
                            defer self.allocator.free(capture_bindings);
                            const capture_record = try self.specializeCaptureRecord(mono_cache, capture_bindings, capture_info.ty);
                            const args = try self.allocator.alloc(ast.ExprId, 1);
                            defer self.allocator.free(args);
                            args[0] = capture_record;
                            break :blk .{ .tag = .{
                                .name = lower_type.lambdaTagName(&self.input.symbols, symbol),
                                .args = try self.output.addExprSpan(args),
                            } };
                        },
                    }
                },
                .erased => blk: {
                    const captures = try self.captureBindingsForErasedVar(inst, venv, fn_entry.fn_def.captures);
                    defer self.allocator.free(captures);
                    const specialized_symbol = try specializations.specializeFnErased(
                        &self.queue,
                        self.fenv,
                        &self.input.symbols,
                        &self.input.types,
                        &self.types,
                        symbol,
                        instantiated_ty,
                        captures,
                    );

                    const capture_expr = if (captures.len == 0)
                        null
                    else blk_capture: {
                        const capture_ty = try lower_type.lowerCaptureBindings(
                            &self.input.types,
                            &self.types,
                            mono_cache,
                            captures,
                            &self.input.symbols,
                        );
                        break :blk_capture try self.specializeCaptureRecord(mono_cache, captures, capture_ty);
                    };
                    break :blk .{ .packed_fn = .{
                        .lambda = specialized_symbol,
                        .captures = capture_expr,
                    } };
                },
            };
        }

        _ = lowered_ty;
        if (self.lookupEnv(venv, symbol) != null or self.isTopLevelValue(symbol)) {
            return .{ .var_ = symbol };
        }
        debugPanic("lambdamono.lower.specializeVarExpr unbound variable");
    }

    fn specializeCaptureRecord(
        self: *Lowerer,
        mono_cache: *lower_type.MonoCache,
        captures: []const lower_type.CaptureBinding,
        capture_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        const fields = try self.allocator.alloc(ast.FieldExpr, captures.len);
        defer self.allocator.free(fields);

        for (captures, 0..) |capture, i| {
            const lowered_capture_ty = try lower_type.lowerType(
                &self.input.types,
                &self.types,
                mono_cache,
                capture.ty,
                &self.input.symbols,
            );
            const capture_expr = try self.output.addExpr(.{
                .ty = lowered_capture_ty,
                .data = .{ .var_ = capture.symbol },
            });
            fields[i] = .{
                .name = self.input.symbols.get(capture.symbol).name,
                .value = capture_expr,
            };
        }

        return try self.output.addExpr(.{
            .ty = capture_ty,
            .data = .{ .record = try self.output.addFieldExprSpan(fields) },
        });
    }

    fn captureBindingsFromSolved(
        self: *Lowerer,
        captures: []const solved.Type.Capture,
    ) std.mem.Allocator.Error![]lower_type.CaptureBinding {
        const out = try self.allocator.alloc(lower_type.CaptureBinding, captures.len);
        for (captures, 0..) |capture, i| {
            out[i] = .{
                .symbol = capture.symbol,
                .ty = capture.ty,
            };
        }
        return out;
    }

    fn captureBindingsFromTypedSymbols(
        self: *Lowerer,
        inst: *InstScope,
        captures_span: solved.Ast.Span(solved.Ast.TypedSymbol),
    ) std.mem.Allocator.Error![]lower_type.CaptureBinding {
        const captures = self.input.store.sliceTypedSymbolSpan(captures_span);
        const out = try self.allocator.alloc(lower_type.CaptureBinding, captures.len);
        for (captures, 0..) |capture, i| {
            out[i] = .{
                .symbol = capture.symbol,
                .ty = try self.cloneInstType(inst, capture.ty),
            };
        }
        return out;
    }

    fn captureBindingsForErasedVar(
        self: *Lowerer,
        inst: *InstScope,
        venv: []const EnvEntry,
        captures_span: solved.Ast.Span(solved.Ast.TypedSymbol),
    ) std.mem.Allocator.Error![]lower_type.CaptureBinding {
        _ = inst;
        const captures = self.input.store.sliceTypedSymbolSpan(captures_span);
        const out = try self.allocator.alloc(lower_type.CaptureBinding, captures.len);
        for (captures, 0..) |capture, i| {
            out[i] = .{
                .symbol = capture.symbol,
                .ty = self.lookupEnv(venv, capture.symbol) orelse
                    debugPanic("lambdamono.lower.captureBindingsForErasedVar missing capture"),
            };
        }
        self.sortCaptureBindings(out);
        return out;
    }

    fn sortCaptureBindings(self: *Lowerer, captures: []lower_type.CaptureBinding) void {
        _ = self;
        std.mem.sortUnstable(lower_type.CaptureBinding, captures, {}, struct {
            fn lessThan(_: void, left: lower_type.CaptureBinding, right: lower_type.CaptureBinding) bool {
                return left.symbol.raw() < right.symbol.raw();
            }
        }.lessThan);
    }

    fn specializeCallExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        call: @FieldType(solved.Ast.Expr.Data, "call"),
        lowered_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.Expr.Data {
        const func_source = self.input.store.getExpr(call.func);
        const func_ty = try self.cloneInstType(inst, func_source.ty);
        const lowered_func = try self.specializeExpr(inst, mono_cache, venv, call.func);
        const lowered_arg = try self.specializeExpr(inst, mono_cache, venv, call.arg);

        return switch (lower_type.lambdaRepr(&self.input.types, func_ty)) {
            .erased => .{ .call_indirect = .{
                .func = lowered_func,
                .args = try self.output.addExprSpan(&.{lowered_arg}),
            } },
            .lset => |lambdas| blk: {
                const branches = try self.allocator.alloc(ast.Branch, lambdas.len);
                defer self.allocator.free(branches);

                for (lambdas, 0..) |lambda, i| {
                    const specialized = try specializations.specializeFnLset(
                        &self.queue,
                        self.fenv,
                        &self.input.symbols,
                        &self.input.types,
                        &self.types,
                        lambda.symbol,
                        func_ty,
                    );

                    const capture_repr = try lower_type.extractLsetFn(
                        &self.input.types,
                        &self.types,
                        mono_cache,
                        func_ty,
                        lambda.symbol,
                        &self.input.symbols,
                    );

                    branches[i] = switch (capture_repr) {
                        .toplevel => .{
                            .pat = try self.output.addPat(.{
                                .ty = self.output.getExpr(lowered_func).ty,
                                .data = .{ .tag = .{
                                    .name = lower_type.lambdaTagName(&self.input.symbols, lambda.symbol),
                                    .args = ast.Span(ast.PatId).empty(),
                                } },
                            }),
                            .body = try self.output.addExpr(.{
                                .ty = lowered_ty,
                                .data = .{ .call = .{
                                    .proc = specialized,
                                    .args = try self.output.addExprSpan(&.{lowered_arg}),
                                } },
                            }),
                        },
                        .lset => |capture_info| blk2: {
                            const capture_symbol = try self.input.symbols.add(base.Ident.Idx.NONE, .synthetic);
                            const capture_pat = try self.output.addPat(.{
                                .ty = capture_info.ty,
                                .data = .{ .var_ = capture_symbol },
                            });
                            const pat = try self.output.addPat(.{
                                .ty = self.output.getExpr(lowered_func).ty,
                                .data = .{ .tag = .{
                                    .name = lower_type.lambdaTagName(&self.input.symbols, lambda.symbol),
                                    .args = try self.output.addPatSpan(&.{capture_pat}),
                                } },
                            });
                            break :blk2 .{
                                .pat = pat,
                                .body = try self.output.addExpr(.{
                                    .ty = lowered_ty,
                                    .data = .{ .call = .{
                                        .proc = specialized,
                                        .args = try self.output.addExprSpan(&.{
                                            lowered_arg,
                                            try self.output.addExpr(.{
                                                .ty = capture_info.ty,
                                                .data = .{ .var_ = capture_symbol },
                                            }),
                                        }),
                                    } },
                                }),
                            };
                        },
                    };
                }

                break :blk .{ .when = .{
                    .cond = lowered_func,
                    .branches = try self.output.addBranchSpan(branches),
                } };
            },
        };
    }

    fn specializeBlockExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        incoming_env: []const EnvEntry,
        block: @FieldType(solved.Ast.Expr.Data, "block"),
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "block") {
        var env = try self.cloneEnv(incoming_env);
        defer self.allocator.free(env);

        const stmts = self.input.store.sliceStmtSpan(block.stmts);
        const out = try self.allocator.alloc(ast.StmtId, stmts.len);
        defer self.allocator.free(out);

        for (stmts, 0..) |stmt_id, i| {
            const result = try self.specializeStmt(inst, mono_cache, env, stmt_id);
            out[i] = result.stmt;
            self.allocator.free(env);
            env = result.env;
        }

        return .{
            .stmts = try self.output.addStmtSpan(out),
            .final_expr = try self.specializeExpr(inst, mono_cache, env, block.final_expr),
        };
    }

    fn specializeForExpr(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        for_expr: @FieldType(solved.Ast.Expr.Data, "for_"),
    ) std.mem.Allocator.Error!@FieldType(ast.Expr.Data, "for_") {
        const iterable = try self.specializeExpr(inst, mono_cache, venv, for_expr.iterable);
        const pat_result = try self.specializePat(inst, mono_cache, for_expr.patt);
        defer self.allocator.free(pat_result.additions);
        const body_env = try self.concatEnv(venv, pat_result.additions);
        defer self.allocator.free(body_env);
        return .{
            .patt = pat_result.pat,
            .iterable = iterable,
            .body = try self.specializeExpr(inst, mono_cache, body_env, for_expr.body),
        };
    }

    const PatResult = struct {
        pat: ast.PatId,
        additions: []EnvEntry,
    };

    fn specializePat(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        pat_id: solved.Ast.PatId,
    ) std.mem.Allocator.Error!PatResult {
        const pat = self.input.store.getPat(pat_id);
        const ty = try self.cloneInstType(inst, pat.ty);
        const lowered_ty = try lower_type.lowerType(&self.input.types, &self.types, mono_cache, ty, &self.input.symbols);

        return switch (pat.data) {
            .var_ => |symbol| .{
                .pat = try self.output.addPat(.{
                    .ty = lowered_ty,
                    .data = .{ .var_ = symbol },
                }),
                .additions = if (symbol.isNone())
                    try self.allocator.dupe(EnvEntry, &.{})
                else
                    try self.allocator.dupe(EnvEntry, &.{.{ .symbol = symbol, .ty = ty }}),
            },
            .tag => |tag| blk: {
                const source_args = self.input.store.slicePatSpan(tag.args);
                const lowered_args = try self.allocator.alloc(ast.PatId, source_args.len);
                defer self.allocator.free(lowered_args);

                var additions = std.ArrayList(EnvEntry).empty;
                defer additions.deinit(self.allocator);
                for (source_args, 0..) |arg_pat, i| {
                    const lowered = try self.specializePat(inst, mono_cache, arg_pat);
                    lowered_args[i] = lowered.pat;
                    try additions.appendSlice(self.allocator, lowered.additions);
                    self.allocator.free(lowered.additions);
                }

                break :blk .{
                    .pat = try self.output.addPat(.{
                        .ty = lowered_ty,
                        .data = .{ .tag = .{
                            .name = tag.name,
                            .args = try self.output.addPatSpan(lowered_args),
                        } },
                    }),
                    .additions = try additions.toOwnedSlice(self.allocator),
                };
            },
        };
    }

    const StmtResult = struct {
        stmt: ast.StmtId,
        env: []EnvEntry,
    };

    fn specializeStmt(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        stmt_id: solved.Ast.StmtId,
    ) std.mem.Allocator.Error!StmtResult {
        const stmt = self.input.store.getStmt(stmt_id);
        return switch (stmt) {
            .decl => |decl| blk: {
                const bind_ty = try self.cloneInstType(inst, decl.bind.ty);
                const env = try self.extendEnv(venv, .{ .symbol = decl.bind.symbol, .ty = bind_ty });
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .decl = .{
                            .bind = .{
                                .ty = try lower_type.lowerType(&self.input.types, &self.types, mono_cache, bind_ty, &self.input.symbols),
                                .symbol = decl.bind.symbol,
                            },
                            .body = try self.specializeExpr(inst, mono_cache, venv, decl.body),
                        },
                    }),
                    .env = env,
                };
            },
            .var_decl => |decl| blk: {
                const bind_ty = try self.cloneInstType(inst, decl.bind.ty);
                const env = try self.extendEnv(venv, .{ .symbol = decl.bind.symbol, .ty = bind_ty });
                break :blk .{
                    .stmt = try self.output.addStmt(.{
                        .var_decl = .{
                            .bind = .{
                                .ty = try lower_type.lowerType(&self.input.types, &self.types, mono_cache, bind_ty, &self.input.symbols),
                                .symbol = decl.bind.symbol,
                            },
                            .body = try self.specializeExpr(inst, mono_cache, venv, decl.body),
                        },
                    }),
                    .env = env,
                };
            },
            .reassign => |reassign| .{
                .stmt = try self.output.addStmt(.{
                    .reassign = .{
                        .target = reassign.target,
                        .body = try self.specializeExpr(inst, mono_cache, venv, reassign.body),
                    },
                }),
                .env = try self.cloneEnv(venv),
            },
            .expr => |expr| .{
                .stmt = try self.output.addStmt(.{ .expr = try self.specializeExpr(inst, mono_cache, venv, expr) }),
                .env = try self.cloneEnv(venv),
            },
            .expect => |expr| .{
                .stmt = try self.output.addStmt(.{ .expect = try self.specializeExpr(inst, mono_cache, venv, expr) }),
                .env = try self.cloneEnv(venv),
            },
            .crash => |msg| .{
                .stmt = try self.output.addStmt(.{ .crash = msg }),
                .env = try self.cloneEnv(venv),
            },
            .return_ => |expr| .{
                .stmt = try self.output.addStmt(.{ .return_ = try self.specializeExpr(inst, mono_cache, venv, expr) }),
                .env = try self.cloneEnv(venv),
            },
            .for_ => |for_stmt| blk: {
                const pat_result = try self.specializePat(inst, mono_cache, for_stmt.patt);
                defer self.allocator.free(pat_result.additions);
                const body_env = try self.concatEnv(venv, pat_result.additions);
                defer self.allocator.free(body_env);
                break :blk .{
                    .stmt = try self.output.addStmt(.{ .for_ = .{
                        .patt = pat_result.pat,
                        .iterable = try self.specializeExpr(inst, mono_cache, venv, for_stmt.iterable),
                        .body = try self.specializeExpr(inst, mono_cache, body_env, for_stmt.body),
                    } }),
                    .env = try self.cloneEnv(venv),
                };
            },
        };
    }

    fn specializeBranchSpan(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        span: solved.Ast.Span(solved.Ast.BranchId),
    ) std.mem.Allocator.Error!ast.Span(ast.BranchId) {
        const source = self.input.store.sliceBranchSpan(span);
        const out = try self.allocator.alloc(ast.Branch, source.len);
        defer self.allocator.free(out);

        for (source, 0..) |branch_id, i| {
            const branch = self.input.store.getBranch(branch_id);
            const pat_result = try self.specializePat(inst, mono_cache, branch.pat);
            defer self.allocator.free(pat_result.additions);
            const branch_env = try self.concatEnv(venv, pat_result.additions);
            defer self.allocator.free(branch_env);
            out[i] = .{
                .pat = pat_result.pat,
                .body = try self.specializeExpr(inst, mono_cache, branch_env, branch.body),
            };
        }

        return try self.output.addBranchSpan(out);
    }

    fn specializeExprSpan(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        span: solved.Ast.Span(solved.Ast.ExprId),
    ) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        const source = self.input.store.sliceExprSpan(span);
        const out = try self.allocator.alloc(ast.ExprId, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |expr_id, i| {
            out[i] = try self.specializeExpr(inst, mono_cache, venv, expr_id);
        }
        return try self.output.addExprSpan(out);
    }

    fn specializeFieldSpan(
        self: *Lowerer,
        inst: *InstScope,
        mono_cache: *lower_type.MonoCache,
        venv: []const EnvEntry,
        span: solved.Ast.Span(solved.Ast.FieldExpr),
    ) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        const source = self.input.store.sliceFieldExprSpan(span);
        const out = try self.allocator.alloc(ast.FieldExpr, source.len);
        defer self.allocator.free(out);
        for (source, 0..) |field, i| {
            out[i] = .{
                .name = field.name,
                .value = try self.specializeExpr(inst, mono_cache, venv, field.value),
            };
        }
        return try self.output.addFieldExprSpan(out);
    }

    fn cloneInstType(self: *Lowerer, inst: *InstScope, ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        const id = self.input.types.unlink(ty);
        if (inst.mapping.get(id)) |cached| return cached;

        const cloned = switch (self.input.types.getNode(id)) {
            .link => unreachable,
            .for_a => try self.input.types.freshUnbd(),
            .unbd => try self.input.types.freshUnbd(),
            .content => |content| blk: {
                const placeholder = try self.input.types.freshUnbd();
                try inst.mapping.put(id, placeholder);
                const node = switch (content) {
                    .primitive => solved.Type.Node{ .content = .{ .primitive = content.primitive } },
                    .func => solved.Type.Node{ .content = .{ .func = .{
                        .arg = try self.cloneInstType(inst, content.func.arg),
                        .lset = try self.cloneInstType(inst, content.func.lset),
                        .ret = try self.cloneInstType(inst, content.func.ret),
                    } } },
                    .list => |elem| solved.Type.Node{ .content = .{
                        .list = try self.cloneInstType(inst, elem),
                    } },
                    .tuple => |tuple| blk2: {
                        const elems = self.input.types.sliceTypeVarSpan(tuple);
                        const out = try self.allocator.alloc(TypeVarId, elems.len);
                        defer self.allocator.free(out);
                        for (elems, 0..) |elem, i| {
                            out[i] = try self.cloneInstType(inst, elem);
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .tuple = try self.input.types.addTypeVarSpan(out),
                        } };
                    },
                    .record => |record| blk2: {
                        const fields = self.input.types.sliceFields(record.fields);
                        const out = try self.allocator.alloc(solved.Type.Field, fields.len);
                        defer self.allocator.free(out);
                        for (fields, 0..) |field, i| {
                            out[i] = .{
                                .name = field.name,
                                .ty = try self.cloneInstType(inst, field.ty),
                            };
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .record = .{ .fields = try self.input.types.addFields(out) },
                        } };
                    },
                    .tag_union => |tag_union| blk2: {
                        const tags = self.input.types.sliceTags(tag_union.tags);
                        const out = try self.allocator.alloc(solved.Type.Tag, tags.len);
                        defer self.allocator.free(out);
                        for (tags, 0..) |tag, i| {
                            const args = self.input.types.sliceTypeVarSpan(tag.args);
                            const out_args = try self.allocator.alloc(TypeVarId, args.len);
                            defer self.allocator.free(out_args);
                            for (args, 0..) |arg, arg_i| {
                                out_args[arg_i] = try self.cloneInstType(inst, arg);
                            }
                            out[i] = .{
                                .name = tag.name,
                                .args = try self.input.types.addTypeVarSpan(out_args),
                            };
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .tag_union = .{ .tags = try self.input.types.addTags(out) },
                        } };
                    },
                    .lambda_set => |lambda_set| blk2: {
                        const lambdas = self.input.types.sliceLambdas(lambda_set);
                        const out = try self.allocator.alloc(solved.Type.Lambda, lambdas.len);
                        defer self.allocator.free(out);
                        for (lambdas, 0..) |lambda, i| {
                            const captures = self.input.types.sliceCaptures(lambda.captures);
                            const out_captures = try self.allocator.alloc(solved.Type.Capture, captures.len);
                            defer self.allocator.free(out_captures);
                            for (captures, 0..) |capture, capture_i| {
                                out_captures[capture_i] = .{
                                    .symbol = capture.symbol,
                                    .ty = try self.cloneInstType(inst, capture.ty),
                                };
                            }
                            out[i] = .{
                                .symbol = lambda.symbol,
                                .captures = try self.input.types.addCaptures(out_captures),
                            };
                        }
                        break :blk2 solved.Type.Node{ .content = .{
                            .lambda_set = try self.input.types.addLambdas(out),
                        } };
                    },
                };
                self.input.types.setNode(placeholder, node);
                break :blk placeholder;
            },
        };

        try inst.mapping.put(id, cloned);
        return cloned;
    }

    fn unify(self: *Lowerer, left: TypeVarId, right: TypeVarId) std.mem.Allocator.Error!void {
        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        try self.unifyRec(left, right, &visited);
    }

    fn unifyRec(
        self: *Lowerer,
        left: TypeVarId,
        right: TypeVarId,
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!void {
        const l = self.input.types.unlink(left);
        const r = self.input.types.unlink(right);
        if (l == r) return;

        const key = (@as(u64, @intFromEnum(l)) << 32) | @as(u64, @intFromEnum(r));
        if (visited.contains(key)) return;
        try visited.put(key, {});

        switch (self.input.types.getNode(l)) {
            .unbd => {
                self.input.types.setNode(l, .{ .link = r });
                return;
            },
            .for_a => debugPanic("lambdamono.lower.unify generalized type without instantiation"),
            else => {},
        }
        switch (self.input.types.getNode(r)) {
            .unbd => {
                self.input.types.setNode(r, .{ .link = l });
                return;
            },
            .for_a => debugPanic("lambdamono.lower.unify generalized type without instantiation"),
            else => {},
        }

        const left_node = self.input.types.getNode(l);
        const right_node = self.input.types.getNode(r);
        switch (left_node) {
            .content => |left_content| switch (right_node) {
                .content => |right_content| try self.unifyContent(left_content, right_content, visited),
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            else => debugPanic("lambdamono.lower.unify incompatible types"),
        }
    }

    fn unifyContent(
        self: *Lowerer,
        left: solved.Type.Content,
        right: solved.Type.Content,
        visited: *std.AutoHashMap(u64, void),
    ) std.mem.Allocator.Error!void {
        switch (left) {
            .primitive => |prim| switch (right) {
                .primitive => |other| {
                    if (prim != other) debugPanic("lambdamono.lower.unify incompatible primitives");
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .func => |func| switch (right) {
                .func => |other| {
                    try self.unifyRec(func.arg, other.arg, visited);
                    try self.unifyRec(func.lset, other.lset, visited);
                    try self.unifyRec(func.ret, other.ret, visited);
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .list => |elem| switch (right) {
                .list => |other| {
                    try self.unifyRec(elem, other, visited);
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .tuple => |tuple| switch (right) {
                .tuple => |other| {
                    const left_elems = self.input.types.sliceTypeVarSpan(tuple);
                    const right_elems = self.input.types.sliceTypeVarSpan(other);
                    if (left_elems.len != right_elems.len) debugPanic("lambdamono.lower.unify tuple arity mismatch");
                    for (left_elems, right_elems) |left_elem, right_elem| {
                        try self.unifyRec(left_elem, right_elem, visited);
                    }
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .record => |record| switch (right) {
                .record => |other| {
                    const left_fields = self.input.types.sliceFields(record.fields);
                    const right_fields = self.input.types.sliceFields(other.fields);
                    if (left_fields.len != right_fields.len) debugPanic("lambdamono.lower.unify record field mismatch");
                    for (left_fields, right_fields) |left_field, right_field| {
                        if (left_field.name != right_field.name) debugPanic("lambdamono.lower.unify record field mismatch");
                        try self.unifyRec(left_field.ty, right_field.ty, visited);
                    }
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .tag_union => |tag_union| switch (right) {
                .tag_union => |other| {
                    const left_tags = self.input.types.sliceTags(tag_union.tags);
                    const right_tags = self.input.types.sliceTags(other.tags);
                    if (left_tags.len != right_tags.len) debugPanic("lambdamono.lower.unify tag mismatch");
                    for (left_tags, right_tags) |left_tag, right_tag| {
                        if (left_tag.name != right_tag.name) debugPanic("lambdamono.lower.unify tag mismatch");
                        const left_args = self.input.types.sliceTypeVarSpan(left_tag.args);
                        const right_args = self.input.types.sliceTypeVarSpan(right_tag.args);
                        if (left_args.len != right_args.len) debugPanic("lambdamono.lower.unify tag arity mismatch");
                        for (left_args, right_args) |left_arg, right_arg| {
                            try self.unifyRec(left_arg, right_arg, visited);
                        }
                    }
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
            .lambda_set => |lambda_set| switch (right) {
                .lambda_set => |other| {
                    const left_lambdas = self.input.types.sliceLambdas(lambda_set);
                    const right_lambdas = self.input.types.sliceLambdas(other);
                    if (left_lambdas.len != right_lambdas.len) debugPanic("lambdamono.lower.unify lambda set mismatch");
                    for (left_lambdas) |left_lambda| {
                        var found = false;
                        for (right_lambdas) |right_lambda| {
                            if (left_lambda.symbol != right_lambda.symbol) continue;
                            found = true;
                            const left_caps = self.input.types.sliceCaptures(left_lambda.captures);
                            const right_caps = self.input.types.sliceCaptures(right_lambda.captures);
                            if (left_caps.len != right_caps.len) debugPanic("lambdamono.lower.unify capture mismatch");
                            for (left_caps) |left_cap| {
                                var capture_found = false;
                                for (right_caps) |right_cap| {
                                    if (left_cap.symbol != right_cap.symbol) continue;
                                    capture_found = true;
                                    try self.unifyRec(left_cap.ty, right_cap.ty, visited);
                                    break;
                                }
                                if (!capture_found) debugPanic("lambdamono.lower.unify capture mismatch");
                            }
                            break;
                        }
                        if (!found) debugPanic("lambdamono.lower.unify lambda set mismatch");
                    }
                },
                else => debugPanic("lambdamono.lower.unify incompatible types"),
            },
        }
    }

    fn lookupEnv(self: *const Lowerer, venv: []const EnvEntry, symbol: Symbol) ?TypeVarId {
        _ = self;
        for (venv) |entry| {
            if (entry.symbol == symbol) return entry.ty;
        }
        return null;
    }

    fn isTopLevelValue(self: *const Lowerer, symbol: Symbol) bool {
        for (self.input.store.defsSlice()) |def| {
            if (def.bind.symbol != symbol) continue;
            return switch (def.value) {
                .val, .run => true,
                .fn_ => false,
            };
        }
        return false;
    }

    fn cloneEnv(self: *Lowerer, env: []const EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        return try self.allocator.dupe(EnvEntry, env);
    }

    fn extendEnv(self: *Lowerer, env: []const EnvEntry, extra: EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, env.len + 1);
        std.mem.copyForwards(EnvEntry, out[0..env.len], env);
        out[env.len] = extra;
        return out;
    }

    fn concatEnv(self: *Lowerer, left: []const EnvEntry, right: []const EnvEntry) std.mem.Allocator.Error![]EnvEntry {
        const out = try self.allocator.alloc(EnvEntry, left.len + right.len);
        std.mem.copyForwards(EnvEntry, out[0..left.len], left);
        std.mem.copyForwards(EnvEntry, out[left.len..], right);
        return out;
    }
};

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

fn debugTodoLowLevel(op: base.LowLevel) noreturn {
    @branchHint(.cold);
    std.debug.panic("TODO lambdamono low-level op {s}", .{@tagName(op)});
}

test "lambdamono lower tests" {
    std.testing.refAllDecls(@This());
}

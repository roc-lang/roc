//! Closure lifting over Monotype IR.

const std = @import("std");

const Common = @import("../common.zig");
const Mono = @import("../monotype/ast.zig");
const MonoType = @import("../monotype/type.zig");
const Ast = @import("ast.zig");
const checked = @import("check").CheckedModule;
const names = @import("check").CheckedNames;

const Allocator = std.mem.Allocator;

/// Lift nested Monotype functions into explicit function bodies.
pub fn run(
    allocator: Allocator,
    mono: Mono.Program,
) Common.LowerError!Ast.Program {
    var owned = mono;
    errdefer owned.deinit();

    var types = owned.types;
    owned.types = @import("../monotype/type.zig").Store.init(allocator);
    var string_literals = owned.string_literals;
    owned.string_literals = .empty;
    var name_store = owned.names;
    owned.names = @import("check").CheckedNames.NameStore.init(allocator);
    var program = Ast.Program.init(allocator, name_store, types, string_literals, owned.next_symbol);
    name_store = undefined;
    types = undefined;
    string_literals = undefined;
    errdefer program.deinit();

    try program.locals.appendSlice(allocator, owned.locals.items);

    var lifter = Lifter.init(allocator, &owned, &program);
    defer lifter.deinit();

    try lifter.lowerDefsAndRoots();
    program.next_symbol = lifter.symbols.next;

    owned.deinit();
    return program;
}

const FnTemplateMap = std.HashMap(Mono.FnTemplate, Ast.FnId, FnTemplateContext, std.hash_map.default_max_load_percentage);
const FnTemplateActiveSet = std.HashMap(Mono.FnTemplate, void, FnTemplateContext, std.hash_map.default_max_load_percentage);

const FnTemplateContext = struct {
    types: *const MonoType.Store,
    names: *const names.NameStore,

    pub fn hash(self: FnTemplateContext, template: Mono.FnTemplate) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hashFnDef(&hasher, template.fn_def);
        hasher.update(&template.source_fn_key.bytes);
        const mono_digest = self.types.typeDigest(self.names, template.mono_fn_ty);
        hasher.update(&mono_digest.bytes);
        return hasher.final();
    }

    pub fn eql(self: FnTemplateContext, lhs: Mono.FnTemplate, rhs: Mono.FnTemplate) bool {
        const lhs_digest = self.types.typeDigest(self.names, lhs.mono_fn_ty);
        const rhs_digest = self.types.typeDigest(self.names, rhs.mono_fn_ty);
        return std.meta.eql(lhs.fn_def, rhs.fn_def) and
            std.mem.eql(u8, lhs.source_fn_key.bytes[0..], rhs.source_fn_key.bytes[0..]) and
            std.mem.eql(u8, lhs_digest.bytes[0..], rhs_digest.bytes[0..]);
    }

    fn hashFnDef(hasher: *std.hash.Wyhash, fn_def: Mono.FnDef) void {
        std.hash.autoHash(hasher, std.meta.activeTag(fn_def));
        switch (fn_def) {
            .local_template,
            .imported_template,
            .checked_generated,
            => |template| hashProcTemplate(hasher, template),
            .local_hosted,
            .imported_hosted,
            => |hosted| {
                hashProcTemplate(hasher, hosted.template);
                std.hash.autoHash(hasher, @intFromEnum(hosted.external_symbol_name));
                std.hash.autoHash(hasher, hosted.dispatch_index);
            },
            .nested => |nested| {
                hashProcTemplate(hasher, nested.owner);
                std.hash.autoHash(hasher, @intFromEnum(nested.site));
                hasher.update(&nested.context_fn_key.bytes);
            },
        }
    }

    fn hashProcTemplate(hasher: *std.hash.Wyhash, template: names.ProcTemplate) void {
        const module_digest = names.procTemplateModuleDigest(template);
        hasher.update(&module_digest.bytes);
        std.hash.autoHash(hasher, @intFromEnum(template.proc_base));
        std.hash.autoHash(hasher, @intFromEnum(template.template));
    }
};

const DefMap = []?Ast.FnId;
const NestedDefMap = []?Ast.FnId;

const MonoFnBody = struct {
    args: Mono.Span(Mono.TypedLocal),
    body: Mono.FnBody,
};

const FunctionMaps = struct {
    expr_map: std.AutoHashMap(Mono.ExprId, Ast.ExprId),
    pat_map: std.AutoHashMap(Mono.PatId, Ast.PatId),
    stmt_map: std.AutoHashMap(Mono.StmtId, Ast.StmtId),
};

const Lifter = struct {
    allocator: Allocator,
    input: *const Mono.Program,
    output: *Ast.Program,
    expr_map: std.AutoHashMap(Mono.ExprId, Ast.ExprId),
    pat_map: std.AutoHashMap(Mono.PatId, Ast.PatId),
    stmt_map: std.AutoHashMap(Mono.StmtId, Ast.StmtId),
    def_map: DefMap,
    nested_def_map: NestedDefMap,
    fn_templates: FnTemplateMap,
    fn_bodies: std.ArrayList(?MonoFnBody),
    nested_fn_ids: std.AutoHashMap(Ast.FnId, void),
    initialized_fns: std.AutoHashMap(Ast.FnId, void),
    symbols: Common.SymbolGen,

    fn init(allocator: Allocator, input: *const Mono.Program, output: *Ast.Program) Lifter {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .expr_map = std.AutoHashMap(Mono.ExprId, Ast.ExprId).init(allocator),
            .pat_map = std.AutoHashMap(Mono.PatId, Ast.PatId).init(allocator),
            .stmt_map = std.AutoHashMap(Mono.StmtId, Ast.StmtId).init(allocator),
            .def_map = &.{},
            .nested_def_map = &.{},
            .fn_templates = FnTemplateMap.initContext(allocator, .{
                .types = &output.types,
                .names = &output.names,
            }),
            .fn_bodies = .empty,
            .nested_fn_ids = std.AutoHashMap(Ast.FnId, void).init(allocator),
            .initialized_fns = std.AutoHashMap(Ast.FnId, void).init(allocator),
            .symbols = .{ .next = input.next_symbol },
        };
    }

    fn deinit(self: *Lifter) void {
        self.initialized_fns.deinit();
        self.nested_fn_ids.deinit();
        self.fn_bodies.deinit(self.allocator);
        self.fn_templates.deinit();
        if (self.nested_def_map.len > 0) self.allocator.free(self.nested_def_map);
        if (self.def_map.len > 0) self.allocator.free(self.def_map);
        self.stmt_map.deinit();
        self.pat_map.deinit();
        self.expr_map.deinit();
    }

    fn lowerDefsAndRoots(self: *Lifter) Allocator.Error!void {
        self.def_map = try self.allocator.alloc(?Ast.FnId, self.input.defs.items.len);
        @memset(self.def_map, null);

        for (self.input.defs.items, 0..) |def, index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.output.fns.items.len)));
            try self.output.fns.append(self.allocator, undefined);
            try self.fn_bodies.append(self.allocator, .{ .args = def.args, .body = def.body });
            self.def_map[index] = fn_id;
            if (def.fn_def) |source| try self.registerFnTemplate(source, fn_id);
        }

        self.nested_def_map = try self.allocator.alloc(?Ast.FnId, self.input.nested_defs.items.len);
        @memset(self.nested_def_map, null);
        for (self.input.nested_defs.items, 0..) |def, index| {
            const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.output.fns.items.len)));
            try self.output.fns.append(self.allocator, undefined);
            try self.fn_bodies.append(self.allocator, .{ .args = def.args, .body = .{ .roc = def.body } });
            self.nested_def_map[index] = fn_id;
            try self.nested_fn_ids.put(fn_id, {});
            try self.registerFnTemplate(def.fn_def, fn_id);
        }

        for (self.input.defs.items, 0..) |def, index| {
            try self.lowerTopLevelDef(self.def_map[index] orelse
                Common.invariant("Monotype definition was not reserved before lifting"), def);
        }

        for (self.input.nested_defs.items, 0..) |def, index| {
            try self.lowerNestedDef(self.nested_def_map[index] orelse
                Common.invariant("Monotype nested definition was not reserved before lifting"), def);
        }

        for (self.input.roots.items) |root| {
            const raw = @intFromEnum(root.def);
            if (raw >= self.def_map.len) Common.invariant("Monotype root references a missing definition");
            const fn_id = self.def_map[raw] orelse
                Common.invariant("Monotype root definition was not lifted");
            try self.output.roots.append(self.allocator, .{
                .fn_id = fn_id,
                .request = root.request,
            });
        }

        for (self.input.layout_requests.items) |request| {
            const fn_id = if (request.def) |def| blk: {
                const raw = @intFromEnum(def);
                if (raw >= self.def_map.len) Common.invariant("Monotype static data layout request references a missing definition");
                break :blk self.def_map[raw] orelse
                    Common.invariant("Monotype static data layout request definition was not lifted");
            } else null;
            try self.output.layout_requests.append(self.allocator, .{
                .checked_type = request.checked_type,
                .ty = request.ty,
                .fn_id = fn_id,
            });
        }
    }

    fn lowerTopLevelDef(self: *Lifter, fn_id: Ast.FnId, def: Mono.Def) Allocator.Error!void {
        var active = self.initFnTemplateActiveSet();
        defer active.deinit();
        var captures = CaptureSet.init(self, &active);
        defer captures.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();
        try bindTypedLocals(self.input, &bound, self.input.typedLocalSpan(def.args));
        switch (def.body) {
            .roc => |body| try captures.collectExpr(body, &bound),
            .hosted => {},
        }

        if (captures.items.items.len != 0) {
            Common.invariant("top-level Monotype definition has free locals after checked closure collection");
        }

        const saved_maps = self.pushFunctionMaps();
        defer self.popFunctionMaps(saved_maps);
        const body: Ast.FnBody = switch (def.body) {
            .roc => |body| .{ .roc = try self.lowerExpr(body) },
            .hosted => .hosted,
        };
        const args = try self.copyTypedLocalSpan(def.args);
        self.output.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = def.symbol,
            .source = def.fn_def,
            .args = args,
            .captures = .empty(),
            .body = body,
            .ret = def.ret,
        };
        try self.initialized_fns.put(fn_id, {});
    }

    fn lowerNestedDef(self: *Lifter, fn_id: Ast.FnId, def: Mono.NestedDef) Allocator.Error!void {
        var active = self.initFnTemplateActiveSet();
        defer active.deinit();
        var captures = CaptureSet.init(self, &active);
        defer captures.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();
        try bindTypedLocals(self.input, &bound, self.input.typedLocalSpan(def.args));
        try captures.collectExpr(def.body, &bound);

        const saved_maps = self.pushFunctionMaps();
        defer self.popFunctionMaps(saved_maps);
        const body = try self.lowerExpr(def.body);
        const args = try self.copyTypedLocalSpan(def.args);
        const capture_span = try self.output.addTypedLocalSpan(captures.items.items);
        self.output.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = def.symbol,
            .source = def.fn_def,
            .args = args,
            .captures = capture_span,
            .body = .{ .roc = body },
            .ret = def.ret,
        };
        try self.initialized_fns.put(fn_id, {});
    }

    fn pushFunctionMaps(self: *Lifter) FunctionMaps {
        const saved = FunctionMaps{
            .expr_map = self.expr_map,
            .pat_map = self.pat_map,
            .stmt_map = self.stmt_map,
        };
        self.expr_map = std.AutoHashMap(Mono.ExprId, Ast.ExprId).init(self.allocator);
        self.pat_map = std.AutoHashMap(Mono.PatId, Ast.PatId).init(self.allocator);
        self.stmt_map = std.AutoHashMap(Mono.StmtId, Ast.StmtId).init(self.allocator);
        return saved;
    }

    fn popFunctionMaps(self: *Lifter, saved: FunctionMaps) void {
        self.stmt_map.deinit();
        self.pat_map.deinit();
        self.expr_map.deinit();
        self.expr_map = saved.expr_map;
        self.pat_map = saved.pat_map;
        self.stmt_map = saved.stmt_map;
    }

    fn lowerExpr(self: *Lifter, expr_id: Mono.ExprId) Allocator.Error!Ast.ExprId {
        if (self.expr_map.get(expr_id)) |cached| return cached;

        const expr = self.input.exprs.items[@intFromEnum(expr_id)];
        const data: Ast.ExprData = switch (expr.data) {
            .local => |local| .{ .local = local },
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
            .lambda => |lambda| try self.liftLambda(expr.ty, lambda),
            .def_ref => |def_id| blk: {
                const raw = @intFromEnum(def_id);
                if (raw >= self.def_map.len) Common.invariant("Monotype definition reference was outside the definition table");
                break :blk .{ .fn_ref = self.def_map[raw] orelse
                    Common.invariant("Monotype definition reference reached lifting before its function was registered") };
            },
            .fn_def => |fn_def| .{ .fn_ref = self.fnIdForTemplate(fn_def) },
            .call_value => |call| .{ .call_value = .{
                .callee = try self.lowerExpr(call.callee),
                .args = try self.lowerExprSpan(call.args),
            } },
            .call_proc => |call| .{ .call_proc = .{
                .callee = self.fnIdForTemplate(call.callee),
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
                .params = try self.copyTypedLocalSpan(loop.params),
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

        const lowered = try self.output.addExpr(.{ .ty = expr.ty, .data = data });
        try self.expr_map.put(expr_id, lowered);
        return lowered;
    }

    fn liftLambda(self: *Lifter, ty: @import("../monotype/type.zig").TypeId, lambda: Mono.LambdaExpr) Allocator.Error!Ast.ExprData {
        const fn_id = try self.reserveFnTemplate(lambda.source);
        if (self.nested_fn_ids.contains(fn_id)) return .{ .fn_ref = fn_id };
        if (self.initialized_fns.contains(fn_id)) return .{ .fn_ref = fn_id };

        try self.setFnBody(fn_id, .{ .args = lambda.args, .body = .{ .roc = lambda.body } });
        var active = self.initFnTemplateActiveSet();
        defer active.deinit();
        var captures = CaptureSet.init(self, &active);
        defer captures.deinit();
        var bound = BoundSet.init(self.allocator);
        defer bound.deinit();
        try bindTypedLocals(self.input, &bound, self.input.typedLocalSpan(lambda.args));
        try captures.collectExpr(lambda.body, &bound);

        const saved_maps = self.pushFunctionMaps();
        defer self.popFunctionMaps(saved_maps);
        const body = try self.lowerExpr(lambda.body);
        const capture_span = try self.output.addTypedLocalSpan(captures.items.items);
        self.output.fns.items[@intFromEnum(fn_id)] = .{
            .symbol = self.symbols.fresh(),
            .source = lambda.source,
            .args = try self.copyTypedLocalSpan(lambda.args),
            .captures = capture_span,
            .body = .{ .roc = body },
            .ret = functionRet(&self.output.types, ty),
        };
        try self.initialized_fns.put(fn_id, {});
        return .{ .fn_ref = fn_id };
    }

    fn reserveFnTemplate(self: *Lifter, template: Mono.FnTemplate) Allocator.Error!Ast.FnId {
        if (self.fn_templates.get(template)) |existing| return existing;

        const fn_id: Ast.FnId = @enumFromInt(@as(u32, @intCast(self.output.fns.items.len)));
        try self.output.fns.append(self.allocator, undefined);
        try self.fn_bodies.append(self.allocator, null);
        try self.registerFnTemplate(template, fn_id);
        return fn_id;
    }

    fn setFnBody(self: *Lifter, fn_id: Ast.FnId, body: MonoFnBody) Allocator.Error!void {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.fn_bodies.items.len) Common.invariant("lifted function body id was outside body table");
        self.fn_bodies.items[raw] = body;
    }

    fn fnBodyForTemplate(self: *Lifter, template: Mono.FnTemplate) ?MonoFnBody {
        const fn_id = self.fnIdForTemplate(template);
        const raw = @intFromEnum(fn_id);
        if (raw >= self.fn_bodies.items.len) Common.invariant("function template body id was outside body table");
        return self.fn_bodies.items[raw];
    }

    fn initFnTemplateActiveSet(self: *Lifter) FnTemplateActiveSet {
        return FnTemplateActiveSet.initContext(self.allocator, .{
            .types = &self.output.types,
            .names = &self.output.names,
        });
    }

    fn registerFnTemplate(self: *Lifter, template: Mono.FnTemplate, fn_id: Ast.FnId) Allocator.Error!void {
        const result = try self.fn_templates.getOrPut(template);
        if (result.found_existing) {
            if (result.value_ptr.* != fn_id) {
                Common.invariant("Monotype function template was assigned two lifted function ids");
            }
            return;
        }
        result.value_ptr.* = fn_id;
    }

    fn fnIdForTemplate(self: *Lifter, template: Mono.FnTemplate) Ast.FnId {
        return self.fn_templates.get(template) orelse
            Common.invariant("Monotype function template reached lifting before its function was registered");
    }

    fn lowerPat(self: *Lifter, pat_id: Mono.PatId) Allocator.Error!Ast.PatId {
        if (self.pat_map.get(pat_id)) |cached| return cached;
        const pat = self.input.pats.items[@intFromEnum(pat_id)];
        const data: Ast.PatData = switch (pat.data) {
            .bind => |local| .{ .bind = local },
            .wildcard => .wildcard,
            .as => |as| .{ .as = .{
                .pattern = try self.lowerPat(as.pattern),
                .local = as.local,
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
        const lowered = try self.output.addPat(.{ .ty = pat.ty, .data = data });
        try self.pat_map.put(pat_id, lowered);
        return lowered;
    }

    fn lowerStmt(self: *Lifter, stmt_id: Mono.StmtId) Allocator.Error!Ast.StmtId {
        if (self.stmt_map.get(stmt_id)) |cached| return cached;
        const stmt = self.input.stmts.items[@intFromEnum(stmt_id)];
        const lowered_stmt: Ast.Stmt = switch (stmt) {
            .let_ => |let_| .{ .let_ = .{
                .pat = try self.lowerPat(let_.pat),
                .value = try self.lowerExpr(let_.value),
                .recursive = let_.recursive,
            } },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .dbg => |expr| .{ .dbg = try self.lowerExpr(expr) },
            .return_ => |expr| .{ .return_ = try self.lowerExpr(expr) },
            .crash => |msg| .{ .crash = msg },
        };
        const lowered = try self.output.addStmt(lowered_stmt);
        try self.stmt_map.put(stmt_id, lowered);
        return lowered;
    }

    fn lowerExprSpan(self: *Lifter, span: Mono.Span(Mono.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        const input_items = self.input.exprSpan(span);
        const lowered = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerExpr(item);
        return try self.output.addExprSpan(lowered);
    }

    fn lowerPatSpan(self: *Lifter, span: Mono.Span(Mono.PatId)) Allocator.Error!Ast.Span(Ast.PatId) {
        const input_items = self.input.patSpan(span);
        const lowered = try self.allocator.alloc(Ast.PatId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerPat(item);
        return try self.output.addPatSpan(lowered);
    }

    fn lowerStmtSpan(self: *Lifter, span: Mono.Span(Mono.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.input.stmtSpan(span);
        const lowered = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |item, i| lowered[i] = try self.lowerStmt(item);
        return try self.output.addStmtSpan(lowered);
    }

    fn copyTypedLocalSpan(self: *Lifter, span: Mono.Span(Mono.TypedLocal)) Allocator.Error!Ast.Span(Ast.TypedLocal) {
        return try self.output.addTypedLocalSpan(self.input.typedLocalSpan(span));
    }

    fn lowerFieldExprSpan(self: *Lifter, span: Mono.Span(Mono.FieldExpr)) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const input_items = self.input.fieldExprSpan(span);
        const lowered = try self.allocator.alloc(Ast.FieldExpr, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |field, i| {
            lowered[i] = .{
                .name = field.name,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addFieldExprSpan(lowered);
    }

    fn lowerRecordDestructSpan(self: *Lifter, span: Mono.Span(Mono.RecordDestruct)) Allocator.Error!Ast.Span(Ast.RecordDestruct) {
        const input_items = self.input.recordDestructSpan(span);
        const lowered = try self.allocator.alloc(Ast.RecordDestruct, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |field, i| {
            lowered[i] = .{
                .name = field.name,
                .pattern = try self.lowerPat(field.pattern),
            };
        }
        return try self.output.addRecordDestructSpan(lowered);
    }

    fn lowerBranchSpan(self: *Lifter, span: Mono.Span(Mono.Branch)) Allocator.Error!Ast.Span(Ast.Branch) {
        const input_items = self.input.branchSpan(span);
        const lowered = try self.allocator.alloc(Ast.Branch, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |branch, i| {
            lowered[i] = .{
                .pat = try self.lowerPat(branch.pat),
                .guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null,
                .body = try self.lowerExpr(branch.body),
            };
        }
        return try self.output.addBranchSpan(lowered);
    }

    fn lowerIfBranchSpan(self: *Lifter, span: Mono.Span(Mono.IfBranch)) Allocator.Error!Ast.Span(Ast.IfBranch) {
        const input_items = self.input.ifBranchSpan(span);
        const lowered = try self.allocator.alloc(Ast.IfBranch, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |branch, i| {
            lowered[i] = .{
                .cond = try self.lowerExpr(branch.cond),
                .body = try self.lowerExpr(branch.body),
            };
        }
        return try self.output.addIfBranchSpan(lowered);
    }
};

const BoundSet = struct {
    locals: std.AutoHashMap(Mono.LocalId, void),
    binders: std.AutoHashMap(checked.PatternBinderId, u32),

    fn init(allocator: Allocator) BoundSet {
        return .{
            .locals = std.AutoHashMap(Mono.LocalId, void).init(allocator),
            .binders = std.AutoHashMap(checked.PatternBinderId, u32).init(allocator),
        };
    }

    fn deinit(self: *BoundSet) void {
        self.binders.deinit();
        self.locals.deinit();
    }

    fn contains(self: *const BoundSet, input: *const Mono.Program, local: Mono.LocalId) bool {
        if (self.locals.contains(local)) return true;
        const binder = input.locals.items[@intFromEnum(local)].binder orelse return false;
        return self.binders.contains(binder);
    }

    fn put(self: *BoundSet, input: *const Mono.Program, local: Mono.LocalId) Allocator.Error!void {
        try self.locals.put(local, {});
        if (input.locals.items[@intFromEnum(local)].binder) |binder| try self.putBinder(binder);
    }

    fn remove(self: *BoundSet, input: *const Mono.Program, local: Mono.LocalId) void {
        _ = self.locals.remove(local);
        if (input.locals.items[@intFromEnum(local)].binder) |binder| self.removeBinder(binder);
    }

    fn putBinder(self: *BoundSet, binder: checked.PatternBinderId) Allocator.Error!void {
        const entry = try self.binders.getOrPut(binder);
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    fn removeBinder(self: *BoundSet, binder: checked.PatternBinderId) void {
        const count = self.binders.getPtr(binder) orelse
            Common.invariant("capture collection removed an unbound checked binder");
        if (count.* > 1) {
            count.* -= 1;
        } else {
            _ = self.binders.remove(binder);
        }
    }
};

const CaptureSet = struct {
    allocator: Allocator,
    lifter: *Lifter,
    items: std.ArrayList(Ast.TypedLocal),
    seen: std.AutoHashMap(Mono.LocalId, void),
    active: *FnTemplateActiveSet,

    fn init(lifter: *Lifter, active: *FnTemplateActiveSet) CaptureSet {
        return .{
            .allocator = lifter.allocator,
            .lifter = lifter,
            .items = .empty,
            .seen = std.AutoHashMap(Mono.LocalId, void).init(lifter.allocator),
            .active = active,
        };
    }

    fn deinit(self: *CaptureSet) void {
        self.seen.deinit();
        self.items.deinit(self.allocator);
    }

    fn addIfFree(self: *CaptureSet, local: Mono.LocalId, bound: *const BoundSet) Allocator.Error!void {
        if (bound.contains(self.lifter.input, local) or self.seen.contains(local)) return;
        const local_data = self.lifter.input.locals.items[@intFromEnum(local)];
        try self.seen.put(local, {});
        try self.items.append(self.allocator, .{
            .local = local,
            .ty = local_data.ty,
        });
    }

    fn collectExpr(self: *CaptureSet, expr_id: Mono.ExprId, bound: *BoundSet) Allocator.Error!void {
        const input = self.lifter.input;
        const expr = input.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .local => |local| try self.addIfFree(local, bound),
            .unit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .def_ref,
            .fn_def,
            .crash,
            => {},
            .list,
            .tuple,
            => |items| for (input.exprSpan(items)) |child| try self.collectExpr(child, bound),
            .record => |fields| for (input.fieldExprSpan(fields)) |field| try self.collectExpr(field.value, bound),
            .tag => |tag| for (input.exprSpan(tag.payloads)) |child| try self.collectExpr(child, bound),
            .nominal,
            .return_,
            .dbg,
            .expect,
            => |child| try self.collectExpr(child, bound),
            .let_ => |let_| {
                try self.collectExpr(let_.value, bound);
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindPat(self.allocator, input, let_.bind, bound, &added);
                try self.collectExpr(let_.rest, bound);
                removeBound(input, bound, added.items);
            },
            .lambda => |lambda| {
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, input, bound, input.typedLocalSpan(lambda.args), &added);
                try self.collectExpr(lambda.body, bound);
                removeBound(input, bound, added.items);
            },
            .call_value => |call| {
                try self.collectExpr(call.callee, bound);
                for (input.exprSpan(call.args)) |arg| try self.collectExpr(arg, bound);
            },
            .call_proc => |call| {
                try self.collectTemplateCaptures(call.callee, bound);
                for (input.exprSpan(call.args)) |arg| try self.collectExpr(arg, bound);
            },
            .low_level => |call| for (input.exprSpan(call.args)) |arg| try self.collectExpr(arg, bound),
            .field_access => |field| try self.collectExpr(field.receiver, bound),
            .tuple_access => |access| try self.collectExpr(access.tuple, bound),
            .structural_eq => |eq| {
                try self.collectExpr(eq.lhs, bound);
                try self.collectExpr(eq.rhs, bound);
            },
            .match_ => |match| {
                try self.collectExpr(match.scrutinee, bound);
                for (input.branchSpan(match.branches)) |branch| {
                    var added = std.ArrayList(Mono.LocalId).empty;
                    defer added.deinit(self.allocator);
                    try bindPat(self.allocator, input, branch.pat, bound, &added);
                    if (branch.guard) |guard| try self.collectExpr(guard, bound);
                    try self.collectExpr(branch.body, bound);
                    removeBound(input, bound, added.items);
                }
            },
            .if_ => |if_| {
                for (input.ifBranchSpan(if_.branches)) |branch| {
                    try self.collectExpr(branch.cond, bound);
                    try self.collectExpr(branch.body, bound);
                }
                try self.collectExpr(if_.final_else, bound);
            },
            .block => |block| {
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                for (input.stmtSpan(block.statements)) |stmt| try self.collectStmt(input, stmt, bound, &added);
                try self.collectExpr(block.final_expr, bound);
                removeBound(input, bound, added.items);
            },
            .loop_ => |loop| {
                for (input.exprSpan(loop.initial_values)) |initial| try self.collectExpr(initial, bound);
                var added = std.ArrayList(Mono.LocalId).empty;
                defer added.deinit(self.allocator);
                try bindTypedLocalsTracked(self.allocator, input, bound, input.typedLocalSpan(loop.params), &added);
                try self.collectExpr(loop.body, bound);
                removeBound(input, bound, added.items);
            },
            .break_ => |maybe| if (maybe) |value| try self.collectExpr(value, bound),
            .continue_ => |continue_| for (input.exprSpan(continue_.values)) |value| try self.collectExpr(value, bound),
        }
    }

    fn collectTemplateCaptures(self: *CaptureSet, template: Mono.FnTemplate, caller_bound: *BoundSet) Allocator.Error!void {
        const body = self.lifter.fnBodyForTemplate(template) orelse return;
        const entry = try self.active.getOrPut(template);
        if (entry.found_existing) return;
        defer _ = self.active.remove(template);

        var local_captures = CaptureSet.init(self.lifter, self.active);
        defer local_captures.deinit();

        var body_bound = BoundSet.init(self.allocator);
        defer body_bound.deinit();
        try bindTypedLocals(self.lifter.input, &body_bound, self.lifter.input.typedLocalSpan(body.args));
        switch (body.body) {
            .roc => |expr| try local_captures.collectExpr(expr, &body_bound),
            .hosted => {},
        }

        for (local_captures.items.items) |capture| {
            try self.addIfFree(capture.local, caller_bound);
        }
    }

    fn collectStmt(self: *CaptureSet, input: *const Mono.Program, stmt_id: Mono.StmtId, bound: *BoundSet, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
        switch (input.stmts.items[@intFromEnum(stmt_id)]) {
            .let_ => |let_| {
                if (let_.recursive) {
                    try bindPat(self.allocator, input, let_.pat, bound, added);
                    try self.collectExpr(let_.value, bound);
                } else {
                    try self.collectExpr(let_.value, bound);
                    try bindPat(self.allocator, input, let_.pat, bound, added);
                }
            },
            .expr,
            .expect,
            .dbg,
            .return_,
            => |expr| try self.collectExpr(expr, bound),
            .crash => {},
        }
    }
};

fn bindTypedLocals(input: *const Mono.Program, bound: *BoundSet, locals: []const Mono.TypedLocal) Allocator.Error!void {
    for (locals) |local| try bound.put(input, local.local);
}

fn bindTypedLocalsTracked(allocator: Allocator, input: *const Mono.Program, bound: *BoundSet, locals: []const Mono.TypedLocal, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    for (locals) |local| {
        try bound.put(input, local.local);
        try added.append(allocator, local.local);
    }
}

fn bindPat(allocator: Allocator, input: *const Mono.Program, pat_id: Mono.PatId, bound: *BoundSet, added: *std.ArrayList(Mono.LocalId)) Allocator.Error!void {
    switch (input.pats.items[@intFromEnum(pat_id)].data) {
        .bind => |local| {
            try bound.put(input, local);
            try added.append(allocator, local);
        },
        .wildcard,
        .int_lit,
        .dec_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .str_lit,
        => {},
        .as => |as| {
            try bindPat(allocator, input, as.pattern, bound, added);
            try bound.put(input, as.local);
            try added.append(allocator, as.local);
        },
        .record => |fields| for (input.recordDestructSpan(fields)) |field| try bindPat(allocator, input, field.pattern, bound, added),
        .tuple => |items| for (input.patSpan(items)) |child| try bindPat(allocator, input, child, bound, added),
        .tag => |tag| for (input.patSpan(tag.payloads)) |child| try bindPat(allocator, input, child, bound, added),
        .nominal => |backing| try bindPat(allocator, input, backing, bound, added),
    }
}

fn removeBound(input: *const Mono.Program, bound: *BoundSet, locals: []const Mono.LocalId) void {
    var index = locals.len;
    while (index > 0) {
        index -= 1;
        bound.remove(input, locals[index]);
    }
}

fn functionRet(types: *const MonoType.Store, ty: MonoType.TypeId) MonoType.TypeId {
    return switch (shapeContent(types, ty)) {
        .func => |fn_ty| fn_ty.ret,
        else => Common.invariant("lifted lambda expression did not have a function type"),
    };
}

fn shapeContent(types: *const MonoType.Store, ty: MonoType.TypeId) MonoType.Content {
    var current = ty;
    while (true) {
        switch (types.get(current)) {
            .named => |named| if (named.backing) |backing| {
                current = backing.ty;
                continue;
            } else {
                return types.get(current);
            },
            else => |content| return content,
        }
    }
}

test "monotype lifted lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}

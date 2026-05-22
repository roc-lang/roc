//! Checked modules to Monotype IR.

const std = @import("std");
const check = @import("check");
const can = @import("can");
const builtins = @import("builtins");

const Common = @import("../common.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const names = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;

pub fn run(
    allocator: Allocator,
    modules: Common.CheckedModules,
    roots: Common.RootRequests,
) Common.LowerError!Ast.Program {
    if (roots.requests.len == 0) {
        Common.invariant("Monotype lowering requires explicit roots");
    }

    var program = Ast.Program.init(allocator);
    errdefer program.deinit();

    var builder = Builder.init(allocator, modules, &program);
    defer builder.deinit();

    for (roots.requests) |request| {
        try builder.lowerRoot(request);
    }

    program.next_symbol = builder.symbols.next;
    return program;
}

const ModuleView = struct {
    key: checked.ModuleId,
    names: *const names.NameStore,
    types: checked.CheckedTypeStoreView,
    bodies: checked.CheckedBodyStoreView,
    templates: *const checked.CheckedProcedureTemplateTable,
    entry_wrappers: *const checked.EntryWrapperTable,
    intrinsic_wrappers: *const checked.IntrinsicWrapperTable,
    static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    method_registry: *const static_dispatch.MethodRegistry,
    resolved_refs: *const checked.ResolvedValueTable,
    nested_proc_sites: *const checked.NestedProcSiteTable,
    exported_procedure_bindings: checked.ExportedProcedureBindingView,
    exported_const_templates: checked.ExportedConstTemplateView,
    top_level_procedure_bindings: *const checked.TopLevelProcedureBindingTable,
    platform_required_bindings: *const checked.PlatformRequiredBindingTable,
    const_templates: *const checked.ConstTemplateTable,
    const_store: *const checked.ConstStore,
};

const MethodLookup = struct {
    view: ModuleView,
    target: static_dispatch.MethodTarget,
};

const BinderMap = std.AutoHashMap(checked.PatternBinderId, Ast.LocalId);
const ConstExprAddress = struct {
    store_module_bytes: [32]u8,
    type_module_bytes: [32]u8,
    node: u32,
    checked_ty: u32,
};

const Builder = struct {
    allocator: Allocator,
    modules: Common.CheckedModules,
    root_view: checked.ImportedModuleView,
    program: *Ast.Program,
    symbols: Common.SymbolGen = .{},
    type_cache: std.AutoHashMap(CheckedTypeAddress, Type.TypeId),
    lowered_templates: std.AutoHashMap(TemplateAddress, Ast.DefId),
    nested_site_cache: std.AutoHashMap(NestedSiteAddress, names.ProcSiteId),
    const_expr_cache: std.AutoHashMap(ConstExprAddress, Ast.ExprId),

    fn init(allocator: Allocator, modules: Common.CheckedModules, program: *Ast.Program) Builder {
        return .{
            .allocator = allocator,
            .modules = modules,
            .root_view = checked.importedView(modules.root.module),
            .program = program,
            .type_cache = std.AutoHashMap(CheckedTypeAddress, Type.TypeId).init(allocator),
            .lowered_templates = std.AutoHashMap(TemplateAddress, Ast.DefId).init(allocator),
            .nested_site_cache = std.AutoHashMap(NestedSiteAddress, names.ProcSiteId).init(allocator),
            .const_expr_cache = std.AutoHashMap(ConstExprAddress, Ast.ExprId).init(allocator),
        };
    }

    fn deinit(self: *Builder) void {
        self.const_expr_cache.deinit();
        self.nested_site_cache.deinit();
        self.lowered_templates.deinit();
        self.type_cache.deinit();
    }

    fn moduleName(self: *Builder, view: ModuleView, id: names.ModuleNameId) Allocator.Error!names.ModuleNameId {
        return self.program.names.internModuleName(view.names.moduleNameText(id));
    }

    fn typeName(self: *Builder, view: ModuleView, id: names.TypeNameId) Allocator.Error!names.TypeNameId {
        return self.program.names.internTypeName(view.names.typeNameText(id));
    }

    fn recordFieldName(self: *Builder, view: ModuleView, id: names.RecordFieldNameId) Allocator.Error!names.RecordFieldNameId {
        return self.program.names.internRecordFieldLabel(view.names.recordFieldLabelText(id));
    }

    fn tagName(self: *Builder, view: ModuleView, id: names.TagNameId) Allocator.Error!names.TagNameId {
        return self.program.names.internTagLabel(view.names.tagLabelText(id));
    }

    fn typeDef(self: *Builder, view: ModuleView, module_name: names.ModuleNameId, type_name: names.TypeNameId) Allocator.Error!Type.TypeDef {
        return .{
            .module_name = try self.moduleName(view, module_name),
            .type_name = try self.typeName(view, type_name),
        };
    }

    fn lowerRoot(self: *Builder, request: checked.RootRequest) Allocator.Error!void {
        const template = request.procedure_template orelse
            Common.invariant("root request reached Monotype without a checked procedure template");
        const def = try self.lowerTemplate(template, moduleView(self.root_view), request.checked_type);
        try self.program.roots.append(self.allocator, .{ .def = def, .request = request });
    }

    fn lowerTemplate(
        self: *Builder,
        template_ref: names.ProcTemplate,
        source_ty_view: ModuleView,
        source_fn_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.DefId {
        const address = TemplateAddress.from(template_ref, source_ty_view.key, source_fn_ty);
        if (self.lowered_templates.get(address)) |existing| return existing;

        const view = self.moduleForDigest(names.procTemplateModuleDigest(template_ref));
        const template = view.templates.get(template_ref.template);
        const fn_ty = try self.lowerType(source_ty_view, source_fn_ty);
        const symbol = self.symbols.fresh();

        const reserved: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        try self.program.defs.append(self.allocator, undefined);
        try self.lowered_templates.put(address, reserved);

        var body_ctx = try BodyContext.init(self.allocator, self, view, template_ref);
        defer body_ctx.deinit();

        const lowered = try body_ctx.lowerTemplateBody(template_ref, template, fn_ty);
        self.program.defs.items[@intFromEnum(reserved)] = .{
            .symbol = symbol,
            .fn_def = fnDefForTemplate(view, template_ref, source_fn_ty),
            .args = lowered.args,
            .body = lowered.body,
            .ret = lowered.ret,
        };
        return reserved;
    }

    fn lowerType(self: *Builder, view: ModuleView, checked_ty: checked.CheckedTypeId) Allocator.Error!Type.TypeId {
        const address = CheckedTypeAddress{
            .module_bytes = view.key.bytes,
            .ty = @intFromEnum(checked_ty),
        };
        if (self.type_cache.get(address)) |cached| return cached;

        const raw = @intFromEnum(checked_ty);
        if (raw >= view.types.payloads.len) Common.invariant("checked type id outside checked type store");

        const reserved = try self.program.types.add(.zst);
        try self.type_cache.put(address, reserved);
        const lowered = try self.lowerTypePayload(view, checked_ty, view.types.payloads[raw]);
        self.program.types.types.items[@intFromEnum(reserved)] = lowered;
        return reserved;
    }

    fn lowerTypePayload(self: *Builder, view: ModuleView, checked_ty: checked.CheckedTypeId, payload: checked.CheckedTypePayload) Allocator.Error!Type.Content {
        return switch (payload) {
            .pending,
            .flex,
            .rigid,
            => Common.invariant("open checked type reached Monotype lowering"),
            .empty_record => .{ .record = .empty() },
            .empty_tag_union => .{ .tag_union = .empty() },
            .record_unbound => |fields| try self.lowerRecord(view, fields),
            .record => |record| try self.lowerRecord(view, record.fields),
            .tuple => |items| blk: {
                const lowered = try self.lowerTypeSlice(view, items);
                defer self.allocator.free(lowered);
                break :blk .{ .tuple = try self.program.types.addSpan(lowered) };
            },
            .tag_union => |tag_union| try self.lowerTagUnion(view, tag_union.tags),
            .function => |fn_ty| blk: {
                const args = try self.lowerTypeSlice(view, fn_ty.args);
                defer self.allocator.free(args);
                break :blk .{ .func = .{
                    .args = try self.program.types.addSpan(args),
                    .ret = try self.lowerType(view, fn_ty.ret),
                } };
            },
            .alias => |alias| blk: {
                const args = try self.lowerTypeSlice(view, alias.args);
                defer self.allocator.free(args);
                break :blk .{ .named = .{
                    .named_type = .{ .module = moduleDigestFromId(view.key), .ty = checked_ty },
                    .def = try self.typeDef(view, alias.origin_module, alias.name),
                    .kind = .alias,
                    .args = try self.program.types.addSpan(args),
                    .backing = .{
                        .ty = try self.lowerType(view, alias.backing),
                        .use = .inspectable,
                    },
                } };
            },
            .nominal => |nominal| blk: {
                switch (nominal.representation) {
                    .builtin => |builtin| switch (checked.builtinRuntimeEncoding(builtin)) {
                        .primitive => |primitive| break :blk .{ .primitive = primitive },
                        .bool_tag_union, .list, .box => {},
                    },
                    else => {},
                }

                const args = try self.lowerTypeSlice(view, nominal.args);
                defer self.allocator.free(args);
                const backing_use: Type.BackingUse = if (nominal.is_opaque) .runtime_layout_only else .inspectable;
                break :blk .{ .named = .{
                    .named_type = .{ .module = moduleDigestFromId(view.key), .ty = checked_ty },
                    .def = try self.typeDef(view, nominal.origin_module, nominal.name),
                    .kind = if (nominal.is_opaque) .@"opaque" else .nominal,
                    .builtin_owner = builtinOwner(nominal.builtin),
                    .args = try self.program.types.addSpan(args),
                    .backing = switch (nominal.representation) {
                        .opaque_without_backing => null,
                        else => .{
                            .ty = try self.lowerType(view, nominal.backing),
                            .use = backing_use,
                        },
                    },
                } };
            },
        };
    }

    fn lowerRecord(self: *Builder, view: ModuleView, fields: []const checked.CheckedRecordField) Allocator.Error!Type.Content {
        const lowered = try self.allocator.alloc(Type.Field, fields.len);
        defer self.allocator.free(lowered);
        for (fields, 0..) |field, i| {
            lowered[i] = .{
                .name = try self.recordFieldName(view, field.name),
                .ty = try self.lowerType(view, field.ty),
            };
        }
        return .{ .record = try self.program.types.addFields(lowered) };
    }

    fn lowerTagUnion(self: *Builder, view: ModuleView, tags: []const checked.CheckedTag) Allocator.Error!Type.Content {
        const lowered = try self.allocator.alloc(Type.Tag, tags.len);
        defer self.allocator.free(lowered);
        for (tags, 0..) |tag, i| {
            const payloads = try self.lowerTypeSlice(view, tag.args);
            defer self.allocator.free(payloads);
            lowered[i] = .{
                .name = try self.tagName(view, tag.name),
                .payloads = try self.program.types.addSpan(payloads),
            };
        }
        return .{ .tag_union = try self.program.types.addTags(lowered) };
    }

    fn lowerTypeSlice(self: *Builder, view: ModuleView, checked_tys: []const checked.CheckedTypeId) Allocator.Error![]Type.TypeId {
        const out = try self.allocator.alloc(Type.TypeId, checked_tys.len);
        errdefer self.allocator.free(out);
        for (checked_tys, 0..) |ty, i| {
            out[i] = try self.lowerType(view, ty);
        }
        return out;
    }

    fn moduleForDigest(self: *Builder, module_digest: names.CheckedModuleDigest) ModuleView {
        if (moduleBytesEqual(module_digest.bytes, self.root_view.key.bytes)) return moduleView(self.root_view);
        for (self.modules.imports) |imported| {
            if (moduleBytesEqual(module_digest.bytes, imported.key.bytes)) return moduleView(imported);
        }
        for (self.modules.root.relation_modules) |relation| {
            if (moduleBytesEqual(module_digest.bytes, relation.key.bytes)) return moduleView(relation);
        }
        Common.invariant("procedure template referenced a checked module that is not in the lowering input");
    }

    fn moduleForId(self: *Builder, module_id: checked.ModuleId) ModuleView {
        if (moduleBytesEqual(module_id.bytes, self.root_view.key.bytes)) return moduleView(self.root_view);
        for (self.modules.imports) |imported| {
            if (moduleBytesEqual(module_id.bytes, imported.key.bytes)) return moduleView(imported);
        }
        for (self.modules.root.relation_modules) |relation| {
            if (moduleBytesEqual(module_id.bytes, relation.key.bytes)) return moduleView(relation);
        }
        Common.invariant("procedure binding referenced a checked module that is not in the lowering input");
    }

    fn lookupMethodTarget(
        self: *Builder,
        owner: static_dispatch.MethodOwner,
        method_view: ModuleView,
        method: names.MethodNameId,
    ) ?MethodLookup {
        var found: ?MethodLookup = null;
        self.lookupMethodTargetInView(moduleView(self.root_view), owner, method_view, method, &found);
        for (self.modules.imports) |imported| {
            self.lookupMethodTargetInView(moduleView(imported), owner, method_view, method, &found);
        }
        for (self.modules.root.relation_modules) |relation| {
            self.lookupMethodTargetInView(moduleView(relation), owner, method_view, method, &found);
        }
        return found;
    }

    fn lookupMethodTargetInView(
        self: *Builder,
        view: ModuleView,
        owner: static_dispatch.MethodOwner,
        method_view: ModuleView,
        method: names.MethodNameId,
        found: *?MethodLookup,
    ) void {
        const view_owner = self.methodOwnerForView(owner, view) orelse return;
        const view_method = view.names.lookupMethodName(method_view.names.methodNameText(method)) orelse return;
        const target = view.method_registry.lookup(.{ .owner = view_owner, .method = view_method }) orelse return;
        if (found.* != null) Common.invariant("checked method registries contain duplicate dispatch targets");
        found.* = .{ .view = view, .target = target };
    }

    fn methodOwnerForView(self: *Builder, owner: static_dispatch.MethodOwner, view: ModuleView) ?static_dispatch.MethodOwner {
        return switch (owner) {
            .builtin => |builtin| .{ .builtin = builtin },
            .nominal => |nominal| blk: {
                const module_name = view.names.lookupModuleName(self.program.names.moduleNameText(nominal.module_name)) orelse return null;
                const type_name = view.names.lookupTypeName(self.program.names.typeNameText(nominal.type_name)) orelse return null;
                break :blk .{ .nominal = .{ .module_name = module_name, .type_name = type_name } };
            },
        };
    }

    fn fnDefForProcedureUse(self: *Builder, source_ty_view: ModuleView, proc: checked.ProcedureUseTemplate) Allocator.Error!Ast.FnTemplate {
        const source_fn_ty = proc.source_fn_ty_payload orelse
            Common.invariant("checked procedure use reached Monotype without a requested function type");
        const fn_template = switch (proc.binding) {
            .top_level => |top_level| blk: {
                const view = self.moduleForId(checked.topLevelProcedureModuleId(top_level));
                const binding = view.top_level_procedure_bindings.get(top_level.binding);
                break :blk fnDefForProcedureBindingBody(view, binding.body, source_fn_ty);
            },
            .imported => |imported| blk: {
                const view = self.moduleForId(checked.importedProcedureModuleId(imported));
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and binding.binding.pattern == imported.pattern) {
                        break :blk fnDefForImportedBindingBody(view, binding.body, source_fn_ty);
                    }
                }
                Common.invariant("imported procedure binding was not exported by its checked module");
            },
            .hosted => |hosted| blk: {
                break :blk fnDefForTemplate(self.moduleForId(moduleIdFromDigest(names.procTemplateModuleDigest(hosted.template))), hosted.template, source_fn_ty);
            },
            .platform_required => |required| blk: {
                const platform_view = self.moduleForId(checked.requiredProcedureModuleId(required));
                const binding = platform_view.platform_required_bindings.lookupByBindingId(@intFromEnum(required.procedure_binding)) orelse
                    Common.invariant("platform required procedure binding id is missing");
                break :blk switch (binding.value_use) {
                    .procedure_value => |procedure| try self.fnDefForProcedureUse(source_ty_view, procedure.procedure),
                    .const_value => Common.invariant("platform required procedure ref resolved to a const binding"),
                };
            },
        };
        try self.lowerFnTemplateDef(source_ty_view, fn_template);
        return fn_template;
    }

    fn lowerFnTemplateDef(self: *Builder, source_ty_view: ModuleView, fn_template: Ast.FnTemplate) Allocator.Error!void {
        switch (fn_template.fn_def) {
            .local_template,
            .imported_template,
            .local_hosted,
            .imported_hosted,
            .checked_generated,
            => |template| _ = try self.lowerTemplate(template, source_ty_view, fn_template.source_fn_ty),
            .nested => {},
        }
    }

    fn nestedFnForExpr(
        self: *Builder,
        view: ModuleView,
        owner: names.ProcTemplate,
        expr_id: checked.CheckedExprId,
    ) Allocator.Error!Ast.NestedFn {
        const address = NestedSiteAddress.from(view.key, owner, expr_id);
        const site = if (self.nested_site_cache.get(address)) |cached|
            cached
        else blk: {
            for (view.nested_proc_sites.sites) |candidate| {
                if (candidate.checked_expr == null or candidate.checked_expr.? != expr_id) continue;
                if (!names.procedureTemplateRefEql(candidate.owner_template, owner)) continue;
                try self.nested_site_cache.put(address, candidate.site);
                break :blk candidate.site;
            }
            Common.invariant("nested function expression reached Monotype without a checked nested function site");
        };
        return .{
            .owner = owner,
            .site = site,
        };
    }

    fn fnTemplateForNestedExpr(
        self: *Builder,
        view: ModuleView,
        owner: names.ProcTemplate,
        expr_id: checked.CheckedExprId,
        checked_fn_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.FnTemplate {
        return .{
            .fn_def = .{ .nested = try self.nestedFnForExpr(view, owner, expr_id) },
            .source_fn_ty = checked_fn_ty,
        };
    }

    fn moduleForConstFnDef(self: *Builder, fn_def: anytype) ModuleView {
        return switch (fn_def) {
            .nested => |nested| self.moduleForDigest(names.procTemplateModuleDigest(nested.owner)),
            .local_template,
            .imported_template,
            .local_hosted,
            .imported_hosted,
            .checked_generated,
            => |template| self.moduleForDigest(names.procTemplateModuleDigest(template)),
        };
    }
};

const LoweredTemplateBody = struct {
    args: Ast.Span(Ast.TypedLocal),
    body: Ast.ExprId,
    ret: Type.TypeId,
};

const BodyContext = struct {
    allocator: Allocator,
    builder: *Builder,
    view: ModuleView,
    owner_template: names.ProcTemplate,
    binders: BinderMap,
    string_literals: []?Ast.StringLiteralId,

    fn init(
        allocator: Allocator,
        builder: *Builder,
        view: ModuleView,
        owner_template: names.ProcTemplate,
    ) Allocator.Error!BodyContext {
        const string_literals = try allocator.alloc(?Ast.StringLiteralId, view.bodies.string_literals.len);
        errdefer allocator.free(string_literals);
        @memset(string_literals, null);
        return .{
            .allocator = allocator,
            .builder = builder,
            .view = view,
            .owner_template = owner_template,
            .binders = BinderMap.init(allocator),
            .string_literals = string_literals,
        };
    }

    fn deinit(self: *BodyContext) void {
        self.allocator.free(self.string_literals);
        self.binders.deinit();
    }

    fn lowerTemplateBody(
        self: *BodyContext,
        template_ref: names.ProcTemplate,
        template: checked.CheckedProcedureTemplate,
        fn_ty: Type.TypeId,
    ) Allocator.Error!LoweredTemplateBody {
        if (!names.procedureTemplateRefEql(self.owner_template, template_ref)) {
            Common.invariant("Monotype body context owner did not match lowered checked template");
        }
        const fn_content = self.builder.program.types.get(fn_ty);
        const ret_ty = switch (fn_content) {
            .func => |fn_data| fn_data.ret,
            else => Common.invariant("checked procedure template root type was not a function"),
        };

        switch (template.body) {
            .checked_body => |body_id| {
                const body = self.view.bodies.bodies[@intFromEnum(body_id)];
                const root = self.view.bodies.exprs[@intFromEnum(body.root_expr)];
                return switch (root.data) {
                    .lambda => |lambda| try self.lowerLambdaTemplate(lambda, ret_ty),
                    .closure => |closure| blk: {
                        if (closure.captures.len != 0) {
                            Common.invariant("checked procedure template root closure had captures");
                        }
                        const lambda_expr = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
                        break :blk switch (lambda_expr.data) {
                            .lambda => |lambda| try self.lowerLambdaTemplate(lambda, ret_ty),
                            else => Common.invariant("checked procedure template root closure did not point at a lambda"),
                        };
                    },
                    .hosted_lambda => Common.invariant("hosted lambda template must lower through hosted metadata, not source lambda body"),
                    else => .{
                        .args = .empty(),
                        .body = try self.lowerExpr(body.root_expr),
                        .ret = ret_ty,
                    },
                };
            },
            .entry_wrapper => |wrapper_id| {
                const wrapper = self.view.entry_wrappers.get(wrapper_id);
                return .{
                    .args = .empty(),
                    .body = try self.lowerExpr(wrapper.body_expr),
                    .ret = ret_ty,
                };
            },
            .intrinsic_wrapper => |wrapper_id| {
                const wrapper = self.view.intrinsic_wrappers.get(wrapper_id);
                return switch (wrapper.intrinsic) {
                    .str_inspect => try self.lowerStrInspectIntrinsic(fn_ty, ret_ty),
                    .structural_eq => Common.invariant("structural equality intrinsic wrapper must lower through checked dispatch plans"),
                };
            },
        }
    }

    fn lowerStrInspectIntrinsic(self: *BodyContext, fn_ty: Type.TypeId, ret_ty: Type.TypeId) Allocator.Error!LoweredTemplateBody {
        const fn_content = self.builder.program.types.get(fn_ty);
        const fn_data = switch (fn_content) {
            .func => |data| data,
            else => Common.invariant("Str.inspect intrinsic had a non-function type"),
        };
        const arg_tys = self.builder.program.types.span(fn_data.args);
        if (arg_tys.len != 1) Common.invariant("Str.inspect intrinsic requires exactly one argument");

        const arg_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_tys[0]);
        const typed_arg = Ast.TypedLocal{ .local = arg_local, .ty = arg_tys[0] };
        const local_expr = try self.builder.program.addExpr(.{ .ty = arg_tys[0], .data = .{ .local = arg_local } });
        const args = [_]Ast.ExprId{local_expr};
        const arg_span = try self.builder.program.addExprSpan(&args);
        const body = try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .low_level = .{
                .op = strInspectLowLevelOp(self.builder.program.types.get(arg_tys[0])),
                .args = arg_span,
            } },
        });
        return .{
            .args = try self.builder.program.addTypedLocalSpan(&.{typed_arg}),
            .body = body,
            .ret = ret_ty,
        };
    }

    fn lowerLambdaTemplate(self: *BodyContext, lambda: anytype, ret_ty: Type.TypeId) Allocator.Error!LoweredTemplateBody {
        const args = try self.allocator.alloc(Ast.TypedLocal, lambda.args.len);
        defer self.allocator.free(args);
        for (lambda.args, 0..) |pattern_id, i| {
            const pat_ty = try self.builder.lowerType(self.view, self.view.bodies.patterns[@intFromEnum(pattern_id)].ty);
            const pat = try self.lowerPattern(pattern_id);
            const local = switch (self.builder.program.pats.items[@intFromEnum(pat)].data) {
                .bind => |local| local,
                else => Common.invariant("function argument pattern was not lowered to a direct binding"),
            };
            args[i] = .{ .local = local, .ty = pat_ty };
        }
        return .{
            .args = try self.builder.program.addTypedLocalSpan(args),
            .body = try self.lowerExpr(lambda.body),
            .ret = ret_ty,
        };
    }

    fn lowerExpr(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        const ty = try self.builder.lowerType(self.view, expr.ty);
        const data: Ast.ExprData = switch (expr.data) {
            .pending,
            .ellipsis,
            .anno_only,
            .runtime_error,
            => Common.invariant("non-runtime checked expression reached Monotype lowering"),
            .num => |num| .{ .int_lit = num.value },
            .typed_int => |num| .{ .int_lit = num.value },
            .frac_f32 => |frac| .{ .frac_f32_lit = frac.value },
            .frac_f64 => |frac| .{ .frac_f64_lit = frac.value },
            .dec => |dec| .{ .dec_lit = dec.value },
            .dec_small => Common.invariant("small decimal literal reached Monotype after numeric finalization"),
            .typed_frac => Common.invariant("typed fractional integer literal reached Monotype after numeric finalization"),
            .str_segment => |str| .{ .str_lit = try self.lowerStringLiteral(str) },
            .bytes_literal => |str| .{ .str_lit = try self.lowerStringLiteral(str) },
            .empty_list => .{ .list = .empty() },
            .empty_record => .{ .record = .empty() },
            .str => |segments| try self.lowerStr(segments),
            .lookup_local => |lookup| return try self.lowerLookupExpr(expr.ty, lookup.resolved),
            .lookup_external => |resolved| return try self.lowerLookupExpr(expr.ty, resolved),
            .lookup_required => |resolved| return try self.lowerLookupExpr(expr.ty, resolved),
            .list => |items| .{ .list = try self.lowerExprSpan(items) },
            .tuple => |items| .{ .tuple = try self.lowerExprSpan(items) },
            .record => |record| try self.lowerRecordExpr(record),
            .tag => |tag| .{ .tag = .{ .name = try self.builder.tagName(self.view, tag.name), .payloads = try self.lowerExprSpan(tag.args) } },
            .zero_argument_tag => |tag| .{ .tag = .{ .name = try self.builder.tagName(self.view, tag.name), .payloads = .empty() } },
            .nominal => |nominal| .{ .nominal = try self.lowerExpr(nominal.backing_expr) },
            .closure => |closure| try self.lowerClosure(expr_id, closure),
            .lambda => |lambda| try self.lowerLambdaExpr(
                lambda,
                try self.builder.fnTemplateForNestedExpr(self.view, self.owner_template, expr_id, expr.ty),
            ),
            .call => |call| .{ .call_value = .{
                .callee = try self.lowerExpr(call.func),
                .args = try self.lowerExprSpan(call.args),
            } },
            .dispatch_call => |plan| try self.lowerDispatch(plan),
            .type_dispatch_call => |plan| try self.lowerDispatch(plan),
            .method_eq => |plan| try self.lowerDispatch(plan),
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
                .negated = eq.negated,
            } },
            .field_access => |field| .{ .field_access = .{
                .receiver = try self.lowerExpr(field.receiver),
                .field = try self.builder.recordFieldName(self.view, field.field_name),
            } },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .match_ => |match| try self.lowerMatch(match),
            .if_ => |if_| try self.lowerIf(if_),
            .block => |block| try self.lowerBlock(block),
            .binop,
            .unary_minus,
            .unary_not,
            => Common.invariant("desugared operator expression reached Monotype without checked dispatch or low-level form"),
            .crash => |msg| .{ .crash = try self.lowerStringLiteral(msg) },
            .dbg => |child| .{ .dbg = try self.lowerExpr(child) },
            .expect => |child| .{ .expect = try self.lowerExpr(child) },
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.expr) },
            .for_ => |for_| try self.lowerIteratorFor(for_),
            .hosted_lambda => Common.invariant("hosted lambda expression reached ordinary Monotype expression lowering"),
            .run_low_level => |low_level| .{ .low_level = .{ .op = low_level.op, .args = try self.lowerExprSpan(low_level.args) } },
        };
        return try self.builder.program.addExpr(.{ .ty = ty, .data = data });
    }

    fn lowerStr(self: *BodyContext, segments: []const checked.CheckedExprId) Allocator.Error!Ast.ExprData {
        if (segments.len != 1) Common.invariant("interpolated strings must be desugared before Monotype lowering");
        return .{ .nominal = try self.lowerExpr(segments[0]) };
    }

    fn lowerStringLiteral(self: *BodyContext, id: checked.CheckedStringLiteralId) Allocator.Error!Ast.StringLiteralId {
        const index = @intFromEnum(id);
        if (index >= self.view.bodies.string_literals.len) {
            Common.invariant("checked string literal id outside checked body string store");
        }
        if (self.string_literals[index]) |existing| return existing;
        const lowered = try self.builder.program.addStringLiteral(self.view.bodies.string_literals[index]);
        self.string_literals[index] = lowered;
        return lowered;
    }

    fn lowerLookupExpr(self: *BodyContext, checked_ty: checked.CheckedTypeId, maybe_ref: ?checked.ResolvedValueId) Allocator.Error!Ast.ExprId {
        const ref_id = maybe_ref orelse Common.invariant("checked lookup reached Monotype without resolved value ref");
        const record = self.view.resolved_refs.records[@intFromEnum(ref_id)];
        const ty = try self.builder.lowerType(self.view, checked_ty);
        const data: Ast.ExprData = switch (record.ref) {
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            => |local| .{ .local = self.binders.get(local.binder) orelse Common.invariant("local lookup referenced an unbound pattern binder") },
            .local_proc => Common.invariant("local procedure lookup must be represented by a checked nested procedure template"),
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .promoted_top_level_proc,
            => |proc| .{ .fn_def = try self.builder.fnDefForProcedureUse(self.view, proc) },
            .platform_required_proc => |proc| .{ .fn_def = try self.builder.fnDefForProcedureUse(self.view, proc.procedure) },
            .top_level_const => |const_use| return try self.restoreConstUse(const_use, checked_ty),
            .imported_const => |const_use| return try self.restoreConstUse(const_use, checked_ty),
            .platform_required_const => |required| return try self.restoreConstUse(required.const_use, checked_ty),
            .platform_required_declaration => Common.invariant("platform required declaration reached Monotype without a binding"),
        };
        return try self.builder.program.addExpr(.{ .ty = ty, .data = data });
    }

    fn restoreConstUse(self: *BodyContext, const_use: checked.ConstUseTemplate, checked_ty: checked.CheckedTypeId) Allocator.Error!Ast.ExprId {
        const requested_ty = const_use.requested_source_ty_payload orelse
            Common.invariant("checked const use reached Monotype without a requested checked type");
        if (requested_ty != checked_ty) {
            Common.invariant("checked const use requested type differs from lookup expression type");
        }

        const store_view = self.builder.moduleForId(checked.constModuleId(const_use.const_ref));
        const template = store_view.const_templates.get(const_use.const_ref);
        const node = switch (template.state) {
            .stored_const => |stored| stored.node,
            .reserved => Common.invariant("reserved checked const template reached Monotype"),
            .eval_template => Common.invariant("checked const eval template reached Monotype before ConstStore publication"),
        };
        return try self.restoreConstNode(store_view, self.view, node, requested_ty);
    }

    fn restoreConstNode(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        node: checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.ExprId {
        const address = ConstExprAddress{
            .store_module_bytes = store_view.key.bytes,
            .type_module_bytes = type_view.key.bytes,
            .node = @intFromEnum(node),
            .checked_ty = @intFromEnum(checked_ty),
        };
        if (self.builder.const_expr_cache.get(address)) |existing| return existing;

        const ty = try self.builder.lowerType(type_view, checked_ty);
        const value = store_view.const_store.get(node);
        switch (value) {
            .fn_value => |fn_id| {
                const expr = try self.restoreConstFn(store_view, type_view, fn_id, checked_ty, ty);
                try self.builder.const_expr_cache.put(address, expr);
                return expr;
            },
            else => {},
        }
        const data = try self.restoreConstData(store_view, type_view, value, checked_ty, ty);
        const expr = try self.builder.program.addExpr(.{ .ty = ty, .data = data });
        try self.builder.const_expr_cache.put(address, expr);
        return expr;
    }

    fn restoreConstData(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        value: checked.ConstValue,
        checked_ty: checked.CheckedTypeId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprData {
        _ = ty;
        return switch (value) {
            .pending => Common.invariant("pending ConstStore node reached Monotype restore"),
            .zst => .unit,
            .scalar => |scalar| restoreScalar(scalar),
            .str => |bytes| .{ .str_lit = try self.builder.program.addStringLiteral(bytes) },
            .list => |items| .{ .list = try self.restoreConstList(store_view, type_view, checked_ty, items) },
            .box => |payload| .{ .nominal = try self.restoreConstNode(store_view, type_view, payload, checkedBoxPayloadType(type_view, checked_ty)) },
            .tuple => |items| .{ .tuple = try self.restoreConstTuple(store_view, type_view, checked_ty, items) },
            .record => |items| .{ .record = try self.restoreConstRecord(store_view, type_view, checked_ty, items) },
            .tag => |tag| .{ .tag = .{
                .name = try self.builder.tagName(store_view, tag.tag_name),
                .payloads = try self.restoreConstTagPayloads(store_view, type_view, checked_ty, tag),
            } },
            .nominal => |nominal| .{ .nominal = try self.restoreConstNode(store_view, type_view, nominal.backing, checkedNominalBackingType(type_view, checked_ty, nominal.named_type)) },
            .fn_value => Common.invariant("ConstStore function value must be restored as an expression"),
        };
    }

    fn restoreConstList(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        checked_ty: checked.CheckedTypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const elem_ty = checkedListElemType(type_view, checked_ty);
        const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            lowered[index] = try self.restoreConstNode(store_view, type_view, item, elem_ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn restoreConstTuple(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        checked_ty: checked.CheckedTypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const item_tys = checkedTupleItems(type_view, checked_ty);
        if (item_tys.len != items.len) Common.invariant("ConstStore tuple length differs from checked type");
        const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            lowered[index] = try self.restoreConstNode(store_view, type_view, item, item_tys[index]);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn restoreConstRecord(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        checked_ty: checked.CheckedTypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const fields = checkedRecordFields(type_view, checked_ty);
        if (fields.len != items.len) Common.invariant("ConstStore record length differs from checked type");
        const lowered = try self.allocator.alloc(Ast.FieldExpr, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            lowered[index] = .{
                .name = try self.builder.recordFieldName(type_view, fields[index].name),
                .value = try self.restoreConstNode(store_view, type_view, item, fields[index].ty),
            };
        }
        return try self.builder.program.addFieldExprSpan(lowered);
    }

    fn restoreConstTagPayloads(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        checked_ty: checked.CheckedTypeId,
        tag: anytype,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const payload_tys = checkedTagPayloadTypes(type_view, checked_ty, store_view, tag.tag_name);
        if (payload_tys.len != tag.payloads.len) Common.invariant("ConstStore tag payload count differs from checked type");
        const lowered = try self.allocator.alloc(Ast.ExprId, tag.payloads.len);
        defer self.allocator.free(lowered);
        for (tag.payloads, 0..) |payload, index| {
            lowered[index] = try self.restoreConstNode(store_view, type_view, payload, payload_tys[index]);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn restoreConstFn(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        fn_id: checked.ConstFnId,
        checked_ty: checked.CheckedTypeId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        _ = checked_ty;
        const raw = @intFromEnum(fn_id);
        if (raw >= store_view.const_store.fns.items.len) Common.invariant("ConstStore function id is out of range");
        const fn_value = store_view.const_store.fns.items[raw];
        const template = constFnTemplateToMono(fn_value);
        if (fn_value.captures.len == 0) {
            try self.builder.lowerFnTemplateDef(type_view, template);
            return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .fn_def = template } });
        }

        const fn_view = self.builder.moduleForConstFnDef(fn_value.fn_def);
        var fn_ctx = try BodyContext.init(self.allocator, self.builder, fn_view, ownerTemplateForConstFnDef(fn_value.fn_def));
        defer fn_ctx.deinit();

        const lambda_expr_id = checkedLambdaExprIdForConstFn(fn_view, fn_value.fn_def);
        const lambda_expr = fn_view.bodies.exprs[@intFromEnum(lambda_expr_id)];

        const captures = try self.allocator.alloc(struct {
            binder: checked.PatternBinderId,
            previous: ?Ast.LocalId,
            value: Ast.ExprId,
            pat: Ast.PatId,
        }, fn_value.captures.len);
        var initialized: usize = 0;
        errdefer {
            while (initialized > 0) {
                initialized -= 1;
                if (captures[initialized].previous) |previous| {
                    fn_ctx.binders.put(captures[initialized].binder, previous) catch {};
                } else {
                    _ = fn_ctx.binders.remove(captures[initialized].binder);
                }
            }
            self.allocator.free(captures);
        }
        defer self.allocator.free(captures);

        for (fn_value.captures, 0..) |capture, index| {
            const capture_ty = checkedBinderType(fn_view, capture.binder);
            const lowered_ty = try self.builder.lowerType(fn_view, capture_ty);
            const value_expr = try fn_ctx.restoreConstNode(store_view, fn_view, capture.value, capture_ty);
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), lowered_ty, capture.binder);
            const pat = try self.builder.program.addPat(.{ .ty = lowered_ty, .data = .{ .bind = local } });
            const previous = fn_ctx.binders.get(capture.binder);
            try fn_ctx.binders.put(capture.binder, local);
            captures[index] = .{
                .binder = capture.binder,
                .previous = previous,
                .value = value_expr,
                .pat = pat,
            };
            initialized += 1;
        }
        defer {
            var index = initialized;
            while (index > 0) {
                index -= 1;
                if (captures[index].previous) |previous| {
                    fn_ctx.binders.put(captures[index].binder, previous) catch {};
                } else {
                    _ = fn_ctx.binders.remove(captures[index].binder);
                }
            }
        }

        var expr = try self.builder.program.addExpr(.{
            .ty = ty,
            .data = switch (lambda_expr.data) {
                .lambda => |lambda| try fn_ctx.lowerLambdaExpr(lambda, template),
                else => Common.invariant("stored capturing function did not reference a checked lambda"),
            },
        });
        var index = captures.len;
        while (index > 0) {
            index -= 1;
            expr = try self.builder.program.addExpr(.{
                .ty = ty,
                .data = .{ .let_ = .{
                    .bind = captures[index].pat,
                    .value = captures[index].value,
                    .rest = expr,
                } },
            });
        }
        return expr;
    }

    fn lowerExprSpan(self: *BodyContext, checked_exprs: []const checked.CheckedExprId) Allocator.Error!Ast.Span(Ast.ExprId) {
        const lowered = try self.allocator.alloc(Ast.ExprId, checked_exprs.len);
        defer self.allocator.free(lowered);
        for (checked_exprs, 0..) |child, i| {
            lowered[i] = try self.lowerExpr(child);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn lowerRecordExpr(self: *BodyContext, record: anytype) Allocator.Error!Ast.ExprData {
        if (record.ext != null) Common.invariant("record extension reached Monotype after row closure");
        const lowered = try self.allocator.alloc(Ast.FieldExpr, record.fields.len);
        defer self.allocator.free(lowered);
        for (record.fields, 0..) |field, i| {
            lowered[i] = .{
                .name = try self.builder.recordFieldName(self.view, field.label),
                .value = try self.lowerExpr(field.value),
            };
        }
        return .{ .record = try self.builder.program.addFieldExprSpan(lowered) };
    }

    fn lowerClosure(self: *BodyContext, expr_id: checked.CheckedExprId, closure: anytype) Allocator.Error!Ast.ExprData {
        const lambda = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
        const nested = try self.builder.fnTemplateForNestedExpr(self.view, self.owner_template, expr_id, lambda.ty);
        const data = switch (lambda.data) {
            .lambda => |lambda_data| try self.lowerLambdaExpr(lambda_data, nested),
            else => Common.invariant("checked closure did not point at a lambda expression"),
        };
        const lambda_expr = try self.builder.program.addExpr(.{
            .ty = try self.builder.lowerType(self.view, lambda.ty),
            .data = data,
        });
        return .{ .nominal = lambda_expr };
    }

    fn lowerLambdaExpr(self: *BodyContext, lambda: anytype, nested: Ast.FnTemplate) Allocator.Error!Ast.ExprData {
        const args = try self.allocator.alloc(Ast.TypedLocal, lambda.args.len);
        defer self.allocator.free(args);
        for (lambda.args, 0..) |pattern_id, i| {
            const pat_ty = try self.builder.lowerType(self.view, self.view.bodies.patterns[@intFromEnum(pattern_id)].ty);
            const pat = try self.lowerPattern(pattern_id);
            const local = switch (self.builder.program.pats.items[@intFromEnum(pat)].data) {
                .bind => |local| local,
                else => Common.invariant("lambda argument pattern was not lowered to a direct binding"),
            };
            args[i] = .{ .local = local, .ty = pat_ty };
        }
        switch (nested.fn_def) {
            .nested => {},
            else => Common.invariant("expression-position lambda was not assigned a nested function identity"),
        }
        return .{ .lambda = .{
            .args = try self.builder.program.addTypedLocalSpan(args),
            .body = try self.lowerExpr(lambda.body),
            .source = nested,
        } };
    }

    fn lowerDispatch(self: *BodyContext, maybe_plan: ?static_dispatch.StaticDispatchPlanId) Allocator.Error!Ast.ExprData {
        const plan_id = maybe_plan orelse Common.invariant("checked dispatch expression reached Monotype without a dispatch plan");
        const plan = self.view.static_dispatch_plans.plans[@intFromEnum(plan_id)];
        const dispatcher_ty = try self.builder.lowerType(self.view, plan.dispatcher_ty);
        const owner = methodOwnerFromType(&self.builder.program.types, dispatcher_ty) orelse {
            switch (plan.result_mode) {
                .equality => |eq| if (eq.structural_allowed) {
                    if (plan.args.len != 2) Common.invariant("structural equality dispatch plan must have two operands");
                    return .{ .structural_eq = .{
                        .lhs = try self.lowerExpr(plan.args[0]),
                        .rhs = try self.lowerExpr(plan.args[1]),
                        .negated = eq.negated,
                    } };
                },
                .value => {},
            }
            Common.invariant("dispatch plan had no method owner and no structural equality permission");
        };

        const lookup = self.builder.lookupMethodTarget(owner, self.view, plan.method) orelse
            Common.invariant("checked method registry is missing resolved dispatch target");
        const template = lookup.target.template orelse
            Common.invariant("checked dispatch target was not backed by a procedure template");
        _ = try self.builder.lowerTemplate(template, self.view, plan.callable_ty);

        const args = try self.lowerExprSpan(plan.args);
        return .{ .call_proc = .{
            .callee = fnDefForTemplate(lookup.view, template, plan.callable_ty),
            .args = args,
        } };
    }

    fn lowerMatch(self: *BodyContext, match: anytype) Allocator.Error!Ast.ExprData {
        const branches = try self.allocator.alloc(Ast.Branch, branchCount(match.branches));
        defer self.allocator.free(branches);
        var index: usize = 0;
        for (match.branches) |branch| {
            const body = try self.lowerExpr(branch.value);
            const guard = if (branch.guard) |guard_expr| try self.lowerExpr(guard_expr) else null;
            for (branch.patterns) |pattern| {
                branches[index] = .{
                    .pat = try self.lowerPattern(pattern.pattern),
                    .guard = guard,
                    .body = body,
                };
                index += 1;
            }
        }
        return .{ .match_ = .{
            .scrutinee = try self.lowerExpr(match.cond),
            .branches = try self.builder.program.addBranchSpan(branches),
        } };
    }

    fn lowerIf(self: *BodyContext, if_: anytype) Allocator.Error!Ast.ExprData {
        const branches = try self.allocator.alloc(Ast.IfBranch, if_.branches.len);
        defer self.allocator.free(branches);
        for (if_.branches, 0..) |branch, i| {
            branches[i] = .{
                .cond = try self.lowerExpr(branch.cond),
                .body = try self.lowerExpr(branch.body),
            };
        }
        return .{ .if_ = .{
            .branches = try self.builder.program.addIfBranchSpan(branches),
            .final_else = try self.lowerExpr(if_.final_else),
        } };
    }

    fn lowerBlock(self: *BodyContext, block: anytype) Allocator.Error!Ast.ExprData {
        const stmts = try self.allocator.alloc(Ast.StmtId, block.statements.len);
        defer self.allocator.free(stmts);
        for (block.statements, 0..) |statement, i| {
            stmts[i] = try self.lowerStatement(statement);
        }
        return .{ .block = .{
            .statements = try self.builder.program.addStmtSpan(stmts),
            .final_expr = try self.lowerExpr(block.final_expr),
        } };
    }

    fn lowerIteratorFor(self: *BodyContext, for_: anytype) Allocator.Error!Ast.ExprData {
        _ = self;
        _ = for_;
        Common.invariant("iterator for must be lowered by the checked iterator dispatch plan before Monotype expression publication");
    }

    fn lowerStatement(self: *BodyContext, statement_id: checked.CheckedStatementId) Allocator.Error!Ast.StmtId {
        const statement = self.view.bodies.statements[@intFromEnum(statement_id)];
        const stmt: Ast.Stmt = switch (statement.data) {
            .pending,
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            .runtime_error,
            => Common.invariant("non-runtime checked statement reached Monotype lowering"),
            .decl => |decl| .{ .let_ = .{
                .pat = try self.lowerPattern(decl.pattern),
                .value = try self.lowerExpr(decl.expr),
            } },
            .var_ => |decl| .{ .let_ = .{
                .pat = try self.lowerPattern(decl.pattern),
                .value = try self.lowerExpr(decl.expr),
            } },
            .reassign => Common.invariant("mutable reassignment must lower to explicit loop/state operations before Monotype publication"),
            .crash => |msg| .{ .crash = try self.lowerStringLiteral(msg) },
            .dbg => |child| .{ .dbg = try self.lowerExpr(child) },
            .expr => |child| .{ .expr = try self.lowerExpr(child) },
            .expect => |child| .{ .expect = try self.lowerExpr(child) },
            .for_ => Common.invariant("iterator for statement must lower to ordinary loop before Monotype publication"),
            .while_ => Common.invariant("while statement must lower to ordinary loop before Monotype publication"),
            .break_ => Common.invariant("break statement reached Monotype outside an explicit loop expression"),
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.expr) },
        };
        return try self.builder.program.addStmt(stmt);
    }

    fn lowerPattern(self: *BodyContext, pattern_id: checked.CheckedPatternId) Allocator.Error!Ast.PatId {
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
        const ty = try self.builder.lowerType(self.view, pattern.ty);
        const data: Ast.PatData = switch (pattern.data) {
            .pending,
            .runtime_error,
            => Common.invariant("non-runtime checked pattern reached Monotype lowering"),
            .assign => |binder| blk: {
                const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), ty, binder);
                try self.binders.put(binder, local);
                break :blk .{ .bind = local };
            },
            .as => |as| blk: {
                const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), ty, as.binder);
                try self.binders.put(as.binder, local);
                break :blk .{ .as = .{
                    .pattern = try self.lowerPattern(as.pattern),
                    .local = local,
                } };
            },
            .applied_tag => |tag| .{ .tag = .{
                .name = try self.builder.tagName(self.view, tag.name),
                .payloads = try self.lowerPatternSpan(tag.args),
            } },
            .nominal => |nominal| .{ .nominal = try self.lowerPattern(nominal.backing_pattern) },
            .record_destructure => |destructs| try self.lowerRecordPattern(destructs),
            .list => Common.invariant("list pattern must be lowered to explicit list operations before Monotype publication"),
            .tuple => |items| .{ .tuple = try self.lowerPatternSpan(items) },
            .num_literal => |num| .{ .int_lit = num.value },
            .small_dec_literal => Common.invariant("small decimal pattern reached Monotype after numeric finalization"),
            .dec_literal => |dec| .{ .dec_lit = dec.value },
            .frac_f32_literal => |value| .{ .frac_f32_lit = value },
            .frac_f64_literal => |value| .{ .frac_f64_lit = value },
            .str_literal => |str| .{ .str_lit = try self.lowerStringLiteral(str) },
            .underscore => .wildcard,
        };
        return try self.builder.program.addPat(.{ .ty = ty, .data = data });
    }

    fn lowerPatternSpan(self: *BodyContext, checked_patterns: []const checked.CheckedPatternId) Allocator.Error!Ast.Span(Ast.PatId) {
        const lowered = try self.allocator.alloc(Ast.PatId, checked_patterns.len);
        defer self.allocator.free(lowered);
        for (checked_patterns, 0..) |child, i| {
            lowered[i] = try self.lowerPattern(child);
        }
        return try self.builder.program.addPatSpan(lowered);
    }

    fn lowerRecordPattern(self: *BodyContext, destructs: []const checked.CheckedRecordDestruct) Allocator.Error!Ast.PatData {
        const lowered = try self.allocator.alloc(Ast.RecordDestruct, destructs.len);
        defer self.allocator.free(lowered);
        for (destructs, 0..) |destruct, i| {
            const child = switch (destruct.kind) {
                .required => |pattern| pattern,
                .sub_pattern => |pattern| pattern,
                .rest => |pattern| pattern,
            };
            lowered[i] = .{
                .name = try self.builder.recordFieldName(self.view, destruct.label),
                .pattern = try self.lowerPattern(child),
            };
        }
        return .{ .record = try self.builder.program.addRecordDestructSpan(lowered) };
    }
};

fn moduleView(view: checked.ImportedModuleView) ModuleView {
    return .{
        .key = view.key,
        .names = checked.importedNames(view),
        .types = view.checked_types,
        .bodies = view.checked_bodies,
        .templates = view.checked_procedure_templates,
        .entry_wrappers = view.entry_wrappers,
        .intrinsic_wrappers = view.intrinsic_wrappers,
        .static_dispatch_plans = view.static_dispatch_plans,
        .method_registry = view.method_registry,
        .resolved_refs = view.resolved_value_refs,
        .nested_proc_sites = view.nested_proc_sites,
        .exported_procedure_bindings = view.exported_procedure_bindings,
        .exported_const_templates = view.exported_const_templates,
        .top_level_procedure_bindings = view.top_level_procedure_bindings,
        .platform_required_bindings = view.platform_required_bindings,
        .const_templates = view.const_templates,
        .const_store = view.const_store,
    };
}

fn fnDefForTemplate(view: ModuleView, template: names.ProcTemplate, source_fn_ty: checked.CheckedTypeId) Ast.FnTemplate {
    if (moduleBytesEqual(view.key.bytes, names.procTemplateModuleDigest(template).bytes)) {
        return .{
            .fn_def = .{ .local_template = template },
            .source_fn_ty = source_fn_ty,
        };
    }
    return .{
        .fn_def = .{ .imported_template = template },
        .source_fn_ty = source_fn_ty,
    };
}

fn fnDefForProcedureBindingBody(view: ModuleView, body: checked.ProcedureBindingBody, source_fn_ty: checked.CheckedTypeId) Ast.FnTemplate {
    return switch (body) {
        .direct_template => |direct| fnDefForCallableTemplate(view, direct.template, source_fn_ty),
        .callable_eval_template => Common.invariant("callable eval template must be restored through ConstStore before Monotype lowering"),
    };
}

fn fnDefForImportedBindingBody(view: ModuleView, body: checked.ImportedProcedureBindingBody, source_fn_ty: checked.CheckedTypeId) Ast.FnTemplate {
    return switch (body) {
        .direct_template => |direct| fnDefForCallableTemplate(view, direct.template, source_fn_ty),
        .callable_eval_template => Common.invariant("imported callable eval template must be restored through ConstStore before Monotype lowering"),
    };
}

fn fnDefForCallableTemplate(view: ModuleView, template: names.CallableProcTemplate, source_fn_ty: checked.CheckedTypeId) Ast.FnTemplate {
    return switch (template) {
        .checked => |checked_template| fnDefForTemplate(view, checked_template, source_fn_ty),
        .lifted,
        .synthetic,
        => Common.invariant("checked procedure binding referenced a post-check function template"),
    };
}

fn restoreScalar(scalar: checked.ConstScalar) Ast.ExprData {
    return switch (scalar) {
        .i8 => |value| .{ .int_lit = signedIntLiteral(value) },
        .i16 => |value| .{ .int_lit = signedIntLiteral(value) },
        .i32 => |value| .{ .int_lit = signedIntLiteral(value) },
        .i64 => |value| .{ .int_lit = signedIntLiteral(value) },
        .i128 => |value| .{ .int_lit = signedIntLiteral(value) },
        .u8 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .u16 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .u32 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .u64 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .u128 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .f32_bits => |bits| .{ .frac_f32_lit = @bitCast(bits) },
        .f64_bits => |bits| .{ .frac_f64_lit = @bitCast(bits) },
        .dec_bits => |bits| .{ .dec_lit = .{ .num = bits } },
    };
}

fn signedIntLiteral(value: anytype) can.CIR.IntValue {
    const widened: i128 = @intCast(value);
    return .{ .bytes = @bitCast(widened), .kind = .i128 };
}

fn unsignedIntLiteral(value: anytype) can.CIR.IntValue {
    const widened: u128 = @intCast(value);
    return .{ .bytes = @bitCast(widened), .kind = .u128 };
}

fn checkedListElemType(view: ModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedTypeId {
    return switch (checkedPayload(view, checked_ty)) {
        .alias => |alias| checkedListElemType(view, alias.backing),
        .nominal => |nominal| blk: {
            if (nominal.builtin == .list and nominal.args.len == 1) break :blk nominal.args[0];
            break :blk checkedListElemType(view, nominal.backing);
        },
        else => Common.invariant("ConstStore list restored with a non-list checked type"),
    };
}

fn checkedBoxPayloadType(view: ModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedTypeId {
    return switch (checkedPayload(view, checked_ty)) {
        .alias => |alias| checkedBoxPayloadType(view, alias.backing),
        .nominal => |nominal| blk: {
            if (nominal.builtin == .box and nominal.args.len == 1) break :blk nominal.args[0];
            break :blk checkedBoxPayloadType(view, nominal.backing);
        },
        else => Common.invariant("ConstStore box restored with a non-box checked type"),
    };
}

fn checkedTupleItems(view: ModuleView, checked_ty: checked.CheckedTypeId) []const checked.CheckedTypeId {
    return switch (checkedPayload(view, checked_ty)) {
        .alias => |alias| checkedTupleItems(view, alias.backing),
        .nominal => |nominal| checkedTupleItems(view, nominal.backing),
        .tuple => |items| items,
        else => Common.invariant("ConstStore tuple restored with a non-tuple checked type"),
    };
}

fn checkedRecordFields(view: ModuleView, checked_ty: checked.CheckedTypeId) []const checked.CheckedRecordField {
    return switch (checkedPayload(view, checked_ty)) {
        .alias => |alias| checkedRecordFields(view, alias.backing),
        .nominal => |nominal| checkedRecordFields(view, nominal.backing),
        .empty_record => &.{},
        .record_unbound => |fields| fields,
        .record => |record| blk: {
            if (!checkedTypeIsEmptyRecord(view, record.ext)) {
                Common.invariant("ConstStore record restored with an open checked record type");
            }
            break :blk record.fields;
        },
        else => Common.invariant("ConstStore record restored with a non-record checked type"),
    };
}

fn checkedTagPayloadTypes(
    view: ModuleView,
    checked_ty: checked.CheckedTypeId,
    tag_view: ModuleView,
    tag_name: names.TagNameId,
) []const checked.CheckedTypeId {
    return switch (checkedPayload(view, checked_ty)) {
        .alias => |alias| checkedTagPayloadTypes(view, alias.backing, tag_view, tag_name),
        .nominal => |nominal| checkedTagPayloadTypes(view, nominal.backing, tag_view, tag_name),
        .tag_union => |tag_union| blk: {
            if (!checkedTypeIsEmptyTagUnion(view, tag_union.ext)) {
                Common.invariant("ConstStore tag restored with an open checked tag union type");
            }
            const wanted = tag_view.names.tagLabelText(tag_name);
            for (tag_union.tags) |tag| {
                if (std.mem.eql(u8, view.names.tagLabelText(tag.name), wanted)) break :blk tag.args;
            }
            Common.invariant("ConstStore tag name was not present in checked tag union type");
        },
        .empty_tag_union => Common.invariant("ConstStore tag restored with an empty checked tag union type"),
        else => Common.invariant("ConstStore tag restored with a non-tag-union checked type"),
    };
}

fn checkedNominalBackingType(
    view: ModuleView,
    checked_ty: checked.CheckedTypeId,
    _: anytype,
) checked.CheckedTypeId {
    return switch (checkedPayload(view, checked_ty)) {
        .alias => |alias| alias.backing,
        .nominal => |nominal| nominal.backing,
        else => Common.invariant("ConstStore nominal restored with a non-named checked type"),
    };
}

fn checkedTypeIsEmptyRecord(view: ModuleView, checked_ty: checked.CheckedTypeId) bool {
    return switch (checkedPayload(view, checked_ty)) {
        .alias => |alias| checkedTypeIsEmptyRecord(view, alias.backing),
        .nominal => |nominal| checkedTypeIsEmptyRecord(view, nominal.backing),
        .empty_record => true,
        else => false,
    };
}

fn checkedTypeIsEmptyTagUnion(view: ModuleView, checked_ty: checked.CheckedTypeId) bool {
    return switch (checkedPayload(view, checked_ty)) {
        .alias => |alias| checkedTypeIsEmptyTagUnion(view, alias.backing),
        .nominal => |nominal| checkedTypeIsEmptyTagUnion(view, nominal.backing),
        .empty_tag_union => true,
        else => false,
    };
}

fn checkedPayload(view: ModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedTypePayload {
    const raw = @intFromEnum(checked_ty);
    if (raw >= view.types.payloads.len) Common.invariant("checked type id outside checked type store during ConstStore restore");
    return view.types.payloads[raw];
}

fn constFnTemplateToMono(fn_value: anytype) Ast.FnTemplate {
    return .{
        .fn_def = constFnDefToMono(fn_value.fn_def),
        .source_fn_ty = fn_value.source_fn_ty,
    };
}

fn constFnDefToMono(fn_def: anytype) Ast.FnDef {
    return switch (fn_def) {
        .local_template => |template| .{ .local_template = template },
        .imported_template => |template| .{ .imported_template = template },
        .nested => |nested| .{ .nested = .{ .owner = nested.owner, .site = nested.site } },
        .local_hosted => |template| .{ .local_hosted = template },
        .imported_hosted => |template| .{ .imported_hosted = template },
        .checked_generated => |template| .{ .checked_generated = template },
    };
}

fn checkedLambdaExprIdForConstFn(view: ModuleView, fn_def: anytype) checked.CheckedExprId {
    return switch (fn_def) {
        .nested => |nested| blk: {
            const site = checkedNestedSite(view, nested);
            const expr_id = site.checked_expr orelse Common.invariant("stored nested function had no checked expression site");
            const expr = view.bodies.exprs[@intFromEnum(expr_id)];
            break :blk switch (expr.data) {
                .lambda => expr_id,
                .closure => |closure| closure.lambda,
                else => Common.invariant("stored nested function site did not point at a lambda or closure"),
            };
        },
        else => Common.invariant("capturing stored function must reference a checked nested function"),
    };
}

fn ownerTemplateForConstFnDef(fn_def: anytype) names.ProcTemplate {
    return switch (fn_def) {
        .nested => |nested| nested.owner,
        else => Common.invariant("capturing stored function must have a nested owner template"),
    };
}

fn checkedNestedSite(view: ModuleView, nested: anytype) checked.NestedProcSite {
    for (view.nested_proc_sites.sites) |site| {
        if (site.site != nested.site) continue;
        if (!names.procedureTemplateRefEql(site.owner_template, nested.owner)) continue;
        return site;
    }
    Common.invariant("stored nested function referenced a missing checked nested site");
}

fn checkedBinderType(view: ModuleView, binder: checked.PatternBinderId) checked.CheckedTypeId {
    const raw = @intFromEnum(binder);
    if (raw >= view.bodies.pattern_binders.len) Common.invariant("stored function capture binder is outside checked body store");
    const pattern = view.bodies.pattern_binders[raw].pattern;
    const pattern_raw = @intFromEnum(pattern);
    if (pattern_raw >= view.bodies.patterns.len) Common.invariant("stored function capture pattern is outside checked body store");
    return view.bodies.patterns[pattern_raw].ty;
}

fn moduleIdFromDigest(ref: names.CheckedModuleDigest) checked.ModuleId {
    return .{ .bytes = ref.bytes };
}

fn moduleDigestFromId(key: checked.ModuleId) names.CheckedModuleDigest {
    return .{ .bytes = key.bytes };
}

fn methodOwnerFromType(types: *const Type.Store, ty: Type.TypeId) ?static_dispatch.MethodOwner {
    return switch (types.ownerHead(ty)) {
        .none => null,
        .builtin => |owner| .{ .builtin = owner },
        .named_type => |def| .{ .nominal = .{
            .module_name = def.module_name,
            .type_name = def.type_name,
        } },
    };
}

fn builtinOwner(builtin: ?checked.CheckedBuiltinNominal) ?static_dispatch.BuiltinOwner {
    return switch (builtin orelse return null) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .list => .list,
        .box => .box,
    };
}

fn strInspectLowLevelOp(content: Type.Content) can.CIR.Expr.LowLevel {
    return switch (content) {
        .primitive => |primitive| switch (primitive) {
            .str => .str_inspect,
            .u8 => .u8_to_str,
            .i8 => .i8_to_str,
            .u16 => .u16_to_str,
            .i16 => .i16_to_str,
            .u32 => .u32_to_str,
            .i32 => .i32_to_str,
            .u64 => .u64_to_str,
            .i64 => .i64_to_str,
            .u128 => .u128_to_str,
            .i128 => .i128_to_str,
            .f32 => .f32_to_str,
            .f64 => .f64_to_str,
            .dec => .dec_to_str,
            .bool => Common.invariant("Bool must lower as an ordinary tag union before Str.inspect"),
        },
        else => Common.invariant("Str.inspect intrinsic reached Monotype without generated structural inspect lowering"),
    };
}

fn branchCount(branches: anytype) usize {
    var count: usize = 0;
    for (branches) |branch| count += branch.patterns.len;
    return count;
}

fn moduleBytesEqual(a: [32]u8, b: [32]u8) bool {
    return std.mem.eql(u8, a[0..], b[0..]);
}

const CheckedTypeAddress = struct {
    module_bytes: [32]u8,
    ty: u32,
};

const TemplateAddress = struct {
    module_bytes: [32]u8,
    proc_base: u32,
    template: u32,
    type_module_bytes: [32]u8,
    source_fn_ty: u32,

    fn from(template: names.ProcTemplate, type_module: checked.ModuleId, source_fn_ty: checked.CheckedTypeId) TemplateAddress {
        return .{
            .module_bytes = names.procTemplateModuleDigest(template).bytes,
            .proc_base = @intFromEnum(template.proc_base),
            .template = @intFromEnum(template.template),
            .type_module_bytes = type_module.bytes,
            .source_fn_ty = @intFromEnum(source_fn_ty),
        };
    }
};

const NestedSiteAddress = struct {
    module_bytes: [32]u8,
    owner_proc_base: u32,
    owner_template: u32,
    expr: u32,

    fn from(
        module_key: checked.ModuleId,
        owner: names.ProcTemplate,
        expr_id: checked.CheckedExprId,
    ) NestedSiteAddress {
        return .{
            .module_bytes = module_key.bytes,
            .owner_proc_base = @intFromEnum(owner.proc_base),
            .owner_template = @intFromEnum(owner.template),
            .expr = @intFromEnum(expr_id),
        };
    }
};

test "monotype lower declarations are referenced" {
    std.testing.refAllDecls(@This());
    _ = can;
    _ = builtins;
}

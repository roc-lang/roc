//! Checked modules to Monotype IR.

const std = @import("std");
const check = @import("check");
const can = @import("can");
const builtins = @import("builtins");
const base = @import("base");

const Common = @import("../common.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const names = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;
const Ident = base.Ident;

/// Lower checked modules and explicit roots into Monotype IR.
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
    module_identity: checked.ModuleIdentity,
    names: *const names.NameStore,
    types: checked.CheckedTypeStoreView,
    bodies: checked.CheckedBodyStoreView,
    checked_const_bodies: *const checked.CheckedConstBodyTable,
    templates: *const checked.CheckedProcedureTemplateTable,
    compile_time_roots: *const checked.CompileTimeRootTable,
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
    callable_eval_templates: checked.CallableEvalTemplateTableView,
    const_templates: *const checked.ConstTemplateTable,
    const_store: *const checked.ConstStore,
    interface_capabilities: *const checked.ModuleInterfaceCapabilities,
};

const MethodLookup = struct {
    view: ModuleView,
    target: static_dispatch.MethodTarget,
};

const BinderMap = std.AutoHashMap(checked.PatternBinderId, Ast.LocalId);
const TypeState = enum {
    reserved,
    lowering,
    uninhabited,
    defaulted,
    lowered,
};

const TypeBinding = struct {
    ty: Type.TypeId,
    state: TypeState,

    fn hasConcreteShape(self: TypeBinding) bool {
        return switch (self.state) {
            .lowering,
            .uninhabited,
            .defaulted,
            .lowered,
            => true,
            .reserved,
            => false,
        };
    }

    fn hasNumericDefault(self: TypeBinding) bool {
        return self.state == .defaulted;
    }

    fn hasFixedShape(self: TypeBinding) bool {
        return switch (self.state) {
            .lowering,
            .lowered,
            => true,
            .reserved,
            .uninhabited,
            .defaulted,
            => false,
        };
    }
};

const LambdaArgLet = struct {
    pat: Ast.PatId,
    value: Ast.ExprId,
};

const MergeBinder = struct {
    binder: checked.PatternBinderId,
    before: Ast.LocalId,
    ty: Type.TypeId,
};

const LoweredLambdaArgs = struct {
    args: Ast.Span(Ast.TypedLocal),
    body: Ast.ExprId,
};

const LoweredCall = struct {
    ret_ty: Type.TypeId,
    data: Ast.ExprData,
};

const ConstExprAddress = struct {
    store_module_bytes: [32]u8,
    type_module_bytes: [32]u8,
    node: u32,
    mono_ty: u32,
};

const InspectDefAddress = struct {
    value_ty: u32,
    str_ty: u32,
};

const InspectDefEntry = union(enum) {
    reserved: Ast.DefId,
    ready: Ast.DefId,

    fn id(self: InspectDefEntry) Ast.DefId {
        return switch (self) {
            .reserved => |def_id| def_id,
            .ready => |def_id| def_id,
        };
    }
};

const Builder = struct {
    allocator: Allocator,
    modules: Common.CheckedModules,
    root_view: checked.ImportedModuleView,
    program: *Ast.Program,
    symbols: Common.SymbolGen = .{},
    type_cache: std.AutoHashMap(CheckedTypeAddress, Type.TypeId),
    lowered_templates: std.AutoHashMap(TemplateAddress, Ast.DefId),
    lowered_nested_fns: std.AutoHashMap(NestedFnTemplateAddress, u32),
    nested_site_cache: std.AutoHashMap(NestedSiteAddress, names.ProcSiteId),
    const_expr_cache: std.AutoHashMap(ConstExprAddress, Ast.ExprId),
    inspect_defs: std.AutoHashMap(InspectDefAddress, InspectDefEntry),
    u64_ty: ?Type.TypeId = null,
    bool_ty: ?Type.TypeId = null,

    fn init(allocator: Allocator, modules: Common.CheckedModules, program: *Ast.Program) Builder {
        return .{
            .allocator = allocator,
            .modules = modules,
            .root_view = checked.importedView(modules.root.module),
            .program = program,
            .type_cache = std.AutoHashMap(CheckedTypeAddress, Type.TypeId).init(allocator),
            .lowered_templates = std.AutoHashMap(TemplateAddress, Ast.DefId).init(allocator),
            .lowered_nested_fns = std.AutoHashMap(NestedFnTemplateAddress, u32).init(allocator),
            .nested_site_cache = std.AutoHashMap(NestedSiteAddress, names.ProcSiteId).init(allocator),
            .const_expr_cache = std.AutoHashMap(ConstExprAddress, Ast.ExprId).init(allocator),
            .inspect_defs = std.AutoHashMap(InspectDefAddress, InspectDefEntry).init(allocator),
        };
    }

    fn deinit(self: *Builder) void {
        self.inspect_defs.deinit();
        self.const_expr_cache.deinit();
        self.nested_site_cache.deinit();
        self.lowered_nested_fns.deinit();
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

    fn declaredModuleForAlias(self: *Builder, view: ModuleView, alias: checked.CheckedAliasType) names.CheckedModuleDigest {
        return self.moduleDigestForOrigin(view, alias.origin_module);
    }

    fn declaredModuleForNominal(self: *Builder, view: ModuleView, nominal: checked.CheckedNominalType) names.CheckedModuleDigest {
        return switch (nominal.representation) {
            .imported_declaration => |imported| moduleDigestFromId(checked.importedNominalDeclarationModuleId(imported)),
            .imported_box_payload_capability => |imported| moduleDigestFromId(checked.importedBoxPayloadCapabilityModuleId(imported)),
            .builtin,
            .local_declaration,
            .local_box_payload_capability,
            .opaque_without_backing,
            => self.moduleDigestForOrigin(view, nominal.origin_module),
        };
    }

    fn moduleDigestForOrigin(self: *Builder, view: ModuleView, origin_module: names.ModuleNameId) names.CheckedModuleDigest {
        const origin_name = view.names.moduleNameText(origin_module);
        if (moduleViewNameMatches(view, origin_name)) return moduleDigestFromId(view.key);

        const root = moduleView(self.root_view);
        if (moduleViewNameMatches(root, origin_name)) return moduleDigestFromId(root.key);

        for (self.modules.imports) |imported| {
            const imported_view = moduleView(imported);
            if (moduleViewNameMatches(imported_view, origin_name)) return moduleDigestFromId(imported_view.key);
        }

        Common.invariant("checked named type origin module was not available to Monotype lowering");
    }

    fn lowerRoot(self: *Builder, request: checked.RootRequest) Allocator.Error!void {
        const def = if (request.procedure_template) |template|
            try self.lowerTemplate(template, moduleView(self.root_view), request.checked_type)
        else if (request.procedure_binding) |binding|
            try self.lowerProcedureBindingRoot(request, binding)
        else if (request.procedure_use) |procedure|
            try self.lowerProcedureUseRoot(request, procedure)
        else
            Common.invariant("root request reached Monotype without a checked procedure template or procedure source");
        try self.program.roots.append(self.allocator, .{ .def = def, .request = request });
    }

    fn lowerProcedureUseRoot(
        self: *Builder,
        request: checked.RootRequest,
        procedure: checked.ProcedureUseTemplate,
    ) Allocator.Error!Ast.DefId {
        const view = moduleView(self.root_view);
        const fn_ty = try self.lowerType(view, request.checked_type);
        const fn_data = switch (self.program.types.get(fn_ty)) {
            .func => |func| func,
            else => Common.invariant("procedure use root had a non-function checked type"),
        };
        const arg_tys = self.program.types.span(fn_data.args);
        const args = try self.allocator.alloc(Ast.TypedLocal, arg_tys.len);
        defer self.allocator.free(args);
        const arg_exprs = try self.allocator.alloc(Ast.ExprId, arg_tys.len);
        defer self.allocator.free(arg_exprs);

        for (arg_tys, 0..) |arg_ty, i| {
            const local = try self.program.addLocal(self.symbols.fresh(), arg_ty);
            args[i] = .{ .local = local, .ty = arg_ty };
            arg_exprs[i] = try self.program.addExpr(.{ .ty = arg_ty, .data = .{ .local = local } });
        }

        const callee = try self.fnDefForProcedureUseWithType(view, procedure, request.checked_type);
        const body = try self.program.addExpr(.{
            .ty = fn_data.ret,
            .data = .{ .call_proc = .{
                .callee = callee,
                .args = try self.program.addExprSpan(arg_exprs),
            } },
        });

        const def: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        try self.program.defs.append(self.allocator, .{
            .symbol = self.symbols.fresh(),
            .fn_def = null,
            .args = try self.program.addTypedLocalSpan(args),
            .body = body,
            .ret = fn_data.ret,
        });
        return def;
    }

    fn lowerProcedureBindingRoot(
        self: *Builder,
        request: checked.RootRequest,
        binding_id: checked.TopLevelProcedureBindingId,
    ) Allocator.Error!Ast.DefId {
        const view = moduleView(self.root_view);
        const fn_ty = try self.lowerType(view, request.checked_type);
        const fn_data = switch (self.program.types.get(fn_ty)) {
            .func => |func| func,
            else => Common.invariant("procedure binding root had a non-function checked type"),
        };
        const arg_tys = self.program.types.span(fn_data.args);
        const args = try self.allocator.alloc(Ast.TypedLocal, arg_tys.len);
        defer self.allocator.free(args);
        const arg_exprs = try self.allocator.alloc(Ast.ExprId, arg_tys.len);
        defer self.allocator.free(arg_exprs);

        for (arg_tys, 0..) |arg_ty, i| {
            const local = try self.program.addLocal(self.symbols.fresh(), arg_ty);
            args[i] = .{ .local = local, .ty = arg_ty };
            arg_exprs[i] = try self.program.addExpr(.{ .ty = arg_ty, .data = .{ .local = local } });
        }

        const binding = view.top_level_procedure_bindings.get(binding_id);
        const callee = try self.lowerProcedureBindingValue(view, binding, request.checked_type, fn_ty);
        const body = try self.program.addExpr(.{
            .ty = fn_data.ret,
            .data = .{ .call_value = .{
                .callee = callee,
                .args = try self.program.addExprSpan(arg_exprs),
            } },
        });

        const def: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        try self.program.defs.append(self.allocator, .{
            .symbol = self.symbols.fresh(),
            .fn_def = null,
            .args = try self.program.addTypedLocalSpan(args),
            .body = body,
            .ret = fn_data.ret,
        });
        return def;
    }

    fn lowerProcedureBindingValue(
        self: *Builder,
        view: ModuleView,
        binding: checked.TopLevelProcedureBinding,
        source_fn_ty: checked.CheckedTypeId,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (binding.body) {
            .direct_template => blk: {
                const fn_template = fnDefForProcedureBindingBody(
                    view,
                    binding.body,
                    source_fn_ty,
                    view.types.rootKey(source_fn_ty),
                    mono_fn_ty,
                );
                try self.lowerFnTemplateDef(view, fn_template);
                break :blk try self.program.addExpr(.{ .ty = mono_fn_ty, .data = .{ .fn_def = fn_template } });
            },
            .callable_eval_template => |template_id| try self.lowerCallableEvalBindingValue(view, template_id, mono_fn_ty),
        };
    }

    fn lowerCallableEvalBindingValue(
        self: *Builder,
        view: ModuleView,
        template_id: checked.CallableEvalTemplateId,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const raw = @intFromEnum(template_id);
        if (raw >= view.callable_eval_templates.templates.len) {
            Common.invariant("callable eval binding referenced a missing checked template");
        }
        const template = view.callable_eval_templates.templates[raw];
        const root = view.compile_time_roots.root(template.root);
        return switch (root.payload) {
            .fn_value => |fn_id| try self.restoreConstFnExpr(view, view, fn_id, mono_fn_ty),
            .pending => try self.lowerPendingCallableEvalBindingValue(view, template, root, mono_fn_ty),
            else => Common.invariant("callable eval binding root did not publish a callable value"),
        };
    }

    fn lowerPendingCallableEvalBindingValue(
        self: *Builder,
        view: ModuleView,
        template: checked.CallableEvalTemplate,
        root: checked.CompileTimeRoot,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const wrapper = view.entry_wrappers.lookupByRoot(template.root) orelse
            Common.invariant("callable eval template root had no checked entry wrapper");

        const wrapper_args = try self.program.types.addSpan(&.{});
        const wrapper_fn_ty = try self.program.types.add(.{ .func = .{
            .args = wrapper_args,
            .ret = mono_fn_ty,
        } });
        const wrapper_template = fnDefForTemplate(
            view,
            wrapper.template,
            wrapper.checked_fn_root,
            view.types.rootKey(wrapper.checked_fn_root),
            wrapper_fn_ty,
        );

        var body_ctx = try BodyContext.init(self.allocator, self, view, wrapper.template);
        defer body_ctx.deinit();
        const root_fn_key = Ast.fnTemplateDigest(wrapper_template, &self.program.types, &self.program.names);
        body_ctx.owner_context_fn_key = root_fn_key;
        body_ctx.current_fn_key = root_fn_key;
        try body_ctx.bindTypeToMono(wrapper.checked_fn_root, wrapper_fn_ty, "callable eval wrapper root mapped one checked type to two monotype types");
        try body_ctx.bindTypeToMono(template.checked_fn_root, mono_fn_ty, "callable eval template root mapped one checked type to two monotype types");
        try body_ctx.bindKnownType(root.checked_type, mono_fn_ty);

        return try body_ctx.lowerExprAtType(wrapper.body_expr, mono_fn_ty);
    }

    fn lowerTemplate(
        self: *Builder,
        template_ref: names.ProcTemplate,
        source_ty_view: ModuleView,
        source_fn_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.DefId {
        const fn_ty = try self.lowerType(source_ty_view, source_fn_ty);
        return try self.lowerTemplateWithMono(template_ref, source_ty_view, source_fn_ty, source_ty_view.types.rootKey(source_fn_ty), fn_ty);
    }

    fn lowerTemplateWithMono(
        self: *Builder,
        template_ref: names.ProcTemplate,
        source_ty_view: ModuleView,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.DefId {
        const fn_ty_digest = self.program.types.typeDigest(&self.program.names, fn_ty);
        const address = TemplateAddress.from(template_ref, source_fn_key, fn_ty_digest);
        if (self.lowered_templates.get(address)) |existing| {
            return existing;
        }

        const view = self.moduleForDigest(names.procTemplateModuleDigest(template_ref));
        const template = view.templates.get(template_ref.template);
        const symbol = self.symbols.fresh();
        const fn_template = fnDefForTemplate(view, template_ref, source_fn_ty, source_fn_key, fn_ty);

        const reserved: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        try self.program.defs.append(self.allocator, undefined);
        try self.lowered_templates.put(address, reserved);

        var body_ctx = try BodyContext.init(self.allocator, self, view, template_ref);
        const root_fn_key = Ast.fnTemplateDigest(fn_template, &self.program.types, &self.program.names);
        body_ctx.owner_context_fn_key = root_fn_key;
        body_ctx.current_fn_key = root_fn_key;
        defer body_ctx.deinit();
        if (moduleBytesEqual(source_ty_view.key.bytes, view.key.bytes)) {
            try body_ctx.bindKnownType(source_fn_ty, fn_ty);
        }
        try body_ctx.bindTypeToMono(template.checked_fn_root, fn_ty, "template function root mapped one checked type to two monotype types");

        const lowered = try body_ctx.lowerTemplateBody(template_ref, template, fn_ty);
        self.program.defs.items[@intFromEnum(reserved)] = .{
            .symbol = symbol,
            .fn_def = fn_template,
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
            .pending => Common.invariant("pending checked type reached Monotype lowering"),
            .flex => |variable| self.lowerCheckedTypeVariable(variable),
            .rigid => |variable| self.lowerCheckedTypeVariable(variable),
            .empty_record => .{ .record = .empty() },
            .empty_tag_union => .{ .tag_union = .empty() },
            .record_unbound => |fields| try self.lowerRecordFields(view, fields),
            .record => |record| try self.lowerRecordRow(view, record.fields, record.ext),
            .tuple => |items| blk: {
                const lowered = try self.lowerTypeSlice(view, items);
                defer self.allocator.free(lowered);
                break :blk .{ .tuple = try self.program.types.addSpan(lowered) };
            },
            .tag_union => |tag_union| try self.lowerTagUnionRow(view, tag_union.tags, tag_union.ext),
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
                    .named_type = .{ .module = self.declaredModuleForAlias(view, alias), .ty = checked_ty },
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
                        .bool_tag_union => {},
                        .list => {
                            if (nominal.args.len != 1) Common.invariant("checked List nominal must have exactly one type argument");
                            break :blk .{ .list = try self.lowerType(view, nominal.args[0]) };
                        },
                        .box => {
                            if (nominal.args.len != 1) Common.invariant("checked Box nominal must have exactly one type argument");
                            break :blk .{ .box = try self.lowerType(view, nominal.args[0]) };
                        },
                    },
                    else => {},
                }

                const args = try self.lowerTypeSlice(view, nominal.args);
                defer self.allocator.free(args);
                const backing_use: Type.BackingUse = if (nominal.is_opaque) .runtime_layout_only else .inspectable;
                break :blk .{ .named = .{
                    .named_type = .{ .module = self.declaredModuleForNominal(view, nominal), .ty = checked_ty },
                    .def = try self.typeDef(view, nominal.origin_module, nominal.name),
                    .kind = if (nominal.is_opaque) .@"opaque" else .nominal,
                    .builtin_owner = builtinOwner(nominal.builtin),
                    .args = try self.program.types.addSpan(args),
                    .backing = switch (nominal.representation) {
                        .opaque_without_backing => null,
                        else => .{
                            .ty = try self.lowerNominalBackingType(view, nominal, args),
                            .use = backing_use,
                        },
                    },
                } };
            },
        };
    }

    fn lowerCheckedTypeVariable(self: *Builder, variable: checked.CheckedTypeVariable) Type.Content {
        _ = self;
        if (variable.numeric_default_phase) |phase| {
            return switch (phase) {
                .mono_specialization => .{ .primitive = .dec },
                .checking_finalized => Common.invariant("checking-finalized numeric variable reached Monotype unresolved"),
            };
        }
        if (variable.row_default) |row_default| {
            return switch (row_default) {
                .empty_record => .{ .record = .empty() },
                .empty_tag_union => .{ .tag_union = .empty() },
            };
        }
        return .{ .tag_union = .empty() };
    }

    fn typeIsDec(self: *Builder, ty: Type.TypeId) bool {
        return switch (self.shapeContent(ty)) {
            .primitive => |primitive| primitive == .dec,
            else => false,
        };
    }

    fn namedBackingType(self: *Builder, ty: Type.TypeId) ?Type.TypeId {
        return switch (self.program.types.get(ty)) {
            .named => |named| if (named.backing) |backing| backing.ty else null,
            else => null,
        };
    }

    fn shapeContent(self: *Builder, ty: Type.TypeId) Type.Content {
        var current = ty;
        while (true) {
            switch (self.program.types.get(current)) {
                .named => |named| if (named.backing) |backing| {
                    current = backing.ty;
                    continue;
                } else {
                    return self.program.types.get(current);
                },
                else => |content| return content,
            }
        }
    }

    fn lowerNominalBackingType(
        self: *Builder,
        view: ModuleView,
        nominal: checked.CheckedNominalType,
        mono_args: []const Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        var ctx = try BodyContext.init(self.allocator, self, view, .{
            .proc_base = @enumFromInt(0),
            .template = @enumFromInt(0),
        });
        defer ctx.deinit();
        try ctx.bindNominalDeclarationFormalsToMonoArgs(nominal, mono_args);
        return try ctx.lowerType(ctx.nominalBackingRoot(nominal));
    }

    fn tupleItemTypes(self: *Builder, ty: Type.TypeId) []const Type.TypeId {
        return switch (self.shapeContent(ty)) {
            .tuple => |items| self.program.types.span(items),
            else => Common.invariant("tuple pattern had a non-tuple checked type"),
        };
    }

    fn recordFieldType(self: *Builder, ty: Type.TypeId, name: names.RecordFieldNameId) Type.TypeId {
        return switch (self.shapeContent(ty)) {
            .record => |fields| {
                for (self.program.types.fieldSpan(fields)) |field| {
                    if (self.program.names.recordFieldLabelTextEql(field.name, name)) return field.ty;
                }
                Common.invariant("record pattern field was absent from checked record type");
            },
            else => Common.invariant("record pattern had a non-record checked type"),
        };
    }

    fn tagPayloadTypes(self: *Builder, ty: Type.TypeId, name: names.TagNameId) []const Type.TypeId {
        return switch (self.shapeContent(ty)) {
            .tag_union => |tags| {
                for (self.program.types.tagSpan(tags)) |tag| {
                    if (self.program.names.tagLabelTextEql(tag.name, name)) return self.program.types.span(tag.payloads);
                }
                Common.invariant("tag pattern was absent from checked tag-union type");
            },
            else => Common.invariant("tag pattern had a non-tag-union checked type"),
        };
    }

    fn lowerRecordFields(self: *Builder, view: ModuleView, fields: []const checked.CheckedRecordField) Allocator.Error!Type.Content {
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

    fn lowerRecordRow(
        self: *Builder,
        view: ModuleView,
        head: []const checked.CheckedRecordField,
        ext: checked.CheckedTypeId,
    ) Allocator.Error!Type.Content {
        var fields = std.ArrayList(Type.Field).empty;
        defer fields.deinit(self.allocator);
        try self.appendRecordFields(view, &fields, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (true) {
            if (seen.contains(current)) Common.invariant("checked record row chain was cyclic at Monotype lowering");
            try seen.put(current, {});

            const payload = self.checkedTypePayload(view, current);
            switch (payload) {
                .alias => |alias| current = alias.backing,
                .empty_record => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_record) break;
                    Common.invariant("open non-record checked row reached Monotype record lowering");
                },
                .record_unbound => |tail_fields| {
                    try self.appendRecordFields(view, &fields, tail_fields);
                    break;
                },
                .record => |record| {
                    try self.appendRecordFields(view, &fields, record.fields);
                    current = record.ext;
                },
                else => Common.invariant("open or non-record checked row reached Monotype record lowering"),
            }
        }

        std.mem.sort(Type.Field, fields.items, self, recordFieldLessThan);
        assertNoDuplicateRecordFields(self, fields.items, "checked record row had duplicate fields at Monotype lowering");

        return .{ .record = try self.program.types.addFields(fields.items) };
    }

    fn appendRecordFields(
        self: *Builder,
        view: ModuleView,
        out: *std.ArrayList(Type.Field),
        fields: []const checked.CheckedRecordField,
    ) Allocator.Error!void {
        for (fields) |field| {
            try out.append(self.allocator, .{
                .name = try self.recordFieldName(view, field.name),
                .ty = try self.lowerType(view, field.ty),
            });
        }
    }

    fn lowerTagUnionRow(
        self: *Builder,
        view: ModuleView,
        head: []const checked.CheckedTag,
        ext: checked.CheckedTypeId,
    ) Allocator.Error!Type.Content {
        var tags = std.ArrayList(Type.Tag).empty;
        defer tags.deinit(self.allocator);
        try self.appendTags(view, &tags, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (true) {
            if (seen.contains(current)) Common.invariant("checked tag row chain was cyclic at Monotype lowering");
            try seen.put(current, {});

            const payload = self.checkedTypePayload(view, current);
            switch (payload) {
                .alias => |alias| current = alias.backing,
                .empty_tag_union => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_tag_union) break;
                    Common.invariant("open non-tag checked row reached Monotype tag-union lowering");
                },
                .tag_union => |tag_union| {
                    try self.appendTags(view, &tags, tag_union.tags);
                    current = tag_union.ext;
                },
                else => Common.invariant("open or non-tag checked row reached Monotype tag-union lowering"),
            }
        }

        std.mem.sort(Type.Tag, tags.items, self, tagLessThan);
        assertNoDuplicateTags(self, tags.items, "checked tag row had duplicate tags at Monotype lowering");

        return .{ .tag_union = try self.program.types.addTags(tags.items) };
    }

    fn appendTags(
        self: *Builder,
        view: ModuleView,
        out: *std.ArrayList(Type.Tag),
        tags: []const checked.CheckedTag,
    ) Allocator.Error!void {
        for (tags) |tag| {
            const payloads = try self.lowerTypeSlice(view, tag.args);
            defer self.allocator.free(payloads);
            try out.append(self.allocator, .{
                .name = try self.tagName(view, tag.name),
                .checked_name = tag.name,
                .payloads = try self.program.types.addSpan(payloads),
            });
        }
    }

    fn checkedTypePayload(self: *Builder, view: ModuleView, ty: checked.CheckedTypeId) checked.CheckedTypePayload {
        _ = self;
        return checkedPayload(view, ty);
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
        return self.lookupMethodTargetByName(owner, method_view.names.methodNameText(method));
    }

    fn lookupMethodTargetByName(
        self: *Builder,
        owner: static_dispatch.MethodOwner,
        method_name: []const u8,
    ) ?MethodLookup {
        var found: ?MethodLookup = null;
        self.lookupMethodTargetInView(moduleView(self.root_view), owner, method_name, &found);
        for (self.modules.imports) |imported| {
            self.lookupMethodTargetInView(moduleView(imported), owner, method_name, &found);
        }
        for (self.modules.root.relation_modules) |relation| {
            self.lookupMethodTargetInView(moduleView(relation), owner, method_name, &found);
        }
        return found;
    }

    fn lookupMethodTargetInView(
        self: *Builder,
        view: ModuleView,
        owner: static_dispatch.MethodOwner,
        method_name: []const u8,
        found: *?MethodLookup,
    ) void {
        const view_owner = self.methodOwnerForView(owner, view) orelse return;
        const view_method = view.names.lookupMethodName(method_name) orelse return;
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
        return try self.fnDefForProcedureUseWithType(source_ty_view, proc, source_fn_ty);
    }

    fn fnDefForProcedureUseWithType(
        self: *Builder,
        source_ty_view: ModuleView,
        proc: checked.ProcedureUseTemplate,
        source_fn_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.FnTemplate {
        const mono_fn_ty = try self.lowerType(source_ty_view, source_fn_ty);
        const source_fn_key = proc.source_fn_ty_template;
        const fn_template = switch (proc.binding) {
            .top_level => |top_level| blk: {
                const view = self.moduleForId(checked.topLevelProcedureModuleId(top_level));
                const binding = view.top_level_procedure_bindings.get(top_level.binding);
                break :blk fnDefForProcedureBindingBody(view, binding.body, source_fn_ty, source_fn_key, mono_fn_ty);
            },
            .imported => |imported| blk: {
                const view = self.moduleForId(checked.importedProcedureModuleId(imported));
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and binding.binding.pattern == imported.pattern) {
                        break :blk fnDefForImportedBindingBody(view, binding.body, source_fn_ty, source_fn_key, mono_fn_ty);
                    }
                }
                Common.invariant("imported procedure binding was not exported by its checked module");
            },
            .hosted => |hosted| blk: {
                break :blk fnDefForTemplate(self.moduleForId(moduleIdFromDigest(names.procTemplateModuleDigest(hosted.template))), hosted.template, source_fn_ty, source_fn_key, mono_fn_ty);
            },
            .platform_required => |required| blk: {
                const app_view = self.moduleForId(checked.requiredProcedureModuleId(required));
                const binding = app_view.top_level_procedure_bindings.get(required.procedure_binding);
                break :blk fnDefForProcedureBindingBody(app_view, binding.body, source_fn_ty, source_fn_key, mono_fn_ty);
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
            => |template| _ = try self.lowerTemplateWithMono(template, source_ty_view, fn_template.source_fn_ty, fn_template.source_fn_key, fn_template.mono_fn_ty),
            .nested => {},
        }
    }

    fn lowerRestoredConstFnTemplate(self: *Builder, type_view: ModuleView, fn_template: Ast.FnTemplate) Allocator.Error!void {
        switch (fn_template.fn_def) {
            .nested => blk: {
                const fn_view = self.moduleForConstFnDef(fn_template.fn_def);
                var fn_ctx = try BodyContext.init(self.allocator, self, fn_view, ownerTemplateForConstFnDef(fn_template.fn_def));
                defer fn_ctx.deinit();
                try self.lowerNestedFnFromContext(&fn_ctx, checkedLambdaExprIdForConstFn(fn_view, fn_template.fn_def), fn_template);
                break :blk;
            },
            else => try self.lowerFnTemplateDef(type_view, fn_template),
        }
    }

    fn lowerFnTemplateDefFromContext(self: *Builder, source_ctx: *BodyContext, fn_template: Ast.FnTemplate) Allocator.Error!void {
        switch (fn_template.fn_def) {
            .local_template,
            .imported_template,
            .local_hosted,
            .imported_hosted,
            .checked_generated,
            => |template| _ = try self.lowerTemplateWithMono(template, source_ctx.view, fn_template.source_fn_ty, fn_template.source_fn_key, fn_template.mono_fn_ty),
            .nested => {},
        }
    }

    fn nestedFnForExpr(
        self: *Builder,
        view: ModuleView,
        owner: names.ProcTemplate,
        expr_id: checked.CheckedExprId,
        context_fn_key: names.TypeDigest,
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
            .context_fn_key = context_fn_key,
        };
    }

    fn fnTemplateForNestedExprWithMono(
        self: *Builder,
        view: ModuleView,
        owner: names.ProcTemplate,
        expr_id: checked.CheckedExprId,
        checked_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
        context_fn_key: names.TypeDigest,
    ) Allocator.Error!Ast.FnTemplate {
        return .{
            .fn_def = .{ .nested = try self.nestedFnForExpr(view, owner, expr_id, context_fn_key) },
            .source_fn_ty = checked_fn_ty,
            .source_fn_key = source_fn_key,
            .mono_fn_ty = mono_fn_ty,
        };
    }

    fn lowerNestedFnFromContext(
        self: *Builder,
        source_ctx: *BodyContext,
        expr_id: checked.CheckedExprId,
        fn_template: Ast.FnTemplate,
    ) Allocator.Error!void {
        const nested = switch (fn_template.fn_def) {
            .nested => |nested| nested,
            else => Common.invariant("local procedure specialization did not have a nested function identity"),
        };
        const fn_ty_digest = self.program.types.typeDigest(&self.program.names, fn_template.mono_fn_ty);
        const address = NestedFnTemplateAddress.from(nested, fn_template.source_fn_key, fn_ty_digest);
        if (self.lowered_nested_fns.contains(address)) return;

        const nested_id: u32 = @intCast(self.program.nested_defs.items.len);
        try self.program.nested_defs.append(self.allocator, undefined);
        try self.lowered_nested_fns.put(address, nested_id);

        var nested_ctx = try source_ctx.childContext(Ast.fnTemplateDigest(fn_template, &self.program.types, &self.program.names));
        defer nested_ctx.deinit();
        try nested_ctx.bindTypeToMono(fn_template.source_fn_ty, fn_template.mono_fn_ty, "nested function root mapped one checked type to two monotype types");

        const lowered = try nested_ctx.lowerNestedFunction(expr_id, fn_template.mono_fn_ty);
        self.program.nested_defs.items[nested_id] = .{
            .symbol = self.symbols.fresh(),
            .fn_def = fn_template,
            .args = lowered.args,
            .body = lowered.body,
            .ret = lowered.ret,
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

    fn restoreConstFnExpr(
        self: *Builder,
        store_view: ModuleView,
        type_view: ModuleView,
        fn_id: checked.ConstFnId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const raw = @intFromEnum(fn_id);
        if (raw >= store_view.const_store.fns.items.len) Common.invariant("ConstStore function id is out of range");
        const fn_value = store_view.const_store.fns.items[raw];
        const template = constFnTemplateToMono(type_view, fn_value, ty);
        if (fn_value.captures.len == 0) {
            try self.lowerRestoredConstFnTemplate(type_view, template);
            return try self.program.addExpr(.{ .ty = ty, .data = .{ .fn_def = template } });
        }

        const fn_view = self.moduleForConstFnDef(fn_value.fn_def);
        var fn_ctx = try BodyContext.init(self.allocator, self, fn_view, ownerTemplateForConstFnDef(fn_value.fn_def));
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
            const lowered_ty = try self.lowerType(fn_view, capture_ty);
            const value_expr = try fn_ctx.restoreConstNode(store_view, fn_view, capture.value, capture_ty);
            const local = try self.program.addLocalWithBinder(self.symbols.fresh(), lowered_ty, capture.binder);
            const pat = try self.program.addPat(.{ .ty = lowered_ty, .data = .{ .bind = local } });
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

        var expr = try self.program.addExpr(.{
            .ty = ty,
            .data = switch (lambda_expr.data) {
                .lambda => |lambda| try fn_ctx.lowerLambdaExpr(lambda, template),
                else => Common.invariant("stored capturing function did not reference a checked lambda"),
            },
        });
        var index = captures.len;
        while (index > 0) {
            index -= 1;
            expr = try self.program.addExpr(.{
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

    fn inspectCall(self: *Builder, value: Ast.ExprId, value_ty: Type.TypeId, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const def_id = try self.inspectDefForType(value_ty, str_ty);
        const fn_ty = try self.oneArgFnType(value_ty, str_ty);
        const callee = try self.program.addExpr(.{
            .ty = fn_ty,
            .data = .{ .def_ref = def_id },
        });
        const args = [_]Ast.ExprId{value};
        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .call_value = .{
                .callee = callee,
                .args = try self.program.addExprSpan(&args),
            } },
        });
    }

    fn inspectDefForType(self: *Builder, value_ty: Type.TypeId, str_ty: Type.TypeId) Allocator.Error!Ast.DefId {
        const address = InspectDefAddress{
            .value_ty = @intFromEnum(value_ty),
            .str_ty = @intFromEnum(str_ty),
        };
        if (self.inspect_defs.get(address)) |entry| return entry.id();

        const def_id: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        try self.program.defs.append(self.allocator, undefined);
        try self.inspect_defs.put(address, .{ .reserved = def_id });

        const arg_local = try self.program.addLocal(self.symbols.fresh(), value_ty);
        const arg_expr = try self.localExpr(arg_local, value_ty);
        const body = try self.inspectBody(arg_expr, value_ty, value_ty, str_ty);

        self.program.defs.items[@intFromEnum(def_id)] = .{
            .symbol = self.symbols.fresh(),
            .fn_def = null,
            .args = try self.program.addTypedLocalSpan(&.{.{ .local = arg_local, .ty = value_ty }}),
            .body = body,
            .ret = str_ty,
        };
        try self.inspect_defs.put(address, .{ .ready = def_id });
        return def_id;
    }

    fn inspectBody(
        self: *Builder,
        value: Ast.ExprId,
        value_ty: Type.TypeId,
        shape_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (self.program.types.get(shape_ty)) {
            .primitive => |primitive| try self.primitiveInspect(value, primitive, str_ty),
            .named => |named| blk: {
                if (named.builtin_owner) |owner| {
                    switch (owner) {
                        .list => break :blk try self.inspectList(value, value_ty, self.singleTypeArg(named.args, "List"), str_ty),
                        .box => {},
                        else => {},
                    }
                }
                if (try self.toInspectCall(value, value_ty, str_ty)) |method_call| break :blk method_call;
                const backing = named.backing orelse Common.invariant("Str.inspect reached opaque named type without checked inspect authority");
                if (backing.use != .inspectable) {
                    break :blk try self.stringExpr("<opaque>", str_ty);
                }
                break :blk try self.inspectBody(value, value_ty, backing.ty, str_ty);
            },
            .record => |fields| try self.inspectRecord(value, self.program.types.fieldSpan(fields), str_ty),
            .tuple => |items| try self.inspectTuple(value, self.program.types.span(items), str_ty),
            .tag_union => |tags| try self.inspectTagUnion(value, value_ty, self.program.types.tagSpan(tags), str_ty),
            .list => |elem_ty| try self.inspectList(value, value_ty, elem_ty, str_ty),
            .func, .erased => try self.stringExpr("<function>", str_ty),
            .zst => try self.stringExpr("{}", str_ty),
            .box => |elem_ty| blk: {
                const unboxed = try self.lowLevelExpr(.box_unbox, &.{value}, elem_ty);
                break :blk try self.inspectCall(unboxed, elem_ty, str_ty);
            },
        };
    }

    fn toInspectCall(self: *Builder, value: Ast.ExprId, value_ty: Type.TypeId, str_ty: Type.TypeId) Allocator.Error!?Ast.ExprId {
        const owner = methodOwnerFromType(&self.program.types, value_ty) orelse return null;
        const lookup = self.lookupMethodTargetByName(owner, "to_inspect") orelse return null;
        const template = lookup.target.template orelse
            Common.invariant("checked to_inspect target was not backed by a procedure template");

        var target_ctx = try BodyContext.init(self.allocator, self, lookup.view, template);
        defer target_ctx.deinit();
        const callable_mono_ty = try target_ctx.instantiateTargetCallTypeFromMonoArgs(lookup.target.callable_ty, &.{value_ty}, str_ty);
        _ = try self.lowerTemplateWithMono(
            template,
            lookup.view,
            lookup.target.callable_ty,
            lookup.view.types.rootKey(lookup.target.callable_ty),
            callable_mono_ty,
        );

        const args = [_]Ast.ExprId{value};
        return try self.program.addExpr(.{ .ty = str_ty, .data = .{ .call_proc = .{
            .callee = fnDefForTemplate(
                lookup.view,
                template,
                lookup.target.callable_ty,
                lookup.view.types.rootKey(lookup.target.callable_ty),
                callable_mono_ty,
            ),
            .args = try self.program.addExprSpan(&args),
        } } });
    }

    fn primitiveInspect(self: *Builder, value: Ast.ExprId, primitive: Type.Primitive, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const args = [_]Ast.ExprId{value};
        return try self.lowLevelExpr(primitiveInspectLowLevelOp(primitive), &args, str_ty);
    }

    fn inspectTuple(self: *Builder, value: Ast.ExprId, items: []const Type.TypeId, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        if (items.len == 0) return try self.stringExpr("()", str_ty);
        var out = try self.stringExpr("(", str_ty);
        for (items, 0..) |item_ty, i| {
            if (i != 0) out = try self.concatExpr(out, try self.stringExpr(", ", str_ty), str_ty);
            const item = try self.program.addExpr(.{
                .ty = item_ty,
                .data = .{ .tuple_access = .{ .tuple = value, .elem_index = @intCast(i) } },
            });
            out = try self.concatExpr(out, try self.inspectCall(item, item_ty, str_ty), str_ty);
        }
        return try self.concatExpr(out, try self.stringExpr(")", str_ty), str_ty);
    }

    fn inspectRecord(self: *Builder, value: Ast.ExprId, fields: []const Type.Field, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        if (fields.len == 0) return try self.stringExpr("{}", str_ty);
        var out = try self.stringExpr("{ ", str_ty);
        for (fields, 0..) |field, i| {
            if (i != 0) out = try self.concatExpr(out, try self.stringExpr(", ", str_ty), str_ty);
            out = try self.concatExpr(out, try self.stringExpr(self.program.names.recordFieldLabelText(field.name), str_ty), str_ty);
            out = try self.concatExpr(out, try self.stringExpr(": ", str_ty), str_ty);
            const field_value = try self.program.addExpr(.{
                .ty = field.ty,
                .data = .{ .field_access = .{ .receiver = value, .field = field.name } },
            });
            out = try self.concatExpr(out, try self.inspectCall(field_value, field.ty, str_ty), str_ty);
        }
        return try self.concatExpr(out, try self.stringExpr(" }", str_ty), str_ty);
    }

    fn inspectTagUnion(self: *Builder, value: Ast.ExprId, value_ty: Type.TypeId, tags: []const Type.Tag, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        if (tags.len == 0) {
            const msg = try self.program.addStringLiteral("uninhabited value reached Str.inspect");
            return try self.program.addExpr(.{
                .ty = str_ty,
                .data = .{ .crash = msg },
            });
        }
        const branches = try self.allocator.alloc(Ast.Branch, tags.len);
        defer self.allocator.free(branches);

        for (tags, 0..) |tag, i| {
            const payload_tys = self.program.types.span(tag.payloads);
            const payload_pats = try self.allocator.alloc(Ast.PatId, payload_tys.len);
            defer self.allocator.free(payload_pats);
            const payload_exprs = try self.allocator.alloc(Ast.ExprId, payload_tys.len);
            defer self.allocator.free(payload_exprs);

            for (payload_tys, 0..) |payload_ty, payload_i| {
                const local = try self.program.addLocal(self.symbols.fresh(), payload_ty);
                payload_pats[payload_i] = try self.program.addPat(.{ .ty = payload_ty, .data = .{ .bind = local } });
                payload_exprs[payload_i] = try self.localExpr(local, payload_ty);
            }

            const pat = try self.program.addPat(.{
                .ty = value_ty,
                .data = .{ .tag = .{
                    .name = tag.name,
                    .payloads = try self.program.addPatSpan(payload_pats),
                } },
            });
            branches[i] = .{
                .pat = pat,
                .body = try self.inspectTagBody(tag.name, payload_exprs, payload_tys, str_ty),
            };
        }

        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .match_ = .{
                .scrutinee = value,
                .branches = try self.program.addBranchSpan(branches),
            } },
        });
    }

    fn inspectTagBody(
        self: *Builder,
        name: names.TagNameId,
        payload_exprs: []const Ast.ExprId,
        payload_tys: []const Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        var out = try self.stringExpr(self.program.names.tagLabelText(name), str_ty);
        if (payload_exprs.len == 0) return out;
        out = try self.concatExpr(out, try self.stringExpr("(", str_ty), str_ty);
        for (payload_exprs, payload_tys, 0..) |payload_expr, payload_ty, i| {
            if (i != 0) out = try self.concatExpr(out, try self.stringExpr(", ", str_ty), str_ty);
            out = try self.concatExpr(out, try self.inspectCall(payload_expr, payload_ty, str_ty), str_ty);
        }
        return try self.concatExpr(out, try self.stringExpr(")", str_ty), str_ty);
    }

    fn inspectList(self: *Builder, value: Ast.ExprId, list_ty: Type.TypeId, elem_ty: Type.TypeId, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const u64_ty = try self.primitiveType(.u64);
        const bool_ty = try self.primitiveType(.bool);

        const len_local = try self.program.addLocal(self.symbols.fresh(), u64_ty);
        const len_pat = try self.bindPat(len_local, u64_ty);
        const len_value = try self.lowLevelExpr(.list_len, &.{value}, u64_ty);
        const len_expr = try self.localExpr(len_local, u64_ty);

        const index_local = try self.program.addLocal(self.symbols.fresh(), u64_ty);
        const out_local = try self.program.addLocal(self.symbols.fresh(), str_ty);
        const index_expr = try self.localExpr(index_local, u64_ty);
        const out_expr = try self.localExpr(out_local, str_ty);

        const done_cond = try self.lowLevelExpr(.num_is_eq, &.{ index_expr, len_expr }, bool_ty);
        const finish = try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .break_ = try self.concatExpr(out_expr, try self.stringExpr("]", str_ty), str_ty) },
        });
        const step = try self.inspectListStep(value, list_ty, elem_ty, str_ty, index_local, out_local, len_local);
        const body = try self.ifExpr(done_cond, finish, step, str_ty);

        const params = [_]Ast.TypedLocal{
            .{ .local = index_local, .ty = u64_ty },
            .{ .local = out_local, .ty = str_ty },
        };
        const initial_values = [_]Ast.ExprId{
            try self.intLiteralExpr(0, u64_ty),
            try self.stringExpr("[", str_ty),
        };
        const loop = try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .loop_ = .{
                .params = try self.program.addTypedLocalSpan(&params),
                .initial_values = try self.program.addExprSpan(&initial_values),
                .body = body,
            } },
        });
        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .let_ = .{
                .bind = len_pat,
                .value = len_value,
                .rest = loop,
            } },
        });
    }

    fn inspectListStep(
        self: *Builder,
        list_value: Ast.ExprId,
        list_ty: Type.TypeId,
        elem_ty: Type.TypeId,
        str_ty: Type.TypeId,
        index_local: Ast.LocalId,
        out_local: Ast.LocalId,
        len_local: Ast.LocalId,
    ) Allocator.Error!Ast.ExprId {
        _ = list_ty;
        _ = len_local;
        const u64_ty = try self.primitiveType(.u64);
        const bool_ty = try self.primitiveType(.bool);
        const index_expr = try self.localExpr(index_local, u64_ty);
        const out_expr = try self.localExpr(out_local, str_ty);

        const first_cond = try self.lowLevelExpr(.num_is_eq, &.{ index_expr, try self.intLiteralExpr(0, u64_ty) }, bool_ty);
        const sep = try self.ifExpr(first_cond, try self.stringExpr("", str_ty), try self.stringExpr(", ", str_ty), str_ty);
        const elem = try self.lowLevelExpr(.list_get_unsafe, &.{ list_value, index_expr }, elem_ty);
        const elem_str = try self.inspectCall(elem, elem_ty, str_ty);
        const with_sep = try self.concatExpr(out_expr, sep, str_ty);
        const next_out = try self.concatExpr(with_sep, elem_str, str_ty);
        const next_index = try self.lowLevelExpr(.num_plus, &.{ index_expr, try self.intLiteralExpr(1, u64_ty) }, u64_ty);
        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .continue_ = .{ .values = try self.program.addExprSpan(&.{ next_index, next_out }) } },
        });
    }

    fn primitiveType(self: *Builder, primitive: Type.Primitive) Allocator.Error!Type.TypeId {
        return switch (primitive) {
            .u64 => blk: {
                if (self.u64_ty) |ty| break :blk ty;
                const ty = try self.program.types.add(.{ .primitive = .u64 });
                self.u64_ty = ty;
                break :blk ty;
            },
            .bool => blk: {
                if (self.bool_ty) |ty| break :blk ty;
                const ty = try self.program.types.add(.{ .primitive = .bool });
                self.bool_ty = ty;
                break :blk ty;
            },
            else => try self.program.types.add(.{ .primitive = primitive }),
        };
    }

    fn oneArgFnType(self: *Builder, arg_ty: Type.TypeId, ret_ty: Type.TypeId) Allocator.Error!Type.TypeId {
        const args = [_]Type.TypeId{arg_ty};
        return try self.program.types.add(.{ .func = .{
            .args = try self.program.types.addSpan(&args),
            .ret = ret_ty,
        } });
    }

    fn singleTypeArg(self: *Builder, span: Type.Span, comptime owner: []const u8) Type.TypeId {
        const args = self.program.types.span(span);
        if (args.len != 1) Common.invariant(owner ++ " type reached Monotype inspect lowering without one type argument");
        return args[0];
    }

    fn localExpr(self: *Builder, local: Ast.LocalId, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{ .ty = ty, .data = .{ .local = local } });
    }

    fn bindPat(self: *Builder, local: Ast.LocalId, ty: Type.TypeId) Allocator.Error!Ast.PatId {
        return try self.program.addPat(.{ .ty = ty, .data = .{ .bind = local } });
    }

    fn stringExpr(self: *Builder, text: []const u8, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .str_lit = try self.program.addStringLiteral(text) },
        });
    }

    fn intLiteralExpr(self: *Builder, value: u64, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{
            .ty = ty,
            .data = .{ .int_lit = unsignedIntLiteral(value) },
        });
    }

    fn lowLevelExpr(self: *Builder, op: can.CIR.Expr.LowLevel, args: []const Ast.ExprId, ret_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .low_level = .{
                .op = op,
                .args = try self.program.addExprSpan(args),
            } },
        });
    }

    fn concatExpr(self: *Builder, left: Ast.ExprId, right: Ast.ExprId, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.lowLevelExpr(.str_concat, &.{ left, right }, str_ty);
    }

    fn ifExpr(self: *Builder, cond: Ast.ExprId, then_expr: Ast.ExprId, else_expr: Ast.ExprId, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const branches = [_]Ast.IfBranch{.{ .cond = cond, .body = then_expr }};
        return try self.program.addExpr(.{
            .ty = ty,
            .data = .{ .if_ = .{
                .branches = try self.program.addIfBranchSpan(&branches),
                .final_else = else_expr,
            } },
        });
    }
};

const LoweredTemplateBody = struct {
    args: Ast.Span(Ast.TypedLocal),
    body: Ast.ExprId,
    ret: Type.TypeId,
};

const BinderRestore = struct {
    binder: checked.PatternBinderId,
    previous: ?Ast.LocalId,
};

const BodyContext = struct {
    allocator: Allocator,
    builder: *Builder,
    view: ModuleView,
    owner_template: names.ProcTemplate,
    owner_context_fn_key: names.TypeDigest,
    current_fn_key: names.TypeDigest,
    binders: BinderMap,
    type_bindings: std.AutoHashMap(CheckedTypeAddress, TypeBinding),
    string_literals: []?Ast.StringLiteralId,
    loop_contexts: std.ArrayList(LoopContext),
    type_binding_revision: u64,

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
            .owner_context_fn_key = .{},
            .current_fn_key = .{},
            .binders = BinderMap.init(allocator),
            .type_bindings = std.AutoHashMap(CheckedTypeAddress, TypeBinding).init(allocator),
            .string_literals = string_literals,
            .loop_contexts = .empty,
            .type_binding_revision = 0,
        };
    }

    fn deinit(self: *BodyContext) void {
        self.loop_contexts.deinit(self.allocator);
        self.allocator.free(self.string_literals);
        self.type_bindings.deinit();
        self.binders.deinit();
    }

    fn childContext(self: *BodyContext, current_fn_key: names.TypeDigest) Allocator.Error!BodyContext {
        var child = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template);
        errdefer child.deinit();
        child.owner_context_fn_key = self.owner_context_fn_key;
        child.current_fn_key = current_fn_key;

        var binder_iter = self.binders.iterator();
        while (binder_iter.next()) |entry| {
            try child.binders.put(entry.key_ptr.*, entry.value_ptr.*);
        }

        var type_iter = self.type_bindings.iterator();
        while (type_iter.next()) |entry| {
            try child.type_bindings.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        child.type_binding_revision = self.type_binding_revision;

        try child.loop_contexts.appendSlice(child.allocator, self.loop_contexts.items);

        return child;
    }

    fn checkedTypeCanLower(self: *BodyContext, checked_ty: checked.CheckedTypeId) Allocator.Error!bool {
        var active = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer active.deinit();
        return try self.checkedTypeCanLowerInner(checked_ty, &active);
    }

    fn checkedTypeHasConcreteShape(self: *BodyContext, checked_ty: checked.CheckedTypeId) Allocator.Error!bool {
        var active = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer active.deinit();
        return try self.checkedTypeHasConcreteShapeInner(checked_ty, &active);
    }

    fn checkedTypeHasFixedShape(self: *BodyContext, checked_ty: checked.CheckedTypeId) Allocator.Error!bool {
        var active = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer active.deinit();
        return try self.checkedTypeHasFixedShapeInner(checked_ty, &active);
    }

    fn checkedTypeSliceCanLower(
        self: *BodyContext,
        checked_tys: []const checked.CheckedTypeId,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (checked_tys) |checked_ty| {
            if (!try self.checkedTypeCanLowerInner(checked_ty, active)) return false;
        }
        return true;
    }

    fn checkedTypeCanLowerInner(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        if (self.type_bindings.contains(self.typeAddress(checked_ty))) return true;
        if (active.contains(checked_ty)) return true;
        try active.put(checked_ty, {});
        defer _ = active.remove(checked_ty);

        return switch (self.builder.checkedTypePayload(self.view, checked_ty)) {
            .pending => Common.invariant("pending checked type reached concrete type availability check"),
            .flex,
            .rigid,
            => true,
            .empty_record,
            .empty_tag_union,
            => true,
            .alias => |alias| blk: {
                if (!try self.checkedTypeSliceCanLower(alias.args, active)) break :blk false;
                break :blk try self.checkedTypeCanLowerInner(alias.backing, active);
            },
            .record_unbound => |fields| try self.checkedRecordFieldsCanLower(fields, active),
            .record => |record| blk: {
                if (!try self.checkedRecordFieldsCanLower(record.fields, active)) break :blk false;
                break :blk try self.checkedTypeCanLowerInner(record.ext, active);
            },
            .tuple => |items| try self.checkedTypeSliceCanLower(items, active),
            .function => |function| blk: {
                if (!try self.checkedTypeSliceCanLower(function.args, active)) break :blk false;
                break :blk try self.checkedTypeCanLowerInner(function.ret, active);
            },
            .tag_union => |tag_union| blk: {
                if (!try self.checkedTagsCanLower(tag_union.tags, active)) break :blk false;
                break :blk try self.checkedTypeCanLowerInner(tag_union.ext, active);
            },
            .nominal => |nominal| blk: {
                if (!try self.checkedTypeSliceCanLower(nominal.args, active)) break :blk false;
                switch (nominal.representation) {
                    .builtin => |builtin| switch (checked.builtinRuntimeEncoding(builtin)) {
                        .primitive,
                        .list,
                        .box,
                        => break :blk true,
                        .bool_tag_union => break :blk try self.checkedTypeCanLowerInner(nominal.backing, active),
                    },
                    .opaque_without_backing => break :blk true,
                    else => break :blk try self.checkedTypeCanLowerInner(nominal.backing, active),
                }
            },
        };
    }

    fn checkedRecordFieldsCanLower(
        self: *BodyContext,
        fields: []const checked.CheckedRecordField,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (fields) |field| {
            if (!try self.checkedTypeCanLowerInner(field.ty, active)) return false;
        }
        return true;
    }

    fn checkedTagsCanLower(
        self: *BodyContext,
        tags: []const checked.CheckedTag,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (tags) |tag| {
            if (!try self.checkedTypeSliceCanLower(tag.args, active)) return false;
        }
        return true;
    }

    fn bindTypeToMono(
        self: *BodyContext,
        generic_ty: checked.CheckedTypeId,
        mono_ty: Type.TypeId,
        comptime conflict_message: []const u8,
    ) Allocator.Error!void {
        const address = CheckedTypeAddress{
            .module_bytes = self.view.key.bytes,
            .ty = @intFromEnum(generic_ty),
        };
        if (self.type_bindings.get(address)) |existing| {
            if (!self.sameType(existing.ty, mono_ty)) {
                try self.bindTypeChildrenToMono(generic_ty, mono_ty, conflict_message);
                if (self.sameType(existing.ty, mono_ty)) return;
                if (existing.state == .uninhabited or existing.state == .defaulted) {
                    self.builder.program.types.types.items[@intFromEnum(existing.ty)] = self.builder.program.types.get(mono_ty);
                    var entry = self.type_bindings.getPtr(address) orelse Common.invariant("checked type binding disappeared while completing replaceable type");
                    entry.state = .lowered;
                    self.type_binding_revision += 1;
                    return;
                }
                Common.invariant(conflict_message);
            }
            return;
        }
        try self.type_bindings.put(address, .{ .ty = mono_ty, .state = self.bindingStateFor(generic_ty, mono_ty) });
        self.type_binding_revision += 1;

        try self.bindTypeChildrenToMono(generic_ty, mono_ty, conflict_message);
    }

    fn bindTypeChildrenToMono(
        self: *BodyContext,
        generic_ty: checked.CheckedTypeId,
        mono_ty: Type.TypeId,
        comptime conflict_message: []const u8,
    ) Allocator.Error!void {
        const generic_payload = self.builder.checkedTypePayload(self.view, generic_ty);
        switch (generic_payload) {
            .pending => Common.invariant("pending checked type reached template type substitution"),
            .flex,
            .rigid,
            .empty_record,
            .empty_tag_union,
            => return,
            .alias => |alias| try self.bindTypeToMono(alias.backing, monoAliasBacking(&self.builder.program.types, mono_ty) orelse mono_ty, conflict_message),
            .record_unbound => |fields| try self.bindRecordFieldsToMono(fields, mono_ty, conflict_message),
            .record => |record| try self.bindRecordFieldsToMono(record.fields, mono_ty, conflict_message),
            .tuple => |items| try self.bindTypeSpanToMono(items, self.monoTupleItems(mono_ty), conflict_message),
            .nominal => |nominal| try self.bindNominalArgsToMono(nominal.args, mono_ty, conflict_message),
            .function => |function| {
                switch (self.builder.shapeContent(mono_ty)) {
                    .func => |mono_fn| {
                        try self.bindTypeSpanToMono(function.args, self.builder.program.types.span(mono_fn.args), conflict_message);
                        try self.bindTypeToMono(function.ret, mono_fn.ret, conflict_message);
                    },
                    else => Common.invariant("template type substitution expected a concrete function type"),
                }
            },
            .tag_union => |tag_union| try self.bindTagsToMono(tag_union.tags, mono_ty, conflict_message),
        }
    }

    fn checkedTypeHasConcreteShapeSlice(
        self: *BodyContext,
        checked_tys: []const checked.CheckedTypeId,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (checked_tys) |checked_ty| {
            if (!try self.checkedTypeHasConcreteShapeInner(checked_ty, active)) return false;
        }
        return true;
    }

    fn checkedTypeHasConcreteShapeInner(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        const payload = self.builder.checkedTypePayload(self.view, checked_ty);
        if (self.type_bindings.get(self.typeAddress(checked_ty))) |binding| {
            switch (payload) {
                .flex,
                .rigid,
                => return binding.hasConcreteShape(),
                else => if (!binding.hasConcreteShape()) return false,
            }
        }
        if (active.contains(checked_ty)) return true;
        try active.put(checked_ty, {});
        defer _ = active.remove(checked_ty);

        return switch (payload) {
            .pending => Common.invariant("pending checked type reached concrete type availability check"),
            .flex => |variable| variable.numeric_default_phase != null or variable.row_default != null,
            .rigid => |variable| variable.numeric_default_phase != null or variable.row_default != null,
            .empty_record,
            .empty_tag_union,
            => true,
            .alias => |alias| blk: {
                if (!try self.checkedTypeHasConcreteShapeSlice(alias.args, active)) break :blk false;
                break :blk try self.checkedTypeHasConcreteShapeInner(alias.backing, active);
            },
            .record_unbound => |fields| try self.checkedRecordFieldsHaveConcreteShape(fields, active),
            .record => |record| blk: {
                if (!try self.checkedRecordFieldsHaveConcreteShape(record.fields, active)) break :blk false;
                break :blk try self.checkedTypeHasConcreteShapeInner(record.ext, active);
            },
            .tuple => |items| try self.checkedTypeHasConcreteShapeSlice(items, active),
            .function => |function| blk: {
                if (!try self.checkedTypeHasConcreteShapeSlice(function.args, active)) break :blk false;
                break :blk try self.checkedTypeHasConcreteShapeInner(function.ret, active);
            },
            .tag_union => |tag_union| blk: {
                if (!try self.checkedTagsHaveConcreteShape(tag_union.tags, active)) break :blk false;
                break :blk try self.checkedTypeHasConcreteShapeInner(tag_union.ext, active);
            },
            .nominal => |nominal| blk: {
                if (!try self.checkedTypeHasConcreteShapeSlice(nominal.args, active)) break :blk false;
                switch (nominal.representation) {
                    .builtin => |builtin| switch (checked.builtinRuntimeEncoding(builtin)) {
                        .primitive,
                        .list,
                        .box,
                        => break :blk true,
                        .bool_tag_union => break :blk try self.checkedTypeHasConcreteShapeInner(nominal.backing, active),
                    },
                    .opaque_without_backing => break :blk true,
                    else => break :blk try self.checkedTypeHasConcreteShapeInner(nominal.backing, active),
                }
            },
        };
    }

    fn checkedRecordFieldsHaveConcreteShape(
        self: *BodyContext,
        fields: []const checked.CheckedRecordField,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (fields) |field| {
            if (!try self.checkedTypeHasConcreteShapeInner(field.ty, active)) return false;
        }
        return true;
    }

    fn checkedTagsHaveConcreteShape(
        self: *BodyContext,
        tags: []const checked.CheckedTag,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (tags) |tag| {
            if (!try self.checkedTypeHasConcreteShapeSlice(tag.args, active)) return false;
        }
        return true;
    }

    fn checkedTypeHasFixedShapeSlice(
        self: *BodyContext,
        checked_tys: []const checked.CheckedTypeId,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (checked_tys) |checked_ty| {
            if (!try self.checkedTypeHasFixedShapeInner(checked_ty, active)) return false;
        }
        return true;
    }

    fn checkedTypeHasFixedShapeInner(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        const payload = self.builder.checkedTypePayload(self.view, checked_ty);
        if (self.type_bindings.get(self.typeAddress(checked_ty))) |binding| {
            switch (payload) {
                .flex,
                .rigid,
                => return binding.hasFixedShape(),
                else => if (!binding.hasFixedShape()) return false,
            }
        }
        if (active.contains(checked_ty)) return true;
        try active.put(checked_ty, {});
        defer _ = active.remove(checked_ty);

        return switch (payload) {
            .pending => Common.invariant("pending checked type reached fixed type availability check"),
            .flex,
            .rigid,
            => false,
            .empty_record,
            .empty_tag_union,
            => true,
            .alias => |alias| blk: {
                if (!try self.checkedTypeHasFixedShapeSlice(alias.args, active)) break :blk false;
                break :blk try self.checkedTypeHasFixedShapeInner(alias.backing, active);
            },
            .record_unbound => |fields| try self.checkedRecordFieldsHaveFixedShape(fields, active),
            .record => |record| blk: {
                if (!try self.checkedRecordFieldsHaveFixedShape(record.fields, active)) break :blk false;
                break :blk try self.checkedTypeHasFixedShapeInner(record.ext, active);
            },
            .tuple => |items| try self.checkedTypeHasFixedShapeSlice(items, active),
            .function => |function| blk: {
                if (!try self.checkedTypeHasFixedShapeSlice(function.args, active)) break :blk false;
                break :blk try self.checkedTypeHasFixedShapeInner(function.ret, active);
            },
            .tag_union => |tag_union| blk: {
                if (!try self.checkedTagsHaveFixedShape(tag_union.tags, active)) break :blk false;
                break :blk try self.checkedTypeHasFixedShapeInner(tag_union.ext, active);
            },
            .nominal => |nominal| blk: {
                if (!try self.checkedTypeHasFixedShapeSlice(nominal.args, active)) break :blk false;
                switch (nominal.representation) {
                    .builtin => |builtin| switch (checked.builtinRuntimeEncoding(builtin)) {
                        .primitive,
                        .list,
                        .box,
                        => break :blk true,
                        .bool_tag_union => break :blk try self.checkedTypeHasFixedShapeInner(nominal.backing, active),
                    },
                    .opaque_without_backing => break :blk true,
                    else => break :blk try self.checkedTypeHasFixedShapeInner(nominal.backing, active),
                }
            },
        };
    }

    fn checkedRecordFieldsHaveFixedShape(
        self: *BodyContext,
        fields: []const checked.CheckedRecordField,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (fields) |field| {
            if (!try self.checkedTypeHasFixedShapeInner(field.ty, active)) return false;
        }
        return true;
    }

    fn checkedTagsHaveFixedShape(
        self: *BodyContext,
        tags: []const checked.CheckedTag,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (tags) |tag| {
            if (!try self.checkedTypeHasFixedShapeSlice(tag.args, active)) return false;
        }
        return true;
    }

    fn bindTypeSpanToMono(
        self: *BodyContext,
        generic_tys: []const checked.CheckedTypeId,
        mono_tys: []const Type.TypeId,
        comptime conflict_message: []const u8,
    ) Allocator.Error!void {
        if (generic_tys.len != mono_tys.len) Common.invariant("template type substitution arity mismatch");
        for (generic_tys, mono_tys) |generic_ty, mono_ty| {
            try self.bindTypeToMono(generic_ty, mono_ty, conflict_message);
        }
    }

    fn bindRecordFieldsToMono(
        self: *BodyContext,
        fields: []const checked.CheckedRecordField,
        mono_ty: Type.TypeId,
        comptime conflict_message: []const u8,
    ) Allocator.Error!void {
        for (fields) |field| {
            try self.bindTypeToMono(field.ty, self.monoRecordField(mono_ty, field.name), conflict_message);
        }
    }

    fn bindTagsToMono(
        self: *BodyContext,
        tags: []const checked.CheckedTag,
        mono_ty: Type.TypeId,
        comptime conflict_message: []const u8,
    ) Allocator.Error!void {
        for (tags) |tag| {
            try self.bindTypeSpanToMono(tag.args, self.monoTagArgs(mono_ty, tag.name), conflict_message);
        }
    }

    fn bindNominalArgsToMono(
        self: *BodyContext,
        generic_args: []const checked.CheckedTypeId,
        mono_ty: Type.TypeId,
        comptime conflict_message: []const u8,
    ) Allocator.Error!void {
        switch (self.builder.program.types.get(mono_ty)) {
            .named => |named| try self.bindTypeSpanToMono(generic_args, self.builder.program.types.span(named.args), conflict_message),
            .list => |elem_ty| {
                if (generic_args.len != 1) Common.invariant("template type substitution List arity mismatch");
                try self.bindTypeToMono(generic_args[0], elem_ty, conflict_message);
            },
            .box => |elem_ty| {
                if (generic_args.len != 1) Common.invariant("template type substitution Box arity mismatch");
                try self.bindTypeToMono(generic_args[0], elem_ty, conflict_message);
            },
            .primitive,
            .tag_union,
            .record,
            .tuple,
            .func,
            .erased,
            .zst,
            => {
                if (generic_args.len != 0) Common.invariant("template type substitution expected a concrete nominal type with arguments");
            },
        }
    }

    fn monoTupleItems(self: *BodyContext, mono_ty: Type.TypeId) []const Type.TypeId {
        return switch (self.builder.shapeContent(mono_ty)) {
            .tuple => |items| self.builder.program.types.span(items),
            else => Common.invariant("template type substitution expected a concrete tuple type"),
        };
    }

    fn monoRecordField(
        self: *BodyContext,
        mono_ty: Type.TypeId,
        generic_field: names.RecordFieldNameId,
    ) Type.TypeId {
        return switch (self.builder.shapeContent(mono_ty)) {
            .record => |record| {
                const wanted = self.view.names.recordFieldLabelText(generic_field);
                for (self.builder.program.types.fieldSpan(record)) |field| {
                    if (Ident.textEql(wanted, self.builder.program.names.recordFieldLabelText(field.name))) return field.ty;
                }
                Common.invariant("template type substitution record field was absent from concrete type");
            },
            else => Common.invariant("template type substitution expected a concrete record type"),
        };
    }

    fn monoTagArgs(
        self: *BodyContext,
        mono_ty: Type.TypeId,
        generic_tag: names.TagNameId,
    ) []const Type.TypeId {
        return switch (self.builder.shapeContent(mono_ty)) {
            .tag_union => |tag_union| {
                const wanted = self.view.names.tagLabelText(generic_tag);
                for (self.builder.program.types.tagSpan(tag_union)) |tag| {
                    if (Ident.textEql(wanted, self.builder.program.names.tagLabelText(tag.name))) {
                        return self.builder.program.types.span(tag.payloads);
                    }
                }
                Common.invariant("template type substitution tag was absent from concrete type");
            },
            else => Common.invariant("template type substitution expected a concrete tag-union type"),
        };
    }

    fn monoAliasBacking(types: *const Type.Store, mono_ty: Type.TypeId) ?Type.TypeId {
        return switch (types.get(mono_ty)) {
            .named => |named| if (named.backing) |backing| backing.ty else null,
            else => null,
        };
    }

    fn bindKnownType(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        mono_ty: Type.TypeId,
    ) Allocator.Error!void {
        try self.bindTypeToMono(checked_ty, mono_ty, "checked type was bound to two monotype types in one body context");
    }

    fn bindingStateFor(self: *BodyContext, checked_ty: checked.CheckedTypeId, mono_ty: Type.TypeId) TypeState {
        const payload = self.builder.checkedTypePayload(self.view, checked_ty);
        if (checkedTypePayloadIsOpenVariable(payload) and self.monoTypeIsUninhabited(mono_ty)) return .uninhabited;
        if (checkedTypePayloadHasMonoDefault(payload) and self.monoTypeIsNumericDefault(mono_ty)) return .defaulted;
        return .lowered;
    }

    fn monoTypeIsUninhabited(self: *BodyContext, ty: Type.TypeId) bool {
        return switch (self.builder.shapeContent(ty)) {
            .tag_union => |tags| tags.len == 0,
            else => false,
        };
    }

    fn monoTypeIsNumericDefault(self: *BodyContext, ty: Type.TypeId) bool {
        return switch (self.builder.shapeContent(ty)) {
            .primitive => |primitive| primitive == .dec,
            else => false,
        };
    }

    fn exprHasNumericDefault(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error!bool {
        return try self.checkedTypeHasNumericDefault(self.view.bodies.exprs[@intFromEnum(expr_id)].ty);
    }

    fn checkedTypeHasNumericDefault(self: *BodyContext, checked_ty: checked.CheckedTypeId) Allocator.Error!bool {
        var active = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer active.deinit();
        return try self.typeHasNumericDefault(checked_ty, &active);
    }

    fn typeSpanHasNumericDefault(
        self: *BodyContext,
        checked_tys: []const checked.CheckedTypeId,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (checked_tys) |checked_ty| {
            if (try self.typeHasNumericDefault(checked_ty, active)) return true;
        }
        return false;
    }

    fn recordFieldsHaveNumericDefault(
        self: *BodyContext,
        fields: []const checked.CheckedRecordField,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (fields) |field| {
            if (try self.typeHasNumericDefault(field.ty, active)) return true;
        }
        return false;
    }

    fn tagsHaveNumericDefault(
        self: *BodyContext,
        tags: []const checked.CheckedTag,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (tags) |tag| {
            if (try self.typeSpanHasNumericDefault(tag.args, active)) return true;
        }
        return false;
    }

    fn typeHasNumericDefault(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        active: *std.AutoHashMap(checked.CheckedTypeId, void),
    ) Allocator.Error!bool {
        const payload = self.builder.checkedTypePayload(self.view, checked_ty);
        if (self.type_bindings.get(self.typeAddress(checked_ty))) |binding| {
            switch (payload) {
                .flex,
                .rigid,
                => return binding.hasNumericDefault(),
                else => {},
            }
        }
        if (active.contains(checked_ty)) return false;
        try active.put(checked_ty, {});
        defer _ = active.remove(checked_ty);

        return switch (payload) {
            .pending => Common.invariant("pending checked type reached numeric default detection"),
            .flex => |variable| variable.numeric_default_phase != null,
            .rigid => |variable| variable.numeric_default_phase != null,
            .empty_record,
            .empty_tag_union,
            => false,
            .alias => |alias| (try self.typeSpanHasNumericDefault(alias.args, active)) or
                try self.typeHasNumericDefault(alias.backing, active),
            .record_unbound => |fields| try self.recordFieldsHaveNumericDefault(fields, active),
            .record => |record| (try self.recordFieldsHaveNumericDefault(record.fields, active)) or
                try self.typeHasNumericDefault(record.ext, active),
            .tuple => |items| try self.typeSpanHasNumericDefault(items, active),
            .function => |function| (try self.typeSpanHasNumericDefault(function.args, active)) or
                try self.typeHasNumericDefault(function.ret, active),
            .tag_union => |tag_union| (try self.tagsHaveNumericDefault(tag_union.tags, active)) or
                try self.typeHasNumericDefault(tag_union.ext, active),
            .nominal => |nominal| (try self.typeSpanHasNumericDefault(nominal.args, active)) or
                switch (nominal.representation) {
                    .builtin => |builtin| switch (checked.builtinRuntimeEncoding(builtin)) {
                        .primitive,
                        .list,
                        .box,
                        => false,
                        .bool_tag_union => try self.typeHasNumericDefault(nominal.backing, active),
                    },
                    .opaque_without_backing => false,
                    else => try self.typeHasNumericDefault(nominal.backing, active),
                },
        };
    }

    fn typeAddress(self: *BodyContext, checked_ty: checked.CheckedTypeId) CheckedTypeAddress {
        return .{
            .module_bytes = self.view.key.bytes,
            .ty = @intFromEnum(checked_ty),
        };
    }

    fn reserveType(self: *BodyContext, checked_ty: checked.CheckedTypeId) Allocator.Error!Type.TypeId {
        const address = self.typeAddress(checked_ty);
        if (self.type_bindings.get(address)) |existing| {
            return existing.ty;
        }
        const reserved = try self.builder.program.types.add(.zst);
        try self.type_bindings.put(address, .{ .ty = reserved, .state = .reserved });
        self.type_binding_revision += 1;
        return reserved;
    }

    fn lowerType(self: *BodyContext, checked_ty: checked.CheckedTypeId) Allocator.Error!Type.TypeId {
        const address = self.typeAddress(checked_ty);
        if (self.type_bindings.get(address)) |binding| {
            switch (binding.state) {
                .reserved => try self.finishType(checked_ty, binding.ty, address),
                .uninhabited,
                .defaulted,
                .lowering,
                .lowered,
                => {},
            }
            return binding.ty;
        }

        const reserved = try self.reserveType(checked_ty);
        try self.finishType(checked_ty, reserved, address);
        return reserved;
    }

    fn finishType(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        mono_ty: Type.TypeId,
        address: CheckedTypeAddress,
    ) Allocator.Error!void {
        {
            const binding = self.type_bindings.getPtr(address) orelse Common.invariant("checked type finished without a monotype binding");
            if (binding.ty != mono_ty) Common.invariant("checked type binding changed while lowering monotype");
            switch (binding.state) {
                .lowering,
                .uninhabited,
                .defaulted,
                .lowered,
                => return,
                .reserved => {},
            }
            binding.state = .lowering;
            self.type_binding_revision += 1;
        }

        const payload = self.builder.checkedTypePayload(self.view, checked_ty);
        switch (payload) {
            .function => |function| try self.publishFunctionType(mono_ty, function),
            .tuple => |items| try self.publishTupleType(mono_ty, items),
            .record_unbound => |fields| try self.publishRecordType(mono_ty, fields),
            .record => |record| try self.publishRecordRowType(mono_ty, record.fields, record.ext),
            .tag_union => |tag_union| try self.publishTagUnionRowType(mono_ty, tag_union.tags, tag_union.ext),
            .alias => |alias| try self.publishAliasType(checked_ty, mono_ty, alias),
            .nominal => |nominal| try self.publishNominalType(checked_ty, mono_ty, nominal),
            else => self.builder.program.types.types.items[@intFromEnum(mono_ty)] = try self.lowerTypePayload(checked_ty, payload),
        }

        const binding = self.type_bindings.getPtr(address) orelse Common.invariant("checked type finished without a monotype binding");
        if (binding.ty != mono_ty) Common.invariant("checked type binding changed while lowering monotype");
        if (binding.state != .lowering) Common.invariant("checked type binding state changed while lowering monotype");
        binding.state = self.bindingStateFor(checked_ty, mono_ty);
        self.type_binding_revision += 1;
    }

    fn publishFunctionType(
        self: *BodyContext,
        mono_ty: Type.TypeId,
        function: checked.CheckedFunctionType,
    ) Allocator.Error!void {
        const args = try self.reserveTypeSlice(function.args);
        defer self.allocator.free(args);
        const ret = try self.reserveType(function.ret);
        self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{ .func = .{
            .args = try self.builder.program.types.addSpan(args),
            .ret = ret,
        } };
        try self.finishTypeSlice(function.args);
        _ = try self.lowerType(function.ret);
    }

    fn publishTupleType(
        self: *BodyContext,
        mono_ty: Type.TypeId,
        items: []const checked.CheckedTypeId,
    ) Allocator.Error!void {
        const reserved_items = try self.reserveTypeSlice(items);
        defer self.allocator.free(reserved_items);
        self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{
            .tuple = try self.builder.program.types.addSpan(reserved_items),
        };
        try self.finishTypeSlice(items);
    }

    fn publishRecordType(
        self: *BodyContext,
        mono_ty: Type.TypeId,
        fields: []const checked.CheckedRecordField,
    ) Allocator.Error!void {
        var lowered = std.ArrayList(Type.Field).empty;
        defer lowered.deinit(self.allocator);
        var checked_fields = std.ArrayList(checked.CheckedTypeId).empty;
        defer checked_fields.deinit(self.allocator);
        try self.appendReservedRecordFields(&lowered, &checked_fields, fields);
        std.mem.sort(Type.Field, lowered.items, self.builder, recordFieldLessThan);
        self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{
            .record = try self.builder.program.types.addFields(lowered.items),
        };
        try self.finishTypeList(checked_fields.items);
    }

    fn publishRecordRowType(
        self: *BodyContext,
        mono_ty: Type.TypeId,
        head: []const checked.CheckedRecordField,
        ext: checked.CheckedTypeId,
    ) Allocator.Error!void {
        var fields = std.ArrayList(Type.Field).empty;
        defer fields.deinit(self.allocator);
        var checked_fields = std.ArrayList(checked.CheckedTypeId).empty;
        defer checked_fields.deinit(self.allocator);
        try self.appendReservedRecordFields(&fields, &checked_fields, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (true) {
            if (seen.contains(current)) Common.invariant("checked record row chain was cyclic at Monotype lowering");
            try seen.put(current, {});

            const payload = self.builder.checkedTypePayload(self.view, current);
            switch (payload) {
                .alias => |alias| current = alias.backing,
                .empty_record => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_record) break;
                    Common.invariant("open non-record checked row reached Monotype record lowering");
                },
                .record_unbound => |tail_fields| {
                    try self.appendReservedRecordFields(&fields, &checked_fields, tail_fields);
                    break;
                },
                .record => |record| {
                    try self.appendReservedRecordFields(&fields, &checked_fields, record.fields);
                    current = record.ext;
                },
                else => Common.invariant("open or non-record checked row reached Monotype record lowering"),
            }
        }

        std.mem.sort(Type.Field, fields.items, self.builder, recordFieldLessThan);
        assertNoDuplicateRecordFields(self.builder, fields.items, "checked record row had duplicate fields at Monotype lowering");
        self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{
            .record = try self.builder.program.types.addFields(fields.items),
        };
        try self.finishTypeList(checked_fields.items);
    }

    fn appendReservedRecordFields(
        self: *BodyContext,
        out: *std.ArrayList(Type.Field),
        checked_fields: *std.ArrayList(checked.CheckedTypeId),
        fields: []const checked.CheckedRecordField,
    ) Allocator.Error!void {
        for (fields) |field| {
            try out.append(self.allocator, .{
                .name = try self.builder.recordFieldName(self.view, field.name),
                .ty = try self.reserveType(field.ty),
            });
            try checked_fields.append(self.allocator, field.ty);
        }
    }

    fn publishTagUnionRowType(
        self: *BodyContext,
        mono_ty: Type.TypeId,
        head: []const checked.CheckedTag,
        ext: checked.CheckedTypeId,
    ) Allocator.Error!void {
        var tags = std.ArrayList(Type.Tag).empty;
        defer tags.deinit(self.allocator);
        var checked_payloads = std.ArrayList(checked.CheckedTypeId).empty;
        defer checked_payloads.deinit(self.allocator);
        try self.appendReservedTags(&tags, &checked_payloads, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (true) {
            if (seen.contains(current)) Common.invariant("checked tag-union row chain was cyclic at Monotype lowering");
            try seen.put(current, {});

            const payload = self.builder.checkedTypePayload(self.view, current);
            switch (payload) {
                .alias => |alias| current = alias.backing,
                .empty_tag_union => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_tag_union) break;
                    Common.invariant("open non-tag-union checked row reached Monotype tag-union lowering");
                },
                .tag_union => |tag_union| {
                    try self.appendReservedTags(&tags, &checked_payloads, tag_union.tags);
                    current = tag_union.ext;
                },
                else => Common.invariant("open or non-tag-union checked row reached Monotype tag-union lowering"),
            }
        }

        std.mem.sort(Type.Tag, tags.items, self.builder, tagLessThan);
        assertNoDuplicateTags(self.builder, tags.items, "checked tag-union row had duplicate tags at Monotype lowering");
        self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{
            .tag_union = try self.builder.program.types.addTags(tags.items),
        };
        try self.finishTypeList(checked_payloads.items);
    }

    fn appendReservedTags(
        self: *BodyContext,
        out: *std.ArrayList(Type.Tag),
        checked_payloads: *std.ArrayList(checked.CheckedTypeId),
        tags: []const checked.CheckedTag,
    ) Allocator.Error!void {
        for (tags) |tag| {
            const payloads = try self.reserveTypeSlice(tag.args);
            defer self.allocator.free(payloads);
            try out.append(self.allocator, .{
                .name = try self.builder.tagName(self.view, tag.name),
                .checked_name = tag.name,
                .payloads = try self.builder.program.types.addSpan(payloads),
            });
            try checked_payloads.appendSlice(self.allocator, tag.args);
        }
    }

    fn publishAliasType(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        mono_ty: Type.TypeId,
        alias: checked.CheckedAliasType,
    ) Allocator.Error!void {
        const args = try self.reserveTypeSlice(alias.args);
        defer self.allocator.free(args);
        const backing = try self.reserveType(alias.backing);
        self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{ .named = .{
            .named_type = .{ .module = self.builder.declaredModuleForAlias(self.view, alias), .ty = checked_ty },
            .def = try self.builder.typeDef(self.view, alias.origin_module, alias.name),
            .kind = .alias,
            .args = try self.builder.program.types.addSpan(args),
            .backing = .{
                .ty = backing,
                .use = .inspectable,
            },
        } };
        try self.finishTypeSlice(alias.args);
        _ = try self.lowerType(alias.backing);
    }

    fn publishNominalType(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        mono_ty: Type.TypeId,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!void {
        switch (nominal.representation) {
            .builtin => |builtin| switch (checked.builtinRuntimeEncoding(builtin)) {
                .primitive => |primitive| {
                    self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{ .primitive = primitive };
                    return;
                },
                .bool_tag_union => {},
                .list => {
                    if (nominal.args.len != 1) Common.invariant("checked List nominal must have exactly one type argument");
                    const elem_ty = try self.reserveType(nominal.args[0]);
                    self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{ .list = elem_ty };
                    try self.finishTypeSlice(nominal.args);
                    return;
                },
                .box => {
                    if (nominal.args.len != 1) Common.invariant("checked Box nominal must have exactly one type argument");
                    const elem_ty = try self.reserveType(nominal.args[0]);
                    self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{ .box = elem_ty };
                    try self.finishTypeSlice(nominal.args);
                    return;
                },
            },
            else => {},
        }

        const args = try self.reserveTypeSlice(nominal.args);
        defer self.allocator.free(args);
        try self.bindNominalDeclarationFormalsToMonoArgs(nominal, args);
        const backing_use: Type.BackingUse = if (nominal.is_opaque) .runtime_layout_only else .inspectable;
        const backing: ?Type.NamedBacking = switch (nominal.representation) {
            .opaque_without_backing => null,
            else => .{
                .ty = try self.reserveType(self.nominalBackingRoot(nominal)),
                .use = backing_use,
            },
        };
        self.builder.program.types.types.items[@intFromEnum(mono_ty)] = .{ .named = .{
            .named_type = .{ .module = self.builder.declaredModuleForNominal(self.view, nominal), .ty = checked_ty },
            .def = try self.builder.typeDef(self.view, nominal.origin_module, nominal.name),
            .kind = if (nominal.is_opaque) .@"opaque" else .nominal,
            .builtin_owner = builtinOwner(nominal.builtin),
            .args = try self.builder.program.types.addSpan(args),
            .backing = backing,
        } };
        try self.finishTypeSlice(nominal.args);
        switch (nominal.representation) {
            .opaque_without_backing => {},
            else => _ = try self.lowerType(self.nominalBackingRoot(nominal)),
        }
    }

    const NominalDeclarationSource = struct {
        view: ModuleView,
        declaration: checked.CheckedNominalDeclaration,
    };

    fn bindNominalDeclarationFormalsToMonoArgs(
        self: *BodyContext,
        nominal: checked.CheckedNominalType,
        mono_args: []const Type.TypeId,
    ) Allocator.Error!void {
        const source = self.nominalDeclarationSource(nominal.representation) orelse return;
        if (source.declaration.formal_args.len != mono_args.len) {
            Common.invariant("checked nominal declaration arity differed from nominal type use");
        }
        for (source.declaration.formal_args, mono_args) |formal, mono_arg| {
            const current_formal = self.checkedTypeInCurrentView(source.view, formal);
            try self.bindTypeToMono(
                current_formal,
                mono_arg,
                "checked nominal backing formal type mapped one checked type to two monotype types",
            );
        }
    }

    fn nominalDeclarationSource(
        self: *BodyContext,
        representation: anytype,
    ) ?NominalDeclarationSource {
        return switch (representation) {
            .local_declaration => |id| .{
                .view = self.view,
                .declaration = self.view.types.nominalDeclarationById(id),
            },
            .imported_declaration,
            .builtin,
            .local_box_payload_capability,
            .imported_box_payload_capability,
            .opaque_without_backing,
            => null,
        };
    }

    fn nominalBackingRoot(
        self: *BodyContext,
        nominal: checked.CheckedNominalType,
    ) checked.CheckedTypeId {
        return switch (nominal.representation) {
            .local_box_payload_capability => |capability| self.view.interface_capabilities.boxPayloadCapability(capability.capability).backing_ty,
            .imported_box_payload_capability => |capability| blk: {
                const source_view = self.builder.moduleForId(checked.importedBoxPayloadCapabilityModuleId(capability));
                const backing = source_view.interface_capabilities.boxPayloadCapability(capability.capability).backing_ty;
                break :blk self.checkedTypeInCurrentView(source_view, backing);
            },
            else => nominal.backing,
        };
    }

    fn checkedTypeInCurrentView(
        self: *BodyContext,
        source_view: ModuleView,
        source_ty: checked.CheckedTypeId,
    ) checked.CheckedTypeId {
        if (moduleBytesEqual(source_view.key.bytes, self.view.key.bytes)) return source_ty;
        return self.view.types.rootForKey(source_view.types.rootKey(source_ty)) orelse
            Common.invariant("imported nominal declaration formal was not projected into the current checked type store");
    }

    fn reserveTypeSlice(
        self: *BodyContext,
        checked_tys: []const checked.CheckedTypeId,
    ) Allocator.Error![]Type.TypeId {
        const out = try self.allocator.alloc(Type.TypeId, checked_tys.len);
        errdefer self.allocator.free(out);
        for (checked_tys, 0..) |checked_ty, i| {
            out[i] = try self.reserveType(checked_ty);
        }
        return out;
    }

    fn finishTypeSlice(
        self: *BodyContext,
        checked_tys: []const checked.CheckedTypeId,
    ) Allocator.Error!void {
        for (checked_tys) |checked_ty| {
            _ = try self.lowerType(checked_ty);
        }
    }

    fn finishTypeList(
        self: *BodyContext,
        checked_tys: []const checked.CheckedTypeId,
    ) Allocator.Error!void {
        for (checked_tys) |checked_ty| {
            _ = try self.lowerType(checked_ty);
        }
    }

    fn lowerTypePayload(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        payload: checked.CheckedTypePayload,
    ) Allocator.Error!Type.Content {
        return switch (payload) {
            .pending => Common.invariant("pending checked type reached contextual Monotype lowering"),
            .flex => |variable| self.builder.lowerCheckedTypeVariable(variable),
            .rigid => |variable| self.builder.lowerCheckedTypeVariable(variable),
            .empty_record => .{ .record = .empty() },
            .empty_tag_union => .{ .tag_union = .empty() },
            .record_unbound => |fields| try self.lowerRecordFields(fields),
            .record => |record| try self.lowerRecordRow(record.fields, record.ext),
            .tuple => |items| blk: {
                const lowered = try self.lowerTypeSlice(items);
                defer self.allocator.free(lowered);
                break :blk .{ .tuple = try self.builder.program.types.addSpan(lowered) };
            },
            .tag_union => |tag_union| try self.lowerTagUnionRow(tag_union.tags, tag_union.ext),
            .function => |fn_ty| blk: {
                const args = try self.lowerTypeSlice(fn_ty.args);
                defer self.allocator.free(args);
                break :blk .{ .func = .{
                    .args = try self.builder.program.types.addSpan(args),
                    .ret = try self.lowerType(fn_ty.ret),
                } };
            },
            .alias => |alias| blk: {
                const args = try self.lowerTypeSlice(alias.args);
                defer self.allocator.free(args);
                break :blk .{ .named = .{
                    .named_type = .{ .module = self.builder.declaredModuleForAlias(self.view, alias), .ty = checked_ty },
                    .def = try self.builder.typeDef(self.view, alias.origin_module, alias.name),
                    .kind = .alias,
                    .args = try self.builder.program.types.addSpan(args),
                    .backing = .{
                        .ty = try self.lowerType(alias.backing),
                        .use = .inspectable,
                    },
                } };
            },
            .nominal => |nominal| blk: {
                switch (nominal.representation) {
                    .builtin => |builtin| switch (checked.builtinRuntimeEncoding(builtin)) {
                        .primitive => |primitive| break :blk .{ .primitive = primitive },
                        .bool_tag_union => {},
                        .list => {
                            if (nominal.args.len != 1) Common.invariant("checked List nominal must have exactly one type argument");
                            break :blk .{ .list = try self.lowerType(nominal.args[0]) };
                        },
                        .box => {
                            if (nominal.args.len != 1) Common.invariant("checked Box nominal must have exactly one type argument");
                            break :blk .{ .box = try self.lowerType(nominal.args[0]) };
                        },
                    },
                    else => {},
                }

                const args = try self.lowerTypeSlice(nominal.args);
                defer self.allocator.free(args);
                const backing_use: Type.BackingUse = if (nominal.is_opaque) .runtime_layout_only else .inspectable;
                break :blk .{ .named = .{
                    .named_type = .{ .module = self.builder.declaredModuleForNominal(self.view, nominal), .ty = checked_ty },
                    .def = try self.builder.typeDef(self.view, nominal.origin_module, nominal.name),
                    .kind = if (nominal.is_opaque) .@"opaque" else .nominal,
                    .builtin_owner = builtinOwner(nominal.builtin),
                    .args = try self.builder.program.types.addSpan(args),
                    .backing = switch (nominal.representation) {
                        .opaque_without_backing => null,
                        else => .{
                            .ty = try self.lowerType(nominal.backing),
                            .use = backing_use,
                        },
                    },
                } };
            },
        };
    }

    fn lowerTypeSlice(self: *BodyContext, checked_tys: []const checked.CheckedTypeId) Allocator.Error![]Type.TypeId {
        const out = try self.allocator.alloc(Type.TypeId, checked_tys.len);
        errdefer self.allocator.free(out);
        for (checked_tys, 0..) |ty, i| {
            out[i] = try self.lowerType(ty);
        }
        return out;
    }

    fn lowerRecordFields(self: *BodyContext, fields: []const checked.CheckedRecordField) Allocator.Error!Type.Content {
        var lowered = std.ArrayList(Type.Field).empty;
        defer lowered.deinit(self.allocator);
        try self.appendRecordFields(&lowered, fields);
        return .{ .record = try self.builder.program.types.addFields(lowered.items) };
    }

    fn lowerRecordRow(
        self: *BodyContext,
        head: []const checked.CheckedRecordField,
        ext: checked.CheckedTypeId,
    ) Allocator.Error!Type.Content {
        var fields = std.ArrayList(Type.Field).empty;
        defer fields.deinit(self.allocator);
        try self.appendRecordFields(&fields, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (true) {
            if (seen.contains(current)) Common.invariant("checked record row chain was cyclic at contextual Monotype lowering");
            try seen.put(current, {});

            switch (self.builder.checkedTypePayload(self.view, current)) {
                .alias => |alias| current = alias.backing,
                .empty_record => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_record) break;
                    Common.invariant("open non-record checked row reached contextual Monotype record lowering");
                },
                .record_unbound => |tail_fields| {
                    try self.appendRecordFields(&fields, tail_fields);
                    break;
                },
                .record => |record| {
                    try self.appendRecordFields(&fields, record.fields);
                    current = record.ext;
                },
                else => Common.invariant("open or non-record checked row reached contextual Monotype record lowering"),
            }
        }

        std.mem.sort(Type.Field, fields.items, self.builder, recordFieldLessThan);
        assertNoDuplicateRecordFields(self.builder, fields.items, "checked record row had duplicate fields at contextual Monotype lowering");
        return .{ .record = try self.builder.program.types.addFields(fields.items) };
    }

    fn appendRecordFields(
        self: *BodyContext,
        out: *std.ArrayList(Type.Field),
        fields: []const checked.CheckedRecordField,
    ) Allocator.Error!void {
        for (fields) |field| {
            try out.append(self.allocator, .{
                .name = try self.builder.recordFieldName(self.view, field.name),
                .ty = try self.lowerType(field.ty),
            });
        }
    }

    fn lowerTagUnionRow(
        self: *BodyContext,
        head: []const checked.CheckedTag,
        ext: checked.CheckedTypeId,
    ) Allocator.Error!Type.Content {
        var tags = std.ArrayList(Type.Tag).empty;
        defer tags.deinit(self.allocator);
        try self.appendTags(&tags, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (true) {
            if (seen.contains(current)) Common.invariant("checked tag row chain was cyclic at contextual Monotype lowering");
            try seen.put(current, {});

            switch (self.builder.checkedTypePayload(self.view, current)) {
                .alias => |alias| current = alias.backing,
                .empty_tag_union => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_tag_union) break;
                    Common.invariant("open non-tag checked row reached contextual Monotype tag-union lowering");
                },
                .tag_union => |tag_union| {
                    try self.appendTags(&tags, tag_union.tags);
                    current = tag_union.ext;
                },
                else => Common.invariant("open or non-tag checked row reached contextual Monotype tag-union lowering"),
            }
        }

        std.mem.sort(Type.Tag, tags.items, self.builder, tagLessThan);
        assertNoDuplicateTags(self.builder, tags.items, "checked tag row had duplicate tags at contextual Monotype lowering");
        return .{ .tag_union = try self.builder.program.types.addTags(tags.items) };
    }

    fn appendTags(
        self: *BodyContext,
        out: *std.ArrayList(Type.Tag),
        tags: []const checked.CheckedTag,
    ) Allocator.Error!void {
        for (tags) |tag| {
            const payloads = try self.lowerTypeSlice(tag.args);
            defer self.allocator.free(payloads);
            try out.append(self.allocator, .{
                .name = try self.builder.tagName(self.view, tag.name),
                .checked_name = tag.name,
                .payloads = try self.builder.program.types.addSpan(payloads),
            });
        }
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
                    .lambda => |lambda| try self.lowerLambdaTemplate(lambda, fn_ty),
                    .closure => |closure| blk: {
                        if (closure.captures.len != 0) {
                            Common.invariant("checked procedure template root closure had captures");
                        }
                        const lambda_expr = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
                        break :blk switch (lambda_expr.data) {
                            .lambda => |lambda| try self.lowerLambdaTemplate(lambda, fn_ty),
                            else => Common.invariant("checked procedure template root closure did not point at a lambda"),
                        };
                    },
                    .hosted_lambda => Common.invariant("hosted lambda template must lower through hosted metadata, not source lambda body"),
                    else => .{
                        .args = .empty(),
                        .body = try self.lowerExprAtType(body.root_expr, ret_ty),
                        .ret = ret_ty,
                    },
                };
            },
            .entry_wrapper => |wrapper_id| {
                const wrapper = self.view.entry_wrappers.get(wrapper_id);
                return .{
                    .args = .empty(),
                    .body = try self.lowerExprAtType(wrapper.body_expr, ret_ty),
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
        const body = try self.builder.inspectCall(local_expr, arg_tys[0], ret_ty);
        return .{
            .args = try self.builder.program.addTypedLocalSpan(&.{typed_arg}),
            .body = body,
            .ret = ret_ty,
        };
    }

    fn lowerLambdaTemplate(self: *BodyContext, lambda: anytype, fn_ty: Type.TypeId) Allocator.Error!LoweredTemplateBody {
        const fn_content = self.builder.program.types.get(fn_ty);
        const fn_data = switch (fn_content) {
            .func => |data| data,
            else => Common.invariant("lambda template had a non-function type"),
        };
        const arg_tys = self.builder.program.types.span(fn_data.args);
        if (arg_tys.len != lambda.args.len) Common.invariant("lambda template arity differs from concrete function type");

        const lowered = try self.lowerLambdaArgsAndBody(lambda.args, arg_tys, lambda.body, fn_data.ret);
        return .{
            .args = lowered.args,
            .body = lowered.body,
            .ret = fn_data.ret,
        };
    }

    fn lowerLambdaArgsAndBody(
        self: *BodyContext,
        checked_args: []const checked.CheckedPatternId,
        arg_tys: []const Type.TypeId,
        checked_body: checked.CheckedExprId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!LoweredLambdaArgs {
        if (arg_tys.len != checked_args.len) Common.invariant("lambda arity differs from concrete function type");

        const args = try self.allocator.alloc(Ast.TypedLocal, checked_args.len);
        defer self.allocator.free(args);
        var arg_lets = std.ArrayList(LambdaArgLet).empty;
        defer arg_lets.deinit(self.allocator);

        for (checked_args, arg_tys, 0..) |pattern_id, arg_ty, i| {
            const pat = try self.lowerPatternAtType(pattern_id, arg_ty);
            switch (self.builder.program.pats.items[@intFromEnum(pat)].data) {
                .bind => |local| args[i] = .{ .local = local, .ty = arg_ty },
                else => {
                    const local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_ty);
                    args[i] = .{ .local = local, .ty = arg_ty };
                    const value = try self.builder.program.addExpr(.{ .ty = arg_ty, .data = .{ .local = local } });
                    try arg_lets.append(self.allocator, .{ .pat = pat, .value = value });
                },
            }
        }

        var body = try self.lowerExprAtType(checked_body, ret_ty);
        var remaining = arg_lets.items.len;
        while (remaining > 0) {
            remaining -= 1;
            const arg_let = arg_lets.items[remaining];
            body = try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .let_ = .{
                .bind = arg_let.pat,
                .value = arg_let.value,
                .rest = body,
            } } });
        }

        return .{
            .args = try self.builder.program.addTypedLocalSpan(args),
            .body = body,
        };
    }

    fn lowerNestedFunction(self: *BodyContext, expr_id: checked.CheckedExprId, fn_ty: Type.TypeId) Allocator.Error!LoweredTemplateBody {
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .lambda => |lambda| try self.lowerNestedLambdaTemplate(lambda, fn_ty),
            .closure => |closure| blk: {
                const lambda_expr = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
                break :blk switch (lambda_expr.data) {
                    .lambda => |lambda| try self.lowerNestedLambdaTemplate(lambda, fn_ty),
                    else => Common.invariant("checked closure did not point at a lambda expression"),
                };
            },
            else => Common.invariant("local procedure site did not point at a lambda or closure"),
        };
    }

    fn lowerNestedLambdaTemplate(self: *BodyContext, lambda: anytype, fn_ty: Type.TypeId) Allocator.Error!LoweredTemplateBody {
        var saved = std.ArrayList(BinderRestore).empty;
        defer saved.deinit(self.allocator);
        for (lambda.args) |pattern_id| try self.savePatternBinders(pattern_id, &saved);
        defer self.restoreBinders(saved.items);
        return try self.lowerLambdaTemplate(lambda, fn_ty);
    }

    fn lowerExprType(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error!Type.TypeId {
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .call => |call| (try self.callResultMonoType(expr.ty, call)) orelse try self.lowerType(expr.ty),
            .dispatch_call => |plan| (try self.dispatchResultMonoType(expr.ty, plan)) orelse try self.lowerType(expr.ty),
            .type_dispatch_call => |plan| (try self.dispatchResultMonoType(expr.ty, plan)) orelse try self.lowerType(expr.ty),
            .method_eq => |plan| (try self.dispatchResultMonoType(expr.ty, plan)) orelse try self.lowerType(expr.ty),
            .lambda => |lambda| try self.lambdaFunctionType(lambda),
            .closure => |closure| try self.closureFunctionType(closure),
            else => try self.lowerType(expr.ty),
        };
    }

    fn lowerExpr(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        switch (expr.data) {
            .call => |call| return try self.lowerCallExpr(expr.ty, call),
            .dispatch_call => |plan| return try self.lowerDispatchExpr(expr.ty, plan),
            .type_dispatch_call => |plan| return try self.lowerDispatchExpr(expr.ty, plan),
            .method_eq => |plan| return try self.lowerDispatchExpr(expr.ty, plan),
            .structural_eq => |eq| return try self.lowerDirectStructuralEq(expr.ty, eq),
            else => {},
        }
        const ty = try self.lowerExprType(expr_id);
        return try self.lowerExprWithType(expr_id, ty);
    }

    fn lowerExprWithType(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        const data: Ast.ExprData = switch (expr.data) {
            .pending,
            .ellipsis,
            .anno_only,
            .runtime_error,
            => Common.invariant("non-runtime checked expression reached Monotype lowering"),
            .num => |num| self.lowerIntLiteral(num.value, ty),
            .typed_int => |num| self.lowerIntLiteral(num.value, ty),
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
            .list => |items| .{ .list = try self.lowerListExpr(items, ty) },
            .tuple => |items| .{ .tuple = try self.lowerExprSpanAtTypes(items, self.builder.tupleItemTypes(ty)) },
            .record => |record| return try self.lowerRecordExpr(record, ty),
            .tag => |tag| blk: {
                const name = try self.builder.tagName(self.view, tag.name);
                break :blk .{ .tag = .{
                    .name = name,
                    .payloads = try self.lowerExprSpanAtTypes(tag.args, self.builder.tagPayloadTypes(ty, name)),
                } };
            },
            .zero_argument_tag => |tag| .{ .tag = .{ .name = try self.builder.tagName(self.view, tag.name), .payloads = .empty() } },
            .nominal => |nominal| .{ .nominal = try self.lowerExprAtType(nominal.backing_expr, self.builder.namedBackingType(ty) orelse ty) },
            .closure => |closure| try self.lowerClosure(expr_id, closure, ty),
            .lambda => |lambda| blk: {
                break :blk try self.lowerLambdaExpr(
                    lambda,
                    try self.builder.fnTemplateForNestedExprWithMono(self.view, self.owner_template, expr_id, expr.ty, self.view.types.rootKey(expr.ty), ty, self.current_fn_key),
                );
            },
            .call => Common.invariant("call expression reached ordinary expression lowering after call-site lowering"),
            .dispatch_call,
            .type_dispatch_call,
            .method_eq,
            => Common.invariant("dispatch expression reached ordinary expression lowering after call-site lowering"),
            .structural_eq => Common.invariant("structural equality reached ordinary expression lowering after explicit equality lowering"),
            .field_access => |field| .{ .field_access = .{
                .receiver = try self.lowerExpr(field.receiver),
                .field = try self.builder.recordFieldName(self.view, field.field_name),
            } },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .match_ => |match| return try self.lowerMatchExpr(match, ty),
            .if_ => |if_| return try self.lowerIfExpr(expr_id, if_, ty),
            .block => |block| try self.lowerBlock(block, ty),
            .binop,
            .unary_minus,
            .unary_not,
            => Common.invariant("desugared operator expression reached Monotype without checked dispatch or low-level form"),
            .crash => |msg| .{ .crash = try self.lowerStringLiteral(msg) },
            .dbg => |child| .{ .dbg = try self.lowerDbgMessage(child) },
            .expect => |child| .{ .expect = try self.lowerExpr(child) },
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.expr) },
            .for_ => |for_| try self.lowerIteratorFor(for_, ty, &.{}),
            .hosted_lambda => Common.invariant("hosted lambda expression reached ordinary Monotype expression lowering"),
            .run_low_level => |low_level| .{ .low_level = .{ .op = low_level.op, .args = try self.lowerExprSpan(low_level.args) } },
        };
        return try self.builder.program.addExpr(.{ .ty = ty, .data = data });
    }

    fn lowerDbgMessage(self: *BodyContext, child: checked.CheckedExprId) Allocator.Error!Ast.ExprId {
        const value = try self.lowerExpr(child);
        const value_ty = self.builder.program.exprs.items[@intFromEnum(value)].ty;
        const str_ty = try self.builder.primitiveType(.str);
        return try self.builder.inspectCall(value, value_ty, str_ty);
    }

    fn lowerStr(self: *BodyContext, segments: []const checked.CheckedExprId) Allocator.Error!Ast.ExprData {
        const str_ty = try self.builder.primitiveType(.str);
        if (segments.len == 0) {
            return .{ .nominal = try self.builder.stringExpr("", str_ty) };
        }

        var out = try self.lowerExprAtType(segments[0], str_ty);
        for (segments[1..]) |segment| {
            const right = try self.lowerExprAtType(segment, str_ty);
            out = try self.builder.concatExpr(out, right, str_ty);
        }
        return .{ .nominal = out };
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

    fn lowerCallExpr(self: *BodyContext, checked_ret_ty: checked.CheckedTypeId, call: anytype) Allocator.Error!Ast.ExprId {
        const lowered = try self.lowerCall(checked_ret_ty, call);
        return try self.builder.program.addExpr(.{
            .ty = lowered.ret_ty,
            .data = lowered.data,
        });
    }

    fn checkedFunctionType(self: *BodyContext, checked_fn_ty: checked.CheckedTypeId) checked.CheckedFunctionType {
        return switch (resolvedPayload(self.view, checked_fn_ty).payload) {
            .function => |function| function,
            else => Common.invariant("checked call function type was not a function"),
        };
    }

    fn functionReturnType(self: *BodyContext, fn_ty: Type.TypeId) Type.TypeId {
        return switch (self.builder.program.types.get(fn_ty)) {
            .func => |func| func.ret,
            else => Common.invariant("checked call function type was not a function"),
        };
    }

    fn lowerCall(self: *BodyContext, checked_ret_ty: checked.CheckedTypeId, call: anytype) Allocator.Error!LoweredCall {
        if (try self.lowerCallThatCannotReachCallee(checked_ret_ty, call)) |lowered| return lowered;

        if (call.direct_target) |target| {
            var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template);
            defer call_ctx.deinit();
            call_ctx.owner_context_fn_key = self.owner_context_fn_key;
            call_ctx.current_fn_key = self.current_fn_key;

            const mono_fn_ty = try call_ctx.instantiateCallTypeFromCaller(call.source_fn_ty_payload, self, checked_ret_ty, call.args);
            const source_fn_key = call_ctx.view.types.rootKey(call.source_fn_ty_payload);
            const callee = try self.fnTemplateForDirectCallWithMono(target, call.source_fn_ty_payload, source_fn_key, mono_fn_ty);
            const fn_data = switch (self.builder.program.types.get(mono_fn_ty)) {
                .func => |data| data,
                else => Common.invariant("checked direct call target had a non-function type"),
            };
            try self.bindTypeToMono(checked_ret_ty, fn_data.ret, "checked direct call result type mapped one checked type to two monotype types");
            return .{
                .ret_ty = fn_data.ret,
                .data = .{ .call_proc = .{
                    .callee = callee,
                    .args = try self.lowerExprSpanAtTypes(call.args, self.builder.program.types.span(fn_data.args)),
                } },
            };
        }

        const fn_ty = try self.lowerType(call.source_fn_ty_payload);
        const fn_data = switch (self.builder.program.types.get(fn_ty)) {
            .func => |data| data,
            else => Common.invariant("checked call function type was not a function"),
        };
        return .{
            .ret_ty = fn_data.ret,
            .data = .{ .call_value = .{
                .callee = try self.lowerExprAtType(call.func, fn_ty),
                .args = try self.lowerExprSpanAtTypes(call.args, self.builder.program.types.span(fn_data.args)),
            } },
        };
    }

    fn lowerCallThatCannotReachCallee(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        call: anytype,
    ) Allocator.Error!?LoweredCall {
        if (self.checkedExprDiverges(call.func)) {
            return try self.lowerDivergentCallOperand(checked_ret_ty, call.func);
        }
        for (call.args) |arg| {
            if (self.checkedExprDiverges(arg)) {
                return try self.lowerDivergentCallOperand(checked_ret_ty, arg);
            }
        }
        return null;
    }

    fn lowerDivergentCallOperand(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        operand: checked.CheckedExprId,
    ) Allocator.Error!LoweredCall {
        if (!try self.checkedTypeCanLower(checked_ret_ty)) {
            Common.invariant("divergent call result type was not concrete");
        }
        const ret_ty = try self.lowerType(checked_ret_ty);
        return .{
            .ret_ty = ret_ty,
            .data = try self.lowerDivergentExprDataAtType(operand, ret_ty),
        };
    }

    fn instantiateCallTypeFromCaller(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        caller: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        checked_args: []const checked.CheckedExprId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != checked_args.len) {
            Common.invariant("checked direct call arity differs from its function type");
        }

        try self.bindCallArgTypesFromCallerToFixedPoint(function.args, caller, checked_args, "checked direct call argument type mapped one checked type to two monotype types");

        const ret_ty = if (caller.type_bindings.get(caller.typeAddress(checked_ret_ty))) |caller_ret_binding| ret: {
            try self.bindTypeToMono(function.ret, caller_ret_binding.ty, "checked direct call return type mapped one checked type to two monotype types");
            break :ret caller_ret_binding.ty;
        } else if (try self.checkedTypeHasFixedShape(function.ret)) ret: {
            const callee_ret_ty = try self.lowerType(function.ret);
            try caller.bindTypeToMono(checked_ret_ty, callee_ret_ty, "checked direct call result type mapped one checked type to two monotype types");
            break :ret callee_ret_ty;
        } else if (try caller.checkedTypeHasFixedShape(checked_ret_ty)) ret: {
            const caller_ret_ty = try caller.lowerType(checked_ret_ty);
            try self.bindTypeToMono(function.ret, caller_ret_ty, "checked direct call return type mapped one checked type to two monotype types");
            break :ret caller_ret_ty;
        } else if (try self.checkedTypeHasConcreteShape(function.ret)) ret: {
            const callee_ret_ty = try self.lowerType(function.ret);
            try caller.bindTypeToMono(checked_ret_ty, callee_ret_ty, "checked direct call result type mapped one checked type to two monotype types");
            break :ret callee_ret_ty;
        } else if (try caller.checkedTypeHasConcreteShape(checked_ret_ty)) ret: {
            const caller_ret_ty = try caller.lowerType(checked_ret_ty);
            try self.bindTypeToMono(function.ret, caller_ret_ty, "checked direct call return type mapped one checked type to two monotype types");
            break :ret caller_ret_ty;
        } else if (try self.checkedTypeCanLower(function.ret)) ret: {
            const callee_ret_ty = try self.lowerType(function.ret);
            try caller.bindTypeToMono(checked_ret_ty, callee_ret_ty, "checked direct call result type mapped one checked type to two monotype types");
            break :ret callee_ret_ty;
        } else if (try caller.checkedTypeCanLower(checked_ret_ty)) ret: {
            const caller_ret_ty = try caller.lowerType(checked_ret_ty);
            try self.bindTypeToMono(function.ret, caller_ret_ty, "checked direct call return type mapped one checked type to two monotype types");
            break :ret caller_ret_ty;
        } else Common.invariant("checked direct call return type was not concrete after call-site binding");

        try self.bindCallArgTypesFromCallerToFixedPoint(function.args, caller, checked_args, "checked direct call argument type mapped one checked type to two monotype types");

        const arg_tys = try self.allocator.alloc(Type.TypeId, function.args.len);
        defer self.allocator.free(arg_tys);
        for (function.args, 0..) |arg_ty, i| {
            if (!try self.checkedTypeCanLower(arg_ty)) {
                Common.invariant("checked direct call argument type was not concrete after call-site binding");
            }
            arg_tys[i] = try self.lowerType(arg_ty);
        }

        const mono_fn_ty = try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(arg_tys),
            .ret = ret_ty,
        } });
        try self.bindTypeToMono(source_fn_ty, mono_fn_ty, "checked direct call function type mapped one checked type to two monotype types");
        return mono_fn_ty;
    }

    fn instantiateTargetCallTypeFromCaller(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        caller: *BodyContext,
        checked_args: []const checked.CheckedExprId,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != checked_args.len) {
            Common.invariant("checked dispatch target arity differs from its function type");
        }

        try self.bindCallArgTypesFromCallerToFixedPoint(function.args, caller, checked_args, "checked dispatch target argument type mapped one checked type to two monotype types");

        if (expected_ret_ty) |ret_ty| {
            try self.bindTypeToMono(function.ret, ret_ty, "checked dispatch target return type mapped one checked type to two monotype types");
        }

        try self.bindCallArgTypesFromCallerToFixedPoint(function.args, caller, checked_args, "checked dispatch target argument type mapped one checked type to two monotype types");

        const arg_tys = try self.allocator.alloc(Type.TypeId, function.args.len);
        defer self.allocator.free(arg_tys);
        for (function.args, 0..) |arg_ty, i| {
            if (!try self.checkedTypeCanLower(arg_ty)) {
                Common.invariant("checked dispatch target argument type was not concrete after call-site binding");
            }
            arg_tys[i] = try self.lowerType(arg_ty);
        }

        if (!try self.checkedTypeCanLower(function.ret)) {
            Common.invariant("checked dispatch target return type was not concrete after call-site binding");
        }
        const ret_ty = try self.lowerType(function.ret);

        const mono_fn_ty = try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(arg_tys),
            .ret = ret_ty,
        } });
        try self.bindTypeToMono(source_fn_ty, mono_fn_ty, "checked dispatch target function type mapped one checked type to two monotype types");
        return mono_fn_ty;
    }

    fn instantiateTargetCallTypeFromMonoArgs(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != arg_tys.len) {
            Common.invariant("checked synthetic dispatch target arity differs from its function type");
        }

        try self.bindTypeSpanToMono(function.args, arg_tys, "checked synthetic dispatch target argument type mapped one checked type to two monotype types");
        try self.bindTypeToMono(function.ret, ret_ty, "checked synthetic dispatch target return type mapped one checked type to two monotype types");

        const mono_fn_ty = try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(arg_tys),
            .ret = ret_ty,
        } });
        try self.bindTypeToMono(source_fn_ty, mono_fn_ty, "checked synthetic dispatch target function type mapped one checked type to two monotype types");
        return mono_fn_ty;
    }

    fn bindCallArgTypesFromCaller(
        self: *BodyContext,
        formal_tys: []const checked.CheckedTypeId,
        caller: *BodyContext,
        checked_args: []const checked.CheckedExprId,
        comptime numeric_literals: bool,
        comptime conflict_message: []const u8,
    ) Allocator.Error!void {
        for (formal_tys, checked_args) |formal_ty, checked_arg| {
            if ((try caller.exprHasNumericDefault(checked_arg)) != numeric_literals) continue;
            if (numeric_literals and try self.checkedTypeHasFixedShape(formal_ty)) continue;
            try self.bindCheckedTypeRelations(
                formal_ty,
                caller,
                caller.view.bodies.exprs[@intFromEnum(checked_arg)].ty,
                conflict_message,
            );
            if (try caller.callArgumentMonoType(checked_arg)) |actual_mono_ty| {
                try self.bindTypeToMono(
                    formal_ty,
                    actual_mono_ty,
                    conflict_message,
                );
                try caller.bindTypeToMono(
                    caller.view.bodies.exprs[@intFromEnum(checked_arg)].ty,
                    actual_mono_ty,
                    conflict_message,
                );
            }
        }
    }

    fn bindCallArgTypesFromCallerToFixedPoint(
        self: *BodyContext,
        formal_tys: []const checked.CheckedTypeId,
        caller: *BodyContext,
        checked_args: []const checked.CheckedExprId,
        comptime conflict_message: []const u8,
    ) Allocator.Error!void {
        while (true) {
            const before_self = self.type_binding_revision;
            const before_caller = caller.type_binding_revision;
            try self.bindCallArgTypesFromCaller(formal_tys, caller, checked_args, false, conflict_message);
            try self.bindCallArgTypesFromCaller(formal_tys, caller, checked_args, true, conflict_message);
            if (self.type_binding_revision == before_self and caller.type_binding_revision == before_caller) return;
        }
    }

    fn bindCheckedTypeRelations(
        self: *BodyContext,
        left_ty: checked.CheckedTypeId,
        right_ctx: *BodyContext,
        right_ty: checked.CheckedTypeId,
        comptime conflict_message: []const u8,
    ) Allocator.Error!void {
        var active = std.AutoHashMap(CheckedTypeRelation, void).init(self.allocator);
        defer active.deinit();
        try self.bindCheckedTypeRelationsInner(left_ty, right_ctx, right_ty, conflict_message, &active);
    }

    fn bindCheckedTypeRelationsInner(
        self: *BodyContext,
        left_ty: checked.CheckedTypeId,
        right_ctx: *BodyContext,
        right_ty: checked.CheckedTypeId,
        comptime conflict_message: []const u8,
        active: *std.AutoHashMap(CheckedTypeRelation, void),
    ) Allocator.Error!void {
        const relation = CheckedTypeRelation{
            .left = self.typeAddress(left_ty),
            .right = right_ctx.typeAddress(right_ty),
        };
        if (active.contains(relation)) return;
        try active.put(relation, {});
        defer _ = active.remove(relation);

        if (self.type_bindings.get(relation.left)) |left_binding| {
            try right_ctx.bindTypeToMono(right_ty, left_binding.ty, conflict_message);
        }
        if (right_ctx.type_bindings.get(relation.right)) |right_binding| {
            try self.bindTypeToMono(left_ty, right_binding.ty, conflict_message);
        }

        const left_payload = self.builder.checkedTypePayload(self.view, left_ty);
        const right_payload = self.builder.checkedTypePayload(right_ctx.view, right_ty);
        switch (left_payload) {
            .function => |left_fn| switch (right_payload) {
                .function => |right_fn| {
                    if (left_fn.args.len != right_fn.args.len) return;
                    for (left_fn.args, right_fn.args) |left_arg, right_arg| {
                        try self.bindCheckedTypeRelationsInner(left_arg, right_ctx, right_arg, conflict_message, active);
                    }
                    try self.bindCheckedTypeRelationsInner(left_fn.ret, right_ctx, right_fn.ret, conflict_message, active);
                },
                else => {},
            },
            .tuple => |left_items| switch (right_payload) {
                .tuple => |right_items| {
                    if (left_items.len != right_items.len) return;
                    for (left_items, right_items) |left_item, right_item| {
                        try self.bindCheckedTypeRelationsInner(left_item, right_ctx, right_item, conflict_message, active);
                    }
                },
                else => {},
            },
            .nominal => |left_nominal| switch (right_payload) {
                .nominal => |right_nominal| {
                    if (left_nominal.args.len != right_nominal.args.len) return;
                    for (left_nominal.args, right_nominal.args) |left_arg, right_arg| {
                        try self.bindCheckedTypeRelationsInner(left_arg, right_ctx, right_arg, conflict_message, active);
                    }
                },
                else => {},
            },
            .alias => |left_alias| switch (right_payload) {
                .alias => |right_alias| {
                    if (left_alias.args.len != right_alias.args.len) return;
                    for (left_alias.args, right_alias.args) |left_arg, right_arg| {
                        try self.bindCheckedTypeRelationsInner(left_arg, right_ctx, right_arg, conflict_message, active);
                    }
                    try self.bindCheckedTypeRelationsInner(left_alias.backing, right_ctx, right_alias.backing, conflict_message, active);
                },
                else => {},
            },
            .record_unbound,
            .record,
            .tag_union,
            .flex,
            .rigid,
            .empty_record,
            .empty_tag_union,
            .pending,
            => {},
        }
    }

    fn callArgumentMonoType(self: *BodyContext, checked_arg: checked.CheckedExprId) Allocator.Error!?Type.TypeId {
        const expr = self.view.bodies.exprs[@intFromEnum(checked_arg)];
        return switch (expr.data) {
            .call => |call| try self.callResultMonoType(expr.ty, call),
            .dispatch_call => |plan| try self.dispatchResultMonoType(expr.ty, plan),
            .type_dispatch_call => |plan| try self.dispatchResultMonoType(expr.ty, plan),
            .method_eq => |plan| try self.dispatchResultMonoType(expr.ty, plan),
            else => if (try self.checkedTypeHasConcreteShape(expr.ty))
                try self.lowerType(expr.ty)
            else
                null,
        };
    }

    fn callResultMonoType(self: *BodyContext, checked_ret_ty: checked.CheckedTypeId, call: anytype) Allocator.Error!?Type.TypeId {
        if (call.direct_target == null) {
            if (!try self.checkedTypeCanLower(checked_ret_ty)) return null;
            return try self.lowerType(checked_ret_ty);
        }

        var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template);
        defer call_ctx.deinit();
        call_ctx.owner_context_fn_key = self.owner_context_fn_key;
        call_ctx.current_fn_key = self.current_fn_key;

        const mono_fn_ty = try call_ctx.instantiateCallTypeFromCaller(call.source_fn_ty_payload, self, checked_ret_ty, call.args);
        return call_ctx.functionReturnType(mono_fn_ty);
    }

    fn fnTemplateForDirectCallWithMono(
        self: *BodyContext,
        target: checked.ResolvedValueId,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.FnTemplate {
        const raw = @intFromEnum(target);
        if (raw >= self.view.resolved_refs.records.len) {
            Common.invariant("checked direct call target is outside resolved value table");
        }
        return switch (self.view.resolved_refs.records[raw].ref) {
            .local_proc => |local| try self.fnTemplateForLocalProcWithMono(local.expr, source_fn_ty, source_fn_key, mono_fn_ty),
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .promoted_top_level_proc,
            => |proc| try self.fnDefForProcedureUseWithMono(proc, source_fn_ty, source_fn_key, mono_fn_ty),
            .platform_required_proc => |proc| try self.fnDefForProcedureUseWithMono(proc.procedure, source_fn_ty, source_fn_key, mono_fn_ty),
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .top_level_const,
            .imported_const,
            .platform_required_declaration,
            .platform_required_const,
            => Common.invariant("checked direct call target was not a procedure"),
        };
    }

    fn fnTemplateForLocalProc(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        source_fn_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.FnTemplate {
        return try self.fnTemplateForLocalProcWithMono(
            expr_id,
            source_fn_ty,
            self.view.types.rootKey(source_fn_ty),
            try self.lowerType(source_fn_ty),
        );
    }

    fn fnTemplateForLocalProcWithMono(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.FnTemplate {
        const fn_template = try self.builder.fnTemplateForNestedExprWithMono(
            self.view,
            self.owner_template,
            expr_id,
            source_fn_ty,
            source_fn_key,
            mono_fn_ty,
            self.owner_context_fn_key,
        );
        try self.builder.lowerNestedFnFromContext(self, expr_id, fn_template);
        return fn_template;
    }

    fn fnDefForProcedureUse(self: *BodyContext, proc: checked.ProcedureUseTemplate) Allocator.Error!Ast.FnTemplate {
        const source_fn_ty = proc.source_fn_ty_payload orelse
            Common.invariant("checked procedure use reached Monotype without a requested function type");
        return try self.fnDefForProcedureUseWithType(proc, source_fn_ty);
    }

    fn fnDefForProcedureUseWithType(
        self: *BodyContext,
        proc: checked.ProcedureUseTemplate,
        source_fn_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.FnTemplate {
        const mono_fn_ty = try self.lowerType(source_fn_ty);
        return try self.fnDefForProcedureUseWithMono(proc, source_fn_ty, proc.source_fn_ty_template, mono_fn_ty);
    }

    fn fnDefForProcedureUseWithMono(
        self: *BodyContext,
        proc: checked.ProcedureUseTemplate,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.FnTemplate {
        const fn_template = switch (proc.binding) {
            .top_level => |top_level| blk: {
                const view = self.builder.moduleForId(checked.topLevelProcedureModuleId(top_level));
                const binding = view.top_level_procedure_bindings.get(top_level.binding);
                break :blk fnDefForProcedureBindingBody(view, binding.body, source_fn_ty, source_fn_key, mono_fn_ty);
            },
            .imported => |imported| blk: {
                const view = self.builder.moduleForId(checked.importedProcedureModuleId(imported));
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and binding.binding.pattern == imported.pattern) {
                        break :blk fnDefForImportedBindingBody(view, binding.body, source_fn_ty, source_fn_key, mono_fn_ty);
                    }
                }
                Common.invariant("imported procedure binding was not exported by its checked module");
            },
            .hosted => |hosted| blk: {
                break :blk fnDefForTemplate(
                    self.builder.moduleForId(moduleIdFromDigest(names.procTemplateModuleDigest(hosted.template))),
                    hosted.template,
                    source_fn_ty,
                    source_fn_key,
                    mono_fn_ty,
                );
            },
            .platform_required => |required| blk: {
                const app_view = self.builder.moduleForId(checked.requiredProcedureModuleId(required));
                const binding = app_view.top_level_procedure_bindings.get(required.procedure_binding);
                break :blk fnDefForProcedureBindingBody(app_view, binding.body, source_fn_ty, source_fn_key, mono_fn_ty);
            },
        };
        try self.builder.lowerFnTemplateDefFromContext(self, fn_template);
        return fn_template;
    }

    fn lowerLookupExpr(self: *BodyContext, checked_ty: checked.CheckedTypeId, maybe_ref: ?checked.ResolvedValueId) Allocator.Error!Ast.ExprId {
        const ref_id = maybe_ref orelse Common.invariant("checked lookup reached Monotype without resolved value ref");
        const record = self.view.resolved_refs.records[@intFromEnum(ref_id)];
        const ty = try self.lowerType(checked_ty);
        const data: Ast.ExprData = switch (record.ref) {
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            => |local| .{ .local = self.binders.get(local.binder) orelse Common.invariant("local lookup referenced an unbound pattern binder") },
            .local_proc => |local| .{ .fn_def = try self.fnTemplateForLocalProc(local.expr, checked_ty) },
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .promoted_top_level_proc,
            => |proc| return try self.lowerProcedureUseValue(proc, checked_ty, ty),
            .platform_required_proc => |proc| return try self.lowerProcedureUseValue(proc.procedure, checked_ty, ty),
            .top_level_const => |const_use| return try self.restoreConstUse(const_use, checked_ty),
            .imported_const => |const_use| return try self.restoreConstUse(const_use, checked_ty),
            .platform_required_const => |required| return try self.restoreConstUse(required.const_use, checked_ty),
            .platform_required_declaration => Common.invariant("platform required declaration reached Monotype without a binding"),
        };
        return try self.builder.program.addExpr(.{ .ty = ty, .data = data });
    }

    fn lowerProcedureUseValue(
        self: *BodyContext,
        proc: checked.ProcedureUseTemplate,
        checked_ty: checked.CheckedTypeId,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const source_fn_ty = proc.source_fn_ty_payload orelse
            Common.invariant("checked procedure value reached Monotype without a requested function type");
        if (source_fn_ty != checked_ty) {
            Common.invariant("checked procedure value requested type differs from lookup expression type");
        }

        return switch (proc.binding) {
            .top_level => |top_level| blk: {
                const view = self.builder.moduleForId(checked.topLevelProcedureModuleId(top_level));
                const binding = view.top_level_procedure_bindings.get(top_level.binding);
                break :blk try self.builder.lowerProcedureBindingValue(view, binding, source_fn_ty, mono_fn_ty);
            },
            .imported => |imported| blk: {
                const view = self.builder.moduleForId(checked.importedProcedureModuleId(imported));
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and binding.binding.pattern == imported.pattern) {
                        break :blk try self.builder.lowerProcedureBindingValue(view, .{
                            .source_scheme = binding.source_scheme,
                            .body = switch (binding.body) {
                                .direct_template => |direct| .{ .direct_template = direct },
                                .callable_eval_template => |template_id| .{ .callable_eval_template = template_id },
                            },
                        }, source_fn_ty, mono_fn_ty);
                    }
                }
                Common.invariant("imported procedure binding was not exported by its checked module");
            },
            .hosted => |hosted| blk: {
                const fn_template = fnDefForTemplate(
                    self.builder.moduleForId(moduleIdFromDigest(names.procTemplateModuleDigest(hosted.template))),
                    hosted.template,
                    source_fn_ty,
                    proc.source_fn_ty_template,
                    mono_fn_ty,
                );
                try self.builder.lowerFnTemplateDefFromContext(self, fn_template);
                break :blk try self.builder.program.addExpr(.{ .ty = mono_fn_ty, .data = .{ .fn_def = fn_template } });
            },
            .platform_required => |required| blk: {
                const view = self.builder.moduleForId(checked.requiredProcedureModuleId(required));
                const binding = view.top_level_procedure_bindings.get(required.procedure_binding);
                break :blk try self.builder.lowerProcedureBindingValue(view, binding, source_fn_ty, mono_fn_ty);
            },
        };
    }

    fn restoreConstUse(self: *BodyContext, const_use: checked.ConstUseTemplate, checked_ty: checked.CheckedTypeId) Allocator.Error!Ast.ExprId {
        _ = checked_ty;
        const requested_ty = const_use.requested_source_ty_payload orelse
            Common.invariant("checked const use reached Monotype without a requested checked type");

        const store_view = self.builder.moduleForId(checked.constModuleId(const_use.const_ref));
        const ty = try self.lowerType(requested_ty);
        const template = store_view.const_templates.get(const_use.const_ref);
        return switch (template.state) {
            .stored_const => |stored| try self.restoreConstNodeAtType(store_view, self.view, stored.node, ty),
            .reserved => Common.invariant("reserved checked const template reached Monotype"),
            .eval_template => |eval| try self.lowerConstEvalTemplateUse(store_view, eval, ty),
        };
    }

    fn lowerConstEvalTemplateUse(
        self: *BodyContext,
        store_view: ModuleView,
        eval: checked.ConstEvalTemplate,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const body = store_view.checked_const_bodies.get(eval.body);
        const entry_template = store_view.templates.get(eval.entry_template.template);

        const wrapper_args = try self.builder.program.types.addSpan(&.{});
        const wrapper_fn_ty = try self.builder.program.types.add(.{ .func = .{
            .args = wrapper_args,
            .ret = ty,
        } });
        const wrapper_template = fnDefForTemplate(
            store_view,
            eval.entry_template,
            entry_template.checked_fn_root,
            store_view.types.rootKey(entry_template.checked_fn_root),
            wrapper_fn_ty,
        );

        var body_ctx = try BodyContext.init(self.allocator, self.builder, store_view, eval.entry_template);
        defer body_ctx.deinit();
        const root_fn_key = Ast.fnTemplateDigest(wrapper_template, &self.builder.program.types, &self.builder.program.names);
        body_ctx.owner_context_fn_key = root_fn_key;
        body_ctx.current_fn_key = root_fn_key;
        try body_ctx.bindTypeToMono(entry_template.checked_fn_root, wrapper_fn_ty, "const eval wrapper root mapped one checked type to two monotype types");
        try body_ctx.bindTypeToMono(body.checked_type, ty, "const eval body mapped one checked type to two monotype types");

        return try body_ctx.lowerExprAtType(body.body_expr, ty);
    }

    fn restoreConstNode(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        node: checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.restoreConstNodeAtType(store_view, type_view, node, try self.builder.lowerType(type_view, checked_ty));
    }

    fn restoreConstNodeAtType(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        node: checked.ConstNodeId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const address = ConstExprAddress{
            .store_module_bytes = store_view.key.bytes,
            .type_module_bytes = type_view.key.bytes,
            .node = @intFromEnum(node),
            .mono_ty = @intFromEnum(ty),
        };
        if (self.builder.const_expr_cache.get(address)) |existing| return existing;

        const value = store_view.const_store.get(node);
        switch (value) {
            .fn_value => |fn_id| {
                const expr = try self.restoreConstFn(store_view, type_view, fn_id, ty);
                try self.builder.const_expr_cache.put(address, expr);
                return expr;
            },
            else => {},
        }
        const data = try self.restoreConstData(store_view, type_view, value, ty);
        const expr = try self.builder.program.addExpr(.{ .ty = ty, .data = data });
        try self.builder.const_expr_cache.put(address, expr);
        return expr;
    }

    fn restoreConstData(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        value: checked.ConstValue,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprData {
        return switch (value) {
            .pending => Common.invariant("pending ConstStore node reached Monotype restore"),
            .zst => .unit,
            .scalar => |scalar| restoreScalar(scalar),
            .str => |bytes| .{ .str_lit = try self.builder.program.addStringLiteral(bytes) },
            .list => |items| .{ .list = try self.restoreConstList(store_view, type_view, ty, items) },
            .box => |payload| .{ .nominal = try self.restoreConstNodeAtType(store_view, type_view, payload, self.constBoxPayloadType(ty)) },
            .tuple => |items| .{ .tuple = try self.restoreConstTuple(store_view, type_view, ty, items) },
            .record => |items| .{ .record = try self.restoreConstRecord(store_view, type_view, ty, items) },
            .tag => |tag| .{ .tag = .{
                .name = try self.builder.tagName(store_view, tag.tag_name),
                .payloads = try self.restoreConstTagPayloads(store_view, type_view, ty, tag),
            } },
            .nominal => |nominal| .{ .nominal = try self.restoreConstNodeAtType(store_view, type_view, nominal.backing, self.constNamedBackingType(ty)) },
            .fn_value => Common.invariant("ConstStore function value must be restored as an expression"),
        };
    }

    fn restoreConstList(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const elem_ty = self.constListElemType(ty);
        const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            lowered[index] = try self.restoreConstNodeAtType(store_view, type_view, item, elem_ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn restoreConstTuple(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const item_tys = self.builder.tupleItemTypes(ty);
        if (item_tys.len != items.len) Common.invariant("ConstStore tuple length differs from checked type");
        const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            lowered[index] = try self.restoreConstNodeAtType(store_view, type_view, item, item_tys[index]);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn restoreConstRecord(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const fields = self.constRecordFields(ty);
        if (fields.len != items.len) Common.invariant("ConstStore record length differs from checked type");
        const lowered = try self.allocator.alloc(Ast.FieldExpr, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            lowered[index] = .{
                .name = fields[index].name,
                .value = try self.restoreConstNodeAtType(store_view, type_view, item, fields[index].ty),
            };
        }
        return try self.builder.program.addFieldExprSpan(lowered);
    }

    fn restoreConstTagPayloads(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        tag: anytype,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const mono_tag_name = try self.builder.tagName(store_view, tag.tag_name);
        const payload_tys = self.builder.tagPayloadTypes(ty, mono_tag_name);
        if (payload_tys.len != tag.payloads.len) Common.invariant("ConstStore tag payload count differs from checked type");
        const lowered = try self.allocator.alloc(Ast.ExprId, tag.payloads.len);
        defer self.allocator.free(lowered);
        for (tag.payloads, 0..) |payload, index| {
            lowered[index] = try self.restoreConstNodeAtType(store_view, type_view, payload, payload_tys[index]);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn constListElemType(self: *BodyContext, ty: Type.TypeId) Type.TypeId {
        return switch (self.builder.shapeContent(ty)) {
            .list => |elem| elem,
            else => Common.invariant("ConstStore list restored with a non-list monotype"),
        };
    }

    fn constBoxPayloadType(self: *BodyContext, ty: Type.TypeId) Type.TypeId {
        return switch (self.builder.shapeContent(ty)) {
            .box => |payload| payload,
            else => Common.invariant("ConstStore box restored with a non-box monotype"),
        };
    }

    fn constRecordFields(self: *BodyContext, ty: Type.TypeId) []const Type.Field {
        return switch (self.builder.shapeContent(ty)) {
            .record => |fields| self.builder.program.types.fieldSpan(fields),
            else => Common.invariant("ConstStore record restored with a non-record monotype"),
        };
    }

    fn constNamedBackingType(self: *BodyContext, ty: Type.TypeId) Type.TypeId {
        return self.builder.namedBackingType(ty) orelse
            Common.invariant("ConstStore nominal restored with a monotype that has no backing");
    }

    fn restoreConstFn(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        fn_id: checked.ConstFnId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return self.builder.restoreConstFnExpr(store_view, type_view, fn_id, ty);
    }

    fn lowerExprSpan(self: *BodyContext, checked_exprs: []const checked.CheckedExprId) Allocator.Error!Ast.Span(Ast.ExprId) {
        const lowered = try self.allocator.alloc(Ast.ExprId, checked_exprs.len);
        defer self.allocator.free(lowered);
        for (checked_exprs, 0..) |child, i| {
            lowered[i] = try self.lowerExpr(child);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn lowerListExpr(self: *BodyContext, checked_exprs: []const checked.CheckedExprId, ty: Type.TypeId) Allocator.Error!Ast.Span(Ast.ExprId) {
        const elem_ty = switch (self.builder.shapeContent(ty)) {
            .list => |elem| elem,
            else => Common.invariant("list expression had a non-list monotype"),
        };
        const lowered = try self.allocator.alloc(Ast.ExprId, checked_exprs.len);
        defer self.allocator.free(lowered);
        for (checked_exprs, 0..) |child, i| {
            lowered[i] = try self.lowerExprAtType(child, elem_ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn lowerExprSpanAtTypes(
        self: *BodyContext,
        checked_exprs: []const checked.CheckedExprId,
        tys: []const Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        if (checked_exprs.len != tys.len) Common.invariant("call argument arity differs from concrete function type");
        const stable_tys = try self.allocator.dupe(Type.TypeId, tys);
        defer self.allocator.free(stable_tys);
        const lowered = try self.allocator.alloc(Ast.ExprId, checked_exprs.len);
        defer self.allocator.free(lowered);
        for (checked_exprs, stable_tys, 0..) |child, ty, i| {
            lowered[i] = try self.lowerExprAtType(child, ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn lowerExprAtType(
        self: *BodyContext,
        checked_expr: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.exprs[@intFromEnum(checked_expr)];
        switch (expr.data) {
            .call => |call| {
                try self.bindKnownType(expr.ty, ty);
                const lowered = try self.lowerCall(expr.ty, call);
                if (!self.sameType(ty, lowered.ret_ty)) {
                    Common.invariant("checked call expression lowered at a type different from its context type");
                }
                return try self.builder.program.addExpr(.{
                    .ty = ty,
                    .data = lowered.data,
                });
            },
            .dispatch_call => |plan| return try self.lowerDispatchExprAtType(expr.ty, plan, ty),
            .type_dispatch_call => |plan| return try self.lowerDispatchExprAtType(expr.ty, plan, ty),
            .method_eq => |plan| return try self.lowerDispatchExprAtType(expr.ty, plan, ty),
            .structural_eq => |eq| {
                try self.bindKnownType(expr.ty, ty);
                const lowered = try self.lowerDirectStructuralEqAtType(eq, ty);
                if (!self.sameType(ty, self.builder.program.exprs.items[@intFromEnum(lowered)].ty)) {
                    Common.invariant("checked structural equality lowered at a type different from its context type");
                }
                return lowered;
            },
            else => {},
        }
        try self.bindKnownType(expr.ty, ty);
        const lowered = try self.lowerExprWithType(checked_expr, ty);
        if (!self.sameType(ty, self.builder.program.exprs.items[@intFromEnum(lowered)].ty)) {
            Common.invariant("checked expression lowered at a type different from its call operand type");
        }
        return lowered;
    }

    fn sameType(self: *BodyContext, expected: Type.TypeId, actual: Type.TypeId) bool {
        if (expected == actual) return true;
        const expected_digest = self.builder.program.types.typeDigest(&self.builder.program.names, expected);
        const actual_digest = self.builder.program.types.typeDigest(&self.builder.program.names, actual);
        return std.mem.eql(u8, expected_digest.bytes[0..], actual_digest.bytes[0..]);
    }

    fn lowerRecordExpr(self: *BodyContext, record: anytype, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const target_fields = switch (self.builder.shapeContent(ty)) {
            .record => |fields| self.builder.program.types.fieldSpan(fields),
            else => Common.invariant("record expression had a non-record monotype"),
        };
        const lowered = try self.allocator.alloc(Ast.FieldExpr, target_fields.len);
        defer self.allocator.free(lowered);
        const base_record = if (record.ext) |ext| try self.lowerExpr(ext) else null;
        const base_ty = if (base_record) |base_expr| self.builder.program.exprs.items[@intFromEnum(base_expr)].ty else ty;
        const base_local = if (base_record) |_| try self.builder.program.addLocal(self.builder.symbols.fresh(), base_ty) else null;
        const base_expr = if (base_local) |local| try self.builder.localExpr(local, base_ty) else null;

        for (target_fields, 0..) |field, i| {
            const value = if (try self.recordUpdateFieldValue(record.fields, field.name)) |field_value|
                try self.lowerExprAtType(field_value, field.ty)
            else if (base_expr) |base_value|
                try self.builder.program.addExpr(.{
                    .ty = field.ty,
                    .data = .{ .field_access = .{
                        .receiver = base_value,
                        .field = field.name,
                    } },
                })
            else
                Common.invariant("closed record literal was missing a checked field value");
            lowered[i] = .{
                .name = field.name,
                .value = value,
            };
        }
        const record_expr = try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .record = try self.builder.program.addFieldExprSpan(lowered) } });
        if (base_record) |base_value| {
            return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
                .bind = try self.builder.bindPat(base_local orelse Common.invariant("record update lowered base without a base local"), base_ty),
                .value = base_value,
                .rest = record_expr,
            } } });
        }
        return record_expr;
    }

    fn recordUpdateFieldValue(
        self: *BodyContext,
        fields: []const checked.CheckedRecordExprField,
        name: names.RecordFieldNameId,
    ) Allocator.Error!?checked.CheckedExprId {
        for (fields) |field| {
            const lowered_name = try self.builder.recordFieldName(self.view, field.label);
            if (self.builder.program.names.recordFieldLabelTextEql(lowered_name, name)) return field.value;
        }
        return null;
    }

    fn lowerClosure(self: *BodyContext, expr_id: checked.CheckedExprId, closure: anytype, closure_ty: Type.TypeId) Allocator.Error!Ast.ExprData {
        const lambda = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
        return switch (lambda.data) {
            .lambda => |lambda_data| blk: {
                const nested = try self.builder.fnTemplateForNestedExprWithMono(self.view, self.owner_template, expr_id, lambda.ty, self.view.types.rootKey(lambda.ty), closure_ty, self.current_fn_key);
                break :blk try self.lowerLambdaExpr(lambda_data, nested);
            },
            else => Common.invariant("checked closure did not point at a lambda expression"),
        };
    }

    fn closureFunctionType(self: *BodyContext, closure: anytype) Allocator.Error!Type.TypeId {
        const lambda = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
        return switch (lambda.data) {
            .lambda => |lambda_data| try self.lambdaFunctionType(lambda_data),
            else => Common.invariant("checked closure did not point at a lambda expression"),
        };
    }

    fn lambdaFunctionType(self: *BodyContext, lambda: anytype) Allocator.Error!Type.TypeId {
        const args = try self.allocator.alloc(Type.TypeId, lambda.args.len);
        defer self.allocator.free(args);
        for (lambda.args, 0..) |pattern_id, i| {
            args[i] = try self.lowerType(self.view.bodies.patterns[@intFromEnum(pattern_id)].ty);
        }
        return try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(args),
            .ret = try self.lowerExprType(lambda.body),
        } });
    }

    fn lowerLambdaExpr(self: *BodyContext, lambda: anytype, nested: Ast.FnTemplate) Allocator.Error!Ast.ExprData {
        var lambda_ctx = try self.childContext(Ast.fnTemplateDigest(nested, &self.builder.program.types, &self.builder.program.names));
        defer lambda_ctx.deinit();
        try lambda_ctx.bindTypeToMono(nested.source_fn_ty, nested.mono_fn_ty, "lambda function root mapped one checked type to two monotype types");

        const fn_content = self.builder.program.types.get(nested.mono_fn_ty);
        const fn_data = switch (fn_content) {
            .func => |data| data,
            else => Common.invariant("nested lambda had a non-function type"),
        };
        const arg_tys = self.builder.program.types.span(fn_data.args);
        if (arg_tys.len != lambda.args.len) Common.invariant("nested lambda arity differs from concrete function type");

        const lowered = try lambda_ctx.lowerLambdaArgsAndBody(lambda.args, arg_tys, lambda.body, fn_data.ret);
        switch (nested.fn_def) {
            .nested => {},
            else => Common.invariant("expression-position lambda was not assigned a nested function identity"),
        }

        return .{ .lambda = .{
            .args = lowered.args,
            .body = lowered.body,
            .source = nested,
        } };
    }

    fn lowerDispatchExpr(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) Allocator.Error!Ast.ExprId {
        return try self.lowerDispatchExprAtType(checked_ret_ty, maybe_plan, null);
    }

    fn lowerDispatchExprAtType(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const plan_id = maybe_plan orelse Common.invariant("checked dispatch expression reached Monotype without a dispatch plan");
        const plan = self.view.static_dispatch_plans.plans[@intFromEnum(plan_id)];
        const lookup = try self.dispatchTarget(plan);
        if (lookup == null) {
            var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template);
            defer call_ctx.deinit();
            call_ctx.owner_context_fn_key = self.owner_context_fn_key;
            call_ctx.current_fn_key = self.current_fn_key;

            const callable_mono_ty = try call_ctx.instantiateCallTypeFromCaller(plan.callable_ty, self, checked_ret_ty, plan.args);
            const fn_data = switch (self.builder.program.types.get(callable_mono_ty)) {
                .func => |data| data,
                else => Common.invariant("checked structural dispatch target had a non-function type"),
            };
            return try self.lowerStructuralEquality(plan, callable_mono_ty, fn_data.ret, self);
        }
        const resolved = lookup.?;
        const template = resolved.target.template orelse
            Common.invariant("checked dispatch target was not backed by a procedure template");
        var target_ctx = try BodyContext.init(self.allocator, self.builder, resolved.view, template);
        defer target_ctx.deinit();

        const dispatch_ret_ty = expected_ret_ty;
        const callable_mono_ty = try target_ctx.instantiateTargetCallTypeFromCaller(resolved.target.callable_ty, self, plan.args, dispatch_ret_ty);
        const fn_data = switch (self.builder.program.types.get(callable_mono_ty)) {
            .func => |data| data,
            else => Common.invariant("checked dispatch target had a non-function type"),
        };
        try self.bindTypeToMono(checked_ret_ty, fn_data.ret, "checked dispatch result type mapped one checked type to two monotype types");
        if (dispatch_ret_ty) |expected| {
            if (!self.sameType(expected, fn_data.ret)) Common.invariant("checked dispatch expression lowered at a type different from its call operand type");
        }
        const call_expr = try self.builder.program.addExpr(.{
            .ty = fn_data.ret,
            .data = try target_ctx.lowerResolvedDispatch(plan, resolved, callable_mono_ty, self),
        });
        return try self.applyDispatchResultMode(plan.result_mode, call_expr, fn_data.ret);
    }

    fn applyDispatchResultMode(
        self: *BodyContext,
        mode: static_dispatch.StaticDispatchResultMode,
        expr: Ast.ExprId,
        expr_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (mode) {
            .value => expr,
            .equality => |eq| if (eq.negated) blk: {
                if (!self.typeHasBuiltinOwner(expr_ty, .bool)) Common.invariant("checked equality dispatch returned a non-Bool value");
                break :blk try self.builder.lowLevelExpr(.bool_not, &.{expr}, expr_ty);
            } else expr,
        };
    }

    fn typeHasBuiltinOwner(self: *BodyContext, ty: Type.TypeId, owner: static_dispatch.BuiltinOwner) bool {
        return switch (methodOwnerFromType(&self.builder.program.types, ty) orelse return false) {
            .builtin => |actual| actual == owner,
            .nominal => false,
        };
    }

    fn dispatchResultMonoType(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) Allocator.Error!?Type.TypeId {
        const plan_id = maybe_plan orelse Common.invariant("checked dispatch expression reached Monotype without a dispatch plan");
        const plan = self.view.static_dispatch_plans.plans[@intFromEnum(plan_id)];
        const lookup = try self.dispatchTarget(plan);
        if (lookup == null) {
            if (!try self.checkedTypeCanLower(checked_ret_ty)) return null;
            return try self.lowerType(checked_ret_ty);
        }
        const resolved = lookup.?;
        const template = resolved.target.template orelse
            Common.invariant("checked dispatch target was not backed by a procedure template");
        var target_ctx = try BodyContext.init(self.allocator, self.builder, resolved.view, template);
        defer target_ctx.deinit();
        const expected_ret_ty: ?Type.TypeId = null;
        const callable_mono_ty = try target_ctx.instantiateTargetCallTypeFromCaller(resolved.target.callable_ty, self, plan.args, expected_ret_ty);
        const ret_ty = target_ctx.functionReturnType(callable_mono_ty);
        try self.bindTypeToMono(checked_ret_ty, ret_ty, "checked dispatch result type mapped one checked type to two monotype types");
        return ret_ty;
    }

    fn dispatchTarget(
        self: *BodyContext,
        plan: static_dispatch.StaticDispatchCallPlan,
    ) Allocator.Error!?MethodLookup {
        const dispatcher_ty = switch (plan.dispatcher) {
            .arg => |index| blk: {
                if (index >= plan.args.len) Common.invariant("dispatch plan dispatcher argument index was outside the argument span");
                break :blk (try self.callArgumentMonoType(plan.args[index])) orelse
                    Common.invariant("dispatch plan dispatcher argument was not concrete in the current specialization");
            },
            .type_only => try self.lowerType(plan.dispatcher_ty),
        };
        const owner = methodOwnerFromType(&self.builder.program.types, dispatcher_ty) orelse {
            if (plan.result_mode == .equality and plan.result_mode.equality.structural_allowed) return null;
            Common.invariant("dispatch plan had no method owner and no structural equality permission");
        };

        const lookup = self.builder.lookupMethodTarget(owner, self.view, plan.method) orelse {
            if (plan.result_mode == .equality and plan.result_mode.equality.structural_allowed) return null;
            Common.invariant("checked method registry is missing resolved dispatch target");
        };
        return lookup;
    }

    fn lowerResolvedDispatch(
        self: *BodyContext,
        plan: static_dispatch.StaticDispatchCallPlan,
        lookup: MethodLookup,
        callable_mono_ty: Type.TypeId,
        arg_ctx: *BodyContext,
    ) Allocator.Error!Ast.ExprData {
        const template = lookup.target.template orelse
            Common.invariant("checked dispatch target was not backed by a procedure template");
        _ = try self.builder.lowerTemplateWithMono(
            template,
            lookup.view,
            lookup.target.callable_ty,
            lookup.view.types.rootKey(lookup.target.callable_ty),
            callable_mono_ty,
        );

        const fn_data = switch (self.builder.program.types.get(callable_mono_ty)) {
            .func => |data| data,
            else => Common.invariant("checked dispatch target had a non-function type"),
        };
        const args = try arg_ctx.lowerExprSpanAtTypes(plan.args, self.builder.program.types.span(fn_data.args));
        return .{ .call_proc = .{
            .callee = fnDefForTemplate(
                lookup.view,
                template,
                lookup.target.callable_ty,
                lookup.view.types.rootKey(lookup.target.callable_ty),
                callable_mono_ty,
            ),
            .args = args,
        } };
    }

    fn lowerStructuralEquality(
        self: *BodyContext,
        plan: static_dispatch.StaticDispatchCallPlan,
        callable_mono_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        arg_ctx: *BodyContext,
    ) Allocator.Error!Ast.ExprId {
        return switch (plan.result_mode) {
            .equality => |eq| if (eq.structural_allowed) blk: {
                if (plan.args.len != 2) Common.invariant("structural equality dispatch plan must have two operands");
                const fn_data = switch (self.builder.program.types.get(callable_mono_ty)) {
                    .func => |data| data,
                    else => Common.invariant("checked structural equality target had a non-function type"),
                };
                const arg_tys = self.builder.program.types.span(fn_data.args);
                if (arg_tys.len != 2) Common.invariant("structural equality callable type must have two operands");
                const lhs = try arg_ctx.lowerExprAtType(plan.args[0], arg_tys[0]);
                const rhs = try arg_ctx.lowerExprAtType(plan.args[1], arg_tys[1]);
                var result = try self.lowerEqualityExpr(arg_tys[0], lhs, rhs, self.view.names.methodNameText(plan.method), ret_ty);
                if (eq.negated) {
                    result = try self.builder.lowLevelExpr(.bool_not, &.{result}, ret_ty);
                }
                break :blk result;
            } else Common.invariant("structural equality dispatch plan did not permit structural equality"),
            .value => Common.invariant("value dispatch plan reached structural equality lowering"),
        };
    }

    fn lowerDirectStructuralEq(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        eq: anytype,
    ) Allocator.Error!Ast.ExprId {
        const ret_ty = try self.lowerType(checked_ret_ty);
        return try self.lowerDirectStructuralEqAtType(eq, ret_ty);
    }

    fn lowerDirectStructuralEqAtType(
        self: *BodyContext,
        eq: anytype,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const lhs_ty = try self.lowerExprType(eq.lhs);
        const rhs_ty = try self.lowerExprType(eq.rhs);
        if (!self.sameType(lhs_ty, rhs_ty)) Common.invariant("checked structural equality operands have different monotypes");
        const lhs = try self.lowerExprAtType(eq.lhs, lhs_ty);
        const rhs = try self.lowerExprAtType(eq.rhs, lhs_ty);
        var result = try self.lowerEqualityExpr(lhs_ty, lhs, rhs, "is_eq", ret_ty);
        if (eq.negated) {
            result = try self.builder.lowLevelExpr(.bool_not, &.{result}, ret_ty);
        }
        return result;
    }

    fn lowerEqualityExpr(
        self: *BodyContext,
        ty: Type.TypeId,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (self.builder.program.types.get(ty)) {
            .list => try self.lowerOwnedEqualityCall(ty, lhs, rhs, method_name, bool_ty),
            .record => |fields| try self.lowerRecordEqualityExpr(self.builder.program.types.fieldSpan(fields), lhs, rhs, method_name, bool_ty),
            .tuple => |items| try self.lowerTupleEqualityExpr(self.builder.program.types.span(items), lhs, rhs, method_name, bool_ty),
            .primitive,
            .zst,
            .named,
            .tag_union,
            .func,
            .erased,
            .box,
            => try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .structural_eq = .{
                .lhs = lhs,
                .rhs = rhs,
                .negated = false,
            } } }),
        };
    }

    fn lowerRecordEqualityExpr(
        self: *BodyContext,
        fields: []const Type.Field,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        var rest = try self.boolLiteral(true, bool_ty);
        var index = fields.len;
        while (index > 0) {
            index -= 1;
            const field = fields[index];
            const lhs_field = try self.builder.program.addExpr(.{ .ty = field.ty, .data = .{ .field_access = .{
                .receiver = lhs,
                .field = field.name,
            } } });
            const rhs_field = try self.builder.program.addExpr(.{ .ty = field.ty, .data = .{ .field_access = .{
                .receiver = rhs,
                .field = field.name,
            } } });
            const field_eq = try self.lowerEqualityExpr(field.ty, lhs_field, rhs_field, method_name, bool_ty);
            rest = try self.builder.ifExpr(field_eq, rest, try self.boolLiteral(false, bool_ty), bool_ty);
        }
        return rest;
    }

    fn lowerTupleEqualityExpr(
        self: *BodyContext,
        items: []const Type.TypeId,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        var rest = try self.boolLiteral(true, bool_ty);
        var index = items.len;
        while (index > 0) {
            index -= 1;
            const item_ty = items[index];
            const lhs_item = try self.builder.program.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                .tuple = lhs,
                .elem_index = @intCast(index),
            } } });
            const rhs_item = try self.builder.program.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                .tuple = rhs,
                .elem_index = @intCast(index),
            } } });
            const item_eq = try self.lowerEqualityExpr(item_ty, lhs_item, rhs_item, method_name, bool_ty);
            rest = try self.builder.ifExpr(item_eq, rest, try self.boolLiteral(false, bool_ty), bool_ty);
        }
        return rest;
    }

    fn lowerOwnedEqualityCall(
        self: *BodyContext,
        ty: Type.TypeId,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const owner = methodOwnerFromType(&self.builder.program.types, ty) orelse
            Common.invariant("owned equality call requested for a type without a method owner");
        const lookup = self.builder.lookupMethodTargetByName(owner, method_name) orelse
            Common.invariant("checked method registry is missing owned equality target");
        const template = lookup.target.template orelse
            Common.invariant("checked equality target was not backed by a procedure template");
        var target_ctx = try BodyContext.init(self.allocator, self.builder, lookup.view, template);
        defer target_ctx.deinit();
        target_ctx.owner_context_fn_key = self.owner_context_fn_key;
        target_ctx.current_fn_key = self.current_fn_key;

        const arg_tys = [_]Type.TypeId{ ty, ty };
        const callable_mono_ty = try target_ctx.instantiateTargetCallTypeFromMonoArgs(lookup.target.callable_ty, &arg_tys, bool_ty);
        _ = try self.builder.lowerTemplateWithMono(
            template,
            lookup.view,
            lookup.target.callable_ty,
            lookup.view.types.rootKey(lookup.target.callable_ty),
            callable_mono_ty,
        );
        const args = [_]Ast.ExprId{ lhs, rhs };
        return try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .call_proc = .{
            .callee = fnDefForTemplate(
                lookup.view,
                template,
                lookup.target.callable_ty,
                lookup.view.types.rootKey(lookup.target.callable_ty),
                callable_mono_ty,
            ),
            .args = try self.builder.program.addExprSpan(&args),
        } } });
    }

    fn boolLiteral(self: *BodyContext, value: bool, bool_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const u64_ty = try self.builder.primitiveType(.u64);
        const lhs = try self.builder.intLiteralExpr(0, u64_ty);
        const rhs = try self.builder.intLiteralExpr(if (value) 0 else 1, u64_ty);
        return try self.builder.lowLevelExpr(.num_is_eq, &.{ lhs, rhs }, bool_ty);
    }

    fn lowerMatchExpr(self: *BodyContext, match: anytype, result_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        if (!self.matchContainsListPattern(match)) {
            return try self.builder.program.addExpr(.{
                .ty = result_ty,
                .data = try self.lowerMatch(match, result_ty),
            });
        }

        const scrutinee = try self.lowerExpr(match.cond);
        const scrutinee_ty = self.builder.program.exprs.items[@intFromEnum(scrutinee)].ty;
        const scrutinee_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), scrutinee_ty);
        const scrutinee_expr = try self.builder.localExpr(scrutinee_local, scrutinee_ty);
        const rest = try self.lowerListPatternMatchAlternatives(scrutinee_expr, scrutinee_ty, match.branches, result_ty);
        return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(scrutinee_local, scrutinee_ty),
            .value = scrutinee,
            .rest = rest,
        } } });
    }

    fn matchContainsListPattern(self: *BodyContext, match: anytype) bool {
        for (match.branches) |branch| {
            for (branch.patterns) |pattern| {
                if (self.patternContainsList(pattern.pattern)) return true;
            }
        }
        return false;
    }

    fn patternContainsList(self: *BodyContext, pattern_id: checked.CheckedPatternId) bool {
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
        return switch (pattern.data) {
            .list => true,
            .as => |as| self.patternContainsList(as.pattern),
            .applied_tag => |tag| blk: {
                for (tag.args) |child| {
                    if (self.patternContainsList(child)) break :blk true;
                }
                break :blk false;
            },
            .nominal => |nominal| self.patternContainsList(nominal.backing_pattern),
            .record_destructure => |destructs| blk: {
                for (destructs) |destruct| {
                    const child = switch (destruct.kind) {
                        .required => |child_pattern| child_pattern,
                        .sub_pattern => |child_pattern| child_pattern,
                        .rest => |child_pattern| child_pattern,
                    };
                    if (self.patternContainsList(child)) break :blk true;
                }
                break :blk false;
            },
            .tuple => |items| blk: {
                for (items) |child| {
                    if (self.patternContainsList(child)) break :blk true;
                }
                break :blk false;
            },
            .assign,
            .pending,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => false,
        };
    }

    fn lowerListPatternMatchAlternatives(
        self: *BodyContext,
        scrutinee: Ast.ExprId,
        scrutinee_ty: Type.TypeId,
        branches: []const checked.CheckedMatchBranch,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const msg = try self.builder.program.addStringLiteral("non-exhaustive checked match reached Monotype");
        var fallback = try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .crash = msg } });

        var branch_index = branches.len;
        while (branch_index > 0) {
            branch_index -= 1;
            const branch = branches[branch_index];
            var pattern_index = branch.patterns.len;
            while (pattern_index > 0) {
                pattern_index -= 1;
                fallback = try self.lowerListPatternMatchAlternative(
                    scrutinee,
                    scrutinee_ty,
                    branch.patterns[pattern_index],
                    branch.guard,
                    branch.value,
                    fallback,
                    result_ty,
                );
            }
        }

        return fallback;
    }

    fn lowerListPatternMatchAlternative(
        self: *BodyContext,
        scrutinee: Ast.ExprId,
        scrutinee_ty: Type.TypeId,
        pattern: checked.CheckedMatchBranchPattern,
        guard: ?checked.CheckedExprId,
        body: checked.CheckedExprId,
        fallback: Ast.ExprId,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const checked_pattern = self.view.bodies.patterns[@intFromEnum(pattern.pattern)];
        switch (checked_pattern.data) {
            .list => |list| {
                var branch_ctx = try self.childContext(self.current_fn_key);
                defer branch_ctx.deinit();
                var saved = std.ArrayList(BinderRestore).empty;
                defer saved.deinit(self.allocator);
                try branch_ctx.saveMatchPatternBinders(pattern, &saved);
                defer branch_ctx.restoreBinders(saved.items);

                const success = try branch_ctx.lowerListPatternSuccess(scrutinee, scrutinee_ty, list, pattern.binder_remaps, guard, body, fallback, result_ty);
                const cond = try self.listPatternCondition(scrutinee, scrutinee_ty, list);
                return try self.builder.ifExpr(cond, success, fallback, result_ty);
            },
            .underscore => {
                var branch_ctx = try self.childContext(self.current_fn_key);
                defer branch_ctx.deinit();
                return try branch_ctx.lowerExprAtType(body, result_ty);
            },
            else => {
                var branch_ctx = try self.childContext(self.current_fn_key);
                defer branch_ctx.deinit();
                var saved = std.ArrayList(BinderRestore).empty;
                defer saved.deinit(self.allocator);
                try branch_ctx.saveMatchPatternBinders(pattern, &saved);
                defer branch_ctx.restoreBinders(saved.items);

                const pat = try branch_ctx.lowerPatternAtType(pattern.pattern, scrutinee_ty);
                try branch_ctx.applyAlternativeBinderRemaps(pattern.binder_remaps);
                const branch_body = try branch_ctx.lowerExprAtType(body, result_ty);
                const wildcard = try self.builder.program.addPat(.{ .ty = scrutinee_ty, .data = .wildcard });
                const match_branches = [_]Ast.Branch{
                    .{ .pat = pat, .guard = if (guard) |guard_expr| try branch_ctx.lowerExpr(guard_expr) else null, .body = branch_body },
                    .{ .pat = wildcard, .body = fallback },
                };
                return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .match_ = .{
                    .scrutinee = scrutinee,
                    .branches = try self.builder.program.addBranchSpan(&match_branches),
                } } });
            },
        }
    }

    fn listPatternCondition(
        self: *BodyContext,
        scrutinee: Ast.ExprId,
        scrutinee_ty: Type.TypeId,
        list: anytype,
    ) Allocator.Error!Ast.ExprId {
        _ = scrutinee_ty;
        const u64_ty = try self.builder.primitiveType(.u64);
        const bool_ty = try self.builder.primitiveType(.bool);
        const len = try self.builder.lowLevelExpr(.list_len, &.{scrutinee}, u64_ty);
        const required = try self.builder.intLiteralExpr(list.patterns.len, u64_ty);
        const op: can.CIR.Expr.LowLevel = if (list.rest == null) .num_is_eq else .num_is_gte;
        return try self.builder.lowLevelExpr(op, &.{ len, required }, bool_ty);
    }

    fn lowerListPatternSuccess(
        self: *BodyContext,
        scrutinee: Ast.ExprId,
        scrutinee_ty: Type.TypeId,
        list: anytype,
        remaps: []const checked.CheckedAlternativeBinderRemap,
        guard: ?checked.CheckedExprId,
        body: checked.CheckedExprId,
        fallback: Ast.ExprId,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const elem_ty = self.constListElemType(scrutinee_ty);
        const values = try self.allocator.alloc(Ast.ExprId, list.patterns.len);
        defer self.allocator.free(values);
        const patterns = try self.allocator.alloc(Ast.PatId, list.patterns.len);
        defer self.allocator.free(patterns);
        const u64_ty = try self.builder.primitiveType(.u64);
        const needs_len = if (list.rest) |rest| rest.index < list.patterns.len or rest.pattern != null else false;
        const len = if (needs_len) try self.builder.lowLevelExpr(.list_len, &.{scrutinee}, u64_ty) else null;

        for (list.patterns, 0..) |pattern_id, index| {
            const item_index = try self.listPatternItemIndex(index, list.patterns.len, list.rest, len, u64_ty);
            values[index] = try self.builder.lowLevelExpr(.list_get_unsafe, &.{ scrutinee, item_index }, elem_ty);
            patterns[index] = try self.lowerPatternAtType(pattern_id, elem_ty);
        }

        var rest_pat: ?Ast.PatId = null;
        var rest_value: ?Ast.ExprId = null;
        if (list.rest) |rest| {
            if (rest.pattern) |rest_pattern| {
                const list_len = len orelse try self.builder.lowLevelExpr(.list_len, &.{scrutinee}, u64_ty);
                const fixed_count = try self.builder.intLiteralExpr(list.patterns.len, u64_ty);
                const rest_len = try self.builder.lowLevelExpr(.num_minus, &.{ list_len, fixed_count }, u64_ty);
                const rest_start = try self.builder.intLiteralExpr(rest.index, u64_ty);
                const range = try self.sublistRangeExpr(rest_start, rest_len, u64_ty);
                rest_value = try self.builder.lowLevelExpr(.list_sublist, &.{ scrutinee, range }, scrutinee_ty);
                rest_pat = try self.lowerPatternAtType(rest_pattern, scrutinee_ty);
            }
        }

        try self.applyAlternativeBinderRemaps(remaps);
        var success = if (guard) |guard_expr| guard_blk: {
            const guarded = try self.lowerExprAtType(body, result_ty);
            const guard_cond = try self.lowerExpr(guard_expr);
            break :guard_blk try self.builder.ifExpr(guard_cond, guarded, fallback, result_ty);
        } else try self.lowerExprAtType(body, result_ty);

        var index = patterns.len;
        while (index > 0) {
            index -= 1;
            success = try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
                .bind = patterns[index],
                .value = values[index],
                .rest = success,
            } } });
        }
        if (rest_pat) |pat| {
            success = try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
                .bind = pat,
                .value = rest_value orelse Common.invariant("list rest pattern had no lowered rest value"),
                .rest = success,
            } } });
        }

        return success;
    }

    fn listPatternItemIndex(
        self: *BodyContext,
        index: usize,
        pattern_count: usize,
        rest: ?checked.CheckedListRestPattern,
        len: ?Ast.ExprId,
        u64_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (rest) |rest_info| {
            if (index >= rest_info.index) {
                const list_len = len orelse Common.invariant("list pattern trailing item index required list length");
                const trailing_count = try self.builder.intLiteralExpr(pattern_count - index, u64_ty);
                return try self.builder.lowLevelExpr(.num_minus, &.{ list_len, trailing_count }, u64_ty);
            }
        }
        return try self.builder.intLiteralExpr(index, u64_ty);
    }

    fn sublistRangeExpr(
        self: *BodyContext,
        start: Ast.ExprId,
        len: Ast.ExprId,
        u64_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const len_name = try self.builder.program.names.internRecordFieldLabel("len");
        const start_name = try self.builder.program.names.internRecordFieldLabel("start");
        const fields = [_]Type.Field{
            .{ .name = len_name, .ty = u64_ty },
            .{ .name = start_name, .ty = u64_ty },
        };
        const ty = try self.builder.program.types.add(.{ .record = try self.builder.program.types.addFields(&fields) });
        const exprs = [_]Ast.FieldExpr{
            .{ .name = len_name, .value = len },
            .{ .name = start_name, .value = start },
        };
        return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .record = try self.builder.program.addFieldExprSpan(&exprs) } });
    }

    fn lowerMatch(self: *BodyContext, match: anytype, result_ty: Type.TypeId) Allocator.Error!Ast.ExprData {
        const scrutinee = try self.lowerExpr(match.cond);
        const scrutinee_ty = self.builder.program.exprs.items[@intFromEnum(scrutinee)].ty;
        const branches = try self.allocator.alloc(Ast.Branch, branchCount(match.branches));
        defer self.allocator.free(branches);
        var index: usize = 0;
        for (match.branches) |branch| {
            for (branch.patterns) |pattern| {
                var saved = std.ArrayList(BinderRestore).empty;
                defer saved.deinit(self.allocator);
                try self.saveMatchPatternBinders(pattern, &saved);
                defer self.restoreBinders(saved.items);

                const pat = try self.lowerPatternAtType(pattern.pattern, scrutinee_ty);
                try self.applyAlternativeBinderRemaps(pattern.binder_remaps);
                const guard = if (branch.guard) |guard_expr| try self.lowerExpr(guard_expr) else null;
                const body = try self.lowerExprAtType(branch.value, result_ty);
                branches[index] = .{
                    .pat = pat,
                    .guard = guard,
                    .body = body,
                };
                index += 1;
            }
        }
        return .{ .match_ = .{
            .scrutinee = scrutinee,
            .branches = try self.builder.program.addBranchSpan(branches),
        } };
    }

    fn saveMatchPatternBinders(
        self: *BodyContext,
        pattern: checked.CheckedMatchBranchPattern,
        saved: *std.ArrayList(BinderRestore),
    ) Allocator.Error!void {
        try self.savePatternBinders(pattern.pattern, saved);
        for (pattern.binder_remaps) |remap| {
            try self.saveBinder(remap.candidate_binder, saved);
            try self.saveBinder(remap.representative_binder, saved);
        }
    }

    fn savePatternBinders(
        self: *BodyContext,
        pattern_id: checked.CheckedPatternId,
        saved: *std.ArrayList(BinderRestore),
    ) Allocator.Error!void {
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
        switch (pattern.data) {
            .assign => |binder| try self.saveBinder(binder, saved),
            .as => |as| {
                try self.savePatternBinders(as.pattern, saved);
                try self.saveBinder(as.binder, saved);
            },
            .applied_tag => |tag| for (tag.args) |arg| try self.savePatternBinders(arg, saved),
            .nominal => |nominal| try self.savePatternBinders(nominal.backing_pattern, saved),
            .record_destructure => |destructs| {
                for (destructs) |destruct| {
                    const child = switch (destruct.kind) {
                        .required => |child_pattern| child_pattern,
                        .sub_pattern => |child_pattern| child_pattern,
                        .rest => |child_pattern| child_pattern,
                    };
                    try self.savePatternBinders(child, saved);
                }
            },
            .list => |list| {
                for (list.patterns) |child| try self.savePatternBinders(child, saved);
                if (list.rest) |rest| if (rest.pattern) |rest_pattern| try self.savePatternBinders(rest_pattern, saved);
            },
            .tuple => |items| for (items) |child| try self.savePatternBinders(child, saved),
            .pending,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => {},
        }
    }

    fn saveBinder(
        self: *BodyContext,
        binder: checked.PatternBinderId,
        saved: *std.ArrayList(BinderRestore),
    ) Allocator.Error!void {
        for (saved.items) |item| {
            if (item.binder == binder) return;
        }
        try saved.append(self.allocator, .{
            .binder = binder,
            .previous = self.binders.get(binder),
        });
    }

    fn applyAlternativeBinderRemaps(
        self: *BodyContext,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!void {
        for (remaps) |remap| {
            const local = self.binders.get(remap.candidate_binder) orelse
                Common.invariant("match alternative binder remap referenced an unbound candidate binder");
            try self.binders.put(remap.representative_binder, local);
        }
    }

    fn restoreBinders(self: *BodyContext, saved: []const BinderRestore) void {
        var index = saved.len;
        while (index > 0) {
            index -= 1;
            if (saved[index].previous) |previous| {
                self.binders.put(saved[index].binder, previous) catch {};
            } else {
                _ = self.binders.remove(saved[index].binder);
            }
        }
    }

    fn lowerIfExpr(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        if_: anytype,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const merge_binders = try self.stateMergeBinders(expr_id);
        defer self.allocator.free(merge_binders);

        if (merge_binders.len == 0) {
            return try self.builder.program.addExpr(.{
                .ty = result_ty,
                .data = try self.lowerIf(if_, result_ty, result_ty, &.{}),
            });
        }

        const state_ty = try self.stateResultType(merge_binders, result_ty);
        const state_expr = try self.builder.program.addExpr(.{
            .ty = state_ty,
            .data = try self.lowerIf(if_, result_ty, state_ty, merge_binders),
        });

        const pattern_items = try self.allocator.alloc(Ast.PatId, merge_binders.len + 1);
        defer self.allocator.free(pattern_items);
        for (merge_binders, 0..) |merge, i| {
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), merge.ty, merge.binder);
            try self.binders.put(merge.binder, local);
            pattern_items[i] = try self.builder.program.addPat(.{ .ty = merge.ty, .data = .{ .bind = local } });
        }

        const result_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), result_ty);
        pattern_items[merge_binders.len] = try self.builder.program.addPat(.{ .ty = result_ty, .data = .{ .bind = result_local } });
        const bind_pat = try self.builder.program.addPat(.{ .ty = state_ty, .data = .{ .tuple = try self.builder.program.addPatSpan(pattern_items) } });
        const rest = try self.builder.localExpr(result_local, result_ty);

        return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
            .bind = bind_pat,
            .value = state_expr,
            .rest = rest,
        } } });
    }

    fn stateMergeBinders(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error![]MergeBinder {
        var reassigned = std.ArrayList(checked.PatternBinderId).empty;
        defer reassigned.deinit(self.allocator);
        try self.collectReassignedBindersInExpr(expr_id, &reassigned);

        var merge_binders = std.ArrayList(MergeBinder).empty;
        errdefer merge_binders.deinit(self.allocator);
        for (reassigned.items) |binder| {
            const before = self.binders.get(binder) orelse continue;
            const ty = self.builder.program.locals.items[@intFromEnum(before)].ty;
            try merge_binders.append(self.allocator, .{
                .binder = binder,
                .before = before,
                .ty = ty,
            });
        }
        return try merge_binders.toOwnedSlice(self.allocator);
    }

    fn stateResultType(
        self: *BodyContext,
        merge_binders: []const MergeBinder,
        result_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const tys = try self.allocator.alloc(Type.TypeId, merge_binders.len + 1);
        defer self.allocator.free(tys);
        for (merge_binders, 0..) |merge, i| tys[i] = merge.ty;
        tys[merge_binders.len] = result_ty;
        return try self.builder.program.types.add(.{ .tuple = try self.builder.program.types.addSpan(tys) });
    }

    fn stateOnlyType(
        self: *BodyContext,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Type.TypeId {
        if (merge_binders.len == 0) return try self.unitType();
        if (merge_binders.len == 1) return merge_binders[0].ty;

        const tys = try self.allocator.alloc(Type.TypeId, merge_binders.len);
        defer self.allocator.free(tys);
        for (merge_binders, 0..) |merge, i| tys[i] = merge.ty;
        return try self.builder.program.types.add(.{ .tuple = try self.builder.program.types.addSpan(tys) });
    }

    fn lowerIf(
        self: *BodyContext,
        if_: anytype,
        result_ty: Type.TypeId,
        branch_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprData {
        const branches = try self.allocator.alloc(Ast.IfBranch, if_.branches.len);
        defer self.allocator.free(branches);
        for (if_.branches, 0..) |branch, i| {
            const cond = try self.lowerExpr(branch.cond);
            var branch_ctx = try self.childContext(self.current_fn_key);
            defer branch_ctx.deinit();
            branches[i] = .{
                .cond = cond,
                .body = try branch_ctx.lowerIfBranchBody(branch.body, result_ty, branch_ty, merge_binders),
            };
        }
        var else_ctx = try self.childContext(self.current_fn_key);
        defer else_ctx.deinit();
        return .{ .if_ = .{
            .branches = try self.builder.program.addIfBranchSpan(branches),
            .final_else = try else_ctx.lowerIfBranchBody(if_.final_else, result_ty, branch_ty, merge_binders),
        } };
    }

    fn lowerIfBranchBody(
        self: *BodyContext,
        body: checked.CheckedExprId,
        result_ty: Type.TypeId,
        branch_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprId {
        if (merge_binders.len == 0) return try self.lowerExprAtType(body, result_ty);

        return try self.lowerBodyThenStateResult(body, result_ty, branch_ty, merge_binders);
    }

    fn lowerIfStateOnly(
        self: *BodyContext,
        if_: anytype,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprData {
        const branches = try self.allocator.alloc(Ast.IfBranch, if_.branches.len);
        defer self.allocator.free(branches);
        for (if_.branches, 0..) |branch, i| {
            const cond = try self.lowerExpr(branch.cond);
            var branch_ctx = try self.childContext(self.current_fn_key);
            defer branch_ctx.deinit();
            branches[i] = .{
                .cond = cond,
                .body = try branch_ctx.lowerBodyThenStateOnly(branch.body, state_ty, merge_binders),
            };
        }
        var else_ctx = try self.childContext(self.current_fn_key);
        defer else_ctx.deinit();
        return .{ .if_ = .{
            .branches = try self.builder.program.addIfBranchSpan(branches),
            .final_else = try else_ctx.lowerBodyThenStateOnly(if_.final_else, state_ty, merge_binders),
        } };
    }

    fn lowerBodyThenStateResult(
        self: *BodyContext,
        body: checked.CheckedExprId,
        result_ty: Type.TypeId,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprId {
        if (self.checkedExprDiverges(body)) return try self.lowerDivergentExprAtType(body, state_ty);

        const checked_body = self.view.bodies.exprs[@intFromEnum(body)];
        switch (checked_body.data) {
            .block => |block| {
                const statements = try self.lowerBlockStatements(block.statements);
                defer self.allocator.free(statements.items);
                const value = if (statements.diverges)
                    try self.unreachableAfterDivergentStatementExpr(result_ty)
                else if (self.checkedExprDiverges(block.final_expr))
                    try self.lowerDivergentExprAtType(block.final_expr, result_ty)
                else
                    try self.lowerExprAtType(block.final_expr, result_ty);
                return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(statements.items[0..statements.len]),
                    .final_expr = try self.stateResultTupleExpr(state_ty, merge_binders, value),
                } } });
            },
            else => {
                const value = try self.lowerExprAtType(body, result_ty);
                return try self.stateResultTupleExpr(state_ty, merge_binders, value);
            },
        }
    }

    fn lowerBodyThenStateOnly(
        self: *BodyContext,
        body: checked.CheckedExprId,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprId {
        if (self.checkedExprDiverges(body)) return try self.lowerDivergentExprAtType(body, state_ty);

        const checked_body = self.view.bodies.exprs[@intFromEnum(body)];
        switch (checked_body.data) {
            .block => |block| {
                var statements = try self.lowerBlockStatements(block.statements);
                defer self.allocator.free(statements.items);
                if (!statements.diverges) {
                    const final_stmt = try self.builder.program.addStmt(.{ .expr = if (self.checkedExprDiverges(block.final_expr))
                        try self.lowerDivergentExprAtType(block.final_expr, try self.lowerType(checked_body.ty))
                    else
                        try self.lowerExpr(block.final_expr) });
                    try statements.append(self.allocator, final_stmt);
                }
                return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(statements.items[0..statements.len]),
                    .final_expr = try self.stateOnlyTupleExpr(state_ty, merge_binders),
                } } });
            },
            else => {
                const value = try self.lowerExpr(body);
                const stmt = try self.builder.program.addStmt(.{ .expr = value });
                return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(&[_]Ast.StmtId{stmt}),
                    .final_expr = try self.stateOnlyTupleExpr(state_ty, merge_binders),
                } } });
            },
        }
    }

    fn stateResultTupleExpr(
        self: *BodyContext,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
        result: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const items = try self.allocator.alloc(Ast.ExprId, merge_binders.len + 1);
        defer self.allocator.free(items);
        for (merge_binders, 0..) |merge, i| {
            const current = self.binders.get(merge.binder) orelse merge.before;
            items[i] = try self.builder.localExpr(current, merge.ty);
        }
        items[merge_binders.len] = result;
        return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .tuple = try self.builder.program.addExprSpan(items) } });
    }

    fn stateOnlyTupleExpr(
        self: *BodyContext,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprId {
        if (merge_binders.len == 0) {
            return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .unit });
        }
        if (merge_binders.len == 1) {
            const merge = merge_binders[0];
            const current = self.binders.get(merge.binder) orelse merge.before;
            return try self.builder.localExpr(current, merge.ty);
        }

        const items = try self.allocator.alloc(Ast.ExprId, merge_binders.len);
        defer self.allocator.free(items);
        for (merge_binders, 0..) |merge, i| {
            const current = self.binders.get(merge.binder) orelse merge.before;
            items[i] = try self.builder.localExpr(current, merge.ty);
        }
        return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .tuple = try self.builder.program.addExprSpan(items) } });
    }

    fn stateOnlyPattern(
        self: *BodyContext,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.PatId {
        if (merge_binders.len == 0) return try self.builder.program.addPat(.{ .ty = state_ty, .data = .wildcard });
        if (merge_binders.len == 1) {
            const merge = merge_binders[0];
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), merge.ty, merge.binder);
            try self.binders.put(merge.binder, local);
            return try self.builder.program.addPat(.{ .ty = state_ty, .data = .{ .bind = local } });
        }

        const pattern_items = try self.allocator.alloc(Ast.PatId, merge_binders.len);
        defer self.allocator.free(pattern_items);
        for (merge_binders, 0..) |merge, i| {
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), merge.ty, merge.binder);
            try self.binders.put(merge.binder, local);
            pattern_items[i] = try self.builder.program.addPat(.{ .ty = merge.ty, .data = .{ .bind = local } });
        }
        return try self.builder.program.addPat(.{ .ty = state_ty, .data = .{ .tuple = try self.builder.program.addPatSpan(pattern_items) } });
    }

    fn lowerBlock(self: *BodyContext, block: anytype, ty: Type.TypeId) Allocator.Error!Ast.ExprData {
        const stmts = try self.lowerBlockStatements(block.statements);
        defer self.allocator.free(stmts.items);
        return .{ .block = .{
            .statements = try self.builder.program.addStmtSpan(stmts.items[0..stmts.len]),
            .final_expr = if (stmts.diverges)
                try self.unreachableAfterDivergentStatementExpr(ty)
            else if (self.checkedExprDiverges(block.final_expr))
                try self.lowerDivergentExprAtType(block.final_expr, ty)
            else
                try self.lowerExprAtType(block.final_expr, ty),
        } };
    }

    const LoweredStatements = struct {
        items: []Ast.StmtId,
        len: usize,
        diverges: bool,

        fn append(self: *LoweredStatements, allocator: Allocator, stmt: Ast.StmtId) Allocator.Error!void {
            if (self.len == self.items.len) {
                const grown = try allocator.realloc(self.items, self.items.len + 1);
                self.items = grown;
            }
            self.items[self.len] = stmt;
            self.len += 1;
        }
    };

    fn lowerBlockStatements(self: *BodyContext, checked_statements: []const checked.CheckedStatementId) Allocator.Error!LoweredStatements {
        const items = try self.allocator.alloc(Ast.StmtId, checked_statements.len);
        var statement_diverges = false;
        for (checked_statements, 0..) |statement, i| {
            items[i] = try self.lowerStatement(statement);
            if (self.checkedStatementDiverges(statement)) statement_diverges = true;
        }
        return .{
            .items = items,
            .len = checked_statements.len,
            .diverges = statement_diverges,
        };
    }

    fn lowerDivergentExprAtType(
        self: *BodyContext,
        checked_expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = try self.lowerDivergentExprDataAtType(checked_expr_id, ty),
        });
    }

    fn lowerDivergentExprDataAtType(
        self: *BodyContext,
        checked_expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprData {
        try self.bindTypeToMono(
            self.view.bodies.exprs[@intFromEnum(checked_expr_id)].ty,
            ty,
            "divergent checked expression type mapped one checked type to two monotype types",
        );
        const checked_expr = self.view.bodies.exprs[@intFromEnum(checked_expr_id)];
        return switch (checked_expr.data) {
            .block => |block| try self.lowerBlock(block, ty),
            .crash => |msg| .{ .crash = try self.lowerStringLiteral(msg) },
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.expr) },
            else => Common.invariant("checked expression was marked divergent but has no divergent lowering path"),
        };
    }

    fn unreachableAfterDivergentStatementExpr(self: *BodyContext, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .crash = try self.builder.program.addStringLiteral("reached code after checked control transfer") },
        });
    }

    fn checkedExprDiverges(self: *BodyContext, expr_id: checked.CheckedExprId) bool {
        const raw = @intFromEnum(expr_id);
        if (raw >= self.view.bodies.exprs.len) {
            Common.invariant("checked divergence referenced a missing expression");
        }
        return self.view.bodies.exprs[raw].diverges;
    }

    fn checkedStatementDiverges(self: *BodyContext, statement_id: checked.CheckedStatementId) bool {
        const raw = @intFromEnum(statement_id);
        if (raw >= self.view.bodies.statements.len) {
            Common.invariant("checked divergence referenced a missing statement");
        }
        return self.view.bodies.statements[raw].diverges;
    }

    const LoopCarry = struct {
        binder: checked.PatternBinderId,
        initial_local: Ast.LocalId,
        param_local: Ast.LocalId,
        ty: Type.TypeId,
    };

    const LoopContext = struct {
        result_ty: Type.TypeId,
        carries: []const LoopCarry,
    };

    fn lowerIteratorFor(
        self: *BodyContext,
        for_: anytype,
        result_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprData {
        const plan_id = for_.plan orelse Common.invariant("checked iterator for reached Monotype without an iterator dispatch plan");
        const plan = self.view.static_dispatch_plans.iterator_for_plans[@intFromEnum(plan_id)];

        const initial_iterator = try self.lowerIteratorDispatch(plan.iter, null);
        const iterator_ty = self.builder.program.exprs.items[@intFromEnum(initial_iterator)].ty;
        try self.bindTypeToMono(plan.iterator_ty, iterator_ty, "checked iterator type mapped one checked type to two monotype types");
        const iterator_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), iterator_ty);
        const iterator_param = Ast.TypedLocal{ .local = iterator_local, .ty = iterator_ty };

        var saved = std.ArrayList(BinderRestore).empty;
        defer saved.deinit(self.allocator);
        for (carries) |carry| {
            try self.saveBinder(carry.binder, &saved);
            try self.binders.put(carry.binder, carry.param_local);
        }
        defer self.restoreBinders(saved.items);

        try self.pushLoopContext(result_ty, carries);
        defer self.popLoopContext();

        const step_expr = try self.lowerIteratorDispatch(plan.next, iterator_param);
        try self.bindTypeToMono(plan.step_ty, self.builder.program.exprs.items[@intFromEnum(step_expr)].ty, "checked iterator step type mapped one checked type to two monotype types");
        const step = self.iteratorStepShape(plan.step_ty);
        const done_body = if (carries.len == 0)
            try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .break_ = null } })
        else
            try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .break_ = try self.loopStateExpr(result_ty, carries) } });

        var branches: [3]Ast.Branch = undefined;
        branches[0] = .{
            .pat = try self.iteratorDonePattern(step),
            .body = done_body,
        };
        branches[1] = try self.iteratorOneBranch(for_, result_ty, step, iterator_ty, carries);
        branches[2] = try self.iteratorSkipBranch(result_ty, step, iterator_ty, carries);

        const match_expr = try self.builder.program.addExpr(.{
            .ty = result_ty,
            .data = .{ .match_ = .{
                .scrutinee = step_expr,
                .branches = try self.builder.program.addBranchSpan(&branches),
            } },
        });

        const params = try self.allocator.alloc(Ast.TypedLocal, 1 + carries.len);
        defer self.allocator.free(params);
        params[0] = iterator_param;
        for (carries, 0..) |carry, i| params[i + 1] = .{ .local = carry.param_local, .ty = carry.ty };

        const initial_values = try self.allocator.alloc(Ast.ExprId, 1 + carries.len);
        defer self.allocator.free(initial_values);
        initial_values[0] = initial_iterator;
        for (carries, 0..) |carry, i| {
            initial_values[i + 1] = try self.builder.localExpr(carry.initial_local, carry.ty);
        }

        return .{ .loop_ = .{
            .params = try self.builder.program.addTypedLocalSpan(params),
            .initial_values = try self.builder.program.addExprSpan(initial_values),
            .body = match_expr,
        } };
    }

    fn lowerWhile(
        self: *BodyContext,
        while_: anytype,
        result_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprData {
        var saved = std.ArrayList(BinderRestore).empty;
        defer saved.deinit(self.allocator);
        for (carries) |carry| {
            try self.saveBinder(carry.binder, &saved);
            try self.binders.put(carry.binder, carry.param_local);
        }
        defer self.restoreBinders(saved.items);

        try self.pushLoopContext(result_ty, carries);
        defer self.popLoopContext();

        const cond = try self.lowerExpr(while_.cond);
        const break_body = try self.breakCurrentLoopExpr();
        const continue_body = try self.lowerWhileBodyThenContinue(while_.body, result_ty, carries);
        const branches = [_]Ast.IfBranch{.{ .cond = cond, .body = continue_body }};
        const body = try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .if_ = .{
            .branches = try self.builder.program.addIfBranchSpan(&branches),
            .final_else = break_body,
        } } });

        const params = try self.allocator.alloc(Ast.TypedLocal, carries.len);
        defer self.allocator.free(params);
        for (carries, 0..) |carry, i| params[i] = .{ .local = carry.param_local, .ty = carry.ty };

        const initial_values = try self.allocator.alloc(Ast.ExprId, carries.len);
        defer self.allocator.free(initial_values);
        for (carries, 0..) |carry, i| {
            initial_values[i] = try self.builder.localExpr(carry.initial_local, carry.ty);
        }

        return .{ .loop_ = .{
            .params = try self.builder.program.addTypedLocalSpan(params),
            .initial_values = try self.builder.program.addExprSpan(initial_values),
            .body = body,
        } };
    }

    fn lowerIteratorDispatch(
        self: *BodyContext,
        plan: static_dispatch.IteratorDispatchCall,
        loop_iterator: ?Ast.TypedLocal,
    ) Allocator.Error!Ast.ExprId {
        if (plan.dispatcher_arg_index >= plan.args.len) Common.invariant("iterator dispatch plan dispatcher argument index was outside the argument span");
        const dispatcher_ty = (try self.iteratorOperandMonoType(plan.args[plan.dispatcher_arg_index], loop_iterator)) orelse
            Common.invariant("iterator dispatch plan dispatcher operand was not concrete in the current specialization");
        const owner = methodOwnerFromType(&self.builder.program.types, dispatcher_ty) orelse
            Common.invariant("iterator dispatch plan had no method owner");
        const lookup = self.builder.lookupMethodTarget(owner, self.view, plan.method) orelse
            Common.invariant("checked iterator dispatch method registry is missing resolved target");
        const template = lookup.target.template orelse
            Common.invariant("checked iterator dispatch target was not backed by a procedure template");

        var target_ctx = try BodyContext.init(self.allocator, self.builder, lookup.view, template);
        defer target_ctx.deinit();
        const callable_mono_ty = try target_ctx.instantiateIteratorTargetCallTypeFromCaller(lookup.target.callable_ty, self, plan.args, loop_iterator);
        try self.bindTypeToMono(plan.callable_ty, callable_mono_ty, "checked iterator dispatch callable type mapped one checked type to two monotype types");
        _ = try self.builder.lowerTemplateWithMono(template, lookup.view, lookup.target.callable_ty, lookup.view.types.rootKey(lookup.target.callable_ty), callable_mono_ty);

        const fn_data = switch (self.builder.program.types.get(callable_mono_ty)) {
            .func => |data| data,
            else => Common.invariant("checked iterator dispatch target had a non-function type"),
        };
        const args = try self.allocator.alloc(Ast.ExprId, plan.args.len);
        defer self.allocator.free(args);
        const arg_tys = self.builder.program.types.span(fn_data.args);
        for (plan.args, 0..) |operand, i| {
            args[i] = try self.lowerIteratorOperandAtType(operand, loop_iterator, arg_tys[i]);
        }

        return try self.builder.program.addExpr(.{
            .ty = fn_data.ret,
            .data = .{ .call_proc = .{
                .callee = fnDefForTemplate(lookup.view, template, lookup.target.callable_ty, lookup.view.types.rootKey(lookup.target.callable_ty), callable_mono_ty),
                .args = try self.builder.program.addExprSpan(args),
            } },
        });
    }

    fn instantiateIteratorTargetCallTypeFromCaller(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        caller: *BodyContext,
        operands: []const static_dispatch.IteratorDispatchOperand,
        loop_iterator: ?Ast.TypedLocal,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != operands.len) {
            Common.invariant("checked iterator dispatch target arity differs from its function type");
        }

        try self.bindIteratorArgTypesFromCallerToFixedPoint(function.args, caller, operands, loop_iterator);

        const arg_tys = try self.allocator.alloc(Type.TypeId, function.args.len);
        defer self.allocator.free(arg_tys);
        for (function.args, 0..) |arg_ty, i| {
            if (!try self.checkedTypeCanLower(arg_ty)) {
                Common.invariant("checked iterator dispatch target argument type was not concrete after call-site binding");
            }
            arg_tys[i] = try self.lowerType(arg_ty);
        }

        if (!try self.checkedTypeCanLower(function.ret)) {
            Common.invariant("checked iterator dispatch target return type was not concrete after call-site binding");
        }
        const ret_ty = try self.lowerType(function.ret);

        const mono_fn_ty = try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(arg_tys),
            .ret = ret_ty,
        } });
        try self.bindTypeToMono(source_fn_ty, mono_fn_ty, "checked iterator dispatch target function type mapped one checked type to two monotype types");
        return mono_fn_ty;
    }

    fn bindIteratorArgTypesFromCaller(
        self: *BodyContext,
        formal_tys: []const checked.CheckedTypeId,
        caller: *BodyContext,
        operands: []const static_dispatch.IteratorDispatchOperand,
        loop_iterator: ?Ast.TypedLocal,
        comptime numeric_literals: bool,
    ) Allocator.Error!void {
        for (formal_tys, operands) |formal_ty, operand| {
            const is_numeric_literal = switch (operand) {
                .checked_expr => |expr| try caller.exprHasNumericDefault(expr),
                .loop_iterator_state => false,
            };
            if (is_numeric_literal != numeric_literals) continue;
            if (numeric_literals and try self.checkedTypeHasFixedShape(formal_ty)) continue;
            switch (operand) {
                .checked_expr => |expr| try self.bindCheckedTypeRelations(
                    formal_ty,
                    caller,
                    caller.view.bodies.exprs[@intFromEnum(expr)].ty,
                    "checked iterator dispatch target argument type mapped one checked type to two monotype types",
                ),
                .loop_iterator_state => {},
            }
            if (try caller.iteratorOperandMonoType(operand, loop_iterator)) |actual_mono_ty| {
                try self.bindTypeToMono(
                    formal_ty,
                    actual_mono_ty,
                    "checked iterator dispatch target argument type mapped one checked type to two monotype types",
                );
                switch (operand) {
                    .checked_expr => |expr| try caller.bindTypeToMono(
                        caller.view.bodies.exprs[@intFromEnum(expr)].ty,
                        actual_mono_ty,
                        "checked iterator dispatch target argument type mapped one checked type to two monotype types",
                    ),
                    .loop_iterator_state => {},
                }
            }
        }
    }

    fn bindIteratorArgTypesFromCallerToFixedPoint(
        self: *BodyContext,
        formal_tys: []const checked.CheckedTypeId,
        caller: *BodyContext,
        operands: []const static_dispatch.IteratorDispatchOperand,
        loop_iterator: ?Ast.TypedLocal,
    ) Allocator.Error!void {
        while (true) {
            const before_self = self.type_binding_revision;
            const before_caller = caller.type_binding_revision;
            try self.bindIteratorArgTypesFromCaller(formal_tys, caller, operands, loop_iterator, false);
            try self.bindIteratorArgTypesFromCaller(formal_tys, caller, operands, loop_iterator, true);
            if (self.type_binding_revision == before_self and caller.type_binding_revision == before_caller) return;
        }
    }

    fn iteratorOperandMonoType(
        self: *BodyContext,
        operand: static_dispatch.IteratorDispatchOperand,
        loop_iterator: ?Ast.TypedLocal,
    ) Allocator.Error!?Type.TypeId {
        return switch (operand) {
            .checked_expr => |expr| try self.callArgumentMonoType(expr),
            .loop_iterator_state => if (loop_iterator) |iterator| iterator.ty else Common.invariant("iterator .next dispatch reached Monotype without a loop iterator local"),
        };
    }

    fn lowerIteratorOperandAtType(
        self: *BodyContext,
        operand: static_dispatch.IteratorDispatchOperand,
        loop_iterator: ?Ast.TypedLocal,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (operand) {
            .checked_expr => |expr| try self.lowerExprAtType(expr, ty),
            .loop_iterator_state => blk: {
                const iterator = loop_iterator orelse Common.invariant("iterator .next dispatch reached Monotype without a loop iterator local");
                if (!self.sameType(iterator.ty, ty)) Common.invariant("iterator .next operand type differed from instantiated callable argument type");
                break :blk try self.builder.localExpr(iterator.local, iterator.ty);
            },
        };
    }

    fn iteratorDonePattern(self: *BodyContext, step: IterStepShape) Allocator.Error!Ast.PatId {
        return try self.builder.program.addPat(.{ .ty = try self.lowerType(step.step_ty), .data = .{ .tag = .{
            .name = try self.builder.tagName(self.view, step.done_tag),
            .payloads = .empty(),
        } } });
    }

    fn iteratorOneBranch(
        self: *BodyContext,
        for_: anytype,
        result_ty: Type.TypeId,
        step: IterStepShape,
        iterator_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.Branch {
        var saved = std.ArrayList(BinderRestore).empty;
        defer saved.deinit(self.allocator);
        try self.savePatternBinders(for_.pattern, &saved);
        for (carries) |carry| try self.saveBinder(carry.binder, &saved);
        defer self.restoreBinders(saved.items);

        const item_ty = try self.lowerType(step.one_item.ty);
        const rest_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), iterator_ty);
        const record_pat = try self.iteratorOnePayloadPattern(for_.pattern, step, item_ty, iterator_ty, rest_local);
        const tag_pat = try self.builder.program.addPat(.{ .ty = try self.lowerType(step.step_ty), .data = .{ .tag = .{
            .name = try self.builder.tagName(self.view, step.one_tag),
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{record_pat}),
        } } });

        const rest_expr = try self.builder.localExpr(rest_local, iterator_ty);
        const block = try self.lowerIteratorBodyThenContinue(for_.body, result_ty, rest_expr, carries);

        return .{ .pat = tag_pat, .body = block };
    }

    fn lowerIteratorBodyThenContinue(
        self: *BodyContext,
        body: checked.CheckedExprId,
        result_ty: Type.TypeId,
        rest_expr: Ast.ExprId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprId {
        const checked_body = self.view.bodies.exprs[@intFromEnum(body)];
        switch (checked_body.data) {
            .block => |block| {
                var statement_diverges = false;
                for (block.statements) |statement| {
                    if (self.checkedStatementDiverges(statement)) statement_diverges = true;
                }
                const extra: usize = if (statement_diverges) 0 else 1;
                const lowered_statements = try self.allocator.alloc(Ast.StmtId, block.statements.len + extra);
                defer self.allocator.free(lowered_statements);
                for (block.statements, 0..) |statement, i| {
                    lowered_statements[i] = try self.lowerStatement(statement);
                }
                if (!statement_diverges) {
                    lowered_statements[block.statements.len] = try self.builder.program.addStmt(.{ .expr = if (self.checkedExprDiverges(block.final_expr))
                        try self.lowerDivergentExprAtType(block.final_expr, result_ty)
                    else
                        try self.lowerExpr(block.final_expr) });
                }
                return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(lowered_statements),
                    .final_expr = try self.continueWithState(result_ty, rest_expr, carries),
                } } });
            },
            else => {
                const body_expr = try self.lowerExpr(body);
                const body_stmt = try self.builder.program.addStmt(.{ .expr = body_expr });
                return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(&[_]Ast.StmtId{body_stmt}),
                    .final_expr = try self.continueWithState(result_ty, rest_expr, carries),
                } } });
            },
        }
    }

    fn lowerWhileBodyThenContinue(
        self: *BodyContext,
        body: checked.CheckedExprId,
        result_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprId {
        const checked_body = self.view.bodies.exprs[@intFromEnum(body)];
        switch (checked_body.data) {
            .block => |block| {
                var statement_diverges = false;
                for (block.statements) |statement| {
                    if (self.checkedStatementDiverges(statement)) statement_diverges = true;
                }
                const extra: usize = if (statement_diverges) 0 else 1;
                const lowered_statements = try self.allocator.alloc(Ast.StmtId, block.statements.len + extra);
                defer self.allocator.free(lowered_statements);
                for (block.statements, 0..) |statement, i| {
                    lowered_statements[i] = try self.lowerStatement(statement);
                }
                if (!statement_diverges) {
                    lowered_statements[block.statements.len] = try self.builder.program.addStmt(.{ .expr = if (self.checkedExprDiverges(block.final_expr))
                        try self.lowerDivergentExprAtType(block.final_expr, result_ty)
                    else
                        try self.lowerExpr(block.final_expr) });
                }
                return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(lowered_statements),
                    .final_expr = try self.continueWithCurrentState(result_ty, carries),
                } } });
            },
            else => {
                const body_expr = try self.lowerExpr(body);
                const body_stmt = try self.builder.program.addStmt(.{ .expr = body_expr });
                return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(&[_]Ast.StmtId{body_stmt}),
                    .final_expr = try self.continueWithCurrentState(result_ty, carries),
                } } });
            },
        }
    }

    fn iteratorSkipBranch(
        self: *BodyContext,
        result_ty: Type.TypeId,
        step: IterStepShape,
        iterator_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.Branch {
        const rest_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), iterator_ty);
        const record_pat = try self.iteratorSkipPayloadPattern(step, iterator_ty, rest_local);
        const tag_pat = try self.builder.program.addPat(.{ .ty = try self.lowerType(step.step_ty), .data = .{ .tag = .{
            .name = try self.builder.tagName(self.view, step.skip_tag),
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{record_pat}),
        } } });
        const rest_expr = try self.builder.localExpr(rest_local, iterator_ty);
        return .{
            .pat = tag_pat,
            .body = try self.continueWithState(result_ty, rest_expr, carries),
        };
    }

    fn iteratorOnePayloadPattern(
        self: *BodyContext,
        item_pattern: checked.CheckedPatternId,
        step: IterStepShape,
        item_ty: Type.TypeId,
        iterator_ty: Type.TypeId,
        rest_local: Ast.LocalId,
    ) Allocator.Error!Ast.PatId {
        const item_field = try self.iteratorRecordDestruct(step.one_item.name, try self.lowerPatternAtType(item_pattern, item_ty));
        const rest_field = try self.iteratorRecordDestruct(step.one_rest.name, try self.builder.bindPat(rest_local, iterator_ty));
        const fields = [_]Ast.RecordDestruct{ item_field, rest_field };
        return try self.builder.program.addPat(.{
            .ty = try self.lowerType(step.one_payload_ty),
            .data = .{ .record = try self.builder.program.addRecordDestructSpan(&fields) },
        });
    }

    fn iteratorSkipPayloadPattern(
        self: *BodyContext,
        step: IterStepShape,
        iterator_ty: Type.TypeId,
        rest_local: Ast.LocalId,
    ) Allocator.Error!Ast.PatId {
        const count_field = try self.iteratorRecordDestruct(step.skip_count.name, try self.builder.program.addPat(.{
            .ty = try self.lowerType(step.skip_count.ty),
            .data = .wildcard,
        }));
        const rest_field = try self.iteratorRecordDestruct(step.skip_rest.name, try self.builder.bindPat(rest_local, iterator_ty));
        const fields = [_]Ast.RecordDestruct{ count_field, rest_field };
        return try self.builder.program.addPat(.{
            .ty = try self.lowerType(step.skip_payload_ty),
            .data = .{ .record = try self.builder.program.addRecordDestructSpan(&fields) },
        });
    }

    fn iteratorRecordDestruct(
        self: *BodyContext,
        field: names.RecordFieldNameId,
        pattern: Ast.PatId,
    ) Allocator.Error!Ast.RecordDestruct {
        return .{
            .name = try self.builder.recordFieldName(self.view, field),
            .pattern = pattern,
        };
    }

    fn pushLoopContext(self: *BodyContext, result_ty: Type.TypeId, carries: []const LoopCarry) Allocator.Error!void {
        try self.loop_contexts.append(self.allocator, .{
            .result_ty = result_ty,
            .carries = carries,
        });
    }

    fn popLoopContext(self: *BodyContext) void {
        _ = self.loop_contexts.pop();
    }

    fn currentLoopContext(self: *BodyContext) LoopContext {
        if (self.loop_contexts.items.len == 0) Common.invariant("break statement reached Monotype outside an explicit loop expression");
        return self.loop_contexts.items[self.loop_contexts.items.len - 1];
    }

    fn breakCurrentLoopExpr(self: *BodyContext) Allocator.Error!Ast.ExprId {
        const loop = self.currentLoopContext();
        const data: Ast.ExprData = if (loop.carries.len == 0)
            .{ .break_ = null }
        else
            .{ .break_ = try self.loopStateExpr(loop.result_ty, loop.carries) };
        return try self.builder.program.addExpr(.{ .ty = loop.result_ty, .data = data });
    }

    fn continueWith(self: *BodyContext, ty: Type.TypeId, values: []const Ast.ExprId) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .continue_ = .{
            .values = try self.builder.program.addExprSpan(values),
        } } });
    }

    fn continueWithCurrentState(
        self: *BodyContext,
        ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprId {
        if (carries.len == 0) return try self.continueWith(ty, &.{});

        const values = try self.allocator.alloc(Ast.ExprId, carries.len);
        defer self.allocator.free(values);
        for (carries, 0..) |carry, i| {
            const current = self.binders.get(carry.binder) orelse Common.invariant("loop-carried mutable binder was absent");
            values[i] = try self.builder.localExpr(current, carry.ty);
        }
        return try self.continueWith(ty, values);
    }

    fn continueWithState(
        self: *BodyContext,
        ty: Type.TypeId,
        rest_expr: Ast.ExprId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprId {
        const values = try self.allocator.alloc(Ast.ExprId, 1 + carries.len);
        defer self.allocator.free(values);
        values[0] = rest_expr;
        for (carries, 0..) |carry, i| {
            const current = self.binders.get(carry.binder) orelse Common.invariant("loop-carried mutable binder was absent");
            values[i + 1] = try self.builder.localExpr(current, carry.ty);
        }
        return try self.continueWith(ty, values);
    }

    fn loopStateExpr(self: *BodyContext, ty: Type.TypeId, carries: []const LoopCarry) Allocator.Error!Ast.ExprId {
        if (carries.len == 0) Common.invariant("empty loop state requested");
        if (carries.len == 1) {
            const current = self.binders.get(carries[0].binder) orelse Common.invariant("loop-carried mutable binder was absent");
            return try self.builder.localExpr(current, carries[0].ty);
        }

        const items = try self.allocator.alloc(Ast.ExprId, carries.len);
        defer self.allocator.free(items);
        for (carries, 0..) |carry, i| {
            const current = self.binders.get(carry.binder) orelse Common.invariant("loop-carried mutable binder was absent");
            items[i] = try self.builder.localExpr(current, carry.ty);
        }
        return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .tuple = try self.builder.program.addExprSpan(items) } });
    }

    fn loopStateType(self: *BodyContext, unit_ty: Type.TypeId, carries: []const LoopCarry) Allocator.Error!Type.TypeId {
        if (carries.len == 0) return unit_ty;
        if (carries.len == 1) return carries[0].ty;

        const tys = try self.allocator.alloc(Type.TypeId, carries.len);
        defer self.allocator.free(tys);
        for (carries, 0..) |carry, i| tys[i] = carry.ty;
        return try self.builder.program.types.add(.{ .tuple = try self.builder.program.types.addSpan(tys) });
    }

    fn unitType(self: *BodyContext) Allocator.Error!Type.TypeId {
        return try self.builder.program.types.add(.{ .record = .empty() });
    }

    fn prepareLoopCarries(self: *BodyContext, binders: []const checked.PatternBinderId) Allocator.Error![]LoopCarry {
        var carries = std.ArrayList(LoopCarry).empty;
        errdefer carries.deinit(self.allocator);
        for (binders) |binder| {
            const initial = self.binders.get(binder) orelse continue;
            const ty = self.builder.program.locals.items[@intFromEnum(initial)].ty;
            try carries.append(self.allocator, .{
                .binder = binder,
                .initial_local = initial,
                .param_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ty),
                .ty = ty,
            });
        }
        return try carries.toOwnedSlice(self.allocator);
    }

    fn finalCarryPattern(self: *BodyContext, carries: []const LoopCarry, ty: Type.TypeId) Allocator.Error!Ast.PatId {
        if (carries.len == 0) Common.invariant("empty loop carry pattern requested");
        if (carries.len == 1) {
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), carries[0].ty, carries[0].binder);
            try self.binders.put(carries[0].binder, local);
            return try self.builder.program.addPat(.{ .ty = ty, .data = .{ .bind = local } });
        }

        const items = try self.allocator.alloc(Ast.PatId, carries.len);
        defer self.allocator.free(items);
        for (carries, 0..) |carry, i| {
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), carry.ty, carry.binder);
            try self.binders.put(carry.binder, local);
            items[i] = try self.builder.program.addPat(.{ .ty = carry.ty, .data = .{ .bind = local } });
        }
        return try self.builder.program.addPat(.{ .ty = ty, .data = .{ .tuple = try self.builder.program.addPatSpan(items) } });
    }

    fn collectReassignedBindersInExpr(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        out: *std.ArrayList(checked.PatternBinderId),
    ) Allocator.Error!void {
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        switch (expr.data) {
            .str => |segments| for (segments) |segment| try self.collectReassignedBindersInExpr(segment, out),
            .list => |items| for (items) |item| try self.collectReassignedBindersInExpr(item, out),
            .tuple => |items| for (items) |item| try self.collectReassignedBindersInExpr(item, out),
            .match_ => |match| {
                try self.collectReassignedBindersInExpr(match.cond, out);
                for (match.branches) |branch| {
                    if (branch.guard) |guard| try self.collectReassignedBindersInExpr(guard, out);
                    try self.collectReassignedBindersInExpr(branch.value, out);
                }
            },
            .if_ => |if_| {
                for (if_.branches) |branch| {
                    try self.collectReassignedBindersInExpr(branch.cond, out);
                    try self.collectReassignedBindersInExpr(branch.body, out);
                }
                try self.collectReassignedBindersInExpr(if_.final_else, out);
            },
            .call => |call| {
                try self.collectReassignedBindersInExpr(call.func, out);
                for (call.args) |arg| try self.collectReassignedBindersInExpr(arg, out);
            },
            .record => |record| {
                if (record.ext) |ext| try self.collectReassignedBindersInExpr(ext, out);
                for (record.fields) |field| try self.collectReassignedBindersInExpr(field.value, out);
            },
            .block => |block| {
                for (block.statements) |statement| try self.collectReassignedBindersInStatement(statement, out);
                try self.collectReassignedBindersInExpr(block.final_expr, out);
            },
            .tag => |tag| for (tag.args) |arg| try self.collectReassignedBindersInExpr(arg, out),
            .nominal => |nominal| try self.collectReassignedBindersInExpr(nominal.backing_expr, out),
            .binop => |binop| {
                try self.collectReassignedBindersInExpr(binop.lhs, out);
                try self.collectReassignedBindersInExpr(binop.rhs, out);
            },
            .unary_minus,
            .unary_not,
            .dbg,
            .expect,
            => |child| try self.collectReassignedBindersInExpr(child, out),
            .field_access => |field| try self.collectReassignedBindersInExpr(field.receiver, out),
            .structural_eq => |eq| {
                try self.collectReassignedBindersInExpr(eq.lhs, out);
                try self.collectReassignedBindersInExpr(eq.rhs, out);
            },
            .tuple_access => |access| try self.collectReassignedBindersInExpr(access.tuple, out),
            .return_ => |ret| try self.collectReassignedBindersInExpr(ret.expr, out),
            .for_ => |for_| {
                try self.collectReassignedBindersInExpr(for_.expr, out);
                try self.collectReassignedBindersInExpr(for_.body, out);
            },
            .run_low_level => |low_level| for (low_level.args) |arg| try self.collectReassignedBindersInExpr(arg, out),
            .lambda,
            .closure,
            .hosted_lambda,
            .pending,
            .num,
            .frac_f32,
            .frac_f64,
            .dec,
            .dec_small,
            .typed_int,
            .typed_frac,
            .str_segment,
            .bytes_literal,
            .lookup_local,
            .lookup_external,
            .lookup_required,
            .empty_list,
            .empty_record,
            .zero_argument_tag,
            .dispatch_call,
            .method_eq,
            .type_dispatch_call,
            .runtime_error,
            .crash,
            .ellipsis,
            .anno_only,
            => {},
        }
    }

    fn collectReassignedBindersInStatement(
        self: *BodyContext,
        statement_id: checked.CheckedStatementId,
        out: *std.ArrayList(checked.PatternBinderId),
    ) Allocator.Error!void {
        const statement = self.view.bodies.statements[@intFromEnum(statement_id)];
        switch (statement.data) {
            .decl => |decl| try self.collectReassignedBindersInExpr(decl.expr, out),
            .var_ => |var_| try self.collectReassignedBindersInExpr(var_.expr, out),
            .reassign => |reassign| {
                for (reassign.reassigned_binders) |binder| try self.appendUniqueBinder(out, binder);
                try self.collectReassignedBindersInExpr(reassign.expr, out);
            },
            .dbg,
            .expr,
            .expect,
            => |expr| try self.collectReassignedBindersInExpr(expr, out),
            .for_ => |for_| {
                try self.collectReassignedBindersInExpr(for_.expr, out);
                try self.collectReassignedBindersInExpr(for_.body, out);
            },
            .while_ => |while_| {
                try self.collectReassignedBindersInExpr(while_.cond, out);
                try self.collectReassignedBindersInExpr(while_.body, out);
            },
            .return_ => |ret| try self.collectReassignedBindersInExpr(ret.expr, out),
            .pending,
            .crash,
            .break_,
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            .runtime_error,
            => {},
        }
    }

    fn appendUniqueBinder(
        self: *BodyContext,
        out: *std.ArrayList(checked.PatternBinderId),
        binder: checked.PatternBinderId,
    ) Allocator.Error!void {
        for (out.items) |existing| {
            if (existing == binder) return;
        }
        try out.append(self.allocator, binder);
    }

    const IterStepShape = struct {
        step_ty: checked.CheckedTypeId,
        done_tag: names.TagNameId,
        one_tag: names.TagNameId,
        skip_tag: names.TagNameId,
        one_payload_ty: checked.CheckedTypeId,
        skip_payload_ty: checked.CheckedTypeId,
        one_item: checked.CheckedRecordField,
        one_rest: checked.CheckedRecordField,
        skip_count: checked.CheckedRecordField,
        skip_rest: checked.CheckedRecordField,
    };

    fn iteratorStepShape(self: *BodyContext, step_ty: checked.CheckedTypeId) IterStepShape {
        const step_payload = resolvedPayload(self.view, step_ty).payload;
        const tags = switch (step_payload) {
            .tag_union => |tag_union| tag_union.tags,
            else => Common.invariant("iterator .next plan did not return a tag union"),
        };

        var done_tag: ?names.TagNameId = null;
        var one_tag: ?names.TagNameId = null;
        var skip_tag: ?names.TagNameId = null;
        var one_payload_ty: ?checked.CheckedTypeId = null;
        var skip_payload_ty: ?checked.CheckedTypeId = null;

        for (tags) |tag| {
            const tag_text = self.view.names.tagLabelText(tag.name);
            if (Ident.textEql(tag_text, "Done")) {
                if (tag.args.len != 0) Common.invariant("iterator Done step carried payloads");
                done_tag = tag.name;
            } else if (Ident.textEql(tag_text, "One")) {
                if (tag.args.len != 1) Common.invariant("iterator One step did not carry one payload");
                one_tag = tag.name;
                one_payload_ty = tag.args[0];
            } else if (Ident.textEql(tag_text, "Skip")) {
                if (tag.args.len != 1) Common.invariant("iterator Skip step did not carry one payload");
                skip_tag = tag.name;
                skip_payload_ty = tag.args[0];
            }
        }

        const one_payload = one_payload_ty orelse Common.invariant("iterator step type was missing One");
        const skip_payload = skip_payload_ty orelse Common.invariant("iterator step type was missing Skip");
        const one_fields = checkedRecordFields(self.view, one_payload);
        const skip_fields = checkedRecordFields(self.view, skip_payload);

        return .{
            .step_ty = step_ty,
            .done_tag = done_tag orelse Common.invariant("iterator step type was missing Done"),
            .one_tag = one_tag orelse Common.invariant("iterator step type was missing One"),
            .skip_tag = skip_tag orelse Common.invariant("iterator step type was missing Skip"),
            .one_payload_ty = one_payload,
            .skip_payload_ty = skip_payload,
            .one_item = checkedRecordFieldByName(self.view, one_fields, "item"),
            .one_rest = checkedRecordFieldByName(self.view, one_fields, "rest"),
            .skip_count = checkedRecordFieldByName(self.view, skip_fields, "count"),
            .skip_rest = checkedRecordFieldByName(self.view, skip_fields, "rest"),
        };
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
            .decl => |decl| blk: {
                if (self.statementValueIsLocalProc(decl.expr)) {
                    const unit_ty = try self.unitType();
                    break :blk .{ .expr = try self.builder.program.addExpr(.{ .ty = unit_ty, .data = .unit }) };
                }
                const value = try self.lowerExpr(decl.expr);
                const value_ty = self.builder.program.exprs.items[@intFromEnum(value)].ty;
                break :blk .{ .let_ = .{
                    .pat = try self.lowerPatternAtType(decl.pattern, value_ty),
                    .value = value,
                } };
            },
            .var_ => |decl| blk: {
                const value = try self.lowerExpr(decl.expr);
                const value_ty = self.builder.program.exprs.items[@intFromEnum(value)].ty;
                break :blk .{ .let_ = .{
                    .pat = try self.lowerPatternAtType(decl.pattern, value_ty),
                    .value = value,
                } };
            },
            .reassign => |decl| blk: {
                const value = try self.lowerExpr(decl.expr);
                const value_ty = self.builder.program.exprs.items[@intFromEnum(value)].ty;
                break :blk .{ .let_ = .{
                    .pat = try self.lowerPatternAtType(decl.pattern, value_ty),
                    .value = value,
                } };
            },
            .crash => |msg| .{ .crash = try self.lowerStringLiteral(msg) },
            .dbg => |child| .{ .dbg = try self.lowerDbgMessage(child) },
            .expr => |child| try self.lowerExprStatement(child),
            .expect => |child| .{ .expect = try self.lowerExpr(child) },
            .for_ => |for_| blk: {
                var reassigned = std.ArrayList(checked.PatternBinderId).empty;
                defer reassigned.deinit(self.allocator);
                try self.collectReassignedBindersInExpr(for_.body, &reassigned);

                const carries = try self.prepareLoopCarries(reassigned.items);
                defer self.allocator.free(carries);

                const unit_ty = try self.unitType();
                const loop_ty = try self.loopStateType(unit_ty, carries);
                const expr = try self.builder.program.addExpr(.{ .ty = loop_ty, .data = try self.lowerIteratorFor(for_, loop_ty, carries) });
                if (carries.len == 0) break :blk .{ .expr = expr };

                break :blk .{ .let_ = .{
                    .pat = try self.finalCarryPattern(carries, loop_ty),
                    .value = expr,
                } };
            },
            .while_ => |while_| blk: {
                var reassigned = std.ArrayList(checked.PatternBinderId).empty;
                defer reassigned.deinit(self.allocator);
                try self.collectReassignedBindersInExpr(while_.cond, &reassigned);
                try self.collectReassignedBindersInExpr(while_.body, &reassigned);

                const carries = try self.prepareLoopCarries(reassigned.items);
                defer self.allocator.free(carries);

                const unit_ty = try self.unitType();
                const loop_ty = try self.loopStateType(unit_ty, carries);
                const expr = try self.builder.program.addExpr(.{ .ty = loop_ty, .data = try self.lowerWhile(while_, loop_ty, carries) });
                if (carries.len == 0) break :blk .{ .expr = expr };

                break :blk .{ .let_ = .{
                    .pat = try self.finalCarryPattern(carries, loop_ty),
                    .value = expr,
                } };
            },
            .break_ => .{ .expr = try self.breakCurrentLoopExpr() },
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.expr) },
        };
        return try self.builder.program.addStmt(stmt);
    }

    fn lowerExprStatement(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error!Ast.Stmt {
        const merge_binders = try self.stateMergeBinders(expr_id);
        defer self.allocator.free(merge_binders);
        if (merge_binders.len == 0) return .{ .expr = try self.lowerExpr(expr_id) };

        const state_ty = try self.stateOnlyType(merge_binders);
        const checked_expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        const state_expr = switch (checked_expr.data) {
            .if_ => |if_| try self.builder.program.addExpr(.{
                .ty = state_ty,
                .data = try self.lowerIfStateOnly(if_, state_ty, merge_binders),
            }),
            else => try self.lowerBodyThenStateOnly(expr_id, state_ty, merge_binders),
        };
        return .{ .let_ = .{
            .pat = try self.stateOnlyPattern(state_ty, merge_binders),
            .value = state_expr,
        } };
    }

    fn statementValueIsLocalProc(self: *BodyContext, expr_id: checked.CheckedExprId) bool {
        return switch (self.view.bodies.exprs[@intFromEnum(expr_id)].data) {
            .lambda,
            .closure,
            => true,
            else => false,
        };
    }

    fn lowerPatternAtType(self: *BodyContext, pattern_id: checked.CheckedPatternId, ty: Type.TypeId) Allocator.Error!Ast.PatId {
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
        try self.bindTypeToMono(pattern.ty, ty, "checked pattern type mapped one checked type to two monotype types");
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
                    .pattern = try self.lowerPatternAtType(as.pattern, ty),
                    .local = local,
                } };
            },
            .applied_tag => |tag| try self.lowerTagPattern(tag, ty),
            .nominal => |nominal| .{ .nominal = try self.lowerPatternAtType(nominal.backing_pattern, self.builder.namedBackingType(ty) orelse ty) },
            .record_destructure => |destructs| try self.lowerRecordPattern(destructs, ty),
            .list => Common.invariant("list pattern must be lowered to explicit list operations before Monotype publication"),
            .tuple => |items| .{ .tuple = try self.lowerTuplePattern(items, ty) },
            .num_literal => |num| self.lowerNumPattern(num.value, ty),
            .small_dec_literal => Common.invariant("small decimal pattern reached Monotype after numeric finalization"),
            .dec_literal => |dec| .{ .dec_lit = dec.value },
            .frac_f32_literal => |value| .{ .frac_f32_lit = value },
            .frac_f64_literal => |value| .{ .frac_f64_lit = value },
            .str_literal => |str| .{ .str_lit = try self.lowerStringLiteral(str) },
            .underscore => .wildcard,
        };
        return try self.builder.program.addPat(.{ .ty = ty, .data = data });
    }

    fn lowerPatternSpanAtTypes(
        self: *BodyContext,
        checked_patterns: []const checked.CheckedPatternId,
        tys: []const Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.PatId) {
        if (checked_patterns.len != tys.len) Common.invariant("pattern arity differs from concrete checked type");
        const lowered = try self.allocator.alloc(Ast.PatId, checked_patterns.len);
        defer self.allocator.free(lowered);
        for (checked_patterns, tys, 0..) |child, child_ty, i| {
            lowered[i] = try self.lowerPatternAtType(child, child_ty);
        }
        return try self.builder.program.addPatSpan(lowered);
    }

    fn lowerTuplePattern(self: *BodyContext, items: []const checked.CheckedPatternId, ty: Type.TypeId) Allocator.Error!Ast.Span(Ast.PatId) {
        return try self.lowerPatternSpanAtTypes(items, self.builder.tupleItemTypes(ty));
    }

    fn lowerTagPattern(self: *BodyContext, tag: anytype, ty: Type.TypeId) Allocator.Error!Ast.PatData {
        const name = try self.builder.tagName(self.view, tag.name);
        return .{ .tag = .{
            .name = name,
            .payloads = try self.lowerPatternSpanAtTypes(tag.args, self.builder.tagPayloadTypes(ty, name)),
        } };
    }

    fn lowerRecordPattern(self: *BodyContext, destructs: []const checked.CheckedRecordDestruct, ty: Type.TypeId) Allocator.Error!Ast.PatData {
        var lowered = std.ArrayList(Ast.RecordDestruct).empty;
        defer lowered.deinit(self.allocator);
        for (destructs) |destruct| {
            const child = switch (destruct.kind) {
                .required => |pattern| pattern,
                .sub_pattern => |pattern| pattern,
                .rest => |pattern| {
                    if (self.patternIsIgnored(pattern)) continue;
                    Common.invariant("record rest pattern must be lowered to explicit rest-record construction before Monotype publication");
                },
            };
            const name = try self.builder.recordFieldName(self.view, destruct.label);
            const child_ty = switch (destruct.kind) {
                .required, .sub_pattern => self.builder.recordFieldType(ty, name),
                .rest => unreachable,
            };
            try lowered.append(self.allocator, .{
                .name = name,
                .pattern = try self.lowerPatternAtType(child, child_ty),
            });
        }
        return .{ .record = try self.builder.program.addRecordDestructSpan(lowered.items) };
    }

    fn patternIsIgnored(self: *BodyContext, pattern_id: checked.CheckedPatternId) bool {
        return switch (self.view.bodies.patterns[@intFromEnum(pattern_id)].data) {
            .underscore => true,
            else => false,
        };
    }

    fn lowerNumPattern(self: *BodyContext, value: can.CIR.IntValue, ty: Type.TypeId) Ast.PatData {
        return if (self.builder.typeIsDec(ty))
            .{ .dec_lit = intValueToDec(value) }
        else
            .{ .int_lit = value };
    }

    fn lowerIntLiteral(self: *BodyContext, value: can.CIR.IntValue, ty: Type.TypeId) Ast.ExprData {
        return switch (self.builder.shapeContent(ty)) {
            .primitive => |primitive| switch (primitive) {
                .f32 => .{ .frac_f32_lit = @floatCast(intValueToF64(value)) },
                .f64 => .{ .frac_f64_lit = intValueToF64(value) },
                .dec => .{ .dec_lit = intValueToDec(value) },
                else => .{ .int_lit = value },
            },
            else => .{ .int_lit = value },
        };
    }
};

fn moduleView(view: checked.ImportedModuleView) ModuleView {
    return .{
        .key = view.key,
        .module_identity = view.module_identity,
        .names = checked.importedNames(view),
        .types = view.checked_types,
        .bodies = view.checked_bodies,
        .checked_const_bodies = view.checked_const_bodies,
        .templates = view.checked_procedure_templates,
        .compile_time_roots = view.compile_time_roots,
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
        .callable_eval_templates = view.callable_eval_templates,
        .const_templates = view.const_templates,
        .const_store = view.const_store,
        .interface_capabilities = view.interface_capabilities,
    };
}

fn moduleViewNameMatches(view: ModuleView, module_name: []const u8) bool {
    return Ident.textEql(module_name, view.names.moduleNameText(view.module_identity.module_name)) or
        Ident.textEql(module_name, view.names.moduleNameText(view.module_identity.display_module_name)) or
        Ident.textEql(module_name, view.names.moduleNameText(view.module_identity.qualified_module_name));
}

fn fnDefForTemplate(
    view: ModuleView,
    template: names.ProcTemplate,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    mono_fn_ty: Type.TypeId,
) Ast.FnTemplate {
    if (moduleBytesEqual(view.key.bytes, names.procTemplateModuleDigest(template).bytes)) {
        return .{
            .fn_def = .{ .local_template = template },
            .source_fn_ty = source_fn_ty,
            .source_fn_key = source_fn_key,
            .mono_fn_ty = mono_fn_ty,
        };
    }
    return .{
        .fn_def = .{ .imported_template = template },
        .source_fn_ty = source_fn_ty,
        .source_fn_key = source_fn_key,
        .mono_fn_ty = mono_fn_ty,
    };
}

fn fnDefForProcedureBindingBody(
    view: ModuleView,
    body: checked.ProcedureBindingBody,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    mono_fn_ty: Type.TypeId,
) Ast.FnTemplate {
    return switch (body) {
        .direct_template => |direct| fnDefForCallableTemplate(view, direct.template, source_fn_ty, source_fn_key, mono_fn_ty),
        .callable_eval_template => Common.invariant("callable eval template must be restored through ConstStore before Monotype lowering"),
    };
}

fn fnDefForImportedBindingBody(
    view: ModuleView,
    body: checked.ImportedProcedureBindingBody,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    mono_fn_ty: Type.TypeId,
) Ast.FnTemplate {
    return switch (body) {
        .direct_template => |direct| fnDefForCallableTemplate(view, direct.template, source_fn_ty, source_fn_key, mono_fn_ty),
        .callable_eval_template => Common.invariant("imported callable eval template must be restored through ConstStore before Monotype lowering"),
    };
}

fn fnDefForCallableTemplate(
    view: ModuleView,
    template: names.CallableProcTemplate,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    mono_fn_ty: Type.TypeId,
) Ast.FnTemplate {
    return switch (template) {
        .checked => |checked_template| fnDefForTemplate(view, checked_template, source_fn_ty, source_fn_key, mono_fn_ty),
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
    if (raw >= view.types.payloads.len) Common.invariant("checked type id outside checked type store");
    return view.types.payloads[raw];
}

fn constFnTemplateToMono(type_view: ModuleView, fn_value: anytype, mono_fn_ty: Type.TypeId) Ast.FnTemplate {
    return .{
        .fn_def = constFnDefToMono(fn_value.fn_def),
        .source_fn_ty = fn_value.source_fn_ty,
        .source_fn_key = type_view.types.rootKey(fn_value.source_fn_ty),
        .mono_fn_ty = mono_fn_ty,
    };
}

fn constFnDefToMono(fn_def: anytype) Ast.FnDef {
    return switch (fn_def) {
        .local_template => |template| .{ .local_template = template },
        .imported_template => |template| .{ .imported_template = template },
        .nested => |nested| .{ .nested = .{ .owner = nested.owner, .site = nested.site, .context_fn_key = .{} } },
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

fn checkedTypePayloadIsOpenVariable(payload: checked.CheckedTypePayload) bool {
    return switch (payload) {
        .flex,
        .rigid,
        => true,
        else => false,
    };
}

fn checkedTypePayloadHasMonoDefault(payload: checked.CheckedTypePayload) bool {
    return switch (payload) {
        .flex => |variable| variable.numeric_default_phase == .mono_specialization,
        .rigid => |variable| variable.numeric_default_phase == .mono_specialization,
        else => false,
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

fn primitiveInspectLowLevelOp(primitive: Type.Primitive) can.CIR.Expr.LowLevel {
    return switch (primitive) {
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
    };
}

fn recordFieldLessThan(builder: *Builder, lhs: Type.Field, rhs: Type.Field) bool {
    return builder.program.names.recordFieldLabelTextLessThan(lhs.name, rhs.name);
}

fn tagLessThan(builder: *Builder, lhs: Type.Tag, rhs: Type.Tag) bool {
    return builder.program.names.tagLabelTextLessThan(lhs.name, rhs.name);
}

fn assertNoDuplicateRecordFields(builder: *Builder, fields: []const Type.Field, comptime message: []const u8) void {
    if (fields.len < 2) return;
    for (fields[1..], 1..) |field, i| {
        if (builder.program.names.recordFieldLabelTextEql(fields[i - 1].name, field.name)) {
            Common.invariant(message);
        }
    }
}

fn assertNoDuplicateTags(builder: *Builder, tags: []const Type.Tag, comptime message: []const u8) void {
    if (tags.len < 2) return;
    for (tags[1..], 1..) |tag, i| {
        if (builder.program.names.tagLabelTextEql(tags[i - 1].name, tag.name)) {
            Common.invariant(message);
        }
    }
}

const ResolvedPayload = struct {
    root: checked.CheckedTypeId,
    payload: checked.CheckedTypePayload,
};

fn resolvedPayload(view: ModuleView, ty: checked.CheckedTypeId) ResolvedPayload {
    var current = ty;
    var remaining = view.types.payloads.len;
    while (remaining > 0) : (remaining -= 1) {
        const payload = checkedPayload(view, current);
        switch (payload) {
            .alias => |alias| current = alias.backing,
            else => return .{ .root = current, .payload = payload },
        }
    }
    Common.invariant("checked type alias chain was cyclic");
}

fn checkedRecordFieldByName(
    view: ModuleView,
    fields: []const checked.CheckedRecordField,
    field_name: []const u8,
) checked.CheckedRecordField {
    for (fields) |field| {
        if (Ident.textEql(view.names.recordFieldLabelText(field.name), field_name)) return field;
    }
    Common.invariant("expected checked record field was absent");
}

fn intValueToDec(value: can.CIR.IntValue) builtins.dec.RocDec {
    const whole = switch (value.kind) {
        .i128 => @as(i128, @bitCast(value.bytes)),
        .u128 => blk: {
            const unsigned = @as(u128, @bitCast(value.bytes));
            if (unsigned > @as(u128, @intCast(std.math.maxInt(i128)))) {
                Common.invariant("integer pattern solved as Dec exceeded Dec whole-number range");
            }
            break :blk @as(i128, @intCast(unsigned));
        },
    };
    return builtins.dec.RocDec.fromWholeInt(whole) orelse {
        Common.invariant("integer pattern solved as Dec exceeded Dec range");
    };
}

fn intValueToF64(value: can.CIR.IntValue) f64 {
    return switch (value.kind) {
        .i128 => builtins.compiler_rt_128.i128_to_f64(@as(i128, @bitCast(value.bytes))),
        .u128 => builtins.compiler_rt_128.u128_to_f64(@as(u128, @bitCast(value.bytes))),
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

const CheckedTypeRelation = struct {
    left: CheckedTypeAddress,
    right: CheckedTypeAddress,
};

const TemplateAddress = struct {
    module_bytes: [32]u8,
    proc_base: u32,
    template: u32,
    source_fn_key_bytes: [32]u8,
    mono_fn_ty_digest_bytes: [32]u8,

    fn from(template: names.ProcTemplate, source_fn_key: names.TypeDigest, mono_fn_ty_digest: names.TypeDigest) TemplateAddress {
        return .{
            .module_bytes = names.procTemplateModuleDigest(template).bytes,
            .proc_base = @intFromEnum(template.proc_base),
            .template = @intFromEnum(template.template),
            .source_fn_key_bytes = source_fn_key.bytes,
            .mono_fn_ty_digest_bytes = mono_fn_ty_digest.bytes,
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

const NestedFnTemplateAddress = struct {
    module_bytes: [32]u8,
    owner_proc_base: u32,
    owner_template: u32,
    site: u32,
    context_fn_key_bytes: [32]u8,
    source_fn_key_bytes: [32]u8,
    mono_fn_ty_digest_bytes: [32]u8,

    fn from(nested: Ast.NestedFn, source_fn_key: names.TypeDigest, mono_fn_ty_digest: names.TypeDigest) NestedFnTemplateAddress {
        return .{
            .module_bytes = names.procTemplateModuleDigest(nested.owner).bytes,
            .owner_proc_base = @intFromEnum(nested.owner.proc_base),
            .owner_template = @intFromEnum(nested.owner.template),
            .site = @intFromEnum(nested.site),
            .context_fn_key_bytes = nested.context_fn_key.bytes,
            .source_fn_key_bytes = source_fn_key.bytes,
            .mono_fn_ty_digest_bytes = mono_fn_ty_digest.bytes,
        };
    }
};

test "monotype lower declarations are referenced" {
    std.testing.refAllDecls(@This());
    _ = can;
    _ = builtins;
}

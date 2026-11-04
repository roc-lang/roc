//! A read-only view of a checked module's types read directly from its sealed
//! solver type store.
//!
//! The view renders each solver variable the way checked type publication
//! (`CheckedTypePublisher` in checked_artifact.zig) renders it into a
//! `CheckedTypePayload`, through the same decision helpers: canonical names,
//! import owners, nominal representations, numeric defaults, empty-row
//! normalization and function-kind finalization. Its identity is the solver's
//! representative variable rather than a published `CheckedTypeId`, and its
//! children are solver variables whose slices alias the solver store's own
//! pools.
//!
//! A variable's row default is the one decision that depends on more than the
//! variable: it comes from the row-tail positions the variable is reached
//! through. The view computes it once, at construction, by walking every
//! position reachable from the module's checked source type roots
//! (`checked_artifact.forEachCheckedSourceTypeRoot`).
//!
//! `verifyAgainstPublication` walks a checked type publication and the view in
//! lockstep from every published source root and asserts that both render the
//! same structure.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const canonical = @import("canonical_names.zig");
const checked_artifact = @import("checked_artifact.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Var = types.Var;

const CheckedTypeId = checked_artifact.CheckedTypeId;
const CheckedTypeStore = checked_artifact.CheckedTypeStore;
const CheckedTypePayload = checked_artifact.CheckedTypePayload;
const CheckedImportViews = checked_artifact.CheckedImportViews;
const CheckedSourceTypeRoots = checked_artifact.CheckedSourceTypeRoots;
const CheckedSourceTypeRootInputs = checked_artifact.CheckedSourceTypeRootInputs;
const CheckedFieldDefault = checked_artifact.CheckedFieldDefault;
const CheckedFunctionKind = checked_artifact.CheckedFunctionKind;
const CheckedBuiltinNominal = checked_artifact.CheckedBuiltinNominal;
const CheckedCommittedKind = checked_artifact.CheckedCommittedKind;
const CheckedNominalRepresentationRef = checked_artifact.CheckedNominalRepresentationRef;
const ImportedOriginIndex = checked_artifact.ImportedOriginIndex;
const LocalNominalDeclarationIds = checked_artifact.LocalNominalDeclarationIds;
const LocalTypeDeclarationIndex = checked_artifact.LocalTypeDeclarationIndex;
const ModuleId = checked_artifact.ModuleId;
const NumericDefaultPhase = checked_artifact.NumericDefaultPhase;
const RowDefault = checked_artifact.RowDefault;
const invariant = checked_artifact.checkedArtifactInvariant;

/// A checked type variable as the view renders it; carries exactly what a
/// `checked_artifact.CheckedTypeVariable` carries.
pub const SealedTypeVariable = struct {
    name: ?[]const u8,
    /// The variable's constraints in the solver store; `SealedTypeView.constraint`
    /// renders each one.
    constraints: []const types.StaticDispatchConstraint,
    numeric_default_phase: ?NumericDefaultPhase,
    row_default: ?RowDefault,
    row_default_discharges_constraints: bool,
    bounded_row_ext: bool,
};

/// One static-dispatch constraint of a checked type variable.
pub const SealedConstraint = struct {
    fn_name: canonical.MethodNameId,
    fn_var: Var,
    origin: types.StaticDispatchConstraint.Origin,
    numeral: ?types.NumeralInfo,
};

/// A checked alias type.
pub const SealedAliasType = struct {
    name: canonical.TypeNameId,
    origin_module: canonical.ModuleIdentityId,
    owner_module: ModuleId,
    source_decl: ?u32,
    builtin_origin: bool,
    backing: Var,
    args: []const Var,
};

/// A checked nominal type.
pub const SealedNominalType = struct {
    name: canonical.TypeNameId,
    origin_module: canonical.ModuleIdentityId,
    owner_module: ModuleId,
    source_decl: u32,
    builtin: ?CheckedBuiltinNominal,
    is_opaque: bool,
    representation: CheckedNominalRepresentationRef,
    args: []const Var,
};

/// A checked record type with at least one field or an open tail. The
/// fields are the solver store's own; `SealedTypeView.recordField` renders
/// each one.
pub const SealedRecordType = struct {
    fields: types.RecordField.SafeMultiList.Slice,
    ext: Var,
};

/// The field kind axis of one record field (design.md "Field Kinds").
pub const SealedFieldKind = union(enum) {
    required,
    optional,
    defaulted: CheckedFieldDefault,
    /// A still-flex kind variable of a scheme interior.
    undetermined: Var,
    err,
};

/// One record field.
pub const SealedRecordField = struct {
    name: canonical.RecordFieldLabelId,
    ty: Var,
    kind: SealedFieldKind,
};

/// A checked tag union with at least one tag or an open tail. The tags are
/// the solver store's own; `SealedTypeView.tag` renders each one.
pub const SealedTagUnionType = struct {
    tags: types.Tag.SafeMultiList.Slice,
    ext: Var,
};

/// One tag of a tag union.
pub const SealedTag = struct {
    name: canonical.TagLabelId,
    args: []const Var,
};

/// A checked function type.
pub const SealedFunctionType = struct {
    kind: CheckedFunctionKind,
    args: []const Var,
    ret: Var,
};

/// One solver variable as checked type publication renders it, shape for
/// shape with `CheckedTypePayload` (which a sealed store never leaves
/// `pending`).
pub const SealedTypePayload = union(enum) {
    err,
    flex: SealedTypeVariable,
    rigid: SealedTypeVariable,
    alias: SealedAliasType,
    record: SealedRecordType,
    tuple: []const Var,
    nominal: SealedNominalType,
    function: SealedFunctionType,
    empty_record,
    tag_union: SealedTagUnionType,
    empty_tag_union,
    /// The field kind a quantified kind variable was instantiated at.
    field_kind: CheckedCommittedKind,
};

/// Checked types read directly from a module's sealed solver store.
pub const SealedTypeView = struct {
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    /// Owners of the named types the view has rendered, resolved once per
    /// origin module.
    imported_origins: *ImportedOriginIndex,
    local_nominal_declarations: LocalNominalDeclarationIds,
    /// The row default of every representative variable that takes one.
    row_defaults: collections.DenseMap(Var, RowDefault),
    /// The representatives reachable from the root set the view was built
    /// over: the domain whose row defaults the view knows.
    reached: std.DynamicBitSetUnmanaged,

    /// A view over `module`, whose type store checking sealed, with row
    /// defaults computed from `roots` (the variables
    /// `collectSourceTypeRoots` enumerates).
    pub fn init(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        imports: CheckedImportViews,
        roots: []const Var,
    ) Allocator.Error!SealedTypeView {
        const type_store = module.typeStoreConst();
        if (std.debug.runtime_safety and !type_store.isSealed()) {
            invariant("a sealed type view was built over a type store checking never sealed", .{});
        }
        const imported_origins = try allocator.create(ImportedOriginIndex);
        errdefer allocator.destroy(imported_origins);
        imported_origins.* = ImportedOriginIndex.init(allocator);
        errdefer imported_origins.deinit();
        var local_nominal_declarations = try LocalNominalDeclarationIds.init(allocator, module);
        errdefer local_nominal_declarations.deinit();
        var reached = try std.DynamicBitSetUnmanaged.initEmpty(allocator, @intCast(type_store.len()));
        errdefer reached.deinit(allocator);
        var self = SealedTypeView{
            .allocator = allocator,
            .module = module,
            .names = names,
            .imports = imports,
            .imported_origins = imported_origins,
            .local_nominal_declarations = local_nominal_declarations,
            .row_defaults = collections.DenseMap(Var, RowDefault).init(allocator),
            .reached = reached,
        };
        errdefer self.row_defaults.deinit();
        try self.computeRowDefaults(roots);
        return self;
    }

    pub fn deinit(self: *SealedTypeView) void {
        self.row_defaults.deinit();
        self.reached.deinit(self.allocator);
        self.local_nominal_declarations.deinit();
        self.imported_origins.deinit();
        self.allocator.destroy(self.imported_origins);
        self.* = undefined;
    }

    fn typeStore(self: *const SealedTypeView) *const types.Store {
        return self.module.typeStoreConst();
    }

    /// The solver variable that stands for `var_`'s whole equivalence class.
    pub fn representative(self: *const SealedTypeView, var_: Var) Var {
        return self.typeStore().resolveVar(var_).var_;
    }

    /// Whether the view's root set reaches `var_`.
    pub fn reaches(self: *const SealedTypeView, var_: Var) bool {
        return self.reached.isSet(@backingInt(self.representative(var_)));
    }

    /// `var_` rendered as checked type publication renders it. Names are
    /// interned in the view's canonical name store; nothing else allocates
    /// beyond resolving an imported origin's owner the first time.
    pub fn payload(self: *const SealedTypeView, var_: Var) Allocator.Error!SealedTypePayload {
        const module = self.module;
        const type_store = self.typeStore();
        const resolved = type_store.resolveVar(var_);
        if (std.debug.runtime_safety and !self.reached.isSet(@backingInt(resolved.var_))) {
            invariant("a sealed type view rendered variable {d} outside its root closure", .{@backingInt(resolved.var_)});
        }
        // The checker marks an otherwise-unresolved identity it closed to
        // `[]`; it renders as a variable carrying `[]` only as its row default.
        if (resolved.desc.flags.empty_tag_union_is_default) return .{ .flex = .{
            .name = null,
            .constraints = &.{},
            .numeric_default_phase = null,
            .row_default = .empty_tag_union,
            .row_default_discharges_constraints = false,
            .bounded_row_ext = false,
        } };
        const names = self.names;
        const idents = module.identStoreConst();
        switch (resolved.desc.content) {
            .err => return .err,
            .field_presence => |presence| return .{ .field_kind = switch (presence) {
                .required, .defaulted => .required,
                .optional => .optional,
            } },
            .flex => |flex| {
                const facts = checked_artifact.flexVariableFacts(module, flex, resolved.desc.flags.bounded_row_ext);
                return .{ .flex = .{
                    .name = if (flex.name) |name| checked_artifact.checkedVariableNameText(module, name) else null,
                    .constraints = type_store.sliceStaticDispatchConstraints(flex.constraints),
                    .numeric_default_phase = facts.numeric_default_phase,
                    .row_default = self.row_defaults.get(resolved.var_),
                    .row_default_discharges_constraints = facts.row_default_discharges_constraints,
                    .bounded_row_ext = facts.bounded_row_ext,
                } };
            },
            .rigid => |rigid| {
                const facts = checked_artifact.rigidVariableFacts(module, rigid);
                return .{ .rigid = .{
                    .name = checked_artifact.checkedVariableNameText(module, rigid.name),
                    .constraints = type_store.sliceStaticDispatchConstraints(rigid.constraints),
                    .numeric_default_phase = facts.numeric_default_phase,
                    .row_default = self.row_defaults.get(resolved.var_),
                    .row_default_discharges_constraints = facts.row_default_discharges_constraints,
                    .bounded_row_ext = facts.bounded_row_ext,
                } };
            },
            .alias => |alias| return .{ .alias = .{
                .name = try names.internTypeIdent(idents, alias.ident.ident_idx),
                .origin_module = try names.internModuleIdentity(module.moduleEnvConst().moduleIdentityHash(alias.origin_module)),
                .owner_module = try self.imported_origins.owner(module.moduleEnvConst(), self.imports, alias.origin_module),
                .source_decl = alias.source_decl.toOptional(),
                .builtin_origin = alias.source_decl.originIsBuiltin(),
                .backing = type_store.getAliasBackingVar(alias),
                .args = type_store.sliceAliasArgs(alias),
            } },
            .structure => |flat| switch (flat) {
                .empty_record => return .empty_record,
                .empty_tag_union => return .empty_tag_union,
                .record => |record| {
                    if (record.fields.len() == 0 and checked_artifact.checkedRecordExtIsEmpty(module, record.ext)) return .empty_record;
                    return .{ .record = .{ .fields = type_store.getRecordFieldsSlice(record.fields), .ext = record.ext } };
                },
                .tuple => |tuple| return .{ .tuple = type_store.sliceVars(tuple.elems) },
                .nominal_type => |nominal| {
                    checked_artifact.debugAssertNominalOpacityMatchesDeclaration(type_store, nominal);
                    const module_env = module.moduleEnvConst();
                    const builtin_nominal = try self.imported_origins.builtinNominal(module_env, self.imports, nominal);
                    return .{ .nominal = .{
                        .name = try names.internTypeIdent(idents, nominal.ident.ident_idx),
                        .origin_module = try names.internModuleIdentity(module_env.moduleIdentityHash(nominal.origin_module)),
                        .owner_module = try self.imported_origins.owner(module_env, self.imports, nominal.origin_module),
                        .source_decl = nominal.statement(),
                        .builtin = builtin_nominal,
                        .is_opaque = nominal.isOpaque(),
                        .representation = try checked_artifact.checkedNominalRepresentationForSourceNominal(
                            module,
                            self.imports,
                            self.imported_origins,
                            &self.local_nominal_declarations,
                            nominal,
                            builtin_nominal,
                        ),
                        .args = type_store.sliceNominalArgs(nominal),
                    } };
                },
                .fn_pure, .fn_unbound => |func| return .{ .function = functionType(type_store, .pure, func) },
                .fn_effectful => |func| return .{ .function = functionType(type_store, .effectful, func) },
                .tag_union => |tag_union| {
                    if (tag_union.tags.len() == 0 and checked_artifact.checkedTagUnionExtIsEmpty(module, tag_union.ext)) return .empty_tag_union;
                    return .{ .tag_union = .{ .tags = type_store.getTagsSlice(tag_union.tags), .ext = tag_union.ext } };
                },
            },
        }
    }

    fn functionType(type_store: *const types.Store, kind: CheckedFunctionKind, func: types.Func) SealedFunctionType {
        return .{
            .kind = checked_artifact.finalizedFunctionKind(kind),
            .args = type_store.sliceVars(func.args),
            .ret = func.ret,
        };
    }

    /// One constraint of a rendered variable.
    pub fn constraint(self: *const SealedTypeView, source: types.StaticDispatchConstraint) Allocator.Error!SealedConstraint {
        return .{
            .fn_name = try self.names.internMethodIdent(self.module.identStoreConst(), source.fn_name),
            .fn_var = source.fn_var,
            .origin = source.origin,
            .numeral = self.typeStore().constraintNumeralInfo(source),
        };
    }

    /// The field at `index` of a rendered record, in the solver row's order.
    pub fn recordField(self: *const SealedTypeView, record: SealedRecordType, index: usize) Allocator.Error!SealedRecordField {
        const module = self.module;
        const name = try self.names.internRecordFieldIdent(module.identStoreConst(), record.fields.items(.name)[index]);
        // The value and kind axes are independent; see design.md "Field Kinds".
        return switch (record.fields.items(.presence)[index].decode()) {
            .required => |type_var| .{ .name = name, .ty = type_var, .kind = .required },
            .unknown => |unknown| .{ .name = name, .ty = unknown.var_, .kind = switch (self.typeStore().resolveVar(unknown.presence).desc.content) {
                .field_presence => |presence| switch (presence) {
                    .required => .required,
                    .defaulted => |id| .{ .defaulted = CheckedFieldDefault.fromParts(
                        try self.names.internModuleIdentity(module.moduleEnvConst().moduleIdentityHash(id.origin_module)),
                        id.expr_node,
                    ) },
                    .optional => .optional,
                },
                .flex => .{ .undetermined = unknown.presence },
                .err => .err,
                .rigid, .alias, .structure => invariant("a sealed type view reached a field presence variable holding non-presence content", .{}),
            } },
        };
    }

    /// The tag at `index` of a rendered tag union, in the solver row's order.
    pub fn tag(self: *const SealedTypeView, tag_union: SealedTagUnionType, index: usize) Allocator.Error!SealedTag {
        return .{
            .name = try self.names.internTagIdent(self.module.identStoreConst(), tag_union.tags.items(.name)[index]),
            .args = self.typeStore().sliceVars(tag_union.tags.items(.args)[index]),
        };
    }

    /// Walk every child position reachable from `roots`, in the order
    /// `CheckedTypePublisher.stepPayload` visits children, recording each
    /// representative reached and the row default its row-tail positions
    /// give it.
    fn computeRowDefaults(self: *SealedTypeView, roots: []const Var) Allocator.Error!void {
        var pending: std.ArrayList(Var) = .empty;
        defer pending.deinit(self.allocator);
        for (roots) |root| try self.reach(&pending, root, null);
        while (pending.pop()) |var_| try self.expand(&pending, var_);
    }

    fn reach(self: *SealedTypeView, pending: *std.ArrayList(Var), var_: Var, candidate: ?RowDefault) Allocator.Error!void {
        const resolved = self.typeStore().resolveVar(var_);
        const index = @backingInt(resolved.var_);
        if (!self.reached.isSet(index)) {
            self.reached.set(index);
            if (resolved.desc.flags.empty_tag_union_is_default) {
                try self.row_defaults.put(resolved.var_, .empty_tag_union);
            } else {
                try pending.append(self.allocator, resolved.var_);
            }
        }
        const row_default = checked_artifact.checkedRowDefaultForContent(self.module, resolved.desc.content, candidate) orelse return;
        const entry = try self.row_defaults.getOrPut(resolved.var_);
        if (entry.found_existing) {
            if (entry.value_ptr.* != row_default) invariant("checked row variable {d} received incompatible defaults", .{index});
            return;
        }
        entry.value_ptr.* = row_default;
    }

    fn expand(self: *SealedTypeView, pending: *std.ArrayList(Var), var_: Var) Allocator.Error!void {
        const module = self.module;
        const type_store = self.typeStore();
        switch (type_store.resolveVar(var_).desc.content) {
            .err, .field_presence => {},
            .flex => |flex| for (type_store.sliceStaticDispatchConstraints(flex.constraints)) |source| {
                try self.reach(pending, source.fn_var, null);
            },
            .rigid => |rigid| for (type_store.sliceStaticDispatchConstraints(rigid.constraints)) |source| {
                try self.reach(pending, source.fn_var, null);
            },
            .alias => |alias| {
                try self.reach(pending, type_store.getAliasBackingVar(alias), null);
                for (type_store.sliceAliasArgs(alias)) |arg| try self.reach(pending, arg, null);
            },
            .structure => |flat| switch (flat) {
                .empty_record, .empty_tag_union => {},
                .record => |record| {
                    if (record.fields.len() == 0 and checked_artifact.checkedRecordExtIsEmpty(module, record.ext)) return;
                    for (type_store.getRecordFieldsSlice(record.fields).items(.presence)) |presence| {
                        switch (presence.decode()) {
                            .required => |type_var| try self.reach(pending, type_var, null),
                            .unknown => |unknown| {
                                switch (type_store.resolveVar(unknown.presence).desc.content) {
                                    .flex => try self.reach(pending, unknown.presence, null),
                                    .field_presence, .err => {},
                                    .rigid, .alias, .structure => invariant("a sealed type view reached a field presence variable holding non-presence content", .{}),
                                }
                                try self.reach(pending, unknown.var_, null);
                            },
                        }
                    }
                    try self.reach(pending, record.ext, .empty_record);
                },
                .tuple => |tuple| for (type_store.sliceVars(tuple.elems)) |elem| try self.reach(pending, elem, null),
                .nominal_type => |nominal| for (type_store.sliceNominalArgs(nominal)) |arg| try self.reach(pending, arg, null),
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    for (type_store.sliceVars(func.args)) |arg| try self.reach(pending, arg, null);
                    try self.reach(pending, func.ret, null);
                },
                .tag_union => |tag_union| {
                    if (tag_union.tags.len() == 0 and checked_artifact.checkedTagUnionExtIsEmpty(module, tag_union.ext)) return;
                    for (type_store.getTagsSlice(tag_union.tags).items(.args)) |args| {
                        for (type_store.sliceVars(args)) |arg| try self.reach(pending, arg, null);
                    }
                    try self.reach(pending, tag_union.ext, .empty_tag_union);
                },
            },
        }
    }
};

/// Append every source variable `module`'s checked type publication
/// publishes as a root to `out`: the roots
/// `checked_artifact.forEachCheckedSourceTypeRoot` enumerates, and for each
/// local nominal declaration the variables its declaration walk publishes.
pub fn collectSourceTypeRoots(
    allocator: Allocator,
    module: TypedCIR.Module,
    inputs: CheckedSourceTypeRootInputs,
    out: *std.ArrayList(Var),
) Allocator.Error!void {
    var local_type_declarations = try LocalTypeDeclarationIndex.init(allocator, module, inputs.source_nodes);
    defer local_type_declarations.deinit();
    var collector = RootCollector{
        .allocator = allocator,
        .module = module,
        .local_type_declarations = &local_type_declarations,
        .out = out,
    };
    defer collector.deinit();
    try checked_artifact.forEachCheckedSourceTypeRoot(allocator, module, inputs, &collector);
}

/// A root visitor that records each root's variable. A nominal declaration
/// publishes its statement, its formals, and the leaves of its annotation
/// walk (`DeclarationAnnoWalk`), which the collector enumerates through the
/// walk's own classification (`declarationAnnoNode`, `declarationApplyBase`).
const RootCollector = struct {
    allocator: Allocator,
    module: TypedCIR.Module,
    local_type_declarations: *const LocalTypeDeclarationIndex,
    out: *std.ArrayList(Var),
    /// Local aliases whose bodies a declaration walk has enumerated. A body's
    /// leaves depend only on the alias's own formals, so each is walked once.
    walked_aliases: std.AutoHashMapUnmanaged(CIR.Statement.Idx, void) = .empty,
    annos: std.ArrayList(PendingAnno) = .empty,

    /// An annotation still to walk, under the formals (header argument
    /// annotations) of the declaration whose body holds it.
    const PendingAnno = struct {
        anno: CIR.TypeAnno.Idx,
        formals: []const CIR.TypeAnno.Idx,
    };

    fn deinit(self: *RootCollector) void {
        self.walked_aliases.deinit(self.allocator);
        self.annos.deinit(self.allocator);
    }

    pub fn visit(self: *RootCollector, var_: Var) Allocator.Error!void {
        try self.out.append(self.allocator, var_);
    }

    pub fn visitRequired(self: *RootCollector, maybe_var: ?Var, comptime missing: []const u8) Allocator.Error!void {
        try self.visit(maybe_var orelse invariant(missing, .{}));
    }

    pub fn visitScheme(self: *RootCollector, var_: Var) Allocator.Error!void {
        try self.visit(var_);
    }

    /// The roots `appendCheckedNominalDeclarationFromStatement` publishes.
    pub fn visitNominalDeclaration(self: *RootCollector, statement_idx: CIR.Statement.Idx, nominal: checked_artifact.NominalDeclStatement) Allocator.Error!void {
        if (nominal.anno == .placeholder) return;
        if (!checked_artifact.localNominalDeclarationIsValid(self.module, statement_idx)) return;
        try self.visit(ModuleEnv.varFrom(statement_idx));

        const store = &self.module.moduleEnvConst().store;
        const formals = store.sliceTypeAnnos(store.getTypeHeader(nominal.header).args);
        for (formals) |formal| {
            if (store.getTypeAnno(formal) != .rigid_var) {
                invariant("nominal declaration header argument was not a rigid type variable", .{});
            }
            try self.visit(ModuleEnv.varFrom(formal));
        }

        // Unnamed fields of a record backing publish as padding.
        var record_anno = nominal.anno;
        while (store.getTypeAnno(record_anno) == .parens) record_anno = store.getTypeAnno(record_anno).parens.anno;
        switch (store.getTypeAnno(record_anno)) {
            .record => |record| for (store.sliceAnnoRecordFields(record.fields)) |field_idx| {
                const field = store.getAnnoRecordField(field_idx);
                if (field.is_unnamed) try self.walk(field.ty, formals);
            },
            .apply,
            .rigid_var,
            .rigid_var_lookup,
            .underscore,
            .lookup,
            .tag_union,
            .tag,
            .tuple,
            .@"fn",
            .parens,
            .malformed,
            => {},
        }
        try self.walk(nominal.anno, formals);
    }

    fn walk(self: *RootCollector, root: CIR.TypeAnno.Idx, root_formals: []const CIR.TypeAnno.Idx) Allocator.Error!void {
        const gpa = self.allocator;
        const module = self.module;
        const store = &module.moduleEnvConst().store;
        try self.annos.append(gpa, .{ .anno = root, .formals = root_formals });
        while (self.annos.pop()) |pending| {
            const formals = pending.formals;
            switch (checked_artifact.declarationAnnoNode(module, self.local_type_declarations, pending.anno)) {
                .leaf => |var_| try self.visit(var_),
                .rigid => |rigid| {
                    if (rigid.name) |name| if (formalNamed(store, formals, name)) continue;
                    try self.visit(rigid.var_);
                },
                .local_alias => |local_alias| try self.enterAlias(local_alias.statement),
                .composite => |composite| switch (store.getTypeAnno(composite)) {
                    .tag_union => |tag_union| {
                        for (store.sliceTypeAnnos(tag_union.tags)) |tag_anno| {
                            const tag = switch (store.getTypeAnno(tag_anno)) {
                                .tag => |tag| tag,
                                .apply,
                                .rigid_var,
                                .rigid_var_lookup,
                                .underscore,
                                .lookup,
                                .tag_union,
                                .tuple,
                                .record,
                                .@"fn",
                                .parens,
                                .malformed,
                                => invariant("nominal declaration tag union contained a non-tag annotation", .{}),
                            };
                            try self.pushAll(store.sliceTypeAnnos(tag.args), formals);
                        }
                        if (tag_union.ext) |ext| try self.annos.append(gpa, .{ .anno = ext, .formals = formals });
                    },
                    .record => |record| {
                        for (store.sliceAnnoRecordFields(record.fields)) |field_idx| {
                            const field = store.getAnnoRecordField(field_idx);
                            if (!field.is_unnamed) try self.annos.append(gpa, .{ .anno = field.ty, .formals = formals });
                        }
                        if (record.ext) |ext| try self.annos.append(gpa, .{ .anno = ext, .formals = formals });
                    },
                    .tuple => |tuple| try self.pushAll(store.sliceTypeAnnos(tuple.elems), formals),
                    .@"fn" => |func| {
                        try self.pushAll(store.sliceTypeAnnos(func.args), formals);
                        try self.annos.append(gpa, .{ .anno = func.ret, .formals = formals });
                    },
                    .apply => |apply| {
                        const args = store.sliceTypeAnnos(apply.args);
                        try self.pushAll(args, formals);
                        switch (checked_artifact.declarationApplyBase(module, self.local_type_declarations, composite, apply, args.len)) {
                            .leaf, .template => |var_| try self.visit(var_),
                            .alias => |statement| try self.enterAlias(statement),
                        }
                    },
                    .rigid_var,
                    .rigid_var_lookup,
                    .underscore,
                    .lookup,
                    .tag,
                    .parens,
                    .malformed,
                    => unreachable, // declarationAnnoNode classifies only these five annotation kinds as composite
                },
            }
        }
    }

    fn pushAll(self: *RootCollector, annos: []const CIR.TypeAnno.Idx, formals: []const CIR.TypeAnno.Idx) Allocator.Error!void {
        for (annos) |anno| try self.annos.append(self.allocator, .{ .anno = anno, .formals = formals });
    }

    /// An alias reference publishes the alias's own root and walks its body
    /// under the alias's formals.
    fn enterAlias(self: *RootCollector, statement_idx: CIR.Statement.Idx) Allocator.Error!void {
        try self.visit(ModuleEnv.varFrom(statement_idx));
        const walked = try self.walked_aliases.getOrPut(self.allocator, statement_idx);
        if (walked.found_existing) return;
        const alias = self.module.getStatement(statement_idx).s_alias_decl;
        if (alias.anno == .placeholder) {
            invariant("checked declaration template alias application resolved to an unfinalized alias declaration", .{});
        }
        const store = &self.module.moduleEnvConst().store;
        const formals = store.sliceTypeAnnos(store.getTypeHeader(alias.header).args);
        for (formals) |formal| {
            if (store.getTypeAnno(formal) != .rigid_var) {
                invariant("alias declaration header argument was not a rigid type variable", .{});
            }
        }
        try self.annos.append(self.allocator, .{ .anno = alias.anno, .formals = formals });
    }

    /// Whether a declaration whose header arguments are `formals` binds `name`.
    fn formalNamed(store: anytype, formals: []const CIR.TypeAnno.Idx, name: Ident.Idx) bool {
        for (formals) |formal| {
            if (store.getTypeAnno(formal).rigid_var.name == name) return true;
        }
        return false;
    }
};

/// Build the sealed type view of `module` and verify it against the source
/// roots `store` published, before anything rewrites the publication. Every
/// root the view enumerates must be one publication published, and every
/// published root must lie in the view's root closure.
pub fn verifyModulePublication(
    allocator: Allocator,
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    inputs: CheckedSourceTypeRootInputs,
    store: *const CheckedTypeStore,
    active: *const CheckedSourceTypeRoots,
) Allocator.Error!void {
    var roots: std.ArrayList(Var) = .empty;
    defer roots.deinit(allocator);
    try collectSourceTypeRoots(allocator, module, inputs, &roots);
    var view = try SealedTypeView.init(allocator, module, names, imports, roots.items);
    defer view.deinit();
    for (roots.items) |root| {
        if (active.roots.get(view.representative(root)) == null) {
            invariant("sealed type view root {d} is not a published source root", .{@backingInt(view.representative(root))});
        }
    }
    try verifyAgainstPublication(allocator, &view, store, active);
}

/// A published type and the representative variable the view renders for it.
const Pair = struct {
    id: CheckedTypeId,
    var_: Var,
};

/// Walk every published source root (`active`) and the view in lockstep and
/// assert that both render the same structure. Publication shares one root
/// between solver graphs with equal closed structure, so ids are never
/// compared, only what they render.
pub fn verifyAgainstPublication(
    allocator: Allocator,
    view: *const SealedTypeView,
    publication: *const CheckedTypeStore,
    active: *const CheckedSourceTypeRoots,
) Allocator.Error!void {
    var visited: std.AutoHashMapUnmanaged(Pair, void) = .empty;
    defer visited.deinit(allocator);
    var pending: std.ArrayList(Pair) = .empty;
    defer pending.deinit(allocator);

    var roots = active.roots.constIterator();
    while (roots.next()) |entry| {
        const var_ = view.representative(entry.key_ptr.*);
        if (!view.reached.isSet(@backingInt(var_))) {
            invariant("published source root {d} (solver variable {d}) lies outside the sealed type view's root closure", .{ @backingInt(entry.value_ptr.*), @backingInt(var_) });
        }
        try pending.append(allocator, .{ .id = entry.value_ptr.*, .var_ = var_ });
    }

    var gate = Gate{ .allocator = allocator, .view = view, .publication = publication, .pending = &pending };
    while (pending.pop()) |pair| {
        const seen = try visited.getOrPut(allocator, pair);
        if (seen.found_existing) continue;
        try gate.compare(publication.payload(pair.id), pair);
    }
}

const Gate = struct {
    allocator: Allocator,
    view: *const SealedTypeView,
    publication: *const CheckedTypeStore,
    pending: *std.ArrayList(Pair),

    fn mismatch(comptime field: []const u8, pair: Pair) noreturn {
        invariant("sealed type view disagrees with checked type publication on " ++ field ++ " at published type {d}, solver variable {d}", .{ @backingInt(pair.id), @backingInt(pair.var_) });
    }

    fn expect(ok: bool, comptime field: []const u8, pair: Pair) void {
        if (!ok) mismatch(field, pair);
    }

    fn child(self: *Gate, id: CheckedTypeId, var_: Var) Allocator.Error!void {
        try self.pending.append(self.allocator, .{ .id = id, .var_ = self.view.representative(var_) });
    }

    fn children(self: *Gate, ids: []const CheckedTypeId, vars: []const Var, comptime field: []const u8, pair: Pair) Allocator.Error!void {
        expect(ids.len == vars.len, field ++ " count", pair);
        for (ids, vars) |id, var_| try self.child(id, var_);
    }

    fn compare(self: *Gate, published: CheckedTypePayload, pair: Pair) Allocator.Error!void {
        const sealed = try self.view.payload(pair.var_);
        const same_kind = switch (published) {
            .pending => mismatch("a pending published payload", pair),
            .err => sealed == .err,
            .flex => sealed == .flex,
            .rigid => sealed == .rigid,
            .alias => sealed == .alias,
            .record => sealed == .record,
            .tuple => sealed == .tuple,
            .nominal => sealed == .nominal,
            .function => sealed == .function,
            .empty_record => sealed == .empty_record,
            .tag_union => sealed == .tag_union,
            .empty_tag_union => sealed == .empty_tag_union,
            .field_kind => sealed == .field_kind,
        };
        if (!same_kind) mismatch("the payload kind", pair);
        switch (published) {
            .pending => unreachable, // rejected above
            .err, .empty_record, .empty_tag_union => {},
            .field_kind => |kind| expect(kind == sealed.field_kind, "field kind", pair),
            .flex => |variable| try self.compareVariable(variable, sealed.flex, pair),
            .rigid => |variable| try self.compareVariable(variable, sealed.rigid, pair),
            .alias => |alias| {
                const other = sealed.alias;
                expect(alias.name == other.name, "alias name", pair);
                expect(alias.origin_module == other.origin_module, "alias origin module", pair);
                expect(std.meta.eql(alias.owner_module.*, other.owner_module), "alias owner module", pair);
                expect(std.meta.eql(alias.source_decl, other.source_decl), "alias source declaration", pair);
                expect(alias.builtin_origin == other.builtin_origin, "alias builtin origin", pair);
                try self.child(alias.backing, other.backing);
                try self.children(alias.args, other.args, "alias argument", pair);
            },
            .nominal => |nominal| {
                const other = sealed.nominal;
                expect(nominal.name == other.name, "nominal name", pair);
                expect(nominal.origin_module == other.origin_module, "nominal origin module", pair);
                expect(std.meta.eql(nominal.owner_module.*, other.owner_module), "nominal owner module", pair);
                expect(nominal.source_decl == other.source_decl, "nominal source declaration", pair);
                expect(nominal.builtin == other.builtin, "nominal builtin", pair);
                expect(nominal.is_opaque == other.is_opaque, "nominal opacity", pair);
                expect(std.meta.eql(nominal.representation, other.representation), "nominal representation", pair);
                expect(nominal.padding_field_types.len == 0 and nominal.declared_fields.len == 0, "nominal declaration-only fields", pair);
                try self.children(nominal.args, other.args, "nominal argument", pair);
            },
            .function => |function| {
                const other = sealed.function;
                expect(function.kind == other.kind, "function kind", pair);
                try self.children(function.args, other.args, "function argument", pair);
                try self.child(function.ret, other.ret);
            },
            .tuple => |items| try self.children(items, sealed.tuple, "tuple item", pair),
            .record => |record| try self.compareRecordRows(record, sealed.record, pair),
            .tag_union => |tag_union| try self.compareTagRows(tag_union, sealed.tag_union, pair),
        }
    }

    /// Compare two records as rows. Publication shares one root between
    /// solver graphs whose canonical keys are equal, and a canonical key
    /// reads a row through every record segment of its extension chain, so a
    /// published row and the solver row it stands for can split their fields
    /// across segments differently. Both sides are read as their flattened
    /// field set, in label order, followed by the chain's tail.
    fn compareRecordRows(self: *Gate, published: checked_artifact.CheckedRecordType, sealed: SealedRecordType, pair: Pair) Allocator.Error!void {
        const gpa = self.allocator;
        var published_fields: std.ArrayList(checked_artifact.CheckedRecordField) = .empty;
        defer published_fields.deinit(gpa);
        var sealed_fields: std.ArrayList(SealedRecordField) = .empty;
        defer sealed_fields.deinit(gpa);

        var published_row = published;
        const published_tail = while (true) {
            try published_fields.appendSlice(gpa, published_row.fields);
            switch (self.publication.payload(published_row.ext)) {
                .record => |next| published_row = next,
                .pending,
                .err,
                .flex,
                .rigid,
                .alias,
                .tuple,
                .nominal,
                .function,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                .field_kind,
                => break published_row.ext,
            }
        };
        var sealed_row = sealed;
        const sealed_tail = while (true) {
            for (0..sealed_row.fields.len) |index| try sealed_fields.append(gpa, try self.view.recordField(sealed_row, index));
            switch (try self.view.payload(sealed_row.ext)) {
                .record => |next| sealed_row = next,
                .err,
                .flex,
                .rigid,
                .alias,
                .tuple,
                .nominal,
                .function,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                .field_kind,
                => break sealed_row.ext,
            }
        };

        expect(published_fields.items.len == sealed_fields.items.len, "record field count", pair);
        std.mem.sort(checked_artifact.CheckedRecordField, published_fields.items, {}, publishedFieldLessThan);
        std.mem.sort(SealedRecordField, sealed_fields.items, {}, sealedFieldLessThan);
        for (published_fields.items, sealed_fields.items) |field, other| {
            expect(field.name == other.name, "record field label", pair);
            switch (other.kind) {
                .required => expect(field.kind.tag == .required, "record field kind", pair),
                .optional => expect(field.kind.tag == .optional, "record field kind", pair),
                .err => expect(field.kind.tag == .err, "record field kind", pair),
                .defaulted => |default| {
                    expect(field.kind.tag == .defaulted, "record field kind", pair);
                    expect(std.meta.eql(field.kind.default, default), "record field default", pair);
                },
                .undetermined => |presence| {
                    const variable = field.kind.undeterminedVariable() orelse mismatch("record field kind", pair);
                    try self.child(variable, presence);
                },
            }
            try self.child(field.ty, other.ty);
        }
        try self.child(published_tail, sealed_tail);
    }

    fn publishedFieldLessThan(_: void, lhs: checked_artifact.CheckedRecordField, rhs: checked_artifact.CheckedRecordField) bool {
        return @backingInt(lhs.name) < @backingInt(rhs.name);
    }

    fn sealedFieldLessThan(_: void, lhs: SealedRecordField, rhs: SealedRecordField) bool {
        return @backingInt(lhs.name) < @backingInt(rhs.name);
    }

    /// Compare two tag unions as rows, flattened across their extension
    /// chains for the reason `compareRecordRows` gives.
    fn compareTagRows(self: *Gate, published: checked_artifact.CheckedTagUnionType, sealed: SealedTagUnionType, pair: Pair) Allocator.Error!void {
        const gpa = self.allocator;
        var published_tags: std.ArrayList(checked_artifact.CheckedTag) = .empty;
        defer published_tags.deinit(gpa);
        var sealed_tags: std.ArrayList(SealedTag) = .empty;
        defer sealed_tags.deinit(gpa);

        var published_row = published;
        const published_tail = while (true) {
            try published_tags.appendSlice(gpa, published_row.tags);
            switch (self.publication.payload(published_row.ext)) {
                .tag_union => |next| published_row = next,
                .pending,
                .err,
                .flex,
                .rigid,
                .alias,
                .record,
                .tuple,
                .nominal,
                .function,
                .empty_record,
                .empty_tag_union,
                .field_kind,
                => break published_row.ext,
            }
        };
        var sealed_row = sealed;
        const sealed_tail = while (true) {
            for (0..sealed_row.tags.len) |index| try sealed_tags.append(gpa, try self.view.tag(sealed_row, index));
            switch (try self.view.payload(sealed_row.ext)) {
                .tag_union => |next| sealed_row = next,
                .err,
                .flex,
                .rigid,
                .alias,
                .record,
                .tuple,
                .nominal,
                .function,
                .empty_record,
                .empty_tag_union,
                .field_kind,
                => break sealed_row.ext,
            }
        };

        expect(published_tags.items.len == sealed_tags.items.len, "tag count", pair);
        std.mem.sort(checked_artifact.CheckedTag, published_tags.items, {}, publishedTagLessThan);
        std.mem.sort(SealedTag, sealed_tags.items, {}, sealedTagLessThan);
        for (published_tags.items, sealed_tags.items) |published_tag, other| {
            expect(published_tag.name == other.name, "tag label", pair);
            try self.children(published_tag.argsSlice(self.publication), other.args, "tag argument", pair);
        }
        try self.child(published_tail, sealed_tail);
    }

    fn publishedTagLessThan(_: void, lhs: checked_artifact.CheckedTag, rhs: checked_artifact.CheckedTag) bool {
        return @backingInt(lhs.name) < @backingInt(rhs.name);
    }

    fn sealedTagLessThan(_: void, lhs: SealedTag, rhs: SealedTag) bool {
        return @backingInt(lhs.name) < @backingInt(rhs.name);
    }

    fn compareVariable(self: *Gate, published: checked_artifact.CheckedTypeVariable, sealed: SealedTypeVariable, pair: Pair) Allocator.Error!void {
        if (published.name) |name| {
            const other = sealed.name orelse mismatch("variable name", pair);
            expect(std.mem.eql(u8, name, other), "variable name", pair);
        } else expect(sealed.name == null, "variable name", pair);
        expect(published.constraints.len == sealed.constraints.len, "constraint count", pair);
        for (published.constraints, sealed.constraints) |published_constraint, source| {
            const other = try self.view.constraint(source);
            expect(published_constraint.fn_name == other.fn_name, "constraint method name", pair);
            expect(std.meta.eql(published_constraint.origin, other.origin), "constraint origin", pair);
            expect(std.meta.eql(published_constraint.numeral, other.numeral), "constraint numeral", pair);
            try self.child(published_constraint.fn_ty, other.fn_var);
        }
        expect(published.numeric_default_phase == sealed.numeric_default_phase, "numeric default phase", pair);
        expect(published.row_default == sealed.row_default, "row default", pair);
        expect(published.row_default_discharges_constraints == sealed.row_default_discharges_constraints, "row default discharge", pair);
        expect(published.bounded_row_ext == sealed.bounded_row_ext, "bounded row extension", pair);
    }
};

const ViewTestError = @import("test/TestEnv.zig").TestEnvError || error{ CorruptArtifact, CorruptBuiltinArtifact, BuiltinArtifactVersionMismatch };

/// Check `source` against the builtin module, publish its source types, and
/// verify the sealed type view against the publication.
fn expectViewMatchesPublication(source: []const u8) ViewTestError!void {
    const TestEnv = @import("test/TestEnv.zig");
    const compiled_builtins = @import("compiled_builtins");
    const CompactWriter = collections.CompactWriter;
    const CheckedModuleArtifact = checked_artifact.CheckedModuleArtifact;
    const allocator = std.testing.allocator;

    var test_env = try TestEnv.init("SealedTypeViewTest", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
    try std.testing.expect(test_env.module_env.types.isSealed());

    const blob = compiled_builtins.builtin_artifact_bin[0..];
    const serialized_bytes = try CheckedModuleArtifact.splitVersionTrailer(blob);
    const backing: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8 =
        @alignCast(blob[0..serialized_bytes.len]);
    const serialized: *const CheckedModuleArtifact.Serialized = @ptrCast(@alignCast(backing.ptr));
    try serialized.validate(backing.len);
    var builtin_artifact = serialized.deserializeStatic(backing, allocator, .{ .static_builtin = test_env.builtin_module.env });
    defer builtin_artifact.deinitRetainingModuleEnv(allocator);

    const sources = [_]TypedCIR.Modules.SourceModule{
        .{ .precompiled = test_env.builtin_module.env },
        .{ .precompiled = test_env.module_env },
    };
    var modules = try TypedCIR.Modules.init(allocator, &sources);
    defer modules.deinit();
    const module = modules.module(1);
    const imports = [_]checked_artifact.PublishImportArtifact{.{
        .module_idx = 0,
        .key = builtin_artifact.key,
        .view = checked_artifact.importedView(&builtin_artifact),
    }};

    var names = canonical.CanonicalNameStore.init(allocator);
    defer names.deinit();
    var source_nodes = try checked_artifact.CheckedSourceNodes.init(allocator, module);
    defer source_nodes.deinit(allocator);
    var publication = try CheckedTypeStore.fromModule(allocator, module, &names, ModuleId{}, &imports, &.{}, &source_nodes, &.{}, &.{});
    defer publication.deinit(allocator);

    try verifyModulePublication(
        allocator,
        module,
        &names,
        publication.imports,
        .{ .source_nodes = &source_nodes, .selected_hoisted_roots = &.{}, .promoted_local_procedures = &.{} },
        &publication.store,
        &publication.source_types.?,
    );
}

test "sealed type view matches publication: open and closed records" {
    try expectViewMatchesPublication(
        \\get_x : { x : I64, ..rest } -> I64
        \\get_x = |rec| rec.x
        \\
        \\closed = { x: 1.I64, y: "a" }
        \\
        \\used = get_x(closed)
        \\
        \\inferred_open = |rec| rec.name
        \\
        \\empty = {}
    );
}

test "sealed type view matches publication: open tag unions" {
    try expectViewMatchesPublication(
        \\describe : [Red, Green, ..others] -> Str
        \\describe = |color| match color {
        \\    Red => "red"
        \\    Green => "green"
        \\    _ => "other"
        \\}
        \\
        \\described = describe(Blue)
        \\
        \\tagged = |n| if n > 0.I64 Positive(n) else NotPositive
    );
}

test "sealed type view matches publication: row tails constrained only by equality" {
    try expectViewMatchesPublication(
        \\same = |a, b| a == b
        \\
        \\same_records = same({ x: 1.I64 }, { x: 2.I64 })
        \\
        \\same_x = |rec| rec.x == 1.I64 and rec == rec
        \\
        \\same_tags = |tag| tag == Tagged
    );
}

test "sealed type view matches publication: numeric literal defaulting" {
    try expectViewMatchesPublication(
        \\n = 42
        \\
        \\fraction = 1.5
        \\
        \\add_one = |x| x + 1
        \\
        \\added = add_one(n)
    );
}

test "sealed type view matches publication: aliases" {
    try expectViewMatchesPublication(
        \\Point : { x : I64, y : I64 }
        \\
        \\Pair(a) : (a, a)
        \\
        \\origin : Point
        \\origin = { x: 0, y: 0 }
        \\
        \\swap : Pair(a) -> Pair(a)
        \\swap = |(first, second)| (second, first)
        \\
        \\swapped = swap((1.I64, 2.I64))
    );
}

test "sealed type view matches publication: local nominal types" {
    try expectViewMatchesPublication(
        \\Color := [Red, Green, Blue]
        \\
        \\Wrapper(a) := { inner : a }
        \\
        \\Secret :: { value : Str }
        \\
        \\red : Color
        \\red = Color.Red
        \\
        \\wrapped : Wrapper(I64)
        \\wrapped = { inner: 1 }
        \\
        \\keep_secret : Secret -> Secret
        \\keep_secret = |secret| secret
    );
}

test "sealed type view matches publication: builtin nominals" {
    try expectViewMatchesPublication(
        \\words : List(Str)
        \\words = ["a", "b"]
        \\
        \\parsed : Try(I64, Str)
        \\parsed = Ok(1)
        \\
        \\first = |list| List.first(list)
        \\
        \\count = List.len(words)
    );
}

test "sealed type view matches publication: pure, effectful and unbound functions" {
    try expectViewMatchesPublication(
        \\double : I64 -> I64
        \\double = |x| x * 2
        \\
        \\apply! : ({} => {}), {} => {}
        \\apply! = |f!, x| f!(x)
        \\
        \\identity = |x| x
        \\
        \\compose = |f, g| |x| g(f(x))
    );
}

test "sealed type view matches publication: tuples" {
    try expectViewMatchesPublication(
        \\pair = (1.I64, "a")
        \\
        \\first = |(a, _)| a
        \\
        \\picked = first(pair)
    );
}

test "sealed type view matches publication: recursive types" {
    try expectViewMatchesPublication(
        \\Tree := [Leaf, Node(Tree, I64, Tree)]
        \\
        \\tree : Tree
        \\tree = Tree.Node(Tree.Leaf, 1, Tree.Leaf)
        \\
        \\size : Tree -> I64
        \\size = |t| match t {
        \\    Leaf => 0
        \\    Node(left, _, right) => 1 + size(left) + size(right)
        \\}
        \\
        \\Expr := [Num(I64), Add(List(Expr))]
        \\
        \\expr : Expr
        \\expr = Expr.Add([Expr.Num(1)])
    );
}

test "sealed type view matches publication: bounded row extensions" {
    try expectViewMatchesPublication(
        \\classify : I64 -> [Negative, Zero, Positive]
        \\classify = |n| if n < 0 Negative else if n == 0 Zero else Positive
        \\
        \\classified = classify(3)
        \\
        \\wrap : a -> [Wrapped(a)]
        \\wrap = |a| Wrapped(a)
    );
}

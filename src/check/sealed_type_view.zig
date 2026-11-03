//! A module's checked source types, read in place from its sealed solver
//! type store.
//!
//! Checking canonicalizes the store before sealing it
//! (`types.Store.canonicalizeAndSeal`), so every child a descriptor names is
//! its class's checked representative and every record and tag union row is
//! flat. A checked type id of a source type is that representative's bit
//! pattern, and the view renders it as a `CheckedTypePayload` without
//! allocating: child ids alias the store's own variable pools, and record
//! fields, tags and constraints are handles that read the store's rows one at
//! a time.
//!
//! What a payload needs besides the store lives in four columns of the
//! module's checked type store, filled when the module is published: the
//! canonical names its identifiers stand for, the checked identity and owning
//! artifact of every module its named types come from, the checked facts of
//! every nominal declaration it holds, and per variable whether publication
//! reached it and through which row-tail positions. A variable's row default
//! is the one rendering decision that depends on more than the variable: it
//! comes from the row-tail positions publication reached it through
//! (`reach`).

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const canonical = @import("canonical_names.zig");
const checked_artifact = @import("checked_artifact.zig");
const type_ids = @import("checked_type_ids.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const Var = types.Var;

const CheckedTypeId = checked_artifact.CheckedTypeId;
const CheckedTypeStore = checked_artifact.CheckedTypeStore;
const CheckedTypePayload = checked_artifact.CheckedTypePayload;
const CheckedTypeVariable = checked_artifact.CheckedTypeVariable;
const CheckedRecordField = checked_artifact.CheckedRecordField;
const CheckedTag = checked_artifact.CheckedTag;
const CheckedStaticDispatchConstraint = checked_artifact.CheckedStaticDispatchConstraint;
const CheckedFunctionKind = checked_artifact.CheckedFunctionKind;
const CheckedBuiltinNominal = checked_artifact.CheckedBuiltinNominal;
const CheckedNominalRepresentationRef = checked_artifact.CheckedNominalRepresentationRef;
const CheckedImportViews = checked_artifact.CheckedImportViews;
const CheckedSourceTypeRoots = checked_artifact.CheckedSourceTypeRoots;
const ImportedOriginIndex = checked_artifact.ImportedOriginIndex;
const LocalNominalDeclarationIds = checked_artifact.LocalNominalDeclarationIds;
const ModuleId = checked_artifact.ModuleId;
const RowDefault = checked_artifact.RowDefault;
const invariant = checked_artifact.checkedArtifactInvariant;

/// The canonical names one identifier of the module stands for, one per name
/// space a checked type uses identifiers in. A name space the module's types
/// never use the identifier in holds `none`.
pub const SealedNameRow = extern struct {
    type_name: u32 = none,
    record_field_label: u32 = none,
    tag_label: u32 = none,
    method_name: u32 = none,

    pub const none: u32 = std.math.maxInt(u32);
};

/// The checked identity of one module identity the module's types name, and
/// the artifact owning that module's checked declarations when one is
/// available to the module.
pub const SealedOriginRow = extern struct {
    owner: ModuleId = .{},
    identity: u32 = SealedNameRow.none,
    has_owner: u32 = 0,
};

/// The checked facts of one nominal declaration in the module's solver
/// store. `representation` is null for a declaration whose owner the module
/// cannot resolve; no reached type names such a declaration.
pub const SealedNominalRow = struct {
    builtin: ?CheckedBuiltinNominal = null,
    representation: ?CheckedNominalRepresentationRef = null,
};

/// Per solver variable: whether publication reached it, and through which
/// row-tail positions.
pub const SealedVarFacts = packed struct(u8) {
    reached: bool = false,
    record_tail: bool = false,
    tag_tail: bool = false,
    _padding: u5 = 0,
};

/// Checked types read directly from a module's sealed solver store.
pub const SealedTypeView = struct {
    env: *const ModuleEnv,
    names: []const SealedNameRow,
    origins: []const SealedOriginRow,
    nominals: []const SealedNominalRow,
    var_facts: []const SealedVarFacts,

    fn typeStore(self: SealedTypeView) *const types.Store {
        return &self.env.types;
    }

    fn nameRow(self: SealedTypeView, ident: Ident.Idx) SealedNameRow {
        const index: usize = ident.idx;
        if (index >= self.names.len) invariant("a sealed checked type named an identifier publication never interned", .{});
        return self.names[index];
    }

    fn present(id: u32) u32 {
        if (id == SealedNameRow.none) invariant("a sealed checked type named an identifier in a name space publication never interned it in", .{});
        return id;
    }

    fn typeName(self: SealedTypeView, ident: Ident.Idx) canonical.TypeNameId {
        return @fromBackingInt(present(self.nameRow(ident).type_name));
    }

    fn recordFieldLabel(self: SealedTypeView, ident: Ident.Idx) canonical.RecordFieldLabelId {
        return @fromBackingInt(present(self.nameRow(ident).record_field_label));
    }

    fn tagLabel(self: SealedTypeView, ident: Ident.Idx) canonical.TagLabelId {
        return @fromBackingInt(present(self.nameRow(ident).tag_label));
    }

    fn methodName(self: SealedTypeView, ident: Ident.Idx) canonical.MethodNameId {
        return @fromBackingInt(present(self.nameRow(ident).method_name));
    }

    fn originRow(self: SealedTypeView, origin: base.ModuleIdentity.Idx) *const SealedOriginRow {
        const index: usize = @backingInt(origin);
        if (index >= self.origins.len) invariant("a sealed checked type named a module identity publication never resolved", .{});
        return &self.origins[index];
    }

    fn originIdentity(self: SealedTypeView, origin: base.ModuleIdentity.Idx) canonical.ModuleIdentityId {
        return @fromBackingInt(present(self.originRow(origin).identity));
    }

    fn originOwner(self: SealedTypeView, origin: base.ModuleIdentity.Idx) *const ModuleId {
        const row = self.originRow(origin);
        if (row.has_owner == 0) invariant("a sealed checked type named a type from a module with no available checked owner", .{});
        return &row.owner;
    }

    fn facts(self: SealedTypeView, var_: Var) SealedVarFacts {
        return self.var_facts[@backingInt(var_)];
    }

    /// The checked representative of `var_`'s class.
    pub fn representative(self: SealedTypeView, var_: Var) Var {
        return self.typeStore().resolveVar(var_).var_;
    }

    /// Whether publication reached `var_`'s class.
    pub fn reaches(self: SealedTypeView, var_: Var) bool {
        return self.facts(self.representative(var_)).reached;
    }

    /// `var_`'s checked type. `var_` is its class's checked representative,
    /// and publication reached it.
    pub fn payload(self: SealedTypeView, var_: Var) CheckedTypePayload {
        const env = self.env;
        const type_store = self.typeStore();
        const resolved = type_store.resolveVar(var_);
        if (std.debug.runtime_safety) {
            if (resolved.var_ != var_) invariant("checked type {d} names a solver variable that is not its class's representative", .{@backingInt(var_)});
            if (!self.facts(var_).reached) invariant("checked type {d} names a solver variable publication never reached", .{@backingInt(var_)});
        }
        // The checker marks an otherwise-unresolved identity it closed to
        // `[]`; it renders as a variable carrying `[]` only as its row default.
        if (resolved.desc.flags.empty_tag_union_is_default) return .{ .flex = .{ .row_default = .empty_tag_union } };
        switch (resolved.desc.content) {
            .err => return .err,
            .field_presence => |presence| return .{ .field_kind = switch (presence) {
                .required, .defaulted => .required,
                .optional => .optional,
            } },
            .flex => |flex| {
                const variable_facts = checked_artifact.flexVariableFacts(env, flex, resolved.desc.flags.bounded_row_ext);
                return .{ .flex = .{
                    .name = if (flex.name) |name| checked_artifact.checkedVariableNameText(env, name) else null,
                    .constraints = .{ .sealed = .{ .constraints = type_store.sliceStaticDispatchConstraints(flex.constraints), .view = self } },
                    .numeric_default_phase = variable_facts.numeric_default_phase,
                    .row_default = self.rowDefault(var_, resolved.desc.content),
                    .row_default_discharges_constraints = variable_facts.row_default_discharges_constraints,
                    .bounded_row_ext = variable_facts.bounded_row_ext,
                } };
            },
            .rigid => |rigid| {
                const variable_facts = checked_artifact.rigidVariableFacts(env, rigid);
                return .{ .rigid = .{
                    .name = checked_artifact.checkedVariableNameText(env, rigid.name),
                    .constraints = .{ .sealed = .{ .constraints = type_store.sliceStaticDispatchConstraints(rigid.constraints), .view = self } },
                    .numeric_default_phase = variable_facts.numeric_default_phase,
                    .row_default = self.rowDefault(var_, resolved.desc.content),
                    .row_default_discharges_constraints = variable_facts.row_default_discharges_constraints,
                    .bounded_row_ext = variable_facts.bounded_row_ext,
                } };
            },
            .alias => |alias| return .{ .alias = .{
                .name = self.typeName(alias.ident.ident_idx),
                .origin_module = self.originIdentity(alias.origin_module),
                .owner_module = self.originOwner(alias.origin_module),
                .source_decl = alias.source_decl.toOptional(),
                .builtin_origin = alias.source_decl.originIsBuiltin(),
                .backing = type_ids.fromVar(type_store.getAliasBackingVar(alias)),
                .args = type_ids.fromVars(type_store.sliceAliasArgs(alias)),
            } },
            .structure => |flat| switch (flat) {
                .empty_record => return .empty_record,
                .empty_tag_union => return .empty_tag_union,
                .record => |record| {
                    if (record.fields.len() == 0 and checked_artifact.checkedRecordExtIsEmpty(env, record.ext)) return .empty_record;
                    return .{ .record = .{
                        .fields = .{ .sealed = .{ .fields = type_store.getRecordFieldsSlice(record.fields), .view = self } },
                        .ext = type_ids.fromVar(record.ext),
                    } };
                },
                .tuple => |tuple| return .{ .tuple = type_ids.fromVars(type_store.sliceVars(tuple.elems)) },
                .nominal_type => |nominal| {
                    checked_artifact.debugAssertNominalOpacityMatchesDeclaration(type_store, nominal);
                    const declaration = type_store.lookupNominalDecl(nominal) orelse
                        invariant("a sealed nominal type has no declaration in its store", .{});
                    const row = self.nominals[@backingInt(declaration)];
                    return .{ .nominal = .{
                        .name = self.typeName(nominal.ident.ident_idx),
                        .origin_module = self.originIdentity(nominal.origin_module),
                        .owner_module = self.originOwner(nominal.origin_module),
                        .source_decl = nominal.statement(),
                        .builtin = row.builtin,
                        .is_opaque = nominal.isOpaque(),
                        .representation = row.representation orelse
                            invariant("a sealed nominal type names a declaration whose owner publication could not resolve", .{}),
                        .args = type_ids.fromVars(type_store.sliceNominalArgs(nominal)),
                    } };
                },
                .fn_pure, .fn_unbound => |func| return .{ .function = functionType(type_store, .pure, func) },
                .fn_effectful => |func| return .{ .function = functionType(type_store, .effectful, func) },
                .tag_union => |tag_union| {
                    if (tag_union.tags.len() == 0 and checked_artifact.checkedTagUnionExtIsEmpty(env, tag_union.ext)) return .empty_tag_union;
                    return .{ .tag_union = .{
                        .tags = .{ .sealed = .{ .tags = type_store.getTagsSlice(tag_union.tags), .view = self } },
                        .ext = type_ids.fromVar(tag_union.ext),
                    } };
                },
            },
        }
    }

    fn functionType(type_store: *const types.Store, kind: CheckedFunctionKind, func: types.Func) checked_artifact.CheckedFunctionType {
        return .{
            .kind = checked_artifact.finalizedFunctionKind(kind),
            .args = type_ids.fromVars(type_store.sliceVars(func.args)),
            .ret = type_ids.fromVar(func.ret),
        };
    }

    /// The row default of the variable `var_` whose content is `content`,
    /// from the row-tail positions publication reached it through.
    fn rowDefault(self: SealedTypeView, var_: Var, content: types.Content) ?RowDefault {
        const var_facts = self.facts(var_);
        const candidate: ?RowDefault = if (var_facts.record_tail) .empty_record else if (var_facts.tag_tail) .empty_tag_union else null;
        return checked_artifact.checkedRowDefaultForContent(self.env, content, candidate);
    }
};

/// The fields of a sealed record row, in the row's order.
pub const SealedRecordFieldRows = struct {
    fields: types.RecordField.SafeMultiList.Slice,
    view: SealedTypeView,

    pub fn len(self: SealedRecordFieldRows) usize {
        return self.fields.len;
    }

    pub fn get(self: SealedRecordFieldRows, index: usize) CheckedRecordField {
        const view = self.view;
        const name = view.recordFieldLabel(self.fields.items(.name)[index]);
        // The value and kind axes are independent; see design.md "Field Kinds".
        return switch (self.fields.items(.presence)[index].decode()) {
            .required => |type_var| .{ .name = name, .ty = type_ids.fromVar(type_var), .kind = .required },
            .unknown => |unknown| .{ .name = name, .ty = type_ids.fromVar(unknown.var_), .kind = switch (view.typeStore().resolveVar(unknown.presence).desc.content) {
                .field_presence => |presence| switch (presence) {
                    .required => .required,
                    .defaulted => |id| .defaultedFromParts(view.originIdentity(id.origin_module), id.expr_node),
                    .optional => .optional,
                },
                .flex => .undetermined(type_ids.fromVar(unknown.presence)),
                .err => .err,
                .rigid, .alias, .structure => invariant("a sealed field presence variable held non-presence content", .{}),
            } },
        };
    }
};

/// The tags of a sealed tag union row, in the row's order.
pub const SealedTagRows = struct {
    tags: types.Tag.SafeMultiList.Slice,
    view: SealedTypeView,

    pub fn len(self: SealedTagRows) usize {
        return self.tags.len;
    }

    pub fn get(self: SealedTagRows, index: usize) CheckedTag {
        return .{
            .name = self.view.tagLabel(self.tags.items(.name)[index]),
            .args = type_ids.fromVars(self.view.typeStore().sliceVars(self.tags.items(.args)[index])),
        };
    }
};

/// The static-dispatch constraints of a sealed type variable. A constraint
/// keeps the function variable checking recorded for it, so reading one
/// resolves that variable to its class's representative.
pub const SealedConstraintRows = struct {
    constraints: []const types.StaticDispatchConstraint,
    view: SealedTypeView,

    pub fn len(self: SealedConstraintRows) usize {
        return self.constraints.len;
    }

    pub fn get(self: SealedConstraintRows, index: usize) CheckedStaticDispatchConstraint {
        const source = self.constraints[index];
        const type_store = self.view.typeStore();
        return .{
            .fn_name = self.view.methodName(source.fn_name),
            .fn_ty = type_ids.fromVar(type_store.resolveVar(source.fn_var).var_),
            .origin = source.origin,
            .numeral = type_store.constraintNumeralInfo(source),
        };
    }
};

/// The columns a module's sealed checked types are read through, owned by
/// its checked type store.
pub const SealedTypeColumns = struct {
    names: *std.ArrayList(SealedNameRow),
    origins: *std.ArrayList(SealedOriginRow),
    nominals: *std.ArrayList(SealedNominalRow),
    var_facts: *std.ArrayList(SealedVarFacts),
};

/// Fill `columns` for `env`, whose solver store checking sealed: intern the
/// canonical name of every identifier the store's types use, resolve every
/// module identity they name and every nominal declaration the store holds,
/// and mark no variable reached. `reach` marks the variables publication
/// reaches.
pub fn buildColumns(
    allocator: Allocator,
    env: *const ModuleEnv,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    columns: SealedTypeColumns,
) Allocator.Error!void {
    const type_store = &env.types;
    if (std.debug.runtime_safety and !type_store.isSealed()) {
        invariant("sealed type columns were built over a type store checking never sealed", .{});
    }
    var builder = ColumnBuilder{
        .allocator = allocator,
        .env = env,
        .names = names,
        .imports = imports,
        .columns = columns,
        .origins = ImportedOriginIndex.init(allocator),
    };
    defer builder.origins.deinit();

    try columns.var_facts.appendNTimes(allocator, .{}, @intCast(type_store.len()));

    var seen = try std.DynamicBitSetUnmanaged.initEmpty(allocator, type_store.descs.backing.len());
    defer seen.deinit(allocator);
    for (0..@as(usize, @intCast(type_store.len()))) |index| {
        const resolved = type_store.resolveVar(@fromBackingInt(@intCast(index)));
        const desc_index: usize = @backingInt(resolved.desc_idx);
        if (seen.isSet(desc_index)) continue;
        seen.set(desc_index);
        try builder.content(resolved.desc.content);
    }

    var local_nominal_declarations = try LocalNominalDeclarationIds.initFromEnv(allocator, env);
    defer local_nominal_declarations.deinit();
    const declaration_count: usize = @intCast(type_store.nominalDeclCount());
    try columns.nominals.ensureTotalCapacity(allocator, declaration_count);
    for (0..declaration_count) |index| {
        const declaration = type_store.getNominalDecl(@fromBackingInt(@intCast(index)));
        try builder.origin(declaration.origin_module);
        const application = types.NominalType{
            .ident = declaration.ident,
            .args = Var.SafeList.Range.empty(),
            .origin_module = declaration.origin_module,
            .source = declaration.source,
        };
        const builtin = try builder.origins.builtinNominal(env, imports, application);
        columns.nominals.appendAssumeCapacity(.{
            .builtin = builtin,
            .representation = try checked_artifact.checkedNominalRepresentationIfAvailable(
                env,
                imports,
                &builder.origins,
                &local_nominal_declarations,
                application,
                builtin,
            ),
        });
    }
}

const ColumnBuilder = struct {
    allocator: Allocator,
    env: *const ModuleEnv,
    names: *canonical.CanonicalNameStore,
    imports: CheckedImportViews,
    columns: SealedTypeColumns,
    origins: ImportedOriginIndex,

    fn row(self: *ColumnBuilder, ident: Ident.Idx) Allocator.Error!*SealedNameRow {
        const index: usize = ident.idx;
        if (index >= self.columns.names.items.len) try self.columns.names.appendNTimes(self.allocator, .{}, index + 1 - self.columns.names.items.len);
        return &self.columns.names.items[index];
    }

    fn typeName(self: *ColumnBuilder, ident: Ident.Idx) Allocator.Error!void {
        const name_row = try self.row(ident);
        if (name_row.type_name != SealedNameRow.none) return;
        name_row.type_name = @backingInt(try self.names.internTypeIdent(&self.env.common.idents, ident));
    }

    fn recordFieldLabel(self: *ColumnBuilder, ident: Ident.Idx) Allocator.Error!void {
        const name_row = try self.row(ident);
        if (name_row.record_field_label != SealedNameRow.none) return;
        name_row.record_field_label = @backingInt(try self.names.internRecordFieldIdent(&self.env.common.idents, ident));
    }

    fn tagLabel(self: *ColumnBuilder, ident: Ident.Idx) Allocator.Error!void {
        const name_row = try self.row(ident);
        if (name_row.tag_label != SealedNameRow.none) return;
        name_row.tag_label = @backingInt(try self.names.internTagIdent(&self.env.common.idents, ident));
    }

    fn methodName(self: *ColumnBuilder, ident: Ident.Idx) Allocator.Error!void {
        const name_row = try self.row(ident);
        if (name_row.method_name != SealedNameRow.none) return;
        name_row.method_name = @backingInt(try self.names.internMethodIdent(&self.env.common.idents, ident));
    }

    fn origin(self: *ColumnBuilder, origin_module: base.ModuleIdentity.Idx) Allocator.Error!void {
        const index: usize = @backingInt(origin_module);
        if (index >= self.columns.origins.items.len) try self.columns.origins.appendNTimes(self.allocator, .{}, index + 1 - self.columns.origins.items.len);
        const origin_row = &self.columns.origins.items[index];
        if (origin_row.identity != SealedNameRow.none) return;
        origin_row.identity = @backingInt(try self.names.internModuleIdentity(self.env.moduleIdentityHash(origin_module)));
        if (try self.origins.ownerIfAvailable(self.env, self.imports, origin_module)) |owner| {
            origin_row.owner = owner;
            origin_row.has_owner = 1;
        }
    }

    fn constraints(self: *ColumnBuilder, range: types.StaticDispatchConstraint.SafeList.Range) Allocator.Error!void {
        for (self.env.types.sliceStaticDispatchConstraints(range)) |constraint| try self.methodName(constraint.fn_name);
    }

    fn content(self: *ColumnBuilder, value: types.Content) Allocator.Error!void {
        const type_store = &self.env.types;
        switch (value) {
            .err => {},
            .field_presence => |presence| switch (presence) {
                .required, .optional => {},
                .defaulted => |id| try self.origin(id.origin_module),
            },
            .flex => |flex| try self.constraints(flex.constraints),
            .rigid => |rigid| try self.constraints(rigid.constraints),
            .alias => |alias| {
                try self.typeName(alias.ident.ident_idx);
                try self.origin(alias.origin_module);
            },
            .structure => |flat| switch (flat) {
                .empty_record, .empty_tag_union, .tuple, .fn_pure, .fn_effectful, .fn_unbound => {},
                .nominal_type => |nominal| {
                    try self.typeName(nominal.ident.ident_idx);
                    try self.origin(nominal.origin_module);
                },
                .record => |record| for (type_store.getRecordFieldsSlice(record.fields).items(.name)) |name| {
                    try self.recordFieldLabel(name);
                },
                .tag_union => |tag_union| for (type_store.getTagsSlice(tag_union.tags).items(.name)) |name| {
                    try self.tagLabel(name);
                },
            },
        }
    }
};

/// Mark every variable reachable from `root` reached, recording the row-tail
/// positions each is reached through.
pub fn reach(allocator: Allocator, env: *const ModuleEnv, var_facts: []SealedVarFacts, root: Var) Allocator.Error!void {
    var walker = ReachWalker{ .env = env, .var_facts = var_facts };
    defer walker.pending.deinit(allocator);
    try walker.reach(allocator, root, null);
    while (walker.pending.pop()) |var_| try walker.expand(allocator, var_);
}

const ReachWalker = struct {
    env: *const ModuleEnv,
    var_facts: []SealedVarFacts,
    pending: std.ArrayList(Var) = .empty,

    fn reach(self: *ReachWalker, allocator: Allocator, var_: Var, tail: ?RowDefault) Allocator.Error!void {
        const resolved = self.env.types.resolveVar(var_);
        const var_facts = &self.var_facts[@backingInt(resolved.var_)];
        if (!var_facts.reached) {
            var_facts.reached = true;
            if (!resolved.desc.flags.empty_tag_union_is_default) try self.pending.append(allocator, resolved.var_);
        }
        switch (tail orelse return) {
            .empty_record => var_facts.record_tail = true,
            .empty_tag_union => var_facts.tag_tail = true,
        }
        if (std.debug.runtime_safety and var_facts.record_tail and var_facts.tag_tail and
            checked_artifact.checkedRowDefaultForContent(self.env, resolved.desc.content, .empty_record) != null)
        {
            invariant("checked row variable {d} received incompatible defaults", .{@backingInt(resolved.var_)});
        }
    }

    fn expand(self: *ReachWalker, allocator: Allocator, var_: Var) Allocator.Error!void {
        const env = self.env;
        const type_store = &env.types;
        switch (type_store.resolveVar(var_).desc.content) {
            .err, .field_presence => {},
            .flex => |flex| for (type_store.sliceStaticDispatchConstraints(flex.constraints)) |source| {
                try self.reach(allocator, source.fn_var, null);
            },
            .rigid => |rigid| for (type_store.sliceStaticDispatchConstraints(rigid.constraints)) |source| {
                try self.reach(allocator, source.fn_var, null);
            },
            .alias => |alias| {
                try self.reach(allocator, type_store.getAliasBackingVar(alias), null);
                for (type_store.sliceAliasArgs(alias)) |arg| try self.reach(allocator, arg, null);
            },
            .structure => |flat| switch (flat) {
                .empty_record, .empty_tag_union => {},
                .record => |record| {
                    if (record.fields.len() == 0 and checked_artifact.checkedRecordExtIsEmpty(env, record.ext)) return;
                    for (type_store.getRecordFieldsSlice(record.fields).items(.presence)) |presence| {
                        switch (presence.decode()) {
                            .required => |type_var| try self.reach(allocator, type_var, null),
                            .unknown => |unknown| {
                                switch (type_store.resolveVar(unknown.presence).desc.content) {
                                    .flex => try self.reach(allocator, unknown.presence, null),
                                    .field_presence, .err => {},
                                    .rigid, .alias, .structure => invariant("a sealed field presence variable held non-presence content", .{}),
                                }
                                try self.reach(allocator, unknown.var_, null);
                            },
                        }
                    }
                    try self.reach(allocator, record.ext, .empty_record);
                },
                .tuple => |tuple| for (type_store.sliceVars(tuple.elems)) |elem| try self.reach(allocator, elem, null),
                .nominal_type => |nominal| for (type_store.sliceNominalArgs(nominal)) |arg| try self.reach(allocator, arg, null),
                .fn_pure, .fn_effectful, .fn_unbound => |func| {
                    for (type_store.sliceVars(func.args)) |arg| try self.reach(allocator, arg, null);
                    try self.reach(allocator, func.ret, null);
                },
                .tag_union => |tag_union| {
                    if (tag_union.tags.len() == 0 and checked_artifact.checkedTagUnionExtIsEmpty(env, tag_union.ext)) return;
                    for (type_store.getTagsSlice(tag_union.tags).items(.args)) |args| {
                        for (type_store.sliceVars(args)) |arg| try self.reach(allocator, arg, null);
                    }
                    try self.reach(allocator, tag_union.ext, .empty_tag_union);
                },
            },
        }
    }
};

/// A published type and the representative variable the view renders for it.
const Pair = struct {
    id: CheckedTypeId,
    var_: Var,
};

/// Walk every published source root (`active`) and the view of `publication`
/// in lockstep and assert that both render the same structure. Publication
/// shares one root between solver graphs with equal closed structure, so
/// ids are never compared, only what they render.
pub fn verifyAgainstPublication(
    allocator: Allocator,
    view: SealedTypeView,
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
        if (!view.reaches(var_)) {
            invariant("published source root {d} (solver variable {d}) was never reached", .{ @backingInt(entry.value_ptr.*), @backingInt(var_) });
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
    view: SealedTypeView,
    publication: *const CheckedTypeStore,
    pending: *std.ArrayList(Pair),

    fn mismatch(comptime field: []const u8, pair: Pair) noreturn {
        invariant("sealed type view disagrees with checked type publication on " ++ field ++ " at published type {d}, solver variable {d}", .{ @backingInt(pair.id), @backingInt(pair.var_) });
    }

    fn expect(ok: bool, comptime field: []const u8, pair: Pair) void {
        if (!ok) mismatch(field, pair);
    }

    fn child(self: *Gate, id: CheckedTypeId, sealed: CheckedTypeId) Allocator.Error!void {
        const var_ = type_ids.asVar(sealed);
        if (self.view.representative(var_) != var_) invariant("sealed type view rendered a child that is not its class's representative", .{});
        try self.pending.append(self.allocator, .{ .id = id, .var_ = var_ });
    }

    fn children(self: *Gate, ids: []const CheckedTypeId, sealed: []const CheckedTypeId, comptime field: []const u8, pair: Pair) Allocator.Error!void {
        expect(ids.len == sealed.len, field ++ " count", pair);
        for (ids, sealed) |id, other| try self.child(id, other);
    }

    fn compare(self: *Gate, published: CheckedTypePayload, pair: Pair) Allocator.Error!void {
        const sealed = self.view.payload(pair.var_);
        if (published == .pending) mismatch("a pending published payload", pair);
        if (std.meta.activeTag(published) != std.meta.activeTag(sealed)) mismatch("the payload kind", pair);
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
                expect(std.meta.eql(alias.owner_module.*, other.owner_module.*), "alias owner module", pair);
                expect(std.meta.eql(alias.source_decl, other.source_decl), "alias source declaration", pair);
                expect(alias.builtin_origin == other.builtin_origin, "alias builtin origin", pair);
                try self.child(alias.backing, other.backing);
                try self.children(alias.args, other.args, "alias argument", pair);
            },
            .nominal => |nominal| {
                const other = sealed.nominal;
                expect(nominal.name == other.name, "nominal name", pair);
                expect(nominal.origin_module == other.origin_module, "nominal origin module", pair);
                expect(std.meta.eql(nominal.owner_module.*, other.owner_module.*), "nominal owner module", pair);
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
    fn compareRecordRows(self: *Gate, published: checked_artifact.CheckedRecordType, sealed: checked_artifact.CheckedRecordType, pair: Pair) Allocator.Error!void {
        const gpa = self.allocator;
        var published_fields: std.ArrayList(CheckedRecordField) = .empty;
        defer published_fields.deinit(gpa);
        var sealed_fields: std.ArrayList(CheckedRecordField) = .empty;
        defer sealed_fields.deinit(gpa);
        const published_tail = try self.flattenRecord(self.publication.view(), published, &published_fields);
        try sealed.fields.appendTo(gpa, &sealed_fields);
        if (self.view.payload(type_ids.asVar(sealed.ext)) == .record) mismatch("a sealed record row whose extension is a record", pair);

        expect(published_fields.items.len == sealed_fields.items.len, "record field count", pair);
        std.mem.sort(CheckedRecordField, published_fields.items, {}, fieldLessThan);
        std.mem.sort(CheckedRecordField, sealed_fields.items, {}, fieldLessThan);
        for (published_fields.items, sealed_fields.items) |field, other| {
            expect(field.name == other.name, "record field label", pair);
            expect(field.kind.tag == other.kind.tag, "record field kind", pair);
            expect(std.meta.eql(field.kind.default, other.kind.default), "record field default", pair);
            if (field.kind.undeterminedVariable()) |variable| try self.child(variable, other.kind.undeterminedVariable().?);
            try self.child(field.ty, other.ty);
        }
        try self.child(published_tail, sealed.ext);
    }

    fn flattenRecord(self: *Gate, view: checked_artifact.CheckedTypeStoreView, record: checked_artifact.CheckedRecordType, out: *std.ArrayList(CheckedRecordField)) Allocator.Error!CheckedTypeId {
        var row = record;
        while (true) {
            try row.fields.appendTo(self.allocator, out);
            switch (view.payload(row.ext)) {
                .record => |next| row = next,
                .pending, .err, .flex, .rigid, .alias, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union, .field_kind => return row.ext,
            }
        }
    }

    fn fieldLessThan(_: void, lhs: CheckedRecordField, rhs: CheckedRecordField) bool {
        return @backingInt(lhs.name) < @backingInt(rhs.name);
    }

    /// Compare two tag unions as rows, flattened across their extension
    /// chains for the reason `compareRecordRows` gives.
    fn compareTagRows(self: *Gate, published: checked_artifact.CheckedTagUnionType, sealed: checked_artifact.CheckedTagUnionType, pair: Pair) Allocator.Error!void {
        const gpa = self.allocator;
        var published_tags: std.ArrayList(CheckedTag) = .empty;
        defer published_tags.deinit(gpa);
        var sealed_tags: std.ArrayList(CheckedTag) = .empty;
        defer sealed_tags.deinit(gpa);
        var row = published;
        const view = self.publication.view();
        const published_tail = while (true) {
            try row.tags.appendTo(gpa, &published_tags);
            switch (view.payload(row.ext)) {
                .tag_union => |next| row = next,
                .pending, .err, .flex, .rigid, .alias, .record, .tuple, .nominal, .function, .empty_record, .empty_tag_union, .field_kind => break row.ext,
            }
        };
        try sealed.tags.appendTo(gpa, &sealed_tags);
        if (self.view.payload(type_ids.asVar(sealed.ext)) == .tag_union) mismatch("a sealed tag union row whose extension is a tag union", pair);

        expect(published_tags.items.len == sealed_tags.items.len, "tag count", pair);
        std.mem.sort(CheckedTag, published_tags.items, {}, tagLessThan);
        std.mem.sort(CheckedTag, sealed_tags.items, {}, tagLessThan);
        for (published_tags.items, sealed_tags.items) |published_tag, other| {
            expect(published_tag.name == other.name, "tag label", pair);
            try self.children(published_tag.args, other.args, "tag argument", pair);
        }
        try self.child(published_tail, sealed.ext);
    }

    fn tagLessThan(_: void, lhs: CheckedTag, rhs: CheckedTag) bool {
        return @backingInt(lhs.name) < @backingInt(rhs.name);
    }

    fn compareVariable(self: *Gate, published: CheckedTypeVariable, sealed: CheckedTypeVariable, pair: Pair) Allocator.Error!void {
        if (published.name) |name| {
            const other = sealed.name orelse mismatch("variable name", pair);
            expect(std.mem.eql(u8, name, other), "variable name", pair);
        } else expect(sealed.name == null, "variable name", pair);
        expect(published.constraints.len() == sealed.constraints.len(), "constraint count", pair);
        for (0..published.constraints.len()) |index| {
            const published_constraint = published.constraints.get(index);
            const other = sealed.constraints.get(index);
            expect(published_constraint.fn_name == other.fn_name, "constraint method name", pair);
            expect(std.meta.eql(published_constraint.origin, other.origin), "constraint origin", pair);
            expect(std.meta.eql(published_constraint.numeral, other.numeral), "constraint numeral", pair);
            try self.child(published_constraint.fn_ty, other.fn_ty);
        }
        expect(published.numeric_default_phase == sealed.numeric_default_phase, "numeric default phase", pair);
        expect(published.row_default == sealed.row_default, "row default", pair);
        expect(published.row_default_discharges_constraints == sealed.row_default_discharges_constraints, "row default discharge", pair);
        expect(published.bounded_row_ext == sealed.bounded_row_ext, "bounded row extension", pair);
    }
};

/// Check `source` against the builtin module, publish its source types, and
/// verify the sealed type view against the publication.
const ViewTestError = @import("test/TestEnv.zig").TestEnvError || error{ CorruptArtifact, CorruptBuiltinArtifact, BuiltinArtifactVersionMismatch };

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

    try verifyAgainstPublication(allocator, publication.store.sealedView(module.moduleEnvConst()), &publication.store, &publication.source_types.?);
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

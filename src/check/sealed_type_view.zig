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
const CheckedRecordField = checked_artifact.CheckedRecordField;
const CheckedTag = checked_artifact.CheckedTag;
const CheckedStaticDispatchConstraint = checked_artifact.CheckedStaticDispatchConstraint;
const CheckedFunctionKind = checked_artifact.CheckedFunctionKind;
const CheckedBuiltinNominal = checked_artifact.CheckedBuiltinNominal;
const CheckedNominalRepresentationRef = checked_artifact.CheckedNominalRepresentationRef;
const CheckedImportViews = checked_artifact.CheckedImportViews;
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

/// The representation interface capability publication gave one source
/// nominal type: a box payload capability of the module.
pub const SealedCapabilityRow = struct {
    var_: u32,
    representation: CheckedNominalRepresentationRef,

    pub fn lessThan(_: void, left: SealedCapabilityRow, right: SealedCapabilityRow) bool {
        return left.var_ < right.var_;
    }
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
    capabilities: []const SealedCapabilityRow,

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

    /// The box payload capability representation publication gave the
    /// nominal type `var_`, if any.
    fn capabilityOf(self: SealedTypeView, var_: Var) ?CheckedNominalRepresentationRef {
        const raw: u32 = @backingInt(var_);
        var low: usize = 0;
        var high: usize = self.capabilities.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            const row = self.capabilities[mid];
            if (row.var_ == raw) return row.representation;
            if (row.var_ < raw) low = mid + 1 else high = mid;
        }
        return null;
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
                        .representation = self.capabilityOf(var_) orelse row.representation orelse
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
            .numeral = .of(type_store.constraintNumeralInfo(source)),
        };
    }
};

/// The columns a module's sealed checked types are read through, owned by
/// its checked type store.
pub const SealedTypeColumns = struct {
    names: *collections.SafeList(SealedNameRow),
    origins: *collections.SafeList(SealedOriginRow),
    nominals: *collections.SafeList(SealedNominalRow),
    var_facts: *collections.SafeList(SealedVarFacts),
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
        _ = columns.nominals.appendAssumeCapacity(.{
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
        if (index >= self.columns.names.items().len) try self.columns.names.appendNTimes(self.allocator, .{}, index + 1 - self.columns.names.items().len);
        return &self.columns.names.items()[index];
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
        if (index >= self.columns.origins.items().len) try self.columns.origins.appendNTimes(self.allocator, .{}, index + 1 - self.columns.origins.items().len);
        const origin_row = &self.columns.origins.items()[index];
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
/// positions each is reached through. Each representative this reaches for
/// the first time is appended to `newly_reached` when one is given.
pub fn reach(allocator: Allocator, env: *const ModuleEnv, var_facts: []SealedVarFacts, root: Var, newly_reached: ?*std.ArrayList(Var)) Allocator.Error!void {
    var walker = ReachWalker{ .env = env, .var_facts = var_facts, .newly_reached = newly_reached };
    defer walker.pending.deinit(allocator);
    try walker.reach(allocator, root, null);
    while (walker.pending.pop()) |var_| try walker.expand(allocator, var_);
}

const ReachWalker = struct {
    env: *const ModuleEnv,
    var_facts: []SealedVarFacts,
    newly_reached: ?*std.ArrayList(Var),
    pending: std.ArrayList(Var) = .empty,

    fn reach(self: *ReachWalker, allocator: Allocator, var_: Var, tail: ?RowDefault) Allocator.Error!void {
        const resolved = self.env.types.resolveVar(var_);
        const var_facts = &self.var_facts[@backingInt(resolved.var_)];
        if (!var_facts.reached) {
            var_facts.reached = true;
            if (self.newly_reached) |list| try list.append(allocator, resolved.var_);
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

/// Check `source` against the builtin module, publish it, and render every
/// source type publication reached through the sealed type view, asserting
/// that each child is a reached checked representative and each row is flat.
const ViewTestError = @import("test/TestEnv.zig").TestEnvError || error{ CorruptArtifact, CorruptBuiltinArtifact, BuiltinArtifactVersionMismatch };

fn expectViewRendersPublishedTypes(source: []const u8) ViewTestError!void {
    const TestEnv = @import("test/TestEnv.zig");
    const compiled_builtins = @import("compiled_builtins");
    const CheckedModuleArtifact = checked_artifact.CheckedModuleArtifact;
    const allocator = std.testing.allocator;

    var test_env = try TestEnv.init("SealedTypeViewTest", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
    try std.testing.expect(test_env.module_env.types.isSealed());

    var builtin_artifact = try CheckedModuleArtifact.viewStaticBuiltin(compiled_builtins.builtin_artifact_bin, allocator, .{ .static_builtin = test_env.builtin_module.env });
    try builtin_artifact.attachSealedTypes(allocator, std.testing.io);
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
    var module_index = try checked_artifact.ModuleIndex.build(allocator, module.moduleEnvConst(), &.{});
    defer module_index.deinit(allocator);
    const publication_keys = try checked_artifact.SealedTypeKeys.create(allocator, std.testing.io, module.moduleEnvConst());
    defer publication_keys.destroy();
    var publication = try CheckedTypeStore.fromModule(allocator, module, &names, ModuleId{}, &imports, &.{}, &module_index, &.{}, &.{}, publication_keys);
    defer publication.deinit(allocator);

    const view = publication.store.sealedView(module.moduleEnvConst());
    var rendered: usize = 0;
    for (view.var_facts, 0..) |facts, raw| {
        if (!facts.reached) continue;
        const var_: Var = @fromBackingInt(@intCast(raw));
        if (view.representative(var_) != var_) continue;
        try expectRenders(view, var_);
        rendered += 1;
    }
    try std.testing.expect(rendered != 0);
}

fn expectChild(view: SealedTypeView, child: CheckedTypeId) error{ TestUnexpectedResult, TestExpectedEqual }!void {
    try std.testing.expect(!type_ids.isSynthesized(child));
    const var_ = type_ids.asVar(child);
    try std.testing.expectEqual(var_, view.representative(var_));
    try std.testing.expect(view.reaches(var_));
}

fn expectRenders(view: SealedTypeView, var_: Var) error{ TestUnexpectedResult, TestExpectedEqual }!void {
    switch (view.payload(var_)) {
        .pending => return error.TestUnexpectedResult,
        .err, .empty_record, .empty_tag_union, .field_kind => {},
        .flex, .rigid => |variable| for (0..variable.constraints.len()) |index| try expectChild(view, variable.constraints.get(index).fn_ty),
        .alias => |alias| {
            try expectChild(view, alias.backing);
            for (alias.args) |arg| try expectChild(view, arg);
        },
        .nominal => |nominal| for (nominal.args) |arg| try expectChild(view, arg),
        .function => |function| {
            for (function.args) |arg| try expectChild(view, arg);
            try expectChild(view, function.ret);
        },
        .tuple => |items| for (items) |item| try expectChild(view, item),
        .record => |record| {
            for (0..record.fields.len()) |index| {
                const field = record.fields.get(index);
                try expectChild(view, field.ty);
                if (field.kind.undeterminedVariable()) |variable| try expectChild(view, variable);
            }
            try expectChild(view, record.ext);
            try std.testing.expect(view.payload(type_ids.asVar(record.ext)) != .record);
        },
        .tag_union => |tag_union| {
            for (0..tag_union.tags.len()) |index| for (tag_union.tags.get(index).args) |arg| try expectChild(view, arg);
            try expectChild(view, tag_union.ext);
            try std.testing.expect(view.payload(type_ids.asVar(tag_union.ext)) != .tag_union);
        },
    }
}

test "sealed type view renders published types: open and closed records" {
    try expectViewRendersPublishedTypes(
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

test "sealed type view renders published types: open tag unions" {
    try expectViewRendersPublishedTypes(
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

test "sealed type view renders published types: row tails constrained only by equality" {
    try expectViewRendersPublishedTypes(
        \\same = |a, b| a == b
        \\
        \\same_records = same({ x: 1.I64 }, { x: 2.I64 })
        \\
        \\same_x = |rec| rec.x == 1.I64 and rec == rec
        \\
        \\same_tags = |tag| tag == Tagged
    );
}

test "sealed type view renders published types: numeric literal defaulting" {
    try expectViewRendersPublishedTypes(
        \\n = 42
        \\
        \\fraction = 1.5
        \\
        \\add_one = |x| x + 1
        \\
        \\added = add_one(n)
    );
}

test "sealed type view renders published types: aliases" {
    try expectViewRendersPublishedTypes(
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

test "sealed type view renders published types: local nominal types" {
    try expectViewRendersPublishedTypes(
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

test "sealed type view renders published types: builtin nominals" {
    try expectViewRendersPublishedTypes(
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

test "sealed type view renders published types: pure, effectful and unbound functions" {
    try expectViewRendersPublishedTypes(
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

test "sealed type view renders published types: tuples" {
    try expectViewRendersPublishedTypes(
        \\pair = (1.I64, "a")
        \\
        \\first = |(a, _)| a
        \\
        \\picked = first(pair)
    );
}

test "sealed type view renders published types: recursive types" {
    try expectViewRendersPublishedTypes(
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

test "sealed type view renders published types: bounded row extensions" {
    try expectViewRendersPublishedTypes(
        \\classify : I64 -> [Negative, Zero, Positive]
        \\classify = |n| if n < 0 Negative else if n == 0 Zero else Positive
        \\
        \\classified = classify(3)
        \\
        \\wrap : a -> [Wrapped(a)]
        \\wrap = |a| Wrapped(a)
    );
}

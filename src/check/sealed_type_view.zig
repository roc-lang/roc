//! A module's checked source types, read in place from its sealed solver
//! type store.
//!
//! Checking canonicalizes the store before sealing it
//! (`types.Store.canonicalizeAndSealReached`), so every child a descriptor
//! names is its class's checked representative and every record and tag
//! union row the view can reach is flat. A checked type id of a source type is that representative's bit
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
//! reached it, through which row-tail positions, and whether its type reaches
//! a flex or rigid variable. A variable's row default
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
/// never use the identifier in is absent.
pub const SealedNameRow = extern struct {
    type_name: collections.OptionalId(canonical.TypeNameId) = .none,
    record_field_label: collections.OptionalId(canonical.RecordFieldLabelId) = .none,
    tag_label: collections.OptionalId(canonical.TagLabelId) = .none,
    method_name: collections.OptionalId(canonical.MethodNameId) = .none,
};

/// The checked identity of one module identity the module's types name, and
/// the artifact owning that module's checked declarations when one is
/// available to the module.
pub const SealedOriginRow = extern struct {
    owner: ModuleId = .{},
    identity: collections.OptionalId(canonical.ModuleIdentityId) = .none,
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

/// Per solver variable: whether publication reached it, through which
/// row-tail positions, and whether its type reaches a flex or rigid variable.
pub const SealedVarFacts = packed struct(u8) {
    reached: bool = false,
    record_tail: bool = false,
    tag_tail: bool = false,
    /// Whether the checked type of this class representative reaches a flex
    /// or rigid variable through the children its payload renders
    /// (`markIdentityVariables`), marked when publication first reaches it.
    /// Meaningful only on reached representatives.
    identity_variables: bool = false,
    _padding: u4 = 0,
};

/// Append the variable of every one of `facts` publication reached to `out`,
/// in variable order, as `fromVar` names it. Whether a variable was reached
/// is unpredictable, so each is written to the end of `out` and only a
/// reached one is kept: the slot past the last reached variable takes the
/// writes of the unreached ones after it.
pub fn appendReached(
    comptime T: type,
    allocator: Allocator,
    out: *std.ArrayList(T),
    facts: []const SealedVarFacts,
    comptime fromVar: fn (Var) T,
) Allocator.Error!void {
    var reached_count: usize = 0;
    for (facts) |var_facts| reached_count += @intFromBool(var_facts.reached);
    try out.ensureUnusedCapacity(allocator, reached_count + 1);
    const slots = out.allocatedSlice();
    var len = out.items.len;
    for (facts, 0..) |var_facts, raw| {
        slots[len] = fromVar(@fromBackingInt(@intCast(raw)));
        len += @intFromBool(var_facts.reached);
    }
    out.items.len = len;
}

/// `var_` itself, for `appendReached` into a list of variables.
pub fn sameVar(var_: Var) Var {
    return var_;
}

/// What reading the rows of a sealed type needs: the module's sealed solver
/// store and the names and module identities publication resolved for it.
pub const SealedNames = struct {
    env: *const ModuleEnv,
    names: []const SealedNameRow,
    origins: []const SealedOriginRow,

    fn typeStore(self: SealedNames) *const types.Store {
        return &self.env.types;
    }

    fn nameRow(self: SealedNames, ident: Ident.Idx) SealedNameRow {
        const index: usize = ident.idx;
        if (index >= self.names.len) invariant("a sealed checked type named an identifier publication never interned", .{});
        return self.names[index];
    }

    fn present(id: anytype) @TypeOf(id).CompactPayload {
        return id.get() orelse invariant("a sealed checked type named an identifier in a name space publication never interned it in", .{});
    }

    fn typeName(self: SealedNames, ident: Ident.Idx) canonical.TypeNameId {
        return present(self.nameRow(ident).type_name);
    }

    fn recordFieldLabel(self: SealedNames, ident: Ident.Idx) canonical.RecordFieldLabelId {
        return present(self.nameRow(ident).record_field_label);
    }

    fn tagLabel(self: SealedNames, ident: Ident.Idx) canonical.TagLabelId {
        return present(self.nameRow(ident).tag_label);
    }

    fn methodName(self: SealedNames, ident: Ident.Idx) canonical.MethodNameId {
        return present(self.nameRow(ident).method_name);
    }

    fn originRow(self: SealedNames, origin: base.ModuleIdentity.Idx) *const SealedOriginRow {
        const index: usize = @backingInt(origin);
        if (index >= self.origins.len) invariant("a sealed checked type named a module identity publication never resolved", .{});
        return &self.origins[index];
    }

    fn originIdentity(self: SealedNames, origin: base.ModuleIdentity.Idx) canonical.ModuleIdentityId {
        return present(self.originRow(origin).identity);
    }

    fn originOwner(self: SealedNames, origin: base.ModuleIdentity.Idx) *const ModuleId {
        const row = self.originRow(origin);
        if (row.has_owner == 0) invariant("a sealed checked type named a type from a module with no available checked owner", .{});
        return &row.owner;
    }
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

    /// Whether `var_` is an uncommitted tag tail a scheme quantified: the
    /// empty tag union inference closed a row with, still carrying
    /// `DescriptorFlags.uncommitted_tag_tail` after the module-end commit
    /// cleared every unquantified one (design.md "Uncommitted Tag Tails").
    /// `payload` reads it as the empty tag union it is.
    pub fn uncommittedTagTail(self: SealedTypeView, var_: Var) bool {
        const resolved = self.typeStore().resolveVar(var_);
        if (!resolved.desc.flags.uncommitted_tag_tail) return false;
        return switch (resolved.desc.content) {
            .structure => |flat| flat == .empty_tag_union,
            .flex, .rigid, .alias, .field_presence, .err => false,
        };
    }

    /// The names, module identities and solver store the rows of this
    /// view's types read.
    fn sealedNames(self: SealedTypeView) SealedNames {
        return .{ .env = self.env, .names = self.names, .origins = self.origins };
    }

    fn typeName(self: SealedTypeView, ident: Ident.Idx) canonical.TypeNameId {
        return self.sealedNames().typeName(ident);
    }

    fn originIdentity(self: SealedTypeView, origin: base.ModuleIdentity.Idx) canonical.ModuleIdentityId {
        return self.sealedNames().originIdentity(origin);
    }

    fn originOwner(self: SealedTypeView, origin: base.ModuleIdentity.Idx) *const ModuleId {
        return self.sealedNames().originOwner(origin);
    }

    /// The box payload capability representation publication gave the
    /// nominal type `var_`, if any. A variable has at most one row.
    fn capabilityOf(self: SealedTypeView, var_: Var) ?CheckedNominalRepresentationRef {
        const rows = self.capabilities;
        if (rows.len == 0) return null;
        const raw: u32 = @backingInt(var_);
        // The last row whose variable is not past `raw` is always among the
        // `span` rows from `first`. Each step selects a half instead of
        // branching on the comparison, whose outcome is unpredictable.
        var first: usize = 0;
        var span: usize = rows.len;
        while (span > 1) {
            const half = span / 2;
            first = if (rows[first + half].var_ <= raw) first + half else first;
            span -= half;
        }
        return if (rows[first].var_ == raw) rows[first].representation else null;
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

    /// Whether `var_`'s checked type reaches a flex or rigid variable.
    /// `var_` is its class's checked representative.
    pub fn hasIdentityVariables(self: SealedTypeView, var_: Var) bool {
        return self.facts(var_).identity_variables;
    }

    /// The kind of `var_`'s checked type: the tag `payload` returns, read
    /// without decoding the payload.
    pub fn payloadTag(self: SealedTypeView, var_: Var) std.meta.Tag(CheckedTypePayload) {
        const env = self.env;
        const type_store = self.typeStore();
        const desc_idx = type_store.resolveDescIdx(var_);
        const word = type_store.wordAt(desc_idx);
        const tag: std.meta.Tag(CheckedTypePayload) = switch (word.kind) {
            .err => .err,
            .field_presence => .field_kind,
            .flex => .flex,
            .rigid => .rigid,
            .alias => .alias,
            .empty_record => .empty_record,
            .empty_tag_union => .empty_tag_union,
            .record => record: {
                const record = type_store.recordAt(desc_idx);
                break :record if (record.fields.len() == 0 and checked_artifact.checkedRecordExtIsEmpty(env, record.ext)) .empty_record else .record;
            },
            .tuple => .tuple,
            .nominal_type => .nominal,
            .fn_pure, .fn_unbound, .fn_effectful => .function,
            .tag_union => tag_union: {
                const tag_union = type_store.tagUnionAt(desc_idx);
                break :tag_union if (tag_union.tags.len() == 0 and checked_artifact.checkedTagUnionExtIsEmpty(env, tag_union.ext)) .empty_tag_union else .tag_union;
            },
        };
        if (std.debug.runtime_safety and std.meta.activeTag(self.payload(var_)) != tag) {
            invariant("checked type {d}'s payload kind disagreed with its decoded payload", .{@backingInt(var_)});
        }
        return tag;
    }

    /// The declaration key of `var_`'s checked type when it is a nominal
    /// type, and null otherwise: the key `payload` names, read without
    /// decoding the payload.
    pub fn nominalKey(self: SealedTypeView, var_: Var) ?canonical.NominalTypeKey {
        const type_store = self.typeStore();
        const desc_idx = type_store.resolveDescIdx(var_);
        if (type_store.wordAt(desc_idx).kind != .nominal_type) {
            std.debug.assert(self.payloadTag(var_) != .nominal);
            return null;
        }
        const nominal = type_store.nominalAt(desc_idx);
        const key: canonical.NominalTypeKey = .{
            .module = self.originIdentity(nominal.origin_module),
            .source_decl = nominal.statement(),
        };
        if (std.debug.runtime_safety) {
            const decoded = self.payload(var_).nominal;
            if (decoded.origin_module != key.module or decoded.source_decl != key.source_decl) {
                invariant("checked type {d}'s nominal key disagreed with its decoded payload", .{@backingInt(var_)});
            }
        }
        return key;
    }

    /// `var_`'s checked type. `var_` is its class's checked representative,
    /// and publication reached it. The content is read by kind from the
    /// stored descriptor, so only the payload of the kind found is decoded.
    pub fn payload(self: SealedTypeView, var_: Var) CheckedTypePayload {
        const env = self.env;
        const type_store = self.typeStore();
        const desc_idx = type_store.resolveDescIdx(var_);
        const word = type_store.wordAt(desc_idx);
        if (std.debug.runtime_safety) {
            if (type_store.resolveVarWord(var_).var_ != var_) invariant("checked type {d} names a solver variable that is not its class's representative", .{@backingInt(var_)});
            if (!self.facts(var_).reached) invariant("checked type {d} names a solver variable publication never reached", .{@backingInt(var_)});
        }
        const flags = word.flags();
        switch (word.kind) {
            .err => return .err,
            .field_presence => return .{ .field_kind = switch (type_store.fieldPresenceAt(desc_idx)) {
                .required, .defaulted => .required,
                .optional => .optional,
            } },
            .flex => {
                const flex = type_store.flexAt(desc_idx);
                const variable_facts = checked_artifact.flexVariableFacts(env, flex, flags.bounded_row_ext);
                return .{ .flex = .{
                    .name = if (flex.name) |name| checked_artifact.checkedVariableNameText(env, name) else null,
                    .constraints = .{ .sealed = .{ .constraints = type_store.sliceStaticDispatchConstraints(flex.constraints), .names = self.sealedNames() } },
                    .numeric_default_phase = variable_facts.numeric_default_phase,
                    .row_default = self.rowDefault(var_, .{ .flex = flex }),
                    .row_default_discharges_constraints = variable_facts.row_default_discharges_constraints,
                    .row_tail = if (variable_facts.bounded_row_ext)
                        .bounded
                    else if (flags.uncommitted_record_tail)
                        .uncommitted_record
                    else
                        .ordinary,
                } };
            },
            .rigid => {
                const rigid = type_store.rigidAt(desc_idx);
                const variable_facts = checked_artifact.rigidVariableFacts(env, rigid);
                return .{ .rigid = .{
                    .name = checked_artifact.checkedVariableNameText(env, rigid.name),
                    .constraints = .{ .sealed = .{ .constraints = type_store.sliceStaticDispatchConstraints(rigid.constraints), .names = self.sealedNames() } },
                    .numeric_default_phase = variable_facts.numeric_default_phase,
                    .row_default = self.rowDefault(var_, .{ .rigid = rigid }),
                    .row_default_discharges_constraints = variable_facts.row_default_discharges_constraints,
                    .row_tail = if (variable_facts.bounded_row_ext) .bounded else .ordinary,
                } };
            },
            .alias => {
                const alias = type_store.aliasAt(desc_idx);
                return .{ .alias = .{
                    .name = self.typeName(alias.ident.ident_idx),
                    .origin_module = self.originIdentity(alias.origin_module),
                    .owner_module = self.originOwner(alias.origin_module),
                    .source_decl = alias.source_decl.toOptional(),
                    .builtin_origin = alias.source_decl.originIsBuiltin(),
                    .backing = type_ids.fromVar(type_store.getAliasBackingVar(alias)),
                    .args = type_ids.fromVars(type_store.sliceAliasArgs(alias)),
                } };
            },
            .empty_record => return .empty_record,
            .empty_tag_union => return .empty_tag_union,
            .record => {
                const record = type_store.recordAt(desc_idx);
                if (record.fields.len() == 0 and checked_artifact.checkedRecordExtIsEmpty(env, record.ext)) return .empty_record;
                // Sealing flattened every row its roots reach.
                std.debug.assert(type_store.kindOf(record.ext) != .record);
                return .{ .record = .{
                    .fields = .{ .sealed = .{ .fields = type_store.getRecordFieldsSlice(record.fields), .names = self.sealedNames() } },
                    .ext = type_ids.fromVar(record.ext),
                } };
            },
            .tuple => return .{ .tuple = type_ids.fromVars(type_store.sliceVars(type_store.tupleAt(desc_idx).elems)) },
            .nominal_type => {
                const nominal = type_store.nominalAt(desc_idx);
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
            .fn_pure, .fn_unbound => return .{ .function = functionType(type_store, .pure, type_store.funcAt(desc_idx)) },
            .fn_effectful => return .{ .function = functionType(type_store, .effectful, type_store.funcAt(desc_idx)) },
            .tag_union => {
                const tag_union = type_store.tagUnionAt(desc_idx);
                if (tag_union.tags.len() == 0 and checked_artifact.checkedTagUnionExtIsEmpty(env, tag_union.ext)) return .empty_tag_union;
                std.debug.assert(type_store.kindOf(tag_union.ext) != .tag_union);
                return .{ .tag_union = .{
                    .tags = .{ .sealed = .{ .tags = type_store.getTagsSlice(tag_union.tags), .names = self.sealedNames() } },
                    .ext = type_ids.fromVar(tag_union.ext),
                } };
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
    names: SealedNames,

    pub fn len(self: SealedRecordFieldRows) usize {
        return self.fields.len;
    }

    pub fn get(self: SealedRecordFieldRows, index: usize) CheckedRecordField {
        const view = self.names;
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
    names: SealedNames,

    pub fn len(self: SealedTagRows) usize {
        return self.tags.len;
    }

    pub fn get(self: SealedTagRows, index: usize) CheckedTag {
        return .{
            .name = self.names.tagLabel(self.tags.items(.name)[index]),
            .args = type_ids.fromVars(self.names.typeStore().sliceVars(self.tags.items(.args)[index])),
        };
    }
};

/// The static-dispatch constraints of a sealed type variable. A constraint
/// keeps the function variable checking recorded for it, so reading one
/// resolves that variable to its class's representative.
pub const SealedConstraintRows = struct {
    constraints: []const types.StaticDispatchConstraint,
    names: SealedNames,

    pub fn len(self: SealedConstraintRows) usize {
        return self.constraints.len;
    }

    pub fn get(self: SealedConstraintRows, index: usize) CheckedStaticDispatchConstraint {
        const source = self.constraints[index];
        const type_store = self.names.typeStore();
        return .{
            .fn_name = self.names.methodName(source.fn_name),
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
/// reaches, and which of them reach a flex or rigid variable.
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
        // A class's descriptor is read once, the first time one of its
        // variables is met.
        const desc_idx = type_store.resolveDescIdx(@fromBackingInt(@intCast(index)));
        const desc_index: usize = @backingInt(desc_idx);
        if (seen.isSet(desc_index)) continue;
        seen.set(desc_index);
        try builder.descriptor(desc_idx);
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

/// Scratch for `reach`, kept by the caller with its capacity between walks;
/// empty between walks.
pub const ReachScratch = struct {
    /// The walk's work list.
    pending: std.ArrayList(Var) = .empty,
    /// The representatives the walk reached for the first time.
    newly_reached: std.ArrayList(Var) = .empty,
    /// Per variable: one more than its position in `newly_reached` while it
    /// is newly reached, else zero. Sized to the store on the first walk
    /// that reaches anything, and zero again between walks.
    positions: std.ArrayList(u32) = .empty,
    /// Edges between newly reached representatives, as child and parent
    /// positions, and the parents of each position once placed.
    edges: std.ArrayList([2]u32) = .empty,
    parent_starts: std.ArrayList(u32) = .empty,
    parents: std.ArrayList(u32) = .empty,
    work: std.ArrayList(u32) = .empty,

    pub fn deinit(self: *ReachScratch, allocator: Allocator) void {
        self.pending.deinit(allocator);
        self.newly_reached.deinit(allocator);
        self.positions.deinit(allocator);
        self.edges.deinit(allocator);
        self.parent_starts.deinit(allocator);
        self.parents.deinit(allocator);
        self.work.deinit(allocator);
    }

    fn clear(self: *ReachScratch) void {
        self.pending.clearRetainingCapacity();
        for (self.newly_reached.items) |var_| {
            if (@backingInt(var_) < self.positions.items.len) self.positions.items[@backingInt(var_)] = 0;
        }
        self.newly_reached.clearRetainingCapacity();
        self.edges.clearRetainingCapacity();
        self.parent_starts.clearRetainingCapacity();
        self.parents.clearRetainingCapacity();
        self.work.clearRetainingCapacity();
    }
};

/// Mark each representative a walk newly reached whose checked type reaches
/// a flex or rigid variable: one that is one, or one with a child that
/// reaches one, over the children `SealedTypeView.payload` renders. Every
/// child of a reached representative is reached, so each child is either
/// newly reached too or was reached by an earlier walk, whose fact is
/// already marked. The new facts are computed backward from the variables
/// and the earlier facts over the reversed edges between the newly reached
/// representatives, so cycles among them are classified exactly and each
/// edge is visited once.
fn markIdentityVariables(allocator: Allocator, env: *const ModuleEnv, var_facts: []SealedVarFacts, scratch: *ReachScratch) Allocator.Error!void {
    const reached = scratch.newly_reached.items;
    if (scratch.positions.items.len < var_facts.len) try scratch.positions.appendNTimes(allocator, 0, var_facts.len - scratch.positions.items.len);
    for (reached, 1..) |var_, position_plus_one| scratch.positions.items[@backingInt(var_)] = @intCast(position_plus_one);

    var lister = IdentityEdgeLister{ .allocator = allocator, .env = env, .var_facts = var_facts, .scratch = scratch };
    for (reached, 0..) |var_, position| {
        switch (env.types.kindOf(var_)) {
            .flex, .rigid => {
                var_facts[@backingInt(var_)].identity_variables = true;
                try scratch.work.append(allocator, @intCast(position));
                continue;
            },
            .err, .field_presence, .alias, .empty_record, .empty_tag_union, .record, .tuple, .nominal_type, .fn_pure, .fn_unbound, .fn_effectful, .tag_union => {},
        }
        lister.parent = @intCast(position);
        lister.reaches = false;
        try identityChildren(env, var_, &lister);
        if (lister.reaches) {
            var_facts[@backingInt(var_)].identity_variables = true;
            try scratch.work.append(allocator, @intCast(position));
        }
    }

    // Parents of each newly reached representative, placed by child.
    const starts = &scratch.parent_starts;
    try starts.appendNTimes(allocator, 0, reached.len + 1);
    for (scratch.edges.items) |edge| starts.items[edge[0] + 1] += 1;
    for (1..reached.len + 1) |position| starts.items[position] += starts.items[position - 1];
    try scratch.parents.resize(allocator, scratch.edges.items.len);
    for (scratch.edges.items) |edge| {
        const child, const parent = edge;
        // `starts[child]` serves as the fill cursor and ends at the start of
        // `child + 1`'s parents, so each is restored after placing below.
        scratch.parents.items[starts.items[child]] = parent;
        starts.items[child] += 1;
    }
    var position = reached.len;
    while (position > 0) : (position -= 1) starts.items[position] = starts.items[position - 1];
    starts.items[0] = 0;

    while (scratch.work.pop()) |child| {
        for (scratch.parents.items[starts.items[child]..starts.items[child + 1]]) |parent| {
            const facts = &var_facts[@backingInt(reached[parent])];
            if (facts.identity_variables) continue;
            facts.identity_variables = true;
            try scratch.work.append(allocator, parent);
        }
    }
}

/// Lists the edges between newly reached representatives, and whether one
/// has a child an earlier walk marked as reaching a variable.
const IdentityEdgeLister = struct {
    allocator: Allocator,
    env: *const ModuleEnv,
    var_facts: []const SealedVarFacts,
    scratch: *ReachScratch,
    parent: u32 = 0,
    reaches: bool = false,

    fn child(self: *IdentityEdgeLister, var_: Var) Allocator.Error!void {
        const representative = self.env.types.resolveVarWord(var_).var_;
        const position_plus_one = self.scratch.positions.items[@backingInt(representative)];
        if (position_plus_one != 0) {
            try self.scratch.edges.append(self.allocator, .{ position_plus_one - 1, self.parent });
        } else if (self.var_facts[@backingInt(representative)].identity_variables) {
            self.reaches = true;
        }
    }
};

/// Call `sink.child` for each child of the representative `var_` whose fact
/// decides `var_`'s identity-variable fact: the children
/// `SealedTypeView.payload` renders, read by kind from the stored descriptor.
/// A flex or rigid variable decides its own fact, so its constraints are not
/// children here.
fn identityChildren(env: *const ModuleEnv, var_: Var, sink: anytype) Allocator.Error!void {
    const type_store = &env.types;
    const desc_idx = type_store.resolveDescIdx(var_);
    switch (type_store.kindAt(desc_idx)) {
        .err, .field_presence, .flex, .rigid, .empty_record, .empty_tag_union => {},
        .alias => {
            const alias = type_store.aliasAt(desc_idx);
            try sink.child(type_store.getAliasBackingVar(alias));
            for (type_store.sliceAliasArgs(alias)) |arg| try sink.child(arg);
        },
        .record => {
            const record = type_store.recordAt(desc_idx);
            if (record.fields.len() == 0 and checked_artifact.checkedRecordExtIsEmpty(env, record.ext)) return;
            for (type_store.getRecordFieldsSlice(record.fields).items(.presence)) |presence| {
                switch (presence.decode()) {
                    .required => |type_var| try sink.child(type_var),
                    .unknown => |unknown| {
                        // A presence variable renders as a child only
                        // while it is undetermined.
                        if (type_store.kindOf(unknown.presence) == .flex) try sink.child(unknown.presence);
                        try sink.child(unknown.var_);
                    },
                }
            }
            try sink.child(record.ext);
        },
        .tuple => for (type_store.sliceVars(type_store.tupleAt(desc_idx).elems)) |elem| try sink.child(elem),
        .nominal_type => for (type_store.sliceNominalArgs(type_store.nominalAt(desc_idx))) |arg| try sink.child(arg),
        .fn_pure, .fn_effectful, .fn_unbound => {
            const func = type_store.funcAt(desc_idx);
            for (type_store.sliceVars(func.args)) |arg| try sink.child(arg);
            try sink.child(func.ret);
        },
        .tag_union => {
            const tag_union = type_store.tagUnionAt(desc_idx);
            if (tag_union.tags.len() == 0 and checked_artifact.checkedTagUnionExtIsEmpty(env, tag_union.ext)) return;
            for (type_store.getTagsSlice(tag_union.tags).items(.args)) |args| {
                for (type_store.sliceVars(args)) |arg| try sink.child(arg);
            }
            try sink.child(tag_union.ext);
        },
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
        if (name_row.type_name != .none) return;
        name_row.type_name = .some(try self.names.internTypeIdent(&self.env.common.idents, ident));
    }

    fn recordFieldLabel(self: *ColumnBuilder, ident: Ident.Idx) Allocator.Error!void {
        const name_row = try self.row(ident);
        if (name_row.record_field_label != .none) return;
        name_row.record_field_label = .some(try self.names.internRecordFieldIdent(&self.env.common.idents, ident));
    }

    fn tagLabel(self: *ColumnBuilder, ident: Ident.Idx) Allocator.Error!void {
        const name_row = try self.row(ident);
        if (name_row.tag_label != .none) return;
        name_row.tag_label = .some(try self.names.internTagIdent(&self.env.common.idents, ident));
    }

    fn methodName(self: *ColumnBuilder, ident: Ident.Idx) Allocator.Error!void {
        const name_row = try self.row(ident);
        if (name_row.method_name != .none) return;
        name_row.method_name = .some(try self.names.internMethodIdent(&self.env.common.idents, ident));
    }

    fn origin(self: *ColumnBuilder, origin_module: base.ModuleIdentity.Idx) Allocator.Error!void {
        const index: usize = @backingInt(origin_module);
        if (index >= self.columns.origins.items().len) try self.columns.origins.appendNTimes(self.allocator, .{}, index + 1 - self.columns.origins.items().len);
        const origin_row = &self.columns.origins.items()[index];
        if (origin_row.identity != .none) return;
        origin_row.identity = .some(try self.names.internModuleIdentity(self.env.moduleIdentityHash(origin_module)));
        if (try self.origins.ownerIfAvailable(self.env, self.imports, origin_module)) |owner| {
            origin_row.owner = owner;
            origin_row.has_owner = 1;
        }
    }

    fn constraints(self: *ColumnBuilder, range: types.StaticDispatchConstraint.SafeList.Range) Allocator.Error!void {
        for (self.env.types.sliceStaticDispatchConstraints(range)) |constraint| try self.methodName(constraint.fn_name);
    }

    /// Intern every name and resolve every module identity the stored
    /// descriptor `desc_idx` holds, reading the payload of its kind alone.
    fn descriptor(self: *ColumnBuilder, desc_idx: types.DescStoreIdx) Allocator.Error!void {
        const type_store = &self.env.types;
        switch (type_store.kindAt(desc_idx)) {
            .err, .empty_record, .empty_tag_union, .tuple, .fn_pure, .fn_effectful, .fn_unbound => {},
            .field_presence => switch (type_store.fieldPresenceAt(desc_idx)) {
                .required, .optional => {},
                .defaulted => |id| try self.origin(id.origin_module),
            },
            .flex => try self.constraints(type_store.flexAt(desc_idx).constraints),
            .rigid => try self.constraints(type_store.rigidAt(desc_idx).constraints),
            .alias => {
                const alias = type_store.aliasAt(desc_idx);
                try self.typeName(alias.ident.ident_idx);
                try self.origin(alias.origin_module);
            },
            .nominal_type => {
                const nominal = type_store.nominalAt(desc_idx);
                try self.typeName(nominal.ident.ident_idx);
                try self.origin(nominal.origin_module);
            },
            .record => for (type_store.getRecordFieldsSlice(type_store.recordAt(desc_idx).fields).items(.name)) |name| {
                try self.recordFieldLabel(name);
            },
            .tag_union => for (type_store.getTagsSlice(type_store.tagUnionAt(desc_idx).tags).items(.name)) |name| {
                try self.tagLabel(name);
            },
        }
    }
};

/// Mark every variable reachable from `root` reached, recording the row-tail
/// positions each is reached through, and mark each representative this
/// reaches for the first time that reaches a flex or rigid variable. Each
/// such representative is also appended to `newly_reached` when one is
/// given. `scratch` is the caller's, empty on entry and on return.
pub fn reach(allocator: Allocator, env: *const ModuleEnv, var_facts: []SealedVarFacts, scratch: *ReachScratch, root: Var, newly_reached: ?*std.ArrayList(Var)) Allocator.Error!void {
    std.debug.assert(scratch.pending.items.len == 0 and scratch.newly_reached.items.len == 0);
    errdefer scratch.clear();
    var walker = ReachWalker{ .env = env, .var_facts = var_facts, .newly_reached = &scratch.newly_reached, .pending = &scratch.pending };
    try walker.reach(allocator, root, null);
    // Most walks start at a type an earlier walk reached and reach nothing.
    if (scratch.newly_reached.items.len == 0) return;
    while (walker.pending.pop()) |var_| try walker.expand(allocator, var_);
    try markIdentityVariables(allocator, env, var_facts, scratch);
    if (newly_reached) |list| try list.appendSlice(allocator, scratch.newly_reached.items);
    scratch.clear();
}

const ReachWalker = struct {
    env: *const ModuleEnv,
    var_facts: []SealedVarFacts,
    newly_reached: *std.ArrayList(Var),
    pending: *std.ArrayList(Var),

    fn reach(self: *ReachWalker, allocator: Allocator, var_: Var, tail: ?RowDefault) Allocator.Error!void {
        const resolved = self.env.types.resolveVarWord(var_);
        const var_facts = &self.var_facts[@backingInt(resolved.var_)];
        if (!var_facts.reached) {
            var_facts.reached = true;
            try self.newly_reached.append(allocator, resolved.var_);
            try self.pending.append(allocator, resolved.var_);
        }
        switch (tail orelse return) {
            .empty_record => var_facts.record_tail = true,
            .empty_tag_union => var_facts.tag_tail = true,
        }
        if (std.debug.runtime_safety and var_facts.record_tail and var_facts.tag_tail and
            checked_artifact.checkedRowDefaultForContent(self.env, self.env.types.resolveVar(var_).desc.content, .empty_record) != null)
        {
            invariant("checked row variable {d} received incompatible defaults", .{@backingInt(resolved.var_)});
        }
    }

    /// Reach every variable the class of `var_` holds, reading the payload
    /// of its descriptor's kind alone.
    fn expand(self: *ReachWalker, allocator: Allocator, var_: Var) Allocator.Error!void {
        const env = self.env;
        const type_store = &env.types;
        const desc_idx = type_store.resolveDescIdx(var_);
        switch (type_store.kindAt(desc_idx)) {
            .err, .field_presence, .empty_record, .empty_tag_union => {},
            .flex => for (type_store.sliceStaticDispatchConstraints(type_store.flexAt(desc_idx).constraints)) |source| {
                try self.reach(allocator, source.fn_var, null);
            },
            .rigid => for (type_store.sliceStaticDispatchConstraints(type_store.rigidAt(desc_idx).constraints)) |source| {
                try self.reach(allocator, source.fn_var, null);
            },
            .alias => {
                const alias = type_store.aliasAt(desc_idx);
                try self.reach(allocator, type_store.getAliasBackingVar(alias), null);
                for (type_store.sliceAliasArgs(alias)) |arg| try self.reach(allocator, arg, null);
            },
            .record => {
                const record = type_store.recordAt(desc_idx);
                if (record.fields.len() == 0 and checked_artifact.checkedRecordExtIsEmpty(env, record.ext)) return;
                for (type_store.getRecordFieldsSlice(record.fields).items(.presence)) |presence| {
                    switch (presence.decode()) {
                        .required => |type_var| try self.reach(allocator, type_var, null),
                        .unknown => |unknown| {
                            switch (type_store.kindOf(unknown.presence)) {
                                .flex => try self.reach(allocator, unknown.presence, null),
                                .field_presence, .err => {},
                                .rigid, .alias, .record, .tuple, .nominal_type, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .tag_union, .empty_tag_union => invariant("a sealed field presence variable held non-presence content", .{}),
                            }
                            try self.reach(allocator, unknown.var_, null);
                        },
                    }
                }
                std.debug.assert(type_store.kindOf(record.ext) != .record);
                try self.reach(allocator, record.ext, .empty_record);
            },
            .tuple => for (type_store.sliceVars(type_store.tupleAt(desc_idx).elems)) |elem| try self.reach(allocator, elem, null),
            .nominal_type => for (type_store.sliceNominalArgs(type_store.nominalAt(desc_idx))) |arg| try self.reach(allocator, arg, null),
            .fn_pure, .fn_effectful, .fn_unbound => {
                const func = type_store.funcAt(desc_idx);
                for (type_store.sliceVars(func.args)) |arg| try self.reach(allocator, arg, null);
                try self.reach(allocator, func.ret, null);
            },
            .tag_union => {
                const tag_union = type_store.tagUnionAt(desc_idx);
                if (tag_union.tags.len() == 0 and checked_artifact.checkedTagUnionExtIsEmpty(env, tag_union.ext)) return;
                for (type_store.getTagsSlice(tag_union.tags).items(.args)) |args| {
                    for (type_store.sliceVars(args)) |arg| try self.reach(allocator, arg, null);
                }
                std.debug.assert(type_store.kindOf(tag_union.ext) != .tag_union);
                try self.reach(allocator, tag_union.ext, .empty_tag_union);
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
        // The published identity-variable fact agrees with a walk of the
        // rendered types.
        const id = type_ids.fromVar(var_);
        try std.testing.expectEqual(try publication.store.view().walkIdentityVariablesForTest(id), try publication.store.rootContainsIdentityVariables(id));
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
            try std.testing.expect(view.payloadTag(type_ids.asVar(record.ext)) != .record);
        },
        .tag_union => |tag_union| {
            for (0..tag_union.tags.len()) |index| for (tag_union.tags.get(index).args) |arg| try expectChild(view, arg);
            try expectChild(view, tag_union.ext);
            try std.testing.expect(view.payloadTag(type_ids.asVar(tag_union.ext)) != .tag_union);
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

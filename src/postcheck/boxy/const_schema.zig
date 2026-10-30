//! The constant store schema of a `.boxy` compile-time evaluator's roots
//! (design.md "Caching Under `.boxy`"): for a root's type, the plan the
//! store writer decodes the root's evaluated value by, and the stored type
//! the value is restored at. Both are built from the program's own type
//! nodes, so a stored type names what the program knows about the type: a
//! nominal by its declaring module's content identity and declaration, a
//! record field by its storage slot, a tag by its label.
//!
//! A function value has no schema here: a root whose value holds one is
//! evaluated by the Solved program's evaluator, which records the evidence a
//! stored function restores with (`ConstStore.FnDef`).

const std = @import("std");
const collections = @import("collections");
const check = @import("check");
const lir_core = @import("lir_core");

const types = @import("types.zig");
const lower = @import("lower.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const const_store = check.ConstStore;
const static_dispatch = check.StaticDispatchRegistry;
const LirProgram = lir_core.Program;
const TypeId = types.TypeId;
const Program = lower.Program;
const invariant = types.invariant;

/// The plans and stored types built so far, by the program type they are of.
pub const ConstSchema = struct {
    plans: collections.DenseMapUnmanaged(TypeId, LirProgram.ConstPlanId) = .empty,
    types: collections.DenseMapUnmanaged(TypeId, const_store.ConstTypeId) = .empty,

    pub fn deinit(self: *ConstSchema, allocator: Allocator) void {
        self.plans.deinit(allocator);
        self.types.deinit(allocator);
    }
};

/// The plan a value of the closed type `ty` is stored by.
pub fn constPlanOf(program: *Program, ty: TypeId) Allocator.Error!LirProgram.ConstPlanId {
    if (program.const_schema.plans.get(ty)) |existing| return existing;
    const allocator = program.allocator;
    const store = &program.store;
    // Reserved before its components are built, so a recursive type's
    // component finds the plan of the type it leads back into.
    const id: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.result.const_plans.items.len));
    try program.result.const_plans.append(allocator, .pending);
    try program.const_schema.plans.put(allocator, ty, id);
    const plan: LirProgram.ConstPlan = switch (store.get(ty)) {
        .primitive => |primitive| switch (primitive) {
            .bool => invariant("a primitive Bool reached a constant plan; Bool is a named tag union"),
            .str => .str,
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => .scalar,
        },
        .empty_record => .zst,
        .list => |elem| .{ .list = try constPlanOf(program, elem) },
        .box => |payload| .{ .box = try constPlanOf(program, payload) },
        .tuple => |items_span| blk: {
            const items = try allocator.dupe(TypeId, store.span(items_span));
            defer allocator.free(items);
            const plans = try allocator.alloc(LirProgram.ConstPlanId, items.len);
            errdefer allocator.free(plans);
            for (items, plans) |item, *slot| slot.* = try constPlanOf(program, item);
            break :blk .{ .tuple = plans };
        },
        .record => |record| blk: {
            if (record.ext != null) invariant("an open record reached a constant plan");
            const fields = try allocator.dupe(types.Field, store.fieldSpan(record.fields));
            defer allocator.free(fields);
            const plans = try allocator.alloc(LirProgram.ConstPlanId, fields.len);
            errdefer allocator.free(plans);
            // A field's value is stored as its storage slot: an optional
            // field as its presence slot's tag (design.md "Field Kinds").
            for (fields, plans) |field, *slot| slot.* = try constPlanOf(program, try program.layouts.fieldStorage(field));
            break :blk .{ .record = plans };
        },
        .tag_union => |tag_union| blk: {
            if (tag_union.ext != null) invariant("an open tag union reached a constant plan");
            const tags = try allocator.dupe(types.Tag, store.tagSpan(tag_union.tags));
            defer allocator.free(tags);
            const variants = try allocator.alloc(LirProgram.ConstTagVariant, tags.len);
            var built: usize = 0;
            errdefer {
                for (variants[0..built]) |variant| {
                    allocator.free(variant.name);
                    allocator.free(variant.payloads);
                }
                allocator.free(variants);
            }
            // The committed layout numbers variants in the type's tag order.
            for (tags, variants, 0..) |tag, *variant, index| {
                const payload_types = try allocator.dupe(TypeId, store.span(tag.payloads));
                defer allocator.free(payload_types);
                const payloads = try allocator.alloc(LirProgram.ConstPlanId, payload_types.len);
                errdefer allocator.free(payloads);
                for (payload_types, payloads) |payload, *slot| slot.* = try constPlanOf(program, payload);
                const text = store.nameText(tag.name);
                variant.* = .{
                    .name = try allocator.dupe(u8, text),
                    .checked_name = try program.result.const_type_names.internTagLabel(text),
                    .discriminant = @intCast(index),
                    .payloads = payloads,
                };
                built += 1;
            }
            break :blk .{ .tag_union = variants };
        },
        .nominal => |nominal| blk: {
            if (!nominal.has_backing) invariant("an opaque type without a backing reached a constant plan");
            break :blk .{ .named = .{
                .named_type = namedType(program, nominal),
                .backing = try constPlanOf(program, try program.layouts.converter.backing(ty)),
            } };
        },
        .function => invariant("a function value reached a constant plan; roots holding function values are evaluated specialized"),
        .variable => invariant("an open type reached a constant plan"),
    };
    program.result.const_plans.items[@backingInt(id)] = plan;
    return id;
}

/// The stored type a value of the closed type `ty` is restored at.
pub fn constTypeOf(program: *Program, ty: TypeId) Allocator.Error!const_store.ConstTypeId {
    if (program.const_schema.types.get(ty)) |existing| return existing;
    const allocator = program.allocator;
    const store = &program.store;
    const names = &program.result.const_type_names;
    const const_types = &program.result.const_types;
    // Reserved before its components are built, so a recursive type's
    // component finds the type it leads back into.
    const id = try const_types.reserve();
    try program.const_schema.types.put(allocator, ty, id);
    const stored: const_store.ConstType = switch (store.get(ty)) {
        .primitive => |primitive| .{ .primitive = primitive },
        .empty_record => .zst,
        .list => |elem| .{ .list = try constTypeOf(program, elem) },
        .box => |payload| .{ .box = try constTypeOf(program, payload) },
        .tuple => |items_span| blk: {
            const items = try allocator.dupe(TypeId, store.span(items_span));
            defer allocator.free(items);
            const ids = try allocator.alloc(const_store.ConstTypeId, items.len);
            defer allocator.free(ids);
            for (items, ids) |item, *slot| slot.* = try constTypeOf(program, item);
            break :blk .{ .tuple = try const_types.appendTypeSpan(ids) };
        },
        .record => |record| blk: {
            if (record.ext != null) invariant("an open record reached a stored type");
            const fields = try allocator.dupe(types.Field, store.fieldSpan(record.fields));
            defer allocator.free(fields);
            const out = try allocator.alloc(const_store.TypeField, fields.len);
            defer allocator.free(out);
            for (fields, out) |field, *slot| {
                // A stored record field is its storage slot; an optional
                // field also names the value type its presence slot holds,
                // which is how a restoring program tells the kinds apart.
                const storage = try program.layouts.fieldStorage(field);
                slot.* = .{
                    .name = try names.internRecordFieldLabel(store.nameText(field.name)),
                    .ty = try constTypeOf(program, storage),
                    .value_ty = switch (field.kind) {
                        .inline_slot => null,
                        .presence_slot => try constTypeOf(program, field.ty),
                    },
                    .default = null,
                };
            }
            break :blk .{ .record = try const_types.appendFieldSpan(out) };
        },
        .tag_union => |tag_union| blk: {
            if (tag_union.ext != null) invariant("an open tag union reached a stored type");
            const tags = try allocator.dupe(types.Tag, store.tagSpan(tag_union.tags));
            defer allocator.free(tags);
            const out = try allocator.alloc(const_store.TypeTag, tags.len);
            defer allocator.free(out);
            for (tags, out) |tag, *slot| {
                const payload_types = try allocator.dupe(TypeId, store.span(tag.payloads));
                defer allocator.free(payload_types);
                const ids = try allocator.alloc(const_store.ConstTypeId, payload_types.len);
                defer allocator.free(ids);
                for (payload_types, ids) |payload, *payload_slot| payload_slot.* = try constTypeOf(program, payload);
                const name = try names.internTagLabel(store.nameText(tag.name));
                slot.* = .{ .name = name, .checked_name = name, .payloads = try const_types.appendTypeSpan(ids) };
            }
            break :blk .{ .tag_union = try const_types.appendTagSpan(out) };
        },
        .nominal => |nominal| blk: {
            if (!nominal.has_backing) invariant("an opaque type without a backing reached a stored type");
            const view = program.module(nominal.decl.module);
            const declaration = view.types().nominalDeclarationById(nominal.decl.decl);
            // The declaration's own arguments; a block-local declaration's
            // outer variables follow them and are not stored.
            const formal_count = declaration.formalArgs(view.types()).len;
            const all_args = try allocator.dupe(TypeId, store.nominalArgs(nominal));
            defer allocator.free(all_args);
            if (all_args.len < formal_count) invariant("a nominal type has fewer arguments than its declaration's formals");
            const ids = try allocator.alloc(const_store.ConstTypeId, formal_count);
            defer allocator.free(ids);
            for (all_args[0..formal_count], ids) |arg, *slot| slot.* = try constTypeOf(program, arg);
            const backing = try constTypeOf(program, try program.layouts.converter.backing(ty));
            break :blk .{ .named = .{
                .named_type = namedType(program, nominal),
                .def = .{
                    .module = try names.internModuleIdentity(view.nameStore().moduleIdentityBytes(declaration.nominal.module)),
                    .type_name = try names.internTypeName(store.nameText(nominal.name)),
                    .source_decl = declaration.nominal.source_decl,
                },
                .kind = if (nominal.is_opaque) .@"opaque" else .nominal,
                .builtin_owner = builtinOwner(nominal.builtin),
                .args = try const_types.appendTypeSpan(ids),
                .backing = .{ .ty = backing, .use = .inspectable, .authority = .checked_public },
            } };
        },
        .function => invariant("a function value reached a stored type; roots holding function values are evaluated specialized"),
        .variable => invariant("an open type reached a stored type"),
    };
    try const_types.fill(id, stored);
    return id;
}

/// The named type a stored nominal value names: its declaration's root in
/// the declaring module's type store.
fn namedType(program: *Program, nominal: types.Nominal) const_store.NamedType {
    const view = program.module(nominal.decl.module);
    const declaration = view.types().nominalDeclarationById(nominal.decl.decl);
    return .{ .module = view.key, .ty = declaration.declaration_root, .ty_module = view.key };
}

/// The builtin owner a stored builtin nominal dispatches through; `Try` has
/// none, as every strategy's stored types agree.
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
        .u8x16 => .u8x16,
        .i8x16 => .i8x16,
        .u16x8 => .u16x8,
        .i16x8 => .i16x8,
        .u32x4 => .u32x4,
        .i32x4 => .i32x4,
        .u64x2 => .u64x2,
        .i64x2 => .i64x2,
        .list => .list,
        .box => .box,
        .dict => .dict,
        .set => .set,
        .parse_tag_union_spec => .parse_tag_union_spec,
        .fields => .fields,
        .field => .field,
        .try_ => null,
        .iter => .iter,
        .stream => .stream,
        .crypto_sha256_digest => .crypto_sha256_digest,
        .crypto_sha256_hasher => .crypto_sha256_hasher,
        .crypto_blake3_digest => .crypto_blake3_digest,
        .crypto_blake3_hasher => .crypto_blake3_hasher,
    };
}

/// Whether a value of `ty` holds a function value anywhere: in a component,
/// a list item, a box payload, or a nominal's backing. Such a root's value
/// has no schema here.
pub fn holdsFunction(program: *Program, ty: TypeId) Allocator.Error!bool {
    const allocator = program.allocator;
    const store = &program.store;
    var seen: collections.DenseMapUnmanaged(TypeId, void) = .empty;
    defer seen.deinit(allocator);
    var pending: std.ArrayList(TypeId) = .empty;
    defer pending.deinit(allocator);
    try pending.append(allocator, ty);
    while (pending.pop()) |current| {
        if (seen.contains(current)) continue;
        try seen.put(allocator, current, {});
        switch (store.get(current)) {
            .function => return true,
            .primitive, .empty_record, .variable => {},
            .list, .box => |child| try pending.append(allocator, child),
            .tuple => |items| try pending.appendSlice(allocator, store.span(items)),
            .record => |record| {
                const fields = try allocator.dupe(types.Field, store.fieldSpan(record.fields));
                defer allocator.free(fields);
                for (fields) |field| try pending.append(allocator, field.ty);
                if (record.ext) |ext| try pending.append(allocator, ext);
            },
            .tag_union => |tag_union| {
                const tags = try allocator.dupe(types.Tag, store.tagSpan(tag_union.tags));
                defer allocator.free(tags);
                for (tags) |tag| try pending.appendSlice(allocator, store.span(tag.payloads));
                if (tag_union.ext) |ext| try pending.append(allocator, ext);
            },
            .nominal => |nominal| if (nominal.has_backing) try pending.append(allocator, try program.layouts.converter.backing(current)),
        }
    }
    return false;
}

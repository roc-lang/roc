//! The constant store schema of a `.boxy` compile-time evaluator's roots
//! (design.md "Caching Under `.boxy`"): for a root's type, the plan the
//! store writer decodes the root's evaluated value by, and the stored type
//! the value is restored at. Both are built from the program's own type
//! nodes, so a stored type names what the program knows about the type: a
//! nominal by its declaring module's content identity and declaration, a
//! record field by its storage slot, a tag by its label.
//!
//! A function value has no schema of its own: it is stored by what it
//! captured (`stored_fn.zig`), whatever its type.

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
    var walk: PlanWalk = .{ .program = program };
    defer walk.deinit();
    const root = try walk.enter(ty);
    while (walk.frames.items.len != 0) {
        const top = walk.frames.items.len - 1;
        var frame = walk.frames.items[top];
        if (try walk.advance(&frame)) |plan| {
            program.result.const_plans.items[@backingInt(frame.id)] = plan;
            _ = walk.frames.pop();
        } else {
            walk.frames.items[top] = frame;
        }
    }
    return root;
}

/// One plan being built, and how far its components have been.
const PlanFrame = struct {
    ty: TypeId,
    id: LirProgram.ConstPlanId,
    /// Where the plans of this frame's finished components start in
    /// `PlanWalk.ids`.
    ids_start: usize,
    /// Where this frame's finished variants start in `PlanWalk.variants`.
    variants_start: usize,
    /// The component being built: a tuple item, a record field, or a tag.
    index: usize = 0,
    /// The payload being built of the tag at `index`.
    payload: usize = 0,
};

/// The plans under construction, innermost last (design.md "Stack Safety").
/// A frame whose component has no plan yet suspends under that component's
/// frame, and finds the component's plan when it resumes.
const PlanWalk = struct {
    program: *Program,
    frames: std.ArrayList(PlanFrame) = .empty,
    /// The plans of the unfinished frames' finished components.
    ids: std.ArrayList(LirProgram.ConstPlanId) = .empty,
    /// The finished variants of the unfinished tag union frames.
    variants: std.ArrayList(LirProgram.ConstTagVariant) = .empty,

    fn deinit(self: *PlanWalk) void {
        const allocator = self.program.allocator;
        // Variants are left here only by a failed walk.
        for (self.variants.items) |variant| {
            allocator.free(variant.name);
            allocator.free(variant.payloads);
        }
        self.variants.deinit(allocator);
        self.ids.deinit(allocator);
        self.frames.deinit(allocator);
    }

    /// Reserve the plan of `ty` and start building it. The plan is reserved
    /// before its components are built, so a recursive type's component
    /// finds the plan of the type it leads back into.
    fn enter(self: *PlanWalk, ty: TypeId) Allocator.Error!LirProgram.ConstPlanId {
        const program = self.program;
        const allocator = program.allocator;
        const id: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.result.const_plans.items.len));
        try program.result.const_plans.append(allocator, .pending);
        try program.const_schema.plans.put(allocator, ty, id);
        try self.frames.append(allocator, .{ .ty = ty, .id = id, .ids_start = self.ids.items.len, .variants_start = self.variants.items.len });
        return id;
    }

    /// The plan of the component `ty`, or null once its frame is entered
    /// for the suspending frame to wait under.
    fn component(self: *PlanWalk, ty: TypeId) Allocator.Error!?LirProgram.ConstPlanId {
        if (self.program.const_schema.plans.get(ty)) |existing| return existing;
        _ = try self.enter(ty);
        return null;
    }

    /// The plans of `frame`'s finished components, as a slice the plan owns.
    fn takeIds(self: *PlanWalk, frame: *const PlanFrame) Allocator.Error![]const LirProgram.ConstPlanId {
        const owned = try self.program.allocator.dupe(LirProgram.ConstPlanId, self.ids.items[frame.ids_start..]);
        self.ids.shrinkRetainingCapacity(frame.ids_start);
        return owned;
    }

    /// Build `frame`'s plan as far as its components allow: the plan, or
    /// null when the frame suspended under a component's frame.
    fn advance(self: *PlanWalk, frame: *PlanFrame) Allocator.Error!?LirProgram.ConstPlan {
        const program = self.program;
        const allocator = program.allocator;
        const store = &program.store;
        switch (store.get(frame.ty)) {
            .primitive => |primitive| return switch (primitive) {
                .bool => invariant("a primitive Bool reached a constant plan; Bool is a named tag union"),
                .str => .str,
                .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => .scalar,
            },
            // The empty record is stored as a record of no fields, as the
            // Solved evaluator stores it; `.zst` is the Solved program's own
            // zero-sized type, which no checked type is.
            .empty_record => return .{ .record = try allocator.alloc(LirProgram.ConstPlanId, 0) },
            .list => |elem| return .{ .list = (try self.component(elem)) orelse return null },
            .box => |payload| {
                // A box of a function is the function's erased callable.
                if (store.get(payload) == .function) return .boxy_fn;
                const payload_plan = (try self.component(payload)) orelse return null;
                if (!store.isSequence(payload)) return .{ .box = payload_plan };
                return .{ .boxed_sequence = .{
                    .payload = payload_plan,
                    .view_layout = try program.layouts.layoutOf(try program.layouts.converter.backing(payload)),
                } };
            },
            .tuple => |items| {
                while (frame.index < store.span(items).len) : (frame.index += 1) {
                    const item = store.span(items)[frame.index];
                    try self.ids.append(allocator, (try self.component(item)) orelse return null);
                }
                return .{ .tuple = try self.takeIds(frame) };
            },
            .record => |record| {
                if (record.ext != .none) invariant("an open record reached a constant plan");
                while (frame.index < store.fieldSpan(record.fields).len) : (frame.index += 1) {
                    // A field's value is stored as its storage slot: an optional
                    // field as its presence slot's tag (design.md "Field Kinds").
                    const storage = try program.layouts.fieldStorage(store.fieldSpan(record.fields)[frame.index]);
                    try self.ids.append(allocator, (try self.component(storage)) orelse return null);
                }
                return .{ .record = try self.takeIds(frame) };
            },
            .tag_union => |tag_union| {
                if (tag_union.ext != null) invariant("an open tag union reached a constant plan");
                // The committed layout numbers variants in the type's tag order.
                while (frame.index < store.tagSpan(tag_union.tags).len) {
                    const tag = store.tagSpan(tag_union.tags)[frame.index];
                    while (frame.payload < store.span(tag.payloads).len) : (frame.payload += 1) {
                        const payload = store.span(tag.payloads)[frame.payload];
                        try self.ids.append(allocator, (try self.component(payload)) orelse return null);
                    }
                    try self.variants.ensureUnusedCapacity(allocator, 1);
                    const payloads = try self.takeIds(frame);
                    errdefer allocator.free(payloads);
                    const text = store.nameText(tag.name);
                    const name = try allocator.dupe(u8, text);
                    errdefer allocator.free(name);
                    self.variants.appendAssumeCapacity(.{
                        .name = name,
                        .checked_name = try program.result.const_type_names.internTagLabel(text),
                        .discriminant = @intCast(frame.index),
                        .payloads = payloads,
                    });
                    frame.index += 1;
                    frame.payload = 0;
                }
                const variants = try allocator.dupe(LirProgram.ConstTagVariant, self.variants.items[frame.variants_start..]);
                self.variants.shrinkRetainingCapacity(frame.variants_start);
                return .{ .tag_union = variants };
            },
            .nominal => |nominal| {
                if (!nominal.has_backing) invariant("an opaque type without a backing reached a constant plan");
                const backing = (try self.component(try program.layouts.converter.backing(frame.ty))) orelse return null;
                return .{ .named = .{ .named_type = namedType(program, nominal), .backing = backing } };
            },
            .function => return .boxy_fn,
            .variable => invariant("an open type reached a constant plan"),
        }
    }
};

/// The stored type a value of the closed type `ty` is restored at.
pub fn constTypeOf(program: *Program, ty: TypeId) Allocator.Error!const_store.ConstTypeId {
    if (program.const_schema.types.get(ty)) |existing| return existing;
    var walk: TypeWalk = .{ .program = program };
    defer walk.deinit();
    const root = try walk.enter(ty);
    while (walk.frames.items.len != 0) {
        const top = walk.frames.items.len - 1;
        var frame = walk.frames.items[top];
        if (try walk.advance(&frame)) |stored| {
            try program.result.const_types.fill(frame.id, stored);
            _ = walk.frames.pop();
        } else {
            walk.frames.items[top] = frame;
        }
    }
    return root;
}

/// One stored type being built, and how far its components have been.
const TypeFrame = struct {
    ty: TypeId,
    id: const_store.ConstTypeId,
    /// Where the stored types of this frame's finished components start in
    /// `TypeWalk.ids`.
    ids_start: usize,
    /// Where this frame's finished entries start in `TypeWalk.fields`,
    /// `TypeWalk.tags` and `TypeWalk.declared`.
    fields_start: usize,
    tags_start: usize,
    declared_start: usize,
    /// Which part of a nominal or a function is being built.
    part: enum { args, rest } = .args,
    /// The component of `part` being built: a tuple item, a record field, a
    /// tag, an argument, or a nominal's declared entry.
    index: usize = 0,
    /// The payload being built of the tag at `index`.
    payload: usize = 0,
    /// A function's stored arguments, once every one is built.
    args: const_store.ConstRange = .{},
};

/// The stored types under construction, innermost last (design.md "Stack
/// Safety"). A frame whose component has no stored type yet suspends under
/// that component's frame, and finds the component's type when it resumes.
/// Every step a resumed frame repeats (a type's backing, a field's storage
/// slot, an interned label) is memoized or interned, so ids, labels and
/// spans are allocated in the order the components are reached.
const TypeWalk = struct {
    program: *Program,
    frames: std.ArrayList(TypeFrame) = .empty,
    /// The stored types of the unfinished frames' finished components.
    ids: std.ArrayList(const_store.ConstTypeId) = .empty,
    fields: std.ArrayList(const_store.TypeField) = .empty,
    tags: std.ArrayList(const_store.TypeTag) = .empty,
    declared: std.ArrayList(const_store.TypeDeclaredField) = .empty,

    fn deinit(self: *TypeWalk) void {
        const allocator = self.program.allocator;
        self.declared.deinit(allocator);
        self.tags.deinit(allocator);
        self.fields.deinit(allocator);
        self.ids.deinit(allocator);
        self.frames.deinit(allocator);
    }

    /// Reserve the stored type of `ty` and start building it. The type is
    /// reserved before its components are built, so a recursive type's
    /// component finds the type it leads back into.
    fn enter(self: *TypeWalk, ty: TypeId) Allocator.Error!const_store.ConstTypeId {
        const program = self.program;
        const allocator = program.allocator;
        const id = try program.result.const_types.reserve();
        try program.const_schema.types.put(allocator, ty, id);
        try self.frames.append(allocator, .{
            .ty = ty,
            .id = id,
            .ids_start = self.ids.items.len,
            .fields_start = self.fields.items.len,
            .tags_start = self.tags.items.len,
            .declared_start = self.declared.items.len,
        });
        return id;
    }

    /// The stored type of the component `ty`, or null once its frame is
    /// entered for the suspending frame to wait under.
    fn component(self: *TypeWalk, ty: TypeId) Allocator.Error!?const_store.ConstTypeId {
        if (self.program.const_schema.types.get(ty)) |existing| return existing;
        _ = try self.enter(ty);
        return null;
    }

    /// Store the types of `frame`'s finished components as one span.
    fn takeIds(self: *TypeWalk, frame: *const TypeFrame) Allocator.Error!const_store.ConstRange {
        const span = try self.program.result.const_types.appendTypeSpan(self.ids.items[frame.ids_start..]);
        self.ids.shrinkRetainingCapacity(frame.ids_start);
        return span;
    }

    /// Build `frame`'s stored type as far as its components allow: the
    /// type, or null when the frame suspended under a component's frame.
    fn advance(self: *TypeWalk, frame: *TypeFrame) Allocator.Error!?const_store.ConstType {
        const program = self.program;
        const allocator = program.allocator;
        const store = &program.store;
        const names = &program.result.const_type_names;
        const const_types = &program.result.const_types;
        switch (store.get(frame.ty)) {
            .primitive => |primitive| return .{ .primitive = primitive },
            .empty_record => return .{ .record = try const_types.appendFieldSpan(&.{}) },
            .list => |elem| return .{ .list = (try self.component(elem)) orelse return null },
            .box => |payload| return .{ .box = (try self.component(payload)) orelse return null },
            .tuple => |items| {
                while (frame.index < store.span(items).len) : (frame.index += 1) {
                    const item = store.span(items)[frame.index];
                    try self.ids.append(allocator, (try self.component(item)) orelse return null);
                }
                return .{ .tuple = try self.takeIds(frame) };
            },
            .record => |record| {
                if (record.ext != .none) invariant("an open record reached a stored type");
                while (frame.index < store.fieldSpan(record.fields).len) : (frame.index += 1) {
                    const field = store.fieldSpan(record.fields)[frame.index];
                    // A stored record field is its storage slot; an optional
                    // field also names the value type its presence slot holds,
                    // which is how a restoring program tells the kinds apart.
                    const storage = try program.layouts.fieldStorage(field);
                    const name = try names.internRecordFieldLabel(store.nameText(field.name));
                    const storage_id = (try self.component(storage)) orelse return null;
                    try self.fields.append(allocator, .{
                        .name = name,
                        .ty = storage_id,
                        .value_ty = switch (field.kind) {
                            .inline_slot => .none,
                            .presence_slot => .some((try self.component(field.ty)) orelse return null),
                            .variable_slot => unreachable, // a compile-time value's type is closed
                        },
                        .default = .none,
                    });
                }
                const span = try const_types.appendFieldSpan(self.fields.items[frame.fields_start..]);
                self.fields.shrinkRetainingCapacity(frame.fields_start);
                return .{ .record = span };
            },
            .tag_union => |tag_union| {
                if (tag_union.ext != null) invariant("an open tag union reached a stored type");
                while (frame.index < store.tagSpan(tag_union.tags).len) {
                    const tag = store.tagSpan(tag_union.tags)[frame.index];
                    while (frame.payload < store.span(tag.payloads).len) : (frame.payload += 1) {
                        const payload = store.span(tag.payloads)[frame.payload];
                        try self.ids.append(allocator, (try self.component(payload)) orelse return null);
                    }
                    const name = try names.internTagLabel(store.nameText(tag.name));
                    try self.tags.ensureUnusedCapacity(allocator, 1);
                    self.tags.appendAssumeCapacity(.{ .name = name, .checked_name = name, .payloads = try self.takeIds(frame) });
                    frame.index += 1;
                    frame.payload = 0;
                }
                const span = try const_types.appendTagSpan(self.tags.items[frame.tags_start..]);
                self.tags.shrinkRetainingCapacity(frame.tags_start);
                return .{ .tag_union = span };
            },
            .nominal => |nominal| {
                if (!nominal.has_backing) invariant("an opaque type without a backing reached a stored type");
                const view = program.module(nominal.decl.module);
                const declaration = view.types().nominalDeclarationById(nominal.decl.decl);
                // The declaration's own arguments; a block-local declaration's
                // context arguments follow them and are not stored.
                const formal_count = declaration.declaredFormalArgs(view.types()).len;
                if (store.nominalArgs(nominal).len < formal_count) invariant("a nominal type has fewer arguments than its declaration's formals");
                if (frame.part == .args) {
                    while (frame.index < formal_count) : (frame.index += 1) {
                        const arg = store.nominalArgs(nominal)[frame.index];
                        try self.ids.append(allocator, (try self.component(arg)) orelse return null);
                    }
                    frame.part = .rest;
                    frame.index = 0;
                }
                const backing_ty = try program.layouts.converter.backing(frame.ty);
                const backing = (try self.component(backing_ty)) orelse return null;
                const declared_order = (try self.declaredOrder(frame, backing_ty)) orelse return null;
                // An opaque declaration's backing is a layout later stages never
                // look into, as Monotype types it.
                return .{ .named = .{
                    .named_type = namedType(program, nominal),
                    .def = .{
                        .module = try names.internModuleIdentity(view.nameStore().moduleIdentityBytes(declaration.nominal.module)),
                        .type_name = try names.internTypeName(store.nameText(nominal.name)),
                        .source_decl = .some(declaration.nominal.source_decl),
                    },
                    .kind = if (nominal.is_opaque) .@"opaque" else .nominal,
                    .builtin_owner = .of(builtinOwner(nominal.builtin)),
                    .args = try self.takeIds(frame),
                    .backing = .some(.{ .ty = backing, .use = if (nominal.is_opaque) .runtime_layout_only else .inspectable, .authority = .checked_public }),
                    .declared_order = declared_order,
                } };
            },
            .function => |function| {
                if (frame.part == .args) {
                    while (frame.index < store.span(function.args).len) : (frame.index += 1) {
                        const arg = store.span(function.args)[frame.index];
                        try self.ids.append(allocator, (try self.component(arg)) orelse return null);
                    }
                    frame.args = try self.takeIds(frame);
                    frame.part = .rest;
                }
                return .{ .func = .{ .args = frame.args, .ret = (try self.component(function.ret)) orelse return null } };
            },
            .variable => invariant("an open type reached a stored type"),
        }
    }

    /// The declared order stored with the nominal `frame` builds, whose
    /// backing is `backing_ty`: the declaration's entries in declared order,
    /// named fields by label, as the checked declaration and the specialized
    /// lowering record it. The fields a row extension supplies have no
    /// entry. Null when the frame suspended under an unnamed field's frame.
    fn declaredOrder(self: *TypeWalk, frame: *TypeFrame, backing_ty: TypeId) Allocator.Error!?const_store.ConstRange {
        const program = self.program;
        const allocator = program.allocator;
        const store = &program.store;
        const record = switch (store.get(backing_ty)) {
            .record => |record| record,
            .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => return .{},
        };
        const declared = record.declared.get() orelse return .{};
        while (frame.index < store.declaredEntries(declared).len) : (frame.index += 1) {
            const entry: const_store.TypeDeclaredField = switch (store.declaredEntries(declared)[frame.index]) {
                .named => |name| .{ .named = try program.result.const_type_names.internRecordFieldLabel(store.nameText(name)) },
                .padding => |ordinal| .{ .padding = (try self.component(store.span(declared.padding)[ordinal])) orelse return null },
            };
            try self.declared.append(allocator, entry);
        }
        const span = try program.result.const_types.appendDeclaredFieldSpan(self.declared.items[frame.declared_start..]);
        self.declared.shrinkRetainingCapacity(frame.declared_start);
        return span;
    }
};

/// The named type a stored nominal value names: its declaring module.
fn namedType(program: *Program, nominal: types.Nominal) const_store.NamedType {
    const view = program.module(nominal.decl.module);
    return .{ .module = view.key };
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
                if (record.ext.get()) |ext| try pending.append(allocator, ext);
                try pending.appendSlice(allocator, store.paddingTypes(record));
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

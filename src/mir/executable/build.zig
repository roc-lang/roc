//! Executable MIR construction state.

const std = @import("std");
const builtin = @import("builtin");
const check = @import("check");
const symbol_mod = @import("symbol");
const LambdaSolved = @import("../lambda_solved/mod.zig");
const MonoRow = @import("../mono_row/mod.zig");
const debug = @import("../debug_verify.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const Layouts = @import("layouts.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const repr = LambdaSolved.Representation;

pub const Proc = struct {
    executable_proc: Ast.ExecutableProcId,
    source_proc: canonical.MirProcedureRef,
    body: Ast.DefId,
};

pub const Program = struct {
    allocator: Allocator,
    canonical_names: canonical.CanonicalNameStore,
    literal_pool: ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    row_shapes: MonoRow.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    root_procs: std.ArrayList(Ast.ExecutableProcId),
    root_metadata: std.ArrayList(ids.RootMetadata),
    callable_set_descriptors: []repr.CanonicalCallableSetDescriptor = &.{},
    layouts: ?Layouts.Layouts = null,

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .row_shapes = MonoRow.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .root_procs = .empty,
            .root_metadata = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        if (self.layouts) |*layouts| layouts.deinit();
        deinitCallableSetDescriptors(self.allocator, self.callable_set_descriptors);
        self.root_metadata.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.row_shapes.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn run(allocator: Allocator, solved: LambdaSolved.Solve.Program) Allocator.Error!Program {
    var input = solved;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.canonical_names = input.canonical_names;
    input.canonical_names = canonical.CanonicalNameStore.init(allocator);
    program.literal_pool = input.literal_pool;
    input.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.row_shapes;
    input.row_shapes = MonoRow.Store.init(allocator);

    var proc_map = std.AutoHashMap(canonical.MirProcedureRef, Ast.ExecutableProcId).init(allocator);
    defer proc_map.deinit();
    var proc_instance_map = std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId).init(allocator);
    defer proc_instance_map.deinit();
    var proc_exec_map = std.AutoHashMap(repr.ProcRepresentationInstanceId, Ast.ExecutableProcId).init(allocator);
    defer proc_exec_map.deinit();
    const normal_proc_count = input.procs.items.len;
    const executable_synthetic_proc_count = input.executable_synthetic_procs.items.len;
    const total_proc_count = normal_proc_count + executable_synthetic_proc_count;

    try proc_map.ensureTotalCapacity(@intCast(total_proc_count));
    try proc_instance_map.ensureTotalCapacity(@intCast(input.procs.items.len));
    try proc_exec_map.ensureTotalCapacity(@intCast(input.procs.items.len));
    for (input.procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(i)));
        proc_map.putAssumeCapacity(proc.proc, executable_proc);
        proc_instance_map.putAssumeCapacity(proc.proc, proc.representation_instance);
        proc_exec_map.putAssumeCapacity(proc.representation_instance, executable_proc);
    }
    for (input.executable_synthetic_procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(normal_proc_count + i)));
        proc_map.putAssumeCapacity(proc.source_proc, executable_proc);
    }

    try program.procs.ensureTotalCapacity(allocator, total_proc_count);
    var type_lowerer = TypeLowerer.init(allocator, &input.types, &program.types, &program.row_shapes);
    defer type_lowerer.deinit();

    for (input.procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(i)));
        const value_store = &input.value_stores.items[@intFromEnum(proc.representation_instance)];
        const proc_instance = &input.proc_instances.items[@intFromEnum(proc.representation_instance)];
        var builder = BodyBuilder{
            .allocator = allocator,
            .input = &input.ast,
            .output = &program.ast,
            .canonical_names = &program.canonical_names,
            .type_lowerer = &type_lowerer,
            .value_store = value_store,
            .representation_store = &input.solve_sessions.items[@intFromEnum(proc_instance.solve_session)].representation_store,
            .env = std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef).init(allocator),
            .expr_map = std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId).init(allocator),
            .executable_proc = executable_proc,
            .source_proc = proc.proc,
            .representation_instance = proc.representation_instance,
            .proc_instance = proc_instance,
            .proc_instances = input.proc_instances.items,
            .solve_sessions = input.solve_sessions.items,
            .value_stores = input.value_stores.items,
            .proc_map = &proc_map,
            .proc_instance_map = &proc_instance_map,
            .proc_exec_map = &proc_exec_map,
            .active_callable_sets = std.ArrayList(ActiveCallableSetType).empty,
        };
        defer builder.deinit();

        program.procs.appendAssumeCapacity(.{
            .executable_proc = executable_proc,
            .source_proc = proc.proc,
            .body = try builder.lowerDef(proc.body),
        });
    }
    for (input.executable_synthetic_procs.items, 0..) |synthetic, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(normal_proc_count + i)));
        program.procs.appendAssumeCapacity(.{
            .executable_proc = executable_proc,
            .source_proc = synthetic.source_proc,
            .body = try lowerExecutableSyntheticProc(allocator, &program, synthetic, executable_proc),
        });
    }

    for (input.root_procs.items, input.root_metadata.items) |root, metadata| {
        const executable_root = executableProcForSource(&program, root) orelse {
            debug.invariant(false, "executable build invariant violated: root source proc has no executable proc");
            unreachable;
        };
        try program.root_procs.append(allocator, executable_root);
        try program.root_metadata.append(allocator, metadata);
    }

    program.callable_set_descriptors = try cloneCallableSetDescriptors(allocator, input.solve_sessions.items);

    input.deinit();
    return program;
}

fn cloneCallableSetDescriptors(
    allocator: Allocator,
    solve_sessions: []const repr.RepresentationSolveSession,
) Allocator.Error![]repr.CanonicalCallableSetDescriptor {
    var descriptors = std.ArrayList(repr.CanonicalCallableSetDescriptor).empty;
    errdefer {
        for (descriptors.items) |descriptor| deinitCallableSetDescriptor(allocator, descriptor);
        descriptors.deinit(allocator);
    }

    var seen = std.AutoHashMap(repr.CanonicalCallableSetKey, void).init(allocator);
    defer seen.deinit();

    for (solve_sessions) |*session| {
        for (session.representation_store.callable_set_descriptors) |descriptor| {
            const seen_entry = try seen.getOrPut(descriptor.key);
            if (seen_entry.found_existing) continue;

            const members = try allocator.alloc(repr.CanonicalCallableSetMember, descriptor.members.len);
            errdefer allocator.free(members);
            var member_count: usize = 0;
            errdefer {
                for (members[0..member_count]) |*member| allocator.free(member.capture_slots);
            }

            for (descriptor.members, 0..) |member, i| {
                const capture_slots = try allocator.dupe(repr.CallableSetCaptureSlot, member.capture_slots);
                members[i] = .{
                    .member = member.member,
                    .proc_value = member.proc_value,
                    .source_proc = member.source_proc,
                    .capture_slots = capture_slots,
                    .capture_shape_key = member.capture_shape_key,
                };
                member_count += 1;
            }

            try descriptors.append(allocator, .{
                .key = descriptor.key,
                .members = members,
            });
        }
    }

    return try descriptors.toOwnedSlice(allocator);
}

pub fn deinitCallableSetDescriptors(
    allocator: Allocator,
    descriptors: []repr.CanonicalCallableSetDescriptor,
) void {
    for (descriptors) |descriptor| deinitCallableSetDescriptor(allocator, descriptor);
    allocator.free(descriptors);
}

fn deinitCallableSetDescriptor(
    allocator: Allocator,
    descriptor: repr.CanonicalCallableSetDescriptor,
) void {
    for (descriptor.members) |member| allocator.free(member.capture_slots);
    allocator.free(descriptor.members);
}

fn lowerExecutableSyntheticProc(
    allocator: Allocator,
    program: *Program,
    synthetic: ids.ExecutableSyntheticProc,
    executable_proc: Ast.ExecutableProcId,
) Allocator.Error!Ast.DefId {
    _ = allocator;
    _ = program;
    _ = executable_proc;
    switch (synthetic.body) {
        .erased_promoted_wrapper => |erased| {
            _ = erased;
            executableInvariant("executable erased promoted wrapper lowering requires canonical executable type payload lowering");
        },
    }
}

const TypeLowerer = struct {
    allocator: Allocator,
    input: *const LambdaSolved.Type.Store,
    output: *Type.Store,
    row_shapes: *MonoRow.Store,
    active: std.AutoHashMap(LambdaSolved.Type.TypeVarId, Type.TypeId),

    fn init(
        allocator: Allocator,
        input: *const LambdaSolved.Type.Store,
        output: *Type.Store,
        row_shapes: *MonoRow.Store,
    ) TypeLowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .row_shapes = row_shapes,
            .active = std.AutoHashMap(LambdaSolved.Type.TypeVarId, Type.TypeId).init(allocator),
        };
    }

    fn deinit(self: *TypeLowerer) void {
        self.active.deinit();
    }

    fn lowerType(self: *TypeLowerer, source: LambdaSolved.Type.TypeVarId) Allocator.Error!Type.TypeId {
        const root = self.input.unlinkConst(source);
        if (self.active.get(root)) |existing| return existing;

        const target = try self.output.addType(.placeholder);
        try self.active.put(root, target);
        errdefer _ = self.active.remove(root);

        const lowered: Type.Content = switch (self.input.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => executableInvariant("executable type lowering received unresolved lambda-solved type"),
            .nominal => |nominal| .{ .nominal = .{
                .nominal = nominal.nominal,
                .backing = try self.lowerType(nominal.backing),
            } },
            .content => |content| switch (content) {
                .primitive => |prim| .{ .primitive = prim },
                .list => |elem| .{ .list = try self.lowerType(elem) },
                .box => |elem| .{ .box = try self.lowerType(elem) },
                .tuple => |span| blk: {
                    const source_items = self.input.sliceTypeVarSpan(span);
                    const items = try self.allocator.alloc(Type.TypeId, source_items.len);
                    defer self.allocator.free(items);
                    for (source_items, 0..) |item, i| {
                        items[i] = try self.lowerType(item);
                    }
                    break :blk .{ .tuple = try self.allocator.dupe(Type.TypeId, items) };
                },
                .func => executableInvariant("executable type lowering requires solved callable representation for function type"),
                .record => |record| try self.lowerRecordType(record.fields),
                .tag_union => |tag_union| try self.lowerTagUnionType(tag_union.tags),
            },
        };

        self.output.types.items[@intFromEnum(target)] = lowered;
        _ = self.active.remove(root);
        return target;
    }

    fn lowerRecordType(self: *TypeLowerer, span: LambdaSolved.Type.Span(LambdaSolved.Type.Field)) Allocator.Error!Type.Content {
        const source_fields = self.input.sliceFields(span);
        const labels = try self.allocator.alloc(canonical.RecordFieldLabelId, source_fields.len);
        defer self.allocator.free(labels);
        for (source_fields, 0..) |field, i| {
            labels[i] = field.name;
        }

        const shape = try self.row_shapes.internRecordShapeFromLabels(labels);
        const shape_fields = self.row_shapes.recordShapeFields(shape);
        if (shape_fields.len != source_fields.len) executableInvariant("executable type lowering record shape arity mismatch");

        const fields = try self.allocator.alloc(Type.RecordFieldType, source_fields.len);
        errdefer self.allocator.free(fields);
        for (source_fields, 0..) |field, i| {
            fields[i] = .{
                .field = shape_fields[i],
                .ty = try self.lowerType(field.ty),
            };
        }

        return .{ .record = .{
            .shape = shape,
            .fields = fields,
        } };
    }

    fn lowerTagUnionType(self: *TypeLowerer, span: LambdaSolved.Type.Span(LambdaSolved.Type.Tag)) Allocator.Error!Type.Content {
        const source_tags = self.input.sliceTags(span);
        const descriptors = try self.allocator.alloc(MonoRow.TagShapeDescriptor, source_tags.len);
        defer self.allocator.free(descriptors);
        for (source_tags, 0..) |tag, i| {
            descriptors[i] = .{
                .name = tag.name,
                .payload_arity = tag.args.len,
            };
        }

        const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(descriptors);
        const shape_tags = self.row_shapes.tagUnionTags(shape);
        if (shape_tags.len != source_tags.len) executableInvariant("executable type lowering tag-union shape arity mismatch");

        const tags = try self.allocator.alloc(Type.TagType, source_tags.len);
        for (tags) |*tag| tag.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (tags[0..source_tags.len]) |tag| {
                if (tag.payloads.len > 0) self.allocator.free(tag.payloads);
            }
            self.allocator.free(tags);
        }
        for (source_tags, 0..) |source_tag, i| {
            const source_payload_tys = self.input.sliceTypeVarSpan(source_tag.args);
            const shape_payloads = self.row_shapes.tagPayloads(shape_tags[i]);
            if (shape_payloads.len != source_payload_tys.len) executableInvariant("executable type lowering tag payload arity mismatch");

            const payloads = try self.allocator.alloc(Type.TagPayloadType, source_payload_tys.len);
            errdefer self.allocator.free(payloads);
            for (source_payload_tys, 0..) |payload_ty, payload_i| {
                payloads[payload_i] = .{
                    .payload = shape_payloads[payload_i],
                    .ty = try self.lowerType(payload_ty),
                };
            }

            tags[i] = .{
                .tag = shape_tags[i],
                .payloads = payloads,
            };
        }

        return .{ .tag_union = .{
            .shape = shape,
            .tags = tags,
        } };
    }
};

const ActiveCallableSetType = struct {
    key: repr.CanonicalCallableSetKey,
    ty: Type.TypeId,
};

const BodyBuilder = struct {
    allocator: Allocator,
    input: *const LambdaSolved.Ast.Store,
    output: *Ast.Store,
    canonical_names: *const canonical.CanonicalNameStore,
    type_lowerer: *TypeLowerer,
    value_store: *const repr.ValueInfoStore,
    representation_store: *const repr.RepresentationStore,
    env: std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef),
    expr_map: std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId),
    executable_proc: Ast.ExecutableProcId,
    source_proc: canonical.MirProcedureRef,
    representation_instance: repr.ProcRepresentationInstanceId,
    proc_instance: *const repr.ProcRepresentationInstance,
    proc_instances: []const repr.ProcRepresentationInstance,
    solve_sessions: []const repr.RepresentationSolveSession,
    value_stores: []const repr.ValueInfoStore,
    proc_map: *const std.AutoHashMap(canonical.MirProcedureRef, Ast.ExecutableProcId),
    proc_instance_map: *const std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId),
    proc_exec_map: *const std.AutoHashMap(repr.ProcRepresentationInstanceId, Ast.ExecutableProcId),
    active_callable_sets: std.ArrayList(ActiveCallableSetType),
    capture_record_arg: ?Ast.TypedValue = null,

    fn deinit(self: *BodyBuilder) void {
        self.active_callable_sets.deinit(self.allocator);
        self.expr_map.deinit();
        self.env.deinit();
    }

    fn lowerDef(self: *BodyBuilder, def_id: LambdaSolved.Ast.DefId) Allocator.Error!Ast.DefId {
        const def = self.input.defs.items[@intFromEnum(def_id)];
        return try self.output.addDef(switch (def.value) {
            .fn_ => |fn_| blk: {
                const args = try self.lowerFnArgSpan(fn_.args);
                const body = try self.lowerExpr(fn_.body);
                break :blk .{
                    .proc = self.executable_proc,
                    .source_proc = def.proc,
                    .specialization_key = try self.executableSpecializationKey(),
                    .value = .{ .fn_ = .{
                        .args = args,
                        .body = body,
                    } },
                };
            },
            .hosted_fn => |hosted| blk: {
                self.capture_record_arg = null;
                const args = try self.lowerParamSpan(hosted.args);
                break :blk .{
                    .proc = self.executable_proc,
                    .source_proc = def.proc,
                    .specialization_key = try self.executableSpecializationKey(),
                    .value = .{ .hosted_fn = .{
                        .args = args,
                        .ret_ty = try self.type_lowerer.lowerType(hosted.ret_ty),
                        .hosted = hosted.hosted,
                    } },
                };
            },
            .val => |expr| blk: {
                self.capture_record_arg = null;
                const body = try self.lowerExpr(expr);
                break :blk .{
                    .proc = self.executable_proc,
                    .source_proc = def.proc,
                    .specialization_key = try self.executableSpecializationKey(),
                    .value = .{ .fn_ = .{
                        .args = Ast.Span(Ast.TypedValue).empty(),
                        .body = body,
                    } },
                };
            },
            .run => |run| blk: {
                self.capture_record_arg = null;
                const body = try self.lowerExpr(run.body);
                break :blk .{
                    .proc = self.executable_proc,
                    .source_proc = def.proc,
                    .specialization_key = try self.executableSpecializationKey(),
                    .value = .{ .fn_ = .{
                        .args = Ast.Span(Ast.TypedValue).empty(),
                        .body = body,
                    } },
                };
            },
        });
    }

    fn executableSpecializationKey(self: *const BodyBuilder) Allocator.Error!repr.ExecutableSpecializationKey {
        if (!canonical.mirProcedureRefEql(self.proc_instance.proc, self.source_proc)) {
            executableInvariant("executable build procedure instance does not match the source procedure being lowered");
        }
        return try repr.cloneExecutableSpecializationKey(self.allocator, self.proc_instance.executable_specialization_key);
    }

    fn executableProcForSpecializationKey(
        self: *const BodyBuilder,
        key: repr.ExecutableSpecializationKey,
    ) Ast.ExecutableProcId {
        for (self.proc_instances, 0..) |instance, i| {
            if (repr.executableSpecializationKeyEql(instance.executable_specialization_key, key)) {
                const instance_id: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(i)));
                return self.proc_exec_map.get(instance_id) orelse executableInvariant("executable specialization key matched an unreserved proc instance");
            }
        }
        executableInvariant("executable specialization key was not reserved before body lowering");
    }

    fn lowerExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!Type.TypeId {
        return try self.lowerExecutableValueTypeInStore(
            logical_ty,
            value_info_id,
            self.value_store,
            self.representation_store,
        );
    }

    fn lowerExecutableValueTypeInStore(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        const value_info = value_store.values.items[@intFromEnum(value_info_id)];
        if (value_info.callable) |callable| {
            const emission = representation_store.callableEmissionPlan(callable.emission_plan);
            return switch (emission) {
                .finite => |key| try self.lowerFiniteCallableSetType(key, representation_store),
                .already_erased => |erased| try self.type_lowerer.output.addType(.{ .erased_fn = .{
                    .sig_key = erased.sig_key,
                    .capture_shape = erased.capture_shape_key,
                    .capture_ty = try self.lowerAlreadyErasedCaptureType(erased, value_store, representation_store),
                } }),
                .erase_proc_value => |erase| try self.type_lowerer.output.addType(.{ .erased_fn = .{
                    .sig_key = erase.erased_fn_sig_key,
                    .capture_shape = erase.capture_shape_key,
                    .capture_ty = try self.lowerProcValueErasedCaptureType(callable, erase, value_store, representation_store),
                } }),
                .erase_finite_set => |erase| try self.type_lowerer.output.addType(.{ .erased_fn = .{
                    .sig_key = erase.adapter.erased_fn_sig_key,
                    .capture_shape = erase.adapter.capture_shape_key,
                    .capture_ty = try self.lowerFiniteSetAdapterCaptureType(erase.adapter),
                } }),
            };
        }
        if (value_info.aggregate) |aggregate| {
            if (value_store != self.value_store or representation_store != self.representation_store) {
                return try self.lowerForeignAggregateExecutableValueType(logical_ty, aggregate, value_store, representation_store);
            }
            return try self.lowerAggregateExecutableValueType(logical_ty, aggregate);
        }
        return try self.type_lowerer.lowerType(logical_ty);
    }

    fn lowerFiniteCallableSetType(
        self: *BodyBuilder,
        key: repr.CanonicalCallableSetKey,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        for (self.active_callable_sets.items) |active| {
            if (repr.callableSetKeyEql(active.key, key)) return active.ty;
        }

        const descriptor = representation_store.callableSetDescriptor(key) orelse {
            executableInvariant("executable callable-set type has no descriptor");
        };
        const ty = try self.type_lowerer.output.addType(.placeholder);
        try self.active_callable_sets.append(self.allocator, .{
            .key = key,
            .ty = ty,
        });
        errdefer _ = self.active_callable_sets.pop();

        const members = try self.allocator.alloc(Type.CallableSetMemberType, descriptor.members.len);
        errdefer self.allocator.free(members);
        for (descriptor.members, 0..) |member, i| {
            members[i] = .{
                .member = member.member,
                .payload_ty = try self.lowerCallableSetMemberPayloadType(member),
            };
        }

        self.type_lowerer.output.types.items[@intFromEnum(ty)] = .{ .callable_set = .{
            .key = key,
            .members = members,
        } };
        _ = self.active_callable_sets.pop();
        return ty;
    }

    fn lowerCallableSetMemberPayloadType(
        self: *BodyBuilder,
        member: repr.CanonicalCallableSetMember,
    ) Allocator.Error!?Type.TypeId {
        if (member.capture_slots.len == 0) return null;

        const target_instance_id = self.proc_instance_map.get(member.source_proc) orelse {
            executableInvariant("executable callable-set member has no reserved representation instance");
        };
        const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];
        const target_value_store = &self.value_stores[@intFromEnum(target_instance.value_store)];
        const target_representation_store = &self.solve_sessions[@intFromEnum(target_instance.solve_session)].representation_store;
        const captures = target_value_store.sliceValueSpan(target_instance.public_roots.captures);
        if (captures.len != member.capture_slots.len) {
            executableInvariant("executable callable-set member capture count differs from target procedure captures");
        }

        const items = try self.allocator.alloc(Type.TypeId, captures.len);
        errdefer self.allocator.free(items);
        const seen = try self.allocator.alloc(bool, captures.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (member.capture_slots) |slot| {
            const index: usize = @intCast(slot.slot);
            if (index >= captures.len) executableInvariant("executable callable-set member capture slot exceeded target capture arity");
            if (seen[index]) executableInvariant("executable callable-set member capture slot was duplicated");
            const capture = captures[index];
            const info = target_value_store.values.items[@intFromEnum(capture)];
            if (builtin.mode == .Debug) {
                const actual_key = try repr.execValueTypeKeyForValue(
                    self.allocator,
                    self.canonical_names,
                    self.type_lowerer.input,
                    target_representation_store,
                    target_value_store,
                    capture,
                );
                if (!repr.canonicalExecValueTypeKeyEql(slot.exec_value_ty, actual_key)) {
                    executableInvariant("executable callable-set member capture slot executable type key differs from target capture value");
                }
            }
            items[index] = try self.lowerExecutableValueTypeInStore(
                info.logical_ty,
                capture,
                target_value_store,
                target_representation_store,
            );
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable callable-set member capture slots were not dense");
        }

        return try self.type_lowerer.output.addType(.{ .tuple = items });
    }

    fn lowerForeignAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        aggregate: repr.AggregateValueInfo,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        return switch (aggregate) {
            .record => |record| blk: {
                const fields = try self.allocator.alloc(Type.RecordFieldType, record.fields.len);
                errdefer if (fields.len > 0) self.allocator.free(fields);
                for (record.fields, 0..) |field, i| {
                    const child = value_store.values.items[@intFromEnum(field.value)];
                    fields[i] = .{
                        .field = field.field,
                        .ty = try self.lowerExecutableValueTypeInStore(child.logical_ty, field.value, value_store, representation_store),
                    };
                }
                break :blk try self.type_lowerer.output.addType(.{ .record = .{
                    .shape = record.shape,
                    .fields = fields,
                } });
            },
            .tuple => |tuple| blk: {
                const items = try self.allocator.alloc(Type.TypeId, tuple.len);
                errdefer if (items.len > 0) self.allocator.free(items);
                const seen = try self.allocator.alloc(bool, tuple.len);
                defer self.allocator.free(seen);
                @memset(seen, false);

                for (tuple) |elem| {
                    const index: usize = @intCast(elem.index);
                    if (index >= tuple.len) executableInvariant("executable foreign aggregate tuple element index exceeded tuple arity");
                    if (seen[index]) executableInvariant("executable foreign aggregate tuple had duplicate element index");
                    const child = value_store.values.items[@intFromEnum(elem.value)];
                    items[index] = try self.lowerExecutableValueTypeInStore(child.logical_ty, elem.value, value_store, representation_store);
                    seen[index] = true;
                }
                for (seen) |was_seen| {
                    if (!was_seen) executableInvariant("executable foreign aggregate tuple did not provide every element");
                }

                break :blk try self.type_lowerer.output.addType(.{ .tuple = items });
            },
            .tag => |tag| try self.lowerForeignTagAggregateExecutableValueType(logical_ty, tag, value_store, representation_store),
            .list => |list| try self.lowerForeignListAggregateExecutableValueType(logical_ty, list, value_store, representation_store),
        };
    }

    fn lowerForeignListAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        list: anytype,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        if (list.elems.len == 0) {
            const elem_ty = try self.lowerLogicalListElemType(logical_ty);
            return try self.type_lowerer.output.addType(.{ .list = elem_ty });
        }
        const first = value_store.values.items[@intFromEnum(list.elems[0])];
        const elem_ty = try self.lowerExecutableValueTypeInStore(first.logical_ty, list.elems[0], value_store, representation_store);
        return try self.type_lowerer.output.addType(.{ .list = elem_ty });
    }

    fn lowerForeignTagAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        tag: anytype,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        const source_tags = self.logicalTagUnionTags(logical_ty);
        const shape_tags = self.type_lowerer.row_shapes.tagUnionTags(tag.union_shape);
        if (shape_tags.len != source_tags.len) {
            executableInvariant("executable foreign aggregate tag metadata shape does not match logical type arity");
        }

        const tags = try self.allocator.alloc(Type.TagType, shape_tags.len);
        for (tags) |*item| item.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (tags) |item| {
                if (item.payloads.len > 0) self.allocator.free(item.payloads);
            }
            self.allocator.free(tags);
        }

        const seen_tags = try self.allocator.alloc(bool, shape_tags.len);
        defer self.allocator.free(seen_tags);
        @memset(seen_tags, false);

        for (shape_tags) |shape_tag| {
            const shape_tag_info = self.type_lowerer.row_shapes.tag(shape_tag);
            const tag_index: usize = @intCast(shape_tag_info.logical_index);
            if (tag_index >= source_tags.len) {
                executableInvariant("executable foreign aggregate tag logical index exceeded logical type arity");
            }
            if (seen_tags[tag_index]) {
                executableInvariant("executable foreign aggregate tag metadata saw duplicate tag logical index");
            }

            const payloads = if (shape_tag == tag.tag)
                try self.lowerForeignSelectedTagPayloadTypes(tag, value_store, representation_store)
            else
                try self.lowerLogicalTagPayloadTypes(shape_tag, source_tags[tag_index]);
            tags[tag_index] = .{
                .tag = shape_tag,
                .payloads = payloads,
            };
            seen_tags[tag_index] = true;
        }
        for (seen_tags) |was_seen| {
            if (!was_seen) executableInvariant("executable foreign aggregate tag metadata did not provide every logical tag");
        }

        return try self.type_lowerer.output.addType(.{ .tag_union = .{
            .shape = tag.union_shape,
            .tags = tags,
        } });
    }

    fn lowerForeignSelectedTagPayloadTypes(
        self: *BodyBuilder,
        tag: anytype,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error![]const Type.TagPayloadType {
        const shape_payloads = self.type_lowerer.row_shapes.tagPayloads(tag.tag);
        if (shape_payloads.len == 0) return &.{};
        const payloads = try self.allocator.alloc(Type.TagPayloadType, shape_payloads.len);
        errdefer self.allocator.free(payloads);
        const seen = try self.allocator.alloc(bool, shape_payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (tag.payloads) |payload| {
            const payload_info = self.type_lowerer.row_shapes.tagPayload(payload.payload);
            if (payload_info.tag != tag.tag) {
                executableInvariant("executable foreign aggregate selected tag payload belongs to a different tag");
            }
            const payload_index: usize = @intCast(payload_info.logical_index);
            if (payload_index >= shape_payloads.len) {
                executableInvariant("executable foreign aggregate selected tag payload index exceeded tag arity");
            }
            if (seen[payload_index]) {
                executableInvariant("executable foreign aggregate selected tag payload was duplicated");
            }
            if (payload.payload != shape_payloads[payload_index]) {
                executableInvariant("executable foreign aggregate selected tag payload id differs from row shape logical slot");
            }
            const child = value_store.values.items[@intFromEnum(payload.value)];
            payloads[payload_index] = .{
                .payload = shape_payloads[payload_index],
                .ty = try self.lowerExecutableValueTypeInStore(child.logical_ty, payload.value, value_store, representation_store),
            };
            seen[payload_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable foreign aggregate selected tag did not provide every payload");
        }
        return payloads;
    }

    fn lowerAlreadyErasedCaptureType(
        self: *BodyBuilder,
        erased: repr.AlreadyErasedCallablePlan,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!?Type.TypeId {
        return switch (erased.capture) {
            .none => blk: {
                if (erased.sig_key.capture_ty != null) {
                    executableInvariant("executable already-erased capture plan is none but signature has hidden capture type");
                }
                break :blk null;
            },
            .zero_sized_ty => |logical_ty| blk: {
                const capture_key = erased.sig_key.capture_ty orelse {
                    executableInvariant("executable already-erased zero-sized capture has no hidden capture type key");
                };
                if (builtin.mode == .Debug) {
                    const actual_key = try repr.execValueTypeKey(
                        self.allocator,
                        self.canonical_names,
                        self.type_lowerer.input,
                        logical_ty,
                    );
                    if (!repr.canonicalExecValueTypeKeyEql(actual_key, capture_key)) {
                        executableInvariant("executable already-erased zero-sized capture type disagrees with signature key");
                    }
                }
                break :blk try self.type_lowerer.lowerType(logical_ty);
            },
            .value => |capture_value| blk: {
                const capture_key = erased.sig_key.capture_ty orelse {
                    executableInvariant("executable already-erased capture value has no hidden capture type key");
                };
                const capture_info = value_store.values.items[@intFromEnum(capture_value)];
                if (builtin.mode == .Debug) {
                    const actual_key = try repr.execValueTypeKeyForValue(
                        self.allocator,
                        self.canonical_names,
                        self.type_lowerer.input,
                        representation_store,
                        value_store,
                        capture_value,
                    );
                    if (!repr.canonicalExecValueTypeKeyEql(actual_key, capture_key)) {
                        executableInvariant("executable already-erased capture value type disagrees with signature key");
                    }
                }
                break :blk try self.lowerExecutableValueTypeInStore(
                    capture_info.logical_ty,
                    capture_value,
                    value_store,
                    representation_store,
                );
            },
        };
    }

    fn lowerProcValueErasedCaptureType(
        self: *BodyBuilder,
        callable: repr.CallableValueInfo,
        erase: repr.ProcValueErasePlan,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!?Type.TypeId {
        if (erase.erased_fn_sig_key.capture_ty == null) {
            if (erase.capture_slots.len != 0) executableInvariant("executable proc-value erase plan has captures but no hidden capture type");
            return null;
        }

        const source = switch (callable.source) {
            .proc_value => |source| source,
            else => executableInvariant("executable proc-value erase plan is attached to a non-proc callable source"),
        };
        if (source.captures.len != erase.capture_slots.len) {
            executableInvariant("executable proc-value erase plan capture slots disagree with callable source captures");
        }

        const items: []Type.TypeId = if (erase.capture_slots.len == 0)
            &.{}
        else
            try self.allocator.alloc(Type.TypeId, erase.capture_slots.len);
        errdefer if (items.len > 0) self.allocator.free(items);

        const seen: []bool = if (erase.capture_slots.len == 0)
            &.{}
        else
            try self.allocator.alloc(bool, erase.capture_slots.len);
        defer if (seen.len > 0) self.allocator.free(seen);
        if (seen.len > 0) @memset(seen, false);

        for (erase.capture_slots) |slot| {
            const index: usize = @intCast(slot.slot);
            if (index >= source.captures.len) executableInvariant("executable proc-value erase capture slot exceeds source capture arity");
            if (seen[index]) executableInvariant("executable proc-value erase capture slot was duplicated");
            const capture = source.captures[index];
            const capture_info = value_store.values.items[@intFromEnum(capture)];
            items[index] = try self.lowerExecutableValueTypeInStore(
                capture_info.logical_ty,
                capture,
                value_store,
                representation_store,
            );
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable proc-value erase plan did not provide every hidden capture slot");
        }

        return try self.type_lowerer.output.addType(.{ .tuple = items });
    }

    fn lowerFiniteSetAdapterCaptureType(
        self: *BodyBuilder,
        adapter: repr.ErasedAdapterKey,
    ) Allocator.Error!?Type.TypeId {
        if (adapter.erased_fn_sig_key.capture_ty == null) return null;
        return try self.lowerFiniteCallableSetType(adapter.callable_set_key, self.representation_store);
    }

    fn lowerAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        aggregate: repr.AggregateValueInfo,
    ) Allocator.Error!Type.TypeId {
        return switch (aggregate) {
            .record => |record| blk: {
                const fields = try self.allocator.alloc(Type.RecordFieldType, record.fields.len);
                errdefer if (fields.len > 0) self.allocator.free(fields);
                for (record.fields, 0..) |field, i| {
                    const child = self.value_store.values.items[@intFromEnum(field.value)];
                    fields[i] = .{
                        .field = field.field,
                        .ty = try self.lowerExecutableValueType(child.logical_ty, field.value),
                    };
                }
                break :blk try self.type_lowerer.output.addType(.{ .record = .{
                    .shape = record.shape,
                    .fields = fields,
                } });
            },
            .tuple => |tuple| blk: {
                const items = try self.allocator.alloc(Type.TypeId, tuple.len);
                errdefer if (items.len > 0) self.allocator.free(items);
                const seen = try self.allocator.alloc(bool, tuple.len);
                defer self.allocator.free(seen);
                @memset(seen, false);

                for (tuple) |elem| {
                    const index: usize = @intCast(elem.index);
                    if (index >= tuple.len) executableInvariant("executable aggregate tuple element index exceeded tuple arity");
                    if (seen[index]) executableInvariant("executable aggregate tuple had duplicate element index");
                    const child = self.value_store.values.items[@intFromEnum(elem.value)];
                    items[index] = try self.lowerExecutableValueType(child.logical_ty, elem.value);
                    seen[index] = true;
                }
                for (seen) |was_seen| {
                    if (!was_seen) executableInvariant("executable aggregate tuple did not provide every element");
                }

                break :blk try self.type_lowerer.output.addType(.{ .tuple = items });
            },
            .tag => |tag| try self.lowerTagAggregateExecutableValueType(logical_ty, tag),
            .list => |list| try self.lowerListAggregateExecutableValueType(logical_ty, list),
        };
    }

    fn lowerListAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        list: anytype,
    ) Allocator.Error!Type.TypeId {
        if (list.elems.len == 0) {
            const elem_ty = try self.lowerLogicalListElemType(logical_ty);
            return try self.type_lowerer.output.addType(.{ .list = elem_ty });
        }

        const first = self.value_store.values.items[@intFromEnum(list.elems[0])];
        if (builtin.mode == .Debug) {
            const first_key = try repr.execValueTypeKeyForValue(
                self.allocator,
                self.canonical_names,
                self.type_lowerer.input,
                self.representation_store,
                self.value_store,
                list.elems[0],
            );
            for (list.elems[1..]) |elem| {
                const elem_key = try repr.execValueTypeKeyForValue(
                    self.allocator,
                    self.canonical_names,
                    self.type_lowerer.input,
                    self.representation_store,
                    self.value_store,
                    elem,
                );
                if (!repr.canonicalExecValueTypeKeyEql(first_key, elem_key)) {
                    executableInvariant("executable aggregate list elements have different executable representations");
                }
            }
        }
        const elem_ty = try self.lowerExecutableValueType(first.logical_ty, list.elems[0]);
        return try self.type_lowerer.output.addType(.{ .list = elem_ty });
    }

    fn lowerLogicalListElemType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
    ) Allocator.Error!Type.TypeId {
        const root = self.type_lowerer.input.unlinkConst(logical_ty);
        return switch (self.type_lowerer.input.getNode(root)) {
            .link => unreachable,
            .nominal => |nominal| try self.lowerLogicalListElemType(nominal.backing),
            .content => |content| switch (content) {
                .list => |elem| try self.type_lowerer.lowerType(elem),
                else => executableInvariant("executable aggregate list metadata attached to non-list logical type"),
            },
            else => executableInvariant("executable aggregate list metadata attached to unresolved logical type"),
        };
    }

    fn lowerTagAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        tag: anytype,
    ) Allocator.Error!Type.TypeId {
        const source_tags = self.logicalTagUnionTags(logical_ty);
        const shape_tags = self.type_lowerer.row_shapes.tagUnionTags(tag.union_shape);
        if (shape_tags.len != source_tags.len) {
            executableInvariant("executable aggregate tag metadata shape does not match logical type arity");
        }

        const tags = try self.allocator.alloc(Type.TagType, shape_tags.len);
        for (tags) |*item| item.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (tags) |item| {
                if (item.payloads.len > 0) self.allocator.free(item.payloads);
            }
            self.allocator.free(tags);
        }

        const seen_tags = try self.allocator.alloc(bool, shape_tags.len);
        defer self.allocator.free(seen_tags);
        @memset(seen_tags, false);

        for (shape_tags) |shape_tag| {
            const shape_tag_info = self.type_lowerer.row_shapes.tag(shape_tag);
            const tag_index: usize = @intCast(shape_tag_info.logical_index);
            if (tag_index >= source_tags.len) {
                executableInvariant("executable aggregate tag logical index exceeded logical type arity");
            }
            if (seen_tags[tag_index]) {
                executableInvariant("executable aggregate tag metadata saw duplicate tag logical index");
            }

            const payloads = if (shape_tag == tag.tag)
                try self.lowerSelectedTagPayloadTypes(tag)
            else
                try self.lowerLogicalTagPayloadTypes(shape_tag, source_tags[tag_index]);
            tags[tag_index] = .{
                .tag = shape_tag,
                .payloads = payloads,
            };
            seen_tags[tag_index] = true;
        }
        for (seen_tags) |was_seen| {
            if (!was_seen) executableInvariant("executable aggregate tag metadata did not provide every logical tag");
        }

        return try self.type_lowerer.output.addType(.{ .tag_union = .{
            .shape = tag.union_shape,
            .tags = tags,
        } });
    }

    fn logicalTagUnionTags(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
    ) []const LambdaSolved.Type.Tag {
        const root = self.type_lowerer.input.unlinkConst(logical_ty);
        return switch (self.type_lowerer.input.getNode(root)) {
            .link => unreachable,
            .nominal => |nominal| self.logicalTagUnionTags(nominal.backing),
            .content => |content| switch (content) {
                .tag_union => |tag_union| self.type_lowerer.input.sliceTags(tag_union.tags),
                else => executableInvariant("executable aggregate tag metadata attached to non-tag-union logical type"),
            },
            else => executableInvariant("executable aggregate tag metadata attached to unresolved logical type"),
        };
    }

    fn lowerLogicalTagPayloadTypes(
        self: *BodyBuilder,
        tag_id: MonoRow.TagId,
        source_tag: LambdaSolved.Type.Tag,
    ) Allocator.Error![]const Type.TagPayloadType {
        const source_payload_tys = self.type_lowerer.input.sliceTypeVarSpan(source_tag.args);
        const shape_payloads = self.type_lowerer.row_shapes.tagPayloads(tag_id);
        if (shape_payloads.len != source_payload_tys.len) {
            executableInvariant("executable aggregate tag logical payload arity differs from row shape");
        }
        if (shape_payloads.len == 0) return &.{};

        const payloads = try self.allocator.alloc(Type.TagPayloadType, shape_payloads.len);
        errdefer self.allocator.free(payloads);
        for (shape_payloads, 0..) |payload_id, i| {
            payloads[i] = .{
                .payload = payload_id,
                .ty = try self.type_lowerer.lowerType(source_payload_tys[i]),
            };
        }
        return payloads;
    }

    fn lowerSelectedTagPayloadTypes(
        self: *BodyBuilder,
        tag: anytype,
    ) Allocator.Error![]const Type.TagPayloadType {
        const shape_payloads = self.type_lowerer.row_shapes.tagPayloads(tag.tag);
        if (shape_payloads.len == 0) return &.{};
        const payloads = try self.allocator.alloc(Type.TagPayloadType, shape_payloads.len);
        errdefer self.allocator.free(payloads);
        const seen = try self.allocator.alloc(bool, shape_payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (tag.payloads) |payload| {
            const payload_info = self.type_lowerer.row_shapes.tagPayload(payload.payload);
            if (payload_info.tag != tag.tag) {
                executableInvariant("executable aggregate selected tag payload belongs to a different tag");
            }
            const payload_index: usize = @intCast(payload_info.logical_index);
            if (payload_index >= shape_payloads.len) {
                executableInvariant("executable aggregate selected tag payload index exceeded tag arity");
            }
            if (seen[payload_index]) {
                executableInvariant("executable aggregate selected tag payload was duplicated");
            }
            if (payload.payload != shape_payloads[payload_index]) {
                executableInvariant("executable aggregate selected tag payload id differs from row shape logical slot");
            }
            const child = self.value_store.values.items[@intFromEnum(payload.value)];
            payloads[payload_index] = .{
                .payload = shape_payloads[payload_index],
                .ty = try self.lowerExecutableValueType(child.logical_ty, payload.value),
            };
            seen[payload_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable aggregate selected tag did not provide every payload");
        }
        return payloads;
    }

    fn lowerFnArgSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TypedSymbol)) Allocator.Error!Ast.Span(Ast.TypedValue) {
        const capture_arg = try self.lowerCaptureRecordArg();
        self.capture_record_arg = capture_arg;

        const input_items = self.input.typed_symbols.items[span.start..][0..span.len];
        const capture_arg_len: usize = if (capture_arg != null) 1 else 0;
        const total_len = input_items.len + capture_arg_len;
        if (total_len == 0) return Ast.Span(Ast.TypedValue).empty();

        const output_items = try self.allocator.alloc(Ast.TypedValue, total_len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |param, i| {
            const value = self.output.freshValueRef();
            try self.env.put(param.binding_info, value);
            const binding = self.value_store.bindings.items[@intFromEnum(param.binding_info)];
            output_items[i] = .{
                .ty = try self.lowerExecutableValueType(param.ty, binding.value),
                .value = value,
            };
        }
        if (capture_arg) |arg| {
            output_items[input_items.len] = arg;
        }
        return try self.output.addTypedValueSpan(output_items);
    }

    fn lowerParamSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TypedSymbol)) Allocator.Error!Ast.Span(Ast.TypedValue) {
        if (span.len == 0) return Ast.Span(Ast.TypedValue).empty();
        const input_items = self.input.typed_symbols.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.TypedValue, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |param, i| {
            const value = self.output.freshValueRef();
            try self.env.put(param.binding_info, value);
            const binding = self.value_store.bindings.items[@intFromEnum(param.binding_info)];
            output_items[i] = .{
                .ty = try self.lowerExecutableValueType(param.ty, binding.value),
                .value = value,
            };
        }
        return try self.output.addTypedValueSpan(output_items);
    }

    fn lowerCaptureRecordArg(self: *BodyBuilder) Allocator.Error!?Ast.TypedValue {
        const captures = self.value_store.sliceValueSpan(self.proc_instance.public_roots.captures);
        if (captures.len == 0) return null;

        const capture_tys = try self.allocator.alloc(Type.TypeId, captures.len);
        errdefer self.allocator.free(capture_tys);
        for (captures, 0..) |capture, i| {
            const info = self.value_store.values.items[@intFromEnum(capture)];
            capture_tys[i] = try self.lowerExecutableValueType(info.logical_ty, capture);
        }

        const capture_record_ty = try self.type_lowerer.output.addType(.{ .tuple = capture_tys });
        return .{
            .ty = capture_record_ty,
            .value = self.output.freshValueRef(),
        };
    }

    fn lowerExpr(self: *BodyBuilder, expr_id: LambdaSolved.Ast.ExprId) Allocator.Error!Ast.ExprId {
        if (self.expr_map.get(expr_id)) |existing| return existing;

        const expr = self.input.exprs.items[@intFromEnum(expr_id)];
        const lowered = switch (expr.data) {
            .var_ => |var_| blk: {
                const value = self.env.get(var_.binding_info) orelse executableInvariant("executable variable occurrence has no lowered binding value");
                const binding = self.value_store.bindings.items[@intFromEnum(var_.binding_info)];
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, binding.value),
                    value,
                    .{ .value_ref = value },
                );
            },
            .int_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .int_lit = literal }),
            .frac_f32_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .frac_f32_lit = literal }),
            .frac_f64_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .frac_f64_lit = literal }),
            .dec_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .dec_lit = literal }),
            .str_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .str_lit = literal }),
            .bool_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .bool_lit = literal }),
            .unit => try self.addValueExpr(expr.ty, expr.value_info, .unit),
            .const_instance => |const_instance| try self.addValueExpr(expr.ty, expr.value_info, .{ .const_instance = const_instance }),
            .record => |record| blk: {
                const fields = try self.lowerRecordFields(record.assembly_order);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .record = .{
                        .shape = record.shape,
                        .fields = fields,
                    } },
                );
            },
            .nominal_reinterpret => |backing| blk: {
                const lowered_backing = try self.lowerExpr(backing);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .nominal_reinterpret = lowered_backing },
                );
            },
            .tag => |tag| blk: {
                const payloads = try self.lowerTagPayloadValues(tag.assembly_order);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .tag = .{
                        .union_shape = tag.union_shape,
                        .tag = tag.tag,
                        .payloads = payloads,
                    } },
                );
            },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .access = .{
                        .record = record,
                        .field = access.field,
                    } },
                );
            },
            .let_ => |let_| blk: {
                const body = try self.lowerExpr(let_.body);
                const previous = try self.env.fetchPut(let_.bind.binding_info, self.exprValue(body));
                defer {
                    if (previous) |entry| {
                        self.env.put(let_.bind.binding_info, entry.value) catch unreachable;
                    } else {
                        _ = self.env.remove(let_.bind.binding_info);
                    }
                }
                const rest = try self.lowerExpr(let_.rest);
                const stmt = try self.output.addStmt(.{ .decl = .{
                    .value = self.exprValue(body),
                    .body = body,
                } });
                const stmt_span = try self.output.addStmtSpan(&.{stmt});
                break :blk try self.output.addExpr(
                    self.output.getExpr(rest).ty,
                    self.exprValue(rest),
                    .{ .block = .{
                        .stmts = stmt_span,
                        .final_expr = rest,
                    } },
                );
            },
            .block => |block| blk: {
                const stmts = try self.lowerStmtSpan(block.stmts);
                const final_expr = try self.lowerExpr(block.final_expr);
                break :blk try self.output.addExpr(
                    self.output.getExpr(final_expr).ty,
                    self.exprValue(final_expr),
                    .{ .block = .{
                        .stmts = stmts,
                        .final_expr = final_expr,
                    } },
                );
            },
            .tuple => |items| blk: {
                const items_span = try self.lowerExprIds(items);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .tuple = items_span },
                );
            },
            .list => |items| blk: {
                const items_span = try self.lowerExprIds(items);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .list = items_span },
                );
            },
            .tag_payload => |payload| blk: {
                const tag_union = try self.lowerExpr(payload.tag_union);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .tag_payload = .{
                        .tag_union = tag_union,
                        .payload = payload.payload,
                    } },
                );
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .tuple_access = .{
                        .tuple = tuple,
                        .elem_index = access.elem_index,
                    } },
                );
            },
            .structural_eq => |eq| blk: {
                const lhs = try self.lowerExpr(eq.lhs);
                const rhs = try self.lowerExpr(eq.rhs);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .structural_eq = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } },
                );
            },
            .low_level => |low_level| blk: {
                const args = try self.lowerExprIds(low_level.args);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .low_level = .{
                        .op = low_level.op,
                        .args = args,
                    } },
                );
            },
            .return_ => |child| blk: {
                const lowered_child = try self.lowerExpr(child);
                break :blk try self.output.addExpr(
                    self.output.getExpr(lowered_child).ty,
                    self.exprValue(lowered_child),
                    .{ .return_ = lowered_child },
                );
            },
            .bool_not => |child| blk: {
                const lowered_child = try self.lowerExpr(child);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .bool_not = lowered_child },
                );
            },
            .crash => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .crash = literal }),
            .runtime_error => try self.addValueExpr(expr.ty, expr.value_info, .runtime_error),
            .match_ => |match_| blk: {
                _ = match_.join_info;
                const cond = try self.lowerExpr(match_.cond);
                const scrutinee_exprs = [_]Ast.ExprId{cond};
                const scrutinees = [_]Ast.ExecutableValueRef{self.exprValue(cond)};
                const lowered_branches = try self.lowerBranchSpan(match_.branches);
                const scrutinee_expr_span = try self.output.addExprSpan(&scrutinee_exprs);
                const scrutinee_span = try self.output.addValueRefSpan(&scrutinees);
                const decision_plan = try self.output.addPatternDecisionPlan(.{
                    .scrutinees = scrutinee_span,
                    .branches = lowered_branches,
                });
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .source_match = .{
                        .scrutinee_exprs = scrutinee_expr_span,
                        .scrutinees = scrutinee_span,
                        .decision_plan = decision_plan,
                        .branches = lowered_branches,
                    } },
                );
            },
            .if_ => |if_| blk: {
                _ = if_.join_info;
                const cond = try self.lowerExpr(if_.cond);
                const then_body = try self.lowerExpr(if_.then_body);
                const else_body = try self.lowerExpr(if_.else_body);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .if_ = .{
                        .cond = cond,
                        .then_body = then_body,
                        .else_body = else_body,
                    } },
                );
            },
            .for_ => |for_| try self.lowerForExpr(expr.ty, expr.value_info, for_),
            .capture_ref => |slot| try self.lowerCaptureRef(expr.ty, slot),
            .call_value => |call| try self.lowerCallValue(expr.ty, call),
            .call_proc => |call| try self.lowerCallProc(expr.ty, call),
            .proc_value => |proc_value| try self.lowerProcValue(expr.ty, expr.value_info, proc_value),
            .inspect => |child| blk: {
                const value = try self.lowerExpr(child);
                const debug_stmt = try self.output.addStmt(.{ .debug = value });
                const stmts = try self.output.addStmtSpan(&.{debug_stmt});
                break :blk try self.output.addExpr(
                    self.output.getExpr(value).ty,
                    self.exprValue(value),
                    .{ .block = .{
                        .stmts = stmts,
                        .final_expr = value,
                    } },
                );
            },
        };
        try self.expr_map.put(expr_id, lowered);
        return lowered;
    }

    const SavedBinding = struct {
        binding: repr.BindingInfoId,
        previous: ?Ast.ExecutableValueRef,
    };

    fn lowerPatScoped(
        self: *BodyBuilder,
        pat_id: LambdaSolved.Ast.PatId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.pats.items[@intFromEnum(pat_id)];
        const ty = try self.type_lowerer.lowerType(pat.ty);
        return try self.lowerPatScopedWithType(pat_id, ty, saved);
    }

    fn lowerPatScopedWithType(
        self: *BodyBuilder,
        pat_id: LambdaSolved.Ast.PatId,
        ty: Type.TypeId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.pats.items[@intFromEnum(pat_id)];
        return try self.output.addPat(.{ .ty = ty, .data = switch (pat.data) {
            .bool_lit => |literal| .{ .bool_lit = literal },
            .int_lit => |literal| .{ .int_lit = literal },
            .frac_f32_lit => |literal| .{ .frac_f32_lit = literal },
            .frac_f64_lit => |literal| .{ .frac_f64_lit = literal },
            .dec_lit => |literal| .{ .dec_lit = literal },
            .str_lit => |literal| .{ .str_lit = literal },
            .wildcard => .wildcard,
            .nominal => |child| .{ .nominal = try self.lowerPatScoped(child, saved) },
            .tuple => |items| .{ .tuple = try self.lowerPatSpanScoped(items, saved) },
            .record => |record| .{ .record = .{
                .shape = record.shape,
                .fields = try self.lowerRecordFieldPatternSpanScoped(record.fields, saved),
                .rest = if (record.rest) |rest| try self.lowerPatScoped(rest, saved) else null,
            } },
            .list => |list| .{ .list = .{
                .items = try self.lowerPatSpanScoped(list.items, saved),
                .rest = if (list.rest) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern) |pattern| try self.lowerPatScoped(pattern, saved) else null,
                } else null,
            } },
            .as => |as| blk: {
                const value = try self.bindPatternValue(as.binding_info, saved);
                break :blk .{ .as = .{
                    .pattern = try self.lowerPatScopedWithType(as.pattern, ty, saved),
                    .bind = value,
                } };
            },
            .var_ => |var_| blk: {
                const value = try self.bindPatternValue(var_.binding_info, saved);
                break :blk .{ .bind = value };
            },
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .payloads = try self.lowerTagPayloadPatternSpan(tag.payloads, saved),
            } },
        } });
    }

    fn bindPatternValue(
        self: *BodyBuilder,
        binding: repr.BindingInfoId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.ExecutableValueRef {
        const value = self.output.freshValueRef();
        const previous = try self.env.fetchPut(binding, value);
        try saved.append(self.allocator, .{
            .binding = binding,
            .previous = if (previous) |entry| entry.value else null,
        });
        return value;
    }

    fn lowerPatSpanScoped(
        self: *BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.PatId),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.PatId) {
        if (span.len == 0) return Ast.Span(Ast.PatId).empty();
        const input_items = self.input.pat_ids.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.PatId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |item, i| {
            output_items[i] = try self.lowerPatScoped(item, saved);
        }
        return try self.output.addPatSpan(output_items);
    }

    fn lowerRecordFieldPatternSpanScoped(
        self: *BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.RecordFieldPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.RecordFieldPattern) {
        if (span.len == 0) return Ast.Span(Ast.RecordFieldPattern).empty();
        const input_items = self.input.record_field_patterns.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.RecordFieldPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .pattern = try self.lowerPatScoped(field.pattern, saved),
            };
        }
        return try self.output.addRecordFieldPatternSpan(output_items);
    }

    fn restoreBindings(self: *BodyBuilder, saved: *std.ArrayList(SavedBinding), start: usize) void {
        while (saved.items.len > start) {
            const binding = saved.pop().?;
            if (binding.previous) |previous| {
                self.env.put(binding.binding, previous) catch unreachable;
            } else {
                _ = self.env.remove(binding.binding);
            }
        }
    }

    fn lowerBranch(self: *BodyBuilder, branch_id: LambdaSolved.Ast.BranchId) Allocator.Error!Ast.BranchId {
        const branch = self.input.branches.items[@intFromEnum(branch_id)];
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const pat = try self.lowerPatScoped(branch.pat, &saved);
        defer self.restoreBindings(&saved, 0);
        const body = try self.lowerExpr(branch.body);
        return try self.output.addBranch(.{
            .pat = pat,
            .body = body,
        });
    }

    fn lowerCaptureRef(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        slot: u32,
    ) Allocator.Error!Ast.ExprId {
        const capture_arg = self.capture_record_arg orelse executableInvariant("executable capture_ref reached procedure without capture record argument");
        const captures = self.value_store.sliceValueSpan(self.proc_instance.public_roots.captures);
        const capture_index: usize = @intCast(slot);
        if (capture_index >= captures.len) executableInvariant("executable capture_ref slot does not exist in procedure capture roots");

        const capture_record = try self.output.addExpr(
            capture_arg.ty,
            capture_arg.value,
            .{ .value_ref = capture_arg.value },
        );
        const capture_value = captures[capture_index];
        return try self.output.addExpr(
            try self.lowerExecutableValueType(source_ty, capture_value),
            self.output.freshValueRef(),
            .{ .tuple_access = .{
                .tuple = capture_record,
                .elem_index = slot,
            } },
        );
    }

    fn lowerForExpr(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        for_: anytype,
    ) Allocator.Error!Ast.ExprId {
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const patt = try self.lowerPatScoped(for_.patt, &saved);
        defer self.restoreBindings(&saved, 0);
        return try self.output.addExpr(
            try self.lowerExecutableValueType(source_ty, value_info_id),
            self.output.freshValueRef(),
            .{ .for_ = .{
                .patt = patt,
                .iterable = try self.lowerExpr(for_.iterable),
                .body = try self.lowerExpr(for_.body),
            } },
        );
    }

    fn lowerStmt(self: *BodyBuilder, stmt_id: LambdaSolved.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.stmts.items[@intFromEnum(stmt_id)];
        return try self.output.addStmt(switch (stmt) {
            .decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                try self.env.put(decl.bind.binding_info, self.exprValue(body));
                break :blk .{ .decl = .{
                    .value = self.exprValue(body),
                    .body = body,
                } };
            },
            .var_decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                try self.env.put(decl.bind.binding_info, self.exprValue(body));
                break :blk .{ .decl = .{
                    .value = self.exprValue(body),
                    .body = body,
                } };
            },
            .reassign => |reassign| blk: {
                const body = try self.lowerExpr(reassign.body);
                const target = self.env.get(reassign.version) orelse executableInvariant("executable reassignment target has no lowered binding value");
                break :blk .{ .reassign = .{
                    .target = target,
                    .body = body,
                } };
            },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .debug => |expr| .{ .debug = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .crash => |literal| .{ .crash = literal },
            .return_ => |expr| blk: {
                const body = try self.lowerExpr(expr);
                break :blk .{ .return_ = self.exprValue(body) };
            },
            .break_ => .break_,
            .for_ => |for_| blk: {
                var saved = std.ArrayList(SavedBinding).empty;
                defer saved.deinit(self.allocator);
                const patt = try self.lowerPatScoped(for_.patt, &saved);
                defer self.restoreBindings(&saved, 0);
                break :blk .{ .for_ = .{
                    .patt = patt,
                    .iterable = try self.lowerExpr(for_.iterable),
                    .body = try self.lowerExpr(for_.body),
                } };
            },
            .while_ => |while_| .{ .while_ = .{
                .cond = try self.lowerExpr(while_.cond),
                .body = try self.lowerExpr(while_.body),
            } },
        });
    }

    fn lowerStmtSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        if (span.len == 0) return Ast.Span(Ast.StmtId).empty();
        const input_items = self.input.stmt_ids.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |stmt, i| {
            output_items[i] = try self.lowerStmt(stmt);
        }
        return try self.output.addStmtSpan(output_items);
    }

    fn lowerExprIds(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        if (span.len == 0) return Ast.Span(Ast.ExprId).empty();
        const input_items = self.input.expr_ids.items[span.start..][0..span.len];
        const exprs = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(exprs);
        for (input_items, 0..) |expr, i| {
            exprs[i] = try self.lowerExpr(expr);
        }
        return try self.output.addExprSpan(exprs);
    }

    fn lowerBranchSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.BranchId)) Allocator.Error!Ast.Span(Ast.BranchId) {
        if (span.len == 0) return Ast.Span(Ast.BranchId).empty();
        const input_items = self.input.branch_ids.items[span.start..][0..span.len];
        const branches = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(branches);
        for (input_items, 0..) |branch, i| {
            branches[i] = try self.lowerBranch(branch);
        }
        return try self.output.addBranchSpan(branches);
    }

    fn lowerTagPayloadPatternSpan(
        self: *BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        if (span.len == 0) return Ast.Span(Ast.TagPayloadPattern).empty();
        const input_items = self.input.tag_payload_patterns.items[span.start..][0..span.len];
        const payloads = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(payloads);
        for (input_items, 0..) |payload, i| {
            payloads[i] = .{
                .payload = payload.payload,
                .pattern = try self.lowerPatScoped(payload.pattern, saved),
            };
        }
        return try self.output.addTagPayloadPatternSpan(payloads);
    }

    fn lowerRecordFields(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.RecordFieldAssembly)) Allocator.Error!Ast.Span(Ast.RecordFieldExpr) {
        if (span.len == 0) return Ast.Span(Ast.RecordFieldExpr).empty();
        const input_items = self.input.record_field_assemblies.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.RecordFieldExpr, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |field, i| {
            const lowered = try self.lowerExpr(field.value);
            values[i] = .{
                .field = field.field,
                .expr = lowered,
                .ty = self.output.getExpr(lowered).ty,
                .value = self.exprValue(lowered),
            };
        }
        return try self.output.addRecordFieldExprSpan(values);
    }

    fn lowerTagPayloadValues(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadAssembly)) Allocator.Error!Ast.Span(Ast.TagPayloadExpr) {
        if (span.len == 0) return Ast.Span(Ast.TagPayloadExpr).empty();
        const input_items = self.input.tag_payload_assemblies.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.TagPayloadExpr, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |payload, i| {
            const lowered = try self.lowerExpr(payload.value);
            values[i] = .{
                .payload = payload.payload,
                .expr = lowered,
                .ty = self.output.getExpr(lowered).ty,
                .value = self.exprValue(lowered),
            };
        }
        return try self.output.addTagPayloadExprSpan(values);
    }

    fn lowerCallProc(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        call: anytype,
    ) Allocator.Error!Ast.ExprId {
        const target_proc = self.proc_map.get(call.proc) orelse executableInvariant("executable call_proc target was not reserved before body lowering");
        const target_instance_id = self.proc_instance_map.get(call.proc) orelse executableInvariant("executable call_proc target has no representation instance");
        const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];

        const arg_items = self.input.expr_ids.items[call.args.start..][0..call.args.len];
        const direct_args = try self.allocator.alloc(Ast.DirectCallArg, arg_items.len);
        defer self.allocator.free(direct_args);
        const stmt_ids = try self.allocator.alloc(Ast.StmtId, arg_items.len);
        defer self.allocator.free(stmt_ids);

        for (arg_items, 0..) |arg, i| {
            const lowered = try self.lowerExpr(arg);
            const value = self.exprValue(lowered);
            direct_args[i] = .{ .value = value };
            stmt_ids[i] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }

        const call_site = self.value_store.call_sites.items[@intFromEnum(call.call_site)];
        if (!repr.canonicalTypeKeyEql(call_site.requested_source_fn_ty, call.requested_source_fn_ty)) {
            executableInvariant("executable call_proc call-site requested source type differs from expression");
        }
        if (!repr.canonicalTypeKeyEql(target_instance.executable_specialization_key.requested_fn_ty, call_site.requested_source_fn_ty)) {
            executableInvariant("executable call_proc target specialization source type differs from call site");
        }
        const result_ty = try self.lowerExecutableValueType(source_ty, call_site.result);
        const result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(result_ty, result_value, .{ .call_direct = .{
            .source = call.proc.proc,
            .executable_specialization_key = try repr.cloneExecutableSpecializationKey(self.allocator, target_instance.executable_specialization_key),
            .executable_proc = target_proc,
            .direct_args = try self.output.addDirectCallArgSpan(direct_args),
            .result_bridge = null,
        } });

        if (stmt_ids.len == 0) return final_call;

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids),
            .final_expr = final_call,
        } });
    }

    fn lowerProcValue(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        proc_value: anytype,
    ) Allocator.Error!Ast.ExprId {
        const value_info = self.value_store.values.items[@intFromEnum(value_info_id)];
        const callable = value_info.callable orelse executableInvariant("executable proc_value reached value without callable metadata");
        const emission = self.representation_store.callableEmissionPlan(callable.emission_plan);
        switch (emission) {
            .finite => {},
            .erase_proc_value => |erase| return try self.lowerProcValueErased(source_ty, value_info_id, callable, proc_value, erase),
            .already_erased,
            .erase_finite_set,
            => executableInvariant("executable proc_value reached erased emission that is not a proc-value erase plan"),
        }

        const construction_id = callable.construction_plan orelse executableInvariant("executable proc_value reached finite callable value without construction metadata");
        const construction = self.representation_store.callableConstructionPlan(construction_id);
        if (construction.result != value_info_id) {
            executableInvariant("executable proc_value construction plan is attached to the wrong value");
        }
        const emission_key = switch (emission) {
            .finite => |key| key,
            else => executableInvariant("executable proc_value construction plan does not have finite callable emission"),
        };
        if (!repr.callableSetKeyEql(emission_key, construction.callable_set_key)) {
            executableInvariant("executable proc_value construction key differs from finite emission key");
        }
        const member = self.representation_store.callableSetMember(construction.callable_set_key, construction.selected_member) orelse {
            executableInvariant("executable proc_value construction selected a missing callable-set member");
        };
        if (!repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, construction.source_fn_ty)) {
            executableInvariant("executable proc_value construction source function type differs from descriptor member");
        }
        if (member.capture_slots.len != construction.capture_values.len) {
            executableInvariant("executable proc_value construction capture count differs from descriptor member");
        }
        const capture_items = self.input.capture_args.items[proc_value.captures.start..][0..proc_value.captures.len];
        if (capture_items.len != construction.capture_values.len) {
            executableInvariant("executable proc_value capture arity does not match construction plan");
        }
        const capture_refs = try self.allocator.alloc(Ast.CaptureValueRef, capture_items.len);
        defer self.allocator.free(capture_refs);
        const stmt_ids = try self.allocator.alloc(Ast.StmtId, capture_items.len);
        defer self.allocator.free(stmt_ids);

        for (capture_items, 0..) |capture, i| {
            if (member.capture_slots[i].slot != capture.slot) {
                executableInvariant("executable proc_value capture slot differs from construction member schema");
            }
            if (capture.value_info != construction.capture_values[i]) {
                executableInvariant("executable proc_value capture value differs from construction plan");
            }
            const lowered = try self.lowerExpr(capture.expr);
            const value = self.exprValue(lowered);
            capture_refs[i] = .{
                .slot = capture.slot,
                .value = value,
                .exec_ty = self.output.getExpr(lowered).ty,
            };
            stmt_ids[i] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }

        const result_ty = try self.lowerExecutableValueType(source_ty, value_info_id);
        const result_value = self.output.freshValueRef();
        const final_value = try self.output.addExpr(result_ty, result_value, .{ .callable_set_value = .{
            .construction_plan = construction_id,
            .callable_set_key = construction.callable_set_key,
            .member = .{
                .callable_set_key = construction.callable_set_key,
                .member_index = construction.selected_member,
            },
            .capture_record = if (capture_refs.len == 0) null else .{
                .capture_shape_key = member.capture_shape_key,
                .values = try self.output.addCaptureValueRefSpan(capture_refs),
                .record_tmp = self.output.freshValueRef(),
            },
        } });

        if (stmt_ids.len == 0) return final_value;

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids),
            .final_expr = final_value,
        } });
    }

    fn lowerProcValueErased(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        callable: repr.CallableValueInfo,
        proc_value: anytype,
        erase: repr.ProcValueErasePlan,
    ) Allocator.Error!Ast.ExprId {
        if (erase.source != value_info_id) {
            executableInvariant("executable proc-value erase plan is attached to the wrong value");
        }
        if (!canonical.procedureCallableRefEql(erase.proc_value, proc_value.proc.callable)) {
            executableInvariant("executable proc-value erase plan procedure differs from proc_value expression");
        }
        const source = switch (callable.source) {
            .proc_value => |source| source,
            else => executableInvariant("executable proc-value erase plan is attached to a non-proc callable source"),
        };
        if (!canonical.mirProcedureRefEql(source.proc, proc_value.proc)) {
            executableInvariant("executable proc-value erase source procedure differs from proc_value expression");
        }
        if (!repr.canonicalTypeKeyEql(source.fn_ty, erase.proc_value.source_fn_ty)) {
            executableInvariant("executable proc-value erase source function type differs from erase plan");
        }
        if (source.captures.len != erase.capture_slots.len) {
            executableInvariant("executable proc-value erase source capture count differs from erase plan");
        }

        const capture_items = self.input.capture_args.items[proc_value.captures.start..][0..proc_value.captures.len];
        if (capture_items.len != erase.capture_slots.len) {
            executableInvariant("executable proc-value erase capture arity does not match erase plan");
        }

        const result_ty = try self.lowerExecutableValueType(source_ty, value_info_id);
        const capture_ty = self.erasedFnCaptureType(result_ty, erase.erased_fn_sig_key);
        if ((capture_ty != null) != (erase.erased_fn_sig_key.capture_ty != null)) {
            executableInvariant("executable proc-value erase capture type disagrees with erased signature");
        }
        if (capture_ty == null and erase.capture_slots.len != 0) {
            executableInvariant("executable proc-value erase has captures but no hidden capture type");
        }

        const selected_executable_proc = self.executableProcForSpecializationKey(erase.executable_specialization_key);

        const lowered_captures: []Ast.ExprId = if (capture_items.len == 0)
            &.{}
        else
            try self.allocator.alloc(Ast.ExprId, capture_items.len);
        defer if (lowered_captures.len > 0) self.allocator.free(lowered_captures);
        const seen: []bool = if (capture_items.len == 0)
            &.{}
        else
            try self.allocator.alloc(bool, capture_items.len);
        defer if (seen.len > 0) self.allocator.free(seen);
        if (seen.len > 0) @memset(seen, false);

        const stmt_count = capture_items.len + (if (capture_ty != null) @as(usize, 1) else @as(usize, 0));
        const stmt_ids: []Ast.StmtId = if (stmt_count == 0)
            &.{}
        else
            try self.allocator.alloc(Ast.StmtId, stmt_count);
        defer if (stmt_ids.len > 0) self.allocator.free(stmt_ids);

        for (capture_items) |capture| {
            const slot_index: usize = @intCast(capture.slot);
            if (slot_index >= capture_items.len) executableInvariant("executable proc-value erase capture slot exceeded capture arity");
            if (seen[slot_index]) executableInvariant("executable proc-value erase capture slot was duplicated");
            if (erase.capture_slots[slot_index].slot != capture.slot) {
                executableInvariant("executable proc-value erase capture slot differs from erase plan");
            }
            if (capture.value_info != source.captures[slot_index]) {
                executableInvariant("executable proc-value erase capture value differs from callable source");
            }
            const lowered = try self.lowerExpr(capture.expr);
            const value = self.exprValue(lowered);
            lowered_captures[slot_index] = lowered;
            stmt_ids[slot_index] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
            seen[slot_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable proc-value erase plan did not provide every capture slot");
        }

        const capture_value: ?Ast.ExecutableValueRef = if (capture_ty) |ty| blk: {
            const capture_expr = if (lowered_captures.len == 0)
                try self.output.addExpr(ty, self.output.freshValueRef(), .unit)
            else
                try self.output.addExpr(ty, self.output.freshValueRef(), .{ .tuple = try self.output.addExprSpan(lowered_captures) });
            const value = self.exprValue(capture_expr);
            stmt_ids[capture_items.len] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = capture_expr,
            } });
            break :blk value;
        } else null;

        const result_value = self.output.freshValueRef();
        const final_value = try self.output.addExpr(result_ty, result_value, .{ .packed_erased_fn = .{
            .sig_key = erase.erased_fn_sig_key,
            .code = selected_executable_proc,
            .capture = capture_value,
            .capture_ty = capture_ty,
            .capture_shape = erase.capture_shape_key,
        } });

        if (stmt_ids.len == 0) return final_value;
        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids),
            .final_expr = final_value,
        } });
    }

    fn lowerCallValue(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        call: anytype,
    ) Allocator.Error!Ast.ExprId {
        const func = try self.lowerExpr(call.func);
        const func_value = self.exprValue(func);
        const call_site = self.value_store.call_sites.items[@intFromEnum(call.call_site)];
        if (!repr.canonicalTypeKeyEql(call_site.requested_source_fn_ty, call.requested_source_fn_ty)) {
            executableInvariant("executable call_value call-site requested source type differs from expression");
        }
        const callable_set_key = switch (call_site.dispatch orelse executableInvariant("executable call_value reached call site without resolved dispatch")) {
            .finite => |key| key,
            .erased => |sig_key| return try self.lowerCallValueErased(source_ty, call, func, func_value, call_site, sig_key),
        };
        const func_value_info_id = self.input.exprs.items[@intFromEnum(call.func)].value_info;
        const func_value_info = self.value_store.values.items[@intFromEnum(func_value_info_id)];
        const callable = func_value_info.callable orelse executableInvariant("executable call_value callee has no callable metadata");
        const emission = self.representation_store.callableEmissionPlan(callable.emission_plan);
        switch (emission) {
            .finite => |key| if (!repr.callableSetKeyEql(key, callable_set_key)) {
                executableInvariant("executable call_value call-site dispatch differs from callee finite emission");
            },
            .already_erased,
            .erase_proc_value,
            .erase_finite_set,
            => executableInvariant("executable call_value finite dispatch reached erased callee emission"),
        }
        const descriptor = self.representation_store.callableSetDescriptor(callable_set_key) orelse {
            executableInvariant("executable call_value finite callable set has no descriptor");
        };
        if (descriptor.members.len == 0) executableInvariant("executable call_value finite callable set has no members");

        const arg_items = self.input.expr_ids.items[call.args.start..][0..call.args.len];
        const arg_values = try self.allocator.alloc(Ast.ExecutableValueRef, arg_items.len);
        defer self.allocator.free(arg_values);
        const stmt_ids = try self.allocator.alloc(Ast.StmtId, arg_items.len + 1);
        defer self.allocator.free(stmt_ids);
        stmt_ids[0] = try self.output.addStmt(.{ .decl = .{
            .value = func_value,
            .body = func,
        } });
        for (arg_items, 0..) |arg, i| {
            const lowered = try self.lowerExpr(arg);
            const value = self.exprValue(lowered);
            arg_values[i] = value;
            stmt_ids[i + 1] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }

        const requested_source_fn_ty = call_site.requested_source_fn_ty;
        const branches = try self.allocator.alloc(Ast.CallableMatchBranch, descriptor.members.len);
        defer self.allocator.free(branches);
        for (descriptor.members, 0..) |member, i| {
            if (!repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, requested_source_fn_ty)) {
                executableInvariant("executable call_value callable-set member source type differs from call site");
            }
            const target = member.source_proc;
            const executable_proc = self.proc_map.get(target) orelse executableInvariant("executable call_value member target was not reserved");
            const target_instance_id = self.proc_instance_map.get(target) orelse executableInvariant("executable call_value member target has no representation instance");
            const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];
            if (!repr.canonicalTypeKeyEql(target_instance.executable_specialization_key.requested_fn_ty, requested_source_fn_ty)) {
                executableInvariant("executable call_value member target specialization source type differs from call site");
            }
            const capture_arg_len: usize = if (member.capture_slots.len == 0) 0 else 1;
            const capture_payload_ty = try self.lowerCallableSetMemberPayloadType(member);
            const capture_payload = if (capture_payload_ty != null) self.output.freshValueRef() else null;
            const direct_args = try self.allocator.alloc(Ast.DirectCallArg, arg_values.len + capture_arg_len);
            defer self.allocator.free(direct_args);
            for (arg_values, 0..) |arg_value, arg_i| {
                direct_args[arg_i] = .{ .value = arg_value };
            }
            if (capture_payload) |payload_value| {
                direct_args[arg_values.len] = .{ .value = payload_value };
            }
            branches[i] = .{
                .member = .{
                    .callable_set_key = callable_set_key,
                    .member_index = member.member,
                },
                .source_fn_ty = member.proc_value.source_fn_ty,
                .capture_payload = capture_payload,
                .capture_payload_ty = capture_payload_ty,
                .executable_specialization_key = try repr.cloneExecutableSpecializationKey(self.allocator, target_instance.executable_specialization_key),
                .executable_proc = executable_proc,
                .direct_args = try self.output.addDirectCallArgSpan(direct_args),
                .result_bridge = null,
            };
        }

        const result_ty = try self.lowerExecutableValueType(source_ty, call_site.result);
        const result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(result_ty, result_value, .{ .callable_match = .{
            .callable_set_key = callable_set_key,
            .requested_source_fn_ty = requested_source_fn_ty,
            .callee = func_value,
            .args = try self.output.addValueRefSpan(arg_values),
            .branches = try self.output.addCallableMatchBranchSpan(branches),
            .result_ty = result_ty,
            .result_value = result_value,
        } });

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids),
            .final_expr = final_call,
        } });
    }

    fn lowerCallValueErased(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        call: anytype,
        func: Ast.ExprId,
        func_value: Ast.ExecutableValueRef,
        call_site: repr.CallSiteInfo,
        sig_key: repr.ErasedFnSigKey,
    ) Allocator.Error!Ast.ExprId {
        if (!repr.canonicalTypeKeyEql(call_site.requested_source_fn_ty, call.requested_source_fn_ty)) {
            executableInvariant("executable erased call_value call-site requested source type differs from expression");
        }
        if (!repr.canonicalTypeKeyEql(sig_key.source_fn_ty, call_site.requested_source_fn_ty)) {
            executableInvariant("executable erased call_value signature source type differs from call site");
        }
        const func_value_info_id = self.input.exprs.items[@intFromEnum(call.func)].value_info;
        const func_value_info = self.value_store.values.items[@intFromEnum(func_value_info_id)];
        const callable = func_value_info.callable orelse executableInvariant("executable erased call_value callee has no callable metadata");
        switch (self.representation_store.callableEmissionPlan(callable.emission_plan)) {
            .already_erased => |erased| if (!repr.erasedFnSigKeyEql(erased.sig_key, sig_key)) {
                executableInvariant("executable erased call_value call-site dispatch differs from already-erased callee emission");
            },
            .erase_proc_value => |erase| if (!repr.erasedFnSigKeyEql(erase.erased_fn_sig_key, sig_key)) {
                executableInvariant("executable erased call_value call-site dispatch differs from proc erase emission");
            },
            .erase_finite_set => |erase| if (!repr.erasedFnSigKeyEql(erase.adapter.erased_fn_sig_key, sig_key)) {
                executableInvariant("executable erased call_value call-site dispatch differs from finite-set adapter emission");
            },
            .finite => executableInvariant("executable erased call_value reached finite callee emission"),
        }

        const capture_ty = self.erasedFnCaptureType(self.output.getExpr(func).ty, sig_key);

        const arg_items = self.input.expr_ids.items[call.args.start..][0..call.args.len];
        const arg_values = try self.allocator.alloc(Ast.ExecutableValueRef, arg_items.len);
        defer self.allocator.free(arg_values);
        const stmt_ids = try self.allocator.alloc(Ast.StmtId, arg_items.len + 1);
        defer self.allocator.free(stmt_ids);
        stmt_ids[0] = try self.output.addStmt(.{ .decl = .{
            .value = func_value,
            .body = func,
        } });
        for (arg_items, 0..) |arg, i| {
            const lowered = try self.lowerExpr(arg);
            const value = self.exprValue(lowered);
            arg_values[i] = value;
            stmt_ids[i + 1] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }

        const result_ty = try self.lowerExecutableValueType(source_ty, call_site.result);
        const result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(result_ty, result_value, .{ .call_erased = .{
            .func = func_value,
            .args = try self.output.addValueRefSpan(arg_values),
            .sig_key = sig_key,
            .capture_ty = capture_ty,
        } });

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids),
            .final_expr = final_call,
        } });
    }

    fn erasedFnCaptureType(
        self: *BodyBuilder,
        func_ty: Type.TypeId,
        sig_key: repr.ErasedFnSigKey,
    ) ?Type.TypeId {
        return switch (self.type_lowerer.output.getType(func_ty)) {
            .link => |next| self.erasedFnCaptureType(next, sig_key),
            .erased_fn => |erased| blk: {
                if (!repr.erasedFnSigKeyEql(erased.sig_key, sig_key)) {
                    executableInvariant("executable erased call callee type signature differs from call site");
                }
                const sig_has_capture = sig_key.capture_ty != null;
                const type_has_capture = erased.capture_ty != null;
                if (sig_has_capture != type_has_capture) {
                    executableInvariant("executable erased call callee type hidden capture disagrees with signature");
                }
                break :blk erased.capture_ty;
            },
            else => executableInvariant("executable erased call callee is not an erased function value"),
        };
    }

    fn addValueExpr(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        data: Ast.Expr.Data,
    ) Allocator.Error!Ast.ExprId {
        return try self.output.addExpr(
            try self.lowerExecutableValueType(source_ty, value_info_id),
            self.output.freshValueRef(),
            data,
        );
    }

    fn exprValue(self: *const BodyBuilder, expr: Ast.ExprId) Ast.ExecutableValueRef {
        return self.output.getExpr(expr).value;
    }
};

fn executableInvariant(comptime message: []const u8) noreturn {
    debug.invariant(false, message);
    unreachable;
}

fn executableProcForSource(program: *const Program, source_proc: canonical.MirProcedureRef) ?Ast.ExecutableProcId {
    for (program.procs.items) |proc| {
        if (canonical.mirProcedureRefEql(proc.source_proc, source_proc))
        {
            return proc.executable_proc;
        }
    }
    return null;
}

pub fn verifyCallableMatchBranch(
    representation_store: *const repr.RepresentationStore,
    callable_set_key: repr.CanonicalCallableSetKey,
    requested_source_fn_ty: canonical.CanonicalTypeKey,
    branch: Ast.CallableMatchBranch,
) void {
    debug.invariant(
        repr.callableSetKeyEql(branch.member.callable_set_key, callable_set_key),
        "executable invariant violated: callable_match branch points at a different callable set",
    );
    const member = representation_store.callableSetMember(callable_set_key, branch.member.member_index) orelse {
        debug.invariant(false, "executable invariant violated: callable_match branch points at missing callable member");
        return;
    };
    debug.invariant(
        repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, requested_source_fn_ty),
        "executable invariant violated: callable_match member source function type differs from call site",
    );
    debug.invariant(
        repr.canonicalTypeKeyEql(branch.source_fn_ty, requested_source_fn_ty),
        "executable invariant violated: callable_match branch source function type differs from call site",
    );
    debug.invariant(
        repr.canonicalTypeKeyEql(branch.executable_specialization_key.requested_fn_ty, requested_source_fn_ty),
        "executable invariant violated: callable_match executable specialization requested type differs from call site",
    );
}

test "executable build owns final program state" {
    std.testing.refAllDecls(@This());
}

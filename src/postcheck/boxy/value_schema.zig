//! The constant store schema of the roots the checked-value writer completes
//! (design.md "Value-Form Roots"): for each root, the plan its value is
//! stored by, the layout of the value, and the stored type it is restored
//! at, exactly as a `.boxy` compile-time program's lowering gives them
//! (`const_schema.zig`). The writer stores the root from its checked
//! expression under that schema, so a value-form root is stored as the
//! evaluator would store it, with no program lowered, compiled or run.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");

const Common = @import("../common.zig");
const const_schema = @import("const_schema.zig");
const lower = @import("lower.zig");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const const_store = check.ConstStore;
const LirProgram = lir_core.Program;
const invariant = types.invariant;

/// The schema of the roots of one checked module set.
pub const ValueSchema = struct {
    program: *lower.Program,

    /// What a root's value is stored by.
    pub const Root = struct {
        plan: LirProgram.ConstPlanId,
        ret_layout: layout.Idx,
        ret_type: const_store.ConstTypeId,
    };

    /// The schema of roots of `modules`, the set a compile-time program
    /// evaluating them lowers, at the evaluation target's width.
    pub fn init(allocator: Allocator, modules: Common.CheckedModules, target_usize: base.target.TargetUsize) Allocator.Error!ValueSchema {
        return .{ .program = try lower.createSchemaProgram(allocator, modules, target_usize) };
    }

    pub fn deinit(self: *ValueSchema) void {
        lower.destroySchemaProgram(self.program);
        self.* = undefined;
    }

    /// The schema of the root `request` asks for, whose module is `module`:
    /// the plan, layout and stored type of its root procedure's result, as a
    /// compile-time lowering of the request records them
    /// (`LirProgram.ConstRootPlan`).
    pub fn root(self: *ValueSchema, module: checked.ModuleId, request: checked.RootRequest) Allocator.Error!Root {
        const program = self.program;
        const module_index = program.moduleByKey(module).index;
        const fn_ty = try program.converter().convert(module_index, request.checked_type, .{ .module = module_index, .vars = &.{}, .bound = &.{} });
        const ret = switch (program.store.get(fn_ty)) {
            .function => |function| function.ret,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a compile-time root's checked type is not a function type"),
        };
        return .{
            .ret_layout = try program.layouts.layoutOf(ret),
            .ret_type = try const_schema.constTypeOf(program, ret),
            .plan = try const_schema.constPlanOf(program, ret),
        };
    }

    /// The plans, layouts and stored types every `Root` names.
    pub fn result(self: *const ValueSchema) *const LirProgram.Result {
        return &self.program.result;
    }

    /// The fields of record plan `plan`, whose component `index` stores the
    /// storage of field `index`.
    pub fn recordFields(self: *const ValueSchema, plan: LirProgram.ConstPlanId) RecordFields {
        const program = self.program;
        const ty = program.const_schema.plan_types.get(plan) orelse invariant("a record plan has no type");
        return switch (program.store.get(ty)) {
            .record => |record| .{ .store = &program.store, .fields = program.store.fieldSpan(record.fields) },
            .empty_record => .{ .store = &program.store, .fields = &.{} },
            .primitive, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record plan's type is not a record"),
        };
    }

    /// The fields of a record plan, in plan order.
    pub const RecordFields = struct {
        store: *const types.Store,
        fields: []const types.Field,

        pub fn name(self: RecordFields, index: usize) []const u8 {
            return self.store.nameText(self.fields[index].name);
        }
    };
};

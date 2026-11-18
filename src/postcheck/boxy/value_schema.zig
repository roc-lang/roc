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

    /// Extend the schema to the roots of `views`' modules too.
    pub fn addModules(self: *ValueSchema, views: []const checked.ImportedModuleView) Allocator.Error!void {
        try lower.addSchemaModules(self.program, views);
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

    /// The view of module `module` of the schema's modules, whose constants
    /// a value-form root of another module may read.
    pub fn moduleView(self: *const ValueSchema, module: checked.ModuleId) *const checked.ImportedModuleView {
        return &self.program.moduleByKey(module).view;
    }

    /// The plans, layouts and stored types every `Root` names.
    pub fn result(self: *const ValueSchema) *const LirProgram.Result {
        return &self.program.result;
    }

    /// The component of record plan `plan` that stores the field named
    /// `label`. The name is interned in the schema's program and matched by
    /// id against the record type's fields, which the plan's components
    /// follow in order.
    pub fn recordComponent(self: *const ValueSchema, plan: LirProgram.ConstPlanId, label: []const u8) Allocator.Error!usize {
        const program = self.program;
        const name = try program.store.internName(label);
        const ty = program.const_schema.plan_types.get(plan) orelse invariant("a record plan has no type");
        const fields = switch (program.store.get(ty)) {
            .record => |record| program.store.fieldSpan(record.fields),
            .empty_record, .primitive, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a record plan's type is not a record of fields"),
        };
        for (fields, 0..) |field, index| {
            if (field.name == name) return index;
        }
        invariant("a value-form record has a field its type does not store");
    }

    /// The variant of tag-union plan `plan` named `label`. The name is
    /// interned in the schema's program and matched by id against the union
    /// type's tags, which the plan's variants follow in order.
    pub fn tagVariant(self: *const ValueSchema, plan: LirProgram.ConstPlanId, label: []const u8) Allocator.Error!usize {
        const program = self.program;
        const name = try program.store.internName(label);
        const ty = program.const_schema.plan_types.get(plan) orelse invariant("a tag union plan has no type");
        const tags = switch (program.store.get(ty)) {
            .tag_union => |tag_union| program.store.tagSpan(tag_union.tags),
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => invariant("a tag union plan's type is not a tag union"),
        };
        for (tags, 0..) |tag, index| {
            if (tag.name == name) return index;
        }
        invariant("a value-form tag is not a variant of its type");
    }
};

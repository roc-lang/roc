//! The host boundary of one platform entrypoint (design.md "Unit-Image Glue
//! Execution"): the committed layouts of its arguments and result, and the
//! field and tag positions of every named type they reach, exactly as a
//! `.boxy` lowering of the entrypoint's root commits them. A caller that runs
//! the entrypoint from unit images restored from the cache marshals values
//! across it under this schema, with no program lowered.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");

const Common = @import("../common.zig");
const lower = @import("lower.zig");
const runtime_schemas = @import("runtime_schemas.zig");
const schemas = @import("../runtime_schemas.zig");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const invariant = types.invariant;

/// The host boundary of one platform entrypoint of a checked module set.
pub const EntrypointSchema = struct {
    program: *lower.Program,
    /// The layout of each argument, in order.
    arg_layouts: []const layout.Idx,
    ret_layout: layout.Idx,
    /// The named records and tag unions the arguments and result reach.
    named: schemas.RuntimeSchemaStore,

    /// The boundary of the entrypoint `request` asks for, a root of the root
    /// module of `modules`, the set a lowering of that root reads.
    pub fn init(allocator: Allocator, modules: Common.CheckedModules, request: checked.RootRequest, target_usize: base.target.TargetUsize) Allocator.Error!EntrypointSchema {
        const program = try lower.createSchemaProgram(allocator, modules, target_usize);
        errdefer lower.destroySchemaProgram(program);
        const root_module = program.modules.views.items[0].index;
        const fn_ty = try program.converter().convert(root_module, request.checked_type, .{ .module = root_module, .vars = &.{}, .bound = &.{} });
        const signature = switch (program.store.get(fn_ty)) {
            .function => |function| function,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .variable => invariant("a platform entrypoint's checked type is not a function type"),
        };
        // Committing a layout can grow the type store under its own spans.
        const params = try allocator.dupe(types.TypeId, program.store.span(signature.args));
        defer allocator.free(params);
        const arg_layouts = try allocator.alloc(layout.Idx, params.len);
        errdefer allocator.free(arg_layouts);
        for (params, arg_layouts) |param, *arg_layout| arg_layout.* = try program.layouts.layoutOf(param);
        const ret_layout = try program.layouts.layoutOf(signature.ret);

        var roots: std.ArrayList(types.TypeId) = .empty;
        defer roots.deinit(allocator);
        try roots.appendSlice(allocator, params);
        try roots.append(allocator, signature.ret);
        return .{
            .program = program,
            .arg_layouts = arg_layouts,
            .ret_layout = ret_layout,
            .named = try runtime_schemas.collect(program, roots.items),
        };
    }

    pub fn deinit(self: *EntrypointSchema) void {
        const allocator = self.program.allocator;
        self.named.deinit();
        allocator.free(self.arg_layouts);
        lower.destroySchemaProgram(self.program);
        self.* = undefined;
    }

    /// The store every layout of the schema is committed in.
    pub fn layouts(self: *const EntrypointSchema) *const layout.Store {
        return &self.program.result.layouts;
    }
};

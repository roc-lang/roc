//! The top-level value def of a module that owns each node: the def node
//! itself, the def's expression, and every node of the def's pattern, so a
//! name bound by a destructure resolves to the def that destructures it.
//! Checking records the table on its module (`ModuleEnv.top_level_def_owners`),
//! and lookups into the module, its own and its importers', read it there.

const std = @import("std");
const base = @import("base");
const can = @import("can");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const TopLevelDefOwner = ModuleEnv.TopLevelDefOwner;

/// Record the owner of every node `module`'s top-level value defs own. When
/// defs share a node, the first def in `global_value_defs` order owns it.
pub fn publish(gpa: Allocator, module: *ModuleEnv) Allocator.Error!void {
    var owners: std.ArrayList(TopLevelDefOwner) = .empty;
    defer owners.deinit(gpa);
    var pending: std.ArrayList(CIR.Pattern.Idx) = .empty;
    defer pending.deinit(gpa);

    const defs = module.store.sliceDefs(module.global_value_defs);
    // Each def owns at least its own node, its expression and its pattern.
    try owners.ensureTotalCapacity(gpa, defs.len * 3);
    for (defs) |def_idx| {
        const def = module.store.getDef(def_idx);
        try owners.append(gpa, .{ .node = @backingInt(ModuleEnv.nodeIdxFrom(def_idx)), .def = def_idx });
        try owners.append(gpa, .{ .node = @backingInt(ModuleEnv.nodeIdxFrom(def.expr)), .def = def_idx });

        // The walk is an explicit worklist (zero-recursion policy).
        pending.clearRetainingCapacity();
        try pending.append(gpa, def.pattern);
        while (pending.pop()) |current| {
            try owners.append(gpa, .{ .node = @backingInt(ModuleEnv.nodeIdxFrom(current)), .def = def_idx });
            switch (module.store.getPattern(current)) {
                .assign, .var_assign => {},
                .as => |as_pattern| try pending.append(gpa, as_pattern.pattern),
                .applied_tag => |tag| try pending.appendSlice(gpa, module.store.slicePatterns(tag.args)),
                .nominal => |nominal| try pending.append(gpa, nominal.backing_pattern),
                .nominal_external => |nominal| try pending.append(gpa, nominal.backing_pattern),
                .record_destructure => |record| {
                    for (module.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                        const destruct = module.store.getRecordDestruct(destruct_idx);
                        try pending.append(gpa, destruct.kind.toPatternIdx());
                    }
                },
                .list => |list| {
                    try pending.appendSlice(gpa, module.store.slicePatterns(list.patterns));
                    if (list.rest_info) |rest| {
                        if (rest.pattern) |rest_pattern| try pending.append(gpa, rest_pattern);
                    }
                },
                .tuple => |tuple| try pending.appendSlice(gpa, module.store.slicePatterns(tuple.patterns)),
                .str_interpolation => |str| {
                    for (0..str.steps.span.len) |offset| {
                        const step = module.store.getStrPatternStep(str.steps, @intCast(offset));
                        if (step.capture) |capture| try pending.append(gpa, capture);
                    }
                },
                .num_literal,
                .num_from_numeral_literal,
                .small_dec_literal,
                .dec_literal,
                .frac_f32_literal,
                .frac_f64_literal,
                .str_literal,
                .underscore,
                .runtime_error,
                => {},
                .deferred_import_ref => base.invariant("compiler invariant violated: deferred import reference pattern reached a stage that runs after import resolution", .{}),
            }
        }
    }

    // A stable sort keeps each node's entries in def order, so the first
    // entry of a run is its first owner.
    std.mem.sort(TopLevelDefOwner, owners.items, {}, struct {
        fn lessThan(_: void, a: TopLevelDefOwner, b: TopLevelDefOwner) bool {
            return a.node < b.node;
        }
    }.lessThan);
    var kept: usize = 0;
    for (owners.items) |owner| {
        if (kept > 0 and owners.items[kept - 1].node == owner.node) continue;
        owners.items[kept] = owner;
        kept += 1;
    }
    try module.setTopLevelDefOwners(owners.items[0..kept]);
}

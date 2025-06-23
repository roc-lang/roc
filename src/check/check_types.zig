const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const types_mod = @import("../types.zig");
const can = @import("canonicalize.zig");
const unifier = @import("check_types/unify.zig");
const occurs = @import("check_types/occurs.zig");
const problem = @import("check_types/problem.zig");
const snapshot = @import("check_types/snapshot.zig");
const CIR = @import("./canonicalize/CIR.zig");
const ModuleEnv = @import("../base/ModuleEnv.zig");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const ModuleWork = base.ModuleWork;

const Var = types_mod.Var;

const Self = @This();

gpa: std.mem.Allocator,
// not owned
types: *types_mod.Store,
can_ir: *const CIR,
// owned
snapshots: snapshot.Store,
problems: problem.Store,
unify_scratch: unifier.Scratch,
occurs_scratch: occurs.Scratch,

/// Init type solver
/// Does *not* own types_store or can_ir, but *does* own other fields
pub fn init(
    gpa: std.mem.Allocator,
    types: *types_mod.Store,
    can_ir: *const CIR,
) std.mem.Allocator.Error!Self {
    return .{
        .gpa = gpa,
        .types = types,
        .can_ir = can_ir,
        .snapshots = snapshot.Store.initCapacity(gpa, 512),
        .problems = problem.Store.initCapacity(gpa, 64),
        .unify_scratch = unifier.Scratch.init(gpa),
        .occurs_scratch = occurs.Scratch.init(gpa),
    };
}

/// Deinit owned fields
pub fn deinit(self: *Self) void {
    self.problems.deinit(self.gpa);
    self.snapshots.deinit();
    self.unify_scratch.deinit();
    self.occurs_scratch.deinit();
}

/// Deinit owned fields
pub fn unify(self: *Self, a: Var, b: Var) void {
    self.unifyWithRegion(a, b, null);
}

/// TODO
pub fn unifyWithRegion(self: *Self, a: Var, b: Var, region: ?Region) void {
    _ = unifier.unifyWithRegion(
        self.can_ir.env,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.unify_scratch,
        &self.occurs_scratch,
        a,
        b,
        region,
    );
}

/// Check the types for all defs
pub fn checkDefs(self: *Self) void {
    const defs_slice = self.can_ir.store.sliceDefs(self.can_ir.all_defs);
    for (defs_slice) |def_idx| {
        const def = self.can_ir.store.getDef(def_idx);
        // TODO: Check patterns
        self.checkExpr(def.expr);

        self.unify(
            @enumFromInt(@intFromEnum(def_idx)),
            @enumFromInt(@intFromEnum(def.expr)),
        );
    }
}

/// Check the types for the provided expr
pub fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx) void {
    const expr = self.can_ir.store.getExpr(expr_idx);
    switch (expr) {
        .num => |_| {},
        .int => |_| {},
        .frac_f64 => |_| {},
        .frac_dec => |_| {},
        .str_segment => |_| {},
        .str => |_| {},
        .single_quote => |_| {},
        .lookup => |_| {},
        .list => |list| {
            const elem_var = list.elem_var;
            for (self.can_ir.store.exprSlice(list.elems)) |single_elem_expr_idx| {
                self.checkExpr(single_elem_expr_idx);
                const single_elem_expr = self.can_ir.store.getExpr(single_elem_expr_idx);
                const elem_region = single_elem_expr.toRegion();
                self.unifyWithRegion(
                    @enumFromInt(@intFromEnum(elem_var)),
                    @enumFromInt(@intFromEnum(single_elem_expr_idx)),
                    elem_region,
                );
            }
        },
        .when => |_| {},
        .@"if" => |_| {},
        .call => |_| {},
        .record => |_| {},
        .empty_record => |_| {},
        .record_access => |_| {},
        .tag => |_| {},
        .zero_argument_tag => |_| {},
        .binop => |_| {},
        .block => |_| {},
        .lambda => |_| {},
        .tuple => |_| {},
        .runtime_error => |_| {},
    }
}

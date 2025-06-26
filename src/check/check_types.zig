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
    _ = unifier.unify(
        self.can_ir.env,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.unify_scratch,
        &self.occurs_scratch,
        a,
        b,
    );
}

/// Check the types for all defs
pub fn checkDefs(self: *Self) void {
    const defs_slice = self.can_ir.store.sliceDefs(self.can_ir.all_defs);
    for (defs_slice) |def_idx| {
        const def = self.can_ir.store.getDef(def_idx);
        // TODO: Check patterns
        self.checkExpr(def.expr);

        // If there's a type annotation, unify the expression with the annotation's signature
        if (def.annotation) |anno_idx| {
            const annotation = self.can_ir.store.getAnnotation(anno_idx);

            self.unify(@enumFromInt(@intFromEnum(def.expr)), annotation.signature);
            self.unify(@enumFromInt(@intFromEnum(def_idx)), annotation.signature);
        } else {
            self.unify(@enumFromInt(@intFromEnum(def_idx)), @enumFromInt(@intFromEnum(def.expr)));
        }
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
        .dec_small => |_| {},
        .str_segment => |_| {},
        .str => |_| {},
        .single_quote => |_| {},
        .lookup => |_| {},
        .list => |list| {
            const elem_var = list.elem_var;
            for (self.can_ir.store.exprSlice(list.elems)) |single_elem_expr_idx| {
                self.checkExpr(single_elem_expr_idx);
                self.unify(
                    @enumFromInt(@intFromEnum(elem_var)),
                    @enumFromInt(@intFromEnum(single_elem_expr_idx)),
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
        .dot_access => |_| {
            // TODO: Implement type checking for dot access
            // This will need to:
            // 1. Check the receiver type
            // 2. Determine if it's record field access or static dispatch
            // 3. Validate the field/method exists and has correct type
            // 4. Type check any arguments if it's a method call
        },
        .runtime_error => |_| {},
    }
}

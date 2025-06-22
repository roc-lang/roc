const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const types = @import("../types.zig");
const can = @import("canonicalize.zig");
const unifier = @import("check_types/unify.zig");
const occurs = @import("check_types/occurs.zig");
const CIR = @import("./canonicalize/CIR.zig");
const ModuleEnv = @import("../base/ModuleEnv.zig");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const ModuleWork = base.ModuleWork;

const Var = types.Var;

const Self = @This();

gpa: std.mem.Allocator,
// not owned
types_store: *types.Store,
can_ir: *const CIR,
// owned
unify_scratch: *unifier.Scratch,
occurs_scratch: *occurs.Scratch,

/// Init type solver
/// Does *not* own types_store or can_ir, but *does* own other fields
pub fn init(gpa: std.mem.Allocator, types_store: *types.Store, can_ir: *const CIR) std.mem.Allocator.Error!Self {
    const unify_scratch = try gpa.create(unifier.Scratch);
    unify_scratch.* = unifier.Scratch.init(gpa);

    const occurs_scratch = try gpa.create(occurs.Scratch);
    occurs_scratch.* = occurs.Scratch.init(gpa);

    return .{
        .gpa = gpa,
        .types_store = types_store,
        .can_ir = can_ir,
        .unify_scratch = unify_scratch,
        .occurs_scratch = occurs_scratch,
    };
}

/// Deinit owned fields
pub fn deinit(self: *Self) void {
    self.unify_scratch.deinit();
    self.gpa.destroy(self.unify_scratch);

    self.occurs_scratch.deinit();
    self.gpa.destroy(self.occurs_scratch);
}

/// Deinit owned fields
pub fn unify(self: *Self, a: Var, b: Var) void {
    const ret = unifier.unify(
        self.can_ir.env,
        self.types_store,
        self.unify_scratch,
        self.occurs_scratch,
        a,
        b,
    );
    switch (ret) {
        .ok => {},
        else => {
            // TODO: Handle errors
            std.debug.print("unify err {}\n", .{ret});
        },
    }
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
        .float => |_| {},
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

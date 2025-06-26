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
        .record => |e| {
            // ## RECORD TYPE CHECKING IMPLEMENTATION
            //
            // This implementation performs field-by-field unification between the record
            // structure's field type variables and the actual field value expression types.
            //
            // ### Prerequisites (set during canonicalization)
            // - record_var must contain .structure.record (not .flex_var)
            // - Each field in the structure must have a fresh type variable
            // - Expression variable must also have the record structure for final output
            //
            // ### Process
            // 1. Resolve record_var to get the record structure
            // 2. Type check each field value expression (to get concrete types)
            // 3. For each field, unify the field type var with the field value type var
            // 4. Unification propagates concrete types through the type system

            // Get the record type content - this MUST be .structure.record for unification to work
            const record_var_resolved = self.types.resolveVar(@enumFromInt(@intFromEnum(e.record_var)));
            const record_var_content = record_var_resolved.desc.content;

            // Process each field
            for (self.can_ir.store.sliceRecordFields(e.fields)) |field_idx| {
                const field = self.can_ir.store.getRecordField(field_idx);

                // STEP 1: Check the field value expression first
                // This ensures the field value has a concrete type to unify with
                self.checkExpr(field.value);

                // STEP 2: Find the corresponding field type in the record structure
                // This only works if record_var_content is .structure.record
                if (record_var_content == .structure and record_var_content.structure == .record) {
                    const record_fields = self.types.getRecordFieldsSlice(record_var_content.structure.record.fields);

                    // STEP 3: Find the field with matching name and unify types
                    const field_names = record_fields.items(.name);
                    const field_vars = record_fields.items(.var_);
                    for (field_names, field_vars) |type_field_name, type_field_var| {
                        if (self.can_ir.env.idents.identsHaveSameText(type_field_name, field.name)) {
                            // Extract the type variable from the field value expression
                            // Different expression types store their type variables in different places
                            const field_expr = self.can_ir.store.getExpr(field.value);
                            const field_expr_type_var = switch (field_expr) {
                                .str, .str_segment => @as(Var, @enumFromInt(@intFromEnum(field.value))),
                                .int => |int_expr| int_expr.num_var,
                                .num => |num_expr| num_expr.num_var,
                                .frac_f64 => |frac_expr| frac_expr.frac_var,
                                .frac_dec => |frac_expr| frac_expr.frac_var,
                                .dec_small => |dec_expr| dec_expr.num_var,
                                else => @as(Var, @enumFromInt(@intFromEnum(field.value))),
                            };

                            // STEP 4: Unify field type variable with field value type variable
                            // This is where concrete types (like Str, Num) get propagated
                            // from field values to the record structure
                            self.unify(type_field_var, field_expr_type_var);
                            break;
                        }
                    }
                }
                // If record_var_content is NOT .structure.record, unification is skipped
                // This typically happens when canonicalization didn't set the record structure properly
            }
        },
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

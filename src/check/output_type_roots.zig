//! Output type roots that inference produced (design.md "Row Union
//! Normalization"). Checked-module publication and the checker's settled row
//! walk both enumerate them through these functions, so every row the checked
//! module publishes is one the checker normalized. A visitor provides
//! `visit(Var) Allocator.Error!void` and
//! `visitRequired(?Var, comptime []const u8) Allocator.Error!void`; the latter
//! receives a var checking must have recorded, with the invariant it states.

const std = @import("std");
const can = @import("can");
const types = @import("types");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;

/// The type var that owns a type-dispatch call's statement.
pub fn typeDispatchOwnerVar(module: anytype, stmt_idx: CIR.Statement.Idx) Var {
    return switch (module.getStatement(stmt_idx)) {
        .s_type_var_alias => |alias| ModuleEnv.varFrom(alias.type_var_anno),
        .s_alias_decl => ModuleEnv.varFrom(stmt_idx),
        .s_decl,
        .s_var,
        .s_var_uninitialized,
        .s_reassign,
        .s_crash,
        .s_dbg,
        .s_expr,
        .s_expect,
        .s_for,
        .s_while,
        .s_infinite_loop,
        .s_breakable_loop,
        .s_break,
        .s_return,
        .s_import,
        .s_nominal_decl,
        .s_where_alias_decl,
        .s_type_anno,
        .s_runtime_error,
        => @panic("type dispatch owner statement was not a type-var alias or type alias"),
    };
}

/// The roots a call or field-access expression publishes beyond its own type.
pub fn forEachCallTypeRoot(module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, visitor: anytype) Allocator.Error!void {
    const expr = module_env.store.getExpr(expr_idx);
    if (expr == .e_call) {
        if (expr.e_call.constraint_fn_var) |constraint_fn_var| try visitor.visit(constraint_fn_var);
    } else if (expr == .e_field_access) {
        const field_access = expr.e_field_access;
        var position: u32 = 0;
        while (position < field_access.segments.len) : (position += 1) {
            try visitor.visit(ModuleEnv.varFrom(module_env.store.fieldAccessSegmentAt(field_access.segments, position)));
        }
    }
}

/// The roots a statically dispatched expression publishes.
pub fn forEachStaticDispatchTypeRoot(module_env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, visitor: anytype) Allocator.Error!void {
    const expr = module_env.store.getExpr(expr_idx);
    if (expr == .e_dispatch_call) {
        try visitor.visit(ModuleEnv.varFrom(expr.e_dispatch_call.receiver));
        try visitor.visit(expr.e_dispatch_call.constraint_fn_var);
    } else if (expr == .e_interpolation) {
        const interpolation = expr.e_interpolation;
        try visitor.visit(ModuleEnv.varFrom(expr_idx));
        try visitor.visitRequired(interpolation.dispatcher_var, "checked interpolation expression had no static dispatch dispatcher type");
        try visitor.visitRequired(interpolation.constraint_fn_var, "checked interpolation expression had no static dispatch constraint type");
        try visitor.visitRequired(interpolation.step_fn_var, "checked interpolation expression had no generated step function type");
    } else if (expr == .e_type_dispatch_call) {
        try visitor.visit(typeDispatchOwnerVar(&module_env.store, expr.e_type_dispatch_call.type_dispatch_stmt));
        try visitor.visit(expr.e_type_dispatch_call.constraint_fn_var);
    } else if (expr == .e_method_eq) {
        try visitor.visit(ModuleEnv.varFrom(expr.e_method_eq.lhs));
        try visitor.visit(expr.e_method_eq.constraint_fn_var);
    }
}

/// The roots of the module's recorded codec requirements, scheme-use
/// substitutions, and generated codec derivations.
pub fn forEachRecordedTypeRoot(module_env: *const ModuleEnv, visitor: anytype) Allocator.Error!void {
    for (module_env.binding_scheme_codec_requirements.items.items) |requirement| {
        const constraint = module_env.types.getStaticDispatchConstraintAt(requirement.constraint_index);
        try visitor.visit(@enumFromInt(requirement.receiver_var));
        try visitor.visit(constraint.fn_var);
    }
    for (module_env.scheme_use_pairs.items.items) |pair| {
        try visitor.visit(@enumFromInt(pair.fresh_var));
    }
    for (module_env.generated_codec_derivations.items.items) |derivation| {
        inline for (.{
            derivation.source_constraint_fn_var,
            derivation.source_runtime_fn_var,
            derivation.source_shape_var,
            derivation.source_body_shape_var,
            derivation.source_encoding_var,
            derivation.source_state_var,
            derivation.source_error_var,
            derivation.constraint_fn_var,
            derivation.runtime_fn_var,
            derivation.shape_var,
            derivation.body_shape_var,
            derivation.encoding_var,
            derivation.state_var,
            derivation.error_var,
        }) |raw_var| {
            try visitor.visit(@enumFromInt(raw_var));
        }
        const calls = module_env.generated_codec_calls.items.items[derivation.calls_start..][0..derivation.calls_len];
        for (calls) |call| {
            inline for (.{ call.dispatcher_var, call.callable_var, call.evidence_var }) |raw_var| {
                try visitor.visit(@enumFromInt(raw_var));
            }
            if (call.subject_var != ModuleEnv.GeneratedCodecCall.no_subject_var) {
                try visitor.visit(@enumFromInt(call.subject_var));
            }
        }
    }
}

/// The roots of every recorded scheme use: the instance a dispatch target,
/// where-method use, or stored nested function names, and each fresh var of
/// the use's substitution.
pub fn forEachSchemeUseTypeRoot(module_env: *const ModuleEnv, visitor: anytype) Allocator.Error!void {
    for (module_env.scheme_uses.items.items) |record| {
        if (record.slot_kind == @intFromEnum(ModuleEnv.SchemeUseRecord.Slot.dispatch_target) or
            record.slot_kind == @intFromEnum(ModuleEnv.SchemeUseRecord.Slot.recursive_dispatch_target) or
            record.slot_kind == @intFromEnum(ModuleEnv.SchemeUseRecord.Slot.where_method_use) or
            record.slot_kind == @intFromEnum(ModuleEnv.SchemeUseRecord.Slot.nested_function_use))
        {
            try visitor.visit(@enumFromInt(record.slot_data));
        }
        const pairs = module_env.scheme_use_pairs.items.items[record.pairs_start .. record.pairs_start + record.pairs_len];
        for (pairs) |pair| {
            try visitor.visit(@enumFromInt(pair.fresh_var));
        }
    }
}

/// The roots of a `for` loop's dispatch plan.
pub fn forEachForLoopDispatchTypeRoot(plan: ModuleEnv.ForLoopDispatchPlan, visitor: anytype) Allocator.Error!void {
    try visitor.visit(@enumFromInt(plan.iterator_var));
    try visitor.visit(@enumFromInt(plan.step_var));
    try visitor.visit(@enumFromInt(plan.iter_fn_var));
    try visitor.visit(@enumFromInt(plan.next_fn_var));
    try visitor.visit(@enumFromInt(plan.step_topology.one_payload_var));
    try visitor.visit(@enumFromInt(plan.step_topology.skip_payload_var));
}

/// The roots of every literal conversion that dispatches to a custom or
/// specialization-time callable: its target and callable, and a literal
/// pattern's equality callable. A direct builtin conversion needs neither.
pub fn forEachLiteralDispatchTypeRoot(module_env: *const ModuleEnv, visitor: anytype) Allocator.Error!void {
    for (module_env.store.literalDispatchPlans()) |plan| {
        switch (plan.dispatchResolution()) {
            .builtin_direct, .checked_error => continue,
            .custom_dispatch, .specialization_dispatch => {},
            .unresolved => {
                try visitor.visitRequired(null, "unresolved literal dispatch plan reached checked type publication");
                continue;
            },
        }
        try visitor.visit(@enumFromInt(plan.target_var));
        try visitor.visit(@enumFromInt(plan.fn_var));
        if (plan.patternContext(&module_env.store)) |context| {
            std.debug.assert(context.equality_fn_var_plus_one != 0);
            try visitor.visit(@enumFromInt(context.equality_fn_var_plus_one - 1));
        }
    }
}

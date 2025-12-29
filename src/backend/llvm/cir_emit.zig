//! CIR to LLVM IR Translation
//!
//! This module translates Canonical IR (CIR) expressions to LLVM IR.
//! It bridges the Roc compiler's intermediate representation with the
//! LLVM code generation infrastructure.
//!
//! ## Expression Categories
//!
//! 1. **Literals**: Numbers, strings, lists, records, tuples, tags
//! 2. **Variables**: Local lookups, external lookups, required lookups
//! 3. **Operations**: Binary ops, unary ops, function calls
//! 4. **Control Flow**: If expressions, match expressions, blocks
//! 5. **Functions**: Lambdas, closures, calls
//! 6. **Special**: Crash, dbg, expect, return
//!
//! ## Translation Strategy
//!
//! Each CIR expression maps to one or more LLVM values/instructions.
//! Complex expressions (if, match, block) create control flow structures.
//! Refcounted values are handled via the refcount module.

const std = @import("../../std.zig");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const Builder = @import("Builder.zig");
const emit = @import("emit.zig");
const builtins_mod = @import("builtins.zig");
const refcount = @import("refcount.zig");
const layout_types = @import("layout_types.zig");
const layout_mod = @import("../../layout/mod.zig");
const can = @import("can");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Layout = layout_mod.Layout;
const Store = layout_mod.Store;

/// Errors during CIR translation
pub const Error = error{
    OutOfMemory,
    NoActiveFunction,
    UnsupportedExpression,
    VariableNotFound,
    TypeMismatch,
};

/// Context for CIR translation
pub const CirContext = struct {
    allocator: Allocator,
    emitter: *emit.LlvmEmitter,
    module_env: *ModuleEnv,
    layout_store: *const Store,
    builtin_ctx: *builtins_mod.BuiltinContext,
    refcount_ctx: *refcount.RefcountContext,

    pub fn init(
        allocator: Allocator,
        emitter: *emit.LlvmEmitter,
        module_env: *ModuleEnv,
        layout_store: *const Store,
        builtin_ctx: *builtins_mod.BuiltinContext,
        refcount_ctx: *refcount.RefcountContext,
    ) CirContext {
        return .{
            .allocator = allocator,
            .emitter = emitter,
            .module_env = module_env,
            .layout_store = layout_store,
            .builtin_ctx = builtin_ctx,
            .refcount_ctx = refcount_ctx,
        };
    }
};

/// Translate a CIR expression to LLVM IR.
/// Returns the LLVM value representing the expression's result.
pub fn emitExpr(ctx: *CirContext, expr: CIR.Expr) Error!Builder.Value {
    return switch (expr) {
        // ========================================
        // Numeric Literals
        // ========================================
        .e_num => |num| try emitNumber(ctx, num.value, num.kind),
        .e_frac_f32 => |frac| try emitFloat32(ctx, frac.value),
        .e_frac_f64 => |frac| try emitFloat64(ctx, frac.value),
        .e_dec => |dec| try emitDec(ctx, dec.value.num),
        .e_dec_small => |dec| try emitDecSmall(ctx, dec.value),

        // ========================================
        // String Literals
        // ========================================
        .e_str_segment => |seg| try emitStrSegment(ctx, seg.literal),
        .e_str => |str_expr| try emitStr(ctx, str_expr.span),

        // ========================================
        // Collections
        // ========================================
        .e_list => |list| try emitList(ctx, list.elems),
        .e_empty_list => try emitEmptyList(ctx),
        .e_tuple => |tuple| try emitTuple(ctx, tuple.elems),
        .e_record => |record| try emitRecord(ctx, record.fields, record.ext),
        .e_empty_record => try emitEmptyRecord(ctx),

        // ========================================
        // Variables
        // ========================================
        .e_lookup_local => |lookup| try emitLocalLookup(ctx, lookup.pattern_idx),
        .e_lookup_external => |lookup| try emitExternalLookup(ctx, lookup),
        .e_lookup_required => return error.UnsupportedExpression, // Platform-specific

        // ========================================
        // Tags
        // ========================================
        .e_tag => |tag| try emitTag(ctx, tag.name, tag.args),
        .e_zero_argument_tag => |tag| try emitZeroArgTag(ctx, tag.name),
        .e_nominal => return error.UnsupportedExpression, // TODO
        .e_nominal_external => return error.UnsupportedExpression, // TODO

        // ========================================
        // Operations
        // ========================================
        .e_binop => |binop| try emitBinop(ctx, binop),
        .e_unary_minus => |unary| try emitUnaryMinus(ctx, unary),
        .e_unary_not => |unary| try emitUnaryNot(ctx, unary),
        .e_dot_access => |access| try emitDotAccess(ctx, access),

        // ========================================
        // Control Flow (defer to Phase 6)
        // ========================================
        .e_if => return error.UnsupportedExpression, // Phase 6
        .e_match => return error.UnsupportedExpression, // Phase 6
        .e_block => return error.UnsupportedExpression, // Phase 8

        // ========================================
        // Functions (defer to Phase 7)
        // ========================================
        .e_call => return error.UnsupportedExpression, // Phase 7
        .e_lambda => return error.UnsupportedExpression, // Phase 7
        .e_closure => return error.UnsupportedExpression, // Phase 7

        // ========================================
        // Special Expressions
        // ========================================
        .e_runtime_error => return error.UnsupportedExpression,
        .e_crash => return error.UnsupportedExpression, // Phase 10
        .e_dbg => |dbg| try emitDbg(ctx, dbg.expr),
        .e_expect => return error.UnsupportedExpression, // Phase 10
        .e_ellipsis => return error.UnsupportedExpression,
        .e_anno_only => return error.UnsupportedExpression,
        .e_return => return error.UnsupportedExpression, // Phase 8
        .e_type_var_dispatch => return error.UnsupportedExpression,
    };
}

// ============================================================
// Numeric Literal Emission
// ============================================================

fn emitNumber(ctx: *CirContext, value: CIR.IntValue, kind: CIR.NumKind) Error!Builder.Value {
    const int_value = value.toI128();

    return switch (kind) {
        .u8, .i8 => ctx.emitter.emitIntConst(.i8, @intCast(int_value)) catch return error.OutOfMemory,
        .u16, .i16 => ctx.emitter.emitIntConst(.i16, @intCast(int_value)) catch return error.OutOfMemory,
        .u32, .i32 => ctx.emitter.emitIntConst(.i32, @intCast(int_value)) catch return error.OutOfMemory,
        .u64, .i64, .num_unbound, .int_unbound => ctx.emitter.emitIntConst(.i64, @intCast(int_value)) catch return error.OutOfMemory,
        .u128, .i128 => ctx.emitter.emitIntConst(.i128, int_value) catch return error.OutOfMemory,
        .f32 => blk: {
            const float_val: f32 = @floatFromInt(int_value);
            break :blk ctx.emitter.emitFloatConst(.float, float_val) catch return error.OutOfMemory;
        },
        .f64 => blk: {
            const float_val: f64 = @floatFromInt(int_value);
            break :blk ctx.emitter.emitFloatConst(.double, float_val) catch return error.OutOfMemory;
        },
        .dec => ctx.emitter.emitIntConst(.i128, int_value) catch return error.OutOfMemory,
    };
}

fn emitFloat32(ctx: *CirContext, value: f32) Error!Builder.Value {
    return ctx.emitter.emitFloatConst(.float, value) catch return error.OutOfMemory;
}

fn emitFloat64(ctx: *CirContext, value: f64) Error!Builder.Value {
    return ctx.emitter.emitFloatConst(.double, value) catch return error.OutOfMemory;
}

fn emitDec(ctx: *CirContext, value: i128) Error!Builder.Value {
    // Dec is stored as i128 internally
    return ctx.emitter.emitIntConst(.i128, value) catch return error.OutOfMemory;
}

fn emitDecSmall(ctx: *CirContext, value: CIR.SmallDecValue) Error!Builder.Value {
    // Convert small Dec to full i128 representation
    const decimal_places: u5 = 18;
    const scale_factor = std.math.pow(i128, 10, decimal_places - value.denominator_power_of_ten);
    const scaled_value = @as(i128, value.numerator) * scale_factor;
    return ctx.emitter.emitIntConst(.i128, scaled_value) catch return error.OutOfMemory;
}

// ============================================================
// String Emission
// ============================================================

fn emitStrSegment(ctx: *CirContext, literal_idx: @import("base").StringLiteral.Idx) Error!Builder.Value {
    // Get the string literal from the module env
    const string_literal = ctx.module_env.common.string_literals.get(@intFromEnum(literal_idx));
    const str_bytes = string_literal.resolve(ctx.module_env.common.source);

    // Create a global string constant
    // For now, return a pointer to the string data
    // TODO: Proper RocStr representation
    return ctx.emitter.emitStringConst(str_bytes) catch return error.OutOfMemory;
}

fn emitStr(ctx: *CirContext, span: CIR.Expr.Span) Error!Builder.Value {
    // A string may be made of multiple segments (for interpolation)
    // For now, handle single-segment strings
    _ = ctx;
    _ = span;
    return error.UnsupportedExpression; // TODO: String interpolation
}

// ============================================================
// Collection Emission
// ============================================================

fn emitList(ctx: *CirContext, elems: CIR.Expr.Span) Error!Builder.Value {
    _ = ctx;
    _ = elems;
    // TODO: Allocate list, populate elements, return RocList struct
    return error.UnsupportedExpression;
}

fn emitEmptyList(ctx: *CirContext) Error!Builder.Value {
    // Empty list is { null, 0, 0 }
    _ = ctx;
    return error.UnsupportedExpression; // TODO
}

fn emitTuple(ctx: *CirContext, elems: CIR.Expr.Span) Error!Builder.Value {
    _ = ctx;
    _ = elems;
    return error.UnsupportedExpression; // TODO
}

fn emitRecord(ctx: *CirContext, fields: CIR.RecordField.Span, ext: ?CIR.Expr.Idx) Error!Builder.Value {
    _ = ctx;
    _ = fields;
    _ = ext;
    return error.UnsupportedExpression; // TODO
}

fn emitEmptyRecord(ctx: *CirContext) Error!Builder.Value {
    // Empty record is represented as an empty struct or unit
    _ = ctx;
    return error.UnsupportedExpression; // TODO
}

// ============================================================
// Variable Emission
// ============================================================

fn emitLocalLookup(ctx: *CirContext, pattern_idx: CIR.Pattern.Idx) Error!Builder.Value {
    // Look up the pattern to get the variable name
    const pattern = ctx.module_env.store.getPattern(pattern_idx);

    // Get the variable name based on pattern type
    const name = switch (pattern) {
        .assign => |assign| ctx.module_env.getIdent(assign.ident),
        else => return error.UnsupportedExpression,
    };

    // Look up in current scope
    if (ctx.emitter.lookupVar(name)) |scoped_val| {
        return scoped_val.value;
    }

    return error.VariableNotFound;
}

fn emitExternalLookup(ctx: *CirContext, lookup: anytype) Error!Builder.Value {
    _ = ctx;
    _ = lookup;
    // TODO: Look up in imported module
    return error.UnsupportedExpression;
}

// ============================================================
// Tag Emission
// ============================================================

fn emitTag(ctx: *CirContext, name: @import("base").Ident.Idx, args: CIR.Expr.Span) Error!Builder.Value {
    _ = ctx;
    _ = name;
    _ = args;
    // TODO: Create tag union value
    return error.UnsupportedExpression;
}

fn emitZeroArgTag(ctx: *CirContext, name: @import("base").Ident.Idx) Error!Builder.Value {
    _ = ctx;
    _ = name;
    // TODO: Create zero-arg tag (just discriminant, no payload)
    return error.UnsupportedExpression;
}

// ============================================================
// Operation Emission
// ============================================================

fn emitBinop(ctx: *CirContext, binop: CIR.Expr.Binop) Error!Builder.Value {
    // Get the left and right expressions
    const lhs_expr = ctx.module_env.store.getExpr(binop.left);
    const rhs_expr = ctx.module_env.store.getExpr(binop.right);

    // Emit values for both operands
    const lhs = try emitExpr(ctx, lhs_expr);
    const rhs = try emitExpr(ctx, rhs_expr);

    // Emit the operation based on the operator
    return switch (binop.op) {
        .add => ctx.emitter.emitAdd(lhs, rhs) catch return error.OutOfMemory,
        .sub => ctx.emitter.emitSub(lhs, rhs) catch return error.OutOfMemory,
        .mul => ctx.emitter.emitMul(lhs, rhs) catch return error.OutOfMemory,
        .div => ctx.emitter.emitSDiv(lhs, rhs) catch return error.OutOfMemory,
        .eq => ctx.emitter.emitICmpEq(lhs, rhs) catch return error.OutOfMemory,
        .neq => ctx.emitter.emitICmpNe(lhs, rhs) catch return error.OutOfMemory,
        .lt => ctx.emitter.emitICmpSlt(lhs, rhs) catch return error.OutOfMemory,
        .lte => ctx.emitter.emitICmpSle(lhs, rhs) catch return error.OutOfMemory,
        .gt => ctx.emitter.emitICmpSgt(lhs, rhs) catch return error.OutOfMemory,
        .gte => ctx.emitter.emitICmpSge(lhs, rhs) catch return error.OutOfMemory,
        .@"and" => ctx.emitter.emitAnd(lhs, rhs) catch return error.OutOfMemory,
        .@"or" => ctx.emitter.emitOr(lhs, rhs) catch return error.OutOfMemory,
        else => return error.UnsupportedExpression,
    };
}

fn emitUnaryMinus(ctx: *CirContext, unary: CIR.Expr.UnaryMinus) Error!Builder.Value {
    const expr = ctx.module_env.store.getExpr(unary.expr);
    const val = try emitExpr(ctx, expr);

    // Negate: 0 - val
    const zero = ctx.emitter.emitIntConst(.i64, 0) catch return error.OutOfMemory;
    return ctx.emitter.emitSub(zero, val) catch return error.OutOfMemory;
}

fn emitUnaryNot(ctx: *CirContext, unary: CIR.Expr.UnaryNot) Error!Builder.Value {
    const expr = ctx.module_env.store.getExpr(unary.expr);
    const val = try emitExpr(ctx, expr);

    // Boolean not: XOR with true (1)
    const true_val = ctx.emitter.emitIntConst(.i1, 1) catch return error.OutOfMemory;
    return ctx.emitter.emitXor(val, true_val) catch return error.OutOfMemory;
}

fn emitDotAccess(ctx: *CirContext, access: anytype) Error!Builder.Value {
    _ = ctx;
    _ = access;
    // TODO: Field access via GEP
    return error.UnsupportedExpression;
}

// ============================================================
// Special Expression Emission
// ============================================================

fn emitDbg(ctx: *CirContext, expr_idx: CIR.Expr.Idx) Error!Builder.Value {
    // dbg just returns the expression value (side effect is printing)
    const expr = ctx.module_env.store.getExpr(expr_idx);
    return try emitExpr(ctx, expr);
}

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
const types = @import("types");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Layout = layout_mod.Layout;
const Store = layout_mod.Store;
const TypeScope = types.TypeScope;

/// Number of decimal places used for Dec (fixed-point decimal) representation.
/// Dec is stored as i128 scaled by 10^DEC_PRECISION.
pub const DEC_PRECISION: u5 = 18;

/// Scale factor for Dec values: 10^18
pub const DEC_SCALE: i128 = std.math.pow(i128, 10, DEC_PRECISION);

/// Errors during CIR translation
pub const Error = error{
    OutOfMemory,
    NoActiveFunction,
    UnsupportedExpression,
    VariableNotFound,
    TypeMismatch,
} || layout_mod.LayoutError;

/// Context for CIR translation
pub const CirContext = struct {
    allocator: Allocator,
    emitter: *emit.LlvmEmitter,
    module_env: *ModuleEnv,
    layout_store: *Store, // mutable: addTypeVar caches results
    type_scope: *const TypeScope, // needed for layout computation
    builtin_ctx: *builtins_mod.BuiltinContext,
    refcount_ctx: *refcount.RefcountContext,
    /// Counter for generating unique lambda names
    lambda_counter: u32,

    pub fn init(
        allocator: Allocator,
        emitter: *emit.LlvmEmitter,
        module_env: *ModuleEnv,
        layout_store: *Store,
        type_scope: *const TypeScope,
        builtin_ctx: *builtins_mod.BuiltinContext,
        refcount_ctx: *refcount.RefcountContext,
    ) CirContext {
        return .{
            .allocator = allocator,
            .emitter = emitter,
            .module_env = module_env,
            .layout_store = layout_store,
            .type_scope = type_scope,
            .builtin_ctx = builtin_ctx,
            .refcount_ctx = refcount_ctx,
            .lambda_counter = 0,
        };
    }

    /// Generate a unique name for a lambda function
    pub fn nextLambdaName(self: *CirContext) []const u8 {
        const name = std.fmt.allocPrint(self.allocator, "roc_lambda_{d}", .{self.lambda_counter}) catch "roc_lambda";
        self.lambda_counter += 1;
        return name;
    }

    /// Get the Layout for a CIR expression index
    pub fn getExprLayout(self: *CirContext, expr_idx: CIR.Expr.Idx) Error!Layout {
        const type_var = ModuleEnv.varFrom(expr_idx);
        const layout_idx = try self.layout_store.addTypeVar(type_var, self.type_scope);
        return self.layout_store.get(layout_idx);
    }

    /// Get the LLVM type for a CIR expression index
    pub fn getExprLlvmType(self: *CirContext, expr_idx: CIR.Expr.Idx) Error!Builder.Type {
        const layout_val = try self.getExprLayout(expr_idx);
        return layout_types.layoutToLlvmType(self.emitter.builder, self.layout_store, layout_val);
    }

    /// Get the Layout for a CIR pattern index
    pub fn getPatternLayout(self: *CirContext, pattern_idx: CIR.Pattern.Idx) Error!Layout {
        const type_var = ModuleEnv.varFrom(pattern_idx);
        const layout_idx = try self.layout_store.addTypeVar(type_var, self.type_scope);
        return self.layout_store.get(layout_idx);
    }

    /// Get the LLVM type for a CIR pattern index
    pub fn getPatternLlvmType(self: *CirContext, pattern_idx: CIR.Pattern.Idx) Error!Builder.Type {
        const layout_val = try self.getPatternLayout(pattern_idx);
        return layout_types.layoutToLlvmType(self.emitter.builder, self.layout_store, layout_val);
    }
};

/// Translate a CIR expression to LLVM IR.
/// Returns the LLVM value representing the expression's result.
pub fn emitExpr(ctx: *CirContext, expr: CIR.Expr) Error!Builder.Value {
    return switch (expr) {
        // Numeric Literals
        .e_num => |num| try emitNumber(ctx, num.value, num.kind),
        .e_frac_f32 => |frac| try emitFloat32(ctx, frac.value),
        .e_frac_f64 => |frac| try emitFloat64(ctx, frac.value),
        .e_dec => |dec| try emitDec(ctx, dec.value.num),
        .e_dec_small => |dec| try emitDecSmall(ctx, dec.value),

        // String Literals
        .e_str_segment => |seg| try emitStrSegment(ctx, seg.literal),
        .e_str => |str_expr| try emitStr(ctx, str_expr.span),

        // Collections
        .e_list => |list| try emitList(ctx, list.elems),
        .e_empty_list => try emitEmptyList(ctx),
        .e_tuple => |tuple| try emitTuple(ctx, tuple.elems),
        .e_record => |record| try emitRecord(ctx, record.fields, record.ext),
        .e_empty_record => try emitEmptyRecord(ctx),

        // Variables
        .e_lookup_local => |lookup| try emitLocalLookup(ctx, lookup.pattern_idx),
        .e_lookup_external => |lookup| try emitExternalLookup(ctx, lookup),
        .e_lookup_required => return error.UnsupportedExpression, // Platform-specific

        // Tags
        .e_tag => |tag| try emitTag(ctx, tag.name, tag.args),
        .e_zero_argument_tag => |tag| try emitZeroArgTag(ctx, tag.name),
        .e_nominal => return error.UnsupportedExpression, // TODO
        .e_nominal_external => return error.UnsupportedExpression, // TODO

        // Operations
        .e_binop => |binop| try emitBinop(ctx, binop),
        .e_unary_minus => |unary| try emitUnaryMinus(ctx, unary),
        .e_unary_not => |unary| try emitUnaryNot(ctx, unary),
        .e_dot_access => |access| try emitDotAccess(ctx, access),

        // Control Flow
        .e_if => |if_expr| try emitIf(ctx, if_expr.branches, if_expr.final_else),
        .e_match => |match_expr| try emitMatch(ctx, match_expr),
        .e_block => |block| try emitBlock(ctx, block.stmts, block.final_expr),

        // Functions
        .e_call => |call| try emitCall(ctx, call.func, call.args),
        .e_lambda => |lambda| try emitLambda(ctx, lambda),
        .e_closure => |closure| try emitClosure(ctx, closure),

        // Special Expressions
        .e_runtime_error => |err| try emitRuntimeError(ctx, err.diagnostic),
        .e_crash => |crash| try emitCrash(ctx, crash.msg),
        .e_dbg => |dbg| try emitDbg(ctx, dbg.expr),
        .e_expect => |expect| try emitExpect(ctx, expect.body),
        .e_ellipsis => return error.UnsupportedExpression,
        .e_anno_only => return error.UnsupportedExpression,
        .e_return => |ret| try emitReturn(ctx, ret.expr),
        .e_type_var_dispatch => return error.UnsupportedExpression,
    };
}

// Numeric Literal Emission

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
    // Dec uses DEC_PRECISION decimal places, so we need to scale by 10^(DEC_PRECISION - given_power)
    const scale_factor = std.math.pow(i128, 10, DEC_PRECISION - value.denominator_power_of_ten);
    const scaled_value = @as(i128, value.numerator) * scale_factor;
    return ctx.emitter.emitIntConst(.i128, scaled_value) catch return error.OutOfMemory;
}

// String Emission

fn emitStrSegment(ctx: *CirContext, literal_idx: @import("base").StringLiteral.Idx) Error!Builder.Value {
    // Get the string literal from the module env
    const string_literal = ctx.module_env.common.string_literals.get(@intFromEnum(literal_idx));
    const str_bytes = string_literal.resolve(ctx.module_env.common.source);

    // Create a global string constant
    // For now, return a pointer to the string data
    // TODO: Proper RocStr representation
    return ctx.emitter.emitStringConst(str_bytes) catch return error.OutOfMemory;
}

fn emitStr(_: *CirContext, _: CIR.Expr.Span) Error!Builder.Value {
    // TODO: String interpolation
    return error.UnsupportedExpression;
}

// Collection Emission

fn emitList(_: *CirContext, _: CIR.Expr.Span) Error!Builder.Value {
    // TODO: Allocate list, populate elements, return RocList struct
    return error.UnsupportedExpression;
}

fn emitEmptyList(_: *CirContext) Error!Builder.Value {
    // TODO: Empty list is { null, 0, 0 }
    return error.UnsupportedExpression;
}

fn emitTuple(_: *CirContext, _: CIR.Expr.Span) Error!Builder.Value {
    // TODO: Tuple emission
    return error.UnsupportedExpression;
}

fn emitRecord(_: *CirContext, _: CIR.RecordField.Span, _: ?CIR.Expr.Idx) Error!Builder.Value {
    // TODO: Record emission
    return error.UnsupportedExpression;
}

fn emitEmptyRecord(_: *CirContext) Error!Builder.Value {
    // TODO: Empty record is represented as an empty struct or unit
    return error.UnsupportedExpression;
}

// Variable Emission

fn emitLocalLookup(ctx: *CirContext, pattern_idx: CIR.Pattern.Idx) Error!Builder.Value {
    // Look up the pattern to get the variable name
    const pattern = ctx.module_env.store.getPattern(pattern_idx);

    // Get the variable name based on pattern type
    // Only patterns that bind a single identifier can be looked up
    const name = switch (pattern) {
        .assign => |assign| ctx.module_env.getIdent(assign.ident),
        .as => |as_pattern| ctx.module_env.getIdent(as_pattern.ident),
        // Patterns that don't bind to a single name (underscore, destructures, etc.)
        // cannot be directly looked up - this indicates a compiler bug if reached
        .underscore => return error.VariableNotFound, // _ doesn't bind
        .record_destructure, .tuple, .list, .applied_tag => {
            // Destructuring patterns bind multiple names; lookup should be by child pattern
            return error.UnsupportedExpression;
        },
        else => return error.UnsupportedExpression,
    };

    // Look up in current scope
    if (ctx.emitter.lookupVar(name)) |scoped_val| {
        // If the value is stored on the stack (is_ptr), load it
        if (scoped_val.is_ptr) {
            return ctx.emitter.emitLoad(scoped_val.llvm_type, scoped_val.value) catch return error.OutOfMemory;
        }
        return scoped_val.value;
    }

    return error.VariableNotFound;
}

fn emitExternalLookup(_: *CirContext, _: anytype) Error!Builder.Value {
    // TODO: Look up in imported module
    return error.UnsupportedExpression;
}

// Tag Emission

fn emitTag(_: *CirContext, _: @import("base").Ident.Idx, _: CIR.Expr.Span) Error!Builder.Value {
    // TODO: Create tag union value
    return error.UnsupportedExpression;
}

fn emitZeroArgTag(_: *CirContext, _: @import("base").Ident.Idx) Error!Builder.Value {
    // TODO: Create zero-arg tag (just discriminant, no payload)
    return error.UnsupportedExpression;
}

// Operation Emission

fn emitBinop(ctx: *CirContext, binop: CIR.Expr.Binop) Error!Builder.Value {
    // Get the left and right expressions
    const lhs_expr = ctx.module_env.store.getExpr(binop.left);
    const rhs_expr = ctx.module_env.store.getExpr(binop.right);

    // Emit values for both operands
    const lhs = try emitExpr(ctx, lhs_expr);
    const rhs = try emitExpr(ctx, rhs_expr);

    // Get the layout to determine if this is a float operation
    const lhs_layout = try ctx.getExprLayout(binop.left);
    const is_float = lhs_layout.tag == .scalar and lhs_layout.data.scalar.tag == .frac;

    // Emit the operation based on the operator and operand type
    return switch (binop.op) {
        .add => if (is_float)
            ctx.emitter.emitFAdd(lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitAdd(lhs, rhs) catch return error.OutOfMemory,
        .sub => if (is_float)
            ctx.emitter.emitFSub(lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitSub(lhs, rhs) catch return error.OutOfMemory,
        .mul => if (is_float)
            ctx.emitter.emitFMul(lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitMul(lhs, rhs) catch return error.OutOfMemory,
        .div => if (is_float)
            ctx.emitter.emitFDiv(lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitSDiv(lhs, rhs) catch return error.OutOfMemory,
        .eq => if (is_float)
            ctx.emitter.emitFCmp(.oeq, lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitICmpEq(lhs, rhs) catch return error.OutOfMemory,
        .neq => if (is_float)
            ctx.emitter.emitFCmp(.one, lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitICmpNe(lhs, rhs) catch return error.OutOfMemory,
        .lt => if (is_float)
            ctx.emitter.emitFCmp(.olt, lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitICmpSlt(lhs, rhs) catch return error.OutOfMemory,
        .lte => if (is_float)
            ctx.emitter.emitFCmp(.ole, lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitICmpSle(lhs, rhs) catch return error.OutOfMemory,
        .gt => if (is_float)
            ctx.emitter.emitFCmp(.ogt, lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitICmpSgt(lhs, rhs) catch return error.OutOfMemory,
        .gte => if (is_float)
            ctx.emitter.emitFCmp(.oge, lhs, rhs) catch return error.OutOfMemory
        else
            ctx.emitter.emitICmpSge(lhs, rhs) catch return error.OutOfMemory,
        .@"and" => ctx.emitter.emitAnd(lhs, rhs) catch return error.OutOfMemory,
        .@"or" => ctx.emitter.emitOr(lhs, rhs) catch return error.OutOfMemory,
        else => return error.UnsupportedExpression,
    };
}

fn emitUnaryMinus(ctx: *CirContext, unary: CIR.Expr.UnaryMinus) Error!Builder.Value {
    const expr = ctx.module_env.store.getExpr(unary.expr);
    const val = try emitExpr(ctx, expr);

    // Get the layout to determine if this is a float operation
    const layout = try ctx.getExprLayout(unary.expr);

    if (layout.tag == .scalar and layout.data.scalar.tag == .frac) {
        // Float negation: 0.0 - val
        const zero = switch (layout.data.scalar.data.frac) {
            .f32 => ctx.emitter.emitF32Const(0.0) catch return error.OutOfMemory,
            .f64, .dec => ctx.emitter.emitF64Const(0.0) catch return error.OutOfMemory,
        };
        return ctx.emitter.emitFSub(zero, val) catch return error.OutOfMemory;
    } else {
        // Integer negation: 0 - val
        const llvm_type = try ctx.getExprLlvmType(unary.expr);
        const zero = ctx.emitter.emitIntConst(llvm_type, 0) catch return error.OutOfMemory;
        return ctx.emitter.emitSub(zero, val) catch return error.OutOfMemory;
    }
}

fn emitUnaryNot(ctx: *CirContext, unary: CIR.Expr.UnaryNot) Error!Builder.Value {
    const expr = ctx.module_env.store.getExpr(unary.expr);
    const val = try emitExpr(ctx, expr);

    // Boolean not: XOR with true (1)
    const true_val = ctx.emitter.emitIntConst(.i1, 1) catch return error.OutOfMemory;
    return ctx.emitter.emitXor(val, true_val) catch return error.OutOfMemory;
}

fn emitDotAccess(_: *CirContext, _: anytype) Error!Builder.Value {
    // TODO: Field access via GEP
    return error.UnsupportedExpression;
}

// Special Expression Emission

fn emitDbg(ctx: *CirContext, expr_idx: CIR.Expr.Idx) Error!Builder.Value {
    // dbg just returns the expression value (side effect is printing)
    const expr = ctx.module_env.store.getExpr(expr_idx);
    return try emitExpr(ctx, expr);
}

/// Emit a runtime error expression.
/// These are inserted when the compiler encounters semantic errors.
/// At runtime, this calls roc_crashed with an error message.
fn emitRuntimeError(ctx: *CirContext, diagnostic_idx: CIR.Diagnostic.Idx) Error!Builder.Value {
    // Get RocOps pointer from refcount context
    const roc_ops = ctx.refcount_ctx.roc_ops_ptr orelse {
        // If no RocOps, just emit unreachable
        const wip = ctx.emitter.wip_function orelse return error.NoActiveFunction;
        _ = wip.@"unreachable"() catch return error.OutOfMemory;
        // Return a dummy value (this code is unreachable)
        return ctx.emitter.emitIntConst(.i64, 0) catch return error.OutOfMemory;
    };

    // Get the diagnostic to build a more informative message
    const diagnostic = ctx.module_env.store.getDiagnostic(diagnostic_idx);

    // Build error message based on diagnostic type
    const msg = switch (diagnostic) {
        .not_implemented => "Runtime error: Feature not yet implemented",
        .exposed_but_not_implemented => "Runtime error: Exposed value not implemented",
        .ident_not_in_scope => "Runtime error: Identifier not in scope",
        .invalid_num_literal => "Runtime error: Invalid numeric literal",
        .invalid_string_interpolation => "Runtime error: Invalid string interpolation",
        .if_expr_without_else => "Runtime error: If expression missing else branch",
        .type_mismatch => "Runtime error: Type mismatch",
        else => "Runtime error: Compilation error encountered",
    };

    const msg_ptr = ctx.emitter.emitStringConst(msg) catch return error.OutOfMemory;
    const msg_len = ctx.emitter.emitIntConst(.i64, @intCast(msg.len)) catch return error.OutOfMemory;

    // Call roc_crashed via builtin
    _ = builtins_mod.emitBuiltinCall(
        ctx.builtin_ctx,
        ctx.emitter,
        "roc_builtins_crash",
        .void,
        &.{ .ptr, .i64, .ptr },
        &.{ msg_ptr, msg_len, roc_ops },
    ) catch return error.OutOfMemory;

    // Emit unreachable (crash never returns)
    const wip = ctx.emitter.wip_function orelse return error.NoActiveFunction;
    _ = wip.@"unreachable"() catch return error.OutOfMemory;

    // Return a dummy value (this code is unreachable)
    return ctx.emitter.emitIntConst(.i64, 0) catch return error.OutOfMemory;
}

/// Emit a crash expression.
/// This terminates execution with a user-provided message.
fn emitCrash(ctx: *CirContext, msg_idx: @import("base").StringLiteral.Idx) Error!Builder.Value {
    // Get the crash message
    const string_literal = ctx.module_env.common.string_literals.get(@intFromEnum(msg_idx));
    const msg = string_literal.resolve(ctx.module_env.common.source);

    // Get RocOps pointer
    const roc_ops = ctx.refcount_ctx.roc_ops_ptr orelse {
        const wip = ctx.emitter.wip_function orelse return error.NoActiveFunction;
        _ = wip.@"unreachable"() catch return error.OutOfMemory;
        return ctx.emitter.emitIntConst(.i64, 0) catch return error.OutOfMemory;
    };

    // Create message string constant
    const msg_ptr = ctx.emitter.emitStringConst(msg) catch return error.OutOfMemory;
    const msg_len = ctx.emitter.emitIntConst(.i64, @intCast(msg.len)) catch return error.OutOfMemory;

    // Call roc_crashed
    _ = builtins_mod.emitBuiltinCall(
        ctx.builtin_ctx,
        ctx.emitter,
        "roc_builtins_crash",
        .void,
        &.{ .ptr, .i64, .ptr },
        &.{ msg_ptr, msg_len, roc_ops },
    ) catch return error.OutOfMemory;

    // Emit unreachable
    const wip = ctx.emitter.wip_function orelse return error.NoActiveFunction;
    _ = wip.@"unreachable"() catch return error.OutOfMemory;

    return ctx.emitter.emitIntConst(.i64, 0) catch return error.OutOfMemory;
}

/// Emit an expect expression.
/// Evaluates the body and crashes if it returns false.
fn emitExpect(ctx: *CirContext, body_idx: CIR.Expr.Idx) Error!Builder.Value {
    const wip = ctx.emitter.wip_function orelse return error.NoActiveFunction;

    // Evaluate the expectation body (should return bool)
    const body_expr = ctx.module_env.store.getExpr(body_idx);
    const condition = try emitExpr(ctx, body_expr);

    // Create blocks for pass and fail cases
    const pass_block = wip.block(0, "expect.pass") catch return error.OutOfMemory;
    const fail_block = wip.block(0, "expect.fail") catch return error.OutOfMemory;

    // Branch based on condition
    _ = wip.brCond(condition, pass_block, fail_block, "") catch return error.OutOfMemory;

    // Emit fail block
    wip.cursor = .{ .block = fail_block };

    // Get RocOps pointer
    if (ctx.refcount_ctx.roc_ops_ptr) |roc_ops| {
        // Create failure message
        const msg = "Expectation failed";
        const msg_ptr = ctx.emitter.emitStringConst(msg) catch return error.OutOfMemory;
        const msg_len = ctx.emitter.emitIntConst(.i64, @intCast(msg.len)) catch return error.OutOfMemory;

        // Call roc_expect_failed
        _ = builtins_mod.emitBuiltinCall(
            ctx.builtin_ctx,
            ctx.emitter,
            "roc_builtins_expect_failed",
            .void,
            &.{ .ptr, .i64, .ptr },
            &.{ msg_ptr, msg_len, roc_ops },
        ) catch return error.OutOfMemory;
    }

    // Emit unreachable in fail block
    _ = wip.@"unreachable"() catch return error.OutOfMemory;

    // Continue in pass block
    wip.cursor = .{ .block = pass_block };

    // Return empty record (expect evaluates to {})
    return ctx.emitter.emitIntConst(.i1, 0) catch return error.OutOfMemory;
}

/// Emit a return expression.
/// This returns a value from the enclosing function.
fn emitReturn(ctx: *CirContext, expr_idx: CIR.Expr.Idx) Error!Builder.Value {
    // Evaluate the return value
    const expr = ctx.module_env.store.getExpr(expr_idx);
    const value = try emitExpr(ctx, expr);

    // Emit return instruction
    ctx.emitter.emitRet(value) catch return error.OutOfMemory;

    // Return a dummy value (this code is unreachable after return)
    return ctx.emitter.emitIntConst(.i64, 0) catch return error.OutOfMemory;
}

// Control Flow Emission

/// Emit an if expression with multiple branches and a final else.
///
/// LLVM structure:
/// ```
///   entry:
///     %cond1 = <evaluate first condition>
///     br i1 %cond1, label %then1, label %check2
///   then1:
///     %val1 = <evaluate first branch body>
///     br label %merge
///   check2:
///     %cond2 = <evaluate second condition>
///     br i1 %cond2, label %then2, label %else
///   then2:
///     %val2 = <evaluate second branch body>
///     br label %merge
///   else:
///     %else_val = <evaluate else body>
///     br label %merge
///   merge:
///     %result = phi [%val1, %then1], [%val2, %then2], [%else_val, %else]
/// ```
fn emitIf(
    ctx: *CirContext,
    branches: CIR.Expr.IfBranch.Span,
    final_else_idx: CIR.Expr.Idx,
) Error!Builder.Value {
    const wip = ctx.emitter.wip_function orelse return error.NoActiveFunction;
    const builder = ctx.emitter.builder;

    // Create the merge block where all branches converge
    const merge_block = wip.block(0, "if.merge") catch return error.OutOfMemory;

    // Track incoming values for the phi node
    var phi_incoming = std.ArrayList(struct { value: Builder.Value, block: Builder.WipFunction.Block.Index }).init(ctx.allocator);
    defer phi_incoming.deinit();

    // Get branch count from span
    const branch_count = branches.count;
    var branch_idx: u32 = 0;

    // Process each if/elif branch
    var iter = ctx.module_env.store.if_branches.iterate(branches);
    while (iter.next()) |branch| {
        const is_last = branch_idx == branch_count - 1;

        // Create blocks for this branch
        const then_block = wip.block(0, "if.then") catch return error.OutOfMemory;
        const next_block = if (is_last)
            merge_block
        else
            wip.block(0, "if.elif") catch return error.OutOfMemory;

        // Evaluate condition
        const cond_expr = ctx.module_env.store.getExpr(branch.condition);
        const cond_val = try emitExpr(ctx, cond_expr);

        // Branch based on condition
        _ = wip.brCond(cond_val, then_block, next_block, "") catch return error.OutOfMemory;

        // Emit then block
        wip.cursor = .{ .block = then_block };
        const body_expr = ctx.module_env.store.getExpr(branch.body);
        const body_val = try emitExpr(ctx, body_expr);

        // Record value for phi node
        phi_incoming.append(.{ .value = body_val, .block = then_block }) catch return error.OutOfMemory;

        // Jump to merge
        _ = wip.br(merge_block, "") catch return error.OutOfMemory;

        // Position at next check block (or merge if last)
        if (!is_last) {
            wip.cursor = .{ .block = next_block };
        }

        branch_idx += 1;
    }

    // Emit else block (we're positioned at the last check block or merge)
    // If there were no branches, we need to handle that case
    if (branch_count == 0) {
        // No branches, just emit the else
        const else_expr = ctx.module_env.store.getExpr(final_else_idx);
        return try emitExpr(ctx, else_expr);
    }

    // Create else block
    const else_block = wip.block(0, "if.else") catch return error.OutOfMemory;
    wip.cursor = .{ .block = else_block };

    const else_expr = ctx.module_env.store.getExpr(final_else_idx);
    const else_val = try emitExpr(ctx, else_expr);

    phi_incoming.append(.{ .value = else_val, .block = else_block }) catch return error.OutOfMemory;

    _ = wip.br(merge_block, "") catch return error.OutOfMemory;

    // Position at merge block and create phi node
    wip.cursor = .{ .block = merge_block };

    // Determine result type from the first value
    const result_type = phi_incoming.items[0].value.typeOf(wip.function, builder);

    // Create phi node
    const phi = wip.phi(result_type, "") catch return error.OutOfMemory;

    // Add incoming values
    for (phi_incoming.items) |incoming| {
        wip.addPhiArg(phi, incoming.block, incoming.value) catch return error.OutOfMemory;
    }

    return phi;
}

/// Emit a match expression.
///
/// Match expressions compile to a series of conditional checks and branches,
/// similar to if expressions but with pattern matching logic.
///
/// For simple patterns (literals, wildcards), this generates direct comparisons.
/// For complex patterns (destructuring), it generates extraction code.
fn emitMatch(ctx: *CirContext, match_expr: CIR.Expr.Match) Error!Builder.Value {
    const wip = ctx.emitter.wip_function orelse return error.NoActiveFunction;
    const builder = ctx.emitter.builder;

    // Evaluate the match subject
    const subject_expr = ctx.module_env.store.getExpr(match_expr.subject);
    const subject_val = try emitExpr(ctx, subject_expr);

    // Create the merge block where all branches converge
    const merge_block = wip.block(0, "match.merge") catch return error.OutOfMemory;

    // Track incoming values for the phi node
    var phi_incoming = std.ArrayList(struct { value: Builder.Value, block: Builder.WipFunction.Block.Index }).init(ctx.allocator);
    defer phi_incoming.deinit();

    // Get branches
    const branches = match_expr.branches;
    const branch_count = branches.count;

    // Process each branch
    var branch_idx: u32 = 0;
    var iter = ctx.module_env.store.match_branches.iterate(branches);

    while (iter.next()) |branch| {
        const is_last = branch_idx == branch_count - 1;

        // Create blocks for this branch
        const body_block = wip.block(0, "match.body") catch return error.OutOfMemory;
        const next_block = if (is_last)
            // Last branch with no next - this should be exhaustive so use unreachable
            wip.block(0, "match.unreachable") catch return error.OutOfMemory
        else
            wip.block(0, "match.next") catch return error.OutOfMemory;

        // Generate pattern check
        const pattern = ctx.module_env.store.getPattern(branch.pattern);
        const matches = try emitPatternCheck(ctx, subject_val, match_expr.subject, pattern);

        // Handle optional guard
        const final_cond = if (branch.guard) |guard_idx| blk: {
            // Guard: check && guard_expr
            const guard_expr = ctx.module_env.store.getExpr(guard_idx);
            const guard_val = try emitExpr(ctx, guard_expr);
            break :blk ctx.emitter.emitAnd(matches, guard_val) catch return error.OutOfMemory;
        } else matches;

        // Branch based on pattern match
        _ = wip.brCond(final_cond, body_block, next_block, "") catch return error.OutOfMemory;

        // Emit body block
        wip.cursor = .{ .block = body_block };

        // Push scope for pattern bindings
        ctx.emitter.pushScope() catch return error.OutOfMemory;

        // Bind pattern variables
        try bindPatternToValue(ctx, branch.pattern, subject_val);

        // Emit body expression
        const body_expr = ctx.module_env.store.getExpr(branch.body);
        const body_val = try emitExpr(ctx, body_expr);

        // Emit decrefs for scope values and pop scope
        try emitScopeCleanup(ctx);

        // Record value for phi node
        phi_incoming.append(.{ .value = body_val, .block = body_block }) catch return error.OutOfMemory;

        // Jump to merge
        _ = wip.br(merge_block, "") catch return error.OutOfMemory;

        // Position at next block
        wip.cursor = .{ .block = next_block };

        branch_idx += 1;
    }

    // If we get to the last "next" block, it's unreachable (exhaustive match)
    _ = wip.@"unreachable"() catch return error.OutOfMemory;

    // Position at merge block and create phi node
    wip.cursor = .{ .block = merge_block };

    if (phi_incoming.items.len == 0) {
        // No branches - this shouldn't happen in valid Roc code
        return error.UnsupportedExpression;
    }

    // Determine result type from the first value
    const result_type = phi_incoming.items[0].value.typeOf(wip.function, builder);

    // Create phi node
    const phi = wip.phi(result_type, "") catch return error.OutOfMemory;

    // Add incoming values
    for (phi_incoming.items) |incoming| {
        wip.addPhiArg(phi, incoming.block, incoming.value) catch return error.OutOfMemory;
    }

    return phi;
}

/// Generate code to check if a value matches a pattern.
/// Returns a boolean (i1) value.
fn emitPatternCheck(
    ctx: *CirContext,
    subject_val: Builder.Value,
    subject_expr_idx: CIR.Expr.Idx,
    pattern: CIR.Pattern,
) Error!Builder.Value {
    return switch (pattern) {
        .assign => ctx.emitter.emitBoolConst(true) catch return error.OutOfMemory,
        .underscore => ctx.emitter.emitBoolConst(true) catch return error.OutOfMemory,
        .num => |num| blk: {
            // Compare with numeric literal using subject's type
            const subject_type = try ctx.getExprLlvmType(subject_expr_idx);
            const lit_val = try emitNumber(ctx, num.value, subject_type);
            break :blk ctx.emitter.emitICmpEq(subject_val, lit_val) catch return error.OutOfMemory;
        },
        .str_literal => {
            // TODO: String comparison needs string comparison builtin
            return error.UnsupportedExpression;
        },
        .applied_tag => {
            // TODO: Check if discriminant matches (extract and compare)
            return error.UnsupportedExpression;
        },
        .record_destructure => ctx.emitter.emitBoolConst(true) catch return error.OutOfMemory,
        .tuple => ctx.emitter.emitBoolConst(true) catch return error.OutOfMemory,
        .list => {
            // TODO: List patterns need length check
            return error.UnsupportedExpression;
        },
        else => return error.UnsupportedExpression,
    };
}

// Function Emission

/// Emit a function call expression.
///
/// Handles both direct calls (function reference) and indirect calls (closure).
fn emitCall(
    ctx: *CirContext,
    func_idx: CIR.Expr.Idx,
    args: CIR.Expr.Span,
) Error!Builder.Value {
    // Evaluate the function expression to get the callee
    const func_expr = ctx.module_env.store.getExpr(func_idx);
    const callee = try emitExpr(ctx, func_expr);

    // Get the argument expression indices for type lookup
    const arg_indices = ctx.module_env.store.exprSlice(args);

    // Evaluate all arguments and collect their types
    var arg_values = std.ArrayList(Builder.Value).init(ctx.allocator);
    defer arg_values.deinit();

    var param_types = std.ArrayList(Builder.Type).init(ctx.allocator);
    defer param_types.deinit();

    for (arg_indices) |arg_idx| {
        const arg_expr = ctx.module_env.store.getExpr(arg_idx);
        const arg_val = try emitExpr(ctx, arg_expr);
        arg_values.append(arg_val) catch return error.OutOfMemory;

        // Get the LLVM type for this argument
        const arg_type = try ctx.getExprLlvmType(arg_idx);
        param_types.append(arg_type) catch return error.OutOfMemory;
    }

    // Get the return type from the function call expression itself
    const return_type = try ctx.getExprLlvmType(func_idx);

    const fn_type = ctx.emitter.createFunctionType(return_type, param_types.items) catch return error.OutOfMemory;

    // Emit the call
    return ctx.emitter.emitCall(fn_type, callee, arg_values.items) catch return error.OutOfMemory;
}

/// Emit a lambda expression (pure function without captures).
///
/// Creates a new LLVM function and returns a pointer to it.
fn emitLambda(ctx: *CirContext, lambda: CIR.Expr.Lambda) Error!Builder.Value {
    const builder = ctx.emitter.builder;

    // Create a unique name for this lambda
    const lambda_name = ctx.nextLambdaName();

    // Get parameter pattern indices for type lookup
    const param_indices = ctx.module_env.store.slicePatterns(lambda.params);

    // Build parameter types from the lambda's parameter patterns
    var param_types = std.ArrayList(Builder.Type).init(ctx.allocator);
    defer param_types.deinit();

    for (param_indices) |param_idx| {
        const param_type = try ctx.getPatternLlvmType(param_idx);
        param_types.append(param_type) catch return error.OutOfMemory;
    }

    // Get return type from the lambda body expression
    const return_type = try ctx.getExprLlvmType(lambda.body);

    // Create function type
    const fn_type = ctx.emitter.createFunctionType(return_type, param_types.items) catch return error.OutOfMemory;

    // Add the function to the module
    const fn_idx = ctx.emitter.addFunction(lambda_name, fn_type) catch return error.OutOfMemory;

    // Save current WIP function (we'll restore it after)
    const saved_wip = ctx.emitter.wip_function;

    // Begin building the lambda body
    ctx.emitter.beginFunction(fn_idx) catch return error.OutOfMemory;

    // Push a scope for the lambda body
    ctx.emitter.pushScope() catch return error.OutOfMemory;

    // Bind parameters to their names with proper types
    for (param_indices, 0..) |pattern_idx, param_idx| {
        const pattern = ctx.module_env.store.getPattern(pattern_idx);
        switch (pattern) {
            .assign => |assign| {
                const name = ctx.module_env.getIdent(assign.ident);
                const wip = ctx.emitter.wip_function.?;
                const param_val = wip.arg(@intCast(param_idx));
                const param_type = try ctx.getPatternLlvmType(pattern_idx);
                const scoped = emit.ScopedValue.simple(param_val, param_type);
                ctx.emitter.defineVar(name, scoped) catch return error.OutOfMemory;
            },
            else => {
                // Complex patterns need Phase 9
            },
        }
    }

    // Emit the lambda body
    const body_expr = ctx.module_env.store.getExpr(lambda.body);
    const result = try emitExpr(ctx, body_expr);

    // Return the result
    ctx.emitter.emitRet(result) catch return error.OutOfMemory;

    // Emit decrefs for scope values and pop scope, then finish function
    try emitScopeCleanup(ctx);
    ctx.emitter.endFunction() catch return error.OutOfMemory;

    // Restore the previous WIP function
    ctx.emitter.wip_function = saved_wip;

    // Return a pointer to the function
    return fn_idx.toValue(builder);
}

/// Emit a closure expression (lambda with captured variables).
///
/// Creates a closure struct containing the function pointer and captured environment.
/// The struct layout is: { fn_ptr: *fn, capture1: T1, capture2: T2, ... }
///
/// Closures are heap-allocated so they can safely escape their defining scope.
/// The memory is allocated with a refcount for proper memory management.
fn emitClosure(ctx: *CirContext, closure: CIR.Expr.Closure) Error!Builder.Value {
    // First, emit the underlying lambda
    const fn_value = try emitLambda(ctx, closure.lambda);

    // If there are no captures, just return the function pointer
    const captures_span = closure.captures;
    if (captures_span.span.len == 0) {
        return fn_value;
    }

    // Get the capture indices
    const capture_indices = ctx.module_env.store.sliceCaptures(captures_span);

    // Build closure struct type: { fn_ptr, capture1, capture2, ... }
    var field_types = std.ArrayList(Builder.Type).init(ctx.allocator);
    defer field_types.deinit();

    // Track size and check if any captured values are refcounted
    var closure_size: u64 = 8; // Start with function pointer size (8 bytes on 64-bit)
    var has_refcounted_captures = false;

    // First field is the function pointer
    field_types.append(.ptr) catch return error.OutOfMemory;

    // Remaining fields are the captured values' types
    for (capture_indices) |capture_idx| {
        const capture = ctx.module_env.store.getCapture(capture_idx);
        const capture_type = try ctx.getPatternLlvmType(capture.pattern_idx);
        field_types.append(capture_type) catch return error.OutOfMemory;

        // Compute size contribution of this field
        closure_size += getTypeSizeBytes(capture_type);

        // Check if this capture is refcounted (for proper decref)
        const capture_layout = try ctx.getPatternLayout(capture.pattern_idx);
        if (layout_types.isRefcounted(capture_layout)) {
            has_refcounted_captures = true;
        }
    }

    // Create the closure struct type
    const closure_type = ctx.emitter.builder.structType(.normal, field_types.items) catch return error.OutOfMemory;

    // Allocate the closure on the heap with refcount
    // This ensures closures can safely escape their defining scope
    const closure_ptr = blk: {
        if (ctx.refcount_ctx.roc_ops_ptr) |roc_ops| {
            // We have RocOps - allocate on the heap
            const size_val = ctx.emitter.emitIntConst(.i64, @intCast(closure_size)) catch return error.OutOfMemory;
            const align_val = ctx.emitter.emitIntConst(.i32, 8) catch return error.OutOfMemory; // Pointer alignment
            const refcounted_val = ctx.emitter.emitBoolConst(has_refcounted_captures) catch return error.OutOfMemory;

            break :blk builtins_mod.emitAllocateWithRefcount(
                ctx.builtin_ctx,
                ctx.emitter,
                size_val,
                align_val,
                refcounted_val,
                roc_ops,
            ) catch return error.OutOfMemory;
        } else {
            // No RocOps available (e.g., in REPL without full runtime)
            // Fall back to stack allocation - caller must ensure closure doesn't escape
            break :blk ctx.emitter.emitAlloca(closure_type) catch return error.OutOfMemory;
        }
    };

    // Store function pointer at index 0
    const fn_field_ptr = ctx.emitter.emitStructGep(closure_type, closure_ptr, 0) catch return error.OutOfMemory;
    ctx.emitter.emitStore(fn_value, fn_field_ptr) catch return error.OutOfMemory;

    // Store each captured value
    var field_idx: u32 = 1;
    for (capture_indices) |capture_idx| {
        const capture = ctx.module_env.store.getCapture(capture_idx);

        // Look up the captured variable's current value
        const name = ctx.module_env.getIdent(capture.name);
        const scoped_val = ctx.emitter.lookupVar(name) orelse
            return error.VariableNotFound;

        // Get the value (load if it's a pointer to a mutable var)
        const capture_val = if (scoped_val.is_ptr)
            ctx.emitter.emitLoad(scoped_val.llvm_type, scoped_val.value) catch return error.OutOfMemory
        else
            scoped_val.value;

        // Store into closure struct
        const field_ptr = ctx.emitter.emitStructGep(closure_type, closure_ptr, field_idx) catch return error.OutOfMemory;
        ctx.emitter.emitStore(capture_val, field_ptr) catch return error.OutOfMemory;

        field_idx += 1;
    }

    return closure_ptr;
}

/// Get the size in bytes for a given LLVM type.
/// This is a compile-time approximation for 64-bit targets.
fn getTypeSizeBytes(ty: Builder.Type) u64 {
    return switch (ty) {
        .void => 0,
        .i1 => 1,
        .i8 => 1,
        .i16 => 2,
        .i32 => 4,
        .i64 => 8,
        .i128 => 16,
        .half => 2,
        .bfloat => 2,
        .float => 4,
        .double => 8,
        .fp128 => 16,
        .x86_fp80 => 10,
        .ppc_fp128 => 16,
        .ptr => 8, // 64-bit pointer
        .token => 0,
        .label => 0,
        .metadata => 0,
        else => 8, // Default to pointer size for complex types
    };
}

/// Emit a block expression.
///
/// Blocks execute statements sequentially, then evaluate to the final expression.
fn emitBlock(
    ctx: *CirContext,
    stmts: CIR.Statement.Span,
    final_expr_idx: CIR.Expr.Idx,
) Error!Builder.Value {
    // Push a new scope for the block
    ctx.emitter.pushScope() catch return error.OutOfMemory;
    errdefer ctx.emitter.popScope(); // Clean up on error (without decref since we're failing anyway)

    // Execute each statement
    var iter = ctx.module_env.store.statements.iterate(stmts);
    while (iter.next()) |stmt| {
        try emitStatement(ctx, stmt.*);
    }

    // Evaluate the final expression
    const final_expr = ctx.module_env.store.getExpr(final_expr_idx);
    const result = try emitExpr(ctx, final_expr);

    // Emit decrefs for scope values and pop scope
    try emitScopeCleanup(ctx);

    return result;
}

/// Emit a statement (helper for block emission)
fn emitStatement(ctx: *CirContext, stmt: CIR.Statement) Error!void {
    switch (stmt) {
        .s_decl => |decl| {
            // Let binding: evaluate value and bind to pattern
            const value_expr = ctx.module_env.store.getExpr(decl.expr);
            const value = try emitExpr(ctx, value_expr);

            // Bind the value to the pattern
            try bindPatternToValue(ctx, decl.pattern, value);
        },

        .s_decl_gen => |decl| {
            // Generalized let binding (polymorphic lambdas, number literals)
            // Treat the same as s_decl for code generation purposes
            const value_expr = ctx.module_env.store.getExpr(decl.expr);
            const value = try emitExpr(ctx, value_expr);
            try bindPatternToValue(ctx, decl.pattern, value);
        },

        .s_var => |var_decl| {
            // Mutable variable: allocate stack slot and store initial value
            const value_expr = ctx.module_env.store.getExpr(var_decl.expr);
            const value = try emitExpr(ctx, value_expr);

            // Get the proper type and layout for the alloca from the expression
            const var_type = try ctx.getExprLlvmType(var_decl.expr);
            const var_layout = try ctx.getExprLayout(var_decl.expr);

            // For mutable vars, we allocate on stack with proper alignment
            // (i128/Dec types need 16-byte alignment on ARM)
            const alignment = emit.LlvmEmitter.getTypeAlignment(var_type);
            const alloca = ctx.emitter.emitAllocaAligned(var_type, alignment) catch return error.OutOfMemory;

            // Store initial value
            ctx.emitter.emitStore(value, alloca) catch return error.OutOfMemory;

            // Bind the alloca (not the value) to the pattern
            const pattern = ctx.module_env.store.getPattern(var_decl.pattern_idx);
            switch (pattern) {
                .assign => |assign| {
                    const name = ctx.module_env.getIdent(assign.ident);
                    // Mark as pointer so lookups will load; include layout for decref
                    const scoped = emit.ScopedValue.refcounted(alloca, .ptr, var_layout, true);
                    ctx.emitter.defineVar(name, scoped) catch return error.OutOfMemory;
                },
                else => return error.UnsupportedExpression,
            }
        },

        .s_reassign => |reassign| {
            // Reassignment: load the variable's storage pointer and store new value
            const value_expr = ctx.module_env.store.getExpr(reassign.expr);
            const value = try emitExpr(ctx, value_expr);

            // Get the pattern to find the variable name
            const pattern = ctx.module_env.store.getPattern(reassign.pattern_idx);
            switch (pattern) {
                .assign => |assign| {
                    const name = ctx.module_env.getIdent(assign.ident);
                    if (ctx.emitter.lookupVar(name)) |scoped| {
                        if (scoped.is_ptr) {
                            // Store to the alloca
                            ctx.emitter.emitStore(value, scoped.value) catch return error.OutOfMemory;
                        } else {
                            return error.UnsupportedExpression; // Can't reassign non-var
                        }
                    } else {
                        return error.VariableNotFound;
                    }
                },
                else => return error.UnsupportedExpression,
            }
        },

        .s_expr => |expr_stmt| {
            // Expression statement: evaluate for side effects
            const expr = ctx.module_env.store.getExpr(expr_stmt.expr);
            _ = try emitExpr(ctx, expr);
        },

        .s_return => |ret| {
            // Return statement: emit return instruction
            const value_expr = ctx.module_env.store.getExpr(ret.value);
            const value = try emitExpr(ctx, value_expr);
            ctx.emitter.emitRet(value) catch return error.OutOfMemory;
        },

        .s_break => {
            // Break: jump to loop exit (needs loop context - TODO)
            return error.UnsupportedExpression;
        },

        .s_crash => {
            // TODO: Crash statement
            return error.UnsupportedExpression;
        },

        .s_dbg => |dbg| {
            // Debug statement: evaluate expression (printing is side effect)
            const expr = ctx.module_env.store.getExpr(dbg.expr);
            _ = try emitExpr(ctx, expr);
        },
        .s_expect => return error.UnsupportedExpression,
        // For/while loops, type declarations, imports, etc. are not handled here
        else => return error.UnsupportedExpression,
    }
}

/// Bind a pattern to a value in the current scope
fn bindPatternToValue(ctx: *CirContext, pattern_idx: CIR.Pattern.Idx, value: Builder.Value) Error!void {
    const pattern = ctx.module_env.store.getPattern(pattern_idx);

    switch (pattern) {
        .assign => |assign| {
            const name = ctx.module_env.getIdent(assign.ident);
            const pattern_type = try ctx.getPatternLlvmType(pattern_idx);
            const scoped = emit.ScopedValue.simple(value, pattern_type);
            ctx.emitter.defineVar(name, scoped) catch return error.OutOfMemory;
        },
        .underscore => {
            // Wildcard: don't bind anything
        },
        .record_destructure => {
            // Record destructuring: Phase 9
            return error.UnsupportedExpression;
        },
        .tuple => {
            // Tuple destructuring: Phase 9
            return error.UnsupportedExpression;
        },
        .list => {
            // List destructuring: Phase 9
            return error.UnsupportedExpression;
        },
        .applied_tag => {
            // Tag destructuring: Phase 9
            return error.UnsupportedExpression;
        },
        else => return error.UnsupportedExpression,
    }
}

/// Emit decrefs for all refcounted values in the current scope and pop the scope.
/// This should be called instead of ctx.emitter.popScope() when exiting scopes
/// that may contain refcounted values.
fn emitScopeCleanup(ctx: *CirContext) Error!void {
    // Get all values that need decref before popping the scope
    const decref_values = ctx.emitter.getDecrefValues();

    // Emit decref for each value that has layout info
    for (decref_values) |scoped_val| {
        if (scoped_val.layout) |layout_val| {
            // If the value is stored on the stack (is_ptr), we need to load it first
            const value_to_decref = if (scoped_val.is_ptr)
                ctx.emitter.emitLoad(scoped_val.llvm_type, scoped_val.value) catch return error.OutOfMemory
            else
                scoped_val.value;

            // Emit the decref call
            refcount.emitDecref(
                ctx.refcount_ctx,
                ctx.emitter,
                ctx.layout_store,
                value_to_decref,
                layout_val,
            ) catch |err| {
                // If decref fails (e.g., RocOps not set up), continue with other cleanup
                // This handles cases like the REPL where not all infrastructure is available
                switch (err) {
                    error.NoActiveFunction => {}, // RocOps not available, skip decref
                    else => return err,
                }
            };
        }
    }

    // Now pop the scope
    ctx.emitter.popScope();
}

//! Monomorphized Intermediate Representation (Mono IR)
//!
//! This module provides the IR layer between CIR and code generation.
//! It solves cross-module index collisions by using globally unique symbols.
//!
//! Key components:
//! - `MonoIR`: Core types (MonoExpr, MonoPattern, MonoSymbol)
//! - `MonoExprStore`: Flat storage for all lowered expressions
//! - `Lower`: CIR → Mono IR lowering pass
//!
//! Usage:
//! ```zig
//! const mono = @import("mono");
//!
//! // Create a store for lowered expressions
//! var store = mono.MonoExprStore.init(allocator);
//! defer store.deinit();
//!
//! // Lower an expression
//! const expr_id = try mono.Lower.lowerExpression(
//!     allocator,
//!     &store,
//!     all_module_envs,
//!     module_idx,
//!     expr_idx,
//! );
//!
//! // Access the lowered expression
//! const mono_expr = store.getExpr(expr_id);
//! ```

const std = @import("std");

/// Core IR types: MonoExpr, MonoPattern, MonoSymbol, etc.
pub const MonoIR = @import("MonoIR.zig");

/// Flat storage for expressions and patterns
pub const MonoExprStore = @import("MonoExprStore.zig");

/// CIR → Mono IR lowering pass
pub const Lower = @import("Lower.zig");

// Re-export commonly used types
pub const MonoExpr = MonoIR.MonoExpr;
pub const MonoPattern = MonoIR.MonoPattern;
pub const MonoSymbol = MonoIR.MonoSymbol;
pub const MonoExprId = MonoIR.MonoExprId;
pub const MonoPatternId = MonoIR.MonoPatternId;
pub const MonoExprSpan = MonoIR.MonoExprSpan;
pub const MonoPatternSpan = MonoIR.MonoPatternSpan;
pub const ClosureRepresentation = MonoIR.ClosureRepresentation;
pub const MonoCapture = MonoIR.MonoCapture;
pub const Recursive = MonoIR.Recursive;
pub const SelfRecursive = MonoIR.SelfRecursive;
pub const JoinPointId = MonoIR.JoinPointId;

test "mono tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(MonoIR);
    std.testing.refAllDecls(MonoExprStore);
    std.testing.refAllDecls(Lower);
}

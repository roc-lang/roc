//! Monomorphized Intermediate Representation (Mono IR)
//!
//! This module provides the IR layer between CIR and code generation.
//! It solves cross-module index collisions by using globally unique symbols.
//!
//! Key components:
//! - `MonoIR`: Core types (MonoExpr, MonoPattern, Symbol)
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

/// Core IR types: MonoExpr, MonoPattern, Symbol, etc.
pub const MonoIR = @import("MonoIR.zig");

/// Flat storage for expressions and patterns
pub const MonoExprStore = @import("MonoExprStore.zig");

/// CIR → Mono IR lowering pass
pub const Lower = @import("Lower.zig");

/// Tail recursion detection and transformation
pub const TailRecursion = @import("TailRecursion.zig");

/// Mono IR-level reference counting insertion pass
pub const RcInsert = @import("rc_insert.zig");

/// Re-export commonly used types from MonoIR
pub const MonoExpr = MonoIR.MonoExpr;
/// Re-export pattern type
pub const MonoPattern = MonoIR.MonoPattern;
/// Re-export symbol type
pub const Symbol = MonoIR.Symbol;
/// Re-export expression ID type
pub const MonoExprId = MonoIR.MonoExprId;
/// Re-export pattern ID type
pub const MonoPatternId = MonoIR.MonoPatternId;
/// Re-export expression span type
pub const MonoExprSpan = MonoIR.MonoExprSpan;
/// Re-export pattern span type
pub const MonoPatternSpan = MonoIR.MonoPatternSpan;
/// Re-export closure representation type
pub const ClosureRepresentation = MonoIR.ClosureRepresentation;
/// Re-export capture type
pub const MonoCapture = MonoIR.MonoCapture;
/// Re-export recursive flag type
pub const Recursive = MonoIR.Recursive;
/// Re-export self-recursive flag type
pub const SelfRecursive = MonoIR.SelfRecursive;
/// Re-export join point ID type
pub const JoinPointId = MonoIR.JoinPointId;
/// Re-export lambda set member type
pub const LambdaSetMember = MonoIR.LambdaSetMember;
/// Re-export lambda set member span type
pub const LambdaSetMemberSpan = MonoIR.LambdaSetMemberSpan;

/// Control flow statement type for tail recursion
pub const CFStmt = MonoIR.CFStmt;
/// Control flow statement ID type
pub const CFStmtId = MonoIR.CFStmtId;
/// Control flow switch branch type
pub const CFSwitchBranch = MonoIR.CFSwitchBranch;
/// Control flow switch branch span type
pub const CFSwitchBranchSpan = MonoIR.CFSwitchBranchSpan;
/// Control flow match branch type
pub const CFMatchBranch = MonoIR.CFMatchBranch;
/// Control flow match branch span type
pub const CFMatchBranchSpan = MonoIR.CFMatchBranchSpan;
/// Layout index span type
pub const LayoutIdxSpan = MonoIR.LayoutIdxSpan;
/// Monomorphized procedure type
pub const MonoProc = MonoIR.MonoProc;

test "mono tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(MonoIR);
    std.testing.refAllDecls(MonoExprStore);
    std.testing.refAllDecls(Lower);
    std.testing.refAllDecls(TailRecursion);
    std.testing.refAllDecls(RcInsert);
}

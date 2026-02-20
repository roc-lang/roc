//! Low-level Intermediate Representation (LIR)
//!
//! This module provides the IR layer between MIR and code generation.
//! It solves cross-module index collisions by using globally unique symbols.
//!
//! Key components:
//! - `LIR`: Core types (LirExpr, LirPattern, Symbol)
//! - `LirExprStore`: Flat storage for all lowered expressions
//! - `MirToLir`: MIR → LIR translation pass
//!
//! Usage:
//! ```zig
//! const lir = @import("lir");
//!
//! // Create a store for lowered expressions
//! var store = lir.LirExprStore.init(allocator);
//! defer store.deinit();
//!
//! // Access the lowered expression
//! const lir_expr = store.getExpr(expr_id);
//! ```

const std = @import("std");

/// Core IR types: LirExpr, LirPattern, Symbol, etc.
pub const LIR = @import("LIR.zig");

/// Flat storage for expressions and patterns
pub const LirExprStore = @import("LirExprStore.zig");

/// Tail recursion detection and transformation
pub const TailRecursion = @import("TailRecursion.zig");

/// MIR → LIR translation pass
pub const MirToLir = @import("MirToLir.zig");

/// LIR-level reference counting insertion pass
pub const RcInsert = @import("rc_insert.zig");

/// Re-export commonly used types from LIR
pub const LirExpr = LIR.LirExpr;
/// Re-export pattern type
pub const LirPattern = LIR.LirPattern;
/// Re-export symbol type
pub const Symbol = LIR.Symbol;
/// Re-export expression ID type
pub const LirExprId = LIR.LirExprId;
/// Re-export pattern ID type
pub const LirPatternId = LIR.LirPatternId;
/// Re-export expression span type
pub const LirExprSpan = LIR.LirExprSpan;
/// Re-export pattern span type
pub const LirPatternSpan = LIR.LirPatternSpan;
/// Re-export closure representation type
pub const ClosureRepresentation = LIR.ClosureRepresentation;
/// Re-export capture type
pub const LirCapture = LIR.LirCapture;
/// Re-export recursive flag type
pub const Recursive = LIR.Recursive;
/// Re-export self-recursive flag type
pub const SelfRecursive = LIR.SelfRecursive;
/// Re-export join point ID type
pub const JoinPointId = LIR.JoinPointId;
/// Re-export lambda set member type
pub const LambdaSetMember = LIR.LambdaSetMember;
/// Re-export lambda set member span type
pub const LambdaSetMemberSpan = LIR.LambdaSetMemberSpan;

/// Control flow statement type for tail recursion
pub const CFStmt = LIR.CFStmt;
/// Control flow statement ID type
pub const CFStmtId = LIR.CFStmtId;
/// Control flow switch branch type
pub const CFSwitchBranch = LIR.CFSwitchBranch;
/// Control flow switch branch span type
pub const CFSwitchBranchSpan = LIR.CFSwitchBranchSpan;
/// Control flow match branch type
pub const CFMatchBranch = LIR.CFMatchBranch;
/// Control flow match branch span type
pub const CFMatchBranchSpan = LIR.CFMatchBranchSpan;
/// Layout index span type
pub const LayoutIdxSpan = LIR.LayoutIdxSpan;
/// LIR procedure type
pub const LirProc = LIR.LirProc;

test "lir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(LIR);
    std.testing.refAllDecls(LirExprStore);
    std.testing.refAllDecls(MirToLir);
    std.testing.refAllDecls(TailRecursion);
    std.testing.refAllDecls(RcInsert);
}

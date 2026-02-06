//! Type checking problem/error types and storage
//!
//! This module provides all the problem data structures used during type checking.
//! Problems are collected by the Store and later rendered to user-friendly messages
//! by the ReportBuilder.

pub const types = @import("./problem/types.zig");
pub const store = @import("./problem/store.zig");
pub const context = @import("./problem/context.zig");

// Re-export context types for error reporting
pub const Context = context.Context;
pub const Category = context.Category;

// Re-export commonly used types at the top level for convenience
pub const Problem = types.Problem;
pub const Store = store.Store;

// Re-export all problem data types
pub const ExtraStringIdx = types.ExtraStringIdx;
pub const MissingPatternsRange = types.MissingPatternsRange;

// Type mismatch types
pub const TypeMismatch = types.TypeMismatch;
pub const TypePair = types.TypePair;
pub const IncompatiblePlatformRequirement = types.IncompatiblePlatformRequirement;
pub const CrossModuleImport = types.CrossModuleImport;

// Static dispatch errors
pub const StaticDispatch = types.StaticDispatch;
pub const DispatcherNotNominal = types.DispatcherNotNominal;
pub const DispatcherDoesNotImplMethod = types.DispatcherDoesNotImplMethod;
pub const TypeDoesNotSupportEquality = types.TypeDoesNotSupportEquality;

// Number errors
pub const NumberDoesNotFit = types.NumberDoesNotFit;
pub const NegativeUnsignedInt = types.NegativeUnsignedInt;
pub const InvalidNumericLiteral = types.InvalidNumericLiteral;
pub const UnusedValue = types.UnusedValue;

// Match/exhaustiveness errors
pub const NonExhaustiveMatch = types.NonExhaustiveMatch;
pub const RedundantPattern = types.RedundantPattern;
pub const UnmatchablePattern = types.UnmatchablePattern;

// Type declaration errors
pub const TypeApplyArityMismatch = types.TypeApplyArityMismatch;
pub const RecursiveAlias = types.RecursiveAlias;
pub const UnsupportedAliasWhereClause = types.UnsupportedAliasWhereClause;

// Nominal type errors
pub const CannotAccessOpaqueNominal = types.CannotAccessOpaqueNominal;
pub const NominalTypeResolutionFailed = types.NominalTypeResolutionFailed;

// Platform errors
pub const PlatformAliasNotFound = types.PlatformAliasNotFound;
pub const PlatformDefNotFound = types.PlatformDefNotFound;

// Comptime errors
pub const ComptimeCrash = types.ComptimeCrash;
pub const ComptimeExpectFailed = types.ComptimeExpectFailed;
pub const ComptimeEvalError = types.ComptimeEvalError;

// Generic errors
pub const VarWithSnapshot = types.VarWithSnapshot;

//! Compilation-related types and functionality.

pub const ModuleEnv = @import("ModuleEnv.zig");
pub const type_writers = @import("type_writers.zig");
pub const cir_types = @import("cir_types.zig");
pub const CIR = @import("CIR.zig");
pub const BuildModule = @import("BuildModule.zig");
pub const BuildEnv = @import("BuildEnv.zig").BuildEnv;

// Re-export CIR types from ModuleEnv
/// Node type representing various AST elements
pub const Node = ModuleEnv.Node;
/// Store for all nodes in the CIR
pub const NodeStore = ModuleEnv.NodeStore;
/// Expression type for Roc expressions
pub const Expr = ModuleEnv.Expr;
/// Pattern type for pattern matching
pub const Pattern = ModuleEnv.Pattern;
/// Statement type for module-level statements
pub const Statement = ModuleEnv.Statement;
/// Type annotation representation
pub const TypeAnno = ModuleEnv.TypeAnno;
/// Diagnostic messages for compilation errors and warnings
pub const Diagnostic = ModuleEnv.Diagnostic;
/// Definition type for value and function definitions
pub const Def = ModuleEnv.Def;
/// Type header for type declarations
pub const TypeHeader = ModuleEnv.TypeHeader;
/// Where clause for type constraints
pub const WhereClause = ModuleEnv.WhereClause;
/// Type annotation with position information
pub const Annotation = ModuleEnv.Annotation;
/// Items exposed by a module
pub const ExposedItem = ModuleEnv.ExposedItem;
/// Fields in record patterns
pub const PatternRecordField = ModuleEnv.PatternRecordField;
/// Arbitrary precision integer values
pub const IntValue = ModuleEnv.IntValue;
/// Roc decimal type representation
pub const RocDec = ModuleEnv.RocDec;
/// Import statements
pub const Import = ModuleEnv.Import;
/// Fields in record expressions
pub const RecordField = ModuleEnv.RecordField;
/// External declarations from other modules
pub const ExternalDecl = ModuleEnv.ExternalDecl;
/// Compilation error reports
pub const Report = ModuleEnv.Report;
// isCastable is not exported, it's internal to ModuleEnv
/// Cast function for index types
pub const castIdx = ModuleEnv.castIdx;

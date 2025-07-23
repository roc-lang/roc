//! Compilation-related types and functionality.

pub const ModuleEnv = @import("ModuleEnv.zig");
pub const type_writers = @import("type_writers.zig");
pub const cir_types = @import("cir_types.zig");

// Re-export CIR types from ModuleEnv
pub const Node = ModuleEnv.Node;
pub const NodeStore = ModuleEnv.NodeStore;
pub const Expr = ModuleEnv.Expr;
pub const Pattern = ModuleEnv.Pattern;
pub const Statement = ModuleEnv.Statement;
pub const TypeAnno = ModuleEnv.TypeAnno;
pub const Diagnostic = ModuleEnv.Diagnostic;
pub const Def = ModuleEnv.Def;
pub const TypeHeader = ModuleEnv.TypeHeader;
pub const WhereClause = ModuleEnv.WhereClause;
pub const Annotation = ModuleEnv.Annotation;
pub const ExposedItem = ModuleEnv.ExposedItem;
pub const PatternRecordField = ModuleEnv.PatternRecordField;
pub const IntValue = ModuleEnv.IntValue;
pub const RocDec = ModuleEnv.RocDec;
pub const Import = ModuleEnv.Import;
pub const RecordField = ModuleEnv.RecordField;
pub const ExternalDecl = ModuleEnv.ExternalDecl;
pub const Report = ModuleEnv.Report;
// isCastable is not exported, it's internal to ModuleEnv
pub const castIdx = ModuleEnv.castIdx;
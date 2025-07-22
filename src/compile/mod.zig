//! Compilation-related types and functionality.

pub const ModuleEnv = @import("ModuleEnv.zig");
pub const type_writers = @import("type_writers.zig");
pub const cir_types = @import("cir_types.zig");

// CIR types that are now part of the compile module
pub const Node = @import("Node.zig");
pub const NodeStore = @import("NodeStore.zig");
pub const Expr = @import("Expression.zig").Expr;
pub const Pattern = @import("Pattern.zig").Pattern;
pub const Statement = @import("Statement.zig").Statement;
pub const TypeAnno = @import("TypeAnnotation.zig").TypeAnno;
pub const Diagnostic = @import("Diagnostic.zig").Diagnostic;
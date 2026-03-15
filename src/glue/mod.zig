//! Glue code generation for Roc platforms.
//!
//! Generates platform-specific binding code (e.g., Zig structs, C headers)
//! from a platform's type information by running a glue spec through the interpreter.

pub const glue = @import("glue.zig");
pub const GlueArgs = glue.GlueArgs;
pub const GlueError = glue.GlueError;
pub const rocGlue = glue.rocGlue;

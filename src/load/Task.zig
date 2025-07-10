//! Task definitions for the build system

const std = @import("std");

const Builder = @import("Builder.zig");

/// The kind of task
kind: Kind,

/// Different kinds of tasks
pub const Kind = union(enum) {
    /// Load a file from disk
    load_file: LoadFile,
    /// Canonicalize a module
    canonicalize: Canonicalize,
    /// Type check a module
    type_check: TypeCheck,
};

/// Load file task data
pub const LoadFile = struct {
    /// Path to the file to load
    path: []const u8,
    /// Module ID to assign
    module_id: Builder.ModuleId,
};

/// Canonicalize task data
pub const Canonicalize = struct {
    /// Module ID to canonicalize
    module_id: Builder.ModuleId,
};

/// Type check task data
pub const TypeCheck = struct {
    /// Module ID to type check
    module_id: Builder.ModuleId,
};

//! Central struct for tracking builtin types required by the Interpreter.
//! This ensures the Interpreter always has access to real, valid builtin types
//! and prevents instantiation with fake/dummy values.

const can = @import("can");

const CIR = can.CIR;

/// Contains all builtin types and modules required by the Interpreter.
/// This struct must be constructed with real, loaded builtin modules.
/// No dummy or fake values are allowed.
pub const BuiltinTypes = struct {
    // Statement indices for builtin type declarations
    bool_stmt: CIR.Statement.Idx,
    result_stmt: CIR.Statement.Idx,
    str_stmt: CIR.Statement.Idx,

    // Module environments for builtins (to look up their types)
    bool_env: *const can.ModuleEnv,
    result_env: *const can.ModuleEnv,
    str_env: *const can.ModuleEnv,

    /// Create BuiltinTypes from deserialized builtin indices and module environments.
    /// All parameters are required - there are no optional or dummy values allowed.
    ///
    /// Parameters:
    ///   - builtin_indices: Deserialized indices from compiled builtins
    ///   - bool_env: Bool module environment
    ///   - result_env: Result module environment
    ///   - str_env: Str module environment
    pub fn init(
        builtin_indices: CIR.BuiltinIndices,
        bool_env: *const can.ModuleEnv,
        result_env: *const can.ModuleEnv,
        str_env: *const can.ModuleEnv,
    ) BuiltinTypes {
        return .{
            .bool_stmt = builtin_indices.bool_type,
            .result_stmt = builtin_indices.result_type,
            .str_stmt = builtin_indices.str_type,
            .bool_env = bool_env,
            .result_env = result_env,
            .str_env = str_env,
        };
    }
};

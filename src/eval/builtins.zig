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
    try_stmt: CIR.Statement.Idx,
    str_stmt: CIR.Statement.Idx,

    // All numeric type statements
    u8_stmt: CIR.Statement.Idx,
    i8_stmt: CIR.Statement.Idx,
    u16_stmt: CIR.Statement.Idx,
    i16_stmt: CIR.Statement.Idx,
    u32_stmt: CIR.Statement.Idx,
    i32_stmt: CIR.Statement.Idx,
    u64_stmt: CIR.Statement.Idx,
    i64_stmt: CIR.Statement.Idx,
    u128_stmt: CIR.Statement.Idx,
    i128_stmt: CIR.Statement.Idx,
    f32_stmt: CIR.Statement.Idx,
    f64_stmt: CIR.Statement.Idx,
    dec_stmt: CIR.Statement.Idx,

    // Module environments for builtins (to look up their types)
    bool_env: *const can.ModuleEnv,
    try_env: *const can.ModuleEnv,
    str_env: *const can.ModuleEnv,
    builtin_env: *const can.ModuleEnv,

    /// Create BuiltinTypes from deserialized builtin indices and module environments.
    /// All parameters are required - there are no optional or dummy values allowed.
    ///
    /// Parameters:
    ///   - builtin_indices: Deserialized indices from compiled builtins
    ///   - bool_env: Bool module environment
    ///   - try_env: Try module environment
    ///   - str_env: Str module environment
    ///   - builtin_env: Builtin module environment (for numeric types)
    pub fn init(
        builtin_indices: CIR.BuiltinIndices,
        bool_env: *const can.ModuleEnv,
        try_env: *const can.ModuleEnv,
        str_env: *const can.ModuleEnv,
        builtin_env: *const can.ModuleEnv,
    ) BuiltinTypes {
        return .{
            .bool_stmt = builtin_indices.bool_type,
            .try_stmt = builtin_indices.try_type,
            .str_stmt = builtin_indices.str_type,
            .u8_stmt = builtin_indices.u8_type,
            .i8_stmt = builtin_indices.i8_type,
            .u16_stmt = builtin_indices.u16_type,
            .i16_stmt = builtin_indices.i16_type,
            .u32_stmt = builtin_indices.u32_type,
            .i32_stmt = builtin_indices.i32_type,
            .u64_stmt = builtin_indices.u64_type,
            .i64_stmt = builtin_indices.i64_type,
            .u128_stmt = builtin_indices.u128_type,
            .i128_stmt = builtin_indices.i128_type,
            .f32_stmt = builtin_indices.f32_type,
            .f64_stmt = builtin_indices.f64_type,
            .dec_stmt = builtin_indices.dec_type,
            .bool_env = bool_env,
            .try_env = try_env,
            .str_env = str_env,
            .builtin_env = builtin_env,
        };
    }
};

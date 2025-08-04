//! Static library version of builtins that provides minimal exports
//! This is a separate entry point to avoid circular imports with builtins module

const std = @import("std");
const builtins = @import("builtins");

// Export key functions that might need compiler-rt symbols
comptime {
    // Export overflow functions that might need compiler-rt symbols
    builtins.num.exportMulWithOverflow(i64, "roc__num_mul_with_overflow_");
    builtins.num.exportMulWithOverflow(i32, "roc__num_mul_with_overflow_");
    builtins.num.exportMulWithOverflow(i16, "roc__num_mul_with_overflow_");
    builtins.num.exportMulWithOverflow(i8, "roc__num_mul_with_overflow_");
    
    // Export other core functions that might be needed
    builtins.num.exportAddWithOverflow(i128, "roc__num_add_with_overflow_");
    builtins.num.exportSubWithOverflow(i128, "roc__num_sub_with_overflow_");
}

// Ensure ___muloti4 symbol is available by using @mulWithOverflow
export fn force_muloti4_symbol() i128 {
    const a: i128 = 123456789;
    const b: i128 = 987654321;
    const result = @mulWithOverflow(a, b);
    return result[0];
}
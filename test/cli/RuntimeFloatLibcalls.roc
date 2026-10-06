RuntimeFloatLibcalls := {}

# Float operations that some target has no instruction for, as top-level
# expects. No target has a float remainder instruction, and a baseline x86-64
# CPU has none for rounding either, so compiled code calls `fmod`, `floor`,
# `ceil` and `trunc` (and their `f`-suffixed F32 forms). An optimized `roc test`
# loads the compiled expects into the compiler's own process, which then has to
# supply those routines itself. Each operand is parsed from text when the
# expect runs, so the operations reach the backends instead of being folded at
# compile time.

wide : Str -> F64
wide = |text| F64.from_str(text) ?? 0

narrow : Str -> F32
narrow = |text| F32.from_str(text) ?? 0

expect wide("7.5").rem_by(wide("2")) == 1.5
expect narrow("7.5").rem_by(narrow("2")) == 1.5
expect wide("-7.5").rem_by(wide("2")) == -1.5
expect narrow("-7.5").rem_by(narrow("2")) == -1.5
expect wide("7.5").div_trunc_by(wide("2")) == 3
expect narrow("7.5").div_trunc_by(narrow("2")) == 3
expect wide("7.5").floor_to_i64_try() == Ok(7)
expect narrow("7.5").floor_to_i64_try() == Ok(7)
expect wide("7.5").ceiling_to_i64_try() == Ok(8)
expect narrow("7.5").ceiling_to_i64_try() == Ok(8)

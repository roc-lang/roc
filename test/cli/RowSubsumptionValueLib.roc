# A module exposing a coerced top-level value and a coerced top-level
# function with a top-level callable alias of it, for
# `RowSubsumptionValue.roc` and `RowSubsumptionCallableValue.roc` to widen
# across the module boundary.
RowSubsumptionValueLib := [].{
    Holder := { d : [B(Str), D] }

    lib_value : [B(Str), D]
    lib_value = Holder.{ d: B("lib") }.d

    lib_fwd : [B(Str), D] -> [B(Str), D]
    lib_fwd = |t| t

    lib_run = lib_fwd
}

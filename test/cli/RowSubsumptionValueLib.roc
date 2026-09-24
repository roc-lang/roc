# A module exposing a coerced top-level value, for
# `RowSubsumptionValue.roc` to widen across the module boundary.
RowSubsumptionValueLib := [].{
    Holder := { d : [B(Str), D] }

    lib_value : [B(Str), D]
    lib_value = Holder.{ d: B("lib") }.d
}

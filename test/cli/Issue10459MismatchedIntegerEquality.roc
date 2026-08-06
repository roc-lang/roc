# Repro for https://github.com/roc-lang/roc/issues/10459: mismatched concrete
# integer types in an expect must be reported as a type error.
Issue10459MismatchedIntegerEquality :: {}.{
    f : I128 -> I128
    f = |x| x
}

expect Issue10459MismatchedIntegerEquality.f(10.I128) == 10.I64

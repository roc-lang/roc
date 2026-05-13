## Numeric utility functions for working with floating-point values.
##
## In particular, [is_approx_eq] and [is_approx_eq_to_places] provide
## IEEE 754-style hybrid absolute-plus-relative tolerance comparisons.
Issue9392NumUtilsDeterministic :: [].{
    is_approx_eq : F64, F64 -> Bool
    is_approx_eq = |a, b| is_approx_eq_with_tols(a, b, 0.000001, 0.000000001)

    is_approx_eq_to_places : F64, F64, U64 -> Bool
    is_approx_eq_to_places = |a, b, places|
        is_approx_eq_with_tols(a, b, ten_pow_neg(places), 0.000000001)
}

is_approx_eq_with_tols : F64, F64, F64, F64 -> Bool
is_approx_eq_with_tols = |a, b, abs_tol, rel_tol| {
    diff = (a - b).abs()
    scale = if a.abs() > b.abs() a.abs() else b.abs()
    rel_part = scale * rel_tol
    threshold = if rel_part > abs_tol rel_part else abs_tol
    diff <= threshold
}

ten_pow_neg : U64 -> F64
ten_pow_neg = |n| {
    var $power = 1.0
    var $i = 0
    while $i < n {
        $power = $power * 10.0
        $i = $i + 1
    }
    1.0 / $power
}

expect Issue9392NumUtilsDeterministic.is_approx_eq(1.0, 1.0)
expect Issue9392NumUtilsDeterministic.is_approx_eq(1.0, 1.0000005)
expect Issue9392NumUtilsDeterministic.is_approx_eq(1.0, 0.9999995)
expect !Issue9392NumUtilsDeterministic.is_approx_eq(1.0, 1.00001)
expect !Issue9392NumUtilsDeterministic.is_approx_eq(1.0, 0.99999)
expect !Issue9392NumUtilsDeterministic.is_approx_eq(1.0, 0.5)

expect Issue9392NumUtilsDeterministic.is_approx_eq_to_places(1.0, 1.0, 10)
expect Issue9392NumUtilsDeterministic.is_approx_eq_to_places(1.0, 1.0000001, 5)
expect !Issue9392NumUtilsDeterministic.is_approx_eq_to_places(1.0, 1.0001, 5)
expect Issue9392NumUtilsDeterministic.is_approx_eq_to_places(1.0, 0.9999, 3)
expect !Issue9392NumUtilsDeterministic.is_approx_eq_to_places(1.0, 0.9999, 5)

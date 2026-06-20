package [] {}

approx_eq : F64, F64 -> Bool
approx_eq = |x, y| {
    to_int = |f| { (f * 1e6).to_i64_try() }

    match (to_int(x), to_int(y)) {
        (Ok(xi), Ok(yi)) => (xi == yi)
        _ => Bool.False
    }
}

expect approx_eq(3, 3)

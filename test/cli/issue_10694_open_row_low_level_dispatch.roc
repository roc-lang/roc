limit : U64
limit = 16

read : Str -> Try(I64, [BadInput])
read = |s| if Str.is_empty(s) { Err(BadInput) } else { Ok(16) }

decode : Str -> Try(I64, _)
decode = |body| {
    n = read(body) ? |_e| BadRead
    limit_i64 = limit.to_i64_try() ?? 0
    if n != limit_i64 {
        Err(Mismatch)
    } else {
        Ok(n)
    }
}

main! = |_args| {
    _ = decode("x")
    Ok({})
}

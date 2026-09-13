Ok(dict_len) = Ok(Dict.empty().insert(5.U64, 5.U64).len())

{ record_value } = {
    dbg 42
    { record_value: 7.U64 }
}

(left, right) = {
    var $total = 0.U64
    for item in [2.U64, 3] { $total = $total + item }
    ($total, 13.U64)
}

Ok(identity) = {
    expect 1 == 1
    Ok(|arg| arg)
}

main! = |_args| {
    echo!("${dict_len.to_str()}, ${record_value.to_str()}, ${left.to_str()}, ${right.to_str()}, ${identity(9.U64).to_str()}\n")
    Ok({})
}

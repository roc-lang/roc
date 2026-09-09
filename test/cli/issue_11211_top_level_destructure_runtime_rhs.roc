# Regression for issue 11211: a top-level binder must survive hoist pruning.
Ok(x) = {
    expect 1 == 1
    Ok(5.U64)
}

main! = |_args| {
    echo!("x: ${x.to_str()}\n")
    Ok({})
}

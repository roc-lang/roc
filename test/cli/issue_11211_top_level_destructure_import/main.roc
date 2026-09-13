import Values

main! = |_args| {
    echo!("${Values.identity(Values.value).to_str()}, ${Values.identity("ok")}\n")
    Ok({})
}

main! = |args| {
    result = match args {
        [] => crash "expected an argument"
        _ => {}
    }

    Ok(result)
}

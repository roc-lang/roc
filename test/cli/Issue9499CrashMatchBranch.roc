main! = |_| {
    result = match [] {
        _ => crash "unreachable"
    }

    Ok(result)
}

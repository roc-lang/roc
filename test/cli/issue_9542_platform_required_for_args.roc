main! = |[arg]| {
    for char in arg {
        echo!("arg: ${char}")
    }

    Ok({})
}

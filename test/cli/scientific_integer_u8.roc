answer : U8
answer = 1e2

main! = |_| if answer == 100 {
    Ok({})
} else {
    Err(Exit(1))
}

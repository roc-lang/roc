A := { a : Str }

main! = |_| {
    for A.{} in [A.{ a: "" }] {
        echo!("body")
    }
    Ok({})
}

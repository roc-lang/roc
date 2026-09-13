A := { a : Str }.{
    f : A -> Str
    f = |A.{}| ""
}

main! = |_| Ok(echo!("independent main ran"))

expect 1 + 1 == 2

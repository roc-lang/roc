f = |t| t.0

main! = |_| Ok(echo!("independent tuple access ran"))

expect (|t| t.0)(("first", "second")) == "first"

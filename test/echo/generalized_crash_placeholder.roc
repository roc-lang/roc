# A generalized value whose body always crashes is a placeholder: there is no
# value to evaluate at compile time, so it is evaluated where it is used and
# crashes only when that use runs (design.md "Specialization-Owned Top-Level
# Values").
todo : a
todo = crash "TODO placeholder reached"

main! = |args| {
    n : U64
    n = if args.len() > 100 { todo } else { 1 }
    echo!("placeholder untouched ${n.to_str()}")
    if args.len() > 0 {
        reached : Str
        reached = todo
        echo!(reached)
    } else {
        {}
    }
    Ok({})
}

# Issue 12810: a keep_if predicate that matches on a Try from a function with
# several Ok constructors.

Interaktion : [Streicheln, Schmusen, Pflegen]

von : Str -> Try(Interaktion, Str)
von = |n|
    if n == "streicheln" { Ok(Streicheln) }
    else if n == "schmusen" { Ok(Schmusen) }
    else if n == "pflegen" { Ok(Pflegen) }
    else { Err(n) }

main! = |_args| {
    ok = List.keep_if(["streicheln"], |n|
        match von(n) {
            Ok(_) => Bool.True
            Err(_) => Bool.False
        })
    echo!("${(List.len(ok)).to_str()}")
    Ok({})
}

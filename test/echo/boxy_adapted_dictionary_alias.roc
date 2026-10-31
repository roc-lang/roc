# A procedure value whose where-clause dictionary is adapted to its
# consumer's widened error row, called through a local alias.
wrap = |f| { run: || f({}).map_err(|_| ShowFailed) }

main! = |_args| {
    f : {} -> Try({}, [Empty])
    f = |_| Ok({})
    g = wrap
    r = g(f)
    (r.run)()?
    echo!("done\n")
    Ok({})
}

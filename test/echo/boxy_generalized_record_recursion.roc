# Mutually recursive generalized record values holding callables.
evens : { check : U64 -> [Yes, No] }
evens = { check: |n| if n == 0 Yes else (odds.check)(n - 1) }
odds : { check : U64 -> [Yes, No] }
odds = { check: |n| if n == 0 No else (evens.check)(n - 1) }
main! = |args| {
    answer = match (evens.check)(List.len(args) + 4) {
        Yes => "even"
        No => "odd"
    }
    echo!("${answer}\n")
    Ok({})
}

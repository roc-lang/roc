# A generalized callable value recursive with a monomorphic one (until
# implicitly opened rows generalize, which makes `is_even` generalized too).
make = |f| f
is_even : U64 -> [Yes, No]
is_even = make(|n| if n == 0 Yes else is_odd(n - 1))
is_odd : U64 -> [Yes, No]
is_odd = make(|n| if n == 0 No else is_even(n - 1))
main! = |args| {
    answer = match is_odd(List.len(args) + 3) {
        Yes => "odd"
        No => "even"
    }
    echo!("${answer}\n")
    Ok({})
}

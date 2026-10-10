# Mutually recursive generalized callable values with two quantified
# variables: each recursive use's substitution is in the scheme's slot order.
make = |f| f
is_even : a, U64 -> [Yes(a), No, ..]
is_even = make(|x, n| if n == 0 Yes(x) else is_odd(x, n - 1))
is_odd : a, U64 -> [Yes(a), No, ..]
is_odd = make(|x, n| if n == 0 No else is_even(x, n - 1))
main! = |args| {
    answer = match is_even("even", List.len(args) + 2) {
        Yes(s) => s
        No => "odd"
    }
    count = match is_odd(List.len(args) + 40, 3) {
        Yes(n) => n.to_str()
        No => "none"
    }
    echo!("${answer} ${count}\n")
    Ok({})
}

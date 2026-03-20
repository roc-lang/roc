app [main!] { pf: platform "platform/main.roc" }

import pf.Stdout

Node(a) := [One(a), Many(List(Node(a)))]

flatten : List(Node(a)) -> List(a)
flatten = |input| {
    # Unannotated inner function - this triggers the stack overflow in issue #9053
    flatten_aux = |l, acc| {
        match l {
            [] => acc
            [One(e), .. as rest] => flatten_aux(rest, List.append(acc, e))
            [Many(e), .. as rest] => flatten_aux(rest, flatten_aux(e, acc))
        }
    }
    flatten_aux(input, [])
}

main! = || {
    Stdout.line!("hello")
}

expect {
    input : List(Node(Str))
    input = [One("a")]
    flatten(input) == ["a"]
}

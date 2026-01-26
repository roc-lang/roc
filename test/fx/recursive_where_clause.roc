app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

compress : List(a) -> List(a) where [a.is_eq : a, a -> Bool]
compress = |l|
    match l {
        [] => []
        [e] => [e]
        [e1, e2, .. as rest] => {
            rest_compression = compress(List.concat([e2], rest))
            if e1 == e2 { rest_compression } else { List.concat([e1], rest_compression) }
        }
    }

main! = || {
    result = compress([1, 1, 2, 2, 2, 3, 3, 1, 1])
    Stdout.line!(Str.inspect(result))
}

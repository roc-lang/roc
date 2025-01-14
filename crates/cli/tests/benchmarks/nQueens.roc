app [main!] { pf: platform "platform/main.roc" }

import pf.Host

main! : {} => {}
main! = \{} ->
    { value, is_error } = Host.get_int!({})
    input_result =
        if is_error then
            Err(GetIntError)
        else
            Ok(value)

    when input_result is
        Ok(n) ->
            queens(n) # original koka 13
            |> Num.to_str
            |> Host.put_line!

        Err(GetIntError) ->
            Host.put_line!("Error: Failed to get Integer from stdin.")

ConsList a : [Nil, Cons a (ConsList a)]

queens = \n -> length(find_solutions(n, n))

find_solutions = \n, k ->
    if k <= 0 then
        # should we use U64 as input type here instead?
        Cons(Nil, Nil)
    else
        extend(n, Nil, find_solutions(n, (k - 1)))

extend = \n, acc, solutions ->
    when solutions is
        Nil -> acc
        Cons(soln, rest) -> extend(n, append_safe(n, soln, acc), rest)

append_safe : I64, ConsList I64, ConsList (ConsList I64) -> ConsList (ConsList I64)
append_safe = \k, soln, solns ->
    if k <= 0 then
        solns
    else if safe(k, 1, soln) then
        append_safe((k - 1), soln, Cons(Cons(k, soln), solns))
    else
        append_safe((k - 1), soln, solns)

safe : I64, I64, ConsList I64 -> Bool
safe = \queen, diagonal, xs ->
    when xs is
        Nil -> Bool.true
        Cons(q, t) ->
            if queen != q && queen != q + diagonal && queen != q - diagonal then
                safe(queen, (diagonal + 1), t)
            else
                Bool.false

length : ConsList a -> I64
length = \xs ->
    length_help(xs, 0)

length_help : ConsList a, I64 -> I64
length_help = \foobar, acc ->
    when foobar is
        Cons(_, lrest) -> length_help(lrest, (1 + acc))
        Nil -> acc

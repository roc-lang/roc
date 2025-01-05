app [main] { pf: platform "platform/main.roc" }

import pf.PlatformTasks

# adapted from https://github.com/koka-lang/koka/blob/master/test/bench/haskell/cfold.hs
main : Task {} []
main =
    { value, is_error } = PlatformTasks.get_int!
    input_result =
        if is_error then
            Err(GetIntError)
        else
            Ok(value)

    when input_result is
        Ok(n) ->
            e = mk_expr(n, 1) # original koka n = 20 (set `ulimit -s unlimited` to avoid stack overflow for n = 20)
            unoptimized = eval(e)
            optimized = eval(const_folding(reassoc(e)))

            unoptimized
            |> Num.to_str
            |> Str.concat(" & ")
            |> Str.concat(Num.to_str(optimized))
            |> PlatformTasks.put_line

        Err(GetIntError) ->
            PlatformTasks.put_line("Error: Failed to get Integer from stdin.")

Expr : [
    Add Expr Expr,
    Mul Expr Expr,
    Val I64,
    Var I64,
]

mk_expr : I64, I64 -> Expr
mk_expr = \n, v ->
    when n is
        0 ->
            if v == 0 then Var(1) else Val(v)

        _ ->
            Add(mk_expr((n - 1), (v + 1)), mk_expr((n - 1), max((v - 1), 0)))

max : I64, I64 -> I64
max = \a, b -> if a > b then a else b

append_add : Expr, Expr -> Expr
append_add = \e1, e2 ->
    when e1 is
        Add(a1, a2) ->
            Add(a1, append_add(a2, e2))

        _ ->
            Add(e1, e2)

append_mul : Expr, Expr -> Expr
append_mul = \e1, e2 ->
    when e1 is
        Mul(a1, a2) ->
            Mul(a1, append_mul(a2, e2))

        _ ->
            Mul(e1, e2)

eval : Expr -> I64
eval = \e ->
    when e is
        Var(_) ->
            0

        Val(v) ->
            v

        Add(l, r) ->
            eval(l) + eval(r)

        Mul(l, r) ->
            eval(l) * eval(r)

reassoc : Expr -> Expr
reassoc = \e ->
    when e is
        Add(e1, e2) ->
            x1 = reassoc(e1)
            x2 = reassoc(e2)

            append_add(x1, x2)

        Mul(e1, e2) ->
            x1 = reassoc(e1)
            x2 = reassoc(e2)

            append_mul(x1, x2)

        _ ->
            e

const_folding : Expr -> Expr
const_folding = \e ->
    when e is
        Add(e1, e2) ->
            x1 = const_folding(e1)
            x2 = const_folding(e2)

            when x1 is
                Val(a) ->
                    when x2 is
                        Val(b) -> Val((a + b))
                        Add(Val(b), x) | Add(x, Val(b)) -> Add(Val((a + b)), x)
                        _ -> Add(x1, x2)

                _ -> Add(x1, x2)

        Mul(e1, e2) ->
            x1 = const_folding(e1)
            x2 = const_folding(e2)

            when x1 is
                Val(a) ->
                    when x2 is
                        Val(b) -> Val((a * b))
                        Mul(Val(b), x) | Mul(x, Val(b)) -> Mul(Val((a * b)), x)
                        _ -> Mul(x1, x2)

                _ -> Mul(x1, x2)

        _ ->
            e

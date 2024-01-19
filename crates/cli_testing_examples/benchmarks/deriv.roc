app "deriv"
    packages { pf: "platform/main.roc" }
    imports [pf.Task]
    provides [main] to pf

# based on: https://github.com/koka-lang/koka/blob/master/test/bench/haskell/deriv.hs
IO a : Task.Task a []

main : Task.Task {} []
main =
    inputResult <- Task.attempt Task.getInt

    when inputResult is
        Ok n ->
            x : Expr
            x = Var "x"

            f : Expr
            f = pow x x

            nest deriv n f # original koka n = 10
            |> Task.map \_ -> {}

        Err GetIntError ->
            Task.putLine "Error: Failed to get Integer from stdin."

nestHelp : I64, (I64, Expr -> IO Expr), I64, Expr -> IO Expr
nestHelp = \s, f, m, x -> when m is
    0 -> Task.succeed x
    _ ->
        w <- Task.after (f (s - m) x)
        nestHelp s f (m - 1) w

nest : (I64, Expr -> IO Expr), I64, Expr -> IO Expr
nest = \f, n, e -> nestHelp n f n e

Expr : [Val I64, Var Str, Add Expr Expr, Mul Expr Expr, Pow Expr Expr, Ln Expr]

divmod : I64, I64 -> Result { div : I64, mod : I64 } [DivByZero]
divmod = \l, r ->
    when Pair (Num.divTruncChecked l r) (Num.remChecked l r) is
        Pair (Ok div) (Ok mod) -> Ok { div, mod }
        _ -> Err DivByZero

pown : I64, I64 -> I64
pown = \a, n ->
    when n is
        0 -> 1
        1 -> a
        _ ->
            when divmod n 2 is
                Ok { div, mod } ->
                    b = pown a div

                    b * b * (if mod == 0 then 1 else a)

                Err DivByZero ->
                    -1

add : Expr, Expr -> Expr
add = \a, b ->
    when Pair a b is
        Pair (Val n) (Val m) ->
            Val (n + m)

        Pair (Val 0) f ->
            f

        Pair f (Val 0) ->
            f

        Pair f (Val n) ->
            add (Val n) f

        Pair (Val n) (Add (Val m) f) ->
            add (Val (n + m)) f

        Pair f (Add (Val n) g) ->
            add (Val n) (add f g)

        Pair (Add f g) h ->
            add f (add g h)

        Pair f g ->
            Add f g

mul : Expr, Expr -> Expr
mul = \a, b ->
    when Pair a b is
        Pair (Val n) (Val m) ->
            Val (n * m)

        Pair (Val 0) _ ->
            Val 0

        Pair _ (Val 0) ->
            Val 0

        Pair (Val 1) f ->
            f

        Pair f (Val 1) ->
            f

        Pair f (Val n) ->
            mul (Val n) f

        Pair (Val n) (Mul (Val m) f) ->
            mul (Val (n * m)) f

        Pair f (Mul (Val n) g) ->
            mul (Val n) (mul f g)

        Pair (Mul f g) h ->
            mul f (mul g h)

        Pair f g ->
            Mul f g

pow : Expr, Expr -> Expr
pow = \a, b ->
    when Pair a b is
        Pair (Val m) (Val n) -> Val (pown m n)
        Pair _ (Val 0) -> Val 1
        Pair f (Val 1) -> f
        Pair (Val 0) _ -> Val 0
        Pair f g -> Pow f g

ln : Expr -> Expr
ln = \f ->
    when f is
        Val 1 -> Val 0
        _ -> Ln f

d : Str, Expr -> Expr
d = \x, expr ->
    when expr is
        Val _ -> Val 0
        Var y -> if x == y then Val 1 else Val 0
        Add f g -> add (d x f) (d x g)
        Mul f g -> add (mul f (d x g)) (mul g (d x f))
        Pow f g ->
            mul (pow f g) (add (mul (mul g (d x f)) (pow f (Val (-1)))) (mul (ln f) (d x g)))

        Ln f ->
            mul (d x f) (pow f (Val (-1)))

count : Expr -> I64
count = \expr ->
    when expr is
        Val _ -> 1
        Var _ -> 1
        Add f g -> count f + count g
        Mul f g -> count f + count g
        Pow f g -> count f + count g
        Ln f -> count f

deriv : I64, Expr -> IO Expr
deriv = \i, f ->
    fprime = d "x" f
    line =
        Num.toStr (i + 1)
        |> Str.concat " count: "
        |> Str.concat (Num.toStr (count fprime))

    Task.putLine line
    |> Task.after \_ -> Task.succeed fprime

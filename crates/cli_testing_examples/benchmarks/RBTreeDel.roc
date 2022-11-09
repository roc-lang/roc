app "rbtree-del"
    packages { pf: "platform/main.roc" }
    imports [pf.Task]
    provides [main] to pf

Color : [Red, Black]

Tree a b : [Leaf, Node Color (Tree a b) a b (Tree a b)]

Map : Tree I64 Bool

ConsList a : [Nil, Cons a (ConsList a)]

main : Task.Task {} []
main =
    Task.after
        Task.getInt
        \n ->
            m = makeMap n # koka original n = 4_200_000
            val = fold (\_, v, r -> if v then r + 1 else r) m 0

            val
            |> Num.toStr
            |> Task.putLine

boom : Str -> a
boom = \_ -> boom ""

makeMap : I64 -> Map
makeMap = \n ->
    makeMapHelp n n Leaf

makeMapHelp : I64, I64, Map -> Map
makeMapHelp = \total, n, m ->
    when n is
        0 -> m
        _ ->
            n1 = n - 1

            powerOf10 =
                n |> Num.isMultipleOf 10

            t1 = insert m n powerOf10

            isFrequency =
                n |> Num.isMultipleOf 4

            key = n1 + ((total - n1) // 5)
            t2 = if isFrequency then delete t1 key else t1

            makeMapHelp total n1 t2

fold : (a, b, omega -> omega), Tree a b, omega -> omega
fold = \f, tree, b ->
    when tree is
        Leaf -> b
        Node _ l k v r -> fold f r (f k v (fold f l b))

depth : Tree * * -> I64
depth = \tree ->
    when tree is
        Leaf -> 1
        Node _ l _ _ r -> 1 + depth l + depth r

insert : Map, I64, Bool -> Map
insert = \t, k, v -> if isRed t then setBlack (ins t k v) else ins t k v

setBlack : Tree a b -> Tree a b
setBlack = \tree ->
    when tree is
        Node _ l k v r -> Node Black l k v r
        _ -> tree

isRed : Tree a b -> Bool
isRed = \tree ->
    when tree is
        Node Red _ _ _ _ -> Bool.true
        _ -> Bool.false

ins : Tree I64 Bool, I64, Bool -> Tree I64 Bool
ins = \tree, kx, vx ->
    when tree is
        Leaf ->
            Node Red Leaf kx vx Leaf

        Node Red a ky vy b ->
            when Num.compare kx ky is
                LT -> Node Red (ins a kx vx) ky vy b
                GT -> Node Red a ky vy (ins b kx vx)
                EQ -> Node Red a ky vy (ins b kx vx)

        Node Black a ky vy b ->
            when Num.compare kx ky is
                LT ->
                    when isRed a is
                        Bool.true -> balanceLeft (ins a kx vx) ky vy b
                        Bool.false -> Node Black (ins a kx vx) ky vy b

                GT ->
                    when isRed b is
                        Bool.true -> balanceRight a ky vy (ins b kx vx)
                        Bool.false -> Node Black a ky vy (ins b kx vx)

                EQ ->
                    Node Black a kx vx b

balanceLeft : Tree a b, a, b, Tree a b -> Tree a b
balanceLeft = \l, k, v, r ->
    when l is
        Leaf ->
            Leaf

        Node _ (Node Red lx kx vx rx) ky vy ry ->
            Node Red (Node Black lx kx vx rx) ky vy (Node Black ry k v r)

        Node _ ly ky vy (Node Red lx kx vx rx) ->
            Node Red (Node Black ly ky vy lx) kx vx (Node Black rx k v r)

        Node _ lx kx vx rx ->
            Node Black (Node Red lx kx vx rx) k v r

balanceRight : Tree a b, a, b, Tree a b -> Tree a b
balanceRight = \l, k, v, r ->
    when r is
        Leaf ->
            Leaf

        Node _ (Node Red lx kx vx rx) ky vy ry ->
            Node Red (Node Black l k v lx) kx vx (Node Black rx ky vy ry)

        Node _ lx kx vx (Node Red ly ky vy ry) ->
            Node Red (Node Black l k v lx) kx vx (Node Black ly ky vy ry)

        Node _ lx kx vx rx ->
            Node Black l k v (Node Red lx kx vx rx)

isBlack : Color -> Bool
isBlack = \c ->
    when c is
        Black -> Bool.true
        Red -> Bool.false

Del a b : [Del (Tree a b) Bool]

setRed : Map -> Map
setRed = \t ->
    when t is
        Node _ l k v r ->
            Node Red l k v r

        _ ->
            t

makeBlack : Map -> Del I64 Bool
makeBlack = \t ->
    when t is
        Node Red l k v r ->
            Del (Node Black l k v r) Bool.false

        _ ->
            Del t Bool.true

rebalanceLeft = \c, l, k, v, r ->
    when l is
        Node Black _ _ _ _ ->
            Del (balanceLeft (setRed l) k v r) (isBlack c)

        Node Red lx kx vx rx ->
            Del (Node Black lx kx vx (balanceLeft (setRed rx) k v r)) Bool.false

        _ ->
            boom "unreachable"

rebalanceRight = \c, l, k, v, r ->
    when r is
        Node Black _ _ _ _ ->
            Del (balanceRight l k v (setRed r)) (isBlack c)

        Node Red lx kx vx rx ->
            Del (Node Black (balanceRight l k v (setRed lx)) kx vx rx) Bool.false

        _ ->
            boom "unreachable"

delMin = \t ->
    when t is
        Node Black Leaf k v r ->
            when r is
                Leaf ->
                    Delmin (Del Leaf Bool.true) k v

                _ ->
                    Delmin (Del (setBlack r) Bool.false) k v

        Node Red Leaf k v r ->
            Delmin (Del r Bool.false) k v

        Node c l k v r ->
            when delMin l is
                Delmin (Del lx Bool.true) kx vx ->
                    Delmin (rebalanceRight c lx k v r) kx vx

                Delmin (Del lx Bool.false) kx vx ->
                    Delmin (Del (Node c lx k v r) Bool.false) kx vx

        Leaf ->
            Delmin (Del t Bool.false) 0 Bool.false

delete : Tree I64 Bool, I64 -> Tree I64 Bool
delete = \t, k ->
    when del t k is
        Del tx _ ->
            setBlack tx

del : Tree I64 Bool, I64 -> Del I64 Bool
del = \t, k ->
    when t is
        Leaf ->
            Del Leaf Bool.false

        Node cx lx kx vx rx ->
            if (k < kx) then
                when del lx k is
                    Del ly Bool.true ->
                        rebalanceRight cx ly kx vx rx

                    Del ly Bool.false ->
                        Del (Node cx ly kx vx rx) Bool.false
            else if (k > kx) then
                when del rx k is
                    Del ry Bool.true ->
                        rebalanceLeft cx lx kx vx ry

                    Del ry Bool.false ->
                        Del (Node cx lx kx vx ry) Bool.false
            else
                when rx is
                    Leaf ->
                        if isBlack cx then makeBlack lx else Del lx Bool.false

                    Node _ _ _ _ _ ->
                        when delMin rx is
                            Delmin (Del ry Bool.true) ky vy ->
                                rebalanceLeft cx lx ky vy ry

                            Delmin (Del ry Bool.false) ky vy ->
                                Del (Node cx lx ky vy ry) Bool.false

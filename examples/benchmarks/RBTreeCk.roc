app "rbtree-ck"
    packages { pf: "platform/main.roc" }
    imports [pf.Task]
    provides [main] to pf

Color : [Red, Black]

Tree a b : [Leaf, Node Color (Tree a b) a b (Tree a b)]

Map : Tree I64 Bool

ConsList a : [Nil, Cons a (ConsList a)]

makeMap : I64, I64 -> ConsList Map
makeMap = \freq, n ->
    makeMapHelp freq n Leaf Nil

makeMapHelp : I64, I64, Map, ConsList Map -> ConsList Map
makeMapHelp = \freq, n, m, acc ->
    when n is
        0 -> Cons m acc
        _ ->
            powerOf10 =
                n % 10 == 0

            m1 = insert m n powerOf10

            isFrequency =
                n % freq == 0

            x = (if isFrequency then Cons m1 acc else acc)

            makeMapHelp freq (n - 1) m1 x

fold : (a, b, omega -> omega), Tree a b, omega -> omega
fold = \f, tree, b ->
    when tree is
        Leaf -> b
        Node _ l k v r -> fold f r (f k v (fold f l b))

main : Task.Task {} []
main =
    Task.after
        Task.getInt
        \n ->
            # original koka n = 4_200_000
            ms : ConsList Map
            ms = makeMap 5 n

            when ms is
                Cons head _ ->
                    val = fold (\_, v, r -> if v then r + 1 else r) head 0

                    val
                    |> Num.toStr
                    |> Task.putLine

                Nil ->
                    Task.putLine "fail"

insert : Tree (Num k) v, Num k, v -> Tree (Num k) v
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

lt = \x, y -> x < y

ins : Tree (Num k) v, Num k, v -> Tree (Num k) v
ins = \tree, kx, vx ->
    when tree is
        Leaf -> Node Red Leaf kx vx Leaf
        Node Red a ky vy b ->
            if lt kx ky then
                Node Red (ins a kx vx) ky vy b
            else if lt ky kx then
                Node Red a ky vy (ins b kx vx)
            else
                Node Red a ky vy (ins b kx vx)

        Node Black a ky vy b ->
            if lt kx ky then
                (if isRed a then balance1 (Node Black Leaf ky vy b) (ins a kx vx) else Node Black (ins a kx vx) ky vy b)
            else if lt ky kx then
                (if isRed b then balance2 (Node Black a ky vy Leaf) (ins b kx vx) else Node Black a ky vy (ins b kx vx))
            else
                Node Black a kx vx b

balance1 : Tree a b, Tree a b -> Tree a b
balance1 = \tree1, tree2 ->
    when tree1 is
        Leaf -> Leaf
        Node _ _ kv vv t ->
            when tree2 is
                Node _ (Node Red l kx vx r1) ky vy r2 ->
                    Node Red (Node Black l kx vx r1) ky vy (Node Black r2 kv vv t)

                Node _ l1 ky vy (Node Red l2 kx vx r) ->
                    Node Red (Node Black l1 ky vy l2) kx vx (Node Black r kv vv t)

                Node _ l ky vy r ->
                    Node Black (Node Red l ky vy r) kv vv t

                Leaf -> Leaf

balance2 : Tree a b, Tree a b -> Tree a b
balance2 = \tree1, tree2 ->
    when tree1 is
        Leaf -> Leaf
        Node _ t kv vv _ ->
            when tree2 is
                Node _ (Node Red l kx1 vx1 r1) ky vy r2 ->
                    Node Red (Node Black t kv vv l) kx1 vx1 (Node Black r1 ky vy r2)

                Node _ l1 ky vy (Node Red l2 kx2 vx2 r2) ->
                    Node Red (Node Black t kv vv l1) ky vy (Node Black l2 kx2 vx2 r2)

                Node _ l ky vy r ->
                    Node Black t kv vv (Node Red l ky vy r)

                Leaf ->
                    Leaf

app "rbtree-insert"
    packages { pf: "platform/main.roc" }
    provides [main] to pf

import pf.Task

main : Task.Task {} []
main =
    tree : RedBlackTree I64 {}
    tree = insert 0 {} Empty

    tree
    |> show
    |> Task.putLine

show : RedBlackTree I64 {} -> Str
show = \tree -> showRBTree tree Num.toStr (\{} -> "{}")

showRBTree : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
showRBTree = \tree, showKey, showValue ->
    when tree is
        Empty -> "Empty"
        Node color key value left right ->
            sColor = showColor color
            sKey = showKey key
            sValue = showValue value
            sL = nodeInParens left showKey showValue
            sR = nodeInParens right showKey showValue

            "Node \(sColor) \(sKey) \(sValue) \(sL) \(sR)"

nodeInParens : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
nodeInParens = \tree, showKey, showValue ->
    when tree is
        Empty ->
            showRBTree tree showKey showValue

        Node _ _ _ _ _ ->
            inner = showRBTree tree showKey showValue

            "(\(inner))"

showColor : NodeColor -> Str
showColor = \color ->
    when color is
        Red -> "Red"
        Black -> "Black"

NodeColor : [Red, Black]

RedBlackTree k v : [Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty]

Key k : Num k

insert : Key k, v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
insert = \key, value, dict ->
    when insertHelp key value dict is
        Node Red k v l r -> Node Black k v l r
        x -> x

insertHelp : Key k, v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
insertHelp = \key, value, dict ->
    when dict is
        Empty ->
            # New nodes are always red. If it violates the rules, it will be fixed
            # when balancing.
            Node Red key value Empty Empty

        Node nColor nKey nValue nLeft nRight ->
            when Num.compare key nKey is
                LT -> balance nColor nKey nValue (insertHelp key value nLeft) nRight
                EQ -> Node nColor nKey value nLeft nRight
                GT -> balance nColor nKey nValue nLeft (insertHelp key value nRight)

balance : NodeColor, k, v, RedBlackTree k v, RedBlackTree k v -> RedBlackTree k v
balance = \color, key, value, left, right ->
    when right is
        Node Red rK rV rLeft rRight ->
            when left is
                Node Red lK lV lLeft lRight ->
                    Node
                        Red
                        key
                        value
                        (Node Black lK lV lLeft lRight)
                        (Node Black rK rV rLeft rRight)

                _ ->
                    Node color rK rV (Node Red key value left rLeft) rRight

        _ ->
            when left is
                Node Red lK lV (Node Red llK llV llLeft llRight) lRight ->
                    Node
                        Red
                        lK
                        lV
                        (Node Black llK llV llLeft llRight)
                        (Node Black key value lRight right)

                _ ->
                    Node color key value left right

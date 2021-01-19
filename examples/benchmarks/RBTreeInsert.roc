app "rbtree-insert"
    packages { base: "platform" }
    imports [base.Task]
    provides [ main ] to base

main : Task.Task {} []
main =
    tree : RedBlackTree I64 {}
    tree = insert 0 {} Empty 

    tree
        |> show 
        |> Task.putLine

show : RedBlackTree I64 {} -> Str
show = \tree -> showRBTree tree Str.fromInt (\{} -> "{}") 

showRBTree : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
showRBTree = \tree, showKey, showValue ->
    when tree is
        Empty -> "Empty"
        Node color key value left right -> 
            "Node "
                |> Str.concat (showColor color)
                |> Str.concat " "
                |> Str.concat (showKey key)
                |> Str.concat " "
                |> Str.concat (showValue value) 
                |> Str.concat " "
                |> Str.concat (nodeInParens left  showKey showValue)
                |> Str.concat " "
                |> Str.concat (nodeInParens right showKey showValue)

nodeInParens : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
nodeInParens = \tree, showKey, showValue ->
    when tree is
        Empty -> showRBTree tree showKey showValue
        Node _ _ _ _ _ -> 
            "(" 
                |> Str.concat (showRBTree tree showKey showValue)
                |> Str.concat ")"
    
showColor : NodeColor -> Str
showColor = \color ->
    when color is
        Red -> "Red"
        Black -> "Black"

NodeColor : [ Red, Black ]

RedBlackTree k v : [ Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty ]

Key k : Num k

insert : Key k, v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
insert = \key, value, dict ->
    when insertHelp key value dict is
        Node Red k v l r ->
            Node Black k v l r

        x ->
            x

insertHelp : (Key k), v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
insertHelp = \key, value, dict ->
  when dict is
    Empty ->
      # New nodes are always red. If it violates the rules, it will be fixed
      # when balancing.
      Node Red key value Empty Empty

    Node nColor nKey nValue nLeft nRight ->
      when Num.compare key nKey is
        LT ->
          balance nColor nKey nValue (insertHelp key value nLeft) nRight

        EQ ->
          Node nColor nKey value nLeft nRight

        GT ->
          balance nColor nKey nValue nLeft (insertHelp key value nRight)

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


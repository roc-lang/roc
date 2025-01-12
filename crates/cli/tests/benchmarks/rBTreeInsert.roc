app [main!] { pf: platform "platform/main.roc" }

import pf.Host

main! : () => ()
main! = \() ->
    tree : RedBlackTree I64 ()
    tree = insert(0, (), Empty)

    tree
    |> show
    |> Host.put_line!

show : RedBlackTree I64 () -> Str
show = \tree -> show_rb_tree(tree, Num.to_str, \() -> "()")

show_rb_tree : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
show_rb_tree = \tree, show_key, show_value ->
    when tree is
        Empty -> "Empty"
        Node(color, key, value, left, right) ->
            s_color = show_color(color)
            s_key = show_key(key)
            s_value = show_value(value)
            s_l = node_in_parens(left, show_key, show_value)
            s_r = node_in_parens(right, show_key, show_value)

            "Node ${s_color} ${s_key} ${s_value} ${s_l} ${s_r}"

node_in_parens : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
node_in_parens = \tree, show_key, show_value ->
    when tree is
        Empty ->
            show_rb_tree(tree, show_key, show_value)

        Node(_, _, _, _, _) ->
            inner = show_rb_tree(tree, show_key, show_value)

            "(${inner})"

show_color : NodeColor -> Str
show_color = \color ->
    when color is
        Red -> "Red"
        Black -> "Black"

NodeColor : [Red, Black]

RedBlackTree k v : [Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty]

Key k : Num k

insert : Key k, v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
insert = \key, value, dict ->
    when insert_help(key, value, dict) is
        Node(Red, k, v, l, r) -> Node(Black, k, v, l, r)
        x -> x

insert_help : Key k, v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
insert_help = \key, value, dict ->
    when dict is
        Empty ->
            # New nodes are always red. If it violates the rules, it will be fixed
            # when balancing.
            Node(Red, key, value, Empty, Empty)

        Node(n_color, n_key, n_value, n_left, n_right) ->
            when Num.compare(key, n_key) is
                LT -> balance(n_color, n_key, n_value, insert_help(key, value, n_left), n_right)
                EQ -> Node(n_color, n_key, value, n_left, n_right)
                GT -> balance(n_color, n_key, n_value, n_left, insert_help(key, value, n_right))

balance : NodeColor, k, v, RedBlackTree k v, RedBlackTree k v -> RedBlackTree k v
balance = \color, key, value, left, right ->
    when right is
        Node(Red, r_k, r_v, r_left, r_right) ->
            when left is
                Node(Red, l_k, l_v, l_left, l_right) ->
                    Node(
                        Red,
                        key,
                        value,
                        Node(Black, l_k, l_v, l_left, l_right),
                        Node(Black, r_k, r_v, r_left, r_right),
                    )

                _ ->
                    Node(color, r_k, r_v, Node(Red, key, value, left, r_left), r_right)

        _ ->
            when left is
                Node(Red, l_k, l_v, Node(Red, ll_k, ll_v, ll_left, ll_right), l_right) ->
                    Node(
                        Red,
                        l_k,
                        l_v,
                        Node(Black, ll_k, ll_v, ll_left, ll_right),
                        Node(Black, key, value, l_right, right),
                    )

                _ ->
                    Node(color, key, value, left, right)

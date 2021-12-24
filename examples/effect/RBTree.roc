interface RBTree exposes [ Dict, empty, size, singleton, isEmpty, insert, remove, update, fromList, toList, balance ] imports []

# The color of a node. Leaves are considered Black.
NodeColor : [ Red, Black ]

Dict k v : [ Node NodeColor k v (Dict k v) (Dict k v), Empty ]

Key k : Num k

Maybe a : [ Just a, Nothing ]

# Create an empty dictionary.
empty : Dict k v
empty =
    Empty

# Create a dictionary with one key-value pair.
singleton : Key k, v -> Dict (Key k) v
singleton = \key, value ->
    # Root node is always Black
    Node Black key value Empty Empty

# {-| Determine the number of key-value pairs in the dictionary. -}
size : Dict k v -> Int *
size = \dict ->
    sizeHelp 0 dict

sizeHelp : Int *, Dict k v -> Int *
sizeHelp = \n, dict ->
    when dict is
        Empty ->
            n

        Node _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left

isEmpty : Dict k v -> Bool
isEmpty = \dict ->
    when dict is
        Empty ->
            True

        Node _ _ _ _ _ ->
            False

insert : Key k, v, Dict (Key k) v -> Dict (Key k) v
insert = \key, value, dict ->
    when insertHelp key value dict is
        Node Red k v l r ->
            Node Black k v l r

        x ->
            x

insertHelp : Key k, v, Dict (Key k) v -> Dict (Key k) v
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

balance : NodeColor, k, v, Dict k v, Dict k v -> Dict k v
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

remove : Key k, Dict (Key k) v -> Dict (Key k) v
remove = \key, dict ->
    # Root node is always Black
    when removeHelp key dict is
        Node Red k v l r ->
            Node Black k v l r

        x ->
            x

# The easiest thing to remove from the tree, is a red node. However, when searching for the
# node to remove, we have no way of knowing if it will be red or not. This remove implementation
# makes sure that the bottom node is red by moving red colors down the tree through rotation
# and color flips. Any violations this will cause, can easily be fixed by balancing on the way
# up again.
removeHelp : Key k, Dict (Key k) v -> Dict (Key k) v
removeHelp = \targetKey, dict ->
    when dict is
        Empty ->
            Empty

        Node color key value left right ->
            if targetKey < key then
                when left is
                    Node Black _ _ lLeft _ ->
                        when lLeft is
                            Node Red _ _ _ _ ->
                                Node color key value (removeHelp targetKey left) right

                            _ ->
                                when moveRedLeft dict is
                                    Node nColor nKey nValue nLeft nRight ->
                                        balance nColor nKey nValue (removeHelp targetKey nLeft) nRight

                                    Empty ->
                                        Empty

                    _ ->
                        Node color key value (removeHelp targetKey left) right
            else
                removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)

removeHelpPrepEQGT : Key k, Dict (Key k) v, NodeColor, Key k, v, Dict (Key k) v, Dict (Key k) v -> Dict (Key k) v
removeHelpPrepEQGT = \_, dict, color, key, value, left, right ->
    when left is
        Node Red lK lV lLeft lRight ->
            Node
                color
                lK
                lV
                lLeft
                (Node Red key value lRight right)

        _ ->
            when right is
                Node Black _ _ (Node Black _ _ _ _) _ ->
                    moveRedRight dict

                Node Black _ _ Empty _ ->
                    moveRedRight dict

                _ ->
                    dict

# When we find the node we are looking for, we can remove by replacing the key-value
# pair with the key-value pair of the left-most node on the right side (the closest pair).
removeHelpEQGT : Key k, Dict (Key k) v -> Dict (Key k) v
removeHelpEQGT = \targetKey, dict ->
    when dict is
        Node color key value left right ->
            if targetKey == key then
                when getMin right is
                    Node _ minKey minValue _ _ ->
                        balance color minKey minValue left (removeMin right)

                    Empty ->
                        Empty
            else
                balance color key value left (removeHelp targetKey right)

        Empty ->
            Empty

getMin : Dict k v -> Dict k v
getMin = \dict ->
    when dict is
        # Node _ _ _ ((Node _ _ _ _ _) as left) _ ->
        Node _ _ _ left _ ->
            when left is
                Node _ _ _ _ _ ->
                    getMin left

                _ ->
                    dict

        _ ->
            dict

moveRedLeft : Dict k v -> Dict k v
moveRedLeft = \dict ->
    when dict is
        # Node clr k v (Node lClr lK lV lLeft lRight) (Node rClr rK rV ((Node Red rlK rlV rlL rlR) as rLeft) rRight) ->
        # Node clr k v (Node lClr lK lV lLeft lRight) (Node rClr rK rV rLeft rRight) ->
        Node clr k v (Node _ lK lV lLeft lRight) (Node _ rK rV rLeft rRight) ->
            when rLeft is
                Node Red rlK rlV rlL rlR ->
                    Node
                        Red
                        rlK
                        rlV
                        (Node Black k v (Node Red lK lV lLeft lRight) rlL)
                        (Node Black rK rV rlR rRight)

                _ ->
                    when clr is
                        Black ->
                            Node
                                Black
                                k
                                v
                                (Node Red lK lV lLeft lRight)
                                (Node Red rK rV rLeft rRight)

                        Red ->
                            Node
                                Black
                                k
                                v
                                (Node Red lK lV lLeft lRight)
                                (Node Red rK rV rLeft rRight)

        _ ->
            dict

moveRedRight : Dict k v -> Dict k v
moveRedRight = \dict ->
    when dict is
        Node _ k v (Node _ lK lV (Node Red llK llV llLeft llRight) lRight) (Node _ rK rV rLeft rRight) ->
            Node
                Red
                lK
                lV
                (Node Black llK llV llLeft llRight)
                (Node Black k v lRight (Node Red rK rV rLeft rRight))

        Node clr k v (Node _ lK lV lLeft lRight) (Node _ rK rV rLeft rRight) ->
            when clr is
                Black ->
                    Node
                        Black
                        k
                        v
                        (Node Red lK lV lLeft lRight)
                        (Node Red rK rV rLeft rRight)

                Red ->
                    Node
                        Black
                        k
                        v
                        (Node Red lK lV lLeft lRight)
                        (Node Red rK rV rLeft rRight)

        _ ->
            dict

removeMin : Dict k v -> Dict k v
removeMin = \dict ->
    when dict is
        Node color key value left right ->
            when left is
                Node lColor _ _ lLeft _ ->
                    when lColor is
                        Black ->
                            when lLeft is
                                Node Red _ _ _ _ ->
                                    Node color key value (removeMin left) right

                                _ ->
                                    when moveRedLeft dict is
                                        Node nColor nKey nValue nLeft nRight ->
                                            balance nColor nKey nValue (removeMin nLeft) nRight

                                        Empty ->
                                            Empty

                        _ ->
                            Node color key value (removeMin left) right

                _ ->
                    Empty

        _ ->
            Empty

# Update the value of a dictionary for a specific key with a given function.
update : Key k, (Maybe v -> Maybe v), Dict (Key k) v -> Dict (Key k) v
update = \targetKey, alter, dictionary ->
    when alter (get targetKey dictionary) is
        Just value ->
            insert targetKey value dictionary

        Nothing ->
            remove targetKey dictionary

get : Key k, Dict (Key k) v -> Maybe v
get = \targetKey, dict ->
    when dict is
        Empty ->
            Nothing

        Node _ key value left right ->
            when Num.compare targetKey key is
                LT ->
                    get targetKey left

                EQ ->
                    Just value

                GT ->
                    get targetKey right

fromList : List { key : Num k, value : v } -> Dict (Num k) v
fromList = \xs ->
    List.walkRight xs (\{ key, value }, dict -> insert key value dict) empty

foldr : (k, v, b -> b), b, Dict k v -> b
foldr = \func, acc, t ->
    when t is
        Empty ->
            acc

        Node _ key value left right ->
            foldr func (func key value (foldr func acc right)) left

#  Convert a dictionary into an association list of key-value pairs, sorted by keys.
toList : Dict k v -> List { key : k, value : v }
toList = \dict ->
    foldr (\key, value, list -> List.append list { key, value }) [] dict

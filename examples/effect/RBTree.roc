interface RBTree exposes [ Dict, empty, singleton, size, isEmpty, insert, remove, update ] imports []

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
size : Dict k v -> Int
size = \dict ->
    sizeHelp 0 dict

sizeHelp : Int, Dict k v -> Int
sizeHelp = \n, dict ->
  when dict is
    Empty ->
      n

    Node _ _ _ left right ->
      sizeHelp (sizeHelp (n+1) right) left

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

insertHelp : (Key k), v, Dict (Key k) v -> Dict (Key k) v
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
removeHelp : Key k -> Dict (Key k) v -> Dict (Key k) v
removeHelp = \targetKey, dict ->
  when dict is
    RBEmpty_elm_builtin ->
      RBEmpty_elm_builtin

    RBNode_elm_builtin color key value left right ->
      if targetKey < key then
        when left is
          RBNode_elm_builtin Black _ _ lLeft _ ->
            when lLeft is
              RBNode_elm_builtin Red _ _ _ _ ->
                RBNode_elm_builtin color key value (removeHelp targetKey left) right

              _ ->
                when moveRedLeft dict is
                  RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                    balance nColor nKey nValue (removeHelp targetKey nLeft) nRight

                  RBEmpty_elm_builtin ->
                    RBEmpty_elm_builtin

          _ ->
            RBNode_elm_builtin color key value (removeHelp targetKey left) right
      else
        removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT : Key k -> Dict (Key k) v -> NColor -> (Key k) -> v -> Dict (Key k) v -> Dict (Key k) v -> Dict (Key k) v
removeHelpPrepEQGT = \targetKey, dict, color, key, value, left, right ->
  when left is
    RBNode_elm_builtin Red lK lV lLeft lRight ->
      RBNode_elm_builtin
        color
        lK
        lV
        lLeft
        (RBNode_elm_builtin Red key value lRight right)

    _ ->
      when right is
        RBNode_elm_builtin Black _ _ (RBNode_elm_builtin Black _ _ _ _) _ ->
          moveRedRight dict

        RBNode_elm_builtin Black _ _ RBEmpty_elm_builtin _ ->
          moveRedRight dict

        _ ->
          dict


# When we find the node we are looking for, we can remove by replacing the key-value
# pair with the key-value pair of the left-most node on the right side (the closest pair).
removeHelpEQGT : Key k -> Dict (Key k) v -> Dict (Key k) v
removeHelpEQGT = \targetKey, dict ->
  when dict is
    RBNode_elm_builtin color key value left right ->
      if targetKey == key then
        when getMin right is
          RBNode_elm_builtin _ minKey minValue _ _ ->
            balance color minKey minValue left (removeMin right)

          RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin
      else
        balance color key value left (removeHelp targetKey right)

    RBEmpty_elm_builtin ->
      RBEmpty_elm_builtin

getMin : Dict k v -> Dict k v
getMin = \dict ->
  when dict is
    RBNode_elm_builtin _ _ _ ((RBNode_elm_builtin _ _ _ _ _) as left) _ ->
      getMin left

    _ ->
      dict

removeMin : Dict k v -> Dict k v
removeMin = \dict ->
  when dict is
    RBNode_elm_builtin color key value ((RBNode_elm_builtin lColor _ _ lLeft _) as left) right ->
      when lColor is
        Black ->
          when lLeft is
            RBNode_elm_builtin Red _ _ _ _ ->
              RBNode_elm_builtin color key value (removeMin left) right

            _ ->
              when moveRedLeft dict is
                RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                  balance nColor nKey nValue (removeMin nLeft) nRight

                RBEmpty_elm_builtin ->
                  RBEmpty_elm_builtin

        _ ->
          RBNode_elm_builtin color key value (removeMin left) right

    _ ->
      RBEmpty_elm_builtin

moveRedLeft : Dict k v -> Dict k v
moveRedLeft = \dict ->
  when dict is
    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV ((RBNode_elm_builtin Red rlK rlV rlL rlR) as rLeft) rRight) ->
      RBNode_elm_builtin
        Red
        rlK
        rlV
        (RBNode_elm_builtin Black k v (RBNode_elm_builtin Red lK lV lLeft lRight) rlL)
        (RBNode_elm_builtin Black rK rV rlR rRight)

    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
      when clr is
        Black ->
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

        Red ->
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

    _ ->
      dict

moveRedRight : Dict k v -> Dict k v
moveRedRight = \dict ->
  when dict is
    Node clr k v (Node lClr lK lV (Node Red llK llV llLeft llRight) lRight) (Node rClr rK rV rLeft rRight) ->
      Node
        Red
        lK
        lV
        (Node Black llK llV llLeft llRight)
        (Node Black k v lRight (Node Red rK rV rLeft rRight))

    Node clr k v (Node lClr lK lV lLeft lRight) (Node rClr rK rV rLeft rRight) ->
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

# Update the value of a dictionary for a specific key with a given function.
update : Key k -> (Maybe v -> Maybe v) -> Dict (Key k) v -> Dict (Key k) v
update = \targetKey, alter, dictionary ->
  when alter (get targetKey dictionary) is
    Just value ->
      insert targetKey value dictionary

    Nothing ->
      remove targetKey dictionary

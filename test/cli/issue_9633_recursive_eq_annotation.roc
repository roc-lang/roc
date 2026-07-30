package [] {}

Tree := [Nil, Node(Tree)].{
    is_eq : _
}

tree : Tree
tree = Nil

expect tree == tree

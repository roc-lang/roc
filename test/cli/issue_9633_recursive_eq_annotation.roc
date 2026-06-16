package [] {}

Tree := [Nil, Node(Tree)]

tree : Tree
tree = Nil

expect tree == tree

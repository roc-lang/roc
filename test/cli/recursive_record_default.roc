package [] {}

Node := { next : Node ?? Node.{} }

root : Node
root = Node.{}

expect True

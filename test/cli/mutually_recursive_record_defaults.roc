package [] {}

Left := { right : Right ?? Right.{} }
Right := { left : Left ?? Left.{} }

root : Left
root = Left.{}

expect True

app [first!, second!] { pf: platform "./platform.roc" }

first! : Box(U64) => Box(U64)
first! = |boxed| Box.box(Box.unbox(boxed) + 1)

second! : Box(U64) => Box(U64)
second! = |boxed| Box.box(Box.unbox(boxed) * 2)

app [addInts, multiplyInts] { pf: platform "./platform/main.roc" }

addInts : I64, I64 -> I64
addInts = |a, b| a + b

multiplyInts : I64, I64 -> I64
multiplyInts = |a, b| a * b

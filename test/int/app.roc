app [multiplyInts] { pf: platform "./platform/main.roc" }

multiplyInts : I64, I64 -> I64
multiplyInts = |a, b| a * b

platform ""
    requires {} { addInts : I64, I64 -> I64, multiplyInts : I64, I64 -> I64 }
    exposes []
    packages {}
    provides { addInts: "addInts", multiplyInts: "multiplyInts" }

addInts : I64, I64 -> I64

multiplyInts : I64, I64 -> I64

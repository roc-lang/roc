app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main : { f: I64, I64 -> I64 }
main = { f: increment } 

increment : I64, I64 -> I64
increment = \x, y -> x + y 

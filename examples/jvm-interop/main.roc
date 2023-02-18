app "libhello"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main : U64 -> Str
main = \num ->
    if num == 0 then
        "I need a positive number here!"
    else
        str = Num.toStr num
        "The number was \(str), OH YEAH!!! 🤘🤘"

# main : Str -> Str
# main = \name -> "Hello from roc \(name)!!!"

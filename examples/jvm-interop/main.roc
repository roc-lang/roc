app "libhello"
    packages { pf: "platform/main.roc" }
    imports []
    provides [interpolate] to pf

interpolate : I32 -> Str
interpolate = \num ->
    if num == 0 then
        "I need a non-zero number here!"
    else
        str = Num.toStr num
        "The number was \(str), OH YEAH!!! 🤘🤘"

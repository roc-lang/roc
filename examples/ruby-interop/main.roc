app "libhello"
    packages { pf: "platform/main.roc" }
    imports []
    provides [makeItRoc] to pf

makeItRoc : Str -> Str
makeItRoc = \str ->
    if Str.isEmpty str then
        "I need a string here!"
    else
        "\(str), OH YEAH!!! 🤘🤘"

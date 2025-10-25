app [processString] { pf: platform "./platform/main.roc" }

processString : Str -> Str
processString = |input|
    "unclosed string

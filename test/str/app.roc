app [processString] { pf: platform "./platform/main.roc" }

processString : Str -> Str
processString = |_input|
    "Got a string from the host"

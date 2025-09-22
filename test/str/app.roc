app [processString] { pf: platform "./platform/main.roc" }

processString : Str -> Str
processString = |input|
    "Got the following from the host: ${input}\n"

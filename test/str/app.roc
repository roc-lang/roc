app [process_string] { pf: platform "./platform/main.roc" }

process_string : Str -> Str
process_string = |input|
    "Got the following from the host: ${input}"

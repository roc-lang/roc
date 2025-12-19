# TEST: Direct call to Core from app
app [process_string] { pf: platform "./platform/main.roc" }

import pf.Core

process_string : Str -> Str
process_string = |input|
    # Include expected substring for test validation, plus Core.wrap result
    "Got the following from the host: ${input} ${Core.wrap(input)}"

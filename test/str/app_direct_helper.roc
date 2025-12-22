# TEST: Direct call to Helper.simple (which does NOT use Core internally)
app [process_string] { pf: platform "./platform/main.roc" }

import pf.Helper

process_string : Str -> Str
process_string = |input|
    # Include expected substring for test validation
    "Got the following from the host: ${input} ${Helper.simple(input)}"

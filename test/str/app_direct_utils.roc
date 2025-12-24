# TEST: Direct call to Utils (no imports in Utils)
app [process_string] { pf: platform "./platform/main.roc" }

import pf.Utils

process_string : Str -> Str
process_string = |input|
    # Include expected substring for test validation
    "Got the following from the host: ${input} ${Utils.tag(input)}"

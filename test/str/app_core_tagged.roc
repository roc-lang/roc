# TEST: Core.wrap_tagged which calls Utils.tag internally (transitive Core→Utils)
app [process_string] { pf: platform "./platform/main.roc" }

import pf.Core

process_string : Str -> Str
process_string = |input|
    # Include expected substring for test validation
    "Got the following from the host: ${input} ${Core.wrap_tagged(input)}"

# TEST: Transitive call - Helper internally calls Core
app [process_string] { pf: platform "./platform/main.roc" }

import pf.Helper

process_string : Str -> Str
process_string = |input|
    # Include expected substring for test validation, plus transitive call result
    # Helper.wrap_fancy -> Core.wrap
    "Got the following from the host: ${input} ${Helper.wrap_fancy(input)}"

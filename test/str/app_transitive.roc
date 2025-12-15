# TEST CASE 2: Call to Helper which internally calls Core - EXPECTED TO FAIL
app [process_string] { pf: platform "./platform/main.roc" }

import pf.Helper

process_string : Str -> Str
process_string = |input|
    # Transitive call: Helper.wrap_fancy -> Core.wrap
    # This fails with "TypeMismatch in body evaluation"
    Helper.wrap_fancy(input)

# TEST: Direct call to Helper (which uses Core internally)
app [process_string] { pf: platform "./platform/main.roc" }

import pf.Helper

process_string : Str -> Str
process_string = |input|
    Helper.wrap_fancy(input)

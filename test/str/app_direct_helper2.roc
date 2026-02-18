# TEST: Direct import of Helper (first in exposes list) - verifies module can be imported
app [process_string] { pf: platform "./platform/main.roc" }

import pf.Helper

process_string : Str -> Str
process_string = |input|
    # Include expected substring for test validation
    # Uses Helper.simple to verify the import actually works
    "Got the following from the host: ${input} ${Helper.simple(input)}"

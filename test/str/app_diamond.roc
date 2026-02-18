# TEST: Diamond dependency pattern
# Tests: Helper→Core→Utils AND Helper→Utils (both paths to Utils)
app [process_string] { pf: platform "./platform/main.roc" }

import pf.Helper

process_string : Str -> Str
process_string = |input|
    # Helper.wrap_quoted uses both Core.wrap AND Utils.quote
    # This exercises: Helper→Utils (direct) and Helper→Core→Utils (transitive)
    "Got the following from the host: ${input} ${Helper.wrap_quoted(input)}"

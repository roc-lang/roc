# `Values.boom` crashes when evaluated; `roc check` evaluates the
# specialization this module's expect uses and reports the crash.
Crash := [].{}

import Values

expect List.len(List.append(Values.boom, 1.U64)) == 1

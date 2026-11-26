platform ""
    requires {}
    exposes [Outer]
    packages {}
    provides { "roc_value": value }

import Outer

value : Outer -> Outer
value = |outer| outer

platform ""
    requires {}
    exposes [Outer]
    packages {}
    provides { "value": value }

import Outer

value : Outer -> Outer
value = |outer| outer

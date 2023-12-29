interface Dep3Blah
    exposes [one, two, foo, bar]
    imports []

import Dep3Other

one = 1

two = 2

foo = "foo from Dep3"
bar = Dep3Other.bar

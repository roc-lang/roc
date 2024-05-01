interface OneDep
    exposes [str]
    imports []

import Dep3 exposing [foo]

str = foo

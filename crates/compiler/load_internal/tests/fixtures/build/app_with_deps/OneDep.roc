interface OneDep
    exposes [str]
    imports []

import Dep3Blah exposing [foo]

str = foo

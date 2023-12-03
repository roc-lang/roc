interface OneDep
    exposes [str]
    imports [Dep3.Blah.{ foo }]

str = foo

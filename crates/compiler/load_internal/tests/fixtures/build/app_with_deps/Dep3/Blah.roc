interface Dep3.Blah
    exposes [one, two, foo, bar]
    imports [Dep3.Other]

one = 1

two = 2

foo = "foo from Dep3"
bar = Dep3.Other.bar

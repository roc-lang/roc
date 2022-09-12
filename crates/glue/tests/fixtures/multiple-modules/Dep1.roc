interface Dep1 exposes [DepStr1, string] imports []

DepStr1 := [ S Str ]

string = \s -> @DepStr1 (S s)

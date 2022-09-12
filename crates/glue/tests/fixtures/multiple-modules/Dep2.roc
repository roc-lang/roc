interface Dep2 exposes [DepStr2, string] imports []

DepStr2 := [ R Str ]

string = \s -> @DepStr2 (R s)

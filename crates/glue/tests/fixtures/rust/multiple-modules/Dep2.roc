module [DepStr2, string]

DepStr2 := [R Str]

string = \s -> @DepStr2(R(s))

module [DepStr1, string]

DepStr1 := [S Str]

string = \s -> @DepStr1(S(s))

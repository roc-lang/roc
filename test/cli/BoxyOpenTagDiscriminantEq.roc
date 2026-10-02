BoxyOpenTagDiscriminantEq := {}

# repro for https://github.com/roc-lang/roc/issues/11871: equality against a
# payload-free tag on an open tag union in a generic function tests the tag.
f = |x| x == B
g = |x| x != B
h : [A, B, C(U8)] -> Bool
h = |x| x == C(3) or x == A

expect f(A) == Bool.False
expect f(B)
expect g(A)
expect g(B) == Bool.False
expect h(C(3))
expect h(A)
expect h(B) == Bool.False
expect f(C("x")) == Bool.False

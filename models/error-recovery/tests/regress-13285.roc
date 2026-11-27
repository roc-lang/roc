# expect: pass errors
# https://github.com/roc-lang/roc/issues/13285 (fixed): an alias and a nominal type with the same name must be
# reported as a redeclaration, not panic.
X : U64

X := [].{
    one = 1
}

y : U64
y = X.one

main! = |_args| Ok({})

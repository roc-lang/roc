# Repro for https://github.com/roc-lang/roc/issues/10809: reading a declaration
# that another module never gave a value is reported here, at the reference.
import Vals

o = Vals.d

main! = |_| Ok({})

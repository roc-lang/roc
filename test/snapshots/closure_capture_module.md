# META
~~~ini
description=Test closure capture resolution uses source module environment
type=snippet
~~~
# SOURCE
~~~roc
# Closures should capture variables from their defining module
outer_val : U64
outer_val = 42

make_closure : {} -> ({} -> U64)
make_closure = |{}|
    || outer_val

use_closure : U64
use_closure = make_closure({})({})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL

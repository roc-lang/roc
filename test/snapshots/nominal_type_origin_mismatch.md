# META
~~~ini
description=Type mismatch showing nominal type origin from different module
type=file
~~~
# SOURCE
~~~roc
module []

import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"

main =
    # This will cause a type mismatch
    expectsPerson("not a person")
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpAssign LineComment LowerIdent OpenRound String CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


import Data exposing [Person]
expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"
main = # This will cause a type mismatch
expectsPerson("not a person")
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **p** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_p` to suppress this warning.
The unused variable is declared here:

**nominal_type_origin_mismatch.md:6:18:6:19:**
```roc
expectsPerson = |p| "Got a person"
```
                 ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

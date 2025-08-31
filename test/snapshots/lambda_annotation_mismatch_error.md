# META
~~~ini
description=Lambda annotation mismatch error message test - verifies error messages assume annotation is correct and implementation is wrong
type=file
~~~
# SOURCE
~~~roc
module [string_function, wrong_type_function]

# Annotation says it takes and returns strings, but implementation uses number addition
string_function : Str -> Str
string_function = |x| x + 42

# Annotation says function returns I64, but implementation returns Frac(_prec)
wrong_type_function : I64 -> I64
wrong_type_function = |x| x * 3.14
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Float ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "string_function")

    (lc "wrong_type_function")
))
~~~
# FORMATTED
~~~roc
module [string_function, wrong_type_function]


# Annotation says it takes and returns strings, but implementation uses number addition
string_function : Str -> Str
string_function = |x| x + 42
# Annotation says function returns I64, but implementation returns Frac(_prec)
wrong_type_function : I64 -> I64
wrong_type_function = |x| x * 3.14
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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

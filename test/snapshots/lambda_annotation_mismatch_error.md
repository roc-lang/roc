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
TYPE MISMATCH - lambda_annotation_mismatch_error.md:5:27:5:29
TYPE MISMATCH - lambda_annotation_mismatch_error.md:9:31:9:35
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "string_function")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "string_function"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "wrong_type_function")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "wrong_type_function"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~

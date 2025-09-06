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
(block
  (binop_colon
    (lc "string_function")
    (binop_arrow_call
      (uc "Str")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "string_function")
    (lambda
      (body
        (binop_plus
          (lc "x")
          (num_literal_i32 42)
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "wrong_type_function")
    (binop_arrow_call
      (uc "I64")
      (uc "I64")
    )
  )
  (binop_equals
    (lc "wrong_type_function")
    (lambda
      (body
        (binop_star
          (lc "x")
          (frac_literal_small 3.14)
        )
      )
      (args
        (lc "x")
      )
    )
  )
)
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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "string_function"))
    (type type_6)
  )
  (Stmt.assign
    (pattern (Patt.ident "string_function"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "wrong_type_function"))
    (type type_18)
  )
  (Stmt.assign
    (pattern (Patt.ident "wrong_type_function"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 32
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 -> #29)
(var #9 _)
(var #10 -> #11)
(var #11 -> #12)
(var #12 Num *)
(var #13 -> #29)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #31)
(var #21 _)
(var #22 -> #23)
(var #23 -> #24)
(var #24 F64)
(var #25 -> #31)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 fn_pure)
(var #30 _)
(var #31 fn_pure)
~~~
# TYPES
~~~roc
string_function : _arg -> Num(_size)
wrong_type_function : _arg -> F64
x : _a
~~~

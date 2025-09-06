# META
~~~ini
description=Record with special character fields (ok case)
type=expr
~~~
# SOURCE
~~~roc
{
    field_with_underscores: "underscore",
    field123: "numbers",
    camelCase: "camel",
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "field_with_underscores")
    (str_literal_big "underscore")
  )
  (binop_colon
    (lc "field123")
    (str_literal_big "numbers")
  )
  (binop_colon
    (lc "camelCase")
    (str_literal_big "camel")
  )
)
~~~
# FORMATTED
~~~roc
{
	field_with_underscores: "underscore",
	field123: "numbers",
	camelCase: "camel",
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.record_field
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 12
(var #0 _)
(var #1 _)
(var #2 Str)
(var #3 _)
(var #4 _)
(var #5 Str)
(var #6 _)
(var #7 _)
(var #8 Str)
(var #9 _)
(var #10 -> #11)
(var #11 {})
~~~
# TYPES
~~~roc
~~~

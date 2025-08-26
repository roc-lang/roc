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
	field_with_underscores : "underscore",
	field123 : "numbers",
	camelCase : "camel",
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_double_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_slash :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

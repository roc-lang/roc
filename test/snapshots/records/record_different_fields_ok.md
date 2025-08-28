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
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "field_with_underscores")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "field123")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "camelCase")
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
{}
~~~

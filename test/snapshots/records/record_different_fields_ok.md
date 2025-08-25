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
(block
  (binop_colon
    (lc "field_with_underscores")
    (tuple_literal
      (binop_colon
        (tuple_literal
          (binop_colon
            (tuple_literal
              (str_literal_big "underscore")
              (lc "field123")
            )
            (str_literal_big "numbers")
          )
          (lc "camelCase")
        )
        (str_literal_big "camel")
      )
      (malformed malformed:expr_unexpected_token)
    )
  )
)
~~~
# FORMATTED
~~~roc
field_with_underscores: ((
	(
		"underscore",
		field123,
	): "numbers",
	camelCase,
): "camel", <malformed>)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 5:1 to 5:1

**Parse Error**
at 1:1 to 5:2

**Unsupported Node**
at 1:1 to 1:1

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "field_with_underscores")
    (Expr.malformed)
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

# META
~~~ini
description=Record with special character fields (error cases)
type=expr
~~~
# SOURCE
~~~roc
{
    _privateField: "leading underscore",
    field_: "trailing underscore",
    PascalCase: "pascal",
    kebab-case: "kebab",
    field$special: "dollar",
    field@symbol: "at symbol",
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma UpperIdent OpColon String Comma LowerIdent OpUnaryMinus LowerIdent OpColon String Comma LowerIdent MalformedUnknownToken LowerIdent OpColon String Comma LowerIdent MalformedOpaqueNameWithoutName LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "_privateField")
    (tuple_literal
      (binop_colon
        (tuple_literal
          (binop_colon
            (tuple_literal
              (binop_colon
                (tuple_literal
                  (str_literal_big "leading underscore")
                  (lc "field_")
                )
                (str_literal_big "trailing underscore")
              )
              (uc "PascalCase")
            )
            (str_literal_big "pascal")
          )
          (binop_minus
            (lc "kebab")
            (lc "case")
          )
        )
        (str_literal_big "kebab")
      )
      (lc "field")
    )
  )
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "special")
    (tuple_literal
      (str_literal_big "dollar")
      (lc "field")
    )
  )
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "symbol")
    (tuple_literal
      (str_literal_big "at symbol")
      (malformed malformed:expr_unexpected_token)
    )
  )
)
~~~
# FORMATTED
~~~roc
_privateField: (
	(
		(
			(
				"leading underscore",
				field_,
			): "trailing underscore",
			PascalCase,
		): "pascal",
		kebab - case,
	): "kebab",
	field,
)
<malformed>special: (
	"dollar",
	field,
)
<malformed>symbol: ("at symbol", <malformed>)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:10 to 6:10

**Parse Error**
at 7:10 to 7:10

**Parse Error**
at 8:1 to 8:1

**Parse Error**
at 1:1 to 8:2

**Unsupported Node**
at 6:10 to 6:11

**Unsupported Node**
at 6:10 to 6:10

**Unsupported Node**
at 7:10 to 7:11

**Unsupported Node**
at 7:10 to 7:10

**Unsupported Node**
at 1:1 to 1:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "_privateField")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "special")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "symbol")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

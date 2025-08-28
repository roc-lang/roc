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
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma UpperIdent OpColon String Comma LowerIdent OpUnaryMinus LowerIdent OpColon String Comma LowerIdent MalformedUnknownToken LowerIdent OpColon String Comma LowerIdent MalformedNominalNameWithoutName LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "_privateField")
    (str_literal_big "leading underscore")
  )
  (binop_colon
    (lc "field_")
    (str_literal_big "trailing underscore")
  )
  (binop_colon
    (uc "PascalCase")
    (str_literal_big "pascal")
  )
  (binop_colon
    (binop_minus
      (lc "kebab")
      (lc "case")
    )
    (str_literal_big "kebab")
  )
  (lc "field")
)
~~~
# FORMATTED
~~~roc
{ _privateField : "leading underscore", field_ : "trailing underscore", PascalCase : "pascal", kebab - case : "kebab", field }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 6:10

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "_privateField")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "field_")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.binop_minus
      (Expr.lookup "kebab")
      (Expr.lookup "case")
    )
    (Expr.str_literal_big)
  )
  (Expr.lookup "field")
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

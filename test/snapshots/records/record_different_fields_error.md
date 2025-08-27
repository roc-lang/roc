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

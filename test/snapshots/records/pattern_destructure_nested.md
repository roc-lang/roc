# META
~~~ini
description=Nested record destructuring pattern in a match expression
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly CloseCurly OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "person")
)
  (branch1     (binop_thick_arrow
      (record_literal
        (lc "name")
        (binop_colon
          (lc "address")
          (record_literal
            (lc "street")
            (lc "city")
            (lc "zipCode")
          )
        )
      )
      (str_literal_big "${name} lives on ${street} in ${city}")
    )
))
~~~
# FORMATTED
~~~roc
match person
	{name, address: {street, city, zipCode}} => "${name} lives on ${street} in ${city}"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:50 to 2:52

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

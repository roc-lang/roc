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
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly CloseCurly OpThinArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "person")
)
  (branch1     (binop_thick_arrow
      (record_literal
        (binop_colon
          (lc "name")
          (lc "name")
        )
        (binop_colon
          (lc "address")
          (record_literal
            (binop_colon
              (lc "street")
              (lc "street")
            )
            (binop_colon
              (lc "city")
              (lc "city")
            )
            (binop_colon
              (lc "zipCode")
              (lc "zipCode")
            )
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
	{name: name, address: {street: street, city: city, zipCode: zipCode}} => "${name} lives on ${street} in ${city}"
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_destructure_nested.md:1:7:1:13
UNUSED VARIABLE - pattern_destructure_nested.md:2:38:2:45
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**pattern_destructure_nested.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 17
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 Str)
(var #15 _)
(var #16 _)
~~~
# TYPES
~~~roc
~~~

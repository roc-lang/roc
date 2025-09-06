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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_destructure_nested.md:2:5:2:92:**
```roc
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
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
~~~
# TYPES
~~~roc
~~~

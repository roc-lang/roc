# META
~~~ini
description=Simple record destructuring pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, age } => name
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma LowerIdent CloseCurly OpThinArrow LowerIdent CloseCurly ~~~
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
          (lc "age")
          (lc "age")
        )
      )
      (lc "name")
    )
))
~~~
# FORMATTED
~~~roc
match person
	{name: name, age: age} => name
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_destructure_simple.md:1:7:1:13
UNUSED VARIABLE - pattern_destructure_simple.md:2:13:2:16
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**pattern_destructure_simple.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**pattern_destructure_simple.md:2:22:2:26:**
```roc
    { name, age } => name
```
                     ^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
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
~~~
# TYPES
~~~roc
~~~

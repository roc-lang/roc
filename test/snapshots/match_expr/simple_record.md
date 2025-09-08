# META
~~~ini
description=Simple record destructuring in match expression
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name } => name
    { age } => age
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent CloseCurly OpThinArrow LowerIdent OpenCurly LowerIdent CloseCurly OpThinArrow LowerIdent CloseCurly ~~~
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
      )
      (block
        (lc "name")
        (binop_thick_arrow
          (block
            (lc "age")
          )
          (lc "age")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match person
	{name: name} =>
		name
		{
			age
		} => age
~~~
# EXPECTED
UNDEFINED VARIABLE - simple_record.md:1:7:1:13
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**simple_record.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**simple_record.md:2:17:2:21:**
```roc
    { name } => name
```
                ^^^^


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
age : _a
~~~

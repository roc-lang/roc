# META
~~~ini
description=Record destructuring with field renaming
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly OpThinArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "person")
)
  (branch1     (binop_thick_arrow
      (record_literal
        (binop_colon
          (lc "name")
          (lc "userName")
        )
        (binop_colon
          (lc "age")
          (lc "userAge")
        )
      )
      (str_literal_big "User ${userName} is ${userAge.to_str()} years old")
    )
))
~~~
# FORMATTED
~~~roc
match person
	{name: userName, age: userAge} => "User ${userName} is ${userAge.to_str()} years old"
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_destructure_rename.md:1:7:1:13
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**pattern_destructure_rename.md:1:7:1:13:**
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
; Total type variables: 12
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 Str)
(var #10 _)
(var #11 _)
~~~
# TYPES
~~~roc
userName : _a
userAge : _a
~~~

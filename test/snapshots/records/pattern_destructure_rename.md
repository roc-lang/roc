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
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly OpFatArrow String CloseCurly ~~~
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
NIL
# PROBLEMS
**Unsupported Node**
at 2:38 to 2:40

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

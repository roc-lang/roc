# META
~~~ini
description=Record containing a string field with field access
type=expr
~~~
# SOURCE
~~~roc
{foo: "Hello"}.foo
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String CloseCurly Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_dot
  (block
    (binop_colon
      (lc "foo")
      (str_literal_big "Hello")
    )
  )
  (dot_lc "foo")
)
~~~
# FORMATTED
~~~roc
{
	foo : "Hello"
}..foo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
; Total type variables: 8
(var #0 _)
(var #1 _)
(var #2 Str)
(var #3 _)
(var #4 -> #7)
(var #5 _)
(var #6 _)
(var #7 {})
~~~
# TYPES
~~~roc
~~~

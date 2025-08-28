# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
 f{o,
     ]

foo =

    "onmo %
~~~
# TOKENS
~~~text
LowerIdent OpenCurly LowerIdent Comma CloseSquare LowerIdent OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (lc "f")
  (record_literal
    (lc "o")
    (malformed malformed:expr_unexpected_token)
  )
  (binop_equals
    (lc "foo")
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
f{
	o,
}
foo = 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:6 to 2:6

**Parse Error**
at 1:3 to 4:1

**Parse Error**
at 6:5 to 6:5

# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~

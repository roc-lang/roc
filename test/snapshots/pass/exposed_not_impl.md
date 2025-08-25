# META
~~~ini
description=Module exposes values that are not implemented
type=file
~~~
# SOURCE
~~~roc
module [foo, bar, MyType, OtherType, foo, MyType]

# This module exposes foo, bar, MyType, and OtherType
# but only implements foo and MyType
# This should generate "exposed but not implemented" errors for bar and OtherType
# Also tests redundant exposed entries for foo and MyType

foo = 42

MyType : [A, B, C]
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent Comma UpperIdent Comma UpperIdent Comma LowerIdent Comma UpperIdent CloseSquare LowerIdent OpAssign Int UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "foo")
    (num_literal_i32 42)
  )
  (binop_colon
    (uc "MyType")
    (list_literal
      (tuple_literal
        (uc "A")
        (uc "B")
        (uc "C")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	foo, bar, MyType, OtherType, foo, MyType
]


# This module exposes foo, bar, MyType, and OtherType
# but only implements foo and MyType
# This should generate "exposed but not implemented" errors for bar and OtherType
# Also tests redundant exposed entries for foo and MyType
foo = 42

MyType: [(A, B, C)]
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 10:10 to 10:18

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~

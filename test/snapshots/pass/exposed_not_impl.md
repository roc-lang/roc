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
NO CHANGE
~~~
# EXPECTED
REDUNDANT EXPOSED - exposed_not_impl.md:1:38:1:41
REDUNDANT EXPOSED - exposed_not_impl.md:1:43:1:49
EXPOSED BUT NOT DEFINED - exposed_not_impl.md:1:14:1:17
EXPOSED BUT NOT DEFINED - exposed_not_impl.md:1:27:1:36
# PROBLEMS
**Pattern in Expression Context**
at 10:1 to 10:7

**Unsupported Node**
at 10:10 to 10:18

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~

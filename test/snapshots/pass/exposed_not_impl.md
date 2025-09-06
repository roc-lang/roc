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
KwModule OpenSquare LowerIdent Comma LowerIdent Comma UpperIdent Comma UpperIdent Comma LowerIdent Comma UpperIdent CloseSquare BlankLine LineComment LineComment LineComment LineComment BlankLine LowerIdent OpAssign Int BlankLine UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")

    (lc "bar")

    (uc "MyType")

    (uc "OtherType")

    (lc "foo")

    (uc "MyType")
))
(block
  (binop_equals
    (lc "foo")
    (num_literal_i32 42)
  )
  (binop_colon
    (uc "MyType")
    (list_literal
      (uc "A")
      (uc "B")
      (uc "C")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [foo, bar, MyType, OtherType, foo, MyType]

# This module exposes foo, bar, MyType, and OtherType
# but only implements foo and MyType
# This should generate "exposed but not implemented" errors for bar and OtherType
# Also tests redundant exposed entries for foo and MyType
foo = 42
MyType : [A, B, C]
~~~
# EXPECTED
REDUNDANT EXPOSED - exposed_not_impl.md:1:38:1:41
REDUNDANT EXPOSED - exposed_not_impl.md:1:43:1:49
EXPOSED BUT NOT DEFINED - exposed_not_impl.md:1:14:1:17
EXPOSED BUT NOT DEFINED - exposed_not_impl.md:1:27:1:36
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.type_alias)
)
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
(var #7 -> #8)
(var #8 Num *)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
~~~
# TYPES
~~~roc
foo : Num(_size)
~~~

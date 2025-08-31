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
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.type_anno
    (name node:uc)
    (type list_literal)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

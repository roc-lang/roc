# META
~~~ini
description=Test qualified tag with type annotation
type=file
~~~
# SOURCE
~~~roc
module [value]

MyType := [TagA, TagB]

value : MyType
value = MyType.TagA
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "MyType")
    (list_literal
      (tuple_literal
        (uc "TagA")
        (uc "TagB")
      )
    )
  )
  (binop_colon
    (lc "value")
    (uc "MyType")
  )
  (binop_equals
    (lc "value")
    (binop_pipe
      (uc "MyType")
      (uc "TagA")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 4:1

**Pattern in Expression Context**
at 5:9 to 5:15

**Unsupported Node**
at 6:9 to 6:15

**Pattern in Expression Context**
at 6:15 to 6:19

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "value")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~

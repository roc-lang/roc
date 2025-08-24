# META
~~~ini
description=Simple test for built-in types in scope
type=file
~~~
# SOURCE
~~~roc
module [MyNumber, MyString]

MyNumber : U64
MyString : Str
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma UpperIdent CloseSquare UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "MyNumber")
    (uc "U64")
  )
  (binop_colon
    (uc "MyString")
    (uc "Str")
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
**Pattern in Expression Context**
at 3:1 to 3:9

**Pattern in Expression Context**
at 3:12 to 3:15

**Pattern in Expression Context**
at 4:1 to 4:9

**Pattern in Expression Context**
at 4:12 to 4:15

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
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

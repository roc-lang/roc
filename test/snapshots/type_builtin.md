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
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

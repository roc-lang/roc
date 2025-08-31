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
KwModule OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "MyNumber")

    (uc "MyString")
))
~~~
# FORMATTED
~~~roc
module [MyNumber, MyString]


MyNumber : U64
MyString : Str
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

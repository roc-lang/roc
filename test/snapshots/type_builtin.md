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
module [MyNumber, MyString]

MyNumber : U64
MyString : Str
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
~~~
# TYPES
~~~roc
~~~

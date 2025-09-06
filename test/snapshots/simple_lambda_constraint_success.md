# META
~~~ini
description=Simple lambda constraint success test - verifies bidirectional type checking works correctly
type=file
~~~
# SOURCE
~~~roc
module [addTwo, addTwoF64]

# Should successfully constrain literal 2 to I64
addTwo : I64 -> I64
addTwo = |x| x + 2

# Should successfully constrain literal 2.0 to F64
addTwoF64 : F64 -> F64
addTwoF64 = |x| x + 2.0
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Float ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "addTwo")

    (lc "addTwoF64")
))
~~~
# FORMATTED
~~~roc
module [addTwo, addTwoF64]

# Should successfully constrain literal 2 to I64
addTwo : I64 -> I64
addTwo = |x| x + 2

# Should successfully constrain literal 2.0 to F64
addTwoF64 : F64 -> F64
addTwoF64 = |x| x + 2.0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "addTwo")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "addTwo"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "addTwoF64")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "addTwoF64"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~

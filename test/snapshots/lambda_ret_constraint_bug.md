# META
~~~ini
description=Lambda return type constraint bug - integer literals in lambda bodies should be constrained by function signature
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform/main.roc" }

helper : I64 -> I64
helper = |n| n * 2

main : I64, I64 -> I64
main = |_, _| helper(5)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "platform/main.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [main] { pf: "platform/main.roc" platform [] }

helper : I64 -> I64
helper = |n| n * 2

main : I64 -> I64 -> I64
main = |_, _| helper(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "helper")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "helper"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "main")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
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

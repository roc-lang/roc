# META
~~~ini
description=Multiple type variables in a single type annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Multiple type variables 'a' and 'b' introduced in annotation
swap : (a, b) -> (b, a)
swap = |pair| {
    (first, second) = pair
    (second, first)
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Multiple type variables 'a' and 'b' introduced in annotation
swap : (a, b) -> (b, a)
swap = |pair| {
	(first, second) = pair((second, first))
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "swap")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "swap"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~

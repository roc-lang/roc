# META
~~~ini
description=Polymorphic tuple function with instantiation crash
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# A polymorphic function that expects a tuple
swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

# Call it with two separate arguments instead of a tuple
# This should trigger instantiation and then crash on error reporting
main = swap(1, 2)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound Int Comma Int CloseRound ~~~
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
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [main] { pf: "../basic-cli/platform.roc" platform [] }

# A polymorphic function that expects a tuple
swap : (a, b) -> (b, a)
swap = |x, y| (y, x)

# Call it with two separate arguments instead of a tuple
# This should trigger instantiation and then crash on error reporting
main = swap((1, 2))
~~~
# EXPECTED
TYPE MISMATCH - test_tuple_instantiation_crash.md:9:8:9:12
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "swap")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "swap"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~

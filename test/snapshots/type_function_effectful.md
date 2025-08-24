# META
~~~ini
description=Effectful function type with fat arrow syntax
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

runEffect! : (_a => _b) -> _a => _b
runEffect! = |fn!, x| fn!(x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpBang OpColon OpenRound LowerIdent OpFatArrow LowerIdent CloseRound OpArrow LowerIdent OpFatArrow LowerIdent LowerIdent OpBang OpAssign OpBar LowerIdent OpBang Comma LowerIdent OpBar LowerIdent OpBang OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (lc "runEffect")
  (unary_not <unary>)
  (malformed malformed:expr_unexpected_token)
  (lc "_a")
  (malformed malformed:expr_unexpected_token)
  (lc "_b")
  (lc "runEffect")
  (binop_pipe
    (unary_not <unary>)
    (lc "fn")
  )
  (unary_not <unary>)
  (binop_pipe
    (lc "x")
    (lc "fn")
  )
  (unary_not <unary>)
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (record_literal)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - type_function_effectful.md:3:31:3:33
PARSE ERROR - type_function_effectful.md:3:34:3:36
# PROBLEMS
**Parse Error**
at 3:12 to 3:12

**Parse Error**
at 3:25 to 3:25

**Parse Error**
at 3:31 to 3:31

**Parse Error**
at 4:12 to 4:12

**Parse Error**
at 4:18 to 4:18

**Parse Error**
at 6:7 to 6:7

**Unsupported Node**
at 3:12 to 3:12

**Unsupported Node**
at 3:15 to 3:23

**Unsupported Node**
at 3:25 to 3:25

**Unsupported Node**
at 3:31 to 3:31

**Unsupported Node**
at 4:10 to 4:12

**Unsupported Node**
at 4:18 to 4:18

**Unsupported Node**
at 6:5 to 6:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "runEffect")
  (Expr.unary_not)
  (Expr.malformed)
  (Expr.lookup "_a")
  (Expr.malformed)
  (Expr.lookup "_b")
  (Expr.lookup "runEffect")
  (Expr.lambda)
  (Expr.unary_not)
  (Expr.lambda)
  (Expr.unary_not)
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg, _arg2 -> {}")
~~~
# TYPES
~~~roc
~~~

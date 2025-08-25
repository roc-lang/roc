# META
~~~ini
description=Basic type variable introduction in type annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Type variable 'a' introduced in annotation and used in body
identity : a -> a
identity = |a| a

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "identity")
    (binop_thin_arrow
      (lc "a")
      (lc "a")
    )
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "a")
      )
      (args
        (lc "a")
      )
    )
  )
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
app { pf: ("../basic-cli/platform.roc" platform [main]) }

identity: (a -> a)
identity = \a -> a

main
(<malformed>! | _) | {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 7:7 to 7:7

**Unsupported Node**
at 4:12 to 4:18

**Unsupported Node**
at 5:12 to 5:16

**Unsupported Node**
at 7:5 to 7:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "identity")
    (Expr.malformed)
  )
  (Expr.malformed)
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

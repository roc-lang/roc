# META
~~~ini
description=Simple function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

apply : (_a -> _b) -> _a -> _b
apply = |fn, x| fn(x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "apply")
    (binop_thin_arrow
      (binop_thin_arrow
        (binop_thin_arrow
          (lc "_a")
          (lc "_b")
        )
        (lc "_a")
      )
      (lc "_b")
    )
  )
  (binop_equals
    (lc "apply")
    (lambda
      (body
        (apply_lc
          (lc "fn")
          (lc "x")
        )
      )
      (args
        (tuple_literal
          (lc "fn")
          (lc "x")
        )
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (record_literal)
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	pf: "../basic-cli/main.roc" platform [
		main,
	],
}

apply: (((_a -> _b) -> _a) -> _b)
apply = \(
	fn,
	x
) -> fn(x)
main! = \_ -> {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:10 to 3:31

**Unsupported Node**
at 4:9 to 4:17

**Unsupported Node**
at 6:1 to 6:6

**Unsupported Node**
at 6:9 to 6:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "apply")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~

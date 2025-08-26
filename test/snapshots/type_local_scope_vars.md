# META
~~~ini
description=Function-local type variables in separate scopes
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

outer : a -> a
outer = |x| {
    inner : b -> b
    inner = |y| y

    inner(x)
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpenRound LowerIdent CloseRound CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "outer")
    (binop_thin_arrow
      (lc "a")
      (lc "a")
    )
  )
  (binop_equals
    (lc "outer")
    (lambda
      (body
        (block
          (binop_colon
            (lc "inner")
            (lc "b")
          )
          (malformed malformed:expr_unexpected_token)
          (binop_colon
            (lc "b")
            (lc "b")
          )
          (binop_equals
            (lc "inner")
            (lambda
              (body
                (lc "y")
              )
              (args
                (lc "y")
              )
            )
          )
          (apply_lc
            (lc "inner")
            (lc "x")
          )
        )
      )
      (args
        (lc "x")
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

outer : a -> a
outer = \x -> {
	inner : b
	->
	b : b
	inner = \y -> y
	

inner(x)
}

main! = \_ -> {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 5:15 to 5:15

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~

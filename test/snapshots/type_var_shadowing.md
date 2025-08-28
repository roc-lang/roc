# META
~~~ini
description=Type variable shadowing produces warning but is allowed
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Outer function with type variable 'a'
outer : a -> a
outer = |x| {
    # Inner function shadows outer 'a' with its own 'a'
    inner : a -> a
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
            (binop_thin_arrow
              (lc "a")
              (lc "a")
            )
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
	pf: "../basic-cli/platform.roc" platform [main],
}

outer : a -> a
outer = \x -> {
		# Inner function shadows outer 'a' with its own 'a'
inner : a -> a
	inner = \y -> y
	inner(x)
}
main! = \_ -> {  }
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
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~

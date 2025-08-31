# META
~~~ini
description=Type variable shadowing in nested function annotations
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

outer : a -> a
outer = |x| {
    inner : a -> a  # Shadows outer 'a'
    inner = |y| y

    inner(x)
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpenRound LowerIdent CloseRound CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }


outer : a -> a
outer = |x| {
	inner : a -> a
	# Shadows outer 'a'
	inner = |y| y
	inner(x)
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

# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]C:k||match 0{0|#
0"
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColon LowerIdent OpOr KwMatch Int OpenCurly Int OpBar Int MalformedString CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "C")
    (binop_or
      (lc "k")
      (match <4 branches>)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

C : k || when 0 is {
	0 | 0
	"
}
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:14 to 1:21

**Parse Error**
at 2:2 to 2:2

**Parse Error**
at 1:14 to 3:2

**Parse Error**
at 3:2 to 3:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

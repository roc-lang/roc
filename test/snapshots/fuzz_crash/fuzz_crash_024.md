# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module [module ] { pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
~~~
# TOKENS
~~~text
KwModule OpenSquare KwModule CloseSquare OpenCurly LowerIdent OpColon KwPlatform MalformedString KwVar LowerIdent OpAssign CloseSquare KwVar LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed malformed:exposed_item_unexpected_token)
))
~~~
# FORMATTED
~~~roc
module [module ]

{
	pf : platform 
	".-/main._]where # A

#el
	var t = ]

#el
	var t = 0
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:9 to 1:16

**Parse Error**
at 1:24 to 1:33

**Parse Error**
at 1:33 to 4:1

**Parse Error**
at 4:8 to 7:1

**Parse Error**
at 1:18 to 7:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.binop_colon
      (Expr.lookup "pf")
      (Expr.malformed)
    )
    (Expr.malformed)
    (Expr.binop_equals
      (Expr.lookup "t")
      (Expr.malformed)
    )
    (Expr.binop_equals
      (Expr.lookup "t")
      (Expr.num_literal_i32 0)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

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
(block
  (block
    (binop_colon
      (lc "pf")
      (malformed malformed:expr_unexpected_token)
    )
    (malformed malformed:expr_unexpected_token)
    (binop_equals
      (var_lc "t")
      (malformed malformed:expr_unexpected_token)
    )
    (binop_equals
      (var_lc "t")
      (num_literal_i32 0)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

{
	pf : 
	
	var t = 
	var t = 0
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:9 to 1:9

**Parse Error**
at 1:24 to 1:24

**Parse Error**
at 1:33 to 1:33

**Parse Error**
at 4:8 to 4:8

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

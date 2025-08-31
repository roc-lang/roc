# META
~~~ini
description=Test that rigid type variables are properly instantiated for polymorphic functions
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Identity function with rigid type variable
id : a -> a
id = |x| x

main! = |_| {
    # Should instantiate 'a' as Int
    num = id(42)
    
    # Should instantiate 'a' as Str (fresh instance)
    text = id("hello")
    
    # Should instantiate 'a' as List Int (fresh instance)
    list = id([1, 2, 3])
    
    {}
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound String CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int Comma Int Comma Int CloseSquare CloseRound BlankLine OpenCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

id : a -> a
id = |x| x
main! = |_| {
	num = id(42)
	text = id("hello")
	list = id([1, 2, 3])
	{}
}

# Identity function with rigid type variable
# Should instantiate 'a' as Int
# Should instantiate 'a' as Str (fresh instance)
# Should instantiate 'a' as List Int (fresh instance)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "id")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.lookup "a")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "id")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
id : _b
~~~

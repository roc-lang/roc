# META
~~~ini
description=Simple plaform module
type=file
~~~
# SOURCE
~~~roc
platform ""
	requires {} { main : Str -> Str }
	exposes []
	packages {}
	provides [entrypoint]

entrypoint : Str -> Str
entrypoint = main
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly CloseCurly OpenCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent CloseCurly KwExposes OpenSquare CloseSquare KwPackages OpenCurly CloseCurly KwProvides OpenSquare LowerIdent CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "entrypoint")
    (binop_thin_arrow
      (uc "Str")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "entrypoint")
    (lc "main")
  )
)
~~~
# FORMATTED
~~~roc
platform "" requires main : Str -> Str exposes  []

entrypoint : Str -> Str
entrypoint = main
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "entrypoint")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "entrypoint")
    (Expr.lookup "main")
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
entrypoint : _a
~~~

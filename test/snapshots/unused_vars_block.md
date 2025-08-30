# META
~~~ini
description=Block variables with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

main! = |_| {
    # Regular unused variable - should warn
    unused_var = 42

    # Regular used variable - should be fine
    used_var = 100

    # Another unused variable - should warn
    another_unused = "hello"

    # Underscore variable that is unused - should be fine
    _ignored # Comment 1
     = # Comment 2
      999 # Comment 3

    # Use only the used_var
    result = used_var + 10
    result
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign String LowerIdent OpAssign Int LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

main! = |_| {
	unused_var = 42
	used_var = 100
	another_unused = "hello"
	_ignored = 999
	result = used_var + 10
	result : result
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
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

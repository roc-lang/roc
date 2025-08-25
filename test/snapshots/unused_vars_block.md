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
(block
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "unused_var")
            (num_literal_i32 42)
          )
          (binop_equals
            (lc "used_var")
            (num_literal_i32 100)
          )
          (binop_equals
            (lc "another_unused")
            (str_literal_big "hello")
          )
          (binop_equals
            (lc "_ignored")
            (num_literal_i32 999)
          )
          (binop_equals
            (lc "result")
            (binop_plus
              (lc "used_var")
              (num_literal_i32 10)
            )
          )
          (binop_colon
            (lc "result")
            (lc "result")
          )
        )
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

main! = \_ -> {
	unused_var = 42
	

# Regular used variable - should be fine
used_var = 100
	

# Another unused variable - should warn
another_unused = "hello"
	

# Underscore variable that is unused - should be fine
_ignored # Comment 1 =  # Comment 2
	999 # Comment 3
	

# Use only the used_var
result = used_var + 10
	result: result
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:6

**Unsupported Node**
at 3:9 to 3:13

# CANONICALIZE
~~~clojure
(Expr.block
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

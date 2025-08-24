# META
~~~ini
description=Block variables with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign String LowerIdent OpAssign Int LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
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
      (lc "result")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNUSED VARIABLE - unused_vars_block.md:5:5:5:15
UNUSED VARIABLE - unused_vars_block.md:11:5:11:19
# PROBLEMS
**Parse Error**
at 3:7 to 3:7

**Unsupported Node**
at 3:5 to 3:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg, _arg2 -> _ret")
~~~
# TYPES
~~~roc
~~~

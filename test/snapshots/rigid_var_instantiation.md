# META
~~~ini
description=Polymorphic function with rigid type variable used at multiple call sites
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic identity function with rigid type variable 'a'
identity : a -> a
identity = |x| x

# Use identity at different call sites with different types
main! = |_| {
    # First call with number
    num = identity(42)
    
    # Second call with string
    str = identity("hello")
    
    # Third call with list
    lst = identity([1, 2, 3])
    
    {}
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int Comma Int Comma Int CloseSquare CloseRound OpenCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "identity")
    (binop_thin_arrow
      (lc "a")
      (lc "a")
    )
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (block
      (binop_equals
        (lc "num")
        (apply_lc
          (lc "identity")
          (num_literal_i32 42)
        )
      )
      (binop_equals
        (lc "str")
        (apply_lc
          (lc "identity")
          (str_literal_big "hello")
        )
      )
      (binop_equals
        (lc "lst")
        (apply_lc
          (lc "identity")
          (list_literal
            (tuple_literal
              (num_literal_i32 1)
              (num_literal_i32 2)
              (num_literal_i32 3)
            )
          )
        )
      )
      (record_literal)
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNUSED VARIABLE - rigid_var_instantiation.md:10:5:10:8
UNUSED VARIABLE - rigid_var_instantiation.md:13:5:13:8
UNUSED VARIABLE - rigid_var_instantiation.md:16:5:16:8
# PROBLEMS
**Parse Error**
at 8:7 to 8:7

**Unsupported Node**
at 4:12 to 4:18

**Unsupported Node**
at 5:12 to 5:16

**Unsupported Node**
at 8:5 to 8:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "identity")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~

# META
~~~ini
description=Test showing type error that would occur if rigid variables were not instantiated
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic function that swaps elements of a tuple
swap : (a, b) -> (b, a)
swap = |pair| {
    (x, y) = pair
    (y, x)
}

# Multiple uses that would conflict if 'a' and 'b' weren't instantiated
main! = |_| {
    # First use: swap (Int, Str)
    result1 = swap((42, "hello"))
    
    # Second use: swap (Bool, List Int)
    # This would fail if 'a' and 'b' from the first call were reused
    result2 = swap((Bool.true, [1, 2, 3]))
    
    # Third use: swap (Str, Str) 
    # This shows even when both types are the same, we still need fresh vars
    result3 = swap(("foo", "bar"))
    
    {}
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound OpenRound Int Comma String CloseRound CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenRound UpperIdent Dot LowerIdent Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenRound String Comma String CloseRound CloseRound OpenCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "swap")
    (binop_thin_arrow
      (tuple_literal
        (lc "a")
        (lc "b")
      )
      (tuple_literal
        (lc "b")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "swap")
    (lambda
      (body
        (block
          (binop_equals
            (tuple_literal
              (lc "x")
              (lc "y")
            )
            (apply_lc
              (lc "pair")
              (tuple_literal
                (lc "y")
                (lc "x")
              )
            )
          )
        )
      )
      (args
        (lc "pair")
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
        (lc "result1")
        (apply_lc
          (lc "swap")
          (tuple_literal
            (num_literal_i32 42)
            (str_literal_big "hello")
          )
        )
      )
      (binop_equals
        (lc "result2")
        (apply_lc
          (lc "swap")
          (tuple_literal
            (binop_pipe
              (uc "Bool")
              (dot_lc "true")
            )
            (list_literal
              (tuple_literal
                (num_literal_i32 1)
                (num_literal_i32 2)
                (num_literal_i32 3)
              )
            )
          )
        )
      )
      (binop_equals
        (lc "result3")
        (apply_lc
          (lc "swap")
          (tuple_literal
            (str_literal_small "foo")
            (str_literal_small "bar")
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
UNDEFINED VARIABLE - rigid_var_no_instantiation_error.md:17:21:17:30
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:13:5:13:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:17:5:17:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:21:5:21:12
# PROBLEMS
**Parse Error**
at 11:7 to 11:7

**Unsupported Node**
at 4:13 to 4:24

**Unsupported Node**
at 5:8 to 5:15

**Unsupported Node**
at 11:5 to 11:7

**Unsupported Node**
at 17:21 to 17:25

**Unsupported Node**
at 17:32 to 17:42

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "swap")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg, _arg2 -> {}")
~~~
# TYPES
~~~roc
~~~

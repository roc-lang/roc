# META
~~~ini
description=Test showing type error that would occur if rigid variables were not instantiated
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound OpenRound Int Comma String CloseRound CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenRound UpperIdent Dot LowerIdent Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenRound String Comma String CloseRound CloseRound OpenCurly CloseCurly CloseCurly ~~~
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
  (binop_equals
    (not_lc "main")
    (lambda
      (body
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
                  (num_literal_i32 1)
                  (num_literal_i32 2)
                  (num_literal_i32 3)
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
	pf: "../basic-cli/platform.roc" platform [
		main,
	],
}

swap: ((a, b) -> (b, a))
swap = \pair -> {
	(x, y) = pair((y, x))
}

# Multiple uses that would conflict if 'a' and 'b' weren't instantiated
main! = \_ -> {
	result1 = swap((42, "hello"))
	# Second use: swap (Bool, List Int)
# This would fail if 'a' and 'b' from the first call were reused
result2 = swap((Bool.true, [1, 2, 3]))
	# Third use: swap (Str, Str) 
# This shows even when both types are the same, we still need fresh vars
result3 = swap(("foo", "bar"))
	{  }
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 4:13 to 4:24

**Unsupported Node**
at 5:8 to 5:15

**Unsupported Node**
at 11:1 to 11:6

**Unsupported Node**
at 11:9 to 11:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "swap")
    (Expr.malformed)
  )
  (Expr.malformed)
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

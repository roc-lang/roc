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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseCurly BlankLine LineComment LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound OpenRound Int Comma String CloseRound CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound OpenRound UpperIdent Dot LowerIdent Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound OpenRound String Comma String CloseRound CloseRound BlankLine OpenCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (lc "swap")
    (binop_arrow_call
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
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

# Polymorphic function that swaps elements of a tuple
swap : (a, b) -> (b, a)
swap = |pair| {
	(x, y) = pair((y, x))
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
# EXPECTED
UNDEFINED VARIABLE - rigid_var_no_instantiation_error.md:17:21:17:30
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:13:5:13:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:17:5:17:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:21:5:21:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**rigid_var_no_instantiation_error.md:6:18:7:5:**
```roc
    (x, y) = pair
    (y, x)
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "swap"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "swap"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 76
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #66)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #25)
(var #21 -> #65)
(var #22 _)
(var #23 _)
(var #24 -> #64)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 -> #66)
(var #29 _)
(var #30 -> #75)
(var #31 _)
(var #32 -> #37)
(var #33 -> #69)
(var #34 Num *)
(var #35 Str)
(var #36 -> #68)
(var #37 _)
(var #38 _)
(var #39 -> #49)
(var #40 -> #71)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 Num *)
(var #45 Num *)
(var #46 Num *)
(var #47 _)
(var #48 -> #70)
(var #49 _)
(var #50 _)
(var #51 -> #56)
(var #52 -> #73)
(var #53 Str)
(var #54 Str)
(var #55 -> #72)
(var #56 _)
(var #57 _)
(var #58 -> #74)
(var #59 _)
(var #60 -> #75)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 tuple)
(var #65 fn_pure)
(var #66 fn_pure)
(var #67 _)
(var #68 tuple)
(var #69 fn_pure)
(var #70 tuple)
(var #71 fn_pure)
(var #72 tuple)
(var #73 fn_pure)
(var #74 {})
(var #75 fn_pure)
~~~
# TYPES
~~~roc
y : _c
x : _c
result1 : _c
result3 : _c
swap : _arg -> _ret
main : _arg -> _ret
pair : _c
result2 : _c
~~~

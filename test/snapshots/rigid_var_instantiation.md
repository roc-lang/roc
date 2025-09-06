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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound String CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int Comma Int Comma Int CloseSquare CloseRound BlankLine OpenCurly CloseCurly CloseCurly ~~~
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
    (lc "identity")
    (binop_arrow_call
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
  (binop_equals
    (not_lc "main")
    (lambda
      (body
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
                (num_literal_i32 1)
                (num_literal_i32 2)
                (num_literal_i32 3)
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
# EXPECTED
UNUSED VARIABLE - rigid_var_instantiation.md:10:5:10:8
UNUSED VARIABLE - rigid_var_instantiation.md:13:5:13:8
UNUSED VARIABLE - rigid_var_instantiation.md:16:5:16:8
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "identity"))
    (type type_10)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
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
; Total type variables: 50
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
(var #12 -> #43)
(var #13 _)
(var #14 _)
(var #15 -> #43)
(var #16 _)
(var #17 -> #49)
(var #18 _)
(var #19 -> #22)
(var #20 -> #45)
(var #21 Num *)
(var #22 _)
(var #23 _)
(var #24 -> #27)
(var #25 -> #46)
(var #26 Str)
(var #27 _)
(var #28 _)
(var #29 -> #35)
(var #30 -> #47)
(var #31 Num *)
(var #32 Num *)
(var #33 Num *)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 -> #48)
(var #38 _)
(var #39 -> #49)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 fn_pure)
(var #44 _)
(var #45 fn_pure)
(var #46 fn_pure)
(var #47 fn_pure)
(var #48 {})
(var #49 fn_pure)
~~~
# TYPES
~~~roc
num : _b
x : _b
lst : _b
str : _b
identity : _arg -> _ret
main : _arg -> _ret
~~~

# META
~~~ini
description=Basic type annotations with type variables and application
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test generic identity function
identity : a -> a
identity = |x| x

# Test function with multiple type parameters
combine : a, b -> (a, b)
combine = |first, second| (first, second)

# Test type application with concrete types
addOne : U64 -> U64
addOne = |n| n + 1

main! = |_| {
    # Test identity with different types
    num = identity(42)
    text = identity("hello")

    # Test combine function
    pair = combine(num, text)

    # Test concrete function
    result = addOne(5)

    result
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound BlankLine LowerIdent CloseCurly ~~~
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
        (str_literal_big "../basic-cli/main.roc")
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
  (binop_colon
    (lc "combine")
    (binop_arrow_call
      (lc "a")
      (binop_arrow_call
        (lc "b")
        (tuple_literal
          (lc "a")
          (lc "b")
        )
      )
    )
  )
  (binop_equals
    (lc "combine")
    (lambda
      (body
        (tuple_literal
          (lc "first")
          (lc "second")
        )
      )
      (args
        (lc "first")
        (lc "second")
      )
    )
  )
  (binop_colon
    (lc "addOne")
    (binop_arrow_call
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "addOne")
    (lambda
      (body
        (binop_plus
          (lc "n")
          (num_literal_i32 1)
        )
      )
      (args
        (lc "n")
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
            (lc "text")
            (apply_lc
              (lc "identity")
              (str_literal_big "hello")
            )
          )
          (binop_equals
            (lc "pair")
            (apply_lc
              (lc "combine")
              (tuple_literal
                (lc "num")
                (lc "text")
              )
            )
          )
          (binop_equals
            (lc "result")
            (apply_lc
              (lc "addOne")
              (num_literal_i32 5)
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
app [main!] { pf: "../basic-cli/main.roc" platform [] }

# Test generic identity function
identity : a -> a
identity = |x| x
# Test function with multiple type parameters
combine : a -> b -> (a, b)
combine = |first, second| (first, second)
# Test type application with concrete types
addOne : U64 -> U64
addOne = |n| n + 1
main! = |_| {
	# Test identity with different types
	num = identity(42)
	text = identity("hello")
	# Test combine function
	pair = combine((num, text))
	# Test concrete function
	result = addOne(5)
	result : result
}
~~~
# EXPECTED
UNUSED VARIABLE - type_annotation_basic.md:21:5:21:9
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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "combine"))
    (type type_24)
  )
  (Stmt.assign
    (pattern (Patt.ident "combine"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "addOne"))
    (type type_37)
  )
  (Stmt.assign
    (pattern (Patt.ident "addOne"))
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
; Total type variables: 91
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
(var #12 -> #77)
(var #13 _)
(var #14 _)
(var #15 -> #77)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 -> #81)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 -> #80)
(var #32 -> #81)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 -> #83)
(var #40 _)
(var #41 -> #42)
(var #42 -> #43)
(var #43 Num *)
(var #44 -> #83)
(var #45 _)
(var #46 -> #90)
(var #47 _)
(var #48 -> #51)
(var #49 -> #85)
(var #50 Num *)
(var #51 _)
(var #52 _)
(var #53 -> #56)
(var #54 -> #86)
(var #55 Str)
(var #56 _)
(var #57 _)
(var #58 -> #63)
(var #59 -> #88)
(var #60 _)
(var #61 _)
(var #62 -> #87)
(var #63 _)
(var #64 _)
(var #65 -> #68)
(var #66 -> #89)
(var #67 Num *)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 -> #90)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 fn_pure)
(var #78 _)
(var #79 _)
(var #80 tuple)
(var #81 fn_pure)
(var #82 _)
(var #83 fn_pure)
(var #84 _)
(var #85 fn_pure)
(var #86 fn_pure)
(var #87 tuple)
(var #88 fn_pure)
(var #89 fn_pure)
(var #90 fn_pure)
~~~
# TYPES
~~~roc
combine : _arg, _arg2 -> (_field, _field2)
num : _c
result : _c
identity : _arg -> _ret
first : _c
pair : _c
addOne : _arg -> Num(_size)
x : _c
text : _c
second : _c
n : _c
main : _arg -> _ret
~~~

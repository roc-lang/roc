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
          (lc "result")
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
	result
}
~~~
# EXPECTED
UNUSED VARIABLE - type_annotation_basic.md:21:5:21:9
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_annotation_basic.md:4:1:4:9:**
```roc
identity : a -> a
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_annotation_basic.md:5:1:5:9:**
```roc
identity = |x| x
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_annotation_basic.md:8:1:8:8:**
```roc
combine : a, b -> (a, b)
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_annotation_basic.md:9:1:9:8:**
```roc
combine = |first, second| (first, second)
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_annotation_basic.md:12:1:12:7:**
```roc
addOne : U64 -> U64
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_annotation_basic.md:13:1:13:7:**
```roc
addOne = |n| n + 1
```
^^^^^^


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
; Total type variables: 90
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
(var #12 -> #76)
(var #13 _)
(var #14 _)
(var #15 -> #76)
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
(var #26 -> #80)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 -> #79)
(var #32 -> #80)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 -> #82)
(var #40 _)
(var #41 -> #42)
(var #42 -> #43)
(var #43 Num *)
(var #44 -> #82)
(var #45 _)
(var #46 -> #89)
(var #47 _)
(var #48 -> #51)
(var #49 -> #84)
(var #50 Num *)
(var #51 _)
(var #52 _)
(var #53 -> #56)
(var #54 -> #85)
(var #55 Str)
(var #56 _)
(var #57 _)
(var #58 -> #63)
(var #59 -> #87)
(var #60 _)
(var #61 _)
(var #62 -> #86)
(var #63 _)
(var #64 _)
(var #65 -> #68)
(var #66 -> #88)
(var #67 Num *)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 _)
(var #72 -> #89)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 fn_pure)
(var #77 _)
(var #78 _)
(var #79 tuple)
(var #80 fn_pure)
(var #81 _)
(var #82 fn_pure)
(var #83 _)
(var #84 fn_pure)
(var #85 fn_pure)
(var #86 tuple)
(var #87 fn_pure)
(var #88 fn_pure)
(var #89 fn_pure)
~~~
# TYPES
~~~roc
combine : _arg, _arg2 -> (_field, _field2)
addOne : _arg -> Num(_size)
x : _c
second : _c
n : _c
identity : _arg -> _ret
main : _arg -> _ret
first : _c
~~~

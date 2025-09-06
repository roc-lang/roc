# META
~~~ini
description=Type variable shadowing produces warning but is allowed
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Outer function with type variable 'a'
outer : a -> a
outer = |x| {
    # Inner function shadows outer 'a' with its own 'a'
    inner : a -> a
    inner = |y| y

    inner(x)
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpenRound LowerIdent CloseRound CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (lc "outer")
    (binop_arrow_call
      (lc "a")
      (lc "a")
    )
  )
  (binop_equals
    (lc "outer")
    (lambda
      (body
        (block
          (binop_colon
            (lc "inner")
            (binop_arrow_call
              (lc "a")
              (lc "a")
            )
          )
          (binop_equals
            (lc "inner")
            (lambda
              (body
                (lc "y")
              )
              (args
                (lc "y")
              )
            )
          )
          (apply_lc
            (lc "inner")
            (lc "x")
          )
        )
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
        (record_literal)
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

# Outer function with type variable 'a'
outer : a -> a
outer = |x| {
	# Inner function shadows outer 'a' with its own 'a'
	inner : a -> a
	inner = |y| y
	inner(x)
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "outer"))
    (type type_10)
  )
  (Stmt.assign
    (pattern (Patt.ident "outer"))
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
; Total type variables: 44
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
(var #12 -> #40)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 -> #38)
(var #20 _)
(var #21 _)
(var #22 -> #38)
(var #23 _)
(var #24 -> #39)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 -> #40)
(var #29 _)
(var #30 -> #43)
(var #31 _)
(var #32 -> #42)
(var #33 -> #43)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 fn_pure)
(var #39 fn_pure)
(var #40 fn_pure)
(var #41 _)
(var #42 {})
(var #43 fn_pure)
~~~
# TYPES
~~~roc
y : _b
outer : _arg -> _ret
main : _arg -> {}
x : _b
inner : _arg -> _ret
~~~

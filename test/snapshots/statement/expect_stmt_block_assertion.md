# META
~~~ini
description=Debug expression stmt
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo : Bool -> Bool
foo = |a| {
    expect a == Bool.True
    a
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwExpect LowerIdent OpEquals UpperIdent Dot UpperIdent LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
(block
  (binop_colon
    (lc "foo")
    (binop_arrow_call
      (uc "Bool")
      (uc "Bool")
    )
  )
  (binop_equals
    (lc "foo")
    (lambda
      (body
        (block
          (expect
            (binop_double_equals
              (lc "a")
              (binop_dot
                (uc "Bool")
                (uc "True")
              )
            )
          )
          (lc "a")
        )
      )
      (args
        (lc "a")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

foo : Bool -> Bool
foo = |a| {
	expect a == (Bool.True)
	a
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "foo"))
    (type type_5)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 23
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #22)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 -> #22)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 fn_pure)
~~~
# TYPES
~~~roc
foo : _arg -> _ret
a : _b
~~~

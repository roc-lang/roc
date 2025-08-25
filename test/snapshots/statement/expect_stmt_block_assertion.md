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
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwExpect LowerIdent OpEquals UpperIdent Dot UpperIdent LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "foo")
    (binop_thin_arrow
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
              (binop_pipe
                (uc "Bool")
                (uc "True")
              )
            )
          )
          (binop_colon
            (lc "a")
            (lc "a")
          )
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
module [
	foo,
]

foo: (Bool -> Bool)
foo = \a -> {
	expect a == Bool.True
	a: a
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:7 to 3:19

**Unsupported Node**
at 4:7 to 4:11

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "foo")
    (Expr.malformed)
  )
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

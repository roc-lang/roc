# META
~~~ini
description=Record destructuring in assignment statement
type=file
~~~
# SOURCE
~~~roc
module [extract_age]

extract_age : { age : U64 } -> U64
extract_age = |person| {
    { age } = person

	{ a: 0 }.a + age - { a: 0 }.a
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenCurly LowerIdent CloseCurly OpAssign LowerIdent BlankLine OpenCurly LowerIdent OpColon Int CloseCurly Dot LowerIdent OpPlus LowerIdent OpBinaryMinus OpenCurly LowerIdent OpColon Int CloseCurly Dot LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "extract_age")
))
(block
  (binop_colon
    (lc "extract_age")
    (binop_arrow_call
      (block
        (binop_colon
          (lc "age")
          (uc "U64")
        )
      )
      (uc "U64")
    )
  )
  (binop_equals
    (lc "extract_age")
    (lambda
      (body
        (block
          (binop_equals
            (block
              (lc "age")
            )
            (lc "person")
          )
          (binop_minus
            (binop_plus
              (binop_dot
                (block
                  (binop_colon
                    (lc "a")
                    (num_literal_i32 0)
                  )
                )
                (dot_lc "a")
              )
              (lc "age")
            )
            (binop_dot
              (block
                (binop_colon
                  (lc "a")
                  (num_literal_i32 0)
                )
              )
              (dot_lc "a")
            )
          )
        )
      )
      (args
        (lc "person")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [extract_age]

extract_age : {
	age : U64
} -> U64
extract_age = |person| {
	{
		age
	} = person
	(({
		a : 0
	}..a) + age) - ({
		a : 0
	}..a)
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
    (pattern (Patt.ident "extract_age"))
    (type type_8)
  )
  (Stmt.assign
    (pattern (Patt.ident "extract_age"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 39
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
(var #10 -> #38)
(var #11 _)
(var #12 _)
(var #13 -> #14)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 Num *)
(var #18 _)
(var #19 -> #36)
(var #20 _)
(var #21 -> #22)
(var #22 -> #23)
(var #23 -> #29)
(var #24 _)
(var #25 Num *)
(var #26 _)
(var #27 -> #37)
(var #28 _)
(var #29 -> #30)
(var #30 _)
(var #31 _)
(var #32 -> #38)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 {})
(var #37 {})
(var #38 fn_pure)
~~~
# TYPES
~~~roc
extract_age : _arg -> _ret
age : _b
person : _b
~~~

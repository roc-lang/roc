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
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenCurly LowerIdent CloseCurly OpAssign LowerIdent OpenCurly LowerIdent OpColon Int CloseCurly Dot LowerIdent OpPlus LowerIdent OpBinaryMinus OpenCurly LowerIdent OpColon Int CloseCurly Dot LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "extract_age")
    (binop_thin_arrow
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
              (binop_colon
                (lc "age")
                (lc "age")
              )
            )
            (lc "person")
          )
          (binop_minus
            (binop_plus
              (binop_pipe
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
            (binop_pipe
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
		age : age
	} = person
	(({
		a : 0
	} | .a) + age) - ({
		a : 0
	} | .a)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 7:2 to 7:10

**Unsupported Node**
at 7:21 to 7:29

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "extract_age")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "extract_age")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
extract_age : _b
~~~

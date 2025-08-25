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
module [
	extract_age,
]

extract_age: ({
	age: U64
} -> U64)
extract_age = \person -> {
	{
		age: age
	} = person
	

(({
		a: 0
	} | .a) + age) - ({
		a: 0
	} | .a)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:15 to 3:35

**Unsupported Node**
at 4:15 to 4:24

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "extract_age")
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

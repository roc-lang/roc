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
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**module_record_destructure.md:5:5:5:12:**
```roc
    { age } = person
```
    ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **age** in this scope.
Is there an **import** or **exposing** missing up-top?

**module_record_destructure.md:7:15:7:18:**
```roc
	{ a: 0 }.a + age - { a: 0 }.a
```
	             ^^^


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
; Total type variables: 38
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
(var #10 -> #37)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 -> #15)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 Num *)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #23)
(var #23 -> #24)
(var #24 -> #30)
(var #25 _)
(var #26 Num *)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 -> #31)
(var #31 _)
(var #32 _)
(var #33 -> #37)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 fn_pure)
~~~
# TYPES
~~~roc
extract_age : _arg -> _ret
person : _b
~~~

# META
~~~ini
description=Debug expression in a block context both statement and expression versions
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = |num| {
    # statement - prints out the value of num convertert to a string
    dbg num.to_str()

    # expression - prints out the value of num and then returns it
    dbg(num)
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LineComment KwDbg LowerIdent Dot LowerIdent OpenRound CloseRound BlankLine LineComment KwDbg OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
(block
  (binop_equals
    (lc "foo")
    (lambda
      (body
        (block
          (malformed)
          (apply_anon
            (binop_dot
              (lc "num")
              (dot_lc "to_str")
            )
          )
          (apply_anon
            (malformed)
            (lc "num")
          )
        )
      )
      (args
        (lc "num")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

foo = |num| {
	# statement - prints out the value of num convertert to a string
	dbg 
	num..to_str()
	# expression - prints out the value of num and then returns it
	dbg(num)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**dbg_stmt_block_example.md:5:5:5:9:**
```roc
    dbg num.to_str()
```
    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**dbg_stmt_block_example.md:8:5:8:8:**
```roc
    dbg(num)
```
    ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 22
(var #0 _)
(var #1 _)
(var #2 -> #21)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #18)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 -> #21)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 fn_pure)
(var #19 -> #20)
(var #20 fn_pure)
(var #21 fn_pure)
~~~
# TYPES
~~~roc
foo : _arg -> _ret
num : _a
~~~

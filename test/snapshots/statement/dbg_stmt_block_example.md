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
~~~
# FORMATTED
~~~roc
module [foo]

foo = |num| {
	# statement - prints out the value of num convertert to a string
	dbg 
	num.to_str()

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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**dbg_stmt_block_example.md:5:9:5:19:**
```roc
    dbg num.to_str()
```
        ^^^^^^^^^^


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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

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


**UNUSED VARIABLE**
Variable **num** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_num` to suppress this warning.
The unused variable is declared here:

**dbg_stmt_block_example.md:3:8:3:11:**
```roc
foo = |num| {
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

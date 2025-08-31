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
~~~
# FORMATTED
~~~roc
module [foo]

foo : Bool -> Bool
foo = |a| {
	expect a == Bool.True
	a : a
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **a** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_a` to suppress this warning.
The unused variable is declared here:

**expect_stmt_block_assertion.md:6:5:6:6:**
```roc
    a
```
    ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "foo")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~

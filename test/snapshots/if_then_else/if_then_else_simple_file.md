# META
~~~ini
description=Example if-then-else statement
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = if 1 A

    else {
	"hello"
    }
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign KwIf Int UpperIdent BlankLine KwElse OpenCurly String CloseCurly ~~~
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

foo = if 1
	A
else 

{
	"hello"
}
~~~
# EXPECTED
INVALID IF CONDITION - if_then_else_simple_file.md:3:10:3:10
INCOMPATIBLE IF BRANCHES - if_then_else_simple_file.md:3:7:3:7
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.if_else)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~

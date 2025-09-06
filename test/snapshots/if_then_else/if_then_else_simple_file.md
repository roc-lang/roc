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
(block
  (binop_equals
    (lc "foo")
    (if_else
      (condition         (num_literal_i32 1)
)
      (then         (uc "A")
)
      (else         (block
          (str_literal_big "hello")
        )
))
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

foo = if 1
	A
else {
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
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 -> #7)
(var #3 Num *)
(var #4 _)
(var #5 Str)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
~~~
# TYPES
~~~roc
foo : _a
~~~

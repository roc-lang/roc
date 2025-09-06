# META
~~~ini
description=Return type mismatch with instantiated function
type=expr
~~~
# SOURCE
~~~roc
{
    identity : a -> a
    identity = |x| x

    needs_string : ((Str -> Str) -> Str)
    needs_string = |f| f(["hello"])

    needs_string(identity)
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpColon OpenRound OpenRound UpperIdent OpArrow UpperIdent CloseRound OpArrow UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound OpenSquare String CloseSquare CloseRound BlankLine LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "identity")
    (binop_thin_arrow
      (lc "a")
      (lc "a")
    )
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "needs_string")
    (binop_thin_arrow
      (binop_thin_arrow
        (uc "Str")
        (uc "Str")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "needs_string")
    (lambda
      (body
        (apply_lc
          (lc "f")
          (list_literal
            (str_literal_big "hello")
          )
        )
      )
      (args
        (lc "f")
      )
    )
  )
  (apply_lc
    (lc "needs_string")
    (lc "identity")
  )
)
~~~
# FORMATTED
~~~roc
identity : a -> a
identity = |x| x

needs_string :
	(Str -> Str) -> Str
needs_string = |f| f(["hello"])

needs_string(identity)
~~~
# EXPECTED
TYPE MISMATCH - test_instantiated_return_crash.md:6:26:6:35
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "identity")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "needs_string")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "needs_string"))
    (Expr.lambda (canonicalized))
  )
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~

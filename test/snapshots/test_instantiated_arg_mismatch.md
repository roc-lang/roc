# META
~~~ini
description=Type mismatch with instantiated function arguments
type=expr
~~~
# SOURCE
~~~roc
{
    pair : a, a -> (a, a)
    pair = |x, y| (x, y)

    pair(42, "hello")
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpenRound Int Comma String CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(binop_thin_arrow
  (record_literal
    (binop_colon
      (lc "pair")
      (lc "a")
    )
    (lc "a")
  )
  (tuple_literal
    (lc "a")
    (lc "a")
  )
)
~~~
# FORMATTED
~~~roc
{ pair : a, a } -> (a, a)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 2:17

**Unsupported Node**
at 1:1 to 2:26

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~

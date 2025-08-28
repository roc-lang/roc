# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0)
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly LowerIdent OpColon Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (block
    (binop_colon
      (lc "o")
      (num_literal_i32 0)
    )
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

{
	o : 0
	
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:4 to 2:4

**Parse Error**
at 1:20 to 2:5

# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~

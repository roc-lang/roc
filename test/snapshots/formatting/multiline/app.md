# META
~~~ini
description=Multiline formatting app
type=file
~~~
# SOURCE
~~~roc
app [
	a1!,
	a2!,
] {
	pf: platform "../basic-cli/main.roc",
	a: "a",
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang Comma LowerIdent OpBang Comma CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(block
  (list_literal
    (lc "a1")
  )
  (unary_not <unary>)
  (lc "a2")
  (unary_not <unary>)
  (malformed malformed:expr_unexpected_token)
  (record_literal
    (binop_colon
      (lc "pf")
      (malformed malformed:expr_unexpected_token)
    )
    (str_literal_big "../basic-cli/main.roc")
    (tuple_literal
      (binop_colon
        (lc "a")
        (str_literal_small "a")
      )
      (malformed malformed:expr_unexpected_token)
    )
  )
)
~~~
# FORMATTED
~~~roc
app {  }

[
	a1,
]<malformed>!
a2<malformed>!
<malformed>
 {
	pf: <malformed>,
	"../basic-cli/main.roc",
	(a: "a", <malformed>),
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Expected Open Curly Brace**
at 1:1 to 1:5

**Parse Error**
at 1:5 to 2:4

**Parse Error**
at 2:5 to 2:5

**Parse Error**
at 3:5 to 3:5

**Parse Error**
at 4:1 to 4:1

**Parse Error**
at 5:6 to 5:6

**Parse Error**
at 7:1 to 7:1

**Parse Error**
at 4:3 to 7:2

**Unsupported Node**
at 1:5 to 2:5

**Unsupported Node**
at 2:5 to 2:5

**Unsupported Node**
at 3:5 to 3:5

**Unsupported Node**
at 4:1 to 4:1

**Unsupported Node**
at 5:6 to 5:6

**Unsupported Node**
at 1:1 to 1:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.unary_not)
  (Expr.lookup "a2")
  (Expr.unary_not)
  (Expr.malformed)
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "pf")
      (Expr.malformed)
    )
    (Expr.str_literal_big)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "{}")
~~~
# TYPES
~~~roc
~~~

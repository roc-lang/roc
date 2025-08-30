# META
~~~ini
description=Record with reserved keyword fields (error case)
type=expr
~~~
# SOURCE
~~~roc
{
    if: "conditional",
    when: "pattern match",
    expect: "test assertion",
    import: "module load",
    and: Bool.true,
    or: Bool.false,
}
~~~
# TOKENS
~~~text
OpenCurly KwIf OpColon String Comma LowerIdent OpColon String Comma KwExpect OpColon String Comma KwImport OpColon String Comma OpAnd OpColon UpperIdent Dot LowerIdent Comma OpOr OpColon UpperIdent Dot LowerIdent Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (if_without_else
    (condition       (malformed malformed:expr_unexpected_token)
)
    (then       (str_literal_big "conditional")
))
  (binop_colon
    (lc "when")
    (str_literal_big "pattern match")
  )
  (binop_colon
    (malformed malformed:expr_unexpected_token)
    (str_literal_big "test assertion")
  )
  (binop_colon
    (malformed malformed:expr_unexpected_token)
    (str_literal_big "module load")
  )
  (binop_colon
    (malformed malformed:expr_unexpected_token)
    (binop_pipe
      (uc "Bool")
      (dot_lc "true")
    )
  )
  (binop_colon
    (malformed malformed:expr_unexpected_token)
    (binop_pipe
      (uc "Bool")
      (dot_lc "false")
    )
  )
)
~~~
# FORMATTED
~~~roc
{
	if :  "conditional",
	when : "pattern match",
	expect : "test assertion",
	import : "module load",
	and : Bool.true,
	or : Bool.false,
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:7 to 2:9

**Parse Error**
at 4:5 to 4:11

**Parse Error**
at 5:5 to 5:11

**Parse Error**
at 6:5 to 6:8

**Parse Error**
at 7:5 to 7:7

**Unsupported Node**
at 6:10 to 6:14

**Unsupported Node**
at 7:9 to 7:13

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.if_else)
  (Expr.binop_colon
    (Expr.lookup "when")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
{}
~~~

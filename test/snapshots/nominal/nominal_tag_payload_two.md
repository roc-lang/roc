# META
~~~ini
description=Example of a more complex nominal tag union with two payload types
type=file
~~~
# SOURCE
~~~roc
module [MyResult, ok, is_ok]

MyResult(ok, err) := [Ok(ok), Err(err)]

ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)

is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result {
    MyResult.Ok(_) => Bool.True
    MyResult.Err(_) => Bool.False
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent Comma LowerIdent CloseSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent Comma Underscore CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent UpperIdent Dot UpperIdent OpenRound Underscore CloseRound OpFatArrow UpperIdent Dot UpperIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "MyResult")

    (lc "ok")

    (lc "is_ok")
))
~~~
# FORMATTED
~~~roc
module [MyResult, ok, is_ok]

MyResult((ok, err)) := [Ok(ok), Err(err)]
ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)
is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result
	MyResult.Ok(_) => Bool.True
	MyResult.Err(_) => Bool.False
~~~
# EXPECTED
NIL
# PROBLEMS
**Pattern in Expression Context**
at 5:25 to 5:26

**Unsupported Node**
at 10:20 to 10:22

**Unsupported Node**
at 11:5 to 11:20

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.lookup "ok")
    (Expr.binop_thin_arrow
      (Expr.lookup "ok")
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "ok")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "is_ok")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "is_ok")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
ok : _b
is_ok : _b
~~~

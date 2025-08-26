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
(block
  (binop_colon_equals
    (apply_uc
      (uc "MyResult")
      (tuple_literal
        (lc "ok")
        (lc "err")
      )
    )
    (list_literal
      (apply_uc
        (uc "Ok")
        (lc "ok")
      )
      (apply_uc
        (uc "Err")
        (lc "err")
      )
    )
  )
  (binop_colon
    (lc "ok")
    (binop_thin_arrow
      (lc "ok")
      (apply_uc
        (uc "MyResult")
        (tuple_literal
          (lc "ok")
          (underscore)
        )
      )
    )
  )
  (binop_equals
    (lc "ok")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "MyResult")
            (uc "Ok")
          )
          (lc "a")
        )
      )
      (args
        (lc "a")
      )
    )
  )
  (binop_colon
    (lc "is_ok")
    (binop_thin_arrow
      (apply_uc
        (uc "MyResult")
        (tuple_literal
          (lc "_ok")
          (lc "_err")
        )
      )
      (uc "Bool")
    )
  )
  (binop_equals
    (lc "is_ok")
    (lambda
      (body
        (match <57 branches>)
      )
      (args
        (lc "result")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	MyResult,
	ok,
	is_ok,
]

MyResult((ok, err)) := [Ok(ok), Err(err)]

ok : ok -> MyResult (ok, _)
ok = \a -> MyResult.Ok(a)

is_ok : MyResult (_ok, _err) -> Bool
is_ok = \result -> when result is {
	MyResult.Ok(_)
	=>
	Bool.True
	MyResult.Err(_)
	=>
	Bool.False
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 9:18 to 9:31

**Parse Error**
at 10:20 to 10:20

**Parse Error**
at 11:21 to 11:21

**Parse Error**
at 9:18 to 12:2

**Parse Error**
at 12:2 to 12:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~

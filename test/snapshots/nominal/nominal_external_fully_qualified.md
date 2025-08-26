# META
~~~ini
description=Example of external nominal tag union fully qualified name
type=file
~~~
# SOURCE
~~~roc
module [handleResult]

import MyResultModule

handleResult : MyResultModule.MyResultType(Str, I32) -> Str
handleResult = |result| {
    match result {
        MyResultModule.MyResultType.Ok(value) => value
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
    }
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport UpperIdent LowerIdent OpColon UpperIdent Dot UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent UpperIdent Dot UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpFatArrow String CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (uc "MyResultModule")
  )
  (binop_colon
    (lc "handleResult")
    (binop_thin_arrow
      (apply_anon
        (binop_pipe
          (uc "MyResultModule")
          (uc "MyResultType")
        )
        (tuple_literal
          (uc "Str")
          (uc "I32")
        )
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "handleResult")
    (lambda
      (body
        (block
          (match <30 branches>)
        )
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
	handleResult,
]

import MyResultModule

handleResult : MyResultModule.MyResultType((Str, I32)) -> Str
handleResult = \result -> {
	match result {
        MyResultModule.MyResultType.Ok(value) => value
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
    }
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 7:5 to 7:18

**Parse Error**
at 8:47 to 8:47

**Parse Error**
at 9:47 to 9:47

**Parse Error**
at 7:5 to 11:1

**Parse Error**
at 11:1 to 11:1

**Parse Error**
at 6:25 to 11:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

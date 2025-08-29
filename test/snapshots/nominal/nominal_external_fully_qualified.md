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
          (match
            (scrutinee               (lc "result")
))
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
module [handleResult]

import MyResultModule
handleResult : MyResultModule.MyResultType((Str, I32)) -> Str
handleResult = |result| {
	match result
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 8:47 to 8:47

**Parse Error**
at 9:47 to 9:47

**Unsupported Node**
at 3:1 to 3:22

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "handleResult")
    (Expr.binop_thin_arrow
      (Expr.apply_ident)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "handleResult")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
handleResult : _a
~~~

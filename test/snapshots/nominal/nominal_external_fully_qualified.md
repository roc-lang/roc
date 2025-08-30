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
(module-header
  (exposes
    (lc "handleResult")
))
~~~
# FORMATTED
~~~roc
module [handleResult]

import MyResultModule
handleResult : MyResultModule.MyResultType((Str, I32)) -> Str
handleResult = |result| {
	match result
		MyResultModule.MyResultType | Ok(value) => value
		MyResultModule.MyResultType | Err(code) => "Error: $(code.toStr())"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:22

**Unsupported Node**
at 8:47 to 8:49

**Unsupported Node**
at 9:9 to 9:46

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

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
NO CHANGE
~~~
# EXPECTED
MODULE NOT FOUND - nominal_external_fully_qualified.md:3:1:3:22
UNUSED VARIABLE - nominal_external_fully_qualified.md:9:41:9:45
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

**Unsupported Node**
at 3:1 to 3:22

**Unsupported Node**
at 5:16 to 5:60

**Unsupported Node**
at 6:16 to 6:25

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "handleResult")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~

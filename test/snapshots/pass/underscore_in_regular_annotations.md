# META
~~~ini
description=Underscores are allowed in regular type annotations (not in type declarations)
type=file
~~~
# SOURCE
~~~roc
module []

main : _ -> _
main = |x| x

identity : a -> a
identity = |x| x

# Function with underscore in annotation
process : List(_) -> Str
process = |list| "processed"

# Record with underscore
get_data : { field: _, other: U32 } -> U32
get_data = |record| record.other

# Pattern matching with underscore type annotation
handle_result : Result(_, Str) -> Str
handle_result = |result|
    match result {
        Ok(_) => "success",
        Err(msg) => msg,
    }

# Underscore in function arguments
map : (a -> b), List(a) -> List(b)
map = |_, _| []

# Named underscore type variables
transform : _a -> _b -> _b
transform = |_, b| b
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon Underscore OpArrow Underscore LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent LowerIdent OpColon UpperIdent OpenRound Underscore CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon OpenCurly LowerIdent OpColon Underscore Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent LowerIdent OpColon UpperIdent OpenRound Underscore Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpFatArrow String Comma UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent Comma CloseCurly LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar OpenSquare CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore Comma LowerIdent OpBar LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "main")
    (binop_thin_arrow
      (underscore)
      (underscore)
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "identity")
    (binop_thin_arrow
      (lc "a")
      (lc "a")
    )
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "process")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (underscore)
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (str_literal_big "processed")
      )
      (args
        (lc "list")
      )
    )
  )
  (binop_colon
    (lc "get_data")
    (binop_thin_arrow
      (record_literal
        (binop_colon
          (lc "field")
          (underscore)
        )
        (binop_colon
          (lc "other")
          (uc "U32")
        )
      )
      (uc "U32")
    )
  )
  (binop_equals
    (lc "get_data")
    (lambda
      (body
        (binop_pipe
          (lc "record")
          (dot_lc "other")
        )
      )
      (args
        (lc "record")
      )
    )
  )
  (binop_colon
    (lc "handle_result")
    (binop_thin_arrow
      (apply_uc
        (uc "Result")
        (tuple_literal
          (underscore)
          (uc "Str")
        )
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "handle_result")
    (lambda
      (body
        (binop_thin_arrow
          (binop_colon
            (match
              (scrutinee                 (lc "result")
))
            (binop_thin_arrow
              (lc "a")
              (lc "b")
            )
          )
          (binop_thin_arrow
            (apply_uc
              (uc "List")
              (lc "a")
            )
            (apply_uc
              (uc "List")
              (lc "b")
            )
          )
        )
      )
      (args
        (lc "result")
      )
    )
  )
  (binop_equals
    (lc "map")
    (lambda
      (body
        (list_literal)
      )
      (args
        (tuple_literal
          (underscore)
          (underscore)
        )
      )
    )
  )
  (binop_colon
    (lc "transform")
    (binop_thin_arrow
      (binop_thin_arrow
        (lc "_a")
        (lc "_b")
      )
      (lc "_b")
    )
  )
  (binop_equals
    (lc "transform")
    (lambda
      (body
        (lc "b")
      )
      (args
        (tuple_literal
          (underscore)
          (lc "b")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

main : _ -> _
main = |x| x
identity : a -> a
identity = |x| x
process : List _ -> Str
process = |list| "processed"
get_data : {field : _, other : U32} -> U32
get_data = |record| record.other
handle_result : Result(_, Str) -> Str
handle_result = |result| match result
 : a -> b -> (List(a) -> List(b))
map = |_, _| []
transform :
	(_a -> _b) -> _b
transform = |_, b| b
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 21:15 to 21:18

**Parse Error**
at 20:18 to 22:18

**Parse Error**
at 23:5 to 26:1

**Parse Error**
at 22:18 to 26:5

**Pattern in Expression Context**
at 3:8 to 3:9

**Pattern in Expression Context**
at 3:13 to 3:14

**Pattern in Expression Context**
at 10:16 to 10:17

**Pattern in Expression Context**
at 14:21 to 14:22

**Pattern in Expression Context**
at 18:24 to 18:25

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "main")
    (Expr.binop_thin_arrow
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "identity")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.lookup "a")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "identity")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "process")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "get_data")
    (Expr.binop_thin_arrow
      (Expr.record_literal
        (Expr.binop_colon
          (Expr.lookup "field")
          (Expr.malformed)
        )
        (Expr.binop_colon
          (Expr.lookup "other")
          (Expr.apply_tag)
        )
      )
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "get_data")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "handle_result")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "handle_result")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "map")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "transform")
    (Expr.binop_thin_arrow
      (Expr.binop_thin_arrow
        (Expr.lookup "_a")
        (Expr.lookup "_b")
      )
      (Expr.lookup "_b")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "transform")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
main : _c
identity : _c
process : _c
get_data : _c
handle_result : _c
map : _c
transform : _c
~~~

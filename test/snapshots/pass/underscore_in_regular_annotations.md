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
        (match <74 branches>)
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

main: (_ -> _)
main = \x -> x

identity: (a -> a)
identity = \x -> x

# Function with underscore in annotation
process: (List(_) -> Str)
process = \list -> "processed"

# Record with underscore
get_data: ({ field: _, other: U32 } -> U32)
get_data = \record -> record.other
handle_result: (Result((_, Str)) -> Str)
handle_result = \result -> when result is (
	{
		Ok(_),
		"success",
		Err(msg),
	} => msg
) -> map: (a -> b) -> (List(a) -> List(b))map = \(
	_,
	_
) -> []
transform: ((_a -> _b) -> _b)
transform = \(_, b) -> b
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 20:5 to 20:18

**Parse Error**
at 21:15 to 21:15

**Parse Error**
at 20:18 to 22:18

**Parse Error**
at 23:5 to 23:5

**Parse Error**
at 20:5 to 26:1

**Unsupported Node**
at 3:8 to 3:14

**Unsupported Node**
at 4:8 to 4:12

**Unsupported Node**
at 6:12 to 6:18

**Unsupported Node**
at 7:12 to 7:16

**Unsupported Node**
at 10:11 to 10:25

**Unsupported Node**
at 11:11 to 11:18

**Unsupported Node**
at 14:12 to 14:43

**Unsupported Node**
at 15:12 to 15:21

**Unsupported Node**
at 18:17 to 18:38

**Unsupported Node**
at 19:17 to 20:5

**Unsupported Node**
at 27:7 to 27:14

**Unsupported Node**
at 30:13 to 30:27

**Unsupported Node**
at 31:13 to 31:20

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "main")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "identity")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "get_data")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "handle_result")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "transform")
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

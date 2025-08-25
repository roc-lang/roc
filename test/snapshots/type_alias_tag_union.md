# META
~~~ini
description=Type alias with tag union and type parameters
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Type alias with type parameters that expands to a tag union
MyResult(ok, err) : [Good(ok), Bad(err)]

# Using the type alias
process : MyResult(Str, I32) -> Str
process = |_result| "processed"

# Another type alias with a single parameter
Option(a) : [Some(a), None]

# Using it with different types
getString : Option(Str) -> Str
getString = |_opt| "default"

getNumber : Option(I32) -> I32
getNumber = |_opt| 0

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (apply_uc
      (uc "MyResult")
      (tuple_literal
        (lc "ok")
        (lc "err")
      )
    )
    (list_literal
      (apply_uc
        (uc "Good")
        (lc "ok")
      )
      (apply_uc
        (uc "Bad")
        (lc "err")
      )
    )
  )
  (binop_colon
    (lc "process")
    (binop_thin_arrow
      (apply_uc
        (uc "MyResult")
        (tuple_literal
          (uc "Str")
          (uc "I32")
        )
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
        (lc "_result")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Option")
      (lc "a")
    )
    (list_literal
      (apply_uc
        (uc "Some")
        (lc "a")
      )
      (uc "None")
    )
  )
  (binop_colon
    (lc "getString")
    (binop_thin_arrow
      (apply_uc
        (uc "Option")
        (uc "Str")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "getString")
    (lambda
      (body
        (str_literal_big "default")
      )
      (args
        (lc "_opt")
      )
    )
  )
  (binop_colon
    (lc "getNumber")
    (binop_thin_arrow
      (apply_uc
        (uc "Option")
        (uc "I32")
      )
      (uc "I32")
    )
  )
  (binop_equals
    (lc "getNumber")
    (lambda
      (body
        (num_literal_i32 0)
      )
      (args
        (lc "_opt")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (record_literal)
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	pf: "../basic-cli/main.roc" platform [
		main,
	],
}

MyResult((ok, err)): [Good(ok), Bad(err)]

# Using the type alias
process: (MyResult((Str, I32)) -> Str)
process = \_result -> "processed"

# Another type alias with a single parameter
Option(a): [Some(a), None]

# Using it with different types
getString: (Option(Str) -> Str)
getString = \_opt -> "default"

getNumber: (Option(I32) -> I32)
getNumber = \_opt -> 0

main! = \_ -> {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 4:21 to 4:41

**Unsupported Node**
at 7:11 to 7:36

**Unsupported Node**
at 8:11 to 8:21

**Unsupported Node**
at 11:13 to 11:28

**Unsupported Node**
at 14:13 to 14:31

**Unsupported Node**
at 15:13 to 15:20

**Unsupported Node**
at 17:13 to 17:31

**Unsupported Node**
at 18:13 to 18:20

**Unsupported Node**
at 20:1 to 20:6

**Unsupported Node**
at 20:9 to 20:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "getString")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "getNumber")
    (Expr.malformed)
  )
  (Expr.malformed)
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

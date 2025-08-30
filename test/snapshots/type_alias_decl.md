# META
~~~ini
description=Type aliases and declarations with various forms
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Simple type alias
UserId : U64

# Generic type alias
Result(ok, err) : [Ok(ok), Err(err)]

# Record type alias
Person : { name : Str, age : U64 }

# Function type alias
MapFn(a, b) : a -> b

# Complex nested type alias
ApiResponse(data) : Result(data, Str)

# Type declaration with tag union
Color : [Red, Green, Blue, Custom(U8, U8, U8)]

# Type declaration with records and generics
Container(item) : {
    contents : List(item),
    metadata : { size : U64, created : Str }
}

main! = |_| {
    # Use the types to validate they work
    userId : UserId
    userId = 123

    person : Person
    person = { name: "Alice", age: 30 }

    color : Color
    color = Red

    userId
}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly UpperIdent OpColon UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon LowerIdent OpArrow LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent Comma UpperIdent CloseRound UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent OpenRound UpperIdent Comma UpperIdent Comma UpperIdent CloseRound CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

UserId : U64
Result((ok, err)) : [Ok(ok), Err(err)]
Person : {name : Str, age : U64}
MapFn((a, b)) : a -> b
ApiResponse(data) : Result(data, Str)
Color : [Red, Green, Blue, Custom((U8, U8, U8))]
Container(item) : {contents : List item, metadata : {size : U64, created : Str}}
main! = |_| {
	userId : UserId
	userId = 123
	person : Person
	person = { name : "Alice", age : 30 }
	color : Color
	color = Red
	userId : userId
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "name")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "age")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.lookup "b")
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "contents")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "metadata")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "size")
            (Expr.apply_tag)
          )
          (Expr.binop_colon
            (Expr.lookup "created")
            (Expr.apply_tag)
          )
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.not_lookup)
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
~~~

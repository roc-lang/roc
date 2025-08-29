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
(block
  (binop_colon
    (uc "UserId")
    (uc "U64")
  )
  (binop_colon
    (apply_uc
      (uc "Result")
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
    (uc "Person")
    (record_literal
      (binop_colon
        (lc "name")
        (uc "Str")
      )
      (binop_colon
        (lc "age")
        (uc "U64")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "MapFn")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (binop_thin_arrow
      (lc "a")
      (lc "b")
    )
  )
  (binop_colon
    (apply_uc
      (uc "ApiResponse")
      (lc "data")
    )
    (apply_uc
      (uc "Result")
      (tuple_literal
        (lc "data")
        (uc "Str")
      )
    )
  )
  (binop_colon
    (uc "Color")
    (list_literal
      (uc "Red")
      (uc "Green")
      (uc "Blue")
      (apply_uc
        (uc "Custom")
        (tuple_literal
          (uc "U8")
          (uc "U8")
          (uc "U8")
        )
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Container")
      (lc "item")
    )
    (record_literal
      (binop_colon
        (lc "contents")
        (apply_uc
          (uc "List")
          (lc "item")
        )
      )
      (binop_colon
        (lc "metadata")
        (record_literal
          (binop_colon
            (lc "size")
            (uc "U64")
          )
          (binop_colon
            (lc "created")
            (uc "Str")
          )
        )
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_colon
            (lc "userId")
            (uc "UserId")
          )
          (binop_equals
            (lc "userId")
            (num_literal_i32 123)
          )
          (binop_colon
            (lc "person")
            (uc "Person")
          )
          (binop_equals
            (lc "person")
            (record_literal
              (binop_colon
                (lc "name")
                (str_literal_big "Alice")
              )
              (binop_colon
                (lc "age")
                (num_literal_i32 30)
              )
            )
          )
          (binop_colon
            (lc "color")
            (uc "Color")
          )
          (binop_equals
            (lc "color")
            (uc "Red")
          )
          (binop_colon
            (lc "userId")
            (lc "userId")
          )
        )
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
app { pf: "../basic-cli/main.roc" platform [main] }

UserId : U64
Result((ok, err)) : [Ok(ok), Err(err)]
Person : {name : Str, age : U64}
MapFn((a, b)) : a -> b
ApiResponse(data) : Result(data, Str)
Color : [Red, Green, Blue, Custom((U8, U8, U8))]
Container(item) : {contents : List item, metadata : {size : U64, created : Str}}
main! = |_| {
		# Use the types to validate they work
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
    (Expr.binop_thin_arrow)
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

# META
~~~ini
description=Type aliases and declarations with various forms
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly UpperIdent OpColon UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon LowerIdent OpArrow LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent Comma UpperIdent CloseRound UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent OpenRound UpperIdent Comma UpperIdent Comma UpperIdent CloseRound CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpColon UpperIdent LowerIdent OpAssign Int LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent LowerIdent CloseCurly ~~~
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
      (tuple_literal
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
  )
  (binop_colon
    (uc "Person")
    (block
      (binop_colon
        (lc "name")
        (binop_colon
          (tuple_literal
            (uc "Str")
            (lc "age")
          )
          (uc "U64")
        )
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
      (tuple_literal
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
  )
  (binop_colon
    (apply_uc
      (uc "Container")
      (lc "item")
    )
    (block
      (binop_colon
        (lc "contents")
        (binop_colon
          (tuple_literal
            (apply_uc
              (uc "List")
              (lc "item")
            )
            (lc "metadata")
          )
          (block
            (binop_colon
              (lc "size")
              (binop_colon
                (tuple_literal
                  (uc "U64")
                  (lc "created")
                )
                (uc "Str")
              )
            )
          )
        )
      )
    )
  )
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
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
        (block
          (binop_colon
            (lc "name")
            (binop_colon
              (tuple_literal
                (str_literal_big "Alice")
                (lc "age")
              )
              (num_literal_i32 30)
            )
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
      (lc "userId")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
TYPE REDECLARED - type_alias_decl.md:7:1:7:37
UNUSED VARIABLE - type_alias_decl.md:33:5:33:11
UNUSED VARIABLE - type_alias_decl.md:36:5:36:10
# PROBLEMS
**Parse Error**
at 27:7 to 27:7

**Pattern in Expression Context**
at 4:1 to 4:7

**Pattern in Expression Context**
at 4:10 to 4:13

**Unsupported Node**
at 7:19 to 8:1

**Pattern in Expression Context**
at 10:1 to 10:7

**Unsupported Node**
at 10:28 to 10:28

**Pattern in Expression Context**
at 10:30 to 10:33

**Unsupported Node**
at 13:15 to 13:21

**Pattern in Expression Context**
at 19:1 to 19:6

**Unsupported Node**
at 19:9 to 21:1

**Unsupported Node**
at 24:14 to 24:14

**Unsupported Node**
at 24:38 to 24:38

**Pattern in Expression Context**
at 24:40 to 24:43

**Unsupported Node**
at 27:5 to 27:7

**Pattern in Expression Context**
at 29:14 to 29:20

**Pattern in Expression Context**
at 32:14 to 32:20

**Unsupported Node**
at 33:34 to 33:35

**Pattern in Expression Context**
at 35:13 to 35:18

**Pattern in Expression Context**
at 36:13 to 36:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "name")
        (Expr.binop_colon
          (Expr.malformed)
          (Expr.malformed)
        )
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "contents")
        (Expr.binop_colon
          (Expr.malformed)
          (Expr.record_literal
            (Expr.binop_colon
              (Expr.lookup "size")
              (Expr.binop_colon
                (Expr.malformed)
                (Expr.malformed)
              )
            )
          )
        )
      )
    )
  )
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~

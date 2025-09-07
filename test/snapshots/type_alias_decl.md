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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment UpperIdent OpColon UpperIdent BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LineComment UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon LowerIdent OpArrow LowerIdent BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent Comma UpperIdent CloseRound BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent OpenRound UpperIdent Comma UpperIdent Comma UpperIdent CloseRound CloseSquare BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent BlankLine LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
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
    (binop_arrow_call
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
          (lc "userId")
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
app [main!] { pf: "../basic-cli/main.roc" platform [] }

# Simple type alias
UserId : U64
# Generic type alias
Result((ok, err)) : [Ok(ok), Err(err)]
# Record type alias
Person : {name: Str, age: U64}
# Function type alias
MapFn((a, b)) : a -> b
# Complex nested type alias
ApiResponse(data) : Result(data, Str)
# Type declaration with tag union
Color : [Red, Green, Blue, Custom((U8, U8, U8))]
# Type declaration with records and generics
Container(item) : {contents: List(item), metadata: { size: U64, created: Str }}
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
# EXPECTED
TYPE REDECLARED - type_alias_decl.md:7:1:7:37
UNUSED VARIABLE - type_alias_decl.md:33:5:33:11
UNUSED VARIABLE - type_alias_decl.md:36:5:36:10
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_alias_decl.md:30:5:30:11:**
```roc
    userId = 123
```
    ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_alias_decl.md:33:5:33:11:**
```roc
    person = { name: "Alice", age: 30 }
```
    ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_alias_decl.md:36:5:36:10:**
```roc
    color = Red
```
    ^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_alias_decl.md:27:1:27:6:**
```roc
main! = |_| {
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 116
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 -> #115)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 -> #87)
(var #87 Num *)
(var #88 _)
(var #89 _)
(var #90 _)
(var #91 _)
(var #92 -> #114)
(var #93 _)
(var #94 Str)
(var #95 _)
(var #96 _)
(var #97 Num *)
(var #98 _)
(var #99 -> #114)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 -> #105)
(var #105 _)
(var #106 _)
(var #107 _)
(var #108 _)
(var #109 -> #115)
(var #110 _)
(var #111 _)
(var #112 _)
(var #113 {})
(var #114 record)
(var #115 fn_pure)
~~~
# TYPES
~~~roc
color : _c
main : _arg -> _ret
person : { name: Str, age: Num(_size2) }
userId : Num(_size2)
~~~

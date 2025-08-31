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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment UpperIdent OpColon UpperIdent BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LineComment UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon LowerIdent OpArrow LowerIdent BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent Comma UpperIdent CloseRound BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent OpenRound UpperIdent Comma UpperIdent Comma UpperIdent CloseRound CloseSquare BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent BlankLine LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Simple type alias
UserId : U64

# Generic type alias
Result((ok, err)) : [Ok(ok), Err(err)]

# Record type alias
Person : {name : Str, age : U64}

# Function type alias
MapFn((a, b)) : a -> b

# Complex nested type alias
ApiResponse(data) : Result(data, Str)

# Type declaration with tag union
Color : [Red, Green, Blue, Custom((U8, U8, U8))]

# Type declaration with records and generics
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
**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_alias_decl.md:33:16:33:20:**
```roc
    person = { name: "Alice", age: 30 }
```
               ^^^^


**UNDEFINED VARIABLE**
Nothing is named **age** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_alias_decl.md:33:31:33:34:**
```roc
    person = { name: "Alice", age: 30 }
```
                              ^^^


**UNUSED VARIABLE**
Variable **color** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_color` to suppress this warning.
The unused variable is declared here:

**type_alias_decl.md:36:5:36:10:**
```roc
    color = Red
```
    ^^^^^


**UNUSED VARIABLE**
Variable **person** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_person` to suppress this warning.
The unused variable is declared here:

**type_alias_decl.md:33:5:33:11:**
```roc
    person = { name: "Alice", age: 30 }
```
    ^^^^^^


**UNUSED VARIABLE**
Variable **userId** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_userId` to suppress this warning.
The unused variable is declared here:

**type_alias_decl.md:38:5:38:11:**
```roc
    userId
```
    ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name node:uc)
    (type uc)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type list_literal)
  )
  (Stmt.type_anno
    (name node:uc)
    (type record_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type apply_uc)
  )
  (Stmt.type_anno
    (name node:uc)
    (type list_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type record_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
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

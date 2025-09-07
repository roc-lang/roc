# META
~~~ini
description=Type variables nested within complex type constructors
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform.roc" }

# Map over Result type
map_result : Result(a, e), (a -> b) -> Result(b, e)
map_result = |result, transform| {
    match result {
        Ok(value) => Ok(transform(value))
        Err(error) => Err(error)
    }
}

# Simple identity function with type variable
identity : a -> a
identity = |x| x

# Nested type variables in records
make_pair : a, b -> { first: a, second: b }
make_pair = |x, y| { first: x, second: y }

# Function that works with lists of any type
list_length : List(_a) -> U64
list_length = |_lst| 42

# Nested Result types
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))

main = |_| "done"
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound UpperIdent OpenRound LowerIdent Comma UpperIdent CloseRound Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LowerIdent OpAssign OpBar Underscore OpBar String ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "platform.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (lc "map_result")
    (binop_arrow_call
      (apply_uc
        (uc "Result")
        (tuple_literal
          (lc "a")
          (lc "e")
        )
      )
      (binop_arrow_call
        (binop_arrow_call
          (lc "a")
          (lc "b")
        )
        (apply_uc
          (uc "Result")
          (tuple_literal
            (lc "b")
            (lc "e")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "map_result")
    (lambda
      (body
        (block
          (match
            (scrutinee               (lc "result")
)
            (branch1               (binop_thick_arrow
                (apply_uc
                  (uc "Ok")
                  (lc "value")
                )
                (malformed)
              )
))
          (malformed)
          (malformed)
          (apply_uc
            (uc "Err")
            (lc "error")
          )
          (malformed)
          (apply_uc
            (uc "Err")
            (lc "error")
          )
        )
      )
      (args
        (lc "result")
        (lc "transform")
      )
    )
  )
  (malformed)
  (binop_colon
    (lc "identity")
    (binop_arrow_call
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
    (lc "make_pair")
    (binop_arrow_call
      (lc "a")
      (binop_arrow_call
        (lc "b")
        (record_literal
          (binop_colon
            (lc "first")
            (lc "a")
          )
          (binop_colon
            (lc "second")
            (lc "b")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "make_pair")
    (lambda
      (body
        (record_literal
          (binop_colon
            (lc "first")
            (lc "x")
          )
          (binop_colon
            (lc "second")
            (lc "y")
          )
        )
      )
      (args
        (lc "x")
        (lc "y")
      )
    )
  )
  (binop_colon
    (lc "list_length")
    (binop_arrow_call
      (apply_uc
        (uc "List")
        (lc "_a")
      )
      (uc "U64")
    )
  )
  (binop_equals
    (lc "list_length")
    (lambda
      (body
        (num_literal_i32 42)
      )
      (args
        (lc "_lst")
      )
    )
  )
  (binop_colon
    (lc "wrap_in_result")
    (binop_arrow_call
      (lc "a")
      (apply_uc
        (uc "Result")
        (tuple_literal
          (apply_uc
            (uc "Result")
            (tuple_literal
              (lc "a")
              (uc "Str")
            )
          )
          (uc "Str")
        )
      )
    )
  )
  (binop_equals
    (lc "wrap_in_result")
    (lambda
      (body
        (apply_uc
          (uc "Ok")
          (apply_uc
            (uc "Ok")
            (lc "value")
          )
        )
      )
      (args
        (lc "value")
      )
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (str_literal_small "done")
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
app [main] { pf: "platform.roc" platform [] }

# Map over Result type
map_result : Result(a, e) -> (a -> b) -> Result(b, e)
map_result = |result, transform| {
	match result
		Ok(value) => Ok
	)
	)
	Err(error)
	=> 
	Err(error)
}

}

# Simple identity function with type variable
identity : a -> a
identity = |x| x
# Nested type variables in records
make_pair : a -> b -> {first: a, second: b}
make_pair = |x, y| { first: x, second: y }
# Function that works with lists of any type
list_length : List _a -> U64
list_length = |_lst| 42
# Nested Result types
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))
main = |_| "done"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **Ok** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:7:22:7:24:**
```roc
        Ok(value) => Ok(transform(value))
```
                     ^^


**PARSE ERROR**
A parsing error occurred: **expected_close_round**
This is an unexpected parsing error. Please check your syntax.

**type_var_nested.md:7:34:7:35:**
```roc
        Ok(value) => Ok(transform(value))
```
                                 ^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**type_var_nested.md:7:35:7:40:**
```roc
        Ok(value) => Ok(transform(value))
```
                                  ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:7:40:7:41:**
```roc
        Ok(value) => Ok(transform(value))
```
                                       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
        ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:7:41:8:9:**
```roc
        Ok(value) => Ok(transform(value))
        Err(error) => Err(error)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:8:20:8:23:**
```roc
        Err(error) => Err(error)
```
                   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

# Simple identity function with type variable
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_nested.md:10:1:13:1:**
```roc
}

# Simple identity function with type variable
identity : a -> a
```


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:4:1:4:11:**
```roc
map_result : Result(a, e), (a -> b) -> Result(b, e)
```
^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_var_nested.md:8:13:8:18:**
```roc
        Err(error) => Err(error)
```
            ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_var_nested.md:8:27:8:32:**
```roc
        Err(error) => Err(error)
```
                          ^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:5:1:5:11:**
```roc
map_result = |result, transform| {
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:13:1:13:9:**
```roc
identity : a -> a
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:14:1:14:9:**
```roc
identity = |x| x
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:17:1:17:10:**
```roc
make_pair : a, b -> { first: a, second: b }
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:18:14:18:15:**
```roc
make_pair = |x, y| { first: x, second: y }
```
             ^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:18:1:18:10:**
```roc
make_pair = |x, y| { first: x, second: y }
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:21:1:21:12:**
```roc
list_length : List(_a) -> U64
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:22:1:22:12:**
```roc
list_length = |_lst| 42
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:25:1:25:15:**
```roc
wrap_in_result : a -> Result(Result(a, Str), Str)
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:26:19:26:24:**
```roc
wrap_in_result = |value| Ok(Ok(value))
```
                  ^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_nested.md:26:1:26:15:**
```roc
wrap_in_result = |value| Ok(Ok(value))
```
^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "map_result"))
    (type type_22)
  )
  (Stmt.assign
    (pattern (Patt.ident "map_result"))
    (Expr.lambda (canonicalized))
  )
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "identity"))
    (type type_53)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "make_pair"))
    (type type_71)
  )
  (Stmt.assign
    (pattern (Patt.ident "make_pair"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "list_length"))
    (type type_90)
  )
  (Stmt.assign
    (pattern (Patt.ident "list_length"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "wrap_in_result"))
    (type type_108)
  )
  (Stmt.assign
    (pattern (Patt.ident "wrap_in_result"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 151
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
(var #24 -> #133)
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
(var #39 -> #129)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 -> #131)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 -> #133)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 -> #136)
(var #56 _)
(var #57 _)
(var #58 -> #136)
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
(var #73 -> #142)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 -> #140)
(var #83 -> #142)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 _)
(var #90 _)
(var #91 _)
(var #92 -> #144)
(var #93 _)
(var #94 Num *)
(var #95 -> #144)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 _)
(var #106 _)
(var #107 _)
(var #108 _)
(var #109 _)
(var #110 -> #148)
(var #111 _)
(var #112 -> #147)
(var #113 -> #146)
(var #114 _)
(var #115 _)
(var #116 _)
(var #117 -> #148)
(var #118 _)
(var #119 -> #150)
(var #120 _)
(var #121 Str)
(var #122 -> #150)
(var #123 _)
(var #124 _)
(var #125 _)
(var #126 _)
(var #127 _)
(var #128 _)
(var #129 fn_pure)
(var #130 _)
(var #131 fn_pure)
(var #132 fn_pure)
(var #133 fn_pure)
(var #134 _)
(var #135 _)
(var #136 fn_pure)
(var #137 _)
(var #138 _)
(var #139 {})
(var #140 record)
(var #141 fn_pure)
(var #142 fn_pure)
(var #143 _)
(var #144 fn_pure)
(var #145 _)
(var #146 fn_pure)
(var #147 fn_pure)
(var #148 fn_pure)
(var #149 _)
(var #150 fn_pure)
~~~
# TYPES
~~~roc
wrap_in_result : _arg -> _ret
list_length : _arg -> Num(_size)
make_pair : _arg -> _arg2 -> { first: _field, second: _field2 }
map_result : _arg -> _arg2 -> _ret
x : _c
_lst : _c
result : _c
y : _c
value : _c
identity : _arg -> _ret
transform : _c
~~~

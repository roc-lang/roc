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
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon Underscore OpArrow Underscore LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound Underscore CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon OpenCurly LowerIdent OpColon Underscore Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound Underscore Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpFatArrow String Comma UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent Comma CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar OpenSquare CloseSquare BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore Comma LowerIdent OpBar LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon
    (lc "main")
    (binop_arrow_call
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
    (lc "process")
    (binop_arrow_call
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
    (binop_arrow_call
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
        (binop_dot
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
    (binop_arrow_call
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
        (match
          (scrutinee             (lc "result")
)
          (branch1             (binop_thick_arrow
              (apply_uc
                (uc "Ok")
                (underscore)
              )
              (binop_thick_arrow
                (str_literal_big "success")
                (binop_thick_arrow
                  (apply_uc
                    (uc "Err")
                    (lc "msg")
                  )
                  (lc "msg")
                )
              )
            )
))
      )
      (args
        (lc "result")
      )
    )
  )
  (binop_colon
    (lc "map")
    (binop_arrow_call
      (binop_arrow_call
        (lc "a")
        (lc "b")
      )
      (binop_arrow_call
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
  (binop_equals
    (lc "map")
    (lambda
      (body
        (list_literal)
      )
      (args
        (underscore)
        (underscore)
      )
    )
  )
  (binop_colon
    (lc "transform")
    (binop_arrow_call
      (binop_arrow_call
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
        (underscore)
        (lc "b")
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
# Function with underscore in annotation
process : List _ -> Str
process = |list| "processed"
# Record with underscore
get_data : {field: _, other: U32} -> U32
get_data = |record| record..other
# Pattern matching with underscore type annotation
handle_result : Result(_, Str) -> Str
handle_result = |result| match result
	Ok(_) => "success" => (Err(msg) => msg)

# Underscore in function arguments
map :
	(a -> b) -> List a -> List b
map = |_, _| []
# Named underscore type variables
transform :
	(_a -> _b) -> _b
transform = |_, b| b
~~~
# EXPECTED
PARSE ERROR - underscore_in_regular_annotations.md:30:22:30:24
PARSE ERROR - underscore_in_regular_annotations.md:30:25:30:27
UNUSED VARIABLE - underscore_in_regular_annotations.md:11:12:11:16
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:3:1:3:5:**
```roc
main : _ -> _
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:4:1:4:5:**
```roc
main = |x| x
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:6:1:6:9:**
```roc
identity : a -> a
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:7:13:7:14:**
```roc
identity = |x| x
```
            ^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:7:1:7:9:**
```roc
identity = |x| x
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:10:1:10:8:**
```roc
process : List(_) -> Str
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:11:1:11:8:**
```roc
process = |list| "processed"
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:14:1:14:9:**
```roc
get_data : { field: _, other: U32 } -> U32
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:15:1:15:9:**
```roc
get_data = |record| record.other
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:18:1:18:14:**
```roc
handle_result : Result(_, Str) -> Str
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:19:1:19:14:**
```roc
handle_result = |result|
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:26:1:26:4:**
```roc
map : (a -> b), List(a) -> List(b)
```
^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:27:1:27:4:**
```roc
map = |_, _| []
```
^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:30:1:30:10:**
```roc
transform : _a -> _b -> _b
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_in_regular_annotations.md:31:1:31:10:**
```roc
transform = |_, b| b
```
^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "main"))
    (type type_4)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "identity"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "process"))
    (type type_26)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "get_data"))
    (type type_42)
  )
  (Stmt.assign
    (pattern (Patt.ident "get_data"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "handle_result"))
    (type type_58)
  )
  (Stmt.assign
    (pattern (Patt.ident "handle_result"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "map"))
    (type type_88)
  )
  (Stmt.assign
    (pattern (Patt.ident "map"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "transform"))
    (type type_101)
  )
  (Stmt.assign
    (pattern (Patt.ident "transform"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 126
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 -> #111)
(var #7 _)
(var #8 _)
(var #9 -> #111)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #113)
(var #17 _)
(var #18 _)
(var #19 -> #113)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 -> #115)
(var #29 _)
(var #30 Str)
(var #31 -> #115)
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
(var #44 -> #117)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 -> #117)
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
(var #60 -> #119)
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
(var #75 -> #119)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 _)
(var #90 -> #122)
(var #91 _)
(var #92 _)
(var #93 _)
(var #94 -> #122)
(var #95 _)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 -> #125)
(var #104 _)
(var #105 _)
(var #106 _)
(var #107 -> #125)
(var #108 _)
(var #109 _)
(var #110 _)
(var #111 fn_pure)
(var #112 _)
(var #113 fn_pure)
(var #114 _)
(var #115 fn_pure)
(var #116 _)
(var #117 fn_pure)
(var #118 _)
(var #119 fn_pure)
(var #120 _)
(var #121 _)
(var #122 fn_pure)
(var #123 _)
(var #124 _)
(var #125 fn_pure)
~~~
# TYPES
~~~roc
get_data : _arg -> _ret
process : _arg -> Str
handle_result : _arg -> _ret
record : _c
msg : _c
identity : _arg -> _ret
list : _c
b : _c
x : _c
main : _arg -> _ret
result : _c
transform : _arg, _arg2 -> _ret
map : _arg, _arg2 -> _ret
~~~

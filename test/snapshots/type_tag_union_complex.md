# META
~~~ini
description=Complex tag union types with multiple variants and nesting
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Simple tag union with no-argument tags
Status : [Loading, Complete, Failed]

# Tag union with mixed argument types
Result : [Success(Str), Error(Str), Warning(Str, I32)]

# Nested tag unions
Response : [Ok(Result), NetworkError, ParseError]

# Multiple tag unions using similar tag names
UserState : [Active(Str), Inactive, Suspended(Str)]
ConnectionState : [Active, Disconnected, Connecting(Str)]

# Function using tag unions
processResult : Result -> Str
processResult = |_result| "processed"

# Function with nested tag union
handleResponse : Response -> Str
handleResponse = |_response| "handled"

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound CloseSquare BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent Comma UpperIdent CloseSquare BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound CloseSquare UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (uc "Status")
    (list_literal
      (uc "Loading")
      (uc "Complete")
      (uc "Failed")
    )
  )
  (binop_colon
    (uc "Result")
    (list_literal
      (apply_uc
        (uc "Success")
        (uc "Str")
      )
      (apply_uc
        (uc "Error")
        (uc "Str")
      )
      (apply_uc
        (uc "Warning")
        (tuple_literal
          (uc "Str")
          (uc "I32")
        )
      )
    )
  )
  (binop_colon
    (uc "Response")
    (list_literal
      (apply_uc
        (uc "Ok")
        (uc "Result")
      )
      (uc "NetworkError")
      (uc "ParseError")
    )
  )
  (binop_colon
    (uc "UserState")
    (list_literal
      (apply_uc
        (uc "Active")
        (uc "Str")
      )
      (uc "Inactive")
      (apply_uc
        (uc "Suspended")
        (uc "Str")
      )
    )
  )
  (binop_colon
    (uc "ConnectionState")
    (list_literal
      (uc "Active")
      (uc "Disconnected")
      (apply_uc
        (uc "Connecting")
        (uc "Str")
      )
    )
  )
  (binop_colon
    (lc "processResult")
    (binop_arrow_call
      (uc "Result")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "processResult")
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
    (lc "handleResponse")
    (binop_arrow_call
      (uc "Response")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "handleResponse")
    (lambda
      (body
        (str_literal_big "handled")
      )
      (args
        (lc "_response")
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
app [main!] { pf: "../basic-cli/main.roc" platform [] }

# Simple tag union with no-argument tags
Status : [Loading, Complete, Failed]
# Tag union with mixed argument types
Result : [Success(Str), Error(Str), Warning((Str, I32))]
# Nested tag unions
Response : [Ok(Result), NetworkError, ParseError]
# Multiple tag unions using similar tag names
UserState : [Active(Str), Inactive, Suspended(Str)]
ConnectionState : [Active, Disconnected, Connecting(Str)]
# Function using tag unions
processResult : Result -> Str
processResult = |_result| "processed"
# Function with nested tag union
handleResponse : Response -> Str
handleResponse = |_response| "handled"
main! = |_| {}
~~~
# EXPECTED
TYPE REDECLARED - type_tag_union_complex.md:7:1:7:55
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_tag_union_complex.md:17:1:17:14:**
```roc
processResult : Result -> Str
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_complex.md:18:1:18:14:**
```roc
processResult = |_result| "processed"
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_complex.md:21:1:21:15:**
```roc
handleResponse : Response -> Str
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_complex.md:22:1:22:15:**
```roc
handleResponse = |_response| "handled"
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_complex.md:24:1:24:6:**
```roc
main! = |_| {}
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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processResult"))
    (type type_56)
  )
  (Stmt.assign
    (pattern (Patt.ident "processResult"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "handleResponse"))
    (type type_66)
  )
  (Stmt.assign
    (pattern (Patt.ident "handleResponse"))
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
; Total type variables: 86
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
(var #58 -> #80)
(var #59 _)
(var #60 Str)
(var #61 -> #80)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 -> #82)
(var #69 _)
(var #70 Str)
(var #71 -> #82)
(var #72 _)
(var #73 -> #85)
(var #74 _)
(var #75 -> #84)
(var #76 -> #85)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 fn_pure)
(var #81 _)
(var #82 fn_pure)
(var #83 _)
(var #84 {})
(var #85 fn_pure)
~~~
# TYPES
~~~roc
_result : _a
_response : _a
handleResponse : _arg -> Str
main : _arg -> {}
processResult : _arg -> Str
~~~

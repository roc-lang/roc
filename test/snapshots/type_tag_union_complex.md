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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare UpperIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound CloseSquare UpperIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent Comma UpperIdent CloseSquare UpperIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound CloseSquare UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "Status")
    (list_literal
      (tuple_literal
        (uc "Loading")
        (uc "Complete")
        (uc "Failed")
      )
    )
  )
  (binop_colon
    (uc "Result")
    (list_literal
      (tuple_literal
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
  )
  (binop_colon
    (uc "Response")
    (list_literal
      (tuple_literal
        (apply_uc
          (uc "Ok")
          (uc "Result")
        )
        (uc "NetworkError")
        (uc "ParseError")
      )
    )
  )
  (binop_colon
    (uc "UserState")
    (list_literal
      (tuple_literal
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
  )
  (binop_colon
    (uc "ConnectionState")
    (list_literal
      (tuple_literal
        (uc "Active")
        (uc "Disconnected")
        (apply_uc
          (uc "Connecting")
          (uc "Str")
        )
      )
    )
  )
  (binop_colon
    (lc "processResult")
    (binop_thin_arrow
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
    (binop_thin_arrow
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
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (record_literal)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
TYPE REDECLARED - type_tag_union_complex.md:7:1:7:55
# PROBLEMS
**Parse Error**
at 24:7 to 24:7

**Unsupported Node**
at 4:10 to 5:1

**Unsupported Node**
at 7:10 to 9:1

**Unsupported Node**
at 10:12 to 11:1

**Unsupported Node**
at 13:13 to 14:1

**Unsupported Node**
at 14:19 to 15:1

**Unsupported Node**
at 17:17 to 17:30

**Unsupported Node**
at 18:17 to 18:27

**Unsupported Node**
at 21:18 to 21:33

**Unsupported Node**
at 22:18 to 22:30

**Unsupported Node**
at 24:5 to 24:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "processResult")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "handleResponse")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg, _arg2 -> {}")
~~~
# TYPES
~~~roc
~~~

# META
~~~ini
description=Complex tag union types with multiple variants and nesting
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound CloseSquare BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent Comma UpperIdent CloseSquare BlankLine LineComment UpperIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound CloseSquare UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **_result** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__result` to suppress this warning.
The unused variable is declared here:

**type_tag_union_complex.md:18:18:18:25:**
```roc
processResult = |_result| "processed"
```
                 ^^^^^^^


**UNUSED VARIABLE**
Variable **_response** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__response` to suppress this warning.
The unused variable is declared here:

**type_tag_union_complex.md:22:19:22:28:**
```roc
handleResponse = |_response| "handled"
```
                  ^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

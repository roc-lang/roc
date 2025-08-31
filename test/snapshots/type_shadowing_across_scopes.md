# META
~~~ini
description=Type shadowing across scopes should produce warning
type=file
~~~
# SOURCE
~~~roc
module [Result, processData]

Result(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
    "processed"

# In a nested module scope, redeclare Result
InnerModule : {
    Result : [Success, Failure]
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment UpperIdent OpColon OpenCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent CloseSquare CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Result")

    (lc "processData")
))
~~~
# FORMATTED
~~~roc
module [Result, processData]


Result((a, b)) : [Ok(a), Err(b)]
processData : Str -> Str
processData = |data| "processed"
# In a nested module scope, redeclare Result
InnerModule : {
	Result : [Success, Failure]
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **data** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_data` to suppress this warning.
The unused variable is declared here:

**type_shadowing_across_scopes.md:6:16:6:20:**
```roc
processData = |data|
```
               ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~

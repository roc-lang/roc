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
PARSE ERROR - type_shadowing_across_scopes.md:11:5:11:11
PARSE ERROR - type_shadowing_across_scopes.md:11:24:11:31
PARSE ERROR - type_shadowing_across_scopes.md:11:31:11:32
PARSE ERROR - type_shadowing_across_scopes.md:12:1:12:2
TYPE REDECLARED - type_shadowing_across_scopes.md:3:1:3:31
MALFORMED TYPE - type_shadowing_across_scopes.md:11:24:11:31
UNUSED VARIABLE - type_shadowing_across_scopes.md:6:16:6:20
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


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**type_shadowing_across_scopes.md:10:15:12:2:**
```roc
InnerModule : {
    Result : [Success, Failure]
}
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name node:apply_uc)
    (type list_literal)
  )
  (Stmt.type_anno
    (name "processData")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processData"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name node:uc)
    (type block)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~

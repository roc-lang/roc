# META
~~~ini
description=Nested instantiation with record field access causing type mismatch
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }
# TODO: if you do this whole thing as an expr block, with `composed` at
# the end instead of `answer =`, it triggers a parser bug!

make_record : a -> { value: a, tag: Str }
make_record = |x| { value: x, tag: "data" }

get_value : { value: a, tag: Str } -> a
get_value = |r| r.value

composed : List(a) -> Str
composed = |n| get_value(make_record(n))

answer = composed([42])
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly LineComment LineComment BlankLine LowerIdent OpColon LowerIdent OpArrow OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent CloseCurly LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon String CloseCurly BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int CloseSquare CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] } # TODO: if you do this whole thing as an expr block, with `composed` at
# the end instead of `answer =`, it triggers a parser bug!

make_record : a -> {value : a, tag : Str}
make_record = |x| { value : x, tag : "data" }

get_value : {value : a, tag : Str} -> a
get_value = |r| r.value

composed : List a -> Str
composed = |n| get_value(make_record(n))

answer = composed([42])
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**test_nested_instantiation_crash.md:6:21:6:26:**
```roc
make_record = |x| { value: x, tag: "data" }
```
                    ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**test_nested_instantiation_crash.md:6:31:6:34:**
```roc
make_record = |x| { value: x, tag: "data" }
```
                              ^^^


**UNUSED VARIABLE**
Variable **r** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_r` to suppress this warning.
The unused variable is declared here:

**test_nested_instantiation_crash.md:9:17:9:18:**
```roc
get_value = |r| r.value
```
                ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "make_record")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "make_record"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "get_value")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "get_value"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "composed")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "composed"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "answer"))
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~

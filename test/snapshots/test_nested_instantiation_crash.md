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
app { pf: "../basic-cli/platform.roc" platform [main] }

# TODO: if you do this whole thing as an expr block, with `composed` at
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
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**test_nested_instantiation_crash.md:6:21:6:29:**
```roc
make_record = |x| { value: x, tag: "data" }
```
                    ^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**test_nested_instantiation_crash.md:6:31:6:42:**
```roc
make_record = |x| { value: x, tag: "data" }
```
                              ^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**test_nested_instantiation_crash.md:6:16:6:17:**
```roc
make_record = |x| { value: x, tag: "data" }
```
               ^


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
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~

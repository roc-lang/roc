# META
~~~ini
description=Pure function with pure annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Function with pure annotation using thin arrow
add : I32, I32 -> I32
add = |x, y| { x: x, y: y }.x

# Another pure function that calls a pure function
double : I32 -> I32
double = |x| add(x, x)

main! = add(1, 2)
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly Dot LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign LowerIdent OpenRound Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

# Function with pure annotation using thin arrow
add : I32 -> I32 -> I32
add = |x, y| { x : x, y : y } | .x

# Another pure function that calls a pure function
double : I32 -> I32
double = |x| add((x, x))

main! = add((1, 2))
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pure_with_pure_annotation.md:5:14:5:30:**
```roc
add = |x, y| { x: x, y: y }.x
```
             ^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**pure_with_pure_annotation.md:5:8:5:9:**
```roc
add = |x, y| { x: x, y: y }.x
```
       ^


**UNUSED VARIABLE**
Variable **y** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:

**pure_with_pure_annotation.md:5:11:5:12:**
```roc
add = |x, y| { x: x, y: y }.x
```
          ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "add")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "add"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "double")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "double"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

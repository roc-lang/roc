# META
~~~ini
description=Dot access with proper variable definitions
type=expr
~~~
# SOURCE
~~~roc
{
    list = [1, 2, 3]
    fn = |x| x + 1
    list.map(fn)
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int LowerIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "list")
    (list_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
      (num_literal_i32 3)
    )
  )
  (binop_equals
    (lc "fn")
    (lambda
      (body
        (binop_plus
          (lc "x")
          (num_literal_i32 1)
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (apply_anon
    (binop_pipe
      (lc "list")
      (dot_lc "map")
    )
    (lc "fn")
  )
)
~~~
# FORMATTED
~~~roc
list = [1, 2, 3]
fn = |x| x + 1
list.map(fn)
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_dot_access_with_vars.md:4:5:4:13:**
```roc
    list.map(fn)
```
    ^^^^^^^^


**UNUSED VARIABLE**
Variable **list** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:

**can_dot_access_with_vars.md:2:5:2:9:**
```roc
    list = [1, 2, 3]
```
    ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "fn"))
    (Expr.lambda (canonicalized))
  )
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
list : List(_elem)
fn : _a
~~~

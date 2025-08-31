# META
~~~ini
description=Tuple containing variations on boolean values
type=expr
~~~
# SOURCE
~~~roc
{
	hello = "Hello"
	world = "World"
	"${hello} ${world}"
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign String LowerIdent OpAssign String String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "hello")
    (str_literal_big "Hello")
  )
  (binop_equals
    (lc "world")
    (str_literal_big "World")
  )
  (str_literal_big "${hello} ${world}")
)
~~~
# FORMATTED
~~~roc
hello = "Hello"
world = "World"
"${hello} ${world}"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **hello** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_hello` to suppress this warning.
The unused variable is declared here:

**string_interpolated.md:2:2:2:7:**
```roc
	hello = "Hello"
```
	^^^^^


**UNUSED VARIABLE**
Variable **world** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_world` to suppress this warning.
The unused variable is declared here:

**string_interpolated.md:3:2:3:7:**
```roc
	world = "World"
```
	^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "hello"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "world"))
    (Expr.str_literal_big)
  )
  (Expr.str_literal_big)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
hello : Str
world : Str
~~~

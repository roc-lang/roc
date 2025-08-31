# META
~~~ini
description=Record containing a string field with field access
type=expr
~~~
# SOURCE
~~~roc
{foo: "Hello"}.foo
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String CloseCurly Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_pipe
  (block
    (binop_colon
      (lc "foo")
      (str_literal_big "Hello")
    )
  )
  (dot_lc "foo")
)
~~~
# FORMATTED
~~~roc
{
	foo : "Hello"
} | .foo
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_string_access.md:1:1:1:15:**
```roc
{foo: "Hello"}.foo
```
^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.lambda)
~~~
# SOLVED
~~~clojure
(expr :tag lambda :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

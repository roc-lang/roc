# META
~~~ini
description=record_builder
type=expr
~~~
# SOURCE
~~~roc
{ Foo.Bar.baz <-
    x: 5,
    y: 0,
}
~~~
# TOKENS
~~~text
OpenCurly UpperIdent Dot UpperIdent Dot LowerIdent OpBackArrow LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_pipe
    (binop_pipe
      (uc "Foo")
      (uc "Bar")
    )
    (dot_lc "baz")
  )
  (malformed)
  (binop_colon
    (lc "x")
    (num_literal_i32 5)
  )
  (binop_colon
    (lc "y")
    (num_literal_i32 0)
  )
)
~~~
# FORMATTED
~~~roc
{
	Foo.Bar | .baz,
	x : 5,
	y : 0,
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<-
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_builder.md:1:15:2:5:**
```roc
{ Foo.Bar.baz <-
    x: 5,
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_builder.md:1:3:1:14:**
```roc
{ Foo.Bar.baz <-
```
  ^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.num_literal_i32 5)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.num_literal_i32 0)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
{}
~~~

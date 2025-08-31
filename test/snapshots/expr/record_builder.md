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
  (malformed malformed:expr_unexpected_token)
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


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_builder.md:2:5:2:9:**
```roc
    x: 5,
```
    ^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_builder.md:3:5:3:9:**
```roc
    y: 0,
```
    ^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.lambda)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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

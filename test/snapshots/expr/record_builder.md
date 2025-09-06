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
	x: 5,
	y: 0,
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_builder.md:1:15:1:17
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_builder.md:2:8:2:9
UNEXPECTED TOKEN IN EXPRESSION - record_builder.md:2:9:2:10
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_builder.md:3:8:3:9
UNEXPECTED TOKEN IN EXPRESSION - record_builder.md:3:9:3:10
UNDEFINED VARIABLE - record_builder.md:1:3:1:14
UNRECOGNIZED SYNTAX - record_builder.md:1:15:1:17
MALFORMED TYPE - record_builder.md:2:8:2:9
UNRECOGNIZED SYNTAX - record_builder.md:2:9:2:10
MALFORMED TYPE - record_builder.md:3:8:3:9
UNRECOGNIZED SYNTAX - record_builder.md:3:9:3:10
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


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_pipe)
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
; Total type variables: 15
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 Num *)
(var #9 _)
(var #10 _)
(var #11 Num *)
(var #12 _)
(var #13 -> #14)
(var #14 {})
~~~
# TYPES
~~~roc
~~~

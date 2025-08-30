# META
~~~ini
description=Match expression with tag patterns for different cases
type=expr
~~~
# SOURCE
~~~roc
match Answer {
    Answer => 1
    Zero => "hello"
    Greeting => 3
    10 => 4
}
~~~
# TOKENS
~~~text
KwMatch UpperIdent OpenCurly UpperIdent OpFatArrow Int UpperIdent OpFatArrow String UpperIdent OpFatArrow Int Int OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (uc "Answer")
)
  (branch1     (binop_thick_arrow
      (uc "Answer")
      (num_literal_i32 1)
    )
)
  (branch2     (binop_thick_arrow
      (uc "Zero")
      (str_literal_big "hello")
    )
)
  (branch3     (binop_thick_arrow
      (uc "Greeting")
      (num_literal_i32 3)
    )
)
  (branch4     (binop_thick_arrow
      (num_literal_i32 10)
      (num_literal_i32 4)
    )
))
~~~
# FORMATTED
~~~roc
match Answer
	Answer => 1
	Zero => "hello"
	Greeting => 3
	10 => 4
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:12 to 2:14

**Unsupported Node**
at 3:5 to 3:9

**Unsupported Node**
at 4:14 to 4:16

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

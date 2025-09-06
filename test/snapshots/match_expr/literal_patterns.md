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
      (block
        (num_literal_i32 3)
        (binop_thick_arrow
          (num_literal_i32 10)
          (num_literal_i32 4)
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match Answer
	Answer => 1
	Zero => "hello"
	Greeting => 
		3
		10 => 4
~~~
# EXPECTED
INCOMPATIBLE MATCH BRANCHES - literal_patterns.md:1:1:1:1
INCOMPATIBLE MATCH PATTERNS - literal_patterns.md:1:1:1:1
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 16
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Num *)
(var #4 _)
(var #5 _)
(var #6 Str)
(var #7 _)
(var #8 _)
(var #9 Num *)
(var #10 _)
(var #11 Num *)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
~~~
# TYPES
~~~roc
~~~

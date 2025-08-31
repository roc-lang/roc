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
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**literal_patterns.md:2:5:2:16:**
```roc
    Answer => 1
```
    ^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**literal_patterns.md:3:5:3:9:**
```roc
    Zero => "hello"
```
    ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**literal_patterns.md:4:5:5:12:**
```roc
    Greeting => 3
    10 => 4
```


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

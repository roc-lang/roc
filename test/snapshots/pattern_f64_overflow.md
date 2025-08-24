# META
~~~ini
description=Match expression with fractional literals that exceed Dec precision
type=expr
~~~
# SOURCE
~~~roc
match x {
    1e100 => "very large number"
    1e-40 => "very small number"
    1.7976931348623157e308 => "near f64 max"
    0.0 => "zero"
    value => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly Float OpFatArrow String Float OpFatArrow String Float OpFatArrow String Float OpFatArrow String LowerIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <15 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_f64_overflow.md:1:7:1:8
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
UNUSED VARIABLE - pattern_f64_overflow.md:6:5:6:10
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 2:11 to 2:11

**Parse Error**
at 3:11 to 3:11

**Parse Error**
at 4:28 to 4:28

**Parse Error**
at 5:9 to 5:9

**Parse Error**
at 6:11 to 6:11

**Parse Error**
at 1:1 to 7:2

**Parse Error**
at 7:2 to 7:2

**Unsupported Node**
at 1:1 to 7:2

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~

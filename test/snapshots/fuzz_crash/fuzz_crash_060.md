# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]C:k||match 0{0|#
0"
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColon LowerIdent OpOr KwMatch Int OpenCurly Int OpBar LineComment Int MalformedString CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon
    (uc "C")
    (binop_or
      (lc "k")
      (match
        (scrutinee           (num_literal_i32 0)
))
    )
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module []

C : k || match 0
#
}
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_060.md:1:12:1:13
PARSE ERROR - fuzz_crash_060.md:1:13:1:14
PARSE ERROR - fuzz_crash_060.md:1:14:1:19
PARSE ERROR - fuzz_crash_060.md:1:20:1:21
PARSE ERROR - fuzz_crash_060.md:1:21:1:22
PARSE ERROR - fuzz_crash_060.md:1:22:1:23
PARSE ERROR - fuzz_crash_060.md:1:23:1:24
PARSE ERROR - fuzz_crash_060.md:2:1:2:2
PARSE ERROR - fuzz_crash_060.md:2:2:2:3
PARSE ERROR - fuzz_crash_060.md:2:3:2:3
PARSE ERROR - fuzz_crash_060.md:2:3:2:3
PARSE ERROR - fuzz_crash_060.md:3:1:3:2
UNDECLARED TYPE VARIABLE - fuzz_crash_060.md:1:11:1:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:2:2:3:1:**
```roc
0"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_060.md:3:1:3:2:**
```roc
}
```
^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_060.md:1:11:1:21:**
```roc
module[]C:k||match 0{0|#
```
          ^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
~~~
# TYPES
~~~roc
~~~

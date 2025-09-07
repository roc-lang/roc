# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
# TOKENS
~~~text
KwModule UpperIdent CloseSquare UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "P")

    (malformed)

    (uc "F")
))
(block
  (uc "P")
  (malformed)
  (uc "F")
)
~~~
# FORMATTED
~~~roc
module [P, ], F]

P
]
F
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_011.md:1:8:1:9
PARSE ERROR - fuzz_crash_011.md:1:9:1:10
PARSE ERROR - fuzz_crash_011.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **header_expected_open_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_011.md:1:1:1:8:**
```roc
module P]F
```
^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_011.md:1:9:1:10:**
```roc
module P]F
```
        ^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.tag_no_args)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
~~~
# TYPES
~~~roc
~~~

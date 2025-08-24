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
KwModule OpenSquare CloseSquare UpperIdent OpColon LowerIdent OpOr KwMatch Int OpenCurly Int OpBar Int MalformedString CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "C")
    (binop_or
      (lc "k")
      (match <4 branches>)
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
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
**Parse Error**
at 1:14 to 1:21

**Parse Error**
at 2:2 to 2:2

**Parse Error**
at 1:14 to 3:2

**Parse Error**
at 3:2 to 3:2

**Unsupported Node**
at 1:21 to 3:1

**Unsupported Node**
at 3:2 to 3:2

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_or
      (Expr.lookup "k")
      (Expr.match)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~

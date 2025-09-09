# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]({})(!{0})
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenRound OpenCurly CloseCurly CloseRound OpenRound OpBang OpenCurly Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(module-header)
(block
  (apply_anon
    (record_literal)
    (unary_not <unary_op>)
  )
)
~~~
# FORMATTED
~~~roc
module []

{}(!{
	0
})
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_072.md:1:9:1:10
PARSE ERROR - fuzz_crash_072.md:1:10:1:11
PARSE ERROR - fuzz_crash_072.md:1:11:1:12
PARSE ERROR - fuzz_crash_072.md:1:12:1:13
PARSE ERROR - fuzz_crash_072.md:1:13:1:14
PARSE ERROR - fuzz_crash_072.md:1:14:1:15
PARSE ERROR - fuzz_crash_072.md:1:15:1:16
PARSE ERROR - fuzz_crash_072.md:1:16:1:17
PARSE ERROR - fuzz_crash_072.md:1:17:1:18
PARSE ERROR - fuzz_crash_072.md:1:18:1:19
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_072.md:1:12:1:13:**
```roc
module[]({})(!{0})
```
           ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.fn_call)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 -> #8)
(var #2 Num *)
(var #3 _)
(var #4 -> #3)
(var #5 _)
(var #6 _)
(var #7 {})
(var #8 <error>)
~~~
# TYPES
~~~roc
~~~

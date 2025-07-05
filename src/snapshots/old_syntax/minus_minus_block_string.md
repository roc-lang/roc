# META
~~~ini
description=minus_minus_block_string
type=expr
~~~
# SOURCE
~~~roc
--""""""
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **--** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**minus_minus_block_string.md:1:1:1:3:**
```roc
--""""""
```
^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),OpBinaryMinus(1:2-1:3),MultilineStringStart(1:3-1:6),StringPart(1:6-1:6),MultilineStringEnd(1:6-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.3 (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

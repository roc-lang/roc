# META
~~~ini
description=minus_minus_six
type=expr
~~~
# SOURCE
~~~roc
(-(-6))
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**minus_minus_six.md:1:2:1:4:**
```roc
(-(-6))
```
 ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**minus_minus_six.md:1:6:1:8:**
```roc
(-(-6))
```
     ^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBinaryMinus(1:2-1:3),NoSpaceOpenRound(1:3-1:4),OpBinaryMinus(1:4-1:5),Int(1:5-1:6),CloseRound(1:6-1:7),CloseRound(1:7-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(e-malformed @1.6-1.8 (reason "expected_expr_close_round_or_comma"))
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

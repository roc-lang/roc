# META
~~~ini
description=Malformed hex number (0x without digits)
type=expr
~~~
# SOURCE
~~~roc
0x
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - parse_malformed_hex_number.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **0x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**parse_malformed_hex_number.md:1:1:1:3:**
```roc
0x
```
^^


# TOKENS
~~~zig
MalformedNumberNoDigits,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expr_unexpected_token"))
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

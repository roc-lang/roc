# META
~~~ini
description=Malformed octal number (0o without digits)
type=expr
~~~
# SOURCE
~~~roc
0o
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - parse_malformed_octal_number.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **0o** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**parse_malformed_octal_number.md:1:1:1:3:**
```roc
0o
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

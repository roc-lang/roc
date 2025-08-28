# META
~~~ini
description=Negative single quote char literal
type=expr
~~~
# SOURCE
~~~roc
-'i'
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - negative_single_quote.md:1:1:1:2
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**negative_single_quote.md:1:1:1:2:**
```roc
-'i'
```
^


# TOKENS
~~~zig
OpBinaryMinus(1:1-1:2),SingleQuote(1:2-1:5),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.2 (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc
MALFORMED INPUT
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

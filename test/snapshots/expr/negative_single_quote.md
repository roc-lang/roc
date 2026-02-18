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
OpBinaryMinus,SingleQuote,
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

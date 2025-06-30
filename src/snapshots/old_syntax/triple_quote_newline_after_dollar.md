# META
~~~ini
description=triple_quote_newline_after_dollar fail
type=expr
~~~
# SOURCE
~~~roc
"""$
(
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"""$** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**triple_quote_newline_after_dollar.md:1:1:1:5:**
```roc
"""$
```
^^^^


# TOKENS
~~~zig
MultilineStringStart(1:1-1:4),StringPart(1:4-1:5),Newline(1:1-1:1),
OpenRound(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.5 (reason "expr_unexpected_token"))
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

# META
~~~ini
description=multi_no_end fail
type=expr
~~~
# SOURCE
~~~roc
"""there is no end
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"""there is no end** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_no_end.md:1:1:1:19:**
```roc
"""there is no end
```
^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
MultilineStringStart(1:1-1:4),StringPart(1:4-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.19 (reason "expr_unexpected_token"))
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

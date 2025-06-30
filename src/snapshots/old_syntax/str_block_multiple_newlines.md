# META
~~~ini
description=str_block_multiple_newlines
type=expr
~~~
# SOURCE
~~~roc
"""


#"""#
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"""** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**str_block_multiple_newlines.md:1:1:1:4:**
```roc
"""
```
^^^


# TOKENS
~~~zig
MultilineStringStart(1:1-1:4),StringPart(1:4-1:4),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(1:1-1:1),
EndOfFile(4:6-4:6),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.4 (reason "expr_unexpected_token"))
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

# META
~~~ini
description=not_multiline_string
type=expr
~~~
# SOURCE
~~~roc
!""""""
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!"""** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**not_multiline_string.md:1:1:1:5:**
```roc
!""""""
```
^^^^


# TOKENS
~~~zig
OpBang(1:1-1:2),MultilineStringStart(1:2-1:5),StringPart(1:5-1:5),MultilineStringEnd(1:5-1:8),EndOfFile(1:8-1:8),
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

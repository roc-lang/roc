# META
~~~ini
description=large_tuple_index
type=expr
~~~
# SOURCE
~~~roc
.18888888888888888888 + h.22222222222222222222
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.18888888888888888888 +** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**large_tuple_index.md:1:1:1:24:**
```roc
.18888888888888888888 + h.22222222222222222222
```
^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
DotInt(1:1-1:22),OpPlus(1:23-1:24),LowerIdent(1:25-1:26),NoSpaceDotInt(1:26-1:47),EndOfFile(1:47-1:47),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.24 (reason "expr_unexpected_token"))
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

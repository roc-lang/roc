# META
~~~ini
description=empty_return fail
type=expr
~~~
# SOURCE
~~~roc
return
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - empty_return.md:1:1:1:7
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **return** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**empty_return.md:1:1:1:7:**
```roc
return
```
^^^^^^


# TOKENS
~~~zig
KwReturn(1:1-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.7 (reason "expr_unexpected_token"))
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

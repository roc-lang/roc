# META
~~~ini
description=expect
type=expr
~~~
# SOURCE
~~~roc
expect 1 == 1

4
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - expect.md:1:1:1:9
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **expect 1** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expect.md:1:1:1:9:**
```roc
expect 1 == 1
```
^^^^^^^^


# TOKENS
~~~zig
KwExpect(1:1-1:7),Int(1:8-1:9),OpEquals(1:10-1:12),Int(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.9 (reason "expr_unexpected_token"))
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

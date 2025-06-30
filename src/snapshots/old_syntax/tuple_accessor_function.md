# META
~~~ini
description=tuple_accessor_function
type=expr
~~~
# SOURCE
~~~roc
.1 (1, 2, 3)
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.1 (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_accessor_function.md:1:1:1:5:**
```roc
.1 (1, 2, 3)
```
^^^^


# TOKENS
~~~zig
DotInt(1:1-1:3),OpenRound(1:4-1:5),Int(1:5-1:6),Comma(1:6-1:7),Int(1:8-1:9),Comma(1:9-1:10),Int(1:11-1:12),CloseRound(1:12-1:13),EndOfFile(1:13-1:13),
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

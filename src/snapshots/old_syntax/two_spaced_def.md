# META
~~~ini
description=two_spaced_def
type=expr
~~~
# SOURCE
~~~roc
# leading comment
x = 5
y = 6

42
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token ** leading comment
x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**two_spaced_def.md:1:2:2:2:**
```roc
# leading comment
x = 5
```


# TOKENS
~~~zig
Newline(1:2-1:18),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Int(2:5-2:6),Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Int(3:5-3:6),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(5:1-5:3),EndOfFile(5:3-5:3),
~~~
# PARSE
~~~clojure
(e-malformed @1.2-2.2 (reason "expr_unexpected_token"))
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

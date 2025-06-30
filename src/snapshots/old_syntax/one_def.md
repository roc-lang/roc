# META
~~~ini
description=one_def
type=expr
~~~
# SOURCE
~~~roc
# leading comment
x=5

42
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token ** leading comment
x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**one_def.md:1:2:2:2:**
```roc
# leading comment
x=5
```


# TOKENS
~~~zig
Newline(1:2-1:18),
LowerIdent(2:1-2:2),OpAssign(2:2-2:3),Int(2:3-2:4),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:1-4:3),EndOfFile(4:3-4:3),
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

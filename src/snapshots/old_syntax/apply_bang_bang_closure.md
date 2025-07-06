# META
~~~ini
description=apply_bang_bang_closure
type=expr
~~~
# SOURCE
~~~roc
!!
 \w->2
 n
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - apply_bang_bang_closure.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!!** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**apply_bang_bang_closure.md:1:1:1:3:**
```roc
!!
```
^^


# TOKENS
~~~zig
OpBang(1:1-1:2),OpBang(1:2-1:3),Newline(1:1-1:1),
OpBackslash(2:2-2:3),LowerIdent(2:3-2:4),OpArrow(2:4-2:6),Int(2:6-2:7),Newline(1:1-1:1),
LowerIdent(3:2-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.3 (reason "expr_unexpected_token"))
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

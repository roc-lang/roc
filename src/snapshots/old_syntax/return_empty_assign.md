# META
~~~ini
description=return_empty_assign
type=expr
~~~
# SOURCE
~~~roc
return
 {}=#
 s
 r
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - return_empty_assign.md:1:1:1:1
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_empty_assign.md:1:1:1:1:**
```roc
return
```



# TOKENS
~~~zig
KwReturn(1:1-1:7),Newline(1:1-1:1),
OpenCurly(2:2-2:3),CloseCurly(2:3-2:4),OpAssign(2:4-2:5),Newline(2:6-2:6),
LowerIdent(3:2-3:3),Newline(1:1-1:1),
LowerIdent(4:2-4:3),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
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

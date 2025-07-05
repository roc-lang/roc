# META
~~~ini
description=double_closure_newlines_binop
type=expr
~~~
# SOURCE
~~~roc
\j->e\B
 ->B
 >s
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\j** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**double_closure_newlines_binop.md:1:1:1:3:**
```roc
\j->e\B
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),LowerIdent(1:2-1:3),OpArrow(1:3-1:5),LowerIdent(1:5-1:6),OpBackslash(1:6-1:7),UpperIdent(1:7-1:8),Newline(1:1-1:1),
OpArrow(2:2-2:4),UpperIdent(2:4-2:5),Newline(1:1-1:1),
OpGreaterThan(3:2-3:3),LowerIdent(3:3-3:4),EndOfFile(3:4-3:4),
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

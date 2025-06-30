# META
~~~ini
description=opaque_destructure_first_item_in_body
type=expr
~~~
# SOURCE
~~~roc
@Thunk it = id (@A {})
it {}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **@Thunk it** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**opaque_destructure_first_item_in_body.md:1:1:1:10:**
```roc
@Thunk it = id (@A {})
```
^^^^^^^^^


# TOKENS
~~~zig
OpaqueName(1:1-1:7),LowerIdent(1:8-1:10),OpAssign(1:11-1:12),LowerIdent(1:13-1:15),OpenRound(1:16-1:17),OpaqueName(1:17-1:19),OpenCurly(1:20-1:21),CloseCurly(1:21-1:22),CloseRound(1:22-1:23),Newline(1:1-1:1),
LowerIdent(2:1-2:3),OpenCurly(2:4-2:5),CloseCurly(2:5-2:6),EndOfFile(2:6-2:6),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.10 (reason "expr_unexpected_token"))
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

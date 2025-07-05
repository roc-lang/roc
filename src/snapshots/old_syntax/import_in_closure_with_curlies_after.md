# META
~~~ini
description=import_in_closure_with_curlies_after
type=expr
~~~
# SOURCE
~~~roc
\L->
 import U
 {}e
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - import_in_closure_with_curlies_after.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\L** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**import_in_closure_with_curlies_after.md:1:1:1:3:**
```roc
\L->
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:3),OpArrow(1:3-1:5),Newline(1:1-1:1),
KwImport(2:2-2:8),UpperIdent(2:9-2:10),Newline(1:1-1:1),
OpenCurly(3:2-3:3),CloseCurly(3:3-3:4),LowerIdent(3:4-3:5),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
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

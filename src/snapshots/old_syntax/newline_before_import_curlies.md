# META
~~~ini
description=newline_before_import_curlies
type=expr
~~~
# SOURCE
~~~roc
import P
 {}
y
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - newline_before_import_curlies.md:1:1:1:9
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **import P** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**newline_before_import_curlies.md:1:1:1:9:**
```roc
import P
```
^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),UpperIdent(1:8-1:9),Newline(1:1-1:1),
OpenCurly(2:2-2:3),CloseCurly(2:3-2:4),Newline(1:1-1:1),
LowerIdent(3:1-3:2),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
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

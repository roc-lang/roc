# META
~~~ini
description=can_ignored_field_in_import
type=expr
~~~
# SOURCE
~~~roc
import P{_:h
}
t!
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - can_ignored_field_in_import.md:1:1:1:9
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **import P** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_ignored_field_in_import.md:1:1:1:9:**
```roc
import P{_:h
```
^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),UpperIdent(1:8-1:9),OpenCurly(1:9-1:10),Underscore(1:10-1:11),OpColon(1:11-1:12),LowerIdent(1:12-1:13),Newline(1:1-1:1),
CloseCurly(2:1-2:2),Newline(1:1-1:1),
LowerIdent(3:1-3:3),EndOfFile(3:3-3:3),
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

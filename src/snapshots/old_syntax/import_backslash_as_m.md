# META
~~~ini
description=import_backslash_as_m
type=expr
~~~
# SOURCE
~~~roc
import"\\"as m
e
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **import"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**import_backslash_as_m.md:1:1:1:8:**
```roc
import"\\"as m
```
^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),StringStart(1:7-1:8),StringPart(1:8-1:10),StringEnd(1:10-1:11),KwAs(1:11-1:13),LowerIdent(1:14-1:15),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.8 (reason "expr_unexpected_token"))
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

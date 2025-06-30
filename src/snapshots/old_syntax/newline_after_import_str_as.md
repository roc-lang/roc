# META
~~~ini
description=newline_after_import_str_as
type=expr
~~~
# SOURCE
~~~roc
import""as
 das
A
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **import"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**newline_after_import_str_as.md:1:1:1:8:**
```roc
import""as
```
^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),StringStart(1:7-1:8),StringPart(1:8-1:8),StringEnd(1:8-1:9),KwAs(1:9-1:11),Newline(1:1-1:1),
LowerIdent(2:2-2:5),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
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

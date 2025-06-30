# META
~~~ini
description=inline_ingested_file_no_ann
type=expr
~~~
# SOURCE
~~~roc
import "users.json" as data

parseJson data
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **import "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**inline_ingested_file_no_ann.md:1:1:1:9:**
```roc
import "users.json" as data
```
^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),StringStart(1:8-1:9),StringPart(1:9-1:19),StringEnd(1:19-1:20),KwAs(1:21-1:23),LowerIdent(1:24-1:28),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:10),LowerIdent(3:11-3:15),EndOfFile(3:15-3:15),
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

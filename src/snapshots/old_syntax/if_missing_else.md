# META
~~~ini
description=if_missing_else fail
type=expr
~~~
# SOURCE
~~~roc
if 5 == 5 then 2
~~~
# EXPECTED
PARSE ERROR - if_missing_else.md:1:16:1:17
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**if_missing_else.md:1:16:1:17:**
```roc
if 5 == 5 then 2
```
               ^


# TOKENS
~~~zig
KwIf(1:1-1:3),Int(1:4-1:5),OpEquals(1:6-1:8),Int(1:9-1:10),LowerIdent(1:11-1:15),Int(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(e-malformed @1.16-1.17 (reason "no_else"))
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

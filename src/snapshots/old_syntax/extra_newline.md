# META
~~~ini
description=extra_newline
type=expr
~~~
# SOURCE
~~~roc
if foo then
    x = a # 1
    x # 2
else # 3
    c # 4
~~~
~~~
# EXPECTED
PARSE ERROR - extra_newline.md:2:5:2:8
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**extra_newline.md:2:5:2:8:**
```roc
    x = a # 1
```
    ^^^


# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:7),LowerIdent(1:8-1:12),Newline(1:1-1:1),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),LowerIdent(2:9-2:10),Newline(2:12-2:14),
LowerIdent(3:5-3:6),Newline(3:8-3:10),
KwElse(4:1-4:5),Newline(4:7-4:9),
LowerIdent(5:5-5:6),Newline(5:8-5:10),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(e-malformed @2.5-2.8 (reason "no_else"))
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

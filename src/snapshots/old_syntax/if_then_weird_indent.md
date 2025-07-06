# META
~~~ini
description=if_then_weird_indent
type=expr
~~~
# SOURCE
~~~roc
if
k
then
A
 else
 e
r
~~~
# EXPECTED
PARSE ERROR - if_then_weird_indent.md:4:1:4:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**if_then_weird_indent.md:4:1:4:1:**
```roc
A
```



# TOKENS
~~~zig
KwIf(1:1-1:3),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
LowerIdent(3:1-3:5),Newline(1:1-1:1),
UpperIdent(4:1-4:2),Newline(1:1-1:1),
KwElse(5:2-5:6),Newline(1:1-1:1),
LowerIdent(6:2-6:3),Newline(1:1-1:1),
LowerIdent(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "no_else"))
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

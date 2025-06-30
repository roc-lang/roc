# META
~~~ini
description=nested_if
type=expr
~~~
# SOURCE
~~~roc
if t1 then
  1
else if t2 then
  2
else
  3
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**nested_if.md:2:3:2:3:**
```roc
  1
```
  


# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:6),LowerIdent(1:7-1:11),Newline(1:1-1:1),
Int(2:3-2:4),Newline(1:1-1:1),
KwElse(3:1-3:5),KwIf(3:6-3:8),LowerIdent(3:9-3:11),LowerIdent(3:12-3:16),Newline(1:1-1:1),
Int(4:3-4:4),Newline(1:1-1:1),
KwElse(5:1-5:5),Newline(1:1-1:1),
Int(6:3-6:4),EndOfFile(6:4-6:4),
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

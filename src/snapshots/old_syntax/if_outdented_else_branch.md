# META
~~~ini
description=if_outdented_else_branch fail
type=expr
~~~
# SOURCE
~~~roc
if thing then
    whatever
else
something better
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**if_outdented_else_branch.md:2:5:2:5:**
```roc
    whatever
```
    


# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:9),LowerIdent(1:10-1:14),Newline(1:1-1:1),
LowerIdent(2:5-2:13),Newline(1:1-1:1),
KwElse(3:1-3:5),Newline(1:1-1:1),
LowerIdent(4:1-4:10),LowerIdent(4:11-4:17),EndOfFile(4:17-4:17),
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

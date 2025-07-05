# META
~~~ini
description=module_dot_tuple malformed
type=expr
~~~
# SOURCE
~~~roc
I.5
~~~
~~~
# EXPECTED
PARSE ERROR - module_dot_tuple.md:1:2:1:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**module_dot_tuple.md:1:2:1:2:**
```roc
I.5
```
 


# TOKENS
~~~zig
UpperIdent(1:1-1:2),NoSpaceDotInt(1:2-1:4),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "expr_no_space_dot_int"))
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

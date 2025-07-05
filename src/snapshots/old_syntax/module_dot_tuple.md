# META
~~~ini
description=module_dot_tuple malformed
type=expr
~~~
# SOURCE
~~~roc
I.5
~~~
# EXPECTED
expr_no_space_dot_int - module_dot_tuple.md:1:2:1:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**module_dot_tuple.md:1:2:1:4:**
```roc
I.5
```
 ^^


# TOKENS
~~~zig
UpperIdent(1:1-1:2),NoSpaceDotInt(1:2-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.2-1.4 (reason "expr_no_space_dot_int"))
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

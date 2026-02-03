# META
~~~ini
description=Caret operator (^) parsing
type=snippet
~~~
# SOURCE
~~~roc
2 ^ 3 ^ 4
~~~
# EXPECTED
CARET OPERATOR NOT SUPPORTED - parse_caret_operator.md:1:1:1:10
# PROBLEMS
**CARET OPERATOR NOT SUPPORTED**
Roc doesn't have a caret operator (**^**).

For exponentiation, use **Num.pow** or **Num.powInt** instead.

**parse_caret_operator.md:1:1:1:10:**
```roc
2 ^ 3 ^ 4
```
^^^^^^^^^


# TOKENS
~~~zig
Int,OpCaret,Int,OpCaret,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "caret_operator_not_supported"))))
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

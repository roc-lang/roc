# META
~~~ini
description=Match expression with no branches should produce error
type=expr
~~~
# SOURCE
~~~roc
match 42 {}
~~~
# EXPECTED
PARSE ERROR - empty_match.md:1:1:1:6
# PROBLEMS
**PARSE ERROR**
A match expression must have at least one branch.

**empty_match.md:1:1:1:6:**
```roc
match 42 {}
```
^^^^^


# TOKENS
~~~zig
KwMatch,Int,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "match_has_no_branches"))
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

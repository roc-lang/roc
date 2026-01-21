# META
~~~ini
description=Formatter crash with inline definition in parentheses followed by method call - issue 9046
type=snippet
~~~
# SOURCE
~~~roc
(x = 5).foo()
~~~
# EXPECTED
PARSE ERROR - formatter_crash_issue_9046.md:1:1:1:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**formatter_crash_issue_9046.md:1:1:1:2:**
```roc
(x = 5).foo()
```
^


# TOKENS
~~~zig
OpenRound,LowerIdent,OpAssign,Int,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir)
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

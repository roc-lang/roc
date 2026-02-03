# META
~~~ini
description=Unary minus and boolean not (should error)
type=snippet
~~~
# SOURCE
~~~roc
-!h
~~~
# EXPECTED
PARSE ERROR - minus_not_h.md:1:1:1:2
PARSE ERROR - minus_not_h.md:1:2:1:3
PARSE ERROR - minus_not_h.md:1:3:1:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**minus_not_h.md:1:1:1:2:**
```roc
-!h
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**minus_not_h.md:1:2:1:3:**
```roc
-!h
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**minus_not_h.md:1:3:1:4:**
```roc
-!h
```
  ^


# TOKENS
~~~zig
OpBinaryMinus,OpBang,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
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

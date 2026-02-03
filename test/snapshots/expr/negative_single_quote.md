# META
~~~ini
description=Negative single quote char literal
type=snippet
~~~
# SOURCE
~~~roc
-'i'
~~~
# EXPECTED
PARSE ERROR - negative_single_quote.md:1:1:1:2
PARSE ERROR - negative_single_quote.md:1:2:1:5
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**negative_single_quote.md:1:1:1:2:**
```roc
-'i'
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**negative_single_quote.md:1:2:1:5:**
```roc
-'i'
```
 ^^^


# TOKENS
~~~zig
OpBinaryMinus,SingleQuote,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
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

# META
~~~ini
description=Unknown operator, should produce an error
type=snippet
~~~
# SOURCE
~~~roc
1 ++ 2
~~~
# EXPECTED
PARSE ERROR - unknown_operator.md:1:1:1:2
PARSE ERROR - unknown_operator.md:1:3:1:4
PARSE ERROR - unknown_operator.md:1:4:1:5
PARSE ERROR - unknown_operator.md:1:6:1:7
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**unknown_operator.md:1:1:1:2:**
```roc
1 ++ 2
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**unknown_operator.md:1:3:1:4:**
```roc
1 ++ 2
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**unknown_operator.md:1:4:1:5:**
```roc
1 ++ 2
```
   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**unknown_operator.md:1:6:1:7:**
```roc
1 ++ 2
```
     ^


# TOKENS
~~~zig
Int,OpPlus,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
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

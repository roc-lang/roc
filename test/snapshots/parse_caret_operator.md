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
PARSE ERROR - parse_caret_operator.md:1:1:1:2
PARSE ERROR - parse_caret_operator.md:1:3:1:4
PARSE ERROR - parse_caret_operator.md:1:5:1:6
PARSE ERROR - parse_caret_operator.md:1:7:1:8
PARSE ERROR - parse_caret_operator.md:1:9:1:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_caret_operator.md:1:1:1:2:**
```roc
2 ^ 3 ^ 4
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_caret_operator.md:1:3:1:4:**
```roc
2 ^ 3 ^ 4
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_caret_operator.md:1:5:1:6:**
```roc
2 ^ 3 ^ 4
```
    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_caret_operator.md:1:7:1:8:**
```roc
2 ^ 3 ^ 4
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**parse_caret_operator.md:1:9:1:10:**
```roc
2 ^ 3 ^ 4
```
        ^


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
		(s-malformed (tag "statement_unexpected_token"))
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

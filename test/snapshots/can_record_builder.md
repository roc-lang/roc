# META
~~~ini
description=Test record builder expression (not yet implemented)
type=snippet
~~~
# SOURCE
~~~roc
{build} foo bar
~~~
# EXPECTED
PARSE ERROR - can_record_builder.md:1:1:1:2
PARSE ERROR - can_record_builder.md:1:2:1:7
PARSE ERROR - can_record_builder.md:1:7:1:8
PARSE ERROR - can_record_builder.md:1:9:1:12
PARSE ERROR - can_record_builder.md:1:13:1:16
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_record_builder.md:1:1:1:2:**
```roc
{build} foo bar
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_record_builder.md:1:2:1:7:**
```roc
{build} foo bar
```
 ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_record_builder.md:1:7:1:8:**
```roc
{build} foo bar
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_record_builder.md:1:9:1:12:**
```roc
{build} foo bar
```
        ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_record_builder.md:1:13:1:16:**
```roc
{build} foo bar
```
            ^^^


# TOKENS
~~~zig
OpenCurly,LowerIdent,CloseCurly,LowerIdent,LowerIdent,
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

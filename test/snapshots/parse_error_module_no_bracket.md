# META
~~~ini
description=Module header without open bracket
type=header
~~~
# SOURCE
~~~roc
module foo
~~~
# EXPECTED
PARSE ERROR - parse_error_module_no_bracket.md:1:8:1:11
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `header_expected_open_square`
This is an unexpected parsing error. Please check your syntax.

**parse_error_module_no_bracket.md:1:8:1:11:**
```roc
module foo
```
       ^^^


# TOKENS
~~~zig
KwModule,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(malformed-header (tag "header_expected_open_square"))
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

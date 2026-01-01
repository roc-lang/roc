# META
~~~ini
description=App header without open bracket
type=header
~~~
# SOURCE
~~~roc
app main {}
~~~
# EXPECTED
PARSE ERROR - parse_error_app_no_bracket.md:1:5:1:9
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_provides_open_square`
This is an unexpected parsing error. Please check your syntax.

**parse_error_app_no_bracket.md:1:5:1:9:**
```roc
app main {}
```
    ^^^^


# TOKENS
~~~zig
KwApp,LowerIdent,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(malformed-header (tag "expected_provides_open_square"))
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

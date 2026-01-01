# META
~~~ini
description=List pattern with old rest syntax
type=expr
~~~
# SOURCE
~~~roc
match list
    [first, ..rest] -> first
~~~
# EXPECTED
PARSE ERROR - parse_error_list_rest_old_syntax.md:2:5:2:6
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_open_curly_after_match`
This is an unexpected parsing error. Please check your syntax.

**parse_error_list_rest_old_syntax.md:2:5:2:6:**
```roc
    [first, ..rest] -> first
```
    ^


# TOKENS
~~~zig
KwMatch,LowerIdent,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpArrow,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expected_open_curly_after_match"))
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

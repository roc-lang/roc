# META
~~~ini
description=Platform header for clause missing close square
type=header
~~~
# SOURCE
~~~roc
platform "test" requires { [Model : model for main : {} -> {} }
~~~
# EXPECTED
PARSE ERROR - parse_error_platform_for_close.md:1:43:1:46
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_for_clause_close_square`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_close.md:1:43:1:46:**
```roc
platform "test" requires { [Model : model for main : {} -> {} }
```
                                          ^^^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,OpenCurly,OpenSquare,UpperIdent,OpColon,LowerIdent,KwFor,LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(malformed-header (tag "expected_for_clause_close_square"))
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

# META
~~~ini
description=Platform header for clause missing rigid name
type=header
~~~
# SOURCE
~~~roc
platform "test" requires { [Model : ] for main : {} -> {} }
~~~
# EXPECTED
PARSE ERROR - parse_error_platform_for_rigid.md:1:37:1:38
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_for_clause_rigid_name`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_rigid.md:1:37:1:38:**
```roc
platform "test" requires { [Model : ] for main : {} -> {} }
```
                                    ^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,OpenCurly,OpenSquare,UpperIdent,OpColon,CloseSquare,KwFor,LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(malformed-header (tag "expected_for_clause_rigid_name"))
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

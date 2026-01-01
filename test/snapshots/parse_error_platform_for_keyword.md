# META
~~~ini
description=Platform header for clause missing for keyword
type=header
~~~
# SOURCE
~~~roc
platform "test" requires { [Model : model] main : {} -> {} }
~~~
# EXPECTED
PARSE ERROR - parse_error_platform_for_keyword.md:1:44:1:48
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_for_keyword`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_for_keyword.md:1:44:1:48:**
```roc
platform "test" requires { [Model : model] main : {} -> {} }
```
                                           ^^^^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,OpenCurly,OpenSquare,UpperIdent,OpColon,LowerIdent,CloseSquare,LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(malformed-header (tag "expected_for_keyword"))
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

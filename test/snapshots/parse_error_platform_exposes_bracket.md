# META
~~~ini
description=Platform header exposes without open bracket
type=header
~~~
# SOURCE
~~~roc
platform "test" requires { main : {} -> {} } exposes foo packages {}
~~~
# EXPECTED
EXPECTED OPENING BRACKET - parse_error_platform_exposes_bracket.md:1:54:1:57
# PROBLEMS
**EXPECTED OPENING BRACKET**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**parse_error_platform_exposes_bracket.md:1:54:1:57:**
```roc
platform "test" requires { main : {} -> {} } exposes foo packages {}
```
                                                     ^^^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,OpenCurly,LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenCurly,CloseCurly,CloseCurly,KwExposes,LowerIdent,KwPackages,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(malformed-header (tag "expected_exposes_open_square"))
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

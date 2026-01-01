# META
~~~ini
description=Platform header without exposes
type=header
~~~
# SOURCE
~~~roc
platform "test" requires { main : {} -> {} } packages {}
~~~
# EXPECTED
EXPECTED EXPOSES - parse_error_platform_no_exposes.md:1:46:1:54
# PROBLEMS
**EXPECTED EXPOSES**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**parse_error_platform_no_exposes.md:1:46:1:54:**
```roc
platform "test" requires { main : {} -> {} } packages {}
```
                                             ^^^^^^^^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,OpenCurly,LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenCurly,CloseCurly,CloseCurly,KwPackages,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(malformed-header (tag "expected_exposes"))
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

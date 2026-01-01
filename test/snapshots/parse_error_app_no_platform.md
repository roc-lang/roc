# META
~~~ini
description=App header without platform
type=file
~~~
# SOURCE
~~~roc
app [main] {}
~~~
# EXPECTED
NO PLATFORM - parse_error_app_no_platform.md:1:1:1:4
# PROBLEMS
**NO PLATFORM**
App files must specify a platform.
Add a platform specification like:
        { pf: platform "../basic-cli/platform.roc" }

**parse_error_app_no_platform.md:1:1:1:4:**
```roc
app [main] {}
```
^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "no_platform"))
	(statements))
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

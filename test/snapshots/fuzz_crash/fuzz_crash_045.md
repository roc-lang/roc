# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform""requires{}{}exposes[]packages{}provides[
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_045.md:1:50:1:51
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_provides_open_curly`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_045.md:1:50:1:51:**
```roc
platform""requires{}{}exposes[]packages{}provides[
```
                                                 ^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,OpenCurly,CloseCurly,OpenCurly,CloseCurly,KwExposes,OpenSquare,CloseSquare,KwPackages,OpenCurly,CloseCurly,KwProvides,OpenSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_provides_open_curly"))
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

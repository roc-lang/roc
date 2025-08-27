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
KwPlatform(1:1-1:9),StringStart(1:9-1:10),StringPart(1:10-1:10),StringEnd(1:10-1:11),KwRequires(1:11-1:19),OpenCurly(1:19-1:20),CloseCurly(1:20-1:21),OpenCurly(1:21-1:22),CloseCurly(1:22-1:23),KwExposes(1:23-1:30),OpenSquare(1:30-1:31),CloseSquare(1:31-1:32),KwPackages(1:32-1:40),OpenCurly(1:40-1:41),CloseCurly(1:41-1:42),KwProvides(1:42-1:50),OpenSquare(1:50-1:51),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.51
	(malformed-header @1.50-1.51 (tag "expected_provides_open_curly"))
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

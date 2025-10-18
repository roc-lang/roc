# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform"
requires{}{n:0[import S	exposing[
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_061.md:2:14:2:15
PARSE ERROR - fuzz_crash_061.md:2:11:2:12
PARSE ERROR - fuzz_crash_061.md:2:16:2:22
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

```roc
platform"
```
        ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_061.md:2:14:2:15:**
```roc
requires{}{n:0[import S	exposing[
```
             ^


**PARSE ERROR**
A parsing error occurred: `expected_requires_signatures_close_curly`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:2:11:2:12:**
```roc
requires{}{n:0[import S	exposing[
```
          ^


**PARSE ERROR**
A parsing error occurred: `import_exposing_no_close`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:2:16:2:22:**
```roc
requires{}{n:0[import S	exposing[
```
               ^^^^^^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,OpenCurly,LowerIdent,OpColon,Int,OpenSquare,KwImport,UpperIdent,KwExposing,OpenSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_requires_signatures_close_curly"))
	(statements
		(s-malformed (tag "import_exposing_no_close"))))
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

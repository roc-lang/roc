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
KwPlatform(1:1-1:9),StringStart(1:9-1:10),StringPart(1:10-1:10),StringEnd(1:10-1:10),
KwRequires(2:1-2:9),OpenCurly(2:9-2:10),CloseCurly(2:10-2:11),OpenCurly(2:11-2:12),LowerIdent(2:12-2:13),OpColon(2:13-2:14),Int(2:14-2:15),OpenSquare(2:15-2:16),KwImport(2:16-2:22),UpperIdent(2:23-2:24),KwExposing(2:25-2:33),OpenSquare(2:33-2:34),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.34
	(malformed-header @2.11-2.16 (tag "expected_requires_signatures_close_curly"))
	(statements
		(s-malformed @2.16-2.34 (tag "import_exposing_no_close"))))
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

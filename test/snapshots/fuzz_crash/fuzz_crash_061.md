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
UNCLOSED STRING - fuzz_crash_061.md:1:9:1:10
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_061.md:2:14:2:15
PARSE ERROR - fuzz_crash_061.md:1:1:1:9
PARSE ERROR - fuzz_crash_061.md:2:16:2:22
# PROBLEMS

┌─────────────────┐
│ UNCLOSED STRING ├─ This string is missing a closing quote. ─────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  platform"                                                                 │
 │          ‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_061.md:1:9 ┘



┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token 0 is not expected in a ────┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  requires{}{n:0[import S exposing[                                         │
 │               ‾                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_061.md:2:14 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: ───────────────────────────────────┐
└┬────────────┘  expected_requires_signatures_close_curly                     │
 │                                                                            │
 │  platform"                                                                 │
 │  ‾‾‾‾‾‾‾‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_061.md:1:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: import_exposing_no_close ──────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  requires{}{n:0[import S exposing[                                         │
 │                 ‾‾‾‾‾‾                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_061.md:2:16 ┘

    This is an unexpected parsing error. Please check your syntax.

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

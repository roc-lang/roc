# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
			} #Ce
	exposes #rd
		[ .
		] # Cse
	packages # Cd
		{ # pen
pkg: 77"..c", mm} #
	provides # Cd
		[ # pen
ar,
		]
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_030.md:8:5:8:6
PARSE ERROR - fuzz_crash_030.md:12:8:12:9
PARSE ERROR - fuzz_crash_030.md:12:9:12:12
PARSE ERROR - fuzz_crash_030.md:12:12:12:13
PARSE ERROR - fuzz_crash_030.md:12:13:12:14
PARSE ERROR - fuzz_crash_030.md:12:15:12:17
PARSE ERROR - fuzz_crash_030.md:12:17:12:18
PARSE ERROR - fuzz_crash_030.md:13:2:13:10
PARSE ERROR - fuzz_crash_030.md:14:3:14:4
PARSE ERROR - fuzz_crash_030.md:15:1:15:3
PARSE ERROR - fuzz_crash_030.md:15:3:15:4
PARSE ERROR - fuzz_crash_030.md:16:3:16:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `exposed_item_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:8:5:8:6:**
```roc
		[ .
```
		  ^


**PARSE ERROR**
A parsing error occurred: `expected_packages_close_curly`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:12:8:12:9:**
```roc
pkg: 77"..c", mm} #
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:12:9:12:12:**
```roc
pkg: 77"..c", mm} #
```
        ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:12:12:12:13:**
```roc
pkg: 77"..c", mm} #
```
           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:12:13:12:14:**
```roc
pkg: 77"..c", mm} #
```
            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:12:15:12:17:**
```roc
pkg: 77"..c", mm} #
```
              ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:12:17:12:18:**
```roc
pkg: 77"..c", mm} #
```
                ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:13:2:13:10:**
```roc
	provides # Cd
```
	^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:14:3:14:4:**
```roc
		[ # pen
```
		^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:15:1:15:3:**
```roc
ar,
```
^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:15:3:15:4:**
```roc
ar,
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:16:3:16:4:**
```roc
		]
```
		^


# TOKENS
~~~zig
KwPlatform,
StringStart,StringPart,StringEnd,
KwRequires,
OpenCurly,CloseCurly,
OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,OpenCurly,CloseCurly,Comma,
CloseCurly,
KwExposes,
OpenSquare,Dot,
CloseSquare,
KwPackages,
OpenCurly,
LowerIdent,OpColon,Int,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseCurly,
KwProvides,
OpenSquare,
LowerIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_packages_close_curly"))
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
#
# Cd
# pen

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

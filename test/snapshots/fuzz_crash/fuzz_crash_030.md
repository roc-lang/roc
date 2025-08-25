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
KwPlatform(1:1-1:9),
StringStart(2:2-2:3),StringPart(2:3-2:6),StringEnd(2:6-2:7),
KwRequires(3:2-3:10),
OpenCurly(4:3-4:4),CloseCurly(4:5-4:6),
OpenCurly(5:4-5:5),LowerIdent(5:5-5:7),OpColon(5:8-5:9),UpperIdent(5:10-5:14),NoSpaceOpenRound(5:14-5:15),UpperIdent(5:15-5:18),CloseRound(5:18-5:19),OpFatArrow(5:20-5:22),OpenCurly(5:23-5:24),CloseCurly(5:24-5:25),Comma(5:25-5:26),
CloseCurly(6:4-6:5),
KwExposes(7:2-7:9),
OpenSquare(8:3-8:4),Dot(8:5-8:6),
CloseSquare(9:3-9:4),
KwPackages(10:2-10:10),
OpenCurly(11:3-11:4),
LowerIdent(12:1-12:4),OpColon(12:4-12:5),Int(12:6-12:8),StringStart(12:8-12:9),StringPart(12:9-12:12),StringEnd(12:12-12:13),Comma(12:13-12:14),LowerIdent(12:15-12:17),CloseCurly(12:17-12:18),
KwProvides(13:2-13:10),
OpenSquare(14:3-14:4),
LowerIdent(15:1-15:3),Comma(15:3-15:4),
CloseSquare(16:3-16:4),EndOfFile(16:4-16:4),
~~~
# PARSE
~~~clojure
(file @1.1-16.4
	(malformed-header @12.8-12.9 (tag "expected_packages_close_curly"))
	(statements
		(s-malformed @12.9-12.12 (tag "statement_unexpected_token"))
		(s-malformed @12.12-12.13 (tag "statement_unexpected_token"))
		(s-malformed @12.13-12.14 (tag "statement_unexpected_token"))
		(s-malformed @12.15-12.17 (tag "statement_unexpected_token"))
		(s-malformed @12.17-12.18 (tag "statement_unexpected_token"))
		(s-malformed @13.2-13.10 (tag "statement_unexpected_token"))
		(s-malformed @14.3-14.4 (tag "statement_unexpected_token"))
		(s-malformed @15.1-15.3 (tag "statement_unexpected_token"))
		(s-malformed @15.3-15.4 (tag "statement_unexpected_token"))
		(s-malformed @16.3-16.4 (tag "statement_unexpected_token"))))
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

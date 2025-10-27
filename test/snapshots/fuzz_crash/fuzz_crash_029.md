# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
			} #Ce
	exposes #rd
		[ #
		] # Cse
	packages # Cd
		vides # Cd
		{ # pen
pkg: "..l", mmen		} # Cose
	provides # Cd
		[ Ok(world), (n # pen
ar,
		]
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_029.md:4:4:4:5
PARSE ERROR - fuzz_crash_029.md:5:14:5:17
PARSE ERROR - fuzz_crash_029.md:5:19:5:21
PARSE ERROR - fuzz_crash_029.md:5:22:5:23
PARSE ERROR - fuzz_crash_029.md:5:23:5:24
PARSE ERROR - fuzz_crash_029.md:5:24:5:25
PARSE ERROR - fuzz_crash_029.md:6:4:6:5
PARSE ERROR - fuzz_crash_029.md:7:2:7:9
PARSE ERROR - fuzz_crash_029.md:8:3:8:4
PARSE ERROR - fuzz_crash_029.md:9:3:9:4
PARSE ERROR - fuzz_crash_029.md:10:2:10:10
PARSE ERROR - fuzz_crash_029.md:11:3:11:8
PARSE ERROR - fuzz_crash_029.md:12:3:12:4
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_029.md:13:6:13:7
PARSE ERROR - fuzz_crash_029.md:13:7:13:10
PARSE ERROR - fuzz_crash_029.md:13:10:13:11
PARSE ERROR - fuzz_crash_029.md:13:11:13:12
PARSE ERROR - fuzz_crash_029.md:13:13:13:17
PARSE ERROR - fuzz_crash_029.md:13:19:13:20
PARSE ERROR - fuzz_crash_029.md:14:2:14:10
PARSE ERROR - fuzz_crash_029.md:15:3:15:4
PARSE ERROR - fuzz_crash_029.md:15:14:15:15
PARSE ERROR - fuzz_crash_029.md:15:16:15:17
PARSE ERROR - fuzz_crash_029.md:15:17:15:18
PARSE ERROR - fuzz_crash_029.md:16:1:16:3
PARSE ERROR - fuzz_crash_029.md:16:3:16:4
PARSE ERROR - fuzz_crash_029.md:17:3:17:4
MALFORMED TYPE - fuzz_crash_029.md:13:6:13:7
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_requires_rigids_close_curly`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:4:4:4:5:**
```roc
			{ # d
```
			^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:14:5:17:**
```roc
			n! : List(Str) => {}, # ure
```
			          ^^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**fuzz_crash_029.md:5:19:5:21:**
```roc
			n! : List(Str) => {}, # ure
```
			               ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:22:5:23:**
```roc
			n! : List(Str) => {}, # ure
```
			                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:23:5:24:**
```roc
			n! : List(Str) => {}, # ure
```
			                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:24:5:25:**
```roc
			n! : List(Str) => {}, # ure
```
			                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:6:4:6:5:**
```roc
			} #Ce
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:7:2:7:9:**
```roc
	exposes #rd
```
	^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:8:3:8:4:**
```roc
		[ #
```
		^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:9:3:9:4:**
```roc
		] # Cse
```
		^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:10:2:10:10:**
```roc
	packages # Cd
```
	^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:11:3:11:8:**
```roc
		vides # Cd
```
		^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:12:3:12:4:**
```roc
		{ # pen
```
		^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_029.md:13:6:13:7:**
```roc
pkg: "..l", mmen		} # Cose
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:7:13:10:**
```roc
pkg: "..l", mmen		} # Cose
```
      ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:10:13:11:**
```roc
pkg: "..l", mmen		} # Cose
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:11:13:12:**
```roc
pkg: "..l", mmen		} # Cose
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:13:13:17:**
```roc
pkg: "..l", mmen		} # Cose
```
            ^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:19:13:20:**
```roc
pkg: "..l", mmen		} # Cose
```
                		^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:14:2:14:10:**
```roc
	provides # Cd
```
	^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:15:3:15:4:**
```roc
		[ Ok(world), (n # pen
```
		^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**fuzz_crash_029.md:15:14:15:15:**
```roc
		[ Ok(world), (n # pen
```
		           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:15:16:15:17:**
```roc
		[ Ok(world), (n # pen
```
		             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:15:17:15:18:**
```roc
		[ Ok(world), (n # pen
```
		              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:16:1:16:3:**
```roc
ar,
```
^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:16:3:16:4:**
```roc
ar,
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:17:3:17:4:**
```roc
		]
```
		^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_029.md:13:6:13:7:**
```roc
pkg: "..l", mmen		} # Cose
```
     ^


# TOKENS
~~~zig
KwPlatform,
StringStart,StringPart,StringEnd,
KwRequires,
OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,OpenCurly,CloseCurly,Comma,
CloseCurly,
KwExposes,
OpenSquare,
CloseSquare,
KwPackages,
LowerIdent,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseCurly,
KwProvides,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,
LowerIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_requires_rigids_close_curly"))
	(statements
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "pkg")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
# ure
# Ce
# rd
#
# Cse
# Cd
# Cd
# pen
pkg : 
# Cose
# Cd
# pen

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "pkg")
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

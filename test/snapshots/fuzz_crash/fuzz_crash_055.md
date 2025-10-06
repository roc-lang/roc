# META
~~~ini
description=fuzz crash
type=file:FuzzCrash055.roc
~~~
# SOURCE
~~~roc
FuzzCrash055 := {}

module(a).h:s
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_055.md:3:1:3:7
PARSE ERROR - fuzz_crash_055.md:3:7:3:8
PARSE ERROR - fuzz_crash_055.md:3:8:3:9
PARSE ERROR - fuzz_crash_055.md:3:9:3:10
PARSE ERROR - fuzz_crash_055.md:3:10:3:12
PARSE ERROR - fuzz_crash_055.md:3:12:3:13
PARSE ERROR - fuzz_crash_055.md:3:13:3:14
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:3:1:3:7:**
```roc
module(a).h:s
```
^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:3:7:3:8:**
```roc
module(a).h:s
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:3:8:3:9:**
```roc
module(a).h:s
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:3:9:3:10:**
```roc
module(a).h:s
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:3:10:3:12:**
```roc
module(a).h:s
```
         ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:3:12:3:13:**
```roc
module(a).h:s
```
           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:3:13:3:14:**
```roc
module(a).h:s
```
            ^


# TOKENS
~~~zig
UpperIdent(1:1-1:13),OpColonEqual(1:14-1:16),OpenCurly(1:17-1:18),CloseCurly(1:18-1:19),
KwModule(3:1-3:7),NoSpaceOpenRound(3:7-3:8),LowerIdent(3:8-3:9),CloseRound(3:9-3:10),NoSpaceDotLowerIdent(3:10-3:12),OpColon(3:12-3:13),LowerIdent(3:13-3:14),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.14
	(type-module @1.1-1.13)
	(statements
		(s-type-decl @1.1-1.19
			(header @1.1-1.13 (name "FuzzCrash055")
				(args))
			(ty-record @1.17-1.19))
		(s-malformed @3.1-3.7 (tag "statement_unexpected_token"))
		(s-malformed @3.7-3.8 (tag "statement_unexpected_token"))
		(s-malformed @3.8-3.9 (tag "statement_unexpected_token"))
		(s-malformed @3.9-3.10 (tag "statement_unexpected_token"))
		(s-malformed @3.10-3.12 (tag "statement_unexpected_token"))
		(s-malformed @3.12-3.13 (tag "statement_unexpected_token"))
		(s-malformed @3.13-3.14 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
FuzzCrash055 := {}

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.19
		(ty-header @1.1-1.13 (name "FuzzCrash055"))
		(ty-record @1.17-1.19)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.19 (type "FuzzCrash055")
			(ty-header @1.1-1.13 (name "FuzzCrash055"))))
	(expressions))
~~~

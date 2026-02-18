# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}|(0,)|||0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_041.md:1:20:1:21
PARSE ERROR - fuzz_crash_041.md:1:21:1:22
PARSE ERROR - fuzz_crash_041.md:1:22:1:23
PARSE ERROR - fuzz_crash_041.md:1:23:1:24
PARSE ERROR - fuzz_crash_041.md:1:24:1:25
PARSE ERROR - fuzz_crash_041.md:1:25:1:26
PARSE ERROR - fuzz_crash_041.md:1:26:1:27
PARSE ERROR - fuzz_crash_041.md:1:27:1:28
PARSE ERROR - fuzz_crash_041.md:1:28:1:29
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_041.md:1:20:1:21:**
```roc
app[]{f:platform""}|(0,)|||0
```
                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_041.md:1:21:1:22:**
```roc
app[]{f:platform""}|(0,)|||0
```
                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_041.md:1:22:1:23:**
```roc
app[]{f:platform""}|(0,)|||0
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_041.md:1:23:1:24:**
```roc
app[]{f:platform""}|(0,)|||0
```
                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_041.md:1:24:1:25:**
```roc
app[]{f:platform""}|(0,)|||0
```
                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_041.md:1:25:1:26:**
```roc
app[]{f:platform""}|(0,)|||0
```
                        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_041.md:1:26:1:27:**
```roc
app[]{f:platform""}|(0,)|||0
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_041.md:1:27:1:28:**
```roc
app[]{f:platform""}|(0,)|||0
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_041.md:1:28:1:29:**
```roc
app[]{f:platform""}|(0,)|||0
```
                           ^


# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpBar,NoSpaceOpenRound,Int,Comma,CloseRound,OpBar,OpBar,OpBar,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "f")
			(e-string
				(e-string-part (raw ""))))
		(packages
			(record-field (name "f")
				(e-string
					(e-string-part (raw ""))))))
	(statements
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
app [] { f: platform "" }
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

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
KwApp(1:1-1:4),OpenSquare(1:4-1:5),CloseSquare(1:5-1:6),OpenCurly(1:6-1:7),LowerIdent(1:7-1:8),OpColon(1:8-1:9),KwPlatform(1:9-1:17),StringStart(1:17-1:18),StringPart(1:18-1:18),StringEnd(1:18-1:19),CloseCurly(1:19-1:20),OpBar(1:20-1:21),NoSpaceOpenRound(1:21-1:22),Int(1:22-1:23),Comma(1:23-1:24),CloseRound(1:24-1:25),OpBar(1:25-1:26),OpBar(1:26-1:27),OpBar(1:27-1:28),Int(1:28-1:29),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.29
	(app @1.1-1.20
		(provides @1.4-1.6)
		(record-field @1.7-1.19 (name "f")
			(e-string @1.17-1.19
				(e-string-part @1.18-1.18 (raw ""))))
		(packages @1.6-1.20
			(record-field @1.7-1.19 (name "f")
				(e-string @1.17-1.19
					(e-string-part @1.18-1.18 (raw ""))))))
	(statements
		(s-malformed @1.20-1.21 (tag "statement_unexpected_token"))
		(s-malformed @1.21-1.22 (tag "statement_unexpected_token"))
		(s-malformed @1.22-1.23 (tag "statement_unexpected_token"))
		(s-malformed @1.23-1.24 (tag "statement_unexpected_token"))
		(s-malformed @1.24-1.25 (tag "statement_unexpected_token"))
		(s-malformed @1.25-1.26 (tag "statement_unexpected_token"))
		(s-malformed @1.26-1.27 (tag "statement_unexpected_token"))
		(s-malformed @1.27-1.28 (tag "statement_unexpected_token"))
		(s-malformed @1.28-1.29 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
MALFORMED INPUT
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

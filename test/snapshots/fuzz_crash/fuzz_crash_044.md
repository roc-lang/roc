# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{{0
}}

""
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_044.md:1:20:1:21
PARSE ERROR - fuzz_crash_044.md:1:21:1:22
PARSE ERROR - fuzz_crash_044.md:1:22:1:23
PARSE ERROR - fuzz_crash_044.md:2:1:2:2
PARSE ERROR - fuzz_crash_044.md:2:2:2:3
PARSE ERROR - fuzz_crash_044.md:4:1:4:2
PARSE ERROR - fuzz_crash_044.md:4:2:4:2
PARSE ERROR - fuzz_crash_044.md:4:2:4:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_044.md:1:20:1:21:**
```roc
app[]{f:platform""}{{0
```
                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_044.md:1:21:1:22:**
```roc
app[]{f:platform""}{{0
```
                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_044.md:1:22:1:23:**
```roc
app[]{f:platform""}{{0
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_044.md:2:1:2:2:**
```roc
}}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_044.md:2:2:2:3:**
```roc
}}
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_044.md:4:1:4:2:**
```roc
""
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_044.md:4:2:4:2:**
```roc
""
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_044.md:4:2:4:3:**
```roc
""
```
 ^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:4-1:5),CloseSquare(1:5-1:6),OpenCurly(1:6-1:7),LowerIdent(1:7-1:8),OpColon(1:8-1:9),KwPlatform(1:9-1:17),StringStart(1:17-1:18),StringPart(1:18-1:18),StringEnd(1:18-1:19),CloseCurly(1:19-1:20),OpenCurly(1:20-1:21),OpenCurly(1:21-1:22),Int(1:22-1:23),
CloseCurly(2:1-2:2),CloseCurly(2:2-2:3),
StringStart(4:1-4:2),StringPart(4:2-4:2),StringEnd(4:2-4:3),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(file @1.1-4.3
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
		(s-malformed @2.1-2.2 (tag "statement_unexpected_token"))
		(s-malformed @2.2-2.3 (tag "statement_unexpected_token"))
		(s-malformed @4.1-4.2 (tag "statement_unexpected_token"))
		(s-malformed @4.2-4.2 (tag "statement_unexpected_token"))
		(s-malformed @4.2-4.3 (tag "statement_unexpected_token"))))
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

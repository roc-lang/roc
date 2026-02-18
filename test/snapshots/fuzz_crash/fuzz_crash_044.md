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

**fuzz_crash_044.md:1:20:1:21:**
```roc
app[]{f:platform""}{{0
```
                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_044.md:1:21:1:22:**
```roc
app[]{f:platform""}{{0
```
                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_044.md:1:22:1:23:**
```roc
app[]{f:platform""}{{0
```
                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_044.md:2:1:2:2:**
```roc
}}
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_044.md:2:2:2:3:**
```roc
}}
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_044.md:4:1:4:2:**
```roc
""
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_044.md:4:2:4:2:**
```roc
""
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_044.md:4:2:4:3:**
```roc
""
```
 ^


# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpenCurly,OpenCurly,Int,
CloseCurly,CloseCurly,
StringStart,StringPart,StringEnd,
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

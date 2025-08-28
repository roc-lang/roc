# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu:;::::::::::::::le[%
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_002.md:1:1:1:5
PARSE ERROR - fuzz_crash_002.md:1:5:1:6
PARSE ERROR - fuzz_crash_002.md:1:6:1:7
PARSE ERROR - fuzz_crash_002.md:1:7:1:8
PARSE ERROR - fuzz_crash_002.md:1:8:1:9
PARSE ERROR - fuzz_crash_002.md:1:9:1:10
PARSE ERROR - fuzz_crash_002.md:1:10:1:11
PARSE ERROR - fuzz_crash_002.md:1:11:1:12
PARSE ERROR - fuzz_crash_002.md:1:12:1:13
PARSE ERROR - fuzz_crash_002.md:1:13:1:14
PARSE ERROR - fuzz_crash_002.md:1:14:1:15
PARSE ERROR - fuzz_crash_002.md:1:15:1:16
PARSE ERROR - fuzz_crash_002.md:1:16:1:17
PARSE ERROR - fuzz_crash_002.md:1:17:1:18
PARSE ERROR - fuzz_crash_002.md:1:18:1:19
PARSE ERROR - fuzz_crash_002.md:1:19:1:20
PARSE ERROR - fuzz_crash_002.md:1:20:1:21
PARSE ERROR - fuzz_crash_002.md:1:21:1:23
PARSE ERROR - fuzz_crash_002.md:1:23:1:24
PARSE ERROR - fuzz_crash_002.md:1:24:1:25
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_002.md:1:1:1:5:**
```roc
modu:;::::::::::::::le[%
```
^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:5:1:6:**
```roc
modu:;::::::::::::::le[%
```
    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:6:1:7:**
```roc
modu:;::::::::::::::le[%
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:7:1:8:**
```roc
modu:;::::::::::::::le[%
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:8:1:9:**
```roc
modu:;::::::::::::::le[%
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:9:1:10:**
```roc
modu:;::::::::::::::le[%
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:10:1:11:**
```roc
modu:;::::::::::::::le[%
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:11:1:12:**
```roc
modu:;::::::::::::::le[%
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:12:1:13:**
```roc
modu:;::::::::::::::le[%
```
           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:13:1:14:**
```roc
modu:;::::::::::::::le[%
```
            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:14:1:15:**
```roc
modu:;::::::::::::::le[%
```
             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:15:1:16:**
```roc
modu:;::::::::::::::le[%
```
              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:16:1:17:**
```roc
modu:;::::::::::::::le[%
```
               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:17:1:18:**
```roc
modu:;::::::::::::::le[%
```
                ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:18:1:19:**
```roc
modu:;::::::::::::::le[%
```
                 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:19:1:20:**
```roc
modu:;::::::::::::::le[%
```
                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:20:1:21:**
```roc
modu:;::::::::::::::le[%
```
                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:21:1:23:**
```roc
modu:;::::::::::::::le[%
```
                    ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:23:1:24:**
```roc
modu:;::::::::::::::le[%
```
                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_002.md:1:24:1:25:**
```roc
modu:;::::::::::::::le[%
```
                       ^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpColon(1:5-1:6),MalformedUnknownToken(1:6-1:7),OpColon(1:7-1:8),OpColon(1:8-1:9),OpColon(1:9-1:10),OpColon(1:10-1:11),OpColon(1:11-1:12),OpColon(1:12-1:13),OpColon(1:13-1:14),OpColon(1:14-1:15),OpColon(1:15-1:16),OpColon(1:16-1:17),OpColon(1:17-1:18),OpColon(1:18-1:19),OpColon(1:19-1:20),OpColon(1:20-1:21),LowerIdent(1:21-1:23),OpenSquare(1:23-1:24),OpPercent(1:24-1:25),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.25
	(malformed-header @1.1-1.5 (tag "missing_header"))
	(statements
		(s-malformed @1.5-1.6 (tag "statement_unexpected_token"))
		(s-malformed @1.6-1.7 (tag "statement_unexpected_token"))
		(s-malformed @1.7-1.8 (tag "statement_unexpected_token"))
		(s-malformed @1.8-1.9 (tag "statement_unexpected_token"))
		(s-malformed @1.9-1.10 (tag "statement_unexpected_token"))
		(s-malformed @1.10-1.11 (tag "statement_unexpected_token"))
		(s-malformed @1.11-1.12 (tag "statement_unexpected_token"))
		(s-malformed @1.12-1.13 (tag "statement_unexpected_token"))
		(s-malformed @1.13-1.14 (tag "statement_unexpected_token"))
		(s-malformed @1.14-1.15 (tag "statement_unexpected_token"))
		(s-malformed @1.15-1.16 (tag "statement_unexpected_token"))
		(s-malformed @1.16-1.17 (tag "statement_unexpected_token"))
		(s-malformed @1.17-1.18 (tag "statement_unexpected_token"))
		(s-malformed @1.18-1.19 (tag "statement_unexpected_token"))
		(s-malformed @1.19-1.20 (tag "statement_unexpected_token"))
		(s-malformed @1.20-1.21 (tag "statement_unexpected_token"))
		(s-malformed @1.21-1.23 (tag "statement_unexpected_token"))
		(s-malformed @1.23-1.24 (tag "statement_unexpected_token"))
		(s-malformed @1.24-1.25 (tag "statement_unexpected_token"))))
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

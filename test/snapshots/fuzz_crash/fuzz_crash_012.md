# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||(|(l888888888|
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_012.md:1:1:1:2
PARSE ERROR - fuzz_crash_012.md:1:2:1:3
PARSE ERROR - fuzz_crash_012.md:1:3:1:4
PARSE ERROR - fuzz_crash_012.md:1:4:1:5
PARSE ERROR - fuzz_crash_012.md:1:5:1:6
PARSE ERROR - fuzz_crash_012.md:1:6:1:16
PARSE ERROR - fuzz_crash_012.md:1:16:1:17
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_012.md:1:1:1:2:**
```roc
||(|(l888888888|
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_012.md:1:2:1:3:**
```roc
||(|(l888888888|
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_012.md:1:3:1:4:**
```roc
||(|(l888888888|
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_012.md:1:4:1:5:**
```roc
||(|(l888888888|
```
   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_012.md:1:5:1:6:**
```roc
||(|(l888888888|
```
    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_012.md:1:6:1:16:**
```roc
||(|(l888888888|
```
     ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_012.md:1:16:1:17:**
```roc
||(|(l888888888|
```
               ^


# TOKENS
~~~zig
OpBar(1:1-1:2),OpBar(1:2-1:3),NoSpaceOpenRound(1:3-1:4),OpBar(1:4-1:5),NoSpaceOpenRound(1:5-1:6),LowerIdent(1:6-1:16),OpBar(1:16-1:17),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.17
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(s-malformed @1.2-1.3 (tag "statement_unexpected_token"))
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))
		(s-malformed @1.4-1.5 (tag "statement_unexpected_token"))
		(s-malformed @1.5-1.6 (tag "statement_unexpected_token"))
		(s-malformed @1.6-1.16 (tag "statement_unexpected_token"))
		(s-malformed @1.16-1.17 (tag "statement_unexpected_token"))))
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

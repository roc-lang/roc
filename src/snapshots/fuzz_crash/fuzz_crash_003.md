# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
= "te
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_003.md:1:1:1:2
PARSE ERROR - fuzz_crash_003.md:1:3:1:4
PARSE ERROR - fuzz_crash_003.md:1:4:1:6
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_003.md:1:1:1:2:**
```roc
= "te
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_003.md:1:3:1:4:**
```roc
= "te
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_003.md:1:4:1:6:**
```roc
= "te
```
   ^^


# TOKENS
~~~zig
OpAssign(1:1-1:2),StringStart(1:3-1:4),StringPart(1:4-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(file @1.1-1.6
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))
		(s-malformed @1.4-1.6 (tag "statement_unexpected_token"))))
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

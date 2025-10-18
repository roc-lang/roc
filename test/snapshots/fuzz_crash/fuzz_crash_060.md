# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
0"
}
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_060.md:1:1:1:2
PARSE ERROR - fuzz_crash_060.md:1:2:1:3
PARSE ERROR - fuzz_crash_060.md:1:3:1:3
PARSE ERROR - fuzz_crash_060.md:1:3:1:3
PARSE ERROR - fuzz_crash_060.md:2:1:2:2
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

```roc
0"
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:1:1:1:2:**
```roc
0"
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:1:2:1:3:**
```roc
0"
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:1:3:1:3:**
```roc
0"
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:1:3:1:3:**
```roc
0"
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:2:1:2:2:**
```roc
}
```
^


# TOKENS
~~~zig
Int,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
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

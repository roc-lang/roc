# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||(|(l888888888|
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_012.md:1:1:1:3:**
```roc
||(|(l888888888|
```
^^


**UNEXPECTED TOKEN IN PATTERN**
The token **|(** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**fuzz_crash_012.md:1:4:1:6:**
```roc
||(|(l888888888|
```
   ^^


**UNEXPECTED TOKEN IN PATTERN**
The token **(|** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**fuzz_crash_012.md:1:3:1:5:**
```roc
||(|(l888888888|
```
  ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_012.md:1:17:1:17:**
```roc
||(|(l888888888|
```
                


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
OpBar(1:1-1:2),OpBar(1:2-1:3),NoSpaceOpenRound(1:3-1:4),OpBar(1:4-1:5),NoSpaceOpenRound(1:5-1:6),LowerIdent(1:6-1:16),OpBar(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(file @1-1-1-17
	(malformed-header @1-1-1-3 (tag "missing_header"))
	(statements
		(e-malformed @1-17-1-17 (reason "expected_expr_bar"))))
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

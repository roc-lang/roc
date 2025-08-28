# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]e="""
import#\
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_078.md:3:1:3:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `incomplete_import`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_078.md:3:1:3:1:**
```roc

```
^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),LowerIdent(1:9-1:10),OpAssign(1:10-1:11),MultilineStringStart(1:11-1:14),StringPart(1:14-1:14),
KwImport(2:1-2:7),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.7
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-decl @1.9-1.14
			(p-ident @1.9-1.10 (raw "e"))
			(e-multiline-string @1.11-1.14
				(e-string-part @1.14-1.14 (raw ""))))
		(s-malformed @2.1-2.7 (tag "incomplete_import"))))
~~~
# FORMATTED
~~~roc
MALFORMED INPUT
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.9-1.10 (ident "e"))
		(e-string @1.11-1.14)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.9-1.10 (type "Str")))
	(expressions
		(expr @1.11-1.14 (type "Str"))))
~~~

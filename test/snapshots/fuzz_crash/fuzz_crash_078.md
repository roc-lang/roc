# META
~~~ini
description=fuzz crash
type=file:FuzzCrash078.roc
~~~
# SOURCE
~~~roc
FuzzCrash078 := {}

import#\
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_078.md:4:1:4:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `incomplete_import`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_078.md:4:1:4:1:**
```roc

```
^


# TOKENS
~~~zig
UpperIdent(1:1-1:13),OpColonEqual(1:14-1:16),OpenCurly(1:17-1:18),CloseCurly(1:18-1:19),
KwImport(3:1-3:7),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.7
	(type-module @1.1-1.13)
	(statements
		(s-type-decl @1.1-1.19
			(header @1.1-1.13 (name "FuzzCrash078")
				(args))
			(ty-record @1.17-1.19))
		(s-malformed @3.1-3.7 (tag "incomplete_import"))))
~~~
# FORMATTED
~~~roc
FuzzCrash078 := {}

# \
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.19
		(ty-header @1.1-1.13 (name "FuzzCrash078"))
		(ty-record @1.17-1.19)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.19 (type "FuzzCrash078")
			(ty-header @1.1-1.13 (name "FuzzCrash078"))))
	(expressions))
~~~
